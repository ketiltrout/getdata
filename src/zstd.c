/* Copyright (C) 2026 Graeme Smecher
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

/* ZSTD_STATIC_LINKING_ONLY exposes zstd's unstable API section.  We define
 * it only for the ZSTD_FRAMEHEADERSIZE_MAX and ZSTD_SKIPPABLEHEADERSIZE
 * macros, whose values are fixed by RFC 8878. */
#define ZSTD_STATIC_LINKING_ONLY
#include <zstd.h>

/* The zstd encoding stores data as independently compressed zstd frames,
 * appended sequentially.
 *
 * Writes are in-place and append-only for forward writes.  Backward writes
 * into already-committed frames trigger a rewrite: the frames overlapping the
 * write are decompressed, patched, recompressed, and written back in-place,
 * and the unchanged frames after them are slid along the file to fit.  Memory
 * use scales with the write span.
 *
 * On open, frame headers are scanned to build an in-memory index for
 * random-access reads.  The index is binary-searched, and a last-hit hint
 * makes sequential reads O(1).  Read-only handles extend the index on demand
 * when a read lands past its end, so a long-lived reader can follow a file
 * that another process is appending to.  Scanning stops at the first frame
 * with an unknown content size, which indicates corruption or a foreign file.
 *
 * Incoming write data accumulates uncompressed in write_buf, and no
 * compression or file I/O happens until the frame closes.  At frame close the
 * whole buffer is compressed by one ZSTD_compress2() call, which records the
 * frame's content size and checksum.  Full and partial frames are closed the
 * same way.
 */

/* Default frame size for compression (128 KiB uncompressed) */
#define GD_ZSTD_FRAME_SIZE (128 * 1024)

/* Default compression level (zstd README.md includes benchmarks) */
#define GD_ZSTD_COMP_LEVEL 1

/* Per RFC 8878, section 3.1.1, the last 32 bits of each compressed frame are a
 * checksum when ZSTD_c_checksumFlag is set (which it is, for frames we wrote.)
 * This allows us to detect when our in-memory index (presumably as a reader)
 * is invalidated by an in-place write from another process.  Because libzstd
 * doesn't offer an API for reading this checksum, we have to jump through a
 * few hoops to get it.  Externally written fields without checksums work OK
 * under this scheme, except that the field we use to detect content changes is
 * only weakly content-dependent.  This affects the reader's coherence
 * (typically meaning a delay before the rewrite is correctly noticed and data
 * is updated), but not the validity of the file on disk (which the writer
 * controls and remains correct.) */
#define GD_ZSTD_CHECK_LEN 4

/* The location of the identity tag within a compressed frame image, whether
 * expressed as a pointer into a buffer holding the frame or as a byte offset
 * of the frame within the file */
#define GD_ZSTD_FRAME_TAG(base,comp_size) ((base) + (comp_size) - GD_ZSTD_CHECK_LEN)

static uint32_t _GD_ZstdTagValue(const void *tag)
{
  uint32_t value;
  memcpy(&value, tag, sizeof(value));
  return value;
}

/* Read a little-endian 32-bit word, as used by zstd magic and length
 * fields */
static uint32_t _GD_ZstdLE32(const uint8_t *p)
{
  uint32_t v;
  memcpy(&v, p, sizeof(v));
#ifdef WORDS_BIGENDIAN
  v = gd_swap32(v);
#endif
  return v;
}

struct gd_zstd_frame {
  uint64_t comp_offset;   /* byte offset of compressed frame in file */
  uint64_t decomp_start;  /* decompressed byte offset where this frame begins */
  uint32_t comp_size;     /* compressed size of this frame */
  uint32_t decomp_size;   /* decompressed size of this frame */
  uint32_t check;         /* identity tag: tail of the compressed frame */
};

struct gd_zstddata {
  int fd;                 /* underlying file descriptor */

  /* Frame index */
  struct gd_zstd_frame *frames;
  uint32_t n_frames;
  uint32_t frames_alloc;
  uint64_t total_decomp;  /* total committed decompressed size in bytes */
  uint64_t comp_end;      /* compressed offset one past the last scanned frame */

  /* Read state */
  ZSTD_DCtx *dctx;
  char *read_buf;         /* decompressed frame cache; rewrite patch scratch */
  uint32_t read_buf_size;
  char *comp_buf;         /* compressed frame fetch and scan-probe buffer */
  size_t comp_buf_size;
  int32_t cached_frame;   /* which frame is cached, -1 = none */
  uint32_t search_hint;   /* frame last returned by _GD_ZstdLocateFrame */

  /* Write state */
  ZSTD_CCtx *cctx;
  char *write_buf;        /* stable input window: data copied here before feeding to compressor */
  char *out_buf;          /* compressed output buffer, size = ZSTD_compressBound(frame_size) */
  uint32_t out_buf_size;
  uint32_t frame_size;    /* target uncompressed frame size */
  uint32_t frame_used;    /* bytes copied into write_buf for the current open frame */
  uint64_t write_offset;  /* file offset for next compressed write */

  size_t last_error;      /* last ZSTD error code */
};

/* Read a frame's identity tag from the file. */
static int _GD_ZstdReadTag(const struct gd_zstddata *z, uint64_t comp_offset,
    uint32_t comp_size, uint32_t *tag)
{
  uint8_t buffer[GD_ZSTD_CHECK_LEN];

  if (gd_PRead(z->fd, buffer, GD_ZSTD_CHECK_LEN,
        (off64_t)GD_ZSTD_FRAME_TAG(comp_offset, comp_size)) != GD_ZSTD_CHECK_LEN)
    return -1;

  *tag = _GD_ZstdTagValue(buffer);
  return 0;
}

/* Add a frame to the in-memory index */
static int _GD_ZstdAddIndexEntry(struct gd_zstddata *z, uint64_t comp_offset,
    uint32_t comp_size, uint32_t decomp_size, uint32_t check)
{
  if (z->n_frames >= z->frames_alloc) {
    uint32_t new_alloc = z->frames_alloc ? z->frames_alloc * 2 : 64;
    struct gd_zstd_frame *new_frames = realloc(z->frames,
        new_alloc * sizeof(*new_frames));
    if (new_frames == NULL)
      return -1;
    z->frames = new_frames;
    z->frames_alloc = new_alloc;
  }

  z->frames[z->n_frames].comp_offset = comp_offset;
  z->frames[z->n_frames].decomp_start = z->total_decomp;
  z->frames[z->n_frames].comp_size = comp_size;
  z->frames[z->n_frames].decomp_size = decomp_size;
  z->frames[z->n_frames].check = check;
  z->n_frames++;
  z->total_decomp += decomp_size;
  return 0;
}

/* Extend the in-memory index with any frames appended past comp_end since
 * the last scan. */
static int _GD_ZstdUpdateIndex(struct gd_zstddata *z)
{
  off64_t pos;
  gd_stat64_t statbuf;

  dtrace("%p", z);

  if (gd_fstat64(z->fd, &statbuf)) {
    dreturn("%i", -1);
    return -1;
  }

  /* A file shorter than the indexed frames means a writer has rewritten it
   * in place behind us (a writer never shrinks its own file mid-index, so
   * this only fires on stale read-only handles).  Drop the frames past the
   * new end; survivors that were nonetheless rewritten fail their identity
   * checks when read, which triggers the full recovery path. */
  if ((uint64_t)statbuf.st_size < z->comp_end) {
    while (z->n_frames > 0) {
      const struct gd_zstd_frame *f = &z->frames[z->n_frames - 1];
      if (f->comp_offset + f->comp_size <= (uint64_t)statbuf.st_size)
        break;
      z->total_decomp = f->decomp_start;
      z->n_frames--;
    }
    z->comp_end = (z->n_frames > 0) ?
      z->frames[z->n_frames - 1].comp_offset +
      z->frames[z->n_frames - 1].comp_size : 0;
    z->search_hint = 0;
    z->cached_frame = -1;
  }

  pos = (off64_t)z->comp_end;

  while (pos < statbuf.st_size) {
    uint8_t hdr[ZSTD_FRAMEHEADERSIZE_MAX];
    size_t remaining, hdr_read;
    size_t comp_size;
    uint32_t magic;
    ssize_t nread;

    remaining = (size_t)(statbuf.st_size - pos);

    /* Read the frame header (or as much of it as we can get) */
    nread = gd_PRead(z->fd, hdr, ZSTD_FRAMEHEADERSIZE_MAX, pos);
    if (nread < 0) {
      dreturn("%i", -1);
      return -1;
    }
    hdr_read = (size_t)nread;
    if (hdr_read < 4)
      break;  /* not even a magic number */

    magic = _GD_ZstdLE32(hdr);

    if ((magic & ZSTD_MAGIC_SKIPPABLE_MASK) == ZSTD_MAGIC_SKIPPABLE_START) {
      /* Skippable frame */
      uint64_t skip_len;

      if (hdr_read < ZSTD_SKIPPABLEHEADERSIZE)
        break;  /* truncated skippable header */
      skip_len = ZSTD_SKIPPABLEHEADERSIZE + (uint64_t)_GD_ZstdLE32(hdr + 4);
      if (skip_len > (uint64_t)remaining)
        break;  /* truncated skippable frame */
      pos += (off64_t)skip_len;
      z->comp_end = (uint64_t)pos;
    } else if (magic == ZSTD_MAGICNUMBER) {
      /* Data frame: use ZSTD_findFrameCompressedSize to determine its
       * compressed size.  This needs the full compressed frame in the
       * buffer; ZSTD_compressBound(content size) is a conservative upper
       * bound on that for any frame we wrote.  comp_buf doubles as the
       * probe buffer. */
      const unsigned long long content_size =
        ZSTD_getFrameContentSize(hdr, hdr_read);
      size_t probe_size;

      /* We are the only producer: all frames have a known content size no
       * larger than a frame's 32-bit maximum.  Anything else (including a
       * header truncated at EOF, which reports an error here) means a
       * corrupt or foreign file -- stop. */
      if (content_size == ZSTD_CONTENTSIZE_UNKNOWN ||
          content_size == ZSTD_CONTENTSIZE_ERROR ||
          content_size > UINT32_MAX)
        break;

      probe_size = ZSTD_compressBound((size_t)content_size);
      if (ZSTD_isError(probe_size) || probe_size > remaining)
        probe_size = remaining;

      if (probe_size > z->comp_buf_size) {
        char *new_buf = realloc(z->comp_buf, probe_size);
        if (new_buf == NULL) {
          dreturn("%i", -1);
          return -1;
        }
        z->comp_buf = new_buf;
        z->comp_buf_size = probe_size;
      }

      if (gd_PRead(z->fd, z->comp_buf, probe_size, pos) !=
          (ssize_t)probe_size)
      {
        dreturn("%i", -1);
        return -1;
      }

      comp_size = ZSTD_findFrameCompressedSize(z->comp_buf, probe_size);

      if (ZSTD_isError(comp_size) || comp_size > UINT32_MAX)
        break;

      if (content_size > 0) {
        if (_GD_ZstdAddIndexEntry(z, (uint64_t)pos, (uint32_t)comp_size,
              (uint32_t)content_size,
              _GD_ZstdTagValue(GD_ZSTD_FRAME_TAG(z->comp_buf, comp_size))))
        {
          dreturn("%i", -1);
          return -1;
        }
      }

      pos += (off64_t)comp_size;
      z->comp_end = (uint64_t)pos;
    } else
      break;  /* not a zstd frame */
  }

  dreturn("%i", 0);
  return 0;
}

/* (Re)build the in-memory index from the start of the file. */
static int _GD_ZstdBuildIndex(struct gd_zstddata *z)
{
  int ret;

  dtrace("%p", z);

  z->n_frames = 0;
  z->total_decomp = 0;
  z->comp_end = 0;
  z->search_hint = 0;
  z->cached_frame = -1;

  ret = _GD_ZstdUpdateIndex(z);
  dreturn("%i", ret);
  return ret;
}

/* Recover a read-only handle's index after an in-place rewrite.  A rewrite
 * replaces a contiguous range of frames and relocates everything after it,
 * so a stale index is typically a valid prefix followed by entries that are
 * wrong in content or position: binary-search the boundary using the
 * identity tags, keep the prefix, and re-extend from there.  'bad', when
 * non-negative, is a frame already known to be invalid, capping the
 * search.  Correctness does not rest on the prefix model: any invalid
 * entry that survives (e.g. an unmoved frame beyond a size-preserving
 * rewrite) fails its per-read identity check and lands back here, and the
 * cap guarantees progress. */
static int _GD_ZstdRecoverIndex(struct gd_zstddata *z, int32_t bad)
{
  uint32_t lo = 0;
  uint32_t hi = (bad >= 0 && (uint32_t)bad < z->n_frames) ? (uint32_t)bad :
    z->n_frames;

  dtrace("%p, %i", z, bad);

  /* frames in [0, lo) have verified tags; frames in [hi, n_frames) are
   * known or presumed invalid */
  while (lo < hi) {
    const uint32_t mid = lo + (hi - lo) / 2;
    const struct gd_zstd_frame *f = &z->frames[mid];
    uint32_t tag;

    if (_GD_ZstdReadTag(z, f->comp_offset, f->comp_size, &tag) == 0 &&
        tag == f->check)
      lo = mid + 1;
    else
      hi = mid;
  }

  if (lo > 0) {
    const struct gd_zstd_frame *f = &z->frames[lo - 1];
    z->total_decomp = f->decomp_start + f->decomp_size;
    z->comp_end = f->comp_offset + f->comp_size;
  } else {
    z->total_decomp = 0;
    z->comp_end = 0;
  }
  z->n_frames = lo;
  z->search_hint = 0;
  z->cached_frame = -1;

  hi = _GD_ZstdUpdateIndex(z);
  dreturn("%i", (int)hi);
  return (int)hi;
}

/* Cheap staleness probe for a read-only handle's frame index: verify that
 * the last indexed frame still begins where the index says a frame of that
 * content size begins.  Appends by another process never disturb it; an
 * in-place rewrite almost always does.  Returns nonzero if stale. */
static int _GD_ZstdIndexStale(struct gd_zstddata *z)
{
  uint8_t hdr[ZSTD_FRAMEHEADERSIZE_MAX];
  const struct gd_zstd_frame *f;
  ssize_t nread;
  uint32_t tag;

  if (z->n_frames == 0)
    return 0;

  f = &z->frames[z->n_frames - 1];
  nread = gd_PRead(z->fd, hdr, ZSTD_FRAMEHEADERSIZE_MAX,
      (off64_t)f->comp_offset);
  if (nread <= 0)
    return 1;

  /* Indexed frames always have a nonzero decompressed size, so this single
   * comparison rejects garbage and truncation (CONTENTSIZE_ERROR, which
   * also covers a bad magic word), a skippable frame (zero), and a data
   * frame of the wrong geometry. */
  if (ZSTD_getFrameContentSize(hdr, (size_t)nread) != f->decomp_size)
    return 1;

  /* same geometry; make sure it is also the same frame */
  if (_GD_ZstdReadTag(z, f->comp_offset, f->comp_size, &tag))
    return 1;

  return tag != f->check;
}

/* Returns the frame index containing byte_offset, or -1 if past EOF. */
static int32_t _GD_ZstdLocateFrame(struct gd_zstddata *z,
    uint64_t byte_offset)
{
  uint32_t lo, hi, i;

  if (byte_offset >= z->total_decomp)
    return -1;

  /* Sequential reads usually hit the frame last returned, or its successor */
  i = z->search_hint;
  if (i < z->n_frames && byte_offset >= z->frames[i].decomp_start) {
    if (byte_offset < z->frames[i].decomp_start + z->frames[i].decomp_size)
      goto found;
    if (++i < z->n_frames && byte_offset < z->frames[i].decomp_start +
        z->frames[i].decomp_size)
      goto found;
  }

  /* Binary search for the last frame with decomp_start <= byte_offset.
   * byte_offset < total_decomp and the frames tile [0, total_decomp), so
   * this frame exists and contains byte_offset. */
  lo = 0;
  hi = z->n_frames - 1;
  while (lo < hi) {
    uint32_t mid = lo + (hi - lo + 1) / 2;
    if (z->frames[mid].decomp_start <= byte_offset)
      lo = mid;
    else
      hi = mid - 1;
  }
  i = lo;

found:
  z->search_hint = i;
  return (int32_t)i;
}

/* Fetch frame frame_idx into comp_buf and decompress it into read_buf,
 * growing both buffers as needed, after verifying the frame against its
 * identity tag.  Does not update cached_frame: the read path fills the
 * frame cache with this, while the rewrite path uses read_buf as its patch
 * scratch. */
static int _GD_ZstdLoadFrame(struct gd_zstddata *z, int32_t frame_idx)
{
  const struct gd_zstd_frame *f;
  size_t result;

  dtrace("%p, %i", z, frame_idx);

  if (frame_idx < 0 || (uint32_t)frame_idx >= z->n_frames) {
    dreturn("%i", -1);
    return -1;
  }

  /* May not exist yet if the file was opened write-only */
  if (z->dctx == NULL) {
    z->dctx = ZSTD_createDCtx();
    if (z->dctx == NULL) {
      dreturn("%i", -1);
      return -1;
    }
  }

  f = &z->frames[frame_idx];

  if (z->read_buf_size < f->decomp_size) {
    char *new_buf = realloc(z->read_buf, f->decomp_size);
    if (new_buf == NULL) {
      dreturn("%i", -1);
      return -1;
    }
    z->read_buf = new_buf;
    z->read_buf_size = f->decomp_size;
  }

  if (z->comp_buf_size < f->comp_size) {
    char *new_buf = realloc(z->comp_buf, f->comp_size);
    if (new_buf == NULL) {
      dreturn("%i", -1);
      return -1;
    }
    z->comp_buf = new_buf;
    z->comp_buf_size = f->comp_size;
  }

  if (gd_PRead(z->fd, z->comp_buf, f->comp_size, (off64_t)f->comp_offset) !=
      (ssize_t)f->comp_size)
  {
    dreturn("%i", -1);
    return -1;
  }

  /* verify this is still the frame the index describes */
  if (_GD_ZstdTagValue(GD_ZSTD_FRAME_TAG(z->comp_buf, f->comp_size)) !=
      f->check)
  {
    dreturn("%i", -1);
    return -1;
  }

  result = ZSTD_decompressDCtx(z->dctx, z->read_buf, f->decomp_size,
      z->comp_buf, f->comp_size);

  if (ZSTD_isError(result)) {
    z->last_error = result;
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

/* Close the current open frame: compress write_buf into out_buf as one
 * frame, write it out, update the frame index, and reset frame_used. */
static int _GD_ZstdEndFrame(struct gd_zstddata *z)
{
  uint64_t frame_comp_start;
  uint32_t decomp_size, comp_size;
  size_t r;

  dtrace("%p", z);

  if (z->frame_used == 0) {
    dreturn("%i", 0);
    return 0;
  }

  decomp_size = z->frame_used;
  frame_comp_start = z->write_offset;

  r = ZSTD_compress2(z->cctx, z->out_buf, z->out_buf_size, z->write_buf,
      decomp_size);
  if (ZSTD_isError(r)) {
    z->last_error = r;
    dreturn("%i", -1);
    return -1;
  }
  comp_size = (uint32_t)r;

  if (gd_PWrite(z->fd, z->out_buf, comp_size, (off64_t)frame_comp_start) !=
      (ssize_t)comp_size)
  {
    dreturn("%i", -1);
    return -1;
  }
  z->write_offset += comp_size;

  if (_GD_ZstdAddIndexEntry(z, frame_comp_start, comp_size, decomp_size,
        _GD_ZstdTagValue(GD_ZSTD_FRAME_TAG(z->out_buf, comp_size))))
  {
    dreturn("%i", -1);
    return -1;
  }

  z->comp_end = z->write_offset;
  z->frame_used = 0;

  dreturn("%i", 0);
  return 0;
}

/* Move the compressed byte range [src, src + len) to src + delta, bouncing
 * through out_buf.  The copy direction is chosen so that overlapping source
 * bytes are always read before they are overwritten. */
static int _GD_ZstdMoveTail(struct gd_zstddata *z, uint64_t src, uint64_t len,
    int64_t delta)
{
  uint64_t done = 0;
  uint64_t at;
  uint32_t chunk;

  if (delta == 0 || len == 0)
    return 0;

  while (done < len) {
    chunk = z->out_buf_size;
    if ((uint64_t)chunk > len - done)
      chunk = (uint32_t)(len - done);

    /* moving down: copy low-to-high; moving up: copy high-to-low */
    at = (delta < 0) ? src + done : src + len - done - chunk;

    if (gd_PRead(z->fd, z->out_buf, chunk, (off64_t)at) != (ssize_t)chunk)
      return -1;
    if (gd_PWrite(z->fd, z->out_buf, chunk, (off64_t)at + delta) !=
        (ssize_t)chunk)
      return -1;

    done += chunk;
  }

  return 0;
}

/* Rewrite committed data in place.  Only the frames overlapping
 * [write_pos, write_pos + data_len) are decompressed, patched, and
 * recompressed; the frames after them are byte-identical compressed images,
 * which are relocated on disk (by the change in the patched frames'
 * compressed size) without recompression.  Frame boundaries are preserved,
 * so the decompressed geometry of the index is unchanged.  Memory use
 * scales with the write span (plus one frame), not with the distance to
 * EOF.  A write extending past EOF appends the excess through the normal
 * streaming path afterwards.
 *
 * Returns the number of samples written, or -1 on error. */
static ssize_t _GD_ZstdRewriteSpan(struct gd_zstddata *z,
    struct gd_raw_file_ *file gd_unused_, uint64_t write_pos,
    const char *data, size_t data_len, size_t sample_size)
{
  int32_t first_frame, last_frame;
  uint64_t write_end, old_end;
  uint64_t patch_point, old_patch_comp, tail_src, tail_len, off;
  int64_t delta;
  char *staged = NULL;
  size_t staged_alloc = 0, staged_used = 0, spos;
  uint32_t i, n_patched;
  uint32_t *new_sizes = NULL;

  dtrace("%p, %p, %" PRIu64 ", %p, %" PRIuSIZE ", %" PRIuSIZE,
      z, file, write_pos, data, data_len, sample_size);

  if (data_len == 0) {
    dreturn("%i", 0);
    return 0;
  }

  old_end = z->total_decomp;
  write_end = write_pos + data_len;

  /* The committed frames overlapping the write.  Callers flush any
   * in-progress frame before calling here, and guarantee
   * write_pos < total_decomp. */
  first_frame = _GD_ZstdLocateFrame(z, write_pos);
  if (first_frame < 0) {
    dreturn("%i", -1);
    return -1;
  }

  if (write_end >= old_end)
    last_frame = (int32_t)z->n_frames - 1;
  else
    last_frame = _GD_ZstdLocateFrame(z, write_end - 1);
  if (last_frame < first_frame) {
    dreturn("%i", -1);
    return -1;
  }
  n_patched = (uint32_t)(last_frame - first_frame + 1);

  new_sizes = malloc(n_patched * sizeof(*new_sizes));
  if (new_sizes == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  /* Stage replacements for the patched frames: decompress each into
   * read_buf, overlay the intersection with the new data, and recompress
   * at the original frame boundary.  read_buf doubles as the patch
   * scratch, so invalidate the read cache up front. */
  z->cached_frame = -1;

  for (i = (uint32_t)first_frame; i <= (uint32_t)last_frame; ++i) {
    const struct gd_zstd_frame *f = &z->frames[i];
    uint64_t lo, hi;
    size_t result, bound;

    if (_GD_ZstdLoadFrame(z, (int32_t)i))
      goto error;

    /* overlay [write_pos, write_end) onto this frame's decompressed range */
    lo = (write_pos > f->decomp_start) ? write_pos : f->decomp_start;
    hi = f->decomp_start + f->decomp_size;
    if (write_end < hi)
      hi = write_end;
    if (lo < hi)
      memcpy(z->read_buf + (lo - f->decomp_start), data + (lo - write_pos),
          (size_t)(hi - lo));

    bound = ZSTD_compressBound(f->decomp_size);
    if (ZSTD_isError(bound))
      goto error;

    if (staged_used + bound > staged_alloc) {
      size_t want = staged_used + bound;
      char *nbuf;

      if (want < staged_alloc * 2)
        want = staged_alloc * 2;
      nbuf = realloc(staged, want);
      if (nbuf == NULL)
        goto error;
      staged = nbuf;
      staged_alloc = want;
    }

    result = ZSTD_compress2(z->cctx, staged + staged_used, bound,
        z->read_buf, f->decomp_size);
    if (ZSTD_isError(result)) {
      z->last_error = result;
      goto error;
    }
    if (result > UINT32_MAX)
      goto error;

    new_sizes[i - (uint32_t)first_frame] = (uint32_t)result;
    staged_used += result;
  }

  patch_point = z->frames[first_frame].comp_offset;
  tail_src = z->frames[last_frame].comp_offset +
    z->frames[last_frame].comp_size;
  old_patch_comp = tail_src - patch_point;
  tail_len = z->comp_end - tail_src;
  delta = (int64_t)staged_used - (int64_t)old_patch_comp;

  /* From here on the file is being mutated: a failure leaves the region at
   * or after patch_point indeterminate, so the error path trims the index
   * back to the untouched prefix. */

  /* Relocate the unchanged compressed tail.  Whichever way it moves, its
   * destination never overlaps [patch_point, patch_point + staged_used),
   * so this happens before the staged frames are written. */
  if (_GD_ZstdMoveTail(z, tail_src, tail_len, delta))
    goto error_disk;

  /* Drop in the staged replacement frames */
  off = patch_point;
  while (off < patch_point + staged_used) {
    uint32_t chunk = (uint32_t)(patch_point + staged_used - off);
    if (chunk > z->out_buf_size)
      chunk = z->out_buf_size;
    if (gd_PWrite(z->fd, staged + (off - patch_point), chunk,
          (off64_t)off) != (ssize_t)chunk)
      goto error_disk;
    off += chunk;
  }

  /* Shrinkage leaves dead bytes past the new end */
  if (delta < 0 &&
      gd_truncate(z->fd, (off64_t)(z->comp_end + (uint64_t)delta)))
  {
    goto error_disk;
  }

  /* Update the index: patched frames get new compressed sizes and identity
   * tags; every frame after them shifts by delta.  Decompressed geometry is
   * unchanged. */
  off = patch_point;
  spos = 0;
  for (i = (uint32_t)first_frame; i <= (uint32_t)last_frame; ++i) {
    const uint32_t nsz = new_sizes[i - (uint32_t)first_frame];
    z->frames[i].comp_offset = off;
    z->frames[i].comp_size = nsz;
    z->frames[i].check =
      _GD_ZstdTagValue(GD_ZSTD_FRAME_TAG(staged + spos, nsz));
    off += nsz;
    spos += nsz;
  }
  for (i = (uint32_t)last_frame + 1; i < z->n_frames; ++i)
    z->frames[i].comp_offset += (uint64_t)delta;

  z->comp_end += (uint64_t)delta;
  z->write_offset = z->comp_end;

  free(staged);
  staged = NULL;
  free(new_sizes);
  new_sizes = NULL;

  /* Append any data extending past the old EOF through the normal
   * streaming path, leaving a trailing partial frame open in write_buf
   * just as a plain append would. */
  if (write_end > old_end) {
    const char *in = data + (old_end - write_pos);
    uint64_t remaining = write_end - old_end;

    while (remaining > 0) {
      uint32_t space = z->frame_size - z->frame_used;
      uint32_t to_feed = (uint32_t)(remaining < space ? remaining : space);

      memcpy(z->write_buf + z->frame_used, in, to_feed);
      z->frame_used += to_feed;
      in += to_feed;
      remaining -= to_feed;

      if (z->frame_used >= z->frame_size && _GD_ZstdEndFrame(z)) {
        dreturn("%i", -1);
        return -1;
      }
    }
  }

  dreturn("%" PRIdSIZE, (ssize_t)(data_len / sample_size));
  return (ssize_t)(data_len / sample_size);

error_disk:
  /* The on-disk suffix is indeterminate; claim only the untouched prefix.
   * The patched frames' decomp_start entries are not yet updated (and a
   * rewrite never changes them anyway), so this is the original value. */
  z->total_decomp = z->frames[first_frame].decomp_start;
  z->n_frames = (uint32_t)first_frame;
  z->comp_end = patch_point;
  z->write_offset = patch_point;
  z->search_hint = 0;
  z->frame_used = 0;

error:
  free(staged);
  free(new_sizes);
  dreturn("%i", -1);
  return -1;
}

/* Parse enc_data: a comma-separated list of key=value pairs, where the keys
 * are "size" (the target uncompressed frame size in bytes) and "level" (the
 * zstd compression level). */
static int _GD_ZstdParseParams(const char *enc_data, uint32_t *frame_size,
    int *comp_level)
{
  const char *ptr = enc_data;
  char *endptr;

  while (*ptr != '\0') {
    if (strncmp(ptr, "size=", 5) == 0) {
      unsigned long val;

      ptr += 5;
      errno = 0;
      val = strtoul(ptr, &endptr, 10);
      if (*ptr < '0' || *ptr > '9' || errno != 0 || val == 0 ||
          val > UINT32_MAX)
        return -1;
      *frame_size = (uint32_t)val;
    } else if (strncmp(ptr, "level=", 6) == 0) {
      long level;

      ptr += 6;
      errno = 0;
      level = strtol(ptr, &endptr, 10);
      if (endptr == ptr || errno != 0 || level < ZSTD_minCLevel() ||
          level > ZSTD_maxCLevel())
        return -1;
      *comp_level = (int)level;
    } else
      return -1;

    if (*endptr == ',' && endptr[1] != '\0')
      ptr = endptr + 1;
    else if (*endptr == '\0')
      ptr = endptr;
    else
      return -1;
  }

  return 0;
}

int _GD_ZstdOpen(int fd, struct gd_raw_file_ *file, const char *enc_data,
    gd_type_t data_type gd_unused_, int swap gd_unused_, unsigned int mode)
{
  struct gd_zstddata *z;
  uint32_t frame_size = GD_ZSTD_FRAME_SIZE;
  int comp_level = GD_ZSTD_COMP_LEVEL;

  dtrace("%i, %p, \"%s\", <unused>, <unused>, 0x%X", fd, file,
      enc_data ? enc_data : "(null)", mode);

  /* Encoding parameters are a comma-separated list of key=value pairs.
   * A malformed parameter fails the open. */
  if (enc_data != NULL && enc_data[0] != '\0' &&
      _GD_ZstdParseParams(enc_data, &frame_size, &comp_level))
  {
    errno = EINVAL;
    dreturn("%i", 1);
    return 1;
  }

  z = calloc(1, sizeof(*z));
  if (z == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  z->fd = -1;
  z->cached_frame = -1;
  z->frame_size = frame_size;

  if (mode & GD_FILE_TEMP) {
    z->fd = _GD_MakeTempFile(file->D, fd, file->name);
  } else if (mode & GD_FILE_WRITE) {
    z->fd = gd_OpenAt(file->D, fd, file->name,
        O_RDWR | O_CREAT | O_BINARY, 0666);
  } else {
    z->fd = gd_OpenAt(file->D, fd, file->name, O_RDONLY | O_BINARY, 0666);
  }

  if (z->fd < 0)
    goto error;

  /* Scan existing frames to build the in-memory index */
  if (_GD_ZstdBuildIndex(z))
    goto error;

  if (mode & GD_FILE_WRITE) {
    size_t bound;
    size_t r;
    gd_stat64_t statbuf;

    /* Discard anything after the last parseable frame: it is already
     * unreachable (the scan stopped there, and appends land at comp_end),
     * and leaving it risks a later scan misparsing the residue of an
     * interrupted write as a frame. */
    if (gd_fstat64(z->fd, &statbuf) ||
        ((uint64_t)statbuf.st_size > z->comp_end &&
         gd_truncate(z->fd, (off64_t)z->comp_end)))
    {
      goto error;
    }

    z->cctx = ZSTD_createCCtx();
    if (z->cctx == NULL)
      goto error;

    /* Set persistent parameters once; they apply to every frame */
    r = ZSTD_CCtx_setParameter(z->cctx, ZSTD_c_compressionLevel, comp_level);
    /* Checksummed frames detect corruption at read time, for 4 bytes/frame */
    if (!ZSTD_isError(r))
      r = ZSTD_CCtx_setParameter(z->cctx, ZSTD_c_checksumFlag, 1);
    if (ZSTD_isError(r))
      goto error;

    z->write_buf = malloc(frame_size);
    if (z->write_buf == NULL)
      goto error;

    bound = ZSTD_compressBound(frame_size);
    if (ZSTD_isError(bound) || bound > UINT32_MAX)
      goto error;
    z->out_buf_size = (uint32_t)bound;
    z->out_buf = malloc(z->out_buf_size);
    if (z->out_buf == NULL)
      goto error;

    /* Append after all existing frames (and any trailing skippable frame) */
    z->write_offset = z->comp_end;
  }

  if (mode & GD_FILE_READ) {
    z->dctx = ZSTD_createDCtx();
    if (z->dctx == NULL)
      goto error;
  }

  file->edata = z;
  file->idata = z->fd;
  file->mode = mode;
  file->pos = 0;

  dreturn("%i", 0);
  return 0;

error:
  if (z->cctx)
    ZSTD_freeCCtx(z->cctx);
  if (z->dctx)
    ZSTD_freeDCtx(z->dctx);
  free(z->write_buf);
  free(z->out_buf);
  free(z->comp_buf);
  free(z->frames);
  if (z->fd >= 0)
    close(z->fd);
  free(z);
  dreturn("%i", 1);
  return 1;
}

off64_t _GD_ZstdSeek(struct gd_raw_file_ *file, off64_t sample,
    gd_type_t data_type gd_unused_, unsigned int mode gd_unused_)
{
  dtrace("%p, %" PRId64 ", <unused>, <unused>", file, (int64_t)sample);

  file->pos = sample;

  dreturn("%" PRId64, (int64_t)sample);
  return sample;
}

ssize_t _GD_ZstdRead(struct gd_raw_file_ *restrict file, void *restrict ptr,
    gd_type_t data_type, size_t nelem)
{
  struct gd_zstddata *z = (struct gd_zstddata *)file->edata;
  const size_t sample_size = GD_SIZE(data_type);
  uint64_t byte_offset = (uint64_t)file->pos * sample_size;
  size_t bytes_remaining = nelem * sample_size;
  char *out = (char *)ptr;
  size_t bytes_read = 0;
  int extended = 0;
  int rescanned = 0;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nelem);

  while (bytes_remaining > 0) {
    int32_t frame_idx;
    uint32_t offset_in_frame, avail, to_copy;

    frame_idx = _GD_ZstdLocateFrame(z, byte_offset);
    if (frame_idx < 0) {
      /* Reading past the indexed frames: another process may have appended
       * data since the index was built.  Writers keep their own index
       * current, so only extend read-only handles, and only once per call. */
      if (!(file->mode & GD_FILE_WRITE) && !extended) {
        extended = 1;
        if (_GD_ZstdUpdateIndex(z) == 0)
          frame_idx = _GD_ZstdLocateFrame(z, byte_offset);
      }

      /* Still missing.  If the index no longer describes the file (an
       * in-place rewrite that grew the file leaves the extension above
       * stuck on mid-frame bytes), rebuild it and restart the read. */
      if (frame_idx < 0 && !(file->mode & GD_FILE_WRITE) && !rescanned &&
          _GD_ZstdIndexStale(z))
      {
        rescanned = 1;
        if (_GD_ZstdRecoverIndex(z, (int32_t)z->n_frames - 1) == 0) {
          out = (char *)ptr;
          byte_offset = (uint64_t)file->pos * sample_size;
          bytes_remaining = nelem * sample_size;
          bytes_read = 0;
          continue;
        }
      }
      if (frame_idx < 0)
        break;
    }

    offset_in_frame =
      (uint32_t)(byte_offset - z->frames[frame_idx].decomp_start);
    avail = z->frames[frame_idx].decomp_size - offset_in_frame;
    to_copy = (uint32_t)(bytes_remaining < avail ? bytes_remaining : avail);

    /* A writer's own cache is authoritative; a reader's must be checked
     * against the file, which someone else may have rewritten in place */
    if (z->cached_frame == frame_idx && !(file->mode & GD_FILE_WRITE)) {
      const struct gd_zstd_frame *f = &z->frames[frame_idx];
      uint32_t tag;

      if (_GD_ZstdReadTag(z, f->comp_offset, f->comp_size, &tag) ||
          tag != f->check)
      {
        z->cached_frame = -1;
      }
    }

    if (z->cached_frame != frame_idx) {
      if (_GD_ZstdLoadFrame(z, frame_idx)) {
        /* A frame that should decompress didn't.  On a read-only handle
         * this can mean a writer has rewritten the file in place behind us,
         * shifting every frame after the rewrite point and leaving our
         * index stale; rebuild the index and restart the read.  Only once
         * per call: a repeat failure is corruption, not end-of-data, so
         * fail the read even if it made partial progress. */
        if (!(file->mode & GD_FILE_WRITE) && !rescanned) {
          rescanned = 1;
          if (_GD_ZstdRecoverIndex(z, frame_idx) == 0) {
            out = (char *)ptr;
            byte_offset = (uint64_t)file->pos * sample_size;
            bytes_remaining = nelem * sample_size;
            bytes_read = 0;
            continue;
          }
        }
        dreturn("%i", -1);
        return -1;
      }
      z->cached_frame = frame_idx;
    }

    memcpy(out, z->read_buf + offset_in_frame, to_copy);
    out += to_copy;
    byte_offset += to_copy;
    bytes_remaining -= to_copy;
    bytes_read += to_copy;
  }

  /* Serve data from the unflushed write buffer if the read extends past
   * all committed frames */
  if (bytes_remaining > 0 && z->write_buf != NULL && z->frame_used > 0) {
    uint64_t wb_start = z->total_decomp;
    uint64_t wb_end   = wb_start + z->frame_used;

    if (byte_offset >= wb_start && byte_offset < wb_end) {
      uint32_t wb_off  = (uint32_t)(byte_offset - wb_start);
      uint32_t wb_avail = z->frame_used - wb_off;
      uint32_t to_copy  = (uint32_t)(bytes_remaining < wb_avail ?
          bytes_remaining : wb_avail);

      memcpy(out, z->write_buf + wb_off, to_copy);
      out += to_copy;
      byte_offset += to_copy;
      bytes_remaining -= to_copy;
      bytes_read += to_copy;
    }
  }

  file->pos += (off64_t)(bytes_read / sample_size);

  dreturn("%" PRIdSIZE, (ssize_t)(bytes_read / sample_size));
  return (ssize_t)(bytes_read / sample_size);
}

ssize_t _GD_ZstdWrite(struct gd_raw_file_ *restrict file,
    const void *restrict ptr, gd_type_t data_type, size_t nelem)
{
  struct gd_zstddata *z = (struct gd_zstddata *)file->edata;
  const size_t sample_size = GD_SIZE(data_type);
  const char *in = (const char *)ptr;
  size_t bytes_remaining = nelem * sample_size;
  size_t total_written = 0;
  uint64_t write_pos;
  uint64_t current_end;
  ssize_t ret;

  dtrace("%p, %p, 0x%X, %" PRIuSIZE, file, ptr, data_type, nelem);

  /* Compute the byte position for this write */
  write_pos = (uint64_t)file->pos * sample_size;
  current_end = z->total_decomp + z->frame_used;

  /* A write into committed frames: commit the open frame (a no-op when it
   * is empty) so the whole target range is described by the frame index,
   * then rewrite in place. */
  if (write_pos < z->total_decomp) {
    if (_GD_ZstdEndFrame(z)) {
      dreturn("%i", -1);
      return -1;
    }
    ret = _GD_ZstdRewriteSpan(z, file, write_pos, in, bytes_remaining,
          sample_size);
    if (ret >= 0)
      file->pos += ret;
    dreturn("%" PRIdSIZE, ret);
    return ret;
  }

  /* A write starting inside the frame under construction patches write_buf
   * in memory, keeping the frame open; anything extending past the frame
   * continues through the append path below. */
  if (write_pos < current_end) {
    uint32_t wb_off = (uint32_t)(write_pos - z->total_decomp);
    uint32_t space = z->frame_size - wb_off;
    uint32_t to_feed = (uint32_t)(bytes_remaining < space ?
        bytes_remaining : space);

    memcpy(z->write_buf + wb_off, in, to_feed);
    if (wb_off + to_feed > z->frame_used)
      z->frame_used = wb_off + to_feed;

    in += to_feed;
    bytes_remaining -= to_feed;
    total_written += to_feed;

    if (z->frame_used >= z->frame_size && _GD_ZstdEndFrame(z)) {
      dreturn("%i", -1);
      return -1;
    }
  }

  /* Zero-fill any gap between current end and write_pos */
  if (current_end < write_pos) {
    uint64_t gap = write_pos - current_end;

    static const char zeros[4096];
    while (gap > 0) {
      uint32_t space = z->frame_size - z->frame_used;
      uint32_t to_zero = (uint32_t)(gap < space ? gap : space);
      if (to_zero > sizeof(zeros))
        to_zero = sizeof(zeros);

      memcpy(z->write_buf + z->frame_used, zeros, to_zero);
      z->frame_used += to_zero;

      if (z->frame_used >= z->frame_size && _GD_ZstdEndFrame(z)) {
        dreturn("%i", -1);
        return -1;
      }

      gap -= to_zero;
    }
  }

  /* Append new data past current end, breaking across frame boundaries */
  while (bytes_remaining > 0) {
    uint32_t space = z->frame_size - z->frame_used;
    uint32_t to_feed = (uint32_t)(bytes_remaining < space ?
        bytes_remaining : space);

    memcpy(z->write_buf + z->frame_used, in, to_feed);
    z->frame_used += to_feed;

    in += to_feed;
    bytes_remaining -= to_feed;
    total_written += to_feed;

    if (z->frame_used >= z->frame_size && _GD_ZstdEndFrame(z)) {
      dreturn("%i", -1);
      return -1;
    }
  }

  file->pos += (off64_t)(total_written / sample_size);

  dreturn("%" PRIdSIZE, (ssize_t)(total_written / sample_size));
  return (ssize_t)(total_written / sample_size);
}

int _GD_ZstdSync(struct gd_raw_file_ *file)
{
  struct gd_zstddata *z = (struct gd_zstddata *)file->edata;

  dtrace("%p", file);

  /* Flush any partial open frame */
  if (z->frame_used > 0) {
    if (_GD_ZstdEndFrame(z)) {
      dreturn("%i", 1);
      return 1;
    }
  }

  if (fsync(z->fd)) {
    dreturn("%i", 1);
    return 1;
  }

  dreturn("%i", 0);
  return 0;
}

int _GD_ZstdClose(struct gd_raw_file_ *file)
{
  struct gd_zstddata *z = (struct gd_zstddata *)file->edata;
  int ret = 0;

  dtrace("%p", file);

  if (file->mode & GD_FILE_WRITE) {
    if (z->frame_used > 0) {
      if (_GD_ZstdEndFrame(z))
        ret = 1;
    }
  }

  if (z->cctx)
    ZSTD_freeCCtx(z->cctx);
  if (z->dctx)
    ZSTD_freeDCtx(z->dctx);

  free(z->write_buf);
  free(z->out_buf);
  free(z->read_buf);
  free(z->comp_buf);
  free(z->frames);

  if (close(z->fd))
    ret = 1;

  free(z);

  file->idata = -1;
  file->edata = NULL;
  file->mode = 0;

  dreturn("%i", ret);
  return ret;
}

off64_t _GD_ZstdSize(int dirfd, struct gd_raw_file_ *file,
    gd_type_t data_type, int swap gd_unused_)
{
  struct gd_zstddata z;
  off64_t size;
  int fd;

  dtrace("%i, %p, 0x%X, <unused>", dirfd, file, data_type);

  /* If the field is open, reuse its index rather than rescanning the whole
   * file.  A writer's index is authoritative; a reader's may trail a
   * concurrent writer, so pick up any newly-appended frames first. */
  if (file->idata >= 0 && file->edata != NULL) {
    struct gd_zstddata *zo = (struct gd_zstddata *)file->edata;

    if (!(file->mode & GD_FILE_WRITE)) {
      /* refresh a reader's index; rebuild it outright if a rewrite has
       * invalidated it */
      if (_GD_ZstdIndexStale(zo)
          ? _GD_ZstdRecoverIndex(zo, (int32_t)zo->n_frames - 1)
          : _GD_ZstdUpdateIndex(zo))
      {
        dreturn("%i", -1);
        return -1;
      }
    }

    size = (off64_t)((zo->total_decomp + zo->frame_used) /
        GD_SIZE(data_type));

    dreturn("%" PRId64, (int64_t)size);
    return size;
  }

  fd = gd_OpenAt(file->D, dirfd, file->name, O_RDONLY | O_BINARY, 0666);
  if (fd < 0) {
    dreturn("%i", -1);
    return -1;
  }

  memset(&z, 0, sizeof(z));
  z.fd = fd;
  z.cached_frame = -1;

  if (_GD_ZstdBuildIndex(&z)) {
    free(z.comp_buf);
    free(z.frames);
    close(fd);
    dreturn("%i", -1);
    return -1;
  }

  size = (off64_t)(z.total_decomp / GD_SIZE(data_type));

  free(z.comp_buf);
  free(z.frames);
  close(fd);

  dreturn("%" PRId64, (int64_t)size);
  return size;
}

int _GD_ZstdStrerr(const struct gd_raw_file_ *file, char *buf, size_t buflen)
{
  const struct gd_zstddata *z;
  int r = 0;

  dtrace("%p, %p, %" PRIuSIZE, file, buf, buflen);

  if (file->edata) {
    z = (const struct gd_zstddata *)file->edata;
    if (ZSTD_isError(z->last_error)) {
      const char *msg = ZSTD_getErrorName(z->last_error);
      strncpy(buf, msg, buflen);
      buf[buflen - 1] = 0;
    } else {
      r = gd_StrError(errno, buf, buflen);
    }
  } else {
    r = gd_StrError(errno, buf, buflen);
  }

  dreturn("%i", r);
  return r;
}
