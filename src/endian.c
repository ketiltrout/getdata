/* Copyright (C) 2008-2011 D. V. Wiebe
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

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#endif

static void _GD_ByteSwapFragment(DIRFILE* D, unsigned long byte_sex,
    int fragment, int move)
{
  unsigned int i, n_raw = 0;

  dtrace("%p, %lx, %i, %i", D, (unsigned long)byte_sex, fragment, move);

  byte_sex = (
#ifdef WORDS_BIGENDIAN
    (byte_sex & GD_LITTLE_ENDIAN) ? GD_LITTLE_ENDIAN : GD_BIG_ENDIAN
#else
    (byte_sex & GD_BIG_ENDIAN) ? GD_BIG_ENDIAN : GD_LITTLE_ENDIAN
#endif
    ) | (byte_sex & GD_ARM_FLAG);

  /* check protection */
  if (D->fragment[fragment].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment].cname);
    dreturnvoid();
    return;
  }

  if (move && byte_sex != D->fragment[fragment].byte_sex) {
    gd_entry_t **raw_entry = (gd_entry_t **)malloc(sizeof(gd_entry_t*) *
        D->n_entries);

    if (raw_entry == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturnvoid();
      return;
    }

    /* Because it may fail, the move must occur out-of-place and then be copied
     * back over the affected files once success is assured */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        /* if the field's data type is one byte long, do nothing */
        if (D->entry[i]->e->u.raw.size == 1)
          continue;

        /* check subencoding support */
        if (!_GD_Supports(D, D->entry[i], GD_EF_TMOVE | GD_EF_TUNLINK))
          break;

        /* add this raw field to the list */
        raw_entry[n_raw++] = D->entry[i];

        if (_GD_MogrifyFile(D, D->entry[i],
              D->fragment[D->entry[i]->fragment_index].encoding, byte_sex,
              D->fragment[D->entry[i]->fragment_index].frame_offset, 0, -1,
              NULL))
          break;
      }

    /* If successful, move the temporary file over the old file, otherwise
     * remove the temporary files */
    if (D->error) {
      for (i = 0; i < n_raw; ++i)
        if ((*_gd_ef[raw_entry[i]->e->u.raw.file[0].subenc].tunlink)(
              D->fragment[fragment].dirfd, raw_entry[i]->e->u.raw.file + 1))
        {
          _GD_SetError(D, GD_E_RAW_IO, 0, raw_entry[i]->e->u.raw.file[0].name,
              errno, NULL);
        }
    } else {
      for (i = 0; i < n_raw; ++i)
        if ((*_gd_ef[raw_entry[i]->e->u.raw.file[0].subenc].tmove)(
              D->fragment[fragment].dirfd, D->fragment[fragment].dirfd,
              raw_entry[i]->e->u.raw.file,
              _gd_ef[raw_entry[i]->e->u.raw.file[0].subenc].tunlink))
        {
          _GD_SetError(D, GD_E_UNCLEAN_DB, 0,
              D->fragment[D->entry[i]->fragment_index].cname, 0, NULL);
          D->flags |= GD_INVALID;
        }
    }

    free(raw_entry);

    if (D->error) {
      dreturnvoid();
      return;
    }
  }

  D->fragment[fragment].byte_sex = byte_sex;
  D->fragment[fragment].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  dreturnvoid();
}

int gd_alter_endianness(DIRFILE* D, unsigned long byte_sex, int fragment,
    int move)
{
  int i;

  dtrace("%p, %lx, %i, %i", D, (unsigned long)byte_sex, fragment, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (byte_sex != GD_BIG_ENDIAN && byte_sex != GD_LITTLE_ENDIAN) {
    _GD_SetError(D, GD_E_ARGUMENT, GD_E_ARG_ENDIANNESS, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i) {
      _GD_ByteSwapFragment(D, byte_sex, i, move);

      if (D->error)
        break;
    }
  } else
    _GD_ByteSwapFragment(D, byte_sex, fragment, move);

  dreturn("%i", (D->error) ? -1 : 0);
  return (D->error) ? -1 : 0;
}

unsigned long gd_endianness(DIRFILE* D, int fragment) gd_nothrow
{
  dtrace("%p, %i", D, fragment);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if (fragment < 0 || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("0x%lx", (unsigned long)D->fragment[fragment].byte_sex);
  return D->fragment[fragment].byte_sex;
}

void _GD_ArmEndianise(uint64_t* databuffer, int is_complex, size_t ns)
{
  uint64_t *p;
  dtrace("%p, %i, %zi", databuffer, is_complex, ns);

  if (is_complex)
    ns *= 2;

  for (p = databuffer; p < databuffer + ns; ++p)
    *p = ((*p & 0xffffffff) << 32) | ((*p & 0xffffffff00000000ULL) >> 32);

  dreturnvoid();
}

void _GD_FixEndianness(void* databuffer, size_t size, size_t ns)
{
  size_t i;

  dtrace("%p, %zu, %zu", databuffer, size, ns);

  switch (size) {
    case 2:
      for (i = 0; i < ns; ++i)
        ((uint16_t*)databuffer)[i] = gd_swap16(((uint16_t*)databuffer)[i]);
      break;
    case 4:
      for (i = 0; i < ns; ++i)
        ((uint32_t*)databuffer)[i] = gd_swap32(((uint32_t*)databuffer)[i]);
      break;
    case 8:
      for (i = 0; i < ns; ++i)
        ((uint64_t*)databuffer)[i] = gd_swap64(((uint64_t*)databuffer)[i]);
      break;
  }

  dreturnvoid();
}
