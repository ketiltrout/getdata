/* Copyright (C) 2008-2011, 2013, 2014, 2016 D. V. Wiebe
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
    gd_entry_t **raw_entry = (gd_entry_t **)_GD_Malloc(D, sizeof(gd_entry_t*) *
        D->n_entries);

    if (raw_entry == NULL) {
      dreturnvoid();
      return;
    }

    /* Because it may fail, the move must occur out-of-place and then be copied
     * back over the affected files once success is assured */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        /* determine encoding scheme */
        if (!_GD_Supports(D, D->entry[i], 0))
          break;

        /* if the field's data type is one byte long, and no in-framework
         * byte-swapping is performed, do nothing */
        if (D->entry[i]->e->u.raw.size == 1 &&
            !(_GD_ef[D->entry[i]->e->u.raw.file[0].subenc].flags & GD_EF_SWAP))
          continue;

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
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_DISCARD |
            GD_FINIRAW_CLOTEMP);
    } else {
      for (i = 0; i < n_raw; ++i)
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_KEEP |
            GD_FINIRAW_CLOTEMP);
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

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) != GD_RDWR) 
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment)
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
  else if (byte_sex != GD_BIG_ENDIAN && byte_sex != GD_LITTLE_ENDIAN)
    _GD_SetError(D, GD_E_ARGUMENT, GD_E_ARG_ENDIANNESS, NULL, 0, NULL);
  else if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i) {
      _GD_ByteSwapFragment(D, byte_sex, i, move);

      if (D->error)
        break;
    }
  } else
    _GD_ByteSwapFragment(D, byte_sex, fragment, move);

  GD_RETURN_ERROR(D);
}

unsigned long gd_endianness(DIRFILE* D, int fragment) gd_nothrow
{
  dtrace("%p, %i", D, fragment);

  GD_RETURN_IF_INVALID(D, "%i", 0);

  if (fragment < 0 || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("0x%lx", (unsigned long)D->fragment[fragment].byte_sex);
  return D->fragment[fragment].byte_sex;
}

static void _GD_ArmEndianise(uint64_t* databuffer, size_t ns)
{
  uint64_t *p;
  dtrace("%p, %zi", databuffer, ns);

  for (p = databuffer; p < databuffer + ns; ++p)
    *p = ((*p & 0xffffffff) << 32) | ((*p & 0xffffffff00000000ULL) >> 32);

  dreturnvoid();
}

/* determine byte sex flags for the machine endianness */
#ifdef FLOATS_BIGENDIAN
#define  GD_FLOAT_SEX     GD_BIG_ENDIAN
#else
#define  GD_FLOAT_SEX     GD_LITTLE_ENDIAN
#endif

#ifdef WORDS_BIGENDIAN
#define  GD_INT_SEX     GD_BIG_ENDIAN
#else
#define  GD_INT_SEX     GD_LITTLE_ENDIAN
#endif

/* returns non-zero if sex1 and sex2 imply byte sex correction is required, and
 * sets *arm_fix if middle-ended double correction is needed; returns 
 */
int _GD_CheckByteSex(gd_type_t type, unsigned sex1, unsigned sex2,
    int skip_bytes, int *restrict arm_fix)
{
  int endian_fix = 0;

  dtrace("0x%X, 0x%X, 0x%X, %p", type, sex1, sex2, arm_fix);

  /* the trivial case */
  if (GD_SIZE(type) < 1 || (skip_bytes && GD_SIZE(type) == 1)) {
    if (arm_fix)
      *arm_fix = 0;
    dreturn("%i/%i", 0, 0);
    return 0;
  }

  /* ensure we have exactly one of GD_BIG_ENDIAN or GD_LITTLE_ENDIAN set in
   * both bitfields */
  if (type & (GD_IEEE754 | GD_COMPLEX)) {
    /* arm check */
    if (arm_fix) {
      if (type == GD_FLOAT64 || type == GD_COMPLEX128)
        *arm_fix = ((sex1 & GD_ARM_FLAG) != (sex2 & GD_ARM_FLAG));
      else
        *arm_fix = 0;
    }

    switch (sex1 & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)) {
      case 0:
        sex1 |= GD_FLOAT_SEX;
        break;
      case GD_LITTLE_ENDIAN | GD_BIG_ENDIAN:
        sex1 &= ~GD_FLOAT_SEX;
        break;
      default:
        break; /* bits are okay */
    }
    switch (sex2 & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)) {
      case 0:
        sex2 |= GD_FLOAT_SEX;
        break;
      case GD_LITTLE_ENDIAN | GD_BIG_ENDIAN:
        sex2 &= ~GD_FLOAT_SEX;
        break;
      default:
        break; /* bits are okay */
    }
  } else {
    if (arm_fix)
      *arm_fix = 0;

    switch (sex1 & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)) {
      case 0:
        sex1 |= GD_INT_SEX;
        break;
      case GD_LITTLE_ENDIAN | GD_BIG_ENDIAN:
        sex1 &= ~GD_INT_SEX;
        break;
      default:
        break; /* bits are okay */
    }
    switch (sex2 & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)) {
      case 0:
        sex2 |= GD_INT_SEX;
        break;
      case GD_LITTLE_ENDIAN | GD_BIG_ENDIAN:
        sex2 &= ~GD_INT_SEX;
        break;
      default:
        break; /* bits are okay */
    }
  }

  /* endianness check */
  endian_fix = ((sex1 & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)) != 
    (sex2 & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)));

  dreturn("%i/%i", endian_fix, arm_fix ? *arm_fix : -1);
  return endian_fix;
}

/* returns non-zero if the byte sex of RAW entry E is different than the native
 * machine endianness */
int _GD_FileSwapBytes(const DIRFILE *restrict D, const gd_entry_t *restrict E)
{
  int swap;

  dtrace("%p, %p", D, E);

  swap = _GD_CheckByteSex(E->EN(raw,data_type),
      D->fragment[E->fragment_index].byte_sex, 0, 0, NULL);

  dreturn("%i", swap);
  return swap;
}

void _GD_FixEndianness(void* databuffer, size_t ns, gd_type_t type, unsigned
    old_sex, unsigned new_sex)
{
  size_t i;
  int endian_fix, arm_fix;

  dtrace("%p, %" PRIuSIZE ", 0x%X, 0x%X, 0x%X", databuffer, ns, type, old_sex,
      new_sex);

  /* compare byte sexes */
  endian_fix = _GD_CheckByteSex(type, old_sex, new_sex, 1, &arm_fix);
  
  /* complex data - treat as twice as many floating point */
  if (type & GD_COMPLEX) {
    ns *= 2;
    type = (GD_SIZE(type) >> 1) | GD_IEEE754;
  }

  if (arm_fix)
    _GD_ArmEndianise(databuffer, ns);

  if (endian_fix)
    switch (GD_SIZE(type)) {
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
