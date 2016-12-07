/* Copyright (C) 2008, 2010-2016 D. V. Wiebe
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

/* Given str with length len, find the last '.' and last '/'.  Points *dot
 * and *slash to the character in question, or NULL if not found.  Also detects
 * a representation suffix.  Returns the suffix (cast to an int) or 0 if no
 * suffix was found.  This functions never fails.
 *
 * flags are _GD_CodeOffset flags.  Specifically:
 *
 * GD_CO_EARLY:  (indicating DSV 5 or earlier) dots is always NULL and
 *               no representation suffix (return value always zero)
 * GD_CO_NAME:   no representation suffix (return value always zero)
 * GD_CO_REPRZ:  '.z' is a valid representation suffix (ie. DSV 10 or later)
 */
int _GD_SlashDot(const char *str, size_t len, unsigned flags, const char **dot,
    const char **slash)
{
  const char *ptr;
  int repr = 0;
  const int early = flags & GD_CO_EARLY;

  dtrace("%p(\"%s\"), %" PRIuSIZE ", 0x%X, %p, %p", str, str, len, flags, dot,
      slash);

  /* Record a representation suffix -- not supported by early standards
   * and not allowed in a field name */
  if (!(flags & (GD_CO_EARLY | GD_CO_NAME)) && len > 2 && str[len - 2] == '.')
    if (str[len - 1] == 'r' ||
        str[len - 1] == 'i' ||
        str[len - 1] == 'm' ||
        str[len - 1] == 'a' ||
        (flags & GD_CO_REPRZ && str[len - 1] == 'z'))
    {
      repr = str[len - 1];
      len -= 2;
    }
  
  *dot = *slash = NULL;
  for (ptr = str + len - 1; ptr >= str; --ptr) {
    if (!early && *dot == NULL && *ptr == '.')
        *dot = ptr;

    if (*slash == NULL && *ptr == '/') {
      /* If dot is already non-NULL, then there's either:
       *
       *   Situation 1: a dot in the subfield name, or else:
       *   Situation 2: a slash in the namespace.
       *
       * Neither of these has ever been allowed by the standards: slash has
       * always been a prohibited character; dot was allowed until DSV 5, but
       * metafields weren't introduced until DSV 6.
       *
       * We assume we're in the the former situation.  It's "somewhat less" of
       * a syntax error.)  So, forget the dot(s) seen earlier.
       */
      if (*dot)
        *dot = NULL;
      *slash = ptr;
    }
    if (*slash && (early || *dot))
      break;
  }

  dreturn("0x%02X (%p, %p)", repr, *dot, *slash);
  return repr;
}

/* This function computes the offset of the parts of a field code, as well
 * as, optionally, verifying that the right bits are there. This is performed
 * in the context of a particular fragment.  For a fragment F (=index), with
 * parent P, a field code has this form:
 *
 *     AAAA.BBBB.CCCC.DDDDEEEEGGGGHHHHIIII/KKKK.M0
 *     ^    ^    ^    ^   ^   ^   ^   ^   ^    ^^
 *     0    b    c    d   e   g   h   i   k    lm
 *
 * where:
 *
 * AAAA.  is P's root namespace
 * BBBB.  is F's local rootspace (in F's /INCLUDE statement)
 * CCCC.  is the subspace under F's rootspace
 * DDDD   is P's prefix
 * EEEE   is F's local prefix (the part in F's /INCLUDE statement)
 * GGGG   is the field name (or the parent's name for a metafield).  If
 *        this field is defined in a subfragment, this may include more affixes.
 *        This is the only required part.
 * HHHH   is F's local suffix (again the part in F's /INCLUDE statement)
 * IIII   is P's suffix
 * /KKKK  is the subfield's name, if applicable
 * .M     is the representation suffix, if any.
 * 0      is the terminating NUL byte
 *
 * If successful, the function fills the passed offset[] with the following:
 *   offset[0] = b: the start of F's namespace.  If AAAA. is missing, this
 *                  is zero.
 *   offset[1] = c: the character after F's namespace.  If BBBB. is missing,
 *                  this equals b.
 *   offset[2] = d: the first character after all the namespaces.  If CCCC. is
 *                  empty, this equals c.
 *   offset[3] = e: the start of F's local prefix.  If DDDD is empty,
 *                  this equals d
 *   offset[4] = g: the character after F's local prefix.  If EEEE is empty,
 *                  this equals e
 *   offset[5] = h: the start of F's local suffix.  If HHHH is empty, this
 *                  equals i.
 *   offset[6] = i: the character after F's local suffix.  If IIII is empty,
 *                  this equals k.
 *   offset[7] = k: the character after the last suffix.  If /KKKK is present,
 *                  this points to the '/' character, otherwise this equals l.
 *   offset[8] = l: the character after the whole field name.  If there is a
 *                  representation suffix, this points to the '.' character.
 *                  If there's no representation suffix, this equals m.
 *   offset[9] = m: is strlen(code);
 *
 * Flags used are:
 *
 * GD_CO_NAME, GD_CO_RERPZ: passed on to _GD_SlashDot (q.v.)
 * GD_CO_EARLY:             the code came from DSV <= 5: no namespaces
 * GD_CO_CHECK:             check that the fragment affixes are present
 *
 * This function returns 0 if the checks all pass (or weren't run); 1 otherwise
 */
int _GD_CodeOffsets(DIRFILE *D, int index, const char *code, unsigned flags,
    size_t offset[GD_N_CODEOFFSETS])
{
  int ret = 0, repr;
  const char *dot = NULL, *slash = NULL;
  const struct gd_fragment_t *F, *P;

  dtrace("%p, %i, \"%s\", 0x%X, %p", D, index, code, flags, offset);

  F = D->fragment + index;
  if (index == 0)
    P = D->fragment + 0;
  else
    P = D->fragment + F->parent;

  offset[9] = strlen(code);

  if (flags & GD_CO_EARLY)
    offset[0] = offset[1] = 0;
  else {
    offset[0] = P->nsl; /* Length of parent namespace */
    if (offset[0])
      offset[0]++; /* for the trailing '.' */

    offset[1] = F->nsl; /* Length of F's local namespace */
    if (offset[1])
      offset[1]++; /* Again, for the trailing '.' */
  }

  /* Find the last dot and the last slash */
  repr = _GD_SlashDot(code + offset[1], offset[9] - offset[1], flags, &dot,
      &slash);

  if (repr)
    offset[8] = offset[9] - 2;
  else
    offset[8] = offset[9];

  if (dot == NULL) /* No dot (i.e. AAAA. is missing) */
    offset[2] = offset[1];
  else
    offset[2] = dot - code + 1;

  offset[3] = offset[2] + P->pxl;
  offset[4] = offset[2] + F->pxl;

  if (slash == NULL) /* Not a subfield */
    offset[7] = offset[8];
  else
    offset[7] = slash - code;

  offset[6] = offset[7] - P->sxl;
  offset[5] = offset[7] - F->sxl;

  if (flags & GD_CO_CHECK) {
    /* Namespace present */
    if (F->nsl && strncmp(code, F->ns, F->nsl))
      ret = 1;
    /* Namespace separated by a dot */
    else if (offset[1] && code[offset[1] - 1] != '.')
      ret = 2;
    /* Prefix present */
    else if (F->pxl && strncmp(code + offset[2], F->px, F->pxl))
      ret = 3;
    /* Suffix present */
    else if (F->sxl && strncmp(code + offset[7] - F->sxl, F->sx, F->sxl))
      ret = 4;
  }

  dreturn("%i (%" PRIuSIZE ", %" PRIuSIZE ", %" PRIuSIZE ", %" PRIuSIZE ", %"
      PRIuSIZE ", %" PRIuSIZE ", %" PRIuSIZE ", %" PRIuSIZE ", %" PRIuSIZE ", %"
      PRIuSIZE ")", ret, offset[0], offset[1], offset[2], offset[3], offset[4],
      offset[5], offset[6], offset[7], offset[8], offset[9]);

  return ret;
}

/* Strips affixes (and, optionally, namespaces) from a code.  Returns a
 * newly malloc'd string, or NULL on error.  Also usually strips the
 * representation suffix.
 *
 * Flags are:
 *
 * GD_CO_NSALL:  remove all namespaces
 * GD_CO_NSROOT: remove only index's root namespace
 * GD_CO_ASSERT: if the code doesn't have the right parts, raise
 *                  GD_E_INTERNAL_ERROR
 * GD_CO_CHECK:  if the code doesn't have the right parts, raise
 *                  GD_E_BAD_CODE
 * GD_CO_EARLY:  field code comes from DSV <= 5: '.' doesn't denote namespace
 * GD_CO_REPR:   don't strip the representation suffix.  Also, if the code is
 *               ambiguous (name.r), add a disambiguating ".z" to it.
 * GD_CO_REPRZ:  '.z' is a valid representation suffix (ie. DSV 10 or later)
 */
char *_GD_StripCode(DIRFILE *D, int index, const char *code, unsigned flags)
{
  size_t ns_start, len, offset[GD_N_CODEOFFSETS];
  char add_repr = 0;
  char *stripped = NULL, *ptr;

  dtrace("%p, %i, \"%s\", 0x%X", D, index, code, flags);

  if (flags & GD_CO_ASSERT)
    flags |= GD_CO_CHECK;

  /* Slice and dice */
  if (_GD_CodeOffsets(D, index, code, flags, offset)) {
    if (flags & GD_CO_ASSERT)
      _GD_InternalError(D);
    else
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, code);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Namespace start depends on flags */
  if (flags & GD_CO_NSALL) /* strip all namespaces */
    ns_start = offset[2]; 
  else if (flags & GD_CO_NSROOT) /* strip fragment rootspace */
    ns_start = offset[1];
  else /* Strip no namespace */
    ns_start = 0;

  /* Calculate length */
  len = offset[2] - ns_start + offset[5] - offset[4] /* NS part + BBBB */
    + offset[8] - offset[7]; /* subfield name (including '/') */

  /* Figure out if this code needs a representation suffix */
  if (flags & GD_CO_REPR) {
    if (offset[8] != offset[9] && code[offset[9] - 1] != 'z') {
      /* we have a "do something" repr, remember it */
      add_repr = code[offset[9] - 1];
    } else if (flags & GD_CO_REPRZ) {
      size_t namelen;
      char name0;
      /* repr is currently none (either absent or .z).  Is this code
       * ambiguous?  Look for a single-character name that is 'r' or 'i'
       * or 'a' or 'm' */

      if (offset[8] > offset[7]) { /* check subfield name */
        name0 = code[offset[7] + 1];
        namelen = offset[8] - offset[7] - 1;
      } else { /* top-level name */
        name0 = code[offset[4]];
        namelen = offset[5] - offset[4];
      }

      if (namelen == 1 && (name0 == 'r' || name0 == 'i' || name0 == 'a'
            || name0 == 'm'))
      {
        add_repr = 'z';
      }
    }
  }

  if (add_repr)
    len += 2;

  stripped = _GD_Malloc(D, len + 1); /* including trailing nul */
  if (stripped == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* Now compose the stripped name.  First the namespace part */
  if (ns_start < offset[2]) {
    memcpy(stripped, code + ns_start, offset[2] - ns_start);
    ptr = stripped + offset[2] - ns_start;
  } else
    ptr = stripped;

  /* BBBB */
  memcpy(ptr, code + offset[4], offset[5] - offset[4]);
  ptr += offset[5] - offset[4];

  /* subfield name and '/' */
  if (offset[7] != offset[8])
    memcpy(ptr, code + offset[7], offset[8] - offset[7]);

  /* representation suffix if any.  We only add a '.z' if it's necessary */
  if (add_repr) {
    stripped[len - 1] = add_repr;
    stripped[len - 2] = '.';
  }

  /* Terminate */
  stripped[len] = 0;

  dreturn("\"%s\"", stripped);
  return stripped;
}

/* Given a field code, add all the bits dictated by the specified fragment,
 * and also possibly a subnamespace.  Returns a malloc'd string or NULL on
 * error.
 *
 * The input code looks like this:
 *
 *     [.][AAAA.]BBBB[/CCCC][.R]
 *
 * where AAAA. is an optional attached subspace, /CCCC is only found in
 * metafields, .R is an optional representation suffix, and a leading dot
 * indicates the code is relative to the fragment's rootspace.  This function
 * returns a newly-malloc'd buffer containing:
 *
 *     NN.[NS.][AAAA.]PPBBBBSS[/CCCC][.R]
 *
 * where NN, PP, SS are the parts from the fragment, and NS. is added only
 * if the passed ns is non-NULL and the original code didn't have a leading
 * dot.  The dot following NN is omitted if NN is empty.
 *
 * If nons is non-zero, then we're in strict mode with DSV <= 9, meaning no
 * namespaces.  If it's <= DSV 5, then embedded '.' will be allowed as part of
 * a field code, otherwise they'll cause a failure later because they're invalid
 * characters.  In this case, NN. and NS. are always ignored, and there's
 * never an AAAA.
 *
 * If offset is non-NULL, it's the offset to the first part of the field name
 * (the first character of PPBBBBSS).  This is used in the parser to quickly
 * find the field name which requires validation during field code composition.
 * In this case, it's guaranteed that /CCCC is missing.
 */
char *_GD_BuildCode(DIRFILE *D, int index, const char *ns, size_t nslen,
    const char *code, int nons, size_t *offset)
{
  char *newcode = NULL, *ptr;
  const char *dot, *slash, *frag_ns, *name;
  size_t frag_nsl, len, newlen;
  int repr;

  dtrace("%p, %i, \"%s\", %" PRIuSIZE ", \"%s\", %i, %p", D, index, ns, nslen,
      code, nons, offset);

  if (nons) { /* No namespaces allowed! */
    frag_ns = ns = NULL;
    frag_nsl = nslen = 0;
  } else {
    frag_ns = D->fragment[index].ns;
    frag_nsl = D->fragment[index].nsl;

    if (code[0] == '.') {
      /* If the input code is root-absolute, discard the subnamespace (and also
       * the initial dot). */
      code++;
      ns = NULL;
      nslen = 0;
    }
  }

  if (nslen)
    nslen++; /* for the '.' */

  len = strlen(code);

  newlen = len + frag_nsl + nslen + D->fragment[index].pxl +
    D->fragment[index].sxl;
  if (frag_nsl)
    newlen++; /* for the '.' */
  
  if (newlen == len) { /* no change except maybe a removed initial '.' */
    newcode = _GD_Strdup(D, code);

    /* If we were passed offset (which only occurs in the parser), we have to do
     * this, I guess */
    if (newcode && offset) {
      repr = _GD_SlashDot(newcode, len, nons ? GD_CO_EARLY : 0, &dot, &slash);
      if (dot)
        *offset = dot - newcode + 1; /* Advance past the '.' */
      else
        *offset = 0;
    }

    dreturn("%p", newcode); /* will be NULL on error */
    return newcode;
  }

  ptr = newcode = _GD_Malloc(D, newlen + 1);
  if (newcode == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* Compose.  First, NN. */
  if (frag_nsl) {
    memcpy(ptr, frag_ns, frag_nsl);
    ptr[frag_nsl] = '.';
    ptr += frag_nsl + 1;
  }

  /* Now NS. */
  if (nslen) {
    memcpy(ptr, ns, nslen - 1);
    ptr[nslen - 1] = '.';
    ptr += nslen;
  }

  repr = _GD_SlashDot(code, len, nons ? GD_CO_EARLY : 0, &dot, &slash);

  if (repr) /* forget about the representation suffix */
    len -= 2;

  /* Now AAAA. */
  if (dot) {
    size_t aalen = dot - code + 1; /* +1 to include the '.' */
    memcpy(ptr, code, aalen);
    ptr += aalen;
    code += aalen; /* Now code points to BBBB in the input string */
    len -= aalen; /* ... and len reflects the loss of AAAA. */
  }

  /* Offset should point here, if present */
  name = ptr;

  /* Now PP */
  if (D->fragment[index].pxl) {
    memcpy(ptr, D->fragment[index].px, D->fragment[index].pxl);
    ptr += D->fragment[index].pxl;
  }

  /* Now BBBB.  This is the only part guaranteed to exist. */
  if (slash) {
    /* Don't copy /CCCC */
    size_t bblen = slash - code;
    memcpy(ptr, code, bblen);
    ptr += bblen;
  } else {
    /* No /CCCC to worry about: copy it all */
    memcpy(ptr, code, len);
    ptr += len;
  }

  /* Now SS */
  if (D->fragment[index].sxl) {
    memcpy(ptr, D->fragment[index].sx, D->fragment[index].sxl);
    ptr += D->fragment[index].sxl;
  }

  /* At this point, we check if PPBBBBSS is "INDEX" and if so, drop all the
   * namespaces (including the AAAA. which was specified with the code itself).
   */
  if (ptr - name == 5 && strncmp(name, "INDEX", 5) == 0 && name != newcode) {
    /* release the memory we don't need anymore.  It doesn't matter what
     * newcode contained.  We know now that all it should contain is "INDEX". */

    /* recompute length -- not just =5, because INDEX can have
     * subfield and/or representation suffixes. */
    newlen -= name - newcode;

    free(newcode);
    newcode = _GD_Malloc(D, newlen + 1);
    if (newcode == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }
    memcpy(newcode, "INDEX", 5);
    ptr = newcode + 5;
    name = newcode; /* for the *offset calculation later */
  }
  
  /* Now /CCCC */
  if (slash) {
    size_t cclen = code + len - slash;
    memcpy(ptr, slash, code + len - slash);
    ptr += cclen;
  }

  /* Finally, the representation suffix, if present, and then the NUL */
  if (repr) {
    ptr[0] = '.';
    ptr[1] = (char)repr;
    ptr[2] = 0;
  } else
    *ptr = 0;

  if (offset)
    *offset = name - newcode;
  
  dreturn("\"%s\"", newcode);
  return newcode;
}

/* Return non-zero if the a field code doesn't contain the correct affixes or
 * namespace. */
int _GD_CheckCodeAffixes(DIRFILE *D, const char *field_code, int fragment,
    unsigned flags)
{
  size_t offset[GD_N_CODEOFFSETS];

  dtrace("%p, \"%s\", %i, 0x%X", D, field_code, fragment, flags);

  if (field_code == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  if (_GD_CodeOffsets(D, fragment, field_code, flags | GD_CO_CHECK, offset)) {
    if (flags & GD_CO_ERROR)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, field_code);
    dreturn("%i", 1);
    return 1;
  }

  dreturn("%i", 0);
  return 0;
}

/* Check for a code -- returns 1 on error.
 * type is one of:
 *
 * GD_VF_CODE:  check a full field code
 * GD_VF_NS:    check a namespace
 * GD_VF_AFFIX: check an affix
 * GD_VF_NAME:  check a field name - used only for new fields (mostly by
 *              the parser)
 *
 * If type is GD_VF_NAME or GD_VF_CODE, and standards <= 5, and early is
 * non-NULL, then it is set to 1 if the passed code contains a '.'.  This
 * is used by the parser to flag fields which are incompatible with namespaces.
 * They will be placed in the nullspace and flagged with GD_EN_EARLY.
 */
int _GD_ValidateField(const char* field_code, size_t nsl, int standards,
    int strict, unsigned type)
{
  size_t len, i;
  int last_dot = 0;

  dtrace("\"%s\", %" PRIuSIZE ", %i, %i, %u", field_code, nsl, standards,
      strict, type);

  len = strlen(field_code);

  /* starting with DSV 6, field codes may not start with a dot, so last_dot is
   * one to start.  Namespaces and affixes never were allowed to do this. */
  if ((type == GD_VF_NS || type == GD_VF_AFFIX) || (strict && standards >= 6))
    last_dot = 1;

  /* Check name length.  Also fail on empty filed name */
  if ((type == GD_VF_NAME) && (field_code[0] == '\0' || (strict &&
          ((len > 50 && standards < 5) || (len > 16 && standards < 3)))))
  {
    dreturn("%i [a]", 1);
    return 1;
  }

  /* Find prohibited characters */
  for (i = 0; i < len; ++i)
    if (field_code[i] == '/' || (
          field_code[i] < 0x20
#if CHAR_MIN != 0
        && field_code[i] >= 0x00
#endif
        ))
    {
      /* these characters are always forbidden */
      dreturn("%i [b]", 1);
      return 1;
    } else if (strict && ((standards >= 5 && (field_code[i] == '<' ||
              field_code[i] == '>' || field_code[i] == ';' ||
              field_code[i] == '|' || field_code[i] == '&')) ||
          (standards == 5 && (field_code[i] == '\\' || field_code[i] == '#'))))
    {
      /* these characters are sometimes forbidden */
      dreturn("%i [c]", 1);
      return 1;
    } else if (field_code[i] == '.') {
      if (type == GD_VF_NS ||
          (type == GD_VF_CODE && (!strict || standards >= 10)))
      {
        if (last_dot) { /* multiple consecutive dots are forbidden */
          dreturn("%i [d]", 1);
          return 1;
        }
      } else if (type == GD_VF_AFFIX ||     /* '.' prohibited in affixes, */
                                    /* and in DSV >= 10 past the ns part, */
          ((standards >= 10 && strict) && i >= nsl) ||
          (standards >= 6 && standards < 10 && strict)) /* and in DSV 6-9 */
      {
        dreturn("%i [e]", 1);
        return 1;
      }
      last_dot = 1;
    } else
      last_dot = 0;

  /* Field codes may not end in a dot */
  if (type == GD_VF_CODE && last_dot) {
    dreturn("%i [f]", 1);
    return 1;
  }

  /* forbidden field names -- directives added after DSV 7 don't need to
   * go in this list because DSV 8 made the slash mandatory. */
  if (type == GD_VF_NAME) {
    if (strict && standards < 8)
      if ((strcmp("FRAMEOFFSET", field_code) == 0 && standards >= 1)
          || (strcmp("ENCODING", field_code) == 0 && standards >= 6)
          || (strcmp("ENDIAN", field_code) == 0 && standards >= 5)
          || (strcmp("INCLUDE", field_code) == 0 && standards >= 3)
          || (strcmp("META", field_code) == 0 && standards >= 6)
          || (strcmp("VERSION", field_code) == 0 && standards >= 5)
          || (strcmp("PROTECT", field_code) == 0 && standards >= 6)
          || (strcmp("REFERENCE", field_code) == 0 && standards >= 6))
      {
        dreturn("%i [g]", 1);
        return 1;
      }
  }

  dreturn("%i", 0);
  return 0;
}

/* Given a field code, determine whether it needs to be updated.  If it does,
 * and updb is non-zero, add a record to the rename data list; otherwise, do
 * nothing.  If the renamed field is a metafield, meta is non-zero.
 *
 * Returns non-zero on error.
 *
 * For any updated field, this function replaces CCCC., GGGG, and/or /KKKK
 * with new bits.  This is the complement of _GD_UpdateCode in fragment.c, which
 * replaces BBBB. and EEEE, and HHHH but leaves the rest unchanged.  (AAAA.,
 * DDDD, and IIII are immutable, because they come from the parent's scope.)
 *
 * NB: Not all matching codes can be updated.  Consider the following:
 *
 * Suppose a field (with attached subspace) named "AA.BBCCDD" is going to be
 * renamed to "EE.FF".  If it's in a fragment with rootspace "GG",
 * prefix "HH", suffix "II" then the full rename request looks like:
 *
 *              GG.AA.HHBBCCDDII -> GG.EE.HHFFII
 *
 * A (bare) field code "CC" in a different fragment with rootspace "GG.AA.",
 * prefix "HHBB" and suffix "DDII" will also have the fully-formed field code
 * "GG.AA.HHBBCCDDII", but the rename doesn't happen here, because the affixes
 * don't match.
 *
 * In this case, if GD_REN_FORCE is set, the update is skipped and the code
 * remains the old "GG.AA.HHBBCCDDII", which is later stored as "CC" in the
 * format metadata, as before.  If GD_REN_FORCE is unset, the rename fails.
 *
 * This situation can't occur with metafield renames because their names (the
 * part after the '/') never contains namespaces or affixes.
 */
static int _GD_RenameCode(DIRFILE *D, gd_entry_t *E, char **code,
    size_t *len, int updb, int force, struct gd_rename_data_ *rdat)
{
  size_t end, offset[GD_N_CODEOFFSETS];
  char *new_code;
  const unsigned early = (E->flags & GD_EN_EARLY) ? GD_CO_EARLY : 0;

  dtrace("%p, %p, %p, %p, %i, %i, %p", D, E, code, len, updb, force, rdat);

  /* Cut up the old code */
  _GD_CodeOffsets(D, E->fragment_index, *code, early, offset);

  /* If we're renaming a metafield, match everything up to the end of the
   * subfield, otherwise match only the parent. */
  if (rdat->flags & GD_REN_META)
    end = offset[8];
  else
    end = offset[7];

  /* Try to match */
  if (end != rdat->old_len || strncmp(*code, rdat->old_name, end)) {
    /* No match */
    dreturn("%i", 0);
    return 0;
  }

  /* Successful, match. */
  if (!updb) {
    /* No update required (just needed to check for match) */
    dreturn("%i", 0);
    return 0;
  }
  
  /* For moves (dst_frag != -1), use UpdateCode to make the new code. */
  if (rdat->dst_frag != -1) {
    new_code = _GD_UpdateCode(D, rdat->src_frag, *code, early,
        D->fragment[rdat->dst_frag].ns, D->fragment[rdat->dst_frag].nsl,
        D->fragment[rdat->dst_frag].px, D->fragment[rdat->dst_frag].pxl,
        D->fragment[rdat->dst_frag].sx, D->fragment[rdat->dst_frag].sxl);
    if (new_code == NULL) {
      dreturn("%i", -1);
      return -1;
    }
  } else {
    /* Otherwise, create the new code by hand. */
    size_t new_len = rdat->new_len + offset[9] - end + 1;
    new_code = _GD_Malloc(D, new_len);
    if (new_code == NULL) {
      dreturn("%i", -1);
      return -1;
    }
    memcpy(new_code, rdat->new_name, rdat->new_len);
    memcpy(new_code + rdat->new_len, *code + end, offset[9] - end + 1);

    /* For renames, verify that it's good -- _GD_BuildCode has already done this
     * for moves. */
    if (_GD_CodeOffsets(D, E->fragment_index, new_code, early | GD_CO_CHECK,
          offset))
    {
      if (force) {
        /* skip this code */
        free(new_code);
        dreturn("%i", 0);
        return 0;
      }

      /* otherwise, fail */
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, new_code);
      free(new_code);
      dreturn("%i", -1);
      return -1;
    }
  }

  /* Store the update in the struct */
  if (rdat->up_size == rdat->n_up) {
    void *ptr = _GD_Realloc(D, rdat->up,
        (rdat->up_size *= 2) * sizeof rdat->up[0]);
    if (ptr == NULL) {
      free(new_code);
      dreturn("%i", -1);
      return -1;
    }
    rdat->up = ptr;
  }
  rdat->up[rdat->n_up].index = E->fragment_index;
  rdat->up[rdat->n_up].dst = code;
  rdat->up[rdat->n_up].dst_len = len;
  if (len)
    rdat->up[rdat->n_up].new_len = strlen(new_code);
  rdat->up[rdat->n_up++].new_code = new_code;

  dreturn("%i", 0);
  return 0;
}

/* Update E->scalar[n].  Returns non-zero on error */
static int _GD_UpdateScalar(DIRFILE *D, gd_entry_t *E, int n, int updb,
    int force, struct gd_rename_data_ *rdat)
{
  dtrace("%p, %p, %i, %i, %i, %p", D, E, n, updb, force, rdat);

  /* nothing to do */
  if (E->scalar[n] == NULL) {
    dreturn("%i (-)", 0);
    return 0;
  }

  if (_GD_RenameCode(D, E, E->scalar + n, NULL, updb, force, rdat)) {
    dreturn("%i", -1);
    return -1;
  }

  /* Force recalculation */
  E->flags &= ~GD_EN_CALC;

  dreturn("%i", 0);
  return 0;
}

/* Update E->in_field[n].  Returns non-zero on error */
static int _GD_UpdateInField(DIRFILE *D, gd_entry_t *E, int n, int updb,
    int force, struct gd_rename_data_ *rdat)
{
  dtrace("%p, %p, %i, %i, %i, %p", D, E, n, updb, force, rdat);

  if (_GD_RenameCode(D, E, E->in_fields + n, NULL, updb, force, rdat)) {
    dreturn("%i", -1);
    return -1;
  }

  /* Clear the cached entry */
  E->e->entry[n] = NULL;

  dreturn("%i", 0);
  return 0;
}

/* Search for and update field metadata to account for a renamed field */
static int _GD_UpdateInputs(DIRFILE *D, struct gd_rename_data_ *rdat)
{
  unsigned u;
  int i;

  /* this is a meta-field update */
  const int meta = rdat->flags & GD_REN_META;

  /* Skip codes that can't be updated instead of failing */
  const int force = rdat->flags & GD_REN_FORCE;

  /* UPDB flag for non-aliases */
  const int updb = rdat->flags & GD_REN_UPDB;

  /* classes of things to update.  If we're renaming a top-level field, then
   * we need to search everything because there may be scalar and/or vector
   * subfields. */
  const int update_scalars = (!meta || (rdat->type & GD_SCALAR_ENTRY_BIT));
  const int update_vectors = (!meta || !(rdat->type & GD_SCALAR_ENTRY_BIT));
  const int update_aliases = !(rdat->flags & GD_REN_DANGLE);

  dtrace("%p, %p", D, rdat);

  for (u = 0; u < D->n_entries; ++u) {
    if (update_vectors)
      switch (D->entry[u]->field_type) {
        case GD_LINCOM_ENTRY:
          for (i = 0; i < D->entry[u]->EN(lincom,n_fields); ++i) {
            if (_GD_UpdateInField(D, D->entry[u], i, updb, force, rdat)) {
              dreturn("%i", -1);
              return -1;
            }
          }
          break;
        case GD_MULTIPLY_ENTRY:
        case GD_DIVIDE_ENTRY:
        case GD_WINDOW_ENTRY:
        case GD_MPLEX_ENTRY:
          if (_GD_UpdateInField(D, D->entry[u], 1, updb, force, rdat)) {
            dreturn("%i", -1);
            return -1;
          }
          /* Fallthrough */
        case GD_LINTERP_ENTRY:
        case GD_BIT_ENTRY:
        case GD_PHASE_ENTRY:
        case GD_POLYNOM_ENTRY:
        case GD_RECIP_ENTRY:
        case GD_SBIT_ENTRY:
        case GD_INDIR_ENTRY:
        case GD_SINDIR_ENTRY:
          if (_GD_UpdateInField(D, D->entry[u], 0, updb, force, rdat)) {
            dreturn("%i", -1);
            return -1;
          }
          break;
        case GD_INDEX_ENTRY:
        case GD_RAW_ENTRY:
        case GD_NO_ENTRY:
        case GD_CONST_ENTRY:
        case GD_CARRAY_ENTRY:
        case GD_SARRAY_ENTRY:
        case GD_STRING_ENTRY:
        case GD_ALIAS_ENTRY:
          break;
      }
    if (update_scalars)
      switch (D->entry[u]->field_type) {
        case GD_LINCOM_ENTRY:
          for (i = 0; i < D->entry[u]->EN(lincom,n_fields); ++i)
            if (_GD_UpdateScalar(D, D->entry[u], i, updb, force, rdat) ||
                _GD_UpdateScalar(D, D->entry[u], i + GD_MAX_LINCOM, updb,
                  force, rdat))
            {
              dreturn("%i", -1);
              return -1;
            }
          break;
        case GD_POLYNOM_ENTRY:
          for (i = 0; i <= D->entry[u]->EN(polynom,poly_ord); ++i)
            if (_GD_UpdateScalar(D, D->entry[u], i, updb, force, rdat)) {
              dreturn("%i", -1);
              return -1;
            }
          break;
        case GD_BIT_ENTRY:
        case GD_SBIT_ENTRY:
        case GD_MPLEX_ENTRY:
          if (_GD_UpdateScalar(D, D->entry[u], 1, updb, force, rdat)) {
            dreturn("%i", -1);
            return -1;
          }
          /* Fallthrough */
        case GD_PHASE_ENTRY:
        case GD_RAW_ENTRY:
        case GD_RECIP_ENTRY:
        case GD_WINDOW_ENTRY:
          if (_GD_UpdateScalar(D, D->entry[u], 0, updb, force, rdat)) {
            dreturn("%i", -1);
            return -1;
          }
          break;
        case GD_INDIR_ENTRY:
          if (!meta || rdat->type == GD_CARRAY_ENTRY)
            if (_GD_UpdateInField(D, D->entry[u], 1, updb, force, rdat)) {
              dreturn("%i", -1);
              return -1;
            }
          break;
        case GD_SINDIR_ENTRY:
          if (!meta || rdat->type == GD_SARRAY_ENTRY)
            if (_GD_UpdateInField(D, D->entry[u], 1, updb, force, rdat)) {
              dreturn("%i", -1);
              return -1;
            }
          break;
        case GD_NO_ENTRY:
        case GD_LINTERP_ENTRY:
        case GD_MULTIPLY_ENTRY:
        case GD_DIVIDE_ENTRY:
        case GD_INDEX_ENTRY:
        case GD_STRING_ENTRY:
        case GD_CONST_ENTRY:
        case GD_CARRAY_ENTRY:
        case GD_SARRAY_ENTRY:
        case GD_ALIAS_ENTRY:
          break;
      }

    /* If we're updating aliases, then updb is always 1 (aliases will
     * be re-hashed later, so there's no reason to manually flag them if
     * we're not updating alias targets) */
    if (update_aliases && D->entry[u]->field_type == GD_ALIAS_ENTRY)
      if (_GD_RenameCode(D, D->entry[u], D->entry[u]->in_fields + 0, NULL, 1,
            force, rdat))
      {
        dreturn("%i", -1);
        return -1;
      }
  }

  dreturn("%i", 0);
  return 0;
}

/* delete rename data */
void _GD_CleanUpRename(struct gd_rename_data_ *rdat, int abort)
{
  unsigned int i;

  dtrace("%p, %i", rdat, abort);

  if (rdat) {
    if (abort) {
      for (i = 0; i < rdat->n_up; ++i)
        free(rdat->up[i].new_code);
    }
    free(rdat->up);
    free(rdat);
  }

  dreturnvoid();
}

/* perform a database update due to a renamed field.  This function
 * cannot fail. */
void _GD_PerformRename(DIRFILE *restrict D,
    struct gd_rename_data_ *restrict rdat)
{
  unsigned i;

  dtrace("%p, %p", D, rdat);

  /* update all the codes in one go */
  for (i = 0; i < rdat->n_up; ++i) {
    free(*rdat->up[i].dst);
    *(rdat->up[i].dst) = rdat->up[i].new_code;
    if (rdat->up[i].dst_len)
      *(rdat->up[i].dst_len) = rdat->up[i].new_len;

    /* Mark fragment dirty */
    D->fragment[rdat->up[i].index].modified = 1;
  }

  /* re-sort the entry list */
  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);

  /* Invalidate field lists */
  rdat->fl->value_list_validity = 0;
  rdat->fl->entry_list_validity = 0;

  /* rehash the aliases */
  _GD_UpdateAliases(D, 1);

  /* done */
  _GD_CleanUpRename(rdat, 0);

  dreturnvoid();
}

/* Prepare for a database update due to a renamed field.  If this is a move
 * request, new_fragment is the destination.  For a rename, new_fragment is -1.
 *
 * new_name must be malloc'd.  This function will steal it, even if it fails
 * (returns NULL).
 *
 * Returns NULL on error otherwise a pointer to the gd_rename_data_ struct. */
struct gd_rename_data_ *_GD_PrepareRename(DIRFILE *restrict D,
    char *restrict new_name, size_t new_len, gd_entry_t *restrict E,
    int new_fragment, unsigned flags)
{
  int i;
  struct gd_rename_data_ *rdat;

  dtrace("%p, \"%s\", %" PRIuSIZE ", %p, %i, 0x%X", D, new_name, new_len, E,
      new_fragment, flags);

  rdat = _GD_Malloc(D, sizeof *rdat);
  if (rdat == NULL) {
    free(new_name);
    dreturn("%p", NULL);
    return NULL;
  }

  rdat->flags = flags | ((E->e->n_meta == -1) ? GD_REN_META : 0);
  if (rdat->flags & GD_REN_META)
    rdat->fl = &E->e->fl;
  else
    rdat->fl = &D->fl;

  /* resolve field type */
  if (E->field_type == GD_ALIAS_ENTRY && E->e->entry[0])
    rdat->type = E->e->entry[0]->field_type;
  else
    rdat->type = E->field_type;

  rdat->src_frag = E->fragment_index;
  rdat->dst_frag = new_fragment;
  rdat->old_name = E->field;
  rdat->old_len = E->e->len;
  rdat->new_name = new_name;
  rdat->new_len = new_len;

  rdat->up_size = 10;
  rdat->up = _GD_Malloc(D, rdat->up_size * sizeof rdat->up[0]);
  if (rdat->up == NULL) {
    free(new_name);
    free(rdat);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Add the new field name update, for a non-meta field, this is just new_name
   */
  rdat->n_up = 1;
  rdat->up[0].new_code = new_name;
  rdat->up[0].new_len = new_len;
  rdat->up[0].index = E->fragment_index;
  rdat->up[0].dst = &E->field;
  rdat->up[0].dst_len = &E->e->len;

  /* Add subfield name updates, too */
  for (i = 0; i < E->e->n_meta; ++i)
    if (_GD_RenameCode(D, E->e->p.meta_entry[i], &E->e->p.meta_entry[i]->field,
          &E->e->p.meta_entry[i]->e->len, 1, 0, rdat))
    {
      _GD_CleanUpRename(rdat, 1);
      dreturn("%p", NULL);
      return NULL;
    }

  /* update all input field codes across the whole database */
  if (_GD_UpdateInputs(D, rdat)) {
    _GD_CleanUpRename(rdat, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", rdat);
  return rdat;
}

static int _GD_Rename(DIRFILE *D, gd_entry_t *E, const char *new_name,
    unsigned flags)
{
  gd_entry_t *Q;
  char *name;
  size_t len = strlen(new_name);
  struct gd_rename_data_ *rdat = NULL;

  dtrace("%p, %p, \"%s\", 0x%X", D, E, new_name, flags);

  if (_GD_ValidateField(new_name, 0, D->standards, 1, GD_VF_CODE))
    GD_SET_RETURN_ERROR(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, new_name);

  if (E->e->n_meta == -1) {
    size_t plen = strlen(E->e->p.parent->field);
    name = _GD_Malloc(D, plen + len + 2);
    if (name == NULL)
      GD_RETURN_ERROR(D);
    memcpy(name, E->e->p.parent->field, plen);
    name[plen] = '/';
    memcpy(name + plen + 1, new_name, len + 1); /* including NUL */
    len += plen + 1;
  } else {
    /* Verify prefix and suffix */
    if (_GD_CheckCodeAffixes(D, new_name, E->fragment_index,
          GD_CO_ERROR | ((E->flags & GD_EN_EARLY) ? GD_CO_EARLY : 0)))
    {
      GD_RETURN_ERROR(D);
    }

    name = _GD_Malloc(D, len + 1);
    if (name == NULL)
      GD_RETURN_ERROR(D);

    memcpy(name, new_name, len + 1);
  }

  /* Duplicate check */
  Q = _GD_FindField(D, name, len, D->entry, D->n_entries, 1, NULL);

  if (Q == E) {
    free(name);
    dreturn("%i", 0);
    return 0;
  }

  if (Q != NULL) {
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, name);
    free(name);
    GD_RETURN_ERROR(D);
  }

  /* prep for metadata update */
  rdat = _GD_PrepareRename(D, name, len, E, -1, flags);

  if (rdat == NULL)
    GD_RETURN_ERROR(D);

  if (E->field_type == GD_RAW_ENTRY) {
    /* Compose the new filename */
    char *filebase = _GD_StripCode(D, E->fragment_index, new_name, GD_CO_NSALL
        | ((E->flags & GD_EN_EARLY) ? GD_CO_EARLY : 0));

    if (filebase == NULL) {
      _GD_CleanUpRename(rdat, 1);
      GD_RETURN_ERROR(D);
    }

    /* Close the old file */
    if (_GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_KEEP)) {
      _GD_CleanUpRename(rdat, 1);
      free(filebase);
      GD_RETURN_ERROR(D);
    }

    if (flags & GD_REN_DATA) {
      struct gd_raw_file_ temp;

      /* check data protection */
      if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
        _GD_CleanUpRename(rdat, 1);
        free(filebase);
        GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[E->fragment_index].cname);
      }

      if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_MOVE)) {
        _GD_CleanUpRename(rdat, 1);
        free(filebase);
        GD_RETURN_ERROR(D);
      }

      memcpy(&temp, E->e->u.raw.file, sizeof(struct gd_raw_file_));
      temp.name = NULL;
      if ((*_GD_ef[temp.subenc].name)(D,
            (const char*)D->fragment[E->fragment_index].enc_data, &temp,
            filebase, 0, 0))
      {
        _GD_CleanUpRename(rdat, 1);
        free(filebase);
        GD_RETURN_ERROR(D);
      }

      if ((*_GD_ef[temp.subenc].name)(D,
            (const char*)D->fragment[E->fragment_index].enc_data,
            E->e->u.raw.file, E->e->u.raw.filebase, 0, 0))
      {
        _GD_CleanUpRename(rdat, 1);
        free(filebase);
        GD_RETURN_ERROR(D);
      }

      if ((*_GD_ef[E->e->u.raw.file[0].subenc].move)(
            D->fragment[E->fragment_index].dirfd, E->e->u.raw.file,
            D->fragment[E->fragment_index].dirfd, temp.name))
      {
        _GD_SetEncIOError(D, GD_E_IO_RENAME, E->e->u.raw.file + 0);
        _GD_CleanUpRename(rdat, 1);
        free(filebase);
        GD_RETURN_ERROR(D);
      }

      /* Nothing may fail from now on */

    } else {
      free(E->e->u.raw.file[0].name);
      E->e->u.raw.file[0].name = NULL;
    }

    free(E->e->u.raw.filebase);
    E->e->u.raw.filebase = filebase;
  }

  D->fragment[E->fragment_index].modified = 1;

  /* Update database metadata */
  _GD_PerformRename(D, rdat);

  D->flags &= ~GD_HAVE_VERSION;

  dreturn("%i", 0);
  return 0;
}

int gd_rename(DIRFILE *D, const char *old_code, const char *new_name,
    unsigned flags)
{
  gd_entry_t *E = NULL;
  int ret;

  dtrace("%p, \"%s\", \"%s\", 0x%X", D, old_code, new_name, flags);

  GD_RETURN_ERR_IF_INVALID(D);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);

  E = _GD_FindField(D, old_code, strlen(old_code), D->entry, D->n_entries, 0,
      NULL);

  if (E == NULL)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, old_code);

  if (E->field_type == GD_INDEX_ENTRY)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0,
        "INDEX");

  /* check metadata protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
    GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);

  ret = _GD_Rename(D, E, new_name, flags);

  dreturn("%i", ret);
  return ret;
}
