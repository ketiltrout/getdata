/* Copyright (C) 2010, 2011 Matthew Truch
 * Copyright (C) 2010, 2012, 2014, 2015 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

#include <getopt.h>

#ifndef HAVE_OFF64_T
#define off64_t off_t
#endif

#define BUF_LEN GD_MAX_LINE_LENGTH
#define F_LEN 16
#define VALID_PRECISION_CHARS "#- +`I.0123456789"

struct field
{
  char *name;
  int type;
  unsigned int spf;
  union {
    double *dbl;
    int64_t *i64;
    uint64_t *u64;
  } u;
  char format[F_LEN];
  size_t n_read;
};

void version(void)
{
  printf("dirfile2ascii (%s)\n"
      "Copyright (C) 2010, 2011, 2012, 2014 Matthew Truch and others\n\n"
      "Please send reports of bugs and other communication to:\n  %s\n\n"
      "This program comes with NO WARRANTY, not even for MERCHANTABILITY "
      "or FITNESS\n"
      "FOR A PARTICULAR PURPOSE. You may redistribute it under the terms of "
      "the GNU\n"
      "Lesser General Public License, either version 2.1 of the License, or "
      "(at your\n"
      "option) any later version.\n\n"
      "You should have received a copy of the GNU Lesser General Public "
      "License along\n"
      "with this program; if not, write to the Free Software Foundation,"
      "Inc.,\n"
      "51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA\n",
      PACKAGE_STRING, PACKAGE_BUGREPORT);
  exit(-1);
}

void usage(char *exe)
{
  printf("Usage: %s [OPTION]... DIRFILE\n"
      "                    [[-a|-A|-e|-E|-F|-g|-G|-o|-i|-u|-x|-X] FIELD]...\n"
      "Dump specified fields from the dirfile DIRFILE as ASCII on stdout.\n\n"

      "Mandatory arguments to long options are mandatory for short options "
      "too.\n"
#if 0
      "  -b, --boxcar    boxcar average over skipped samples before output.  "
      "Ignored\n"
      "                    if --skip is not specified.\n"
#endif
      "  -d, --delimiter=D      separate columns by D (default: a single "
      "space)\n"
      "  -f, --first-frame=F-M  read from frame F to frame M (inclusive).\n"
      "  -f, --first-frame=F:N  equivalent to --first-frame=F --num-frames=N\n"
      "  -f, --first-frame=F    if F >= 0, start reading at frame F.  If F is "
      "-1 and\n"
      "                           --num-frames=N is specified, read the last N "
      "frames.\n"
      "                           If not specified, F = 0 is assumed.\n"
      "  -n, --num-frames=N     read at most N frames.  If not specified, or "
      "if N = 0,\n"
      "                           all frames to the end of the dirfile are "
      "read.\n"
      "  -p, --precision=FMT    use FMT to format output.  FMT may contain any "
      "of the\n"
      "                           flag characters, a field width, and/or a "
      "precision as\n"
      "                           specified in printf(3).  It may NOT contain "
      "a length\n"
      "                           modifier.\n"
      "  -q, --quiet            don't write verbose message to standard error "
      "(the\n"
      "                           default).\n"
      "  -s, --skip=S           if S > 0, output only one sample for every S "
      "frames.\n"
      "  -v, --verbose          write verbosely to standard error.\n"
      "  -z, --fill=STRING      print STRING to fill up columns which run past "
      "the end\n"
      "                           of their corresponding field.  The default "
      "is to\n"
      "                           fill floating point columns with \"NaN\" and "
      "integer\n"
      "                           columns with \"0\" (without quotation marks)."
      "\n\n"

      "Any number of fields may be specified.  Each specified field is printed "
      "in a\n"
      "separate column.  A field name may be preceded by a short option, one "
      "of -a, -A,\n"
      "-e, -E, -F, -g, -G, -o, -i, -u, -x, -X, indicating the conversion to be "
      "used.\n"
      "See printf(3) for the meaning of these conversion specifiers.  The "
      "output flags,\n"
      "width, and precision may be specified by --precision.  If no "
      "conversion\n"
      "specifier is given, %%f is used.\n\n"
      "For conversion specifiers %%a, %%A, %%e, %%E, %%f, %%F, %%g, %%G, data "
      "is read from the\n"
      "dirfile as double precision floats.  For conversion specifier %%i, the "
      "data is\n"
      "read as 64-bit signed integers.  For conversion specifiers %%o, "
      "%%u, %%x, %%X, the\n"
      "data is read as 64-bit unsigned integers.\n", exe);
  exit(-1);
}

int main (int argc, char **argv)
{
  char char_buffer[BUF_LEN];
  char *tmp;
  char *delimiter = " ";
  char *precision = "";
  char *zero = NULL;
  char *dirfile_name = NULL;
  DIRFILE *dirfile;
  size_t nf = 0;
  off64_t ff = 0;
  size_t n_want;
  unsigned int j;
  unsigned int max_spf = 0;
  unsigned int min_spf = INT_MAX;
  int c, i;
  int numfields = 0;
  struct field fields[BUF_LEN];
  int verbose = 0;
  int interpolate = 0;
  size_t skip = 0;
  int skipping = 0;
  size_t k, z;
#if 0
  int average = 0;
#endif
  int optind = 0;
  const double NaN = NAN;
  const struct option longopts[] = {
#if 0
    { "boxcar",      0, NULL, 'b' },
#endif
    { "delimieter",  1, NULL, 'd' },
    { "fill",        1, NULL, 'z' },
    { "first-frame", 1, NULL, 'f' },
    { "help",        0, NULL, '?' },
    { "num-frames",  1, NULL, 'n' },
    { "precision",   1, NULL, 'p' },
    { "quiet",       0, NULL, 'q' },
    { "skip",        1, NULL, 's' },
    { "verbose",     0, NULL, 'v' },
    { "version",     0, NULL,  2  },
    { 0,             0, 0,     0  }
  };

  /* handy things to know about conversion specifiers */
#define READ_AS_DOUBLE 0x000
#define READ_AS_INT    0x100
#define READ_AS_UINT   0x200
  struct {
    int t;
    const char* f;
  } type_data[0x7f];
  type_data[(int)'a'].t = READ_AS_DOUBLE;
  type_data[(int)'a'].f = "a";
  type_data[(int)'A'].t = READ_AS_DOUBLE;
  type_data[(int)'A'].f = "A";
  type_data[(int)'e'].t = READ_AS_DOUBLE;
  type_data[(int)'e'].f = "e";
  type_data[(int)'E'].t = READ_AS_DOUBLE;
  type_data[(int)'E'].f = "E";
  type_data[(int)'f'].t = READ_AS_DOUBLE;
  type_data[(int)'f'].f = "f";
  type_data[(int)'F'].t = READ_AS_DOUBLE;
  type_data[(int)'F'].f = "F";
  type_data[(int)'g'].t = READ_AS_DOUBLE;
  type_data[(int)'g'].f = "g";
  type_data[(int)'G'].t = READ_AS_DOUBLE;
  type_data[(int)'G'].f = "G";
  type_data[(int)'i'].t = READ_AS_INT;
  type_data[(int)'i'].f = PRId64;
  type_data[(int)'o'].t = READ_AS_UINT;
  type_data[(int)'o'].f = PRIo64;
  type_data[(int)'u'].t = READ_AS_UINT;
  type_data[(int)'u'].f = PRIu64;
  type_data[(int)'x'].t = READ_AS_UINT;
  type_data[(int)'x'].f = PRIx64;
  type_data[(int)'X'].t = READ_AS_UINT;
  type_data[(int)'X'].f = PRIX64;

  while ((c = getopt_long(argc, argv,
          "-f:n:d:x:X:g:G:e:E:a:A:F:i:o:p:s:u:z:bvqh?", longopts, &optind)) != -1)
  {
    switch (c) {
      case 1: /* The case of no option */
        if (dirfile_name == NULL) {
          dirfile_name = optarg;
        } else { /* Standard output field */
          if (numfields >= BUF_LEN) {
            fprintf(stderr, "Error: Too many fields!\n");
            exit(-2);
          }
          fields[numfields].type = 'f';
          fields[numfields].name = optarg;
          numfields++;
        }
        break;
      case 2:
        version();
        break;
      case 'x': /* output field types */
      case 'X':
      case 'i':
      case 'o':
      case 'g':
      case 'G':
      case 'e':
      case 'E':
      case 'u':
      case 'F':
      case 'A':
      case 'a':
        if (numfields >= BUF_LEN) {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = c;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'f': /* firstframe */
        ff = (off64_t)strtoll(optarg, &tmp, 0);
        if (tmp[0] == ':' && tmp[1] != '\0')
          nf = (size_t)strtoll(&(tmp[1]), NULL, 0);
        else if (tmp[0] == '-' && tmp[1] != '\0')
          nf = (size_t)(strtoll(&(tmp[1]), NULL, 0) - ff);
        break;
      case 'n': /* numframes */
        nf = (size_t)strtoll(optarg, NULL, 0);
        break;
      case 'p':
        if (strlen(optarg) < (F_LEN - 6))
          precision = optarg;
        else
          fprintf(stderr, "Ignoring exceedingly long -p argument (%s).\n",
              optarg);
        break;
      case 'v':
        verbose = 1;
        break;
      case 'q':
        verbose = 0;
        break;
      case 'd':
        delimiter = optarg;
        break;
      case 's':
        skip = strtoll(optarg, NULL, 0);
        break;
      case 'z':
        zero = optarg;
        break;
      case 'b':
#if 0
        average = 1;
#endif
        fprintf(stderr, "Warning: Ignoring -b found on command line.\n");
        break;
      case 'h':
      case '?':
      default:
        usage(argv[0]);
        break;
    }
  }

  if (dirfile_name == NULL) {
    fprintf(stderr, "Error: No dirfile specified.\n");
    exit(-4);
  }

  if (numfields < 1) {
    fprintf(stderr, "Error: At least one field must be specified.\n");
    exit(-3);
  }

  if ((ff == -1) && (nf == 0)) {
    fprintf(stderr, "Warning: Ignoring negative first frame with no frame "
        "count given.\n");
    ff = 0;
  }

  for (z = 0; z < strlen(precision); z++) {
    if (strchr(VALID_PRECISION_CHARS, precision[z]) == NULL) {
      fprintf(stderr,
          "Error: Invalid character (%c) found in precision string (%s).\n", 
          precision[z], precision);
      exit(-5);
    }
  }

  if (skip)
    skipping = 1;
  if (skip < 1)
    skip = 1;

  dirfile = gd_open(dirfile_name, GD_RDONLY);
  if (gd_error(dirfile)) {
    fprintf(stderr, "GetData error: %s\n", gd_error_string(dirfile, char_buffer,
          BUF_LEN));
    gd_close(dirfile);
    exit(1);
  }

  if (nf == 0) {
    nf = gd_nframes(dirfile) - ff;
    if (gd_error(dirfile)) {
      fprintf(stderr, "GetData error: %s\n", gd_error_string(dirfile,
            char_buffer, BUF_LEN));
      gd_close(dirfile);
      exit(3);
    }
  }

  if (ff == -1) {
    ff = gd_nframes(dirfile) - nf;
    if (gd_error(dirfile)) {
      fprintf(stderr, "GetData error: %s\n", gd_error_string(dirfile,
            char_buffer, BUF_LEN));
      gd_close(dirfile);
      exit(3);
    }
  }

  /* Get spfs and sanity checks for all fields */
  for (i = 0; i < numfields; i++) {
    fields[i].spf = gd_spf(dirfile, fields[i].name);
    if (gd_error(dirfile)) {
      fprintf(stderr, "GetData error: %s\n", gd_error_string(dirfile,
            char_buffer, BUF_LEN));
      gd_close(dirfile);
      exit(3);
    }

    if (max_spf && fields[i].spf != max_spf)
      interpolate = 1;

    if (fields[i].spf > max_spf)
      max_spf = fields[i].spf;

    if (fields[i].spf < min_spf)
      min_spf = fields[i].spf;
  }

  if (interpolate && nf == 1 && min_spf == 1) {
    fprintf(stderr, "Error: Interpolation required, "
        "but at least one field has only 1 sample.\n");
    exit(-6);
  }

  if (verbose) {
    fprintf(stderr, "Reading %d field%s from %s\n", numfields,
        (numfields > 1) ? "s" : "", dirfile_name);
    fprintf(stderr, "First frame: %" PRIu64 " Number of frames: %" PRIdSIZE
        "\n", ff, nf);
  }

  for (i = 0; i < numfields; i++) { /* Read in all the fields */
    n_want = nf * fields[i].spf;

    if (type_data[fields[i].type].t == READ_AS_DOUBLE) {
      fields[i].u.dbl = (double *)malloc(sizeof(double) * n_want);
      if (fields[i].u.dbl == NULL) {
        perror("malloc");
        gd_close(dirfile);
        exit(4);
      }

      fields[i].n_read = gd_getdata(dirfile, fields[i].name, ff, 0, nf, 0,
          GD_FLOAT64, fields[i].u.dbl);
    } else if (type_data[fields[i].type].t == READ_AS_INT) {
      fields[i].u.i64 = (int64_t *)malloc(sizeof(int64_t) * n_want);
      if (fields[i].u.i64 == NULL) {
        perror("malloc");
        gd_close(dirfile);
        exit(4);
      }
      fields[i].n_read = gd_getdata(dirfile, fields[i].name, ff, 0, nf, 0,
          GD_INT64, fields[i].u.i64);
    } else {
      fields[i].u.u64 = (uint64_t *)malloc(sizeof(uint64_t) * n_want);
      if (fields[i].u.u64 == NULL) {
        perror("malloc");
        gd_close(dirfile);
        exit(4);
      }
      fields[i].n_read = gd_getdata(dirfile, fields[i].name, ff, 0, nf, 0,
          GD_UINT64, fields[i].u.u64);
    }

    if (gd_error(dirfile)) {
      fprintf(stderr, "GetData error: %s\n", gd_error_string(dirfile,
            char_buffer, BUF_LEN));
      gd_close(dirfile);
      exit(5);
    } else if (!zero && n_want > fields[i].n_read) {
      if (verbose)
        fprintf(stderr,
            "Short read on field %i, padding %" PRIuSIZE " frames\n",
            i, n_want - fields[i].n_read);
      if (type_data[fields[i].type].t == READ_AS_DOUBLE) {
        for (k = fields[i].n_read; k < n_want; ++k)
          fields[i].u.dbl[k] = NaN;
      } else if (type_data[fields[i].type].t == READ_AS_INT) {
        memset(fields[i].u.i64 + fields[i].n_read, 0,
            (n_want - fields[i].n_read) * sizeof(int64_t));
      } else {
        memset(fields[i].u.u64 + fields[i].n_read, 0,
            (n_want - fields[i].n_read) * sizeof(uint64_t));
      }
    }
  }

  /* Generate format string with precision */
  for (i = 0; i < numfields; i++)
    snprintf(fields[i].format, F_LEN, "%%%s%s", precision,
        type_data[fields[i].type].f);

  for (k = 0; k < nf; k += skip) {
    for (j = 0; j < (skipping ? 1 : max_spf); j++) {
      for (i = 0; i < numfields; i++) {
        if (fields[i].spf == max_spf || skipping) {
          if (zero && k * fields[i].spf + j >= fields[i].n_read)
            printf("%s", zero);
          else if (type_data[fields[i].type].t == READ_AS_DOUBLE)
            printf(fields[i].format, fields[i].u.dbl[k * fields[i].spf + j]);
          else if (type_data[fields[i].type].t == READ_AS_INT)
            printf(fields[i].format, fields[i].u.i64[k * fields[i].spf + j]);
          else
            printf(fields[i].format, fields[i].u.u64[k * fields[i].spf + j]);
        } else { /* need to interpolate: */
          /*      formula is  y = y0 + (x-x0) * (y1-y0)/(x1-x0) */
          /*                 val= y0 +  diff  *     slope       */
          /* Note that if max_spfs isn't a multiple of all spfs then some of  */
          /* the data will only be approximated (except at frame boundaries). */
          unsigned int prev_samp, next_samp, offset;
          double slope, diff, val;

          prev_samp = (unsigned int)floor((double)j * (double)fields[i].spf /
              (double)max_spf);
          next_samp = prev_samp + 1;
          diff = ((double)prev_samp + (double)(j % (max_spf / fields[i].spf)) /
              (double)(max_spf / fields[i].spf)) - (double)prev_samp;

          if (k == (nf - 1) && next_samp == fields[i].spf)
            /* we're on ultimate sample, so use slope from previous set */
            offset = 1;
          else
            offset = 0;

          if (zero && k * fields[i].spf + j >= fields[i].n_read) {
            printf("%s", zero);
          } else if (type_data[fields[i].type].t == READ_AS_DOUBLE) {
            slope = (fields[i].u.dbl[k * fields[i].spf + next_samp - offset] -
                fields[i].u.dbl[k * fields[i].spf + prev_samp - offset]) /
              ((double)next_samp - (double)prev_samp);
            val = fields[i].u.dbl[k * fields[i].spf + prev_samp] + diff * slope;
            printf(fields[i].format, val);
          } else if (type_data[fields[i].type].t == READ_AS_INT) {
            slope = ((double)fields[i].u.i64[k * fields[i].spf + next_samp
                - offset] - (double)fields[i].u.i64[k * fields[i].spf
                + prev_samp - offset]) / ((double)next_samp
                  - (double)prev_samp);
            val = (double)fields[i].u.i64[k * fields[i].spf + prev_samp] + diff
              * slope;
            printf(fields[i].format, (int64_t)val);
          } else {
            slope = ((double)fields[i].u.u64[k * fields[i].spf + next_samp
                - offset] - (double)fields[i].u.u64[k * fields[i].spf
                + prev_samp - offset]) / ((double)next_samp
                  - (double)prev_samp);
            val = (double)fields[i].u.u64[k * fields[i].spf + prev_samp] + diff
              * slope;
            printf(fields[i].format, (uint64_t)val);
          }
        }

        if (i < (numfields - 1)) {
          printf("%s", delimiter);
        } else {
          printf("\n");
        }
      }
    }
  }

  gd_close(dirfile);
  return 0;
}
