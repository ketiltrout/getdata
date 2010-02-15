#define _XOPEN_SOURCE 1000
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <inttypes.h>
#include <getdata.h>
#include <getopt.h>
#include <math.h>

#define BUF_LEN 1024
#define F_LEN 16
#define VALID_PRECISION_CHARS "#- +`I.0123456789"

struct field
{
  char *name;
  int8_t type;
  uint16_t spf;
  double *dbl;
  int64_t *i64;
  char format[F_LEN];
};

void usage(char *exe)
{
  fprintf(stderr, "Dump specified fields from a dirfile as ascii on stdout\n");
  fprintf(stderr, "Usage: %s dirfile [-f firstframe] [-n numframes]\n", exe);
  fprintf(stderr, "  [-v | -q] [-d delimeter] [-p width.precision] \n");
  fprintf(stderr, "  [-s skip [-a]] [-Z] field1 [[-Z] field2] ...\n");
  fprintf(stderr, "Z is one of x, X, g, G, e, E, o, or i\n");
  fprintf(stderr, "firstframe can be -1 meaning \"Count From End\"\n");
  fprintf(stderr, "firstframe can have \"firstframe:numframes\" format.\n");
  fprintf(stderr, "firstframe can have \"firstframe-lastframe\" format.\n");
  fprintf(stderr, "numframes can be -1 meaning \"Read To End\"\n");
  fprintf(stderr, "It is an error if both firstframe and numframes are -1\n");
  exit(-1);
}

int main (int argc, char **argv)
{
  char char_buffer[BUF_LEN];
  char *tmp;
  char delimeter[BUF_LEN] = " ";
  char precision[F_LEN] = "";
  char *dirfile_name = NULL;
  DIRFILE *dirfile;
  off_t nf = -1;
  off_t ff = 0;
  size_t n_read, n_want;
  uint16_t max_spf = 0;
  uint16_t min_spf = UINT16_MAX;
  int c;
  int numfields = -1;
  struct field fields[BUF_LEN];
  int verbose = 1;
  int interpolate = 0;
  size_t skip = 0;
  int skipping = 0;
  int average = 0;

  while ((c = getopt(argc, argv, "-f:n:d:x:X:g:G:e:E:i:o:p:s:avqh?")) != -1)
  {
    switch (c)
    {
      case 1: /* The case of no option */
        if (numfields == -1) /* This is the dirfile */
        {
          numfields = 0;
          dirfile_name = optarg;
        } else { /* Standard output field */
          if (numfields > BUF_LEN)
          {
            fprintf(stderr, "Error: Too many fields!\n");
            exit(-2);
          }
          fields[numfields].type = 1;
          fields[numfields].name = optarg;
          numfields++;
        }
        break;
      case 'x': /* Hex output field */
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = -1;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'X':
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = -2;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'i': /* Integer output field */
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = -3;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'o': /* Octal output field */
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = -4;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'g': /* "Nice" floating point output field */
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = 2;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'G':
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = 3;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'e': /* Scientific notation output field */
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = 4;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'E':
        if (numfields > BUF_LEN)
        {
          fprintf(stderr, "Error: Too many fields!\n");
          exit(-2);
        }
        fields[numfields].type = 5;
        fields[numfields].name = optarg;
        numfields++;
        break;
      case 'f': /* firstframe */
        ff = strtoll(optarg, &tmp, 0);
        if (tmp[0] == ':' && tmp[1] != '\0')
          nf = strtoll(&(tmp[1]), NULL, 0);
        else if (tmp[0] == '-' && tmp[1] != '\0')
          nf = strtoll(&(tmp[1]), NULL, 0) - ff;
        break;
      case 'n': /* numframes */
        nf = strtoll(optarg, NULL, 0);
        break;
      case 'p':
        if (strlen(optarg) < (F_LEN - 6))
          strcpy(precision, optarg);
        else
          fprintf(stderr, "Ignoring exceedingly long -p argument (%s).\n", optarg);
        break;
      case 'v':
        verbose = 1;
        break;
      case 'q':
        verbose = 0;
        break;
      case 'd':
        strncpy(delimeter, optarg, BUF_LEN - 1);
        delimeter[BUF_LEN - 1] = '\0';
        break;
      case 's':
        skip = strtoll(optarg, NULL, 0);
        break;
      case 'a':
        average = 1;
        fprintf(stderr, "Warning: Averaging not yet implimented.\n");
      case 'h':
      case '?':
      default:
        usage(argv[0]);
        break;
    }
  }

  if (numfields < 1)
  {
    fprintf(stderr, "Error: At least one field must be specified.\n");
    exit(-3);
  }

  if ((ff == -1) && (nf == -1))
  {
    fprintf(stderr, "Error: Both firstframe and numframes cannot be set to -1.\n");
    exit(-4);
  }

  for (int i = 0; i < strlen(precision); i++)
  {
    if (strchr(VALID_PRECISION_CHARS, precision[i]) == NULL)
    {
      fprintf(stderr, "Error: Invalid character (%c) found in precision string (%s).\n", 
              precision[i], precision);
      exit(-5);
    }
  }

  if (skip)
    skipping = 1;
  if (skip < 1)
    skip = 1;

  dirfile = dirfile_open(dirfile_name, GD_RDONLY);
  if (get_error(dirfile))
  {
    fprintf(stderr, "GetData error: %s\n", get_error_string(dirfile, char_buffer, BUF_LEN));
    dirfile_close(dirfile);
    exit(1);
  }

  /* Getdata would deal with these conditions for us (nf or ff set to -1)
   * but that's a race condition in the case that we're looking at live data 
   * if we're looping really slowly.  */
  if (nf == -1)
  {
    nf = get_nframes(dirfile) - ff;
    if (get_error(dirfile))
    {
      fprintf(stderr, "GetData error: %s\n", get_error_string(dirfile, char_buffer, BUF_LEN));
      dirfile_close(dirfile);
      exit(3);
    }
  }
  if (ff == -1)
  {
    ff = get_nframes(dirfile) - nf;
    if (get_error(dirfile))
    {
      fprintf(stderr, "GetData error: %s\n", get_error_string(dirfile, char_buffer, BUF_LEN));
      dirfile_close(dirfile);
      exit(3);
    }
  }

  for (int i = 0; i < numfields; i++) /* Get spfs and sanity checks for all fields */
  {
    fields[i].spf = get_spf(dirfile, fields[i].name);
    if (get_error(dirfile))
    {
      fprintf(stderr, "GetData error: %s\n", get_error_string(dirfile, char_buffer, BUF_LEN));
      dirfile_close(dirfile);
      exit(3);
    }

    if (!max_spf && fields[i].spf != max_spf)
      interpolate = 1;

    if (fields[i].spf > max_spf)
      max_spf = fields[i].spf;

    if (fields[i].spf < min_spf)
      min_spf = fields[i].spf;
  }
  if (interpolate && nf == 1 && min_spf == 1)
  {
    fprintf(stderr, "Error: Interpolation required, but at least one field has only 1 sample.\n");
    exit(-6);
  }

  if (verbose)
  {
    fprintf(stderr, "Reading %d field%s from %s\n", numfields, (numfields > 1) ? "s" : "", dirfile_name);
    fprintf(stderr, "First frame: %zd Number of frames: %zd\n", ff, nf);
  }

  for (int i = 0; i < numfields; i++) /* Read in all the fields */
  {
    n_want = nf * fields[i].spf;

    if (fields[i].type > 0)
    {
      fields[i].dbl = malloc(sizeof(double) * n_want);
      if (fields[i].dbl == NULL)
      {
        fprintf(stderr, "Unable to allocate memory.\n");
        dirfile_close(dirfile);
        exit(4);
      }
    } else {
      fields[i].i64 = malloc(sizeof(int64_t) * n_want);
      if (fields[i].i64 == NULL)
      {
        fprintf(stderr, "Unable to allocate memory.\n");
        dirfile_close(dirfile);
        exit(4);
      }
    }

    if (fields[i].type > 0)
      n_read = getdata(dirfile, fields[i].name, ff, 0, nf, 0, GD_FLOAT64, fields[i].dbl);
    else
      n_read = getdata(dirfile, fields[i].name, ff, 0, nf, 0, GD_INT64, fields[i].i64);
    if (get_error(dirfile))
    {
      fprintf(stderr, "GetData error: %s\n", get_error_string(dirfile, char_buffer, BUF_LEN));
      dirfile_close(dirfile);
      exit(5);
    }
  }

  for (int i = 0; i < numfields; i++) /* Generate format string with precision */
  {
    switch (fields[i].type)
    {
      case 1:
        snprintf(fields[i].format, F_LEN, "%%%sf", precision);
        break;
      case 2:
        snprintf(fields[i].format, F_LEN, "%%%sg", precision);
        break;
      case 3:
        snprintf(fields[i].format, F_LEN, "%%%sG", precision);
        break;
      case 4:
        snprintf(fields[i].format, F_LEN, "%%%se", precision);
        break;
      case 5:
        snprintf(fields[i].format, F_LEN, "%%%sE", precision);
        break;
      case -1:
        snprintf(fields[i].format, F_LEN, "0x%%%s" PRIx64, precision);
        break;
      case -2:
        snprintf(fields[i].format, F_LEN, "0X%%%s" PRIX64, precision);
        break;
      case -3:
        snprintf(fields[i].format, F_LEN, "%%%s" PRIi64, precision);
        break;
      case -4:
        snprintf(fields[i].format, F_LEN, "0%%%s" PRIo64, precision);
        break;
      default:
        break;
    }
  }

  for (size_t k = 0; k < nf; k += skip)
  {
    for (uint16_t j = 0; j < (skipping ? 1 : max_spf); j++)
    {
      for (int i = 0; i < numfields; i++)
      {
        if (fields[i].spf == max_spf || skipping)
        {
          if (fields[i].type > 0)
            printf(fields[i].format, fields[i].dbl[k * fields[i].spf + j]);
          else
            printf(fields[i].format, fields[i].i64[k * fields[i].spf + j]);
        }
        else /* need to interpolate: formula is y = y0 + (x-x0) * (y1-y0)/(x1-x0) */
        {    /*                                val= y0 +  diff  *     slope       */
             /* Note that if max_spfs isn't a multiple of all spfs then some of   */
             /* the data will only be approximated (except at frame boundaries).  */
          uint16_t prev_samp, next_samp, offset;
          double slope, diff, val;
          prev_samp = (uint16_t)floor((double)j * (double)fields[i].spf / (double)max_spf);
          next_samp = prev_samp + 1;
          diff = ((double)prev_samp + (double)(j % (max_spf / fields[i].spf)) /
                 (double)(max_spf / fields[i].spf)) - (double)prev_samp;
          if (k == (nf - 1) && next_samp == fields[i].spf) /* we're on ultimate sample. */
            offset = 1;                               /* so use slope from previous set */
          else
            offset = 0;
          if (fields[i].type > 0)
          {
            slope = (fields[i].dbl[k * fields[i].spf + next_samp - offset] -
                     fields[i].dbl[k * fields[i].spf + prev_samp - offset]) /
                    ((double)next_samp - (double)prev_samp);
            val = fields[i].dbl[k * fields[i].spf + prev_samp] + diff * slope;
            printf(fields[i].format, val);
          } else {
            slope = ((double)fields[i].i64[k * fields[i].spf + next_samp - offset] -
                     (double)fields[i].i64[k * fields[i].spf + prev_samp - offset]) /
                    ((double)next_samp - (double)prev_samp);
            val = (double)fields[i].i64[k * fields[i].spf + prev_samp] + diff * slope;
            printf(fields[i].format, (int64_t)val);
          }
        }
         
        if (i < (numfields - 1))
        {
          printf("%s", delimeter);
        } else {
          printf("\n");
        }
      }
    }
  }

  dirfile_close(dirfile);
  return 0;
}
