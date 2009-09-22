/* Frameindex look-up */
#include "../src/getdata.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data = "data RAW FLOAT64 1\n";
  double d[1000];
  int i;

  mkdir(filedir, 0777);

  for (i = 0; i < 1000; ++i)
    d[i] = sqrt((i+600) / 500.);

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, d, 1000 * sizeof(double));
  close(i);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY | GD_VERBOSE);
  double f1 = get_framenum(D, "data", 1.09);
  double f2 = get_framenum(D, "data", 1.49);
  double f3 = get_framenum(D, "data", 1.79);
  int error = get_error(D);

  dirfile_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (error) {
    fprintf(stderr, "error=%i\n", error);
    return 1;
  }
  if (f1 > -5.96730894763914 || f1 < -5.96730894763916) {
    fprintf(stderr, "f1=%.15g\n", f1);
    return 1;
  }
  if (f2 > 510.050010695550 || f2 < 510.050010695548) {
    fprintf(stderr, "f2=%.15g\n", f2);
    return 1;
  }
  if (f3 > 1002.04807025293 || f3 < 1002.04807025291) {
    fprintf(stderr, "f3=%.15g\n", f3);
    return 1;
  }

  return 0;
}
