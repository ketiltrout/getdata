/* A test that used to fail from Matt -- there must be 10 fields, and
 * the last one must be 12 characters long. Fixed in SVN:124 */
#include <stdio.h>
#include <stdlib.h>
#include "../src/getdata.h"
#include <unistd.h>

int main (void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* a = __TEST__ "dirfile/a";
  const char* b = __TEST__ "dirfile/b";
  const char* c = __TEST__ "dirfile/c";
  const char* d = __TEST__ "dirfile/d";
  const char* e = __TEST__ "dirfile/e";
  const char* f = __TEST__ "dirfile/f";
  const char* g = __TEST__ "dirfile/g";
  const char* h = __TEST__ "dirfile/h";
  const char* i = __TEST__ "dirfile/i";
  const char* j = __TEST__ "dirfile/jjjjjjjjjjjj";

  DIRFILE *df;

  df = dirfile_open(filedir, GD_RDWR|GD_CREAT|GD_EXCL);
  dirfile_add_raw(df, "a", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "b", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "c", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "d", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "e", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "f", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "g", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "h", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "i", GD_FLOAT64, 1, 0);
  dirfile_add_raw(df, "jjjjjjjjjjjj", GD_FLOAT64, 1, 0);

  unlink(j);
  unlink(i);
  unlink(h);
  unlink(g);
  unlink(f);
  unlink(e);
  unlink(d);
  unlink(c);
  unlink(b);
  unlink(a);
  unlink(format);
  rmdir(filedir);
  return 0;
}
