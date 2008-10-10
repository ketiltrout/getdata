/* Field sort test for dirfile_add */
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
  const char* j = __TEST__ "dirfile/j";
  const char* k = __TEST__ "dirfile/k";
  int r = 0;

  DIRFILE *D;

  D = dirfile_open(filedir, GD_RDWR|GD_CREAT|GD_EXCL);
  dirfile_add_raw(D, "d", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "b", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "h", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "e", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "g", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "c", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "k", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "a", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "f", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "i", GD_FLOAT64, 1, 0);
  dirfile_add_raw(D, "j", GD_FLOAT64, 1, 0);

  const char** field_list = get_field_list(D);

  if (get_error(D))
    r = 1;
  else if (field_list == NULL)
    r = 1;
  else if (field_list[0][0] != 'a')
    r = 1;
  else if (field_list[1][0] != 'b')
    r = 1;
  else if (field_list[2][0] != 'c')
    r = 1;
  else if (field_list[3][0] != 'd')
    r = 1;
  else if (field_list[4][0] != 'e')
    r = 1;
  else if (field_list[5][0] != 'f')
    r = 1;
  else if (field_list[6][0] != 'g')
    r = 1;
  else if (field_list[7][0] != 'h')
    r = 1;
  else if (field_list[8][0] != 'i')
    r = 1;
  else if (field_list[9][0] != 'j')
    r = 1;
  else if (field_list[10][0] != 'k')
    r = 1;

  unlink(k);
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
  return r;
}
