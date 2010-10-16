/* Field sort test for dirfile_add */
#include "test.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int r = 0;

void CheckSPF(DIRFILE *D, const char* f, int v)
{
  gd_entry_t e;

  gd_entry(D, f, &e);

  if (gd_error(D)) {
    r = 1;
    return;
  }

  CHECKS(e.field, f);
  CHECKI(e.EN(raw,spf), v);
  gd_free_entry_strings(&e);
}

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

  DIRFILE *D;

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_VERBOSE);
  gd_add_raw(D, "d", GD_FLOAT64, 1, 0);
  gd_add_raw(D, "b", GD_FLOAT64, 2, 0);
  gd_add_raw(D, "h", GD_FLOAT64, 3, 0);
  gd_add_raw(D, "e", GD_FLOAT64, 4, 0);
  gd_add_raw(D, "g", GD_FLOAT64, 5, 0);
  gd_add_raw(D, "c", GD_FLOAT64, 6, 0);
  gd_add_raw(D, "k", GD_FLOAT64, 7, 0);
  gd_add_raw(D, "a", GD_FLOAT64, 8, 0);
  gd_add_raw(D, "f", GD_FLOAT64, 9, 0);
  gd_add_raw(D, "i", GD_FLOAT64, 10, 0);
  gd_add_raw(D, "j", GD_FLOAT64, 11, 0);

  /* The idea here is that a field look-up will fail unless the library has
   * added the field in the correct location */
  CheckSPF(D, "a", 8);
  CheckSPF(D, "b", 2);
  CheckSPF(D, "c", 6);
  CheckSPF(D, "d", 1);
  CheckSPF(D, "e", 4);
  CheckSPF(D, "f", 9);
  CheckSPF(D, "g", 5);
  CheckSPF(D, "h", 3);
  CheckSPF(D, "i", 10);
  CheckSPF(D, "j", 11);
  CheckSPF(D, "k", 7);
  gd_close(D);

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
