/* Copyright (C) 2016, 2017 D. V. Wiebe
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
int main(void)
{
#ifdef ENC_SKIP_TEST
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data" ENC_SUFFIX;
  uint32_t c;
  uint32_t d[100];
  off_t nf;
  int r = 0, e;
  size_t n1, n2, n3;
  unsigned i;
  DIRFILE *D;

#define NI 7
  const off_t offs[] = { 5, 12, 15, BIG_JUMP + 10, 8, BIG_JUMP + 20,
    BIG_JUMP + 12 };

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | ENC_ENCODED | GD_LITTLE_ENDIAN
      | GD_VERBOSE | GD_CREAT | GD_EXCL);
  gd_add_spec(D, "data RAW UINT32 1", 0);
  for (i = 0; i < NI; ++i) {
    c = offs[i];
    n1 = gd_putdata(D, "data", offs[i], 0, 0, 1, GD_UINT32, &c);
    CHECKIi(i, n1, 1);

    e = gd_error(D);
    CHECKIi(i, e, GD_E_OK);
  }
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  nf = gd_nframes(D);
  CHECKU(nf, BIG_JUMP + 21);

  n2 = gd_getdata(D, "data", 0, 0, 0, 100, GD_UINT32, d);
  CHECKI(n2, 100);

  if (n2 > 100)
    n2 = 100;
  for (i = 0; i < n2; ++i) {
    if (i == 5 || i == 8 || i == 12 || i == 15)
      CHECKXi(i, d[i], i);
    else
      CHECKXi(i, d[i], 0);
  }

  n3 = gd_getdata(D, "data", 0, BIG_JUMP, 0, 100, GD_UINT32, d);
  CHECKI(n3, 21);

  for (i = 0; i < n3; ++i) {
    if (i == 10 || i == 12 || i == 20)
      CHECKXi(BIG_JUMP + i, d[i], BIG_JUMP + i);
    else
      CHECKXi(BIG_JUMP + i, d[i], 0);
  }

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
