/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Attempt to delete a field */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data = "data CONST UINT8 13\n"
    "raw RAW UINT8 data\n"
    "lincom LINCOM raw data data\n"
    "polynom POLYNOM lincom data data\n"
    "recip RECIP polynom data\n"
    "bit BIT recip data data\n"
    "phase PHASE bit data\n"
    "window WINDOW phase bit GT data\n"
    "mplex MPLEX window phase data data\n"
    ;
  int fd, ret, error, r = 0;
  unsigned int spf;
  int nb, bn, s, cv, p;
  double m0, b0, a0, a1, d, t;
  DIRFILE *D;
  gd_entry_t entry;

  rmdirfile();
  mkdir(filedir, 0700);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR);
  ret = gd_delete(D, "data", GD_DEL_DEREF);
  error = gd_error(D);

  /* check */
  spf = gd_spf(D, "raw");
  gd_entry(D, "lincom", &entry);
  m0 = entry.EN(lincom,m[0]);
  b0 = entry.EN(lincom,b[0]);
  gd_free_entry_strings(&entry);
  gd_entry(D, "polynom", &entry);
  a0 = entry.EN(polynom,a[0]);
  a1 = entry.EN(polynom,a[1]);
  gd_free_entry_strings(&entry);
  gd_entry(D, "recip", &entry);
  d = entry.EN(recip,dividend);
  gd_free_entry_strings(&entry);
  gd_entry(D, "bit", &entry);
  nb = entry.EN(bit,numbits);
  bn = entry.EN(bit,bitnum);
  gd_free_entry_strings(&entry);
  gd_entry(D, "phase", &entry);
  s = (int)entry.EN(phase,shift);
  gd_free_entry_strings(&entry);
  gd_entry(D, "window", &entry);
  t = entry.EN(window,threshold.r);
  gd_free_entry_strings(&entry);
  gd_entry(D, "mplex", &entry);
  cv = entry.EN(mplex,count_val);
  p = entry.EN(mplex,period);
  gd_free_entry_strings(&entry);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKU(spf, 13);
  CHECKF(m0, 13.);
  CHECKF(b0, 13.);
  CHECKF(a0, 13.);
  CHECKF(a1, 13.);
  CHECKF(d, 13.);
  CHECKI(nb, 13);
  CHECKI(bn, 13);
  CHECKI(s, 13);
  CHECKF(t, 13.);
  CHECKI(cv, 13);
  CHECKI(p, 13);
  CHECKI(ret, 0);

  return r;
}
