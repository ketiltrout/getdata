dnl Copyright (C) 2011-2017 D. V. Wiebe
dnl
dnl llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
dnl
dnl This file is part of the GetData project.
dnl
dnl GetData is free software; you can redistribute it and/or modify it under
dnl the terms of the GNU Lesser General Public License as published by the
dnl Free Software Foundation; either version 2.1 of the License, or (at your
dnl option) any later version.
dnl
dnl GetData is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public License
dnl along with GetData; if not, write to the Free Software Foundation, Inc.,
dnl 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

m4_define(getdata_major,    0)
m4_define(getdata_minor,    10)
m4_define(getdata_revision, 0)
m4_define(getdata_extra,    [])
m4_define(getdata_pkg_extra,[])
m4_define(getdata_version,
          getdata_major.getdata_minor.getdata_revision[]getdata_extra)
m4_define(getdata_pkg_version,
          getdata_version[]getdata_pkg_extra)

dnl libgetdata current interface version
m4_define(getdata_iface_version,    8)
dnl libgetdata current interface implementation revision
m4_define(getdata_impl_revision,    0)
dnl libgetdata interface age (current interface - oldest supported interface)
m4_define(getdata_iface_age,        0)

dnl The rest of these are all CURRENT:REVISION:AGE

dnl libgetdata++ interface version info
m4_define(getdataxx_version, 7:0:0)

dnl libfgetdata interface version info
m4_define(fgetdata_version, 6:0:0)

dnl libf95getdata interface version info
m4_define(f95getdata_version, 7:0:0)

dnl libgetdata-matlab interface version info
m4_define(matlabgetdata_version, 2:0:2)
