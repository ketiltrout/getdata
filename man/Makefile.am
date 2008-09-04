# (C) 2008 D. V. Wiebe
#
##########################################################################
#
# This file is part of the GetData project.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# GetData is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with GetData; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
AUTOMAKE_OPTIONS = foreign

if HAVE_OFF64_T
LFS_transitional_mans = getdata64.3 get_nframes64.3 putdata64.3
else
LFS_transitional_mans =
endif

dist_man_MANS = dirfile_close.3 dirfile_flush.3 dirfile_open.3 getdata.3 \
								get_entry.3 get_error_string.3 get_field_list.3 get_nfields.3 \
								get_nframes.3 get_spf.3 putdata.3 dirfile.5 dirfile-format.5 \
								$(LFS_transitional_mans)

clean-local:
	rm -rf *~ 
