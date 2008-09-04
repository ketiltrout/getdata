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
# GetData is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GetData; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
#
AUTOMAKE_OPTIONS = foreign
INCLUDES = -Wall -I$(srcdir)/.. -I$(top_srcdir)/src -D__TEST__=\"$@\"
LDADD=../libdirfile.la
TESTS=entry getdata nframes putdata spf

entry_SOURCES=entry.cpp
getdata_SOURCES=getdata.cpp
nframes_SOURCES=nframes.cpp
putdata_SOURCES=putdata.cpp
spf_SOURCES=spf.cpp

check_PROGRAMS=${TESTS}

clean-local:
	rm -rf *odirfile
	rm -rf *~
