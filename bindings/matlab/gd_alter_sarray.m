function gd_alter_carray(D, field_code, array_len)
% GD_ALTER_SARRAY  Modify the metadata of a SARRAY field
%
%   GD_ALTER_SARRAY(DIRFILE,NAME,LEN)
%             modifies the metadata of the SARRAY field called NAME in the
%             dirfile specified by DIRFILE, setting the the length of the
%             SARRAY to LEN.
%
%   The DIRFILE object should have previously been created with GD_OPEN.
%
%   See the documentation on the C API function gd_alter_carray(3) in section 3
%   of the UNIX manual for more details.
%
%   See also GD_ALTER_ENTRY, GD_OPEN, GETDATA_CONSTANTS

  GD = getdata_constants();
  gd_alter_entry(D, field_code, struct('field_type', GD.SARRAY_ENTRY, ...
  'array_len', array_len), 0);
end

% Copyright (C) 2014 D. V. Wiebe
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This file is part of the GetData project.
%
% GetData is free software; you can redistribute it and/or modify it under
% the terms of the GNU Lesser General Public License as published by the
% Free Software Foundation; either version 2.1 of the License, or (at your
% option) any later version.
%
% GetData is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
% License for more details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with GetData; if not, write to the Free Software Foundation, Inc.,
% 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
