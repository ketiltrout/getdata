function gd_alter_indir(D, field_code, in1, in2)
% GD_ALTER_INDIR  Modify the metadata of an INDIR field
%
%   GD_ALTER_INDIR(DIRFILE,NAME,INPUT1,INPUT2)
%             modifies the metadata of the INDIR field called NAME in the
%             dirfile specified by DIRFILE.  The input fields are set to INPUT1,
%             and INPUT2, if non-zero.
%
%   The DIRFILE object should have previously been created with GD_OPEN.
%
%   See the documentation on the C API function gd_alter_indir(3) in section
%   3 of the UNIX manual for more details.
%
%   See also GD_ALTER_ENTRY, GD_OPEN

  GD = getdata_constants();
  gd_alter_entry(D, field_code, struct('field_type', GD.INDIR_ENTRY, ...
  'in_fields', {{in1; in2}}), 0);
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
