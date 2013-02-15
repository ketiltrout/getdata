function gd_alter_bit(D, field_code, in_fields, bitnum, numbits)
% GD_ALTER_BIT  Modify the metadata of a BIT field
%
%   GD_ALTER_BIT(DIRFILE,NAME,INPUT,BITNUM,NUMBITS)
%             modifies the metadata of the BIT field called NAME in the
%             dirfile specified by DIRFILE.  The input field is set to INPUT,
%             the first bit to BITNUM, and the number of bits to NUMBITS.
%
%   The DIRFILE object should have previously been created with GD_OPEN.
%
%   See the documentation on the C API function gd_alter_bit(3) in section 3
%   of the UNIX manual for more details.
%
%   See also GD_ALTER_ENTRY, GD_OPEN

  GD = getdata_constants();
  gd_alter_entry(D, field_code, struct('field_type', GD.BIT_ENTRY, ...
  'in_fields', {in_fields}, 'bitnum', bitnum, 'numbits', numbits), 0);
end

% Copyright (C) 2013 D. V. Wiebe
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
