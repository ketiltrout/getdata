function gd_add_sindir(D, field_code, in1, in2, fragment_index)
% GD_ADD_INDIR  Add a SINDIR field
%
%   GD_ADD_INDIR(DIRFILE,NAME,INPUT1,INPUT2,FRAGMENT) 
%             adds a SINDIR field called NAME to the dirfile specified by
%             DIRFILE.  The input fields are INPUT1 and INPUT2 and the field
%             is added to the fragment indexed by FRAGMENT.
%
%   The DIRFILE object should have previously been created with GD_OPEN.
%
%   See the documentation on the C API function gd_add_sindir(3) in section 3
%   of the UNIX manual for more details.
%
%   See also GD_ADD, GD_MADD_INDIR, GD_OPEN

  GD = getdata_constants();
  gd_add(D, struct('field', field_code, 'field_type', GD.SINDIR_ENTRY, ...
  'fragment_index', fragment_index, 'in_fields', {{in1; in2}}));
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
