function gd_add_polynom(D, field_code, in_field, a, fragment_index);
% GD_ADD_POLYNOM  Add a POLYNOM field
%
%   GD_ADD_POLYNOM(DIRFILE,NAME,INPUT,A,FRAGMENT)
%             adds a POLYNOM field called NAME to the dirfile specified by
%             DIRFILE.  The input field is INPUT and the co-efficients are
%             given in the numerical array A, which may be complex valued.
%             The field is added to the fragment indexed by FRAGMENT.
%
%   The DIRFILE object should have previously been created with GD_OPEN.
%
%   See the documentation on the C API function gd_add_polynom(3) in section 3
%   of the UNIX manual for more details.
%
%   See also GD_ADD, GD_MADD_POLYNOM, GD_OPEN

  GD = getdata_constants();
  gd_add(D, struct('field', field_code, 'field_type', GD.POLYNOM_ENTRY, ...
  'fragment_index', fragment_index, 'in_fields', {in_field}, 'a', a));
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
