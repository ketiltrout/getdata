function gd_madd_mplex(D, parent, field_code, in_field1, in_field2, ...
count_val, period)
% GD_MADD_MPLEX  Add a MPLEX metafield
%
%   GD_MADD_MPLEX(DIRFILE,PARENT,NAME,INPUT1,INPUT2,COUNT_VAL,COUNT_MAX)
%             adds a MPLEX metafield called NAME under PARENT to the dirfile
%             specified by DIRFILE.  The input data field is INPUT1, the index
%             field is INPUT2.  The target value is COUNT_VAL and the maximum
%             value of INPUT2 is COUNT_MAX.
%
%   The DIRFILE object should have previously been created with GD_OPEN.
%
%   See the documentation on the C API function gd_madd_mplex(3) in section 3
%   of the UNIX manual for more details.
%
%   See also GD_ADD_MPLEX, GD_MADD, GD_OPEN

  GD = getdata_constants();
  gd_madd(D, struct('field', field_code, 'field_type', GD.MPLEX_ENTRY, ...
  'in_fields', {{in_field1; in_field2}}, 'count_val', count_val, ...
  'period', period), parent);
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
