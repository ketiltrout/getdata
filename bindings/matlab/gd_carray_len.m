function carray_len = gd_carray_len(D,field_code)
% GD_CARRAY_LEN  Report the length of a CARRAY field
%
%   L = GD_CARRAY_LEN(DIRFILE,FIELD_CODE)
%             this function is deprecated.  Use GD_ARRAY_LEN instead.

  warning('GD_CARRAY_LEN is deprecated.  Use GD_ARRAY_LEN instead.');

  gd_carray_len = gd_array_len(D, field_code);
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
