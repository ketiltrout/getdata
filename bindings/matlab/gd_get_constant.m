function n = gd_get_constant(D, field_code, varargin)
% GD_GET_CONSTANT  Retrieve CONST data
%
%   N = GD_GET_CONSTANT(DIRFILE,FIELD,TYPE)
%             is equivalent to N = GD_GET_CARRAY_SLICE(DIRFILE,FIELD,0,1,TYPE)
%
%   N = GD_GET_CONSTANT(DIRFILE,FIELD)
%             is equivalent to N = GD_GET_CARRAY_SLICE(DIRFILE,FIELD,0,1)
%
%   See also GD_GET_CARRAY_SLICE

if (numel(varargin) > 0)
  n = gd_get_carray_slice(D, field_code, 0, 1, varargin{1});
else
  n = gd_get_carray_slice(D, field_code, 0, 1);
end
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
