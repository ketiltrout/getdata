function gd_put_sarray(D, field_code, data)
% GD_PUT_SARRAY  Modify SARRAY values
%
%   GD_PUT_SARRAY(DIRFILE,FIELD_CODE,DATA)
%             is equivalent to calling GD_PUT_SARRAY_SLICE(DIRFILE, ...
%                                                             FIELD_CODE,0,DATA)
%
%   See also GD_PUT_SARRAY_SLICE

  gd_put_sarray_slice(D, field_code, 0, data);
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
