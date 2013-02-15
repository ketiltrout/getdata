function gd_madd_window(D, parent, field_code, in_field1, in_field2, windop, ...
threshold)
% GD_MADD_WINDOW  Add a WINDOW metafield
%
%   GD_MADD_WINDOW(DIRFILE,PARENT,NAME,INPUT1,INPUT2)
%             adds a WINDOW metafield called NAME under PARENT to the dirfile
%             specified by DIRFILE.  The data input field is INPUT1, and the
%             mask field is INPUT2. The operator is specified by WINDOP, which
%             should be one of the GD.WINDOP_... members provided by
%             GETDATA_CONSTANTS, and the threshold is THRESHOLD.
%
%   The DIRFILE object should have previously been created with GD_OPEN.
%
%   See the documentation on the C API function gd_madd_window(3) in section 3
%   of the UNIX manual for more details.
%
%   See also GD_ADD_WINDOW, GD_MADD, GD_OPEN, GETDATA_CONSTANTS

  GD = getdata_constants();
  gd_madd(D, struct('field', field_code, 'field_type', GD.WINDOW_ENTRY, ...
  'in_fields', {{in_field1; in_field2}}, 'windop', windop, ...
  'threshold', threshold), parent);
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
