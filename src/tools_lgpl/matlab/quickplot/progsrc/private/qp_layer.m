function str = qp_layer(Z)
%QP_LAYER Create standard string for a QUICKPLOT vertical layer specification.

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2023 Stichting Deltares.
%
%   This library is free software; you can redistribute it and/or
%   modify it under the terms of the GNU Lesser General Public
%   License as published by the Free Software Foundation version 2.1.
%
%   This library is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%   Lesser General Public License for more details.
%
%   You should have received a copy of the GNU Lesser General Public
%   License along with this library; if not, see <http://www.gnu.org/licenses/>.
%
%   contact: delft3d.support@deltares.nl
%   Stichting Deltares
%   P.O. Box 177
%   2600 MH Delft, The Netherlands
%
%   All indications and logos of, and references to, "Delft3D" and "Deltares"
%   are registered trademarks of Stichting Deltares, and remain the property of
%   Stichting Deltares. All rights reserved.
%
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

if iscell(Z)
    switch Z{1}
        case 'z'
            str = sprintf('z = %g m',Z{2});
        case 'dz_below_max'
            if isequal(Z{2},0)
                str = 'at surface';
            else
                str = sprintf('%g m below surface',Z{2});
            end
        case 'dz_above_min'
            if isequal(Z{2},0)
                str = 'at bed';
            else
                str = sprintf('%g m above bed',Z{2});
            end
        otherwise
            str = 'unknown level';
    end
else
    str = sprintf('layer %i',Z);
end