function qp_figaspect(fig,wh_pix)
%QP_FIGASPECT Reshapes figure to match paper size.

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

fu = get(fig,'units');
set(fig,'units','pixels')

opos = get(fig,'outerposition');
ipos = get(fig,'position');
bnd  = opos-ipos;

% new inner size
if nargin>1
    npos(3:4) = wh_pix;
else
    sz = get(fig,'papersize');
    npos(3:4) = sz*sqrt(prod(ipos(3:4))/prod(sz));
end

%fit to screen
pxmon = qp_getscreen(fig);
if any(npos(3:4)+bnd(3:4)>pxmon(3:4))
    % figure too big, so make it fit
    mxsz      = pxmon(3:4)-bnd(3:4);
    npos(3:4) = round(min(mxsz./npos(3:4))*npos(3:4));
else
    npos      = round(npos);
end

% new inner offset
npos(1:2) = ipos(1:2)+(ipos(3:4)-npos(3:4))/2;

% new outer position
opos       = npos+bnd;

% the figure fits (now), but the offset may still put it partly off the screen
opos(1:2) = max(opos(1:2),pxmon(1:2));
opos(1:2) = min(opos(1:2)+opos(3:4),pxmon(1:2)+pxmon(3:4)) - opos(3:4);
set(fig,'outerposition',opos)

set(fig,'units',fu)
