function qp_gridviewhelper(UD,Info,DomainNr,Props,fld)
% QP_GRIDVIEWHELPER

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

F = UD.GridView.Fig;
UseGrid = get(F,'userdata');
i_grd = Props(fld).UseGrid;
if isfield(Props,'Geom')
    ihyphen = strfind(Props(fld).Geom,'-');
else
    ihyphen = [];
end
if ~isempty(ihyphen)
    NewLoc = Props(fld).Geom(ihyphen+1:end);
else
    NewLoc = 'NODE';
end
UseGridNew={Info.Name,DomainNr,i_grd,NewLoc};
if ~iscell(UseGrid) || (~isequal(UseGrid(1:3),UseGridNew(1:3)) && UseGridNew{2}>0)
    set(F,'name','Grid View: updating grid ...')
    %
    % read grid
    [Chk,GRID]=qp_getdata(Info,DomainNr,Props(i_grd),'grid');
    [GRID.ValLocation] = deal(NewLoc);
    %
    % push grid to gridview (which triggers update)
    % TODO: Change selection?
    qp_gridview('setgrid',F,GRID)
    set(F,'name','Grid View')
    set(F,'userdata',UseGridNew)
elseif iscell(UseGrid) && length(UseGrid)>3 && ~isequal(UseGrid{4},NewLoc)
    % change grid location
    [Range,RangeMax] = qp_gridview('setloc',F,NewLoc);
    switch Range.Type
        case 'pwline'
            set(UD.MainWin.EditMN,'userdata',Range.Range,'string','') % set string to empty to force update
            set(UD.MainWin.AllM,'userdata',{0 Range.Range(1) RangeMax})
        case 'point'
            set(UD.MainWin.AllM,'userdata',{0 Range.Range(1) RangeMax(1)})
            if length(RangeMax)>1
                set(UD.MainWin.AllN,'userdata',{0 Range.Range(2) RangeMax(2)})
            end
        case 'range'
            if iscell(Range.Range)
                set(UD.MainWin.AllM,'userdata',{isequal(Range.Range{1},1:RangeMax(1)) Range.Range{1} RangeMax(1)})
                if length(RangeMax)>1
                    set(UD.MainWin.AllN,'userdata',{isequal(Range.Range{2},1:RangeMax(2)) Range.Range{2} RangeMax(2)})
                end
            else
                r = Range.Range;
                set(UD.MainWin.AllM,'userdata',{r(1)==1 && r(2)==RangeMax(1) r(1):r(2) RangeMax(1)})
                if length(RangeMax)>1
                    set(UD.MainWin.AllN,'userdata',{r(3)==1 && r(4)==RangeMax(2) r(3):r(4) RangeMax(2)})
                end
            end
    end
end
