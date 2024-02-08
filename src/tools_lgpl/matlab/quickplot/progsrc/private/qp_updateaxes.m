function qp_updateaxes(obj,evd)
%QP_UPDATEAXES Update axes properties after zoom/pan.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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

if ishandle(obj)
    switch class(obj)
        case 'matlab.graphics.axis.decorator.NumericRuler'
            ax = get(obj,'parent');
        otherwise % double or figure
            ax=get(obj,'currentaxes');
    end
else
    ax = evd.AffectedObject;
end

Items = qp_get_items(ax);
for i = 1:length(Items)
    refreshAction = getappdata(Items(i),'axesRefresh');
    if ~isempty(refreshAction)
        refreshArgs = getappdata(Items(i),'axesRefreshArguments');
        if ~isempty(refreshArgs)
            refreshAction(refreshArgs{:})
        end
    end
end

setappdata(ax,'xlimmode','manual')
setappdata(ax,'ylimmode','manual')
if ~isempty(ax)
    basicaxestype=getappdata(ax,'BasicAxesType');
    if ischar(basicaxestype)
        switch basicaxestype
            case {'LimitingFactorsAxes','LimitingFactorsAxes2'}
                if isequal(basicaxestype,'LimitingFactorsAxes2')
                    ax2=ax;
                    ax=getappdata(ax2,'LimitingFactorsAxes');
                    set(ax,'xlim',get(ax2,'xlim'))
                else
                    ax2 = getappdata(ax,'LimitingFactorsAxes');
                end
                set(ax,'xticklabelmode','auto','xtickmode','auto');
                tick(ax,'x','autodate');
                set(ax2,'xlim',get(ax,'xlim'), ...
                    'ylim',getappdata(ax2,'YLim'), ...
                    'xtick',get(ax,'xtick'), ...
                    'xticklabel',get(ax,'xticklabel'))
                set(ax,'xticklabel','')
            otherwise
                setaxesprops(ax)
        end
    end
end
lat = getappdata(ax,'linkedaxestype');
if strcmp(lat,'SecondY')
    ax2 = getappdata(ax,'linkedaxes');
    set(ax2,'xlim',get(ax,'xlim'))
    setaxesprops(ax2)
end
mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
UD=getappdata(mfig,'QPHandles');
qp_plotmanager('refreshaxprop',UD)