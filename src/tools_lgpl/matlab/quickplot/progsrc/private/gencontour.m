function hNew=gencontour(hOld,Ops,Parent,X,Y,Z,Thresholds)
%GENCONTOUR Generic plot routine for contour plot.

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

delete(hOld);

Thresholds(Thresholds==-inf)=-realmax;
Thresholds(Thresholds==inf)=realmax;
switch Ops.presentationtype
    case 'contour lines'
        [dummy,hNew]=contourfcorr('line',X,Y,Z,Thresholds);
        set(hNew,'color',Ops.colour)
        set(hNew, ...
            'linewidth',Ops.linewidth, ...
            'linestyle',Ops.linestyle, ...
            'marker',Ops.marker, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
    case 'coloured contour lines'
        [dummy,hNew]=contourfcorr('cline',X,Y,Z,Thresholds);
        remap(hNew,Thresholds)
        set(hNew, ...
            'linewidth',Ops.linewidth, ...
            'linestyle',Ops.linestyle, ...
            'marker',Ops.marker, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
    case 'contour patches'
        if any(~isnan(Z(:)))
            [dummy,hNew]=contourfcorr(X,Y,Z,Thresholds);
            remap(hNew,Thresholds)
        else
            hNew=patch(1,1,1,'xdata',[],'ydata',[],'cdata',[]);
        end
        set(hNew,'edgecolor','none');
        hNew=flipud(hNew);
    case 'contour patches with lines'
        if any(~isnan(Z(:)))
            [dummy,hNew]=contourfcorr(X,Y,Z,Thresholds);
            remap(hNew,Thresholds)
        else
            hNew=patch(1,1,1,'xdata',[],'ydata',[],'cdata',[]);
        end
        set(hNew,'edgecolor',Ops.colour, ...
            'linewidth',Ops.linewidth, ...
            'linestyle',Ops.linestyle, ...
            'marker',Ops.marker, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
        hNew=flipud(hNew);
end
set(hNew,'parent',Parent)
if ~isnan(min(Thresholds)) && ~strcmp(Ops.presentationtype,'contour lines')
    if length(Thresholds)>1
        set(Parent,'clim',[1 length(Thresholds)]);
    else
        set(Parent,'clim',[1 2])
    end
end

function remap(hNew,Thresholds)
for i=1:length(hNew)
    hNewC=get(hNew(i),'FaceVertexCData');
    ii = max(find(Thresholds<=hNewC(1)));
    if ~isempty(ii)
        set(hNew(i),'FaceVertexCData',repmat(ii,size(hNewC)))
    end
end
