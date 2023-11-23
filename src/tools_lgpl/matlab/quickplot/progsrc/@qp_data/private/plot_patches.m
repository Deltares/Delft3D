function h = plot_patches(Parent,Data,varargin)
%PLOT_PATCHES Plot patches with value-dependent colors.

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

PO = initPlotOption;
PO = addPlotOption(PO,'Color','color',[0 0 0]);
PO = addPlotOption(PO,'LineWidth','float',0.001);
PO = addPlotOption(PO,'LineStyle','list','none', ...
    {'none','-','--',':','.-'});
PO = addPlotOption(PO,'Marker','list','none', ...
    {'none','+','o','*','.','x','s','d','v','^','>','<','p','h'});
PO = addPlotOption(PO,'Level','float',0);

if isequal(Parent,'getoptions')
    h = PO;
    return
else
    Ops = expandPlotOptions(PO,varargin{:});
end

x = Data.Grid.X.Data;
y = Data.Grid.Y.Data;
b = squeeze(Data.Value.Data);
switch Data.Value.Stagger
    case 'Nodes2D'
        b = (b(1:end-1,1:end-1)+b(2:end,1:end-1)+b(1:end-1,2:end)+b(2:end,2:end))/4;
    case {'Faces2D','HFaces3D'}
        b = b(2:end,2:end);
end
[m,n] = ndgrid(1:size(b,1),1:size(b,2));
i = sub2ind(size(x),m(:),n(:));
p = [i i+1 i+size(x,1)+1 i+size(x,1)]';
b = b(:)';
remove = any(isnan(x(p)),1);
p(:,remove) = [];
b(:,remove) = [];
xp = x(p);
yp = y(p);
h = patch(xp,yp, ...
    repmat(Ops.Level,size(xp)),b(:)', ...
    'parent',Parent, ...
    'edgecolor',Ops.Color, ...
    'facecolor','flat', ...
    'linewidth',Ops.LineWidth, ...
    'linestyle',Ops.LineStyle, ...
    'marker',Ops.Marker);
