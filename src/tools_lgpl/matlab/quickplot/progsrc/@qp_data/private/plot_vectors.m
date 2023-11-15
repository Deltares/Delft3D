function h = plot_vectors(Parent,Data,varargin)
%PLOT_VECTORS Plot vectors in 2DH.

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
PO = addPlotOption(PO,'Color','color',[0 0 1]);
PO = addPlotOption(PO,'LineWidth','float',0.5);
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
u = squeeze(Data.Value(1).Data);
v = squeeze(Data.Value(2).Data);
switch Data.Value(1).Stagger
    case 'Edges2D-d1'
        x = (x(1:end-1,1:end-1)+x(2:end,1:end-1)+x(1:end-1,2:end)+x(2:end,2:end))/4;
        y = (y(1:end-1,1:end-1)+y(2:end,1:end-1)+y(1:end-1,2:end)+y(2:end,2:end))/4;
        u = (u(1:end-1,2:end)+u(2:end,2:end))/2;
        v = (v(2:end,1:end-1)+v(2:end,2:end))/2;
    case 'Faces2D-d1'
        x = (x(1:end-1,1:end-1)+x(2:end,1:end-1)+x(1:end-1,2:end)+x(2:end,2:end))/4;
        y = (y(1:end-1,1:end-1)+y(2:end,1:end-1)+y(1:end-1,2:end)+y(2:end,2:end))/4;
        u = u(2:end,2:end);
        v = v(2:end,2:end);
end
x = x(:);
y = y(:);
u = u(:);
v = v(:);
remove = isnan(x);
x(remove) = [];
y(remove) = [];
u(remove) = [];
v(remove) = [];
if matlabversionnumber<7
    h = quiver(x,y,u,v);
    set(h, ...
        'color',Ops.Color, ...
        'linewidth',Ops.LineWidth);
    set(h(1), ...
        'marker',Ops.Marker);
else
    h = quiver(x,y,u,v, ...
        'color',Ops.Color, ...
        'linewidth',Ops.LineWidth, ...
        'marker',Ops.Marker);
end

