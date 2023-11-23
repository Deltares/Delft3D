function h = plot_edges(Parent,Data,varargin)
%PLOT_EDGES Plot logical quantity at Struct2D Edges2D.

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
PO = addPlotOption(PO,'LineWidth','float',0.001);
PO = addPlotOption(PO,'LineStyle','list','-', ...
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

X = Data.Grid.X.Data;
Y = Data.Grid.Y.Data;
XDam = [zeros(size(X,1),1), Data.Value(1).Data];
YDam = [zeros(1,size(X,2)); Data.Value(2).Data];

h = thindam(X,Y,XDam,YDam, ...
    'parent',Parent);
set(h,'zdata',repmat(Ops.Level,size(get(h,'ydata'))), ...
    'color',Ops.Color, ...
    'linewidth',Ops.LineWidth, ...
    'linestyle',Ops.LineStyle, ...
    'marker',Ops.Marker)
