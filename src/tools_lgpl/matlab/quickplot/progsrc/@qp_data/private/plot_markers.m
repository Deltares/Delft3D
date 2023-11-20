function h = plot_markers(Parent,Data,varargin)
%PLOT_MARKERS Plot patches with value-dependent colors.

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
PO = addPlotOption(PO,'EdgeColor','flatcolor',[0 0 0]);
PO = addPlotOption(PO,'FaceColor','flatcolor','flat');
PO = addPlotOption(PO,'FontColor','color','k');
PO = addPlotOption(PO,'BoxEdgeColor','nonecolor','none');
PO = addPlotOption(PO,'BoxFaceColor','nonecolor','none');
PO = addPlotOption(PO,'Marker','list','o', ...
    {'none','+','o','*','.','x','s','d','v','^','>','<','p','h'});
PO = addPlotOption(PO,'MarkerSize','posfloat',6);
PO = addPlotOption(PO,'ShowValues','logical',false);
PO = addPlotOption(PO,'FontProps','font',default_font);
PO = addPlotOption(PO,'HorizontalAlignment','list','left', ...
    {'left','center','right'});
PO = addPlotOption(PO,'VerticalAlignment','list','middle', ...
    {'top','cap','middle','baseline','bottom'});
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
        % no actions needed
    case {'Faces2D','HFaces3D'}
        x = (x(1:end-1,1:end-1)+x(2:end,1:end-1)+x(1:end-1,2:end)+x(2:end,2:end))/4;
        y = (y(1:end-1,1:end-1)+y(2:end,1:end-1)+y(1:end-1,2:end)+y(2:end,2:end))/4;
        b = b(2:end,2:end);
end
x = x(:);
y = y(:);
b = b(:);
remove = isnan(x);
x(remove) = [];
y(remove) = [];
z = repmat(Ops.Level,size(x));
b(remove) = [];
if Ops.ShowValues
    ht = zeros(1,length(x));
    for i = 1:length(x)
        Str = sprintf('%g',b(i));
        ht(i) = text(x(i),y(i),z(i), ...
            Str, ...
	        Ops.FontProps, ...
	        'HorizontalAlignment',Ops.HorizontalAlignment, ...
	        'VerticalAlignment',Ops.VerticalAlignment, ...
            'Color',Ops.FontColor, ...
            'BackgroundColor',Ops.BoxFaceColor, ...
            'EdgeColor',Ops.BoxEdgeColor);
    end
else
    ht = [];
end
if isequal(Ops.EdgeColor','flat') || ...
        isequal(Ops.FaceColor,'flat')
    hm = patch('vertices',[x y z], ...
        'facevertexcdata',b, ...
        'faces',(1:length(b))', ...
        'parent',Parent, ...
        'facecolor','none', ...
        'linestyle','none', ...
        'marker',Ops.Marker, ...
        'markersize',Ops.MarkerSize, ...
        'markeredgecolor',Ops.EdgeColor, ...
        'markerfacecolor',Ops.FaceColor);
else
    hm = line(x,y,z, ...
        'parent',Parent, ...
        'linestyle','none', ...
        'marker',Ops.Marker, ...
        'markersize',Ops.MarkerSize, ...
        'markeredgecolor',Ops.EdgeColor, ...
        'markerfacecolor',Ops.FaceColor);
end
h = [hm ht];
