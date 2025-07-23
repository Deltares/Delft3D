function hNew = qp_plot_line(hNew, Parent, x, y, z, val, Ops)
%QP_PLOT_LINE Plot function of QuickPlot for line, particle tracks, and polygons.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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

if isempty(val)
    hNew = plotline_uniform(hNew, Parent, x, y, z, Ops);
else
    hNew = plotline_color(hNew, Parent, x, y, z, val, Ops);
end

function hNew = plotline_uniform(hNew, Parent, x, y, z, Ops)
if isempty(hNew)
    if isempty(z)
        crds = {x, y};
    else
        crds = {x, y, z};
    end
    hNew = line(crds{:}, ...
        'parent', Parent, ...
        Ops.LineParams{:});
    set(Parent, 'layer', 'top')
elseif ishandle(hNew)
    if isempty(z)
        crds = {'xdata', x, 'ydata', y};
    else
        crds = {'xdata', x, 'ydata', y, 'zdata', z};
    end
    set(hNew, crds{:});
else
    hNew = [];
end

function hNew = plotline_color(hNew, Parent, X, Y, Z, Val, Ops)
if ~isempty(hNew)
    delete(hNew)
end
o = 0;
fill = ~strcmp(Ops.facecolour,'none');
for c=1:size(Val,2)
    vNaN=isnan(Val(:,c));
    if any(vNaN)
        bs=findseries(~vNaN);
    else
        bs=[1 length(vNaN)];
    end
    for i=1:size(bs,1)
        from=bs(i,1);
        to=bs(i,2);
        ecol='interp';
        fcol='none';
        if fill && X(from,c)==X(to,c) && ...
                Y(from,c)==Y(to,c)
            ecol='none';
            fcol='flat';
            vl=from;
        elseif from>1
            from=from-1;
            X(from,c)=NaN;
            if ~isempty(Y)
                Y(from,c)=NaN;
            end
            if ~isempty(Z)
                Z(from,c)=NaN;
            end
            Val(from,c)=NaN;
            vl=from:to;
        else
            to=to+1;
            if to>size(Val,1)
                cfill = ':';
            else
                cfill = c;
            end
            X(to,cfill)=NaN;
            if ~isempty(Y)
                Y(to,cfill)=NaN;
            end
            if ~isempty(Z)
                Z(to,cfill)=NaN;
            end
            Val(to,cfill)=NaN;
            vl=from:to;
        end
        switch Ops.basicaxestype
            case 'X-Z'
                xy = {X(from:to,c), Z(from:to,c)};
            case 'X-Y-Z'
                xy = {X(from:to,c), Y(from:to,c), Z(from:to,c)};
            otherwise
                xy = {X(from:to,c), Y(from:to,c)};
        end
        hNew(o+i)=patch(xy{:}, ...
            Val(vl,c), ...
            'edgecolor',ecol, ...
            'facecolor',fcol, ...
            'linestyle',Ops.linestyle, ...
            'linewidth',Ops.linewidth, ...
            'marker',Ops.marker, ...
            'markersize',Ops.markersize, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour, ...
            'parent',Parent);
    end
    o = o+size(bs,1);
end
