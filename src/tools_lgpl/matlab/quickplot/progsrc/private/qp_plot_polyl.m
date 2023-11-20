function [hNew,Thresholds,Param]=qp_plot_polyl(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_POLYL Plot function of QuickPlot for polyline data sets.

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

T_=1; ST_=2; M_=3; N_=4; K_=5;

FirstFrame = Param.FirstFrame;
Quant=Param.Quant;
Units=Param.Units;
if ~isempty(Units)
    PName=sprintf('%s (%s)',Quant,Units);
else
    PName=Quant;
end
TStr       = Param.TStr;
Selected   = Param.Selected;
multiple   = Param.multiple;
NVal       = Param.NVal;

DimFlag=Props.DimFlag;
Thresholds=Ops.Thresholds;

NeedsCell = ~strcmp(Ops.facecolour,'none') || isfield(data,'Val');
if NVal==4 && isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'polylines')
    NeedsCell = 0;
end
if isfield(data,'XY') && iscell(data.XY)
    if NeedsCell
        % no change
    else
        len = cellfun(@(a)size(a,1), data.XY);
        tlen = sum(len+1)-1;
        XY = NaN(tlen,2);
        offset = 0;
        for i = 1:length(data.XY)
            if ~isempty(data.XY{i})
                XY(offset+(1:len(i)),:) = data.XY{i}(:,1:2);
                offset = offset+len(i)+1;
            end
        end
        data.XY = XY;
    end
else
    if ~isfield(data,'XY')
        data.XY = [data.X data.Y];
        data = rmfield(data,{'X','Y'});
    end
    if NeedsCell
        breaks = none(isnan(data.XY),2);
        % could use mat2cell below, but this would keep all the singleton NaNs
        PolyStartEnd = findseries(breaks);
        XY = cell(1,size(PolyStartEnd,1));
        for i = 1:size(PolyStartEnd,1)
            XY{i} = data.XY(PolyStartEnd(i,1):PolyStartEnd(i,2),:);
        end
        data.XY = XY;
        if isfield(data,'Val')
            VL = cell(1,size(PolyStartEnd,1));
            for i = 1:size(PolyStartEnd,1)
                VL{i} = data.Val(PolyStartEnd(i,1):PolyStartEnd(i,2));
                % check if all values are equal (doesn't work for NaN ...)
                if all(VL{i}==VL{i}(1)) || all(isnan(VL{i}))
                    VL{i} = VL{i}(1);
                end
            end
            % if all polylines have constant value, then convert cell to
            % simple vector
            if all(cellfun('length',VL)==1)
                VL = cat(2,VL{:});
            end
            data.Val = VL;
        end
    end
end
%
if isfield(Ops,'presentationtype')
    switch Ops.presentationtype
        case {'markers','values','labels'}
            if iscell(data.XY)
                XY = NaN(length(data.XY),2);
                for i = 1:length(data.XY)
                    if ~isempty(data.XY{i})
                        if size(data.XY{i},1)==1
                            XY(i,:) = data.XY{i};
                        else
                            d = pathdistance(data.XY{i}(:,1),data.XY{i}(:,2));
                            uNode = d~=[NaN;d(1:end-1)];
                            XY(i,:) = interp1(d(uNode),data.XY{i}(uNode,1:2),d(end)/2);
                        end
                    end
                end
                data.XY = XY;
            end
        otherwise
            if NVal==4
                % currently converting text valued polygons to polygons
                % without data, but it might be nicer to convert text to
                % class labels.
                % labels = unique(data.Val);
                % [~,data.Val] = ismember(data.Val, labels);
                % NVal = 1;
                % However, it might be better to do that generically in
                % qp_plot for all geometry types. --> DELFT3D-37699
                NVal=0;
            end
    end
end
switch NVal
    case 0
        if strcmp(Ops.facecolour,'none')
            if ishandle(hNew)
                set(hNew,'xdata',data.XY(:,1), ...
                         'ydata',data.XY(:,2));
            else
                hNew=line(data.XY(:,1),data.XY(:,2), ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
                set(Parent,'layer','top')
            end
        else
            if ~FirstFrame
                delete(hNew)
            end
            hNew = plot_polygons(data.XY,[],Parent,Ops);
            %
            set(Parent,'layer','top')
        end
        qp_title(Parent,TStr,'quantity',Quant,'unit',Units,'time',TStr)
    case {1,5,6}
        switch Ops.presentationtype
            case {'values'}
                hNew=gentextfld(hNew,Ops,Parent,data.Val,data.XY(:,1),data.XY(:,2));
            otherwise
                if ~FirstFrame
                    delete(hNew)
                end
                hNew = plot_polygons(data.XY,data.Val,Parent,Ops);
                %
                set(Parent,'layer','top')
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                end
        end
    case {2,3}
        if multiple(M_) % network
        else % point
        end
    case 4
        switch Ops.presentationtype
            case {'markers'}
                hNew=genmarkers(hNew,Ops,Parent,[],data.XY(:,1),data.XY(:,2));
            case {'labels'}
                hNew=gentextfld(hNew,Ops,Parent,data.Val,data.XY(:,1),data.XY(:,2));
        end
end


function hNew = plot_polygons(XY,val,Parent,Ops)
if strcmp(Ops.facecolour,'none')
    hNewP = [];
else
    [XYP,valP] = process_polygons_parts(XY,val);
    hNewP = plot_polygons_fill(XYP,valP,Parent,Ops);
end
hNewL = plot_polygons_outline(XY,val,Parent,Ops);
hNew = [hNewL;hNewP];


function hNew = plot_polygons_outline(XY,val,Parent,Ops)
len = cellfun('length',XY);
tlen = sum(len+1);
X = NaN(tlen,1);
Y = NaN(tlen,1);
hasval = ~isempty(val);
if hasval
    V = NaN(tlen,1);
end
offset = 0;
for i = 1:length(XY)
    range = offset+(1:len(i));
    X(range) = XY{i}(:,1);
    Y(range) = XY{i}(:,2);
    if hasval
        if iscell(val)
            % could be vector of same length or single value which will be
            % expanded automatically.
            V(range) = val{i}(:);
        else
            V(range) = val(i);
        end
    end
    offset = offset+len(i)+1;
end
%
if hasval
    hNew = patch(X,Y,V, ...
        'edgecolor','interp', ...
        'facecolor','none', ...
        'linestyle',Ops.linestyle, ...
        'linewidth',Ops.linewidth, ...
        'marker',Ops.marker, ...
        'markersize',Ops.markersize, ...
        'markeredgecolor',Ops.markercolour, ...
        'markerfacecolor',Ops.markerfillcolour, ...
        'parent',Parent);
else
    hNew = line(X,Y, ...
        'parent',Parent, ...
        Ops.LineParams{:});
end


function [XY,V] = process_polygons_parts(XY,V)
% An object (a single cell of XY) may contains multiple contour parts
% separated by NaNs. The first contour part marks an outer contour. The
% other contour parts mark either inner contour (a contour of a hole) or
% another outer contour (a contour of another region, or the contour of an
% island inside a hole). The outer contour is merged with the contour of
% its holes while other outer contours result in additional entries in XY.
% If present and non-empty, the V array with values will be extended in
% case we add new cells to XY.
hasval = false;
if nargin>1 && ~isempty(V)
    hasval = true;
end
inew = 0;
if size(XY,1) ~= 1
    XYnew = cell(1000,1);
else
    XYnew = cell(1,1000);
end
if hasval
    Vnew = zeros(1,1000);
end
for iobj = 1:length(XY)
    xy = XY{iobj};
    %
    nansep = find(isnan(xy(:,1)));
    if isempty(nansep)
        if ~isequal(xy(1,:),xy(end,:))
            XY{iobj} = zeros(0,2);
        end
        continue
    end
    BP = [[0;nansep] [nansep;size(xy,1)+1]];
    BPln = BP(:,2)-BP(:,1)-1;
    np = size(BP,1);
    inside = false(np);
    inside_check = 'sequential';
    switch inside_check
        case 'never'
            % default matrix inside is valid
        case 'sequential'
            % check only for polygons inside the previous ones back to the
            % latest outer one
            i1 = 1; % latest outer
            for j = 2:np
                is_inside = false;
                for i = i1:j-1
                    inside(j,i) = all(inpolygon(xy(BP(j,1)+1:BP(j,2)-1,1),xy(BP(j,1)+1:BP(j,2)-1,2),xy(BP(i,1)+1:BP(i,2)-1,1),xy(BP(i,1)+1:BP(i,2)-1,2)));
                    is_inside = is_inside | inside(j,i);
                end
                if ~is_inside
                    i1 = j;
                end
            end
        case 'always'
            % check any combination
            for i = 1:np
                for j = i+1:np
                    inside(i,j) = all(inpolygon(xy(BP(i,1)+1:BP(i,2)-1,1),xy(BP(i,1)+1:BP(i,2)-1,2),xy(BP(j,1)+1:BP(j,2)-1,1),xy(BP(j,1)+1:BP(j,2)-1,2)));
                    inside(j,i) = all(inpolygon(xy(BP(j,1)+1:BP(j,2)-1,1),xy(BP(j,1)+1:BP(j,2)-1,2),xy(BP(i,1)+1:BP(i,2)-1,1),xy(BP(i,1)+1:BP(i,2)-1,2)));
                end
            end
    end
    %
    % determine contour type: 0 = outer contour, 1 = inner contour (hole)
    type = mod(sum(inside,2),2);
    first = true;
    for ip = find(type==0)' % loop over outer contours
        %
        inrank = sum(inside(ip,:));
        inhere = find(inside(:,ip));
        ipx = [ip;inhere(sum(inside(inhere,:),2)==inrank+1)];
        %
        xyr = NaN(sum(BPln(ipx)+1)-1,2);
        or = 0;
        for i = ipx'
            xyr(or+(1:BPln(i)),:) = xy(BP(i,1)+1:BP(i,2)-1,1:2);
            or = or + BPln(i)+1;
        end
        bp = cumsum(BPln(ipx)+1);
        %
        for i = 1:length(bp)-1
            % find the shortest distance between the first contour
            % and the second one.
            d1min = inf;
            for i2 = bp(i)+1:bp(i+1)-1
                [d1,i1] = min( (xyr(1:bp(i)-1,1)-xyr(i2,1)).^2 + (xyr(1:bp(i)-1,2)-xyr(i2,2)).^2);
                if d1<d1min
                    d1min = d1;
                    i1min = i1;
                    i2min = i2;
                end
            end
            % merge the first and second contour along the line
            % between the two points with shortest distance.
            xyr(1:bp(i+1)-1,:) = xyr([i1min:bp(i)-1 2:i1min i2min:bp(i+1)-1 bp(i)+2:i2min i1min],:);
            % the merged contour is now contour one, continue to
            % merge it with the contour of the next hole.
        end
        %
        if first
            XY{iobj} = xyr;
            first = false;
        else
            if inew==length(XYnew)
                XYnew{2*inew} = [];
                if hasval
                    Vnew(2*inew) = 0;
                end
            end
            inew = inew+1;
            XYnew{inew} = xyr;
            if hasval
                Vnew(inew) = V(iobj);
            end
        end
    end
end
if size(XY,1) ~= 1
    XY = [XY; XYnew(1:inew)];
else
    XY = [XY, XYnew(1:inew)];
end
if hasval
    if size(V,1) ~= 1
        V = [V; Vnew(1:inew)'];
    else
        V = [V, Vnew(1:inew)];
    end
end


function hNew = plot_polygons_fill(XY,V,Parent,Ops)
hasval = ~isempty(V);
nnodes = cellfun('size',XY,1);
unodes = unique(nnodes);
unodes(unodes==0)=[];
hNew = zeros(length(unodes),1);
for i = 1:length(unodes)
    n = unodes(i);
    if n==0
        continue
    end
    nr = n-1; % number of nodes without duplication of first node
    %
    poly_n = find(nnodes==n);
    npoly = length(poly_n);
    tvertex = nr*npoly;
    %
    XYvertex = NaN(tvertex,2);
    if hasval
        Vpatch = NaN(npoly,1);
    else
        Vpatch = [];
    end
    offset = 0;
    for ip = 1:npoly
        XYvertex(offset+(1:nr),:) = XY{poly_n(ip)}(1:nr,1:2);
        offset = offset+nr;
        if hasval
            if iscell(V)
                Vpatch(ip) = V{poly_n(ip)};
            else
                Vpatch(ip) = V(poly_n(ip));
            end
        end
    end
    %
    facecolor = Ops.facecolour;
    if strcmp(facecolor,'yes')
        facecolor = 'flat';
    end
    hNew(i) = patch('vertices',XYvertex, ...
        'faces',reshape(1:tvertex,[nr npoly])', ...
        'facevertexcdata',Vpatch, ...
        'edgecolor','k', ...
        'facecolor',facecolor, ...
        'linestyle','none', ...
        'marker','none', ...
        'parent',Parent);
end
