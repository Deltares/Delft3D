function [X,Y,V] = face2surf(x,y,v,thu,thv,option)
%FACE2SURF Construct surface data from values at patch face centers.
%   [X2,Y2,V2] = FACE2SURF(X,Y,V) constructs coordinate arrays X2 and Y2
%   and value array V2 containing data at cell centres, cell corners and
%   cell interfaces based on coordinate arrays (X,Y) for the grid cell
%   corners and a values array V for the grid cell centers. The size of V
%   should be equal to size(X)-1.
%
%   [X3,Y3,V3] = FACE2SURF(X,Y,V,THU,THV) constructs coordinate arrays X3
%   and Y3 and value array V3 containing data at cell centres, cell
%   interface (possibly 2-valued) and cell corners (possibly 4-valued)
%   based on coordinate arrays (X,Y) for the grid cell corners, a values
%   array V for the grid cell centers and logical arrays THU and THV
%   indicating whether or not cell interfaces are open (0) or closed (1).
%   The size of V should be equal to size(X)-1, the size of THU should be
%   [size(X,1) size(X,2)-1] and the size of THV should be [size(X,1)-1
%   size(X,2)].
%
%   Example 1
%     x = repmat((1:15)',1,8);
%     y = repmat(1:8,15,1);
%     v = -[magic(7);magic(7)];
%     x(8,1:3) = NaN;
%     y(8,1:3) = NaN;
%     v(7:8,1:3) = NaN;
%     %
%     [x2,y2,v2] = face2surf(x,y,v);
%     figure
%     surf(x2,y2,v2,'facecolor','interp')
%     view(0,90)
%
%   Example 2 (x,y,v defined as in example 1)
%     thu = zeros(15,7);
%     thu(:,1:4) = 1;
%     thv = zeros(14,8);
%     thv(4:8,:) = 1;
%     %
%     [x3,y3,v3] = face2surf(x,y,v,thu,thv);
%     figure
%     surf(x3,y3,v3,'facecolor','interp','linestyle',':')
%     view(0,90)
%     set(thindam(x,y,thu(:,[1 1:end]),thv([1 1:end],:)),'color','k','linewidth',2)
%
%   See also SURF, THINDAM.

%   Experimental features:
%   [XYV,TRI] = FACE2SURF(X,Y,V,THU,THV,'tripatch')
%   [XYV,QUAD] = FACE2SURF(X,Y,V,THU,THV,'quadpatch')
%   These calls produce a data set of triangles and quadrangles.
%
%   Example 3 (x,y,v,thu,thv defined as in example 2)
%     [xyv,tri] = face2surf(x,y,v,thu,thv,'tripatch');
%     figure
%     h = trisurf(tri,xyv(:,1),xyv(:,2),xyv(:,3));
%     set(h,'edgecolor','k','linestyle',':','facecolor','interp')
%     view(0,90)
%     set(thindam(x,y,thu(:,[1 1:end]),thv([1 1:end],:)),'color','k','linewidth',2)
%
%   Example 4 (x,y,v,thu,thv defined as in example 2)
%     [xyv,quad] = face2surf(x,y,v,thu,thv,'quadpatch');
%     figure
%     h = trisurf(quad,xyv(:,1),xyv(:,2),xyv(:,3));
%     set(h,'edgecolor','k','linestyle',':','facecolor','interp')
%     view(0,90)
%     set(thindam(x,y,thu(:,[1 1:end]),thv([1 1:end],:)),'color','k','linewidth',2)

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

V = [];
if nargin==3 || nargin==4
    if ~isequal(size(x),size(y)) && ~isequal(size(x),size(y)+[1 0])
        error('Coordinate array should have equal size.')
    else
        err = false;
        single_v = ~iscell(v);
        if single_v
            v = {v};
        end
        for i=1:numel(v)
            if length(size(x))~=length(size(v{i}))
                err = true;
            elseif ~all(size(x)==size(v{i}) | size(x)==size(v{i})+1)
                err = true;
            end
        end
        if err
            error('The length of each dimension i of the values array size(V,i) should equal the length of the corresponding dimension of the coordinate variable size(X,i) or one less size(X,i)-1.')
        end
    end
    if nargin==3
        if nargout==2
            option = 'quadpatch';
        else
            option = 'surf';
        end
    else
        option = thu;
        if ~ischar(option)
            error('Option argument should read ''surf'',''tripatch'' or ''quadpatch''.')
        end
    end
    switch lower(option)
        case 'surf'
            [X,Y,V] = double_resolution(x,y,v);
        case 'tripatch'
            thu = zeros(size(x)-[0 1]);
            thv = zeros(size(x)-[1 0]);
            [X,Y] = make_xyv_tris(3,x,y,thu,thv,v);
        case 'quadpatch'
            thu = zeros(size(x)-[0 1]);
            thv = zeros(size(x)-[1 0]);
            [X,Y] = make_xyv_tris(4,x,y,thu,thv,v);
        otherwise
            error('Option argument should read ''surf'',''tripatch'' or ''quadpatch''.')
    end
elseif nargin==5 || nargin==6
    if ~isequal(size(x),size(y)) && ~isequal(size(x),size(y)+[1 0])
        error('Coordinate array should have equal size.')
    elseif ~isequal(size(x),size(thu)+[0 1])
        error('Thin dam U array should be of size [size(X,1) size(X,2)-1].')
    elseif ~isequal(size(x),size(thv)+[1 0])
        error('Thin dam V array should be of size [size(X,1)-1 size(X,2)].')
    else
        single_v = ~iscell(v);
        if single_v
            v = {v};
        end
        for i=1:numel(v)
            if ~isequal(size(x),size(v{i})+1)
                error('Values array should be of size(X)-1.')
            end
        end
    end
    if nargin==5
        if nargout==2
            option = 'quadpatch';
        else
            option = 'surf';
        end
    elseif ~ischar(option)
        error('Option argument should read ''surf'',''tripatch'' or ''quadpatch''.')
    end
    switch lower(option)
        case 'surf'
            [X,Y,V] = tripple_resolution(x,y,thu,thv,v);
        case 'tripatch'
            [X,Y] = make_xyv_tris(3,x,y,thu,thv,v);
        case 'quadpatch'
            [X,Y] = make_xyv_tris(4,x,y,thu,thv,v);
        otherwise
            error('Option argument should read ''surf'',''tripatch'' or ''quadpatch''.')
    end
else
    error('Invalid number of input arguments.')
end
if single_v && ~isempty(V)
    V = V{1};
end


function [xm,ym] = compute_xy_mean(x,y)
%Compute the coordinates of the cell centres.

xm = (x(1:end-1,1:end-1)+x(2:end,1:end-1)+x(1:end-1,2:end)+x(2:end,2:end))/4;
ym = (y(1:end-1,1:end-1)+y(2:end,1:end-1)+y(1:end-1,2:end)+y(2:end,2:end))/4;


function [x2,y2,v2] = double_resolution(x,y,v)
%Construct a data set of double resolution with data at grid cell centers,
%grid cell corners and grid cell interfaces. This resolution is enough to
%capture the edges of the domain if there are no thin dams that cause
%discontinuities in the values.

nrows = [size(x,1), size(y,1), size(v{1},1)];
ncols = [size(x,2), size(y,2), size(v{1},2)];

max_nrows = max(nrows);
max_ncols = max(ncols);

double_rows = min(nrows) ~= max_nrows;
double_cols = min(ncols) ~= max_ncols;

x2 = expand_one_array(x, double_rows, size(x,1)==max_nrows, double_cols, size(x,2)==max_ncols);
y2 = expand_one_array(y, double_rows, size(y,1)==max_nrows, double_cols, size(y,2)==max_ncols);

if iscell(v)
    v2 = cell(size(v));
    for i=1:length(v)
        v2{i} = expand_one_array(v{i}, double_rows, size(v{i},1)==max_nrows, double_cols, size(v{i},2)==max_ncols);
    end
else
    v2 = expand_one_array(v, double_rows, size(v,1)==max_nrows, double_cols, size(v,2)==max_ncols);
end


function v2 = expand_one_array(v, double_rows, odd_rows_given, double_cols, odd_cols_given)
szv2 = size(v);
if double_rows
    if odd_rows_given % poles given
        szv2(1) = szv2(1)*2 - 1;
        i = 1:2:szv2(1);
    else % fences given
        szv2(1) = szv2(1)*2 + 1;
        i = 2:2:szv2(1);
    end
else
    i = ':';
end
if double_cols
    if odd_cols_given % poles given
        szv2(2) = szv2(2)*2 - 1;
        j = 1:2:szv2(2);
    else % fences given
        szv2(2) = szv2(2)*2 + 1;
        j = 2:2:szv2(2);
    end
else
    j = ':';
end

v2 = NaN(szv2);
v2(i,j) = v;

if double_rows
    if odd_rows_given % poles given
        v2(2:2:end,j) = (v(1:end-1,:)+v(2:end,:))/2;
    else % fences given
        v2(1,j) = v2(2,j); % first pole
        v2(end,j) = v2(end-1,j); % last pole
        % intermediate poles: average if both fences have value, left/right
        % value if only one fence has value, NaN if neither has value
        m = isnan(v);
        v(m) = 0;
        v0 = v(1:end-1,:) + v(2:end,:);
        m0 = ~m(1:end-1,:) + ~m(2:end,:);
        m0(m0==0) = NaN;
        v2(3:2:end-2,j) = v0./m0;
    end
end
if double_cols
    if odd_cols_given % poles given
        v2(:,2:2:end) = (v2(:,1:2:end-2)+v2(:,3:2:end))/2;
    else % fences given
        v2(:,1) = v2(:,2); % first pole
        v2(:,end) = v2(:,end-1); % last pole
        % intermediate poles: average if both fences have value, left/right
        % value if only one fence has value, NaN if neither has value
        v2c = v2(:,2:2:end-1);
        m = isnan(v2c);
        v2c(m) = 0;
        v0 = v2c(:,1:end-1) + v2c(:,2:end);
        m0 = ~m(:,1:end-1) + ~m(:,2:end);
        m0(m0==0) = NaN;
        v2(:,3:2:end-2) = v0./m0;
    end
end


function [x3,y3,v3] = tripple_resolution(x,y,thu,thv,v)
%Construct a data set of tripple resolution with single valued data at grid
%cell centers, quadruple valued data at grid cell corners and double valued
%data at grid cell interfaces. This resolution is needed to discontinuities
%in the values at thin dams at cell interfaces.

[x2,y2,v2] = double_resolution(x,y,v);
sz2 = size(x2);
I1 = sort([1:sz2(1), 3:2:sz2(1)-2]);
I2 = sort([1:sz2(2), 3:2:sz2(2)-2]);
x3 = x2(I1,I2);
y3 = y2(I1,I2);
v3 = v2(I1,I2);
sz3 = size(x3);
ix = 1;
iy = sz3(1);
%
thu([1 end],:) = 0;
thv(:,[1 end]) = 0;
%
[i1,i2] = find(thu);
Ii1 = (i1-2)*3+3;
Ii2 = (i2-1)*3+2;
Ii0 = sub2ind(sz3,Ii1,Ii2);
v3(Ii0) = v3(Ii0-ix);
v3(Ii0+ix) = v3(Ii0+2*ix);
%
[i1,i2] = find(thv);
Ii1 = (i1-1)*3+2;
Ii2 = (i2-2)*3+3;
Ii0 = sub2ind(sz3,Ii1,Ii2);
v3(Ii0) = v3(Ii0-iy);
v3(Ii0+iy) = v3(Ii0+2*iy);
%
v3(1,:) = v3(2,:);
v3(end,:) = v3(end-1,:);
v3(:,1) = v3(:,2);
v3(:,end) = v3(:,end-1);
%
point = thu(2:end-1,1:end-1)+2*thv(1:end-1,2:end-1)+4*thu(2:end-1,2:end)+8*thv(2:end,2:end-1);
%
for p = [3 5:7 9:15]
    [i1,i2] = find(point==p);
    Ii1 = (i1-1)*3+3;
    Ii2 = (i2-1)*3+3;
    Ii0 = sub2ind(sz3,Ii1,Ii2);
    %
    switch p
        case 3
            %      *
            %  --.
            %  * |
            v3(Ii0) = v3(Ii0-ix-iy);
            %
            v3(Ii0+ix) = (v3(Ii0+2*ix-iy)+v3(Ii0+2*ix+2*iy)+v3(Ii0-ix+2*iy))/3;
            v3(Ii0+iy) = v3(Ii0+ix);
            v3(Ii0+ix+iy) = v3(Ii0+ix);
        case 5
            %    |
            %  * . *
            %    |
            v3(Ii0) = (v3(Ii0-ix-iy)+v3(Ii0-ix+2*iy))/2;
            v3(Ii0+iy) = v3(Ii0);
            %
            v3(Ii0+ix) = (v3(Ii0+2*ix-iy)+v3(Ii0+2*ix+2*iy))/2;
            v3(Ii0+ix+iy) = v3(Ii0+ix);
        case 6
            %  * |
            %  --.
            %      *
            v3(Ii0+iy) = v3(Ii0-ix+2*iy);
            %
            v3(Ii0) = (v3(Ii0+2*ix-iy)+v3(Ii0+2*ix+2*iy)+v3(Ii0-ix-iy))/3;
            v3(Ii0+ix) = v3(Ii0);
            v3(Ii0+ix+iy) = v3(Ii0);
        case 7
            %  * |
            %  --. *
            %  * |
            v3(Ii0) =  v3(Ii0-ix-iy);
            %
            v3(Ii0+iy) =  v3(Ii0-ix+2*iy);
            %
            v3(Ii0+ix) = (v3(Ii0+2*ix-iy)+v3(Ii0+2*ix+2*iy))/2;
            v3(Ii0+ix+iy) = v3(Ii0+ix);
        case 9
            %  *
            %    .--
            %    | *
            v3(Ii0+ix) = v3(Ii0+2*ix-iy);
            %
            v3(Ii0) = (v3(Ii0-ix-iy)+v3(Ii0+2*ix+2*iy)+v3(Ii0-ix+2*iy))/3;
            v3(Ii0+iy) = v3(Ii0);
            v3(Ii0+ix+iy) = v3(Ii0);
        case 10
            %    *
            %  --.--
            %    *
            v3(Ii0) = (v3(Ii0-ix-iy)+v3(Ii0+2*ix-iy))/2;
            v3(Ii0+ix) = v3(Ii0);
            %
            v3(Ii0+iy) = (v3(Ii0-ix+2*iy)+v3(Ii0+2*ix+2*iy))/2;
            v3(Ii0+ix+iy) = v3(Ii0+iy);
        case 11
            %    *
            %  --.--
            %  * | *
            v3(Ii0) = v3(Ii0-ix-iy);
            %
            v3(Ii0+ix) = v3(Ii0+2*ix-iy);
            %
            v3(Ii0+iy) = (v3(Ii0-ix+2*iy)+v3(Ii0+2*ix+2*iy))/2;
            v3(Ii0+ix+iy) = v3(Ii0+iy);
        case 12
            %    | *
            %    .--
            %  *
            v3(Ii0+ix+iy) = v3(Ii0+2*ix+2*iy);
            %
            v3(Ii0+ix) = (v3(Ii0+2*ix-iy)+v3(Ii0+2*ix+2*iy)+v3(Ii0-ix+2*iy))/3;
            v3(Ii0+iy) = v3(Ii0+ix);
            v3(Ii0+ix+iy) = v3(Ii0+ix);
        case 13
            %    | *
            %  * .--
            %    | *
            v3(Ii0+ix) = v3(Ii0+2*ix-iy);
            %
            v3(Ii0+ix+iy) = v3(Ii0+2*ix+2*iy);
            %
            v3(Ii0) = (v3(Ii0-ix-iy)+v3(Ii0-ix+2*iy))/2;
            v3(Ii0+iy) = v3(Ii0);
        case 14
            %  * | *
            %  --.--
            %    *
            v3(Ii0+iy) = v3(Ii0-ix+2*iy);
            %
            v3(Ii0+ix+iy) = v3(Ii0+2*ix+2*iy);
            %
            v3(Ii0) = (v3(Ii0-ix-iy)+v3(Ii0+2*ix-iy))/2;
            v3(Ii0+ix) = v3(Ii0);
        case 15
            %  * | *
            %  --.--
            %  * | *
            v3(Ii0) = v3(Ii0-ix-iy);
            %
            v3(Ii0+ix) = v3(Ii0+2*ix-iy);
            %
            v3(Ii0+iy) = v3(Ii0-ix+2*iy);
            %
            v3(Ii0+ix+iy) = v3(Ii0+2*ix+2*iy);
    end
end


function [xyz,tri] = make_xyv_tris(nangle,x,y,thu,thv,v)
v = v{1};
thu([1 end],:) = 1;
thv(:,[1 end]) = 1;
thu(2:end-1,:) = thu(2:end-1,:) | isnan(v(1:end-1,:)) | isnan(v(2:end,:));
thv(:,2:end-1) = thv(:,2:end-1) | isnan(v(:,1:end-1)) | isnan(v(:,2:end));
%
[xyz,offset,vci,vi,vui,vvi] = compute_xyz(x,y,v,thu,thv);
%
ntris = 2*numel(thu) + 2*numel(thv) + ... % 2 triangles for every (non) blocked dam
    2*sum(thu(:)) + 2*sum(thv(:)) + ... % 2 extra triangles for every blocked dam
    - 8*sum(isnan(v(:))) ... % 8 triangles less for every missing value (surrounded by blocked dams)
    - 2*2*sum(size(v)); % 2 triangles less for every boundary dam (if int
tri = ones(ntris,nangle);
tri_offset = 0;
% TRIANGLES                 QUADRANGLES
% blocked      ~blocked     blocked      ~blocked
%    /|\          / \          /|\          / \
%   / | \        /   \        / | \        /   \
%  * -|- *  or  *-----*      *  .  *  or  *     *
%   \ | /        \   /        \ | /        \   /
%    \|/          \ /          \|/          \ /
%
blocked = thu(:)==1;
notblocked = ~blocked;
nel = numel(thu);
NaNcol = repmat(NaN,1,size(thu,2));
v1 = reshape([NaNcol;vi],nel,1);
v2 = reshape([vi;NaNcol],nel,1);
v12= reshape(vui,nel,2);
vr = reshape(vci(:,1:end-1,:),nel,4);
vl = reshape(vci(:,2:end,:),nel,4);
blocked1 = blocked & ~isnan(v1);
blocked2 = blocked & ~isnan(v2);
nquad = sum(blocked1) + sum(blocked2) + sum(notblocked);
switch nangle
    case 3
        idx_tri = tri_offset + (1:2*nquad);
        tri(idx_tri,:) = ...
            [v1(blocked1) v12(blocked1,1) vr(blocked1,4)
            vl(blocked1,1) v12(blocked1,1) v1(blocked1)
            v2(blocked2) v12(blocked2,2) vl(blocked2,2)
            vr(blocked2,3) v12(blocked2,2) v2(blocked2)
            v1(notblocked) v2(notblocked) vr(notblocked,4)
            v2(notblocked) v1(notblocked) vl(notblocked,1)];
    case 4
        idx_tri = tri_offset + (1:nquad);
        tri(idx_tri,:) = ...
            [v1(blocked1) vl(blocked1,1) v12(blocked1,1) vr(blocked1,4)
            v2(blocked2) vr(blocked2,3) v12(blocked2,2) vl(blocked2,2)
            v1(notblocked) vl(notblocked,1) v2(notblocked) vr(notblocked,4)];
end
tri_offset = idx_tri(end);
% TRIANGLES                 QUADRANGLES
% blocked   ~blocked        blocked   ~blocked
%    *                         *
%   /|\         *             / \         *
%  / | \       /|\           /   \       / \
%  -----  or  / | \          --.--  or  /   \
%  \ | /      \ | /          \   /      \   /
%   \|/        \|/            \ /        \ /
%    *          *              *          *
%
blocked = thv(:)==1;
notblocked = ~blocked;
nel = numel(thv);
NaNcol = NaN(size(thv,1),1);
v1 = reshape([NaNcol vi],nel,1);
v2 = reshape([vi NaNcol],nel,1);
v12= reshape(vvi,nel,2);
vl = reshape(vci(1:end-1,:,:),nel,4);
vr = reshape(vci(2:end,:,:),nel,4);
blocked1 = blocked & ~isnan(v1);
blocked2 = blocked & ~isnan(v2);
nquad = sum(blocked1) + sum(blocked2) + sum(notblocked);
switch nangle
    case 3
        idx_tri = tri_offset + (1:2*nquad);
        tri(idx_tri,:) = ...
            [v1(blocked1) v12(blocked1,1) vr(blocked1,1)
            vl(blocked1,2) v12(blocked1,1) v1(blocked1)
            v2(blocked2) v12(blocked2,2) vl(blocked2,3)
            vr(blocked2,4) v12(blocked2,2) v2(blocked2)
            v1(notblocked) v2(notblocked) vr(notblocked,1)
            v2(notblocked) v1(notblocked) vl(notblocked,2)];
    case 4
        idx_tri = tri_offset + (1:nquad);
        tri(idx_tri,:) = ...
            [v1(blocked1) vl(blocked1,2) v12(blocked1,1) vr(blocked1,1)
            v2(blocked2) vr(blocked2,4) v12(blocked2,2) vl(blocked2,3)
            v1(notblocked) vl(notblocked,2) v2(notblocked) vr(notblocked,1)];
end


function [xyz,offset,vci,vi,vui,vvi] = compute_xyz(x,y,v,thu,thv)
% COMPUTE_VC Computes value at grid corners.
%
% thin dams surrounding internal grid point
%      4
%    2 * 8
%      1
point = thu(2:end-1,1:end-1)+2*thv(1:end-1,2:end-1)+4*thu(2:end-1,2:end)+8*thv(2:end,2:end-1);
%             00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
point2nval = [ 1  1  1  2  1  2  2  3  1  2  2  3  2  3  3  4];
nmissing = isnan(v(1:end-1,1:end-1))+isnan(v(2:end,1:end-1))+isnan(v(1:end-1,2:end))+isnan(v(2:end,2:end));
%
% count number (non-missing) values and neighbouring thin dams
nval = ones(size(v))+thu(2:end,:)+thu(1:end-1,:)+thv(:,2:end)+thv(:,1:end-1);
nval(isnan(v)) = 0;
%
edge1 = ~isnan(v(1,1:end-1)) + ~isnan(v(1,2:end)) - ~thv(1,2:end-1);
edge2 = ~isnan(v(end,1:end-1)) + ~isnan(v(end,2:end)) - ~thv(end,2:end-1);
edge3 = ~isnan(v(1:end-1,1)) + ~isnan(v(2:end,1)) - ~thu(2:end-1,1);
edge4 = ~isnan(v(1:end-1,end)) + ~isnan(v(2:end,end)) - ~thu(2:end-1,end);
%
% Count all points that will be needed (only non-missing)
%
ntotal = sum(nval(:)) + ...% cell centres and interfaces
    sum(point2nval(point(:)+1))- sum(nmissing(:))+ ... % cell corners (internal)
    sum(edge1) + sum(edge2) + sum(edge3) + sum(edge4) + ... % cell corners (side walls)
    sum(sum(~isnan(v([1 end],[1 end])))); % cell corners (grid corners)
%
szv = size(v);
ix = 1;
iy = szv(1);
%
% allocate arrays for grid points and indices
%
xyz = zeros(ntotal,3);
vci = NaN([numel(x) 4]);
vi  = NaN(size(v));
vui = NaN([size(thu) 2]);
vvi = NaN([size(thv) 2]);
%
% cell centers
%
[xm,ym] = compute_xy_mean(x,y);
idx = ~isnan(v);
nidx = sum(idx(:));
xyz(1:nidx,:) = [xm(idx) ym(idx) v(idx)];
vi(idx) = 1:nidx;
offset = nidx;
%
% cell interfaces (internal)
%
xi = (x(2:end-1,1:end-1)+x(2:end-1,2:end))/2;
yi = (y(2:end-1,1:end-1)+y(2:end-1,2:end))/2;
v1 = v(1:end-1,:);
v2 = v(2:end,:);
thi = thu(2:end-1,:);
vui_loc = reshape(vui(2:end-1,:,:),[numel(thi) 2]);
%
% idx = thi==0;
% nidx = sum(idx(:));
% if nidx>0
%     idx_xyz = offset+(1:nidx);
%     xyz(idx_xyz,:) = [xi(idx) yi(idx) (v1(idx)+v2(idx))/2];
%     vui_loc(idx,1:2) = repmat(idx_xyz',1,2);
%     offset = idx_xyz(end);
% end
%
idx = (thi~=0) & ~isnan(v1);
nidx = sum(idx(:));
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [xi(idx) yi(idx) v1(idx)];
    vui_loc(idx,1) = idx_xyz;
    offset = idx_xyz(end);
end
%
idx = (thi~=0) & ~isnan(v2);
nidx = sum(idx(:));
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [xi(idx) yi(idx) v2(idx)];
    vui_loc(idx,2) = idx_xyz;
    offset = idx_xyz(end);
end
vui(2:end-1,:,:) = reshape(vui_loc,[size(thi) 2]);
%
xi = (x(1:end-1,2:end-1)+x(2:end,2:end-1))/2;
yi = (y(1:end-1,2:end-1)+y(2:end,2:end-1))/2;
v1 = v(:,1:end-1);
v2 = v(:,2:end);
thi = thv(:,2:end-1);
vvi_loc = reshape(vvi(:,2:end-1,:),[numel(thi) 2]);
%
% idx = thi==0;
% nidx = sum(idx(:));
% if nidx>0
%     idx_xyz = offset+(1:nidx);
%     xyz(idx_xyz,:) = [xi(idx) yi(idx) (v1(idx)+v2(idx))/2];
%     vvi_loc(idx,1:2) = repmat(idx_xyz',1,2);
%     offset = idx_xyz(end);
% end
%
idx = (thi~=0) & ~isnan(v1);
nidx = sum(idx(:));
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [xi(idx) yi(idx) v1(idx)];
    vvi_loc(idx,1) = idx_xyz;
    offset = idx_xyz(end);
end
%
idx = (thi~=0) & ~isnan(v2);
nidx = sum(idx(:));
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [xi(idx) yi(idx) v2(idx)];
    vvi_loc(idx,2) = idx_xyz;
    offset = idx_xyz(end);
end
vvi(:,2:end-1,:) = reshape(vvi_loc,[size(thi) 2]);
%
% cell interfaces (side walls)
%
idx = find(~isnan(v(1,:)));
nidx = length(idx);
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [(x(1,idx)+x(1,idx+1))'/2 (y(1,idx)+y(1,idx+1))'/2 v(1,idx)'];
    vui(1,idx,2) = idx_xyz;
    offset = idx_xyz(end);
end
%
idx = find(~isnan(v(end,:)));
nidx = length(idx);
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [(x(end,idx)+x(end,idx+1))'/2 (y(end,idx)+y(end,idx+1))'/2 v(end,idx)'];
    vui(end,idx,1) = idx_xyz;
    offset = idx_xyz(end);
end
%
idx = find(~isnan(v(:,1)));
nidx = length(idx);
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [(x(idx,1)+x(idx+1,1))/2 (y(idx,1)+y(idx+1,1))/2 v(idx,1)];
    vvi(idx,1,2) = idx_xyz;
    offset = idx_xyz(end);
end
%
idx = find(~isnan(v(:,end)));
nidx = length(idx);
if nidx>0
    idx_xyz = offset+(1:nidx);
    xyz(idx_xyz,:) = [(x(idx,end)+x(idx+1,end))/2 (y(idx,end)+y(idx+1,end))/2 v(idx,end)];
    vvi(idx,end,1) = idx_xyz;
    offset = idx_xyz(end);
end
%
% cell corners (internal)
%
for p = 0:15
    [i1,i2] = find(point==p);
    if isempty(i1)
        continue
    end
    idx_v = sub2ind(szv,i1+1,i2+1);
    idx_x = sub2ind(szv+1,i1+1,i2+1);
    %
    switch p
        case {0,1,2,4,8}
            %                     |
            %   *   or --*   or   *   or   *-- or   *
            %                                       |
            val = (v(idx_v-ix-iy)+v(idx_v-ix)+v(idx_v-iy)+v(idx_v))/4;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1:4);
        case 3
            %      *
            %  --.
            %  * |
            val = v(idx_v-ix-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1);
            %
            val = (v(idx_v-ix)+v(idx_v-iy)+v(idx_v))/3;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2:4);
        case 5
            %    |
            %  * . *
            %    |
            val = (v(idx_v-ix-iy)+v(idx_v-ix))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,[1 4]);
            %
            val = (v(idx_v-iy)+v(idx_v))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2:3);
        case 6
            %  * |
            %  --.
            %      *
            val = (v(idx_v-ix-iy)+v(idx_v-iy)+v(idx_v))/3;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1:3);
            %
            val = v(idx_v-ix);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,4);
        case 7
            %  * |
            %  --. *
            %  * |
            val = v(idx_v-ix-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1);
            %
            val = (v(idx_v-iy)+v(idx_v))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2:3);
            %
            val = v(idx_v-ix);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,4);
        case 9
            %  *
            %    .--
            %    | *
            val = v(idx_v-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2);
            %
            val = (v(idx_v-ix-iy)+v(idx_v-ix)+v(idx_v))/3;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,[1 3 4]);
        case 10
            %    *
            %  --.--
            %    *
            val = (v(idx_v-ix-iy)+v(idx_v-iy))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1:2);
            %
            val = (v(idx_v-ix)+v(idx_v))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3:4);
        case 11
            %    *
            %  --.--
            %  * | *
            val = v(idx_v-ix-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1);
            %
            val = v(idx_v-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2);
            %
            val = (v(idx_v-ix)+v(idx_v))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3:4);
        case 12
            %    | *
            %    .--
            %  *
            val = (v(idx_v-ix-iy)+v(idx_v-iy)+v(idx_v-ix))/3;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,[1 2 4]);
            %
            val = v(idx_v);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3);
        case 13
            %    | *
            %  * .--
            %    | *
            val = (v(idx_v-ix-iy)+v(idx_v-ix))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,[1 4]);
            %
            val = v(idx_v-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2);
            %
            val = v(idx_v);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3);
        case 14
            %  * | *
            %  --.--
            %    *
            val = (v(idx_v-ix-iy)+v(idx_v-iy))/2;
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1:2);
            %
            val = v(idx_v);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3);
            %
            val = v(idx_v-ix);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,4);
        case 15
            %  * | *
            %  --.--
            %  * | *
            val = v(idx_v-ix-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1);
            %
            val = v(idx_v-iy);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2);
            %
            val = v(idx_v);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3);
            %
            val = v(idx_v-ix);
            [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,4);
    end
end
%
% cell corners (side walls)
%
%  * | *
%  --.--
%
i1 = 2:szv(1);
i1(thu(i1,1)==0) = [];
if ~isempty(i1)
    idx_x = i1;
    val = v(i1,1);
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3);
    %
    val = v(i1-1,1);
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,4);
end
%
%    *
%  --.--
%
i1 = 2:szv(1);
i1(thu(i1,1)~=0) = [];
if ~isempty(i1)
    idx_x = i1;
    val = (v(i1,1)+v(i1-1,1))/2;
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3:4);
end
%
%  --.--
%  * | *
%
i1 = 2:szv(1);
i1(thu(i1,end)==0) = [];
if ~isempty(i1)
    idx_x = numel(x)-size(x,1)+i1;
    val = v(i1-1,end);
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1);
    %
    val = v(i1,end);
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2);
end
%
%  --.--
%    *
%
i1 = 2:szv(1);
i1(thu(i1,end)~=0) = [];
if ~isempty(i1)
    idx_x = numel(x)-size(x,1)+i1;
    val = (v(i1,end)+v(i1-1,end))/2;
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1:2);
end
%
%  | *
%  .--
%  | *
%
i2 = 2:szv(2);
i2(thv(1,i2)==0) = [];
if ~isempty(i2)
    idx_x = (i2-1)*size(x,1)+1;
    val = v(1,i2-1)';
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2);
    %
    val = v(1,i2)';
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3);
end
%
%  |
%  . *
%  |
%
i2 = 2:szv(2);
i2(thv(1,i2)~=0) = [];
if ~isempty(i2)
    idx_x = (i2-1)*size(x,1)+1;
    val = (v(1,i2-1)+v(1,i2))'/2;
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2:3);
end
%
%  * |
%  --.
%  * |
%
i2 = 2:szv(2);
i2(thv(end,i2)==0) = [];
if ~isempty(i2)
    idx_x = i2*size(x,1);
    val = v(end,i2-1)';
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1);
    %
    val = v(end,i2)';
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,4);
end
%
%    |
%  * .
%    |
%
i2 = 2:szv(2);
i2(thv(end,i2)~=0) = [];
if ~isempty(i2)
    idx_x = i2*size(x,1);
    val = (v(end,i2-1)+v(end,i2))'/2;
    [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,[1 4]);
end
%
% cell corners (grid corners)
%
% | *
% .--
%
idx_x = 1;
val = v(1,1);
[xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,3);
%
% * |
% --.
%
idx_x = size(x,1);
val = v(end,1);
[xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,4);
%
% --.
% * |
%
idx_x = numel(x);
val = v(end,end);
[xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,1);
%
% .--
% | *
%
idx_x = numel(x)-size(x,1)+1;
val = v(1,end);
[xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,2);
%
vci = reshape(vci,[size(x) 4]);


function [xyz,offset,vci] = register_vci(xyz,offset,x,y,idx_x,val,vci,i)
%
% determine indices of active points
%
idx_val = 1:length(val);
idx_val(isnan(val)) = [];
if isempty(idx_val)
    return
end
%
% store point
%
idx_xyz = offset + (1:length(idx_val))';
xyz(idx_xyz,1) = x(idx_x(idx_val));
xyz(idx_xyz,2) = y(idx_x(idx_val));
xyz(idx_xyz,3) = val(idx_val);
offset = idx_xyz(end);
%
% store reference to point
%
vci(idx_x(idx_val),i) = repmat(idx_xyz,1,length(i));
