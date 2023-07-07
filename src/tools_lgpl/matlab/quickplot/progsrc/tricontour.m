function H=tricontour(tri,x,y,z,v,levels,color)
%TRICONTOUR Contour plot for triangulated data.
%   TRICONTOUR(TRI,X,Y,Z,Levels) displays the contour plot based on
%   triangles defined in the M-by-3 face matrix TRI as a surface. A
%   row of TRI contains indexes into the X,Y, and Z vertex vectors
%   to define a single triangular face.
%
%   TRICONTOUR(TRI,X,Y,Z,V,Levels) displays the contour plot based on
%   triangles defined in the M-by-3 face matrix TRI as a surface. A
%   row of TRI contains indexes into the X,Y, and Z vertex vectors
%   to define a single triangular face. The contours are determined
%   by the values V and the Levels.
%
%   H = TRICONTOUR(...) a vector H of handles to LINE or PATCH objects,
%   one handle per line. TRICONTOUR is not compatible with CLABEL.
%
%   The contours are normally colored based on the current colormap
%   and are drawn as PATCH objects. You can override this behavior
%   with the syntax CONTOUR(...,'LINESPEC') to draw the contours as
%   LINE objects with the color and linetype specified.
%
%   Example:
%      x=rand(20); y=rand(20); z=rand(20); tri=delaunay(x,y);
%      tricontour(tri,x,y,z,.3:.1:1); colorbar
%
%   See also CONTOUR, CONTOURF, TRICONTOURF

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

if nargin<5
    error('Not enough input arguments.')
elseif nargin>7
    error('Too many input arguments.')
end
getdata=0;
if nargin==7
    if strcmp(color,'getdata')
        getdata=1;
        lin = '*';
        col = '*';
    else
        [lin,col,mark,msg] = colstyle(color);
        if ~isempty(msg)
            error(msg)
        end
    end
elseif nargin==6
    if ischar(levels)
        color=levels;
        levels=v;
        v=z;
        z=[];
        if strcmp(color,'getdata')
            getdata=1;
            lin = '*';
            col = '*';
        else
            [lin,col,mark,msg] = colstyle(color);
            if ~isempty(msg)
                error(msg)
            end
        end
    else
        lin = '';
        col = '';
    end
else
    levels=v;
    v=z;
    z=[];
    lin = '';
    col = '';
end

if ~getdata
    ax = newplot;
    if isempty(col) % no color spec was given
        colortab = get(ax,'colororder');
        ncol = size(colortab,1);
    end
end

x=transpose(x(:));
y=transpose(y(:));
zdef=~isempty(z);
if zdef
    z=transpose(z(:));
end
v=transpose(v(:));

Patches=1:size(tri,1);
nLevels = length(levels);
if getdata
    H=cell(1,nLevels);
else
    H=zeros(1,nLevels);
end
NonEmptyLevel=zeros(1,nLevels);
levelTri = v(tri);
for LevelNr = 1:nLevels
    thisLevel = levels(LevelNr);
    nLarger = sum(levelTri>thisLevel,2);
    nSmaller = sum(levelTri<thisLevel,2);
    
    % if there is a NaN, consider all values smaller
    NaN_tri = any(isnan(levelTri),2);
    nSmaller(NaN_tri) = 3;
    nLarger(NaN_tri) = 0;
    
    % CLIndex:
    %
    %      nSmaller nLarger nEqual
    %  1 =                    3     -> no line
    %  2 =    1               2     -> line along edge
    %  3 =    2               1     -> single point (skip)
    %  4 =    3                     -> whole patch
    %  5 =             1      2     -> line along edge
    %  6 =    1        1      1     -> slice through node
    %  7 =    2        1            -> slice
    %  8 =   --- not possible ---
    %  9 =             2      1     -> single point (skip)
    % 10 =    1        2            -> slice
    % 11 =   --- not possible ---
    % 12 =   --- not possible ---
    % 13 =             3            -> no line
    
    
    CLIndex = 1 + nSmaller + 4*nLarger;
    
    % include single points
    %Cutslevel=[0 3 2 0 3 3 3 0 2 3 0 0 0 0 0];
    % skip single points
    Cutslevel=[0 3 0 0 3 3 3 0 0 3 0 0 0 0 0];
    
    NLevel=sum(Cutslevel(CLIndex));
    XLevel=NaN(NLevel,1);
    YLevel=XLevel;
    if zdef
        ZLevel=XLevel;
    end
    LvlOffset=0;
        
%% single point (skip)
    
    % patches with one equal and two larger or two smaller generate only a
    % single point for the contour lines. In most cases this point is not
    % relevant because neighboring patches will contribute line segments
    % that run until that point. However, in exceptional cases this point
    % may the only point on the "contour", namely if a (local) minimum or
    % maximum matches a contour level. We exclude such points for the time
    % being.
%     Patch=Patches((CLIndex==3) | (CLIndex==9));
%     if ~isempty(Patch)
%         LvlIndex=LvlOffset+(1:2:(2*length(Patch)));
%         Index=tri(Patch,:);
%         [Dummy,Permutation]=sort(v(Index)==level,2);
%         Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
%         % third element has elevation equal to level
%         Index=Index(:,3);
%         XPoint=x(Index);
%         XLevel(LvlIndex(:))=XPoint(:);
%         YPoint=y(Index);
%         YLevel(LvlIndex(:))=YPoint(:);
%         if zdef
%             ZPoint=z(Index);
%             ZLevel(LvlIndex(:))=ZPoint(:);
%         end
%         LvlOffset=LvlOffset+2*length(Patch);
%     end

%% line along edge
    % patches with two equal: the contour follows exactly an edge.
    % WARNING: this contour segment may be generated twice, which may
    % result in a problem when trying to stitch the polygons together.
    % Let's take a chance since this set will be empty in most cases.
    Patch=Patches((CLIndex==2) | (CLIndex==5));
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index)==thisLevel,2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % first element has elevation different from level
        Index=Index(:,[2 3]);
        XPoint=transpose(x(Index));
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose(y(Index));
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose(z(Index));
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end

%% slice through node
    % patches with one equal, one larger and one smaller
    Patch=Patches(CLIndex==6);
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % first index points to smallest elevation
        % second index points to elevation equal to level
        % third index points to higher elevation
        VTemp=v(Index);
        Lambda=min(1,(thisLevel-VTemp(:,1))./(VTemp(:,3)-VTemp(:,1))); % large - small
        XPoint=transpose([transpose(x(Index(:,1)))+Lambda.*transpose(x(Index(:,3))-x(Index(:,1))) transpose(x(Index(:,2)))]);
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose([transpose(y(Index(:,1)))+Lambda.*transpose(y(Index(:,3))-y(Index(:,1))) transpose(y(Index(:,2)))]);
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose([transpose(z(Index(:,1)))+Lambda.*transpose(z(Index(:,3))-z(Index(:,1))) transpose(z(Index(:,2)))]);
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end

%% slice
    % patches with two larger and one smaller
    Patch=Patches(CLIndex==10);
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index)<thisLevel,2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % first two indices point to levels larger than level
        % last index points to elevation smaller than level
        VTemp=v(Index);
        Lambda=min(1,(thisLevel-VTemp(:,3)*ones(1,2))./(VTemp(:,[1 2])-VTemp(:,3)*ones(1,2))); % large - small
        XPoint=transpose(transpose(x(Index(:,3)))*ones(1,2)+Lambda.*(x(Index(:,[1 2]))-transpose(x(Index(:,3)))*ones(1,2)));
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose(transpose(y(Index(:,3)))*ones(1,2)+Lambda.*(y(Index(:,[1 2]))-transpose(y(Index(:,3)))*ones(1,2)));
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose(transpose(z(Index(:,3)))*ones(1,2)+Lambda.*(z(Index(:,[1 2]))-transpose(z(Index(:,3)))*ones(1,2)));
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end

%% slice
    % patches with two smaller and one larger
    Patch=Patches(CLIndex==7);
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index)>thisLevel,2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % first two indices point to levels smaller than level
        % last index points to elevation larger than level
        VTemp=v(Index);
        Lambda=min(1,(thisLevel-VTemp(:,[1 2]))./(VTemp(:,3)*ones(1,2)-VTemp(:,[1 2]))); % large - small
        XPoint=transpose(x(Index(:,[1 2]))+Lambda.*(transpose(x(Index(:,3)))*ones(1,2)-x(Index(:,[1 2]))));
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose(y(Index(:,[1 2]))+Lambda.*(transpose(y(Index(:,3)))*ones(1,2)-y(Index(:,[1 2]))));
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose(z(Index(:,[1 2]))+Lambda.*(transpose(z(Index(:,3)))*ones(1,2)-z(Index(:,[1 2]))));
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end

%% whole patch
    % patches with three equal
    % Patch=Patches(CLIndex==1);

%% continue
    NonEmptyLevel(LevelNr)=1;
    
    VLevel=thisLevel*ones(size(XLevel));
    
    if getdata
        if zdef
            H{LevelNr}={XLevel YLevel ZLevel LevelNr};
        else
            H{LevelNr}={XLevel YLevel LevelNr};
        end
    elseif isempty(col) && isempty(lin)
        if ~zdef
            ZLevel=VLevel;
        end
        NewH = patch('XData',transpose(XLevel), ...
            'YData',transpose(YLevel), ...
            'ZData',transpose(ZLevel), ...
            'CData',transpose(VLevel), ...
            'facecolor','none', ...
            'edgecolor','flat',...
            'userdata',thisLevel, ...
            'parent',ax);
        H(LevelNr)=NewH;
    else
        if ~zdef
            ZLevel=VLevel;
        end
        NewH = line('XData',XLevel, ...
            'YData',YLevel, ...
            'ZData',ZLevel, ...
            'userdata',thisLevel, ...
            'parent',ax);
        H(LevelNr)=NewH;
    end
end

levels=levels(logical(NonEmptyLevel));

if getdata
    % do nothing
elseif isempty(col) && ~isempty(lin)
    nlvl = length(levels);
    colortab = colortab(mod(1:nlvl,ncol)+1,:);
    for i = 1:length(H)
        set(H(i),'linestyle',lin,'color',colortab(i,:));
    end
else
    if ~isempty(lin)
        set(H,'linestyle',lin);
    end
    if ~isempty(col)
        set(H,'color',col);
    end
end


