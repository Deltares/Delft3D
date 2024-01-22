function H=tricontourf(tri,x,y,z,v,varargin)
%TRICONTOURF Filled contour plot for triangulated data.
%   TRICONTOURF(TRI,X,Y,Z,Levels) fills the areas between (and above
%   the highest and below the lowest) the contours specified in the
%   Levels array. The contour lines can be plotted using the
%   TRICONTOUR command NaN's in the data leave holes in the filled
%   contour plot.
%
%   TRICONTOURF(TRI,X,Y,Z,V,Levels) contouring done based on V
%   instead of Z. This allows contoured data on 3D surfaces.
%
%   H = TRICONTOURF(...) a vector H of handles to PATCH objects,
%   one handle per contourrange. TRICONTOURF is not compatible with
%   CLABEL. The CData field of the patches contains the number of
%   the contour class (1 for the area below the lowest threshold,
%   ... until ... N+1 for the area above the highest threshold).
%
%   Options
%     Return: [ data | {handles} ]
%     ZPlane: ZVal (contours projected on the plane z=ZVal
%     CLevel: [ min | max | index | index0 | {classic} ]
%
%   Example
%      x=rand(20); y=rand(20); z=rand(20); tri=delaunay(x,y);
%      thresholds=.3:.1:.8;
%      subplot(2,2,1)
%      tricontourf(tri,x,y,z,thresholds); title('classic'); colorbar
%      subplot(2,2,2)
%      tricontourf(tri,x,y,z,thresholds,'clevel','min');
%      title('min'); classbar(colorbar,thresholds)
%      subplot(2,2,3)
%      tricontourf(tri,x,y,z,thresholds,'clevel','max');
%      title('max'); classbar(colorbar,thresholds,'max')
%      subplot(2,2,4)
%      tricontourf(tri,x,y,z,thresholds,'clevel','index');
%      title('index'); colorbar
%      classbar(colorbar,1:length(thresholds)+1,'labelcolor','label',thresholds)
%
%   See also CONTOUR, CONTOURF, TRICONTOUR

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

ZPlane=NaN;
getdata=0;
clevelname='classic';

if nargin<5
    error('Not enough input arguments.')
else
    i0=0;
    if isequal(size(z),size(v))
        i0=1;
        levels=varargin{1};
    else
        levels=v;
        v=z;
        z=[];
    end
end
if mod(nargin-5-i0,2)~=0
    error('Invalid number of input arguments.')
else
    for i=(1+i0):2:length(varargin)
        switch lower(varargin{i})
            case 'zplane'
                ZPlane=varargin{i+1};
                if ~isnumeric(ZPlane) || ~isequal(size(ZPlane),[1 1])
                    error('Invalid ZPlane option.')
                end
            case 'return'
                getdata=strcmp('data',varargin{i+1});
            case 'clevel'
                switch lower(varargin{i+1})
                    case {'min','max','index','index0','classic'}
                        clevelname=lower(varargin{i+1});
                    otherwise
                        error('Invalid CLevel name')
                end
            otherwise
                error('Invalid option %i: %s.',i,varargin{i}(:)')
        end
    end
end

zdef=~isempty(z);
x=x(:);
y=y(:);
if zdef
    z=z(:);
end
v=v(:);

Patches=1:size(tri,1);
if getdata
    H={};
else
    H=[];
end
NonEmptyLevel=zeros(size(levels));
maxfinite=max(max(levels),max(v(isfinite(v(:)))));
minfinite=min(min(levels),min(v(isfinite(v(:)))));
v(v(:)==-inf)=-realmax;
v(v(:)==+inf)=realmax;

for LevelNr=1:length(levels)+1
    if (LevelNr==1)
        if length(levels)==0
            level=minfinite;
        else
            level=levels(LevelNr);
        end
        plevel=minfinite;
        Smaller=logical(any(isnan(v(tri)),2)*[1 1 1]);
        Nsmaller=sum(Smaller,2);
        Larger=(v(tri)>=level) & ~Smaller;
        Nlarger=sum(Larger,2);
    elseif (LevelNr>length(levels))
        plevel=level;
        level=maxfinite;
        Nsmaller=3-Nlarger;
        Smaller=~Larger;
        Larger=logical(zeros(size(tri)));
        Nlarger=zeros(size(tri,1),1);
    else
        plevel=level;
        level=levels(LevelNr);
        Nsmaller=3-Nlarger;
        Smaller=~Larger;
        Larger=Larger & (v(tri)>=level);
        Nlarger=sum(Larger,2);
    end
    CLIndex=6-Nsmaller+3*Nlarger; % Nsmaller+2*(3-Nsmaller-Nlarger)+5*Nlarger;

    NTriangles=[0 0 0 1 2 1 2 3 2 0 2 1 0 0 0 0];
    NPoints =  [0 0 0 3 4 3 4 5 4 0 4 3 0 0 0 0];

    NPoints=sum(NPoints(CLIndex));
    Coord=zeros([NPoints 3+zdef]);
    NTriangles=sum(NTriangles(CLIndex));
    TRI=ones(NTriangles,3);
    TRIOffset=0;
    PNTOffset=0;

    % patches with three smaller
    %Patch=Patches(CLIndex==3);

    % patches with two smaller and one inside level
    Patch=Patches(CLIndex==4);
    if ~isempty(Patch)
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(reshape(v(Index),[length(Patch) 3]),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);

        XPoint=zeros(3,size(Index,1));
        YPoint=zeros(3,size(Index,1));
        if zdef
            ZPoint=zeros(3,size(Index,1));
        end
        VPoint=zeros(3,size(Index,1));
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(1,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(1,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(1,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        Lambda=(plevel-v(Index(:,2)))./(v(Index(:,3))-v(Index(:,2)));
        XPoint(2,:)=transpose(x(Index(:,2))+Lambda.*(x(Index(:,3))-x(Index(:,2))));
        YPoint(2,:)=transpose(y(Index(:,2))+Lambda.*(y(Index(:,3))-y(Index(:,2))));
        if zdef
            ZPoint(2,:)=transpose(z(Index(:,2))+Lambda.*(z(Index(:,3))-z(Index(:,2))));
        end
        VPoint(1:2,:)=plevel;
        XPoint(3,:)=transpose(x(Index(:,3)));
        YPoint(3,:)=transpose(y(Index(:,3)));
        if zdef
            ZPoint(3,:)=transpose(z(Index(:,3)));
        end
        VPoint(3,:)=transpose(v(Index(:,3)));

        PNTIndex=PNTOffset+(1:(3*length(Patch)));
        Coord(PNTIndex,1)=XPoint(:);
        Coord(PNTIndex,2)=YPoint(:);
        if zdef
            Coord(PNTIndex,3)=ZPoint(:);
        end
        Coord(PNTIndex,3+zdef)=VPoint(:);
        PNTOffset=PNTOffset+3*length(Patch);

        TRIIndex=TRIOffset+(1:length(Patch));
        TRI(TRIIndex,:)=transpose(reshape(PNTIndex,[3 length(Patch)]));
        TRIOffset=TRIOffset+length(Patch);
    end

    % patches with one smaller and two inside level
    Patch=Patches(CLIndex==5);
    if ~isempty(Patch)
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(reshape(v(Index),[length(Patch) 3]),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);

        XPoint=zeros(4,size(Index,1));
        YPoint=zeros(4,size(Index,1));
        if zdef
            ZPoint=zeros(4,size(Index,1));
        end
        VPoint=zeros(4,size(Index,1));
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,2))-v(Index(:,1)));
        XPoint(2,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,2))-x(Index(:,1))));
        YPoint(2,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,2))-y(Index(:,1))));
        if zdef
            ZPoint(2,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,2))-z(Index(:,1))));
        end
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(3,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(3,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(3,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        VPoint(2:3,:)=plevel;
        XPoint(1,:)=transpose(x(Index(:,2)));
        YPoint(1,:)=transpose(y(Index(:,2)));
        if zdef
            ZPoint(1,:)=transpose(z(Index(:,2)));
        end
        VPoint(1,:)=transpose(v(Index(:,2)));
        XPoint(4,:)=transpose(x(Index(:,3)));
        YPoint(4,:)=transpose(y(Index(:,3)));
        if zdef
            ZPoint(4,:)=transpose(z(Index(:,3)));
        end
        VPoint(4,:)=transpose(v(Index(:,3)));

        PNTIndex=PNTOffset+(1:(4*length(Patch)));
        Coord(PNTIndex,1)=XPoint(:);
        Coord(PNTIndex,2)=YPoint(:);
        if zdef
            Coord(PNTIndex,3)=ZPoint(:);
        end
        Coord(PNTIndex,3+zdef)=VPoint(:);

        TRIIndex=TRIOffset+(1:2:(2*length(Patch)));
        PNTIndex=PNTOffset-1+(1:4:(4*length(Patch)));
        TRI(TRIIndex,:)=transpose(ones(3,1)*PNTIndex+[1;2;3]*ones(1,length(Patch)));
        TRI(TRIIndex+1,:)=transpose(ones(3,1)*PNTIndex+[1;3;4]*ones(1,length(Patch)));
        TRIOffset=TRIOffset+2*length(Patch);

        PNTOffset=PNTOffset+4*length(Patch);
    end

    % patches with three inside level
    Patch=Patches(CLIndex==6);
    if ~isempty(Patch)
        Index=reshape(transpose(tri(Patch,:)),[3*length(Patch) 1]);

        PNTIndex=PNTOffset+(1:(3*length(Patch)));
        Coord(PNTIndex,1)=x(Index);
        Coord(PNTIndex,2)=y(Index);
        if zdef
            Coord(PNTIndex,3)=z(Index);
        end
        Coord(PNTIndex,3+zdef)=v(Index);
        PNTOffset=PNTOffset+3*length(Patch);

        TRIIndex=TRIOffset+(1:length(Patch));
        TRI(TRIIndex,:)=transpose(reshape(PNTIndex,[3 length(Patch)]));
        TRIOffset=TRIOffset+length(Patch);
    end

    % patches with two smaller and one larger
    Patch=Patches(CLIndex==7);
    if ~isempty(Patch)
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(reshape(v(Index),[length(Patch) 3]),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);

        XPoint=zeros(4,size(Index,1));
        YPoint=zeros(4,size(Index,1));
        if zdef
            ZPoint=zeros(4,size(Index,1));
        end
        VPoint=zeros(4,size(Index,1));
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(2,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(2,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(2,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        Lambda=(plevel-v(Index(:,2)))./(v(Index(:,3))-v(Index(:,2)));
        XPoint(3,:)=transpose(x(Index(:,2))+Lambda.*(x(Index(:,3))-x(Index(:,2))));
        YPoint(3,:)=transpose(y(Index(:,2))+Lambda.*(y(Index(:,3))-y(Index(:,2))));
        if zdef
            ZPoint(3,:)=transpose(z(Index(:,2))+Lambda.*(z(Index(:,3))-z(Index(:,2))));
        end
        VPoint(2:3,:)=plevel;
        Lambda=(level-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(1,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(1,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(1,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        Lambda=(level-v(Index(:,2)))./(v(Index(:,3))-v(Index(:,2)));
        XPoint(4,:)=transpose(x(Index(:,2))+Lambda.*(x(Index(:,3))-x(Index(:,2))));
        YPoint(4,:)=transpose(y(Index(:,2))+Lambda.*(y(Index(:,3))-y(Index(:,2))));
        if zdef
            ZPoint(4,:)=transpose(z(Index(:,2))+Lambda.*(z(Index(:,3))-z(Index(:,2))));
        end
        VPoint([1 4],:)=level;

        PNTIndex=PNTOffset+(1:(4*length(Patch)));
        Coord(PNTIndex,1)=XPoint(:);
        Coord(PNTIndex,2)=YPoint(:);
        if zdef
            Coord(PNTIndex,3)=ZPoint(:);
        end
        Coord(PNTIndex,3+zdef)=VPoint(:);

        TRIIndex=TRIOffset+(1:2:(2*length(Patch)));
        PNTIndex=PNTOffset-1+(1:4:(4*length(Patch)));
        TRI(TRIIndex,:)=transpose(ones(3,1)*PNTIndex+[1;2;3]*ones(1,length(Patch)));
        TRI(TRIIndex+1,:)=transpose(ones(3,1)*PNTIndex+[1;3;4]*ones(1,length(Patch)));
        TRIOffset=TRIOffset+2*length(Patch);

        PNTOffset=PNTOffset+4*length(Patch);
    end

    % patches with one smaller, one larger and one inside level
    Patch=Patches(CLIndex==8);
    if ~isempty(Patch)
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(reshape(v(Index),[length(Patch) 3]),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);

        XPoint=zeros(5,size(Index,1));
        YPoint=zeros(5,size(Index,1));
        if zdef
            ZPoint=zeros(5,size(Index,1));
        end
        VPoint=zeros(5,size(Index,1));
        Lambda=(level-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(2,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(2,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(2,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        Lambda=(level-v(Index(:,2)))./(v(Index(:,3))-v(Index(:,2)));
        XPoint(3,:)=transpose(x(Index(:,2))+Lambda.*(x(Index(:,3))-x(Index(:,2))));
        YPoint(3,:)=transpose(y(Index(:,2))+Lambda.*(y(Index(:,3))-y(Index(:,2))));
        if zdef
            ZPoint(3,:)=transpose(z(Index(:,2))+Lambda.*(z(Index(:,3))-z(Index(:,2))));
        end
        VPoint(2:3,:)=level;
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(1,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(1,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(1,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,2))-v(Index(:,1)));
        XPoint(4,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,2))-x(Index(:,1))));
        YPoint(4,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,2))-y(Index(:,1))));
        if zdef
            ZPoint(4,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,2))-z(Index(:,1))));
        end
        VPoint([1 4],:)=plevel;
        XPoint(5,:)=transpose(x(Index(:,2)));
        YPoint(5,:)=transpose(y(Index(:,2)));
        if zdef
            ZPoint(5,:)=transpose(z(Index(:,2)));
        end
        VPoint(5,:)=transpose(v(Index(:,2)));

        PNTIndex=PNTOffset+(1:(5*length(Patch)));
        Coord(PNTIndex,1)=XPoint(:);
        Coord(PNTIndex,2)=YPoint(:);
        if zdef
            Coord(PNTIndex,3)=ZPoint(:);
        end
        Coord(PNTIndex,3+zdef)=VPoint(:);

        TRIIndex=TRIOffset+(1:3:(3*length(Patch)));
        PNTIndex=PNTOffset-1+(1:5:(5*length(Patch)));
        TRI(TRIIndex,:)=transpose(ones(3,1)*PNTIndex+[1;2;3]*ones(1,length(Patch)));
        TRI(TRIIndex+1,:)=transpose(ones(3,1)*PNTIndex+[1;3;4]*ones(1,length(Patch)));
        TRI(TRIIndex+2,:)=transpose(ones(3,1)*PNTIndex+[3;4;5]*ones(1,length(Patch)));
        TRIOffset=TRIOffset+3*length(Patch);

        PNTOffset=PNTOffset+5*length(Patch);
    end

    % patches with one larger and two inside level
    Patch=Patches(CLIndex==9);
    if ~isempty(Patch)
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(reshape(v(Index),[length(Patch) 3]),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        XPoint=zeros(4,size(Index,1));
        YPoint=zeros(4,size(Index,1));
        if zdef
            ZPoint=zeros(4,size(Index,1));
        end
        VPoint=zeros(4,size(Index,1));
        if size(Index,1)==1
            X=x(Index);
            Y=y(Index);
            if zdef
                Z=z(Index);
            end
            V=v(Index);
        else
            X=transpose(x(Index));
            Y=transpose(y(Index));
            if zdef
                Z=transpose(z(Index));
            end
            V=transpose(v(Index));
        end;
        Lambda=(level-V(1,:))./(V(3,:)-V(1,:));
        XPoint(2,:)=X(1,:)+Lambda.*(X(3,:)-X(1,:));
        YPoint(2,:)=Y(1,:)+Lambda.*(Y(3,:)-Y(1,:));
        if zdef
            ZPoint(2,:)=Z(1,:)+Lambda.*(Z(3,:)-Z(1,:));
        end
        Lambda=(level-V(2,:))./(V(3,:)-V(2,:));
        XPoint(3,:)=X(2,:)+Lambda.*(X(3,:)-X(2,:));
        YPoint(3,:)=Y(2,:)+Lambda.*(Y(3,:)-Y(2,:));
        if zdef
            ZPoint(3,:)=Z(2,:)+Lambda.*(Z(3,:)-Z(2,:));
        end
        VPoint(2:3,:)=level;
        XPoint(1,:)=X(1,:);
        YPoint(1,:)=Y(1,:);
        if zdef
            ZPoint(1,:)=Z(1,:);
        end
        VPoint(1,:)=V(1,:);
        XPoint(4,:)=X(2,:);
        YPoint(4,:)=Y(2,:);
        if zdef
            ZPoint(4,:)=Z(2,:);
        end
        VPoint(4,:)=V(2,:);

        PNTIndex=PNTOffset+(1:(4*length(Patch)));
        Coord(PNTIndex,1)=XPoint(:);
        Coord(PNTIndex,2)=YPoint(:);
        if zdef
            Coord(PNTIndex,3)=ZPoint(:);
        end
        Coord(PNTIndex,3+zdef)=VPoint(:);

        TRIIndex=TRIOffset+(1:2:(2*length(Patch)));
        PNTIndex=PNTOffset-1+(1:4:(4*length(Patch)));
        TRI(TRIIndex,:)=transpose(ones(3,1)*PNTIndex+[1;2;3]*ones(1,length(Patch)));
        TRI(TRIIndex+1,:)=transpose(ones(3,1)*PNTIndex+[1;3;4]*ones(1,length(Patch)));
        TRIOffset=TRIOffset+2*length(Patch);

        PNTOffset=PNTOffset+4*length(Patch);
    end

    % patches with one smaller and two larger
    Patch=Patches(CLIndex==11);
    if ~isempty(Patch)
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(reshape(v(Index),[length(Patch) 3]),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);

        XPoint=zeros(4,size(Index,1));
        YPoint=zeros(4,size(Index,1));
        if zdef
            ZPoint=zeros(4,size(Index,1));
        end
        VPoint=zeros(4,size(Index,1));
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,2))-v(Index(:,1)));
        XPoint(2,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,2))-x(Index(:,1))));
        YPoint(2,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,2))-y(Index(:,1))));
        if zdef
            ZPoint(2,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,2))-z(Index(:,1))));
        end
        Lambda=(plevel-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(3,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(3,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(3,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        VPoint(2:3,:)=plevel;
        Lambda=(level-v(Index(:,1)))./(v(Index(:,2))-v(Index(:,1)));
        XPoint(1,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,2))-x(Index(:,1))));
        YPoint(1,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,2))-y(Index(:,1))));
        if zdef
            ZPoint(1,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,2))-z(Index(:,1))));
        end
        Lambda=(level-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(4,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(4,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(4,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        VPoint([1 4],:)=level;

        PNTIndex=PNTOffset+(1:(4*length(Patch)));
        Coord(PNTIndex,1)=XPoint(:);
        Coord(PNTIndex,2)=YPoint(:);
        if zdef
            Coord(PNTIndex,3)=ZPoint(:);
        end
        Coord(PNTIndex,3+zdef)=VPoint(:);

        TRIIndex=TRIOffset+(1:2:(2*length(Patch)));
        PNTIndex=PNTOffset-1+(1:4:(4*length(Patch)));
        TRI(TRIIndex,:)=transpose(ones(3,1)*PNTIndex+[1;2;3]*ones(1,length(Patch)));
        TRI(TRIIndex+1,:)=transpose(ones(3,1)*PNTIndex+[1;3;4]*ones(1,length(Patch)));
        TRIOffset=TRIOffset+2*length(Patch);

        PNTOffset=PNTOffset+4*length(Patch);
    end

    % patches with two larger and one inside level
    Patch=Patches(CLIndex==12);
    if ~isempty(Patch)
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(reshape(v(Index),[length(Patch) 3]),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);

        XPoint=zeros(3,size(Index,1));
        YPoint=zeros(3,size(Index,1));
        if zdef
            ZPoint=zeros(3,size(Index,1));
        end
        VPoint=zeros(3,size(Index,1));
        Lambda=(level-v(Index(:,1)))./(v(Index(:,2))-v(Index(:,1)));
        XPoint(1,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,2))-x(Index(:,1))));
        YPoint(1,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,2))-y(Index(:,1))));
        if zdef
            ZPoint(1,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,2))-z(Index(:,1))));
        end
        Lambda=(level-v(Index(:,1)))./(v(Index(:,3))-v(Index(:,1)));
        XPoint(2,:)=transpose(x(Index(:,1))+Lambda.*(x(Index(:,3))-x(Index(:,1))));
        YPoint(2,:)=transpose(y(Index(:,1))+Lambda.*(y(Index(:,3))-y(Index(:,1))));
        if zdef
            ZPoint(2,:)=transpose(z(Index(:,1))+Lambda.*(z(Index(:,3))-z(Index(:,1))));
        end
        VPoint(1:2,:)=level;
        XPoint(3,:)=transpose(x(Index(:,1)));
        YPoint(3,:)=transpose(y(Index(:,1)));
        if zdef
            ZPoint(3,:)=transpose(z(Index(:,1)));
        end
        VPoint(3,:)=transpose(v(Index(:,1)));

        PNTIndex=PNTOffset+(1:(3*length(Patch)));
        Coord(PNTIndex,1)=XPoint(:);
        Coord(PNTIndex,2)=YPoint(:);
        if zdef
            Coord(PNTIndex,3)=ZPoint(:);
        end
        Coord(PNTIndex,3+zdef)=VPoint(:);
        PNTOffset=PNTOffset+3*length(Patch);

        TRIIndex=TRIOffset+(1:length(Patch));
        TRI(TRIIndex,:)=transpose(reshape(PNTIndex,[3 length(Patch)]));
        TRIOffset=TRIOffset+length(Patch);
    end

    % patches with three larger
    %Patch=Patches(CLIndex==15);

    Coord(Coord(:,3+zdef)==realmax,3+zdef)=maxfinite; % Coord(~isfinite(Coord(:,3))&(Coord(:,3)>0),3)=maxfinite;
    Coord(Coord(:,3+zdef)==-realmax,3+zdef)=minfinite; % Coord(~isfinite(Coord(:,3))&(Coord(:,3)<0),3)=minfinite;

    [Coord,I,J]=unique(Coord,'rows');
    TRI=J(TRI);
    if getdata
        H{LevelNr}={Coord(:,1:3) TRI LevelNr};
    elseif ~isempty(Coord)
        NonEmptyLevel(LevelNr)=1;
        if ~isnan(ZPlane)
            Coord(:,3)=ZPlane;
        end
        userdataopt={};
        switch clevelname
            case 'min'
                clevel=plevel;
            case 'max'
                clevel=level;
            case 'index'
                clevel=LevelNr;
            case 'index0'
                clevel=LevelNr-1;
            case 'classic'
                clevel=LevelNr;
                userdataopt={'userdata',level};
        end
        visopt={};
        if ~isfinite(clevel)
            visopt={'visible','off'};
        end
        NewH = patch('Vertices',Coord(:,1:3), ...
            'Faces',TRI, ...
            'facevertexcdata',clevel*ones(size(TRI,1),1), ...
            'edgecolor','none', ...
            'facecolor','flat', ...
            userdataopt{:}, ...
            visopt{:});
        setappdata(NewH,'MinThreshold',plevel);
        setappdata(NewH,'MaxThreshold',level);
        H=[H NewH];
    end
end
