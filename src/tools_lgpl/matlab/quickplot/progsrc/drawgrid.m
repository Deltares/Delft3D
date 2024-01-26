function [x,y]=drawgrid(varargin)
%DRAWGRID Plots the grid.
%   DRAWGRID(NFStruct) draws the grid stored in Delft3D result file
%   indicated by the NEFIS data structure NFStruct (as obtained from
%   VS_USE) in the current axes. The routine supports TRIM files, COM files
%   and old TRAM and BOTM files.
%
%   [X,Y]=DRAWGRID(NFStruct) returns X and Y co-ordinates (but does not
%   draw the grid).
%
%   DRAWGRID(X,Y) draws the grid given by the specified X and Y matrices in
%   the current axes.
%
%   ...,'optionname',optionval,...
%   supported option names and values are:
%   * 'color'      value can be RGB-triplet, color character, or
%                  'ortho' for orthogonality of grid
%                  'msmo' for M smoothness of grid
%                  'nsmo' for N smoothness of grid
%   * 'm1n1'       1x2 array containing the starting numbers for the grid
%                  lines in the first and second grid direction. Grid line
%                  numbers start at the user specified values [N M] instead
%                  of the default [1 1].
%   * 'fontsize'   size of the font used for grid numbering (default 4)
%   * 'gridstep'   label step used for grid numbering (default 10)
%                  if gridstep is [], grid lines are not labeled
%   * 'ticklength' length of ticks in axes coordinates or 'auto' (default)
%   * 'parent'     axes handle in which to plot grid (default: gca)
%   * 'clipzero'   by default co-ordinate pairs of (0,0) are
%                  clipped. Set to 'off' to plot co-ordinates
%                  at (0,0).
%   * 'zdata'      value to raise or lower gridplot in 3D-space (default 0)
%
%   Example 1:
%      TRIM = vs_use('d:\demo\trim-s33.dat');
%      drawgrid(TRIM)
%      set(gca,'DataAspectRatio',[1 1 1])
%
%   Example 2:
%      [X,Y] = ndgrid(0:10,0:20);
%      drawgrid(X,Y,'fontsize',8,'color','r','m1n1',[5 0],'clipzero','off')
%      set(gca,'DataAspectRatio',[1 1 1])
%
%   See also: 

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

xcor=[];
ycor=[];
ZData=0;
C=[];
prop='';
P.fontsize=4;
P.m1n1=[1 1];
P.gridstep=10;
P.color='k';
P.parent=[];
P.ticklength='auto';
P.clipzero=1;
i=1;
while i<=nargin
   if isstruct(varargin{i})
      C=varargin{i};
   elseif ischar(varargin{i})
      switch lower(varargin{i})
         case 'color' % ortho, msmo, nsmo
            i=i+1;
            if ischar(varargin{i}) & length(varargin{i})>1
               prop=varargin{i};
            else
               P.color=varargin{i};
            end
         case 'm1n1'
            i=i+1;
            P.m1n1=varargin{i};
         case 'fontsize'
            i=i+1;
            P.fontsize=varargin{i};
         case 'gridstep'
            i=i+1;
            P.gridstep=varargin{i};
         case 'ticklength'
            i=i+1;
            P.ticklength=varargin{i};
         case 'parent'
            i=i+1;
            P.parent=varargin{i};
         case 'clipzero'
            i=i+1;
            P.clipzero=isequal(lower(varargin{i}),'on');
         case 'zdata'
            i=i+1;
            ZData=varargin{i};             
         otherwise
            error('Invalid string argument: %s.',varargin{i})
      end
   elseif isempty(xcor)
      xcor=varargin{i};
   else
      ycor=varargin{i};
   end
   i=i+1;
end

if isempty(C) & isempty(xcor)
   C=vs_use('lastread');
end

if isempty(xcor)
   switch vs_type(C)
      case {'Delft3D-com','Delft3D-tram','Delft3D-botm'}
         xcor=vs_get(C,'GRID','XCOR','quiet');
         ycor=vs_get(C,'GRID','YCOR','quiet');
         [mask,Chk]=vs_get(C,'TEMPOUT','CODB',idx([M_ N_]),'quiet');
         %
         xcor(mask~=1)=NaN;
      case 'Delft3D-trim'
         xcor=vs_get(C,'map-const','XCOR','quiet');
         ycor=vs_get(C,'map-const','YCOR','quiet');
         mask=vs_get(C,'map-const','KCS','quiet');
         mask=conv2(double(mask==1),[1 1;1 1],'same')>0;
         %
         xcor(mask~=1)=NaN;
      otherwise
         error('Invalid NEFIS file for this action.')
   end
end

if P.clipzero
   % clip points with both x and y co-ordinates equal to 0.
   xcor((xcor==0) & (ycor==0))=NaN;
end
ycor(isnan(xcor))=NaN;

% if requested, return x and y co-ordinates.
if nargout==2
   x=xcor;
   y=ycor;
   return
end

if isempty(P.parent)
   P.parent=gca;
end

if strcmp(P.ticklength,'auto')
   % determine appropriate tick length automatically
   mdx=abs(sqrt(diff(xcor,1,2).^2+diff(ycor,1,2).^2));
   mdxval=mdx(~isnan(mdx(:)));
   if ~isempty(mdxval)
      mdx=mean(mdxval);
   else
      mdx=NaN;
   end
   mdy=abs(sqrt(diff(xcor).^2+diff(ycor).^2));
   mdyval=mdy(~isnan(mdy(:)));
   if ~isempty(mdxval)
      mdy=mean(mdyval);
   else
      mdy=NaN;
   end
   mgrd=max([mdx mdy]);
   mdx=max(xcor(:))-min(xcor(:));
   mdy=max(ycor(:))-min(ycor(:));
   mdx=sqrt(mdx^2+mdy^2);
   if ~isnan(mdx)
      mgrd = min(mgrd,mdx/100);
   end
   if isnan(mgrd)
      mgrd=1;
   end
elseif isnumeric(P.ticklength)
   mgrd=P.ticklength;
else
   error('Unknown value for ticklength.')
end

grid1D=0;
if size(xcor,1)==1
   grid1D=1;
   xcor = cat(1,xcor,xcor);
   ycor = cat(1,ycor,ycor);
elseif size(xcor,2)==1
   grid1D=1;
   xcor = cat(2,xcor,xcor);
   ycor = cat(2,ycor,ycor);
end
zcor = zeros(size(xcor))+ZData;
S=surface(xcor,ycor,zcor,'parent',P.parent);
set(S,'facecolor','none','edgecolor',P.color,'linewidth',0.00001);

if ~isempty(prop)
   cdata=[];
   switch lower(prop)
      case 'msmo'
         cdata=Local_smooth(xcor,ycor);
         cthr=[1:.1:2 2.5 3];
      case 'nsmo'
         cdata=Local_smooth(xcor',ycor')';
         cthr=[1:.1:2 2.5 3];
      case 'ortho'
         cdata=Local_orthog(xcor,ycor);
         cthr=[0:.01:0.1 0.15 0.2];
      otherwise
         warning(sprintf('Unknown property: %s.',prop));
   end
   if ~isempty(cdata)
      %cdata(isnan(xcor))=NaN;
      %ycor(isnan(xcor))=min(ycor(:));
      %xcor(isnan(xcor))=min(xcor(:));
      %cntrs=contourf(xcor,ycor,cdata,cthr);
      %
      %S(2)=surface(xcor,ycor,zeros(size(xcor)),cdata,'edgecolor','flat','facecolor','none','linestyle','none','marker','.');
      %
      set(S,'cdata',cdata,'facecolor','interp');
   end
end

P.m1n1=P.m1n1-1;
h=S;
if ~isempty(P.gridstep) & ~grid1D
   if isequal(size(P.gridstep),[1 1])
      P.gridstep=P.gridstep+[0 0];
   end
   P.gridstep=max(1,round(P.gridstep));
   mstep=P.gridstep(1);
   vm=mstep-rem(P.m1n1(1),mstep):mstep:size(xcor,1);
   if isempty(vm)
      vm=P.m1n1(1);
   end
   if (vm(1)-1)>=mstep/2
      vm=[1 vm];
   else
      vm(1)=1;
   end
   if (size(xcor,1)+P.m1n1(1)-floor((size(xcor,1)+P.m1n1(1))/mstep)*mstep)<mstep/2
      vm(end)=size(xcor,1);
   else
      vm(end+1)=size(xcor,1);
   end
   for m=vm
      hi=procrow(xcor(m,:),ycor(m,:),zcor(m,:),m+P.m1n1(1),mgrd,P);
      h=cat(2,h,hi);
   end
   nstep=P.gridstep(2);
   vn=nstep-rem(P.m1n1(2),nstep):nstep:size(xcor,2);
   if isempty(vn)
      vn=P.m1n1(2);
   end
   if (vn(1)-1)>=nstep/2
      vn=[1 vn];
   else
      vn(1)=1;
   end
   if (size(xcor,2)+P.m1n1(2)-floor((size(xcor,2)+P.m1n1(2))/nstep)*nstep)<nstep/2
      vn(end)=size(xcor,2);
   else
      vn(end+1)=size(xcor,2);
   end
   for n=vn
      hi=procrow(xcor(:,n),ycor(:,n),zcor(:,n),n+P.m1n1(2),mgrd,P);
      h=cat(2,h,hi);
   end
end
if nargout>0
   x=h;
end


function SMO=Local_smooth(X,Y)
% smoothness in M direction (N direction by (X',Y')')
[m n]=size(X);
SMO=repmat(NaN,m,n);
SMO(1:m-1,:)=sqrt(diff(X).^2+diff(Y).^2);
SMO(2:m-1,:)=SMO(1:m-2,:)./SMO(2:m-1,:);
SMO([1 m],:)=NaN;
SMO=max(SMO,1./SMO);
%SMO(:,1:n-1)=max(SMO(:,1:n-1),SMO(:,2:n));


function ORTH=Local_orthog(X,Y)
[m n]=size(X);
dx1=repmat(NaN,m,n); dy1=dx1;
dx1(2:m-1,:)=X(3:m,:)-X(1:m-2,:);
dy1(2:m-1,:)=Y(3:m,:)-Y(1:m-2,:);
ind=find(isnan(dx1));
[mi,ni]=ind2sub([m n],ind);
ind1=ind(mi<m);
dx1(ind1)=X(ind1+1)-X(ind1);
dy1(ind1)=Y(ind1+1)-Y(ind1);
ind=ind(isnan(dx1(ind)));
[mi,ni]=ind2sub([m n],ind);
ind1=ind(mi>1);
dx1(ind1)=X(ind1)-X(ind1-1);
dy1(ind1)=Y(ind1)-Y(ind1-1);

dx2=repmat(NaN,m,n); dy2=dx2;
dx2(:,2:n-1)=X(:,3:n)-X(:,1:n-2);
dy2(:,2:n-1)=Y(:,3:n)-Y(:,1:n-2);
ind=find(isnan(dx2));
[mi,ni]=ind2sub([m n],ind);
ind1=ind(ni<n);
dx2(ind1)=X(ind1+m)-X(ind1);
dy2(ind1)=Y(ind1+m)-Y(ind1);
ind=ind(isnan(dx2(ind)));
[mi,ni]=ind2sub([m n],ind);
ind1=ind(ni>1);
dx2(ind1)=X(ind1)-X(ind1-m);
dy2(ind1)=Y(ind1)-Y(ind1-m);

ds1sq=dx1.^2+dy1.^2;
ds2sq=dx2.^2+dy2.^2;
ds3sq=(dx1+dx2).^2+(dy1+dy2).^2;
ORTH=abs((-ds3sq+ds1sq+ds2sq)./(2*sqrt(ds1sq.*ds2sq)));
%[m n]=size(X);
%dM=repmat(NaN,m+1,n);
%dM(2:m,:)=sqrt(diff(X).^2+diff(Y).^2);
%dN=repmat(NaN,m,n+1);
%dN(:,2:n)=sqrt(diff(X,1,2).^2+diff(Y,1,2).^2);
%D1=repmat(NaN,m+1,n+1);
%D1(2:m,2:n)=sqrt((X(2:m,1:n-1)-X(1:m-1,2:n)).^2+(Y(2:m,1:n-1)-Y(1:m-1,2:n)).^2);
%D2=repmat(NaN,m+1,n+1);
%D2(2:m,2:n)=sqrt((X(1:m-1,1:n-1)-X(2:m,2:n)).^2+(Y(1:m-1,1:n-1)-Y(2:m,2:n)).^2);
%cos1=(dM(2:m+1,:).^2+dN(:,2:n+1).^2-D1(2:m+1,2:n+1).^2)./(2*dM(2:m+1,:).*dN(:,2:n+1));
%cos2=(dM(1:m,:).^2+dN(:,2:n+1).^2-D2(1:m,2:n+1).^2)./(2*dM(1:m,:).*dN(:,2:n+1));
%cos3=(dM(1:m,:).^2+dN(:,1:n).^2-D1(1:m,1:n).^2)./(2*dM(1:m,:).*dN(:,1:n));
%cos4=(dM(2:m+1,:).^2+dN(:,1:n).^2-D2(2:m+1,1:n).^2)./(2*dM(2:m+1,:).*dN(:,1:n));
%ORTH=max(max(abs(cos1),abs(cos2)),max(abs(cos3),abs(cos4)));


function h=procrow(X,Y,Z,m,mgrd,P)
h=[];
Act=~isnan(X);
if Act(1)
   n=1;
   if (n<length(X)) & ~isnan(X(n+1))
      dx=X(n)-X(n+1);
      dy=Y(n)-Y(n+1);
      dz=Z(n)-Z(n+1);
      if dx~=0 | dy~=0
         scale=mgrd/(sqrt(dx^2+dy^2));
         dx=scale*dx;
         dy=scale*dy;
         dz=scale*dz;
         h(1)=line(X(n)+[0 dx],Y(n)+[0 dy],Z(n)+[0 dz],'color',P.color,'linewidth',0.00001,'parent',P.parent);
         h(2)=textalign(m,X(n),dx,Y(n),dy,Z(n),dz,P);
      end
   else
      h(1)=text(X(n),Y(n),Z(n),int2str(m),'clipping','on','fontsize',P.fontsize,'color',P.color,'parent',P.parent);
   end
end
if Act(end)
   n=length(Act);
   if (n>1) & ~isnan(X(n-1))
      dx=X(n)-X(n-1);
      dy=Y(n)-Y(n-1);
      dz=Z(n)-Z(n-1);
      if dx~=0 | dy~=0
         scale=mgrd/(sqrt(dx^2+dy^2));
         dx=scale*dx;
         dy=scale*dy;
         dz=scale*dz;
         h(end+1)=line(X(n)+[0 dx],Y(n)+[0 dy],Z(n)+[0 dz],'color',P.color,'linewidth',0.00001,'parent',P.parent);
         h(end+1)=textalign(m,X(n),dx,Y(n),dy,Z(n),dz,P);
      end
   else
      h(end+1)=text(X(n),Y(n),Z(n),int2str(m),'clipping','on','fontsize',P.fontsize,'color',P.color,'parent',P.parent);
   end
end
D=diff(Act(:)'); % force row vector
Di=find(D);      % row vector because D is a rowvector
for i=Di
   if D(i)<0 % -1
      n=i;
      if (n>1) & ~isnan(X(n-1)),
         dx=X(n)-X(n-1);
         dy=Y(n)-Y(n-1);
         dz=Z(n)-Z(n-1);
         if dx~=0 | dy~=0
            scale=mgrd/(sqrt(dx^2+dy^2));
            dx=scale*dx;
            dy=scale*dy;
            dz=scale*dz;
            h(end+1)=line(X(n)+[0 dx],Y(n)+[0 dy],Z(n)+[0 dz],'color',P.color,'linewidth',0.00001,'parent',P.parent);
            h(end+1)=textalign(m,X(n),dx,Y(n),dy,Z(n),dz,P);
         end
      else
         h(end+1)=text(X(n),Y(n),Z(n),int2str(m),'clipping','on','fontsize',P.fontsize,'color',P.color,'parent',P.parent);
      end
   else
      n=i+1;
      if (n<length(X)) & ~isnan(X(n+1))
         dx=X(n)-X(n+1);
         dy=Y(n)-Y(n+1);
         dz=Z(n)-Z(n+1);
         if dx~=0 | dy~=0
            scale=mgrd/(sqrt(dx^2+dy^2));
            dx=scale*dx;
            dy=scale*dy;
            dz=scale*dz;
            h(end+1)=line(X(n)+[0 dx],Y(n)+[0 dy],Z(n)+[0 dz],'color',P.color,'linewidth',0.00001,'parent',P.parent);
            h(end+1)=textalign(m,X(n),dx,Y(n),dy,Z(n),dz,P);
         end
      else
         h(end+1)=text(X(n),Y(n),Z(n),int2str(m),'clipping','on','fontsize',P.fontsize,'color',P.color,'parent',P.parent);
      end
   end
end


function T=textalign(m,x1,dx,y1,dy,z1,dz,P)
T=text(x1+dx,y1+dy,z1+dz,int2str(m),'clipping','on','fontsize',P.fontsize,'color',P.color,'parent',P.parent);
angl=atan2(dy,dx);
if (angl<=pi/2) & (angl>-pi/2)
   set(T,'rotation',angl*180/pi);
else
   set(T,'rotation',angl*180/pi+180,'horizontalalignment','right');
end
