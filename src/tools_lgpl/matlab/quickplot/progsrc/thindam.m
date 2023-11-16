function varargout=thindam(varargin)
%THINDAM Plot dams, weirs and vanes.
%   THINDAM(NFStruct,T)
%   plots the KFU/V dams for time step T if T>0
%   plots the KCU/V thin dams if T==0 (default)
%
%   THINDAM(NFStruct,T,'3D')
%   same as above but now the dams will be
%   plotted with as 3D surfaces based on the
%   bottom and waterlevel information in the
%   specified NEFIS file.
%
%
%   THINDAM(XCOR,YCOR,UDAM,VDAM);
%   plots the dams on the specified grid.
%
%   The XCOR and YCOR represent the bottom points.
%   Valid entries for UDAM and VDAM are:
%
%   1) 0/1 matrix of same size as XCOR/YCOR
%      1 = dam, 0 = no dam
%
%   2) D x 2 matrix specifying the M,N coordinates
%      of the dams
%
%   3) D x 4 matrix specifying the begin/end M,N
%      coordinates of the dams like in the Delft3D
%      input files.
%
%   THINDAM(..., 'bottom',<bottom_args>, ...
%                'top',<top_args>)
%   plots the dams as 3D surfaces with specified
%   top and bottom elevations. The elevation data
%   should be specified with positive direction up.
%
%   Additional options:
%
%   * 'parent',axes handle
%     the dams/vanes are plotted in the specified
%     axes instead of the current axes.
%
%   * 'angle',<angle_args>
%     the dams/vanes are rotated to match the specified
%     angle in degrees with respect to the positive X-axis
%     (positive angle in anti-clockwise direction).
%
%   * 'color',<color_args>
%     the dams are colored using the specified data.
%
%   * 'thickness',<thickness_args>
%     the thickness of the dams is specified (default 0).
%
%   If a color data field has been specified, there are
%   a few more options available:
%
%   * 'shape',<type> (default 'dam')
%     alternative: 'rhombus'.
%   * 'drawdams',<onoff> (default 'on')
%     set to 'off' if you don't want the dams.
%   * 'drawlabels',<ofoff> (default 'off')
%     set to 'on' if you want to plot the color values as
%     text.
%   * 'labelformat',<format> (default '%g')
%     defines the format for displaying the values.
%   * 'fontsize',<size> (default 4)
%     defines the size of the values.
%
%   Valid entries for all <..._args> are:
%
%   * a constant, uniform value for alle dams
%
%   * a matrix of same size as XCOR/YCOR containing
%     elevations in the depth points.
%
%   * a matrix of same size as XCOR/YCOR containing
%     elevations in the waterlevel points. To
%     distinguish this entry from the former you
%     need to add the string 'H' or 'S' as an extra
%     argument after the matrix:
%        THINDAM(... ,ELEVMATRIX,'H', ...)
%     The elevation will be used uniformly along
%     each elementary dam.
%
%   * two matrices of same size as XCOR/YCOR containing
%     elevations in the U resp. V points.
%
%   * two D x s arrays specifying the height of the
%     individual dams. If the array is D x 1 the
%     elevation is taken uniformly, is the array is
%     D x 2 the first elevation is taken for the
%     dam end with lowest (M,N) the second elevation
%     for the dam end with highest (M,N). The first
%     array specifies the elevation for dams in U
%     direction the second array for dams in the V
%     direction:
%        THINDAM(... ,ELEVU,ELEVV, ...)
%     This option cannot be used in combination with
%     option 1 for the UDAM and VDAM entries (nor with
%     the option. The number of heights should match
%     the number of dam records / elementary dams.
%
%
%   THINDAM('xyw',XDAM,YDAM,WDAM,...)
%   plots dams at specified locations with specified
%   width. An option entry should be either:
%
%   * a constant (uniform value for alle dams), or
%
%   * a matrix of same size as XDAM/YDAM/WDAM.
%
%
%   H=THINDAM(...)
%   returns the handle of the line/surface object.
%
%   [X,Y]=THINDAM(...)                           (2D case only)
%   returns the x,y-arrays normally used for plotting.
%   The dams are not plotted.
%
%   [X,Y,M,N]=THINDAM(...)                       (2D case only)
%   returns nx2 matrices containing per row a pair of
%   endpoints for an elementary dam. The dams are not plotted.
%
%   [X,Y,Z]=THINDAM(...)                         (3D case only)
%   returns the x,y,z-arrays normally used for plotting.
%   The dams are not plotted.
%
%   [X,Y,BOTTOM,TOP]=THINDAM(...)                (3D case only)
%   returns nx2 matrices containing per row a pair of endpoints
%   for an elementary dam: X coordinates, Y coordinates, bottom
%   elevation and top elevation. The dams are not plotted.
%
%   [X,Y,BOTTOM,TOP,M,N]=THINDAM(...)            (3D case only)
%   returns nx2 matrices containing per row a pair of endpoints
%   for an elementary dam: X coordinates, Y coordinates, bottom
%   elevation, top elevation, M index, N index. The dams are
%   not plotted.

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

%
% ------- do some initialization
%
xcor=[];
ycor=[];
MNu=-1;
MNv=-1;
C=[];
DRYFL=[];
belev={};
telev={};
color={};
dangl={};
thickness={};
Flag3D=0;
XYW=0;
%
% ------- interpret the first part of the input arguments
%         distinguish between NEFIS file input or explicit
%         xcor,ycor,MNu,MNv
%
for i=1:nargin
   if isstruct(varargin{i}) & isempty(xcor)
      C=varargin{i};
   elseif ischar(varargin{i}) & isequal(lower(varargin{i}),'3d')
      Flag3D=1;
   elseif ischar(varargin{i}) & isequal(lower(varargin{i}),'xyw')
      XYW=1;
   elseif ~isnumeric(varargin{i}) & ~islogical(varargin{i})
      i=i-1;
      break
   elseif isequal(size(varargin{i}),[1 1]) & isempty(DRYFL) & isempty(xcor) & ~XYW % scalar number -> timestep
      DRYFL=varargin{i};
   elseif ~isempty(DRYFL) | ~isempty(C)
      i=i-1;
      break
   elseif isempty(xcor)
      xcor=varargin{i};
   elseif isempty(ycor)
      ycor=varargin{i};
   elseif isequal(MNu,-1)
      MNu=varargin{i};
   elseif isequal(MNv,-1)
      MNv=varargin{i};
      break
   end
end
%
% ------- if xcor is not specified data must come from NEFIS file.
%
if isempty(xcor)
   %
   % ------- autoselect NEFIS file if not specified
   %
   if isempty(C)
      C=vs_use('lastread');
      if isempty(C)
         %
         % ------- autoselection failed
         %
         error('No NEFIS file specified.')
      end
   end
   %
   % ------- if DRYFL was not specified, use 0 as default value
   %
   if isempty(DRYFL)
      DRYFL=0;
   end
   %
   % ------- read data from NEFIS file
   %
   switch vs_type(C)
      case 'Delft3D-com'
         Info=vs_disp(C,'BOTTIM',[]);
         belev={'D' -vs_get(C,'BOTTIM',{Info.SizeDim},'DP','quiet')'};
         if DRYFL>0
            kcu=vs_get(C,'KENMTIM',{DRYFL},'KFU','quiet')';
            kcv=vs_get(C,'KENMTIM',{DRYFL},'KFV','quiet')';
            telev={'S' vs_get(C,'CURTIM',{DRYFL},'S1','quiet')'};
         else
            kcu=vs_get(C,'KENMCNST','KCU','quiet')';
            kcv=vs_get(C,'KENMCNST','KCV','quiet')';
            telev={'D' belev{2}+5};
         end
         xcor=vs_get(C,'GRID','XCOR','quiet')';
         ycor=vs_get(C,'GRID','YCOR','quiet')';
      case 'Delft3D-trim'
         belev={'D' -vs_get(C,'map-const','DP0','quiet')'};
         if DRYFL>0
            kcu=vs_get(C,'map-series',{DRYFL},'KFU','quiet')';
            kcv=vs_get(C,'map-series',{DRYFL},'KFV','quiet')';
            telev={'S' vs_get(C,'map-series',{DRYFL},'S1','quiet')'};
         else
            kcu=vs_get(C,'map-const','KCU','quiet')';
            kcv=vs_get(C,'map-const','KCV','quiet')';
            telev={'D' belev{2}+5};
         end
         xcor=vs_get(C,'map-const','XCOR','quiet')';
         ycor=vs_get(C,'map-const','YCOR','quiet')';
      otherwise
         error('Invalid NEFIS file for this action.');
   end
   if ~Flag3D
      belev={};
      telev={};
   end
   xcor(xcor==0)=NaN;
   ycor(ycor==0)=NaN;
   MNu=~kcu;
   MNv=~kcv;
else % ~isempty(xcor)
   %
   % ------- check in case of explicit specification for all arguments
   %
   if XYW
      if isequal(MNu,-1)
         error('Not enough input arguments.');
      elseif ~isequal(MNv,-1)
         error('Too many input arguments.');
      end
      if ~isequal(size(xcor),size(ycor))
         error('Size of X and Y coordinates do not match.');
      elseif ~isequal(size(MNu),size(ycor))
         if isequal(size(MNu),[1 1])
            MNu=repmat(MNu,size(xcor));
         else
            error('Size of width array invalid.');
         end
      end
   elseif isequal(MNv,-1)
      error('Not enough input arguments.');
   end
end
%
% ------- do some further initialization
%
szxcor=size(xcor);
UVallowed=1;
if XYW
   xcor=xcor(:);
   ycor=ycor(:);
   dr=MNu(:);
   % dummy definitions ...
   MNv=[];
   ind=[];
   dam_M=[];
   dam_N=[];
   szxcor=[];
   dam_Mu=[];
   dam_Mv=[];
   Indu=[];
   Indv=[];
else
   %
   % ------- convert U dams in uniform format
   %
   Indu=[];
   dam_Mu=zeros(0,2);
   dam_Nu=zeros(0,2);
   if isequal(size(MNu),szxcor)
      [M,N]=find(MNu(:,2:end));
      if size(M,1)==1
         M=M';
         N=N';
      end
      dam_Mu=[M M];
      dam_Nu=[N N+1];
      UVallowed=0;
   elseif size(MNu,2)==2
      dam_Mu=[MNu(:,1) MNu(:,1)];
      dam_Nu=[MNu(:,2)-1 MNu(:,2)];
   elseif size(MNu,2)==4
      if ~isempty(MNu),
         MNu(:,[3 4])=MNu(:,[3 4])-MNu(:,[1 2]);
         if any( ~( (abs(MNu(:,3))==abs(MNu(:,4))) | ...
               abs(MNu(:,3))==0              | ...
               abs(MNu(:,4))==0              ) ),
            error('Invalid combination of MNu coordinates.');
         end;
         x=max(abs(MNu(:,[3 4]))+1,[],2);
         n=max(x);
         M=repmat(MNu(:,1),1,n)+sign(MNu(:,3))*(0:(n-1));
         N=repmat(MNu(:,2),1,n)+sign(MNu(:,4))*(0:(n-1));
         if size(M,2)>1
            M=M';
            N=N';
         end
         I=repmat(0:(n-1),size(MNu,1),1)<repmat(x,1,n); I=I';
         Indu=repmat(1:size(MNu,1),n,1);
         Indu=Indu(I);
         M=M(I);
         N=N(I);
         dam_Mu=[M M];
         dam_Nu=[N-1 N];
      end
   elseif ~isempty(MNu)
      error('Could not interpret MNu data.');
   end
   %
   % ------- check whether all dams look good
   %
   if any(dam_Nu(:)==0)
      error('U dams not allowed for N=1.')
   end
   if any(dam_Mu(:)>szxcor(1)) | any(dam_Nu(:)>szxcor(2))
      error('U dam encountered outside domain.')
   end
   %
   % ------- convert v dams in uniform format
   %
   Indv=[];
   dam_Mv=zeros(0,2);
   dam_Nv=zeros(0,2);
   if isequal(size(MNv),szxcor)
      if size(MNv,1)>=2
         [M,N]=find(MNv(2:end,:));
         if size(M,1)==1
            M=M';
            N=N';
         end
         dam_Mv=[M M+1];
         dam_Nv=[N N];
         UVallowed=0;
      end
   elseif size(MNv,2)==2
      dam_Mv=[MNv(:,1)-1 MNv(:,1)];
      dam_Nv=[MNv(:,2) MNv(:,2)];
   elseif size(MNv,2)==4
      if ~isempty(MNv)
         MNv(:,[3 4])=MNv(:,[3 4])-MNv(:,[1 2]);
         if any( ~( (abs(MNv(:,3))==abs(MNv(:,4))) | ...
               abs(MNv(:,3))==0              | ...
               abs(MNv(:,4))==0              ) )
            error('Invalid combination of MNv coordinates.');
         end
         x=max(abs(MNv(:,[3 4]))+1,[],2);
         n=max(x);
         M=repmat(MNv(:,1),1,n)+sign(MNv(:,3))*(0:(n-1)); % M=M';
         N=repmat(MNv(:,2),1,n)+sign(MNv(:,4))*(0:(n-1)); % N=N';
         I=repmat(0:(n-1),size(MNv,1),1)<repmat(x,1,n); I=I';
         Indv=repmat(1:size(MNv,1),n,1);
         Indv=Indv(I);
         M=M(I);
         N=N(I);
         dam_Mv=[M-1 M];
         dam_Nv=[N N];
      end
   elseif ~isempty(MNv)
      error('Could not interpret MNv data.');
   end
   %
   % ------- check whether all dams look good
   %
   if any(dam_Mv(:)==0)
      error('V dams not allowed for M=1.')
   end
   if any(dam_Mv(:)>szxcor(1)) | any(dam_Nv(:)>szxcor(2))
      error('V dam encountered outside domain.')
   end
end
%
% ------- interpret the optional input arguments
%
Parent=[];
drawdams=1;
drawlabels=0;
values=0;
labelformat='%g';
fontsize=4;
damshape='dam';
while i<nargin
   i=i+1;
   if ~ischar(varargin{i})
      error('Invalid input argument %i.',i)
   end
   switch lower(varargin{i})
      case 'bottom'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         Flag3D=1;
         [belev,j]=analysebelev(varargin,i,szxcor,'bottom elevation data',UVallowed,MNu,MNv,dam_Mu,dam_Mv,Indu,Indv,XYW);
         i=j;
      case 'top'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         Flag3D=1;
         [telev,j]=analysebelev(varargin,i,szxcor,'top elevation data',UVallowed,MNu,MNv,dam_Mu,dam_Mv,Indu,Indv,XYW);
         i=j;
      case 'color'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         [color,j]=analysebelev(varargin,i,szxcor,'color',UVallowed,MNu,MNv,dam_Mu,dam_Mv,Indu,Indv,XYW);
         i=j;
      case 'angle',
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         [dangl,j]=analysebelev(varargin,i,szxcor,'angle',UVallowed,MNu,MNv,dam_Mu,dam_Mv,Indu,Indv,XYW);
         i=j;
      case 'thickness'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         [thickness,j]=analysebelev(varargin,i,szxcor,'thickness',UVallowed,MNu,MNv,dam_Mu,dam_Mv,Indu,Indv,XYW);
         i=j;
      case 'parent'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         Parent=varargin{i+1};
         i=i+1;
      case 'drawdams'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         switch lower(varargin{i+1})
            case 'off'
               drawdams=0;
            case 'on'
               drawdams=1;
            otherwise
               error('The option ''drawdams'' should be followed by ''on'' or ''off''.');
         end
         i=i+1;
      case 'shape'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         damshape=lower(varargin{i+1});
         damshapes={'dam','rhombus'};
         if isempty(strmatch(damshape,damshapes,'exact'))
            error(['Invalid ''shape''. Should be one of ',sprintf('''%s'' ',damshapes{:})]);
         end
         i=i+1;
      case 'drawlabels'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         switch lower(varargin{i+1})
            case 'off'
               drawlabels=0;
            case 'on'
               drawlabels=1;
            otherwise
               error('The option ''drawlabels'' should be followed by ''on'' or ''off''.');
         end
         i=i+1;
      case 'labelformat'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         labelformat=varargin{i+1};
         i=i+1;
      case 'fontsize'
         if length(varargin)==i
            error(['Error processing option: ',varargin{i}])
         end
         fontsize=varargin{i+1};
         i=i+1;
      otherwise
         error('Unknown property: %s.',varargin{i})
   end
end
if isempty(Parent) & nargout<=1
   Parent=gca;
end
%
% ------- check for consistency
%
if ~isempty(belev) & isempty(telev)
   error('Top level of dams not specified.')
elseif isempty(belev) & ~isempty(telev)
   error('Bottom level of dams not specified.')
end
%
if XYW
   if isempty(dangl)
      xcor=xcor(:,[1 1])+dr*[-.5 .5];
      ycor=ycor(:,[1 1]);
   else
      dangl=usebelev(dangl,ind,dam_M,dam_N,szxcor,'angle');
      dangl=mean(dangl,2)*pi/180;
      xcor=xcor(:,[1 1])+dr.*cos(dangl)*[-1 1]/2;
      ycor=ycor(:,[1 1])+dr.*sin(dangl)*[-1 1]/2;
   end
   xwi=[];
   ywi=[];
else
   %
   % ------- convert dam locations into linear matrix index
   %
   dam_M=cat(1,dam_Mu,dam_Mv);
   dam_N=cat(1,dam_Nu,dam_Nv);
   dam_S1_M=cat(1,[dam_Mu(:,2) dam_Mu(:,2)+1],dam_Mv(:,[2 2]));
   dam_S1_N=cat(1,dam_Nu(:,[2 2]),[dam_Nv(:,2) dam_Nv(:,2)+1]);
   if isempty(dam_M)
      ind   =[];
      ind_S1=[];
   else
      ind   =sub2ind(szxcor,dam_M,dam_N);
      ind_S1=sub2ind(szxcor,min(dam_S1_M,szxcor(1)),min(dam_S1_N,szxcor(2)));
   end
   %
   % ------- compute basic x,y-coordinates
   %
   xw=(xcor([1 1:end-1],[1 1:end-1])+xcor(:,[1 1:end-1])+xcor([1 1:end-1],:)+xcor)/4;
   yw=(ycor([1 1:end-1],[1 1:end-1])+ycor(:,[1 1:end-1])+ycor([1 1:end-1],:)+ycor)/4;
   xcor=xcor(ind);
   ycor=ycor(ind);
   xwi=reshape(xw(ind_S1),size(ind_S1));
   ywi=reshape(yw(ind_S1),size(ind_S1));
   if ~isempty(xwi)
      I=isnan(xwi(:,1));
      xwi(I,1)=mean(xcor(I,:),2);
      ywi(I,1)=mean(ycor(I,:),2);
      I=isnan(xwi(:,2));
      xwi(I,2)=mean(xcor(I,:),2);
      ywi(I,2)=mean(ycor(I,:),2);
   end
   %
   % ------- apply rotation when appropriate
   %
   if ~isempty(dangl)
      xcor0=mean(xcor,2);
      dx=diff(xcor,[],2)/2;
      ycor0=mean(ycor,2);
      dy=diff(ycor,[],2)/2;
      dangl=usebelev(dangl,ind,dam_M,dam_N,szxcor,'angle');
      dangl=mean(dangl,2)*pi/180;
      dr=sqrt(dx.^2+dy.^2);
      xcor=xcor0(:,[1 1])+dr.*cos(dangl)*[-1 1];
      ycor=ycor0(:,[1 1])+dr.*sin(dangl)*[-1 1];
   end
end
%
% ------- first possible return if not 3D
%
if ~Flag3D
   if nargout==4
      varargout={xcor ycor dam_M dam_N};
      return
   end
end
%
% ------- switch to quasi-3D in case of coloring or thickness
%         is used. Extract the appropriate data.
%
if ~isempty(color) | ~isempty(thickness)
   if isempty(belev)
      %
      % ------- take special care of this 2D return statement for
      %         compatibility with 2D without color or thickness.
      %
      if nargout==2
         xcor=[xcor repmat(NaN,size(dam_M,1),1)]';
         ycor=[ycor repmat(NaN,size(dam_N,1),1)]';
         varargout={xcor(:) ycor(:)};
         return
      end
      belev={'C' 0};
      telev={'C' 0};
      Flag3D=1;
   end
   if ~isempty(color)
      color=usebelev(color,ind,dam_M,dam_N,szxcor,'color');
   end
   if ~isempty(thickness)
      halfthick=usebelev(thickness,ind,dam_M,dam_N,szxcor,'thickness')/2;
   end
end
%
% ------- distinguish between 3D (surface) and 2D (line) approach
%
if Flag3D
   %
   % ------- extract the bottom and top elevation data
   %
   bottom=usebelev(belev,ind,dam_M,dam_N,szxcor,'bottom elevation');
   top=usebelev(telev,ind,dam_M,dam_N,szxcor,'top elevation');
   if XYW
      top=repmat(top,[1 2]);
      bottom=repmat(bottom,[1 2]);
   end
   %
   % ------- deal with data return possibilities
   %
   if nargout==4
      varargout={xcor,ycor,bottom,top};
      return
   elseif nargout==6
      varargout={xcor,ycor,bottom,top,dam_M,dam_N};
      return
   end
   %
   % ------- flip matrices for following matrix manipulations
   %
   xcor=xcor';
   ycor=ycor';
   xwi=xwi';
   ywi=ywi';
   bottom=bottom';
   top=top';
   %
   % ------- prepare some variables
   %
   L=size(xcor,2);
   doubleNaN=repmat(NaN,[2 L]);
   %
   % ------- distinguish between thindams and thickdams
   %
   if isempty(thickness)
      %
      % ------- simple thindam case: each dam is one face of a surface
      %
      if isequal(damshape,'rhombus')
         x=reshape([xcor(1,:);xwi;xcor(2,:);doubleNaN],[2 3*L]);
         y=reshape([ycor(1,:);ywi;ycor(2,:);doubleNaN],[2 3*L]);
         z=reshape([bottom;bottom;doubleNaN],[2 3*L]);
      else
         x=reshape([xcor;xcor;doubleNaN],[2 3*L]);
         y=reshape([ycor;ycor;doubleNaN],[2 3*L]);
         z=reshape([bottom;top;doubleNaN],[2 3*L]);
      end
      %
      % ------- return data if desired
      %
      if nargout==3
         varargout={x,y,z};
         return
      end
      %
      % ------- plot thindams
      %
      h=surface(x,y,z,'edgecolor',[.5 .5 .5],'facecolor',[.5 .5 .5],'parent',Parent);
      set(h,'edgelighting',get(h,'facelighting'));
      %
      % ------- plot text if appropriate
      %
      xx=mean(xcor); blank=isnan(xx); xx(blank)=[];
      if ~isempty(color) & drawlabels & length(xx)
         yy=mean(ycor); yy(blank)=[];
         cc=mean(color'); cc(blank)=[];
         h(1+length(xx))=0; % preallocate memory
         for i=1:length(xx)
            h(1+i)=text(xx(i),yy(i),sprintf(labelformat,cc(i)),'parent',Parent,'fontunits','points','fontsize',fontsize,'horizontalalignment','center','verticalalignment','middle','clipping','on');
         end
      end
      %
      % ------- apply coloring if appropriate
      %
      if ~isempty(color) & drawdams
         color=color';
         color=reshape([color;color;doubleNaN],[2 3*L]);
         set(h(1),'cdata',color,'edgecolor','interp','facecolor','interp');
      end
      %
      % ------- all done except returning
      %
      if ~drawdams
         delete(h(1));
         h=h(2:end);
      end
      %
      % ------- all done except returning
      %
   else % thickness not empty, halfthick defined
      %
      % ------- thick dam case: each dam consists of six faces of a surface
      %
      halfthick=halfthick';
      dx=diff(xcor);
      dy=diff(ycor);
      daml=sqrt(dx.^2+dy.^2);
      xperp=dy./daml;
      yperp=-dx./daml;
      xup=xcor+halfthick.*xperp([1 1],:);
      yup=ycor+halfthick.*yperp([1 1],:);
      xdw=xcor-halfthick.*xperp([1 1],:);
      ydw=ycor-halfthick.*yperp([1 1],:);
      x=reshape([xup;xup;xdw;xdw;xup;doubleNaN
         xup(1,:);xdw(1,:);xup(1,:);xdw(1,:);doubleNaN
         xup(2,:);xdw(2,:);xup(2,:);xdw(2,:);doubleNaN],[2 12*L]);
      y=reshape([yup;yup;ydw;ydw;yup;doubleNaN
         yup(1,:);ydw(1,:);yup(1,:);ydw(1,:);doubleNaN
         yup(2,:);ydw(2,:);yup(2,:);ydw(2,:);doubleNaN],[2 12*L]);
      z=reshape([bottom;top;top;bottom;bottom;doubleNaN
         bottom([1 1],:);top([1 1],:);doubleNaN
         bottom([2 2],:);top([2 2],:);doubleNaN],[2 12*L]);
      %
      % ------- return data if desired
      %
      if nargout==3
         varargout={x,y,z};
         return
      end
      %
      % ------- plot thindams
      %
      h=surface(x,y,z,'edgecolor',[.5 .5 .5],'facecolor',[.5 .5 .5],'parent',Parent);
      set(h,'edgelighting',get(h,'facelighting'));
      %
      % ------- apply coloring if appropriate
      %
      if ~isempty(color)
         color=color';
         color=reshape([color;color;color;color;color;doubleNaN
            color([1 1 1 1],:);doubleNaN
            color([2 2 2 2],:);doubleNaN],[2 12*L]);
         set(h,'cdata',color,'edgecolor','interp','facecolor','interp');
      end
   end
   %
   % ------- all done except returning
   %
else % 2D case
   %
   % ------- combine dams into one interrupted line
   %
   xcor=[xcor repmat(NaN,size(xcor,1),1)]';
   ycor=[ycor repmat(NaN,size(ycor,1),1)]';
   %
   % ------- return data if desired
   %
   if nargout==2
      varargout={xcor(:) ycor(:)};
      return
   end
   %
   % ------- plot thindams
   %
   if isempty(xcor)
      h=line(NaN,NaN,'parent',Parent);
   else
      h=line(xcor(:),ycor(:),'parent',Parent);
   end
   %
   % ------- for the case of colored lines, the 3D approach with surfaces
   %         is used. So, no color option here.
   %
   % ------- all done except returning
   %
end
%
% ------- return graphics handle if desired
%
if nargout>0
   varargout={h};
end
%
% ------- all done
%
% -------------------------------------------------------------------------------------------------
%
function [belev,j]=analysebelev(argin,i,szxcor,bottom,UVallowed,MNu,MNv,dam_Mu,dam_Mv,Indu,Indv,XYW)
% ------- analyse input for "bottom", "top", "color", "thickness", "angle"
%         all expressions based on analysis of bottom input data
%
% ------- find next string (probably next option, if not deal with that later on)
%
j=i;
while j<length(argin)
   if ~ischar(argin{j+1})
      j=j+1;
   else
      break
   end
end
%
% ------- initialize output using the arguments found
%
belev={'?' argin{(i+1):j}};
switch j-i
   case 1
      if isequal(size(belev{2}),[1 1])
         %
         %         * a constant, uniform value for alle dams
         %
         belev{1}='C';
      elseif isequal(size(belev{2}),szxcor)
         %
         % ------- distinguish between data in depth and waterlevel points
         %
         if (length(argin)>j) & length(argin{j+1})==1 & any(argin{j+1}=='SsHh')
            %
            %         * a matrix of same size as XCOR/YCOR containing
            %           elevations in the waterlevel points. To
            %           distinguish this entry from the former you
            %           need to add the string 'H' or 'S' as an extra
            %           argument after the matrix:
            %              THINDAM(... ,ELEVMATRIX,'H', ...)
            %           The elevation will be used uniformly along
            %           each elementary dam.
            %
            belev{1}='S';
            j=j+1;
         else
            %
            %         * a matrix of same size as XCOR/YCOR containing
            %           elevations in the depth points.
            %
            belev{1}='D';
         end
      elseif XYW & isequal(size(belev{2}),size(MNu))
         %
         %         * a matrix of same size as X,Y,WIDTH in case
         %           of XYW specification.
         %
         belev{2}=belev{2}(:);
         belev{1}='XYW';
      else
         error('Could not interpret %s of dams.',bottom)
      end
   case 2
      %
      %         * two D x s arrays specifying the height of the
      %           individual dams. If the array is D x 1 the
      %           elevation is taken uniformly, is the array is
      %           D x 2 the first elevation is taken for the
      %           dam end with lowest (M,N) the second elevation
      %           for the dam end with highest (M,N). The first
      %           array specifies the elevation for dams in U
      %           direction the second array for dams in the V
      %           direction:
      %              THINDAM(... ,ELEVU,ELEVV, ...)
      %           This option cannot be used in combination with
      %           option 1 for the UDAM and VDAM entries (nor with
      %           the option. The number of heights should match
      %           the number of dam records / elementary dams.
      %
      belev{1}='UV';
      %
      % ------- check for valid dimensions: number of columns should
      %         be 1 or 2 (if not empty).
      %
      if isequal(size(belev{2}),szxcor) & isequal(size(belev{3}),szxcor)
         %
         %         * two matrices of same size as XCOR/YCOR containing
         %           elevations in the U resp. V points.
         %
         belev{1}='UVmap';
         return
      elseif (~any(size(belev{2},2)==[1 2]) & ~isempty(belev{2})) | ...
            (~any(size(belev{3},2)==[1 2]) & ~isempty(belev{3}))
         error('Could not interpret %s of dams.',bottom)
      elseif ~UVallowed
         %
         % ------- cannot combine the specification for separate dams
         %         with a "random" number of dams taken from a field.
         %
         error('Method used for specifying %s of dams not allowed.',bottom)
      end
      %
      % ------- deal with empty arrays appropriately
      %
      if isempty(belev{2})
         belev{2}=zeros(0,1);
      end
      if isempty(belev{3})
         belev{3}=zeros(0,1);
      end
      %
      % ------- check datavector length matches number of U dams
      %         for type 3) dam specification: [M1 N1 M2 N2] check
      %         both expanded and unexpanded dam lists. If data vector
      %         length matches the unexpanded dam list, expand the
      %         data vector.
      %
      if size(dam_Mu,1)~=size(belev{2},1)
         if size(MNu,1)~=size(belev{2},1)
            if size(belev{2},1)==1
               belev{2}=belev{2}(ones(size(Indu)),:);
            else
               error('Length of array containing %s of U dams\ndoes not match number of U dams.',bottom)
            end
         else
            belev{2}=belev{2}(Indu,:);
         end
      end
      %
      % ------- do the same for the V dams
      %
      if size(dam_Mv,1)~=size(belev{3},1)
         if size(MNv,1)~=size(belev{3},1)
            if size(belev{3},1)==1
               belev{3}=belev{3}(ones(size(Indv)),:);
            else
               error('Length of array containing %s of V dams\ndoes not match number of V dams.',bottom)
            end
         else
            belev{3}=belev{3}(Indv,:);
         end
      end
   otherwise
      %
      % ------- too many or too few arguments specified.
      %
      error('Could not interpret %s of dams.',bottom)
end
%
% -------------------------------------------------------------------------------------------------
%
function bottom=usebelev(belev,ind,M,N,szxcor,botstr)
% ------- use input for "bottom", "top", "color", "thickness", "angle"
%         all expressions based on analysis of bottom input data
%
switch belev{1}
   case 'XYW'
      %
      %         * a matrix of same size as X,Y,WIDTH in case
      %           of XYW specification.
      %
      bottom=belev{2};
   case 'C'
      %
      %         * a constant, uniform value for alle dams
      %
      bottom=repmat(belev{2},size(M));
      %
   case 'D'
      %
      %         * a matrix of same size as XCOR/YCOR containing
      %           elevations in the depth points.
      %
      bottom=belev{2}(ind);
      %
   case 'S'
      %
      %         * a matrix of same size as XCOR/YCOR containing
      %           elevations in the waterlevel points. To
      %           distinguish this entry from the former you
      %           need to add the string 'H' or 'S' as an extra
      %           argument after the matrix:
      %              THINDAM(... ,ELEVMATRIX,'H', ...)
      %           The elevation will be used uniformly along
      %           each elementary dam.
      %
      m=max(M,[],2);
      n=max(N,[],2);
      inds=sub2ind(szxcor,m(:,[1 1]),n(:,[1 1]));
      %
      % ------- have to distinguish between U and V dams
      %         to check upstream and downstream. So, determine
      %         dam direction from variations in M along dam.
      %
      udir=abs(diff(N,[],2));
      m=m-udir;
      n=n+1-(~udir);
      inds2=sub2ind(szxcor,m(:,[1 1]),n(:,[1 1]));
      %
      % ------- Determine upstream and downstream value and
      %         take the mean of the two (or if eitherone
      %         is NaN take the other).
      %
      bottom=belev{2}(inds);
      bottom2=belev{2}(inds2);
      I=~isnan(bottom2);
      bottom(I)=(bottom(I)+bottom2(I))/2;
      J=isnan(bottom);
      bottom(J)=bottom2(J);
      %
   case 'UVmap'
      %
      %         * two matrices of same size as XCOR/YCOR containing
      %           elevations in the U resp. V points.
      %
      m=max(M,[],2);
      n=max(N,[],2);
      inds=sub2ind(szxcor,m(:,[1 1]),n(:,[1 1]));
      %
      % ------- have to distinguish between U and V dams.
      %
      udir=logical(abs(diff(N,[],2)));
      %
      % ------- Determine upstream and downstream value and
      %         take the mean of the two (or if eitherone
      %         is NaN take the other).
      %
      bottom(udir,2)=belev{2}(inds(udir));
      bottom(~udir,2)=belev{3}(inds(~udir));
      bottom(:,1)=bottom(:,2);
      %
   case 'UV'
      %
      %         * two D x s arrays specifying the height of the
      %           individual dams. If the array is D x 1 the
      %           elevation is taken uniformly, is the array is
      %           D x 2 the first elevation is taken for the
      %           dam end with lowest (M,N) the second elevation
      %           for the dam end with highest (M,N). The first
      %           array specifies the elevation for dams in U
      %           direction the second array for dams in the V
      %           direction:
      %              THINDAM(... ,ELEVU,ELEVV, ...)
      %           This option cannot be used in combination with
      %           option 1 for the UDAM and VDAM entries (nor with
      %           the option. The number of heights should match
      %           the number of dam records / elementary dams.
      %
      %
      % ------- U dams
      %
      if size(belev{2},2)==1
         bottom=belev{2}(:,[1 1]);
      else
         bottom=belev{2};
      end
      %
      % ------- V dams
      %
      if size(belev{3},2)==1
         bottom=[bottom;belev{3}(:,[1 1])];
      else
         bottom=[bottom;belev{3}];
      end
      %
   otherwise
      error('Unknown method for determining %s.',botstr)
end
