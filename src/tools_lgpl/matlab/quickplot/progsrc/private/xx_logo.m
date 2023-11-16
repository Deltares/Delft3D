function xx_logo(logoname,ax,varargin)
%XX_LOGO Plot a logo in an existing coordinate system.
%   XX_LOGO('LogoID',Axes)
%   Converts the Axes into a logo. Supported LogoIDs
%   are:
%     'wl' or 'dh' for WL | Delft Hydraulics
%     'ut'         for University of Twente
%     'deltares'   for Deltares
%
%   XX_LOGO('LogoID',Axes,Pos)
%   where Pos is a 1x4 matrix: the position in the
%   Axes where the logo should be plotted.
%   where Pos is a 1x5 matrix: the position and
%   rotation of the logo in the Axes object. The
%   rotation should be specified in radians.
%
%   ...,LineWidth,EdgeColor,FaceColor)
%   specify non default line width, edge and face colors for Twente logo.
%   ...,LineWidth,EdgeColor,FaceColor1,FaceColor2)
%   specify non default line width, edge and two face colors for Deltares
%   logos. If the second face color is not specified, it is taken equal to
%   the first. The Deltares logo defaults to its standard colors, whereas
%   the Deltares logo defaults to a transparent logo.

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

switch lower(logoname)
    case {'deltares'}
        F = 'Local_DL';
    case {'wl','dh'}
        F = 'Local_DH';
    case 'ut',
        F = 'Local_UT';
end
if nargin>2 & ~isequal(size(varargin{1}),[1 1])
    pos = varargin{1};
    args = varargin(2:end);
else
    pos = [];
    args = varargin;
end
if isempty(pos)
    pos=[0 0 1 1];
    set(ax,'xlim',[-0.5 0.5], ...
        'ylim',[-0.5 0.5], ...
        'visible','off', ...
        'dataaspectratio',[1 1 1]);
elseif pos(3)<pos(4)
    pos(2)=pos(2)+(pos(4)-pos(3))/2;
    pos(4)=pos(3);
elseif pos(4)<pos(3)
    pos(1)=pos(1)+(pos(3)-pos(4))/2;
    pos(3)=pos(4);
end
if length(pos)==4
    ang=0;
else
    ang=pos(5);
end
feval(F,ax,pos,ang,args{:})

function Local_DL(ax,pos,ang,lw,edge,face,face2)
% LOCAL_DL Draws the Deltares logo.

if nargin<4
    lw=0.5;
end
if nargin<5
    edge='none';
end
if nargin<6
    face=[159 185 191]/255;
end
if nargin<7
    if nargin==6
        face2=face;
    else
        face2=[0 136 162]/255;
    end
end

x = [0.265218 0.181690 0.135286 0.166222 0.245109 0.368854 0.458569 ...
    0.421445 0.325543 0.204892 0.144567 0.112084 0.143020 0.356479 ...
    0.523534 0.658107 0.818975 0.862285 0.837536 0.786492 0.700249 ...
    0.667388 0.724619 0.707605 0.616343 0.470943 0.315604 0.244748 0.268311];
y = [1.000000 0.935034 0.809743 0.786541 0.772619 0.784994 0.767979 ...
    0.692185 0.613298 0.494194 0.396745 0.290015 0.330232 0.525130 ...
    0.613298 0.661249 0.685998 0.693732 0.724668 0.738589 0.742783 ...
    0.749417 0.846866 0.890177 0.910285 0.899457 0.875093 0.879479 0.998453];
d = cumsum([0 sqrt(diff(x).^2+diff(y).^2)]);
s = max(d)*(0:300)/300;
xs = interp1(d,x,s,'spline');
ys = interp1(d,y,s,'spline');
[xs,ys]=adjustxy(xs-0.5,ys-0.5,pos,ang);
patch(xs,ys,1,'facecolor',face,'linewidth',lw,'edgecolor',edge,'parent',ax,'clipping','off');

x = [0.233701 0.331605 0.464278 0.574991 0.696684 0.781778 0.837592 ...
    0.847657 0.795503 0.725964 0.731454 0.753413 0.787268 0.818377 ...
    0.876022 0.887001 0.887916 0.827527 0.767138 0.686619 0.445978 ...
    0.365459 0.363629 0.447193 0.538293 0.588716 0.491727 0.407548 ...
    0.328860 0.234616];
y = [0.017385 0.000000 0.009150 0.040259 0.109798 0.189402 0.268091 ...
    0.336715 0.373315 0.389784 0.403509 0.423639 0.446514 0.465728 ...
    0.506903 0.524287 0.548992 0.581932 0.578272 0.561802 0.455663 ...
    0.357760 0.297371 0.234768 0.218740 0.169273 0.055814 0.031110 ...
    0.021045 0.017385];
d = cumsum([0 sqrt(diff(x).^2+diff(y).^2)]);
s = max(d)*(0:300)/300;
xs = interp1(d,x,s,'spline');
ys = interp1(d,y,s,'spline');
[xs,ys]=adjustxy(xs-0.5,ys-0.5,pos,ang);
patch(xs,ys,1,'facecolor',face2,'linewidth',lw,'edgecolor',edge,'parent',ax,'clipping','off');


function Local_DH(ax,pos,ang,lw,edge,face,face2)
% LOCAL_DH Draws the Delft Hydraulics logo.

if nargin<6
    face=[];
end
if nargin<7
    face2=face;
end
if nargin<5
    edge='w';
end
if nargin<4
    lw = 1;
end

N=20;
phi=55*pi/180;
rad=(sin(phi)-7*cos(phi)/6)/(10*(1-cos(phi)));

t=0:N;
phi=phi*t/N;
rx=sin(phi)*rad;
ry=(1-cos(phi))*rad;

pos(1)=pos(1)+pos(3)/2;
pos(2)=pos(2)+pos(4)/2;
vshf=1/15;

if isempty(face)
    % draw the right three (incomplete) wave parts
    x=[1/10 fliplr((2/10)-rx) (2/5)-rx (2/10)+fliplr(rx) rx(1)];
    y=[9/20 (1/3)+fliplr(ry) (1/3)+ry (17/30)-fliplr(ry) (17/30)-ry(1)];
    [xr,yr]=adjustxy(x-0.8,-y-0.1+vshf,pos,ang);
    line(xr,yr,'color',edge,'linewidth',lw,'parent',ax,'clipping','off');
    [xr,yr]=adjustxy(x-0.6,y-1+vshf,pos,ang);
    line(xr,yr,'color',edge,'linewidth',lw,'parent',ax,'clipping','off');
    [xr,yr]=adjustxy(x-0.4,-y-0.1+vshf,pos,ang);
    line(xr,yr,'color',edge,'linewidth',lw,'parent',ax,'clipping','off');

    % draw the (complete) left wave part
    x=[rx fliplr((2/10)-rx)]; x=[x (2/10)+fliplr(x) x(1)];
    y=[(17/30)-ry (1/3)+fliplr(ry)]; y=[y fliplr(y) y(1)]+vshf;
    [xr,yr]=adjustxy(x-1,y-1,pos,ang);
    line(xr,yr,'color',edge,'linewidth',lw,'parent',ax,'clipping','off');

    % compute the lower intersection point of the border with
    % the left wave part
    i0=max(find(x(1:(2*N))<(2/15)));
    d=((2/15)-x(i0))/(x(i0+1)-x(i0));
    y0=y(i0)+d*(y(i0+1)-y(i0));

    % draw the border
    x=[2/15 2/15 1 1 2/15 2/15];
    y=[17/30 13/15 13/15 0 0 y0-vshf]+vshf;
    [xr,yr]=adjustxy(x-1,y-1,pos,ang);
    line(xr,yr,'color',edge,'linewidth',lw,'parent',ax,'clipping','off');
else
    x=[1+1/3 1+1/3 3 5 7 9 10 10]/10;
    y=[0 5 4 5 4 5+2/3 5+2/3 0]/10+vshf;
    [xr,yr]=adjustxy(x-1,y-1,pos,ang);
    patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
    x=[1+1/3 1+1/3 10 10 9 7 5 3]/10;
    y=[5 8+2/3 8+2/3 5+2/3 5+2/3 4 5 4]/10+vshf;
    [xr,yr]=adjustxy(x-1,y-1,pos,ang);
    patch(xr,yr,1,'facecolor',face2,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
    x=[rx fliplr((2/10)-rx)]; x=[x (2/10)+fliplr(x)];
    y=[(17/30)-ry (1/3)+fliplr(ry)]; y=[y fliplr(y)]+vshf;
    [xr,yr]=adjustxy(-x,y-1,pos,ang);
    patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
    [xr,yr]=adjustxy(x-0.6,y-1,pos,ang);
    patch(xr,yr,1,'facecolor',face2,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
    [xr,yr]=adjustxy(-x-0.4,y-1,pos,ang);
    patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
    [xr,yr]=adjustxy(x-1,y-1,pos,ang);
    patch(xr,yr,1,'facecolor',face2,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
end

function Local_UT(ax,pos,ang,lw,edge,face)
% LOCAL_UT draws the University of Twente logo.

if nargin<6
    face=[1 1 1];
end
if nargin<5
    edge='none';
end
if nargin<4
    lw = 0.5;
end

N=40;

r1=1;
r2=0.6;
r3=0.5;
dx=0.2;
ddx=0.05;

t=0:N;

phi    = acos(dx/r1);
phi_r1 = phi+((1.5*pi-2*phi)/N)*t;
x_r1   = r1*cos(phi_r1);
y_r1   = r1*sin(phi_r1);

phi    = acos(dx/r2);
phi_r2 = phi+((1.5*pi-2*phi)/N)*t;
x_r2   = r2*cos(phi_r2);
y_r2   = r2*sin(phi_r2);

phi    = acos(dx/r3);
phi_r3 = phi+((1.5*pi-2*phi)/N)*t;
x_r3   = r3*cos(phi_r3);
y_r3   = r3*sin(phi_r3);

[xr,yr]=adjustxy([x_r1 fliplr(x_r2)]/2,[y_r1 fliplr(y_r2)]/2,pos,ang);
patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
[xr,yr]=adjustxy(-[x_r1 fliplr(x_r2)]/2,-[y_r1 fliplr(y_r2)]/2,pos,ang);
patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
[xr,yr]=adjustxy([x_r3 -dx -x_r3 dx]/2,[y_r3 -dx -y_r3 dx]/2,pos,ang);
patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
[xr,yr]=adjustxy([1 1 dx+ddx]/2,[dx+ddx 1 1]/2,pos,ang);
patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');
[xr,yr]=adjustxy(-[1 1 dx+ddx]/2,-[dx+ddx 1 1]/2,pos,ang);
patch(xr,yr,1,'facecolor',face,'edgecolor',edge,'linewidth',lw,'parent',ax,'clipping','off');


function [xr,yr]=adjustxy(x,y,pos,ang)
xr=pos(1)+pos(3)*x*cos(ang)-pos(4)*y*sin(ang);
yr=pos(2)+pos(4)*y*cos(ang)+pos(3)*x*sin(ang);
