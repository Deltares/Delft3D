function H = qp_drawsymbol(symbolname,ops)
%QP_DRAWSYMBOL  Draw a north arrow or incident wave arrow.

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

if nargin<2
   ops=[];
end
x0    = getval(ops,'x0',0);
y0    = getval(ops,'y0',0);
xsc   = getval(ops,'xsc',1);
ysc   = getval(ops,'ysc',1);
alpha = getval(ops,'alpha',0);
z0    = getval(ops,'z0',0);
color = getval(ops,'color','k');

lcx = []; lcy = [];
pcx = []; pcy = [];
pwx = []; pwy = [];
tcx = []; tcy = []; tct = {};
tsc = sqrt(xsc^2+ysc^2);
switch symbolname
   case 'north arrow (1)'
      s2_5 = sqrt(2)/5;
      %
      x   = [ 0  s2_5 1 ]';
      y   = [ 0 -s2_5 0 ]';
      pwx = [ x  y -x -y ];
      pwy = [ y -x -y  x ];
      %
      x   = [ 0  s2_5 1 ]';
      y   = [ 0  s2_5 0 ]';
      pcx = [ x  y -x -y ];
      pcy = [ y -x -y  x ];
      %
      tcx = [ 1.4 0   -1.4  0   ];
      tcy = [ 0   1.4  0   -1.4 ];
      tct = { 'E' 'N'  'W'  'S' };
   case 'north arrow (2)'
      x   = [ 0  1    0.5  0.5 ]';
      y   = [ 1 -0.5 -0.5 -1   ]';
      pwx = [ x;-flipud(x) ] ;
      pwy = [ y; flipud(y) ];
      %
      lcx = [ -0.2 -0.2  0.2 0.2 ];
      lcy = [ -0.4  0.2 -0.4 0.2 ];
      %tcx = 0;
      %tcy = 0;
      %tct = {'N'};
   case 'north arrow (3)'
      lcx = [  0 0   0.2 0   -0.2 ];
      lcy = [ -1 0.6 0.2 0.6  0.2 ];
      %
      tcx = 0.2;
      tcy = 1;
      tct = {'N'};
   case 'north arrow (4)'
      lcx = [  0 0   ];
      lcy = [ -1 0.2 ];
      %
      pwx = [ 0.2 0   -0.2 ]';
      pwy = [ 0.2 0.6  0.2 ]';
      %
      tcx = 0.2;
      tcy = 1;
      tct = {'N'};
   case 'incident wave (1)'
      lcx = [  0 0   0.2 0   -0.2 NaN 0.2*sin((0:.1:1.5)*2*pi/1.5)];
      lcy = [ -1 1   0.6 1    0.6 NaN -1:.1:0.5];
end
ca = cos(alpha);
sa = -sin(alpha);
H = [];
h = 0;
if ~isempty(lcx)
   h = h+1;
   X = x0+xsc*lcx*ca-ysc*lcy*sa;
   Y = y0+ysc*lcy*ca+xsc*lcx*sa;
   Z = z0+0*lcx;
   H(h) = line(X,Y,Z,'color',color);
end
if ~isempty(pcx)
   h = h+1;
   X = x0+xsc*pcx*ca-ysc*pcy*sa;
   Y = y0+ysc*pcy*ca+xsc*pcx*sa;
   Z = z0+0*pcx;
   H(h) = patch(X,Y,Z,1,'facecolor',color,'edgecolor',color);
end
if ~isempty(pwx)
   h = h+1;
   X = x0+xsc*pwx*ca-ysc*pwy*sa;
   Y = y0+ysc*pwy*ca+xsc*pwx*sa;
   Z = z0+0*pwx;
   H(h) = patch(X,Y,Z,1,'facecolor','w','edgecolor',color);
end
if ~isempty(tcx)
   nt = length(tcx);
   X = x0+xsc*tcx*ca-ysc*tcy*sa;
   Y = y0+ysc*tcy*ca+xsc*tcx*sa;
   Z = z0+0*tcx;
   for i = nt:-1:1
      H(h+i) = text(X(i),Y(i),Z(i),tct{i}, ...
         'rotation',alpha*180/pi, ...
         'color',color, ...
         'fontsize',8*tsc, ...
         'HorizontalAlignment','center', ...
         'VerticalAlignment','middle');
   end
   %h = h+nt;
end

function val = getval(ops,fld,val)
if isfield(ops,fld)
   val = getfield(ops,fld);
end
