function [xo,yo]=int_lnln(x1,y1,x2,y2,path)
%INT_LNLN Intersection of two lines.
%   [XCROSS,YCROSS]=INT_LNLN(XLINE1,YLINE1,XLINE2,YLINE2)
%   Computes the points where the line (XLINE1,YLINE1)
%   crosses the line (XLINE2,YLINE2).

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
    path = 0;
end

if path==2
    xo=x1;
    yo=y1;
    x1=x2;
    y1=y2;
    x2=xo;
    y2=yo;
end

% enforce column vectors
x2 = x2(:);
y2 = y2(:);

X2=x2(1:end-1);
Y2=y2(1:end-1);
dX2=diff(x2);
dY2=diff(y2);

N=length(x1);
xo=cell(1,N);
yo=xo;

for i=1:N-1
   dx1=x1(i)-x1(i+1);
   dy1=y1(i)-y1(i+1);

   Det=dX2.*dy1-dY2.*dx1;
   Det(Det==0)=NaN;
   m = (dy1*(x1(i)-X2)-dx1*(y1(i)-Y2)) ./Det;
   l = (-dY2.*(x1(i)-X2)+dX2.*(y1(i)-Y2))./Det;
   ln=(l>=0) & (l<=1) & (m>=0) & (m<=1);

   if path == 0
       l=l(ln);
       xo{i}=X2(ln)+m(ln).*dX2(ln);
       yo{i}=Y2(ln)+m(ln).*dY2(ln);
   else
       l=cat(1,0,l(ln));
       xo{i}=cat(1,x1(i),X2(ln)+m(ln).*dX2(ln));
       yo{i}=cat(1,y1(i),Y2(ln)+m(ln).*dY2(ln));
   end

   [l,ind]=unique(l); % includes sort !
   xo{i}=xo{i}(ind);
   yo{i}=yo{i}(ind);
end

if path ~= 0
    xo{end}=x1(end);
    yo{end}=y1(end);
end
xo=cat(1,xo{:});
yo=cat(1,yo{:});
