function gsqs = cellarea(x,y,Unit)
%CELLAREA Compute surface area of grid cells.
%   AREA = CELLAREA(X,Y) computes the area of the grid cells defined by the
%   X and Y coordinates. The returned array AREA will be one element
%   smaller in both dimensions than the arrays X and Y.
%
%   AREA = CELLAREA(LON,LAT,'deg') computes the area of grid cells in earth
%   spherical coordinates measured using degrees east and north. Use 'rad'
%   instead of 'deg' if the coordinates are given in radians instead of
%   degrees.

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

if nargin>2
    switch lower(Unit)
        case {'deg','degree','degrees'}
            unit = 1;
        case {'rad','radian','radians'}
            unit = 2;
        otherwise
            error('Unkown unit specified as third argument.')
    end
else
    unit = 0;
end

m = 2:size(x,1);
md = m-1;
n = 2:size(x,2);
nd = n-1;

ymin = min(min(y(md,nd),y(md,n)),min(y(m,nd),y(m,n)));

gsqs = area_of_onesegment(unit,x(m,n),x(md,n),y(m,n),y(md,n),ymin);
gsqs = gsqs + area_of_onesegment(unit,x(md,n),x(md,nd),y(md,n),y(md,nd),ymin);
gsqs = gsqs + area_of_onesegment(unit,x(md,nd),x(m,nd),y(md,nd),y(m,nd),ymin);
gsqs = gsqs + area_of_onesegment(unit,x(m,nd),x(m,n),y(m,nd),y(m,n),ymin);
gsqs = abs(gsqs);


function gsqs = area_of_onesegment(unit,x1,x2,y1,y2,ymin)
%AREA_OF_ONESEGMENT Compute surface area between segment (x1,y1)-(x2,y2) and ymin.
xdis1 = distance(unit,x1,y1,x2,y1);
xdis2 = distance(unit,x1,y2,x2,y2);
ydis1 = distance(unit,x1,y1,x1,ymin);
ydis2 = distance(unit,x2,y2,x2,ymin);
xdis = (xdis1 + xdis2)/2;
ydis = (ydis1 + ydis2)/2;
gsqs = xdis .* ydis .* sign(x1-x2);


function d = distance(unit,x1,y1,x2,y2)
%DISTANCE Compute distance between two points (x1,y1) and (x2,y2).
if unit==0
    % cartesian coordinates
    d = sqrt((x2-x1).^2 + (y2-y1).^2);
    
else
    % spherical coordinates
    SphereRadius = 6378137; % assuming earth coordinates
    
    if unit==1
        deg2rad = pi / 180;
        x1 = x1 * deg2rad;
        x2 = x2 * deg2rad;
        y1 = y1 * deg2rad;
        y2 = y2 * deg2rad;
    end
    
    xcrd1 = cos(y1)*sin(x1);
    ycrd1 = cos(y1)*cos(x1);
    zcrd1 = sin(y1);
    
    xcrd2 = cos(y2) * sin(x2);
    ycrd2 = cos(y2) * cos(x2);
    zcrd2 = sin(y2);
    
    slin = sqrt((xcrd2-xcrd1)^2 + (ycrd2-ycrd1)^2 + (zcrd2-zcrd1)^2);
    alpha = sin(slin/2);
    d  = SphereRadius * 2 * alpha;
end
