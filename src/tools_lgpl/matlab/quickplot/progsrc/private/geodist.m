function d = geodist(dlon1, dlat1, dlon2, dlat2, radius)
%GEODIST Distance between two points on a sphere.
%   DIST = GEODIST(LON1,LAT1,LON2,LAT2,RAD) computes the distance between two
%   points (LON1,LAT1) and (LON2,LAT2) on a sphere of radius RAD.

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
    radius = 6378137; %earth
end

ddegrad = pi/180;

rlon1 = dlon1*ddegrad;
rlon2 = dlon2*ddegrad;
rlat1 = dlat1*ddegrad;
rlat2 = dlat2*ddegrad;

x1 = cos(rlat1).*sin(rlon1);
y1 = cos(rlat1).*cos(rlon1);
z1 = sin(rlat1);

x2 = cos(rlat2).*sin(rlon2);
y2 = cos(rlat2).*cos(rlon2);
z2 = sin(rlat2);

dslin = sqrt((x2-x1).^2 + (y2-y1).^2 + (z2-z1).^2);
alpha = asin(dslin/2);
d     = 2*radius*alpha;