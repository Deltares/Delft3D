function [lon,lat] = qp_proj_rotatepole(lon,lat,lonsp,latsp,deg)
%QP_PROJ_ROTATEPOLE Support routine to convert rotated pole coordinates to regular longitude and latitude.

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

if deg
   d2r = pi/180;
else
   d2r = 1;
end
x = cos(lat*d2r).*cos(lon*d2r);
y = cos(lat*d2r).*sin(lon*d2r);
z = sin(lat*d2r);
%
rlat = latsp*d2r + pi/2;
x1 = cos(rlat)*x - sin(rlat)*z;
y1 = y;
z1 = sin(rlat)*x + cos(rlat)*z;
%
rlon = lonsp*d2r;
x = cos(rlon)*x1 - sin(rlon)*y1;
y = sin(rlon)*x1 + cos(rlon)*y1;
z = z1;
%
lon = atan2(y,x)/d2r;
lat = atan2(z,sqrt(x.^2+y.^2))/d2r;
