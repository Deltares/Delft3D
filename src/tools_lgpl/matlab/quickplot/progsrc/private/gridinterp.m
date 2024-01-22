function [x,y]=gridinterp(DataInCell,DimK,ReqLoc,x,y)
%GRIDINTERP Compute grid locations from corner co-ordinates.

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

if DataInCell
    % keep d-grid
elseif DimK
    switch ReqLoc
        case {'z','w'}
            x=interp2cen(x,1);
            y=interp2cen(y,1);
        case 'u'
            n=2:size(x,2);
            x(:,n,:)=(x(:,n,:)+x(:,n-1,:))/2;
            x(:,1,:)=NaN;
            y(:,n,:)=(y(:,n,:)+y(:,n-1,:))/2;
            y(:,1,:)=NaN;
        case 'v'
            n=2:size(x,3);
            x(:,:,n)=(x(:,:,n)+x(:,:,n-1))/2;
            x(:,:,1)=NaN;
            y(:,:,n)=(y(:,:,n)+y(:,:,n-1))/2;
            y(:,:,1)=NaN;
    end
else
    switch ReqLoc
        case {'z','w'}
            x=interp2cen(x);
            y=interp2cen(y);
        case 'u'
            n=2:size(x,1);
            x(n,:)=(x(n,:)+x(n-1,:))/2;
            x(1,:)=NaN;
            y(n,:)=(y(n,:)+y(n-1,:))/2;
            y(1,:)=NaN;
        case 'v'
            n=2:size(x,2);
            x(:,n)=(x(:,n)+x(:,n-1))/2;
            x(:,1)=NaN;
            y(:,n)=(y(:,n)+y(:,n-1))/2;
            y(:,1)=NaN;
    end
end
