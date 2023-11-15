function d0 = pathdistance(varargin)
%PATHDISTANCE Computes the distance along a path.
%   Computes the distance along the path from the first point for every
%   point on the path. The coordinates may be specified as row or column
%   vectors. In case the specified coordinates arrays are matrices the
%   columns of the matrices are treated as independent paths and the
%   distance along each column individually is determined.
%
%   Distance = PATHDISTANCE(Coord) computes the distance in a linear space.
%
%   Distance = PATHDISTANCE(XCoord,YCoord) computes the distance in a
%   two-dimensional Cartesian space.
%
%   Distance = PATHDISTANCE(XCoord,YCoord,ZCoord) computes the distance in
%   three-dimensional Cartesian space.
%
%   Distance = PATHDISTANCE(Longitude,Latitude,'Geographic') computes the
%   distances over the spherical earth.
%
%   Distance = PATHDISTANCE(Longitude,Latitude,ZCoord,'Geographic')
%   computes the distances as
%   SQRT(spherical earth distance^2 + vertical distance^2)
%
%   NaNs are skipped in the computation of the path length, that is, the
%   vector Distance will contain a NaN for the each NaN included in the
%   coordinate vector(s), but the accumulated distance will include the
%   distance between the last point before and the first point after the
%   gap.
%
%   Example:
%      PATHDISTANCE([1 2 NaN 2 3],[0 0 NaN 1.5 1.5])
%      returns [0 1 NaN 2.5 3.5]

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

igeo=0;
iopt=0;
x0=[];
y0=[];
z0=[];
for i=1:length(varargin)
    if ischar(varargin{i})
        switch lower(varargin{i})
            case {'geographic','spherical','deg'}
                igeo=1;
        end
    elseif isempty(x0)
        x0 = varargin{i};
        iopt=1;
    elseif isempty(y0)
        y0 = varargin{i};
        iopt=2;
    elseif isempty(z0)
        z0 = varargin{i};
        iopt=3;
    else
        error('Too many numerical arguments.')
    end
end

% iopt=1 : 1D
% iopt=2 : 2D
% iopt=3 : 3D

if isvector(x0) && size(x0,2) > 1
    row_vectors = true;
    x0 = x0';
    if iopt >= 2
        y0=y0';
    end
    if iopt == 3
        z0=z0';
    end
else
    row_vectors = false;
end

% process all columns ...
npnt = size(x0,1);
ncol = size(x0,2);

d0 = NaN(size(x0));
first_pnt = true(1,ncol);
any_first_pnt = true;
dp = zeros(1,ncol);

if iopt==1
    xp = NaN(1,ncol);
    for i = 1:npnt
        defined = ~isnan(x0(i,:));
        if any(defined)
            if any_first_pnt
                start_pnt = first_pnt & defined;
                if any(start_pnt)
                    % set initial values for first active point
                    xp(start_pnt) = x0(i,start_pnt);
                    first_pnt(defined) = false;
                    any_first_pnt = any(first_pnt);
                end
            end
            xnew = x0(i,defined);
            dnew = dp(defined) + abs(xnew-xp(defined));
            d0(i,defined) = dnew;
            dp(defined) = dnew;
            xp(defined) = xnew;
        end
    end
elseif iopt==2
    xp = NaN(1,ncol);
    yp = NaN(1,ncol);
    for i = 1:npnt
        defined = ~isnan(x0(i,:)) & ~isnan(y0(i,:));
        if any(defined)
            if any_first_pnt
                start_pnt = first_pnt & defined;
                if any(start_pnt)
                    % set initial values for first active point
                    xp(start_pnt) = x0(i,start_pnt);
                    yp(start_pnt) = y0(i,start_pnt);
                    first_pnt(defined) = false;
                    any_first_pnt = any(first_pnt);
                end
            end
            xnew = x0(i,defined);
            ynew = y0(i,defined);
            if igeo
                dnew = dp(defined) + geodist(xp(defined), yp(defined), xnew, ynew);
            else
                dnew = dp(defined) + sqrt((xnew-xp(defined)).^2+(ynew-yp(defined)).^2);
            end
            d0(i,defined) = dnew;
            dp(defined) = dnew;
            xp(defined) = xnew;
            yp(defined) = ynew;
        end
    end
elseif iopt==3
    xp = NaN(1,ncol);
    yp = NaN(1,ncol);
    zp = NaN(1,ncol);
    for i = 1:npnt
        defined = ~isnan(x0(i,:)) & ~isnan(y0(i,:)) & ~isnan(z0(i,:));
        if any(defined)
            if any_first_pnt
                start_pnt = first_pnt & defined;
                if any(start_pnt)
                    % set initial values for first active point
                    xp(start_pnt) = x0(i,start_pnt);
                    yp(start_pnt) = y0(i,start_pnt);
                    zp(start_pnt) = z0(i,start_pnt);
                    first_pnt(defined) = false;
                    any_first_pnt = any(first_pnt);
                end
            end
            xnew = x0(i,defined);
            ynew = y0(i,defined);
            znew = z0(i,defined);
            if igeo
                dnew = dp(defined) + sqrt(geodist(xp(defined), yp(defined), xnew, ynew).^2 + (znew-zp(defined)).^2);
            else
                dnew = dp(defined) + sqrt((xnew-xp(defined)).^2 + (ynew-yp(defined)).^2 + (znew-zp(defined)).^2);
            end
            d0(i,defined) = dnew;
            dp(defined) = dnew;
            xp(defined) = xnew;
            yp(defined) = ynew;
            zp(defined) = znew;
        end
    end
end

if row_vectors
    d0 = d0';
end