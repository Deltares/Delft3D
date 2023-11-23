function [mn0,mn]=piecewise(mn,mnmax)
%PIECEWISE Checks and fixes a piecewise grid line.
%   A piecewise grid line follows either a grid line or a diagonal line
%   across the grid. The function PIECEWISE checks whether each line
%   segment is either horizontal, vertical or diagonal on the grid.
%
%   [MN0,MN2] = PIECEWISE(MN1)
%   The input array MN1 should consist of the grid points intended to be
%   on the line; each line should contain an M,N pair (integers). The
%   output array MN0 will contain all points on the line (expanded list)
%   fixing possible deviations from horizontal, vertical or diagonal
%   lines in the original MN1 array. The output array MN2 contains only
%   those points at which the line changes direction.
%
%   [MN0,MN2] = PIECEWISE(MN1,MNMAX)
%   The line is restricted to the area defined by 1:MNMAX(1) and
%   1:MNMAX(2).

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

if isempty(mn)
    mn0 = mn;
    return
end
mn0=mn(1,:);
mndiff=diff(mn,1,1);
num=size(mndiff,1);
i=1;
while i<=num
    dmn = mndiff(i,:);
    sdmn = sign(dmn);
    dmax = max(abs(dmn));
    if ~isequal(dmax*sdmn,dmn)
        dmin = min(abs(dmn));
        abs_dmn = dmn.*sdmn;
        if abs_dmn(2) < abs_dmn(1)/2
            dmn = [dmax-dmin 0; dmin dmin];
        elseif abs_dmn(1) < abs_dmn(2)/2
            dmn = [0 dmax-dmin; dmin dmin];
        elseif abs_dmn(2) < abs_dmn(1)
            dmn = [dmin dmin; dmax-dmin 0];
        else%if abs_dmn(1) < abs_dmn(2)
            dmn = [dmin dmin; 0 dmax-dmin];
        end
        dmn(1,:) = dmn(1,:).*sdmn;
        dmn(2,:) = dmn(2,:).*sdmn;
        %
        mndiff = [mndiff(1:i-1,:);dmn;mndiff(i+1:end,:)];
        num = num+1;
        %
        dmn = dmn(1,:);
        sdmn = sign(dmn);
        dmax = max(abs(dmn));
    end
    if dmax~=0
        mn0 = [mn0; repmat(mn0(end,:),dmax,1)+(1:dmax)'*sdmn];
    end
    i=i+1;
end
if nargin>1
    mn0(:,1) = max(1,min(mn0(:,1),mnmax(1)));
    mn0(:,2) = max(1,min(mn0(:,2),mnmax(2)));
    mndiff = diff(mn0,1,1);
    mn0(all(mndiff==0,2),:)=[];
end
if nargout>1
    mndiff = diff(mn0,1,1);
    mn = mn0([1;find(any(diff(mndiff,1,1),2))+1;size(mn0,1)],:);
    if size(mn,1)==2 && isequal(mn(1,:),mn(end,:))
        mn = mn(1,:);
    end
end
