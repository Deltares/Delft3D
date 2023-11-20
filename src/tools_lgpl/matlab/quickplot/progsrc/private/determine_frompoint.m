function DistanceState = determine_frompoint(DistanceState,point)
%DETERMINE_FROMPOINT Determine shortest path distance(s) from a point.

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

% main routine selects between two implementations
% currently only method 1 accessible
if nargin<2
    point=-1;
end
DistanceState = determine_frompoint_1(DistanceState,point);


% implementation 1
function DistanceState = determine_frompoint_1(DistanceState,point)
%
Npnt = DistanceState.Npnt;
ilast = DistanceState.ilast;
SEG = DistanceState.SEG;
dist = DistanceState.dist;
%
if isfield(DistanceState,'distfromlast')
    % second or later call to this routine for this mesh
    distfromlast = DistanceState.distfromlast;
    frompoint = DistanceState.frompoint;
    %
    nconn = DistanceState.nconn;
    offset = DistanceState.offset;
    %
    N = DistanceState.N;
    inext = DistanceState.inext;
    ds = DistanceState.ds;
else
    % first call to this routine for this mesh
    % initialize variables
    distfromlast = zeros(Npnt,1);
    distfromlast(ilast) = eps;
    frompoint = repmat(ilast,Npnt,1);
    %
    Nseg = length(dist);
    SEG = reshape(SEG(:,[1 2 2 1]),2*Nseg,2);
    [SEG,i] = sortrows(SEG);
    dist = [dist;dist];
    dist = dist(i);
    %
    nconn = full(sparse(SEG(:,1),1,1));
    offset = cumsum([0;nconn(1:end-1)]);
    %
    N = nconn(ilast);
    idx = offset(ilast)+(1:N);
    inext = SEG(idx,2);
    ds = dist(idx);
end
%
curdist = 0;
if point > 0
    curdist = distfromlast(point);
end
%
while ~isempty(ds) && curdist == 0
    %
    [dd,j] = min(ds);
    ilast = inext(j);
    distfromlast(ilast) = dd;
    %
    N2 = nconn(ilast);
    idx = offset(ilast)+(1:N2);
    inext2 = SEG(idx,2);
    ds2 = dd+dist(idx);
    %
    done = true(N2,1);
    for j2 = 1:N2
        inext2j2 = inext2(j2);
        if distfromlast(inext2j2) == 0
            % check for shorter distances.
            %[alreadyinlist,jj]=max(inext==inext2j2);
            %if alreadyinlist
            jj = inext==inext2j2;
            if any(jj)
                if ds(jj) > ds2(j2)
                    ds(jj) = ds2(j2);
                    frompoint(inext2j2) = ilast;
                end
            else
                done(j2) = 0;
            end
        end
    end
    inext2(done) = [];
    ds2(done) = [];
    %
    N2 = length(inext2);
    if N2 == 0
        inext(j) = [];
        ds(j) = [];
        N = N-1;
    else
        M = N:N+N2-1;
        M(1) = j;
        if N2 > 1
            N = M(N2);
        end
        inext(M) = inext2;
        ds(M) = ds2;
        frompoint(inext2) = ilast;
    end
    %
    if point > 0
        curdist = distfromlast(point);
    end
end
%
DistanceState.SEG = SEG;
DistanceState.dist = dist;
%
DistanceState.distfromlast = distfromlast;
DistanceState.frompoint = frompoint;
%
DistanceState.nconn = nconn;
DistanceState.offset = offset;
%
DistanceState.N = N;
DistanceState.inext = inext;
DistanceState.ds = ds;


% implementation 2
function DistanceState = determine_frompoint_2(DistanceState,point)
%
Npnt = DistanceState.Npnt;
ilast = DistanceState.ilast;
SEG = DistanceState.SEG;
dist = DistanceState.dist;
%
i1=SEG(:,1);
i2=SEG(:,2);
Nseg = length(i1);
%
if isfield(DistanceState,'')
    distfromlast = DistanceState.distfromlast;
    frompoint = DistanceState.frompoint;
else
    distfromlast = repmat(inf,Npnt,1);
    distfromlast(ilast)=0;
    frompoint = repmat(ilast,Npnt,1);
end
%
anychanged = 1;
while anychanged
    anychanged = 0;
    for j = 1:Nseg
        ii1 = i1(j);
        ii2 = i2(j);
        d1 = distfromlast(ii1);
        d2 = distfromlast(ii2);
        d12 = dist(j);
        %
        if d2>d1+d12
            distfromlast(ii2)=d1+d12;
            frompoint(ii2)=ii1;
            anychanged = 1;
        elseif d1>d2+d12
            distfromlast(ii1)=d2+d12;
            frompoint(ii1)=ii2;
            anychanged = 1;
        end
    end
end
%
DistanceState.distfromlast = distfromlast;
DistanceState.frompoint = frompoint;
