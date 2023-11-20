function S = shyfemmesh(cmd,FileName)
%SHYFEMMESH Read a SHYFEM mesh topology file.
%   MESH = SHYFEMMESH('open',FILENAME) reads a ISMAR-CNR SHYFEM mesh
%   topology file and returns a structure containing all mesh information.
%   The returned structure contains fields
%    * NodeCoor: NNODES x 3 array with XYZ coordinates of NNODES mesh
%                nodes.
%    * Faces:    NELM x MAXNODE array with the indices of nodes for each of
%                the NELM elements. The number of nodes per element is at
%                most MAXNODE but may be smaller in which case the last
%                node indices are 0.
%
%    See also: ADCIRCMESH, NODELEMESH, MIKEMESH

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

switch cmd
    case {'open','read'}
        S = local_open(FileName);
    otherwise
        error('Unknown command argument: %s',cmd)
end

function S = local_open(FileName)
S.FileName = FileName;
S.FileType = 'SHYFEM mesh';
fid = fopen(FileName,'r','n','US-ASCII');
try
    Line = getline(fid);
    X=sscanf(Line,'%f');
    if length(X)==4 && all(X==round(X)) && X(1)>3
        % don't perform this call if the line suggests less than 3 elements
        S = shyfemgpp(S,fid,X);
    elseif length(X)>=1 && X(1)==round(X(1)) && X(1)>=0 && X(1)<=3
        S = shyfemnew(S,fid,Line);
    else
        error('Format of line 1 not expected for SHYFEM mesh file.\nLine 1: %s\n',Line)
    end
    %
    fclose(fid);
catch ME
    fclose(fid);
    rethrow(ME);
end

function S = shyfemgpp(S,fid,X)
if length(X)~=4
    error('The first line should contain 4 integers:\n%s',Line)
end
nNodes=X(1);
nElm=X(3);
nNodePerElm=3;
Elm = fscanf(fid,'%i',[nNodePerElm nElm]);
if numel(Elm)~=nNodePerElm*nElm
    error('Unable to read element table.')
end
for i = 1:nNodes
    nNeighb = fscanf(fid,'%i',1);
    N = fscanf(fid,'%i',nNeighb+1);
    if length(N)~=nNeighb+1
        error('Unable to read neighbours of node: %i',i)
    end
end
Coords=fscanf(fid,'%f',[5 nNodes]);
if ~isequal(Coords(1,:),1:nNodes)
    error('Node numbers in file don''t match 1:%i',nNodes)
end
S.SubType  = 'GPP';
S.NodeCoor = Coords(2:4,:)';
S.NodeType = Coords(5,:)';
S.Faces = Elm';

function S = shyfemnew(S,fid,Line)
Part0 = cell(100,1);
ip0   = 0;
Part1 = zeros(1000,1);
ip1   = 0;
Part2 = zeros(1000,1);
ip2   = 0;
Part3 = cell(100,1);
ip3   = 0;
while ~isempty(Line)
    Val = sscanf(Line,'%f');
    switch Val(1)
        case 0
            ip0 = ip0+1;
            if ip0>size(Part0,1)
                Part0{2*length(Part0)} = [];
            end
            [z,n,err,r] = sscanf(Line,'%i',1);
            Part0{ip0} = Line(r:end);
        case 1
            ip1 = ip1+1;
            if ip1>size(Part1,1)
                Part1(2*size(Part1,1),:) = 0;
            end
            Part1(ip1,1:length(Val)) = Val;
        case 2
            ip2 = ip2+1;
            if ip2>size(Part2,1)
                Part2(2*size(Part2,1),:) = 0;
            end
            Part2(ip2,1:length(Val)) = Val;
        case 3
            ip3 = ip3+1;
            if ip3>size(Part3,1)
                Part3{2*length(Part3)} = [];
            end
            Idx = fscanf(fid,'%i',[Val(4) 1]);
            Part3{ip3} = [Val;Idx]';
        otherwise
            fclose(fid);
            error('Unrecognized line:\n%s',Line)
    end
    Line = getline(fid);
end
S.Header = Part0(1:ip0);
S.Part1  = Part1(1:ip1,:);
S.Part2  = Part2(1:ip2,:);
S.Part3  = Part3(1:ip3);
%
S.NodeCoor        = S.Part1(:,4:5);
S.NodeNumber      = S.Part1(:,2);
[SINode,Reorder]  = sort(S.NodeNumber);
Map(SINode)       = Reorder;
S.Faces           = Map(S.Part2(:,5:7));
S.Boundary        = cell(1,length(S.Part3));
for i = 1:length(S.Part3)
    S.Boundary{i} = Map(S.Part3{i}(5:end));
end

function S = getline(fid)
S = '';
while ~feof(fid) && isempty(S)
    S = fgetl(fid);
end
