function S = smsmesh(cmd,FileName)
%SMSMESH Read a Surface-water Modeling System mesh topology file.
%   MESH = SMSMESH('open',FILENAME) reads:
%    * a Surface-water Modelling System (SMS) mesh topology file, or
%    * a SMS mesh2d file
%   and returns a structure containing all mesh information. The former
%   format is for example accepted by FVCOM, and the latter by TUFLOW.
%   The returned structure contains at least the following fields
%    * NodeCoor: NNODES x 3 array with XYZ coordinates of NNODES mesh
%                nodes.
%    * Faces:    NELM x MAXNODE array with the indices of nodes for each of
%                the NELM elements. The number of nodes per element is at
%                most MAXNODE but may be smaller in which case the last
%                node indices are 0.
%
%    See also: NODELEMESH, ADCIRCMESH

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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
%   $$
%   $$

switch cmd
    case {'open','read'}
        S = local_open(FileName);
    otherwise
        error('Unknown command argument: %s',cmd)
end

function A = readmat(fid,nValPerLine,nLines,VarName)
offset = 0;
while 1
    [Apart,count]=fscanf(fid,'%f',[nValPerLine nLines]);
    nLinesRead = count/nValPerLine;
    if nLinesRead<nLines
        Line = fgetl(fid);
        if ~ischar(Line)
            error('End-of-file reached while reading %s',VarName)
        elseif round(nLinesRead) ~= nLinesRead
            VarName(1) = upper(VarName(1));
            error('%s data line interrupted by comment "%s"',VarName,Line)
        end
    end
    %
    if offset==0
        A = Apart;
        if nLinesRead<nLines
            A(nValPerLine,nLines) = 0;
        end
    else
        A(:,offset+(1:nLinesRead)) = Apart;
    end
    nLines = nLines-nLinesRead;
    if nLines==0
        fgetl(fid);
        return
    end
    offset = offset+nLinesRead;
end

function S = local_open(FileName)
[fid,msg] = fopen(FileName,'r','n','US-ASCII');
if fid<0
    error('%s: %s',FileName,msg)
end
S.FileName = FileName;
try
    Line = fgetl(fid);
    fseek(fid,0,-1);
    if strcmpi(strtrim(Line),'MESH2D')
        S.FileType = 'SMS mesh2d';
        S = local_open_mesh2d(fid,S);
    else
        S.FileType = 'SMS mesh';
        S = local_open_grd(fid,S);
        S = local_open_dep(S);
    end
    fclose(fid);
catch err
    fclose(fid);
    rethrow(err)
end


function S = local_open_grd(fid,S)
Line = fgetl(fid);
nNodes = sscanf(Line,'Node Number = %i',1);
Line = fgetl(fid);
nElm = sscanf(Line,'Cell Number = %i',1);
if isempty(nNodes)
    error('Invalid mesh: expecting "Node Number =" on first line of file')
elseif nNodes==0
    error('Invalid mesh: number of nodes = 0')
elseif isempty(nElm)
    error('Invalid mesh: expecting "Cell Number =" on second line of file')
elseif nElm==0
    error('Invalid mesh: number of cells = 0')
end
%
Elm = readmat(fid,5,nElm,'cell node indices');
if ~isequal(Elm(1,:),1:nElm)
    error('Cell numbers in file don''t match 1:%i',nElm)
end
S.Faces = Elm(2:4,:)'; % last column contains element type
%
Coords = readmat(fid,4,nNodes,'node coordinates');
if ~isequal(Coords(1,:),1:nNodes)
    error('Node numbers in file don''t match 1:%i',nNodes)
end
S.NodeCoor = Coords(2:4,:)';


function S = local_open_dep(S)
[p,f,e] = fileparts(S.FileName);
if length(f)>4 && strcmpi(f(end-3:end),'_grd') && all(S.NodeCoor(:,3)==0)
    f(end-2:end) = f(end-2:end)-'grd'+'dep';
    depFil = fullfile(p,[f e]);
    fid = fopen(depFil,'r');
    if fid>0
        try
            Line = fgetl(fid);
            nNodes2 = sscanf(Line,'Node Number = %i',1);
            if isempty(nNodes2)
                error('Expecting "Node Number = " on first line of %s',depFil)
            elseif nNodes2~=nNodes
                error('Number of nodes in %s (%i) does not match number of grid nodes (%i)',depFil,nNodes2,nNodes)
            end
            D = readmat(fid,3,nNodes,'bed levels')';
            if isequal(D(:,1:2),S.NodeCoor(:,1:2))
                S.NodeCoor(:,3) = D(:,3);
            end
            fclose(fid);
        catch err
            fclose(fid);
            rethrow(err)
        end
    end
end


function S = local_open_mesh2d(fid,S)
Line = fgetl(fid);
if ~strcmpi(strtrim(Line),'MESH2D')
    error('Expecting MESH2D on the first line of the file; found "%s".',Line)
end
%
Line = fgetl(fid);
S.MeshName = sscanf(Line,'MESHNAME "%[^"]');
%
nMaxElm = 1000;
Elm = zeros(6,nMaxElm); % [1] type, [2-5] up to 4 coordinates, [6] material
Line = fgetl(fid);
[tok,rem] = strtok(Line);
nElm = 0;
while 1
    switch tok
        case 'E3T'
            elmType = 3;
        case 'E4Q'
            elmType = 4;
        case 'ND'
            break
        otherwise
            error('Unknown element type "%s" encountered.',tok)
    end
    nElm = nElm+1;
    if nElm > nMaxElm
        nMaxElm = 2*nMaxElm;    
        Elm(6,nMaxElm) = 0;
    end
    elm = sscanf(rem,'%i');
    if elm(1) ~= nElm
        error('The elements are not defined in sequence. Not yet supported!')
    end
    switch elmType
        case 3
            if length(elm) ~= 5
                error('Unexpected number of parameters on line "%s".',Line)
            end
            Elm(:,nElm) = [3; elm(2:4); 0; elm(5)];
        case 4
            if length(elm) ~= 6
                error('Unexpected number of parameters on line "%s".',Line)
            end
            Elm(:,nElm) = [4; elm(2:6)];
    end
    %
    Line = fgetl(fid);
    if ~ischar(Line)
        error('End-of-file while reading element definitions.')
    end
    [tok,rem] = strtok(Line);
end
Elm(:,nElm+1:end) = [];
%
nMaxNodes = max(max(Elm(2:5,:)));
Coords = zeros(3,nMaxNodes);
nNodes = 0;
while ismember(tok,{'ND'})
    nNodes = nNodes+1;
    if nNodes > nMaxNodes % shouldn't need this based on the highest node number in the element table
        nMaxNodes = 2*nMaxNodes;
        Coords(3,nMaxNodes) = 0;
    end
    node = sscanf(rem,'%i %f %f %f');
    if node(1) ~= nNodes
        error('The node coordinates are not specified in sequence. Not yet supported!')
    end
    Coords(:,nNodes) = node(2:end);
    %
    Line = fgetl(fid);
    if ~ischar(Line)
        break
    end
    [tok,rem] = strtok(Line);
end
S.NodeCoor = Coords(:,1:nNodes)';
S.FaceType = Elm(1,:);
S.Faces = Elm(2:5,:)';
S.FaceMaterial = Elm(6,:);
