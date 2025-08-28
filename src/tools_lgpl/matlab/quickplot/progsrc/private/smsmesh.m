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
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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
    if strcmpi(strtok(Line),'MESH2D')
        S.FileType = 'SMS mesh2d';
        S = local_open_mesh2d(fid,S);
    elseif strcmpi(strtok(Line),'Node')
        S.FileType = 'SMS mesh';
        S = local_open_grd(fid,S);
        S = local_open_dep(S);
    else
        error('Expected "MESH2D" or "Node Number =" on the first line. Found: %s', Line)
    end
    fclose(fid);
catch err
    fclose(fid);
    rethrow(err)
end


function S = local_open_grd(fid,S)
Line = fgetl(fid);
try
    nNodes = sscanf(Line,'Node Number = %i',1);
catch
    nNodes = [];
end
if isempty(nNodes)
    error('Invalid mesh: expecting "Node Number =" on first line of file')
elseif nNodes==0
    error('Invalid mesh: number of nodes = 0')
end

Line = fgetl(fid);
try
    nElm = sscanf(Line,'Cell Number = %i',1);
catch
    nElm = [];
end
if isempty(nElm)
    error('Invalid mesh: expecting "Cell Number =" on second line of file')
elseif nElm==0
    error('Invalid mesh: number of cells = 0')
end

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
if ~strcmpi(strtok(Line),'MESH2D')
    error('Expecting MESH2D on the first line of the file; found "%s".',Line)
end
%
firstElement = true;
S.nMaterials = NaN;
nMaxElm = 1000;
nMaxNodes = 1000;
nNodeStrings = 0;
Elm = NaN(5, nMaxElm); % [1] type, [2-5] up to 4 coordinates, [6-] material (not yet allocated)
Coords = NaN(3, nMaxNodes);
while 1
    Line = fgetl(fid);
    if ~ischar(Line)
        break
    end
    [tok,rem] = strtok(Line);
    switch tok
        case 'ND'
            % node definition found ...
            node = sscanf(rem,'%i %f %f %f', [1 4]); % ... may contain more values
            if node(1) > nMaxNodes
                Coords(:,nMaxNodes+1:2*nMaxNodes) = NaN;
                nMaxNodes = 2*nMaxNodes;
            end
            Coords(:,node(1)) = node(2:end);
            continue
        case 'E2L'
            % 1 2
            elmType = 2;
            nIDs = 2;
        case 'E3L'
            % 1 2 3
            elmType = 23;
            nIDs = 3;
        case 'E3T'
            %  3
            % 1 2
            elmType = 3;
            nIDs = 3;
        case 'E6T'
            %   5
            %  6 4
            % 1 2 3
            elmType = 36;
            nIDs = 6;
        case 'E4Q'
            % 4 3
            % 1 2
            elmType = 4;
            nIDs = 4;
        case 'E8Q'
            % 7 6 5
            % 8   4
            % 1 2 3
            elmType = 48;
            nIDs = 8;
        case 'E9Q'
            % 7 6 5
            % 8 9 4
            % 1 2 3
            elmType = 49;
            nIDs = 9;
        case {'PG', 'PD', 'PO', 'GG', 'GP', 'BD', 'BV', 'MD', 'MV', 'BCE', 'BCN', 'BCS', 'TIME', 'BEDISP'}
            % considered obsolete by SMS
            continue
        case 'NUM_MATERIALS_PER_ELEM'
            % new in SMS version 11.0
            S.nMaterials = sscanf(rem, '%i', 1);
            Elm(end+1:5+S.nMaterials, :) = NaN;
            continue
        case 'MESHNAME'
            S.MeshName = sscanf(rem, '"%[^"]');
            continue
        case 'NS'
            % node strings, last node index is specified as negative number
            if nNodeStrings == 0
                nNodeStrings = 1; % start of first node string
                S.NodeString{nNodeStrings} = [];
            elseif S.NodeString{nNodeStrings}(end) < 0 % check if previous node string was finished
                % finished, start new node string
                nNodeStrings = nNodeStrings + 1;
                S.NodeString{nNodeStrings} = [];
            else
                % continue node string
            end
            nsNodes = sscanf(rem, '%i', [1,inf]);
            negNode = find(nsNodes < 0, 1);
            if isempty(negNode)
                % end of node string not included
            else
                nsNodes = nsNodes(1:negNode);
            end
            S.NodeString{nNodeStrings} = cat(2, S.NodeString{nNodeStrings}, nsNodes);
            continue
        otherwise
            % skip ... many more keywords are possible
            continue
    end

    % element definition found ...
    elm = sscanf(rem,'%f');
    if elm(1) > nMaxElm
        Elm(:, nMaxElm+1:2*nMaxElm) = NaN;
        nMaxElm = 2*nMaxElm;
    end
    switch elmType
        case {2, 23, 3, 36, 4, 48, 49}
            nVal = 1 + nIDs;
            % auto-detect number of materials
            if firstElement && isnan(S.nMaterials)
                S.nMaterials = max(0, length(elm) - nVal);
                Elm(end+1:5+S.nMaterials, :) = NaN;
            end
            nVal = nVal + S.nMaterials;
            if length(elm) < nVal
                error('Expected at least %i numbers on line "%s", found %i.', nVal, Line, length(elm))
            end
            IDs = zeros(4,1);
            % The following code ignores the additional nodes upon reading.
            % It would be better to read them in and, optionally, ignore
            % them at a later stage ...
            switch elmType
                case {2, 3, 4}
                    IDs(1:nIDs) = elm(2:nIDs+1);
                case 23
                    IDs(1:2) = elm([2 4]);
                case 36
                    IDs(1:3) = elm([2 4 6]);
                case {48, 49}
                    IDs(1:4) = elm([2 4 6 8]);
            end
            Elm(:, elm(1)) = [elmType; IDs; elm(nIDs+2:nVal)];
        otherwise
            error('Element type "%s" not yet implemented.', tok)
    end
end

nNodes = find(any(~isnan(Coords),1), 1, 'last' );
if nNodes == 0
    error('No node definitions found in MESH2D file.')
end

nElm = find(any(~isnan(Elm),1), 1, 'last' );
if nElm == 0
    error('No element definitions found in MESH2D file.')
end

S.NodeCoor = Coords(:,1:nNodes)';
S.FaceType = Elm(1,1:nElm);
S.Faces = Elm(2:5,1:nElm)';
S.FaceMaterial = Elm(6:end,1:nElm)';
