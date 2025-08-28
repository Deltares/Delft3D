function varargout = gmsh(cmd,varargin)
%GMSH Read/write for GMSH grid files.
%
%   FileInfo=GMSH('open',FileName)
%   NewFileInfo=GMSH('write',FileName,FileInfo)

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
%   $HeadURL$
%   $Id$

% File format description
% https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format
switch lower(cmd)
    case 'open'
        varargout{1} = local_open_file(varargin{:});
    case 'write'
        local_write_file(varargin{:});
    otherwise
        error('Unknown command specified: %s',cmd)
end


function S = local_open_file(filename)
S.FileType = 'Gmsh';
S.FileName = filename;

fid = fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Cannot open "%s".',filename)
elseif feof(fid)
    fclose(fid);
    error('The file "%s" is empty.',filename)
end
try
    S = local_parse_file(fid, S);
    fclose(fid);
catch err
    fclose(fid);
    rethrow(err)
end


function local_write_file(filename, Data, varargin)
Ops.Version = 4.1;
Ops.Binary = 'n';

i = 1;
while i <= length(varargin)
    switch lower(varargin{i})
        case 'binary'
            Ops.Binary = varargin{i+1};
        case 'version'
            Ops.Version = varargin{i+1};
        otherwise
            error('Unknown option: %s.', varargin{i})
    end
    i = i+2;
end
fid = fopen(filename,'w',Ops.Binary);
try
    local_flush_file(fid, Data, Ops)
    fclose(fid);
catch err
    fclose(fid);
    rethrow(err)
end


function S = local_parse_file(fid, S)
block = 0;
while ~feof(fid)
    block = block+1;
    if block == 1
        keywords = {'$MeshFormat', ...
            '$PostFormat', ...
            '$NOD'};
    else
        keywords = {'$ElementData', ...
            '$ElementNodeData', ...
            '$Elements', ...
            '$ELM', ...
            '$Entities', ...
            '$GhostElements', ...
            '$NodeData', ...
            '$Nodes', ...
            '$Parametrizations', ...
            '$PartitionedEntities', ...
            '$Periodic', ...
            '$PhysicalMeshFormat', ...
            '$PhysicalNames', ...
            '[EOF]'};
    end
    keyword = check_line(fid,keywords);
    if ~ischar(keyword) % EOF
        break
    end
    %fprintf(1,'Found %s block ...\n',keyword);
    
    switch keyword
        case '$PostFormat'
            error('POS ASCII file format not yet supported.')
            
        case '$MeshFormat'
            x = fscanf(fid,'%f %i %i',3);
            S.Version = x(1);
            S.Binary = x(2);
            S.DataSize = x(3);
            if ~ismember(S.Version,[2.2, 4.1])
                error('Expecting GMSH file version number 2.2 or 4.1 when reading %g in %f.', S.Version, S.FileName)
            elseif ~ismember(S.Binary,[0,1])
                error('GMSH file type %i is not supported; only 0 (ASCII) and 1 (BINARY) are supported.',S.Binary)
            elseif S.DataSize==4
                S.BFloat = 'float32';
            elseif S.DataSize==8
                S.BFloat = 'float64';
            else
                error('GMSH data size %i is not supported; only 4 and 8 are supported.',S.DataSize)
            end
            S.Binary = S.Binary == 1;
            if S.Binary && S.Version > 1.4
                fgetl(fid); % read line ending
                ONE = fread(fid,1,'uint32',0,'l');
                switch ONE
                    case 1
                        S.ByteOrder = 'l';
                    case 16777216
                        S.ByteOrder = 'b';
                    otherwise
                        error('The binary endian flag should be 1.')
                end
            end
            
        case {'$ElementData', '$ElementNodeData', '$NodeData'}
            Line = fgetl(fid);
            nStringTags = sscanf(Line,'%i',1);
            strings = cell(1,nStringTags);
            for i = 1:nStringTags
                strings{i} = deblank(fgetl(fid));
                if strings{i}(1)=='"' && strings{i}(end)=='"'
                    strings{i} = strings{i}(2:end-1);
                end
            end
            
            Line = fgetl(fid);
            nRealTags = sscanf(Line,'%i',1);
            reals = fscanf(fid,'%f',nRealTags);
            fgetl(fid);
            
            Line = fgetl(fid);
            nIntegerTags = sscanf(Line,'%i',1);
            integers = fscanf(fid,'%i',nIntegerTags);
            fgetl(fid);
            
            % string 1: name of the post-processing view
            % string 2: name of the interpolation scheme
            %
            % real 1: time value associated with the dataset
            %
            % integer 1: time step index (starting at 0)
            % integer 2: number of field components of the data in the view (1, 3 or 9)
            nComponents = integers(2);
            % integer 3: number of entities (nodes or elements) in the view
            nLocations = integers(3);
            % integer 4: partition index for the view data (0 for no partition)

            if S.Binary
                fgetl(fid); % read line ending
                Nr = zeros(1,nLocations);
                Data = zeros(nComponents,nLocations);
                for i = 1:nLocations
                    Nr(i) = fread(fid,1,'uint32',0,S.ByteOrder);
                    Data(:,i) = fread(fid,nComponents,S.BFloat,0,S.ByteOrder)';
                end
            else
                data = fscanf(fid,'%f',[1+nComponents,nLocations]);
                Nr = data(1,:);
                Data = data(2:end,:);
            end
            %
            S.Field.Nr = Nr;
            S.Field.Data = Data;
            
        case {'$Elements','$ELM'}
            if S.Version >= 3
                if S.Binary
                    data = fread(fid,4,'uint64',0,S.ByteOrder);
                else
                    data = fscanf(fid,'%i',4);
                end
                %nEntityBlocks = data(1);
                nElements = data(2);
                %minElementNr = data(3);
                %maxNElementNr = data(4);
            else
                data = fscanf(fid,'%i',1);
                nEntities = 1;
                nElements = data(1);
                fgetl(fid); % read line ending
            end

            for iEntity = 1:nEntities
                Nr = zeros(1,nElements);
                elementType = zeros(1,nElements);
                Nodes = zeros(1,nElements);
                
                if S.Version >= 3
                    nElementCount = 0;
                    while nElementCount < nElements
                        if S.Binary
                            data = fread(fid,3,'uint32',0,S.ByteOrder);
                            data(4) = fread(fid,1,'uint64',0,S.ByteOrder);
                        else
                            data = fscanf(fid,'%i',4);
                        end
                        %nEntityDims = data(1);
                        %entityNr = data(2);
                        eType = data(3);
                        nElem = data(4);
                        
                        element = getElementProps(eType);
                        
                        if S.Binary
                            data = fread(fid,[1+element.nNodes,nElem],'uint64',0,S.ByteOrder);
                        else
                            data = fscanf(fid,'%f',[1+element.nNodes,nElem]);
                        end
                        
                        j = nElementCount + (1:nElem);
                        elementType(j) = eType;
                        Nr(j) = data(1,:);
                        Nodes(1:element.nNodes,j) = data(2:end,:);
                        
                        nElementCount = nElementCount + nElem;
                    end
                elseif S.Binary % version 2.2 binary
                    nElementCount = 0;
                    while nElementCount < nElements
                        data = fread(fid,3,'uint32',0,S.ByteOrder);
                        eType = data(1);
                        nElem = data(2);
                        nTags = data(3);
                        
                        element = getElementProps(eType);
                        data = fread(fid,[1+nTags+element.nNodes,nElem],'uint32',0,S.ByteOrder);
                        
                        j = nElementCount + (1:nElem);
                        elementType(j) = eType;
                        Nr(j) = data(1,:);
                        Nodes(1:element.nNodes,j) = data(nTags+2:end,:);
                        
                        nElementCount = nElementCount + nElem;
                    end
                elseif S.Version == 1 % version 1 (always ascii)
                    for i = 1:nElements
                        data = fscanf(fid,'%i',5);
                        Nr(i) = data(1);
                        elementType(i) = data(2);
                        %physEntity(i) = data(3);
                        %elemEntity(i) = data(4);
                        
                        element = getElementProps(elementType(i));
                        nNodes = element.nNodes;
                        if nNodes ~= data(5)
                            error('Element %i: number of nodes %i does not agree with element type %i.',Nr(i),data(5),elementType(i))
                        end
                        Nodes(1:nNodes,i) = fscanf(fid,'%i',nNodes);
                    end
                    
                else % version 2.2 ascii
                    Nr = zeros(1,nElements);
                    elementType = zeros(1,nElements);
                    Nodes = zeros(1,nElements);
                    for i = 1:nElements
                        data = fscanf(fid,'%i',3);
                        Nr(i) = data(1);
                        elementType(i) = data(2);
                        nTags = data(3);
                        
                        element = getElementProps(elementType(i));
                        nNodes = element.nNodes;
                        data = fscanf(fid,'%i',nTags+nNodes);
                        
                        Nodes(1:nNodes,i) = data(nTags+1:end);
                    end
                end
                
                S.Element.Nr = Nr;
                S.Element.elementType = elementType;
                S.Element.Nodes = Nodes;
            end
            
        case '$GhostElements--TODO'

        case {'$Nodes','$NOD'}
            if ~isfield(S,'Version')
                S.Version = 1.0;
                S.Binary = false;
                %S.DataSoze = ?
            end
            
            if S.Version >= 3
                if S.Binary
                    data = fread(fid,4,'uint64',0,S.ByteOrder);
                else
                    data = fscanf(fid,'%i',4);
                end
                nEntities = data(1);
                nNodes = data(2);
                %minNodeNr = data(3);
                %maxNodeNr = data(4);
            else
                data = fscanf(fid,'%i',1);
                nEntities = 1;
                nNodes = data(1);
                fgetl(fid); % read line ending
            end
            
            for iEntity = 1:nEntities
                if S.Version >= 3
                    if S.Binary
                        data = fread(fid,3,'uint32',0,S.ByteOrder);
                        data(4) = fread(fid,1,'uint64',0,S.ByteOrder);
                    else
                        data = fscanf(fid,'%i',4);
                    end
                    nEntityDims = data(1);
                    %entityNr = data(2);
                    isParametric = data(3);
                    nNodesInBlock = data(4);
                    
                    if S.Binary
                        NodeNr = fread(fid,nNodesInBlock,'uint64',0,S.ByteOrder);
                    else
                        NodeNr = fscanf(fid,'%i',nNodesInBlock);
                    end
                    
                    if isParametric
                        nCoords = 3 + nEntityDims;
                    else
                        nCoords = 3;
                    end
                    if S.Binary
                        Coords = fread(fid,[nCoords,nNodesInBlock],S.BFloat,0,S.ByteOrder);
                    else
                        Coords = fscanf(fid,'%f',[nCoords,nNodesInBlock]);
                    end

                else
                    if S.Binary
                        NodeNr = zeros(1,nNodes);
                        Coords = zeros(3,nNodes);
                        for i = 1:nNodes
                            NodeNr(i) = fread(fid,1,'uint32',0,S.ByteOrder);
                            Coords(:,i) = fread(fid,[3,1],S.BFloat,0,S.ByteOrder);
                        end
                    else
                        data = fscanf(fid,'%i %f %f %f',[4,nNodes]);
                        NodeNr = data(1,:);
                        Coords = data(2:end,:);
                    end
                end
                                                
                S.Node.Nr = NodeNr;
                S.Node.Coords = Coords;
            end

        case '$Parametrizations--TODO'
        case '$PartitionedEntities--TODO'
        case '$Periodic--TODO'
        case '$PhysicalMeshFormat--TODO'
            
        case '$PhysicalNames'
            nPhysicals = fscanf(fid,'%i \n',1);
            S.Physical.Nr     = zeros(1,nPhysicals);
            S.Physical.NumDim = zeros(1,nPhysicals);
            S.Physical.Name   = cell(nPhysicals,1);
            for i = 1:nPhysicals
                Line = fgetl(fid);
                [data,n,err,j] = sscanf(Line,'%i %i "%[^"]');
                S.Physical.Nr(i) = data(2);
                S.Physical.NumDim(i) = data(1);
                S.Physical.Name{i} = char(data(3:end))';
            end

        otherwise % enerything unrecognized such as $Comments is skipped
            % skip block
            %fprintf(1,'... %s is skipped\n',keyword);
            endkeyword = [keyword(1) 'End' keyword(2:end)];
            while ~feof(fid)
                try
                    check_line(fid,endkeyword)
                    break
                catch
                end
            end
            continue
            
    end
    
    switch keyword
        case {'$NOD','$ELM'}
            endkeyword = ['$END' keyword(2:end)];
        otherwise
            endkeyword = ['$End' keyword(2:end)];
    end
    check_line(fid,endkeyword)
end

if ~isfield(S,'Node')
    error('No $Nodes block found in "%s".', S.FileName)
elseif ~isfield(S,'Element')
    error('No $Elements block found in "%s".', S.FileName)
end


function local_flush_file(fid, Data, Ops)
switch Ops.Version
    case 4.1
        local_flush_4p1_file(fid, Data, Ops)
    case 2.2
        local_flush_2p2_file(fid, Data, Ops)
end


function local_flush_2p2_file(fid, Data, Ops)
binary = ~strcmp(Ops.Binary,'n');

fprintf(fid,'$MeshFormat\n');
if binary
    fprintf(fid,'2.2 1 8\n');
    fwrite(fid,1,'uint32');
    fprintf(fid,'\n');
else
    fprintf(fid,'2.2 0 8\n');
end
fprintf(fid,'$EndMeshFormat\n');

nNodes = length(Data.X);
XYZ = [Data.X Data.Y zeros(size(Data.X))]';
fprintf(fid,'$Nodes\n');
fprintf(fid,'%i\n',nNodes);
if binary
    for i = 1:nNodes
        fwrite(fid,i,'uint32');
        fwrite(fid,XYZ(:,i),'float64');
    end
else
    iXYZ = [1:nNodes;XYZ];
    fprintf(fid,'%i %f %f %f\n',iXYZ);
end
fprintf(fid,'$EndNodes\n');

nFaces = size(Data.FaceNodeConnect,1);
nNodesPerFace = sum(~isnan(Data.FaceNodeConnect),2);
%
SupportedFaces = nNodesPerFace <= 4;
if ~all(SupportedFaces)
    warning('Gmsh file cannot store pentagons and hexagons. Skipping %i faces.',sum(~SupportedFaces))
end
%
iFace = (1:nFaces)';
iFNC = [iFace Data.FaceNodeConnect]';
fprintf(fid,'$Elements\n');
fprintf(fid,'%i\n',sum(SupportedFaces));
%
faceTypes = unique(nNodesPerFace(SupportedFaces));
nFaceTypes = length(faceTypes);
for iElmType = 1:nFaceTypes
    faceType = faceTypes(iElmType);
    if faceType == 3 % triangles
        elementType = 2;
        nNodes = 3;
    elseif faceType == 4 % quads
        elementType = 3;
        nNodes = 4;
    end
    %
    elementMask = nNodesPerFace == faceType;
    nElements = sum(elementMask);
    iFNC_reduced = iFNC(1:1+nNodes,elementMask);
    if binary
        fwrite(fid,[elementType,nElements,0],'uint32');
        fwrite(fid,iFNC_reduced,'uint32');
    else
        
        format = ['%i ' sprintf('%i',elementType) ' 0' repmat(' %i',1,nNodes) '\n'];
        fprintf(fid,format,iFNC_reduced);
    end
end
fprintf(fid,'$EndElements\n');


function local_flush_4p1_file(fid, Data, Ops)
binary = ~strcmp(Ops.Binary,'n');

fprintf(fid,'$MeshFormat\n');
if binary
    fprintf(fid,'4.1 1 8\n');
    fwrite(fid,1,'uint32');
    fprintf(fid,'\n');
else
    fprintf(fid,'4.1 0 8\n');
end
fprintf(fid,'$EndMeshFormat\n');

nNodes = length(Data.X);
XYZ = [Data.X Data.Y zeros(size(Data.X))]';
fprintf(fid,'$Nodes\n');
if binary
    fwrite(fid,[1,nNodes,1,nNodes],'uint64');
    fwrite(fid,[2,0,0],'uint32');
    fwrite(fid,nNodes,'uint64');
    fwrite(fid,1:nNodes,'uint64');
    fwrite(fid,XYZ,'float64');
else
    fprintf(fid,'1 %i 1 %i\n',nNodes,nNodes);
    fprintf(fid,'2 0 0 %i\n',nNodes);
    fprintf(fid,'%i\n',1:nNodes);
    fprintf(fid,'%f %f %f\n',XYZ);
end
fprintf(fid,'$EndNodes\n');

% create separate entities for different face sizes ...
nFaces = size(Data.FaceNodeConnect,1);
nNodesPerFace = sum(~isnan(Data.FaceNodeConnect),2);
faceTypes = unique(nNodesPerFace);
%
SupportedFaces = nNodesPerFace <= 4;
if ~all(SupportedFaces)
    warning('Gmsh file cannot store pentagons and hexagons. Skipping %i faces.',sum(~SupportedFaces))
    faceTypes = unique(nNodesPerFace(SupportedFaces));
end
nSupFaces = sum(SupportedFaces);
%
nFaceTypes = length(faceTypes);
iFace = (1:nFaces)';
iFNC = [iFace Data.FaceNodeConnect]';
fprintf(fid,'$Elements\n');
if binary
    fwrite(fid,[1,nSupFaces,1,nFaces],'uint64');
else
    fprintf(fid,'1 %i 1 %i\n',nSupFaces,nFaces);
end
for iElmType = 1:nFaceTypes
    faceType = faceTypes(iElmType);
    if faceType == 3 % triangles
        elementType = 2;
        nNodes = 3;
    elseif faceType == 4 % quads
        elementType = 3;
        nNodes = 4;
    end
    
    elementMask = nNodesPerFace == faceType;
    nElements = sum(elementMask);
    iFNC_reduced = iFNC(1:1+nNodes,elementMask);
    if binary
        fwrite(fid,[2,0,elementType],'uint32');
        fwrite(fid,nElements,'uint64');
        fwrite(fid,iFNC_reduced,'uint64');
    else
        fprintf(fid,'2 0 %i %i\n',elementType,nElements);
        fprintf(fid,['%i' repmat(' %i',1,nNodes) '\n'],iFNC_reduced);
    end
end
fprintf(fid,'$EndElements\n');


function out = check_line(fid,str)
Line = get_next_line(fid);
if ischar(str)
    if ~ischar(Line)
        error('End of file while trying to read "%s".',str)
    elseif ~isequal(Line,str)
        error('Reading "%s" when expecting "%s".',Line,str)
    end
else
    if ~ischar(Line)
        if ~ismember('[EOF]',str)
            strcat = sprintf('"%s" or ',str{:});
            error('End of file while trying to read %s.',strcat(1:end-4))
        end
    elseif ~ismember(Line,str)
        strcat = sprintf('"%s" or ',str{:});
        error('Reading "%s" when expecting %s.',Line,strcat(1:end-4))
    end
end
if nargout > 0
    out = Line;
end


function Line = get_next_line(fid)
while true
    Line = fgetl(fid);
    
    % check for end of file
    if ~ischar(Line)
        return
    end
    
    % remove comment
    comment = strfind(Line,'//');
    if ~isempty(comment)
        Line = Line(1:comment(1)-1);
    end
    
    % remove spaces
    Line = strtrim(Line);
    
    % if something remains, that's the line that we were looking for
    if ~isempty(Line)
        return
    end
    
    % otherwise try read the next line
end


function element = getElementProps(elementType)
switch elementType
    case 1
        element.Name = 'line';
        element.nNodes = 2;
    case 2
        element.Name = 'tri';
        element.nNodes = 3;
    case 3
        element.Name = 'quad';
        element.nNodes = 4;
    case 15
        element.Name = 'pnt';
        element.nNodes = 1;
    otherwise
        error('Unknown element type %i.',elementType)
end