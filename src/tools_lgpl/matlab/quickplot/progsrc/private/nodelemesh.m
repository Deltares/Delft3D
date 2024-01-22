function S = nodelemesh(cmd,FileName)
%NODELEMESH Read a node/element mesh topology files.
%   MESH = NODELEMESH('open',FILENAME) reads a pair of node/element mesh
%   topology files and returns a structure containing all mesh information.
%   Currently supports Triangle .node/.ele file pairs and EasyMesh .n/.e
%   file pairs. The returned structure contains fields
%    * NodeCoor: NNODES x 2 array with XY coordinates of NNODES mesh
%                nodes.
%    * Faces:    NELM x MAXNODE array with the indices of nodes for each of
%                the NELM elements. The number of nodes per element is at
%                most MAXNODE but may be smaller in which case the last
%                node indices are 0.
%
%    See also: ADCIRCMESH, MIKEMESH

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
S.FileType = 'nodelemesh';
[p,f,e] = fileparts(S.FileName);
switch lower(e)
    case {'.node','.ele'}
        eNode = '.node';
        eEle  = '.ele';
        SubType = 'Triangle';
    case {'.n','.e'}
        eNode = '.n';
        eEle  = '.e';
        SubType = 'EasyMesh';
    otherwise
        error('Unexpected file extension %s for node/element file(s)',e)
end
S.FileType = [SubType ' mesh'];
if strcmp(upper(e),e)
    eNode = upper(eNode);
    eEle = upper(eEle);
end
S.NodeFile = fullfile(p,[f eNode]);
S.EleFile = fullfile(p,[f eEle]);
%
% read node data
%
fid = fopen(S.NodeFile,'r','n','US-ASCII');
try
    % triangle: 5961  2  0  1
    % easymesh: <number of nodes>
    S.NodeHeader = sscanf(fgetl(fid),'%i',inf);
    nNodes = S.NodeHeader(1);
    switch SubType
        case 'Triangle'
            Coords = fscanf(fid,'%f',[4 nNodes]);
            if ~isequal(Coords(1,:),1:nNodes)
                error('Node numbers in file don''t match 1:%i',nNodes)
            end
            S.NodeCoor = Coords(2:3,:)';
            S.NodeType = Coords(4,:)';
        case 'EasyMesh'
            % <node number:> <x> <y> <marker>
            Coords = fscanf(fid,'%i: %f %f %f',[4 nNodes]);
            if ~isequal(Coords(1,:),0:nNodes-1)
                error('Node numbers in file don''t match 1:%i',nNodes)
            end
            S.NodeCoor = Coords(2:3,:)';
            S.NodeType = Coords(4,:)';
            % the last two lines are comments inserted by the program to facilitate the reading of the node file
    end
    fclose(fid);
catch
    fclose(fid);
    error(lasterr)
end
%
% read ele data
%
fid = fopen(S.EleFile,'r','n','US-ASCII');
try
    % triangle: 11489  3  0
    % easymesh: <number of elements>
    S.EleHeader = sscanf(fgetl(fid),'%i',inf);
    nElm = S.EleHeader(1);
    %
    switch SubType
        case 'Triangle'
            nNodePerElm = S.EleHeader(2);
            Elm = fscanf(fid,'%i',[nNodePerElm+1 nElm]);
            if ~isequal(Elm(1,:),1:nElm)
                error('Element numbers in file don''t match 1:%i',nElm)
            end
            S.Faces = Elm(2:end,:)';
        case 'EasyMesh'
            % <element number:> <i> <j> <k> <ei> <ej> <ek> <si> <sj> <sk> <xV> <yV> <marker>
            % where ei, ej and ek are indices of neighbouring elements
            %       si, sj and sk are indices of edges
            %       xV and yV are element circumcenter coordinates
            %       marker is material flag
            Elm = fscanf(fid,'%i: %i %i %i %i %i %i %i %i %i %f %f %f',[13 nElm]);
            if ~isequal(Elm(1,:),0:nElm-1)
                error('Element numbers in file don''t match 1:%i',nElm)
            end
            S.Faces = Elm(2:4,:)'+1;
            % the last two lines are comments inserted by the program to facilitate the reading of the element file
    end
    fclose(fid);
catch
    fclose(fid);
    error(lasterr)
end