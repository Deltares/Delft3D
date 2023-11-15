function S = mikemesh(cmd,FileName)
%MIKEMESH Read a DHI Mike FM mesh topology file.
%   MESH = MIKEMESH('open',FILENAME) reads a DHI Mike FM mesh topology file
%   and returns a structure containing all mesh information. The returned
%   structure contains fields 
%    * NodeCoor: NNODES x 3 array with XYZ coordinates of NNODES mesh
%                nodes.
%    * Faces:    NELM x MAXNODE array with the indices of nodes for each of
%                the NELM elements. The number of nodes per element is at
%                most MAXNODE but may be smaller in which case the last
%                node indices are 0.
%
%    See also: ADCIRCMESH, NODELEMESH

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
S.FileType = 'MikeFM mesh';
fid = fopen(FileName,'r','n','US-ASCII');
try
    nNodes=fscanf(fid,'%i');
    if length(nNodes)>1 % e.g. 100079 1000 4875
        nNodes = nNodes(3);
    end
    S.Proj=fgetl(fid);
    Coords=fscanf(fid,'%f',[5 nNodes]);
    if ~isequal(Coords(1,:),1:nNodes)
        error('Node numbers in file don''t match 1:%i',nNodes)
    end
    S.NodeCoor = Coords(2:4,:)';
    S.NodeType = Coords(5,:)';
    %
    nElm = fscanf(fid,'%i',1);
    nNodePerElm = fscanf(fid,'%i',1);
    % the 3rd number on the line seems to be
    % 21 if nNodePerElm=3
    % 25 if nNodePerElm=4
    % a mesh with nNodePerElm=4 may contain some triangles (last column =
    % 0) amongst the quadrilaterals
    X = fscanf(fid,'%i',1);
    Elm = fscanf(fid,'%i',[nNodePerElm+1 nElm]);
    if ~isequal(Elm(1,:),1:nElm)
        error('Element numbers in file don''t match 1:%i',nElm)
    end
    S.Faces = Elm(2:end,:)';
    fclose(fid);
catch
    fclose(fid);
    error(lasterr)
end