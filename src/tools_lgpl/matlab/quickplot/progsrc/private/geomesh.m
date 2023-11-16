function S = geomesh(cmd,FileName)
%GEOMESH Read a Geo mesh topology file.
%   MESH = GEOMESH('open',FILENAME) reads a GeoSystems *.gem mesh
%   topology file and returns a structure containing all mesh information.
%   The returned structure contains fields
%    * NodeCoor: NNODES x 3 array with XYZ coordinates of NNODES mesh
%                nodes.
%    * Faces:    NELM x MAXNODE array with the indices of nodes for each of
%                the NELM elements. The number of nodes per element is at
%                most MAXNODE but may be smaller in which case the last
%                node indices are 0.
%
%    See also: NODELEMESH, MIKEMESH, ADIRCMESH

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
S.FileType = 'GeoSystems mesh';
[fid,msg] = fopen(FileName,'r','n','US-ASCII');
if fid<0
    error('%s: %s',FileName,msg)
end
Line = fgetl(fid);
if ~strcmp(Line,'MESH FILE')
   fclose(fid);
    error('%s does not start with string MESH FILE',FileName)
end
%
jump_to_key(fid,'[MESHPOINTS]')
Line = fgetl(fid); %  765 : number of points
nNodes = sscanf(Line,'%i',1);
Nodes = fscanf(fid,'%f',[4 nNodes])';
S.NodeCoor = Nodes(:,2:4);
%
jump_to_key(fid,'[ELEMENTS]')
Line = fgetl(fid); % 1376 : number of elements (number, layernumber, number of nodes, nodenumbers)
nElm = sscanf(Line,'%i',1);
Elms = fscanf(fid,'%i',[6 nElm])';
S.Faces = Elms(:,4:6);
S.ElmLyr = Elms(:,2);
S.Layers = unique(S.ElmLyr);
%
jump_to_key(fid,'[BOUNDARYMESHPOINTS]')
Line = fgetl(fid); %  152 : number of boundary mesh points
nBnd = sscanf(Line,'%i',1);
S.BndNodes = fscanf(fid,'%i',[1 nBnd]);
%
fclose(fid);

function jump_to_key(fid,key)
Line='';
while ~feof(fid) && ~strcmp(Line,key)
    Line = fgetl(fid);
end
if feof(fid)
    fclose(fid);
    error('Unable to locate %s in %s',key,FileName)
end
