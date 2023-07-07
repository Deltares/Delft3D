function S = smsmesh(cmd,FileName)
%SMSMESH Read a Surface-water Modeling System mesh topology file.
%   MESH = SMSMESH('open',FILENAME) reads a Surface-water Modelling System
%   mesh topology file and returns a structure containing all mesh information.
%   This format is for example accepted by FVCOM. The returned structure
%   contains fields
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
S.FileName = FileName;
S.FileType = 'SMS mesh';
[fid,msg] = fopen(FileName,'r','n','US-ASCII');
if fid<0
    error('%s: %s',FileName,msg)
end
try
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
    fclose(fid);
catch err
    fclose(fid);
    rethrow(err)
end
%
[p,f,e] = fileparts(FileName);
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