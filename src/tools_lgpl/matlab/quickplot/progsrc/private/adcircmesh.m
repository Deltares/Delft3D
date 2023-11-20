function S = adcircmesh(cmd,FileName)
%ADCIRCMESH Read an Adcirc fort.14 mesh topology file.
%   MESH = ADCIRCMESH('open',FILENAME) reads an Adcirc fort.14 mesh
%   topology file and returns a structure containing all mesh information.
%   The returned structure contains fields
%    * NodeCoor: NNODES x 3 array with XYZ coordinates of NNODES mesh
%                nodes.
%    * Faces:    NELM x MAXNODE array with the indices of nodes for each of
%                the NELM elements. The number of nodes per element is at
%                most MAXNODE but may be smaller in which case the last
%                node indices are 0.
%
%    See also: NODELEMESH, MIKEMESH

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

function A = readelm(fid,nElm)
A = zeros(4,nElm);
for elm = 1:nElm
    Line = fgetl(fid);
    [Elm,count]=sscanf(Line,'%i');
    if count<2 || count<2+Elm(2)
        if isempty(Line)
            error('End-of-file reached while reading element node table')
        else
            error('Error reading node indices for element %i from line "%s"',elm,Line)
        end
    end
    A(1:2+Elm(2),elm) = Elm;
end

function S = local_open(FileName)
S.FileName = FileName;
S.FileType = 'Adcirc 14 mesh';
[fid,msg] = fopen(FileName,'r','n','US-ASCII');
if fid<0
    error('%s: %s',FileName,msg)
end
try
    Line = fgetl(fid);
    Excl = strfind(Line,'!');
    if ~isempty(Excl)
        Line = Line(1:Excl(1));
    end
    S.GridName = Line;
    %
    Line = fgetl(fid);
    Values = sscanf(Line,'%i',2);
    nElm = Values(1);
    nNodes = Values(2);
    if nNodes==0
       error('Invalid mesh: number of nodes = 0')
    elseif nElm==0
       error('Invalid mesh: number of elements = 0')
    end
    %
    Coords = readmat(fid,4,nNodes,'node coordinates');
    if ~isequal(Coords(1,:),1:nNodes)
        error('Node numbers in file don''t match 1:%i',nNodes)
    end
    S.NodeCoor = Coords(2:4,:)';
    %
    Elm = readelm(fid,nElm);
    if ~isequal(Elm(1,:),1:nElm)
        error('Element numbers in file don''t match 1:%i',nElm)
    end
    S.Faces = Elm(3:end,:)';
    %
    Line = fgetl(fid);
    if ischar(Line)
        nOpenBndSeg = sscanf(Line,'%i',1);
        fgetl(fid); % line contains total number of open boundary nodes
        for seg = 1:nOpenBndSeg
            Line = fgetl(fid);
            nBndSegNod = sscanf(Line,'%i',1);
            S.Bnd(seg).Type = 'open';
            S.Bnd(seg).Nodes = readmat(fid,1,nBndSegNod,sprintf('open boundary segment %i',seg));
        end
        %
        Line = fgetl(fid);
        if ischar(Line)
            nLandBndSeg = sscanf(Line,'%i',1);
            fgetl(fid); % line contains total number of land boundary nodes
            for seg = 1:nLandBndSeg
                Line = fgetl(fid);
                N = sscanf(Line,'%i',2);
                if length(N)==1
                    N(2) = -999;
                end
                S.Bnd(nOpenBndSeg+seg).Type = N(2);
                switch N(2)
                    case 0 % EXTERNAL NO NORMAL FLOW - ESSENTIAL, FREE SLIP
                        NVal = 1;
                    case 3 % EXTERNAL BARRIER - ESSENTIAL, FREE SLIP
                        % NODE NO.,BARLANHT, BARLANCFSP
                        NVal = 3;
                    case 24 % INTERNAL BARRIER - NATURAL, FREE SLIP
                        % NODE NO.,IBCONN,BARINHT,BARINCFSB,BARINCFSP
                        NVal = 5;
                    otherwise
                        % don't know, try to be smart ...
                        here = ftell(fid);
                        Line = fgetl(fid);
                        [dummy,NVal] = sscanf(Line,'%f');
                        fseek(fid,here,-1);
                end
                Data = readmat(fid,NVal,N(1),sprintf('land boundary segment %i',seg));
                S.Bnd(nOpenBndSeg+seg).Nodes = Data(1,:);
                S.Bnd(nOpenBndSeg+seg).Data = Data(2:end,:);
            end
        end
    end
    fclose(fid);
catch
    fclose(fid);
    error(lasterr)
end

