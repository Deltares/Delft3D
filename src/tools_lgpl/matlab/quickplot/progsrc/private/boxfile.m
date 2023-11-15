function varargout=boxfile(cmd,varargin)
%BOXFILE Read/write SIMONA box files.
%   BOXFILE can be used to read and write Waqua/Triwaq
%   field files used for depth and roughness data.
%
%   DEPTH=BOXFILE('read',FILENAME)
%   read the data from the boxfile. This call uses
%   creates a matrix that tightly fits the data.
%   Use ...,SIZE) or ...,GRID) where GRID was generated
%   by WLGRID to get a depth array corresponding to the
%   indicated grid (or larger when the grid indices in
%   the datafile indicate that).
%
%   BOXFILE('write',FILENAME,MATRIX)
%   write the MATRIX to the file in boxfile format.
%   Missing values (NaN's) are replaced by 999.999.

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

if nargin==0
    if nargout>0
        varargout=cell(1,nargout);
    end
    return
end

switch cmd
    case 'read'
        varargout={Local_depread(varargin{:})};
    case 'write'
        Out=Local_depwrite(varargin{:});
        if nargout>0
            varargout{1}=Out;
        end
    otherwise
        error('Unknown command: %s',var2str(cmd))
end


function DP=Local_depread(filename,dimvar)
%    DEPTH=BOXFILE('read',FILENAME)
%    read the data from the boxfile. This call uses
%    creates a matrix that tightly fits the data.
%    Use ...,SIZE) or ...,GRID) where GRID was generated
%    by WLGRID to get a depth array corresponding to the
%    indicated grid (or larger when the grid indices in
%    the datafile indicate that).

DP=[];

dim=[];
if nargin==2
    if isstruct(dimvar) % new grid format G.X, G.Y, G.Enclosure
        dim=size(dimvar.X)+1;
    elseif iscell(dimvar) % old grid format {X Y Enclosure}
        dim=size(dimvar{1})+1;
    else
        dim=dimvar;
    end
end

if strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.*','Select depth file');
    if ~ischar(fname)
        return
    end
    filename=[fpath,fname];
end

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Cannot open %s.',filename)
end
% BOX: MNMN  =  (   2,    2 ;   38,    6) Variable_var =
% BOX MNMN=(   1,   1; 201,  10), LAYER=   1 VARIAble_val=
try
    S = '#';
    while ~isempty(S) && S(1) == '#'
        S = fgetl(fid);
    end
    
    separators = sprintf('\\/=(),:;\t');
    i=0;
    while ~feof(fid)
        i=i+1;
        %
        S = upper(S);
        S(ismember(S,separators))=' ';
        [MNMN,nread,errortext,inext]=sscanf(S,' BOX MNMN %d %d %d %d',[1 4]);
        if nread ~= 4
            error('BOX keyword or MNMN indices not found.');
        end
        %
        S = S(inext:end);
        [LAYER,nread,errortext,inext]=sscanf(S,' LAYER %d',1);
        if nread == 0
            LAYER = 0;
        else
            S = S(inext:end);
        end
        %
        if isempty(S)
            S = fgetl(fid);
            S = upper(S);
            S(ismember(S,separators))=' ';
        end
        [VALUES,S] = strtok(S);
        if length(VALUES)<4
            error('Invalid VALUE type encountered in BOX file')
        end
        %
        data{i,1} = MNMN;
        data{i,2} = VALUES(1:4);
        switch VALUES(1:4)
            case 'CONS' %CONST_VALUES
                vals = sscanf(S,'%f',1);
                if isempty(vals)
                    vals = fscanf(fid,'%f',1);
                end
                data{i,3} = vals;
            case 'CORN' %CORNER_VALUES
                vals = sscanf(S,'%f',[1 4]);
                if isempty(vals)
                    vals =[];
                end
                while length(vals)<4
                    xvals = fscanf(fid,'%f%*[ ,]',[1 4-length(vals)]);
                    vals = [vals xvals];
                end
                data{i,3} = vals;
            case 'VARI' %VARIABLE_VALUES
                nval2read = (MNMN(4)-MNMN(2)+1)*(MNMN(3)-MNMN(1)+1);
                vals = sscanf(S,'%f',[1 nval2read]);
                if isempty(vals)
                    vals =[];
                end
                nvalread = length(vals);
                if nvalread < nval2read
                    vals(1,nval2read) = 0;
                end
                while nvalread < nval2read
                    [xvals,nval] = fscanf(fid,' %f%*[ ,]',[1 nval2read-nvalread]);
                    if nval==0
                        str = fgetl(fid);
                        error('Error while reading "%s".\nExpected to read %i values for BOX MNMN=(%i, %i, %i, %i); only %i values read.\n',str,nval2read,data{i,2},nvalread);
                    end
                    vals(nvalread+1:nvalread+nval) = xvals;
                    nvalread = nvalread + nval;
                end
                data{i,3} = reshape(vals,[MNMN(4)-MNMN(2)+1 MNMN(3)-MNMN(1)+1])';
            otherwise
                error('Unkown VALUE type ''%s'' encountered in BOX file',VALUES)
        end
        %
        S = fgetl(fid);
        if isempty(deblank(S))
            S = '#';
        end
        while isequal(S,'#')
            S = fgetl(fid);
        end
    end
    fclose(fid);
catch
    fclose(fid);
    rethrow(lasterror)
end
if isempty(dim)
    maxM=0;
    maxN=0;
    for i=1:size(data,1)
        MNMN=data{i,1};
        maxM=max(maxM,max(MNMN([1 3])));
        maxN=max(maxN,max(MNMN([2 4])));
    end
    dim=[maxM maxN];
end
DP=repmat(NaN,dim);
for i=1:size(data,1)
    MNMN=data{i,1};
    DP(MNMN(1):MNMN(3),MNMN(2):MNMN(4))=data{i,3};
end


function OK=Local_depwrite(filename,DP)
%    BOXFILE('write',FILENAME,MATRIX)
%    write the MATRIX to the file in boxfile format.
%    Missing values (NaN's) are replaced by 999.999.

fid=fopen(filename,'w','n','US-ASCII');
if fid<0
    error('Cannot open %s.',filename)
end

% BOX: MNMN  =  (   2,    2 ;   38,    6) Variable_var =
NpL=5;
Mmax=size(DP,1);
Nmax=size(DP,2);
offset=0;
DP(isnan(DP))=999.999;
for i=1:ceil(Nmax/NpL)
    fprintf(fid,' BOX: MNMN  =  ( %4i, %4i ; %4i, %4i) Variable_var = \n',1,offset+1,Mmax,min(offset+NpL,Nmax));
    NtL=min(offset+NpL,Nmax)-offset;
    Format=[repmat(' %13.6f',1,NtL) '\n'];
    fprintf(fid,Format,DP(:,offset+(1:NtL))');
    offset=offset+NpL;
end
fclose(fid);
OK=1;
