function Out=bagmap(cmd,varargin)
%BAGMAP Read output files BAGGER-BOS-RIZA bagger option.
%   FILEINFO=BAGMAP('open',FILENAME) opens the bagger map file
%   (bagbgv.<case>, bagcbv.<case>, or bagdzi.<case>) written by Delft3D-MOR
%   and returns a structure containing information about the file.
%
%   MAP=BAGMAP('read',FILEINFO,INDEX,SUBFIELD) reads one map from the
%   bagger file. Time step indicated by index. In case of multiple fields
%   per timestep use subfield to indicate the field number.
%
%   See also: WLDEP, WLFDEP, BAGDPT

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
    error('Missing command.')
end

switch cmd
    case 'open'
        Out=Local_bmopen(varargin{:});
    case 'read'
        Out=Local_bmread(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd))
end


function Structure=Local_bmopen(filename,quiet)
hWaitBar=[];
if nargin<2
    quiet=0;
end
Structure.Check='NotOK';
Structure.FileType='baggermap';

if nargin==0 || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.*','Select bagger field file');
    if ~ischar(fname)
        return
    end
    filename=[fpath,fname];
end
Structure.FileName=filename;

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Cannot open %s.',filename);
end
try
    fseek(fid,0,1);
    fsize=ftell(fid);
    fseek(fid,0,-1);
    
    Line=fgetl(fid);
    nm=sscanf(Line,'nmax = %i mmax = %i',2);
    N=nm(1); Structure.N=N;
    M=nm(2); Structure.M=M;
    
    Line=fgetl(fid);  % var (e.g. x or bagt) n 1
    if isequal(Line(1),'x')
        Structure.X=zeros(N,M);
        Structure.Y=zeros(N,M);
        for n=1:N
            Structure.X(n,:)=fscanf(fid,'%f',[1 M]);
            fgetl(fid); % EOL
            Line=fgetl(fid); % x n %i
        end
        for n=1:N
            Structure.Y(n,:)=fscanf(fid,'%f',[1 M]);
            fgetl(fid); % EOL
            Line=fgetl(fid); % y n %i
        end
    end
    Structure.X=transpose(Structure.X);
    Structure.Y=transpose(Structure.Y);
    
    Structure.Quantity=lower(deblank(Line));
    i=1;
    fbase=ftell(fid);
    Line=fgetl(fid); % Time =      16895
    preallblock=1000;
    preall=preallblock;
    Structure.Time(preall)=0;
    Structure.Offset(1,preall)=0;
    if ~quiet
        hWaitBar = waitbar(0,'Please wait while reading ...');
        set(hWaitBar,'closerequestfcn','');
    end
    nfields=[];
    while ~feof(fid)
        if ~quiet && ~isempty(nfields)
            waitbar(min(1,i/nfields),hWaitBar);
        end
        if i>preall
            preall=preall+preallblock;
            Structure.Time(preall)=0;
            Structure.Offset(1,preall)=0;
        end
        Structure.Time(i)=sscanf(Line,'Time = %i',1);
        offset=ftell(fid);
        Line=fgetl(fid);  % bagt n= %i
        subs=1;
        while isempty(strfind(Line,'Time')) && ~feof(fid)
            Structure.Offset(subs,i)=offset;
            for n=1:N
                fscanf(fid,'%f',[1 M]);
                fgetl(fid); % EOL
                offset=ftell(fid);
                Line=fgetl(fid);  % bagt n= %i
            end
            subs=subs+1;
        end
        i=i+1;
        if i==3
            fblock=diff(Structure.Offset(1,1:2));
            nfields=(fsize-fbase)/fblock;
            if nfields==2
                break
            elseif nfields==round(nfields)
                nsub=size(Structure.Offset,1);
                Structure.Offset=repmat(Structure.Offset(:,1),1,nfields)+repmat((0:nfields-1)*fblock,nsub,1);
                if nfields>preall
                    Structure.Time(nfields)=0;
                else
                    Structure.Time(nfields+1:preall)=[];
                end
                for i=1:nfields-1
                    waitbar(i/(nfields-1),hWaitBar);
                    fseek(fid,fbase+i*fblock,-1);
                    Line=fgetl(fid); % Time =      16895
                    Structure.Time(i+1)=sscanf(Line,'Time = %i',1);
                end
                preall=-1;
                break
            end
        end
    end
    fclose(fid);
catch
    fclose(fid);
    if ishandle(hWaitBar)
        delete(hWaitBar);
    end
    rethrow(lasterror)
end
if ishandle(hWaitBar)
    delete(hWaitBar);
end
if isempty(nfields)
    nfields=i-1;
end
if preall>0
    Structure.Time(nfields+1:preall)=[];
    Structure.Offset(:,nfields+1:preall)=[];
end
Structure.Check='OK';


function Data=Local_bmread(S,Index,Subfield)
if nargin<3
    Subfield=1;
end
fid=fopen(S.FileName,'r','n','US-ASCII');
if fid<0
    error('Cannot open %s.',S.FileName)
end
Index=Index(:)';
Data=zeros(S.N,S.M,length(Index));
for j=1:length(Index)
    i=Index(j);
    fseek(fid,S.Offset(Subfield,i),-1);
    for n=1:S.N
        fgetl(fid);  % bagt n= %i
        Data(n,:,j)=fscanf(fid,'%f',[1 S.M]);
        fgetl(fid); % EOL
    end
end
fclose(fid);
Data=transpose(Data);
