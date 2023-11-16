function varargout = incremental(cmd,varargin)
%INCREMENTAL Read Delft-FLS, SOBEK2D and SIMONA incremental files.
%   FILEDATA = INCREMENTAL('open',FILENAME,SIZE) opens the specified
%   incremental file and returns the general information in a structure.
%   The SIZE argument is a dummy argument for Delft-FLS and SOBEK2D
%   incremental files; for SIMONA incremental files the argument is
%   required since those incremental files don't include the domain size.
%
%   [DATA,FILEDATA] = INCREMENTAL('read',FILEDATA,FIELD,TIME) determines
%   the data (classes) for the selected FIELD at TIME from the incremental
%   data file. FILEDATA should be provided as obtained from an open or read
%   call to INCREMENTAL. The routine returns the data and an updated
%   FILEDATA (contains state at retrieved time for faster reading of data
%   at later times). FIELD should be the field index 1 to 5.
%
%   INCDATA = INCREMENTAL('inc2sparse',FILEDATA) loads incremental file
%   into memory for data analysis. Use INCANALYSIS to analyse the returned
%   data structure.
%
%   See also INCANALYSIS, FLS.

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
    case 'open'
        varargout{1} = Local_open_incremental(varargin{:});
    case 'inc2sparse'
        varargout{1} = Local_inc2sparse(varargin{:});
    case 'read'
        [varargout{1:2}] = Local_read_inc(varargin{:});
    case 'readtimes'
        varargout{1} = Local_inc2sparse(varargin{1},1);
end


function FileInfo=Local_open_incremental(filename,DomainSize)
% open incremental file
FileInfo.Check='NotOK';
FileInfo.FileType='FLS-inc';

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    return
end
try
    FileInfo.FileName=filename;
    
    Line=fgetnel(fid); % /* runid or MAIN DIMENSIONS ...
    if strcmp(Line(1:2),'/*')
        Line=fgetnel(fid);
    end
    
    % FLS file:
    % MAIN DIMENSIONS                MMAX   NMAX
    %
    % SOBEK overland file:
    % INC1.0
    %
    % SIMONA file:
    % CLASSES OF INCREMENTAL FILE    H           Z           U           V           M           A
    
    switch Line(1:3)
        case 'INC'
            FileInfo.Type = 'SOBEK';
            FileInfo.Sobek=str2double(Line(4:end));
            Line=fgetnel(fid);
            % DOMAIN                        NUMBER   ID
        case 'MAI'
            FileInfo.Type = 'FLS';
            FileInfo.Sobek=0;
        case 'CLA'
            FileInfo.Type = 'SIMONA';
    end
    
    if ~isequal(FileInfo.Type,'SIMONA')
        dmn=1;
        while dmn
            if FileInfo.Sobek
                Line=fgetnel(fid);
                %                              <NUMBER> 'ID'
                [T,Rem]=strtok(Line);
                FileInfo.Domain(dmn).Number=str2double(T); % <NUMBER>
                Quote=strfind(Rem,'''');
                FileInfo.Domain(dmn).Id=deblank(Rem(Quote(1)+1:Quote(end)-1));
                fgetnel(fid);
                % MAIN DIMENSIONS                MMAX   NMAX
            end
            
            Line=fgetnel(fid); %                               <MMAX> <NMAX>
            X=sscanf(Line,'%i',2);
            FileInfo.Domain(dmn).NRows=X(1);
            FileInfo.Domain(dmn).NCols=X(2);
            
            if FileInfo.Sobek
                fgetnel(fid);
                % GRID                           DX      DY      X0      Y0
                Line=fgetnel(fid);
                %                               <DX>    <DY>    <XO>    <Y0>
                X=sscanf(Line,'%f',4);
                FileInfo.Domain(dmn).XCellSize=X(1);
                FileInfo.Domain(dmn).YCellSize=X(2);
                FileInfo.Domain(dmn).XCorner=X(3);
                FileInfo.Domain(dmn).YCorner=X(4);
                Line=fgetnel(fid);
                % domain                        END
            else
                fgetnel(fid);
                % GRID                           DX      X0      Y0
                Line=fgetnel(fid);
                %                               <DX>    <X0>    <Y0>
                X=sscanf(Line,'%f',3);
                FileInfo.Domain(dmn).XCellSize=X(1);
                FileInfo.Domain(dmn).YCellSize=X(1);
                FileInfo.Domain(dmn).XCorner=X(2);
                FileInfo.Domain(dmn).YCorner=X(3);
            end
            
            if FileInfo.Sobek
                Line=fgetnel(fid);
                % DOMAIN                        NUMBER   ID
                if strcmp(Line(1:6),'DOMAIN')
                    dmn=dmn+1;
                else
                    dmn=0;
                end
            else
                dmn=0;
            end
        end
        
        if ~FileInfo.Sobek
            Line=fgetnel(fid);
        end
        
        if strcmpi(Line(1:min(length(Line),5)),'START')
            % START TIME T0: 1996.01.01 10:00:00
            FileInfo.StartTime=sscanf(Line,'%*[ 0A-z]: %i.%i.%i %i:%i:%i',6)';
            Line=fgetnel(fid);
        end
        
        % The following record is not part of any standard (neither FLS nor Sobek)
        if strcmpi(Line(1:min(length(Line),9)),'TIME UNIT')
            % TIME UNIT
            FileInfo.TimeUnit=sscanf(Line(10:end),'%s');
            Line=fgetnel(fid);
        end
    else
        FileInfo.Domain(1).NRows = DomainSize(1);
        FileInfo.Domain(1).NCols = DomainSize(2);
    end
    
    % FLS file:
    % CLASSES OF INCREMENTAL FILE    H       C       Z       U       V
    %
    % SOBEK overland file:
    % CLASSES OF INCREMENTAL FILE    Waterdepth(m) Velocity(m/s)  Waterlevel(m) U-velocity(m/s)  V-velocity(m/s)
    %
    % SIMONA file:
    % CLASSES OF INCREMENTAL FILE    H           Z           U           V           M           A
    if strcmp(FileInfo.Type,'SIMONA')
        NQuant=6;
    else
        NQuant=5;
    end
    Names = Line(29:end);
    for i=1:NQuant
        [FileInfo.Quant(i).Name,Names] = strtok(Names);
        FileInfo.Quant(i).Class=[];
    end
    Line=fgetnel(fid);
    while ~isempty(Line) && strcmp(Line(1),' ')
        X=sscanf(Line,'%f',[1 NQuant]);
        if length(X)<NQuant
            break
        end % empty line of spaces?
        for i=1:NQuant
            if X(i)~=-999
                FileInfo.Quant(i).Class(end+1)=X(i);
            end
        end
        Line=fgetnel(fid); % Line of class boundaries, or ENDCLASSES
    end
    
    FileInfo.StartData=ftell(fid);
    for i=1:NQuant
        FileInfo.Quant(i).Time=-inf;
        FileInfo.Quant(i).Offset=FileInfo.StartData;
        %  FileInfo.Quant(i).NEntries=0;
        if ~isempty(FileInfo.Quant(i).Class)
            FileInfo.Quant(i).Field=cell(1,length(FileInfo.Domain));
            for dmn=1:length(FileInfo.Domain)
                FileInfo.Quant(i).Field{dmn}=repmat(NaN,FileInfo.Domain(dmn).NRows,FileInfo.Domain(dmn).NCols);
            end
        else
            FileInfo.Quant(i).Field=[];
        end
    end
    
    if strcmp(FileInfo.Type,'SIMONA')
        Line = lower(fgetl(fid));
        X = sscanf(Line,'%i, %i, time= %f',3);
        StartTime = X(3)/60;
    else
        StartTime=fscanf(fid,'%f',1);
    end
    if ~isempty(StartTime)
        FileInfo.Begin=StartTime;
        FileInfo.End=GetLastTime(fid);
        FileInfo.DisplayStep=0.25; % time step for displaying INCREMENTAL file <-------------
    end
    if strcmp(FileInfo.Type,'SIMONA')
        FileInfo.End=FileInfo.End/60;
    end
    fclose(fid);
    FileInfo.Check='OK';
catch
    fclose(fid);
    rethrow(lasterror)
end


function LastTime=GetLastTime(fid)
fseek(fid,0,1);
while 1
    Chunk=min(ftell(fid),1000);
    fseek(fid,-Chunk,0);
    Str=fread(fid,[1 Chunk],'*char');
    LastPoint=max(strfind(Str,'.'));
    if isempty(LastPoint)
        fseek(fid,-Chunk,0); % continue with next chunk
    else
        fseek(fid,-Chunk+LastPoint-10,0);
        Str=fread(fid,[1 30],'*char');
        break
    end
end
LastPoint=max(strfind(Str,'.')); % 10
Blanks=find(ismember(Str,[10 13 32]));
i=sum(Blanks<LastPoint);
Str=Str(Blanks(i)+1:Blanks(i+1)-1);
LastTime=sscanf(Str,'%f');


function Data=Local_inc2sparse(FI,nsteps)
%
% NOTE:
% To distinguish between points in (space,time) at which the value does not
% change (indicated by a value 0 in the sparse matrix) and drying i.e.
% transitioning to the 0 class all class numbers are incremented by 1.
%
if nargin==1
    nsteps = 2;
end
if isfield(FI,'Class') % fls fileinfo, lower to inc/class fileinfo
    FileInfo=FI.Class.FileInfo;
else
    FileInfo=FI;
end
%
%qnts = {'Waterdepth(m)','Velocity(m/s)','Waterlevel(m)','U-velocity(m/s)','V-velocity(m/s)'};
nQuants = length(FileInfo.Quant);
for d = 1:length(FileInfo.Domain)
    if isfield(FileInfo.Domain(d),'Id')
        Domain.Name = FileInfo.Domain(d).Id;
    end
    Domain.Size = [FileInfo.Domain(d).NRows FileInfo.Domain(d).NCols];
    for q = 1:nQuants
        Domain.Quant(q).Name = FileInfo.Quant(q).Name;
        Domain.Quant(q).NTim = 0;
        Domain.Quant(q).Time = repmat(NaN,1,10000);
        Domain.Quant(q).NVal = 0;
        Domain.Quant(q).Data = [];
    end
    Data.Domain(d) = Domain;
end
%
fid=fopen(FileInfo.FileName,'r','n','US-ASCII');
if fid<0
    return
end
%
for first=1:-1:0
    fseek(fid,FileInfo.StartData,-1);
    while 1
        switch FileInfo.Type
            case 'SOBEK'
                % .08333 0 1 2
                % 13 14 1
                % 13 15 4
                % 13 16 5
                % 14 15 1
                % 20 22 0
                % 21 21 0
                % .08333 0 1 3
                % 30 77 0
                nr0 = 4;
                ti = 1;
                qi = 3;
                [X,nr]=fscanf(fid,'%f %i %i %i',4);
            case 'FLS'
                % .1223 0 1
                % 13 185 2
                % 14 185 8
                % 15 205 2
                % 17 185 9
                % 30 205 3
                % 32 193 3
                nr0 = 3;
                ti = 1;
                qi = 3;
                [X,nr]=fscanf(fid,'%f %i %i',3);
            case 'SIMONA'
                % 360,    1, time=   360.000, field=h
                % 360,    2, time=   360.000, field=zeta
                % 360, 1,    2,  174, 2
                % 360, 2,    2,  174, 9
                % 360, 1,    2,  175, 2
                % 360, 2,    2,  175, 9
                % 360, 1,    2,  176, 2
                % 360, 2,    2,  176, 9
                % 360, 1,    2,  177, 2
                Line = lower(fgetl(fid));
                ti = 3;
                qi = 2;
                nr0 = 3;
                [X,nr]=sscanf(Line,'%i, %i, time= %f',3);
                X(3)=X(3)/60;
        end
        if nr ~= nr0
            pushbackline(fid);
            fprintf('Error reading time line: %s\n',fgetl(fid));
            break
        else
            t=X(ti);
            q=X(qi);
            if nr==4
                d=X(4);
            end
        end
        %
        % apply changes and track offset
        switch FileInfo.Type
            case 'SIMONA'
                nr0 = 5;
                [X,nr]=fscanf(fid,'%i, %i, %i, %i, %i',[5 inf]);
            otherwise
                nr0 = 3;
                [X,nr]=fscanf(fid,'%i',[3 inf]);
        end
        %
        ntim = Data.Domain(d).Quant(q).NTim+1;
        if nr>nr0-1 || ntim==1
            nr = nr/nr0;
            Data.Domain(d).Quant(q).NTim = ntim;
            %
            if first
                if ntim>length(Data.Domain(d).Quant(q).Time)
                    Data.Domain(d).Quant(q).Time = [Data.Domain(d).Quant(q).Time repmat(NaN,1,ntim-1)];
                end
                Data.Domain(d).Quant(q).Time(ntim)=t;
                %
                if ntim==1
                    Data.Domain(d).Quant(q).NVal = prod(Data.Domain(d).Size);
                elseif strcmp(FileInfo.Type,'SIMONA')
                    Q = X(2,1:floor(nr));
                    for q = 1:nQuants
                        Data.Domain(d).Quant(q).NVal = Data.Domain(d).Quant(q).NVal+sum(Q==q);
                    end
                else
                    Data.Domain(d).Quant(q).NVal = Data.Domain(d).Quant(q).NVal+floor(nr);
                end
            else
                % Data.Domain(d).Quant(q).Time(ntim)=t;
                %
                if floor(nr)~=nr
                    X=X(:,1:end-1);
                    nr = floor(nr);
                end
                if nr0==5
                    % SIMONA case
                    if ntim==1
                        nr = prod(Data.Domain(d).Size);
                        V = ones(3,nr);
                        %V(1,:) = 1 %=ntim
                        V(2,:) = 1:nr;
                        %V(3,:) = 1 %=dry
                        Data.Domain(d).Quant(q).NVal = nr;
                        Data.Domain(d).Quant(q).Data(:,1:nr)=V;
                        if ~isempty(X)
                            I=sub2ind(Data.Domain(d).Size,X(3,:),X(4,:));
                            X(5,:)=X(5,:)+1;
                            for q = 1:nQuants
                                isQ = X(2,:)==q;
                                Data.Domain(d).Quant(q).Data(3,I(isQ))=X(5,isQ);
                            end
                        end
                    else
                        X(3,:)=sub2ind(Data.Domain(d).Size,X(3,:),X(4,:));
                        X(4,:)=ntim;
                        X(5,:)=X(5,:)+1;
                        for q = 1:nQuants
                            n = Data.Domain(d).Quant(q).NVal;
                            isQ = X(2,:)==q;
                            nr = sum(isQ);
                            Data.Domain(d).Quant(q).NVal = n+nr;
                            Data.Domain(d).Quant(q).Data(:,n+(1:nr))=X(3:5,isQ);
                        end
                    end
                else
                    % SOBEK and FLS cases
                    if ntim==1
                        nr = prod(Data.Domain(d).Size);
                        V = ones(3,nr);
                        %V(1,:) = 1 %=ntim
                        V(2,:) = 1:nr;
                        %V(3,:) = 1 %=dry
                        if ~isempty(X)
                            I = sub2ind(Data.Domain(d).Size,X(1,:),X(2,:));
                            V(3,I)=X(3,:)+1;
                        end
                        X = V;
                    else
                        X(2,:)=sub2ind(Data.Domain(d).Size,X(1,:),X(2,:));
                        X(1,:)=ntim;
                        X(3,:)=X(3,:)+1;
                    end
                    n = Data.Domain(d).Quant(q).NVal;
                    Data.Domain(d).Quant(q).NVal = n+nr;
                    Data.Domain(d).Quant(q).Data(:,n+(1:nr))=X;
                end
            end
        end
        Chr=fscanf(fid,'%c',1);
        if feof(fid)
            % reached EOF
            break
        else
            switch Chr
                case '.'
                    % SOBEK and FLS cases
                    % M,N,CLASS reading stopped by FLOATING POINT TIME
                    pushbackline(fid);
                case {'t','T'}
                    % SIMONA case
                    pushbackline(fid);
                otherwise
                    % unknown error: break loop explicitely
                    pushbackline(fid);
                    Line = fgetl(fid);
                    fclose(fid);
                    error('Invalid character while reading line "%s".',Line)
            end
        end
    end
    %
    if nsteps==1
        fclose(fid);
        Times = cell(length(Data.Domain),length(nQuants));
        for d = 1:length(Data.Domain)
            for q = 1:nQuants
                Times{d,q} = Data.Domain(d).Quant(q).Time(1:Data.Domain(d).Quant(q).NTim);
            end
        end
        FileInfo.Times = unique([Times{:}]);
        if isfield(FI,'Class') % fls fileinfo, lower to inc/class fileinfo
            FI.Class.FileInfo = FileInfo;
            Data = FI;
        else
            Data = FileInfo;
        end
        return
    end
    %
    if first
        for d = 1:length(Data.Domain)
            for q = 1:length(Data.Domain(d).Quant)
                % ntim = Data.Domain(d).Quant(q).NTim;
                % Data.Domain(d).Quant(q).Time = zeros(1,ntim);
                Data.Domain(d).Quant(q).NTim = 0;
                nval = Data.Domain(d).Quant(q).NVal;
                Data.Domain(d).Quant(q).Data = zeros(3,nval);
                Data.Domain(d).Quant(q).NVal = 0;
            end
        end
    end
end
for d = 1:length(Data.Domain)
    for q = 1:nQuants
        Data.Domain(d).Quant(q).Time = Data.Domain(d).Quant(q).Time(1:Data.Domain(d).Quant(q).NTim);
    end
    Data.Domain(d).Quant = rmfield(Data.Domain(d).Quant,'NTim');
    Data.Domain(d).Quant = rmfield(Data.Domain(d).Quant,'NVal');
end
fclose(fid);


function [Data,FI2]=Local_read_inc(FileInfo,Field,Time)
Data=[];
FI2=[];
if isfield(FileInfo,'Class') % fls fileinfo, lower to inc/class fileinfo
    FI2=FileInfo;
    FileInfo=FileInfo.Class.FileInfo;
end

if Time<FileInfo.Quant(Field).Time
    FileInfo.Quant(Field).Field=cell(1,length(FileInfo.Domain));
    for dmn=1:length(FileInfo.Domain)
        FileInfo.Quant(Field).Field{dmn}=repmat(NaN,FileInfo.Domain(dmn).NRows,FileInfo.Domain(dmn).NCols);
    end
    FileInfo.Quant(Field).Offset=FileInfo.StartData;
    FileInfo.Quant(Field).Time=-inf;
end
fid=fopen(FileInfo.FileName,'r','n','US-ASCII');
if fid<0
    if isempty(FI2)
        FI2=FileInfo;
    end
    return
end

fseek(fid,FileInfo.Quant(Field).Offset,-1);
t=FileInfo.Quant(Field).Time;
X=[-inf 0 -1];
%
switch FileInfo.Type
    case 'SOBEK'
        nr0 = 4;
        ti = 1;
        qi = 3;
        X=[-inf 0 -1];
    case 'FLS'
        nr0 = 3;
        ti = 1;
        qi = 3;
        X=[-inf 0 -1];
    case 'SIMONA'
        ti = 3;
        qi = 2;
        nr0 = 3;
        X=[0 -1 -inf];
end
%
while t<=Time
    if X(qi)==-1
        % first time do nothing ...
    elseif strcmp(FileInfo.Type,'SIMONA') || X(qi)==Field
        % apply changes and track offset
        switch FileInfo.Type
            case 'SIMONA'
                [X,nr]=fscanf(fid,'%i, %i, %i, %i, %i',[5 inf]);
                i=1:floor(nr/5);
                i(X(2,i)==Field)=[];
                ix=3;
                iy=4;
                ic=5;
            otherwise
                [X,nr]=fscanf(fid,'%i',[3 inf]);
                i=1:floor(nr/3);
                ix=1;
                iy=2;
                ic=3;
        end
        if ~isempty(i)
            Ind=sub2ind(size(FileInfo.Quant(Field).Field{dmn}),X(ix,i),X(iy,i));
            FileInfo.Quant(Field).Field{dmn}(Ind)=X(ic,i);
        end
        FileInfo.Quant(Field).Time=t;
        Chr=lower(fscanf(fid,'%c',1));
        if strcmp(Chr,'.') | strcmp(Chr,'t')
            % M,N,CLASS reading stopped by FLOATING POINT TIME
            pushbackline(fid);
            FileInfo.Quant(Field).Offset=ftell(fid);
        else
            % reached EOF or unknown error: break loop explicitely
            break
        end
    else
        % skip data and track offset
        % this happens only for non-SIMONA cases
        FileInfo.Quant(Field).Offset=ftell(fid);
        fscanf(fid,'%i',[3 inf]);
        Chr=fscanf(fid,'%c',1);
        if strcmp(Chr,'.')
            % M,N,CLASS reading stopped by FLOATING POINT TIME
            pushbackline(fid);
            FileInfo.Quant(Field).Offset=ftell(fid);
        else
            % reached EOF or unknown error: break loop explicitely
            break
        end
    end
    switch FileInfo.Type
        case 'SOBEK'
            [X,nr]=fscanf(fid,'%f %i %i %i',4);
        case 'FLS'
            [X,nr]=fscanf(fid,'%f %i %i',3);
        case 'SIMONA'
            Line = lower(fgetl(fid));
            [X,nr]=sscanf(Line,'%i, %i, time= %f',3);
            X(3)=X(3)/60;
    end
    if nr==nr0
        if X(qi)==Field
            t=X(ti);
            if nr0==4
                dmn=X(4);
            else
                dmn=1;
            end
        end
    else
        pushbackline(fid);
        fprintf('Error reading time line: %s\n',fgetl(fid));
        break
    end
end
fclose(fid);
Data=FileInfo.Quant(Field).Field;
%
if length(Data)==1
    Data=Data{1};
end % backward compatibility
if isempty(FI2)
    FI2=FileInfo;
    
else
    FI2.Class.FileInfo=FileInfo;
end


function pushbackline(fid)
%PUSHBACKLINE Resets file to start of current line
NR=0;
while 1
    nr=min(10,ftell(fid));
    fseek(fid,-nr-NR,0);
    X=fread(fid,nr,'uint8');
    if any(X==10)
        % line feed found
        i=max(find(X==10));
        fseek(fid,-nr+i,0);
        break
    elseif nr<10
        % beginning of file, cannot go back further
        break
    end
    NR=nr;
end


function Line=fgetnel(Fid)
% get the next non-empty line from the Input
Line='';
while ~feof(Fid) && isempty(Line)
    Line=fgetl(Fid);
end
