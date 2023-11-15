function [Out,FI2]=fls(cmd,varargin)
%FLS Read Delft-FLS and SOBEK2D incremental files.
%   FILEDATA = FLS('open',FILENAME) opens the specified Delft-FLS input
%   file (mdf file), incremental file, binary/ascii history file, or
%   cross-sections data file. In case of an mdf file, the function reads
%   and checks data from the file and scans the directory for simulation
%   output.
%
%   [DATA,FILEDATA] = FLS('inc',FILEDATA,FIELD,TIME) determines the data
%   (classes) for the selected FIELD at TIME from the incremental data
%   file. The routine returns the data and an updated FileData (contains
%   state at retrieved time for faster reading of data at later times).
%
%   INCDATA = FLS('inc',FILEDATA) loads incremental file into memory for
%   data analysis. Use INCANALYSIS to analyse the returned data structure.
%
%   TIMES = FLS('bin',FILEDATA,'T',TINDEX) determines the time in the
%   binary history file. If the time index TINDEX is not specified, all
%   available times will be returned.
%
%   DATA = FLS('bin',FILEDATA,FIELD,STATION,TINDEX) reads data for the
%   selected station at the specified time steps from the binary history
%   file, where FIELD equals 'S'(or 'Z'), 'U', 'V' or 'H' for water level,
%   velocity components in X and Y direction and water depth, respectively.
%   If the time index TINDEX is not specified, data is returned for all
%   time steps.
%
%   DATA = FLS('his',FILEDATA,STATION) reads all data for a station from
%   the ascii history file.
%
%   DATA = FLS('cross',FILEDATA,'T',TINDEX) reads time associated with the
%   specified time indices. If TINDEX is not specified, all available times
%   will be returned.
%
%   DATA = FLS('cross',FILEDATA,CROSSNR,TINDEX) reads data of specified
%   cross-section numbers at specified time indices. If TINDEX is not
%   specified, data is returned for all time steps.
%
%   DATA = FLS('bottom',FILEDATA,TIME) determines the bed level data at
%   indicated TIME based on the initial bed level and dam break data.
%
%   See also INCANALYSIS, SOBEK, ARCGRID, QPFOPEN, QPREAD.

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

if nargout>0
    Out=[];
    Data=[];
end
if nargin==0
    return
end
switch lower(cmd)
    case {'read','open','readbin','readhis','readcross','readinc'}
        if nargin==1
            [fn,fp]=uigetfile('*.*');
            if ~ischar(fn)
                return
            end
            filename=[fp fn];
        else
            filename=varargin{1};
        end
        [fp,fn,ext]=fileparts(filename);
        Structure=[];
        switch lower(ext)
            case '.mdf'
                Structure=Local_read_fls(filename);
            case '.inc'
                Structure=incremental('open',filename);
            case '.bin'
                Structure=Local_open_historybin(filename);
            case '.his'
                Structure=Local_open_historyhis(filename);
            case '.crs'
                Structure=Local_open_cross(filename);
        end
        if isstruct(Structure) & strcmp(Structure.Check,'OK')
            Data = Structure;
        end
    case 'inc2sparse'
        Data=incremental('inc2sparse',varargin{:});
    case 'inc'
        [Data,FI2]=incremental('read',varargin{:});
    case 'bin'
        Data=Local_read_bin(varargin{:});
    case 'his'
        Data=Local_read_his(varargin{:});
    case 'bottom'
        Data=Local_read_bottom(varargin{:});
    case 'cross'
        Data=Local_read_cross(varargin{:});
    otherwise
        uiwait(msgbox('unknown command','modal'));
end
if nargout>0
    Out=Data;
end

function Data=Local_read_bottom(FileInfo,Time)
Data=FileInfo.Depth;
for i=1:length(FileInfo.DamBreak)
    T=max(find(FileInfo.DamBreak(i).Table(:,1)<=Time));
    if T==size(FileInfo.DamBreak(i).Table,1) % >= last time
        Data(FileInfo.DamBreak(i).M,FileInfo.DamBreak(i).N)=FileInfo.DamBreak(i).Table(end,2);
    elseif T==1
        Data(FileInfo.DamBreak(i).M,FileInfo.DamBreak(i).N)=FileInfo.DamBreak(i).Table(1,2);
    else
        f1=(Time-FileInfo.DamBreak(i).Table(T,1))/(FileInfo.DamBreak(i).Table(T+1,1)-FileInfo.DamBreak(i).Table(T,1));
        Data(FileInfo.DamBreak(i).M,FileInfo.DamBreak(i).N)=FileInfo.DamBreak(i).Table(T,2)*f1+FileInfo.DamBreak(i).Table(T+1,2)*(1-f1);
    end
end

function Structure=Local_read_fls(filename)
Structure.Check='NotOK';
Structure.FileType='FLS-mdf';

fid=fopen(filename,'rt','n','US-ASCII');

% Find grid dimensions
% ARC-INFO header not supported <-------------------------------------------
fseek(fid,0,-1);
Line=Local_seek_Keyword(fid,'MAIN DIMENSIONS');
if ~isempty(Line)
    Line=fgetnel(fid);
    Structure.Size=sscanf(Line,'%f',[1 2]);
else
    fclose(fid);
    return
end

% Find grid coordinates
Line=Local_seek_Keyword(fid,'GRID');
if ~isempty(Line)
    Line=fgetnel(fid);
    X=sscanf(Line,'%f',[1 3]);
    Structure.GridSize=X(1);
    Structure.UpperLeft=X(2:3); % LowerLeft!
else
    fclose(fid);
    return
end

% Store filename and base
Structure.FileName=filename;
Structure.FileBase=filename(1:(length(filename)-4)); % remove .mdf or .MDF

% Construct depth data
Structure.Depth=arcgrid('read',[Structure.FileBase,'000.adp']);
if strcmp(Structure.Depth.Check,'NotOK')
    Structure.Depth=arcgrid('read',[Structure.FileBase,'000.aht']);
    if strcmp(Structure.Depth.Check,'NotOK')
        Structure.Depth=Local_read_array(fid,'DEPTH',Structure.Size); % also stored in '<base>.adp' file
        Temp=Local_read_array(fid,'HEIGHT',Structure.Size); % also stored in '<base>.aht' file
        if isempty(Structure.Depth) & isempty(Temp)
            fclose(fid);
            disp('No elevation data found in mdf file.');
            return
        elseif ~isempty(Structure.Depth) & ~isempty(Temp)
            fclose(fid);
            disp('Both DEPTH and HEIGHT keyword encountered in mdf file.');
            return
        elseif ~isempty(Temp) % HEIGHT but not DEPTH
            Structure.Depth=-Temp;
        end
    else
        Structure.Depth=-Structure.Depth.Data;
    end
else
    Structure.Depth=Structure.Depth.Data;
end

% Extreme waterdepths are stored in '<base>.ahx' file

% Find computational time frame
fseek(fid,0,-1);
Line=Local_seek_Keyword(fid,'TIMEFRAME');
if ~isempty(Line)
    Line=fgetnel(fid);
    X=sscanf(Line,'%f',[1 3]);
    Structure.Step=X(1); % time step in seconds
    Structure.Begin=X(2); % start time in hours
    Structure.End=X(3); % end time in hours
    Structure.DisplayStep=0.25; % time step for displaying INCREMENTAL file <-------------
else
    fclose(fid);
    disp('No time frame in mdf file.');
    return
end

% Find output time steps
fseek(fid,0,-1);
Line=Local_seek_Keyword(fid,'TIMES OF HIS');
if ~isempty(Line)
    Line=fgetnel(fid);
    X=sscanf(Line,'%f',[1 3]);
    Structure.HistWriteStep=X(1);
    Structure.MapWriteStep=X(2);
    Structure.FirstWrite=X(3);
else
    fclose(fid);
    disp('No output times in mdf file.');
    return
end

%
filename=fopen(fid); % get filename
Path=filename(1:max(strfind(filename,filesep))); % extract path

% Find breaking dams
fseek(fid,0,-1);
Structure.DamBreak=[];
Line=Local_seek_Keyword(fid,'BREAKING DAMS');
if ~isempty(Line)
    i=1;
    while 1
        Line=fgetnel(fid);
        X=sscanf(Line,'%f',[1 5]);
        switch length(X)
            case 5
                M=X(1);
                N=X(2);
                Structure.DamBreak(i).M=M;
                Structure.DamBreak(i).N=N;
                Structure.DamBreak(i).Table=[-inf Structure.Depth(M,N); ...
                    X(3) Structure.Depth(M,N); ...
                    X(4) Structure.Depth(M,N)+X(5)];
                i=i+1;
            case 3 %
                BDTfile=[Path Line(max(strfind(deblank(Line),' '))+1:end)];
                if exist(BDTfile,'file')
                    M=X(1);
                    N=X(2);
                    Structure.DamBreak(i).M=M;
                    Structure.DamBreak(i).N=N;
                    Structure.DamBreak(i).Table=load(BDTfile);
                    Structure.DamBreak(i).Table(:,1)=X(3)+Structure.DamBreak(i).Table(:,1);
                    Structure.DamBreak(i).Table(:,2)=Structure.Depth(M,N)+Structure.DamBreak(i).Table(:,2);
                    Structure.DamBreak(i).Table=[-inf Structure.Depth(M,N); Structure.DamBreak(i).Table];
                    i=i+1;
                else
                    disp(['Warning: Breaking dam file ' BDTfile ' not found.']);
                end
            otherwise
                break
        end
    end
end

% Find output map file types
fseek(fid,0,-1);
Structure.Map.H=0; % waterdepth
Structure.Map.U=0; % flow velocity
Structure.Map.Z=0; % waterlevel
Structure.Map.u=0; % u-velocity
Structure.Map.v=0; % v-velocity
Line=Local_seek_Keyword(fid,'QUANTITIES IN MAPFILES');
if ~isempty(Line)
    Structure.Map.H=~isempty(strfind(Line(23:end),'H')); % waterdepth
    Structure.Map.U=~isempty(strfind(Line(23:end),'C')); % flow velocity
    Structure.Map.Z=~isempty(strfind(Line(23:end),'Z')); % waterlevel
    Structure.Map.u=~isempty(strfind(Line(23:end),'U')); % u-velocity
    Structure.Map.v=~isempty(strfind(Line(23:end),'V')); % v-velocity
    for c='HUZuv'
        if getfield(Structure.Map,c)
            switch c
                case 'H' % waterdepth
                    LocExtension='amh';
                case 'U' % flow velocity
                    LocExtension='amc';
                case 'Z' % waterlevel
                    LocExtension='amz';
                case 'u' % u-velocity
                    LocExtension='amu';
                case 'v' % v-velocity
                    LocExtension='amv';
            end
            Files=dir([Structure.FileBase '*.' LocExtension]);
            Files=str2mat(Files.name);
            if isempty(Files)
                Files=[];
            else
                Files=Files(:,size(Files,2)+(-6:-4));
                Files=sort(sscanf(str2mat(transpose(Files),' '),'%f'));
                if Structure.MapWriteStep<1
                    Files=Structure.MapWriteStep*Files;
                else
                    Files=Structure.FirstWrite+Structure.MapWriteStep*ceil((Files-Structure.FirstWrite)/Structure.MapWriteStep);
                end
            end
            Structure.Map=setfield(Structure.Map,strcat(c,'Times'),Files);
        end
    end
end

% Find output his locations STATIONS FOR HISTORY
fseek(fid,0,-1);
Structure.Bin.M=[];
Structure.Bin.N=[];
Line=Local_seek_Keyword(fid,'STATIONS');
if ~isempty(Line)
    Line=fgetnel(fid);
    Loc=0;
    while ~isempty(Line) & strcmp(Line(1),' ')
        X=sscanf(Line,'%i',[1 2]);
        if length(X)<2
            break
        end % empty line of spaces?
        Loc=Loc+1;
        Structure.Bin.M(Loc)=X(1);
        Structure.Bin.N(Loc)=X(2);
        Line=fgetnel(fid);
    end
    % History data is stored in '<base>.bin' file
    Structure.Bin.FileInfo=Local_open_historybin([Structure.FileBase '.bin']);
    % Look for history file
    Structure.His.FileInfo=Local_open_historyhis([Structure.FileBase '.his']);
end


% Find classes of the incremental file
fseek(fid,0,-1);
Structure.Class.H=[];
Structure.Class.U=[];
Structure.Class.Z=[];
Structure.Class.u=[];
Structure.Class.v=[];
Line=Local_seek_Keyword(fid,'CLASSES OF INCREMENTAL FILE');
if ~isempty(Line)
    Line=fgetnel(fid);
    while ~isempty(Line) & strcmp(Line(1),' ')
        X=sscanf(Line,'%f',[1 5]);
        if length(X)<5
            break
        end % empty line of spaces?
        if X(1)~=-999
            Structure.Class.H(end+1)=X(1);
        end
        if X(2)~=-999
            Structure.Class.U(end+1)=X(2);
        end
        if X(3)~=-999
            Structure.Class.Z(end+1)=X(3);
        end
        if X(4)~=-999
            Structure.Class.u(end+1)=X(4);
        end
        if X(5)~=-999
            Structure.Class.v(end+1)=X(5);
        end
        Line=fgetnel(fid);
    end
    % The incremental file is '<base>.inc' file for Delft-FLS and
    % it is FLS_H.INC for Sobek overland.
    Structure.Class.FileInfo=incremental('open',[Structure.FileBase '.inc']);
    if strcmp(Structure.Class.FileInfo.Check,'NotOK')
        % Sobek?
        fp=fileparts(Structure.FileBase);
        Structure.Class.FileInfo=incremental('open',fullfile(fp,'fls_h.inc'));
        if strcmp(Structure.Class.FileInfo.Check,'OK')
            fprintf('WARNING: Sobek incremental files not fully supported.\n');
        end
    end
end

% Find output cross sections CROSS SECTIONS
fseek(fid,0,-1);
Structure.Cross.M=[];
Structure.Cross.N=[];
Line=Local_seek_Keyword(fid,'CROSS SECTIONS');
if ~isempty(Line)
    Line=fgetnel(fid);
    while ~isempty(Line) & strcmp(Line(1),' ')
        X=sscanf(Line,'%f',[1 4]);
        if length(X)<4
            break
        end % empty line of spaces?
        Structure.Cross.M(end+1,1:2)=[X(1) X(3)];
        Structure.Cross.N(end+1,1:2)=[X(2) X(4)];
        Line=fgetnel(fid);
    end
    % The cross sections file is '<base>.crs' file
    Structure.Cross.FileInfo=Local_open_cross([Structure.FileBase '.crs']);
end

fclose(fid);
Structure.Check='OK';

function Line=Local_seek_Keyword(fid,Keyword)
% search a keyword
while ~feof(fid)
    Line=fgetnel(fid);
    if (length(Line)>length(Keyword)) & (~isempty(strfind(Line,Keyword)))
        return
    end
end
Line='';

function Array=Local_read_array(fid,Keyword,Size,ArrayIn)
% read/construct an array
%   Array=Local_read_array(fid,Keyword,Size,ArrayIn);
%   Array=Local_read_array(fid,Keyword,Size);
if nargin<4
    Array=[];
else
    Array=ArrayIn;
end

fseek(fid,0,-1);

MissingValue=-999;

filename=fopen(fid); % get filename
Path=filename(1:max(strfind(filename,filesep))); % extract path

while ~feof(fid)
    Line=upper(fgetnel(fid));
    if ~isempty(strfind(Line,Keyword))
        % operator specified?
        Op=strfind(Line,'OPERATOR');
        if ~isempty(Op)
            Op=Op+8;
            while (Op<length(Line)) & strcmp(Line(Op),' ')
                Op=Op+1;
            end
            switch Line(Op)
                case 'A'
                    switch Line(Op+(0:2)),
                        case 'ALL'
                            OpType=Line(Op+(0:2));
                        otherwise
                            OpType='?';
                    end
                case 'M'
                    switch Line(Op+(0:2))
                        case {'MIN','MAX','MIS'}
                            OpType=Line(Op+(0:2));
                        otherwise
                            OpType='?';
                    end
                case {'+','-','*','/'}
                    OpType=Line(Op);
                otherwise
                    OpType='?';
            end
        else
            OpType='ALL';
        end
        FN=strfind(Line,'FILENAME');
        if ~isempty(FN)
            FileName=sscanf(Line((FN+8):end),'%s',1);
            FileName=[Path FileName];
            subfid=fopen(FileName,'rt','n','US-ASCII');
            if subfid>0
                Array=Local_read_array(subfid,Keyword,Size,Array); % recursive file reading
                fclose(subfid);
            else
                fprintf('Cannot open the file %s.',FileName);
                fprintf('The data will be incomplete.\n');
                Array=[];
            end
        else
            LUNI=strfind(Line,'UNI'); % UNIFORM
            LCBL=strfind(Line,'CBL'); % CBLOCK
            LVBL=strfind(Line,'VBL'); % VBLOCK
            LPOI=strfind(Line,'POI'); % POINT
            LCPO=strfind(Line,'CPO'); % CPOLYGON
            LVPO=strfind(Line,'VPO'); % VPOLYGON
            LARE=strfind(Line,'ARE'); % AREA
            LARC=strfind(Line,'ARC'); % ARCINFO
            LFRE=strfind(Line,'FRE'); % FREE FORMAT
            array=MissingValue*ones(Size);
            if ~isempty(LUNI) % UNIFORM
                Value=sscanf(Line((LARC+7):end),'%f',1);
                array=Value*ones(Size);
            elseif ~isempty(LCBL) % CBLOCK
                Value=sscanf(Line((LARC+7):end),'%f',1);
                Line=upper(fgetnel(fid));
                MN=sscanf(Line,'%f',4);
                array(max(1,MN(2)):min(end,MN(4)),max(1,MN(1)):min(end,MN(3)))=Value;
            elseif ~isempty(LVBL) % VBLOCK
                Line=upper(fgetnel(fid));
                MN=sscanf(Line,'%f',4);
                array(max(1,MN(2)):min(end,MN(4)),max(1,MN(1)):min(end,MN(3)))= ...
                    transpose(fscanf(fid,'%f',[min(size(array,1),MN(3))-max(1,MN(1))+1 ...
                    min(size(array,2),MN(4))-max(1,MN(2))+1]));
            elseif ~isempty(LPOI) % POINT
                Line=upper(fgetnel(fid));
                [MN,NumRead]=sscanf(Line,'%f',3);
                while NumRead==3
                    array(MN(2),MN(1))=MN(3);
                    Line=upper(fgetnel(fid));
                    if ischar(Line)
                        [MN,NumRead]=sscanf(Line,'%f',3);
                    else
                        NumRead=-1;
                    end
                end
                fseek(fid,-length(Line)-2,0); % unread last line
            elseif ~isempty(LCPO) % CPOLYGON
                disp('CPOLYGON not yet implemented.');
                Value=sscanf(Line((LARC+7):end),'%f',1);
                Line=upper(fgetnel(fid));
                [MN,NumRead]=sscanf(Line,'%f',2);
                while NumRead==2
                    %        array(MN(2),MN(1))=Value;
                    Line=upper(fgetnel(fid));
                    if ischar(Line)
                        [MN,NumRead]=sscanf(Line,'%f',2);
                    else
                        NumRead=-1;
                    end
                end
                if ischar(Line)
                    fseek(fid,-length(Line)-2,0);
                end % unread last line
            elseif ~isempty(LVPO) % VPOLYGON
                disp('VPOLYGON not yet implemented.');
                Line=upper(fgetnel(fid));
                [MN,NumRead]=sscanf(Line,'%f',3);
                while NumRead==3
                    %        array(MN(2),MN(1))=MN(3);
                    Line=upper(fgetnel(fid));
                    if ischar(Line)
                        [MN,NumRead]=sscanf(Line,'%f',3);
                    else
                        NumRead=-1;
                    end
                end
                fseek(fid,-length(Line)-2,0); % unread last line
            elseif ~isempty(LARE) % AREA
                disp('AREA not yet implemented.');
                Value=sscanf(Line((LARC+7):end),'%f',1);
                Line=upper(fgetnel(fid));
                [MN,NumRead]=sscanf(Line,'%f',2);
                while NumRead==3
                    %        array(MN(2),MN(1))=Value;
                    Line=upper(fgetnel(fid));
                    if ischar(Line)
                        [MN,NumRead]=sscanf(Line,'%f',2);
                    else
                        NumRead=-1;
                    end
                end
                fseek(fid,-length(Line)-2,0); % unread last line
            elseif ~isempty(LARC) % ARCINFO
                FileName=sscanf(Line((LARC+9):end),'%s',1);
                Path=filename(1:max(strfind(filename,filesep)));
                FileName=[Path FileName];
                array=arcgrid('read',FileName);
                if ~isfield(array,'Data')
                    fprintf('Error reading ARCINFO file: %s\n',FileName);
                    array=[]; %zeros(Size);
                else
                    array=array.Data;
                end
            elseif ~isempty(LFRE) % FREE FORMAT
                FileName=sscanf(Line((LARC+12):end),'%s',1);
                Local_fid=fopen(FileName,'rt','n','US-ASCII');
                array=fscanf(Local_fid,'%f',Size); % transpose
                fclose(Local_fid);
            else
                fprintf('Unknow process in line:\n%s\n',Line);
            end
            if ~isequal(size(array),Size)
                uiwait(msgbox('Invalid size of data matrix.','modal'));
                Array=[];
                return
            end
            switch OpType
                case 'ALL'
                    if isempty(Array),
                        Array=array;
                    else
                        Index=(array(:)~=MissingValue);
                        Array(Index)=array(Index);
                    end
                case 'MIS'
                    Index=(Array(:)~=MissingValue) & (array(:)~=MissingValue);
                    Array(Index)=array(Index);
                case '+'
                    Index=(Array(:)~=MissingValue) & (array(:)~=MissingValue);
                    Array(Index)=Array(Index)+array(Index);
                case '-'
                    Index=(Array(:)~=MissingValue) & (array(:)~=MissingValue);
                    Array(Index)=Array(Index)-array(Index);
                case '*'
                    Index=(Array(:)~=MissingValue) & (array(:)~=MissingValue);
                    Array(Index)=Array(Index).*array(Index);
                case '/'
                    Index=(Array(:)~=MissingValue) & (array(:)~=0) & (array(:)~=MissingValue);
                    Array(Index)=Array(Index)./array(Index);
                case 'MAX'
                    Index=(Array(:)~=MissingValue) & (array(:)~=MissingValue);
                    Array(Index)=max(Array(Index),array(Index));
                case 'MIN'
                    Index=(Array(:)~=MissingValue) & (array(:)~=MissingValue);
                    Array(Index)=min(Array(Index),array(Index));
                otherwise
                    uiwait(msgbox(['Operator [',OpType,'] not implemented'],'modal'));
            end
        end
    end
end

if nargin<4
    % set MissingValues to NaN
    Array(Array(:)==MissingValue)=NaN;
end

function FileInfo=Local_open_historybin(filename)
% open history .bin file
FileInfo.Check='NotOK';
FileInfo.FileType='FLS-bin';

fid=fopen(filename,'r','b');
if fid<0
    return
end

FileInfo.FileName=filename;
FileInfo.OS=determineOS(fid);
if isequal(FileInfo.OS,'PCWIN') | isequal(FileInfo.OS,'NEW PCWIN')
    fclose(fid);
    fid=fopen(filename,'r','l');
end

[FileInfo.Header,Error]=freadblock(fid,FileInfo.OS,'char');
if length(FileInfo.Header)==4 % This is not a header
    fseek(fid,0,-1);
    FileInfo.Header='';
end

[FileInfo.NumSta,Error]=freadblock(fid,FileInfo.OS,'int32'); % Number of stations
if length(FileInfo.NumSta)~=1
    fclose(fid);
    error('Unexpected non-scalar number of stations.')
end
FileInfo.M=ones(1,FileInfo.NumSta);
FileInfo.N=ones(1,FileInfo.NumSta);
FileInfo.DP=ones(1,FileInfo.NumSta);

for i=1:FileInfo.NumSta
    [Data,Error]=freadblock(fid,FileInfo.OS,{'int32' 'int32' 'float32'});
    FileInfo.M(i)=Data{1};
    FileInfo.N(i)=Data{2};
    FileInfo.DP(i)=Data{3};
end
FileInfo.Begin=ftell(fid);
FileInfo.NumTimes=0;

switch FileInfo.OS
    case 'PCWIN'
        SkipBlock=2+4+(2+4*4)*FileInfo.NumSta-1;
    case 'NEW PCWIN'
        SkipBlock=2+4+((2+4)*4)*FileInfo.NumSta-1;
    case 'UNIX'
        SkipBlock=3*4+6*4*FileInfo.NumSta-4;
end

if Error
    uiwait(msgbox('Error while reading BIN header','modal'));
    return
end

while ~feof(fid) & ~Error
    x=fread(fid,SkipBlock,'uchar'); % <------------------ ( 4 T 4 ) , ( 16 S U V H 16 ) * NumSta
    switch FileInfo.OS
        case 'PCWIN'
            Data=fread(fid,1,'uint8');
            Error=~feof(fid) & ~isequal(Data,16); % last record size should be 16
        case 'NEW PCWIN'
            Data=fread(fid,1,'uint8');
            Error=~feof(fid) & ~isequal(Data,4); % last record size should be 4
        case 'UNIX'
            Data=fread(fid,1,'uint32');
            Error=~feof(fid) & ~isequal(Data,16); % last record size should be 16
        otherwise
    end
    if ~Error & ~isempty(Data)
        FileInfo.NumTimes=FileInfo.NumTimes+1;
    end
end
fclose(fid);

if Error % display message but continue with the first records available to the user
    uiwait(msgbox(['Error while reading BIN record index ',num2str(FileInfo.NumTimes)],'modal'));
end

FileInfo.Check='OK';

function [OS]=determineOS(fid)
floc=ftell(fid);
fseek(fid,0,-1);
FirstByte=fread(fid,1,'uint8');
if (FirstByte==0) % e.g. 0 0 0 4 (without header) or e.g. 0 0 0 131 (with header)
    OS='UNIX';
    fseek(fid,floc,-1);
else % 4 (without header) or something larger with header, e.g. 75
    OS='PCWIN';
    fseek(fid,floc,-1);
end

function [Data,Error]=freadblock(fid,OS,Type)
Data=[];
Error=0;
switch OS
    case 'PCWIN'
        N1=fread(fid,1,'uint8');
    case 'NEW PCWIN'
        if ftell(fid)==0
            N1=fread(fid,1,'uint8'); % 75
        end
    case 'UNIX'
        N1=fread(fid,1,'uint32');
    otherwise
        fclose(fid);
        error('%s formatted file not supported.',OS)
end
switch OS
    case {'PCWIN','UNIX'}
        if iscell(Type) % variable type
            for i=1:length(Type)
                switch Type{i}
                    case 'int32'
                        Data{i}=fread(fid,1,'int32');
                    case 'float32'
                        Data{i}=fread(fid,1,'float32');
                    case 'char'
                        Data{i}=char(fread(fid,1,'uchar'));
                end
            end
        else % constant type
            switch Type
                case 'int32'
                    Data=fread(fid,N1/4,'int32');
                case 'float32'
                    Data=fread(fid,N1/4,'float32');
                case 'char'
                    Data=char(fread(fid,N1,'uchar'));
                case 'skip'
                    fread(fid,N1,'uchar');
            end
        end
    case {'NEW PCWIN'}
        if iscell(Type) % variable type
            for i=1:length(Type)
                switch Type{i}
                    case 'int32'
                        N1=fread(fid,1,'uint8');
                        Data{i}=fread(fid,1,'int32');
                        N2=fread(fid,1,'uint8');
                    case 'float32'
                        N1=fread(fid,1,'uint8');
                        Data{i}=fread(fid,1,'float32');
                        N2=fread(fid,1,'uint8');
                    case 'char'
                        N1=fread(fid,1,'uint8');
                        Data{i}=char(fread(fid,1,'uchar'));
                        N2=fread(fid,1,'uint8');
                end
            end
        else % constant type
            switch Type
                case 'int32'
                    N1=fread(fid,1,'uint8');
                    Data=fread(fid,N1/4,'int32');
                    N2=fread(fid,1,'uint8');
                case 'float32'
                    N1=fread(fid,1,'uint8');
                    Data=fread(fid,N1/4,'float32');
                    N2=fread(fid,1,'uint8');
                case 'char'
                    N1=fread(fid,1,'uint8');
                    Data=char(fread(fid,N1,'uchar'));
                    N2=fread(fid,1,'uint8');
                case 'skip'
                    N1=fread(fid,1,'uint8');
                    fread(fid,N1,'uchar');
                    N2=fread(fid,1,'uint8');
            end
        end
end
switch OS
    case 'PCWIN'
        N2=fread(fid,1,'uint8');
    case 'UNIX'
        N2=fread(fid,1,'uint32');
end
if ~isequal(N1,N2)
    fclose(fid);
    error('Error reading formatted binary file: inconsistent block length %i versus %i.',N1,N2)
end

function Data=Local_read_bin(FileInfo,Field,Station,Index)
Data=[];
if nargin<2 % no Field
    return
end
if strcmp(Field,'T') % FileInfo, 'T', ...
    if nargin<3 % FileInfo, 'T' % read time at all time indices
        Index=0;
    elseif nargin<4 % FileInfo, 'T', Index % read time at specified time indices
        Index=Station;
    end
else
    if nargin<3 % FileInfo, 'Field'
        return;
    elseif nargin<4 % FileInfo, 'Field', Station  % read field at all time indices
        Index=0;
    else % FileInfo, 'Field', Station, Index % read field at specified time indices
    end
end

if isfield(FileInfo,'His') % fls fileinfo, lower to his fileinfo
    FileInfo=FileInfo.Bin.FileInfo;
end

if isequal(FileInfo.OS,'PCWIN')
    fid=fopen(FileInfo.FileName,'r','l');
else
    fid=fopen(FileInfo.FileName,'r','b');
end
if fid<0
    return
end
switch FileInfo.OS
    case 'PCWIN'
        RecordPadding=1;
    otherwise
        RecordPadding=4;
end
switch Field
    case 'T'
        Shift=RecordPadding;
    case {'S','Z'}
        Shift=4+2*RecordPadding+(Station-1)*(16+2*RecordPadding)+RecordPadding;
    case 'U'
        Shift=4+2*RecordPadding+(Station-1)*(16+2*RecordPadding)+RecordPadding+4;
    case 'V'
        Shift=4+2*RecordPadding+(Station-1)*(16+2*RecordPadding)+RecordPadding+8;
    case 'H'
        Shift=4+2*RecordPadding+(Station-1)*(16+2*RecordPadding)+RecordPadding+12;
    otherwise
        uiwait(msgbox(['Invalid field: ',Field],'modal'));
        return
end
fseek(fid,FileInfo.Begin+Shift,-1);
BytesAllStations=4+2*RecordPadding+(4*4+2*RecordPadding)*FileInfo.NumSta;
if (Index==0) | length(Index)>1
    Data=fread(fid,FileInfo.NumTimes,'float32',BytesAllStations-4);
    if length(Index)>1
        Data=Data(Index);
    end
else
    fseek(fid,BytesAllStations*(Index-1),0);
    Data=fread(fid,1,'float32');
end
fclose(fid);


function FileInfo=Local_open_cross(filename)
% open cross sections file
FileInfo.Check='NotOK';
FileInfo.FileType='FLS-cross';

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    return
end

FileInfo.FileName=filename;

Line=fgetnel(fid); %/* runid
if length(Line)<2 | ~isequal(Line(1:2),'/*')
    fclose(fid);
    return
end
fgetnel(fid); % * TIME IN COLUMN 1, CROSS SECTIONS IN COLUMN 2 - N

FileInfo.NumCross=0;
Line=fgetnel(fid);
while length(Line)>2 & strcmp(Line(2),'*')
    FileInfo.NumCross=FileInfo.NumCross+1;
    X=sscanf(strrep(Line((strfind(Line,'M1N1M2N2')+8):end),',',' '),'%f',4);
    if length(X)==4
        FileInfo.M(FileInfo.NumCross,1:2)=X([1 3]);
        FileInfo.N(FileInfo.NumCross,1:2)=X([2 4]);
    else
        FileInfo.M(FileInfo.NumCross,1)=X;
        Line=fgetnel(fid);
        X=sscanf(Line,'%f',3);
        FileInfo.M(FileInfo.NumCross,2)=X(2);
        FileInfo.N(FileInfo.NumCross,1:2)=X([1 3]);
    end
    FileInfo.DataStart=ftell(fid);
    Line=fgetnel(fid);
end

ntim=1;
while ~feof(fid)
    fgetnel(fid);
    ntim=ntim+1;
end
FileInfo.NumTimes=ntim;

fclose(fid);
FileInfo.Check='OK';

function Data=Local_read_cross(FileInfo,CrossNr,TIndex)
Data=[];
if nargin<2 % no cross section
    return
elseif nargin<3
    TIndex=0;
end
if strcmp(CrossNr,'T')
    CrossNr=0;
end

if isfield(FileInfo,'Cross') % fls fileinfo, lower to cross fileinfo
    FileInfo=FileInfo.Cross.FileInfo;
end

fid=fopen(FileInfo.FileName,'r','n','US-ASCII');
if fid<0
    return
end
fseek(fid,FileInfo.DataStart,-1);
Data=fscanf(fid,'%f',[FileInfo.NumCross+1 inf]);
fclose(fid);
if (TIndex==0)
    Data=Data(CrossNr+1,:);
else
    Data=Data(CrossNr+1,TIndex);
end

function Line=fgetnel(Fid)
% get the next non-empty line from the Input
Line='';
while ~feof(Fid) & isempty(Line)
    Line=fgetl(Fid);
end

function FileInfo=Local_open_historyhis(filename)
% open history file
FileInfo.Check='NotOK';
FileInfo.FileType='FLS-his';

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    return
end

FileInfo.FileName=filename;

Line=fgetl(fid); % * STATION NR, M, N     :

while ~feof(fid) & ~isempty(Line)
    Colon=strfind(Line,':');
    X=sscanf(Line((Colon+1):end),'%i',3);
    Station=X(1);
    FileInfo.M(Station)=X(2);
    FileInfo.N(Station)=X(3);

    Line=fgetl(fid); % * STATION BOTTOM HEIGHT:     4.70
    Colon=strfind(Line,':');
    X=sscanf(Line((Colon+1):end),'%f',1);
    FileInfo.ZB(Station)=X(1);

    Line=fgetl(fid); % *   MAX, MIN, AVE, VAR : WATERDEPTH    (H):    1.00    0.00    0.36    0.41
    Colon=strfind(Line,':');
    X=sscanf(Line((Colon(2)+1):end),'%f',4);
    FileInfo.H(1:4,Station)=X;

    Line=fgetl(fid); % *   MAX, MIN, AVE, VAR : FLOW VELOCITY (C):    0.40    0.00    0.15    0.17
    Colon=strfind(Line,':');
    X=sscanf(Line((Colon(2)+1):end),'%f',4);
    FileInfo.C(1:4,Station)=X;

    Line=fgetl(fid); % *   MAX, MIN, AVE, VAR : WATERLEVEL    (Z):    5.70    4.70    5.06    0.41
    Colon=strfind(Line,':');
    X=sscanf(Line((Colon(2)+1):end),'%f',4);
    FileInfo.Z(1:4,Station)=X;

    Line=fgetl(fid); % *   MAX, MIN, AVE, VAR : U-VELOCITY    (U):    0.00   -0.31   -0.12    0.13
    Colon=strfind(Line,':');
    X=sscanf(Line((Colon(2)+1):end),'%f',4);
    FileInfo.U(1:4,Station)=X;

    Line=fgetl(fid); % *   MAX, MIN, AVE, VAR : V-VELOCITY    (V):    0.02   -0.25   -0.10    0.11
    Colon=strfind(Line,':');
    X=sscanf(Line((Colon(2)+1):end),'%f',4);
    FileInfo.V(1:4,Station)=X;

    fgetl(fid); % *   TIME       H       C       Z       U       V
    fgetl(fid); % S  1

    Line=fgetl(fid); % size: Length, Width
    X=sscanf(Line,'%i',[1 2]);
    FileInfo.NTimes(Station)=X(1);
    FileInfo.Start(Station)=ftell(fid);

    fscanf(fid,'%f',X); % data
    fgetl(fid); % end-of-last-data-line

    Line=fgetl(fid); % * STATION NR, M, N
end
FileInfo.NumSta=length(FileInfo.M);
fclose(fid);
FileInfo.Check='OK';

function Data=Local_read_his(FileInfo,Station)
Data=[];
if nargin<2, % no Station
    return
end

if isfield(FileInfo,'His') % fls fileinfo, lower to his fileinfo
    FileInfo=FileInfo.His.FileInfo;
end

fid=fopen(FileInfo.FileName,'r','n','US-ASCII');
fseek(fid,FileInfo.Start(Station),-1);
Data=fscanf(fid,'%f',[6 FileInfo.NTimes(Station)]);
Data=transpose(Data);
fclose(fid);
