function Out = mike(cmd,varargin)
%MIKE Read/write DHI Mike files.
%   FILEINFO = MIKE('open',FILENAME) opens a DHI Mike DFS? file or a pair
%   of CT?/DT? files.
%
%   DATA = MIKE('read',FILEINFO,ITEM,TIMESTEP) reads data for the selected
%   ITEM from a Mike file. If no TIMESTEP is specified then the last
%   timestep for the ITEM in the file is returned. If the data files
%   contains just one dataset, the ITEM number is not required.
%
%   DATA = MIKE('read',FILEINFO,ITEM,-1) read the grid from the data file.
%
%   DATA = MIKE(...,SELECTION) where SELECTION equals {M} for 1D, {M N} for
%   2D and {M N K} for 3D returns only the selected m,n,k-indices. The
%   number of indices should match the dimension of the data file.
%
%   See also QPFOPEN, QPREAD.

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

if nargin == 0
    if nargout > 0
        Out = [];
    end
    return
end

switch cmd
    case 'open'
        Out = Local_open_mike(varargin{:});
    case 'read'
        switch lower(varargin{1}.FileType)
            case 'mikectdt'
                Out = Local_read_mike(varargin{:});
            case 'mikedfs'
                Out = Local_read_mike_new(varargin{:});
        end
    otherwise
        error('unknown command: %s',var2str(cmd))
end

function S = Local_open_mike(filename,varargin)
S.Check = 'NotOK';
logid = 0;
if nargin>1
    for i = 1:length(varargin)
        if strcmpi(varargin{i},'debug')
            logid = 1;
        end
    end
end
        

if nargin == 0
    [fn,fp] = uigetfile('*.ct?');
    if ~ischar(fn)
        return
    end
    filename = [fp fn];
end

% Filename: has it an extension?
lastdot = max(strfind(filename,'.'));
lastsep = max(strfind(filename,filesep));
if ~isempty(lastdot) && (isempty(lastsep) || (lastdot > lastsep)) % has extension!
    file_ext = filename(lastdot:end);
    filename = filename(1:(lastdot-1));
else
    error('Missing extension? Could not determine filetype.')
end

S.FileName = filename;
S.FileType = 'MikeCTDT';
S.Format = 'l';
switch file_ext
    case {'.ct2','.dt2'}
        S.Def = '.ct2';
        S.Dat = '.dt2';
    case {'.ct1','.dt1'}
        S.Def = '.ct1';
        S.Dat = '.dt1';
    case {'.ct0','.dt0'}
        S.Def = '.ct0';
        S.Dat = '.dt0';
    otherwise
        fid = fopen([filename file_ext],'r');
        str = fread(fid,20,'*char');
        fclose(fid);
        typ = sscanf(str,'DHI_%[^_]',1);
        if ~isempty(typ)
            S.FileType = ['Mike' typ];
            S.Dat = file_ext;
            S.Def = '';
        else
            error('Invalid filename or unknown MIKE file format.')
        end
end

S.FileName = absfullfile(filename);

if isempty(S.Def)
    S = Local_open_mike_new(S,filename,logid);
    return
end

dat_file = [filename S.Dat];
def_file = [filename S.Def];


% -----------------------------------------------------
% Reading the dat and def files
% -----------------------------------------------------

% Check existence of dat file
if ~exist(dat_file, 'file')
    error('Datafile "%s" does not exist.',dat_file)
end

% Start reading the def file
fidef = fopen(def_file,'r','l');
if fidef < 0
    error('Cannot open definition file: %s.',def_file)
end

BS = fread(fidef,1,'int32'); % 600
if ~isequal(BS,600)
    fclose(fidef);
    fidef = fopen(def_file,'r','b');
    S.Format = 'b';
    BS = fread(fidef,1,'int32'); % 600
    if ~isequal(BS,600)
        S.Format = 'unknown';
        fclose(fidef);
        return
    end
end
X = fread(fidef,[1 600/4],'float32');
BS = fread(fidef,1,'int32'); % 600

S.RefDate = X(1)+694020+X(5)/(24*3600);
S.StartTimeStep = X(2);
S.NumTimeSteps = X(4);
%if S.NumTimeSteps > 0,
S.TimeStep = X(6)/(60*60*24);
%end
S.NumItems = X(7);
%S.DataDim = X(20:21);
%S.Origin = X(24:25); % latitude, longitude in degrees
%S.GridCell = X(28:29); % in meters
S.NumCoords = X(8); % 0 t/m 4
if S.NumCoords == 0
    S.DataDim = 0; % 1 added while reading!
else
    S.DataDim = X(19+(1:S.NumCoords)); % 1 added while reading!
    S.Origin = X(23+(1:S.NumCoords));
    S.GridCell = X(27+(1:S.NumCoords));
    if S.NumCoords == 2
        S.Orientation = X(144); % in degrees
        S.Land = X(147);
    end
end

S.Item = [];
for i = 1:S.NumItems
    Min = X(31+7*(i-1)+1);
    Max = X(31+7*(i-1)+2);
    %if Min > Max, break; end
    S.Item(i).Min = Min;
    S.Item(i).Max = Max;
    S.Item(i).Mean = X(31+7*(i-1)+3);
    %  S.Item(i).X = X(31+7*(i-1)+4);
end
%S.NumItems = length(S.Item)
S.DataField(1).Data = X;

BS = fread(fidef,1,'int32'); % 436
X = fread(fidef,[1 436],'*char');
BS = fread(fidef,1,'int32'); % 436

S.Description = deblank(X(1:40));
if any(S.Description == 0)
    S.Description = S.Description(1:min(find(S.Description == 0))-1);
end
S.DataField(1).Description = X;
fclose(fidef);

for i = 1:S.NumItems
    S.Item(i).Name = X(96+20*(i-1)+(1:20));
    if any(S.Item(i).Name == 0)
        S.Item(i).Name = S.Item(i).Name(1:min(find(S.Item(i).Name == 0))-1);
    end
end

S.Check = 'OK';

function S = Local_open_mike_new(Sin,filename,logid)
S = Sin;
file = [filename S.Dat];

% Check existence of file
if ~exist(file, 'file')
    error('File "%s" does not exist.',file)
end

% Start reading the file
fid = fopen(file,'r','l');
if fid < 0
    error('Cannot open file: %s.',file)
end

X = fread(fid,[1 64],'*char');
if ~strcmp(X,['DHI_' S.FileType(5:7) '_ MIKE Zero - this file contains binary data, do not edit'])
    error('Invalid start of MIKE Zero file.')
end

if strcmp(S.FileType,'MikeDFS')
    X = fread(fid,[1 17],'*char'); %  FOpenFileCreate
end

X = fread(fid,1,'uchar'); %<end of text>

V = fread(fid,[1 6],'int16'); % FileCreationDate
S.Date = datenum(V(1),V(2),V(3),V(4),V(5),V(6));

X = fread(fid,2,'int32'); %104, 206

V = fread(fid,[1 6],'int16'); % FileCreationDate

X = fread(fid,4,'int32'); %104, 206, 0, 0

S.Data = {};
switch lower(S.Dat)
    case {'.dfs0','.dfs1','.dfs2','.dfs3'}
        n = lower(S.Dat(end));
        S.NumCoords = n-'0';
        S.DataType = 'structured';
    case {'.dfsu'}
        S.NumCoords = 'u';
        S.DataType = 'unstructured';
    case { '.xns11'}
        S.NumCoords = 'x';
        S.DataType = 'crosssections';
    otherwise
        S.NumCoords = 0;
        S.DataType = 'unknown';
end

S.SparseStorage = 0;
S = read_info(fid,S,0,logid,'');
if strcmp(S.DataType,'unstructured')
    fm = strmatch('MIKE_FM',S.Attrib.Name,'exact');
    S.NumCoords = S.Attrib(fm).Data(3);
    S.NumLayers = max(1,S.Attrib(fm).Data(4));
    if S.NumCoords == 3
        S.NumNodes = S.Attrib(fm).Data(1)/(S.NumLayers+1);
    else
        S.NumNodes = S.Attrib(fm).Data(1);
    end
    S.NumCells = S.Attrib(fm).Data(2)/max(1,S.NumLayers);
end
fclose(fid);


function Info = read_info(fid,Info,loaddata,logid,indent)
it = 0;
att = 0;
crs = 0;
static = 0;
fld = 0;
while 1
    Typ = fread(fid,1,'uchar');
    if isempty(Typ) % End of File
        break
    end
    switch Typ
        case 1
            % data block floating point single precision
            N = fread(fid,1,'uint32');
            logger(logid,indent,'%d single precision numbers: ',N)
            if loaddata == 1
                data = fread(fid,[1 N],'float32');
                arraylogger(logid,data)
                Info.Data{end+1} = data;
            else
                it = it+1;
                while Info.Item(it).Static
                    it = it+1;
                end
                Info.Item(it).Data(fld) = ftell(fid);
                data = fread(fid,[1 N],'float32');
                arraylogger(logid,data)
            end
        case 2
            % data block floating point double precision
            N = fread(fid,1,'uint32');
            logger(logid,indent,'%d double precision numbers: ',N)
            data = fread(fid,[1 N],'float64');
            arraylogger(logid,data)
            Info.Data{end+1} = data;
        case 3
            % data block characters / string
            N = fread(fid,1,'uint32');
            logger(logid,indent,'%d characters: ',N)
            str = fread(fid,[1 N],'*char');
            logger(logid,indent,'"%s"\n',str)
            Info.Data{end+1} = str;
        case 4
            % data block signed 4-byte integers
            N = fread(fid,1,'uint32');
            logger(logid,indent,'%d signed 4-byte integers: ',N)
            data = fread(fid,[1 N],'int32');
            arraylogger(logid,data)
            Info.Data{end+1} = data;
        case 5
            % data block unsigned 4-byte integers
            N = fread(fid,1,'uint32');
            logger(logid,indent,'%d unsigned 4-byte integers: ',N)
            data = fread(fid,[1 N],'uint32');
            arraylogger(logid,data)
            Info.Data{end+1} = data;
        case 6
            % data block signed 2-byte integers
            N = fread(fid,1,'uint32');
            logger(logid,indent,'%d signed 2-byte integers: ',N)
            data = fread(fid,[1 N],'int16');
            arraylogger(logid,data)
            Info.Data{end+1} = data;
        case 7
            % data block unsigned 2-byte integers
            N = fread(fid,1,'uint32');
            logger(logid,indent,'%d unsigned 2-byte integers: ',N)
            data = fread(fid,[1 N],'uint16');
            arraylogger(logid,data)
            Info.Data{end+1} = data;
        case 254
            % meta data block
            Opt = fread(fid,1,'uchar');
            X = fread(fid,1,'uchar');
            i2.Data = {};
            logger(logid,indent,'\n')
            logger(logid,indent,'--> Block %d with X = %d\n',Opt,X)
            i2 = read_info(fid,i2,1,logid,[indent '  ']);
            switch Opt
                case 1
                    % cross section information
                    % 'JOTIJA'    [         0]    'CURRENT'    '3'    
                    crs = crs+1;
                    Info.CrossSection.Name{crs}    = deblank(i2.Data{4});
                    Info.CrossSection.Branch{crs}  = deblank(i2.Data{1});
                    Info.CrossSection.Offset(crs)  = i2.Data{2};
                    Info.CrossSection.Version{crs} = deblank(i2.Data{3});
                case 2
                    % cross section flags
                    % 25 int32  : 0 2 1 ? ? 0 0 0 0 1 50 0 0 0 0 0 ? 0 0 0 0 0 0 0 0
                    %                   1 2            3           4
                    % 1: unknown range 4-17
                    % 2: NPnts in cross section (size in block 3)
                    % 3: NLevels in tables (size in block 5)
                    % 4: 2 (if block 6 present)
                    % 23 float64: 0 0 0 0 0 ? ? ? ? 0 1 1 1 1 1 0.001 1 1 1 1 1 1 1
                    Info.CrossSection.Integers(crs,:) = i2.Data{1};
                    Info.CrossSection.Reals(crs,:)    = i2.Data{2};
                    Info.CrossSection.NPnts(crs)      = i2.Data{1}(5);
                case 3
                    % cross section coordinates
                    % 6 * NPnts float64
                    % Y, Z, 1, 0, 0, 0
                    Info.CrossSection.YZ{crs} = cat(1,i2.Data{1:2});
                    %Info.CrossSection.ones(crs,:) = i2.Data{3};
                    %Info.CrossSection.zeros(crs,:) = i2.Data{4/5/6};
                case 5
                    % cross section elevation, area, radius, perimeter
                    % 6 * 50 float64
                    % Z, Area(Z), Hydraulic Radius(Z), Wetted Perimeter(Z), 0, 1
                    Info.CrossSection.ZARP{crs} = cat(1,i2.Data{1:4});
                case 6
                    % cross section georeferenced coordinates
                    % N * 
                    Info.CrossSection.GeoLine{crs} = cat(1,i2.Data{:});
                case 16
                    % X = 39  {}
                case 17
                    % File title; user defined title of the file.
                    % X = 39
                    Info.FileTitle = deblank(i2.Data{1});
                case 18
                    % Application title; title of the application that created the file.
                    % X = 39
                    Info.CreatingProgram = deblank(i2.Data{1});
                case 19
                    % Application version number; version of the application that created the file.
                    % X = 39
                    %  [0], [1], [100]
                case 20
                    % Data type; used to tag the file as a special DFS file type, see (1) below.
                    %
                    % The data type tag is a user specified integer. The
                    % data type tag is used as an identification tag for
                    % the type of DFS file at hand. The user should tag
                    % bathymetries, result files, input files etc. matching
                    % those tags required for the DFS file type at hand. A
                    % wrong data type tag in some contexts is an error. Not
                    % all DFS files and tools handling DFS use the tag.
                    %
                    % In Appendix G there is a list of data type tags
                    % currently used within the DHI model complex. A
                    % programmer writing a new type of DFS file should
                    % choose the tag carefully, such that it does not
                    % interfere with existing DFS file types.
                    % X = 39
                    %  [1]
                case 21
                    % Type of DFS file storing format, see (2) below.
                    %
                    % The storing format specifies whether all items are
                    % stored in all time steps, and if a time-varying
                    % spatial axes is used. Currently the DFS file system
                    % only supports files with all items in all time steps
                    % and not any of the time-varying spatial axes. Thus
                    % this is not used, and is not presently planned to be
                    % explored.
                    % X = 39
                    %  [1], [2] --> It seems 2 correlates with the occurence of a 51/117 record
                case 22
                    % Delete values, see (3) below.
                    %
                    % A delete value represents a not-defined value. If a
                    % value is not available, or if it does not make sense
                    % to set a value, setting the delete value indicates
                    % that there is no value. There is a delete value for
                    % data of type float, double, integer (32 bit),
                    % unsigned integer and byte (8 bit).
                    % X = 39
                    Info.Clipping = i2.Data{1};
                case 23
                    % X = 39
                    %  [-1.0000e-030], [-1.0000e-077], [-1.0000e-255]
                case 24
                    % X = 39
                    %  ' '
                case 25
                    % minimum value?
                    % X = 39
                    %  [-9876789], [2.1475e+009]
                case 26
                    % maximum value?
                    % X = 39
                    %  [9876789], [2.1475e+009]
                case 27
                    % Geographic map projection information.
                    % X = 39
                    Info.ProjectionName = deblank(i2.Data{1});
                    Info.ProjectionData = i2.Data{2};
                case 28
                    % X = 39
                    Info.NumItems = i2.Data{1}+1;
                case 29
                    % X = 39
                    Info.SparseStorage = i2.Data{1};
                case 32
                    % X = 78  {}
                case 48
                    % X = 117 {}
                case 49
                    % number of time dependent quantities
                    Info.NumTimDepItems = i2.Data{1};
                case 51
                    % per item
                    % X = 117: {[10.0085 10 3.6116e+003 3.6132e+004 3.6032e+004 10.0084 361 360 0]}
                    % X = 117: {[4611 -12364.1 -121720 1.59038e+09 1.52705e+09 -6156.77 21 20 0]}                    
                    % 10.0085 10 = Max & Min of Item
                    % 10.0084 = last value
                    % 361 = NumTimeSteps
                    %Info.Item(it).NumTimeSteps = i2.Data{1}(7);
                    %fprintf('%g %g %g %g %g %g %g %g %g\n',i2.Data{1});
                case 53
                    % quantity description
                    %  1          2                                 3      4    5                                 6        7
                    % {[100000]  'P(25,10): H Water Depth m '      [1000] [1]  [10.0085 10.0000 0]               [0 0 0]  [0 0 0]}
                    % {[999]     'P(0,10)-P(50,10): Component 1 '  [0]    [1]  [4.5768 -2.3594e-005 1083]        [0 0 0]  [0 0 0]}
                    % {[100000]  'naam '                           [1000] [1]  [-1.0000e-255 -1.0000e-255 30000] [0 0 0]  [0 0 0]}
                    % {[999]     'Static item '                    [0]    [1]  [0 0 0]                           [0 0 0]  [0 0 0]}
                    % {[999]     'Dummy '                          [0]    [1]  [0 0 1589]                        [0 0 0]  [0 0 0]}
                    % {[999]     'E-coli '                         [0]    [1]  [0 0 1589]                        [0 0 0]  [0 0 0]}
                    it = it+1;
                    Info.Item(it).Name = deblank(i2.Data{2});
                    Info.Item(it).Static = static;
                    Info.Item(it).EUMTypeNr = i2.Data{1};
                    Info.Item(it).EUMTypeStr = EUMType(i2.Data{1});
                    Info.Item(it).EUMUnitNr = i2.Data{3};
                    [Info.Item(it).EUMUnitStr, Info.Item(it).UnitStr] = EUMUnit(i2.Data{3});
                    Info.Item(it).Max = i2.Data{5}(1);
                    Info.Item(it).Min = i2.Data{5}(2);
                    Info.Item(it).NumClip = i2.Data{5}(3);
                    if ~static
                        Info.Item(it).Data = repmat(-1,1,Info.NumTimeSteps);
                    end
                    Info.Item(it).MatrixSize = 1;
                    Info.Item(it).CellSize = 0;
                case 54
                    % X = 117
                    % per item
                    % [0], [1]
                    %fprintf('54/117 -> %i\n',i2.Data{1});
                case 64
                    % X+156
                    static = 1;
                case 65
                    % per item
                    % X = 156
                    sz = [Info.Item(it).MatrixSize 1];
                    Info.Item(it).Data = reshape(i2.Data{1},sz);
                case 75
                    % per item
                case {76,79,82}
                    % per item
                    % X = 121
                    % 0: -
                    % 1: 76: {[1000 51]  [0 100]}
                    % 2: 79: {[1000 44 78]  [0 0 150 150]}
                    % 3: 82: {[1000 10 20 5]  [0 0 0 10 10 1]}
                    NDim = length(i2.Data{1})-1;
                    Info.Item(it).MatrixSize = i2.Data{1}(2:end);
                    %Info.Item(it).Offset = i2.Data{2}(1:NDim);
                    Info.Item(it).CellSize = i2.Data{2}((NDim+1):end);
                case 80
                    % start time dependent data
                    % X = 195
                    % {}
                    it = 0;
                case 81
                    % markers between time dependent datasets - time step
                    % X = 195
                    % {} : EQuidistant times
                    % {0} : Non-EQuidistant times
                    % {2280}
                    % {4200}
                    fld = fld+1;
                    if strcmp(Info.TimeType,'NEQ')
                        Info.Times(fld) = i2.Data{1} * Info.TimeUnit;
                    end
                    it = 0;
                case {85,86} % reference date, so type: calendar
                    % X = 78
                    % {'1990-01-01 '  '12:00:00 '  [1400]  [0 20]  [361 0]}
                    % {'1990-01-01 '  '12:00:00 '  [1400]  [0 20]  [361 0]}
                    % {'1990-01-01 '  '12:00:00 '  [1400]  [0 1800]  [1 0]}
                    % {'1990-01-01 '  '00:00:00 '  [1400]  [0 30]  [30 0]}
                    % {'2009-10-13 '  '07:02:00 '  [1400]  [0 39840]  [21 0]}
                    V = sscanf([i2.Data{1}(1:end-1),' ',i2.Data{2}],'%d-%d-%d %d:%d:%d');
                    Info.RefDate = datenum(V(1),V(2),V(3),V(4),V(5),V(6));
                    switch EUMTimeUnit(i2.Data{3})
                        case 'SECOND' % 1400
                            Info.TimeUnit = 1/(3600*24);
                        case 'MINUTE' % 1401
                            Info.TimeUnit = 1/(60*24);
                        case 'HOUR' % 1402
                            Info.TimeUnit = 1/24;
                        case 'DAY' % 1403
                            Info.TimeUnit = 1;
                        case 'MONTH' % 1405
                            error('MONTH unit not yet implemented!')
                        case 'YEAR' % 1404
                            error('YEAR unit not yet implemented!')
                    end
                    Info.NumTimeSteps = i2.Data{5}(1);
                    switch Opt
                        case 85
                            Info.TimeType = 'EQ'; % equidistant
                            Info.TimeOffset = i2.Data{4}(1) * Info.TimeUnit; % assumption ...
                            Info.TimeStep = i2.Data{4}(2) * Info.TimeUnit;
                            Info.Times = Info.TimeOffset + (0:Info.NumTimeSteps-1) * Info.TimeStep;
                        case 86
                            Info.TimeType = 'NEQ'; % non-equidistant
                            % i2.Data{4}(1) first time
                            % i2.Data{4}(2) last time
                            Info.Times = zeros(1,Info.NumTimeSteps);
                    end
                case 96
                    % X = 234
                    % {'M21_Misc '  [327.8968 0.2000 -900 5 71 93 40]}
                    % 327.8968 = ProjectionData(3)
                    %
                    % {'MIKE_FM '  [3192 4578 3 7]}
                    att = att+1;
                    Info.Attrib(att).Name = deblank(i2.Data{1});
                    Info.Attrib(att).Data = i2.Data{2};
                otherwise
                    % Opt = 75, X = 121: {[0]}
                    Fld = sprintf('Fld%i_%i',Opt,X);
                    if isfield(Info,Fld)
                        Fld1 = Fld;
                        i = 1;
                        while isfield(Info,Fld1)
                            i = i+1;
                            Fld1 = sprintf('%s_%i',Fld,i);
                        end
                        Fld = Fld1;
                    end
                    %fprintf('%s\nGeneral Field: %s\n\n',fopen(fid),Fld);
                    Info = setfield(Info,Fld,i2);
            end
        case 255
            logger(logid,indent(1:end-2),'<--\n')
            return
        otherwise
            error('Unknown type %i',Typ)
    end
end
Info.Check = 'OK';


function Data = Local_read_mike(S,varargin)
Data = [];

IN = varargin;
if ~isempty(IN) && iscell(IN{end})
    subscr = IN{end};
    if length(subscr) ~= S.NumCoords,
        error('Invalid number of indices.');
    end
    IN = IN(1:end-1);
else
    subscr = {};
end

switch length(IN)
    case 0
        if S.NumItems == 1
            Item = 1;
            TimeStep = max(1,S.NumTimeSteps);
        else
            error('No item specified.')
        end
    case 1 % TimeStep or Item
        if S.NumItems == 1
            Item = 1;
            TimeStep = IN{1};
        else
            Item = IN{1};
            if ischar(Item)
                Item = ustrcmpi(Item,{S.Item.Name});
                if isempty(Item)
                    error('Invalid item name')
                end
            end
            TimeStep = max(1,S.NumTimeSteps);
        end
    case 2
        Item = IN{1};
        if ischar(Item)
            Item = ustrcmpi(Item,{S.Item.Name});
            if isempty(Item)
                error('Invalid item name')
            end
        end
        TimeStep = IN{2};
    otherwise
        error('Too many input arguments.')
end

if isempty(subscr)
    Size = S.DataDim+1;
else
    Size = zeros(1,S.NumCoords);
    for i = 1:S.NumCoords
        if isequal(subscr{i},0)
            subscr{i} = 1:(S.DataDim(i)+1);
            Size(i) = S.DataDim(i)+1;
        else
            Size(i) = length(subscr{i});
        end
    end
end

if isequal(TimeStep,-1)
    if S.DataDim == 0 % 0D
        Data = [];
    else
        sz = S.DataDim+1;
        switch length(sz)
            case 1
                Data.X = (0:sz(1))*S.GridCell(1);
                if ~isempty(subscr)
                    Data.X = Data.X(subscr{:});
                end
            case 2
                Data.X = repmat(transpose(0:sz(1))*S.GridCell(1), ...
                    1,sz(2)+1);
                Data.Y = repmat((0:sz(2))*S.GridCell(2), ...
                    sz(1)+1,1);
                if ~isempty(subscr)
                    Data.X = Data.X(subscr{:});
                end
                if ~isempty(subscr)
                    Data.Y = Data.Y(subscr{:});
                end
            case 3
                Data.X = repmat(transpose(0:sz(1))*S.GridCell(1), ...
                    [1,sz(2)+1,sz(3)+1]);
                Data.Y = repmat((0:sz(2))*S.GridCell(2), ...
                    [sz(1)+1,1,sz(3)+1]);
                Data.Z = repmat(reshape((0:sz(3))*S.GridCell(3), ...
                    [1 1 sz(3)+1]), ...
                    [sz(1)+1,sz(2)+1,1]);
                if ~isempty(subscr)
                    Data.X = Data.X(subscr{:});
                end
                if ~isempty(subscr)
                    Data.Y = Data.Y(subscr{:});
                end
                if ~isempty(subscr)
                    Data.Z = Data.Z(subscr{:});
                end
        end
    end
    return
end

dat_file = [S.FileName S.Dat];
fidat = fopen(dat_file,'r',S.Format);
if isequal(TimeStep,0)
    TimeStep = 1:max(1,S.NumTimeSteps);
end
if length(TimeStep) ~= 1
    Data = zeros([length(TimeStep) Size]);
    for i = 1:length(TimeStep)
        fseek(fidat,(S.NumItems*(TimeStep(i)-1)+Item-1)*prod(S.DataDim+1)*4,-1);
        Tmp = fread(fidat,[1 prod(S.DataDim+1)],'float32');
        if ~isempty(subscr)
            Tmp = Tmp(1,subscr{:});
        end
        Data(i,:) = Tmp;
    end
else
    fseek(fidat,(S.NumItems*(TimeStep-1)+Item-1)*prod(S.DataDim+1)*4,-1);
    Data = fread(fidat,[1 prod(S.DataDim+1)],'float32');
    Data = reshape(Data,[S.DataDim+1 1]);
    if ~isempty(subscr)
        Data = Data(subscr{:});
    end
end

fclose(fidat);


function Data = Local_read_mike_new(S,varargin)
Data = [];

IN = varargin;
if iscell(IN{end})
    subscr = IN{end};
    if length(subscr) ~= S.NumCoords
        error('Invalid number of indices.')
    end
    IN = IN(1:end-1);
else
    subscr = {};
end

switch length(IN)
    case 0
        if S.NumItems == 1
            Item = 1;
            Info = S.Item(Item);
            if Info.Static
                TimeStep = 1;
            else
                TimeStep = S.NumTimeSteps;
                if TimeStep == 0 % Doesn't mean all timesteps. Well, it would give the same result since there are none.
                    TimeStep = [];
                end
            end
        else
            error('No item specified.')
        end
    case 1 % TimeStep or Item
        if S.NumItems == 1
            Item = 1;
            Info = S.Item(Item);
            TimeStep = IN{1};
        else
            Item = IN{1};
            if ischar(Item)
                Item = ustrcmpi(Item,{S.Item.Name});
                if isempty(Item)
                    error('Invalid item name')
                end
            end
            Info = S.Item(Item);
            if Info.Static
                TimeStep = 1;
            else
                TimeStep = S.NumTimeSteps;
                if TimeStep == 0 % Doesn't mean all timesteps. Well, it would give the same result since there are none.
                    TimeStep = [];
                end
            end
        end
    case 2
        Item = IN{1};
        if ischar(Item)
            Item = ustrcmpi(Item,{S.Item.Name});
            if isempty(Item)
                error('Invalid item name')
            end
        end
        Info = S.Item(Item);
        TimeStep = IN{2};
    otherwise
        error('Too many input arguments.')
end

if isempty(subscr)
    Size = Info.MatrixSize;
else
    Size = zeros(1,S.NumCoords);
    for i = 1:S.NumCoords
        if isequal(subscr{i},0)
            subscr{i} = 1:Info.MatrixSize(i);
            Size(i) = Info.MatrixSize(i);
        else
            Size(i) = length(subscr{i});
        end
    end
end

if isequal(TimeStep,-1)
    if Info.CellSize(1) == 0 % 0D
        Data = [];
    else
        switch length(Info.MatrixSize)
            case 1
                Data.X = (0:Info.MatrixSize(1))*Info.CellSize(1);
                if ~isempty(subscr)
                    Data.X = Data.X(subscr{:});
                end
            case 2
                Data.X = repmat(transpose(0:Info.MatrixSize(1))*Info.CellSize(1), ...
                    1,Info.MatrixSize(2)+1);
                Data.Y = repmat((0:Info.MatrixSize(2))*Info.CellSize(2), ...
                    Info.MatrixSize(1)+1,1);
                if ~isempty(subscr)
                    Data.X = Data.X(subscr{:});
                end
                if ~isempty(subscr)
                    Data.Y = Data.Y(subscr{:});
                end
            case 3
                Data.X = repmat(transpose(0:Info.MatrixSize(1))*Info.CellSize(1), ...
                    [1,Info.MatrixSize(2)+1,Info.MatrixSize(3)+1]);
                Data.Y = repmat((0:Info.MatrixSize(2))*Info.CellSize(2), ...
                    [Info.MatrixSize(1)+1,1,Info.MatrixSize(3)+1]);
                Data.Z = repmat(reshape((0:Info.MatrixSize(3))*Info.CellSize(3), ...
                    [1 1 Info.MatrixSize(3)+1]), ...
                    [Info.MatrixSize(1)+1,Info.MatrixSize(2)+1,1]);
                if ~isempty(subscr)
                    Data.X = Data.X(subscr{:});
                end
                if ~isempty(subscr)
                    Data.Y = Data.Y(subscr{:});
                end
                if ~isempty(subscr)
                    Data.Z = Data.Z(subscr{:});
                end
        end
    end
    return
end

if S.SparseStorage
    ix = strmatch('X-Encoding Key',{S.Item.Name},'exact');
    IX = S.Item(ix).Data;
    iy = strmatch('Y-Encoding Key',{S.Item.Name},'exact');
    IY = S.Item(iy).Data;
    iz = strmatch('Z-Encoding Key',{S.Item.Name},'exact');
    IZ = S.Item(iz).Data;
end
if Info.Static
    Data = S.Item(Item).Data;
    if ~isempty(subscr)
        Data = Data(subscr{:});
    end
else
    dat_file = [S.FileName S.Dat];
    fidat = fopen(dat_file,'r',S.Format);
    if isequal(TimeStep,0)
        TimeStep = 1:S.NumTimeSteps;
    end
    if length(TimeStep) ~= 1
        Data = zeros([length(TimeStep) Size]);
        for i = 1:length(TimeStep)
            fseek(fidat,Info.Data(TimeStep(i)),-1);
            Tmp = fread(fidat,[1 prod(Info.MatrixSize)],'float32');
            if ~isempty(subscr)
                Tmp = Tmp(1,subscr{:});
            end
            Data(i,:) = Tmp;
        end
    else
        fseek(fidat,Info.Data(TimeStep),-1);
        if S.SparseStorage
            Vals = fread(fidat,length(IX),'float32');
            Data = repmat(NaN,[Info.MatrixSize 1]);
            Data(sub2ind(size(Data),IX+1,IY+1,IZ+1)) = Vals;
        else
            Data = fread(fidat,prod(Info.MatrixSize),'float32');
            Data = reshape(Data,[Info.MatrixSize 1]);
        end
        if ~isempty(subscr)
            Data = Data(subscr{:});
        end
    end
    fclose(fidat);
end
Data(Data == S.Clipping) = NaN;


function logger(logid,indent,varargin)
if logid > 0
    fprintf(logid,[indent varargin{1}],varargin{2:end});
end


function arraylogger(logid,data)
if logid>0
    str = sprintf('%g ',data(1:min(length(data),10)));
    if length(data) > 10
        fprintf(logid,'%s...\n',str);
    else
        fprintf('%s\n', str);
    end
end


function Types = EUMType(key)
Types = {100000	'Water_Level'
100001	'Discharge'
100002	'Wind_Velocity'
100003	'Wind_Direction'
100004	'Rainfall'
100005	'Evaporation'
100006	'Temperature'
100007	'Concentration'
100008	'Bacteria_Concentration'
100009	'Resistance_factor'
100010	'Sediment_Transport'
100011	'Bottom_level'
100012	'Bottom_level_change'
100013	'Sediment_fraction'
100014	'Sediment_fraction_change'
100015	'Gate_Level'
100016	'Flow_velocity'
100017	'Density'
100018	'Dam_breach_level'
100019	'Dam_breach_width'
100020	'Dam_breach_slope'
100021	'Sunshine'
100022	'Sun_radiation'
100023	'Relative_humidity'
100024	'Salinity'
100025	'Surface_Slope'
100026	'Flow_Area'
100027	'Flow_Width'
100028	'Hydraulic_Radius'
100029	'Resistance_Radius'
100030	'Mannings_M'
100031	'Mannings_n'
100032	'Chezy_No'
100033	'Conveyance'
100034	'Froude_No'
100035	'Water_Volume'
100036	'Flooded_Area'
100037	'Water_Volume_Error'
100038	'Acc_Water_Volume_Error'
100039	'Component_Mass'
100040	'Component_Mass_Error'
100041	'Acc_Component_Mass_Error'
100042	'Relative_Component_Mass_Error'
100043	'Relative_Acc_Component_Mass_Error'
100044	'Component_Decay'
100045	'Acc_Component_Decay'
100046	'Component_Transport'
100047	'Acc_Component_Transport'
100048	'Component_Disp_Transport'
100049	'Acc_Component_Disp_Transport'
100050	'Component_Conv_Transport'
100051	'Acc_Component_Conv_Transport'
100052	'Acc_Sediment_transport'
100053	'Dune_length'
100054	'Dune_height'
100055	'Bed_sediment_load'
100056	'Suspended_sediment_load'
100057	'Irrigation'
100058	'Relative_moisture_content'
100059	'Ground_water_depth'
100060	'Snow_Water_Content'
100061	'Infiltration'
100062	'Recharge'
100063	'OF1_Flow'
100064	'IF1_Flow'
100065	'CapillaryFlux'
100066	'SurfStorage_OF1'
100067	'SurfStorage_OF0'
100068	'Sediment_layer'
100069	'Bed_level'
100070	'Rainfall_Intensity'
100071	'Production_rate'
100072	'Sediment_mass'
100073	'Primary_production'
100074	'Vol_specific_prod_rate'
100075	'Secchi_depth'
100076	'Acc_Sediment_Mass'
100077	'Sediment_Mass_per_m'
100078	'Surface_Elevation'
100079	'Bathymetry'
100080	'Flow_Flux'
100081	'Bed_sediment_load_per_m'
100082	'Suspended_load_per_m'
100083	'Sediment_transport_per_m'
100084	'Wave_height'
100085	'Wave_period'
100086	'Wave_frequency'
100087	'Potential_evaporation_rate'
100088	'Rainfall_rate'
100089	'Water_Flow'
100090	'Return_Flow_Fraction'
100091	'Linear_Routing_Coefficient'
100092	'Specific_runoff'
100093	'Machine_Efficiency'
100094	'Target_power'
100095	'Wave_direction'
100096	'Accumulated_transport_per_meter'
100097	'Significant_wave_height'
100098	'Critical_Shields_parameter'
100099	'Phib_angle_og_bed_velocity'
100100	'Profile_number'
100101	'Climate_number'
100102	'Spectral_description'
100103	'Spreading_factor'
100104	'Reference_point_number'
100105	'Wind_friction_factor'
100106	'Wave_Disturbance_Coefficient'
100107	'Time_of_first_wave_arrival'
100108	'Surface_Curvature'
100109	'Radiation_Stress'
100120	'Spectral_density'
100121	'Frequency_integrated_spectral_density'
100122	'Directional_integrated_spectral_density'
100123	'Viscosity'
100124	'Standard_deviation_DSD'
100125	'Beach_position'
100126	'Trench_position'
100127	'Grain_diameter'
100128	'Settling_velocity'
100129	'Geometrical_deviation'
100130	'Breaking_wave'
100131	'Dune_position'
100132	'Contour_angle'
100133	'Flow_direction'
100134	'Bed_slope'
100135	'Surface_area'
100136	'Catchment_area'
100137	'Roughness'
100138	'Active_Depth'
100139	'Sediment_Gradation'
100140	'Groundwater_recharge'
100141	'Solute_flux'
100142	'River_structure_geometry'
100143	'River_chainage'
100144	'Dimensionless_factor'
100145	'Dimensionless_exponent'
100146	'Storage_depth'
100147	'River_width'
100148	'Flow_routing_time_constant'
100149	'_1st_order_rate_AD_model'
100150	'_1st_order_rate_WQ_model'
100151	'Erosion_deposition_coefficient'
100152	'Shear_stress'
100153	'Dispersion_coefficient'
100154	'Dispersion_factor'
100155	'Sediment_volume_per_length_unit'
100157	'Latitude_longitude'
100158	'Specific_gravity'
100159	'Transmission_coefficient'
100160	'Reflection_coefficient'
100161	'Friction_factor'
100162	'Radiation_intensity'
100163	'Duration'
100164	'Respiration_production_per_area'
100165	'Respiration_production_per_volume'
100166	'Sediment_depth'
100167	'Angle_of_repose'
100168	'Half_order_rate_WQ_model'
100169	'Rearation_constant'
100170	'Deposition_rate'
100171	'BOD_at_river_bed'
100172	'Crop_demand'
100173	'Irrigated_area'
100174	'Livestock_demand'
100175	'Number_of_livestock'
100176	'Total_Gas'
100177	'Ground_water_abstraction'
100178	'Melting_coefficient'
100179	'Rain_melting_coefficient_per_degree_per_time'
100180	'Elevation'
100181	'Cross_section_X_data'
100182	'Vegetation_height'
100183	'Geographical_coordinate'
100184	'Angle'
100185	'ItemGeometry0D'
100186	'ItemGeometry1D'
100187	'ItemGeometry2D'
100188	'ItemGeometry3D'
100189	'Temperature_lapse_rate'
100190	'Correction_of_precipitation'
100191	'Temperature_correction'
100192	'Precipitation_correction'
100193	'Max_Water'
100194	'Lower_Baseflow'
100195	'Mass_flux'
100196	'Pressure2'
100197	'Turbulent_kinetic_energy'
100198	'Dissipation_of_TKE'
100199	'Salt_Flux'
100200	'Temperature_Flux'
100201	'Concentration_Non_Dim'
100202	'Latent_Heat'
100203	'Heat_Flux'
100204	'Specific_Heat'
100205	'Visibility'
100206	'Ice_thickness'
100207	'Structure_geometry___time'
100208	'Discharge___time'
100209	'Fetch_length'
100210	'Rubble_mound'
100211	'Grid_Spacing'
100212	'TimeStep'
100213	'Length_Scale'
100214	'Erosion_Coefficient_Factor'
100215	'Friction_Coefficient'
100216	'Transition_Rate'
100217	'Distance'
100218	'Time_Correction_At_Noon'
100219	'Critical_Velocity'
100220	'Light_Extinction_Background'
100221	'Particle_Production_Rate'
100222	'First_Order_Grazing_Rate_Dependance'
100223	'Resuspension_Rate'
100224	'Adsorption_Coefficient'
100225	'Desorption_Coefficient'
100226	'Sedimentation_Velocity'
100227	'Boundary_Layer_Thickness'
100228	'Diffusion_Coefficient'
100229	'Bioconcentration_Factor'
100230	'Fcoli_Concentration'
100231	'Specific_Discharge'
100232	'Precipitation'
100233	'Specific_Precipitation'
100234	'Power'
100235	'Conveyance_Loss'
100236	'Infiltration_Flux'
100237	'Evaporation_Flux'
100238	'Ground_Water_Abstraction_Flux'
100239	'Fraction'
100240	'Yield_Factor'
100241	'Specific_Solute_Flux_Per_Area'
100242	'Current_Speed'
100243	'Current_Direction'
100244	'Current_Magnitude'
100245	'First_Order_Piston_Position'
100246	'Subharmonic_Piston_Position'
100247	'Superharmonic_Piston_Position'
100248	'First_Order_Flap_Position'
100249	'Subharmonic_Flap_Position'
100250	'Superharmonic_Flap_Position'
100251	'Length_Zero_Crossing'
100252	'Time_Zero_Crossing'
100253	'Length_Logged_Data'
100254	'Force_Logged_Data'
100255	'Speed_Logged_Data'
100256	'Volume_Flow_Logged_Data'
100257	'_2D_Surface_Elevation_Spectrum'
100258	'_3D_Surface_Elevation_Spectrum'
100259	'Directional_Spreading_Function'
100260	'Auto_Spectrum'
100261	'Cross_Spectrum'
100262	'Coherence_Spectrum'
100263	'Coherent_Spectrum'
100264	'Frequency_Response_Spectrum'
100265	'Phase_Spectrum'
100266	'FIR_Filter_coefficient'
100267	'Fourier_a_Coefficient'
100268	'Fourier_b_Coefficient'
100269	'u_velocity_component'
100270	'v_velocity_component'
100271	'w_velocity_component'
100272	'Bed_Thickness'
100273	'Dispersion_Velocity_Factor'
100274	'Wind_speed'
100275	'Shore_Current_Zone'
100276	'Depth_of_Wind'
100277	'Emulsification_Constant_K1'
100278	'Emulsification_Constant_K2'
100279	'Light_Extinction'
100280	'Water_Depth'
100281	'Reference_settling_velocity'
100282	'Phase_Error'
100283	'Level_Amplitude_Error'
100284	'Discharge_Amplitude_Error'
100285	'Level_Correction'
100286	'Discharge_Correction'
100287	'Level_Simulated'
100288	'Discharge_Simulated'
100289	'Summ_Q_Corrected'
100290	'Time_Scale'
100291	'Sponge_Coefficient'
100292	'Porosity_Coefficient'
100293	'Filter_Coefficient'
100294	'Skewness'
100295	'Asymmetry'
100296	'Atiltness'
100297	'Kurtosis'
100298	'Auxiliary_variable_w'
100299	'Roller_Thickness'
100300	'Line_Thickness'
100301	'Marker_Size'
100302	'Roller_Celerity'
100303	'Encroachment_offset'
100304	'Encroachment_position'
100305	'Encroachment_width'
100306	'Conveyance_reduction'
100307	'Water_level_change'
100308	'Energy_level_change'
100309	'Horizontal_particle_velocity'
100310	'Vertical_particle_velocity'
100311	'Area_fraction'
100312	'Catchment_slope'
100313	'Average_length'
100314	'PE'
100315	'Inverse_exponent'
100316	'Time_shift'
100317	'Attenuation'
100318	'Population'
100319	'Industrial_output'
100320	'Agricultural_area'
100321	'Population_usage'
100322	'Industrial_use'
100323	'Agricultural_usage'
100324	'Layer_Thickness'
100325	'Snow_Depth'
100326	'Snow_Cover_Percentage'
100353	'Pressure_Head'
100354	'Crop_Coefficient'
100355	'Aroot_Kristensen_and_Jensen'
100356	'C1_Kristensen_and_Jensen'
100357	'C2_Kristensen_and_Jensen'
100358	'C3_Kristensen_and_Jensen'
100359	'Irrigation_Demand'
100360	'Transmissivity'
100361	'Darcy_Velocity'
100362	'Leakage_Coeff_Drain_Time_Const'
100363	'Conductance'
100364	'Height_Above_Ground'
100365	'Pumping_Rate'
100366	'Depth_Below_Ground'
100367	'Cell_Height'
100368	'Head_Gradient'
100369	'Ground_Water_Flow_Velocity'
100370	'Grid_Codes'
100371	'Drainage_Time_Constant'
100372	'Head_Elevation'
100373	'Length_Error'
100374	'Storage_Coefficient'
100375	'Specific_Yield'
100376	'Exchange_Rate'
100377	'Volumetric_Water_Content'
100378	'Storage_Change_Rate'
100379	'Seepage'
100380	'Root_Depth'
100381	'Rill_Depth'
100382	'Logical'
100383	'Leaf_Area_Index'
100384	'Irrigation_Rate'
100385	'Irrigation_Index'
100386	'Interception'
100387	'Evapotranspiration_Rate'
100388	'Erosion_Surface_Load'
100389	'Erosion_Concentration'
100390	'Epsilon_UZ'
100391	'Drainage'
100392	'Deficit'
100393	'Crop_Yield'
100394	'Crop_Type'
100395	'Crop_Stress'
100396	'Crop_Stage'
100397	'Crop_Loss'
100398	'Crop_Index'
100399	'Age'
100400	'Conductivity'
100401	'Print_Scale_Equivalence'
100402	'Concentration_1'
100403	'Concentration_2'
100404	'Concentration_3'
100405	'Concentration_4'
100406	'Sediment_diameter'
100407	'Mean_Wave_Direction'
100408	'Flow_Direction_1'
100409	'Air_Pressure'
100410	'Decay_Factor'
100411	'Sediment_Bed_Density'
100412	'Dispersion_Coefficient'
100413	'Velocity_Profile'
100414	'Habitat_Index'
100415	'Angles'
100416	'Hydraulic_Length'
100417	'SCS_Catchment_slope'
100418	'Turbidity_FTU'
100419	'Turbidity_MgPerL'
100420	'Bacteria_Flow'
100421	'Bed_Distribution'
100422	'Surface_Elevation_at_Paddle'
100423	'Unit_Hydrograph_Response'
100424	'Transfer_Rate'
100425	'Return_period'
100426	'Constant_Settling_Velocity'
100427	'Deposition_Concentration_Flux'
100428	'Settling_Velocity_Coefficient'
100429	'Erosion_Coefficient'
100430	'Volume_Flux'
100431	'Precipitation_Rate'
100432	'Evaporation_Rate'
100433	'Co_Spectrum'
100434	'Quad_Spectrum'
100435	'Propagation_Direction'
100436	'Directional_Spreading'
100437	'Mass_per_Unit_Area'
100438	'Incident_Spectrum'
100439	'Reflected_Spectrum'
100440	'Reflection_Function'
100441	'Bacteria_Flux'
100442	'Head_Difference'
100443	'Energy'
100444	'Directional_Standard_Deviation'
100445	'Rainfall_Depth'
100446	'Ground_Water_Abstraction_Depth'
100447	'Evapo_Transpiration'
100448	'Longitudinal_Infiltration'
100449	'Pollutant_Load'
100450	'Pressure'
100451	'Cost_per_Time'
100452	'Mass'
100453	'Mass_per_Time'
100454	'Mass_per_Area_per_Time'
100455	'Kd'
100456	'Porosity'
100457	'Half_Life'
100458	'Dispersivity'
100459	'Friction_Coefficient_cfw'
100460	'Wave_amplitude'
100461	'Sediment_grain_diameter'
100463	'Sediment_spill'
100464	'Number_of_Particles'
100500	'Ellipsoidal_height'
100501	'Cloudiness'
100502	'Probability'
100503	'Activity_of_Dispersant'
100504	'Dredge_Rate'
100505	'Dredge_Spill'
100506	'Clearness_Coefficient'
100507	'Coastline_Orientation'
100508	'Reduction_Factor'
100509	'Active_Beach_Height'
100510	'Update_Period'
100511	'Accumulated_Erosion'
100512	'Erosion_Rate'
100513	'Non_dimensional_Transport'
100514	'Local_coordinate'
100515	'Radii_of_Gyration'
100516	'Percentage'
100517	'Line_Capacity'
999	'Undefined'
110001	'Diverted_discharge'
110002	'Demand_carry_over_fraction'
110003	'Groundwater_demand'
110004	'Dam_crest_level'
110005	'Seepage_flux'
110006	'Seepage_fraction'
110007	'Evaporation_fraction'
110008	'Residence_time'
110009	'Owned_fraction_of_inflow'
110010	'Owned_fraction_of_volume'
110011	'Reduction_level'
110012	'Reduction_threshold'
110013	'Reduction_fraction'
110014	'Total_losses'
110015	'Counts_per_liter'
110016	'Assimilative_Capacity'
110017	'Still_Water_Depth'
110018	'Total_Water_Depth'
110019	'Maximum_wave_height'
110020	'Ice_concentration'
110021	'Wind_friction_speed'
110022	'Roughness_length'
110023	'Drag_coefficient'
110024	'Charnock_constant'
110025	'Breaking_parameter_Gamma'
110026	'Threshold_period'
110027	'Courant_number'
110028	'Time_step_factor'
110029	'Element_length'
110030	'Element_area'
110031	'Roller_angle'
110032	'Rate_of_bed_level_change'
110033	'Bed_level_change'
110034	'Sediment_transport_direction'
110035	'Wave_action_density'
110036	'Zero_moment_of_wave_action'
110037	'First_moment_of_wave_action'
110038	'Bed_Mass'
110039	'Water_Quality'
110040	'Status'
110041	'Setting'
110042	'Reaction_Rate'
110043	'Fast_Runoff_Discharge'
110044	'Slow_Runoff_Discharge'
110045	'Average_Sediment_Transport_per_length_unit'
110046	'Valve_Setting'
110047	'Wave_energy_density'
110048	'Wave_energy_distribution'
110049	'Wave_energy'
110050	'Radiation_Melting_Coefficient'
110051	'Rain_melting_coefficient'
110052	'Friction'
110053	'Wave_action_density_rate'
110054	'Element_area_long_lat'
110100	'Electric_Current'
110200	'Heat_Flux_Resistance'
110210	'Absolute_Humidity'
110220	'Length'
110230	'Volume'
110231	'Element_Volume'
110232	'Wave_Power'
110233	'Moment_of_Inertia'
110234	'Topography'
110235	'Scour_Depth'
110236	'Scour_Width'
110237	'Cost_per_Volume'
110238	'Cost_per_Energy'
110239	'Cost_per_Mass'
110240	'Application_Intensity'
110241	'Cost'
110242	'Voltage'
110243	'Normal_Velocity'
110244	'Gravity'
110245	'Vessel_Displacement'
110246	'Hydrostatic_Matrix'
110247	'Wave_Number'
110248	'Radiation_Potential'
110249	'Added_Mass_TT'
110250	'Radiation_Damping'
110251	'Frequency'
110252	'Sound_Exposure_Level'
110253	'Transmission_Loss'
110254	'pH'
110255	'Acoustic_Attenuation'
110256	'Sound_Speed'
110257	'Leakage'
110258	'Height_Above_Keel'
110259	'Submerged_Mass'
110260	'Deflection'
110261	'Linear_Damping_Coefficient'
110262	'Quadratic_Damping_Coefficient'
110263	'Damping_TT'
110264	'RAO_Motion'
110265	'RAO_Rotation'
110266	'Added_Mass_Coefficient'
110267	'Electrical_Conductivity'
110268	'Added_Mass_TR'
110269	'Added_Mass_RT'
110270	'Added_Mass_RR'
110271	'Damping_TR'
110272	'Damping_RT'
110273	'Damping_RR'
110274	'Fender_Force'
110275	'Force'
110276	'Moment'
110277	'Reduced_Pollutant_Load'
110278	'Size_and_Position'
110279	'Frame_Rate'
110280	'Dynamic_Viscosity'
110281	'Grid_Rotation'
110282	'Agent_Density'
110283	'Emitter_Coefficient'
110284	'Pipe_Diameter'
110285	'Speed'
110286	'Velocity'
110287	'Direction'
110288	'Displacement'
110289	'Position'
110290	'Rotation'
110291	'Torque'
110292	'Overtopping'
110293	'Flow_Rate'
110294	'Acceleration'
110295	'Dimensionless_Acceleration'
110296	'Time'
110297	'Resistance'
110298	'Amount_of_Substance'
110299	'Molar_Concentration'
110300	'Molal_Concentration'
110301	'Suspended_sediment_load_per_area'
110302	'Bollard_Force'};
if nargin>0
    if ischar(key)
        iType = strcmp(Types(:,2),key);
        Types = Types{iType,1};
    else
        iType = [Types{:,1}]==key;
        Types = Types{iType,2};
    end
end

function [Units,Qunit] = EUMUnit(key)
Units = {1000	'meter'	'm'
1001	'kilometer'	'km'
1007	'centimeter'	'cm'
1002	'millimeter'	'mm'
1003	'feet'	'ft'
1014	'feet_US'	'ftUS'
1004	'inch'	'in'
1013	'inch_US'	'inUS'
1005	'mile'	'mi'
1016	'mile_US'	'miUS'
1006	'yard'	'yd'
1015	'yard_US'	'ydUS'
1800	'meter_pow_3_per_sec'	'm3 s-1'
1816	'meter_pow_3_per_min'	'm3 min-1'
1801	'feet_pow_3_per_sec'	'ft3 s-1'
1817	'feet_pow_3_per_min'	'ft3 min-1'
1814	'feet_pow_3_per_day'	'ft3 d-1'
1815	'feet_pow_3_per_year'	'ft3 yr-1'
1810	'meter_pow_3_per_day'	'm3 d-1'
1805	'meter_pow_3_per_year'	'm3 yr-1'
1804	'acre_feet_per_day'	'acre ft d-1'
1821	'meter_pow_3_per_hour'	'm3 h-1'
1818	'gallon_per_min'	'gal min-1'
1820	'liter_per_minute'	'l min-1'
1819	'liter_per_sec'	'l s-1'
1803	'Mgal_per_day'	'Mgal d-1'
1823	'MgalUK_per_day'	'MgalUK d-1'
1802	'Ml_per_day'	'Ml d-1'
2000	'meter_per_sec'	'm s-1'
2002	'feet_per_sec'	'ft s-1'
2020	'miles_per_hour'	'mi h-1'
2021	'km_per_hour'	'km h-1'
2019	'knot'	'knot'
2401	'degree'	'degree'
2800	'degree_Celsius'	'absolute degC'
2900	'delta_degree_Celsius'	'degC'
2801	'degree_Fahrenheit'	'absolute degF'
2901	'delta_degree_Fahrenheit'	'degF'
2802	'degree_Kelvin'	'degK'
2201	'mu_g_per_meter_pow_3'	'ug m-3'
2202	'mg_per_meter_pow_3'	'mg m-3'
2203	'gram_per_meter_pow_3'	'g m-3'
2200	'kg_per_meter_pow_3'	'kg m-3'
2204	'mu_g_per_liter'	'ug l-1'
2205	'mg_per_liter'	'mg l-1'
2206	'gram_per_liter'	'g l-1'
2207	'pound_per_feet_pow_3'	'lb ft-3'
2208	'ton_per_meter_pow_3'	'ton m-3'
3000	'million_per__100_ml'	'1e6 (100_ml)-1'
3001	'per__100_ml'	'(100_ml)-1'
3002	'per_liter'	'l-1'
99000	'__'	'-'
1831	'feet_US_pow_3_per_sec'	'ftUS3 s-1'
1833	'feet_US_pow_3_per_day'	'ftUS3 d-1'
1834	'feet_US_pow_3_per_year'	'ftUS3 yr-1'
1832	'feet_US_pow_3_per_min'	'ftUS3 min-1'
1835	'yard_US_pow_3_per_sec'	'ydUS3 s-1'
1830	'yard_pow_3_per_sec'	'yd3 s-1'
2006	'meter_per_day'	'm d-1'
2036	'feet_US_per_sec'	'ft_US s-1'
99001	'percent'	'percent'
2601	'percent_per_day'	'percent d-1'
2023	'cm_per_sec'	'cm s-1'
2017	'mm_per_sec'	'mm s-1'
2011	'inch_per_sec'	'in s-1'
2035	'inch_US_per_sec'	'inUS s-1'
99003	'hours_per_day'	'h d-1'
6600	'watt_per_meter_pow_2'	'W m-2'
5703	'joule_per_meter_pow_2_per_day'	'J m-2 d-1'
5700	'kJ_per_meter_pow_2_per_hour'	'kJ m-2 h-1'
5701	'kJ_per_meter_pow_2_per_day'	'kJ m-2 d-1'
5702	'MJ_per_meter_pow_2_per_day'	'MJ m-2 d-1'
6200	'PSU'	'PSU'
99002	'per_thousand'	'1e-3'
3200	'meter_pow_2'	'm2'
3203	'feet_pow_2'	'ft2'
3600	'meter_pow__1_per_3__per_sec'	'm^{1/3} s-1'
3601	'feet_pow__1_per_3__per_sec'	'ft^{1/3} s-1'
3800	'sec_per_meter_pow__1_per_3_'	's m^{-1/3}'
3801	'sec_per_feet_pow__1_per_3_'	's ft^{-1/3}'
4000	'meter_pow__1_per_2__per_sec'	'm^{1/2} s-1'
4001	'feet_pow__1_per_2__per_sec'	'ft^{1/2} s-1'
4002	'feet_US_pow__1_per_2__per_sec'	'ftUS^{1/2} s-1'
1600	'meter_pow_3'	'm3'
1601	'liter'	'l'
1609	'megaliter'	'Ml'
1606	'km_pow_3'	'km3'
1607	'acre_feet'	'acre ft'
1603	'feet_pow_3'	'ft3'
1604	'gallon'	'gal'
1608	'megagallon'	'Mgal'
1610	'_10_pow_6meter_pow_3'	'10e6 m3'
3202	'acre'	'acre'
1200	'kilogram'	'kg'
1201	'gram'	'g'
1202	'milligram'	'mg'
1203	'microgram'	'ug'
1204	'ton'	'ton'
1207	'pound'	'lb'
1205	'kiloton'	'kton'
1206	'megaton'	'Mton'
4200	'kg_per_sec'	'kg s-1'
4203	'gram_per_sec'	'g s-1'
4202	'mg_per_sec'	'mg s-1'
4201	'mu_g_per_sec'	'ug s-1'
4215	'pound_per_sec'	'lb s-1'
1615	'yard_pow_3'	'yd3'
1614	'yard_US_pow_3'	'ydUS3'
1836	'liter_per_day'	'l d-1'
2001	'mm_per_hour'	'mm h-1'
2004	'mm_per_day'	'mm d-1'
2016	'inch_per_hour'	'in h-1'
2018	'cm_per_hour'	'cm h-1'
2009	'feet_per_day'	'ft d-1'
2014	'inch_per_min'	'in min-1'
2031	'mu_m_per_sec'	'um s-1'
4801	'millimeter_per_day'	'mm d-1'
4802	'inch_per_day'	'in d-1'
2030	'liter_per_sec_per_ha'	'l s-1 ha-1'
2040	'mm_per_year'	'mm yr-1'
3201	'meter_pow_3_per_meter'	'm3 m-1'
3207	'feet_pow_3_per_feet'	'ft3 ft-1'
3214	'feet_US_pow_3_per_feet_US'	'ftUS3 ftUS-1'
3213	'yard_pow_3_per_yard'	'yd3 yd-1'
3212	'yard_US_pow_3_per_yard_US'	'ydUS3 ydUS-1'
4204	'kg_per_hour'	'kg h-1'
4400	'gram_per_meter_pow_2'	'g m-2'
4500	'gram_per_meter_pow_2_per_day'	'g m-2 d-1'
4600	'gram_per_meter_pow_3_per_hour'	'g m-3 h-1'
1208	'ton_US'	'tonUS'
4401	'kg_per_meter'	'kg m-1'
4700	'meter_pow_3_per_sec_per_meter'	'm3 s-1 m-1'
4702	'meter_pow_2_per_sec'	'm2 s-1'
4704	'feet_pow_2_per_sec'	'ft2 s-1'
4721	'cm_pow_3_per_sec_per_cm'	'cm3 s-1 cm-1'
4722	'mm_pow_3_per_sec_per_mm'	'mm3 s-1 mm-1'
4718	'feet_pow_3_per_sec_per_feet'	'ft3 s-1 ft-1'
4723	'feet_US_pow_3_per_sec_per_feet_US'	'ftUS3 s-1 ftUS-1'
4724	'inch_pow_3_per_sec_per_inch'	'in3 s-1 in-1'
4725	'inch_US_pow_3_per_sec_per_inch_US'	'inUS3 s-1 inUS-1'
4726	'yard_US_pow_3_per_sec_per_yard_US'	'ydUS3 s-1 ydUS-1'
4727	'yard_pow_3_per_sec_per_yard'	'yd3 s-1 yd-1'
4701	'meter_pow_3_per_year_per_meter'	'm3 yr-1 m-1'
1400	'second'	's'
2602	'hertz'	'Hz'
1403	'day'	'd'
2003	'liter_per_sec_per_km_pow_2'	'l s-1 km-2'
2005	'acre_feet_per_sec_per_acre'	'acre ft s-1 acre-1'
2007	'feet_pow_3_per_sec_per_mile_pow_2'	'ft3 s-1 mi-2'
4900	'watt'	'W'
4901	'kilowatt'	'kW'
4902	'megawatt'	'MW'
4903	'gigawatt'	'GW'
2400	'radian'	'rad'
5000	'per_meter'	'm-1'
5003	'per_feet'	'ft-1'
5005	'per_feet_US'	'ftUS-1'
6800	'meter_pow_3_per_sec_pow_2'	'm3 s-2'
5200	'meter_pow_2_sec_per_rad'	'm2 s rad-1'
5213	'feet_pow_2_sec_per_rad'	'ft2 s rad-1'
5201	'meter_pow_2_per_rad'	'm2 rad-1'
5215	'feet_pow_2_per_rad'	'ft2 rad-1'
5202	'meter_pow_2_sec'	'm2 s'
5217	'feet_pow_2_sec'	'ft2 s'
9400	'square_meter_per_sec'	'm2 s-1'
9401	'square_feet_per_sec'	'ft2 s-1'
99013	'Integer'	'-'
3204	'hectare'	'ha'
3205	'km_pow_2'	'km2'
3206	'mile_pow_2'	'mi2'
1010	'millifeet'	'mft'
4205	'kg_per_day'	'kg d-1'
4207	'kg_per_year'	'kg yr-1'
1402	'hour'	'h'
2603	'per_hour'	'h-1'
2600	'per_day'	'd-1'
2605	'per_sec'	's-1'
4501	'gram_per_meter_pow_2_per_sec'	'g m-2 s-1'
4503	'kg_per_meter_pow_2_per_sec'	'kg m-2 s-1'
5400	'newton_per_meter_pow_2'	'N m-2'
5401	'kN_per_meter_pow_2'	'kN m-2'
5402	'pound_per_feet_per_sec_pow_2'	'lb ft-1 s-1 2'
5500	'newton_per_meter_pow_3'	'N m-3'
5501	'kN_per_meter_pow_3'	'kN m-3'
4601	'gram_per_meter_pow_3_per_day'	'g m-3 d-1'
5300	'_mg_per_l__pow__1_per_2__per_day'	'mg l^{-1/2} d-1'
5301	'_mg_per_l__pow__1_per_2__per_hour'	'mg l^{-1/2} h-1'
2008	'meter_per_hour'	'm h-1'
1806	'gal_per_day_per_head'	'gal d-1 head-1'
1807	'liter_per_day_per_head'	'l d-1 head-1'
1808	'meter_pow_3_per_sec_per_head'	'm3 s-1 head-1'
2010	'mm_per_month'	'mm month-1'
5800	'mm_per_C_per_day'	'mm degC-1 d-1'
5801	'mm_per_C_per_hour'	'mm degC-1 h-1'
5802	'inch_per_F_per_day'	'in degF-1 d-1'
5803	'inch_per_F_per_hour'	'in degF-1 h-1'
5900	'_per_C_per_day'	'degC-1 d-1'
5901	'_per_C_per_hour'	'degC-1 h-1'
5902	'_per_F_per_day'	'degF-1 d-1'
5903	'_per_F_per_hour'	'degF-1 h-1'
6000	'Celsius_per__100meter'	'degC (100 m)-1'
6001	'Celsius_per__100feet'	'degC (100 ft)-1'
6003	'Fahrenheit_per__100feet'	'degF (100 ft)-1'
6002	'Fahrenheit_per__100meter'	'degF (100 m)-1'
5001	'percent_per__100meter'	'percent (100 m)-1'
5002	'percent_per__100feet'	'percent (100 ft)-1'
4206	'gram_per_day'	'g d-1'
4220	'ton_per_sec'	'ton s-1'
6100	'pascal'	'Pa'
6101	'hectopascal'	'hPa'
6108	'millibar'	'mbar'
6400	'meter_pow_2_per_sec_pow_2'	'm2 s-1 2'
4720	'feet_pow_2_per_sec_pow_2'	'ft2 s-1 2'
6401	'meter_pow_2_per_sec_pow_3'	'm2 s-1 3'
6402	'feet_pow_2_per_sec_pow_3'	'ft2 s-1 3'
6300	'PSU_meter_pow_3_per_sec'	'PSU m3 s-1'
6302	'non_dim_meter_pow_3_per_sec'	'm3 s-1'
6303	'PSU_feet_pow_3_per_sec'	'PSU ft3 s-1'
6301	'C_meter_pow_3_per_sec'	'degC m3 s-1'
6304	'F_feet_pow_3_per_sec'	'degF ft3 s-1'
99007	'mg_per_gram'	'mg g-1'
99018	'ml_per_liter'	'ml l-1'
99020	'per_million'	'1e-6'
99008	'mg_per_kg'	'mg kg-1'
99019	'mu_l_per_liter'	'ul l-1'
6500	'joule_per_kg'	'j kg-1'
6700	'joule_kg_per_K'	'J kg degK-1'
2012	'meter_per_min'	'm min-1'
2013	'feet_per_min'	'ft min-1'
2015	'feet_per_hour'	'ft h-1'
6801	'feet_pow_3_per_sec_pow_2'	'ft3 s-1 2'
1009	'nautical_mile'	'nmi'
1401	'minute'	'min'
1405	'month'	'month'
1404	'year'	'yr'
4602	'gram_per_meter_pow_3_per_sec'	'g m-3 s-1'
6900	'meter_pow_3_per_gram'	'm3 g-1'
6901	'liter_per_gram'	'l g-1'
2027	'meter_pow_3_per_ha_per_sec'	'm3 ha-1 s-1'
2026	'meter_pow_3_per_ha_per_hour'	'm3 ha-1 h-1'
2025	'meter_pow_3_per_ha_per_day'	'm3 ha-1 d-1'
2024	'feet_pow_3_per_acre_per_sec'	'ft3 acre-1 s-1'
2041	'feet_pow_3_per_acre_per_hour'	'ft3 acre-1 h-1'
2042	'feet_pow_3_per_acre_per_day'	'ft3 acre-1 d-1'
2029	'liter_per_min_per_ha'	'l min-1 ha-1'
2028	'gallon_per_min_per_acre'	'gal min-1 acre-1'
4904	'horsepower'	'hp'
4502	'kg_per_ha_per_hour'	'kg_per_ha h-1'
4504	'kg_per_ha_per_day'	'kg_per_ha d-1'
4507	'pound_per_feet_pow_2_per_sec'	'lb ft-2 s-1'
4505	'pound_per_acre_per_day'	'lb acre-1 d-1'
7000	'newton'	'N'
7100	'meter_pow_2_per_hertz'	'm2 Hz-1'
7103	'feet_pow_2_per_hertz'	'ft2 Hz-1'
7101	'meter_pow_2_per_hertz_per_deg'	'm2 Hz-1 deg-1'
7102	'meter_pow_2_per_hertz_per_rad'	'm2 Hz-1 rad-1'
7104	'feet_pow_2_per_hertz_per_deg'	'ft2 Hz-1 deg-1'
7105	'feet_pow_2_per_hertz_per_rad'	'ft2 Hz-1 rad-1'
8100	'kg_per_sec_pow_2'	'kg s-1 2'
9100	'meter_pow_2_per_kg'	'm2 kg-1'
99004	'person'	'person'
2604	'currency_per_year'	'currency yr-1'
1809	'liter_per_person_per_day'	'l person-1 d-1'
1611	'meter_pow_3_per_currency'	'm3 currency-1'
4803	'meter_pow_3_per_km_pow_2_per_day'	'm3 km-2 d-1'
5004	'per_inch'	'in-1'
4708	'meter_pow_2_per_hour'	'm2 h-1'
4709	'meter_pow_2_per_day'	'm2 d-1'
4710	'feet_pow_2_per_hour'	'ft2 h-1'
4711	'feet_pow_2_per_day'	'ft2 d-1'
2037	'feet_US_per_day'	'ftUS d-1'
2038	'inch_US_per_hour'	'inUS h-1'
2039	'inch_US_per_min'	'inUS min-1'
1811	'gallon_per_sec'	'gal s-1'
1812	'gallon_per_day'	'gal d-1'
1813	'gallon_per_year'	'gal yr-1'
4402	'kg_per_meter_pow_2'	'kg m-2'
99011	'meter_pow_3_per_meter_pow_3'	'm3 m-3'
99012	'liter_per_meter_pow_3'	'l m-3'
4403	'kg_per_ha'	'kg ha-1'
4406	'kg_per_km_pow_2'	'kg km-2'
4408	'gram_per_km_pow_2'	'g km-2'
4410	'gram_per_ha'	'g ha-1'
2210	'ton_per_meter_pow_2'	'ton m-2'
4407	'ton_per_km_pow_2'	'ton km-2'
4409	'ton_per_ha'	'ton ha-1'
2209	'pound_per_feet_pow_2'	'lb ft-2'
4405	'pound_per_acre'	'lb acre-1'
4411	'pound_per_mile_pow_2'	'lb mi-2'
4412	'kg_per_acre'	'kg acre-1'
4413	'kg_per_feet_pow_2'	'kg ft-2'
4414	'kg_per_mile_pow_2'	'kg mi-2'
4415	'ton_per_acre'	'ton acre-1'
4416	'ton_per_feet_pow_2'	'ton ft-2'
4417	'ton_per_mile_pow_2'	'ton mi-2'
4418	'gram_per_acre'	'g acre-1'
4419	'gram_per_feet_pow_2'	'g ft-2'
4420	'gram_per_mile_pow_2'	'g mi-2'
4421	'pound_per_ha'	'lb ha-1'
4422	'pound_per_meter_pow_2'	'lb m-2'
4423	'pound_per_km_pow_2'	'lb km-2'
2214	'pound_per_feet_US_pow_3'	'lb ftUS-3'
2212	'pound_per_yard_US_pow_3'	'lb ydUS-3'
2213	'pound_per_yard_pow_3'	'lb yd-3'
99005	'gram_per_gram'	'g g-1'
99006	'gram_per_kg'	'g kg-1'
99009	'mu_g_per_gram'	'ug g-1'
99010	'kg_per_kg'	'kg kg-1'
1008	'micrometer'	'um'
2606	'_10_pow_9_per_day'	'1e9 d-1'
2607	'_10_pow_12_per_year'	'1e12 yr-1'
4706	'meter_pow_3_per_sec_per__10mm'	'm3 s-1 (10 mm)-1'
4707	'feet_pow_3_per_sec_per_inch'	'ft3 s-1 in-1'
4404	'mg_per_meter_pow_2'	'mg m-2'
2211	'mu_g_per_meter_pow_2'	'ug m-2'
3401	'_1000_per_meter_pow_2_per_day'	'1e3 m-2 d-1'
3402	'_per_meter_pow_2_per_sec'	'm-2 s-1'
5604	'terajoule'	'TJ'
5603	'gigajoule'	'GJ'
5602	'megajoule'	'MJ'
5601	'kilojoule'	'kJ'
5600	'joule'	'J'
5607	'petajoule'	'PJ'
5608	'exajoule'	'EJ'
5605	'kilowatt_hour'	'kW h'
5609	'megawatt_hour'	'MW h'
5610	'gigawatt_hour'	'GW h'
1011	'liter_per_meter_pow_2'	'l m-2'
7503	'meter_pow_3_per_hour_per_meter'	'm3 h-1 m-1'
7504	'meter_pow_3_per_day_per_meter'	'm3 d-1 m-1'
4719	'feet_pow_3_per_hour_per_feet'	'ft3 h-1 ft-1'
4732	'feet_pow_3_per_day_per_feet'	'ft3 d-1 ft-1'
4712	'galUK_per_day_per_feet'	'galUK d-1 ft-1'
4713	'gallon_per_day_per_feet'	'gal d-1 ft-1'
4714	'gallon_per_min_per_feet'	'gal min-1 ft-1'
4715	'liter_per_day_per_meter'	'l d-1 m-1'
7501	'liter_per_min_per_meter'	'l min-1 m-1'
7500	'liter_per_sec_per_meter'	'l s-1 m-1'
4208	'gram_per_min'	'g min-1'
4212	'pound_per_day'	'lb d-1'
4213	'pound_per_hour'	'lb h-1'
4214	'pound_per_min'	'lb min-1'
4217	'pound_per_year'	'lb yr-1'
4218	'ton_per_year'	'ton yr-1'
6105	'MetresWater'	'mH_2O'
6106	'FeetWater'	'ftH_2O'
6102	'kilopascal'	'kPa'
6104	'megapascal'	'MPa'
6103	'psi'	'psi'
6107	'bar'	'bar'
6110	'decibar'	'decibar'
4506	'kg_per_meter_pow_2_per_day'	'kg m-2 d-1'
6902	'meter_pow_3_per_mg'	'm3 mg-1'
6903	'meter_pow_3_per_mu_g'	'm3 ug-1'
4219	'ton_per_day'	'ton d-1'
1012	'millimeterD50'	'millimeterD50'
1250	'per_kg'	'kg-1'
1251	'per_gram'	'g-1'
1252	'per_mg'	'mg-1'
1253	'per_mu_g'	'ug-1'
1254	'per_ton'	't-1'
1255	'per_kiloton'	'kt-1'
1256	'per_megaton'	'Mt-1'
1257	'per_pound'	'lb-1'
1258	'per_ton_US'	'tonUS-1'
1406	'millisecond'	'ms'
3208	'feet_US_pow_2'	'ftUS2'
3209	'yard_US_pow_2'	'ydUS2'
3210	'mile_US_pow_2'	'miUS2'
3211	'acre_US'	'acreUS'
3215	'liter_per_meter'	'l m-1'
1602	'milliliter'	'ml'
1605	'milligallon'	'mgal'
1612	'gallonUK'	'galUK'
1613	'megagallonUK'	'MgalUK'
1822	'galUK_per_day'	'galUK d-1'
1824	'feet_pow_3_per_PE_per_day'	'ft3 PE-1 d-1'
1825	'meter_pow_3_per_PE_per_day'	'm3 PE-1 d-1'
1826	'galUK_per_sec'	'galUK s-1'
1827	'galUK_per_year'	'galUK yr-1'
1828	'galUK_per_PE_per_day'	'galUK PE-1 d-1'
2022	'acre_feet_per_day_per_acre'	'acre ft d-1 acre-1'
2032	'Mgal_per_day_per_acre'	'Mgal d-1 acre-1'
2033	'MgalUK_per_day_per_acre'	'MgalUK d-1 acre-1'
2034	'Ml_per_day_per_ha'	'Ml d-1 ha-1'
2043	'liter_per_hour_per_ha'	'l h-1 ha-1'
2044	'liter_per_day_per_ha'	'l d-1 ha-1'
2100	'meter_per_sec_pow_2'	'm s-2'
2101	'feet_per_sec_pow_2'	'ft s-2'
2215	'pound_per_feet_US_pow_2'	'lb ftUS-2'
2300	'kg_per_meter_per_sec'	'kg m-1 s-1'
2301	'Pascal_second'	'Pa s'
2402	'degree50'	'degree50'
2403	'degree_pow_2'	'deg2'
2500	'degree_per_meter'	'deg m-1'
2501	'radian_per_meter'	'rad m-1'
2510	'degree_per_sec'	'deg s-1'
2511	'radian_per_sec'	'rad s-1'
2608	'meter_pow_2_per_sec_per_ha'	'm2 s-1 ha-1'
2609	'feet_pow_2_per_sec_per_acre'	'ft2 s-1 acre-1'
2610	'rev_per_min'	'rev min-1'
2611	'percent_per_hour'	'percent h-1'
2613	'percent_per_sec'	'percent s-1'
2850	'_per_degree_C'	'degC-1'
2851	'_per_degree_F'	'degF-1'
3003	'per_meter_pow_3'	'm-3'
3004	'per_ml'	'ml-1'
3005	'per_feet_pow_3'	'ft-3'
3006	'per_gal'	'gal-1'
3007	'per_mgal'	'mgal-1'
3008	'per_km_pow_3'	'km-3'
3009	'per_acre_feet'	'acre-1 ft-1'
3010	'per_Mgal'	'Mgal-1'
3011	'per_Ml'	'Ml-1'
3012	'per_gallonUK'	'galUK-1'
3013	'per_MgalUK'	'MgalUK-1'
3014	'per_yard_US_pow_3'	'ydUS-3'
3015	'per_yard_pow_3'	'yd-3'
3100	'sec_per_meter'	's m-1'
3400	'Einstein_per_meter_pow_2_per_day'	'Einstein m-2 d-1'
4209	'kg_per_PE_per_day'	'kg PE-1 d-1'
4210	'kg_per_min'	'kg min-1'
4216	'pound_per_PE_per_day'	'lb PE-1 d-1'
4424	'mg_per_ha'	'mg ha-1'
4425	'mg_per_km_pow_2'	'mg km-2'
4426	'mg_per_acre'	'mg acre-1'
4427	'mg_per_feet_pow_2'	'mg ft-2'
4428	'mg_per_mile_pow_2'	'mg mile-2'
4429	'pound_per_meter'	'lb m-1'
4430	'ton_per_meter'	't m-1'
4603	'mg_per_liter_per_day'	'mg l-1 d-1'
4728	'yard_pow_3_per_year_per_yard'	'yd3 yr-1 yd'
4729	'yard_US_pow_3_per_year_per_yard_US'	'ydUS3 yr-1 ydUS'
5006	'per_inch_US'	'inUS-1'
5100	'meter_pow_3_per_sec_2'	'm3 s-2'
5203	'meter_pow_2_per_deg'	'm2 deg-1'
5204	'meter_pow_2_sec_pow_2_per_rad'	'm2 s2 rad-1'
5205	'meter_pow_2_per_sec_per_rad'	'm2 s-1 rad-1'
5206	'meter_pow_2_sec_per_deg'	'm2_s deg-1'
5207	'meter_pow_2_sec_pow_2_per_deg'	'm2 s2 deg-1'
5208	'meter_pow_2_per_sec_per_deg'	'm2 s-1 deg-1'
5209	'feet_pow_2_per_sec_per_rad'	'ft2 s-1 deg-1'
5210	'feet_pow_2_per_sec_per_deg'	'ft2 s-1 deg-1'
5211	'feet_pow_2_sec_pow_2_per_rad'	'ft2_sec2 deg-1'
5212	'feet_pow_2_sec_pow_2_per_deg'	'ft2_sec2 deg-1'
5214	'feet_pow_2_sec_per_deg'	'ft2_sec deg-1'
5216	'feet_pow_2_per_deg'	'ft2 deg-1'
7060	'kg_meter_pow_2'	'kg_m2'
5606	'watt_second'	'W s'
5650	'per_joule'	'J-1'
5651	'per_kJ'	'kJ-1'
5652	'per_MJ'	'MJ-1'
5653	'per_GJ'	'GJ-1'
5654	'per_TJ'	'TJ-1'
5655	'per_PJ'	'PJ-1'
5656	'per_EJ'	'EJ-1'
5657	'per_kWh'	'kW-1 h-1'
5658	'per_Ws'	'W-1 s-1'
5659	'per_MWh'	'MW-1 h-1'
5660	'per_GWh'	'GW-1 h-1'
5710	'mm_per__kJ_per_meter_pow_2_'	'mm kJ-1 m-2'
5711	'mm_per__MJ_per_meter_pow_2_'	'mm MJ-1 m-2'
6109	'micropascal'	'uPa'
6150	'dB_re_1_mu_Pa_pow_2_sec'	'dB re 1 uPa2 s'
6160	'dB_per_lambda'	'dB lambda-1'
6403	'meter_pow_2_per_sec_pow_3_per_rad'	'm2 s-3 rad-1'
6404	'feet_pow_2_per_sec_pow_3_per_rad'	'ft2 s-3 rad-1'
6802	'AFD_per_sec'	'AFD s-1'
6803	'IMGD_per_sec'	'IMGD s-1'
6804	'MGD_per_sec'	'MGD s-1'
6805	'GPM_per_sec'	'GPM s-1'
6806	'meter_pow_3_per_day_per_sec'	'm3 d-1 s-1'
6807	'meter_pow_3_per_hour_per_sec'	'm3 h-1 s-1'
6808	'Ml_per_day_per_sec'	'Ml d-1 s-1'
6809	'liter_per_min_per_sec'	'l min-1 s-1'
6810	'liter_per_sec_pow_2'	'l s-1 2'
7001	'kilonewton'	'kN'
7002	'meganewton'	'MN'
7003	'millinewton'	'mN'
7050	'kg_meter'	'kg_m'
7070	'kg_meter_per_sec'	'kg_m s-1'
7080	'kg_meter_pow_2_per_sec'	'kg_m2 s-1'
7200	'meter_pow_2_per_hertz_pow_2'	'm2 Hz-2'
7201	'meter_pow_2_per_hertz_pow_2_per_deg'	'm2 Hz-2 deg-1'
7202	'meter_pow_2_per_hertz_pow_2_per_rad'	'm2 Hz-2 rad-1'
7502	'Ml_per_day_per_meter'	'Ml d-1 m-1'
7505	'feet_pow_3_per_sec_per_psi'	'ft3 s-1 psi-1'
7506	'gallon_per_min_per_psi'	'gal min-1 psi-1'
7507	'Mgal_per_day_per_psi'	'Mgal d-1 psi-1'
7508	'MgalUK_per_day_per_psi'	'MgalUK d-1 psi-1'
7509	'acre_feet_per_day_per_psi'	'acre_ft d-1 psi-1'
9200	'_per_meter_per_sec'	'm-1 s-1'
9201	'meter_per_sec_per_ha'	'm s-1 ha-1'
9202	'feet_per_sec_per_acre'	'ft s-1 acre-1'
9300	'per_meter_pow_2'	'm-2'
9301	'per_acre'	'acre-1'
9302	'per_hectare'	'ha-1'
9303	'per_km_pow_2'	'km-2'
9351	'currency_per_meter_pow_3'	'currency m-3'
9352	'currency_per_feet_pow_3'	'currency ft-3'
9600	'per_watt'	'W-1'
9700	'newton_meter'	'N m'
9701	'kilonewton_meter'	'kN m'
9702	'meganewton_meter'	'MN m'
9703	'newton_millimeter'	'N mm'
9800	'newton_meter_second'	'N m s'
9900	'newton_per_meter_per_sec'	'N m-1 s-1'
12000	'mole'	'mol'
12001	'millimole'	'mmol'
12002	'micromole'	'umol'
12003	'nanomole'	'nmol'
12020	'mole_per_liter'	'mol l-1'
12021	'mmol_per_liter'	'mmol l-1'
12022	'mu_mol_per_liter'	'umol l-1'
12023	'nmol_per_liter'	'nmol l-1'
12024	'mole_per_meter_pow_3'	'mol m-3'
12025	'mmol_per_meter_pow_3'	'mmol m-3'
12026	'mu_mol_per_meter_pow_3'	'umol m-3'
12040	'mole_per_kg'	'mol kg-1'
12041	'mmol_per_kg'	'mmol kg-1'
12042	'mu_mol_per_kg'	'umol kg-1'
12043	'nmol_per_kg'	'nmol kg-1'
99014	'meter_per_meter'	'm m-1'
99015	'per_minute'	'min-1'
2612	'percent_per_min'	'percent min-1'
99016	'per_month'	'month-1'
99017	'per_year'	'yr-1'
99021	'g_acceleration'	'ga'
99100	'ampere'	'A'
99101	'milliampere'	'mA'
99102	'microampere'	'uA'
99103	'kiloampere'	'kA'
99104	'megaampere'	'MA'
99150	'volt'	'V'
99151	'millivolt'	'mV'
99152	'microvolt'	'uV'
99153	'kilovolt'	'kV'
99154	'megavolt'	'MV'
99180	'ohm'	'ohm'
99181	'kOhm'	'kohm'
99182	'Mohm'	'Mohm'
0	'undefined'	''
99200	'watt_per_meter'	'W m-1'
99201	'kW_per_meter'	'kW m-1'
99202	'MW_per_meter'	'MW m-1'
99203	'GW_per_meter'	'GW m-1'
99204	'kW_per_feet'	'kW ft-1'
99250	'siemens'	'S'
99251	'millisiemens'	'mS'
99252	'microsiemens'	'uS'
99260	'siemens_per_meter'	'S m-1'
99261	'mS_per_cm'	'mS cm-1'
99262	'mu_S_per_cm'	'uS cm-1'
99263	'kg_per__meter_sec_'	'kg m-1 s-1'
99264	'cPoise'	'cP'
99265	'lbf_sec_per_feet_pow_2'	'lbf s ft-2'
99266	'pound_per__sec_feet_'	'lb s-1 ft-1'};
if nargin>0
    if ischar(key)
        iType = strcmp(Units(:,2),key);
        Qunit = Units{iType,3};
        Units = Units{iType,1};
    else
        iType = [Units{:,1}]==key;
        Qunit = Units{iType,3};
        Units = Units{iType,2};
    end
end

function Times = EUMTimeUnit(key)
Times = {1400 'SECOND'
1401   'MINUTE'
1402   'HOUR'
1403   'DAY'
1405   'MONTH'
1404   'YEAR'};
if nargin>0
    if ischar(key)
        iType = strcmp(Times(:,2),key);
        Times = Times{iType,1};
    else
        iType = [Times{:,1}]==key;
        Times = Times{iType,2};
    end
end
