function varargout = grib(cmd,varargin)
%GRIB Read GRIB files. (BETA VERSION)
%
%      FI = grib('open',FILENAME,<DEBUG_FID>) 
%
%   opens the specified GRIB file and returns information 
%   on the file content. Optionally while scanning the 
%   file, debug message are written to the file specified 
%   by DEBUG_FID. Drop the third input argument if debug 
%   information should not be written.
%
%      DATA = grib('read',FI,BLOCKNR) 
%
%   reads the data of the specified block number from 
%   the GRIB data file that was opened previously.
%   [DATA,LAT,LON,DATENUM] = GRIB('read',FI,BLOCKNR) 
%   reads the data, grid coordinates and time (reference + P1).
%
%      grib('disp',FI,<BLOCKNR>)
%      grib('list',FI,<BLOCKNR>)
%
%   display all meta-info from grib file, generate list 
%   with one line per block. When BLOCKNR is not 
%   given, all blocks are displayed one at a time. BLOCKNR 
%   can be an array of blocks, e.g. [1 2 5 6].
%
%      grib('find',FI,<BLOCKNRS>,<name,value>)
%
%   finds block indices BLOCKNR that match specified 
%   <name,value> combinations. When BLOCKNRS is 
%   specified, only a subset from BLOCKNRS is returned.
%   Multiple <name,value> pairs can be supplied, or 
%   use INTERSECT after multiple grib_block calls.
%   The names refer to numeric or character fieldnames 
%   in the FI.Block(:).Info struct.
%
%      grib('meta',FI,<BLOCKNRS>,<fieldname>,<name,value>)
%
%   extract the specific meta-data field from each block.
%   Supported field are all fields of FI.Block(:).Info.
%   By default fieldname is 'P1', this allows to get
%   all analysis times [0 3 6 9 12 18] hr for one parameter, e.g.
%     ind.p = grib('find',FI,'ParamID', 1,'LevelID',105,'OK',1);
%     P1    = grib('meta',FI,ind.p,'fieldname','P1');
%
%   Example:
%
%        FI = grib('open','HIRLAM_SURFACE_1997010200.grib')
%     ind.p = grib('find',FI,'ParamID', 1,'LevelID',105,'P1',0,'OK',1);
%     ind.u = grib('find',FI,'ParamID',33,'LevelID',105,'P1',0,'OK',1);
%     ind.v = grib('find',FI,'ParamID',34,'LevelID',105,'P1',0,'OK',1);
%
%             grib('list',FI,[ind.p ind.u ind.v]);
%
%     [D.p,D.lat,D.lon,D.t] = grib('read',FI,ind.p);
%     [U.u,U.lat,U.lon,U.t] = grib('read',FI,ind.u); % NB staggered grid
%     [V.v,V.lat,V.lon,V.t] = grib('read',FI,ind.v); % NB staggered grid
%
%     pcolor(D.lon,D.lat,D.p);shading interp
%     hold on
%     quiver(D.lon,D.lat,.1.*U.u,.1.*V.v,0,'k'); % neglected staggering & global velocity orientation.
%     title(datestr(D.t))
%
%   See also: QPFOPEN, QPREAD

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

%% cases
switch lower(cmd)
    case 'open'
         varargout{1}          = grib_open(varargin{:});
    case 'read'
        [varargout{1:nargout}] = grib_read(1,varargin{:});
    case 'readdata'
        [varargout{1:nargout}] = grib_read(0,varargin{:});
    case 'disp'
        [varargout{1:nargout}] = grib_disp(varargin{:},'type','disp');
    case 'list'
        [varargout{1:nargout}] = grib_disp(varargin{:},'type','list');
    case 'meta'
        [varargout{1:nargout}] = grib_disp(varargin{:},'type','meta');
    case 'find'
        [varargout{1:nargout}] = grib_find(varargin{:});
    case 'time'
        varargout{1} = grib_time(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd))
end

%% grib_read
function [Values,lat,lon,Date,TRI] = grib_read(returngrid,FI,block)
%GRIB_READ Implements GRIB('read',...) call.
%   [VALUES,LATITUDE,LONGITUDE,Date] = GRIB_READ(FI,BLOCK) reads the selected
%   data block from the GRIB file and returns the latitude and longitude
%   coordinates and the time (reference + P1).

if block>length(FI.Block)
    error('Block number does not exist in "%s"',FI.FileName)
end
Info = FI.Block(block).Info;
Grid = Info.Grid;

% time is reference time + time range prediction
Date = grib_time(Info);
%
fid = fopen(FI.FileName,'r','b');
BitMask = [];
if isfield(Grid,'BitMaskID')
    if Grid.BitMaskID==0
        
        %% Jump to beginning of bit mask
        fseek(fid,Info.Section(3).Start+6,-1);
        
        %% Read bit mask
        if length(Grid.Ni)==1
            BitMask = logical(fread(fid,[Grid.Ni Grid.Nj],'*ubit1'));
        else
            BitMask = logical(fread(fid,sum(Grid.Ni),'*ubit1'));
        end
    else
        fclose(fid);
        error('Bit Map %i required for reading data.',Grid.BitMaskID)
    end
end
%
% Jump to beginning of data values
% For some reason it doesn't work if we jump to the beginning of values
% directly, but the following approach seems to work: jump to beginning of
% section and read the last couple of bytes.
%
fseek(fid,Info.Section(end).Start,-1);
fread(fid,11,'uchar');
if ~isempty(BitMask)
    NVal = sum(BitMask(:));
elseif length(Grid.Ni)==1
    NVal = Grid.Ni*Grid.Nj;
else
    NVal = sum(Grid.Ni);
end
if Info.BitsPerValue==0
    rawValues = zeros(NVal,1);
else
    rawValues = fread(fid,NVal,sprintf('ubit%i',Info.BitsPerValue));
end
rawValues = Info.ScaleFactorD * (Info.MinimumValue + rawValues*Info.ScaleFactorE);
if ~isempty(BitMask)
    if length(Grid.Ni)==1
        Values = repmat(NaN,[Grid.Ni Grid.Nj]);
    else
        Values = repmat(NaN,sum(Grid.Ni),1);
    end
    Values(BitMask) = rawValues;
elseif length(Grid.Ni)==1
    Values = reshape(rawValues,[Grid.Ni Grid.Nj]);
else
    Values = rawValues;
end
%
fclose(fid);
if nargout<=1
    return
end

%% Generate grid
lat=[];
lon=[];
TRI=[];
if returngrid
    switch Grid.BaseTypeID
        case {0,4}
            % 0 = Latitude/longitude grid - equidistant cylindrical or Plate Carrée projection'
            % 4 = Gaussian latitude/longitude grid
            lat1 = Grid.Latitude1/1000;
            lat2 = Grid.Latitude2/1000;
            lon1 = Grid.Longitude1/1000;
            lon2 = Grid.Longitude2/1000;
            if length(Grid.Ni)==1
                %Grid.Dj = (Grid.Latitude2-Grid.Latitude1)/(Grid.Nj-1)
                dj = (0:Grid.Nj-1)/(Grid.Nj-1);
                %Grid.Di = (Grid.Longitude2-Grid.Longitude1)/(Grid.Ni-1)
                di = (0:Grid.Ni-1)'/(Grid.Ni-1);
                lat = repmat(lat1+(lat2-lat1)*dj,Grid.Ni,1      );
                lon = repmat(lon1+(lon2-lon1)*di,1      ,Grid.Nj);
            else
                lat = zeros(1,sum(Grid.Ni));
                lon = zeros(1,sum(Grid.Ni));
                offset = 0;
                for jLat = 1:Grid.Nj
                    Ni = Grid.Ni(jLat);
                    lat(offset+(1:Ni)) = lat1+(lat2-lat1)*(jLat-1)/(Grid.Nj-1);
                    lon(offset+(1:Ni)) = lon1+(lon2-lon1)*(0:Ni-1)/(Ni-1);
                    offset = offset+Ni;
                end
                %
                nTRIperLine = (Grid.Ni(1:end-1)+Grid.Ni(2:end)-2).*(Grid.Ni(1:end-1)>0).*(Grid.Ni(2:end)>0);
                TRI = zeros(sum(nTRIperLine),3);
                offset = 0;
                iTRI = 1;
                for jLat = 1:Grid.Nj-1
                    Ni = Grid.Ni(jLat);
                    Ni2 = Grid.Ni(jLat+1);
                    if Ni==0 || Ni2==0
                        continue
                    end
                    offset2 = offset+Ni;
                    i1 = offset+1;
                    i2 = offset2+1;
                    while i1<offset+Ni || i2<offset2+Ni2
                        if i2==offset2+Ni2 || ...
                                (i1<offset+Ni && (lon(i1+1)<lon(i2+1) || ...
                                (lon(i1+1)==lon(i2+1) && lat(i1)>0)))
                            TRI(iTRI,:) = [i1 i2 i1+1];
                            i1 = i1+1;
                        else
                            TRI(iTRI,:) = [i1 i2 i2+1];
                            i2 = i2+1;
                        end
                        iTRI = iTRI+1;
                    end
                    offset = offset+Ni;
                end
            end
        otherwise
            warning('Coordinates for grid type "%s" not yet supported.',Grid.TypeName)
            lat = [];
            lon = [];
    end
    
    %% If appropriate, rotate grid
    if Grid.Rotated
        [lon,lat]=qp_proj_rotatepole(lon,lat,Grid.LongitudeSP/1000,Grid.LatitudeSP/1000,1);
    end
end

%% grib_open
function FI = grib_open(filename,debug)
%GRIB_OPEN Implements GRIB('open',...) call.
%   FI = GRIB_OPEN(FILENAME,DEBUG) opens the selected file and determines
%   the file contents. The optional DEBUG flag will be used as file
%   identifier to write debug messages to.

FI.FileName = filename;
FI.FileType = 'GRIB';
FI.Block    = []; % create empty block, so grib call goes well even if file has size 0
FI.OK       = 1;  % -1/-2/-3 error finding/opening/reading  or interpreting
FI.Error    = '';
%
% Debug
%
if nargin==1
    debug = 0;
end
%
% Open file
%
dprintf(debug,'Trying to open "%s" (big-endian) ...\n',filename);
%
tmp = dir(filename);
if isempty(tmp)
    FI.OK       = -1; % error finding
    FI.Error    = sprintf('File cannot be found: ''%s'' \n',filename);
elseif tmp.bytes==0
    FI.OK       = -3; % error reading
    FI.Error    = sprintf('File size is 0:  ''%s'' \n',filename); % e.g. \\knmidata\data\hirlam\HIRLAM_SURFACE_2005062900.grib
else    
fid = fopen(filename,'r','b');
if fid==-1
    FI.OK       = -2; % error opening
    FI.Error    = sprintf('File cannot be opened:  ''%s'' \n',filename);
else
if fid<0
    error('Unable to open file ''%s''',filename)
end
block = 1;
%
dprintf(debug,'File opened successfully, trying to identify GRIB blocks\n');
%
while 1
    %
    % Read "Indicator Section" (Section 0)
    %
    % Verify whether first four characters match 'GRIB'. I don't know why
    % some blocks start after a couple of null characters.
    %
    type = 0;
    nempty = 0;
    while isequal(type,0)
        nempty = nempty+1;
        type = fread(fid,1,'uchar');
    end
    %
    if nempty>1
        dprintf(debug,'Skipping %i null characters\n',nempty-1);
    end
    start = ftell(fid)-1;
    type = char([type fread(fid,[1 3],'uchar')]);
    %
    if feof(fid)
        dprintf(debug,'End-of-file reached\n');
        dprintf(debug,'%i GRIB blocks encountered in the file.\n\n',length(FI.Block));
        break
    else
        if block>1
            dprintf(debug,'\n');
        end
        dprintf(debug,'First 4 characters "%s" should correspond to "GRIB": ',type);
        %
        if strcmp(type,'GRIB')
            dprintf(debug,'OK\n');
        else
            dprintf(debug,'NotOK\n');
            fclose(fid);
            error('Block %i of ''%s'' starts with ''%s'' instead of ''GRIB''.',block,filename,type)
        end
    end
    %
    % Read length and edition
    %
    Length = fread(fid,1,'ubit24');
    Edition = fread(fid,1,'uchar');
    dprintf(debug,'Edition=%i\nLength=%i\n',Edition,Length);
    switch Edition
        case 0
            % Not supported
            error('Encountered unsupported GRIB "Edition 0" block.')
        case 1
            % OK
            LenSec0 = 8;
        case 2
            Length = fread(fid,1,'uint64');
            LenSec0 = 16;
        otherwise
            warning('Block %i of ''%s'': GRIB edition %i unknown.',block,filename,Edition)
    end
    FI.Block(block).Edition = Edition;
    %
    % Skip to last section
    %
    dprintf(debug,'Skipping to end of block ...\n');
    fseek(fid,Length-LenSec0-4,0);
    %
    % Read "End Section"
    %
    % Verify whether last four characters match '7777'
    %
    finish = char(fread(fid,[1 4],'uchar'));
    dprintf(debug,'Last 4 characters "%s" should correspond to "7777": ',finish);
    if strcmp(finish,'7777')
        dprintf(debug,'OK\n');
    else
        dprintf(debug,'NotOK\n');
        %
        % This may be a GRIB Edition -1 file (see CDI source code for support)
        %

       %fclose(fid); % happens at end of grib_open
       %error('Block %i in ''%s'' ends on ''%s'' instead of ''7777''.',block,filename,finish)
        FI.Error    = sprintf('Block %i in ''%s'' ends on ''%s'' instead of ''7777''. \n',block,filename,finish); 
        FI.Block(block) = []; % remove last erronous block, but keep any previous blocks that are possibly correct (as block 1:16 in \\knmidata\data\hirlam\HIRLAM_SURFACE_2005062906.grib)
        FI.OK           = -3; % set error code for error reading
        break
    end
    FI.Block(block).Start = start;
    FI.Block(block).End   = ftell(fid);
    block = block+1;
end
%
% Process blocks
%
for block = 1:length(FI.Block)
    dprintf(debug,'Block %i\n',block);
    switch FI.Block(block).Edition
        case 1
            fseek(fid,FI.Block(block).Start+8,-1);
            End = FI.Block(block).End-4;
            FI.Block(block).Info = parse_edition_1_block(fid,End,debug);
        case 2
            fseek(fid,FI.Block(block).Start+16,-1);
            End = FI.Block(block).End-4;
            FI.Block(block).Info = parse_edition_2_block(fid,End,debug);
    end
    dprintf(debug,'\n');

    %% Apply some checks to distinguish good data from data with issues
    %  for use in automatic processing.
    FI.Block(block).Info.OK = 1;
    if FI.Block(1).Edition>=2
        FI.Block(block).Info.OK = 0;
    elseif FI.Block(block).Info.BitsPerValue==0
        FI.Block(block).Info.OK = 0;
    %elseif % next issue
    %elseif % another issue
    end
end
%
dprintf(debug,'Finished reading file\n');
fclose(fid);
end % fid==-1
end % dir

%% parse_edition_1_block
function Info = parse_edition_1_block(fid,End,debug)
%PARSE_EDITION_1_BLOCK Reads the parameters from an edition 1 block.
%   INFO = PARSE_EDITION_1_BLOCK(FID,END,DEBUG) reads the parameters from
%   the various sections in the specified file. The file position indicator
%   is located at the end of section 0. The final section starts at the
%   location indicated by END. The DEBUG argument will be used as file
%   identifier to which debug messages will be written.

% Parse "Identification Section" (Section 1)
%
Section(1).Start = ftell(fid);
Section(1).Type  = 1;
dprintf(debug,'Section 1 at offset=%i is of type 1\n',Section(1).Start);
%
Info.Message = {};
%
% octet 1-3: Length of section
Len = fread(fid,1,'ubit24');
Section(1).Length = Len;
dprintf(debug,'Length=%i\n',Len);
%
% octet 4: GRIB tables Version No. (currently 3 for international exchange) - Version numbers 128-254 are reserved for local use
Info.GribTablesVersion = fread(fid,1,'uchar');
%
% octet 5: Identification of originating/generating centre (see Code table 0 = Common Code Table C-1 in Part C/c.)
Info.CentreID = fread(fid,1,'uchar');
Info.CentreName = center(Info.CentreID);
%
% octet 6: Generating process identification number (allocated by originating centre)
Info.ProcessID = fread(fid,1,'uchar');
%
% octet 7: Grid definition (Number of grid used - from catalogue defined by originating centre)
GridID = fread(fid,1,'uchar');
if GridID~=255
    %
    % 255 represents a "non-catalogued grid", i.e. a grid without an ID
    %
    Info.GridID = GridID;
end
%
% octet 8: Flag (see Regulation 92.3.2 and Code table 1)
Flag = fread(fid,1,'uchar');
Flag2 = bitget(Flag,8);
Flag3 = bitget(Flag,7);
%
% octet 9: Indicator of parameter (see Code table 2)
Info.ParamID = fread(fid,1,'uchar');
%
% octet 10: Indicator of type of level (see Code table 3)
Info.LevelID = fread(fid,1,'uchar');
[v1,v2,v3] = vcoord(Info.LevelID);
Info.LevelName = v1;
%
% octet 11-12: Height, pressure, etc. of levels (see Code table 3)
if isempty(v2) & isempty(v3)
    HPLevel = fread(fid,2,'uchar');
    Info.Level = [];
elseif isempty(v3)
    Info.Level.Description = v2;
    Info.Level.Value = fread(fid,1,'uint16');
else
    HPLevel = fread(fid,2,'uchar');
    Info.Level(1).Description = v2;
    Info.Level(1).Value = HPLevel(1);
    Info.Level(2).Description = v3;
    Info.Level(2).Value = HPLevel(2);
end
%
% octet 13: Year of century - Reference time of data - date and time of start of averaging or accumulation period
Year = fread(fid,1,'uchar');
%
% octet 14: Month - Reference time of data - date and time of start of averaging or accumulation period
Month = fread(fid,1,'uchar');
%
% octet 15: Day - Reference time of data - date and time of start of averaging or accumulation period
Day = fread(fid,1,'uchar');
%
% octet 16: Hour - Reference time of data - date and time of start of averaging or accumulation period
Hour = fread(fid,1,'uchar');
%
% octet 17: Minute - Reference time of data - date and time of start of averaging or accumulation period
Minute = fread(fid,1,'uchar');
%
% octet 18: Indicator of unit of time range (see Code table 4)
Info.TimeUnitID = fread(fid,1,'uchar');
[Info.TimeUnitName,Info.TimeUnitDays,Info.TimeUnitMonths,Info.TimeUnitYears] = tunit(Info.TimeUnitID);
%
% octet 19: P1 - Period of time (number of time units) (0 for analyses or initialized analyses). Units of time given by octet 18
P1 = fread(fid,1,'uchar');
%
% octet 20: P2 - Period of time (number of time units); or Time interval between successive analyses, initialized analyses or forecasts, undergoing averaging or accumulation. Units of time given by octet 18
P2 = fread(fid,1,'uchar');
%
% octet 21: Time range indicator (see Code table 5)
Info.TimeRangeID = fread(fid,1,'uchar');
Info.TimeRangeDescription = trange(Info.TimeRangeID);
%
if Info.TimeRangeID==10
    Info.P1 = P2*256+P1; % big-endian
elseif ~isempty(strfind(Info.TimeRangeDescription,'P1'))
    Info.P1 = P1;
end
if ~isempty(strfind(Info.TimeRangeDescription,'P2'))
    Info.P2 = P2;
end
%
% octet 22-23: Number included in average, when octet 21 (Code table 5) indicates an average or accumulation; otherwise set to zero
NInclAvg = fread(fid,1,'uint16');
%
% octet 24: Number missing from averages or accumulations
NMissAvg = fread(fid,1,'uchar');
%
if ~isempty(strfind(Info.TimeRangeDescription,' N '))
    Info.NValIncluded = NInclAvg;
    Info.NValMissing  = NMissAvg;
end
%
% octet 25: Century of reference time of data
Century = fread(fid,1,'uchar');
sCentury = sign(Century);
Century = abs(Century);
if Year==100
    Century = Century+1;
    Year = 0;
end
Info.Date = sCentury * datenum((Century-1)*100+Year,Month,Day,Hour,Minute,0);
%
% octet 26: Sub-centre identification (see common Code table C-1 in Part C/c., Note (3))
Info.SubCentreID = fread(fid,1,'uchar');
%
% octet 27-28: Units decimal scale factor (D)
Info.ScaleFactorD = 10^(-getbit16(fid,1));
%
% skip remainder
% octet 29-40: Reserved: need not be present
% octot 41-nn: Reserved for originating centre use
fseek(fid,Len-28,0);
%
[Info.ParamName,Info.ParamUnit] = parameter(Info.CentreID,Info.SubCentreID,Info.GribTablesVersion,Info.ParamID);
dprintf(debug,'Parameter: %s (%s)\n',Info.ParamName,Info.ParamUnit);
%
sections = [2 3 4];
if ~Flag2
    sections(sections==2)=[];
end
if ~Flag3
    sections(sections==3)=[];
end
for i=1:length(sections)
    Section(1+i).Start = ftell(fid);
    Section(1+i).Type  = sections(i);
    dprintf(debug,'Section %i at offset=%i is of type %i\n',1+i,Section(1+i).Start,sections(i));
    %
    % octet 1-3: Length of section
    Len = fread(fid,1,'ubit24');
    Section(1+i).Length = Len;
    dprintf(debug,'Length=%i\n',Len);
    %
    switch sections(i)
        case 2
            %
            % octet 4: NV - number of vertical coordinate parameters
            NV = fread(fid,1,'uchar');
            %
            % octet 5: PV - location (octet number) of the list of vertical coordinate parameters, if present; or
            %          PL - location (octet number) of the list of numbers of points in each row (if no vertical coordinate parameters are present), if present; or
            %          255 (all bits set to 1) if neither are present
            if NV>0
                PV = fread(fid,1,'uchar');
                PL = PV + 4*NV;
            else
                PV = -1;
                PL = fread(fid,1,'uchar');
                if PL==255
                    PL = -1;
                end
            end
            %
            % octet 6: Data representation type (see Code table 6)
            Grid.TypeID = fread(fid,1,'uchar');
            [Grid.TypeName,Grid.BaseTypeID,Grid.Rotated,Grid.Stretched,Grid.SpaceView] = gridtype(Grid.TypeID);
            %
            % octet 7-32: Grid definition (according to data representation type - octet 6 above)
            switch Grid.BaseTypeID
                case {0,1,4}
                    % 0 = Latitude/longitude grid - equidistant cylindrical or Plate Carrée projection'
                    % 1 = Mercator projection
                    % 4 = Gaussian latitude/longitude grid
                    %
                    % octet 7-8: Ni - number of points along a parallel
                    Grid.Ni = fread(fid,1,'uint16');
                    if Grid.Ni==65535
                        Grid.Ni = [];
                    end
                    %
                    % octet 9-10: Nj - number of points along a meridian
                    Grid.Nj = fread(fid,1,'uint16');
                    %       
                    % octet 11-13: La1 - latitude of first grid point (millidegrees)
                    Grid.Latitude1 = getbit24(fid,1);
                    %
                    % octet 14-16: Lo1 - longitude of first grid point (millidegrees)
                    Grid.Longitude1 = getbit24(fid,1);
                    %
                    % octet 17: Resolution and component flags (see Code table 7)
                    Grid = grib_resolutionflags(Grid,fid);
                    %
                    % octet 18-20: La2 - latitude of last grid point
                    Grid.Latitude2 = getbit24(fid,1);
                    %
                    % octet 21-23: Lo2 - longitude of last grid point
                    Grid.Longitude2 = getbit24(fid,1);
                    %
                    if Grid.BaseTypeID==1
                        % Mercator projection
                        %
                        % octet 24–26: Latin – latitude(s) at which the Mercator projection cylinder intersects the Earth
                        Grid.LatitudeIntersection = getbit24(fid,1);
                        %
                        % octet 27: Set to zero (reserved)
                        Zero = fread(fid,1,'uchar');
                        %
                        % octet 28: Scanning mode (flags – see Flag/Code table 8)
                        Grid = grib_scanmode(Grid,fid);
                        %
                        % octet 29–31: Di – longitudinal direction grid length (see Note 2)
                        Grid.Di = getbit24(fid,1);
                        %
                        % octet 32–34: Dj – latitudinal direction grid length (see Note 2)
                        Grid.Di = getbit24(fid,1);
                        %
                        % octet 35–42: Set to zero (reserved)
                        Zero = fread(fid,8,'uchar');
                    else
                        % octet 24-25: Di - i direction increment
                        Grid.Di = getbit16(fid,1);
                        %
                        if Grid.BaseTypeID==0
                            % octet 26-27: Dj - j direction increment
                            Grid.Dj = fread(fid,1,'uint16');
                        else
                            % octet 26-27: N - number of parallels between a pole and the equator
                            Grid.NParallels = fread(fid,1,'uint16');
                        end
                        %
                        % octet 28: Scanning mode (flags - see Flag/Code table 8)
                        Grid = grib_scanmode(Grid,fid);
                        %
                        % octet 29-32: Set to zero (reserved)
                        Zero = fread(fid,4,'uchar');
                    end
                case 2 % Gnomonic projection
                    Info.Message{end+1,1} = sprintf('Grid type "%s" not yet supported.',Grid.TypeName);
                    fseek(fid,Section(1+i).Start+Len,-1);
                    continue
                case {3,5}
                    % 3 = Lambert conformal, secant or tangent, conic or bi-polar, projection
                    % 5 = Polar stereographic projection
                    %
                    % octet 7-8: Nx – number of points along x-axis
                    Grid.Nx = fread(fid,1,'uint16');
                    %
                    % octet 9–10: Ny – number of points along y-axis
                    Grid.Ny = fread(fid,1,'uint16');
                    %
                    % octet 11–13: La1 – latitude of first grid point
                    Grid.Latitude1 = getbit24(fid,1);
                    %
                    % octet 14–16: Lo1 – longitude of first grid point
                    Grid.Longitude1 = getbit24(fid,1);
                    %
                    % octet 17: Resolution and component flags (see Code table 7)
                    Grid = grib_resolutionflags(Grid,fid);
                    %
                    % octet 18–20: LoV – orientation of the grid; i.e. the longitude value of the meridian which is parallel to
                    %                    the y-axis (or columns of the grid) along which latitude increases as the
                    %                    Y-coordinate increases (the orientation longitude may or may not appear on a
                    %                    particular grid)
                    Grid.LongitudeMeridian = getbit24(fid,1);
                    %
                    % octet 21–23: Dx – X-direction grid length (see Note 2)
                    Grid.Dx = getbit24(fid,1);
                    %
                    % octet 24–26: Dy – Y-direction grid length (see Note 2)
                    Grid.Dy = getbit24(fid,1);
                    %
                    % octet 27: Projection centre flag (see Note 5)
                    Flag = fread(fid,1,'uchar');
                    % bit 1: 0 North Pole is on the projection plane
                    %        1 South Pole is on the projection plane
                    Pole = {'North','South'};
                    Grid.PolePlane = Pole{bitget(Flag,8)+1};
                    %
                    % octet 28: Scanning mode (flags – see Flag/Code table 8)
                    Grid = grib_scanmode(Grid,fid);
                    %
                    if Grid.BaseTypeID==3
                        % octet 29–31: Latin 1 – first latitude from the pole at which the secant cone cuts the sphere
                        Grid.LatitudeIntersection1 = getbit24(fid,1);
                        %
                        % octet 32–34: Latin 2 – second latitude from the pole at which the secant cone cuts the sphere
                        Grid.LatitudeIntersection2 = getbit24(fid,1);
                        %
                        % octet 35–37: Latitude of the southern pole in millidegrees (integer)
                        Grid.LatitudeSP = getbit24(fid,1);
                        %
                        % octet 38–40: Longitude of the southern pole in millidegrees (integer)
                        Grid.LongitudeSP = getbit24(fid,1);
                        %
                        % octet 41–42: Set to zero (reserved)
                        Zero = fread(fid,2,'uchar');
                    else
                        % octet 29–32: Set to zero (reserved)
                        Zero = fread(fid,4,'uchar');
                    end
                case 6 % Universal Transverse Mercator (UTM) projection
                    Info.Message{end+1,1} = sprintf('Grid type "%s" not yet supported.',Grid.TypeName);
                    fseek(fid,Section(1+i).Start+Len,-1);
                    continue
                case 7 % Simple polyconic projection
                    Info.Message{end+1,1} = sprintf('Grid type "%s" not yet supported.',Grid.TypeName);
                    fseek(fid,Section(1+i).Start+Len,-1);
                    continue
                case 8 % Albers equal-area, secant or tangent, conic or bi-polar, projection
                    Info.Message{end+1,1} = sprintf('Grid type "%s" not yet supported.',Grid.TypeName);
                    fseek(fid,Section(1+i).Start+Len,-1);
                    continue
                case 9 % Miller's cylindrical projection
                    Info.Message{end+1,1} = sprintf('Grid type "%s" not yet supported.',Grid.TypeName);
                    fseek(fid,Section(1+i).Start+Len,-1);
                    continue
                case 13 % Oblique Lambert conformal, secant or tangent, conic or bi-polar, projection
                    Info.Message{end+1,1} = sprintf('Grid type "%s" not yet supported.',Grid.TypeName);
                    fseek(fid,Section(1+i).Start+Len,-1);
                    continue
                case 50 % Spherical harmonic coefficients
                    %
                    % octet 7-8: J – pentagonal resolution parameter
                    Grid.J = fread(fid,1,'uint16');
                    %
                    % octet 9-10: K – pentagonal resolution parameter
                    Grid.K = fread(fid,1,'uint16');
                    %
                    % octet 11-12: M – pentagonal resolution parameter
                    Grid.M = fread(fid,1,'uint16');
                    %
                    % octet 13: Representation type
                    Grid.RepType = fread(fid,1,'uchar');
                    %
                    % octet 14: Representation mode
                    Grid.RepMode = fread(fid,1,'uchar');
                    %
                    % octet 15-32: Set to zero (reserved)
                    Zero = fread(fid,18,'uchar');
                case 90 % Space view, perspective or orthographic                    
                    %
                    % octet 7–8: Nx – number of points along x-axis (columns)
                    Grid.Nx = fread(fid,1,'uint16');
                    %
                    % octet 9–10: Ny – number of points along y-axis (rows or lines)
                    Grid.Ny = fread(fid,1,'uint16');
                    %
                    % octet 11–13: Lap – latitude of sub-satellite point
                    Grid.LatitudeSatellite = getbit24(fid,1);
                    %
                    % octet 14–16: Lop – longitude of sub-satellite point
                    Grid.LongitudeSatellite = getbit24(fid,1);
                    %
                    % octet 17: Resolution and component flags (see Code table 7)
                    Grid = grib_resolutionflags(Grid,fid);
                    %
                    % octet 18–20: dx – apparent diameter of Earth in grid lengths, in x-direction
                    Grid.Dx = getbit24(fid,1);
                    %
                    % octet 21–23: dy – apparent diameter of Earth in grid lengths, in y-direction
                    Grid.Dy = getbit24(fid,1);
                    %
                    % octet 24–25: Xp – x-coordinate of sub-satellite point
                    Grid.XSatellite = getbit16(fid,1);
                    %
                    % octet 26–27: Yp – y-coordinate of sub-satellite point
                    Grid.YSatellite = getbit16(fid,1);
                    %
                    % octet 28: Scanning mode (flags – see Flag/Code table 8)
                    Grid = grib_scanmode(Grid,fid);
                    %
                    % octet 29–31: Orientation of the grid; i.e. the angle in millidegrees between the increasing y-axis and the
                    %              meridian of the sub-satellite point in the direction of increasing latitude (see Note 3)
                    Grid.Angle = getbit24(fid,1);
                    %
                    % octet 32–34: Nr – altitude of the camera from the Earth’s centre, measured in units of the Earth’s
                    %             (equatorial) radius (see Note 4)
                    Grid.Altitude = getbit24(fid,1);
                    %
                    % octet 35–36: Xo – x-coordinate of origin of sector image
                    Grid.XOrigin = getbit16(fid,1);
                    %
                    % octet 37–38: Yo – y-coordinate of origin of sector image
                    Grid.YOrigin = getbit16(fid,1);
                    %
                    % octet 39–44: Set to zero (reserved)
                    Zero = fread(fid,6,'uchar');
                otherwise
                    Info.Message{end+1,1} = sprintf('Grid type "%s" not yet supported.',Grid.TypeName);
                    fseek(fid,Section(1+i).Start+Len,-1);
                    continue
            end
            %
            if Grid.Rotated && Grid.Stretched
                %
                % octet 33-35: Latitude of the southern pole in millidegrees (integer)
                Grid.LatitudeSP = getbit24(fid,1);
                %
                % octet 36-38: Longitude of the southern pole in millidegrees (integer)
                Grid.LongitudeSP = getbit24(fid,1);
                %
                % octet 39-42: Angle of rotation (represented in the same way as the reference value)
                Grid.Rotation = getfloat(fid,1);
                %
                % octet 43-45: Latitude of pole of stretching in millidegrees (integer)
                Grid.LatitudeStretch = getbit24(fid,1);
                %
                % octet 46-48: Longitude of pole of stretching in millidegrees (integer)
                Grid.LongitudeStretch = getbit24(fid,1);
                %
                % octet 49-52: Stretching factor (representation as for the reference value)
                Grid.StretchFactor = getfloat(fid,1);
            elseif Grid.Rotated
                %
                % octet 33-35: Latitude of the southern pole in millidegrees (integer)
                Grid.LatitudeSP = getbit24(fid,1);
                %
                % octet 36-38: Longitude of the southern pole in millidegrees (integer)
                Grid.LongitudeSP = getbit24(fid,1);
                %
                % octet 39-42: Angle of rotation (represented in the same way as the reference value)
                Grid.Rotation = getfloat(fid,1);
            elseif Grid.Stretched
                %
                % octet 33-35: Latitude of pole of stretching in millidegrees (integer)
                Grid.LatitudeStretch = getbit24(fid,1);
                %
                % octet 36-38: Longitude of pole of stretching in millidegrees (integer)
                Grid.LongitudeStretch = getbit24(fid,1);
                %
                % octet 39-42: Stretching factor (representation as for the reference value)
                Grid.StretchFactor = getfloat(fid,1);
            end
            %
            if PV>0
                %
                % octet PV: List of vertical coordinate parameters (length = NV × 4 octets); if present, then PL = 4NV + PV
                Grid.VCoord = getfloat(fid,NV);
            end
            if PL>0 && isempty(Grid.Ni)
                %
                % octet PL: List of numbers of points in each row (length = NROWS x 2 octets, where NROWS is the total number of rows defined within the grid description)
                Grid.Ni = fread(fid,Grid.Nj,'uint16');
            end
        case 3
            %
            % octet 4: Number of unused bits at end of Section 3
            NBits = fread(fid,1,'uchar');
            %
            % octet 5-6: Table reference:
            %            If the octets contain zero, a bit-map follows
            %            If the octets contain a number, it refers to a predetermined bit-map provided by the centre
            Grid.BitMaskID = fread(fid,1,'uint16');
            %
            % octet 7-n: The bit-map - contiguous bits with a bit to data point correspondence, ordered as defined in the grid definition
            if Grid.BitMaskID==0
                %
                % skip bit mask
                %
                % Grid.BitMask = logical(fread(fid,[Grid.Ni Grid.Nj],'*ubit1'));
                fseek(fid,Section(1+i).Start+Section(1+i).Length,-1);
            end
        case 4
            %
            % octet 4: Flag (see Code table 11) (first 4 bits). Number of unused bits at end of Section 4 (last 4 bits)
            Flag = fread(fid,2,'ubit4');
            NBits = Flag(2);
            Flag = Flag(1);
            %
            Bit1 = bitget(Flag,4); % 0=grid point, 1=spherical harmonics
            Bit2 = bitget(Flag,3); % 0=simple packing, 1=2nd order packing (bit1=0) or complex packing (bit1=1)
            Bit3 = bitget(Flag,2); % 0=float, 1=int
            Bit4 = bitget(Flag,1); % extra flags in octet 14 (0=no, 1=yes)
            %
            Info.PackingID = Bit1*2+Bit2;
            switch Info.PackingID
                case 0
                    Info.PackingName = 'Grid Point - Simple Packing';
                case 1
                    Info.PackingName = 'Grid Point - Second-order Packing';
                case 2
                    Info.PackingName = 'Spherical harmonics - Simple Packing';
                case 3
                    Info.PackingName = 'Spherical harmonics - Complex Packing';
            end
            %
            switch Bit3
                case 0
                    Info.ValueType = 'Floating Point';
                case 1
                    Info.ValueType = 'Integer';
            end
            %
            % octet 5-6: Scale factor (E)
            Info.ScaleFactorE = 2^getbit16(fid,1);
            %
            % octet 7-10: Reference value (minimum of packed values)
            Info.MinimumValue = getfloat(fid,1);
            if Bit3
                Info.MinimumValue = round(Info.MinimumValue);
            end
            %
            % octet 11: Number of bits containing each packed value
            Info.BitsPerValue = fread(fid,1,'uchar');
            %
            % octet 12-: Variable, depending on the flag value in octet 4
            %
            % skip data
            %
            fseek(fid,Section(1+i).Start+Section(1+i).Length,-1);
    end
end
%
finish = char(fread(fid,[1 4],'uchar'));
if ~strcmp(finish,'7777')
    dprintf(debug,'Last 4 characters "%s" should correspond to "7777"\n',finish);
    fclose(fid)
    error('Didn''t reach end section "7777" as expected.');
end
%
Info.Grid = Grid;
Info.Section = Section;

%% parse_edition_2_block
function Info = parse_edition_2_block(fid,End,debug)
%PARSE_EDITION_2_BLOCK Reads the parameters from an edition 2 block.
%   INFO = PARSE_EDITION_2_BLOCK(FID,END,DEBUG) reads the parameters from
%   the various sections in the specified file. The file position indicator
%   is located at the end of section 0. The final section starts at the
%   location indicated by END. The DEBUG argument will be used as file
%   identifier to which debug messages will be written.

% Parse all sections
%
Info.Message = {'Edition 2 data block not yet supported.'};
b = 1;
while 1
    Section(b).Start = ftell(fid);
    dprintf(debug,'Section %i at offset=%i',b,Section(b).Start);
    %
    Len = fread(fid,1,'uint32');
    Sec = fread(fid,1,'uchar');
    dprintf(debug,'is of type %i\nLength=%i\n',Sec,Len);
    fseek(fid,Len-5,0);
    %
    Section(b).Type = Sec;
    Section(b).Length = Len;
    %
    if ftell(fid)==End
        break
    else
        b = b+1;
    end
end
dprintf(debug,'A total of %i sections found\n',b);
Info.Section = Section;

%% Resolution and component flags (see Code table 7)
function Grid = grib_resolutionflags(Grid,fid)
Flag = fread(fid,1,'uchar');
% bit 1: 0 Direction increments not given
%        1 Direction increments given
% bit 2: 0 Earth assumed spherical with radius 6367.47 km
%        1 Earth assumed oblate spheroidal with size as determined by IAU in 1965 (6378.160 km, 6356.775 km, f = 1/297.0)
% bit 3-4: Reserved
% bit 5: 0 Resolved u- and v-components of vector quantities relative to easterly and northerly directions
%        1 Resolved u- and v-components of vector quantities relative to the defined grid in the direction of increasing x and y (or i and j) coordinates respectively
% bit 6-8: Reserved - set to zero
Grid.DiGiven = bitget(Flag,8);
Grid.EarthID = bitget(Flag,7);
earth = {'Spherical R=6367.47 km','Oblate spherical R=6378.160 km, f = 1/297.0'};
Grid.EarthDescription = earth{Grid.EarthID+1};
vecdir = {'East-North','I-J'};
Grid.VectorDir = vecdir{bitget(Flag,4)+1};

%% Scanning mode (flags - see Flag/Code table 8)
function Grid = grib_scanmode(Grid,fid)
% read scanmode octet and process its bits
ScanMode = fread(fid,1,'uchar');
% bit 1: 0 Points scan in +i direction
%        1 Points scan in -i direction
Grid.PointScanPositiveI = bitget(ScanMode,8);
% bit 2: 0 Points scan in -j direction
%        1 Points scan in +j direction
Grid.PointScanPositiveJ = 1-bitget(ScanMode,7);
% bit 3: 0 Adjacent points in i direction are consecutive
%        1 Adjacent points in j direction are consecutive
order = {'IJ','JI'};
Grid.PointScanOrder = order{1+bitget(ScanMode,8)};

%% grid_time
function Date = grib_time(Info)
switch Info.TimeRangeID
    case {6}
        % assumed valid at R - (P1+P2)/2
        P = -(Info.P1+Info.P2)/2;
    case {7}
        % assumed valid at R + (-P1+P2)/2
        P = (-Info.P1+Info.P2)/2;
    case {0,10,117}
        % assumed valid at R + P1
        P = Info.P1;
    case {2,3}
        % assumed valid at R + (P1+P2)/2
        P = (Info.P1+Info.P2)/2;
    case {4,5}
        % assumed valid at R + P2
        P = Info.P2;
    otherwise
        % assumed valid at R
        P = 0;
end

if ~isnan(Info.TimeUnitDays)
    Date = Info.Date + P*Info.TimeUnitDays;
else
    Date = num2cell(datevec(Info.Date));
    Date{2} = Date{2} + P*Info.TimeUnitMonths;
    Date{1} = Date{1} + P*Info.TimeUnitYears;
    Date = datenum(Date);
end


%% dprintf
function dprintf(debug,varargin)
if debug
    fprintf(debug,varargin{:});
end

%--------------------------------------------------------------------------
% Positive (unsigned) integers are handled correctly by fread
% However, GRIB stores negative integers and floating point in non-IEEE
% format. The following routines handle those cases: use them instead of
% the indicated fread statement.
%--------------------------------------------------------------------------
%% getbit24
function V = getbit24(fid,N)
%GETBIT24 Replacement for fread(fid,N,'bit24').
%   V = GETBIT24(FID,N) reads N 24 bit signed integers from the specified
%   file. Commonly negative integers are stored as the bitwise inverse of
%   their positive counterpart minus one, e.g. 10 and -10 are stored as
%       010100000000000000000000 (10,big endian)
%       110011111111111111111111 (-10,big endian)
%   In the GRIB files a negative value differs only from their positive
%   counterpart by the fact that the last bit will be set, i.e. -10 is
%   stored as
%       010100000000000000000001 (-10,big endian)

V = fread(fid,N,'ubit24');
sV = logical(bitget(V,24));
V = bitset(V,24,0);
V(sV) = -V(sV);

%% getbit16
function V = getbit16(fid,N)
%GETBIT16 Replacement for fread(fid,N,'bit16').
%   V = GETBIT16(FID,N) reads N 16 bit signed integers from the specified
%   file. Commonly negative integers are stored as the bitwise inverse of
%   their positive counterpart minus one, e.g. 10 and -10 are stored as
%       0101000000000000 (10,big endian)
%       1100111111111111 (-10,big endian)
%   In the GRIB files a negative value differs only from their positive
%   counterpart by the fact that the last bit will be set, i.e. -10 is
%   stored as
%       0101000000000001 (-10,big endian)

V = fread(fid,N,'ubit16');
sV = logical(bitget(V,16));
V = bitset(V,16,0);
V(sV) = -V(sV);

%% getfloat
function V = getfloat(fid,N)
%GETFLOAT Replacement for fread(fid,N,'float32').
%   V = GETFLOAT(FID,N) reads N single precision floating point values from
%   the specified file. The GRIB files uses 24 bits for the fraction, 7
%   bits for the exponent, and 1 bit for the sign.
%
%       BBBBBBBBBBBBBBBBBBBBBBBBAAAAAAAS
%
%   The floating point value should be computed as (-1)^S x B x 16^(A-70)
%   which is often written as (-1)^S x 2^(-24) x B x 16^(A-64).

V = fread(fid,[4 N],'ubit8');
E = V(1,:)';
V = ([0 65536 256 1]*V)';
sV = logical(bitget(E,8));
E = bitset(E,8,0);
V = V .* 16.^(E-70);
V(sV) = -V(sV);

%--------------------------------------------------------------------------
% Table routines
%--------------------------------------------------------------------------

%% parameter
function [ParName,ParUnit] = parameter(CenterID,SubCenterID,TableVersion,ParamID)
persistent CodeTable
if isempty(CodeTable)
    CodeTable = readTables;
end
%
% Look at all list of tables
Table = CodeTable.TableID;
%
% Select only lines for this CenterID
Table = Table(Table(:,1)==CenterID,:);
if ~isempty(Table)
    %
    % Select only lines for this SubCenterID
    TableX = Table(Table(:,2)==SubCenterID,:);
    if ~isempty(TableX)
        Table = TableX;
    else
        %
        % Use default table instead
        Table = Table(Table(:,2)==-1,:);
    end
    %
    % Select only lines for this TableVersion
    TableX = Table(Table(:,3)==TableVersion,:);
    if ~isempty(TableX)
        Table = TableX;
    else
        %
        % Use default table instead
        Table = Table(Table(:,3)==-1,:);
    end
    %
    if ~isempty(Table)
        %
        % Specific table found
        iTable = Table(1,4);
        if isempty(CodeTable.Table{iTable})
            try
                Table = readNamesTable(CodeTable.Names{iTable});
                CodeTable.Table{iTable} = Table;
            catch
                Table = {};
            end
        else
            Table = CodeTable.Table{iTable};
        end
    end
end
if isempty(Table)
    %
    % No table found
    ParName = sprintf('Variable %i of Table %i of %s - Sub-Center %i', ...
        ParamID,TableVersion,center(CenterID),SubCenterID);
    ParUnit = '';
else
    %
    % Specific table found
    ParName = Table{ParamID+1};
    oBrack = strfind(ParName,'[');
    cBrack = strfind(ParName,']');
    if length(oBrack)==1 && length(cBrack)==1
        ParUnit = ParName(oBrack+1:cBrack-1);
        ParName = deblank(ParName(1:oBrack-1));
    end
end


%% readNamesTable
function Names = readNamesTable(File)
fid = fopen([basedir filesep 'grib' filesep File],'r','n','US-ASCII');
Names = {};
if fid>0
    Line = fgetl(fid);
    TableVersion = sscanf(Line,'%i:%i:%i:%i');
    Names = repmat({''},255,1);
    while ~feof(fid)
        Line = fgetl(fid);
        ival = sscanf(Line,'%i',1);
        colon = strfind(Line,':');
        if length(colon)>=2
            Names{ival+1} = Line(colon(2)+1:end);
        end
    end
    fclose(fid);
end


%%basedir
function dir = basedir
try
   dir = qp_basedir('deploy');
   % this is the correct location in MATLAB 2013b when the grib folder is
   % included in the compilation.
   if ~exist(fullfile(dir,'grib'),'dir')
       dir = fullfile(dir,'d3d_qp');
       % this is the correct location in MATLAB 2023a when the grib folder
       % is included in the compilation.
       if ~exist(fullfile(dir,'grib'),'dir')
           ui_message('warning','Unable to locate the grib mapping folder in "%s".',dir)
       end
   end
catch
    % this is the correct location in source code mode
   dir = fileparts(which(mfilename));
end


%% readTables
function Codes = readTables
fid = fopen([basedir filesep 'grib' filesep 'tablelookup.lst'],'r','n','US-ASCII');
Codes.TableID=zeros(0,3);
Codes.Names=cell(0,1);
if fid>0
    while ~feof(fid)
        Line = fgetl(fid);
        v=sscanf(Line,'%i:%i:%i:%s',[1 inf]);
        if length(v)<4
            break
        end
        Codes.TableID(end+1,1:3) = v(1:3);
        Codes.Names{end+1,1} = char(v(4:end));
    end
    fclose(fid);
end
nTables = size(Codes.TableID,1);
Codes.TableID(:,4) = (1:nTables)';
Codes.Table = cell(nTables,1);


%% center
function varargout = center(i,varargin)
persistent CodeTable
if nargin>1
    CodeTable(i+1,1:nargin-1) = varargin;
    return
elseif isempty(CodeTable)
    CodeTable = cell(256,1);
    centertable_default
end
varargout = CodeTable(i+1,:);

%% centertable_default
function centertable_default
for i=0:255
    center(i,sprintf('Center %i - Reserved for other centres',i));
end
center(0,'WMO Secretariat');
% 01-09: WMCs
center(1,'Melbourne');
center(2,'Melbourne');
center(3,')');
center(4,'Moscow');
center(5,'Moscow');
center(6,')');
center(7,'US National Weather Service, National Centres for Environmental Prediction (NCEP)');
center(8,'US National Weather Service TelecommunicationsGateway (NWSTG)');
center(9,'US National Weather Service - Other');
% 10-25: Centres in Region I
center(10,'Cairo (RSMC)');
center(11,')');
center(12,'Dakar (RSMC)');
center(13,')');
center(14,'Nairobi (RSMC)');
center(15,')');
center(16,'Casablanca (RSMC)');
center(17,'Tunis (RSMC)');
center(18,'Tunis Casablanca (RSMC)');
center(19,')');
center(20,'Las Palmas');
center(21,'Algiers (RSMC)');
center(22,'ACMAD');
center(23,'Mozambique NMC');
center(24,'Pretoria (RSMC)');
center(25,'La Réunion (RSMC)');
% 26-40: Centres in Region II
center(26,'Khabarovsk (RSMC)');
center(27,')');
center(28,'New Delhi (RSMC)');
center(29,')');
center(30,'Novosibirsk (RSMC)');
center(31,')');
center(32,'Tashkent (RSMC)');
center(33,'Jeddah (RSMC)');
center(34,'Tokyo (RSMC), Japan Meteorological Agency');
center(35,')');
center(36,'Bangkok');
center(37,'Ulan Bator');
center(38,'Beijing (RSMC)');
center(39,')');
center(40,'Seoul');
% 41-50: Centres in Region III
center(41,'Buenos Aires (RSMC)');
center(42,')');
center(43,'Brasilia (RSMC)');
center(44,')');
center(45,'Santiago');
center(46,'Brazilian Space Agency - INPE');
center(47,'Colombia NMC');
center(48,'Ecuador NMC');
center(49,'Peru NMC');
center(50,'Venezuela NMC');
% 51-63: Centres in Region IV
center(51,'Miami (RSMC)');
center(52,'Miami RSMC, National Hurricane Center');
center(53,'Montreal (RSMC)');
center(54,')');
center(55,'San Francisco');
center(56,'ARINC Centre');
center(57,'U.S. Air Force Air Force Global Weather Central');
center(58,'Fleet Numerical Meteorology and Oceanography Center, Monterey, CA');
center(59,'The NOAA Forecast Systems Laboratory, Boulder, CO, USA');
center(60,'United States National Centre for Atmospheric Research (NCAR)');
center(61,'Service ARGOS - Landover');
center(62,'U.S. Naval Oceanographic Office');
center(63,'IRI (International Research Institute for Climate and Society)');
% 64-73: Centres in Region V
center(64,'Honolulu (RSMC)');
center(65,'Darwin (RSMC)');
center(66,')');
center(67,'Melbourne (RSMC)');
center(68,'Reserved');
center(69,'Wellington (RSMC)');
center(70,')');
center(71,'Nadi (RSMC)');
center(72,'Singapore');
center(73,'Malaysia NMC');
% 74-99: Centres in Region VI
center(74,'UK Meteorological Office - Exeter (RSMC)');
center(75,')');
center(76,'Moscow (RSMC)');
center(77,'Reserved');
center(78,'Offenbach (RSMC)');
center(79,')');
center(80,'Rome (RSMC)');
center(81,')');
center(82,'Norrköping');
center(83,')');
center(84,'Toulouse (RSMC)');
center(85,'Toulouse (RSMC)');
center(86,'Helsinki');
center(87,'Belgrade');
center(88,'Oslo');
center(89,'Prague');
center(90,'Episkopi');
center(91,'Ankara');
center(92,'Frankfurt/Main)');
center(93,'London (WAFC)');
center(94,'Copenhagen');
center(95,'Rota');
center(96,'Athens');
center(97,'European Space Agency (ESA)');
center(98,'European Centre for Medium-range Weather Forecast (ECMWF), (RSMC)');
center(99,'De Bilt');
% Additional Centres
center(100,'Brazzaville');
center(101,'Abidjan');
center(102,'Libyan Arab Jamahiriya NMC');
center(103,'Madagascar NMC');
center(104,'Mauritius NMC');
center(105,'Niger NMC');
center(106,'Seychelles NMC');
center(107,'Uganda NMC');
center(108,'Tanzania NMC');
center(109,'Zimbabwe NMC');
center(110,'Hong-Kong, China');
center(111,'Afghanistan NMC');
center(112,'Bahrain NMC');
center(113,'Bangladesh NMC');
center(114,'Bhutan NMC');
center(115,'Cambodia NMC');
center(116,'Democratic People''s Republic of Korea NMC');
center(117,'Islamic Republic of Iran NMC');
center(118,'Iraq NMC');
center(119,'Kazakhstan NMC');
center(120,'Kuwait NMC');
center(121,'Kyrgyz Republic NMC');
center(122,'Lao People''s Democratic Republic NMC');
center(123,'Macao, China');
center(124,'Maldives NMC');
center(125,'Myanmar NMC');
center(126,'Nepal NMC');
center(127,'Oman NMC');
center(128,'Pakistan NMC');
center(129,'Qatar NMC');
center(130,'Republic of Yemen NMC');
center(131,'Sri Lanka NMC');
center(132,'Tajikistan NMC');
center(133,'Turkmenistan NMC');
center(134,'United Arab Emirates NMC');
center(135,'Uzbekistan NMC');
center(136,'Socialist Republic of Viet Nam NMC');
% 137:139
center(140,'Bolivia NMC');
center(141,'Guyana NMC');
center(142,'Paraguay NMC');
center(143,'Suriname NMC');
center(144,'Uruguay NMC');
center(145,'French Guyana');
center(146,'Brazilian Navy Hydrographic Centre');
center(147,'COmision Nacional de Actividades Espaciales (CONAE) - Argentina');
% 148:149
center(150,'Antigua and Barbuda NMC');
center(151,'Bahamas NMC');
center(152,'Barbados NMC');
center(153,'Belize NMC');
center(154,'British Caribbean Territories Centre');
center(155,'San Jose');
center(156,'Cuba NMC');
center(157,'Dominica NMC');
center(158,'Dominican Republic NMC');
center(159,'El Salvador NMC');
center(160,'US NOAA/NESDIS');
center(161,'US NOAA Office of Oceanic and Atmospheric Research');
center(162,'Guatemala NMC');
center(163,'Haiti NMC');
center(164,'Honduras NMC');
center(165,'Jamaica NMC');
center(166,'Mexico');
center(167,'Netherlands Antilles and Aruba NMC');
center(168,'Nicaragua NMC');
center(169,'Panama NMC');
center(170,'Saint Lucia NMC');
center(171,'Trinidad and Tobago NMC');
center(172,'French Departments in RA IV');
center(173,'US National Aeronautics and Space Administration (NASA)');
center(174,'Integrated System Data Management/Marine Environmental Data Service (ISDM/MEDS - Canada)');
% 175:189
center(190,'Cook Islands NMC');
center(191,'French Polynesia NMC');
center(192,'Tonga NMC');
center(193,'Vanuatu NMC');
center(194,'Brunei Darussalam NMC');
center(195,'Indonesia NMC');
center(196,'Kiribati NMC');
center(197,'Federated States of Micronesia NMC');
center(198,'New Caledonia NMC');
center(199,'Niue');
center(200,'Papua New Guinea NMC');
center(201,'Philippines NMC');
center(202,'Samoa NMC');
center(203,'Solomon Islands NMC');
% 204:209
center(210,'Frascati (ESA/ESRIN)');
center(211,'Lannion');
center(212,'Lisboa');
center(213,'Reykjavik');
center(214,'Madrid');
center(215,'Zürich');
center(216,'Service ARGOS Toulouse');
center(217,'Bratislava');
center(218,'Budapest');
center(219,'Ljubljana');
center(220,'Warsaw');
center(221,'Zagreb');
center(222,'Albania NMC');
center(223,'Armenia NMC');
center(224,'Austria NMC');
center(225,'Azerbaijan NMC');
center(226,'Belarus NMC');
center(227,'Belgium NMC');
center(228,'Bosnia and Herzegovina NMC');
center(229,'Bulgaria NMC');
center(230,'Cyprus NMC');
center(231,'Estonia NMC');
center(232,'Georgia NMC');
center(233,'Dublin');
center(234,'Israel NMC');
center(235,'Jordan NMC');
center(236,'Latvia NMC');
center(237,'Lebanon NMC');
center(238,'Lithuania NMC');
center(239,'Luxembourg');
center(240,'Malta NMC');
center(241,'Monaco');
center(242,'Romania NMC');
center(243,'Syrian Arab Republic NMC');
center(244,'The former Yugoslav Republic of Macedonia NMC');
center(245,'Ukraine NMC');
center(246,'Republic of Moldova NMC');
center(247,'Operational Programme for the Exchange of weather RAdar information (OPERA) - EUMETNET');
% 248:253
center(254,'EUMETSAT Operation Centre');
center(255,'Missing value');

%% vcoord
function varargout = vcoord(i,varargin)
persistent CodeTable
if nargin>1
    CodeTable(i+1,1:nargin-1) = varargin;
    return
elseif isempty(CodeTable)
    CodeTable = cell(256,3);
    vcoordtable_default
end
varargout = CodeTable(i+1,:);

%% vcoordtable_default
function vcoordtable_default
for i=0:254
    vcoord(i,sprintf('VCoord %i - Reserved',i),'','');
end
% 0: Reserved
vcoord(01,'Ground or water surface','','');
vcoord(02,'Cloud base level','','');
vcoord(03,'Level of cloud tops','','');
vcoord(04,'Level of 0°C isotherm','','');
vcoord(05,'Level of adiabatic condensation lifted from the surface','','');
vcoord(06,'Maximum wind level','','');
vcoord(07,'Tropopause','','');
vcoord(08,'Nominal top of atmosphere','','');
vcoord(09,'Sea bottom','','');
% 10-19: Reserved
vcoord(20,'Isothermal level','Temperature in 1/100 K','');
% 21-99: Reserved
vcoord(100,'Isobaric surface','Pressure in hPa','');
vcoord(101,'Layer between two isobaric surfaces','Pressure of top in kPa','Pressure of bottom in kPa');
vcoord(102,'Mean sea level','','');
vcoord(103,'Specified altitude above mean sea level','Altitude in metres','');
vcoord(104,'Layer between two specified altitudes above mean sea level','Altitude of top in hm','Altitude of bottom in hm');
vcoord(105,'Specified height level above ground','Height in metres','');
vcoord(106,'Layer between two specified height levels','Height of top in hm','Height of bottom in hm above ground');
vcoord(107,'Sigma level','Sigma value in 1/10 000','');
vcoord(108,'Layer between two sigma levels','Sigma value of top in 1/100','Sigma value of bottom in 1/100');
vcoord(109,'Hybrid level','Level number','');
vcoord(110,'Layer between two hybrid levels','Level number of top','Level number of bottom');
vcoord(111,'Depth below land surface','Depth in centimetres','');
vcoord(112,'Layer between two depths below land','Depth of upper surface in cm','Depth of lower surface in cm');
vcoord(113,'Isentropic (theta) level','Potential temperature in K','');
vcoord(114,'Layer between two isentropic levels','475 K minus theta of top in K','475 K minus theta of bottom in K');
vcoord(115,'Level at specified pressure difference','Pressure difference in hPa from ground to level','');
vcoord(116,'Layer between two levels at specified pressure differences from ground to level','Pressure difference from ground to top level in hPa','Pressure difference from ground to bottom level in hPa');
vcoord(117,'Potential vorticity surface','10^-9 K m2 kg-1 s-1','');
% 118: Reserved
vcoord(119,'ETA* level','ETA value in 1/10000','');
vcoord(120,'Layer between two ETA* levels','ETA value at top of layer in 1/100','ETA value at bottom of layer in 1/100');
vcoord(121,'Layer between two isobaric surfaces (high precision)','1100 hPa minus pressure of top in hPa','1100 hPa minus pressure of bottom in hPa');
% 122-124: Reserved
vcoord(125,'Specified height level above ground (high precision)','Height in centimetres','');
% 126-127: Reserved
vcoord(128,'Layer between two sigma levels (high precision)','1.1 minus sigma of top, in 1/1000 of sigma','1.1 minus sigma of bottom, in 1/1000 of sigma');
% 129-140: Reserved
vcoord(141,'Layer between two isobaric surfaces (mixed precision)','Pressure of top in kPa','1100 hPa minus pressure of bottom in hPa');
% 142-159: Reserved
vcoord(160,'Depth below sea level','Depth in metres','');
% 161-199: Reserved
vcoord(200,'Entire atmosphere (considered as a single layer)','','');
vcoord(201,'Entire ocean (considered as a single layer)','','');
% 202-254: Reserved
vcoord(255,'','','');

%% tunit
function varargout = tunit(i,varargin)
persistent CodeTable
if nargin>1
    CodeTable(i+1,1:nargin-1) = varargin;
    return
elseif isempty(CodeTable)
    CodeTable = cell(256,1);
    tunittable_default
end
varargout = CodeTable(i+1,:);

%% tunittable_default
function tunittable_default
for i=0:254
    tunit(i,sprintf('unit %i - Reserved',i),NaN,NaN,NaN);
end
tunit(0,'Minute',1/(24*60),0,0);
tunit(1,'Hour',1/24,0,0);
tunit(2,'Day',1,0,0);
tunit(3,'Month',NaN,1,0);
tunit(4,'Year',NaN,0,1);
tunit(5,'Decade (10 years)',NaN,0,10);
tunit(6,'Normal (30 years)',NaN,0,30);
tunit(7,'Century (100 years)',NaN,0,100);
% 8-9: Reserved
tunit(10,'3 hours',3/24,0,0);
tunit(11,'6 hours',6/24,0,0);
tunit(12,'12 hours',12/24,0,0);
tunit(13,'Quarter of an hour',1/24/4,0,0);
tunit(14,'Half an hour',1/24/2,0,0);
% 15-253: Reserved
tunit(254,'Second',1/(24*3600),0,0);
tunit(255,'',NaN,0,0);

%% trange
function varargout = trange(i,varargin)
persistent CodeTable
if nargin>1
    CodeTable(i+1,1:nargin-1) = varargin;
    return
elseif isempty(CodeTable)
    CodeTable = cell(256,1);
    trangetable_default
end
varargout = CodeTable(i+1,:);

%% trangetable_default
function trangetable_default
for i=0:254
    trange(i,sprintf('Time range %i - Reserved',i));
end
trange(0,'Forecast product valid for reference time + P1 (P1 > 0), or Uninitialized analysis product for reference time (P1 = 0), or Image product for reference time (P1 = 0)');
trange(1,'Initialized analysis product for reference time (P1 = 0)');
trange(2,'Product with a valid time ranging between reference time + P1 and reference time + P2');
trange(3,'Average (reference time + P1 to reference time + P2)');
trange(4,'Accumulation (reference time + P1 to reference time + P2) product considered valid at reference time + P2');
trange(5,'Difference (reference time + P2 minus reference time + P1) product considered valid at reference time + P2');
% 6-9: Reserved
trange(10,'P1 occupies octets 19 and 20; product valid at reference time + P1');
% 11-50: Reserved
trange(51,'Climatological mean value depending on P1 and P2');
% Climatological mean value: multiple year averages of quantities which are
% themselves means over some period of time (P2) less than a year. The
% reference time (R) indicates the date and time of the start of a period
% of time, given by R to R + P2, over which a mean is formed; N indicates
% the number of such period-means that are averaged together to form the
% climatological value, assuming that the N period-mean fields are
% separated by one year. The reference time indicates the start of the
% N-year climatology. If P1 = 0 then the data averaged in the basic
% interval P2 are assumed to be continuous, i.e. all available data are
% simply averaged together. If P1 = 1 (the unit of time – octet 18, Code
% table 4 – is not relevant here) then the data averaged together in the
% basic interval P2 are valid only at the time (hour, minute) given in the
% reference time, for all the days included in the P2 period. The units of
% P2 are given by the contents of octet 18 and Code table 4.
%
% 52-112: Reserved
trange(113,'Average of N forecasts (or initialized analyses); each product has forecast period of P1 (P1 = 0 for initialized analyses); products have reference times at intervals of P2, beginning at the given reference time');
trange(114,'Accumulation of N forecasts (or initialized analyses); each product has forecast period of P1 (P1 = 0 for initialized analyses); products have reference times at intervals of P2, beginning at the given reference time');
trange(115,'Average of N forecasts, all with the same reference time; the first has a forecast period of P1, the remaining forecasts follow at intervals of P2');
trange(116,'Accumulation of N forecasts, all with the same reference time; the first has a forecast period of P1, the remaining forecasts follow at intervals of P2');
trange(117,'Average of N forecasts; the first has a forecast period of P1, the subsequent ones have forecast periods reduced from the previous one by an interval of P2; the reference time for the first is given in octets 13 to 17, the subsequent ones have reference times increased from the previous one by an interval of P2. Thus all the forecasts have the same valid time, given by the initial reference time + P1');
trange(118,'Temporal variance, or covariance, of N initialized analyses; each product has forecast period of P1 = 0; products have reference times at intervals of P2, beginning at the given reference time');
trange(119,'Standard deviation of N forecasts, all with the same reference time with respect to the time average of forecasts; the first forecast has a forecast period of P1, the remaining forecasts follow at intervals of P2');
% 120-122: Reserved
trange(123,'Average of N uninitialized analyses, starting at the reference time, at intervals of P2');
trange(124,'Accumulation of N uninitialized analyses, starting at the reference time, at intervals of P2');
trange(125,'Standard deviation of N forecasts, all with the same reference time with respect to time average of the time tendency of forecasts; the first forecast has a forecast period of P1, the remaining forecasts follow at intervals of P2');
% 126-254: Reserved
trange(255,'');

%% gridtype
function varargout = gridtype(i,varargin)
persistent CodeTable
if nargin>1
    CodeTable(i+1,1:nargin-1) = varargin;
    return
elseif isempty(CodeTable)
    CodeTable = cell(256,5);
    gridtypetable_default
end
varargout = CodeTable(i+1,:);

%% gridtypetable_default
function gridtypetable_default
for i=0:191
    gridtype(i,sprintf('Grid type %i - Reserved',i),i,0,0,0);
end
for i=192:254
    gridtype(i,sprintf('Grid type %i - Reserved for local use',i),i,0,0,0);
end
gridtype(0,'Latitude/longitude grid - equidistant cylindrical or Plate Carrée projection',0,0,0,0);
gridtype(1,'Mercator projection',1,0,0,0);
gridtype(2,'Gnomonic projection',2,0,0,0);
gridtype(3,'Lambert conformal, secant or tangent, conic or bi-polar, projection',3,0,0,0);
gridtype(4,'Gaussian latitude/longitude grid',4,0,0,0);
gridtype(5,'Polar stereographic projection',5,0,0,0);
gridtype(6,'Universal Transverse Mercator (UTM) projection',6,0,0,0);
gridtype(7,'Simple polyconic projection',7,0,0,0);
gridtype(8,'Albers equal-area, secant or tangent, conic or bi-polar, projection',8,0,0,0);
gridtype(9,'Miller''s cylindrical projection',9,0,0,0);
gridtype(10,'Rotated latitude/longitude grid',0,1,0,0);
% 11-12: Reserved
gridtype(13,'Oblique Lambert conformal, secant or tangent, conic or bi-polar, projection',13,0,0,0);
gridtype(14,'Rotated Gaussian latitude/longitude grid',4,1,0,0);
% 15-19: Reserved
gridtype(20,'Stretched latitude/longitude grid',0,0,1,0);
% 21-23: Reserved
gridtype(24,'Stretched Gaussian latitude/longitude grid',4,0,1,0);
% 25-29: Reserved
gridtype(30,'Stretched and rotated latitude/longitude grids',0,1,1,0);
% 31-33: Reserved
gridtype(34,'Stretched and rotated Gaussian latitude/longitude grids',4,1,1,0);
% 35-49: Reserved
gridtype(50,'Spherical harmonic coefficients',50,0,0,0);
% 51-59: Reserved
gridtype(60,'Rotated spherical harmonic coefficients',50,1,0,0);
% 61-69: Reserved
gridtype(70,'Stretched spherical harmonics',50,0,1,0);
% 71-79: Reserved
gridtype(80,'Stretched and rotated spherical harmonic coefficients',50,1,1,0);
% 81-89: Reserved
gridtype(90,'Space view, perspective or orthographic',90,0,0,1);
% 91-191: Reserved
% 192-254: Reserved for local use
gridtype(255,'');
%%
