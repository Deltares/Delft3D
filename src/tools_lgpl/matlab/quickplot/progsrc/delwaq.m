function [Out1,Out2]=delwaq(cmd,varargin)
%DELWAQ Read/write Delwaq files.
%   STRUCT = DELWAQ('open',FILENAME) opens the specified Delwaq HIS/MAP
%   file, or it opens the combination of Delwaq LGA and CCO files.
%
%   [TIME,DATA] = DELWAQ('read',STRUCT,SUBSTANCENR,SEGMENTNR,TSTEP) reads
%   the specified substance (0 for all), specified segment (0 for all) and
%   specified time step (0 for all) from the Delwaq HIS or MAP file. The
%   returned TIME is a MATLAB serial date if reference date and time step
%   size information is available in the file; otherwise time index
%   information is returned. The returned array is of size NSUBS x NSEGM x
%   NTIMES where NSUBS is the number of selected substances, NSEGM the
%   number of selected segments, and NTIMES the number of selected time
%   steps.
%
%   STRUCT = DELWAQ('write',FILENAME,HEADER,SUBSTANCENAMES,TIMEINFO,...
%   TIME,DATA) writes the data to a Delwaq MAP file. Substance names should
%   be specified as a cell array or char matrix. The TIMEINFO argument
%   should be empty (time=index) or a [1 2] matrix containing the reference
%   time (a MATLAB date value) and timestep (in seconds). The size of the
%   data matrix should be NSubstance x NSegment x NTime. See example below.
%   This command writes a delwaq file using the byte ordering native to the
%   writing platform.
%
%   STRUCT = DELWAQ('write',FILENAME,HEADER,SUBSTANCENAMES,...
%   SEGMENTNAMES,TIMEINFO,TIME,DATA) writes the data to a Delwaq HIS file.
%   Substance and segment names should be specified as a cell array or char
%   matrix. The size of the data matrix should be NSubstance x NSegment
%   x NTime. See also comments on TIMEINFO above. This command writes a
%   delwaq file using the byte ordering native to the writing platform.
%
%   STRUCT = DELWAQ('write',FILENAME,PLATFORM,HEADER,SUBSTANCENAMES,...
%   TIMEINFO,TIME,DATA)
%   STRUCT = DELWAQ('write',FILENAME,PLATFORM,HEADER,SUBSTANCENAMES,...
%   SEGMENTNAMES,TIMEINFO,TIME,DATA)
%   where PLATFORM = 'pc' or 'unix' should be used to write a file using
%   byte ordering for the specified platform independent of the writing
%   platform.
%
%   STRUCT = DELWAQ('write',STRUCT,TIME,DATA) adds the data to a Delwaq
%   HIS/MAP file. The size of the data matrix should be NSubstance x
%   NSegment x NTime.
%
%   Example 1:
%      % copy a Delwaq MAP file assuming the whole file fits in memory
%      F     = delwaq('open',INFILE);
%      [t,d] = delwaq('read',F,0,0,0);
%      delwaq('write',OUTFILE,F.Header,F.SubsName, ...
%          [F.T0 F.TStep*3600*24],t,d)
%
%   Example 2:
%      % copy a Delwaq MAP file if one time step fits in memory
%      F     = delwaq('open',INFILE);
%      for it = 1:F.NTimes
%         [t,d] = delwaq('read',F,0,0,it);
%         if it==1
%            F2 = delwaq('write',OUTFILE,F.Header,F.SubsName, ...
%                    [F.T0 F.TStep*3600*24],t,d);
%         else
%            F2 = delwaq('write',F2,t,d);
%         end
%      end
%
%   See also: WAQFIL, QPFOPEN.

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

switch lower(cmd)
    case 'open'
        if nargout>1
            error('Too many output arguments.')
        end
        Out1=Local_delwaq_open(varargin{:});
    case 'openlga'
        if nargout>1
            error('Too many output arguments.')
        end
        Out1=Local_lga_open(varargin{:});
    case 'read'
        [Out1,Out2]=Local_delwaq_read(varargin{:});
    case 'write'
        Out1=Local_delwaq_write(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd))
end


function S=Local_delwaq_open(filename)
% LOCAL_DELWAQ_OPEN  Read basic information from MAP/HIS/PLO files.
S.Check='NotOK';
S.FileType='DelwaqMAP';
%
% Ask for filename if none given
%
if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.*','Select Delwaq file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end
%
% Check whether filename refers to LGA/CCO file. If so handle the file
% accordingly.
%
switch lower(filename(end-3:end))
    case {'.lga','.cco'}
        S=Local_lga_open(filename(1:end-3));
        return
end
%
% Determine byte ordering and formatting (binary of FORTRAN unformatted).
%
S.FileName=filename;
[S.ByteOrder,S.FormatType]=determineOS(filename);
fid = fopen(filename,'r',S.ByteOrder);
if fid<0
    return
end
try
    %
    % In case of an unformatted file verify header size.
    %
    switch S.FormatType
        case 'formatted4'
            Sz=fread(fid,1,'uint32');
            if Sz~=160
                error('Record 1 has unexpected dimensions.')
            end
    end
    Str=char(fread(fid,[40 4],'uchar')');
    S.Header=Str;
    %
    % Determine reference time and time unit from fourth header line.
    % T0: YYYY-MM-DD HH:MM:SS  (scu=NNNNNNNNU)
    %
    [S.T0,S.TStep] = delwaqt0(Str(4,:));
    %
    % Finish reading header and determine file type: PLOT file (2D or 3D) or
    % MAP/HIS file.
    %
    DelparPlot=0;
    Delpar3DMode=0;
    switch S.FormatType
        case 'formatted4'
            Sz=fread(fid,2,'uint32');
            if Sz(2)==24
                DelparPlot=1;
            elseif Sz(2)==32
                DelparPlot=1;
                Delpar3DMode=1;
            elseif Sz(2)~=8
                error('Record 2 has unexpected dimensions.');
            end
        case 'binary'
            % no record sizes in the file, so we need another way to determine
            % whether it is a DelparPlot file ... assume that it is not
            floc=ftell(fid);
            Nsubs=fread(fid,1,'int32');
            if Nsubs>1e8
                error('Unable to handle 100 million substances.')
            elseif Nsubs==-1
                DelparPlot=1;
                Delpar3DMode=1;
            elseif Nsubs<0
                error('Negative number of substances.')
            else
                Nseg=fread(fid,1,'int32');
                SubsNames=fread(fid,[20 Nsubs],'uchar');
                if ~all(ismember(SubsNames(:),[0 32:126 160:255]))
                    DelparPlot=1;
                end
            end
            fseek(fid,floc,-1);
    end
    %
    % In case of a 3D PLOT file, skip the -1 flag.
    %
    if Delpar3DMode
        fread(fid,1,'int32');
    end
    %
    % Determine the number of substances.
    %
    Nsubs=fread(fid,1,'int32');
    if isempty(Nsubs) || Nsubs==0
        error('Number of substances equals zero or invalid Delwaq file.');
    end
    %
    % Determine the PLOT file characteristics.
    %
    if DelparPlot
        S.GridSize=fread(fid,[1 2],'int32');
        S.NumSegm=prod(S.GridSize);
        if Delpar3DMode
            S.K=fread(fid,1,'int32');
            S.NumSegm=S.NumSegm*S.K;
        end
        Nseg=S.NumSegm;
        S.PlotData=fread(fid,3,'int32'); % IYEAR IMONTH IOFSET
        switch S.FormatType
            case 'formatted4'
                Sz=fread(fid,2,'uint32');
                if Sz~=20
                    error('Record 2A has unexpected dimensions.')
                end
        end
        S.PlotWindow=fread(fid,5,'float32');
    else
        Nseg=fread(fid,1,'int32');
        if Nseg==0
            error('No segments/station or invalid Delwaq file.');
        end
        S.NumSegm=Nseg;
    end
    %
    % In case of an unformatted file, verify dimensions.
    %
    switch S.FormatType
        case 'formatted4'
            Sz=fread(fid,2,'uint32');
            if Sz~=Nsubs*20
                error('Record 3 has unexpected dimensions.')
            end
    end
    %
    % Read substance names.
    %
    SubsNames=char(fread(fid,[20 Nsubs],'uchar')');
    %
    % first check one string then check all (better performance if this is
    % actually not a delwaq file).
    %
    if ~all(ismember(SubsNames(1,:),char([0 32:126 160:255]))) || ...
            ~all(ismember(SubsNames(:),char([0 32:126 160:255])))
        error('Invalid substance names.');
    end
    S.SubsName=strtrim(cellstr(SubsNames));
    %
    % In case of an unformatted file, verify dimensions.
    %
    switch S.FormatType
        case 'formatted4'
            Sz=fread(fid,1,'uint32');
    end
    %
    % Detemine file type: distinguish between MAP and HIS files.
    %
    S.NBytesBlock=(Nseg*Nsubs+1)*4;
    if ~DelparPlot
        tmploc=ftell(fid);
        switch S.FormatType
            case 'formatted4'
                %
                % In case of an unformatted file, the FORTRAN block indicators
                % help to determine the file type.
                %
                Sz=fread(fid,1,'uint32');
                if Sz==Nseg*(4+20)
                    %
                    % Block size is equal to the block size expected for the
                    % segment names, so it could very well be a DelwaqHIS file ...
                    %
                    if Sz~=S.NBytesBlock
                        %
                        % It is certainly not a MAP file if the block size is not
                        % equal to the size of the time/field data block ...
                        %
                        S.FileType='DelwaqHIS';
                    elseif isequal(lower(S.FileName(end-3:end)),'.his')
                        %
                        % The size of the data block cannot be used as a
                        % distinction, now try the file extension. If it is equal to
                        % 'HIS' it most probably is a DelwaqHIS file ...
                        %
                        S.FileType='DelwaqHIS';
                    else
                        %
                        % Block size cannot be used as distinction. File extension
                        % does not indicate HIS type. Final attempt, do as if
                        % reading a HIS file and read the first segment name. If it
                        % is a valid string, then it is a HIS file.
                        %
                        nr=fread(fid,1,'uint32');
                        if (nr<=S.NumSegm), % it can still be a DelwaqHIS file ...
                            str=fread(fid,20,'uchar');
                            if all(str>=32) % Yes, it probably is a DelwaqHIS file ...
                                S.FileType='DelwaqHIS';
                            end
                        end
                    end
                elseif Sz~=S.NBytesBlock
                    %
                    % Size of data block does not match list of segment names nor
                    % does it match size of time/field data block. So, it is
                    % neither a HIS nor a MAP file.
                    %
                    error('Data record has unexpected dimensions.')
                end
            otherwise
                %
                % Block size records are missing, so try reading the file as if it
                % were a HIS type file. If all segment names are valid strings,
                % then it is most likely a HIS file. Note this check is more
                % strict if there are more segments. Since files with relatively
                % few segments are generally HIS files, this is okay: the
                % probability of decision error is thus small.
                %
                % Carry out the check in a loop over "small" blocks to detect a
                % huge map-file as soon as possible. I don't understand why it
                % is suddenly a problem to execute fread(fid,20*b,'20*uchar',4)
                % for b equal to about 60 or more in MATLAB 6.5 ...
                %
                fseek(fid,4,0);
                S.FileType='DelwaqHIS';
                maxStatBlockSize = 30;
                StatBlocks = [repmat(maxStatBlockSize,1,floor(Nseg/maxStatBlockSize)) Nseg-maxStatBlockSize*floor(Nseg/maxStatBlockSize)];
                for b = StatBlocks
                    if b>0
                        str=fread(fid,20*b,'20*uchar',4);
                        if any(str<32 & str~=9) % most certainly this is a DelwaqMAP file ...
                            S.FileType='DelwaqMAP';
                            break
                        end
                    end
                end
        end
        fseek(fid,tmploc,-1);
    else
        S.FileType='DelparPLOT';
    end
    %
    % In case of HIS file, read segment/station names.
    %
    if strcmp(S.FileType,'DelwaqHIS')
        %
        % Verify block size (again) as above. Should always be true based on
        % check carried out above to determine file type.
        %
        switch S.FormatType
            case 'formatted4'
                Sz=fread(fid,1,'uint32');
                if Sz~=Nseg*(4+20)
                    error('Record 4 has unexpected dimensions.')
                end
        end
        %
        % Actually read the segment names.
        %
        offset=ftell(fid);
        S.SegmentID=fread(fid,[Nseg 1],'int32',20);
        fseek(fid,offset+4,-1);
        S.SegmentName=cellstr(fread(fid,[20 Nseg],'20*char=>char',4)');
        fseek(fid,-4,0);
        %
        % Skip end block size.
        %
        switch S.FormatType
            case 'formatted4'
                Sz=fread(fid,1,'uint32');
        end
    end
    %
    % Start of data block for all file type. Store as reference location.
    %
    S.DataStart=ftell(fid);
    %
    % Verify data block size in case of unformatted file.
    %
    switch S.FormatType
        case 'formatted4'
            Sz=fread(fid,1,'uint32');
            if Sz~=S.NBytesBlock
                error('Data record has unexpected dimensions.')
            end
            S.NBytesBlock=S.NBytesBlock+2*4;
    end
    %
    % Move to end of file and determine number of records.
    %
    fseek(fid,0,1);
    S.NTimes=(ftell(fid)-S.DataStart)/S.NBytesBlock;
    fclose(fid);
catch
    fclose(fid);
    rethrow(lasterror)
end
%
% In case of HIS file, check whether there is a compagnion HIA file that
% contains longer substance or segment names.
%
if isequal(S.FileType,'DelwaqHIS')
    HIA=S.FileName;
    HIA(end)=HIA(end)-'S'+'A';
    if exist(HIA)
        try
            hia = inifile('open',HIA);
            %
            par=ustrcmpi('Long Parameters',hia.Data(:,1));
            if par>0
                PAR=hia.Data{par,2};
                for i=1:size(PAR,1)
                    j=str2double(PAR{i,1});
                    if j==round(j)
                        S.SubsName{j}=PAR{i,2};
                    end
                end
            end
            par=ustrcmpi('Parameter Descriptions',hia.Data(:,1));
            if par>0
                PAR=hia.Data{par,2};
                for i=1:size(PAR,1)
                    j=str2double(PAR{i,1});
                    if j==round(j)
                        S.SubsName{j}=sprintf('%s (%s)',S.SubsName{j},PAR{i,2});
                    end
                end
            end
            %
            loc=ustrcmpi('Long Locations',hia.Data(:,1));
            if loc>0
                LOC=hia.Data{loc,2};
                for i=1:size(LOC,1)
                    j=str2double(LOC{i,1});
                    if j==round(j)
                        S.SegmentName{j}=LOC{i,2};
                    end
                end
            end
            loc=ustrcmpi('Location Descriptions',hia.Data(:,1));
            if loc>0
                LOC=hia.Data{loc,2};
                for i=1:size(LOC,1)
                    j=str2double(LOC{i,1});
                    if j==round(j)
                        S.SegmentName{j}=sprintf('%s (%s)',S.SegmentName{j},LOC{i,2});
                    end
                end
            end
        catch
        end
    end
end
S.Check='OK';


function S=Local_lga_open(filebase)
% LOCAL_LGA_OPEN  Read data from pair of LGA/CCO files.
S.Check='NotOK';
S.FileType='DelwaqLGA';
[p,f,e] = fileparts(filebase);
switch lower(e)
    case '.'
        % extension already removed
    case {'.lga','.cco'}
        % remove extension
        filebase = filebase(1:end-3);
    otherwise
        error('LGA/CCO file extension should match .lga/.cco')
end
S.FileBase=filebase;
%
% Start by reading the LGA file (note: on case sensitive platforms this
% code works only for files with lower case file extensions). Subsequently
% determinte file fromatting and byte order.
%
filename=strcat(S.FileBase,'lga');
[S.ByteOrder,S.FormatType]=determineOS(filename);
fid = fopen(filename,'r',S.ByteOrder);
if fid<0
    error('Cannot open LGA file.')
end
%
% In case of a FORTRAN unformatted file, verify record size.
%
switch S.FormatType
    case 'formatted4'
        Sz=fread(fid,1,'uint32');
        if Sz~=4*7
            error('Record 1 has unexpected dimensions.')
        end
end
%
% Read first record: grid dimensions.
%
X=fread(fid,[1 7],'int32');
S.MNK=[X(1) X(2) X(4)];
S.NoSegPerLayer=X(3);
S.NoSeg=S.NoSegPerLayer*S.MNK(3);
S.NoExchMNK=X(5:7);
%
% Verify size of second record in case of an unformatted file.
%
switch S.FormatType
    case 'formatted4'
        Sz=fread(fid,2,'uint32');
        if Sz(2)~=4*prod(S.MNK(1:2))
            error('Aggragation record has unexpected dimensions.')
        end
end
%
% Read aggregation table and verify size (check on premature EOF).
%
S.Index=fread(fid,prod(S.MNK(1:2)),'int32');
if length(S.Index)~=prod(S.MNK(1:2))
    error('Error reading aggagration data from file.')
end
%
% Reshape index table, make 3D and close file.
%
S.Index=reshape(S.Index,S.MNK(1:2));
sgni=sign(S.Index(:,:,1));
absi=abs(S.Index(:,:,1));
for k=X(4):-1:2
    S.Index(:,:,k)=sgni.*(absi+(k-1)*S.NoSegPerLayer);
end
fclose(fid);

%
% Open the associated CCO file.
%
filename=strcat(S.FileBase,'cco');
fid = fopen(filename,'r',S.ByteOrder);
if fid<0
    error('Cannot open CCO file.')
end
%
% In case of a FORTRAN unformatted file, verify size of first record. I
% have only encountered cases where the first record is empty; so, only
% such cases are currently supported.
%
switch S.FormatType
    case 'formatted4'
        Sz=fread(fid,1,'int32');
        if Sz>0
            warning('Delwaq CCO: Record 1 not empty.')
            fread(fid,[1 Sz],'int32');
            Sz=fread(fid,1,'uint32');
        elseif Sz==0
            Sz=fread(fid,1,'uint32');
            if Sz~=0
                error('Unexpected value for second integer.')
            end
        end
end
%
% Verify size of second record in case of an unformatted file.
%
switch S.FormatType
    case 'formatted4'
        Sz=fread(fid,1,'uint32');
        if Sz~=4*7
            error('Record 2 has unexpected dimensions.')
        end
end
%
% Read second record: grid dimensions, reference coordinate and number of
% layers.
%
MN=fread(fid,[1 2],'int32');
S.XY0=fread(fid,[1 2],'float32');
X=fread(fid,[1 3],'int32');
if ~isequal(MN([2 1]),S.MNK(1:2)) || X(3)~=S.MNK(3)
    error('Size in CCO file [%i %i %i] does not match size in LGA file [%i %i %i].',MN([2 1]),X(3),S.MNK)
end
%
% Skip next nine records. No idea what they were meant to do.
%
for i=1:9
    switch S.FormatType
        case 'formatted4'
            Sz=fread(fid,2,'uint32');
            if Sz(2)~=4
                error('Record has unexpected dimensions.')
            end
    end
    fread(fid,1,'uint32'); % treat as dummy
end
%
% Verify size of x-coordinates block if possible.
%
switch S.FormatType
    case 'formatted4'
        Sz=fread(fid,2,'uint32');
        if Sz(2)~=4*prod(S.MNK(1:2))
            error('X-coordinate record has unexpected dimensions.')
        end
end
%
% Read x-coordinates.
%
S.X=fread(fid,S.MNK(1:2),'float32');
%
% Verify size of y-coordinates block if possible.
%
switch S.FormatType
    case 'formatted4'
        Sz=fread(fid,2,'uint32');
        if Sz(2)~=4*prod(S.MNK(1:2))
            error('Y-coordinate record has unexpected dimensions.')
        end
end
%
% Read y-coordinates and close file.
%
S.Y=fread(fid,S.MNK(1:2),'float32');
fclose(fid);
%
% Clip zero coordinates.
%
S.X(S.X==0 & S.Y==0)=NaN;
S.Y(isnan(S.X))=NaN;
%
% Flip order of dimensions to match standard ordering.
%
S.DimOrder = 'as in file';
S=lgaflip(S);
%
% Try loading the bed level file ...
%
dpsfil = [S.FileBase 'dps'];
try
    dps = waqfil('open',dpsfil);
    S.DPS = dps.Srf;
end
%
S.Check='OK';


function S=lgaflip(S)
% LGAFLIP  Flips dimensions of a CCO grid.
S.DimOrder = 'flipped';
S.MNK([1 2]) = S.MNK([2 1]);
S.NoExchMNK([1 2]) = S.NoExchMNK([2 1]);
S.Index = permute(S.Index,[2 1 3]);
S.X = S.X';
S.Y = S.Y';


function [ByteOrder,FormatType]=determineOS(filename)
%DETERMINEOS  Determine the byte ordering and file formatting.
ByteOrder='b';
FormatType='unknown';
%
% Start by opening as big-endian.
%
fid=fopen(filename,'r','b');
if fid<0
    error('Cannot open file: %s.',filename)
end
%
% Read uint32 values and interpret as block sizes.
%
Size=fread(fid,1,'uint32');
if isempty(Size)
    loc = ftell(fid);
    fclose(fid);
    if loc>0
        error('File is almost empty; insufficient data in file to do anything useful with it.')
    else
        error('File is empty.')
    end
end
while Size==0 % I'm not convinced by matching zeros ...
    Size=fread(fid,2,'uint32');
    if feof(fid)
        %
        % Files containing only zeros do not make sense.
        %
        fclose(fid);
        error('Unexpected end of file.')
    end
    Size=Size(2);
end
%
% If value is less than 1000, it might be a FORTRAN unformatted file. If
% the closing block size indicator matches this opening block size, I am
% satisfied. Otherwise, it is considered to be a binary big-endian file,
% which I have never seen. If the value is larger than 1000, the value is
% considered too big, and the byte ordering is assumed to be little-endian.
% I have never seen an unformatted version of a little-endian delwaq file,
% so I can stop here. See, the FLS function for dealing with little-endian
% unformatted files (1 byte size indicators).
%
if Size<1000
    fseek(fid,Size,0);
    Size2=fread(fid,1,'uint32');
    if isequal(Size2,Size)
        FormatType='formatted4';
    end
else
    fseek(fid,-4,0);
    ByteOrder='l';
    Size=fread(fid,1,'uint32',ByteOrder);
    if Size<1000
        fseek(fid,Size,0);
        Size2=fread(fid,1,'uint32',ByteOrder);
        if isequal(Size2,Size)
            FormatType='formatted4';
        else
            FormatType='binary';
        end
    else
        FormatType='binary';
    end
end
%
% Close the file.
%
fclose(fid);


function [OTime,Data]=Local_delwaq_read(S,Subs,Seg,Time)
% LOCAL_DELWAQ_READ  Read data from HIS/MAP/PLO file.
if nargin<4
    Time=0;
    if nargin<3
        Seg=0;
        if nargin<2
            Subs=0;
            if nargin<1
                error('No file specified.')
            end
        end
    end
end
%
% Time must be a timestep number
%
if ~isnumeric(Time)
    error('Invalid timestep')
elseif isequal(Time(:),(1:S.NTimes)') || isequal(Time,0)
    if isequal(Time,1)
        tim=1;
        ntim=1;
    else
        tim=0;
        ntim=S.NTimes;
    end
else
    tim=Time;
    ntim=length(tim);
end
%
% Subs can be either a substance name or number (0 for all substances)
%
NSubs=length(S.SubsName);
if iscellstr(Subs)
    subs=zeros(1,numel(Subs));
    for i=1:numel(Subs)
        subs(i)=ustrcmpi(Subs{i},S.SubsName);
        if subs(i)<0
            error('Non-unique substance name: %s.',Subs{i})
        end
    end
elseif ischar(Subs)
    subs=ustrcmpi(Subs,S.SubsName);
    if subs<0
        error('Non-unique substance name.')
    end
elseif ~isnumeric(Subs)
    error('Substance specifier should be name or number.')
elseif isequal(Subs,0)
    subs=1:NSubs;
elseif any(Subs>NSubs | Subs<1)
    error('Substance number out of range.')
else
    subs=Subs;
end
nsubs=length(subs);
%
% Seg can be either a segment number (or name if appropriate) (0 for all segments)
%
if iscellstr(Seg)
    switch S.FileType
        case 'DelwaqHIS'
            seg=zeros(1,numel(Seg));
            for i=1:numel(Seg)
                seg(i)=ustrcmpi(Seg{i},S.SegmentName);
                if seg(i)<0
                    error('Non-unique segment name: %s.',Seg{i})
                end
            end
        otherwise
            error('No names available for segments.')
    end
elseif ischar(Seg)
    switch S.FileType
        case 'DelwaqHIS'
            seg=ustrcmpi(Seg,S.SegmentName);
            if seg<0
                error('Non-unique segment name.')
            end
        otherwise
            error('No names available for segments.')
    end
elseif ~isnumeric(Seg)
    error('Segment specifier should be name or number.')
elseif isequal(Seg,0)
    seg=1:S.NumSegm;
elseif any(Seg>S.NumSegm | Seg<1)
    error('Segment number out of range.')
else
    seg=Seg;
end
nseg=length(seg);
%
% Open data file with appropriate byte ordering.
%
fid=fopen(S.FileName,'r',S.ByteOrder);
if fid<0
    error('Cannot open file.')
end
NSeg=S.NumSegm;
NTot=NSubs*NSeg;
%
% Take into account possibly extra bytes for unformated files compared to
% binary files.
%
FormRecSize=0;
switch S.FormatType
    case 'formatted4'
        FormRecSize=4;
end
%
% Distinguish between reading all time steps and one or a couple of time
% steps. Don't use SKIP argument if the file is bigger than 2GB.
%
can_use_skip = (S.NBytesBlock*S.NTimes+S.DataStart)<2^31;
if tim==0
    %
    % Going to read all time steps: read them in one go.
    %
    fseek(fid,S.DataStart+FormRecSize,-1);
    OTime=local_fread(fid,can_use_skip,[1 S.NTimes],'int32',4*NTot+2*FormRecSize)';
    %
    % Distinguish between reading one and reading multiple substances and/or
    % locations. In case of multiple substances, read all and filter after
    % reading. In case of multiple segments, read them one by one since
    % reading all of them may require far too much memory.
    %
    if nsubs==0 || nseg==0
        Data=zeros(nsubs,nseg,S.NTimes);
    elseif nsubs>1 && nseg>1
        fseek(fid,S.DataStart+FormRecSize+4,-1);
        Data=local_fread(fid,can_use_skip,[NTot S.NTimes],'float32',4+2*FormRecSize);
        Data=reshape(Data,NSubs,NSeg,S.NTimes);
        Data=Data(subs,seg,:);
    elseif nsubs>1
        N=NSubs*(NSeg-1);
        fseek(fid,S.DataStart+FormRecSize+4+4*(seg-1)*NSubs,-1);
        Data=local_fread(fid,can_use_skip,[NSubs S.NTimes],'float32',4*(1+N)+2*FormRecSize);
        Data=reshape(Data,NSubs,1,S.NTimes);
        Data=Data(subs,1,:);
    elseif nseg>1
        Data=zeros(nseg,S.NTimes);
        for i=1:length(seg)
            fseek(fid,S.DataStart+FormRecSize+4+4*(seg(i)-1)*NSubs+4*(subs-1),-1);
            Data(i,:)=local_fread(fid,can_use_skip,[1 S.NTimes],'float32',4*NTot+2*FormRecSize);
        end
        Data=reshape(Data,1,nseg,S.NTimes);
    else
        fseek(fid,S.DataStart+FormRecSize+4+4*(seg-1)*NSubs+4*(subs-1),-1);
        Data=local_fread(fid,can_use_skip,[1 S.NTimes],'float32',4*NTot+2*FormRecSize)';
        Data=reshape(Data,1,1,S.NTimes);
    end
else
    %
    % Going to read one or more time steps, but not all. Since they may be
    % located at irregular intervals. Loop over the time steps to read them
    % one by one.
    %
    OTime=repmat(NaN,ntim,1);
    Data=repmat(NaN,[nsubs nseg ntim]);
    for ti=1:ntim
        tim1=tim(ti);
        local_fseek(fid,S.DataStart,tim1,S.NBytesBlock,FormRecSize)
        OTime(ti)=fread(fid,1,'int32');
        %
        % Distinguish between reading one and reading multiple substances
        % and/or locations. In case of multiple substances, read all and
        % filter after reading. In case of multiple segments, read them one
        % by one since reading all of them may require far too much memory.
        %
        if nsubs==0 || nseg==0
        elseif nsubs>1 && nseg>1
            tmpData=fread(fid,[NSubs NSeg],'float32');
            Data(:,:,ti)=tmpData(subs,seg);
        elseif nsubs>1
            fseek(fid,4*(seg-1)*NSubs,0);
            tmpData=fread(fid,[NSubs 1],'float32');
            Data(:,:,ti)=tmpData(subs,1);
        elseif nseg>1
            if can_use_skip
                fseek(fid,4*(subs-1),0);
                tmpData=local_fread(fid,can_use_skip,[1 NSeg],'float32',4*(NSubs-1));
                Data(:,:,ti)=tmpData(1,seg);
            else
                tmpData=fread(fid,[NSubs NSeg],'float32');
                Data(:,:,ti)=tmpData(subs,seg);
            end
        else
            fseek(fid,4*(seg-1)*NSubs+4*(subs-1),0);
            Data(:,:,ti)=fread(fid,1,'float32');
        end
    end
end
fclose(fid);
%
% Determine associated times.
%
if S.TStep~=0
    OTime=S.T0+OTime*S.TStep;
end


function Data=local_fread(fid,can_use_skip,N,Type,Skip)
if can_use_skip
    Data=fread(fid,N,sprintf('%i*%s',N(1),Type),Skip);
else
    Data=zeros(N);
    for i=1:N(2)
        Data(:,i)=fread(fid,[N(1) 1],Type);
        fseek(fid,Skip,0);
    end
end


function local_fseek(fid,DataStart,tim1,NBytesBlock,FormRecSize)
tim0 = tim1-1;
offset = DataStart+tim0*NBytesBlock+FormRecSize;
if offset >= 2^31
    fseek(fid,DataStart+FormRecSize,-1);
    step = max(floor(2^31/NBytesBlock),1);
    stepList = [repmat(step,1,floor(tim0/step)) tim0-floor(tim0/step)*step];
    for thisStep = stepList
        fseek(fid,thisStep*NBytesBlock,0);
    end
else
    fseek(fid,offset,-1);
end


function S=Local_delwaq_write(Fl,varargin)
%LOCAL_DELWAQ_WRITE  Write data to a MAP/HIS/PLO file.
Inp=varargin;
j=1;
%
% Distinguish between adding time steps to a file and creating a completely
% new file.
%
if isstruct(Fl)
    %
    % Append to an already existing file. Use the dimensions from the file.
    %
    Initialise=0;
    S=Fl;
    Time=Inp{j};
    Data=Inp{j+1};
    Nsubs=length(S.SubsName);
    Nseg=S.NumSegm;
elseif ischar(Fl)
    %
    % Create a new file. Use the dimensions of the data. The default
    % formatting is based on the current computer, but the user may indicate
    % otherwise.
    %
    Initialise=1;
    S.Check='NotOK';
    S.FileType='DelwaqMAP';
    S.FileName=Fl;
    lEnd = {'pcwin','pcwin64','pc','glnx86','glnxa64','l','ieee-le'};
    bEnd = {'sun4','sol2','hp700','hpux','sgi','sgi64','ibm_rs','unix','b','ieee-be'};
    if ischar(Inp{1})
        switch lower(Inp{1}),
            case [lEnd bEnd]
                Format=Inp{1};
                j=j+1;
            otherwise
                Format=computer;
        end
    else
        Format=computer;
    end
    switch lower(Format)
        case bEnd
            S.ByteOrder='b';
            S.FormatType='formatted4';
        otherwise
            S.ByteOrder='l';
            S.FormatType='binary';
    end
    %
    % Determine header contents.
    %
    if iscellstr(Inp{j})
        Header=Inp{j};
    else
        Header={Inp{j}};
    end
    %
    % Determine substance names.
    %
    j=j+1;
    if iscellstr(Inp{j})
        SubsNames=Inp{j};
    else
        if size(Inp{j},1)>1
            SubsNames=cellstr(Inp{j});
        else
            SubsNames={Inp{j}};
        end
    end
    Nsubs=length(SubsNames);
    %
    % Distinguish between HIS files (segment names specified) and MAP files.
    % PLO files currently not supported.
    %
    j=j+1;
    Nseg=-1;
    if iscellstr(Inp{j})
        S.FileType='DelwaqHIS';
        SegNames=Inp{j};
        Nseg=length(SegNames);
        j=j+1;
    elseif ischar(Inp{j})
        S.FileType='DelwaqHIS';
        if size(Inp,1)>1
            SegNames=cellstr(Inp{j});
        else
            SegNames={Inp{j}};
        end
        Nseg=length(SegNames);
        j=j+1;
    else
        SegNames={};
    end
    %
    % Get times and data.
    %
    RefTime=Inp{j};
    Time=Inp{j+1};
    Data=Inp{j+2};
    %
    % Set number of segments in case of MAP file based on data size.
    %
    if Nseg<0
        Nseg=size(Data,2);
    end
    S.NumSegm=Nseg;
    S.NBytesBlock=(Nseg*Nsubs+1)*4;
else
    error('Invalid file specification.')
end
%
% Verify data size.
%
if size(Data,1)~=Nsubs
    error('Number of data rows (%i) does not match number of substances (%i).',size(Data,1),Nsubs)
end
if size(Data,2)~=Nseg
    error('Number of data columns (%i) does not match number of segments (%i).',size(Data,2),Nseg)
end
if size(Data,3)~=length(Time)
    error('Number of times (%i) does not match data fields (%i).',length(Time),size(Data,3))
end
%
% In case of file creation, write header records.
%
if Initialise
    fid=fopen(S.FileName,'w',S.ByteOrder);
    if fid<0
        error('Cannot open file: %s',S.FileName)
    end
    %
    % Create header (including time indicator on fourth line).
    %
    Header=char(Header);
    if size(Header,2)<40
        Header(1,40)=' ';
    elseif size(Header,2)>40
        Header=Header(:,1:40);
    end
    if size(Header,1)<4
        Header(4,1)=' ';
    elseif size(Header,1)>4
        Header=Header(1:4,:);
    end
    if ~isempty(RefTime)
        S.T0=RefTime(1);
        S.TStep=RefTime(2)/(24*3600);
        ClockUnit='s';
        if (round(RefTime(2)/(24*3600))*24*3600==RefTime(2))
            ClockUnit='d';
            RefTime(2)=RefTime(2)/(24*3600);
        elseif (round(RefTime(2)/3600)*3600==RefTime(2))
            ClockUnit='u';
            RefTime(2)=RefTime(2)/3600;
        elseif (round(RefTime(2)/60)*60==RefTime(2))
            ClockUnit='m';
            RefTime(2)=RefTime(2)/60;
        end;
        dTstr = sprintf('%8g',RefTime(2));
        if length(dTstr)>8
            dTstr = sprintf('%8.2g',RefTime(2));
        end
        RT = str2double(dTstr);
        if RT~=RefTime(2)
            S.TStep = RT;
            warning('Time scale rounded to %g seconds.',RT)
        end
        Header(4,:)=sprintf('T0: %4i.%2.2i.%2.2i %2.2i:%2.2i:%2.2i  (scu=%s%c)',round(datevec(RefTime(1))),dTstr,ClockUnit);
    else
       [S.T0,S.TStep] = delwaqt0(Header(4,:));        
    end
    S.Header=Header;
    %
    % Write header (with associated data blocks in case of an unformatted
    % file).
    %
    switch S.FormatType
        case 'formatted4'
            fwrite(fid,4*40,'int32');
    end
    fwrite(fid,Header','uchar');
    switch S.FormatType
        case 'formatted4'
            fwrite(fid,[4*40 8],'int32');
    end
    %
    % Write dimensions.
    %
    fwrite(fid,[Nsubs Nseg],'int32');
    switch S.FormatType
        case 'formatted4'
            fwrite(fid,[8 20*Nsubs],'int32');
    end
    %
    % Write substance names.
    %
    SubsNames=char(SubsNames);
    if size(SubsNames,2)<20
        SubsNames(1,20)=' ';
        SubsNames(SubsNames==0)=' ';
    elseif size(SubsNames,2)>20
        SubsNames=SubsNames(:,1:20);
        warning('Substance names truncated.');
    end
    S.SubsName=cellstr(SubsNames);
    fwrite(fid,SubsNames','uchar');
    switch S.FormatType
        case 'formatted4'
            fwrite(fid,20*Nsubs,'int32');
    end
    %
    % In case of a HIS file, write segment names.
    %
    if strcmp(S.FileType,'DelwaqHIS')
        switch S.FormatType
            case 'formatted4'
                fwrite(fid,[(4+20)*Nseg],'int32');
        end
        SegNames=str2mat(SegNames);
        if size(SegNames,2)<20
            SegNames(1,20)=' ';
            SegNames(SegNames==0)=' ';
        elseif size(SegNames,2)>20
            SegNames=SegNames(:,1:20);
            warning('Segment names truncated.')
        end
        for i=1:Nseg
            fwrite(fid,i,'int32');
            fwrite(fid,SegNames(i,:),'uchar');
            S.SegmentID(i)=i;
            S.SegmentName{i}=deblank(SegNames(i,:));
        end
        switch S.FormatType
            case 'formatted4'
                fwrite(fid,[(4+20)*Nseg],'int32');
        end
    end
    %
    % Store beginning of actual data block.
    %
    S.DataStart=ftell(fid);
    switch S.FormatType
        case 'formatted4'
            S.NBytesBlock=S.NBytesBlock+2*4;
    end
    S.NTimes=0;
else
    %
    % Open the existing file.
    %
    fid=fopen(S.FileName,'a',S.ByteOrder);
    if fid<0
        error('Cannot open file: %s',S.FileName)
    end
end
%
% Deal with each time step to be written individually.
%
for i=1:length(Time)
    %
    % Write opening size in case of unformatted file.
    %
    switch S.FormatType
        case 'formatted4'
            fwrite(fid,S.NBytesBlock,'int32');
    end
    %
    % Write time.
    %
    t=Time(i)-S.T0;
    if S.TStep~=0,
        t=round(t/S.TStep);
    end
    fwrite(fid,t,'int32');
    %
    % Write data.
    %
    fwrite(fid,Data(:,:,i),'float32');
    %
    % Write closing size in case of unformatted file.
    %
    switch S.FormatType
        case 'formatted4'
            fwrite(fid,S.NBytesBlock,'int32');
    end
    S.NTimes=S.NTimes+1;
end
%
% Close file.
%
S.Check='OK';
fclose(fid);
