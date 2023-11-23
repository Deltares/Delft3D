function varargout=cfx(cmd,varargin)
% CFX File operations for CFX file.
%     FILEDATA = CFX('open',FILENAME) opens an ascii or binary CFX4 dump
%     (.dmp), geometry (.geo), or output print file and determines the file
%     contents.
%
%     DATA = CFX('read',FILEDATA,VARIABLE,DMTYPE,I,PH,T) reads the data for
%     the VARIABLE for one or more domains from a CFX dump file for
%     specified time and phase.
%        domain type    : DMTYPE = 'block' or 'patch'
%        domain numbers : I
%        time index     : T
%        phase number   : PH
%     For single phase PH flow the phase value may be skipped: ...,I,T).
%     For multi phase flows the phase PH has to be specified in order to
%     specify a time index T (otherwise T = last index and PH = 1). If time
%     index T is not specified the last index is taken.
%
%     DATA = CFX('read',FILEDATA,VARIABLE,PH,T) reads the data for the
%     VARIABLE for all blocks from a CFX dump file for specified time and
%     phase. For single phase PH flow the phase value may be skipped:
%     ...,I,T). For multi phase flows the phase PH has to be specified in
%     order to specify a time index T (otherwise T = last index and PH =
%     1). If time index T is not specified the last index is taken.
%
%     DATA = CFX('read',FILEDATA,'entry',I) reads data from record I of a
%     CFX dump file.
%
%     VARIABLES = CFX('vars',FILEDATA,DMTYPE) returns all acceptable
%     variable names for the specified domain. Default DMTYPE = 'block'.
%     Note: Valid variable names are the names of the active variable as
%           used by CFX and the additional variables 'X COORDINATES','Y
%           COORDINATES', 'Z COORDINATES' for both blocks and patches and
%           'U CONVECTION','V CONVECTION','W CONVECTION' for blocks only.
%           When reading coordinates all specified phase and time
%           parameters are ignored.
%
%     [X,Y,Z] = CFX('read',FILEDATA,B) reads the X,Y,Z coordinates of
%     the selected blocks B from a CFX geometry file. If B is not specified
%     all blocks are read.
%
%     FILEDATA = CFX('write',FILENAME,DATAIN,X,Y,Z) writes a CFX geometry
%     file. DATAIN should have the same structure as a FILEDATA object
%     returned from a CFX open call for .geo file.
%
%     DATA = CFX('read',FILEDATA,DMTYPE,B,D) reads a dataset D of domain B
%     from the CFX output file. The domain type DMTYPE can be either
%     'block' or 'patch'. The dataset can be indicated a string or an
%     index.
%
%     DATA = CFX('read',FILEDATA,'monitoring point') reads the data for the
%     monitoring point from the CFX output file.

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
        varargout=cell(nargout,1);
    end
    return
end
switch cmd
    case {'opendmp','openfo','opengeo','open'}
        if nargin==1
            fils=cmd(5:end);
            if isempty(fils)
                fils='*.*';
            else
                fils=['*.' fils];
            end
            [fn,fp]=uigetfile(fils);
            if ~ischar(fn)
                varargout{1}.Check='NotOK';
                return
            end
            filename=[fp fn];
            INP={filename};
        else
            filename=varargin{1};
            INP=varargin;
        end
        ext=cmd(5:end);
        if isempty(ext)
            [PATH,NAME,EXT] = fileparts(filename);
            ext=lower(EXT(2:end));
        end
        switch ext
            case 'dmp'
                Structure=Local_interpret_dmp_file(INP{:});
            case 'geo'
                Structure=Local_interpret_geo_file(INP{:});
            case 'fo'
                Structure=Local_interpret_fo_file(INP{:});
            otherwise
                error('Unknown CFX file extension "%s".',ext)
        end
        varargout{1}=Structure;
        if nargout>0
            if ~isstruct(Structure)
                varargout{1}=[];
            else
                varargout{1}=Structure;
            end
        end
    case {'read','readdmp','readfo','readgeo'}
        if nargin<0
            varargout{1}=[];
        else
            FD=varargin{1};
            if ~isstruct(FD) || ~isfield(FD,'FileType')
                error('Invalid input argument for reading.')
            end
            switch lower(FD.FileType)
                case 'dmp'
                    varargout{1}=Local_read_dmp_file(varargin{:});
                case 'fo'
                    varargout{1}=Local_read_fo_file(varargin{:});
                case 'geo'
                    TMP=Local_read_geo_file(varargin{:});
                    [varargout{1:3}]=deal(TMP{:});
            end
        end
    case {'write','writegeo'}
        if nargin>0
            varargout{1}=Local_write_geo_file(varargin{:});
        end
    case 'vars'
        if nargin<0
            varargout{1}='';
        else
            FD=varargin{1};
            if ~isstruct(FD) || ~isfield(FD,'FileType')
                error('Invalid input argument.')
            end
            switch lower(FD.FileType)
                case 'dmp'
                    varargout{1}=Local_vars_dmp_file(varargin{:});
                otherwise
                    varargout{1}='';
            end
        end
    otherwise
        uiwait(msgbox('unknown command','modal'));
end


function Structure=Local_interpret_dmp_file(filename)
Structure.Check='NotOK';

if nargin==0
    [fn,fp]=uigetfile('*.dmp');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'r','b','US-ASCII');
if (fid<0)
    return
end

Structure.FileName=filename;
Structure.FileType='dmp';

if isequal(fread(fid,1,'int32'),43)
    fseek(fid,0,-1);
    Structure.FileMode='binary';
    Structure=Local_interpret_bindmp_file(Structure,fid);
else
    fseek(fid,0,-1);
    Structure.FileMode='ascii';
    Structure=Local_interpret_ascdmp_file(Structure,fid);
end

fclose(fid);
Structure.Check='OK';


function S=Local_interpret_bindmp_file(Sin,fid)
% FILE STRUCTURE OF DMP FILE:
%
% SUMMARY OF FILE             allways
% INTEGER DATA                allways
% STORING OF BOUNDARY NODES   allways
% ACTIVE VARIABLES            allways
% ARRAY IPNODB                allways
% ARRAY IPFACB                allways
% ARRAY IBLK                  allways
% ARRAY CBLK                  allways
% ARRAY INFPCH                allways
% ARRAY CPATCH                allways
% ARRAY INFGLU                in case of GLUE PATCHES
% OPTIONS                     allways
% BOUNDARY POINTERS           allways
%
% the following is repeated for each data group ----------------.
% ---- DATA GROUP NUMBER ---- allways                           |
% LAST SET OF DATA            only for last data group          |
% TIME STEP NUMBER            allways                           |
% TIME AND DT                 not for STEADY FLOW computation   |
% RESULTS AT END OF TIMESTEP  allways                           |
% ARRAY XNN                   only in first data group          |
%                                                               |
% the following is repeated for each phase ----------.          |
% PHASE NUMBER                allways                |          |
%                                                    |          |
% the following is repeated for each variable --.    |          |
% VARIABLE NAME               allways           |    |          |
% VALUE AT CELL CENTERS       allways           |    |          |
% VALUE AT BOUNDARIES         allways           |    |          |
%                                                    |          |
% CONVECTION COEFFICIENTS     allways                |          |
%                                                               |
% TMULT V                     allways                           |
% REFERENCE PRESSURE          allways                           |
S=Sin;
i=0;
while ~feof(fid)
    fread(fid,1,'int32'); %43
    Str=deblank(fread(fid,[1 30],'*char'));
    if strcmp(Str,'END OF FILE')
        break
    end
    i=i+1;
    S.Entry(i).Name=Str;
    S.Entry(i).DataType=fread(fid,1,'*char');
    S.Entry(i).Size=fread(fid,[1 3],'int32');
    fread(fid,1,'int32'); %43
    S.Entry(i).Loc=ftell(fid);
    Local_skip_bindmp_data(fid,S.Entry(i).DataType,S.Entry(i).Size)
end
S=Local_analyse_dmp_file(S,fid);


function S=Local_analyse_dmp_file(Sin,fid)
S=Sin;
BinaryDmpFile=strcmp(S.FileMode,'binary');
EntryNr=2; % INTEGER DATA
fseek(fid,S.Entry(EntryNr).Loc,-1);
if BinaryDmpFile
    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
else
    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
end
S.NumOfVars=X(1);
S.NumPhases=X(2);
S.NumBlocks=X(3);
S.NumPatches=X(4);
S.NumGlue=X(5);
S.NumCells=X(6);
S.NumBNodes=X(7);
S.NumTStep=X(9);

EntryNr=4; % ACTIVE VARIABLES
fseek(fid,S.Entry(EntryNr).Loc,-1);
if BinaryDmpFile
    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
else
    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
end
S.ActiveVars=X;
S.NumVars=sum(S.ActiveVars);

EntryNr=7; % ARRAY IBLK
fseek(fid,S.Entry(EntryNr).Loc,-1);
if BinaryDmpFile
    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
else
    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
end
X=reshape(X,[5 S.NumBlocks]);
for i=1:S.NumBlocks
    S.Block(i).I=X(1,i);
    S.Block(i).J=X(2,i);
    S.Block(i).K=X(3,i);
    S.Block(i).IJK=prod(X(1:3,i));
    S.Block(i).IJK1=prod(X(1:3,i)+1);
    % skip offset X(4,i) [sum(IJK)+1] and X(5,i) [sum(IJK1)+1]
end

EntryNr=8; % ARRAY CBLK
fseek(fid,S.Entry(EntryNr).Loc,-1);
if BinaryDmpFile
    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
else
    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,[S.Entry(EntryNr).Size S.Entry(EntryNr).Length]);
end
for i=1:S.NumBlocks
    S.Block(i).Name=deblank(X(i,:));
end

EntryNr=9; % ARRAY INFPCH
fseek(fid,S.Entry(EntryNr).Loc,-1);
if BinaryDmpFile
    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
else
    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
end
X=reshape(X,[9 S.NumPatches]);
for i=1:S.NumPatches
    S.Patch(i).Imin=X(1,i);
    S.Patch(i).Imax=X(2,i);
    S.Patch(i).Jmin=X(3,i);
    S.Patch(i).Jmax=X(4,i);
    S.Patch(i).Kmin=X(5,i);
    S.Patch(i).Kmax=X(6,i);
    S.Patch(i).IJK=(S.Patch(i).Imax-S.Patch(i).Imin+1)*(S.Patch(i).Jmax-S.Patch(i).Jmin+1)*(S.Patch(i).Kmax-S.Patch(i).Kmin+1);
    S.Patch(i).Direc=X(7,i);
    % Direction value meanings:
    % HIGH I = 1
    % HIGH J = 2
    % HIGH K = 3
    % LOW  I = 4
    % LOW  J = 5
    % LOW  K = 6
    S.Patch(i).Block=X(8,i);
    S.Patch(i).Label=X(9,i);
end

EntryNr=10; % ARRAY CPATCH
fseek(fid,S.Entry(EntryNr).Loc,-1);
if BinaryDmpFile
    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
else
    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,[S.Entry(EntryNr).Size S.Entry(EntryNr).Length]);
end
for i=1:S.NumPatches
    S.Patch(i).Type=deblank(X(2*i-1,:));
    S.Patch(i).Name=deblank(X(2*i,:));
end

if S.NumGlue>0
    EntryNr=11; % ARRAY INFGLU
    fseek(fid,S.Entry(EntryNr).Loc,-1);
    if BinaryDmpFile
        X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
    else
        X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
    end
    X=reshape(X,[5 S.NumGlue]);
    for i=1:S.NumGlue
        S.Glue(i).Patch1=X(1,i);
        S.Glue(i).Patch2=X(2,i);
        S.Glue(i).DirChange=transpose(X(3:5,i));
    end
    Offset=1;
else
    Offset=0;
end

EntryNr=11+Offset; % OPTIONS
fseek(fid,S.Entry(EntryNr).Loc,-1);
if BinaryDmpFile
    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
else
    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
end
% 0 0 0 0 1 0 0 1 0 0 0 1 0 0: laminar   transient incompressible isothermal cartesian_coordinates
% 0 0 1 0 0 0 0 0 0 0 0 0 0 0: turbulent steady    incompressible isothermal cartesian_coordinates
% 0 0 1 0 1 0 0 0 0 0 0 0 0 0: turbulent steady    compressible   non-isothermal? cartesian_coordinates?
% 0 0 0 0 0 0 0 0 1 0 0 1 1 0: laminar   transient sliding-mesh cylindrical_coordinates
% 0 0 0 0 0 0 0 0 0 0 0 1 1 0: laminar   transient sliding-mesh cartesian_coordinates
% 0 0 1 0 0 0 0 0 0 0 0 1 0 0: turbulent transient incompressible isothermal cartesian_coordinates data_geom
% 0 0 1 0 0 0 0 0 0 0 0 0 0 1: turbulent steady    incompressible isothermal cartesian_coordinates two-dimensional
S.Options.Vector=X;
Transient=X(12);
MovingGrid=X(13);
S.Options.TwoDimensional=X(14);
S.Options.Turbulent=X(3);
S.Options.Steady=~Transient;
S.Options.SlidingGrid=MovingGrid;
switch X(9)
    case 1
        S.Options.Coordinates='Cylindrical';
    case 0
        S.Options.Coordinates='Cartesian';
end

DataGroups=strmatch('----',{S.Entry(:).Name});
NumTimes=length(DataGroups);
S.Convection(S.NumPhases,NumTimes)=0; % initialize CONVECTION COEFFICIENTS array
S.RefPressure(1,NumTimes)=0; % initialize REFERENCE PRESSURE array
if MovingGrid
    S.XEntry=zeros(1,NumTimes);
end
for i=1:NumTimes
    EntryNr=DataGroups(i)+1;
    % skip the LAST SET OF DATA entry when present
    if strcmp(S.Entry(EntryNr).Name,'LAST SET OF DATA')
        EntryNr=EntryNr+1;
    end
    % read TIME STEP NUMBER
    fseek(fid,S.Entry(EntryNr).Loc,-1);
    if BinaryDmpFile
        X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
    else
        X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
    end
    EntryNr=EntryNr+1;
    S.TimeStepNum(i)=X;
    if Transient
        fseek(fid,S.Entry(EntryNr).Loc,-1);
        if BinaryDmpFile
            X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
        else
            X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
        end
        EntryNr=EntryNr+1;
        S.Time(i)=X(1);
        S.TimeStep(i)=X(2);
    else % stationary flow
        S.Time=0;
        S.TimeStep=0;
    end
    % skip the field RESULTS AT END OF TIME STEP
    EntryNr=EntryNr+1;
    if (i==1) || MovingGrid
        if ~strcmp(S.Entry(EntryNr).Name,'ARRAY XNN'),
            error('Missing ARRAY XNN field for time step %i.',i)
        else
            % read XNN
            S.XEntry(i)=EntryNr;
            EntryNr=EntryNr+1;
        end
    else
        if strcmp(S.Entry(EntryNr).Name,'ARRAY XNN')
            warning('Unexpected ARRAY XNN field encountered')
            EntryNr=EntryNr+1;
        end
    end
    for phase=1:S.NumPhases
        % skip PHASE NUMBER
        EntryNr=EntryNr+1;
        for var=1:S.NumVars
            if (phase==1) && (i==1)
                % read VARIABLE NAME
                fseek(fid,S.Entry(EntryNr).Loc,-1);
                if BinaryDmpFile
                    X=Local_read_bindmp_data(fid,S.Entry(EntryNr).DataType,S.Entry(EntryNr).Size);
                else
                    X=Local_read_ascdmp_data(fid,S.Entry(EntryNr).DataType,[S.Entry(EntryNr).Size S.Entry(EntryNr).Length]);
                end
                EntryNr=EntryNr+1;
                S.Variable(var).Name=deblank(X);
                S.Variable(var).CellData(S.NumPhases,NumTimes)=0; % initialize
            else
                % skip VARIABLE NAME
                EntryNr=EntryNr+1;
            end
            % store entrynr of VALUES AT CELL CENTERS
            S.Variable(var).CellData(phase,i)=EntryNr;
            EntryNr=EntryNr+1;
            % skip VALUES AT BOUNDARIES: always entrynr of VALUES AT CELL CENTERS+1
            EntryNr=EntryNr+1;
        end
        % store entrynr of CONVECTION COEFFICIENTS
        S.Convection(phase,i)=EntryNr;
        EntryNr=EntryNr+1;
    end
    % skip TMULT V
    EntryNr=EntryNr+1;
    % skip REFERENCE PRESSURE
    S.RefPressure(i)=EntryNr; % Why not read now?
    EntryNr=EntryNr+1;
end


function VarN=Local_vars_dmp_file(FileInfo,Block)
VarN='';
switch nargin
    case 0
        uiwait(msgbox('Insufficient parameters.','modal'));
        return
    case 1
        Block='block';
end
switch lower(Block)
    case 'block'
        VarN={FileInfo.Variable(:).Name, ...
            'X COORDINATES', 'Y COORDINATES', 'Z COORDINATES', ...
            'U CONVECTION', 'V CONVECTION', 'W CONVECTION'}' ;
    case 'patch'
        VarN={FileInfo.Variable(:).Name, ...
            'X COORDINATES', 'Y COORDINATES', 'Z COORDINATES'}' ;
end

function Data=Local_read_dmp_file(FileInfo,Variable,Block,BlockNr,Phase,TimeStepNr)
Data=[];
switch nargin
    case {0,1}
        uiwait(msgbox('Insufficient parameters.','modal'));
        return;
    case 2 % Variable
        Block='BLOCK';
        BlockNr=1:FileInfo.NumBlocks;
        Phase=1;
        TimeStepNr=length(FileInfo.TimeStepNum);
    case 3
        if ischar(Variable) && ischar(Block) % Variable, Block
            Phase=1;
            TimeStepNr=length(FileInfo.TimeStepNum);
            switch lower(Block)
                case 'patch'
                    BlockNr=1:FileInfo.NumPatches;
                case 'block'
                    BlockNr=1:FileInfo.NumBlocks;
                otherwise
                    error(['Could not interpret: ' Block '.'])
            end
        elseif ischar(Variable) && strcmpi(Variable,'ENTRY') % 'ENTRY' EntryNr
            EntryNr=Block;
            fid=fopen(FileInfo.FileName,'r','b','US-ASCII');
            fseek(fid,FileInfo.Entry(EntryNr).Loc,-1);
            if strcmp(FileInfo.FileMode,'binary')
                Data=Local_read_bindmp_data(fid,FileInfo.Entry(EntryNr).DataType,FileInfo.Entry(EntryNr).Size);
            else
                Data=Local_read_ascdmp_data(fid,FileInfo.Entry(EntryNr).DataType,FileInfo.Entry(EntryNr).Size);
            end
            fclose(fid);
            return
        elseif FileInfo.NumPhases>1 % Variable, Phase
            Phase=Block;
            Block='BLOCK';
            BlockNr=1:FileInfo.NumBlocks;
            TimeStepNr=length(FileInfo.TimeStepNum);
        else % Variable, TimeStepNr
            TimeStepNr=Block;
            Block='BLOCK';
            BlockNr=1:FileInfo.NumBlocks;
            Phase=1;
        end
    case 4
        if ischar(Block) % Variable, Block, BlockNr
            Phase=1;
            TimeStepNr=length(FileInfo.TimeStepNum);
        else % Variable, Phase, TimeStepNr
            Phase=Block;
            TimeStepNr=BlockNr;
            Block='BLOCK';
            BlockNr=1:FileInfo.NumBlocks;
        end
    case 5
        if FileInfo.NumPhases>1 % Variable, Block, BlockNr, Phase
            TimeStepNr=length(FileInfo.TimeStepNum);
        else % Variable, Block, BlockNr, TimeStepNr
            TimeStepNr=Phase;
            Phase=1;
        end
    case 6 % Variable, Block, BlockNr, Phase, TimeStepNr
        % all parameters specified
end
Variable=upper(Variable);

switch upper(Block)
    case 'BLOCK'
        fid=fopen(FileInfo.FileName,'r','b','US-ASCII');
        switch Variable
            case {'X COORDINATES','Y COORDINATES','Z COORDINATES'} % XNN
                %
                % In case of Cylindrical coordinates this will return
                % X, R, Theta for X, Y, Z
                % Suggestion for extension:
                % CARTESIAN X, Y, Z, U, V, W
                % where
                %   CARTESIAN X = X COORDINATES
                %   CARTESIAN Y = Y COORDINATES * cos(Z COORDINATES)
                %   CARTESIAN Z = Y COORDINATES * sin(Z COORDINATES)
                % Note however that the U, V and W velocities are specified in the
                % cell centers. How are those defined in case of cylindrical coordinates?
                % 1) average of CARTESIAN coordinates
                % 2) average of CYLINDRICAL coordinates converted to CARTESIAN coordinates
                % This choice determines the Z COORDINATES (Theta) of the cell centers
                % that can be used in the following expressions:
                %
                %   CARTESIAN U = U VELOCITY
                %   CARTESIAN V = V VELOCITY * cos(Z COORDINATES) - W VELOCITY * sin(Z COORDINATES)
                %   CARTESIAN W = V VELOCITY * sin(Z COORDINATES) + W VELOCITY * cos(Z COORDINATES)
                %
                if length(FileInfo.XEntry)==1 % fixed grid
                    EntryNr=FileInfo.XEntry;
                else
                    EntryNr=FileInfo.XEntry(TimeStepNr);
                end
                fseek(fid,FileInfo.Entry(EntryNr).Loc,-1);
                switch Variable
                    case {'X COORDINATES'}
                        x=1;
                    case {'Y COORDINATES'}
                        x=2;
                    case {'Z COORDINATES'}
                        x=3;
                end
                Data=cell(1,FileInfo.NumBlocks);
                if strcmp(FileInfo.FileMode,'binary')
                    NriCB=0;
                    for i=1:x
                        for b=1:FileInfo.NumBlocks
                            if (i==x) && ismember(b,BlockNr)
                                [Data{b},NriCB]=Local_read_bindmp_datapart(fid,'R',FileInfo.Block(b).IJK1,NriCB);
                                Data{b}=reshape(Data{b},[FileInfo.Block(b).I+1 FileInfo.Block(b).J+1 FileInfo.Block(b).K+1]);
                            else
                                [NriCB]=Local_skip_bindmp_datapart(fid,'R',FileInfo.Block(b).IJK1,NriCB);
                            end
                        end
                    end
                else % ascii
                    NoCL=FileInfo.Entry(EntryNr).NPerLine;
                    for i=1:x
                        for b=1:FileInfo.NumBlocks
                            if (i==x) && ismember(b,BlockNr)
                                [Data{b},NoCL]=Local_read_ascdmp_datapart(fid,'R', ...
                                    [FileInfo.Block(b).IJK1 FileInfo.Entry(EntryNr).Length], ...
                                    NoCL,FileInfo.Entry(EntryNr).NPerLine);
                                Data{b}=reshape(Data{b},[FileInfo.Block(b).I+1 FileInfo.Block(b).J+1 FileInfo.Block(b).K+1]);
                            else
                                [NoCL]=Local_skip_ascdmp_datapart(fid,'R', ...
                                    [FileInfo.Block(b).IJK1 FileInfo.Entry(EntryNr).Length], ...
                                    NoCL,FileInfo.Entry(EntryNr).NPerLine);
                            end
                        end
                    end
                end
            case {'U CONVECTION','V CONVECTION','W CONVECTION'} % CONVECTION COEFFICIENTS
                EntryNr=FileInfo.Convection(Phase,TimeStepNr);
                fseek(fid,FileInfo.Entry(EntryNr).Loc,-1);
                switch Variable
                    case {'U CONVECTION'}
                        x=1;
                    case {'V CONVECTION'}
                        x=2;
                    case {'W CONVECTION'}
                        x=3;
                end
                Data=cell(1,FileInfo.NumBlocks);
                if strcmp(FileInfo.FileMode,'binary')
                    NriCB=0;
                    for i=1:x
                        for b=1:FileInfo.NumBlocks
                            if (i==x) && ismember(b,BlockNr)
                                [Data{b},NriCB]=Local_read_bindmp_datapart(fid,'R',FileInfo.Block(b).IJK,NriCB);
                                Data{b}=reshape(Data{b},[FileInfo.Block(b).I FileInfo.Block(b).J FileInfo.Block(b).K]);
                            else
                                [NriCB]=Local_skip_bindmp_datapart(fid,'R',FileInfo.Block(b).IJK,NriCB);
                            end
                        end
                    end
                else % ascii
                    NoCL=FileInfo.Entry(EntryNr).Length;
                    for i=1:x
                        for b=1:FileInfo.NumBlocks
                            if (i==x) && ismember(b,BlockNr)
                                [Data{b},NoCL]=Local_read_ascdmp_datapart(fid,'R', ...
                                    [FileInfo.Block(b).IJK FileInfo.Entry(EntryNr).Length], ...
                                    NoCL,FileInfo.Entry(EntryNr).NPerLine);
                                Data{b}=reshape(Data{b},[FileInfo.Block(b).I FileInfo.Block(b).J FileInfo.Block(b).K]);
                            else
                                [NoCL]=Local_skip_ascdmp_datapart(fid,'R', ...
                                    [FileInfo.Block(b).IJK FileInfo.Entry(EntryNr).Length], ...
                                    NoCL,FileInfo.Entry(EntryNr).NPerLine);
                            end
                        end
                    end
                end
            otherwise % CFX VARIABLE
                if ischar(Variable)
                    % Variable=strmatch(Variable,{FileInfo.Variable(:).Name},'exact');
                    % if isempty(Variable),
                    Variable=ustrcmpi(Variable,{FileInfo.Variable(:).Name});
                    if Variable<0
                        fclose(fid);
                        return
                    end
                end
                EntryNr=FileInfo.Variable(Variable).CellData(Phase,TimeStepNr);
                fseek(fid,FileInfo.Entry(EntryNr).Loc,-1);
                Data=cell(1,FileInfo.NumBlocks);
                if strcmp(FileInfo.FileMode,'binary')
                    NriCB=0;
                    for b=1:FileInfo.NumBlocks
                        if ismember(b,BlockNr)
                            [Data{b},NriCB]=Local_read_bindmp_datapart(fid,FileInfo.Entry(EntryNr).DataType,FileInfo.Block(b).IJK,NriCB);
                            Data{b}=reshape(Data{b},[FileInfo.Block(b).I FileInfo.Block(b).J FileInfo.Block(b).K]);
                        else
                            [NriCB]=Local_skip_bindmp_datapart(fid,FileInfo.Entry(EntryNr).DataType,FileInfo.Block(b).IJK,NriCB);
                        end
                    end
                else % ascii
                    NoCL=FileInfo.Entry(EntryNr).Length;
                    for b=1:FileInfo.NumBlocks
                        if ismember(b,BlockNr)
                            [Data{b},NoCL]=Local_read_ascdmp_datapart(fid, ...
                                FileInfo.Entry(EntryNr).DataType, ...
                                [FileInfo.Block(b).IJK FileInfo.Entry(EntryNr).Length], ...
                                NoCL,FileInfo.Entry(EntryNr).NPerLine);
                            Data{b}=reshape(Data{b},[FileInfo.Block(b).I FileInfo.Block(b).J FileInfo.Block(b).K]);
                        else
                            [NoCL]=Local_skip_ascdmp_datapart(fid, ...
                                FileInfo.Entry(EntryNr).DataType, ...
                                [FileInfo.Block(b).IJK FileInfo.Entry(EntryNr).NPerLine], ...
                                NoCL,FileInfo.Entry(EntryNr).NPerLine);
                        end
                    end
                end
        end
        fclose(fid);
    case 'PATCH'
        switch Variable
            case {'X COORDINATES','Y COORDINATES','Z COORDINATES'} % XNN
                % get block data
                % if the dmp-file is open when calling the cfx statement,
                % it will be close in the cfx statement!
                x=cfx('readdmp',FileInfo,Variable,Phase,TimeStepNr);
                % extract the appropriate patches from the block data
                for p=1:FileInfo.NumPatches
                    if ismember(p,BlockNr)
                        I=FileInfo.Patch(p).Imin:(FileInfo.Patch(p).Imax+1);
                        J=FileInfo.Patch(p).Jmin:(FileInfo.Patch(p).Jmax+1);
                        K=FileInfo.Patch(p).Kmin:(FileInfo.Patch(p).Kmax+1);
                        switch FileInfo.Patch(p).Direc
                            case 1 % high I
                                I=max(I);
                            case 2 % high J
                                J=max(J);
                            case 3 % high K
                                K=max(K);
                            case 4 % low I
                                I=min(I);
                            case 5 % low J
                                J=min(J);
                            case 6 % low K
                                K=min(K);
                        end
                        Data{p}=x{FileInfo.Patch(p).Block}(I,J,K);
                        Sz=size(Data{p});
                        DummyDim=mod(FileInfo.Patch(p).Direc-1,3)+1;
                        if DummyDim<=2
                            Sz(DummyDim)=[];
                        end
                        Data{p}=reshape(Data{p},Sz);
                    end
                end
            otherwise % CFX VARIABLE
                fid=fopen(FileInfo.FileName,'r','b','US-ASCII');
                if ischar(Variable)
                    Variable=strmatch(Variable,{FileInfo.Variable(:).Name},'exact');
                    if isempty(Variable)
                        fclose(fid);
                        return
                    end
                end
                EntryNr=FileInfo.Variable(Variable).CellData(Phase,TimeStepNr)+1;
                fseek(fid,FileInfo.Entry(EntryNr).Loc,-1);
                Data=cell(1,FileInfo.NumPatches);
                if strcmp(FileInfo.FileMode,'binary')
                    NriCB=0;
                    for p=1:FileInfo.NumPatches
                        if ismember(p,BlockNr)
                            [Data{p},NriCB]=Local_read_bindmp_datapart(fid,FileInfo.Entry(EntryNr).DataType,FileInfo.Patch(p).IJK,NriCB);
                            Data{p}=reshape(Data{p},[FileInfo.Patch(p).Imax-FileInfo.Patch(p).Imin+1 ...
                                FileInfo.Patch(p).Jmax-FileInfo.Patch(p).Jmin+1 ...
                                FileInfo.Patch(p).Kmax-FileInfo.Patch(p).Kmin+1]);
                            Sz=size(Data{p});
                            DummyDim=mod(FileInfo.Patch(p).Direc-1,3)+1;
                            if DummyDim<=2
                                Sz(DummyDim)=[];
                            end
                            Data{p}=reshape(Data{p},[Sz 1]);
                        else
                            [NriCB]=Local_skip_bindmp_datapart(fid,FileInfo.Entry(EntryNr).DataType,FileInfo.Patch(p).IJK,NriCB);
                        end
                    end
                else % ascii
                    NoCL=FileInfo.Entry(EntryNr).Length;
                    for p=1:FileInfo.NumPatches
                        if ismember(p,BlockNr)
                            [Data{p},NoCL]=Local_read_ascdmp_datapart(fid, ...
                                FileInfo.Entry(EntryNr).DataType, ...
                                [FileInfo.Patch(p).IJK FileInfo.Entry(EntryNr).Length], ...
                                NoCL,FileInfo.Entry(EntryNr).NPerLine);
                            Data{p}=reshape(Data{p},[FileInfo.Patch(p).Imax-FileInfo.Patch(p).Imin+1 ...
                                FileInfo.Patch(p).Jmax-FileInfo.Patch(p).Jmin+1 ...
                                FileInfo.Patch(p).Kmax-FileInfo.Patch(p).Kmin+1]);
                            Sz=size(Data{p});
                            DummyDim=mod(FileInfo.Patch(p).Direc-1,3)+1;
                            if DummyDim<=2
                                Sz(DummyDim)=[];
                            end
                            Data{p}=reshape(Data{p},Sz);
                        else
                            [NoCL]=Local_skip_ascdmp_datapart(fid, ...
                                FileInfo.Entry(EntryNr).DataType, ...
                                [FileInfo.Patch(p).IJK FileInfo.Entry(EntryNr).Length], ...
                                NoCL,FileInfo.Entry(EntryNr).NPerLine);
                        end
                    end
                end
                fclose(fid);
        end
end


function Local_skip_bindmp_data(fid,DataType,Size)
switch DataType
    case 'C'
        fseek(fid,Size(2)*Size(1)+8*Size(3),0);
    case {'I','R'}
        fseek(fid,Size(2)*4+8*Size(3),0);
end


function M=Local_read_bindmp_data(fid,DataType,Size)
M=[];
switch DataType
    case 'C'
        M=char(zeros(1,Size(1)*Size(2)));
        Mi=1;
        for i=1:Size(3)
            NBytes=fread(fid,1,'int32');
            M(Mi+(1:(NBytes))-1)=fread(fid,NBytes,'*char');
            Mi=Mi+NBytes;
            NBytes=fread(fid,1,'int32');
        end
        M=transpose(reshape(M,[Size(1) Size(2)]));
        NBytes=fread(fid,1,'int32');
    case 'I'
        M=zeros(1,Size(2));
        Mi=1;
        for i=1:Size(3)
            NBytes=fread(fid,1,'int32');
            M(Mi+(1:(NBytes/4))-1)=fread(fid,NBytes/4,'int32');
            Mi=Mi+NBytes/4;
            NBytes=fread(fid,1,'int32');
        end
    case 'R'
        M=zeros(1,Size(2));
        Mi=1;
        for i=1:Size(3)
            NBytes=fread(fid,1,'int32');
            M(Mi+(1:(NBytes/4))-1)=fread(fid,NBytes/4,'float32');
            Mi=Mi+NBytes/4;
            NBytes=fread(fid,1,'int32');
        end
end


function [NriCB]=Local_skip_bindmp_datapart(fid,DataType,Size,NriCB)
% NriCB: Number of values remaining in the Current Block
% if NriCB==0, the header NBytes must be read, before the values can be read
% if NriCB becomes 0, the trailer NBytes will be read
if nargin==3
    NriCB=0;
end
switch DataType
    case 'C'
        Mi=1;
        while Mi<Size
            if NriCB==0
                NriCB=fread(fid,1,'int32');
            end
            % now read minimum of Number of remaining to be read and Number in Current Block
            ReadNow=min(Size-Mi+1,NriCB);
            fseek(fid,ReadNow,0);
            NriCB=NriCB-ReadNow;
            Mi=Mi+ReadNow;
            if NriCB==0 % skip trailing size
                fread(fid,1,'int32');
            end
        end
    case {'I','R'}
        Mi=1;
        while Mi<Size
            if NriCB==0
                NriCB=fread(fid,1,'int32')/4;
            end
            % now read minimum of Number of remaining to be read and Number in Current Block
            ReadNow=min(Size-Mi+1,NriCB);
            fseek(fid,ReadNow*4,0);
            NriCB=NriCB-ReadNow;
            Mi=Mi+ReadNow;
            if NriCB==0 % skip trailing size
                fread(fid,1,'int32');
            end
        end
end


function [M,NriCB]=Local_read_bindmp_datapart(fid,DataType,Size,NriCB)
% NriCB: Number of values remaining in the Current Block
% if NriCB==0, the header NBytes must be read, before the values can be read
% if NriCB becomes 0, the trailer NBytes will be read
if nargin==3
    NriCB=0;
end
M=[];
switch DataType
    case 'C'
        M=char(zeros(Size,1));
        Mi=1;
        while Mi<Size
            if NriCB==0
                NriCB=fread(fid,1,'int32');
            end
            % now read minimum of Number of remaining to be read and Number in Current Block
            ReadNow=min(Size-Mi+1,NriCB);
            M(Mi+(1:ReadNow)-1)=fread(fid,ReadNow,'char');
            NriCB=NriCB-ReadNow;
            Mi=Mi+ReadNow;
            if NriCB==0 % skip trailing size
                fread(fid,1,'int32');
            end
        end
    case 'I'
        M=zeros(1,Size);
        Mi=1;
        while Mi<Size
            if NriCB==0
                NriCB=fread(fid,1,'int32')/4;
            end
            % now read minimum of Number of remaining to be read and Number in Current Block
            ReadNow=min(Size-Mi+1,NriCB);
            M(Mi+(1:ReadNow)-1)=fread(fid,ReadNow,'int32');
            NriCB=NriCB-ReadNow;
            Mi=Mi+ReadNow;
            if NriCB==0 % skip trailing size
                fread(fid,1,'int32');
            end
        end
    case 'R'
        M=zeros(1,Size);
        Mi=1;
        while Mi<Size
            if NriCB==0
                NriCB=fread(fid,1,'int32')/4;
            end
            % now read minimum of Number of remaining to be read and Number in Current Block
            ReadNow=min(Size-Mi+1,NriCB);
            M(Mi+(1:ReadNow)-1)=fread(fid,ReadNow,'float32');
            NriCB=NriCB-ReadNow;
            Mi=Mi+ReadNow;
            if NriCB==0 % skip trailing size
                fread(fid,1,'int32');
            end
        end
end


function S=Local_interpret_ascdmp_file(Sin,fid)
S=Sin;
i=0;
InterpretOK=0;
while ~feof(fid)
    Line=fgetl(fid);
    if length(Line)<70
        break
    else
        DataType=Line(33);
        if isempty(strfind('CIRH',DataType))
            break
        end
    end
    Str=deblank(Line(2:31));
    if strcmp(Str,'END OF FILE')
        InterpretOK=1;
        break
    end
    i=i+1;
    S.Entry(i).Name=Str;
    S.Entry(i).DataType=DataType;
    Format=deblank(Line(35:52)); % (NPerLine (1X,SingleFormat))
    if ~isempty(Format)
        BraceOpen=strfind(Format,'(');
        S.Entry(i).NPerLine=sscanf(Format((BraceOpen(1)+1):(BraceOpen(2)-1)),'%i',1);
        switch S.Entry(i).DataType
            case 'C'
                From=strfind(Format,'A');
                To=strfind(Format,')');
            case 'I'
                From=strfind(Format,'I');
                To=strfind(Format,')');
            case 'R'
                From=strfind(Format,'E');
                To=strfind(Format,'.');
        end
        S.Entry(i).Length=sscanf(Format((From(1)+1):(To(1)-1)),'%i',1)+1;
    else
        S.Entry(i).NPerLine=1;
        S.Entry(i).Length=0;
    end
    S.Entry(i).Size=sscanf(Line(53:61),'%i',1);
    S.Entry(i).Loc=ftell(fid);
    NLines=sscanf(Line(62:70),'%i',1);
    for l=1:NLines
        Line=fgetl(fid);
    end
end
if ~InterpretOK
    uiwait(msgbox(['Cannot interpret header:''' Line ''''],'modal'))
    return;
end
S=Local_analyse_dmp_file(S,fid);


function M=Local_read_ascdmp_data(fid,DataType,Size)
M=[];
switch DataType
    case 'C'
        M=fscanf(fid,['%' sprintf('%i',Size(2)) 'c'],Size(1));
        M=transpose(reshape(M,[Size(2) Size(1)]));
        M=M(:,2:end); % remove leading space
    case 'I'
        M=fscanf(fid,'%i',Size(1));
    case 'R'
        M=fscanf(fid,'%f',Size(1));
end


function [NoCL]=Local_skip_ascdmp_datapart(fid,DataType,Size,NoCL,NpL)
% NpL: Number of values per Line
% NoCL: Number of values remaining on the Current Line
% if NoCL becomes 0, the line feed will be read
if Size(1)>NoCL
    fgetl(fid); % skip remaining on this line
    Size(1)=Size(1)-NoCL;
    NoCL=NpL;
end
for i=1:floor(Size(1)/NpL)
    fgetl(fid); % skip whole lines
end
Size(1)=Size(1)-NpL*floor(Size(1)/NpL);
if (NoCL>Size(1)) && (Size(1)>0)
    % skip part of last line
    switch DataType
        case 'C'
            fscanf(fid,['%' sprintf('%i',Size(2)) 'c'],Size(1));
        case 'I'
            fscanf(fid,'%i',Size(1));
        case 'R'
            fscanf(fid,'%f',Size(1));
    end
    NoCL=NoCL-Size(1);
end


function [M,NoCL]=Local_read_ascdmp_datapart(fid,DataType,Size,NoCL,NpL)
% NpL: Number of values per Line
% NoCL: Number of values remaining on the Current Line
% if NoCL becomes 0, the line feed will be read
M=[];
switch DataType
    case 'C'
        M=fscanf(fid,['%' sprintf('%i',Size(2)) 'c'],Size(1));
        M=transpose(reshape(M,[Size(2) Size(1)]));
        M=M(:,2:end); % remove leading space
    case 'I'
        M=fscanf(fid,'%i',Size(1));
    case 'R'
        M=fscanf(fid,'%f',Size(1));
end
if NoCL>Size(1)
    Size(1)=Size(1)-NoCL;
    Size(1)=Size(1)-NpL*floor(Size(1)/NpL);
else
    % All values were read from the current line
    % Do as if it were the last line of more lines
    Size(1)=NpL-NoCL+Size(1);
end
if Size(1)==0
    % Reading stopped exactly at the end of a line
    fgetl(fid); % read line feed
    NoCL=NpL;
else
    NoCL=NpL-Size(1);
end


function Structure=Local_interpret_geo_file(filename)
Structure.Check='NotOK';

if nargin==0
    [fn,fp]=uigetfile('*.geo');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'r','b','US-ASCII');
if fid<0
    error('Unable to open %s.',filename)
end
Structure.FileName=filename;
Structure.FileType='geo';

try
    fgetl(fid); % /* GEOMETRY FILE FOR CFX-4 FROM CFX-MESHIMPORT */
    
    Line=fgetl(fid); %       4     47     18  32800  39380  /* NBLOCK,NPATCH,NGLUE,NELEM,NPOINT */
    X=sscanf(Line,'%i',5);
    Structure.NBlock=X(1);
    Structure.NPatch=X(2);
    Structure.NGlue=X(3);
    Structure.NElement=X(4);
    Structure.NPPoint=X(5);
    
    fgetl(fid); % /* BLOCK NAMES AND SIZE (NI,NJ,NK) */
    for i=1:Structure.NBlock
        Line=fgetl(fid); %BLOCK-NUMBER-1                       190      50      18
        Structure.Block(i).Name=deblank(Line(1:32));
        X=sscanf(Line(33:end),'%i',3);
        Structure.Block(i).I=X(1);
        Structure.Block(i).J=X(2);
        Structure.Block(i).K=X(3);
    end
    
    fgetl(fid); % /* PATCH TYPE, NAME, NO., RANGE, DIREC, BLK. NO., AND LABEL */
    for i=1:Structure.NPatch
        Line=fgetl(fid); %BLKBDY  GLUE-PATCH----1HIGH-J-BLK----1           1
        Structure.Patch(i).Type=deblank(Line(1:8));
        Structure.Patch(i).Name=deblank(Line(9:42));
        % i==sscanf(Line(43:end),'%i',1);
        Line=fgetl(fid); %     1    40    20    20     1    10    2       1       1
        X=sscanf(Line,'%i',9);
        Structure.Patch(i).Imin=X(1);
        Structure.Patch(i).Imax=X(2);
        Structure.Patch(i).Jmin=X(3);
        Structure.Patch(i).Jmax=X(4);
        Structure.Patch(i).Kmin=X(5);
        Structure.Patch(i).Kmax=X(6);
        Structure.Patch(i).Direc=X(7);
        Structure.Patch(i).Block=X(8);
        Structure.Patch(i).Label=X(9);
    end
    
    fgetl(fid); % /* BLOCK TO BLOCK GLUEING INFORMATION */
    for i=1:Structure.NGlue
        Line=fgetl(fid); %       1      2      1      2      3      1
        X=sscanf(Line,'%i',6);
        Structure.Glue(i).Data=X;
    end
    
    for b=1:Structure.NBlock
        fgetl(fid); % /* VERTEX CO-ORDS (X,Y,Z) FOR BLOCK     1 */
        Structure.Block(b).Start=ftell(fid);
        % (I+1)*(J+1)*(K+1) vertices
        X=fscanf(fid,'%f');
    end
    
    Line=fgetl(fid); % /* *** LAST LINE OF GRID FILE *** */
    fclose(fid);
catch
    fclose(fid);
    rethrow(lasterror)
end
if strcmp(Line,'/* *** LAST LINE OF GRID FILE *** */')
    Structure.Check='OK';
end


function Structure=Local_write_geo_file(FileName,Structure,X,Y,Z)

if nargin<5
    error('Not enough input arguments')
end

fid=fopen(FileName,'w','b','US-ASCII');

Structure.FileName=FileName;
Structure.FileType='geo';

fprintf(fid,'/* GEOMETRY FILE FOR CFX-4 FROM MATLAB */\n');

fprintf(fid, ...
    ' %6i %6i %6i %6i %6i  /* NBLOCK,NPATCH,NGLUE,NELEM,NPOINT */\n', ...
    length(Structure.Block),length(Structure.Patch), ...
    length(Structure.Glue),Structure.NElement,Structure.NPPoint);

fprintf(fid,'/* BLOCK NAMES AND SIZE (NI,NJ,NK) */\n');
for i=1:Structure.NBlock
    fprintf(fid,'%-32s %7i %7i %7i\n', ...
        Structure.Block(i).Name,Structure.Block(i).I, ...
        Structure.Block(i).J,Structure.Block(i).K);
end

fprintf(fid,'/* PATCH TYPE, NAME, NO., RANGE, DIREC, BLK. NO., AND LABEL */\n');
for i=1:Structure.NPatch
    fprintf(fid,'%-8s%-32s %7i\n', ...
        Structure.Patch(i).Type,Structure.Patch(i).Name,i);
    fprintf(fid,'%6i %5i %5i %5i %5i %5i %4i %7i %7i\n', ...
        Structure.Patch(i).Imin,Structure.Patch(i).Imax, ...
        Structure.Patch(i).Jmin,Structure.Patch(i).Jmax, ...
        Structure.Patch(i).Kmin,Structure.Patch(i).Kmax, ...
        Structure.Patch(i).Direc,Structure.Patch(i).Block, ...
        Structure.Patch(i).Label);
end

fprintf(fid,'/* BLOCK TO BLOCK GLUEING INFORMATION */\n');
for i=1:Structure.NGlue
    fprintf(fid,'%6i %6i %6i %6i %6i %6i\n', ...
        Structure.Glue(i).Data);
end

if ~iscell(X), X={X}; end
if ~iscell(Y), X={Y}; end
if ~iscell(Z), X={Z}; end
for b=1:Structure.NBlock
    fprintf(fid,'/* VERTEX CO-ORDS (X,Y,Z) FOR BLOCK %5i */\n',b);
    % (I+1)*(J+1)*(K+1) vertices
    XYZ=transpose([X{b}(:) Y{b}(:) Z{b}(:)]);
    fprintf(fid,'%14.6e %14.6e %14.6e\n',XYZ);
end

fprintf(fid,'/* *** LAST LINE OF GRID FILE *** */\n');
fclose(fid);


function Structure=Local_interpret_fo_file(filename)
Structure.Check='NotOK';

if nargin==0
    [fn,fp]=uigetfile('*.fo');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'r','b','US-ASCII');

Structure.FileName=filename;
Structure.FileType='fo';

% scan for version
Line=fgetl(fid);
Structure.Version=str2double(Line(5:7));

% scan for problem title
Line=fgetl(fid);
while (length(Line)<30) || ~strcmp(Line(1:30),'                           ***')
    Line=fgetl(fid);
    if feof(fid)
        fclose(fid);
        return
    end
end
Structure.Title=deblank(Line(33:92));

% scan for topology
Line=fgetl(fid);
while ~strcmp(Line,' TOPOLOGY  ')
    Line=fgetl(fid);
    if feof(fid)
        fclose(fid);
        return
    end
end

% read topology
fgetl(fid); % empty line
fgetl(fid); %    BLOCK      NI      NJ      NK    BLOCK NAME
Line=fgetl(fid);
while ~isempty(Line)
    X=sscanf(Line,'%i',4);
    Nr=X(1);
    Structure.Block(Nr).I=X(2);
    Structure.Block(Nr).J=X(3);
    Structure.Block(Nr).K=X(4);
    Structure.Block(Nr).Name=shrinkStr(Line(37:end));
    Structure.Block(Nr).NDatasets=0;
    Line=fgetl(fid); % BLOCK INFO or EMPTY LINE
end
fgetl(fid); % PATCH   IST   IFN   JST   JFN   KST   KFN    WALL   BLOCK  GROUP NO.  TYPE    PATCH NAME
Line=fgetl(fid);
while ~isempty(Line)
    X=sscanf(Line,'%i',7);
    Nr=X(1);
    Structure.Patch(Nr).Imin=X(2);
    Structure.Patch(Nr).Imax=X(3);
    Structure.Patch(Nr).Jmin=X(4);
    Structure.Patch(Nr).Jmax=X(5);
    Structure.Patch(Nr).Kmin=X(6);
    Structure.Patch(Nr).Kmax=X(7);
    Structure.Patch(Nr).Wall=shrinkStr(Line(46:51));
    X=sscanf(Line(52:70),'%i',2);
    Structure.Patch(Nr).Block=X(1);
    Structure.Patch(Nr).Group=X(2);
    Structure.Patch(Nr).Type=shrinkStr(Line(71:79));
    Structure.Patch(Nr).Name=shrinkStr(Line(80:end));
    Structure.Patch(Nr).NDatasets=0;
    Line=fgetl(fid); % PATCH INFO or EMPTY LINE
end

% scan for block data, meanwhile read monitoring data
Line=fgetl(fid);
STEP=0;
monitordata=0;
while length(Line)<7 || ~strcmp(Line(1:7),' *-*-*-')
    if length(Line)>10 && strcmp(Line(1:10),'==========')
        monitordata=1;
    end
    if length(Line)>10 && strcmp(Line(1:10),'MONITORING')
        monitordata=1;
    end
    if monitordata
        monitordata=0;
        STEP=STEP+1;
        if length(Line)>10 && strcmp(Line(1:10),'==========')
            Line=fgetl(fid); %        STEP NUMBER     1      TIME =  1.000E-03      TIME STEP = 1.00E-03
            %sscanf(Line,' STEP NUMBER %i TIME = %f TIME STEP = %f');
            Structure.Monitoring.Time(STEP)=sscanf(Line,' STEP NUMBER %*i TIME = %f');
        else
            Structure.Monitoring.Time(STEP)=0;
        end
        mp=0;
        while 1
            if length(Line)>10 && strcmp(Line(1:10),'MONITORING')
                break
            end
            if length(Line)>7 && strcmp(Line(1:7),' *-*-*-')
                break
            end
            Line=fgetl(fid);
        end
        while length(Line)>10 && strcmp(Line(1:10),'MONITORING')
            if STEP==1
                Structure.Monitoring.I=str2double(Line(22:24));
                Structure.Monitoring.J=str2double(Line(26:28));
                Structure.Monitoring.K=str2double(Line(30:32));
                Structure.Monitoring.Block=shrinkStr(Line(45:end));
            end
            fgetl(fid); % ITER  I--------------ABSOLUTE RESIDUAL SOURCE SUMS--------------I I-------------FIELD VALUES AT MONITORING POINT-------------I
            fgetl(fid); %   NO.   UMOM      VMOM      WMOM      MASS      TKIN      EDIS      U VEL.    V VEL.    W VEL.    PRESS.      K        EPS.
            if mp==0
                Structure.Monitoring.Start(STEP)=ftell(fid);
                Structure.Monitoring.Iter(STEP)=0;
                mp=1;
            end
            Line=fgetl(fid);
            while ~isempty(Line)
                Structure.Monitoring.Iter(STEP)=Structure.Monitoring.Iter(STEP)+1;
                Line=fgetl(fid);
            end
        end
    else
        Line=fgetl(fid);
    end
    while isempty(Line)
        Line=fgetl(fid);
        if feof(fid)
            fclose(fid);
            return
        end
    end
end

% interpret block data
% *-*-*-  BLOCK-NUMBER-1                     PLANE K =  1         U  VELOCITY     (M/S)        -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
while ~feof(fid) && strcmp(Line(1:7),' *-*-*-')
    BlockName=shrinkStr(Line(10:40));
    NBlock=strmatch(BlockName,{Structure.Block(:).Name},'exact');
    if ~isequal(size(NBlock),[1 1])
        fprintf('Error recognizing block name in line:\n%s',Line);
        fclose(fid);
        return
    end
    NValue=Structure.Block(NBlock).NDatasets+1;
    Structure.Block(NBlock).Data(NValue).Name=shrinkStr(Line(63:94));
    Structure.Block(NBlock).Data(NValue).Start=ftell(fid);
    for KPlane=1:Structure.Block(NBlock).K
        % skip data
        for l=1:( (4+(Structure.Block(NBlock).J+2)) * ...
                ceil((Structure.Block(NBlock).I+2)/12) )
            Line=fgetl(fid);
        end
        % skip empty lines
        while isempty(Line)
            Line=fgetl(fid);
        end
    end
    Structure.Block(NBlock).NDatasets=NValue;
    while isempty(Line) && ~feof(fid)
        Line=fgetl(fid);
    end
end

% interpret patch data
for NPatch=1:length(Structure.Patch)
    if strcmp(Structure.Patch(NPatch).Type,'WALL')
        % WALL PATCH NAME: WALL1                              BLOCK NAME: BLOCK-NUMBER-1
        while ~feof(fid) && ((length(Line)<7) || ~strcmp(Line(1:7),' *-*-*-'))
            Line=fgetl(fid);
        end
        while ~feof(fid) && length(Line)>7 && strcmp(Line(1:7),' *-*-*-')
            NValue=Structure.Patch(NPatch).NDatasets+1;
            % *-*-*-  WALL1                            LOW K  PLANE AT K =  1               PRESSURE  (PA)         -*-*-*-*-*-*-*-*-*-*-*-*-*-*
            Structure.Patch(NPatch).Data(NValue).Name=shrinkStr(Line(70:102));
            Structure.Patch(NPatch).Data(NValue).Start=ftell(fid);
            switch Structure.Patch(NPatch).Wall(end) % LOW/HIGH I,J,K
                case 'K'
                    NLines=(4+(Structure.Patch(NPatch).Jmax-Structure.Patch(NPatch).Jmin+1))*ceil((Structure.Patch(NPatch).Imax-Structure.Patch(NPatch).Imin+1)/12);
                case 'J'
                    NLines=(4+(Structure.Patch(NPatch).Kmax-Structure.Patch(NPatch).Kmin+1))*ceil((Structure.Patch(NPatch).Imax-Structure.Patch(NPatch).Imin+1)/12);
                case 'I'
                    NLines=(4+(Structure.Patch(NPatch).Kmax-Structure.Patch(NPatch).Kmin+1))*ceil((Structure.Patch(NPatch).Jmax-Structure.Patch(NPatch).Jmin+1)/12);
            end
            for l=1:NLines
                Line=fgetl(fid);
            end
            Structure.Patch(NPatch).NDatasets=NValue;
            while isempty(Line) && ~feof(fid)
                Line=fgetl(fid);
            end
        end
    end
end

% interpret inlet data

% interpret outlet data

fclose(fid);
Structure.Check='OK';


function StrOut=shrinkStr(StrIn)
X=find(StrIn~=32);
MinS=min(X);
MaxS=max(X);
StrOut=StrIn(MinS:MaxS);


function Data=Local_read_geo_file(FileInfo,BlockNr)
Data=[];
if nargin<1
    uiwait(msgbox('Insufficient parameters.','modal'));
    return
elseif nargin<2
    BlockNr=1:length(FileInfo.Block);
end
fid=fopen(FileInfo.FileName,'r','b','US-ASCII');
for i=1:length(BlockNr)
    fseek(fid,FileInfo.Block(BlockNr(i)).Start,-1);
    X=fscanf(fid,'%f',[3 (FileInfo.Block(BlockNr(i)).I+1)*(FileInfo.Block(BlockNr(i)).J+1)*(FileInfo.Block(BlockNr(i)).K+1)]);
    Data{1}{i}=reshape(X(1,:),[(FileInfo.Block(BlockNr(i)).I+1) (FileInfo.Block(BlockNr(i)).J+1) (FileInfo.Block(BlockNr(i)).K+1)]);
    Data{2}{i}=reshape(X(2,:),[(FileInfo.Block(BlockNr(i)).I+1) (FileInfo.Block(BlockNr(i)).J+1) (FileInfo.Block(BlockNr(i)).K+1)]);
    Data{3}{i}=reshape(X(3,:),[(FileInfo.Block(BlockNr(i)).I+1) (FileInfo.Block(BlockNr(i)).J+1) (FileInfo.Block(BlockNr(i)).K+1)]);
end
if length(Data{1})==1
    Data{1}=Data{1}{1};
    Data{2}=Data{2}{1};
    Data{3}=Data{3}{1};
end
fclose(fid);


function Data=Local_read_fo_file(FileInfo,Entry,Nr,Field)
Data=[];
switch upper(Entry)
    case 'BLOCK'
        if nargin<4
            error('Insufficient parameters.')
        end
        if Nr<=length(FileInfo.Block)
            if ischar(Field)
                Fields={FileInfo.Block(Nr).Data(:).Name};
                FieldEXACT=strmatch(Field,Fields,'exact');
                if isequal(size(Field),[1 1]) % exact match
                    Field=FieldEXACT;
                else % no exact match, try free match
                    Field=strmatch(Field,Fields);
                end
            end
            if isnumeric(Field) && isequal(size(Field),[1 1]) && (Field>0) && (Field==round(Field)) && (Field<=FileInfo.Block(Nr).NDatasets)
                Data=Read_block_fo_file(FileInfo.FileName, ...
                    FileInfo.Block(Nr).Data(Field).Start, ...
                    FileInfo.Block(Nr).I, ...
                    FileInfo.Block(Nr).J, ...
                    FileInfo.Block(Nr).K);
            else
                error('Invalid data field indicator.')
            end
        else
            error('Invalid block number.')
        end
    case 'PATCH'
        if nargin<4
            error('Insufficient parameters.')
        end
        if Nr<=length(FileInfo.Patch)
            if ischar(Field)
                Fields={FileInfo.Patch(Nr).Data(:).Name};
                FieldEXACT=strmatch(Field,Fields,'exact');
                if isequal(size(Field),[1 1]) % exact match
                    Field=FieldEXACT;
                else % no exact match, try free match
                    Field=strmatch(Field,Fields);
                end
            end
            if isnumeric(Field) && isequal(size(Field),[1 1]) && (Field>0) && (Field==round(Field)) && (Field<=FileInfo.Patch(Nr).NDatasets)
                Data=Read_patch_fo_file(FileInfo.FileName, ...
                    FileInfo.Patch(Nr).Data(Field).Start, ...
                    FileInfo.Patch(Nr).Imax-FileInfo.Patch(Nr).Imin+1, ...
                    FileInfo.Patch(Nr).Jmax-FileInfo.Patch(Nr).Jmin+1, ...
                    FileInfo.Patch(Nr).Kmax-FileInfo.Patch(Nr).Kmin+1, ...
                    FileInfo.Patch(Nr).Wall(end));
            else
                error('Invalid data field indicator.')
            end
        else
            error('Invalid patch number.')
        end
    case 'MONITORING POINT'
        if nargin<2
            error('Insufficient parameters.')
        elseif nargin==2
            Nr=1;
        end
        if Nr<=length(FileInfo.Monitoring)
            Data=Read_monitoring_fo_file(FileInfo.FileName, ...
                FileInfo.Monitoring(Nr).Start, ...
                FileInfo.Monitoring(Nr).Iter);
        else
            error('Invalid monitoring point number.')
        end
end


function Data=Read_block_fo_file(FileName,Location,I,J,K)
fid=fopen(FileName,'r','b','US-ASCII');
if fid<0
    Data=[];
    return
end
fseek(fid,Location,-1);
Data=zeros(I+2,J+2,K);
for k=1:K
    NTotal=I+2;
    i=0;
    while NTotal>0
        fgetl(fid);
        fgetl(fid);
        DataPart=fscanf(fid,'%f',[min(12,NTotal)+1 J+2]);
        Data(i+(1:min(12,NTotal)),:,k)=DataPart(2:end,:);
        fgetl(fid); % read end of line of last data line
        fgetl(fid);
        fgetl(fid);
        i=i+min(12,NTotal);
        NTotal=NTotal-12;
    end
    for i=1:5
        fgetl(fid);
    end
end
fclose(fid);


function Data=Read_patch_fo_file(FileName,Location,I,J,K,plane)
fid=fopen(FileName,'r','b','US-ASCII');
if fid<0
    Data=[];
    return
end
fseek(fid,Location,-1);
switch plane
    case 'K' % a K-plane
        % I=I
        % J=J
    case 'J' % a J-plane
        % I=I
        J=K;
    case 'I' % an I-plane
        I=J;
        J=K;
end
Data=zeros(I,J);
NTotal=I;
i=0;
while NTotal>0
    fgetl(fid);
    fgetl(fid);
    DataPart=fscanf(fid,'%f',[min(12,NTotal)+1 J]);
    Data(i+(1:min(12,NTotal)),:)=DataPart(2:end,:);
    fgetl(fid); % read end of line of last data line
    fgetl(fid);
    fgetl(fid);
    i=i+min(12,NTotal);
    NTotal=NTotal-12;
end
fclose(fid);


function Data=Read_monitoring_fo_file(FileName,Location,NIter)
fid=fopen(FileName,'r','b','US-ASCII');
if fid<0
    Data=[];
    return
end

fseek(fid,Location(1),-1);
header=fgetl(fid);
header=sscanf(header,'%f');
nCol=length(header);
Data=zeros(sum(NIter),nCol);

j=0;
for i=1:length(Location)
    Nread=0;
    fseek(fid,Location(i),-1);
    TMP=transpose(fscanf(fid,'%f',[nCol inf]));
    szTMP=size(TMP,1);
    Data(j+(1:szTMP),:)=TMP;
    j=j+szTMP;
    Nread=Nread+szTMP;
    while szTMP<NIter % another block of data to be read ...
        Line=fgetl(fid);
        if (length(Line)<10) || ~strcmp(Line(1:10),'MONITORING')
            fclose(fid);
            return
        end
        fgetl(fid);
        fgetl(fid);
        TMP=transpose(fscanf(fid,'%f',[nCol inf]));
        szTMP=size(TMP,1);
        Data(j+(1:szTMP),:)=TMP;
        j=j+szTMP;
        Nread=Nread+szTMP;
    end
end
fclose(fid);
