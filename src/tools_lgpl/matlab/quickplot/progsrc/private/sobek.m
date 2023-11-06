function Network=sobek(cmd,varargin)
%SOBEK Read and plot SOBEK topology.
%   Network = SOBEK('open',FileName) opens the files associated with a
%   SOBEK network. The function supports both old SOBEK-RE and new
%   SOBEK-Rural/Urban/River networks. In case of a SOBEK-RE model select
%   the DEFTOP.1 file; in case of a SOBEK-Rural/Urban/River model select
%   the NETWORK.NTW file.
%
%   SOBEK('plot',Network) plots the network in the current axes.
%
%   SOBEK('plot',Network,HisFile,Quantity,Time) plots the specified
%   quantity from the specified HIS-file at the specified time on the
%   network in the current axes.

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
        Network=localreadnetw(varargin{:});
    case 'plot'
        Network=localplot(varargin{:});
end


function l=localplot(Network,HisFile,Name,Time)
xx=Network.Branch.XYXY(:,[1 3 3]);
yy=Network.Branch.XYXY(:,[2 4 4]);
xx(:,3)=NaN;
xx=xx';
yy(:,3)=NaN;
yy=yy';

if nargin==1
    l=plot(Network.Node.XY(:,1),Network.Node.XY(:,2),'.');
    l(2)=line(xx(:),yy(:));
else
    [p,n,e]=fileparts(HisFile.FileName);
    switch lower([n e])
        case {'flowmap.his','morpmap.his','minmax.his','sedtmap.his'}
            %
            % Sobek RE
            %
            %flowmap.his
            [t,Val]=delwaq('read',HisFile,Name,0,Time);
            XY=cat(1,Network.Branch.XY{Network.Grid.IBranch});
            l=patch(XY(:,1)',XY(:,2)',Val, ...
                'marker','.', ...
                'markersize',6, ...
                'linestyle','none', ...
                'markerfacecolor','flat', ...
                'markeredgecolor','flat', ...
                'facecolor','none');
        case 'calcpnt.his'
            %
            % Sobek Rural (point data)
            %
            %calcpnt.his
            [t,Val]=delwaq('read',HisFile,Name,0,Time);
            [ID,NodeReorder,NodeIndex]=intersect(HisFile.SegmentName,Network.Node.ID);
            l=patch(Network.Node.XY(NodeIndex,1)',Network.Node.XY(NodeIndex,2)',Val(NodeReorder), ...
                'marker','.', ...
                'markersize',6, ...
                'linestyle','none', ...
                'markerfacecolor','flat', ...
                'markeredgecolor','flat', ...
                'facecolor','none');
        case 'reachseg.his'
            %
            % Sobek Rural (segment data)
            %
            %reachseg.his
            [t,Val]=delwaq('read',HisFile,Name,0,Time);
            [ID,BranchReorder,KnownBranches]=intersect(HisFile.SegmentName,Network.Branch.ID);
            MissingBranches=1:Network.nBranches;
            MissingBranches(KnownBranches)=[];
            %
            MB=ismember(Network.Node.Type(Network.Branch.IFrom(MissingBranches)), ...
                {'SBK_PROFILE','SBK_LATERALFLOW','SBK_MEASSTAT','SBK_PUMP','SBK_WEIR'});
            ProfileBranches=MissingBranches(MB);
            OtherPart=zeros(1,length(ProfileBranches));
            %
            %    '3B_OPENWATER','3B_PAVED','3B_UNPAVED','3B_WEIR','SBK_SBK-3B-NODE'
            %
            %    'SBK_BOUNDARY','SBK_CHANNELCONNECTION','SBK_CONNECTIONNODE', ...
            %    'SBK_CHANNEL_STORCONN&LAT','SBK_CONN&LAT','SBK_GRIDPOINT'
            %
            %    'SBK_LATERALFLOW','SBK_MEASSTAT','SBK_PROFILE','SBK_PUMP','SBK_WEIR',
            %
            for i=1:length(ProfileBranches)
                Ref=find(Network.Branch.ITo(KnownBranches)==Network.Branch.IFrom(ProfileBranches(i)));
                if ~isempty(Ref)
                    OtherPart(i)=Ref;
                end
            end
            stimes=0;
            while any(OtherPart==0) && stimes<5
                stimes=stimes+1;
                NewBranches=ProfileBranches(OtherPart>0);
                for i=1:length(ProfileBranches)
                    Ref=find(Network.Branch.ITo(NewBranches)==Network.Branch.IFrom(ProfileBranches(i)));
                    if ~isempty(Ref)
                        OtherPart(i)=OtherPart(ProfileBranches==NewBranches(Ref));
                    end
                end
            end
            ProfileBranches(OtherPart==0)=[];
            OtherPart(OtherPart==0)=[];
            KnownBranches=cat(2,KnownBranches,ProfileBranches);
            ValueRef=cat(2,BranchReorder,BranchReorder(OtherPart));
            SEG=[Network.Branch.IFrom Network.Branch.ITo];
            SEG=SEG(KnownBranches,[1 2]);
            Val=Val(ValueRef)';
            %
            XY=Network.Node.XY(SEG(:),:);
            SEG(:)=1:2*size(SEG,1);
            Val=repmat(Val,2,1);
            l=patch('vertices',XY, ...
                'faces',SEG, ...
                'facevertexcdata',Val, ...
                'linewidth',3, ...
                'linestyle','-', ...
                'edgecolor','flat', ...
                'facecolor','none');
    end
end


function Network=localread_sobekRE_topo(filename)

Network.FileName=filename;
Network.FileType='SOBEK River network';
Network.Check='NotOK';

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    return
end
Str=fgetl(fid);
Network.Version=1.0;
if strmatch('TP_',Str)
    Network.Version=sscanf('%f',Str(4:end));
    Str=fgetl(fid);
end

i=0;
while strmatch('NODE',Str)
    i=i+1;
    %NODE id '0' nm 'Knoop51' px 75000.0 py 1000.0 node
    Quotes=strfind(Str,'''');
    Network.Node.ID{i}    =Str(Quotes(1)+1:Quotes(2)-1);
    Network.Node.Name{i}  =Str(Quotes(3)+1:Quotes(4)-1);
    Network.Node.XY(i,1:2)=sscanf(Str(Quotes(4)+1:end),' px %f py %f',[1 2]);
    Str=fgetl(fid);
end
Nodes=Network.Node.ID;

i=0;
while strmatch('BRCH',Str)
    i=i+1;
    %BRCH id '8' nm 'BRANCH 04' bn '0' en '11' al 7327 brch
    Quotes=strfind(Str,'''');
    Network.Branch.ID{i}    =Str(Quotes(1)+1:Quotes(2)-1);
    Network.Branch.Name{i}  =Str(Quotes(3)+1:Quotes(4)-1);
    Network.Branch.From{i}  =Str(Quotes(5)+1:Quotes(6)-1);
    Network.Branch.To{i}    =Str(Quotes(7)+1:Quotes(8)-1);
    Network.Branch.Length(i)=sscanf(Str(Quotes(8)+1:end),' al %i',[1 1]);
    Str=fgetl(fid);
end

fclose(fid);

NBranch=length(Network.Branch.ID);
Network.Branch.IFrom=zeros(NBranch,1);
Network.Branch.ITo  =zeros(NBranch,1);

for i=1:NBranch
    from  =strmatch(Network.Branch.From{i},Nodes,'exact');
    to    =strmatch(Network.Branch.To{i}  ,Nodes,'exact');
    if isequal(size(from),[1 1]) && isequal(size(to),[1 1])
        Network.Branch.IFrom(i) =from;
        Network.Branch.ITo(i)   =to;
    else
        return
    end
end

Network.Branch.XYXY=cat(2,Network.Node.XY(Network.Branch.IFrom,:),Network.Node.XY(Network.Branch.ITo,:));
Branches=Network.Branch.ID;

if isequal(Str,-1)
    Network.Check='OK';
end

f=fileparts(filename);
files=dir(fullfile(f,'*.1'));
defgrd = find(strcmpi('defgrd.1',{files.name}));
if length(defgrd)==1
    fid=fopen(fullfile(f,files(defgrd).name),'r','n','US-ASCII');
else
    fid=0;
end
if fid>0
    %read grid:
    Network.Check='NotOK';
    %
    for i=1:NBranch
        Str=fgetl(fid);
        Quotes=strfind(Str,'''');
        %GRID id '1312' nm '(null)' ci '8' lc 9.9999e+009 se 0 oc 0 gr gr 'GridPoints on Branch <BRANCH 04> with length: 5684.0' PDIN 0 0 '' pdin CLTT 'Location [m]' '1/R [1/m]' cltt CLID '(null)' '(null)' clid TBLE
        %Network.Grid.ID{i}    =Str(Quotes(1)+1:Quotes(2)-1);
        %Network.Grid.Name{i}  =Str(Quotes(3)+1:Quotes(4)-1);
        Network.Grid.Branch{i}=Str(Quotes(5)+1:Quotes(6)-1);
        IBr=strmatch(Network.Grid.Branch{i},Branches,'exact');
        if isequal(size(IBr),[1 1])
            Network.Grid.IBranch(i)=IBr;
        end
        %
        Locs=fscanf(fid,'%f %f <',[2 inf]);
        Network.Grid.Location{i}=Locs(1,:);
        %
        %tble
        fgetl(fid);
        % grid
        fgetl(fid);
    end
    fclose(fid);
    Network.Check='OK';
end

f=fileparts(filename);
defstr = find(strcmpi('defstr.1',{files.name}));
if length(defstr)==1
    fid=fopen(fullfile(f,files(defstr).name),'r','n','US-ASCII');
else
    fid=0;
end
if fid>0
    %read structures:
    Network.Check='NotOK';
    %
    i=1;
    while 1
        Str=fgetl(fid);
        if ~ischar(Str)
            break
        end
        Quotes=strfind(Str,'''');
        %STRU id '01' nm 'stuw Borgh zom' ci '-1' lc 9.9999e+009 stru
        %STRU id '04' nm 'sluis   Limmel' ci '006' lc 496 stru
        %STCM id '01' nm 'stuw Borgh compound' ci '003' lc 545 stcm
        %Network.Struct.ID{i}    =Str(Quotes(1)+1:Quotes(2)-1);
        %Network.Struct.Name{i}  =Str(Quotes(3)+1:Quotes(4)-1);
        Network.Struct.Branch{i}=Str(Quotes(5)+1:Quotes(6)-1);
        lc=strfind(Str,' lc ');
        Network.Struct.Location(i)=sscanf(Str(lc:end),' lc %f',1);
        IBr=strmatch(Network.Struct.Branch{i},Branches,'exact');
        if isequal(size(IBr),[1 1])
            Network.Struct.IBranch(i)=IBr;
            Network.Struct.XY(i,1:2)=Network.Branch.XYXY(IBr,1:2)+Network.Struct.Location(i)*(Network.Branch.XYXY(IBr,3:4)-Network.Branch.XYXY(IBr,1:2))/Network.Branch.Length(IBr);
        else
            Network.Struct.IBranch(i)=NaN;
            Network.Struct.XY(i,1:2)=NaN;
        end
        i=i+1;
    end
    fclose(fid);
    Network.Check='OK';
end

for i=1:NBranch
    grd=Network.Grid.Location{Network.Grid.IBranch==i};
    strc=Network.Struct.Location(Network.Struct.IBranch==i);
    %
    % Structures get a point upstream and downstream: at the location of the
    % structure (strc) and 1 m downstream (strc+1).
    %
    Locs=unique([grd strc strc+1]);
    Network.Branch.Locations{i}=Locs;
    Network.Branch.XY{i}=repmat(Network.Branch.XYXY(i,1:2),length(Locs),1)+Locs'*(Network.Branch.XYXY(i,3:4)-Network.Branch.XYXY(i,1:2))/Network.Branch.Length(i);
end

Network.nNodes = sum(cellfun('size',Network.Branch.XY,1));
Network.nBranches = NBranch;


function Network=localreadnetw(filename)
%LOCALREADNETW Read contents of SOBEK network files.
%   NETWORK = LOCALREADNETW(FILENAME) reads the contents of SOBEK-RE or
%   SOBEK-Rural/Urban/River network files. In case of SOBEK-RE the DEFTOP.1
%   file should be selected, whereas in case of SOBEK-Rural/Urban/River the
%   NETWORK.NTW file should be selected.
%
%   Reading SOBEK-RE DEFTOP.1 and associated files has been implemented as
%   a separate function LOCALREAD_SOBEKRE_TOPO. The current function
%   implements reading the newer SOBEK-Rural/Urban/River files.

[p,n,e]=fileparts(filename);
switch lower([n e])
    case 'network.ntw'
    case 'deftop.1'
        Network=localread_sobekRE_topo(filename);
        return
    otherwise
        error('Unable to recognize filetype: %s',[n e])
end
Network.FileName=filename;
Network.FileType='SOBEK network';
Network.Check='NotOK';

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    return
end
Str=fgetl(fid);
if isempty(strmatch('"NTW',Str))
    fclose(fid);
    return
end
Network.Version=sscanf(Str(5:end),'%f',1);
if Network.Version<6.3
    fclose(fid);
    error('SOBEK Network file version %g too old.',Network.Version);
end

%
% Naming issue: the word branch is used inconsistently in the SOBEK files.
% Most commonly it refers to a reach segment/link. A reach connects
% boundary and connection nodes; structures and cross sections will be
% located on a reach. These objects divide the reach into reach segments
% (in netter referred to as links). These links are stored in the first
% section of the NTW file. The calculation points are defined on the
% reaches.
%
% In the following the word branch is used to refer to these reach segments
% or links.
%
% Count branches ...
%
Text = fread(fid,[1 inf],'char=>char');
fclose(fid);
%
Line = multiline(Text,'cell');
%
ln=1;
nLines = length(Line);
while ln<=nLines && ~isequal(Line{ln},'"*"')
    ln=ln+1;
end
nBranches = ln-1;
%
% Load data ...
%
% The parameters in each line differ for different NETWORK.NTW versions.
% The following table lists the parameters per line for the latest
% versions.
%
Versions  =[ -1 -1       6.3   6.4  6.5  6.6];
x = NaN;
Parameters={
    'S' 'BrID'            0     0    0    0
    'S' 'BrName'          1     1    1    1
    'I' 'BrReach'         2     2    2    2
    'I' 'BrType'          3     3    3    3
    'S' 'BrObjID'         4     4    4    4
    'S' 'BrUserObjID'     x     5    5    5
    'F' 'BrFrmZ'          5     6    6    6
    'F' 'BrToZ'           6     7    7    7
    'F' 'BrDepth'         7     8    8    8
    'F' 'BrLength'        8     9    9    9
    'F' 'BrLengthMap'     9    10   10   10
    'F' 'BrLengthUser'   10    11   11   11
    'F' 'BrVolume'       11    12   12   12
    'F' 'BrWidth'        12    13   13   13
%   -------
    'S' 'NdFrmID'        13    14   14   14
    'S' 'NdFrmName'      14    15   15   15
    'S' 'NdFrmArea'      15    16   16   16
    'I' 'NdFrmReach'     16    17   17   17
    'I' 'NdFrmType'      17    18   18   18
    'S' 'NdFrmObjID'     18    19   19   19
    'S' 'NdFrmUserObjID'  x    20   20   20
    'F' 'NdFrmX'         19    21   21   21
    'F' 'NdFrmY'         20    22   22   22
    'F' 'NdFrmZ'         21    23   23   23
    'F' 'NdFrmReachDist' 22    24   24   24
    'I' 'NdFrmSys'       23    25   25    x
    'S' 'NdFrmSysStr'     x     x    x   25
    'I' 'NdFrmIden'      24    26   26   26
%   -------
    'S' 'NdToID'         25    27   27   27
    'S' 'NdToName'       26    28   28   28
    'S' 'NdToArea'       27    29   29   29
    'I' 'NdToReach'      28    30   30   30
    'I' 'NdToType'       29    31   31   31
    'S' 'NdToObjID'      30    32   32   32
    'S' 'NdToUserObjID'   x    33   33   33
    'F' 'NdToX'          31    34   34   34
    'F' 'NdToY'          32    35   35   35
    'F' 'NdToZ'          33    36   36   36
    'F' 'NdToReachDist'  34    37   37   37
    'I' 'NdToSys'        35    38   38    x
    'S' 'NdToSysStr'      x     x    x   38
    'I' 'NdToIden'       36    39   39   39};
%
index = [Parameters{:,Versions==Network.Version}]+1;
if isempty(index)
    error('The network.ntw file indicates version %g. This version is not supported.',Network.Version)
end
infile = index==round(index);
Parameters = Parameters(infile,:);
%
Network.Branch = readBlock(Parameters,Text,nBranches);
%
Network.Branch.ID     = Network.Branch.BrID;
Network.Branch.Name   = Network.Branch.BrName;
Network.Branch.LinkNr = Network.Branch.BrReach;
Network.Branch.Type   = Network.Branch.BrObjID;
%
From     = Network.Branch.NdFrmID;
FromName = Network.Branch.NdFrmName;
FromType = Network.Branch.NdFrmObjID;
FromXY   = [Network.Branch.NdFrmX Network.Branch.NdFrmY];
%
To       = Network.Branch.NdToID;
ToName   = Network.Branch.NdToName;
ToType   = Network.Branch.NdToObjID;
ToXY     = [Network.Branch.NdToX Network.Branch.NdToY];
%
% Process information ...
%
PointIDs=cat(1,From,To);
Name=cat(1,FromName,ToName);
Type=cat(1,FromType,ToType);
XY=cat(1,FromXY,ToXY);
%
idummy = cellfun('isempty',Type);
XY(idummy,:) = NaN;
%
[Network.Node.ID,I,J]=unique(PointIDs);
Network.Node.XY      =XY(I,:);
Network.Node.Name    =Name(I,:);
Network.Node.Type    =Type(I,:);
Network.Branch.XYXY  =[FromXY ToXY];
Network.Branch.IFrom =J(1:nBranches);
Network.Branch.ITo   =J(nBranches+1:2*nBranches);
Network.nNodes=length(Network.Node.ID);
Network.nBranches=nBranches;
%
% Identify the branches connected to each node (link = branch)
%
Network.Node.FlowLinks = cell(size(Network.Node.ID));
for i = 1:length(Network.Node.ID)
    nm = Network.Node.ID{i};
    links = find(strcmp(nm,Network.Branch.NdFrmID) | strcmp(nm,Network.Branch.NdToID));
    links(~ismember(Network.Branch.BrObjID(links),{'SBK_CHANNEL', 'SBK_CHANNEL&LAT', 'SBK_DAMBRK', 'SBK_INTCULVERT', 'SBK_INTORIFICE', 'SBK_INTPUMP', 'SBK_INTWEIR', 'SBK_PIPE', 'SBK_PIPE&INFILTRATION', 'SBK_PIPE&RUNOFF'}))=[];
    Network.Node.FlowLinks{i} = links;
end
%
% Identify the reach number to which each branch belongs (edgenr = reachnr)
% ... unfortunately the SOBEK BrReach/LinkNr doesn't seem to be correct.
%
EdgeNr = (1:length(Network.Branch.BrID))';
changes = true;
while changes
    changes = false;
    for i = 1:length(Network.Node.ID)
        switch Network.Node.Type{i}
            case {'SBK_1D2DBOUNDARY', 'SBK_BOUNDARY', ...
                    'SBK_CHANNELCONNECTION', 'SBK_CHANNELLINKAGENODE', 'SBK_CHANNEL_CONN&LAT', 'SBK_CHANNEL_STORCONN&LAT', ...
                    'SBK_CONN&LAT', 'SBK_CONN&LAT&RUNOFF', 'SBK_CONN&MEAS', 'SBK_CONN&RUNOFF', 'SBK_CONNECTIONNODE', ...
                    'SBK_GRIDPOINT', 'SBK_GRIDPOINTFIXED'}
                % these nodes separate edges
            otherwise
                iBr = Network.Node.FlowLinks{i};
                eNr = EdgeNr(iBr);
                eMin = min(eNr);
                if max(eNr)~=eMin
                    EdgeNr(iBr) = eMin;
                    changes = true;
                end
        end
    end
end
[~,~,Network.Branch.EdgeNr] = unique(EdgeNr);
%
ln=ln+1;
nReaches = 0;
while ln<=nLines
    Str = Line{ln};
    if ~isempty(Str) && Str(1)=='['
        switch deblank(Str)
            case '[Reach description]'
                % [Reach description]
                %
                % 267 
                % "1","","WV4","WV7",0,2,125626.8,463624.3,125663.8,463701.9,85.94241,1,1000,-1
                % "2","","WV7","WV8",0,2,125587.5,463701.9,125626.8,463767.3,76.29544,1,1000,-1
                % "3","","WV8","WV9",0,2,125558.4,463767.3,125587.5,463805.1,47.7051,1,1000,-1
                %
                Parameters={
                  'S' 'ReachID'
                  'S' 'ReachName'
                  'S' 'NodeFromID'
                  'S' 'NodeToID'
                  'I' 'I1'
                  'I' 'I2'
                  'F' 'NodeFromX'
                  'F' 'NodeFromY'
                  'F' 'NodeToX'
                  'F' 'NodeToY'
                  'F' 'ReachLength'
                  'I' 'VectorSplit' % 0=Full Vector, 1=By Vector, 2=By Coord
                  'I' 'VectorSplitLen'
                  'I' 'Equidistance'}; % -1=yes, 0=no
                iReach = strfind(Text,'[Reach description]');
                TextRem = Text(iReach+19:end);
                %
                [nReaches,nCount,ErrMsg,next] = sscanf(TextRem,'%i',1);
                Network.Reach = readBlock(Parameters,TextRem(next:end),nReaches);
                %
                Network.Reach.ID     = Network.Reach.ReachID;
                Network.Reach.Name   = Network.Reach.ReachName;
                Network.Reach.FromID = Network.Reach.NodeFromID;
                Network.Reach.ToID   = Network.Reach.NodeToID;
            case '[D2Grid description]'
                % "1.20"
                % 1
                iD2Grid = strfind(Text,'[D2Grid description]');
                TextRem = Text(iD2Grid+20:end);
                [X,nCount,ErrMsg,next] = sscanf(TextRem,' "%f" %i',2);
                %Version = X(1);
                nD2Grid = X(2);
                %
                % "14","FLS_GRID","14","14",0,"..\FIXED\dem100m9_klein.asc",104,111,134815,452730,100,100,0,0,1,0,5,1,0,-1,0
                Parameters={
                    'S' 'GridID'
                    'S' 'GridType'
                    'S' 'NodeFromID'
                    'S' 'NodeToID'
                    'I' 'I1'
                    'S' 'FileName'
                    'I' 'NCols'
                    'I' 'NRows'
                    'F' 'XULCorner'
                    'F' 'YULCorner'
                    'F' 'CellSizeX'
                    'F' 'CellSizeY'
                    'I' 'I2'
                    'I' 'I3'
                    'I' 'I4'
                    'I' 'I5'
                    'I' 'I6'
                    'I' 'I7'
                    'I' 'I8'
                    'I' 'I9'
                    'I' 'I10'};
                %
                Network.Grid2D = readBlock(Parameters,TextRem(next:end),nD2Grid);
                p = fileparts(Network.FileName);
                for i = nD2Grid:-1:1
                    Network.Grid2D.FileData{i} = arcgrid('open',fullfile(p,Network.Grid2D.FileName{i}));
                end
                fprintf('%s: %i grids\n',p,nD2Grid);
            case '[Model connection node]'
            case '[Model connection branch]'
            case '[Nodes with calculationpoint]'
                iCalcPnt = strfind(Text,'[Nodes with calculationpoint]');
                TextRem = Text(iCalcPnt+29:end);
                [X,nCount,ErrMsg,next] = sscanf(TextRem,' "%f" %i',2);
                %Version = X(1);
                nCalcPnt = X(2);
                Network.CalcPnt = readBlock({'S' 'ID'},TextRem(next:end),nCalcPnt);
                %
                % to add: SBK_GRIDPOINT, SBK_GRIDPOINTFIXED,
                %         SBK_CHANNELCONNECTION, SBK_CHANNELLINKAGENODE,
                %         SBK_CHANNEL_STORCONN&LAT
                %
                inodes = ismember(Network.Node.Type, ...
                    {'SBK_GRIDPOINT', 'SBK_GRIDPOINTFIXED', 'SBK_CHANNELCONNECTION', 'SBK_CHANNELLINKAGENODE','SBK_CHANNEL_STORCONN&LAT'});
                Network.CalcPnt.ID = sort([Network.CalcPnt.ID;Network.Node.ID(inodes)]);
            case '[Reach options]'
            case '[NTW properties]'
                % "1.00"
                % 3
                % v1=4
                % v2=0
                % v3=970
                % ---> Netter version 4.00.970
        end
    end
    ln = ln+1;
end
if isfield(Network,'Reach')
    Network.Reach.IFrom = inlist(Network.Reach.FromID,Network.Node.ID);
    Network.Reach.ITo = inlist(Network.Reach.ToID,Network.Node.ID);
end
try
    Network.Description = parse_caselist(fileparts(filename));
catch
    % can't determine description from caselist file ...
end

% parse storage node properties
Network = parse_nodes(fullfile(fileparts(filename),'NODES.DAT'),Network);

Network.Settings = parse_settings(fullfile(fileparts(filename),'SETTINGS.DAT'));
if ismember('Water Quality 1D',Network.Settings.Components)
    Network.Delwaq = parse_ntrdlwq(fileparts(filename));
end

cpfilename = [filename(1:end-3) filename(end-2:end-1)-'NT'+'CP'];
Network = parse_calcpoints(cpfilename,Network,nReaches);
Network.Check='OK';


function Settings = parse_settings(filename)
if ~exist(filename,'file')
    Settings.Components = {};
    return
end
Settings = inifile('open',filename);
Components = {'Flow 1D CF','Flow 1D SF','Flow 1D RE','Flow 2D','Flow 3D','Rainfall Runoff','Morphology 1D','Water Quality 1D','Water Quality 2D','Water Quality 3D','Emission Module','Real-Time Control'};
CLabels = {'Channel','Sewer','RIVER','FLS','D3DFLOW','3B','1DMOR','DELWAQ','2DWAQ','3DWAQ','WLM','RTC'};
Included = false(size(CLabels));
for i = 1:length(CLabels)
    Included(i) = inifile('get',Settings,'General',CLabels{i},0)~=0;
end
Settings.Components = Components(Included);


function description = parse_caselist(case_folder)
[lit_folder,Case] = fileparts(case_folder);
Lines = readfile(fullfile(lit_folder,'CASELIST.CMT'));
CaseSpace = [Case ' '];
Matching = strncmp(CaseSpace,Lines,length(CaseSpace));
description = char(sscanf(Lines{Matching},'%*d ''%[^'']'))';


function Network = parse_nodes(filename,Network)
if ~exist(filename,'file')
    return
end
fid = fopen(filename,'r','n','US-ASCII');
if fid < 0
    % file doesn't exist
    return
end
initially_zero = zeros(Network.nNodes,1);
Storage.Type = initially_zero;
Storage.ReservoirArea  = initially_zero;
Storage.ReservoirLevel = initially_zero;
Storage.ReservoirTable = cell(Network.nNodes,1);
Storage.StreetArea  = initially_zero;
Storage.StreetLevel = initially_zero;
Storage.StreetTable = cell(Network.nNodes,1);
Str = fgetl(fid);
IDs = Network.Node.ID;
try
    while strncmp(Str,'NODE',4)
        S = strquotedsplit(Str);
        %
        if length(S)<3 || ~strcmp(S(1),'NODE') || ~strcmp(S(2),'id') || S{3}(1)~='''' || S{3}(end)~=''''
            error('Unable to interpret line "%s" while parsing "%s"; expect line starting with "NODE id ''<id>''"',Str,filename)
        else
            id = S{3}(2:end-1);
        end
        while ~isequal(S{end},'node')
            Str1 = fgetl(fid);
            if ~ischar(Str1)
                error('Unable to locate end string ''node'' for while parsing "%s".',filename)
            end
            S = cat(2,S,strquotedsplit(Str1));
            Str = cat(2,Str,' ',Str1);
        end
        %
        i = find(strcmp(IDs,id));
        if isempty(i)
            %seems to happen ...
            Str = fgetl(fid);
            continue
        end
        %
        if length(S)<5 || ~strcmp(S(4),'ty')
            error('Unable to interpret line "%s" while parsing "%s"; expect line starting with "NODE id ''<id>'' ty <type>"',Str,filename)
        else
            ty = S(5);
        end
        if ~ismember(ty,{'0','1','2','3'})
            % 0 = ? encountered in Geul model
            % 1 = Reservoir
            % 2 = Closed - ignore StreetArea
            % 3 = Loss - ignore StreetArea
            error('Unknown storage option %s while parsing line "%s" of "%s".',ty,Str,filename)
        else
            ty = str2double(ty);
        end
        Storage.Type(i) = ty;
        %
        Storage.ReservoirArea(i)  = getvalue(S,'ws',NaN);
        Storage.ReservoirLevel(i) = getvalue(S,'wl',NaN);
        Storage.ReservoirTable{i} = parsetable(S,'sw');
        Storage.StreetArea(i)  = getvalue(S,'ss',NaN); % might find: ss of ct ss PDIN ...
        Storage.StreetLevel(i) = getvalue(S,'ml',NaN);
        Storage.StreetTable{i} = parsetable(S,'ss');
        %
        Str = fgetl(fid);
    end
    % no more NODE data
    if ischar(Str)
        error('Unable to interpret line "%s" while parsing "%s"; expecting line starting with "NODE".',Str,filename)
    else
        % all OK
    end
catch Excep
    fclose(fid);
    rethrow(Excep);
end
fclose(fid);
Network.Node.Storage = Storage;


function v = getvalue(keys,key,default)
match = strcmp(keys,key);
if any(match)
    j = find(match);
    v = str2double(keys(j+1));
else
    v = default;
end


function T = parsetable(keys,key)
T = [];
tables = strcmp(keys,'ct');
if any(tables)
    j = find(tables);
    j = j(strcmp(keys(j+1),key));
    if ~isempty(j)
        keys = keys(j:end);
        % ct <key> PDIN 1 0 '' pdin TBLE val val < val val < val val < tble
        % 1  2     3    4 5 6  7    8    9   10  11 9+3 ...
        val1 = keys(9:3:end);
        nval = find(strcmp(val1,'tble'))-1;
        T = cell(nval,2);
        for i = 1:nval
            T{i,1} = str2double(keys{6 + 3*i});
            T{i,2} = str2double(keys{7 + 3*i});
        end
    end
end


function S = strquotedsplit(Str)
Nquotes = cumsum(Str=='''');
inString = Nquotes~=2*floor(Nquotes/2);
isBreak = Str==' ' & ~inString;
maxNParts = sum(isBreak)+1; % maximum number of strings if none is empty
S = cell(1,maxNParts);
iBreak = find(isBreak);
k = 0;
for i = 1:length(iBreak)
    if i == 1
        % add start of string until first break only if it's not empty
        if iBreak(1) > 1
            k = k+1;
            S{k} = Str(1:iBreak(1)-1);
        end
    else
        % add new strings only if they are not empty
        if iBreak(i) > iBreak(i-1)+1
            k = k+1;
            S{k} = Str(iBreak(i-1)+1:iBreak(i)-1);
        end
    end
end
% add any remaining string after last break if not empty
if isempty(iBreak)
    % special case of Str equal to one keyword
    if ~isempty(Str)
        k = k+1;
        S{k} = Str;
    end
elseif iBreak(end) < length(Str)
    k = k+1;
    S{k} = Str(iBreak(end)+1:end);
end
% reduce to actual number of strings
S = S(1:k);



function Network = parse_calcpoints(filename,Network,nReaches)
if ~exist(filename,'file')
    return
end
fid = fopen(filename,'r','n','US-ASCII');
if fid < 0
    % file doesn't exist
    return
end
%CP_1.0
Str = fgetl(fid);
if ~ischar(Str)
    % empty file
    fclose(fid);
    return
end
if strncmp(Str,'BRCH',4)
    % not CP_ version header
    CPVersion=0;
    fseek(fid,0,-1);
elseif ~strncmp(Str,'CP_',3)
    fclose(fid);
    error('Expecting "%s" to start with "CP_" while first line reads: %s',filename,Str)
else
    CPVersion=sscanf(Str(3:end),'%f',1);
end
%
% Preallocate arrays ...
%
Network.Reach.CalcPoints = cell(1,nReaches);
%
for i = 1:nReaches
    %
    % BRCH id '1' cp 1 ct bc
    % TBLE
    if i>1
        ID = fscanf(fid,'tble brch BRCH id ''%[^'']'' cp 1 ct bc TBLE');
    else
        ID = fscanf(fid,'BRCH id ''%[^'']'' cp 1 ct bc TBLE');
    end
    if ~strcmp(ID,Network.Reach.ID{i})
        fclose(fid);
        error('Expecting reach "%s", but reading reach "%s"',Network.Reach.ID{i},ID)
    end
    %
    % 575.625082042046 157.159833170141 <
    Network.Reach.CalcPoints{i} = fscanf(fid,'%f %f <',[2 inf])';
    %
    % tble brch
    % <empty line>
end
fclose(fid);


function Out = readBlock(Parameters,Text,N)
format = strcat(Parameters{:,1});
format = strrep(format,'S','%q');
format = strrep(format,'I','%d');
format = strrep(format,'F','%f');
%
NPar = size(Parameters,1);
[a{1:NPar}] = strread(Text,format,N,'delimiter',',');
%
for i=1:size(Parameters,1)
    Out.(Parameters{i,2}) = a{i};
end

function Delwaq = parse_ntrdlwq(folder)
filename = fullfile(folder,'NTRDLWQ.POI');
if ~exist(filename,'file')
    Delwaq = [];
    return
end
Lines = readfile(filename);
%
Delwaq.FileName = filename;
%POI3.0
if isempty(Lines) || (length(Lines)==1 && isempty(Lines{1}))
    % empty file
    Delwaq = [];
    return
end
assert_line(filename,Lines,1,'POI3.0')
%# Segments links and nodes
assert_line(filename,Lines,2,'# Segments links and nodes')
%Reach=0
if ~assert_line(filename,Lines,3,'Reach=0');
    % still an empty file
    Delwaq = [];
    return
end
%
NSeg = sscanf(Lines{4},'%i');
DqParts = cell(NSeg,4);
%
iLine = 4;
for i = 1:NSeg
    %Segment 1  Name:   Color: 255
    %
    % reaches of this segment
    %3,"9903_4_mQuitz1","mQuitz1_9903_5","9903_5_9903_6"
    Parts = Qstrsplit(Lines{iLine+2},',');
    % str2double(Parts{1}) is the number of labels
    DqParts{i,1} = Parts(2:end);
    DqParts{i,3} = repmat(i,length(Parts)-1,1);
    %
    % laterals? of this segment
    %1,"1"
    Parts = Qstrsplit(Lines{iLine+3},',');
    DqParts{i,2} = Parts(2:end);
    DqParts{i,4} = repmat(i,length(Parts)-1,1);
    %
    iLine = iLine+3;
end
Delwaq.Reaches.ID = removeQuotes(cat(2,DqParts{:,1})');
Delwaq.Reaches.Segment = cat(1,DqParts{:,3});

[Delwaq.Reaches.ID,reorder] = sort(Delwaq.Reaches.ID);
Delwaq.Reaches.Segment = Delwaq.Reaches.Segment(reorder);
%
%# Storage Nodes
assert_line(filename,Lines,iLine+1,'# Storage Nodes')
%64,14
values = sscanf(Lines{iLine+2},'%i,%i');
NStorage = values(2);
%
iLine = iLine+2;
for i = 1:NStorage
    % number indicating segment for storage node?
    %
    iLine = iLine+1;
end
%
%# Internal network flows
assert_line(filename,Lines,iLine+1,'# Internal network flows')
%
iLine = iLine+1;
while 1
    %"Node","0701_1542",12,1,2,0,"-0701_1542_0701_1544","0701_1540_0701_1542"
    %"Node","0-F2032W, 0-F2033W",61,2,2,1,"-31","12_16",1
    Parts = Qstrsplit(Lines{iLine+1},',');
    if ~strcmp(Parts{1},'"Node"')
        if strcmp(Lines{iLine+1},'# Default branch boundary names')
            break
        end
        error('Expecting internal network flow line to start with "Node", but encountered: %s',Lines{iLine+1})
    end
    if length(Parts)<5
        error('Unable to parse internal network flows line %i of %s\n%s\n',iLine+1,filename,Lines{iLine+1})
    end
    NExchanges = str2double(Parts{4});
    NSegments = str2double(Parts{5});
    for j = 1:NExchanges
        %1,2202,"-0701_1542_0701_1544","0701_1540_0701_1542"
        SegNrs = sscanf(Lines{iLine+1+j},'%i,%i',2);
    end
    %
    iLine = iLine+1+NExchanges;
end
%
%# Default branch boundary names
%
%# Lateral Discharges to links
%
%# Default node boundary names
%
%# Boundary flows of nodes
%
%# Boundary types - monitoring stations
%
%# Surface water types
%
%# Segment surface water types
%
%# Boundary connection points
%# Connection Points for links
%
%# Connection Points for nodes
%
%# Grid boundary aliases
%
%# Segments georeference: segnr, x, y, z
%
%# Dry waste for nodes: drywaste nr, nodeID, x, y, z, nrofsegments, seg 1, .., seg nrofsegments
%
%# Dry waste for links: drywaste nr, linkID, x, y, z, nrofsegments, seg 1, .., seg nrofsegments
%
%# History point for nodes: History nr, nodeID, x, y, z, nrofsegments, seg 1, .., seg nrofsegments
%
%# History for links: History nr, linkID, x, y, z, nrofsegments, seg 1, .., seg nrofsegments
%

function OK = assert_line(filename,Strs,i,Ref)
ok = strcmp(Strs{i},Ref);
if nargout==1
    OK = ok;
elseif ~ok
    error('Expecting line %i of "%s" to read "%s" while it actually reads: %s',i,filename,Ref,Strs{i})
end

function Lines = readfile(filename)
fid = fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Error opening %s.',filename)
end
Lines = textscan(fid,'%s','delimiter','\n','whitespace','');
Lines = Lines{1};
fclose(fid);

function Parts = Qstrsplit(Line,sep)
% quick version of strsplit - slowest part of strsplit is the handling of
% special separators and multiple separators
[Parts,~] = regexp(Line, sep, 'split', 'match');
nQuotes = cellfun(@(x)sum(x=='"'),Parts);
oddQuotes = nQuotes~=round(nQuotes/2)*2;
if any(oddQuotes)
    oddQuotes = find(oddQuotes);
    nOdd = length(oddQuotes);
    if nOdd~=round(nOdd/2)*2
        oddQuotes(end+1) = length(Parts);
        nOdd = nOdd+1;
    end
    for i = nOdd-1:-2:1
        newStr = Parts(oddQuotes(i):oddQuotes(i+1));
        newStr(2,1:end-1) = {','};
        newStr = cat(2,newStr{:});
        Parts = Parts([1:oddQuotes(i) oddQuotes(i+1)+1:end]);
        Parts{oddQuotes(i)} = newStr;
    end
end

function Strs = removeQuotes(Strs)
F = @(x) x(x~='"');
Strs = cellfun(F,Strs,'UniformOutput',false);