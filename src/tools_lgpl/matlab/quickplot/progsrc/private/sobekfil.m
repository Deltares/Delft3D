function varargout=sobekfil(FI,domain,field,cmd,varargin)
%SOBEKFIL QP support for SOBEK-RE and Rural/Urban/River network and output files.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [TZshift   ,TZstr  ]    = XXXFIL(FI,Domain,DataFld,'timezone')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%
%   The DataFld can only be either an element of the DataProps structure.

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

%========================= GENERAL CODE =======================================

T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments')
elseif nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'getparams'
            varargout={[]};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,domain,Props)};
        return
    case 'times'
        varargout={readtim(FI,domain,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,domain,Props,varargin{:})};
        return
    case 'subfields'
        varargout={subfields(FI,domain,Props)};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        Parent=varargin{1};
        Ops=varargin{2};
        switch Props.Name
            case 'network*'
                hNew=sobek('plot',FI);
                recolor(hNew,'b',Ops.colour)
        end
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] [] [] []};
fidx=find(DimFlag);
subfield = subfields(FI,domain,Props);
if ~isempty(subfield)
    subfield = subfield{varargin{1}};
    idx(fidx(1:length(varargin)-1))=varargin(2:end);
else
    idx(fidx(1:length(varargin)))=varargin;
end
sz=getsize(FI,domain,Props);
if isempty(idx{T_})
    idx{T_}=sz(T_);
elseif isequal(idx{T_},0)
    idx{T_} = 1:sz(T_);
end
if sz(M_)>0 && (isequal(idx{M_},0) || isempty(idx{M_}))
    idx{M_}=1:sz(M_);
end
if idx{ST_}==0
    idx{ST_}=1:sz(ST_);
end

if strcmp(FI.FileType,'SOBEK River network')
    % Always ValLocation = 'NODE'
    if strcmp(Props.Name,'1D flow network (nodes only)')
        Ans.XY=FI.Node.XY;
        Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
    else
        Ans.XY=cat(1,FI.Branch.XY{FI.Grid.IBranch});
        BrLen=cellfun('size',FI.Branch.XY,1)-1;
        BrFirstPoint = zeros(1,FI.nBranches);
        BrLastPoint = zeros(1,FI.nBranches);
        SEG = zeros(sum(BrLen),2);
        offset = 0;
        Noffset = 0;
        for i = FI.Grid.IBranch
            BL = BrLen(i);
            SEG(offset+(1:BL),1) = Noffset+(1:BL)';
            offset = offset+BL;
            BrFirstPoint(i) = Noffset+1;
            BrLastPoint(i)  = Noffset+BL+1;
            %
            Noffset = Noffset+BL+1;
        end
        SEG(:,2)=SEG(:,1)+1;
        %
        for i=length(FI.Node.ID):-1:1
            Points = [BrFirstPoint(FI.Branch.IFrom==i) BrLastPoint(FI.Branch.ITo==i)];
            nodeSEG{i} = zeros(length(Points)*(length(Points)-1),2);
            j=0;
            for p1 = Points
                for p2 = Points(2:end)
                    j=j+1;
                    nodeSEG{i}(j,:) = [p1 p2];
                end
            end
        end
        SEG = cat(1,SEG,nodeSEG{:});
        %
        Ans.SEG=SEG;
        %
        Renum = zeros(FI.nNodes,1);
        Renum(idx{M_}) = 1:length(idx{M_});
        Ans.XY = Ans.XY(idx{M_},:);
        Ans.SEG = Renum(Ans.SEG);
        Ans.SEG(any(Ans.SEG==0,2),:) = [];
        %
        if Props.NVal>0
            DataFile=FI.Data{Props.Subs(1)};
            Ans.ValLocation = 'NODE';
            [t,Ans.Val]=delwaq('read',DataFile,Props.Subs(2),idx{M_},idx{T_});
            Ans.Time=t;
        end
    end
    varargout={Ans FI};
    return
end
%
dom = domains(FI,domain);
if isequal(Props.Geom,'SEG-NODE') || isequal(Props.Geom,'SEG-EDGE')
    if DimFlag(ST_)
        selected = idx{ST_};
    else
        selected = idx{M_};
    end
    Ans.XY = [];
    Ans.SEG = zeros(0,2);
    Ans.ValLocation = Props.Geom(5:end);
    switch Ans.ValLocation
        case 'NODE'
            if ~isequal(Props.Subs,0)
                DataFile=FI.Data{Props.Subs(1)};
                inodes = find(ismember(FI.Node.ID,DataFile.SegmentName));
            else
                inodes = find(ismember(FI.Node.Type,Props.DFil));
            end
            inodes = inodes(selected);
            Ans.XY=FI.Node.XY(inodes,:);
            if DimFlag(M_)
                Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
                Renum=zeros(1,FI.nNodes);
                Renum(inodes)=1:length(inodes);
                Ans.SEG=Renum(Ans.SEG);
                Ans.SEG(any(Ans.SEG==0,2),:)=[];
            end
            if Props.NVal>0
                if ~isequal(Props.Subs,0)
                    % delwaq returns array of size NSUBS x NSEGS x NTIMES
                    Ans.Val = NaN(1,length(selected),length(idx{T_}));
                    %
                    [Member,Idx] = ismember(DataFile.SegmentName,FI.Node.ID(inodes));
                    [t,Ans.Val(1,Idx(Member),:)]=delwaq('read',DataFile,Props.Subs(2),find(Member),idx{T_});
                    %
                    % need to return a matrix NTIMES x NSEGS (NSUBS = 1)
                    Ans.Val = permute(Ans.Val,[3,2,1]);
                    Ans.Time = t;
                else
                    if isempty(subfield)
                        fld = 'ID';
                    else
                        fld=subfield;
                    end
                    Ans.Val=FI.Node.(fld)(inodes)';
                end
            end
        case 'EDGE'
            if ~isequal(Props.Subs,0)
                DataFile=FI.Data{Props.Subs(1)};
                FileType=DataFile.FileType;
            else
                FileType='dummy';
            end
            if strcmp(FileType,'DelwaqMAP') || strcmp(Props.Name,'all water quality segments')
                iedges = find(ismember(FI.Branch.Type,Props.DFil));
                [~,dwqIdx] = ismember(FI.Branch.ID(iedges),FI.Delwaq.Reaches.ID);
                segNr = FI.Delwaq.Reaches.Segment(dwqIdx);
                filter = ismember(segNr,selected);
                iedges = iedges(filter);
                segNr  = segNr(filter);
            elseif ~isequal(Props.Subs,0)
                [mem,index] = ismember(FI.Branch.ID,DataFile.SegmentName(selected));
                selectedEdgeNr = FI.Branch.EdgeNr(mem);
                iselected = selected(index(mem));
                [iedges,index] = ismember(FI.Branch.EdgeNr,selectedEdgeNr);
                selected = iselected(index(iedges));
            else
                iedges = find(ismember(FI.Branch.Type,Props.DFil));
                iedges = iedges(selected);
            end
            %
            Ans.SEG=cat(2,FI.Branch.IFrom(iedges,1),FI.Branch.ITo(iedges,1));
            inodes = unique(Ans.SEG(:));
            Ans.XY=FI.Node.XY(inodes,:);
            Renum=zeros(1,FI.nNodes);
            Renum(inodes)=1:length(inodes);
            Ans.SEG=Renum(Ans.SEG);
            %
            if Props.NVal>0
                if ~isequal(Props.Subs,0)
                    Ans.Val = NaN(length(idx{T_}),length(selected));
                    %
                    if strcmp(DataFile.FileType,'DelwaqHIS')
                        [t,Ans.Val]=delwaq('read',DataFile,Props.Subs(2),selected,idx{T_});
                    else
                        [t,Ans.Val]=delwaq('read',DataFile,Props.Subs(2),segNr,idx{T_});
                    end
                    Ans.Time=t;
                elseif strcmp(Props.Name,'all reach segments: water quality segment number')
                    [~,dwqIdx] = ismember(FI.Branch.ID(iedges),FI.Delwaq.Reaches.ID);
                    Ans.Val = FI.Delwaq.Reaches.Segment(dwqIdx);
                else
                    if strcmp(Props.Name,'all reach segments: reach number')
                        fld='BrReach';
                    elseif isempty(subfield)
                        fld = 'ID';
                    else
                        fld=subfield;
                    end
                    Ans.Val=FI.Branch.(fld)(iedges)';
                end
            end
    end
else
    switch dom
        case 'Rainfall Runoff'
            % no other cases
        case 'Flow 2D'
            if Props.NVal==0
                Quant.Name = 'grid';
                Quant.Units = '';
                Quant.DimFlag = [0 0 1 1 0];
                Quant.DataInCell = 0;
            else
                Quant.Name = 'data';
                Quant.Units = '';
                Quant.DimFlag = [0 0 1 1 0];
                Quant.DataInCell = 1;
            end
            Quant.NVal = Props.NVal;
            Quant.Sign = 1;
            Quant.UseGrid = 1;
            GRID = Props.Subs;
            if GRID==0
                GRID = length(FI.Grid2D.GridID):-1:1;
            end
            for g = GRID
                Data = arcgridfil(FI.Grid2D.FileData{g},1,Quant,cmd,idx{M_},idx{N_});
                Data.XUnits = 'm';
                Data.YUnits = 'm';
                Ans(g) = Data;
            end
        case 'Flow 1D'
            Ans.XY = [];
            Ans.SEG = zeros(0,2);
            Ans.ValLocation = Props.Geom(5:end);
            if strcmp(FI.FileType,'SOBEK River network')
                % Always ValLocation = 'NODE'
                if strcmp(Props.Name,'1D flow network (nodes only)')
                    Ans.XY=FI.Node.XY;
                    Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
                else
                    Ans.XY=cat(1,FI.Branch.XY{FI.Grid.IBranch});
                    BrLen=cellfun('size',FI.Branch.XY,1)-1;
                    BrFirstPoint = zeros(1,FI.nBranches);
                    BrLastPoint = zeros(1,FI.nBranches);
                    SEG = zeros(sum(BrLen),2);
                    offset = 0;
                    Noffset = 0;
                    for i = FI.Grid.IBranch
                        BL = BrLen(i);
                        SEG(offset+(1:BL),1) = Noffset+(1:BL)';
                        offset = offset+BL;
                        BrFirstPoint(i) = Noffset+1;
                        BrLastPoint(i)  = Noffset+BL+1;
                        %
                        Noffset = Noffset+BL+1;
                    end
                    SEG(:,2)=SEG(:,1)+1;
                    %
                    for i=length(FI.Node.ID):-1:1
                        Points = [BrFirstPoint(FI.Branch.IFrom==i) BrLastPoint(FI.Branch.ITo==i)];
                        nodeSEG{i} = zeros(length(Points)*(length(Points)-1),2);
                        j=0;
                        for p1 = Points
                            for p2 = Points(2:end)
                                j=j+1;
                                nodeSEG{i}(j,:) = [p1 p2];
                            end
                        end
                    end
                    SEG = cat(1,SEG,nodeSEG{:});
                    %
                    Ans.SEG=SEG;
                    %
                    Renum = zeros(FI.nNodes,1);
                    Renum(idx{M_}) = 1:length(idx{M_});
                    Ans.XY = Ans.XY(idx{M_},:);
                    Ans.SEG = Renum(Ans.SEG);
                    Ans.SEG(any(Ans.SEG==0,2),:) = [];
                    %
                    DataIndex = idx{M_};
                    NodeHasData = 1:length(idx{M_});
                end
            elseif strcmp(Ans.ValLocation,'NODE')
                if Props.DFil==-3
                    % note: inodes will be monotonic since both Node.ID and CalcPnt have been sorted in sobek.m.
                    inodes=ismember(FI.Node.ID,FI.CalcPnt.ID(idx{ST_}));
                elseif Props.DFil==-1
                    nodetype = strtok(Props.Name);
                    if strcmp(nodetype,'all')
                        inodes=idx{M_};
                    else
                        inodes=strmatch(nodetype,FI.Node.Type);
                        inodes=inodes(idx{ST_});
                    end
                elseif Props.NVal==0
                    inodes=idx{M_};
                else
                    inodes=idx{M_};
                    DataFile=FI.Data{Props.DFil};
                    [NodeHasData,DataIndex]=ismember(FI.Node.ID(idx{M_}),DataFile.SegmentName);
                    DataIndex(~NodeHasData)=[];
                end
                Ans.XY=FI.Node.XY(inodes,:);
                if DimFlag(M_)
                    Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
                    Renum=zeros(1,FI.nNodes);
                    Renum(inodes)=1:length(inodes);
                    Ans.SEG=Renum(Ans.SEG);
                    Ans.SEG(any(Ans.SEG==0,2),:)=[];
                end
            elseif strcmp(Ans.ValLocation,'EDGE')
                Ans.XY=FI.Node.XY;
                branchtype = strtok(Props.Name);
                if strcmp(branchtype,'all')
                    iedges=idx{M_};
                else
                    iedges=strmatch(branchtype,FI.Branch.Type);
                    iedges=iedges(idx{ST_});
                end
                Ans.SEG=cat(2,FI.Branch.IFrom(iedges,1),FI.Branch.ITo(iedges,1));
                if Props.DFil~=-2
                    %
                    % Process whole network since some of the selected edges may be
                    % linked to the his-file content via edges not selected.
                    %
                    DataFile=FI.Data{Props.DFil};
                    [p,f] = fileparts(DataFile.FileName);
                    if strcmpi(f,'delwaq')
                        % indexing via segment numbers
                        [EdgeHasData,dwqIdx] = ismember(FI.Branch.ID(iedges),FI.Delwaq.Reaches.ID);
                        DataIndex = FI.Delwaq.Reaches.Segment(dwqIdx(EdgeHasData));
                    else
                        [EdgeHasData,DataIndex]=ismember(FI.Branch.ID,DataFile.SegmentName);
                        %
                        found = true;
                        while found
                            found = false;
                            for i = find(~EdgeHasData)'
                                ifn = FI.Branch.IFrom(i);
                                if ismember(FI.Node.Type(ifn), ...
                                        {'SBK_PROFILE','SBK_MEASSTAT','SBK_SBK-3B-REACH','SBK_WEIR','SBK_CULVERT','SBK_PUMP','SBK_LATERALFLOW'})
                                    ibr = find(EdgeHasData & FI.Branch.ITo==ifn);
                                    if ~isempty(ibr)
                                        EdgeHasData(i) = true;
                                        DataIndex(i) = DataIndex(ibr);
                                        found = true;
                                    end
                                end
                            end
                        end
                        %
                        % Now clip data set to selected edges
                        %
                        EdgeHasData = EdgeHasData(iedges);
                        DataIndex = DataIndex(iedges);
                        DataIndex(~EdgeHasData) = [];
                    end
                end
            end
            
            if Props.DFil==-1
                switch Props.Name
                    case 'all nodes: ID'
                        fld='ID';
                    case 'all nodes: name'
                        fld='Name';
                    case 'all nodes: type'
                        fld='Type';
                    otherwise
                        fld='ID';
                end
                Ans.Val=FI.Node.(fld)(inodes)';
            elseif Props.DFil==-2
                switch Props.Name
                    case 'all reach segments: ID'
                        fld='ID';
                    case 'all reach segments: type'
                        fld='Type';
                    case 'all reach segments: name'
                        fld='Name';
                    case 'all reach segments: reach number'
                        fld='BrReach';
                    case 'all reach segments: water quality segment number'
                        Ans.Val=repmat(NaN,length(iedges),1);
                        [dwqMtch,dwqIdx] = ismember(FI.Branch.ID(iedges),FI.Delwaq.Reaches.ID);
                        Ans.Val(dwqMtch) = FI.Delwaq.Reaches.Segment(dwqIdx(dwqMtch));
                        fld = [];
                    otherwise
                        fld='ID';
                end
                if ~isempty(fld)
                    Ans.Val=FI.Branch.(fld)(iedges);
                end
            elseif Props.DFil==-3
                Ans.Val=FI.CalcPnt.ID(idx{ST_});
            elseif Props.NVal>0
                DataFile=FI.Data{Props.DFil};
                Ans.Val = NaN(length(idx{T_}),length(idx{M_}));
                if strcmp(Ans.ValLocation,'NODE')
                    [t,Ans.Val(:,NodeHasData)]=delwaq('read',DataFile,Props.Subs,DataIndex,idx{T_});
                else
                    [t,Ans.Val(:,EdgeHasData)]=delwaq('read',DataFile,Props.Subs,DataIndex,idx{T_});
                end
                Ans.Time=t;
            end
    end
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Domains = domains(FI,d)
if strcmp(FI.FileType,'SOBEK River network')
    Domains = {};
else
    Domains = flipud(FI.Settings.Components');
    Flow1D = {'Flow 1D CF', 'Flow 1D RE', 'Flow 1D SF'};
    if any(ismember(Flow1D,Domains))
        Domains = [setdiff(Domains,Flow1D);{'Flow 1D'}];
    end
end
if nargin>1
    if isempty(Domains)
        Domains = 'Flow 1D';
    else
        Domains = Domains{d};
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [nodetypes,reachtypes] = types(component)
switch component
    case 'rr'
        nodetypes  = {'3B_BOUNDARY', '3B_CONNECTIONNODE', '3B_EXTERNALRUNOFF', '3B_FRICTION', '3B_GREENHOUSE', '3B_HBV', '3B_INDUSTRY', '3B_NAM', '3B_OPENWATER', '3B_ORIFICE', '3B_PAVED', '3B_PUMP', '3B_QH-RELATION', '3B_SACRAMENTO', '3B_SCS', '3B_UNPAVED', '3B_WEIR', '3B_WWTP', ...
            'SBK_CONN&RUNOFF','SBK_SBK-3B-REACH','SBK_SBK-3B-NODE'};
        reachtypes = {'3B_LINK', '3B_LINK_RWZI'};
    case '2d'
        nodetypes  = {'FLS_BOUNDARY', 'FLS_BOUNDARYCORNER', 'FLS_BREAKINGDAM', 'FLS_GRID', 'FLS_HISTORY', 'FLS_INIWLPOINT'};
        reachtypes = {'FLS_LINE1D2DBOUNDARY', 'FLS_LINEBOUNDARY', 'FLS_LINEHISTORY'};
    case '1d'
        nodetypes  = {'SBK_1D2DBOUNDARY', 'SBK_BOUNDARY', 'SBK_BRIDGE', 'SBK_CHANNELCONNECTION', 'SBK_CHANNELLINKAGENODE', 'SBK_CHANNEL_CONN&LAT', 'SBK_CHANNEL_STORCONN&LAT', 'SBK_CMPSTR', 'SBK_CONN&LAT', 'SBK_CONN&LAT&RUNOFF', 'SBK_CONN&MEAS', 'SBK_CONN&RUNOFF', 'SBK_CONNECTIONNODE', 'SBK_CULVERT', 'SBK_DATABASESTRUCTURE', 'SBK_EXTPUMP', 'SBK_EXTRARESISTANCE', 'SBK_EXTWEIR', 'SBK_GENERALSTRUC', 'SBK_GRIDPOINT', 'SBK_GRIDPOINTFIXED', 'SBK_LATERALFLOW', 'SBK_MEASSTAT', 'SBK_ORIFICE', 'SBK_PROFILE', 'SBK_PUMP', 'SBK_RIVERADVANCEDWEIR', 'SBK_RIVERPUMP', 'SBK_RIVERWEIR', 'SBK_SBK-3B-NODE', 'SBK_SBK-3B-REACH', 'SBK_UNIWEIR', 'SBK_WEIR'};
        reachtypes = {'SBK_CHANNEL', 'SBK_CHANNEL&LAT', 'SBK_DAMBRK', 'SBK_INTCULVERT', 'SBK_INTORIFICE', 'SBK_INTPUMP', 'SBK_INTWEIR', 'SBK_PIPE', 'SBK_PIPE&INFILTRATION', 'SBK_PIPE&RUNOFF', 'SBK_RWAPIPE&RUNOFF'};
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                           'Units' 'Geom'  'Coords' 'DimFlag' 'DataInCell' 'NVal' 'DFil' 'Subs' 'UseGrid'};
DataProps={'1D flow network'                ''      'SEG-NODE' 'xy' [0 0 6 0 0]   0           0      0      0      1};
separator={'-------'                        ''      ''         ''   [0 0 0 0 0]   0           0      0      0      0};
%
if strcmp(FI.FileType,'SOBEK River network')
    dom = 'Flow 1D';
    DataProps(2,:)=DataProps(1,:);
    DataProps{2,1}='1D flow network (nodes only)';
    nodetypes = {};
    reachtypes = {};
else
    nodetypes = unique(FI.Node.Type);
    nodetypes(cellfun('isempty',nodetypes)) = [];
    reachtypes = unique(FI.Branch.Type);
    reachtypes(cellfun('isempty',reachtypes)) = [];
    %
    dom = domains(FI,domain);
    switch dom
        case 'Flow 1D'
            [nodetypes_1d,reachtypes_1d] = types('1d');
            nodetypes = intersect(nodetypes,nodetypes_1d);
            reachtypes = intersect(reachtypes,reachtypes_1d);
            DataProps{1,8}=nodetypes;
            %
            DataProps(2,:)=DataProps(1,:);
            DataProps{2,1}='all nodes';
            DataProps{2,7}=4;
            DataProps{2,8}=nodetypes;
            %
            i0 = size(DataProps,1);
            for i = length(nodetypes):-1:1
                DataProps(i0+i,:)=DataProps(1,:);
                DataProps{i0+i,1}=[nodetypes{i} ' nodes'];
                DataProps{i0+i,5}=[0 3 0 0 0];
                DataProps{i0+i,7}=4;
                DataProps{i0+i,8}=nodetypes(i);
                DataProps{i0+i,10}=0;
            end
            %
            if isfield(FI,'CalcPnt')
                DataProps(end+1,:)=DataProps(end,:);
                DataProps{end,1}='calculation points';
                DataProps{end,8}={'SBK_CHANNELCONNECTION', 'SBK_CHANNELLINKAGENODE', 'SBK_CHANNEL_CONN&LAT', 'SBK_CHANNEL_STORCONN&LAT', 'SBK_CONN&LAT', 'SBK_CONN&LAT&RUNOFF', 'SBK_CONN&MEAS', 'SBK_CONN&RUNOFF', 'SBK_CONNECTIONNODE', 'SBK_GRIDPOINT', 'SBK_GRIDPOINTFIXED', 'SBK_SBK-3B-NODE'}';
            end
            %
            if ~isempty(reachtypes)
                DataProps(end+1,:)=separator;
                %
                DataProps(end+1,:)=DataProps(1,:);
                DataProps{end,1}='all reach segments';
                DataProps{end,3}='SEG-EDGE';
                DataProps{end,6}=1;
                DataProps{end,7}=4;
                DataProps{end,8}=reachtypes;
                %
                DataProps(end+1,:)=DataProps(end,:);
                DataProps{end,1}='all reach segments: reach number';
                DataProps{end,7}=1;
                %
                i0 = size(DataProps,1);
                for i = length(reachtypes):-1:1
                    DataProps(i0+i,:)=DataProps(i0,:);
                    DataProps{i0+i,1}=[reachtypes{i} ' reach segments'];
                    DataProps{i0+i,5}=[0 3 0 0 0];
                    DataProps{i0+i,7}=4;
                    DataProps{i0+i,8}=reachtypes(i);
                    DataProps{i0+i,10}=0;
                end
            end
        case 'Flow 2D'
            [nodetypes_2d,reachtypes_2d] = types('2d');
            nodetypes = intersect(nodetypes,nodetypes_2d);
            reachtypes = intersect(reachtypes,reachtypes_2d);
            %
            nmesh = length(FI.Grid2D.GridID);
            DataProps = {'mesh'                           ''      'sQUAD'    'xy' [0 0 inf inf 0]   0           0      0      0       1};
            if nmesh>1
                DataProps{1,1} = 'all meshes';
                DataProps(2,:) = DataProps(1,:);
                DataProps{2,1} = 'all meshes: bed levels';
                DataProps{2,2} = 'm';
                DataProps{2,6} = 1;
                DataProps{2,7} = 1;
            else % one mesh
                DataProps{1,5} = [0 0 1 1 0];
                DataProps(2,:) = DataProps(1,:);
                DataProps{1,10} = 1;
                DataProps{2,1} = 'mesh: bed levels';
                DataProps{2,2} = 'm';
                DataProps{2,6} = 1;
                DataProps{2,7} = 1;
            end
            DataProps(3,:) = separator;
            %
            i0 = size(DataProps,1);
            for i = length(nodetypes):-1:1
                DataProps(i0+i,:)=DataProps(1,:);
                DataProps{i0+i,1}=[nodetypes{i} ' nodes'];
                DataProps{i0+i,3}='SEG-NODE';
                DataProps{i0+i,5}=[0 3 0 0 0];
                DataProps{i0+i,7}=4;
                DataProps{i0+i,8}=nodetypes(i);
                DataProps{i0+i,10}=0;
            end
            DataProps(end+1,:) = separator;
            %
            i0 = size(DataProps,1);
            for i = length(reachtypes):-1:1
                DataProps(i0+i,:)=DataProps(i0,:);
                DataProps{i0+i,1}=[reachtypes{i} ' lines'];
                DataProps{i0+i,3}='SEG-EDGE';
                DataProps{i0+i,5}=[0 3 0 0 0];
                DataProps{i0+i,7}=4;
                DataProps{i0+i,8}=reachtypes(i);
                DataProps{i0+i,10}=0;
            end
            DataProps(end+1,:) = separator;
            %
            if nmesh>1
                i0 = size(DataProps,1);
                for i = 1:nmesh
                    DataProps(i0+3*(i-1)+1,:) = DataProps(1,:);
                    DataProps{i0+3*(i-1)+1,1} = ['mesh ' FI.Grid2D.GridID{i}];
                    DataProps{i0+3*(i-1)+1,5} = [0 0 1 1 0];
                    DataProps{i0+3*(i-1)+1,9} = i;
                    DataProps{i0+3*(i-1)+1,10} = i0+3*(i-1)+1;
                    %
                    DataProps(i0+3*(i-1)+2,:) = DataProps(i0+3*(i-1)+1,:);
                    DataProps{i0+3*(i-1)+2,1} = ['mesh ' FI.Grid2D.GridID{i} ': bed levels'];
                    DataProps{i0+3*(i-1)+2,2} = 'm';
                    DataProps{i0+3*(i-1)+2,6} = 1;
                    DataProps{i0+3*(i-1)+2,7} = 1;
                    if i<nmesh
                        DataProps(i0+3*(i-1)+3,:) = separator;
                    end
                end
            end
        case 'Rainfall Runoff'
            [nodetypes_rr,reachtypes_rr] = types('rr');
            nodetypes = intersect(nodetypes,nodetypes_rr);
            reachtypes = intersect(reachtypes,reachtypes_rr);
            %
            DataProps{1,1} = 'rainfall runoff network';
            DataProps{1,8} = nodetypes;
            %
            DataProps(end+1,:)=DataProps(1,:);
            DataProps{end,1}='all nodes';
            DataProps{end,7}=4;
            %
            i0 = size(DataProps,1);
            for i = length(nodetypes):-1:1
                DataProps(i0+i,:)=DataProps(1,:);
                DataProps{i0+i,1}=[nodetypes{i} ' nodes'];
                DataProps{i0+i,2}='SEG-NODE';
                DataProps{i0+i,5}=[0 3 0 0 0];
                DataProps{i0+i,7}=4;
                DataProps{i0+i,8}=nodetypes(i);
                DataProps{i0+i,10}=0;
            end
            DataProps(end+1,:)=separator;
            %
            DataProps(end+1,:)=DataProps(1,:);
            DataProps{end,1}='all links';
            DataProps{end,3}='SEG-EDGE';
            DataProps{end,6}=1;
            DataProps{end,7}=4;
            DataProps{end,8}=reachtypes;
            %
            i0 = size(DataProps,1);
            for i = length(reachtypes):-1:1
                DataProps(i0+i,:)=DataProps(i0,:);
                DataProps{i0+i,1}=[reachtypes{i} ' links'];
                DataProps{i0+i,3}='SEG-EDGE';
                DataProps{i0+i,5}=[0 3 0 0 0];
                DataProps{i0+i,7}=4;
                DataProps{i0+i,8}=reachtypes(i);
                DataProps{i0+i,10}=0;
            end
        case 'Water Quality 1D'
            [nodetypes_1d,reachtypes_1d] = types('1d');
            nodetypes = intersect(nodetypes,nodetypes_1d);
            reachtypes = intersect(reachtypes,reachtypes_1d);
            %
            DataProps{1,1}='all reach segments: water quality segment number';
            DataProps{1,3}='SEG-EDGE';
            DataProps{1,5}=[0 0 1 0 0];
            DataProps{1,6}=1;
            DataProps{1,7}=1;
            DataProps{1,8}=reachtypes;
            %
            DataProps(2,:) = DataProps(1,:);
            DataProps{2,1} = 'all water quality segments';
            DataProps{2,7} = 0;
        otherwise
            DataProps(1,:) = [];
    end
end
if strcmp(dom,'Flow 1D') || strcmp(dom,'Water Quality 1D')
    for i = 1:length(FI.Data)
        [pn,fn]=fileparts(FI.Data{i}.FileName);
        fn=lower(fn);
        if xor(strcmp(dom,'Water Quality 1D'), ...
                strcmp(fn,'delwaq')) % or sobekwq?
            continue
        end
        DataProps(end+1,:)=separator;
        for j=1:length(FI.Data{i}.SubsName)
            DataProps(end+1,:)={FI.Data{i}.SubsName{j} '' 'SEG-NODE' 'xy' [1 0 6 0 0] 0 1 nodetypes [i,j] 0};
            switch fn
                case {'reachseg','reachvol'}
                    DataProps{end,3} = 'SEG-EDGE';
                    DataProps{end,6} =  1; % DataInCell
                    DataProps{end,8} = reachtypes;
                case 'delwaq'
                    DataProps{end,3} = 'SEG-EDGE';
                    DataProps{end,6} =  1; % DataInCell
                    DataProps{end,8} = reachtypes;
                    %Should process name by means of substdb of d3d_waqfil...
            end
        end
    end
end
Out=cell2struct(DataProps,PropNames,2);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,domain,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
if strcmp(FI.FileType,'SOBEK River network')
    if strcmp(Props.Name,'1D flow network (nodes only)')
        sz(M_)=length(FI.Node.ID);
    else
        sz(M_)=sum(cellfun('size',FI.Branch.XY,1));
    end
    if ~isequal(Props.Subs,0)
        FIH = FI.Data{Props.Subs(1)};
        sz(T_) = FIH.NTimes;
        %sz(M_) = FIH.NumSegm;
    end
    return
end
%
switch Props.Geom
    case 'SEG-NODE'
        szm = sum(ismember(FI.Node.Type,Props.DFil));
    case 'SEG-EDGE'
        szm = sum(ismember(FI.Branch.Type,Props.DFil));
    otherwise
        szm = 0;
end
if Props.DimFlag(M_)
    sz(M_) = szm;
else
    sz(ST_) = szm;
end
%
switch domains(FI,domain)
    case 'Rainfall Runoff'
        % no other cases
    case 'Flow 2D'
        if strcmp(Props.Geom,'sQUAD') && Props.Subs>0
            sz(M_) = FI.Grid2D.NCols(Props.Subs);
            sz(N_) = FI.Grid2D.NRows(Props.Subs);
        end
    case {'Flow 1D','Water Quality 1D'}
        if strcmp(Props.Name,'all water quality segments')
            sz(M_) = max(FI.Delwaq.Reaches.Segment);
        elseif ~isequal(Props.Subs,0)
            FIH = FI.Data{Props.Subs(1)};
            sz(T_) = FIH.NTimes;
            if strcmp(FIH.FileType,'DelwaqHIS')
                switch Props.Geom
                    case 'SEG-NODE'
                        sz(M_) = sum(ismember(FI.Node.ID,FIH.SegmentName));
                    case 'SEG-EDGE'
                        sz(M_) = sum(ismember(FI.Branch.ID,FIH.SegmentName));
                end
            else
                sz(M_) = FIH.NumSegm;
            end
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [T,Tsc]=readtim(FI,domain,Props,t)
%======================== SPECIFIC CODE =======================================
T=delwaq('read',FI.Data{Props.DFil},1,1,t);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,domain,Props)
%======================== SPECIFIC CODE =======================================
switch Props.Geom
    case 'SEG-NODE'
        S  =FI.Node.ID(ismember(FI.Node.Type,Props.DFil));
    case 'SEG-EDGE'
        ibranches = strcmp(FI.Branch.Type,Props.DFil);
        S = FI.Branch.ID(ibranches);
        for i = 1:length(S)
            ID = S{i};
            Name = FI.Branch.Name{i};
            if isempty(ID)
                S{i} = Name;
            elseif ~isempty(Name)
                S{i} = [ID ':' Name];
            end
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=subfields(FI,domain,Props)
switch Props.Name
    case {'all nodes','all links','all reach segments'}
        S = {'ID','Name','Type'};
    otherwise
        S = {};
end
% -----------------------------------------------------------------------------
