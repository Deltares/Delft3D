function varargout=netcdffil(FI,domain,field,cmd,varargin)
%NETCDFFIL QP support for netCDF files.
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
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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
end

OrigFI = FI;
if FI.NumDomains>1
    if isempty(domain)
        % continue
    elseif domain>FI.NumDomains || domain==0
        % continue
    else
        FI = FI.Partitions{domain};
        domain = 1;
    end
end

if nargin==2
    if ~isempty(domain) && domain > 1
        emptyParts = cell(1,FI.NumDomains);
        
        partFI = FI.Partitions{1};
        infileStruct = infile(partFI,1);
        [infileStruct.iPart] = deal(1);
        for iq = 1:length(infileStruct)
            infileStruct(iq).Partitions = emptyParts;
            infileStruct(iq).Partitions{1} = infileStruct(iq);
            
            varid = infileStruct(iq).varid;
            if iscell(varid) || isempty(varid)
                infileStruct(iq).ncVarName = infileStruct(iq).Name;
            else
                infileStruct(iq).ncVarName = partFI.Dataset(varid+1).Name;
            end
        end
        
        for ipart = 2:FI.NumDomains
            partFI = FI.Partitions{ipart};
            infileStruct2 = infile(partFI,1);
            [infileStruct2.iPart] = deal(ipart);
            for iq2 = 1:length(infileStruct2)
                infileStruct2(iq2).Partitions = emptyParts;
                infileStruct2(iq2).Partitions{ipart} = infileStruct2(iq2);
                
                varid = infileStruct2(iq2).varid;
                if iscell(varid) || isempty(varid)
                    infileStruct2(iq2).ncVarName = infileStruct2(iq2).Name;
                else
                    infileStruct2(iq2).ncVarName = partFI.Dataset(varid+1).Name;
                end
            end
            
            iq = 1;
            for iq2 = 1:length(infileStruct2)
                if strcmp(infileStruct2(iq2).Name,'-------')
                    % in case of a separator, merge it only if the
                    % reference is also a separator
                    if strcmp(infileStruct(iq).Name,'-------')
                        % merge ... nothing to do
                    else
                        % insert
                        infileStruct = insert_quantity(infileStruct,iq,infileStruct2(iq2));
                    end
                    iq = iq+1;
                else
                    iq1 = find_quantity(infileStruct2,iq2,infileStruct);
                    if iq1 > 0
                        % matching quantity found, merge with iq1
                        infileStruct(iq1).iPart = [infileStruct(iq1).iPart ipart];
                        infileStruct(iq1).Partitions{ipart} = infileStruct2(iq2).Partitions{ipart};
                        iq = iq1+1;
                    else
                        % no matching quantity not found.
                        infileStruct = insert_quantity(infileStruct,iq,infileStruct2(iq2));
                        iq = iq+1;
                    end
                end
            end
        end
        infileStruct = rmfield(infileStruct,'ncVarName');
        if domain == FI.NumDomains+1
            for i = 1:length(infileStruct)
                if infileStruct(i).DimFlag(M_)
                    infileStruct(i).DimFlag(M_) = inf;
                end
                if infileStruct(i).DimFlag(N_)
                    infileStruct(i).DimFlag(N_) = inf;
                end
            end
        end
        varargout={infileStruct};
    else
        varargout={infile(FI,domain)};
    end
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
% identify which partition actually contains this file
if isfield(FI,'Partitions')
    firstPart = Props.iPart(1);
    useFI = FI.Partitions{firstPart};
else
    useFI = FI;
end
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(useFI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(useFI,domain,Props);
        return
    case 'stations'
        varargout={readsts(useFI,Props,0)};
        return
    case 'subfields'
        varargout={getsubfields(useFI,Props,varargin{:})};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        hNew = plotthis(FI,Props,varargin{:});
        varargout={hNew FI};
        return
    otherwise
        [XYRead, DataRead, DataInCell, ZRead]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);

[subf,rec]=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin))) = varargin;
    marg = find(fidx==M_);
else
    % initialize and read indices ...
    idx(fidx(1:(length(varargin)-1))) = varargin(2:end);
    marg = 1 + find(fidx==M_);
end

if isfield(FI.Attribute,'Name')
    gAtts = {FI.Attribute.Name};
else
    gAtts = {};
end
iSource = ustrcmpi('source',gAtts);
if iSource > 0
    is_dflowfm = ~isempty(strfind(FI.Attribute(iSource).Value,'D-Flow FM'));
else
    is_dflowfm = false;
end
if FI.NumDomains>1
    args = varargin;
    spatial = false;
    if ~isempty(marg) && ~isempty(Props.Coords)
        spatial = ~isempty(strfind(Props.Coords,'xy'));
    end
    if domain == FI.NumDomains+2
        % merged partitions
        if DimFlag(K_)
            cmd = strrep(cmd, 'grid', 'z');
        else
            cmd = strrep(cmd, 'grid', '');
        end
        if marg <= numel(args)
            args{marg} = 0;
        end
        %
        if iscell(Props.varid)
            if strcmp(Props.varid{1},'stream_function') || strcmp(Props.varid{1},'net_discharge_into_cell')
                % select all M_
                for i = 1:length(Props.Partitions)
                    PProps = Props.Partitions{i};
                    PFI = FI.Partitions{i};
                    PProps.Geom = 'UGRID2D-EDGE';
                    PProps.varid = Props.varid{2};
                    PProps.DimName{M_} = PFI.Dataset(PFI.Dataset(PProps.varid+1).Mesh{2}).Mesh{6};
                    Props.Partitions{i} = PProps;
                end
                Props.Geom = 'UGRID2D-EDGE';
            end
        end
    else
        % all partitions - unmerged - only "all m" allowed ...
    end
    if isempty(cmd)
        Data = [];
    elseif ~spatial
        % read non-spatial data from the first file ... should be consistent across all files and no way to merge anyway
        Data = netcdffil(FI,1,Props,cmd,args{:});
    else
        iOut = 1;
        for i = Props.iPart
            Data2 = netcdffil(FI,i,Props.Partitions{i},cmd,args{:});
            if iOut == 1
                Data = Data2;
            else
                flds = fieldnames(Data2);
                for j = 1:length(flds)
                    Data(iOut).(flds{j}) = Data2.(flds{j});
                end
            end
            iOut = iOut + 1;
        end
    end
    if spatial && domain == FI.NumDomains+2
        % merged partitions
        partData = Data;
        Data = [];
        
        % XY values i.e. mesh
        if XYRead
            m = 1;
            Data.X = FI.MergedPartitions(m).X;
            Data.XUnits = FI.MergedPartitions(m).XYUnits;
            Data.Y = FI.MergedPartitions(m).Y;
            Data.YUnits = FI.MergedPartitions(m).XYUnits;
            Data.EdgeNodeConnect = FI.MergedPartitions(m).EdgeNodeConnect;
            Data.FaceNodeConnect = FI.MergedPartitions(m).FaceNodeConnect;
            if isfield(FI.MergedPartitions,'EdgeFaceConnect')
                Data.EdgeFaceConnect = FI.MergedPartitions(m).EdgeFaceConnect;
            end
        end
        
        valLoc = Props.Geom(end-3:end);
        if isfield(partData,'Time') && length(partData(1).Time)>1
            nPrefixDim = 1;
            prefixDim = {':'};
        else
            nPrefixDim = 0;
            prefixDim = {};
        end
        if Props.DimFlag(ST_) > 0
            nPrefixDim = nPrefixDim + 1;
            prefixDim{end+1} = ':';
        end

        % z values
        if ZRead && isfield(partData, 'Z')
            if isfield(partData, 'ZLocation')
                zLoc = partData(1).ZLocation;
            else
                zLoc = valLoc;
            end
            Data = mergePartData(Data, partData, FI, zLoc,{'Z'}, nPrefixDim, prefixDim);
            Data.ZLocation = zLoc;
        end
        
        % data values 
        Data.ValLocation = valLoc;
        if isfield(partData,'Time')
            Data.Time = partData(1).Time;
        end
        if isfield(partData,'Classes')
            Data.Classes = partData(1).Classes;
            Data.ClassVal = partData(1).ClassVal;
        end
        valFields = {'Val','XComp','YComp','NormalComp','TangentialComp'};
        Data = mergePartData(Data, partData, FI, valLoc, valFields, nPrefixDim, prefixDim);
        %
        if iscell(field.varid)
            if strcmp(field.varid{1},'stream_function') % note field is the original copy of Props
                if DataRead
                    Data.Val = compute_stream_function(Data.Val, Data.EdgeNodeConnect, FI.MergedPartitions(m).nNodes);
                end
                Data.ValLocation = 'NODE';
            elseif strcmp(Props.varid{1},'net_discharge_into_cell')
                if DataRead
                    Data.Val = compute_net_discharge_into_cell(Data.Val, Data.EdgeFaceConnect, FI.MergedPartitions(m).nFaces);
                end
                Data.ValLocation = 'FACE';
            end
        end
        if ~isequal(idx{M_},0)
            if XYRead
                switch Data.ValLocation
                    case 'NODE'
                        newINode = zeros(size(Data.X));
                        newINode(idx{M_}) = 1:length(idx{M_});
                        %
                        FNC = Data.FaceNodeConnect;
                        Mask = isnan(FNC);
                        FNC(Mask) = 1;
                        FNC = newINode(FNC);
                        FNC(Mask) = NaN;
                        FNC(any(FNC==0,2),:) = [];
                        %
                        ENC = Data.EdgeNodeConnect;
                        ENC = newINode(ENC);
                        ENC(any(ENC==0,2),:) = [];
                        %
                        Data.X = Data.X(idx{M_});
                        Data.Y = Data.Y(idx{M_});
                        Data.EdgeNodeConnect = ENC;
                        Data.FaceNodeConnect = FNC;
                    case 'EDGE'
                        Data.EdgeNodeConnect = Data.EdgeNodeConnect(idx{M_},:);
                    case 'FACE'
                        Data.FaceNodeConnect = Data.FaceNodeConnect(idx{M_},:);
                end
            end
            %
            for v = {'Val','XComp','YComp','NormalComp','TangentialComp'}
                fld = v{1};
                if isfield(partData,fld)
                    Data.(fld) = Data.(fld)(prefixDim{:},idx{M_},:);
                end
            end
            %
            if isfield(Data,'ZLocation')
                if isequal(Data.ZLocation,Data.ValLocation)
                    Data.Z = Data.Z(prefixDim{:},idx{M_},:);
                end
            end
        end
    end
    varargout = {Data OrigFI};
    return
end

% modify SubFld content for reading ...
if isempty(subf)
    % initialize and read indices ...
    Props.SubFld = rec;
else
    % initialize and read indices ...
    rec.Val = rec.Val(varargin{1},:);
    Props.SubFld = rec;
end

% select appropriate dimensions ...
sz=getsize(FI,Props);
for d_ = 1:length(DimFlag)
    if DimFlag(d_)
        %
        % Rule: if time not specified, use last time
        %
        if d_==T_ && isempty(idx{T_})
            idx{T_}=sz(T_);
        end
        %
        % Rule: if specified 0, the use all
        %
        if isequal(idx{d_},0)
            idx{d_}=1:sz(d_);
        end
        %
        % Rule: if dimension = 0, then error
        %
        if sz(d_)==0
            switch d_
                case T_
                    error('No time steps available in the file.')
                case ST_
                    error('No stations available in the file.')
                otherwise
                    error('Empty dimension encountered: unable to read the data.')
            end
        end
    end
end

% expand SubFld into higher order dimensions
if ~isempty(Props.SubFld)
    Props.DimName = cat(2,Props.DimName,Props.SubFld.Fld{:});
    idx = cat(2,idx,num2cell(Props.SubFld.Val));
end
% read data ...
ivar = get_varid(Props);
Info=FI.Dataset(ivar+1);
%
if isfield(Info.Attribute,'Name')
    Attribs = {Info.Attribute.Name};
else
    Attribs = {};
end
%
XYneeded = false;
removeTime   = 0;
activeloaded = 0;
szData = [];
nDims = sum(sz>0);
if DataRead && Props.NVal>0
    if iscell(Props.varid)
        switch Props.varid{1}
            case 'stream_function'
                edge_idx = idx;
                edge_idx{3} = 1:FI.Dimension(Info.TSMNK(3)+1).Length;
                Props.DimName{M_} = FI.Dataset(FI.Dataset(ivar+1).Mesh{3}).Mesh{6};
                [Discharge, status] = qp_netcdf_get(FI,ivar,Props.DimName,edge_idx);
                %
                meshInfo    = FI.Dataset(Info.Mesh{3});
                if isempty(meshInfo.Attribute)
                    meshAttribNames = {};
                else
                    meshAttribNames = {meshInfo.Attribute.Name};
                end
                connect     = strmatch('edge_node_connectivity',meshAttribNames,'exact');
                [EdgeNodeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
                EdgeNodeConnect(EdgeNodeConnect<0) = NaN;
                %
                % Compute stream function psi (u = dpsi/dy, v = -dpsi/dx)
                Psi = compute_stream_function(Discharge, EdgeNodeConnect, sz(3));
                %
                Ans.Val = Psi(idx{3});
            case 'net_discharge_into_cell'
                edge_idx = idx;
                edge_idx{3} = 1:FI.Dimension(Info.TSMNK(3)+1).Length;
                Props.DimName{M_} = FI.Dataset(FI.Dataset(ivar+1).Mesh{3}).Mesh{6};
                [Discharge, status] = qp_netcdf_get(FI,ivar,Props.DimName,edge_idx);
                %
                meshInfo    = FI.Dataset(Info.Mesh{3});
                if isempty(meshInfo.Attribute)
                    meshAttribNames = {};
                else
                    meshAttribNames = {meshInfo.Attribute.Name};
                end
                connect     = strmatch('edge_face_connectivity',meshAttribNames,'exact');
                if isempty(connect)
                    % TODO: reconstruct EdgeFaceConnect from EdgeNodeConnect and FaceNodeConnect.
                    error('No edge_face_connectivity found in the netCDF file.')
                else
                    [EdgeFaceConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
                    EdgeFaceConnect(EdgeFaceConnect<0) = NaN;
                end
                %
                Psi = compute_net_discharge_into_cell(Discharge, EdgeFaceConnect, sz(3));
                %
                Ans.Val = Psi(idx{3});
            case 'erosion_sedimentation'
                [data, status] = qp_netcdf_get(FI,Props.varid{2},Props.DimName,idx);
                if any(idx{T_}==1)
                    first = find(idx{T_}==1);
                    data1 = data(first(1),:);
                else
                    idx1 = idx;
                    idx1{T_} = 1;
                    [data1, status] = qp_netcdf_get(FI,Props.varid{2},Props.DimName,idx1);
                end
                for i = 1:length(idx{T_})
                    data(i,:) = data(i,:) - data1;
                end
                szData = size(data);
                %
                if length(idx{T_})==1
                    szV = [size(data) 1];
                    data = reshape(data,szV(2:end));
                    removeTime = 1;
                end
                %
                Ans.Val = data;
            case {'node_index','edge_index','face_index'}
                Ans.Val = idx{3}(:);
            otherwise
                error('Special case "%s" not yet implemented.',Props.varid{1})
        end
    else
        for ii=1:length(Props.varid)
            if ~isempty(Info.CharDim)
                cdim = Info.Dimension(Info.CharDim==Info.Dimid);
                cidx = 1:FI.Dimension(Info.CharDim+1).Length;
                [data, status] = qp_netcdf_get(FI,Props.varid(ii),[Props.DimName cdim],[idx {cidx}]);
                data = deblank(num2cell(data,ndims(data)));
            else
                [data, status] = qp_netcdf_get(FI,Props.varid(ii),Props.DimName,idx);
            end
            szData = size(data);
            %
            if Props.DimFlag(T_) && length(idx{T_})==1
                szV = [size(data) 1];
                data = reshape(data,szV(2:end));
                removeTime = 1;
            end
            %
            if ii==1
                if length(Props.varid)==1
                    Ans.Val = data;
                else
                    Ans.XComp = data;
                end
            elseif ii==2
                Ans.YComp = data;
            else
                Ans.ZComp = data;
            end
        end
    end
    %
    switch Props.VectorDef
        case {-4,4}
            if Props.VectorDef == -4 % angle is the FROM-angle defined clockwise positive from North
                Ans.Angle     = mod(Ans.YComp - 180,360);
            else % angle is the TO-angle defined clockwise positive from North
                Ans.Angle     = Ans.YComp;
            end
            Ans.Magnitude = Ans.XComp;
            Ans.XComp = Ans.Magnitude.*sind(Ans.Angle);
            Ans.YComp = Ans.Magnitude.*cosd(Ans.Angle);
            Ans = rmfield(Ans,'Angle');
            Ans = rmfield(Ans,'Magnitude');
        case 5
            Ans.NormalComp = Ans.XComp;
            Ans.TangentialComp = Ans.YComp;
            % rotation at end of function
            XYneeded = true;
        otherwise
            % no rotation
    end
    %
    if Props.NVal==6
        fm = ustrcmpi('flag_meanings', Attribs);
        Ans.Classes = strsplit(Info.Attribute(fm).Value, ' ');
        fv = ustrcmpi('flag_values', Attribs);
        if fv>0
            Ans.ClassVal = double(Info.Attribute(fv).Value);
            %[~, Ans.Val] = ismember(Ans.Val, Info.Attribute(fv).Value);
        else
            fm = ustrcmpi('flag_masks', Attribs);
            if fm>0
                ui_message('warning', 'The quantity %s uses ''flag_masks'' which are not yet supported.', Info.Name)
            else
                ui_message('warning', 'The quantity %s uses ''flag_meanings'' attribute,\nbut the associated ''flag_values'' or ''flag_masks'' attribute can''t be found.', Info.Name)
            end
        end
    end
    %
    hdim = 1-cellfun('isempty',Props.DimName);
    hdim(M_) = hdim(M_)*2;
    hdim(N_) = hdim(N_)*2;
    hdim(hdim==0)=[];
    hdim = hdim==2;
    %
    if nDims < length(szData)
        szData = szData(1:nDims);
    end
else
    szData = cellfun('length',idx(sz>0));
    hdim   = logical([0 0 1 1 0]);
    hdim   = hdim(sz>0);
    Ans    = [];
end
%
% Read coordinates
%
if Props.hasCoords
    if Props.ClosedPoly && DataInCell
        coordname={'XBounds','YBounds'};
        isbounds = 1;
        %
        vdim = Info.('XBounds');
        CoordInfo = FI.Dataset(vdim);
        specificCoordDims = setdiff(CoordInfo.Dimension,Props.DimName(~cellfun('isempty',Props.DimName)));
        npolpntDim = specificCoordDims{1};
        npolpntDimid = CoordInfo.Dimid(strcmp(CoordInfo.Dimension,npolpntDim));
        npolpnt = FI.Dimension(npolpntDimid+1).Length+2; % add one to close polygon and one for NaN
    else
        coordname={'X','Y'};
        isbounds = 0;
        npolpnt = NaN;
    end
else
    coordname={};
    isbounds = 0;
    npolpnt = NaN;
end

if ~isnan(npolpnt)
    for f = {'Val','XComp','YComp','Angle','Magnitude','NormalComp','TangentialComp'}
        fc = f{1};
        if isfield(Ans,fc)
            szV = size(Ans.(fc));
            Ans.(fc) = repmat(Ans.(fc)(:)',npolpnt,1);
            szV(1) = szV(1)*npolpnt;
            Ans.(fc) = reshape(Ans.(fc),szV);
        end
    end
end

getOptions = {};
if XYRead || XYneeded || ZRead
    if ~XYRead && ~XYneeded
        % just ZRead
    elseif strncmp(Props.Geom,'UGRID',5)
        %ugrid
        mesh_settings = Info.Mesh;
        msh = mesh_settings{3};
        dloc = mesh_settings{4};
        if dloc == -1
            switch Props.Geom(end-3:end)
                case 'NODE'
                    dloc = 0;
                case 'EDGE'
                    dloc = 1;
                case 'FACE'
                    dloc = 2;
            end
        end
        %
        [Ans,meshAttribNames,getOptions] = qp_netcdf_ugrid_get_xy(Ans,FI,msh,dloc,idx{M_});
        meshInfo = FI.Dataset(msh);
        %
        Ans.ValLocation = Props.Geom(max(strfind(Props.Geom,'-'))+1:end);
        connect = strmatch('edge_node_connectivity',meshAttribNames,'exact');
        iconnect = [];
        if strcmp(Ans.ValLocation,'EDGE') || ~isfield(Ans,'FaceNodeConnect') || (~DataRead && ~isempty(connect))
            % "~DataRead" is a hack to load EdgeNodeConnect if available for use in GridView
            iconnect = strmatch(meshInfo.Attribute(connect).Value,{FI.Dataset.Name},'exact');
            if isempty(iconnect)
                ui_message('warning','The edge_node_connectivity variable "%s" could not be found! Using empty set.',meshInfo.Attribute(connect).Value)
                Ans.EdgeNodeConnect = zeros(0,2);
            else
                [Ans.EdgeNodeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
            end
        end
        if isfield(Ans,'EdgeNodeConnect') && ~isempty(iconnect)
            if isempty(FI.Dataset(iconnect).Attribute)
                istart = [];
            else
                istart = strmatch('start_index',{FI.Dataset(iconnect).Attribute.Name},'exact');
            end
            if isempty(istart)
                start = 0;
            else
                start = FI.Dataset(iconnect).Attribute(istart).Value;
            end
            start = verify_start_index(istart, start, min(Ans.EdgeNodeConnect(Ans.EdgeNodeConnect>=0)), max(Ans.EdgeNodeConnect(:)), length(Ans.X), 'node', meshInfo.Attribute(connect).Value);
            Ans.EdgeNodeConnect = Ans.EdgeNodeConnect - start + 1;
            edgeInvalid = any(Ans.EdgeNodeConnect<1,2);
            if any(edgeInvalid)
                ui_message('warning','%i invalid edges detected in edge-node connectivity variable %s; edges removed.',sum(edgeInvalid),meshInfo.Attribute(connect).Value)
                Ans.EdgeNodeConnect(edgeInvalid,:) = [];
            end
        end
        if mesh_settings{2}==1 % also: if strncmp(Props.Geom,'UGRID1D',7)
            aEG = strcmp({meshInfo.Attribute.Name},'edge_geometry');
            if any(aEG)
                % get edge geometry
                [BrX,BrY,xUnit] = get_edge_geometry(FI,msh);
                Ans.EdgeGeometry.X = BrX;
                Ans.EdgeGeometry.Y = BrY;
            end
        end     
        %
        switch Ans.ValLocation
            case 'NODE'
                Ans.X = Ans.X(idx{M_});
                Ans.Y = Ans.Y(idx{M_});
                if isfield(Ans,'FaceNodeConnect')
                    Cnct = all(ismember(Ans.FaceNodeConnect,idx{M_}) | isnan(Ans.FaceNodeConnect),2);
                    renum(idx{M_}) = 1:length(idx{M_});
                    Ans.FaceNodeConnect = Ans.FaceNodeConnect(Cnct,:);
                    Ans.FaceNodeConnect(~isnan(Ans.FaceNodeConnect)) = renum(Ans.FaceNodeConnect(~isnan(Ans.FaceNodeConnect)));
                end
                if isfield(Ans,'EdgeNodeConnect')
                    Cnct = all(ismember(Ans.EdgeNodeConnect,idx{M_}),2);
                    renum(idx{M_}) = 1:length(idx{M_});
                    Ans.EdgeNodeConnect = Ans.EdgeNodeConnect(Cnct,:);
                    Ans.EdgeNodeConnect(~isnan(Ans.EdgeNodeConnect)) = renum(Ans.EdgeNodeConnect(~isnan(Ans.EdgeNodeConnect)));
                    if isfield(Ans,'EdgeGeometry')
                        Ans.EdgeGeometry.X = Ans.EdgeGeometry.X(Cnct);
                        Ans.EdgeGeometry.Y = Ans.EdgeGeometry.Y(Cnct);
                    end
                end
            case 'EDGE'
                for fld = {'Val','XComp','YComp','Angle','Magnitude','NormalComp','TangentialComp'}
                    Fld = fld{1};
                    if isfield(Ans,Fld)
                        Ans.(Fld)(edgeInvalid(idx{M_})) = [];
                    end
                end
                idx{M_}(edgeInvalid(idx{M_})) = [];
                Ans.EdgeNodeConnect = Ans.EdgeNodeConnect(idx{M_},:);
                if isfield(Ans,'EdgeGeometry')
                    Ans.EdgeGeometry.X = Ans.EdgeGeometry.X(idx{M_});
                    Ans.EdgeGeometry.Y = Ans.EdgeGeometry.Y(idx{M_});
                end
            case 'FACE'
                Ans.FaceNodeConnect = Ans.FaceNodeConnect(idx{M_},:);
        end
        %
        %[Ans.XFace, status] = qp_netcdf_get(FI,'mesh2d_face_x');
        %[Ans.YFace, status] = qp_netcdf_get(FI,'mesh2d_face_y');
    elseif strcmp(Info.Type,'ugrid_mesh_contact')
        meshes = Info.Mesh{2};
        Contacts = qp_netcdf_get(FI,FI.Dataset(Info.Mesh{3}));
        if isempty(FI.Dataset(Info.Mesh{3}).Attribute)
            istart = [];
        else
            istart = strmatch('start_index',{FI.Dataset(Info.Mesh{3}).Attribute.Name},'exact');
        end
        if isempty(istart)
            start = 0;
        else
            start = FI.Dataset(Info.Mesh{3}).Attribute(istart).Value;
        end
        Contacts = Contacts - start + 1;
        Contacts = Contacts(idx{M_},:);
        for imesh = 2:-1:1
            % get X,Y coordinates of NODE/EDGE/FACE [see: meshes{imesh,2}] locations
            % with indices Contacts(:,imesh) from mesh meshes{imesh,1}
            idx = Contacts(:,imesh);
            Mesh = qp_netcdf_ugrid_get_xy([],FI,meshes{imesh,1},meshes{imesh,2},idx);
            switch meshes{imesh,2}
                case 0 % get NODE xy
                    x = Mesh.X(idx);
                    y = Mesh.Y(idx);
                    
                case 1 % get EDGE xy
                    enc = Mesh.EdgeNodeConnect(idx,:);
                    %
                    x = Mesh.X(enc);
                    x = sum(x,2)/2;
                    %
                    y = Mesh.Y(enc);
                    y = sum(y,2)/2;
                    
                case 2 % get FACE xy
                    fnc = Mesh.FaceNodeConnect(idx,:);
                    missing = isnan(fnc);
                    nNodes = sum(~missing,2);
                    fnc(missing) = 1;
                    %
                    x = Mesh.X(fnc);
                    x(missing) = 0;
                    x = sum(x,2)./nNodes;
                    %
                    y = Mesh.Y(fnc);
                    y(missing) = 0;
                    y = sum(y,2)./nNodes;
            end
            % x = Contacts(:,imesh);
            % y = Contacts(:,imesh);
            Ans.X(imesh,:) = x(:);
            Ans.Y(imesh,:) = y(:);
        end
        Ans.X(3,:) = NaN;
        Ans.Y(3,:) = NaN;
        Ans.X = Ans.X(:);
        Ans.Y = Ans.Y(:);
    elseif strcmp(Info.Type,'simple_geometry')
        simpleType = Info.Mesh{2};
        switch simpleType
            case {'point'}
                Ans.X = qp_netcdf_get(FI,FI.Dataset(Info.X));
                Ans.Y = qp_netcdf_get(FI,FI.Dataset(Info.Y));
            case {'line'}
                Ans.X = qp_netcdf_get(FI,FI.Dataset(Info.X));
                Ans.Y = qp_netcdf_get(FI,FI.Dataset(Info.Y));
                NodeCount = qp_netcdf_get(FI,Info.Mesh{4}-1);
                Ans.X = mat2cell(Ans.X,NodeCount);
                Ans.Y = mat2cell(Ans.Y,NodeCount);
                Ans.X = Ans.X(idx{M_});
                Ans.Y = Ans.Y(idx{M_});
                %
                Ans.X = Ans.X';
                Ans.X(2,:) = {NaN};
                Ans.X = cat(1,Ans.X{:});
                Ans.Y = Ans.Y';
                Ans.Y(2,:) = {NaN};
                Ans.Y = cat(1,Ans.Y{:});
            otherwise
                error('Simple geometry type "%s" not yet implemented.',simpleType)
        end
    else
        firstbound = 1;
        for iCoord = 1:length(coordname)
            vdim = getfield(Info,coordname{iCoord});
            if isempty(vdim)
                if length(Props.varid)>1
                    Info2 = FI.Dataset(Props.varid(2)+1);
                    vdim = getfield(Info2,coordname{iCoord});
                    if ~isempty(vdim)
                        Info = Info2;
                    else
                        continue
                    end
                else
                    continue
                end
            end
            CoordInfo = FI.Dataset(vdim);
            if isempty(CoordInfo.Attribute)
                CoordAttribs = {};
            else
                CoordAttribs = {CoordInfo.Attribute.Name};
            end
            %
            dims = Props.DimName;
            dimvals = idx;
            if isbounds
                % when plotting the coordinate bounds variable, the
                % variable itself already has a dimension like "Two" or
                % "nMaxmesh2_face_nodes" which conflicts with the dimension
                % of the bounds to be added later. Since these coordinates
                % are non-spatial anyway, remove them.
                dims(6:end) = [];
                dimvals(6:end) = [];
                %
                vdim2 = getfield(Info,coordname{iCoord}(1));
                if isempty(vdim2)
                    CoordInfo2 = [];
                else
                    CoordInfo2 = FI.Dataset(vdim2);
                end
                %
                dims(end+1) = setdiff(CoordInfo.Dimension,dims(~cellfun('isempty',dims)));
                id = strmatch(dims{end},{FI.Dimension.Name},'exact');
                dimvals{end+1} = 1:FI.Dimension(id).Length;
                coordname{iCoord}=coordname{iCoord}(1);
            else
                CoordInfo2 = CoordInfo;
            end
            [Coord, status] = qp_netcdf_get(FI,CoordInfo,dims,dimvals);
            Coord = expand_hdim(Coord,szData,hdim);
            %
            %--------------------------------------------------------------------
            % Delft3D special
            %
            active_id = strmatch('active',CoordAttribs,'exact');
            if ~isempty(active_id)
                activevar = CoordInfo.Attribute(active_id).Value;
                [Active, status] = qp_netcdf_get(FI,activevar,Props.DimName,idx);
                Coord(Active~=1,:)=NaN; % Active~=1 excludes boundary points, Active==0 includes boundary points
            end
            %--------------------------------------------------------------------
            % PALM special
            %
            if ~isempty(FI.Attribute)
                GlobalAttribs = {FI.Attribute.Name}';
                switch coordname{iCoord}
                    case 'X'
                        xo = strmatch('origin_x',GlobalAttribs);
                        if ~isempty(xo)
                            Coord = FI.Attribute(xo).Value + Coord;
                        end
                    case 'Y'
                        yo = strmatch('origin_y',GlobalAttribs);
                        if ~isempty(yo)
                            Coord = FI.Attribute(yo).Value + Coord;
                        end
                end
            end
            %
            %--------------------------------------------------------------------
            % ROMS special
            %
            if ~isnan(CoordInfo.TSMNK(3))
                xcoord = FI.Dimension(CoordInfo.TSMNK(3)+1).Name;
                underscore = strfind(xcoord,'_');
                if activeloaded==0
                    activeloaded = -1;
                    if ~isempty(underscore)
                        location = xcoord(underscore+1:end);
                        maskvar = ['mask_' location];
                        actvarid = strmatch(maskvar,{FI.Dataset.Name},'exact');
                        ActiveInfo = FI.Dataset(actvarid);
                        if ~isempty(ActiveInfo)
                            [Active, status] = qp_netcdf_get(FI,ActiveInfo,Props.DimName,idx);
                            activeloaded = 1;
                        end
                    end
                end
            end
            if activeloaded>0 && isequal(ActiveInfo.Dimension,CoordInfo.Dimension)
                Coord(~Active)=NaN;
            end
            %--------------------------------------------------------------------
            %
            if removeTime
                szCoord = size(Coord);
                Coord = reshape(Coord,[szCoord(2:end) 1]);
            end
            %
            szCoord = size(Coord);
            if length(Coord)>2
                Coord = reshape(Coord,[prod(szCoord(1:end-1)) szCoord(end)]);
            end
            if isbounds
                % only polygons supported
                if size(Coord,2)==2
                    % This could be either
                    %   1) 1D edge or
                    %   2) a 2D grid with 1D coordinates.
                    % In the latter case the variable will have both X and Y
                    % dimensions and the dimension names will match the coordinate
                    % variable names.
                    if ~isempty(Info.X) && ~isempty(Info.Y) && isequal(CoordInfo2.Dimension{1},CoordInfo2.Name)
                        % This is the 2D grid case:
                        % Expand the 2-valued bounds to rectangular bounds, but
                        % first of all make sure that we have [min max] order for
                        % the coordinates.
                        Coord = sort(Coord,2);
                        Coord(:,3:6) = NaN;
                        if firstbound % X
                            Coord(:,1:5) = Coord(:,[1 2 2 1 1]);
                        else % Y
                            Coord(:,1:5) = Coord(:,[2 2 1 1 2]);
                        end
                    else
                        % This is the 1D edge case:
                        % Just add a NaN column to separate the edges.
                        Coord(:,3) = NaN;
                    end
                else
                    % add two NaNs
                    % the first one will be used to close the bounds polygons
                    % the second one will be used to separate the polygons
                    Coord(:,end+1:end+2) = NaN;
                    for j=1:size(Coord,1)
                        for k=1:size(Coord,2)
                            if isnan(Coord(j,k))
                                Coord(j,k) = Coord(j,1);
                                break
                            end
                        end
                    end
                end
                firstbound = 0;
                Coord = Coord';
                Coord = Coord(:);
            end
            %
            Ans.(coordname{iCoord}) = Coord;
            if ~isempty(CoordInfo2.Attribute)
                Attribs = {CoordInfo2.Attribute.Name};
                j = strmatch('units',Attribs,'exact');
                if ~isempty(j)
                    unit = CoordInfo2.Attribute(j).Value;
                    units = {'degrees_east','degree_east','degreesE','degreeE', ...
                        'degrees_north','degree_north','degreesN','degreeN'};
                    if ismember(unit,units)
                        unit = 'deg';
                    end
                    Ans.([coordname{iCoord} 'Units']) = unit;
                end
            end
        end
    end
    %
    if ~isempty(Info.Z) && Props.hasCoords
        vCoordExtended = false;
        vdimid = Info.Z;
        CoordInfo = FI.Dataset(vdimid);
        if is_dflowfm
            if strend(CoordInfo.Name,'_zcc')
                iName  = strrep(CoordInfo.Name,'_zcc','_zw');
                iDimid = strcmp(iName,{FI.Dataset.Name});
                if none(iDimid)
                    error('Unable to locate "%s" for the verical location of the cell interfaces.',iName)
                end
                CoordInfo = FI.Dataset(iDimid);
                idx{K_} = unique([idx{K_} idx{K_}+1]);
                Props.DimName{K_} = CoordInfo.Dimension{3};
                vCoordExtended = true;
            elseif strend(CoordInfo.Name,'_layer_z') || strend(CoordInfo.Name,'_layer_sigma') || strend(CoordInfo.Name,'_layer_sigma_z')
                iName  = strrep(CoordInfo.Name,'_layer_','_interface_');
                iDimid = strcmp(iName,{FI.Dataset.Name});
                if none(iDimid)
                    error('Unable to locate "%s" for the verical location of the cell interfaces.',iName)
                end
                CoordInfo = FI.Dataset(iDimid);
                idx{K_} = unique([idx{K_} idx{K_}+1]);
                Props.DimName{K_} = CoordInfo.Dimension{1};
                vCoordExtended = true;
            end
        end
        %
        if isempty(CoordInfo.Attribute)
            Attribs = {};
        else
            Attribs = {CoordInfo.Attribute.Name};
        end
        j=strmatch('formula_terms',Attribs,'exact');
        formula = '';
        if ~isempty(j)
            formula = CoordInfo.Attribute(j).Value;
            FormulaTerms = multiline(formula,' ','cell');
            i = 1;
            while i<=length(FormulaTerms)
                if isempty(FormulaTerms{i})
                    FormulaTerms(i,:)=[];
                else
                    i=i+1;
                end
            end
            FormulaTerms = reshape(FormulaTerms,2,length(FormulaTerms)/2)';
            FTerror = sprintf('Empty attribute ''formula_terms'' on vertical coordinate variable ''%s''.',CoordInfo.Name);
        else
            FormulaTerms = cell(0,2);
            FTerror = sprintf('Missing attribute ''formula_terms'' on vertical coordinate variable ''%s''.',CoordInfo.Name);
        end
        %
        j=strmatch('positive',Attribs,'exact');
        if ~isempty(j)
            switch lower(CoordInfo.Attribute(j).Value)
                case 'up'
                    signup = 1;
                case 'down'
                    signup = -1;
                otherwise
                    ui_message('warning','Unknown value for attribute ''positive'': %s',CoordInfo.Attribute(j).Value)
                    signup = 1;
            end
        else
            signup = 1;
        end
        j=strmatch('standard_name',Attribs,'exact');
        zLocVar = '';
        zUnitVar = '';
        try
            if ~isempty(j)
                standard_name = CoordInfo.Attribute(j).Value;
                if isnan(Info.TSMNK(N_))
                    HDIMS = {':'};
                    hdims = 2;
                else
                    HDIMS = {':',':'};
                    hdims = 2:3;
                end
                switch standard_name
                    case 'atmosphere_ln_pressure_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        p0var = formvar(FormulaTerms, 'p0');
                        levvar = formvar(FormulaTerms, 'lev');
                        [p0  , status] = qp_netcdf_get(FI,p0var,Props.DimName,idx,getOptions{:});
                        [lev , status] = qp_netcdf_get(FI,levvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = p0var;
                        %
                        Z = zeros(szData);
                        for t=1:size(Z,1)
                            for k=1:length(lev)
                                Z(t,HDIMS{:},k) = p0 * exp(-lev(k));
                            end
                        end
                    case 'atmosphere_sigma_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        sigmavar = formvar(FormulaTerms, 'sigma');
                        psvar = formvar(FormulaTerms, 'ps');
                        ptopvar = formvar(FormulaTerms, 'ptop');
                        [sigma  , status] = qp_netcdf_get(FI,sigmavar,Props.DimName,idx,getOptions{:});
                        [ps     , status] = qp_netcdf_get(FI,psvar,Props.DimName,idx,getOptions{:});
                        [ptop   , status] = qp_netcdf_get(FI,ptopvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = psvar;
                        zLocVar  = psvar;
                        szZData  = updateSize(szData,size(ps),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                Z(t,HDIMS{:},k) = ptop+sigma(k)*(ps(t,HDIMS{:})-ptop);
                            end
                        end
                    case 'atmosphere_hybrid_sigma_pressure_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        try
                            avar = formvar(FormulaTerms, 'a');
                        catch
                            avar = -1;
                        end
                        if ischar(avar)
                            avar = formvar(FormulaTerms, 'a');
                            bvar = formvar(FormulaTerms, 'b');
                            psvar = formvar(FormulaTerms, 'ps');
                            p0var = formvar(FormulaTerms, 'p0');
                            [a      , status] = qp_netcdf_get(FI,avar,Props.DimName,idx,getOptions{:});
                            [b      , status] = qp_netcdf_get(FI,bvar,Props.DimName,idx,getOptions{:});
                            [ps     , status] = qp_netcdf_get(FI,psvar,Props.DimName,idx,getOptions{:});
                            [p0     , status] = qp_netcdf_get(FI,p0var,Props.DimName,idx,getOptions{:});
                            zUnitVar = psvar;
                            zLocVar  = psvar;
                            szZData  = updateSize(szData,size(ps),hdims);
                            %
                            Z = zeros(szZData);
                            for t=1:size(Z,1)
                                for k=1:length(a)
                                    Z(t,HDIMS{:},k) = a(k)*p0+b(k)*ps(t,HDIMS{:});
                                end
                            end
                        else
                            apvar = formvar(FormulaTerms, 'ap');
                            bvar = formvar(FormulaTerms, 'b');
                            psvar = formvar(FormulaTerms, 'ps');
                            [ap     , status] = qp_netcdf_get(FI,apvar,Props.DimName,idx,getOptions{:});
                            [b      , status] = qp_netcdf_get(FI,bvar,Props.DimName,idx,getOptions{:});
                            [ps     , status] = qp_netcdf_get(FI,psvar,Props.DimName,idx,getOptions{:});
                            zUnitVar = psvar;
                            zLocVar  = psvar;
                            szZData  = updateSize(szData,size(ps),hdims);
                            %
                            Z = zeros(szZData);
                            for t=1:size(Z,1)
                                for k=1:length(ap)
                                    Z(t,HDIMS{:},k) = ap(k)+b(k)*ps(t,HDIMS{:});
                                end
                            end
                        end
                    case 'atmosphere_hybrid_height_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        avar = formvar(FormulaTerms, 'a');
                        bvar = formvar(FormulaTerms, 'b');
                        orogvar = formvar(FormulaTerms, 'orog');
                        [a     , status] = qp_netcdf_get(FI,avar,Props.DimName,idx,getOptions{:});
                        [b     , status] = qp_netcdf_get(FI,bvar,Props.DimName,idx,getOptions{:});
                        [orog  , status] = qp_netcdf_get(FI,orogvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = avar;
                        zLocVar  = orogvar;
                        szZData  = updateSize(szData,size(orog),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(a)
                                Z(t,HDIMS{:},k) = a(k) + b(k)*orog(t,HDIMS{:});
                            end
                        end
                    case 'atmosphere_sleve_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        avar = formvar(FormulaTerms, 'a');
                        b1var = formvar(FormulaTerms, 'b1');
                        b2var = formvar(FormulaTerms, 'b2');
                        ztopvar = formvar(FormulaTerms, 'ztop');
                        zsurf1var = formvar(FormulaTerms, 'zsurf1');
                        zsurf2var = formvar(FormulaTerms, 'zsurf2');
                        [a       , status] = qp_netcdf_get(FI,avar,Props.DimName,idx,getOptions{:});
                        [b1      , status] = qp_netcdf_get(FI,b1var,Props.DimName,idx,getOptions{:});
                        [b2      , status] = qp_netcdf_get(FI,b2var,Props.DimName,idx,getOptions{:});
                        [ztop    , status] = qp_netcdf_get(FI,ztopvar,Props.DimName,idx,getOptions{:});
                        [zsurf1  , status] = qp_netcdf_get(FI,zsurf1var,Props.DimName,idx,getOptions{:});
                        [zsurf2  , status] = qp_netcdf_get(FI,zsurf2var,Props.DimName,idx,getOptions{:});
                        zUnitVar = ztopvar;
                        zLocVar  = zsurf1var;
                        szZData  = updateSize(szData,size(zsurf1),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(a)
                                Z(t,HDIMS{:},k) = a(k)*ztop+b1(k)*zsurf1(t,HDIMS{:})+b2(k)*zsurf2(t,HDIMS{:});
                            end
                        end
                    case 'ocean_sigma_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        sigmavar = formvar(FormulaTerms, 'sigma');
                        etavar = formvar(FormulaTerms, 'eta');
                        try
                            depthvar = formvar(FormulaTerms, 'bedlevel');
                            dsign = -1;
                        catch
                            depthvar = formvar(FormulaTerms, 'depth');
                            dsign = 1;
                        end
                        [sigma  , status] = qp_netcdf_get(FI,sigmavar,Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,etavar,Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,depthvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = etavar;
                        zLocVar  = etavar;
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        if is_dflowfm
                            % some hacks for D-Flow FM
                            if dsign < 0
                                depth = -depth;
                            elseif length(depthvar)>10 && strcmp(depthvar(end-9:end),'waterdepth')
                                depth = depth-eta;
                            end
                        elseif dsign < 0
                            error('Unexpected key bedlevel in formula_term')
                        end
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                Z(t,HDIMS{:},k) = eta(t,HDIMS{:})+(depth+eta(t,HDIMS{:}))*sigma(k);
                            end
                        end
                    case 'ocean_s_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        svar = formvar(FormulaTerms, 's');
                        etavar = formvar(FormulaTerms, 'eta');
                        depthvar = formvar(FormulaTerms, 'depth');
                        avar = formvar(FormulaTerms, 'a');
                        bvar = formvar(FormulaTerms, 'b');
                        depth_cvar = formvar(FormulaTerms, 'depth_c');
                        [s      , status] = qp_netcdf_get(FI,svar,Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,etavar,Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,depthvar,Props.DimName,idx,getOptions{:});
                        [a      , status] = qp_netcdf_get(FI,avar,Props.DimName,idx,getOptions{:});
                        [b      , status] = qp_netcdf_get(FI,bvar,Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,depth_cvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = etavar;
                        zLocVar  = etavar;
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        C = (1-b)*sinh(a*s)/sinh(a) + b*(tanh(a*(s+0.5))/(2*tanh(0.5*a))-0.5);
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(s)
                                Z(t,HDIMS{:},k) = eta(t,HDIMS{:})*(1+s(k))+depth_c*s(k)+(depth-depth_c)*C(k);
                            end
                        end
                    case 'ocean_s_coordinate_g1'
                        if isempty(FormulaTerms), error(FTerror), end
                        svar = formvar(FormulaTerms, 's');
                        cvar = formvar(FormulaTerms, 'c');
                        etavar = formvar(FormulaTerms, 'eta');
                        depthvar = formvar(FormulaTerms, 'depth');
                        depth_cvar = formvar(FormulaTerms, 'depth_c');
                        [s      , status] = qp_netcdf_get(FI,svar,Props.DimName,idx,getOptions{:});
                        [C      , status] = qp_netcdf_get(FI,cvar,Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,etavar,Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,depthvar,Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,depth_cvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = etavar;
                        zLocVar  = etavar;
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(s)
                                S = depth_c*s(k) + (depth - depth_c)*C(k);
                                Z(t,HDIMS{:},k) = S + eta(t,HDIMS{:})*(1+S./depth);
                            end
                        end
                    case 'ocean_s_coordinate_g2'
                        if isempty(FormulaTerms), error(FTerror), end
                        svar = formvar(FormulaTerms, 's');
                        cvar = formvar(FormulaTerms, 'c');
                        etavar = formvar(FormulaTerms, 'eta');
                        depthvar = formvar(FormulaTerms, 'depth');
                        depth_cvar = formvar(FormulaTerms, 'depth_c');
                        [s      , status] = qp_netcdf_get(FI,svar,Props.DimName,idx,getOptions{:});
                        [C      , status] = qp_netcdf_get(FI,cvar,Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,etavar,Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,depthvar,Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,depth_cvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = etavar;
                        zLocVar  = etavar;
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(s)
                                S = (depth_c*s(k) + depth*C(k))./(depth_c + depth);
                                Z(t,HDIMS{:},k) = eta(t,HDIMS{:}) + (eta(t,HDIMS{:}) + depth).*S;
                            end
                        end
                    case 'ocean_sigma_z_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        sigmavar = formvar(FormulaTerms, 'sigma');
                        etavar = formvar(FormulaTerms, 'eta');
                        depthvar = formvar(FormulaTerms, 'depth');
                        depth_cvar = formvar(FormulaTerms, 'depth_c');
                        zlevvar = formvar(FormulaTerms, 'zlev');
                        [sigma  , status] = qp_netcdf_get(FI,sigmavar,Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,etavar,Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,depthvar,Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,depth_cvar,Props.DimName,idx,getOptions{:});
                        % nsigma is deprecated ... should depend on valid sigma or zlev ...
                        try
                            nsigmavar = formvar(FormulaTerms, 'nsigma');
                            [nsigma , status] = qp_netcdf_get(FI,nsigmavar,Props.DimName,idx,getOptions{:});
                        catch
                            nsigma = NaN;
                        end
                        [zlev   , status] = qp_netcdf_get(FI,zlevvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = etavar;
                        zLocVar  = etavar;
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        if ~isnan(nsigma)
                            % CF-1.8: first nsigma layers are sigma layers,
                            % other layers are z layers
                            zlev(1:nsigma) = NaN;
                            sigma(nsigma+1:end) = NaN;
                        else
                            % CF-1.9 or later
                            nsigma = sum(~isnan(sigma));
                            nz = length(sigma) - nsigma;
                            if any(~isnan(sigma(1:nz))) || any(isnan(zlev(1:nz))) || any(~isnan(zlev(nz+1:end)))
                                error('Inconsistent sigma/z layer data encountered in %s and %s variables.',sigmavar,zlevvar)
                            end
                        end
                        K=idx{K_};
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                if isnan(zlev(k)) && ~isnan(sigma(k))
                                    Z(t,HDIMS{:},k) = eta(t,HDIMS{:}) + sigma(k)*(min(depth_c,depth)+eta(t,HDIMS{:}));
                                elseif isnan(sigma(k)) && ~isnan(zlev(k))
                                    Z(t,HDIMS{:},k) = max(-depth,zlev(k));
                                elseif isnan(sigma(k)) % and hence also zlev(k)
                                    error('Both zlev and sigma undefined for layer %i.',K(k))
                                else % both defined
                                    error('Both zlev and sigma defined for layer %i.',K(k))
                                end
                            end
                        end
                    case 'ocean_double_sigma_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        sigmavar = formvar(FormulaTerms, 'sigma');
                        depthvar = formvar(FormulaTerms, 'depth');
                        z1var = formvar(FormulaTerms, 'z1');
                        z2var = formvar(FormulaTerms, 'z2');
                        avar = formvar(FormulaTerms, 'a');
                        hrefvar = formvar(FormulaTerms, 'href');
                        k_cvar = formvar(FormulaTerms, 'k_c');
                        [sigma  , status] = qp_netcdf_get(FI,sigmavar,Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,depthvar,Props.DimName,idx,getOptions{:});
                        [z1     , status] = qp_netcdf_get(FI,z1var,Props.DimName,idx,getOptions{:});
                        [z2     , status] = qp_netcdf_get(FI,z2var,Props.DimName,idx,getOptions{:});
                        [a      , status] = qp_netcdf_get(FI,avar,Props.DimName,idx,getOptions{:});
                        [href   , status] = qp_netcdf_get(FI,hrefvar,Props.DimName,idx,getOptions{:});
                        [k_c    , status] = qp_netcdf_get(FI,k_cvar,Props.DimName,idx,getOptions{:});
                        zUnitVar = z1var;
                        zLocVar  = depthvar;
                        szZData  = updateSize(szData,size(depth),hdims);
                        %
                        K=idx{K_};
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                f = 0.5*(z1+z2) + 0.5*(z1-z2)*tanh(2*a/(z1-z2)*(depth-href));
                                if K(k)<=k_c
                                    Z(t,HDIMS{:},k) = sigma(k)*f;
                                else
                                    Z(t,HDIMS{:},k) = f + (sigma(k)-1)*(depth-f);
                                end
                            end
                        end
                    otherwise
                        if ~isempty(formula)
                            ui_message('warning','Formula for %s not implemented',standard_name)
                        end
                        [Z, status] = qp_netcdf_get(FI,CoordInfo,Props.DimName,idx,getOptions{:});
                        if all(isnan(Z(:)))
                            error('Vertical coordinate variable %s returned only missing values.',CoordInfo.Name)
                        end
                        zLocVar  = CoordInfo.Name;
                        nZ = length(Z);
                        if signup<0
                            Z=-Z;
                        end
                        %
                        if is_dflowfm
                            %
                            % hack for 1D z-layers in FM
                            %
                            if strend(CoordInfo.Name,'_layer_z') || strend(CoordInfo.Name,'_interface_z')
                                us = strfind(CoordInfo.Name,'_');
                                waterlevel = [CoordInfo.Name(1:us(1)) 's1'];
                                izw = strmatch(waterlevel,{FI.Dataset.Name});
                                %
                                zb_t_dependent = true;
                                bedlevel   = [CoordInfo.Name(1:us(1)) 'mor_bl'];
                                izb = strmatch(bedlevel,{FI.Dataset.Name});
                                if isempty(izb)
                                    zb_t_dependent = false;
                                    bedlevel   = [CoordInfo.Name(1:us(1)) 'flowelem_bl'];
                                    izb = strmatch(bedlevel,{FI.Dataset.Name});
                                end
                                %
                                if ~isempty(izw) && ~isempty(izb)
                                    [zw, status] = qp_netcdf_get(FI,FI.Dataset(izw),Props.DimName,idx,getOptions{:});
                                    %
                                    szZwData = size(zw);
                                    szZData = szData;
                                    szZData(hdims) = szZwData(hdims);
                                    Z = expand_hdim(Z,szZData,hdim);
                                    %
                                    for k=1:nZ
                                        Z(:,HDIMS{:},k) = min(Z(:,HDIMS{:},k),zw);
                                    end
                                    %
                                    [zb, status] = qp_netcdf_get(FI,FI.Dataset(izb),Props.DimName,idx,getOptions{:});
                                    if zb_t_dependent
                                        for k=1:nZ
                                            Z(:,HDIMS{:},k) = max(Z(:,HDIMS{:},k),zb);
                                        end
                                    else
                                        for t=1:size(Z,1)
                                            for k=1:nZ
                                                Z(t,HDIMS{:},k) = max(Z(t,HDIMS{:},k),zb(1,HDIMS{:}));
                                            end
                                        end
                                    end
                                    %
                                    zLocVar = waterlevel;
                                elseif ~isempty(szData)
                                    Z = expand_hdim(Z,szData,hdim);
                                end
                            else
                               % full grid _zcc and _zw coordinates don't
                               % need special treatment.
                            end
                        elseif ~isempty(szData)
                            Z = expand_hdim(Z,szData,hdim);
                        end
                end
            else
                [Z, status] = qp_netcdf_get(FI,CoordInfo,Props.DimName,idx);
                if signup<0
                    Z=-Z;
                end
                Z = expand_hdim(Z,szData,hdim);
            end
        catch Ex
            qp_error({sprintf('Retrieving vertical coordinate "%s" failed, continuing with layer index as vertical coordinate.',CoordInfo.Name),'The error message encountered reads:'},Ex,'netcdffil')
            kDim = length(szData);
            kVec = ones(1,max(2,kDim));
            kVec(kDim) = length(idx{K_});
            if vCoordExtended
                Z = reshape(idx{K_},kVec) - 0.5;
            else
                Z = reshape(idx{K_},kVec);
            end
            if kDim > 1
                Z = repmat(Z,szData(1:kDim-1));
            end
        end
        %
        if ~isempty(zLocVar)
            zLocVar = strmatch(zLocVar,{FI.Dataset.Name},'exact')-1;
            Info = FI.Dataset(zLocVar+1);
            if iscell(Info.Mesh)
                switch Info.Mesh{4}
                    case 0
                        Ans.ZLocation = 'NODE';
                    case 1
                        Ans.ZLocation = 'EDGE';
                    case 2
                        Ans.ZLocation = 'FACE';
                end
            end
        elseif isfield(Ans,'ValLocation')
            Ans.ZLocation = Ans.ValLocation;
        end
        %
        if ~isempty(zUnitVar)
            zUnitVar = strmatch(zUnitVar,{FI.Dataset.Name},'exact')-1;
            Info = FI.Dataset(zUnitVar+1);
            zUnitAtt = strmatch('units',{Info.Attribute.Name},'exact');
            Ans.ZUnits = Info.Attribute(zUnitAtt).Value;
        else
            j = strmatch('units',Attribs,'exact');
            if ~isempty(j)
                Ans.ZUnits = CoordInfo.Attribute(j).Value;
            end
        end
        %--------------------------------------------------------------------
        %
        if removeTime
            szCoord = size(Z);
            Z = reshape(Z,[szCoord(2:end) 1]);
        end
        %--------------------------------------------------------------------
        Ans.Z = Z;
        %
        if isfield(Ans,'X')
            % z coordinates may be defined at ...
            % * value locations (e.g. z and values in faces),
            % * grid locations (e.g. z at nodes), or
            % * other location (e.g. z in faces, values at edges).
            % Be careful when trying to synchronize dimensions!
            %
            szZ = size(Z);
            if isfield(Ans,'Val')
                szV = size(Ans.Val);
            elseif isfield(Ans,'XComp')
                szV = size(Ans.XComp);
            else
                szV = NaN*szZ;
            end
            if ~isequal(szZ,szV)
                if npolpnt>0 && prod(szZ)*npolpnt==szV(1)
                    % polygons
                    Z = repmat(Z(:)',npolpnt,1);
                    Ans.Z = Z(:);
                elseif length(idx{K_})>1
                    % The last dimension of Z is the vertical dimension
                    zdim = ndims(Z);
                    kZ = szZ(zdim);
                    % That dimensions of X/Y should be one.
                    kX = size(Ans.X,kZ);
                    if kX==1
                        rep = ones(1,zdim);
                        rep(zdim) = kZ;
                        Ans.X = repmat(Ans.X,rep);
                        Ans.Y = repmat(Ans.Y,rep);
                    end
                end
            end
        end
    end
    %
    if ~Props.hasCoords
        %
        % Define a simple regular grid
        %
        Ans.X = repmat(idx{M_}(:),1,length(idx{N_}));
        Ans.Y = repmat(idx{N_}(:)',length(idx{M_}),1);
        if Props.DimFlag(T_) && length(idx{T_})>1
            Ans.X = reshape(Ans.X,[1 size(Ans.X)]);
            Ans.Y = reshape(Ans.Y,[1 size(Ans.Y)]);
        end
        if Props.DimFlag(K_)
            Ans.Z = idx{K_};
        end
    end
end

if ~isnan(npolpnt)
    for f = {'Z'}
        fc = f{1};
        if isfield(Ans,fc)
            szV = size(Ans.(fc));
            Ans.(fc) = repmat(Ans.(fc)(:)',npolpnt,1);
            szV(1) = szV(1)*npolpnt;
            Ans.(fc) = reshape(Ans.(fc),szV);
        end
    end
end

if isfield(Ans,'NormalComp')
    if Props.MNK==1
        dx = diff(Ans.X(Ans.EdgeNodeConnect),1,2);
        dy = diff(Ans.Y(Ans.EdgeNodeConnect),1,2);
        ln = sqrt(dx.^2+dy.^2); % should never be 0, so no need to protect
        dx = dx./ln;
        dy = dy./ln;
        szTC = size(Ans.TangentialComp);
        Ans.XComp = zeros(szTC);
        Ans.YComp = zeros(szTC);
        if Props.DimFlag(T_) && length(idx{T_})>1
            id = {0,':',0};
            ND = prod(szTC(3:end));
            dx = dx';
            dy = dy';
        else
            id = {':',0};
            ND = prod(szTC(2:end));
        end
        for t = 1:max(1,length(idx{T_}))
            if Props.DimFlag(T_) && length(idx{T_})>1
                id{1} = t;
            end
            for nd = 1:ND
                id{end} = nd;
                Ans.XComp(id{:}) = Ans.TangentialComp(id{:}).*dx + Ans.NormalComp(id{:}).*dy;
                Ans.YComp(id{:}) = Ans.TangentialComp(id{:}).*dy - Ans.NormalComp(id{:}).*dx;
            end
        end
        Ans = rmfield(Ans,'NormalComp');
        Ans = rmfield(Ans,'TangentialComp');
    else
        Ans = rmfield(Ans,'XComp');
        Ans = rmfield(Ans,'YComp');
    end
end

if XYneeded && ~XYRead
    f = {'X','Y','FaceNodeConnect','EdgeNodeConnect'};
    f(~isfield(Ans,f)) = [];
    Ans = rmfield(Ans,f);
end

% read time ...
T=[];
if Props.DimFlag(T_)
    T=readtim(FI,Props,idx{T_});
end
Ans.Time=T;

% Work around stupid DeltaShell files that write lat/lon, but actually store metric coordinates.
if ~isempty(FI.Attribute)
    ds = strcmp('FileVersion_DeltaShell',{FI.Attribute.Name});
    if any(ds) && isfield(Ans,'XUnits') && strcmp(Ans.XUnits,'deg')
        if min(Ans.X(:))<-360 || max(Ans.X(:))>360
            Ans.XUnits = 'm';
            Ans.YUnits = 'm';
        end
    end
end

varargout={Ans OrigFI};
% -----------------------------------------------------------------------------

% -------------------------------------------------------------------------
function [TZshift,TZstr]=gettimezone(FI,domain,Props)
TZstr = '';
timevar = FI.Dataset(get_varid(Props)+1).Time;
if isempty(timevar) || ~isfield(FI.Dataset(timevar).Info,'TZshift')
    TZshift = NaN;
else
    TZshift = FI.Dataset(timevar).Info.TZshift;
end
% -------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function start_index = verify_start_index(istart, start_index, minIndex, maxIndex, limitIndex, location, variable)
if isempty(istart)
    if minIndex == 1 && maxIndex == limitIndex
        start_index = 1;
        ui_message('warning','No start_index found on %s.\nDefault value is 0, but data suggest otherwise.\nUsing start_index=1.', variable)
    else
        start_index = 0;
    end
else
    if minIndex-start_index+1 < 1
        error('File specifies start_index %g for %s, but lowest %s index in file is %g.', start_index, variable, location, minIndex)
    elseif maxIndex-start_index+1 > limitIndex
        error('File specifies start_index %g for %s and the largest %s index in file is %g, but the last %s is only %g.', start_index, variable, location, maxIndex, location, limitIndex)
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units' 'TemperatureType' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'SubFld' 'MNK' 'varid'  'DimName' 'hasCoords' 'VectorDef' 'ClosedPoly' 'UseGrid'};
DataProps={'dummy field'            ''      ''                ''     ''      [0 0 0 0 0]  0           0      []       0     []          {}          0         0          0          0};
Out=cell2struct(DataProps,PropNames,2);
%Out.MName='M';
%Out.NName='N';
%Out.KName='K';
% -----------------------------------------------------------------------------
ndims = length(FI.Dimension);
nvars = length(FI.Dataset);
ngatts = length(FI.Attribute);
%
Dummy = Out;
Dummy.Name='-------';
%
% Following lines are a hack to detect vector quantities for XBeach netcdf files
% NOTE: also [min(x-component) min(y-component)] is incorrectly detected as vector quantity
%
% for i=1:length(FI.Dataset)
%     for j=1:length(FI.Dataset(i).Attribute)
%         if strcmp(FI.Dataset(i).Attribute(j).Name,'long_name')
%             nm=FI.Dataset(i).Attribute(j).Value;
%             if strfind(nm,'x-')
%                 nm=strrep(nm,'x-','');
%                 nm=[nm ', x-component'];
%             elseif strfind(nm,'y-')
%                 nm=strrep(nm,'y-','');
%                 nm=[nm ', y-component'];
%             end
%             FI.Dataset(i).Attribute(j).Value=nm;
%         end
%     end
% end
%
nmesh  = 0;
meshes = cell(0,2);
if nvars==0
    Out = Out([]);
else
    %
    % Process variables and add them to the list
    %
    for ivar=1:nvars
        Insert=Out(1);
        %
        % Get variable attributes
        %
        Info = FI.Dataset(ivar);
        if isfield(Info.Attribute,'Name')
            Attribs = {Info.Attribute.Name};
        else
            Attribs = {};
        end
        %
        % Show long name, or standard name, or variable name
        %
        j = strmatch('standard_name',Attribs,'exact');
        if ~isempty(j)
            standard_name = Info.Attribute(j).Value;
        else
            standard_name = '';
        end
        j = strmatch('long_name',Attribs,'exact');
        if ~isempty(j) && ~isempty(Info.Attribute(j).Value)
            Insert.Name = Info.Attribute(j).Value;
        else
            if ~isempty(standard_name)
                Insert.Name = standard_name;
            else
                Insert.Name = Info.Name;
            end
        end
        %
        % Show units
        %
        j = strmatch('units',Attribs,'exact');
        if ~isempty(j)
            Insert.Units = Info.Attribute(j).Value;
        else
            Insert.Units = '';
        end
        switch standard_name
            case {'air_potential_temperature','air_temperature', 'air_temperature_at_cloud_top', ...
                    'air_temperature_at_effective_cloud_top_defined_by_infrared_radiation', ...
                    'air_temperature_threshold','brightness_temperature', ...
                    'brightness_temperature_at_cloud_top','canopy_temperature', ...
                    'dew_point_temperature','dynamic_tropopause_potential_temperature', ...
                    'equivalent_potential_temperature','equivalent_temperature', ...
                    'fire_temperature','freezing_temperature_of_sea_water', ...
                    'land_ice_temperature','product_of_air_temperature_and_omega', ...
                    'product_of_air_temperature_and_specific_humidity', 'product_of_eastward_sea_water_velocity_and_temperature', ...
                    'product_of_eastward_wind_and_air_temperature', 'product_of_northward_sea_water_velocity_and_temperature', ...
                    'product_of_northward_wind_and_air_temperature','product_of_omega_and_air_temperature', ...
                    'product_of_upward_air_velocity_and_air_temperature','pseudo_equivalent_potential_temperature', ...
                    'pseudo_equivalent_temperature','sea_ice_surface_temperature', ...
                    'sea_ice_temperature','sea_surface_foundation_temperature', ...
                    'sea_surface_skin_temperature','sea_surface_subskin_temperature', ...
                    'sea_surface_temperature','sea_water_conservative_temperature', ...
                    'sea_water_potential_temperature','sea_water_potential_temperature_at_sea_floor', ...
                    'sea_water_temperature','soil_temperature','square_of_air_temperature', ...
                    'square_of_sea_surface_temperature','surface_brightness_temperature', ...
                    'surface_temperature','temperature_in_surface_snow','temperature_of_sensor_for_oxygen_in_sea_water', ...
                    'toa_brightness_temperature','toa_brightness_temperature_assuming_clear_sky', ...
                    'toa_brightness_temperature_of_standard_scene','tropical_cyclone_eye_brightness_temperature', ...
                    'tropopause_air_temperature','virtual_temperature','wet_bulb_temperature'}
                Insert.TemperatureType = 'absolute';
            otherwise
                Insert.TemperatureType = 'unspecified';
        end
        %
        % Scalar variables by default
        %
        Insert.NVal = 1;
        Insert.DimName = cell(1,5);
        if strcmp(Info.Type,'simple_geometry')
            Insert.NVal = 0;
        elseif strcmp(Info.Datatype,'char')
            Insert.NVal = 4;
        elseif any(strcmp('flag_values',Attribs))
            Insert.NVal = 6;
        end
        %
        % Link to dimension variables
        %
        for i=1:5
            if ~isnan(Info.TSMNK(i))
                if i==T_ 
                    if ~isempty(Info.Time)
                        if isfield(FI.Dataset(Info.Time).Info,'RefDate') && isnumeric(FI.Dataset(Info.Time).Info.RefDate)
                            Insert.DimFlag(i)=1;
                        elseif isfield(FI.Dataset(Info.Time).Info,'DT') && ~isempty(FI.Dataset(Info.Time).Info.DT)
                            Insert.DimFlag(i)=3;
                        else
                            Insert.DimFlag(i)=5;
                        end
                    else
                        Insert.DimFlag(i)=5;
                    end
                elseif i==ST_ && ~isempty(Info.Station)
                    Insert.DimFlag(i)=5;
                else
                    Insert.DimFlag(i)=1;
                end
                Insert.DimName{i}=FI.Dimension(Info.TSMNK(i)+1).Name;
            end
        end
        for i=6:length(Info.TSMNK)
            Insert.DimFlag(i)=1;
            Insert.DimName{i}=FI.Dimension(Info.TSMNK(i)+1).Name;
        end
        %
        % Any extra dimensions are wrapped into the subfields.
        %
        if ~isempty(Info.SubFieldDim)
            nsfd = length(Info.SubFieldDim);
            Insert.SubFld=cell(nsfd,2);
            for id = 1:nsfd
                d = Info.SubFieldDim(id)+1;
                [isStringDim,iVar] = ismember(d-1,Info.SubFieldChr(:,2));
                Insert.SubFld(id,1:2) = {FI.Dimension(d).Name FI.Dimension(d).Length};
                if isStringDim
                    stcrd = Info.SubFieldChr(iVar,1);
                    if FI.Dataset(stcrd).CharDim==FI.Dataset(stcrd).Dimid(1)
                        % PRESERVE_FVD=true
                        [labels, status] = qp_netcdf_get(FI,stcrd-1,fliplr(FI.Dataset(stcrd).Dimension));
                    else
                        [labels, status] = qp_netcdf_get(FI,stcrd-1,FI.Dataset(stcrd).Dimension);
                    end
                    Insert.SubFld{id,3} = cellstr(labels);
                end
            end
        end
        %
        meshAttribNames = {};
        if ~isempty(Info.Mesh)
            meshInfo    = FI.Dataset(Info.Mesh{3});
            if ~isempty(meshInfo.Attribute)
                meshAttribNames = {meshInfo.Attribute.Name};
            end
            nmesh = nmesh+1;
            switch Info.Mesh{1}
                case 'ugrid'
                    tpd = Info.Mesh{2};
                    Insert.Geom = sprintf('UGRID%iD',tpd);

                case 'simple_geometry'
                    switch Info.Mesh{2}
                        case 'line'
                            Insert.Geom = 'POLYL';
                    end
                    
                case 'ugrid_mesh_contact'
                    Insert.Geom = 'POLYL';
                    Insert.SubFld = [];
                    Insert.NVal = 0;
    
                otherwise
                    Insert.Geom = upper(Info.Mesh{1});
            end
            BaseGeom = Insert.Geom;
            if ismember(Info.Mesh{1},{'ugrid','ugrid1d_network'})
                switch Info.Mesh{4}
                    case -1 % the mesh itself
                        Insert.SubFld = [];
                        Insert.Geom = [Insert.Geom '-NODE'];
                        Insert.DimFlag(3) = 6;
                        Insert.NVal = 0;
                    case 0 % node
                        Insert.Geom = [Insert.Geom '-NODE'];
                        Insert.DimFlag(3) = 6;
                    case 1 % edge
                        Insert.Geom = [Insert.Geom '-EDGE'];
                        Insert.DimFlag(3) = 6;
                    case 2 % face
                        Insert.Geom = [Insert.Geom '-FACE'];
                        Insert.DimFlag(3) = 6;
                        Insert.DataInCell = 1;
                    case 3 % volume
                        Insert.Geom = [Insert.Geom '-VOLUME'];
                        Insert.DimFlag(3) = 6;
                        Insert.DataInCell = 1;
                end
            end
            Insert.Coords = 'xy';
            Insert.hasCoords=1;
        elseif ~isempty(Info.X) && ~isempty(Info.Y)
            Insert.hasCoords=1;
            mesh = [Info.X Info.Y];
            found = false;
            for m = 1:size(meshes,1)
                if isequal(meshes{m,2},mesh)
                    found = true;
                    break
                end
            end
            if ~found
                meshes(end+1,:) = {size(Out,1)+1 mesh};
            end
            if ~isempty(Info.XBounds) && ~isempty(Info.YBounds)
                Insert.Geom = 'POLYG';
                Insert.ClosedPoly = 1;
                Insert.Coords = 'xy';
                Insert.DataInCell = 1;
            elseif ~Insert.DimFlag(N_) % 1D data set or unstructured data set
                if Insert.DimFlag(K_)
                    Insert.Geom = 'PNT+';
                    Insert.Coords = 'xy+z';
                else
                    Insert.Geom = 'PNT';
                    Insert.Coords = 'xy';
                end
            end
        elseif ~isempty(Info.Z)
            Insert.hasCoords = 1;
        end
        %
        Insert.varid = Info.Varid;
        %
        if ~isempty(Info.Mesh) && isequal(Info.Type,'ugrid_mesh') && isequal(Info.Mesh{4},-1)
            Insert.varid = {'node_index' Insert.varid};
        end
        %
        Out(end+1)=Insert;
        %
        if ismember('balance_quantity',Attribs)
            Insert.NVal = -1;
            Insert.varid = {'balance', Insert.varid, Insert.SubFld{2,3}};
            Insert.SubFld = [];
            Insert.Name = [Insert.Name ' (graph)'];
            Out(end+1) = Insert;
        end
        %
        if ~isempty(Info.Mesh)
            if isequal(Info.Type,'ugrid_mesh') && isequal(Info.Mesh{4},-1)
                Nm = Insert.Name;
                %
                Insert.Name = [Nm ' - node indices'];
                Insert.NVal = 1;
                Insert.varid{1} = 'node_index';
                Out(end+1) = Insert;
                %
                if ~isempty(Info.Mesh{6})
                    Insert.Name = [Nm ' - edge indices'];
                    Insert.Geom = [BaseGeom '-EDGE'];
                    Insert.varid{1} = 'edge_index';
                    Insert.DimName{M_} = Info.Mesh{6};
                    Out(end+1) = Insert;
                end
                %
                if length(Info.Mesh)>=7 && ~isempty(Info.Mesh{7})
                    Insert.Name = [Nm ' - face indices'];
                    Insert.Geom = [BaseGeom '-FACE'];
                    Insert.DataInCell = 1;
                    Insert.varid{1} = 'face_index';
                    Insert.DimName{M_} = Info.Mesh{7};
                    Out(end+1) = Insert;
                end
            end
        end
        %
        %if strcmp(standard_name,'discharge') && strncmp(Insert.Name,'Discharge',9) && strcmp(Insert.Geom,'UGRID-EDGE') && Insert.DimFlag(K_)>0
        %    Insert.Name = ['Depth integrated d' Insert.Name(2:end)];
        %    Insert.DimFlag(K_)=0;
        %    %
        %    Out(end+1)=Insert;
        %end
        %
        streamfunc = false;
        if strcmp(Insert.Geom,'UGRID2D-EDGE') && Insert.DimFlag(K_)==0
            if strcmp(standard_name,'discharge')
                streamfunc = true;
                prefix = '';
            else
                ireg = regexp(Insert.Name,'discharge through flow link','once');
                ireg2 = regexp(Insert.Name,'time of','once');
                try
                    convFac = qp_unitconversion(Insert.Units,'m3 s-1');
                catch
                    convFac = 'error';
                end
                if ~isempty(ireg) && isempty(ireg2) && isnumeric(convFac)
                    streamfunc = true;
                    prefix = Insert.Name(1:ireg-1);
                end
            end
        end
        if streamfunc
            Insert.Name = [prefix, 'stream function']; % previously: discharge potential
            Insert.Geom = 'UGRID2D-NODE';
            Insert.varid = {'stream_function' Insert.varid};
            Insert.DimName{M_} = FI.Dataset(FI.Dataset(Insert.varid{2}+1).Mesh{3}).Mesh{5};
            %
            Out(end+1)=Insert;
            %
            if ismember('edge_face_connectivity',meshAttribNames)
                Insert.Name = [prefix, 'net discharge into cell']; % can be used as stationarity check for the stream function
                Insert.Geom = 'UGRID2D-FACE';
                Insert.varid{1} = 'net_discharge_into_cell';
                Insert.DimName{M_} = FI.Dataset(FI.Dataset(Insert.varid{2}+1).Mesh{3}).Mesh{7};
                Insert.DataInCell = 1;
                %
                Out(end+1)=Insert;
            end
        else
            switch lower(Insert.Name)
                case 'time-varying bottom level in flow cell center'
                    if ~isnan(Info.TSMNK(T_)) && FI.Dimension(Info.TSMNK(T_)+1).Length>1
                        Insert.Name = 'cum. erosion/sedimentation';
                        Insert.varid = {'erosion_sedimentation' Insert.varid};
                        %
                        Out(end+1)=Insert;
                    end
            end
        end
    end
    Out(1)=[];
end
nOut = length(Out);
for m = size(meshes,1):-1:1
    mo = nOut + m;
    Out(mo)=Out(meshes{m,1});
    crd = meshes{m,2};
    XInfo = FI.Dataset(crd(1));
    YInfo = FI.Dataset(crd(2));
    %
    if nmesh==0 && size(meshes,1)==1
        name = 'grid';
    else
        xname = XInfo.Name;
        yname = YInfo.Name;
        name = ['grid (' xname ,', ', yname ')'];
    end
    Out(mo).Name = name;
    Out(mo).Units = '';
    Out(mo).NVal = 0;
    Out(mo).varid = crd-1;
    %
    Out(mo).DimName = cell(1,5);
    Out(mo).DimFlag = double(~isnan(XInfo.TSMNK) | ~isnan(YInfo.TSMNK));
    for d = 1:length(XInfo.TSMNK)
        if ~isnan(XInfo.TSMNK(d))
            Out(mo).DimName{d} = FI.Dimension(XInfo.TSMNK(d)+1).Name;
        elseif ~isnan(YInfo.TSMNK(d))
            Out(mo).DimName{d} = FI.Dimension(YInfo.TSMNK(d)+1).Name;
        end
    end
    if Out(mo).DimFlag(M_) && Out(mo).DimFlag(N_)
        Out(mo).Geom = '';
    else
        Out(mo).Geom = 'PNT';
    end
    Out(mo).hasCoords   = 1;
    if isempty(XInfo.XBounds)
        Out(mo).ClosedPoly = 0;
    else
        Out(mo).Geom = 'POLYG';
        Out(mo).ClosedPoly = 1;
    end
end
hasCoords = [Out.hasCoords]==1;
OutNoCoords = Out(~hasCoords);
OutCoords = Out(hasCoords);
%
OutCoords = group_matching_dimensions(OutCoords, Dummy);
OutNoCoords = group_matching_dimensions(OutNoCoords, Dummy);
Out = [OutCoords OutNoCoords];
%
for i = length(OutCoords)+(1:length(OutNoCoords))
    if Out(i).DimFlag(M_)
        Out(i).MName = Out(i).DimName{M_};
        [V,cnt,err] = sscanf(Out(i).MName,'%f',2);
        if cnt == 1 && isempty(err)
            Out(i).MName = 'M';
        end
    end
    if Out(i).DimFlag(N_)
        Out(i).NName = Out(i).DimName{N_};
        [V,cnt,err] = sscanf(Out(i).NName,'%f',2);
        if cnt == 1 && isempty(err)
            Out(i).NName = 'N';
        end
    end
    if Out(i).DimFlag(K_)
        Out(i).KName = Out(i).DimName{K_};
        [V,cnt,err] = sscanf(Out(i).KName,'%f',2);
        if cnt == 1 && isempty(err)
            Out(i).KName = 'K';
        end
    end
end
%
[Out.AppendName] = deal('');
for i = 1:length(Out)
    if iscell(Out(i).varid)
        %TODO
    elseif ~isempty(Out(i).varid)
        Info = FI.Dataset(Out(i).varid(1)+1);
        for j = 1:length(Info.Attribute)
            if strcmp(Info.Attribute(j).Name,'cell_methods')
                Out(i).AppendName = [Out(i).AppendName ' - ' Info.Attribute(j).Value];
            end
        end
    end
    %
    if strcmp(Out(i).Geom,'PNT')
        Out(i).AppendName = [Out(i).AppendName ' (points)'];
    end
end
%
OutNames = cellfun(@(x,y)cat(2,x,y), {Out.Name}, {Out.AppendName}, 'uniformoutput', false);
[~,~,iuName] = unique(OutNames);
for i = 1:length(Out)
    if sum(iuName == iuName(i)) > 1
        switch Out(i).Geom
            case {'UGRID1D_NETWORK-NODE','UGRID1D_NETWORK-EDGE','UGRID1D-NODE','UGRID1D-EDGE'}
                dimStr = ' - 1D';
            case {'UGRID2D-NODE','UGRID2D-EDGE','UGRID2D-FACE'}
                if Out(i).DimFlag(K_)
                    dimStr = ' - 3D';
                else
                    dimStr = ' - 2D';
                end
            otherwise
                if Out(i).DimFlag(M_) && Out(i).DimFlag(N_) 
                    if Out(i).DimFlag(K_)
                        dimStr = ' - 3D';
                    else
                        dimStr = ' - 2D';
                    end
                else
                    dimStr = '';
                end
        end
        Out(i).AppendName = [dimStr, Out(i).AppendName];
    end
end
%
Meshes = zeros(0,2);
for loop = 1:2
    for i = 1:length(Out)
        switch Out(i).Geom
            case {'UGRID1D_NETWORK-NODE','UGRID1D_NETWORK-EDGE','UGRID1D-NODE','UGRID1D-EDGE','UGRID2D-NODE','UGRID2D-EDGE','UGRID2D-FACE'}
                varid = get_varid(Out(i))+1;
                thisMesh = FI.Dataset(varid).Mesh;
                if loop == 1
                    if thisMesh{4} == -1 && Out(i).NVal == 0
                        Meshes(end+1,:) = [thisMesh{3} i];
                    end
                else % loop == 2
                    j = find(Meshes(:,1) == thisMesh{3});
                    Out(i).UseGrid = Meshes(j,2);
                end
        end
    end
end
%
% all partitions ... no index across all partitions ... set DimFlag to inf
% to display dimension size as ?
%
if domain==FI.NumDomains+1
    for i = 1:length(Out)
        if Out(i).DimFlag(M_)
            Out(i).DimFlag(M_) = inf;
        end
        if Out(i).DimFlag(N_)
            Out(i).DimFlag(N_) = inf;
        end
    end
end
%
% detect vector quantities
%
% VecStdNameTable: component_1, component_2, vector_type, quickplot_combined_name
%   vector_type = 0: x and y
%                 4: magnitude and to direction
%                -4: magnitude and from direction
%                 5: normal and tangential (on ugrid edge)
VecStdNameTable = {
    'sea_water_speed',               'direction_of_sea_water_velocity',4,'sea_water_velocity'
    'sea_ice_speed',                 'direction_of_sea_ice_speed',     4,'sea_ice_velocity'
    'wind_speed',                    'wind_to_direction',              4,'air_velocity'
    'wind_speed',                    'wind_from_direction',           -4,'air_velocity'
    'eastward_sea_water_velocity',   'northward_sea_water_velocity',   0,'sea_water_velocity'
    'sea_water_x_velocity',          'sea_water_y_velocity',           0,'sea_water_velocity'
    'eastward_sea_ice_velocity',     'northward_sea_ice_velocity',     0,'sea_ice_velocity'
    'eastward_wind_shear',           'northward_wind_shear',           0,'wind_shear'
    'surface_downward_eastward_wind','surface_downward_northward_wind',0,'surface_downward_wind'
    'eastward_wind',                 'northward_wind',                 0,'air_velocity'};
nVecStdNames = size(VecStdNameTable,1);
i=1;
varid_Out = get_varid(Out);
while i<length(varid_Out)
    if iscell(Out(i).varid) || isempty(Out(i).varid)
        i=i+1;
        continue
    end
    %
    % own convention has preference over CF convention of standard names
    % since there may be multiple quantities with the same standard name
    % (but defined on different meshes or derived using different cell
    % methods).
    %
    y=[];
    z=[];
    ncmp = strfind(Out(i).Name,', n-component');
    xcmp = strfind(Out(i).Name,', x-component');
    xcmp2 = strfind(Out(i).Name,' (x-component)');
    if ~isempty(ncmp)
        Ystr = Out(i).Name; Ystr(ncmp+2)='t';
        Name = Out(i).Name([1:ncmp-1 ncmp+13:end]);
        j=1; j2=2;
        VectorDef = 5; % normal and tangential
        %
        names = {Out.Name};
        y = find(strcmp(names,Ystr) & strcmp({Out.AppendName},Out(i).AppendName));
        if length(y)>1
            y = [];
        end
    elseif ~isempty(xcmp) || ~isempty(xcmp2)
        if ~isempty(xcmp)
            Ystr = Out(i).Name; Ystr(xcmp+2)='y';
            Zstr = Out(i).Name; Zstr(xcmp+2)='z';
            Name = Out(i).Name([1:xcmp-1 xcmp+13:end]);
        else
            Ystr = Out(i).Name; Ystr(xcmp2+2)='y';
            Zstr = Out(i).Name; Zstr(xcmp2+2)='z';
            Name = Out(i).Name([1:xcmp2-1 xcmp2+14:end]);
        end
        col = 1;
        VectorDef = 0; % x and y
        %
        names = {Out.Name};
        y = find(strcmp(names,Ystr) & strcmp({Out.AppendName},Out(i).AppendName));
        if length(y)>1
            y = [];
        end
        %
        z = find(strcmp(names,Zstr) & strcmp({Out.AppendName},Out(i).AppendName));
        if length(z)>1
            z = [];
        end
    end
    %
    if isempty(y)
        % no (unique) match found yet
        stdname = FI.Dataset(Out(i).varid+1).StdName;
        j = strcmp(stdname,VecStdNameTable(:,1:2));
        if any(j(:))
            % standard name matches known standard name of a vector component
            row = find(any(j,2));
            col = j*[1;2];
            col(col==0) = [];
            j2 = row;
            j2(col == 1) = j2(col == 1) + nVecStdNames; 
            Ystr = VecStdNameTable(j2);
            %
            ii = i+1:length(Out);
            varid = varid_Out(ii);
            ii(varid<0) = [];
            varid(varid<0) = [];
            stdnames = {FI.Dataset(varid+1).StdName};
            ymatch = ismember(stdnames,Ystr) & strcmp({Out(ii).AppendName},Out(i).AppendName);
            %
            if any(ymatch)
                if sum(ymatch) > 1
                    y = []; % non-unique match, don't do something random
                else
                    y = ii(ymatch);
                    stdname = stdnames(ymatch);
                    rmatch = strcmp(stdname,Ystr);
                    row = row(rmatch);
                    col = col(rmatch);
                    Name = VecStdNameTable{row,4};
                    VectorDef = VecStdNameTable{row,3};
                end
            end
        end
    end
    %
    if ~isempty(y)
        Out(i).NVal=2;
        Out(i).Name = Name;
        Out(i).VectorDef = VectorDef;
        Out(i).MNK = VectorDef==5;
        if col == 1
            Out(i).varid=[Out(i).varid Out(y).varid];
        else
            Out(i).varid=[Out(y).varid Out(i).varid];
        end
        if ~isempty(z)
            Out(i).NVal = 3;
            Out(i).varid(3) = Out(z).varid;
        end
        Out([y,z])=[];
        varid_Out([y,z]) = [];
        if y<i && ~isempty(z) && z<i
            i=i-2;
        elseif y<i || (~isempty(z) && z<i)
            i=i-1;
        end
    end
    i=i+1;
end
%
for i = 1:length(Out)
    Out(i).Name = [Out(i).Name Out(i).AppendName];
end
Out = rmfield(Out,'AppendName');
% -----------------------------------------------------------------------------


function OutCoords = group_matching_dimensions(Out, Dummy)
matchDims = 1:length(Out);
for i = 1:length(Out)
    for j = 1:i-1
        if isequal(Out(i).DimName,Out(j).DimName)
            matchDims(i) = j;
            break
        end
    end
end
%
OutCoords = [];
if ~isempty(matchDims)
   for m = unique(matchDims)
      OutCoords = [OutCoords Out(matchDims==m) Dummy];
   end
end


function ivar = get_varid(Props)
ivar = repmat(-1,size(Props));
for i = 1:numel(Props)
    if iscell(Props(i).varid)
        ivar(i) = Props(i).varid{2};
    elseif ~isempty(Props(i).varid)
        ivar(i) = Props(i).varid(1);
    end
end


% -----------------------------------------------------------------------------
function [Subf,rec]=getsubfields(FI,Props,f)
if isempty(Props.SubFld)
    Subf = {};
    rec  = [];
else
    nSubFld = size(Props.SubFld,1);
    Subf    = {''};
    Val     = zeros(1,0);
    for d = 1:nSubFld
        nval = Props.SubFld{d,2};
        sfld = Props.SubFld{d,1};
        if size(Props.SubFld,2)==3
            strs = Props.SubFld{d,3};
        else
            strs = [];
        end
        %
        newSubf = cell(1,length(Subf)*nval);
        newVal  = zeros(length(Subf)*nval,d);
        v0      = 0;
        for sf = 1:length(Subf)
            subf_f = Subf{sf};
            Val_f  = Val(sf,:);
            if ~isempty(subf_f)
                sep = ', ';
            else
                sep = '';
            end
            for v = 1:nval
                if ~isempty(strs)
                    newSubf{v0+v} = [subf_f sep strs{v}];
                elseif nval>1
                    newSubf{v0+v} = sprintf('%s%s%s=%i',subf_f,sep,sfld,v);
                else
                    newSubf{v0+v} = subf_f;
                end
                newVal(v0+v,:)=[Val_f v];
            end
            v0 = v0+nval;
        end
        Subf = newSubf;
        Val  = newVal;
    end
    if isequal(Subf,{''})
        Subf = {};
    end
    rec.Fld = Props.SubFld(:,1)';
    rec.Val = Val;
end
if nargin>2 && f~=0
    Subf = Subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
M_ = 3;
ndims = length(Props.DimFlag);
sz = zeros(1,ndims);
%
% single partition value
%
varid = Props.varid;
loc = [];
if iscell(varid)
    switch varid{1}
        case 'stream_function'
            % get underlying discharge on edge variable
            Info = FI.Dataset(varid{2}+1);
            if ~isnan(Info.TSMNK(1))
                sz(1) = FI.Dimension(Info.TSMNK(1)+1).Length;
            end
            % get the x-coordinates variable for the nodes of the mesh
            XVar = FI.Dataset(Info.Mesh{3}).X;
            % get the node dimension
            dimNodes = FI.Dataset(XVar).TSMNK(3)+1;
            sz(M_) = FI.Dimension(dimNodes).Length;
            varid{2} = XVar;
        case 'net_discharge_into_cell'
            % get underlying discharge on edge variable
            Info = FI.Dataset(varid{2}+1);
            if ~isnan(Info.TSMNK(1))
                sz(1) = FI.Dimension(Info.TSMNK(1)+1).Length;
            end
            % get the mesh variable
            Info = FI.Dataset(Info.Mesh{3});
            % get the face dimension
            sz(M_) = FI.Dimension(strcmp({FI.Dimension.Name},Info.Mesh{7})).Length;
        case 'node_index'
            Info = FI.Dataset(varid{2}+1);
            sz(M_) = FI.Dimension(strcmp({FI.Dimension.Name},Info.Mesh{5})).Length;
            loc = 0;
        case 'edge_index'
            Info = FI.Dataset(varid{2}+1);
            sz(M_) = FI.Dimension(strcmp({FI.Dimension.Name},Info.Mesh{6})).Length;
            loc = 1;
        case 'face_index'
            Info = FI.Dataset(varid{2}+1);
            sz(M_) = FI.Dimension(strcmp({FI.Dimension.Name},Info.Mesh{7})).Length;
            loc = 2;
        otherwise
            Props.varid = Props.varid{2};
            sz = getsize(FI,Props);
    end
    varid = varid{2};
elseif ~isempty(Props.varid)
    for q = 1:length(Props.varid)
        Info=FI.Dataset(Props.varid(q)+1);
        for d_ = 1:ndims
            if Props.DimFlag(d_) && Info.TSMNK(d_)>=0
                sz(d_) = FI.Dimension(Info.TSMNK(d_)+1).Length;
            end
        end
    end
end
%
% in case of merged partitions overrule the previous value
%
if isfield(FI,'MergedPartitions') && ...
        Props.DimFlag(M_) && ...
        isfinite(Props.DimFlag(M_)) && ...
        ~isempty(varid)
    M = FI.Dataset(varid+1).Mesh;
    if iscell(M) && strcmp(M{1},'ugrid')
        if isempty(loc)
            loc = M{4};
        end
        switch loc
            case {0,-1} % nodes
                sz(M_) = FI.MergedPartitions.nNodes;
            case 1 % edges
                sz(M_) = FI.MergedPartitions.nEdges;
            case 2 % faces
                sz(M_) = FI.MergedPartitions.nFaces;
        end
    end
end
%======================== SPECIFIC CODE =======================================

% -----------------------------------------------------------------------------
function Domains=domains(FI)
if FI.NumDomains > 1
    format = sprintf('%%%d.%dd-',FI.DomainCount.Digits,FI.DomainCount.Digits);
    Domains = multiline(sprintf(['partition ' format],FI.DomainCount.Offset+(0:FI.NumDomains-1)),'-','cell');
    Domains{end} = 'all partitions';
    if isfield(FI,'MergedPartitions')
        Domains{end+1} = 'merged partitions';
    end
else
    Domains = {};
end


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
varid = get_varid(Props)+1;
tvar = FI.Dataset(varid).Time;
if isempty(tvar)
    tinfo = [];
    T = [];
else
    tinfo = FI.Dataset(tvar).Info;
    if nargin>2
        if isequal(t,0)
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name,0,FI.Dataset(tvar).Size);
        elseif length(t)==1
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name,t-1,1);
        elseif isequal(t,t(1):t(2)-t(1):t(end))
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name,t(1)-1,(t(end)-t(1))/(t(2)-t(1))+1,t(2)-t(1));
        else
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name,t(1)-1,t(end)-t(1)+1);
            T = T(t-t(1)+1);
        end
    else
        T = nc_varget(FI.Filename,FI.Dataset(tvar).Name);
    end
    T = double(T);
end
if ~isstruct(tinfo) % likely even empty
    % continue with T = T;
elseif ~isfield(tinfo,'RefDate') || isempty(tinfo.RefDate)
    T = tinfo.DT * T;
elseif ischar(tinfo.RefDate)
    switch tinfo.RefDate
        case 'day as %Y%m%d.%f'
            Y = floor(T/10000);
            T = T - Y*10000;
            M = floor(T/100);
            T = T - M*100;
            D = floor(T);
            T = T - D;
            T = datenum(Y,M,D) + T;
        otherwise
            T = tinfo.DT * T;
    end
else
    T = tinfo.RefDate + tinfo.DT * T;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
stcrd = FI.Dataset(get_varid(Props)+1).Station;
if FI.Dataset(stcrd).CharDim==FI.Dataset(stcrd).Dimid(1)
    % PRESERVE_FVD=true
    [Stations, status] = qp_netcdf_get(FI,stcrd-1,fliplr(FI.Dataset(stcrd).Dimension));
else
    [Stations, status] = qp_netcdf_get(FI,stcrd-1,FI.Dataset(stcrd).Dimension);
end
if t~=0
    if isa(Station,'string')
        Stations = Stations(t);
    else
        Stations = Stations(t,:);
    end
end
S=cellstr(Stations);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Dimension = matchdims(CoordDims,VarDims)
Dimension = zeros(1,length(VarDims));
%
for i = 1:length(CoordDims)
    idim = strmatch(CoordDims{i},VarDims,'exact');
    if ~isempty(idim)
        Dimension(idim) = i;
    else
        error('Coordinate dimension ''%s'' not matched by variable dimension.',CoordDims{i})
    end
end
%
for i = 1:length(VarDims)
    if Dimension(i) == 0
        Dimension(i) = max(Dimension)+1;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Coord = expand_hdim(Coord,szData,hdim)
szC = size(Coord);
if length(szC)<length(hdim)
    szC(end+1:length(hdim))=1;
end
if length(szData)<length(hdim)
    szData(end+1:length(hdim))=1;
end
if ~isequal(szC(hdim),szData(hdim))
    ndim = length(hdim); % catch bounds dimension
    repC = szData(1:ndim)./szC(1:ndim);
    repC(~hdim)=1;
    Coord = repmat(Coord,repC);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function szZData  = updateSize(szData,szFld,hdims)
szZData = szData;
szZData(hdims) = szFld(hdims);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [X,Y,EdgeX,EdgeY] = branch2xy(BrX,BrY,xUnit,BrL,BrNr,BrOffset,eBrNr,EdgeNode)
EdgeX = cell(size(eBrNr));
EdgeY = EdgeX;
X = zeros(size(BrNr));
Y = X;
if strcmp(xUnit,'deg')
    cUnit = {'Geographic'};
else
    cUnit = {};
end
% need to loop over all branches on which at least one edge or one node is
% located. An edge may be located on a branch without nodes if the edge
% matches the whole branch. A node may be located on a branch without edge
% in case of the mesh covers only a part of the network (parallel
% partition).
uBrNr = unique([eBrNr;BrNr]);
doublePoints = false(size(uBrNr));
%
% first check all the nodes such that they are all available when checking
% whether an edge with one node on the branch is at the beginning or the
% end of the branch.
for i = 1:length(uBrNr)
    bN = uBrNr(i);
    bX = BrX{bN};
    bY = BrY{bN};
    Mask = diff(bX)==0 & diff(bY)==0;
    if any(Mask)
        doublePoints(i) = true;
        bX(Mask)=[];
        bY(Mask)=[];
        % Update BrX/Y such that we don't need to perform this check again
        % when processing the edges.
        BrX{bN} = bX;
        BrY{bN} = bY;
    end
    bS = pathdistance(bX,bY,cUnit{:});
    %
    for j = find(BrNr==bN)'
        s  = (BrOffset(j)/BrL(bN))*bS(end);
        if s>bS(end)
            error('Offset %g larger than branch length %g',BrOffset(j),BrL(bN));
        else
            x = interp1(bS,bX,s);
            y = interp1(bS,bY,s);
        end
        X(j) = x;
        Y(j) = y;
    end
end
%
% now we can check all the edges
distmax = 0;
nwarn = 0;
for i = 1:length(uBrNr)
    bN = uBrNr(i);
    bX = BrX{bN};
    bY = BrY{bN};
    bS = pathdistance(bX,bY,cUnit{:});
    %
    for j = find(eBrNr==bN)'
        n = EdgeNode(j,:);
        nBranches = BrNr(n);
        if all(nBranches==bN)
            % both nodes on this branch, select the segment
            s  = (sort(BrOffset(n))/BrL(bN))*bS(end);
            I = bS>s(1) & bS<s(2);
            x = interp1(bS,bX,s);
            y = interp1(bS,bY,s);
            EdgeX{j} = [x(1);bX(I);x(2)];
            EdgeY{j} = [y(1);bY(I);y(2)];
        elseif all(nBranches~=bN)
            % both nodes on other branches, select the whole branch
            EdgeX{j} = bX;
            EdgeY{j} = bY;
            x1 = X(n(1));
            y1 = Y(n(1));
            x2 = X(n(2));
            y2 = Y(n(2));
            dist1 = min(sqrt((bX([1 end])-x1).^2 + (bY([1 end])-y1).^2));
            dist2 = min(sqrt((bX([1 end])-x2).^2 + (bY([1 end])-y2).^2));
            if min(dist1) > 0 && min(dist2) > 0
                dist = max(min(dist1),min(dist2));
                nwarn = nwarn + 1;
                if dist > distmax
                    msg = {'The edge %i connecting node %i to %i is supposed to lie on branch %i,\nbut both nodes don''t seem to lie on that branch (mismatch = %g).\n',j,n(1),n(2),bN,dist};
                    distmax = dist;
                end
            elseif min(dist1) > 0
                dist = min(dist1);
                nwarn = nwarn + 1;
                if dist > distmax
                    msg = {'The edge %i connecting node %i to %i is supposed to lie on branch %i,\nbut node %i doesn''t seem to lie on that branch (mismatch = %g).\n',j,n(1),n(2),bN,n(1),dist};
                    distmax = dist;
                end
            elseif min(dist2) > 0
                dist = min(dist2);
                nwarn = nwarn + 1;
                if dist > distmax
                    msg = {'The edge %i connecting node %i to %i is supposed to lie on branch %i,\nbut node %i doesn''t seem to lie on that branch (mismatch = %g).\n',j,n(1),n(2),bN,n(2),dist};
                    distmax = dist;
                end
            end
        else
            % one node on this branch, one on another branch
            if nBranches(1)==bN
                n1 = n(1);
                n2 = n(2);
            else
                n2 = n(1);
                n1 = n(2);
            end
            s  = (BrOffset(n1)/BrL(bN))*bS(end);
            x = interp1(bS,bX,s);
            y = interp1(bS,bY,s);
            x2 = X(n2);
            y2 = Y(n2);
            dist = sqrt((bX([1 end])-x2).^2 + (bY([1 end])-y2).^2);
            if dist(1) < dist(2)
                % second node seems closer to the beginning of the branch
                I = bS<s;
                EdgeX{j} = [bX(I);x];
                EdgeY{j} = [bY(I);y];
                if dist(1) > 0
                    nwarn = nwarn + 1;
                    if dist(1) > distmax
                        msg = {'The edge %i connecting node %i to %i is supposed to lie on branch %i,\nbut node %i doesn''t seem to lie on that branch (mismatch = %g).\n',j,n(1),n(2),bN,n2,dist(1)};
                        distmax = dist(1);
                    end
                end
            else
                % second node seems closer to the end node of the branch
                I = bS>s;
                EdgeX{j} = [x;bX(I)];
                EdgeY{j} = [y;bY(I)];
                if dist(2) > 0
                    nwarn = nwarn + 1;
                    if dist(2) > distmax
                        msg = {'The edge %i connecting node %i to %i is supposed to lie on branch %i,\nbut node %i doesn''t seem to lie on that branch (mismatch = %g).\n',j,n(1),n(2),bN,n2,dist(2)};
                        distmax = dist(2);
                    end
                end
            end
        end
    end
end
if distmax > eps(single(1))
    msg1 = sprintf('Detected %i branch mismatches. Largest mismatch occurred at:',nwarn);
    msg2 = sprintf(msg{:});
    ui_message('warning',{msg1,msg2});
end
if any(doublePoints)
    if sum(doublePoints)==1
        ui_message('warning','Double geometry points encountered on branch: %i',find(doublePoints))
    else
        ui_message('warning','Double geometry points encountered on branches: %s',vec2str(find(doublePoints),'nobrackets'))
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function unit = get_unit(Info)
unit = [];
if ~isempty(Info.Attribute)
    Attribs = {Info.Attribute.Name};
    j = strmatch('units',Attribs,'exact');
    if ~isempty(j)
        unit = Info.Attribute(j).Value;
        units = {'degrees_east','degree_east','degreesE','degreeE', ...
            'degrees_north','degree_north','degreesN','degreeN'};
        if ismember(unit,units)
            unit = 'deg';
        end
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function [BrX,BrY,xUnit,BrL] = get_edge_geometry(FI,csp)
CSP = FI.Dataset(csp);
atteg = strmatch('edge_geometry',{CSP.Attribute.Name});
veg = strmatch(CSP.Attribute(atteg).Value,{FI.Dataset.Name},'exact');
% node count dimension
VEG = FI.Dataset(veg);
attnc = strmatch('node_count',{VEG.Attribute.Name});
ndc = [];
if ~isempty(attnc)
    ndc = strmatch(VEG.Attribute(attnc).Value,{FI.Dataset.Name},'exact');
    ndcd = strmatch(VEG.Attribute(attnc).Value,{FI.Dimension.Name},'exact');
    if isempty(ndc)
        if ~isempty(ndcd)
            ui_message('error','Geometry %s attribute node_count reads "%s". This is a dimension, but should be a variable.',VEG.Name,VEG.Attribute(attnc).Value)
        else
            ui_message('error','Geometry %s attribute node_count reads "%s". Variable not found.',VEG.Name,VEG.Attribute(attnc).Value)
        end
    end
end
if isempty(ndc)
    attnc = strmatch('part_node_count',{VEG.Attribute.Name});
    ui_message('error','Incorrect attribute "part_node_count" used for specifying the node_count for geometry variable "%s".',VEG.Name)
    ndc = strmatch(VEG.Attribute(attnc).Value,{FI.Dataset.Name},'exact');
end
%
if isempty(FI.Dataset(veg).X)
    error('Missing X coordinate for geometry variable "%s".',VEG.Name)
elseif isempty(FI.Dataset(veg).Y)
    error('Missing Y coordinate for geometry variable "%s".',VEG.Name)
end
[BrX, status] = qp_netcdf_get(FI,FI.Dataset(FI.Dataset(veg).X));
[BrY, status] = qp_netcdf_get(FI,FI.Dataset(FI.Dataset(veg).Y));
[NDC, status] = qp_netcdf_get(FI,FI.Dataset(ndc));
BrX = mat2cell(BrX,NDC,1);
BrY = mat2cell(BrY,NDC,1);
%
xUnit = get_unit(FI.Dataset(FI.Dataset(veg).X));
%
if nargout>3
    attbl = strmatch('edge_length',{CSP.Attribute.Name});
    if isempty(attbl)
        attbl = strmatch('branch_lengths',{CSP.Attribute.Name});
        if ~isempty(attbl)
            ui_message('error','Incorrect attribute "branch_lengths" used for specifying the edge_length for 1D UGRID variable "%s".',CSP.Name)
        end
    end
    if ~isempty(attbl)
        vbl = strmatch(CSP.Attribute(attbl).Value,{FI.Dataset.Name},'exact');
        [BrL, status] = qp_netcdf_get(FI,FI.Dataset(vbl));
    else
        BrL = zeros(size(BrX));
        for i = 1:length(BrX)
            brl = pathdistance(BrX{i},BrY{i}); % Cartesian or spherical?
            BrL(i) = brl(end);
        end
    end
end


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd
    case 'initialize'
        optfig(mfig);
        set(findobj(mfig,'tag','ncdump'),'enable','on')
        set(findobj(mfig,'tag','ncdumpto=?'),'enable','on','backgroundcolor',Active)
    case 'ncdump'
        out = get(findobj(mfig,'tag','ncdumpto=?'),'value');
        switch out
            case 1
                [f,p] = uiputfile('*.ncdump','Specify Dump File');
                if ischar(f)
                    fid = fopen([p,f],'w','n','US-ASCII');
                    nc_dump(FI.FileName,fid)
                    fclose(fid);
                end
            case {2,3}
                f = tempname;
                fid = fopen(f,'w','n','US-ASCII');
                nc_dump(FI.FileName,fid);
                fclose(fid);
                C = getfile(f);
                delete(f);
                if out==2
                    clipboard('copy',sprintf('%s\n',C{:}));
                else
                    C = strrep(C,sprintf('\t'),'   ');
                    C(cellfun(@isempty,C)) = {' '};
                    ui_message('message',C);
                end
            case 4
                nc_dump(FI.FileName)
        end
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
uicontrol('Parent',h0, ...
    'Style','pushbutton', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions ncdump', ...
    'Position',[11 voffset-3 140 24], ...
    'String','NetCDF Dump to', ...
    'Horizontalalignment','left', ...
    'Enable','off', ...
    'Tag','ncdump');
dumpto = uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Inactive, ...
    'Position',[161 voffset 170 20], ...
    'String',{'File','Clipboard','Message Window'}, ...
    'Enable','off', ...
    'Tag','ncdumpto=?');
if ~isstandalone
    set(dumpto, 'String',{'File','Clipboard','Message Window','MATLAB Command Window'})
end
% -----------------------------------------------------------------------------

function C = getfile(file)
if ischar(file)
    localfopen = true;
    fid = fopen(file,'r','n','US-ASCII');
else
    localfopen = false;
    fid = file;
end
C = cell(1000,1);
i = 0;
while 1
    L = fgetl(fid);
    if ischar(L)
        i = i+1;
        if i>length(C)
            C{2*i} = [];
        end
        C{i} = L;
    else
        break
    end
end
C = C(1:i);
if localfopen
    fclose(fid);
end

function Psi = compute_stream_function(Discharge, EdgeNodeConnect, nNodes)
mask_edges = isnan(Discharge);
EdgeNodeConnect(mask_edges,:) = [];
Discharge(mask_edges) = [];
%
Psi = zeros(nNodes,1);
Psi(EdgeNodeConnect) = NaN;
nNodes = sum(isnan(Psi));
%
if nNodes > 0
    nNodes_done = 0;
    hPB = progressbar(0, 'title', 'Computing stream function ...');
    
    % loop in case there are multiple disconnected areas
    while nNodes_done < nNodes
        % start by setting the first node to zero
        Mask = isnan(Psi);
        i = find(Mask,1,'first');
        Psi(i) = 0;
        
        % loop while something changes
        found = true;
        while found
            nNodes_done = sum(~isnan(Psi));
            progressbar(nNodes_done/nNodes, hPB);
            
            % change is caused by any edge that connects a node with
            % defined value to a node without value (NaN)
            found = false;
            for i = 1:size(EdgeNodeConnect,1)
                if ~isnan(Psi(EdgeNodeConnect(i,1))) && isnan(Psi(EdgeNodeConnect(i,2))) && ~isnan(Discharge(i))
                    Psi(EdgeNodeConnect(i,2)) = Psi(EdgeNodeConnect(i,1)) + Discharge(i);
                    found = true;
                elseif isnan(Psi(EdgeNodeConnect(i,1))) && ~isnan(Psi(EdgeNodeConnect(i,2))) && ~isnan(Discharge(i))
                    Psi(EdgeNodeConnect(i,1)) = Psi(EdgeNodeConnect(i,2)) - Discharge(i);
                    found = true;
                end
            end
        end
        
        % for each individual area the minimum should be set to 0
        Psi(Mask) = Psi(Mask) - min(Psi(Mask));
        
        % remove all used edges to speed up searching
        used = ~isnan(Psi(EdgeNodeConnect(:,1)));
        EdgeNodeConnect(used,:) = [];
        Discharge(used) = [];
    end
    delete(hPB)
end


function Psi = compute_net_discharge_into_cell(Discharge, EdgeFaceConnect, nFaces)
from_somewhere = EdgeFaceConnect(:,1)~=0 & ~isnan(EdgeFaceConnect(:,1));
to_somewhere = EdgeFaceConnect(:,2)~=0 & ~isnan(EdgeFaceConnect(:,2));
Psi = -accumarray(EdgeFaceConnect(from_somewhere,1),Discharge(from_somewhere)',[nFaces,1]) ...
      +accumarray(EdgeFaceConnect(to_somewhere,2),Discharge(to_somewhere)',[nFaces,1]);


function check = strend(Str,SubStr)
len_ss = length(SubStr);
if length(Str) <  len_ss
    check = false;
else
    check = strcmp(Str(end-len_ss+1:end), SubStr);
end

function netcdf_var = formvar(FormulaTerms, key)
keycol =[key, ':'];
index = find(strcmpi(FormulaTerms(:,1), keycol));
if isempty(index)
    error('No key %s found in the formula terms.', key)
else
    netcdf_var = FormulaTerms{index,2};
end


function iq2 = find_quantity(structList1,iq1,structList2)
if iq1 > length(structList1)
    iq2 = -1;
    return
end
struct1 = structList1(iq1);
isAMatch = zeros(size(structList2));
for iq2 = 1:length(structList2)
    struct2 = structList2(iq2);
    if strcmp(struct1.ncVarName,struct2.ncVarName) && ...
            strcmp(struct1.Geom,struct2.Geom)
        if strcmp(struct1.Name,struct2.Name)
            isAMatch(iq2) = 2;
        else
            isAMatch(iq2) = 1;
        end
    end
end
iq2 = find(isAMatch==2);
if isempty(iq2)
    iq2 = 0;
    if max(isAMatch) == 1
        % not perfect, but maybe still a match ...
        iq2 = ustrcmpi(struct1.Name,{structList2.Name});
        if iq2 < 0
            iq2 = 0;
        else
            % exactly one found
        end
    end
elseif length(iq2) > 1
    iq2
    iq2 = 0;
    fprintf('MULTIPLE matches found\n');
else
    % exactly one found
end


function structList = insert_quantity(structList,iq,struct)
flds = fieldnames(structList);
flds2 = fieldnames(struct);
if ~isequal(flds,flds2)
    missingFields = setdiff(flds,flds2);
    if ~isempty(missingFields)
        % add dummy entries for the missing fields
        for f = 1:length(missingFields)
            struct(1).(missingFields{f}) = [];
        end
    end
    % no more missing fields
    sharedFields = flds;
    
    extraFields = setdiff(flds2,flds);
    % need to make sure that the order of the fields is the same, so
    % execute the next line always
    struct = orderfields(struct,[sharedFields;extraFields]);
    if ~isempty(extraFields)
        for f = 1:length(extraFields)
            structList(1).(extraFields{f}) = [];
        end
    end
end
structList = [structList(1:iq-1) struct structList(iq:end)];


function [Ans,meshAttribNames,getOptions] = qp_netcdf_ugrid_get_xy(Ans,FI,msh,dloc,idx)
meshInfo      = FI.Dataset(msh);
%
dimNodes = meshInfo.Mesh{5};
dimEdges = meshInfo.Mesh{6};
dimFaces = meshInfo.Mesh{7};
allDims = {FI.Dimension.Name};
MeshSubset = {};
switch dloc
    case 0 % data at NODE
        MeshSubset = {'NODE' dimNodes idx
            'EDGE' dimEdges -1
            'FACE' dimFaces -1};
        if ~isempty(dimEdges)
            MeshSubset{2,3} = 1:FI.Dimension(strcmp(dimEdges,allDims)).Length;
        end
        if ~isempty(dimFaces)
            MeshSubset{3,3} = 1:FI.Dimension(strcmp(dimFaces,allDims)).Length;
        end
    case 1 % data at EDGE
        MeshSubset = {'NODE' dimNodes 1:FI.Dimension(strcmp(dimNodes,allDims)).Length
            'EDGE' dimEdges idx
            'FACE' dimFaces -1};
        if ~isempty(dimFaces)
            MeshSubset{3,3} = 1:FI.Dimension(strcmp(dimFaces,allDims)).Length;
        end
    case 2 % data at FACE
        MeshSubset = {'NODE' dimNodes 1:FI.Dimension(strcmp(dimNodes,allDims)).Length
            'EDGE' dimEdges -1
            'FACE' dimFaces idx};
        if ~isempty(dimEdges)
            MeshSubset{2,3} = 1:FI.Dimension(strcmp(dimEdges,allDims)).Length;
        end
end
if isempty(MeshSubset)
    getOptions = {};
else
    getOptions = {'mesh_subsets' MeshSubset};
end
%
for c = 'XY'
    if isempty(meshInfo.(c))
        error('No %s coordinate found for %s.',c,meshInfo.Name)
    end
    CoordInfo2 = FI.Dataset(meshInfo.(c));
    [Ans.(c), status] = qp_netcdf_get(FI,CoordInfo2);
    %
    unit = get_unit(CoordInfo2);
    if ischar(unit)
        Ans.([c 'Units']) = unit;
    end
end
if strcmp(meshInfo.Mesh{1},'ugrid1d_network')
    % Ans.X contains mesh node branch index
    % Ans.Y contains mesh node offset/chainage
    %
    attcsp = strmatch('coordinate_space',{meshInfo.Attribute.Name});
    csp = strmatch(meshInfo.Attribute(attcsp).Value,{FI.Dataset.Name},'exact');
    [BrX,BrY,xUnit,BrL] = get_edge_geometry(FI,csp);
    %
    if isempty(FI.Dataset(meshInfo.X).Attribute)
        istart = [];
    else
        istart = strmatch('start_index',{FI.Dataset(meshInfo.X).Attribute.Name});
    end
    if isempty(istart)
        start_index = 0;
    else
        start_index = FI.Dataset(meshInfo.X).Attribute(istart).Value;
    end
    start_index = verify_start_index(istart, start_index, min(Ans.X), max(Ans.X), length(BrX), 'branch', FI.Dataset(meshInfo.X).Name);
    Ans.X = Ans.X-start_index+1;
    %
    % Get edge_node_connectivity
    attENC = strmatch('edge_node_connectivity',{meshInfo.Attribute.Name});
    [e2n, status] = qp_netcdf_get(FI, meshInfo.Attribute(attENC).Value);
    i_e2n = strmatch(meshInfo.Attribute(attENC).Value, {FI.Dataset.Name});
    if isempty(FI.Dataset(i_e2n).Attribute)
        istart = [];
    else
        istart = strmatch('start_index',{FI.Dataset(i_e2n).Attribute.Name});
    end
    if ~isempty(istart)
        start_index = FI.Dataset(i_e2n).Attribute(istart).Value;
    else
        start_index = 0;
    end
    start_index = verify_start_index(istart, start_index, min(e2n(:)), max(e2n(:)), length(Ans.X), 'node',FI.Dataset(i_e2n).Name);
    e2n = e2n-start_index+1;
    %
    % Get mesh_edge branch affinity (read from edge_coordinates or reconstructed)
    attECO = strmatch('edge_coordinates',{meshInfo.Attribute.Name});
    if ~isempty(attECO)
        ecoords = strsplit(meshInfo.Attribute(attECO).Value);
        for iec = 1:length(ecoords)
            i_eBrNr = strmatch(ecoords{iec},{FI.Dataset.Name});
            if isempty(FI.Dataset(i_eBrNr).Attribute)
                ecAtt = {};
            else
                ecAtt = {FI.Dataset(i_eBrNr).Attribute.Name};
            end
            if ismember('units',ecAtt) || ismember('standard_name',ecAtt)
                % x-coordinate, y-coordinate, offset
                continue
            end
            % branch_id
            [eBrNr, status] = qp_netcdf_get(FI,FI.Dataset(i_eBrNr));
            if any(eBrNr<0)
                ui_message('warning','Invalid %s data: negative branch ids encountered. Ignoring this data.',FI.Dataset(i_eBrNr).Name)
                eBrNr = [];
            end
            break
        end
        if isempty(FI.Dataset(i_eBrNr).Attribute)
            istart = [];
        else
            istart = strmatch('start_index',{FI.Dataset(i_eBrNr).Attribute.Name});
        end
        if ~isempty(istart)
            start_index = FI.Dataset(i_eBrNr).Attribute(istart).Value;
        else
            start_index = 0;
        end
        start_index = verify_start_index(istart, start_index, min(eBrNr), max(eBrNr), length(BrX), 'branch',FI.Dataset(i_eBrNr).Name);
        eBrNr = eBrNr-start_index+1;
    else
        eBrNr = [];
    end
    if isempty(eBrNr)
        % TODO: create networknode(i)
        % networknode(i) = N if mesh node i coincides with network node N
        % networknode(i) = -1 if mesh node i does not coincide with a network node
        networknode = -ones(size(Ans.X));
        networknode(Ans.Y==0 | Ans.Y==BrL(Ans.X)) = 1;
        %
        % reconstruct mesh_edge branch affinity
        eBrNr = Ans.X(e2n);
        for i = 1:size(eBrNr,1)
            n1 = networknode(e2n(i,1));
            if n1<0
                % start node isn't a network node, so edge must
                % be on same branch.
                % eBrNr(i,1) is correct.
                continue
            end
            n2 = networknode(e2n(i,2));
            if n2<0
                % end node isn't a network node, so edge must
                % be on same branch.
                eBrNr(i,1) = eBrNr(i,2);
                continue
            end
            % both start and end node of edge match a network node.
            % identify the branches between the network nodes.
            %
            if 1
                % if one branch, select that one.
                1
            else
                % if multiple branches, select one and give warning.
                2
            end
        end
        eBrNr = eBrNr(:,1);
    end
    %
    if ischar(xUnit)
        Ans.XUnits = xUnit;
        Ans.YUnits = xUnit;
    end
    [Ans.X,Ans.Y,Ans.EdgeGeometry.X,Ans.EdgeGeometry.Y] = branch2xy(BrX,BrY,xUnit,BrL,Ans.X,Ans.Y,eBrNr,e2n);
end
%
if isempty(meshInfo.Attribute)
    meshAttribNames = {};
else
    meshAttribNames = {meshInfo.Attribute.Name};
end
connect = strmatch('face_node_connectivity',meshAttribNames,'exact');
if ~isempty(connect)
    iconnect = strmatch(meshInfo.Attribute(connect).Value,{FI.Dataset.Name},'exact');
    if isempty(iconnect)
        ui_message('warning','The face_node_connectivity variable "%s" could not be found! Using empty set.',meshInfo.Attribute(connect).Value)
    else
        [Ans.FaceNodeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
        nNodes = sum(~isnan(Ans.FaceNodeConnect),2);
        min_nNodes = min(nNodes);
        if min_nNodes<3
            nError = sum(nNodes==min_nNodes);
            error('%i faces found with %i nodes. Number of nodes per face should be at least 3.',nError,min_nNodes)
        end
        if isempty(FI.Dataset(iconnect).Attribute)
            istart = [];
        else
            istart = strmatch('start_index',{FI.Dataset(iconnect).Attribute.Name},'exact');
        end
        if isempty(istart)
            start = 0;
        else
            start = FI.Dataset(iconnect).Attribute(istart).Value;
        end
        start = verify_start_index(istart, start, min(Ans.FaceNodeConnect(Ans.FaceNodeConnect>=0)), max(Ans.FaceNodeConnect(:)), length(Ans.X), 'node', meshInfo.Attribute(connect).Value);
        Ans.FaceNodeConnect = Ans.FaceNodeConnect - start + 1;
        Ans.FaceNodeConnect(Ans.FaceNodeConnect<1) = NaN;
        % check for indices after missing value
        dFNC = diff(isnan(Ans.FaceNodeConnect),1,2);
        if any(dFNC(:)<0)
            dFNC = max(dFNC,0);
            Mask = [zeros(size(dFNC(:,1))), cumsum(dFNC,2)];
            Ans.FaceNodeConnect(Mask==1) = NaN;
        end
    end
end


function Data = mergePartData(Data,partData,FI,valLoc,valFields,nPrefixDim,prefixDim)
switch valLoc
    case 'NODE'
        nloc = FI.MergedPartitions.nNodes;
        domainMask = FI.MergedPartitions.nodeDMask;
        globalIndex = FI.MergedPartitions.nodeGIndex;
    case 'EDGE'
        nloc = FI.MergedPartitions.nEdges;
        domainMask = FI.MergedPartitions.edgeDMask;
        globalIndex = FI.MergedPartitions.edgeGIndex;
    case 'FACE'
        nloc = FI.MergedPartitions.nFaces;
        domainMask = FI.MergedPartitions.faceDMask;
        globalIndex = FI.MergedPartitions.faceGIndex;
end
for v = valFields
    fld = v{1};
    if isfield(partData,fld)
        sz = size(partData(1).(fld));
        sz(nPrefixDim+1) = nloc;
        Data.(fld) = NaN(sz);
        for p = 1:length(partData)
            masked = domainMask{p};
            Data.(fld)(prefixDim{:},globalIndex{p}(masked),:) = partData(p).(fld)(prefixDim{:},masked,:);
        end
    end
end

function val = get_attrib(Attributes,key)
val = [];
if isfield(Attributes,'Name')
    ikey = ustrcmpi(key,{Attributes.Name});
    if ikey>0
       val = Attributes(ikey).Value;
    end
end

function map = get_colors(n)
n_hues = 6;
n1 = ceil(n/n_hues);
hsv = repmat([0:(n_hues-1)]'/n_hues,n1,1);
n2 = ceil(sqrt(n1));
offset = 0;
for s = n2:-1:1
    hsv(offset + (1:n_hues*n2),2) = s/n2;
    for v = n2:-1:1
        hsv(offset + (1:n_hues),3) = v/n2;
        offset = offset + n_hues;
    end
end
map = hsv2rgb(hsv(1:n,:));

function hNew = plotthis(FI,Props,Parent,Ops,hOld,varargin)
hNew = [];
% balance plot
switch Props.varid{1}
    case 'balance'
        t = varargin{1};
        idx = {t 0 0};
        T = readtim(FI,Props,t);
        varid = Props.varid{2};
        Info = FI.Dataset(varid+1);
        [Fluxes, status] = qp_netcdf_get(FI, varid, FI.Dataset(varid+1).Dimension, idx);
        hold(Parent,'on')
        hNew1 = area(Parent, T, Fluxes(:,:,1));
        hNew2 = area(Parent, T, -Fluxes(:,:,2));
        colours = get_colors(size(Fluxes,2));
        for i = 1:size(Fluxes,2)
            set(hNew1(i),'facecolor', colours(i,:))
            set(hNew2(i),'facecolor', colours(i,:))
        end
        hNew = [hNew1, hNew2];
        legend(protectstring(Props.varid{3}))
        %
        setappdata(Parent,'AxesType','Time-<blocking>')
        setappdata(Parent,'BasicAxesType','Time-<blocking>')
        setappdata(Parent,'xquantity','time')
        tick(Parent,'x','autodate')
        %
        balArea = get_attrib(Info.Attribute,'balance_area');
        quant = get_attrib(Info.Attribute,'balance_quantity');
        set(get(Parent,'title'),'string',balArea,'interpreter','none')
        set(get(Parent,'xlabel'),'string','time \rightarrow')
        setappdata(Parent,'xtickmode','autodate')
        set(get(Parent,'ylabel'),'string',[quant,' (',Props.Units,') \rightarrow'])
        
    otherwise
        ui_message('error','Unknown plot type "%s".', Props.varid{1})
end