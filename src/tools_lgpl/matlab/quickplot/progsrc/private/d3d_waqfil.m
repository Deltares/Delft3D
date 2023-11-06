function varargout=d3d_waqfil(FI,domain,field,cmd,varargin)
%D3D_WAQFIL QP support for Delft3D-WAQ and -PART map and history files.
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
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{1})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,Props,0)};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        if isfield(Props,'BalSubFld')
            % balance plot
            Parent=varargin{1};
            Ops=varargin{2};
            hOld=varargin{3};
            delete([hOld{:}])
            Station=varargin{4};
            [Time,Data]=delwaq('read',FI,Props.BalSubFld{1},Station,0);
            Labels=Props.BalSubFld{2};
            %
            BalAreas=FI.SegmentName;
            if strcmpi(BalAreas{end},'Sum_of_balance_areas')
                BalAreas{end}=[];
            end
            ibal = -1;
            %
            FluxLabels=Labels;
            InOut=zeros(1,length(Labels));
            for i=1:length(Labels)
                if length(Labels{i})>3 && (strcmpi(Labels{i}(end-2:end),'_in') || strcmpi(Labels{i}(end-2:end),' in'))
                    InOut(i)=1;
                    FluxLabels{i}=deblank(FluxLabels{i}(1:end-3));
                elseif length(Labels{i})>4 && (strcmpi(Labels{i}(end-3:end),'_out') || strcmpi(Labels{i}(end-3:end),' out'))
                    InOut(i)=-1;
                    FluxLabels{i}=deblank(FluxLabels{i}(1:end-4));
                else
                    FluxLabels{i}='';
                end
                %
                switch lower(FluxLabels{i})
                    case ''
                        Labels{i}=substdb(Labels{i});
                    case 'all bo+lo'
                        FluxLabels{i} = 'all boundaries and waste loads';
                    case 'waste-loa'
                        FluxLabels{i} = 'waste loads';
                    case {'transp','transport'}
                        FluxLabels{i} = 'other internal areas';
                    case 'other'
                        FluxLabels{i} = 'non-balance areas';
                        ibal = 0;
                    otherwise
                        if ibal>=0
                            if InOut(i)==1
                                ibal=ibal+1;
                                FluxLabels{i} = BalAreas{ibal};
                            else
                                FluxLabels{i} = BalAreas{ibal};
                            end
                        end
                        % Can't expand boundary names
                end
            end
            %
            if isequal(qp_option(FI,'nettransport'),1)
                transin            = InOut ==  1;
                transout           = InOut == -1;
                Data(transin,:,:)  = Data(transin,:,:)+Data(transout,:,:);
                for i = find(transin)
                    Labels{i} = ['Net transport in from ' FluxLabels{i}];
                end
                Labels(transout)   = [];
                Data(transout,:,:) = [];
            else
                for i = 1:length(Labels)
                    if InOut(i)==1
                        Labels{i} = ['Transport in from ' FluxLabels{i}];
                    elseif InOut(i)==-1
                        Labels{i} = ['Transport out to ' FluxLabels{i}];
                    end
                end
            end
            %
            NoContribution=all(Data==0,3);
            Data(NoContribution,:,:)=[];
            Labels(NoContribution)=[];
            %
            hNew=balanceplot(Time,squeeze(Data)','parent',Parent,'color',Ops.colour);
            for i=1:length(Labels)
                Labels{i}=strrep(Labels{i},'_','\_');
            end
            legend(Parent,hNew(end-(0:length(Labels)-1)),Labels,'Location','SouthWest');
            %
            setappdata(Parent,'AxesType','Time-<blocking>')
            setappdata(Parent,'BasicAxesType','Time-<blocking>')
            setappdata(Parent,'xquantity','time')
            tick(Parent,'x','autodate')
            %
            LocationStr=readsts(FI,Props,Station);
            set(get(Parent,'title'),'string',LocationStr,'interpreter','none')
            set(get(Parent,'xlabel'),'string','time \rightarrow')
            setappdata(Parent,'xtickmode','autodate')
            set(get(Parent,'ylabel'),'string',[Props.Name,' (',Props.Units,') \rightarrow'])
        else
            % limiting factors
            Parent=varargin{1};
            Ops=varargin{2};
            hNew=plotlimitingfactors(FI,Parent,varargin(4:end),'color',Ops.colour);
            setappdata(Parent,'AxesType','LimitingFactorsAxes')
            setappdata(getappdata(Parent,'LimitingFactorsAxes'), ...
                'AxesType','LimitingFactorsAxes2')
            colormap(getappdata(Parent,'LimitingFactorsAxes'),'jet')
        end
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
if isfield(FI,'Grid') && isfield(FI.Grid,'FileType') && strcmp(FI.Grid.FileType,'Serafin')
    DataInCell=0;
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);
ValVecReason = 'layers';

subf=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    if varargin{1} > length(Props.SubFld{1})
        Props.Val1 = Props.SubFld{1};
        ValVecReason = 'fraction_sum';
    elseif iscell(Props.SubFld{1})
        Props.Val1=Props.SubFld{1}{varargin{1}};
    else
        Props.Val1=Props.SubFld{1}(varargin{1});
    end
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

% select appropriate timestep ...
sz=getsize(FI,Props);
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
end

% select appropriate spatial indices ...
%================== NEFIS SPECIFIC CODE =======================================
%if DimFlag(M_)&& DimFlag(N_)
%  sz([M_ N_])=sz([N_ M_]);
%  idx([M_ N_])=idx([N_ M_]);
%end
clipZ = qp_option(FI,'clipwherezundefined','default',1);

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
        if (i~=K_) && ~strcmp(subtype,'plot') && ~strcmp(Props.Geom,'POLYG') && ~strcmp(Props.Geom,'UGRID2D-FACE')
            if DataInCell
                if isequal(idx{i},1)
                    idx{i}=[1 2];
                    ind{i}=2;
                elseif idx{i}(1)==1
                    ind{i}=2:length(idx{i});
                else
                    idx{i}=[idx{i}(1)-1 idx{i}];
                    ind{i}=(1:(length(idx{i})-1))+1;
                end
            else % not DataInCell
                if idx{i}(1)==1
                    idx{i}=idx{i};
                    ind{i}=1:length(idx{i});
                else
                    idx{i}=[idx{i}(1) idx{i}];
                    ind{i}=(1:(length(idx{i})-1))+1;
                end
            end
        else % i==K_ || strcmp(subtype,'plot')
            ind{i}=1:length(idx{i});
        end
    end
end
if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end
if isbinary
    if isequal(idx{T_},1:sz(T_))
        % don't change it yet
    elseif ~isequal(size(idx{T_}),[1 1]) && sz(T_)>0
        % error('Can only read one or all timesteps from Delwaq bin file.')
    end
end

% read grid ...
x=[];
y=[];
z=[];
ZUnits='-';
ZQuantity = 'minus layer number';
XUnits='';
%if XYRead || strcmp(subtype,'plot')
%
missingvalue = clip2single(999.999);
%
%======================== SPECIFIC CODE =======================================
switch subtype
    case 'history'
        if Props.DimFlag(ST_)
            stations = idx{ST_};
        else
            stations = idx{M_};
        end
        if isequal(qp_option(FI,'treatas1d'),1)
            x = stations;
        end
        if DimFlag(K_)
            if DataInCell
                idxK_=[idx{K_} idx{K_}(end)+1];
            else
                idxK_=idx{K_};
            end
            cthk=-(0:sz(K_));
            cthk=cthk(idxK_);
            cthk=reshape(cthk,[1 1 length(cthk)]);
            z=repmat(cthk,[1 length(stations) 1]);
        end
    case 'grid'
        x=FI.X(idx{[M_ N_]});
        y=FI.Y(idx{[M_ N_]});
        if DimFlag(K_)
            x=reshape(x,[1 size(x)]);
            y=reshape(y,[1 size(y)]);
            x=repmat(x,[1 1 1 length(idx{K_})+DataInCell]);
            y=repmat(y,[1 1 1 length(idx{K_})+DataInCell]);
        end
    case {'map','plot'}
        switch subtype
            case 'map'
                switch FI.Grid.FileType
                    case 'Serafin'
                        x=FI.Grid.Discr.X(idx{M_});
                        y=FI.Grid.Discr.Y(idx{M_});
                    case 'ESRI-Shape'
                        xy=shape('read',FI.Grid,0,'polyline');
                        MaxNPnt = max(cellfun(@(v)size(v,1), xy));
                        x = NaN(FI.Grid.NShapes,MaxNPnt);
                        y = NaN(FI.Grid.NShapes,MaxNPnt);
                        for i=1:FI.Grid.NShapes
                            NPnt = size(xy{i},1);
                            x(i,1:NPnt) = xy{i}(:,1)';
                            y(i,1:NPnt) = xy{i}(:,2)';
                        end
                        getPnt=ismember(FI.Grid.Index(:,1),idx{M_});
                        if ~all(getPnt)
                            x=x(getPnt,:);
                            y=y(getPnt,:);
                        end
                    case 'netCDF'
                        Ans = [];
                        if isfield(FI.Grid,'Mesh')
                            switch FI.Grid.Mesh{1}
                                case 'ugrid'
                                    if isfield(FI.Grid,'Aggregation') && ~isempty(FI.Grid.Aggregation)
                                        select = {};
                                    else
                                        select = {idx{M_}};
                                    end
                                    [Success,Ans] = qp_getdata(FI.Grid,FI.Grid.Mesh{4},'griddata',select{:});
                                    Ans.ValLocation = Props.Geom(9:end);
                            end
                        else
                            if DataInCell
                                [x, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.BCoordinates{1},FI.Grid.CoordDims);
                                [y, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.BCoordinates{2},FI.Grid.CoordDims);
                            else
                                [x, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.CCoordinates{1},FI.Grid.CoordDims(1));
                                [y, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.CCoordinates{2},FI.Grid.CoordDims(1));
                            end
                        end
                        if ~isempty(x)
                            if isfield(FI.Grid,'Aggregation') && ~isempty(FI.Grid.Aggregation)
                                [agg, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.Aggregation,FI.Grid.AggregationDims);
                                clip = isnan(agg);
                                x(clip,:)=[];
                                y(clip,:)=[];
                                %agg(clip)=[]; % For aggregation, use agg or FI.Grid.Index
                            end
                            x=x(idx{M_},:);
                            y=y(idx{M_},:);
                        elseif ~isempty(Ans)
                            if isfield(FI.Grid,'Aggregation') && ~isempty(FI.Grid.Aggregation)
                                [agg, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.Aggregation,FI.Grid.AggregationDims);
                                agg = agg(:,1); % select one layer
                                clip = isnan(agg);
                                clippedNodes = Ans.FaceNodeConnect(clip,:);
                                Ans.FaceNodeConnect(clip,:)=[];
                                clippedNodes = setdiff(clippedNodes(:),Ans.FaceNodeConnect(:));
                                Ans.X(clippedNodes) = NaN;
                                Ans.Y(clippedNodes) = NaN;
                                %agg(clip)=[]; % For aggregation, use agg or FI.Grid.Index
                            end
                        end
                        if isfield(FI.Grid,'Unit')
                            XUnits = FI.Grid.Unit;
                        end
                    case 'arcgrid'
                        eidx=idx;
                        eidx{M_}=eidx{M_}+1;
                        eidx{N_}=eidx{N_}+1;
                        x=transpose(FI.Grid.XCorner+((1:FI.Grid.NCols+1)-1)*FI.Grid.CellSize(1))*ones(1,FI.Grid.NRows+1);
                        y=ones(FI.Grid.NCols+1,1)*(FI.Grid.YCorner+(FI.Grid.NRows+1)*FI.Grid.CellSize(2)-(1:FI.Grid.NRows+1)*FI.Grid.CellSize(2));
                        x=x(eidx{[M_ N_]});
                        y=y(eidx{[M_ N_]});
                    otherwise
                        if DimFlag(M_) && DimFlag(N_)
                            x=FI.Grid.X(idx{[M_ N_]});
                            y=FI.Grid.Y(idx{[M_ N_]});
                        end
                end
            case 'plot'
                if isbinary
                    X=LocFI.PlotWindow;
                else
                    [X,Chk]=vs_let(LocFI,casemod([DELWAQ '_PARAMS']),casemod('PLOT_WINDOW'),'quiet');
                end
                gidx=idx;
                if DataInCell
                    gidx{M_}=[gidx{M_}(1)-1 gidx{M_}];
                    gidx{N_}=[gidx{N_}(1)-1 gidx{N_}];
                else
                    gidx{M_}=gidx{M_}-0.5;
                    gidx{N_}=gidx{N_}-0.5;
                end
                x=repmat(X(1)+(X(2)-X(1))*(gidx{N_})/sz(N_),length(gidx{M_}),1);
                y=repmat(transpose(X(3)+(X(4)-X(3))*(gidx{M_})/sz(M_)),1,length(gidx{N_}));
        end
        if DimFlag(K_) && ~strcmp(Props.Geom,'POLYG')
            if isbinary
                names=LocFI.SubsName;
            else
                [names,Chk]=vs_get(LocFI,casemod([DELWAQ '_PARAMS']),casemod('SUBST_NAMES'),'quiet');
            end
            ld=strmatch('localdepth',lower(names));
            if iscell(ld)
                if strcmp(subtype,'plot') && (length(ld)~=sz(K_)) && ~isempty(ld)
                    ui_message('warning', ...
                        {sprintf('Number of LocalDepth layers does not match number of layers of %s.',Props.Name), ...
                        'Z coordinate based on LocalDepth disabled.'});
                    ld=[];
                end
            end
            wlflag=strmatch('zcoordwaterlevel',lower(names));
            dpsflag = false;
            if DataInCell
                idxK_=[idx{K_} idx{K_}(end)+1];
            else
                idxK_=idx{K_};
            end
            if isempty(ld)
                cthk=-(0:sz(K_));
                cthk=cthk(idxK_);
                cthk=reshape(cthk,[1 1 1 length(cthk)]);
                if isempty(x)
                    szx = [length(idx{M_}), 1];
                else
                    szx = size(x);
                end
                z=repmat(cthk,[1 szx 1]);
            else
                ZUnits = 'm';
                ZQuantity = 'elevation relative to water level';
                if isbinary
                    if iscell(ld)
                        for k=length(ld):-1:1
                            [T,Tmp]=delwaq('read',LocFI,ld(k),0,idx{T_});
                            % only one substance, so, size(Tmp,1)=1
                            Tmp=permute(Tmp,[3 2 1]);
                            z(:,:,k)=Tmp;
                        end
                    else
                        [T,z]=delwaq('read',LocFI,ld,0,idx{T_});
                        z=permute(z,[3 2 1]);
                    end
                    if ~isempty(wlflag)
                        [T,wl]=delwaq('read',LocFI,wlflag,0,idx{T_});
                        wl=permute(wl,[3 2 1]);
                    elseif isfield(FI,'Grid') && isfield(FI.Grid,'DPS');
                        dpsflag = true;
                        dps = FI.Grid.DPS;
                    end
                else
                    if iscell(ld)
                        for k=length(ld):-1:1
                            [z(:,:,k),Chk]=vs_let(LocFI,casemod([DELWAQ '_RESULTS']),idx(T_), ...
                                casemod(sprintf('SUBST_%3.3i',ld(k))),'quiet');
                        end
                    else
                        [z,Chk]=vs_let(LocFI,casemod([DELWAQ '_RESULTS']),idx(T_), ...
                            casemod(sprintf('SUBST_%3.3i',ld)),'quiet');
                    end
                    if ~isempty(wlflag)
                        [wl,Chk]=vs_let(LocFI,casemod([DELWAQ '_RESULTS']),idx(T_), ...
                            casemod(sprintf('SUBST_%3.3i',wlflag)),'quiet');
                    end
                end
                if isfield(Props,'BedLayer') && Props.BedLayer == -1
                    NoSegFlow=prod(sz([M_ N_ K_]));
                    if size(z,2)>NoSegFlow
                        z=z(:,1:NoSegFlow);
                        if wlflag
                            wl=wl(:,1:NoSegFlow);
                        end
                    end
                end
                %
                z(z==-999)=NaN;
                z(z==missingvalue)=NaN;
                if wlflag
                    wl(wl==-999)=NaN;
                    wl(wl==missingvalue)=NaN;
                end
                if strcmp(subtype,'plot')
                    z=reshape(z,[size(z,1) sz([M_ N_ K_])]);
                    if wlflag
                        wl=reshape(wl,[size(z,1) sz(M_) sz(N_)]);
                    end
                else
                    z=z(:,max(FI.Grid.Index,1));
                    z=reshape(z,[size(z,1) size(FI.Grid.Index)]);
                    nact=FI.Grid.Index<=0;
                    if wlflag
                        wl=wl(:,max(FI.Grid.Index(:,:,1),1));
                        wl=reshape(wl,[size(z,1) size(FI.Grid.Index(:,:,1))]);
                    elseif dpsflag
                        wlflag = true;
                        dps = -dps(max(FI.Grid.Index(:,:,1),1));
                        dps = reshape(dps, [1 size(dps)]);
                        wl = max(z,[],4) + repmat(dps,size(z,1),1);
                    end
                    %
                    if strcmp(subtype,'map')
                        for i=1:size(z,1)
                            z(i,nact)=NaN;
                        end
                    end
                end
                %
                %negdepth=0;
                z0 = z(:,:,:,1);
                for i=2:size(z,4)
                    z1 = z(:,:,:,i);
                    %if any(z1(:)<z0(:))
                    %   negdepth=1;
                    %end
                    z1(z1<z0) = NaN;
                    z0 = max(z0,z1);
                    z(:,:,:,i) = z1;
                end
                %if negdepth
                %   ui_message('warning',{'Cells with negative depth encountered!','These have been removed from the data set.'})
                %end
                %
                %zerodepth=any(z(:)==0);
                %if zerodepth
                %    z(z==0)=NaN;
                %    ui_message('warning',{'Cells with zero depth encountered!','These have been removed from the data set.'})
                %end
                %
                if isempty(wlflag)
                    szz = [size(z) 1];
                    wl = repmat(0,szz([1 2 3]));
                    wl(all(isnan(z(:,:,:,:)),4)) = NaN;
                else
                    ZQuantity = 'elevation';
                end
                if DataInCell
                    kmax = size(z,4);
                    for i = 1:size(z,1)
                        for k = kmax+1:-1:2
                            z(i,:,:,k) = wl(i,:,:) - z(i,:,:,k-1);
                        end
                        z(i,:,:,1) = wl(i,:,:);
                    end
                else
                    for i = 1:size(z,1)
                        for k = size(z,4):-1:2
                            z(i,:,:,k) = wl(i,:,:) - (z(i,:,:,k) + z(i,:,:,k-1))/2;
                        end
                        z(i,:,:,1) = wl(i,:,:) - z(i,:,:,1)/2;
                    end
                end
                %
                if isequal(idx{N_},0) % unstructured data sets
                    z = z(:,idx{M_},1,idxK_);
                else
                    z = z(:,idx{[M_ N_]},idxK_);
                end
            end
            %      cthk=-(0:(FI.Grid.MNK(3)-1));
            %      cthk=cthk(idx{K_});
            x=reshape(x,[1 size(x)]);
            x=repmat(x,[1 1 1 length(idxK_)]);
            %      x=repmat(x,[1 1 1 length(cthk)+DataInCell]);
            y=reshape(y,[1 size(y)]);
            y=repmat(y,[1 1 1 length(idxK_)]);
            %      y=repmat(y,[1 1 1 length(cthk)+DataInCell]);
        end
end
%========================= GENERAL CODE =======================================
x(x==-999) = NaN;
x(x==-missingvalue) = NaN;
%end

% grid interpolation ...
if ~strcmp(subtype,'history') && ~strcmp(Props.Geom,'TRI') && ~strcmp(Props.Geom,'POLYG')
    [x,y]=gridinterp(DataInCell | strcmp(subtype,'plot'),DimFlag(K_),Props.ReqLoc,x,y);
end

% load data ...
%================== NEFIS SPECIFIC CODE =======================================
elidx=idx(2:end);
allt=isbinary && length(idx{T_})>1;
if allt
    allt_=idx{T_};
    %  idx{T_}=0;
end
elidx(~DimFlag(2:end))=[];
mapgrid=isfield(FI,'Grid');
if mapgrid
    mapgrid=~isempty(FI.Grid);
end
TDam=0;
T=[];
if ~isempty(Props.SubFld) && isnumeric(Props.SubFld)
    % for S1/S2 quantities we read only the last layer
    elidx(end+1)={':'};
end
if strcmp(Props.Name,'grid') && ~strcmp(Props.Geom,'POLYG')
    if isequal(FI.FileType,'DelwaqLGA')
        L=FI.Index(:,:,1);
    else
        L=FI.Grid.Index(:,:,1);
    end
    x=x(:,:,1);
    y=y(:,:,1);
    z=z(:,:,1);
    val1=[diff(L,[],1)~=0 ;zeros(1,size(L,2))];
    val1=reshape(val1,[1 size(val1)]);
    val1=val1(1,elidx{:});
    val2=[diff(L,[],2)~=0, zeros(size(L,1),1)];
    val2=reshape(val2,[1 size(val2)]);
    val2=val2(1,elidx{:});
    TDam=1;
    Props.NVal=2;
elseif (strcmp(subtype,'map') && mapgrid) || strcmp(subtype,'plot') || strcmp(subtype,'grid')
    szPerTimestep = cellfun('length',elidx);
    if strcmp(subtype,'plot')
        if DimFlag(K_)
            szV = sz([M_ N_ K_]);
        else
            szV = sz([M_ N_]);
        end
        if isfield(Props,'BedLayer') && Props.BedLayer == 1
            if isfield(FI,'K')
                nlyr = FI.K-1;
            else
                Info = vs_disp(FI,Props.Group,Props.Val1);
                nlyr = Info.SizeDim/prod(szV)-1;
            end
            index = reshape(1:prod(szV),szV)+prod(szV)*max(1,nlyr);
        else
            index = reshape(1:prod(szV),szV);
        end
    else
        if isfield(Props,'BedLayer') && Props.BedLayer == 1
            index = FI.Grid.Index(:,:,1)+sign(FI.Grid.Index(:,:,1))*FI.Grid.NoSeg;
        else
            if mapgrid
                index=FI.Grid.Index;
            else
                index=FI.Index;
            end
            if ~DimFlag(K_) && isempty(Props.SubFld)
                index=index(:,:,1);
            end
        end
    end
    if strcmp(ValVecReason,'layers') && (iscell(Props.Val1) || (isnumeric(Props.Val1) && length(Props.Val1)>1))
        % layers shouldn't be included in index because the segment numbers
        % will be derived from index, and we don't have that many segments
        % per quantity
        index   = index(elidx{1:end-1},1);
        Props.Val1 = Props.Val1(elidx{end});
    else
        index   = index(elidx{:});
    end
    missing = index<=0;
    if any(missing(:))
        mn = min(index(~missing));
        if isempty(mn)
            mn = 1;
        end
        index(missing) = mn;
    end
    %
    [seg,ia,ic] = unique(index);
    if strcmp(ValVecReason,'layers') && (iscell(Props.Val1) || (isnumeric(Props.Val1) && length(Props.Val1)>1))
        nlyr = length(Props.Val1);
        missing = repmat(missing,[ones(1,length(elidx)-1) nlyr]);
        numic = length(ic);
        maxic = max(ic);
        for k = nlyr:-1:2
            ic(numic*(k-1)+(1:numic)) = ic(1:numic)+maxic*(k-1);
        end
    end
    %
    val2=[];
    if isempty(Props.Val1)
        %
        % Requesting segment number
        %
        val1=[];
    elseif strcmp(subtype,'grid')
        %
        % Delwaq Vol, Flux, ... file
        % Supports current only a single time step per read
        %
        val1=waqfil('read',FI.Attributes.(Props.Val1),idx{T_},Props.AttPar{:})';
        val1=val1(seg); % all segments have been read, extract segments needed
        if strcmp(Props.Name,'bed level')
            val1 = -val1;
        end
    elseif strcmp(ValVecReason,'layers') && iscell(Props.Val1) && DimFlag(K_)
        %
        % Plot grid or non-aggregated map grid with multiple layers stored in
        % different substances.
        %
        val1=zeros([length(idx{T_}) szPerTimestep]);
        szV = size(val1);
        for s = 1:length(Props.Val1)
            [Tmp,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1{s},{seg},'quiet');
            val1(:,:,:,s)=reshape(Tmp,szV(1:3));
        end
    else
        if isbinary
            [T,val1]=delwaq('read',LocFI,Props.Val1,seg,idx{T_});
            if length(Props.Val1)>1 && strcmp(ValVecReason,'fraction_sum')
                val1 = sum(val,1);
            end
            val1=permute(val1,[3 2 1]);
        elseif iscell(Props.Val1)
            for ip = 1:length(Props.Val1)
                [val1p,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1{ip},{seg},'quiet');
                if ip == 1
                    val1 = val1p;
                else
                    val1 = val1 + val1p;
                end
            end
        else
            [val1,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1,{seg},'quiet');
        end
        if ~isempty(Props.Val2)
            if isbinary
                [T,val2]=delwaq('read',LocFI,Props.Val2,seg,idx{T_});
                val2=permute(val2,[3 2 1]);
            else
                [val2,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val2,{seg},'quiet');
            end
        end
    end
    %
    if isempty(val1) % segment number
        val1=reshape(index,[1 szPerTimestep]);
        val1(:,missing) = NaN;
    else
        val1 = val1(:,ic);
        val1(:,missing) = NaN;
        if size(val1,2) ~= prod(szPerTimestep)
            nTimes = size(val1,1);
            nLayers = size(val1,2)/prod(szPerTimestep);
            val1 = reshape(val1,[nTimes*prod(szPerTimestep) nLayers]);
            val1(val1==-999) = 0;
            bed2d_method = 'last_layer_non_zero';
            switch bed2d_method
                case 'last_layer_non_zero'
                    for k = nLayers:-1:1
                        if k==nLayers
                            needsval = val1(:,k)==0;
                        else
                            getval = val1(:,k)~=0 & needsval;
                            if any(getval)
                                val1(getval,nLayers) = val1(getval,k);
                                needsval(getval) = false;
                            end
                        end
                    end
                    val1 = val1(:,nLayers);
                case 'sum_all_layers'
                    val1 = sum(val1,2);
                case 'last_layer'
                    val1 = val1(:,nLayers);
            end
            val1 = reshape(val1,[nTimes prod(szPerTimestep)]);
        end
        val1 = reshape(val1,[size(val1,1) szPerTimestep]);
        if ~isempty(val2)
            val2 = val2(:,ic);
            val2(:,missing) = NaN;
            val2 = reshape(val2,[size(val2,1) szPerTimestep]);
        end
    end
else
    st_ = ST_;
    if DimFlag(M_) && ~DimFlag(N_)
        st_ = M_;
    end
    if isbinary
        if DimFlag(K_)
            ival = Props.Val1(idx{K_});
        else
            ival = Props.Val1;
        end
        [T,val1]=delwaq('read',LocFI,ival,idx{st_},idx{T_});
        if length(Props.Val1)>1 && strcmp(ValVecReason,'fraction_sum')
            val1 = sum(val,1);
        end
        val1=permute(val1,[3 2 1]);
    else
        if strcmp(ValVecReason,'layers') && iscell(Props.Val1)
            %
            % Multiple layers stored in different substances.
            %
            val1=zeros([length(idx{T_}) length(idx{st_}) length(idx{K_})]);
            i_s = 0;
            for s=idx{K_}
                i_s = i_s+1;
                [val1(:,:,i_s),Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1{s},idx(st_),'quiet'); % load station
            end
        elseif iscell(Props.Val1)
            for ip = 1:length(Props.Val1)
                [val1p,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1{ip},idx(st_),'quiet'); % load station
                if ip == 1
                    val1 = val1p;
                else
                    val1 = val1 + val1p;
                end
            end
        else
            [val1,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1,idx(st_),'quiet'); % load station
        end
    end
    if isempty(Props.Val2)
        val2=[];
    else
        if isbinary
            [T,val2]=delwaq('read',LocFI,Props.Val2,idx{st_},idx{T_});
            val1=permute(val1,[3 2 1]);
        else
            [val2,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val2,idx(st_),'quiet'); % load station
        end
    end
end
if allt
    idx{T_}=allt_;
end
if ~isempty(val1)
    if ~isempty(z) && clipZ
        if DataInCell
            val1(isnan(z(:,:,:,2:end)))=NaN;
        else
            val1(isnan(z))=NaN;
        end
    end
    val1(val1==-999)=NaN;
    val1(val1==missingvalue)=NaN;
end
if ~isempty(val2)
    if ~isempty(z) && clipZ
        if DataInCell
            val2(isnan(z(:,:,:,2:end)))=NaN;
        else
            val2(isnan(z))=NaN;
        end
    end
    val2(val2==-999)=NaN;
    val2(val2==missingvalue)=NaN;
end

if DataInCell && isequal(Props.ReqLoc,'d')
    Props.ReqLoc='z';
end
% combine vectors components ...
if isequal(Props.VecType,'m')
    [val1,val2]=dir2uv(val1,val2);
end
% data interpolation ...
if isequal(Props.Loc,'d') && isequal(Props.ReqLoc,'z')
    val1=interp2cen(val1,'t');
    if ~isempty(val2)
        val2=interp2cen(val2,'t');
    end
elseif isequal(Props.Loc,'u') && isequal(Props.ReqLoc,'z')
    [val1,val2]=uv2cen(val1,val2);
end
% combine vectors components ...
if isequal(Props.VecType,'u') && Props.MNK<=1
    % rotate n,m components into x,y direction ...
    error('Alfa rotation not available for Delwaq files...');
end

%======================== SPECIFIC CODE =======================================
% select active points ...
act_from_z = 0;
if strcmp(subtype,'map') && mapgrid && ~strcmp(Props.Geom,'TRI') && ~strcmp(Props.Geom,'POLYG') && ~strcmp(Props.Geom,'UGRID2D-FACE') && ~strcmp(Props.Geom,'UGRID2D-NODE') && ~strcmp(Props.Geom,'UGRID1D-NODE')
    act=FI.Grid.Index(idx{[M_ N_]},1)~=0;
    gridact=~isnan(x(:,:,:,1));
elseif strcmp(subtype,'plot') && ~isempty(z)
    act_from_z = 1;
    act=~isnan(z(:,:,:,1));
    gridact=1;
else
    act=1;
    gridact=1;
end
%========================= GENERAL CODE =======================================
if XYRead && ~strcmp(subtype,'history') && ~isempty(x)
    if DimFlag(K_)
        if ~isequal(gridact,1)
            szx=[size(x) 1]; % extent szx for the case that dataset in K dir. is 1
            szx1=szx([1:2 4:end]);
            szx1(2)=szx(2)*szx(3);
            x=reshape(x,szx1);
            x(:,gridact~=1,:)=NaN;
            x=reshape(x,szx);
            y=reshape(y,szx1);
            y(:,gridact~=1,:)=NaN;
            y=reshape(y,szx);
        end
        %---
        if ~act_from_z
            szz=[size(z) 1]; % extent szx for the case that dataset in K dir. is 1
            szz1=szz([1:2 4:end]);
            szz1(2)=szz(2)*szz(3);
            z=reshape(z,szz1);
            z(:,act~=1,:)=NaN;
            z=reshape(z,szz);
        end
    else
        x(gridact~=1)=NaN;
        y(gridact~=1)=NaN;
    end
end
if Props.NVal>0 && ~TDam && ~strcmp(subtype,'history')
    %  if strcmp(subtype,'plot')
    %    act=permute(act,[1 3 2]);
    %  end
    szz=[size(val1) 1]; % extent szx for the case that dataset in K dir. is 1
    if act_from_z
        szz1=szz([1 4:end]);
        szz1(1)=szz(1)*szz(2)*szz(3);
        if length(szz1)<2
            szz1(1,2)=1;
        end
        val1=reshape(val1,szz1);
        val1(act~=1,:)=NaN;
    else
        szz1=szz([1:2 4:end]);
        szz1(2)=szz(2)*szz(3);
        val1=reshape(val1,szz1);
        val1(:,act~=1,:)=NaN;
    end
    val1=reshape(val1,szz);
    if ~isempty(val2)
        val2(:,isnan(val1))=NaN;
    end
end

% read time ...
if DimFlag(T_) && isempty(T)
    T=readtim(FI,Props,idx{T_});
end

% select subrange if necessary ... M,N,K only
DimMask=[0 0 1 1 1];
if DataInCell
    for i=[M_ N_ K_]
        if DimFlag(i)
            allidx(i)=0;
        end
    end
end
if DimFlag(K_) && isempty(z)
    if DataInCell
        k = idx{K_};
        k = -[k(1)-1;k(:)];
    else
        k = -idx{K_}+0.5;
    end
    z = reshape(k,[1 1 1 length(k)]);
    szx = [size(x) 1];
    z = repmat(z,[szx(1:3) 1]); % for all time, M and N
end
if ~all(allidx(DimMask & DimFlag))
    if XYRead
        if DataInCell
            if DimFlag(M_) && DimFlag(N_) && DimFlag(K_)
                z=z(:,ind{[M_ N_]},:);
            end
        else
            if DimFlag(M_) && DimFlag(N_)
                ii=ind([M_ N_]);
            elseif DimFlag(M_)
                ii={ind{M_} 1};
            else
                ii={':' ':'};
            end
            jj = {':' ii{:} ':'};
            if ~DimFlag(K_)
                jj(1) = [];
                jj(end) = [];
            end
            x=x(jj{:});
            y=y(jj{:});
            if DimFlag(K_)
                z=z(jj{:});
            end
        end
    end
    % DimMask=[0 1 1 1 1];
    ind=ind(DimMask & DimFlag);
    if Props.NVal==1
        val1=val1(:,ind{:});
    elseif Props.NVal==2
        val1=val1(:,ind{:});
        val2=val2(:,ind{:});
    end
end
%========================= GENERAL CODE =======================================

% reshape if a single station is selected ...
if DimFlag(ST_) && length(idx{ST_})==1
    sz=[size(val1) 1]; sz(2)=[];
    if isempty(val2)
        val1=reshape(val1,sz);
    else
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
    if DimFlag(K_)
        sz=[size(z) 1]; sz(2)=[];
        z=reshape(z,sz);
    end
end

% reshape if a single timestep is selected ...
if (~DimFlag(T_) || (DimFlag(T_) && isequal(size(idx{T_}),[1 1]))) && ~strcmp(Props.Geom,'TRI')
    sz=size(val1); sz=[sz(2:end) 1];
    if Props.NVal==1
        val1=reshape(val1,sz);
    elseif Props.NVal==2
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
    if DimFlag(K_) && ~strcmp(Props.Geom,'POLYG')
        if ~isempty(x)
            sz=size(x); sz=[sz(2:end) 1];
            x=reshape(x,sz);
            if ~isempty(y)
                y=reshape(y,sz);
            end
        end
        if ~isempty(z) && DimFlag(K_)
            sz=size(z); sz=[sz(2:end) 1];
            z=reshape(z,sz);
        end
    end
end

% generate output ...
if XYRead
    switch Props.Geom
        case {'UGRID2D-FACE','UGRID2D-EDGE','UGRID2D-NODE'}
            % Ans already filled with geometry information except for
            % z coordinates
            if DimFlag(K_)
                Ans.ZName = ZQuantity;
                Ans.ZUnits = ZUnits;
                Ans.ZLocation = Ans.ValLocation;
                Ans.Z = z;
            end
        case 'POLYG'
            if size(x,2)==2 % segments
                x(:,end+1) = NaN;
                y(:,end+1) = NaN;
            else % polygons
                x(:,end+1:end+2) = NaN;
                y(:,end+1:end+2) = NaN;
                % close all polygons
                for j=1:size(x,1)
                    for k=1:size(x,2)
                        if isnan(x(j,k))
                            x(j,k) = x(j,1);
                            y(j,k) = y(j,1);
                            break
                        end
                    end
                end
            end
            if size(val1,2)>1
                val1 = reshape(val1,[size(val1,1) 1 size(val1,2)]);
            end
            val1 = repmat(val1,1,size(x,2));
            val1 = permute(val1,[2 1 3]);
            val1 = reshape(val1,[size(val1,1)*size(val1,2) size(val1,3)]);
            x = x';
            Ans.X = x(:);
            y = y';
            Ans.Y = y(:);
        case 'TRI'
            Ans.TRI=FI.Grid.Discr.Elem;
            if size(x,1)>1 %2D
                x=reshape(x,[1 size(x)]);
                y=reshape(y,[1 size(y)]);
            end
            if size(z,1)>1
                x=repmat(x,[size(z,1) 1 1 1]);
                y=repmat(y,[size(z,1) 1 1 1]);
            end
            Ans.XYZ=cat(5,x,y,z);
            szXYZ=size(Ans.XYZ);
            Ans.XYZ=reshape(Ans.XYZ,szXYZ([1 2 4 5]));
        otherwise
            if ~isempty(x)
                Ans.X=x;
            end
            if ~isempty(y)
                Ans.Y=y;
            end
            if DimFlag(K_)
                Ans.ZName = ZQuantity;
                Ans.ZUnits = ZUnits;
                Ans.Z = z;
            end
    end
    %
    if ~isempty(XUnits)
        Ans.XUnits = XUnits;
        Ans.YUnits = XUnits;
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    if TDam
        Ans.XDam=val1;
        Ans.YDam=val2;
    else
        Ans.XComp=val1;
        Ans.YComp=val2;
    end
end

% read time ...
if DimFlag(T_)
    Ans.Time=T;
end

% create updated data info
if isfield(FI,'DwqBin')
    FI.DwqBin=LocFI;
elseif isfield(FI,'Nfs')
    FI.Nfs=LocFI;
else
    FI=LocFI;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
PropNames={'Name'                   'Units' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'     'Val2'    'SubFld' 'MNK' 'ClosedPoly'};
if isbinary
    Type=lower(LocFI.FileType);
else
    Type=lower(LocFI.SubType);
end
plotfile=0;
includegrid=1;
enablegridview=1;
mass_per='n/a';
reducebedto2d = qp_option(FI,'reducebedto2d','default',1);
reducedptstatto2d = qp_option(FI,'reducedptstatto2d','default',1);
switch Type
    case {'delwaqlga'}
        DataProps={'grid'          ''     '' 'xy'     [0 0 1 1 0]  0         0     ''       'd'   'd'       ''      ''               ''              ''    []       0 0
            'segment number'       ''     '' 'xy'     [0 0 1 1 0]  1         1     ''       'z'   'z'       'c'     ''               ''              ''    []       0 0
            '-------'              ''     '' ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''              ''    []       0 0};
        Out=cell2struct(DataProps,PropNames,2);
        if FI.MNK(3)>1
            Out(2).DimFlag(K_) = 1;
            Out(2).Coords(end+(1:2))='+z';
        end
        if isfield(FI,'Attributes') && isstruct(FI.Attributes)
            [Out(:).AttPar] = deal({});
            flds = fieldnames(FI.Attributes);
            for i = 1:length(flds)
                Out(end+1) = Out(2);
                Out(end).Val1 = flds{i};
                name = flds{i};
                units = '';
                switch name
                    case 'vol'
                        Out(end).DimFlag(T_) = 5;
                        name = 'water volumes';
                        units = 'm^3';
                    case 'sal'
                        Out(end).DimFlag(T_) = 5;
                        name = 'salinity';
                        units = 'ppt';
                    case 'tem'
                        Out(end).DimFlag(T_) = 5;
                        name = 'temperature';
                        units = '°C';
                    case 'vdf'
                        Out(end).DimFlag(T_) = 5;
                        name = 'vertical diffusivity';
                        units = 'm^2/s';
                    case 'tau'
                        Out(end).DimFlag(T_) = 5;
                        name = 'bed shear stress';
                        units = 'N/m^2';
                    case 'are'
                        Out(end).DimFlag(T_) = 5;
                        name = 'flow areas';
                        units = 'm^2';
                    case 'flo'
                        Out(end).DimFlag(T_) = 5;
                        name = 'fluxes';
                        units = 'm^3/s';
                    case 'poi'
                        name = 'segment indices';
                    case 'len'
                        name = 'lengths';
                        units = 'm';
                    case 'chz'
                        name = 'chezy - direction 1';
                        units = 'm^{1/2}/s';
                        Out(end).Name = name;
                        Out(end).Units = units;
                        Out(end).DimFlag(K_) = 0;
                        Out(end).AttPar = {1};
                        Out(end+1) = Out(end);
                        %
                        name = 'chezy - direction 2';
                        units = 'm^{1/2}/s';
                        Out(end).AttPar = {2};
                    case {'srf','srfold'}
                        if isfield(FI.Attributes.(name),'Srf')
                            Out(end).DimFlag(K_) = 0;
                        else
                            Out(end).DimFlag(T_) = 5;
                        end
                        if strcmp(name,'srfold')
                            name = 'surface areas (old format)';
                        else
                            name = 'surface areas';
                        end
                        units = 'm^2';
                    case 'dps'
                        name = 'bed level';
                        units = 'm';
                        Out(end).DimFlag(K_) = 0;
                end
                Out(end).Name = name;
                Out(end).Units = units;
            end
        end
        [Out.UseGrid]=deal(1);
        return
    case {'delft3d-waq-history','delwaqhis','delft3d-par-his','delft3d-par-history'}
        DataProps={'--constituents'       ''      '' ''    [1 5 0 0 0]  0         1     ''       'z'   'z'       'c'     casemod('DELWAQ_RESULTS') casemod('SUBST_001')     ''    []       0 0};
        if isequal(qp_option(FI,'treatas1d'),1)
            DataProps{5}=[1 0 1 0 0];
        end
        mass_per='cell';
    case {'delft3d-par-detailed_plot','delparplot','delft3d-par-plot','delft3d-par-psf'}
        plotfile=1;
        DataProps={'--constituents'       ''      '' 'xy'    [1 0 1 1 0]  1         1     ''       'z'   'z'       'c'     casemod('DELPAR_RESULTS') casemod('SUBST_001')     ''    []       0 0};
        mass_per='cell';
    otherwise
        DataProps={'segment number'       ''     '' 'xy'     [0 5 0 0 0]  1         1     ''       'z'   'z'       'c'     ''               ''              ''    []       0 0
            '-------'              ''     '' ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''              ''    []       0 0
            '--constituents'       ''     '' 'xy'     [1 5 0 0 0]  1         1     ''       'z'   'z'       'c'     casemod('DELWAQ_RESULTS') casemod('SUBST_001')     ''    []       0 0};
        mass_per='cell';
        if isfield(FI,'Grid') && ~isempty(FI.Grid)
            for r = [1 3]
                DataProps{r,5}(ST_)=0;
                DataProps{r,5}(M_)=1;
                DataProps{r,5}(N_)=1;
                if isfield(FI.Grid,'FileType')
                    switch FI.Grid.FileType
                        case {'Serafin'} % triangular
                            includegrid=0;
                            enablegridview=0;
                            %
                            DataProps{r,3}='TRI';
                            DataProps{r,5}(M_)=6;
                            DataProps{r,5}(N_)=0;
                            %DataProps{r,6}=0;
                        case {'netCDF','ESRI-Shape'} % polygon bounds
                            enablegridview=0;
                            %
                            if isfield(FI.Grid,'Mesh')
                                switch FI.Grid.Mesh{3}
                                    case 0
                                        imesh = FI.Grid.Mesh{2};
                                        nMeshDims = FI.Grid.Dataset(imesh).Mesh{2};
                                        switch nMeshDims
                                            case 1
                                                DataProps{r,3}='UGRID1D-NODE';
                                            case 2
                                                DataProps{r,3}='UGRID2D-NODE';
                                        end
                                    case 2
                                        DataProps{r,3}='UGRID2D-FACE';
                                end
                            else
                                DataProps{r,3}='POLYG';
                            end
                            DataProps{r,5}(M_)=6;
                            DataProps{r,5}(N_)=0;
                            DataProps{r,6}=1;
                            DataProps{r,end}=1; %closedpoly
                    end
                end
                if length(FI.Grid.MNK)>2 && FI.Grid.MNK(3)>1
                    DataProps{r,4}(end+(1:2))='+z';
                    DataProps{r,5}(K_)=1;
                    DataProps{r,5}(T_)=1; % in case of 3D data set, Z may be time dependent and hence "segment number" will be time dependent
                end
            end
        else % map as his
            DataProps=DataProps(3,:);
        end
end
Out=cell2struct(DataProps,PropNames,2);
if ~isempty(DELWAQ)
    [Out.Group]=deal(casemod([DELWAQ '_RESULTS']));
end
if isequal(mass_per,'cell')
    if isfield(FI,'DwqBin')
        Header = FI.DwqBin.Header;
    elseif isfield(FI,'Nfs')
        Header = vs_get(FI.Nfs,casemod([DELWAQ '_PARAMS']),casemod('TITLE'),'quiet');
    else
        Header = '';
    end
    if ~isempty(Header) && strcmpi(Header(3,34:40),'mass/m2')
        mass_per='m2';
    end
end
%======================== SPECIFIC CODE REMOVE ================================
%for i=size(Out,1):-1:1
%  Info=vs_disp(LocFI,Out(i).Group,Out(i).Val1);
%  if ~isstruct(Info)
%    % remove references to non-stored data fields
%    Out(i,:)=[];
%  elseif isequal(Info.SizeDim,1)
%    % remove references to non-stored data fields
%    Out(i,:)=[];
%  elseif strcmp(Out(i).Name,'epsilon'),
%    % turbulent kinetic energy automatically caught by previous case
%    % epsilon only present as second field in RTUR1: RTUR1(:,:,:,2)
%    if ~isequal(Info.SizeDim(4),2)
%      Out(i,:)=[];
%    end
%  end
%end
%======================== SPECIFIC CODE ADD ===================================
icnst=strmatch('--constituents',{Out.Name});
ii=0;
FI = qp_option(FI,'balancefile','ifnew',0);
FI = qp_option(FI,'showfractions','ifnew','subfield');
minlen=20;
if ~isempty(icnst)
    [Out.BedLayer]=deal(0);
    [Out.ShortName]=deal('');
    [Out.SubsGrp]=deal('');
    %
    if isbinary
        names=LocFI.SubsName;
    else
        [names,Chk]=vs_get(LocFI,casemod([DELWAQ '_PARAMS']),casemod('SUBST_NAMES'),'quiet');
    end
    if qp_option(FI,'balancefile')
        if iscell(names)
            names=strvcat(names{:});
        end
        if size(names,2)>7 && sum(names(:,7)=='_')>size(names,1)/2
            minlen = 6;
            nn1 = names(:,1:6);
            nn2 = cellstr(names(:,8:end));
        elseif size(names,2)>11
            minlen = 10;
            nn1 = names(:,1:10);
            nn2 = cellstr(names(:,11:end));
        else
            nn1 = names;
            nn2 = names(:,[]);
        end
        [names,dummy,index]=unique(nn1,'rows');
        names=cellstr(names);
        Ins=Out(icnst*ones(length(names),1));
        for j=1:length(names)
            sub = find(index==j);
            Ins(j).Name=names{j};
            Ins(j).Val1=sub(1);
            if length(sub)==1
                % normal history
            else
                % actual balance
                %Ins(j).SubFld={sub nn2(sub)};
                Ins(j).BalSubFld={sub nn2(sub)};
                Ins(j).NVal=-1;
                Ins(j).DimFlag(1)=0;
            end
        end
    else
        names=cellstr(names);
        Ins=Out(icnst*ones(length(names),1));
        for j=1:length(names)
            Ins(j).Name=names{j};
            Ins(j).Val1=j;
        end
    end
    j=0;
    KnownMultilayer=0;
    if isfield(LocFI,'K')
        KnownMultilayer=LocFI.K>1;
    elseif ~isbinary
        InfoPLOT=vs_disp(LocFI,casemod('PLO-VERSION'),[]);
        InfoPSF=vs_disp(LocFI,casemod('PSF-VERSION'),[]);
        if isstruct(InfoPLOT) || isstruct(InfoPSF)
            [PloDimensions,Chk]=vs_get(LocFI,casemod('DELPAR_PARAMS'),casemod('SIZES'),'quiet');
            KnownMultilayer=PloDimensions(4)>1;
        end
    end
    while j<length(Ins)
        j=j+1;
        if strcmp(Ins(j).Name,'*already processed*')
            % skip it
        elseif plotfile && KnownMultilayer
            Ins(j).DimFlag=[1 0 1 1 1];
            if isbinary
                Ins(j).Val1=j+ii;
            else
                Ins(j).Val1=casemod(sprintf('SUBST_%3.3i',j+ii));
            end
        else%if plotfile
            if length(Ins(j).Name)>17 && strcmp(Ins(j).Name(15:17),'lay')
                % j will be 1
                nm=Ins(j).Name(1:14);
                nmidx=1:14;
                lay=Ins(j).Name(18:20);
                nsubplo=1;
                while j+nsubplo<=length(Ins) && length(Ins(j+nsubplo).Name)>=20 && strcmp(Ins(j+nsubplo).Name(18:20),lay)
                    nsubplo=nsubplo+1;
                end
                nlayplo=1;
                while j+nsubplo<=length(Ins) && strcmp(Ins(j+nsubplo).Name(nmidx),nm(nmidx))
                    j=j+nsubplo;
                    nlayplo=nlayplo+1;
                end
                j=j-nsubplo*(nlayplo-1);
                % j will again be 1
                for jj=0:nsubplo-1
                    Ins(j+jj).Name=deblank(Ins(j+jj).Name(1:14));
                    %Ins(j+jj).DimFlag(5)=1;
                    Ins(j+jj).DimFlag=[1 0 1 1 1];
                    if isbinary
                        Ins(j+jj).Val1=j+ii+jj+(0:nlayplo-1)*nsubplo;
                    else
                        Ins(j+jj).Val1={};
                        for s=0:nlayplo-1,
                            Ins(j+jj).Val1{s+1}=casemod(sprintf('SUBST_%3.3i',j+ii+jj+s*nsubplo));
                        end
                    end
                end
                rmidx=j-1+nsubplo+(1:(nsubplo*(nlayplo-1)));
                if any(rmidx>length(Ins))
                    nmiss=sum(rmidx>length(Ins)); % nmiss <= nsubplo-1
                    for jj=nsubplo-nmiss:nsubplo-1
                        Ins(j+jj).Val1(end)=[];
                    end
                    rmidx(rmidx>length(Ins))=[];
                end
                Ins(rmidx)=[];
                j=j+nsubplo-1;
                ii=ii+length(rmidx);
            elseif length(Ins(j).Name)>8 && strcmp(Ins(j).Name(6:8),'_of')
                % j will be 1
                if length(Ins(j).Name)>10
                    nm=[Ins(j).Name(1:5) '[...]' Ins(j).Name(11:end)];
                    nmidx=[1:5 11:length(Ins(j).Name)];
                else
                    nm=Ins(j).Name(1:5);
                    nmidx=1:5;
                end
                lay=Ins(j).Name(9:10);
                nsubplo=1;
                while j+nsubplo<=length(Ins) && length(Ins(j+nsubplo).Name)>=10 && strcmp(Ins(j+nsubplo).Name(9:10),lay)
                    nsubplo=nsubplo+1;
                end
                nlayplo=1;
                while j+nsubplo<=length(Ins) && strcmp(Ins(j+nsubplo).Name(nmidx),nm(nmidx))
                    j=j+nsubplo;
                    nlayplo=nlayplo+1;
                end
                j=j-nsubplo*(nlayplo-1);
                % j will again be 1
                for jj=0:nsubplo-1
                    if length(Ins(j+jj).Name)>10
                        nm=[Ins(j).Name(1:5) '[...]' Ins(j).Name(11:end)];
                    else
                        nm=Ins(j).Name(1:5);
                    end
                    Ins(j+jj).Name=deblank(nm);
                    %
                    %DF = Ins(j+jj).DimFlag;
                    %DF(K_) = 1;
                    %Ins(j+jj).DimFlag=DF;
                    Ins(j+jj).DimFlag(K_)=1;
                    %
                    if isbinary
                        Ins(j+jj).Val1=j+ii+jj+(0:nlayplo-1)*nsubplo;
                    else
                        Ins(j+jj).Val1={};
                        for s=0:nlayplo-1,
                            Ins(j+jj).Val1{s+1}=casemod(sprintf('SUBST_%3.3i',j+ii+jj+s*nsubplo));
                        end
                    end
                end
                rmidx=j-1+nsubplo+(1:(nsubplo*(nlayplo-1)));
                if any(rmidx>length(Ins))
                    nmiss=sum(rmidx>length(Ins)); % nmiss <= nsubplo-1
                    for jj=nsubplo-nmiss:nsubplo-1
                        Ins(j+jj).Val1(end)=[];
                    end
                    rmidx(rmidx>length(Ins))=[];
                end
                Ins(rmidx)=[];
                j=j+nsubplo-1;
                ii=ii+length(rmidx);
            elseif plotfile
                Ins(j).DimFlag(K_)=0;
                if isbinary
                    Ins(j).Val1=j+ii;
                else
                    Ins(j).Val1=casemod(sprintf('SUBST_%3.3i',j+ii));
                end
            elseif strcmp(subtype,'history') && length(Ins(j).Name)>3 && strcmp(Ins(j).Name(end-2:end),'_01')
                idx = j;
                k = 2;
                for jj = j+1:length(Ins)
                    CheckName = sprintf('%s_%2.2i',Ins(j).Name(1:end-3),k);
                    if strcmp(Ins(jj).Name,CheckName)
                        idx = [idx jj];
                        k = k+1;
                    end
                end
                k = k-1;
                if isbinary
                    Ins(j).Val1=idx;
                else
                    Ins(j).Val1={};
                    for s=1:length(idx)
                        Ins(j).Val1{s}=casemod(sprintf('SUBST_%3.3i',idx(s)));
                    end
                end
                Ins(j).DimFlag = [1 5 0 0 1];
                for jj=idx(2:end)
                    Ins(jj).Name='*already processed*';
                end
                Ins(j).Name = Ins(j).Name(1:end-3);
            elseif isbinary
                j0 = Ins(j).Val1;
                Ins(j).Val1=j0+ii;
            else
                Ins(j).Val1=casemod(sprintf('SUBST_%3.3i',j+ii));
            end
        end
    end
    Ins(strmatch('*already processed*',{Ins.Name}))=[];
    %
    [Ins(:).BedLayer]=deal(0);
    if strcmp(subtype,'map')
        bedlayer = 0;
        if isfield(FI.Grid,'NoSegPerLayer')
            noseg_ifbedlayer = FI.Grid.NoSegPerLayer*(FI.Grid.MNK(3)+1);
        else % e.g. in case of Telemac
            noseg_ifbedlayer = prod(FI.Grid.MNK+[0 0 1]);
        end
        if isbinary
            bedlayer = FI.DwqBin.NumSegm==noseg_ifbedlayer;
        else
            Info = vs_disp(FI.Nfs,casemod([DELWAQ '_RESULTS']),casemod('SUBST_001'));
            bedlayer = Info.SizeDim==noseg_ifbedlayer;
        end
        if bedlayer
            nVal=length(Ins);
            Ins=cat(1,Ins,Ins);
            for j=2*nVal:-1:nVal+1
                if strcmpi(Ins(j).Name,'LocalDepth')
                    Ins(j,:)=[];
                else
                    Ins(j).BedLayer=1;
                    Ins(j).DimFlag(K_)=0;
                end
            end
        end
    elseif strcmp(subtype,'plot')
        iLD=strcmpi('LocalDepth',names);
        if any(iLD) && Ins(iLD).DimFlag(K_)
            iLD = find(iLD);
            %
            if isbinary
                if isfield(FI,'K')
                    KMAX = FI.K;
                else
                    KMAX = length(Props.Val1);
                end
                %
                NoSegPerLayer = prod(FI.GridSize);
                LastLayer = NoSegPerLayer*(KMAX-1)+(1:NoSegPerLayer);
                [T,val1]=delwaq('read',LocFI,iLD,0,1);
            else
                NoSegPerLayer = prod(PloDimensions(5:6));
                LastLayer = NoSegPerLayer*(PloDimensions(4)-1)+(1:NoSegPerLayer);
                [val1,Chk]=vs_let(LocFI,casemod(Ins(iLD).Group),{1},casemod(Ins(iLD).Val1),'quiet'); % load all
            end
            SecLastLayer = LastLayer - NoSegPerLayer;
            %
            missingvalue = double(single(999.999));
            %
            vLast = val1(LastLayer);
            vLast(vLast == missingvalue) = 0;
            vLast2 = val1(SecLastLayer);
            vLast2(vLast2 == missingvalue) = 0;
            if all(vLast==0) && any(vLast2~=0)
                nVal=length(Ins);
                [Ins(:).BedLayer]=deal(-1);
                Ins=cat(1,Ins,Ins);
                for j=2*nVal:-1:nVal+1
                    if strcmpi(Ins(j).Name,'LocalDepth')
                        Ins(j,:)=[];
                    else
                        Ins(j).BedLayer=1;
                        Ins(j).DimFlag(K_)=0;
                    end
                end
            end
        end
    end
    %
    showfractions = qp_option(FI,'showfractions');
    %
    % check whether substance names were expanded by adding 001,002,...
    % the following algorithm requires fractions to be numbered
    % consecutively. For this puerpose we sort the entries first.
    %
    [sorted_names,reorder] = sort({Ins.Name}');
    Ins = Ins(reorder);
    %
    names = cellstr(substdb);
    onenames = names(wildstrmatch('*01',names));
    j=1;
    while j<=length(Ins)
        nm = Ins(j).Name;
        if length(nm)<3 % name to too short for name expansion
            j=j+1;
            continue
        end
        if strcmp(nm(end-1:end),'01') % do the last two characters match '01'? [two digits if there are less than 100 fractions)
            n=2;
            f='%s%2.2d';
            if strcmp(nm(end-2:end),'001') % do the last three characters match '001'? (three digits if there are more than 99 fractions)
                n=3;
                f='%s%3.3d';
            end
        else % the last two characters are not '01'
            j=j+1;
            continue
        end
        if any(strcmpi(nm,onenames)) % there exists a name in the process definition file that matches the full name (no name expansion)
            j=j+1;
            continue
        end
        % determine first (j) and last (k) fraction entries
        k = j+1;
        while 1
            nm2 = sprintf(f, nm(1:end-n), k-j+1);
            if k>length(Ins) || ~strcmp(Ins(k).Name, nm2)
                k = k-1;
                break
            elseif k == length(Ins)
                break
            end
            k = k+1;
        end
        %
        switch showfractions
            case 'subfield'
                Ins(j).Name = nm(1:end-n);
                nms{k-j+2} = 'sum of all fractions';
                for i = k-j+1:-1:1
                    nms{i} = sprintf('fraction %i',i);
                end
                if ischar(Ins(j).Val1)
                    Ins(j).SubFld = {{Ins(j:k).Val1} nms};
                else
                    Ins(j).SubFld = {[Ins(j:k).Val1] nms};
                end
                Ins(j+1:k)=[];
            case 'quantities'
                for i = j:k
                    Ins(i).ShortName = Ins(i).Name;
                    Ins(i).Name = Ins(i).Name(1:end-n);
                end
        end
        j=j+1;
    end
    if reducebedto2d
        for j=1:length(Ins)
            s1 = strfind(Ins(j).Name,'S1');
            s2 = strfind(Ins(j).Name,'S2');
            s = max([s1 s2]);
            if ~isempty(s)
                smatch = strmatch(Ins(j).Name(s+2:end),{'','M2','TOT','-DIS','-PAR','OMP'},'exact');
                if ~isempty(smatch)
                    if Ins(j).DimFlag(5)
                        Ins(j).DimFlag(5)=0;
                        Ins(j).SubFld=FI.Grid.MNK(3);
                    end
                end
            end
        end
    end
    shownames = qp_settings('delwaq_names');
    for j=1:length(Ins)
        if isempty(Ins(j).ShortName)
            Ins(j).ShortName = Ins(j).Name;
            frac = '';
        else
            frac = sprintf(' fraction %s', Ins(j).ShortName(length(Ins(j).Name)+1:end));
        end
        if strncmp(Ins(j).Name,'DPTAVG_',7)
            [LN1,Ins(j).Units,Ins(j).SubsGrp]=substdb(Ins(j).Name(8:end),mass_per,'minmatchlen',minlen-7);
            LN = ['depth average of ' LN1];
            if reducedptstatto2d
                Ins(j).DimFlag(K_) = 0;
            end
        elseif strncmp(Ins(j).Name,'DPTMAX_',7)
            [LN1,Ins(j).Units,Ins(j).SubsGrp]=substdb(Ins(j).Name(8:end),mass_per,'minmatchlen',minlen-7);
            LN = ['maximum of ' LN1 ' over water depth'];
            if reducedptstatto2d
                Ins(j).DimFlag(K_) = 0;
            end
        elseif strncmp(Ins(j).Name,'DPTMIN_',7)
            [LN1,Ins(j).Units,Ins(j).SubsGrp]=substdb(Ins(j).Name(8:end),mass_per,'minmatchlen',minlen-7);
            LN = ['minimum of ' LN1 ' over water depth'];
            if reducedptstatto2d
                Ins(j).DimFlag(K_) = 0;
            end
        else
            [LN,Ins(j).Units,Ins(j).SubsGrp]=substdb(Ins(j).Name,mass_per,'minmatchlen',minlen);
        end
        switch shownames
            case 'expanded'
                Ins(j).Name = [LN frac];
            case 'short'
                Ins(j).Name = Ins(j).ShortName;
            case 'both'
                if isempty(strfind(LN, Ins(j).Name))
                    Ins(j).Name = [LN frac ' [' Ins(j).ShortName ']'];
                else
                    Ins(j).Name = [LN frac];
                end
        end
        if Ins(j).BedLayer>0
            Ins(j).SubsGrp=[Ins(j).SubsGrp '-bedlayer'];
            Ins(j).Name=[Ins(j).Name ' (bed layer)'];
            Ins(j).Units='kg/m^2';
        end
        if qp_option(FI,'balancefile') && isfield(Ins,'BalSubFld')
            %
            % balance file
            %
            Ins(j).Units = [Ins(j).Units '/d'];
            Out(1).BalSubFld=[];
        end
        if isequal(Ins(j).ShortName,'Limit Chlo')
            Ins(end+1)=Ins(j);
            Ins(end).Name = [Ins(j).Name ' (limiting factors)'];
            Ins(end).NVal = -1;
            %Ins(end).DimFlag(T_) = 0;
        end
    end
    % find vectors
    iX=find(strncmpi('x-comp',{Ins.Name}',6) | strcmpi('horizontal flow velocity first direction',{Ins.Name}'))';
    for i = iX
        if Ins(i).Name(1)=='x'
            ystr = Ins(i).Name;
            ystr(1) = 'y';
        else
            ystr = 'horizontal flow velocity second direction';
        end
        iY = find(strcmpi(ystr,{Ins.Name}'));
        if length(iY)==1
            if Ins(i).Name(1)~='x'
                Ins(i).Name = 'horizontal flow velocity';
            elseif length(Ins(i).Name)>15 && isequal('x-component of ',Ins(i).Name(1:15))
                Ins(i).Name = Ins(i).Name(16:end);
            elseif length(Ins(i).Name)>10 && isequal('x-comp of ',Ins(i).Name(1:10))
                Ins(i).Name = Ins(i).Name(11:end);
            else
                continue
            end
            Ins(i).Val2 = Ins(iY).Val1;
            Ins(i).ShortName = {Ins(i).ShortName Ins(iY).ShortName};
            Ins(i).NVal = 2;
            Ins(iY).Name = '*already processed*';
        end
    end
    Ins(strcmp('*already processed*',{Ins.Name})) = [];
    %
    [~,reorder]=sort({Ins.SubsGrp});
    Ins=Ins(reorder);
    [subsgrp,~,J]=unique({Ins.SubsGrp});
    for i=length(subsgrp):-1:1
        j=find(J==i);
        [~,reorder]=sort(upper({Ins(j).Name}));
        Ins(j+i-1)=Ins(j(reorder));
        if strcmp(showfractions,'quantities')
            % remove any (at most 2) leading zeros in fraction number
            % inserted to make sure the fractions are sorted in numerical
            % order.
            for j1 = j'+i-1
                nm = strrep(Ins(j1).Name,'fraction 0','fraction ');
                Ins(j1).Name = strrep(nm,'fraction 0','fraction ');
            end
        end
        if i>1
            j0=j(1)+i-2;
            Ins(j0).Name='-------';
            Ins(j0).Units='';
            Ins(j0).DimFlag=[0 0 0 0 0];
            Ins(j0).Group='';
            Ins(j0).Val1='';
            Ins(j0).ShortName='';
            Ins(j0).SubsGrp='';
        end
    end
    %end
    Out = [Out(1:icnst-1); Ins; Out(icnst+1:end)];
end
if isfield(FI,'Grid') && ~isempty(FI.Grid) && includegrid
    if isequal(Out(1).Name,'segment number')
        Out=Out([1 1:length(Out)]);
    else
        Out=Out([1 1 1 1:length(Out)]);
        Out(2).Name='segment number';
        Out(2).Units='';
        %
        Out(3).Name='-------';
        Out(3).Units='';
        Out(3).DimFlag=[0 0 0 0 0];
        Out(3).Group='';
        Out(3).Val1='';
        Out(3).ShortName='';
        Out(3).SubsGrp='';
    end
    Out(1).Name='grid';
    Out(1).Units='';
    Out(1).DimFlag=Out(4).DimFlag;
    Out(1).DimFlag(T_)=0;
    Out(1).DimFlag(K_)=0;
    if strcmp(FI.Grid.FileType,'netCDF')
        Out(1).DataInCell=2;
    else
        Out(1).DataInCell=0;
    end
    Out(1).NVal=0;
    Out(1).SubFld=[];
    Out(1).Loc='d';
    Out(1).ReqLoc='d';
    %
    % if the first field is a quantity in a sediment layer, then the
    % 3D segment information might get lost. Let's verify whether any of
    % the quantities has a 3rd dimension.
    %
    hask=0;
    for i=3:length(Out)
        hask = hask | Out(i).DimFlag(K_);
    end
    %
    Out(2).DimFlag(K_)=hask; %1
    %Out(2).DimFlag(T_)=0; % switch off time, but keep it 3D
    Out(2).DataInCell=1;
    Out(2).NVal=1;
    Out(2).SubFld=[];
    Out(2).Loc='z';
    Out(2).ReqLoc='z';
    %Out(2).Group='';
    Out(2).Val1='';
end

if isPartFile(FI)
    lasti = 0;
    OilTypes = {};
    for i = 1:length(Out)
        Substance = Out(i).Name;
        if length(Substance)>5 && strcmp(Substance(end-5:end),' stick')
            Substance = deblank(Substance(1:end-6));
            Out(i).Name = [Substance ' (sticking)'];
            Out(i).Units = 'kg/m^2';
            OilTypes{end+1} = Substance;
        elseif length(Substance)>4 && strcmp(Substance(end-4:end),' disp')
            Substance = deblank(Substance(1:end-5));
            Out(i).Name = [Substance ' (dispersed)'];
            Out(i).Units = 'kg/m^3';
            OilTypes{end+1} = Substance;
        end
    end
    OilTypes = unique(OilTypes);
    for i = 1:length(Out)
        if ismember(Out(i).Name,OilTypes)
            Out(i).Name = [Out(i).Name ' (floating)'];
            Out(i).Units = 'kg/m^2';
        end
    end
    for i = 1:length(Out)
        if isempty(Out(i).Units)
            switch Out(i).Name
                case {'grid','segment number','-------'}
                otherwise
                    Out(i).Units = 'kg/m^3';
            end
        end
    end
end

% enable GridView
if (strcmp(subtype,'map') || strcmp(subtype,'plot')) && isfield(FI,'Grid') && ~isempty(FI.Grid) && enablegridview
    [Out.UseGrid] = deal(1);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
if ~iscell(Props.SubFld)
    subf={};
else
    subf=Props.SubFld{2};
end
if nargin>2 && f~=0
    subf=subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
if isequal(LocFI.FileType,'DelwaqLGA')
    if Props.DimFlag(T_)
        sz(T_) = FI.Attributes.(Props.Val1).NTimes;
    end
    sz([M_ N_])=FI.MNK(1:2);
    if Props.DimFlag(K_)
        sz(K_) = FI.MNK(3);
    end
    return
end
switch subtype
    case 'map'
        %if Props.DimFlag(M_) && Props.DimFlag(N_)
        sz([M_ N_])=FI.Grid.MNK(1:2);
        %end
        if Props.DimFlag(K_)
            sz(K_)=FI.Grid.MNK(3);
        end
    case 'plot'
        if isbinary
            sz([M_ N_])=FI.GridSize;
        else
            [X,Chk]=vs_get(LocFI,casemod('DELPAR_PARAMS'),casemod('SIZES'),'quiet');
            sz([M_ N_])=X([5 6]);
        end
        if Props.DimFlag(K_)
            if isbinary
                if isfield(LocFI,'K')
                    sz(K_)=LocFI.K;
                else
                    sz(K_)=length(Props.Val1);
                end
            else
                sz(K_)=X(4);
            end
        end
    otherwise
        if Props.DimFlag(ST_) || (Props.DimFlag(M_) && ~Props.DimFlag(N_))
            m_ = M_;
            if Props.DimFlag(ST_)
                m_ = ST_;
            end
            if isbinary
                sz(m_)=LocFI.NumSegm;
                if Props.DimFlag(K_)
                    sz(K_)=length(Props.Val1);
                end
            else
                if iscell(Props.Val1)
                    sz(K_)=length(Props.Val1);
                    Info=vs_disp(LocFI,Props.Group,Props.Val1{1});
                else
                    Info=vs_disp(LocFI,Props.Group,Props.Val1);
                end
                sz(m_)=Info.SizeDim;
            end
        end
end
if isfield(Props,'BedLayer') && Props.BedLayer<0
    sz(K_)=sz(K_)-1;
end
if Props.DimFlag(T_)
    if isbinary
        sz(T_)=LocFI.NTimes;
    else
        Info=vs_disp(LocFI,Props.Group,[]);
        sz(T_)=Info.SizeDim;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
if strcmp(subtype,'grid')
    T=FI.Attributes.(Props.Val1).Times;
    if t~=0
        T=T(t);
    end
elseif isbinary
    T=delwaq('read',LocFI,1,1,t);
else
    if strcmp(subtype,'plot')
        [X,Chk]=vs_get(LocFI,casemod([DELWAQ '_PARAMS']),casemod('TIME_OFFSET'),'quiet');
        T0=datenum(X(1),X(2),X(3)/3600/24);
        TStep=1/(24*3600);
    else
        [Str,Chk]=vs_get(LocFI,casemod([DELWAQ '_PARAMS']),casemod('TITLE'),'quiet');
        [T0,TStep] = delwaqt0(Str(4,:));
    end
    if isequal(Props.Group,casemod([DELWAQ '_RESULTS']))
        [T,Chk]=vs_let(LocFI,casemod([DELWAQ '_RESULTS']),{t},casemod('TIME'),'quiet');
        T=T0+T*TStep;
    else % ONE FIELD
        T=T0;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
if isbinary
    S=LocFI.SegmentName;
else
    if ~isempty(strmatch(LocFI.SubType,{'Delft3D-waq-map','Delft3D-par-map'}))
        S='';
        return
    end
    if isfield(LocFI,'SegmentName')
        S=LocFI.SegmentName;
    else
        [S,Chk]=vs_get(LocFI,casemod([DELWAQ '_PARAMS']),casemod('LOCATION_NAMES'),'quiet');
        S=cellstr(S);
    end
end
if ~isequal(t,0)
    S=S(t);
end
% -----------------------------------------------------------------------------


function [Full,Unit,GroupID]=substdb(Abb,cmd,varargin)
% substance database
persistent x
if nargin>=2
    switch cmd
        case 'filename'
            if isempty(x)
                substdb;
            end
            if isfield(x,'ProcDefFile')
                Full = x.ProcDefFile;
            else
                Full = '[unable to locate]';
            end
            return
        case 'reload'
            x=[];
            substdb;
            return
    end
else
    cmd = 'n/a'; % mass_per not specified as 2nd argument
end
%
% nargin == 0 or 1
%
if isempty(x)
    ErrMsg='';
    x=-1;
    try
        ErrMsg='Cannot find or open ';
        tbl=qp_settings('delwaq_procdef');
        for WAQDIR = {'waq','dwaq'}
            waq = WAQDIR{1};
            if isequal(tbl,'auto') || ~exist(tbl,'file')
                tbl=cat(2,getenv('D3D_HOME'),filesep,getenv('ARCH'),filesep,waq,filesep,'default',filesep,'proc_def.dat');
            end
            if ~exist(tbl,'file')
                if isstandalone % from d3d_qp.exe on Windows (or old Linux)
                    % two levels up to "D3D_HOME/ARCH" and then proc_def.dat should be located in waq/default
                    tbl=cat(2,qp_basedir('exe'),filesep,'..',filesep,'..',filesep,waq,filesep,'default',filesep,'proc_def.dat');
                elseif ispc % from d3d_qp.m on Windows (or old Linux)
                    % one level up to "D3D_HOME/ARCH" and then proc_def.dat should be located in waq/default
                    tbl=cat(2,qp_basedir('exe'),'..',filesep,waq,filesep,'default',filesep,'proc_def.dat');
                else % from d3d_qp.m on (new) Linux
                    % one level to "share" and then proc_def.dat should be located in dwaq
                    tbl=cat(2,qp_basedir('exe'),'..',filesep,waq,filesep,'proc_def.dat');
                end
            end
        end
        if ~exist(tbl,'file') && ispc
            for drive = 'cdef'
                subdir = dir([drive ':\S*']);
                for j = 1:length(subdir)
                    if subdir(j).isdir
                        tbl = [drive ':\' subdir(j).name '\programs\delwaq\fixed\proc_def.dat'];
                        if exist(tbl,'file')
                            break
                        end
                    end
                end
                if exist(tbl,'file')
                    break
                end
            end
        end
        if ~exist(tbl,'file')
            tbl='proc_def.dat';
        end
        TBL=vs_use(tbl,'quiet');
        [ID,Chk]=vs_get(TBL,'TABLE_P2','ITEM_ID','quiet');
        [NM,Chk]=vs_get(TBL,'TABLE_P2','ITEM_NM','quiet');
        [GRPID,Chk]=vs_get(TBL,'TABLE_P2','GROUPID','quiet');
        [UNIT,Chk]=vs_get(TBL,'TABLE_P2','UNIT','quiet');
        [SUBS,Chk]=vs_get(TBL,'TABLE_R2','R2_SID','quiet');
        isSubs=ismember(ID,SUBS,'rows');
        [WK,Chk]=vs_get(TBL,'TABLE_P2','WK','quiet');
        isTransp=WK=='x';
        if Chk
            x=[];
            x.ID=lower(ID);
            x.NM=NM;
            x.GRPID=GRPID;
            x.UNIT=UNIT;
            x.ProcDefFile=tbl;
            x.NonTranspSubs=isSubs & ~isTransp;
        end
    catch
        ui_message('error',[ErrMsg tbl]);
        x=-1;
    end
end
%
Unit='';
GroupID='';
if nargin==0
    if isstruct(x)
        Full=x.ID;
    else
        Full='';
    end
else
    if isstruct(x)
        db=ustrcmpi(lower(Abb),cellstr(x.ID),'casematch',4,varargin{:});
        % much faster:
        %db=strmatch(lower(Abb),x.ID,'exact');
        %if isempty(db), db=-1; end
        if db>0
            Full=deblank(x.NM(db,:));
            if isequal(lower(Full),'undefined')
                Full=Abb;
            end
            Unit=deblank(x.UNIT(db,:));
            Unit=Unit(2:end-1);
            if strcmp(Unit,'no unit')
                Unit = '-';
            elseif strcmp(Unit,'various') || strcmp(Unit,'?')
                Unit = '';
            elseif x.NonTranspSubs(db,:)
                if strcmp(cmd,'m2') % data file indicates /m2
                    if length(Unit)>3 && strcmp(Unit(end-2:end),'/m2')
                        % procdef also says /m2 --> OK, no change needed
                    else
                        % procdef says /cell --> add per m2
                        Unit = [Unit '/m2'];
                    end
                elseif strcmp(cmd,'cell') % data file indicates /cell
                    if length(Unit)>3 && strcmp(Unit(end-2:end),'/m2')
                        % procdef says /m2 --> strip off m2
                        Unit = Unit(1:end-3);
                    else
                        % procdef also says /cell --> OK, no change needed
                    end
                else % strcmp(cmd,'n/a')
                    % don't care, no change needed
                end
            end
            GroupID=deblank(x.GRPID(db,:));
        else
            Full=Abb;
        end
    else
        Full=Abb;
    end
end

% -----------------------------------------------------------------------------
function [LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI)
%==============================================================================
isbinary=isfield(FI,'ByteOrder') | isfield(FI,'DwqBin');
subtype='history';
casemod=@nochange;
if strcmp('DelwaqLGA',FI.FileType)
    LocFI=FI;
    subtype='grid';
    DELWAQ='';
elseif strcmp('DelparPLOT',FI.FileType)
    if isfield(FI,'DwqBin')
        LocFI=FI.DwqBin;
    else
        LocFI=FI;
    end
    isbinary=1;
    subtype='plot';
    DELWAQ='DELPAR';
elseif isbinary
    if isfield(FI,'DwqBin')
        LocFI=FI.DwqBin;
    else
        LocFI=FI;
    end
    if ~isempty(strmatch(lower(FI.FileType),{'delwaqmap','delparmap'}))
        subtype='map';
    end
    DELWAQ='';
else
    %Extract NEFIS data
    if isfield(FI,'Nfs')
        LocFI=FI.Nfs;
    else
        LocFI=FI;
    end
    if strcmp(lower(LocFI.SubType(end-2:end)),'map')
        subtype='map';
    elseif strcmp(lower(LocFI.SubType(end-3:end)),'plot') || strcmp(lower(LocFI.SubType(end-2:end)),'psf')
        subtype='plot';
    end
    DELWAQ=['DEL' upper(LocFI.SubType(9:11))];
    if ~isstruct(vs_disp(LocFI,[DELWAQ '_PARAMS'],[]))
        casemod=@lower;
    end
end


% -----------------------------------------------------------------------------
function bool=isPartFile(FI)
%==============================================================================
[LocFI,isbinary,subtype,DELWAQ]=KeyParamFI(FI);
if strcmp(DELWAQ,'DELPAR')
    bool = 1;
elseif strcmp(DELWAQ,'DELWAQ')
    bool = 0;
elseif strcmp(subtype,'grid')
    bool = 0;
elseif isequal(qp_option(FI,'balancefile'),1)
    bool = 0;
else
    bool = 0; % unknown
    if isfield(FI,'DwqBin')
        Subs = FI.DwqBin.SubsName;
    else
        Subs = FI.SubsName;
    end
    for i = 1:length(Subs)
        sub = Subs{i};
        if length(sub)>5 && strcmp(sub(end-4:end),'stick')
            bool = 1;
        elseif length(sub)>8 && strcmp(sub(end-7:end),'stick_01')
            bool = 1;
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
shownames = {'expanded','short','both'};
showfractions = {'subfield','quantities'};
switch cmd
    case 'initialize'
        OK=optfig(mfig);
        switch lower(FI.FileType)
            case 'delwaqlga'
                cellflds = {'loadvolflux'};
            otherwise
                cellflds = {'treatas1d','balancefile','clipwherezundefined','reducebedto2d','reducedptstatto2d','showfractions'};
        end
        for cellfld = cellflds
            Fld = cellfld{1};
            f = findobj(mfig,'tag',Fld);
            enable = 'on';
            switch Fld
                case {'showfractions'}
                    defval = showfractions{1};
                case {'clipwherezundefined','reducebedto2d','reducedptstatto2d'}
                    defval = 1;
                case 'treatas1d'
                    defval = 0;
                    if isfield(FI,'Grid') && ~isempty(FI.Grid)
                        enable = 'off';
                    end
                case 'balancefile'
                    defval = 0;
                otherwise
                    defval = 0;
            end
            value = qp_option(FI,Fld,'default',defval);
            if ischar(value)
                value = ustrcmpi(value,showfractions);
            end
            set(f,'value',value,'enable',enable)
            %
            switch Fld
                case 'balancefile'
                    Fld = 'nettransport';
                    f = findobj(mfig,'tag',Fld);
                    if value
                        value = qp_option(FI,Fld,'default',0);
                        set(f,'value',value,'enable','on')
                    else
                        set(f,'value',0,'enable','off')
                    end
                case 'loadvolflux'
                    Fld = 'loadvolflux';
                    f = findobj(mfig,'tag',Fld);
                    if isfield(FI,'Attributes') && isstruct(FI.Attributes)
                        set(f,'string','Reload Volumes, Fluxes, ...')
                    end
            end
        end
        options(FI,mfig,'updateprocdef');
        
    case {'updateprocdef'}
        f1 = findobj(mfig,'tag','shownames');
        set(f1,'value',ustrcmpi(qp_settings('delwaq_names'),shownames))
        %
        f1 = findobj(mfig,'tag','autoprocdef');
        f2 = findobj(mfig,'tag','procdefname');
        f3 = findobj(mfig,'tag','procdefbrowse');
        if strcmp(qp_settings('delwaq_procdef'),'auto')
            set(f1,'value',1,'enable','on')
            set(f2,'string',substdb('cmd','filename'),'enable','inactive')
            set(f2,'backgroundcolor',[1 1 0],'backgroundcolor',Inactive) % need dummy backgroundcolor to get proper color set (R2012a)
            set(f3,'enable','off')
        else
            set(f1,'value',0,'enable','on')
            set(f2,'string',substdb('cmd','filename'),'enable','on','backgroundcolor',Active)
            set(f3,'enable','on')
        end
        
    case {'shownames'}
        f1 = findobj(mfig,'tag','shownames');
        qp_settings('delwaq_names',shownames{get(f1,'value')})
        
    case {'showfractions'}
        f = findobj(mfig,'tag','showfractions');
        if nargin>3
            Log = varargin{1};
            value = ustrcmpi(Log, showfractions);
            if value < 0
                error('Invalid argument "%s" specified for %s.',Log,cmd)
            end
        else
            value = get(f,'value');
        end
        set(f,'value',value)
        NewFI = qp_option(NewFI,cmd,showfractions{value});
        cmdargs = {cmd value};

    case {'autoprocdef'}
        f1 = findobj(mfig,'tag','autoprocdef');
        if get(f1,'value')
            % now auto
            qp_settings('delwaq_procdef','auto')
            substdb('cmd','reload')
        else
            % now manual
            qp_settings('delwaq_procdef','manual but not yet specied')
        end
        options(FI,mfig,'updateprocdef');
        
    case {'procdefname'}
        f1 = findobj(mfig,'tag','procdefname');
        qp_settings('delwaq_procdef',get(f1,'string'))
        substdb('cmd','reload')
        options(FI,mfig,'updateprocdef');
        
    case {'procdefbrowse'}
        procdef = qp_settings('delwaq_procdef');
        if isequal(procdef,'auto')
            procdef = 'proc_def.dat';
        end
        [fn,pn]=uigetfile({'proc_def.dat','Process definition files'},'Select process definition file',procdef);
        if ischar(fn)
            qp_settings('delwaq_procdef',[pn fn])
            substdb('cmd','reload')
        end
        options(FI,mfig,'updateprocdef');
        
    case {'loadvolflux'}
        NewFI.Attributes = [];
        base = FI.FileBase;
        H = progressbar(0,'title','Scanning for files ...');
        attribs = dir([base '*']);
        nattribs = length(attribs);
        bytesdone = 0;
        bytestotal = sum([attribs.bytes]);
        for i = 1:nattribs
            [p,n,extp] = fileparts(attribs(i).name);
            progressbar(bytesdone/bytestotal,H,'title',['Reading ' attribs(i).name]);
            bytesdone = bytesdone + attribs(i).bytes;
            ext = extp(2:end); % extension without point
            try
                switch lower(extp)
                    case {'.vol','.sal','.tem','.vdf','.tau'}
                        NewFI.Attributes.(ext) = waqfil('open',[base ext],FI.NoSeg);
                    case {'.are','.flo','.poi','.len'}
                        %NewFI.Attributes.(ext) = waqfil('open',[base ext],sum(FI.NoExchMNK));
                    case {'.srf','.dps','.chz','.srfold'}
                        NewFI.Attributes.(ext) = waqfil('open',[base ext]);
                end
            catch e
                if strcmp(e.message,'Could this be a new format srf file?')
                    NewFI.Attributes.(ext) = waqfil('open',[base ext],FI.NoSeg);
                end
            end
        end
        delete(H)
        f = findobj(mfig,'tag','loadvolflux');
        set(f,'string','Reload Volumes, Fluxes, ...')
        
    case {'treatas1d','balancefile','nettransport','clipwherezundefined','reducebedto2d','reducedptstatto2d'}
        f = findobj(mfig,'tag',cmd);
        if nargin>3
            Log = varargin{1};
            if ~isequal(Log,0) && ~isequal(Log,1)
                error('Invalid argument specified for %s.',cmd)
            end
            value = Log;
        else
            value = get(f,'value');
        end
        set(f,'value',value)
        NewFI = qp_option(NewFI,cmd,value);
        cmdargs = {cmd value};
        %
        if isequal(cmd,'balancefile')
            Fld = 'nettransport';
            f = findobj(mfig,'tag',Fld);
            if value
                value = qp_option(FI,Fld,'default',0);
                set(f,'value',value,'enable','on')
            else
                set(f,'value',0,'enable','off')
            end
        end
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function OK=optfig(h0)
Active=[1 1 1];
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
width=FigPos(3)-20;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions treatas1d', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Treat as 1D', ...
    'Enable','off', ...
    'Tooltip','Assume that history stations are ordered along a line', ...
    'Tag','treatas1d');
%
voffset=voffset-30;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions balancefile', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Balance File', ...
    'Enable','off', ...
    'Tooltip','Treat file as balance file', ...
    'Tag','balancefile');
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions nettransport', ...
    'Horizontalalignment','left', ...
    'Position',[181 voffset width 18], ...
    'String','Net transport', ...
    'Enable','off', ...
    'Tooltip','Combine "transport in" and "transport out" to "net transport" term', ...
    'Tag','nettransport');
%
voffset=voffset-30;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions loadvolflux', ...
    'Enable','off', ...
    'Position',[11 voffset 160 20], ...
    'String','Load Volumes, Fluxes, ...', ...
    'Tag','loadvolflux')
%
voffset=voffset-30;
h2 = uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shownames', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String',{'Show Expanded Names','Show Short Code Names','Show Both Names'}, ...
    'Enable','on', ...
    'Tooltip','Show expanded and/or short code names for D-Water Quality quantities', ...
    'Tag','shownames');
voffset=voffset-25;
h2 = uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions showfractions', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String',{'Fractions Collected in Subfield','Fractions as Separate Quantities'}, ...
    'Enable','off', ...
    'Tooltip','Select the way in which enumerated instances of the same quantity (so-called, fractions) are displayed.', ...
    'Tag','showfractions');
%
voffset=voffset-30;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions autoprocdef', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Auto Locate Process Definition File', ...
    'Enable','off', ...
    'Tooltip','Automatically locate process definition file', ...
    'Tag','autoprocdef');
voffset=voffset-25;
h2 = uicontrol('Parent',h0, ...
    'Style','edit', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions procdefname', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width-30 20], ...
    'String','procdef.dat', ...
    'Enable','off', ...
    'Tooltip','Process definition file', ...
    'Tag','procdefname');
h2 = uicontrol('Parent',h0, ...
    'Style','pushbutton', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions procdefbrowse', ...
    'Horizontalalignment','left', ...
    'Position',[21+width-30 voffset 20 20], ...
    'String','...', ...
    'Enable','off', ...
    'Tooltip','Browse for process definition file', ...
    'Tag','procdefbrowse');
%
voffset=voffset-30;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions clipwherezundefined', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Clip Where Z Undefined', ...
    'Enable','off', ...
    'Tooltip','Clip values where Z Coordinate is undefined', ...
    'Tag','clipwherezundefined');
%
voffset=voffset-30;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions reducebedto2d', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Reduce S1/S2 Quantities to 2D', ...
    'Enable','off', ...
    'Tooltip','List quantities in bed layers S1 or S2 as 2D quantities', ...
    'Tag','reducebedto2d');
voffset=voffset-25;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions reducedptstatto2d', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Reduce Depth Statistics Quantities to 2D', ...
    'Enable','off', ...
    'Tooltip','List statistical quantities over water depth as 2D quantities', ...
    'Tag','reducedptstatto2d');
OK=1;
% -----------------------------------------------------------------------------

function S = nochange(S)
% dummy routine to not change the case
