function varargout=tekalfil(FI,domain,field,cmd,varargin)
%TEKALFIL QP support for Tekal, ESRI shape and ArcInfo generate files.
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
        varargout={readtim(FI,Props,varargin{:})};
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
        Parent=varargin{1};
        Ops=varargin{2};
        hOld=varargin{3};
        subf={'ellipsephase','ellipsephasevec','ellipse','cross'};
        subf = subf{varargin{4}};
        idx = varargin(5:6);
        if isequal(idx{1},0)
            idx{1}=':';
        end
        if isequal(idx{2},0)
            idx{2}=':';
        end
        idx{3} = ':';
        %
        data = FI.Field(Props.Block).Data(idx{:});
        data(data==999.999) = NaN;
        hNew = plot_tidalellipses(data(:,:,1),data(:,:,2), ...
            'ep',data(:,:,7),data(:,:,8),data(:,:,10),data(:,:,9), ...
            'color',Ops.colour,'parent',Parent,'plottype',subf);
        set(hNew,'linestyle',Ops.linestyle,'linewidth',Ops.linewidth)
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);

subf=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    Props.SubFld=varargin{1};
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

%========================= GENERAL CODE =======================================
gidx = idx;
for i=[ST_ M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end)) && i~=ST_ && ~isequal(FI.FileType,'ESRI-Shape')
            error('Only scalars or ranges allowed for index %i',i);
        end
        if DataInCell && strcmp(FI.FileType,'tekal')
            if idx{i}(1)==1
                if length(idx{i})==1
                    gidx{i} = [1 1];
                else
                    gidx{i} = idx{i};
                    idx{i} = idx{i}(2:end);
                end
            else
                gidx{i} = [idx{i}(1)-1 idx{i}];
            end
        end
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

xy='dummy';
x='dummy';
xname='';
y='dummy';
z='dummy';
val1=[];
val2=[];
OutTime=NaN;

% read grid and data ...
already_selected = 0;
blck = Props.Block;
if isempty(Props.SubFld) && ~isempty(Props.Select)
    Props.SubFld = 1;
end
switch FI.FileType
    case 'tekal'
        if isfield(FI,'plotonpoly')
            already_selected = 1;
            Data=tekal('read',FI.plotonpoly,idx{M_});
            if ~iscell(Data)
                Data={Data};
            end
            for i=1:length(Data)
                Data{i}(end+1,:) = NaN;
            end
            if ~isempty(Props.Select)
                val_1 = tekal('read',FI,1);
                val_1 = val_1(idx{M_},Props.Select);
                szP = cellfun('size',Data,1);
                val1 = zeros(sum(szP),1);
                off = 0;
                for i=1:length(szP)
                    val1(off+(1:szP(i))) = val_1(i);
                    off = off+szP(i);
                end
            end
            Data = cat(1,Data{:});
        elseif isfield(FI,'combinelines') && FI.combinelines % LDB
            switch Props.Name
                case 'lines'
                    Data=tekal('read',FI,idx{M_});
                    already_selected = 1;
                case 'labels'
                    Data=tekal('read',FI,idx{M_});
                    if iscell(Data)
                        for i = 1:numel(Data)
                            d = Data{i};
                            j = find(d(:,1)~=999.999 | d(:,2)~=999.999);
                            Data{i} = d(j(1),:);
                        end
                        Data=cat(1,Data{:});
                    else
                        j = find(Data(:,1)~=999.999 | Data(:,2)~=999.999);
                        Data = Data(j(1),:);
                    end
                    val1 = {FI.Field(idx{M_}).Name};
                    already_selected = 1;
                otherwise
                    Data=tekal('read',FI,0);
            end
            if iscell(Data) % if there is more than one block
                for i=1:length(Data)-1
                    Data{i}(end+1,:)=NaN;
                end
                Data=cat(1,Data{:});
            end
            blck=1;
        else
            if isempty(blck)
                blck     = Props.Select{Props.SubFld}(1);
                idx{ST_} = Props.Select{Props.SubFld}(2);
            elseif ~isempty(Props.SubFld)
                idx{ST_} = Props.Select{Props.SubFld};
            end
            Data=tekal('read',FI,blck);
            if ~isempty(FI.Field(blck).Time)
                OutTime = FI.Field(blck).Time;
            end
        end
        if strcmp(Props.Geom,'POLYL')
            if iscell(Data)
                for i = 1:numel(Data)
                    d = Data{i};
                    d(d(:,1)==999.999 & d(:,2)==999.999,:) = NaN;
                    Data{i} = d;
                end
            else
                Data(Data(:,1)==999.999 & Data(:,2)==999.999,:)=NaN;
            end
        end
    case 'GeoJSON'
        already_selected = 1;
        geotypes = FI.features.data(1,:);
        switch Props.Name
            case 'line'
                geoset = {'MultiLineString'};
            case 'point'
                geoset = {'Point'};
        end
        ifeature = find(ismember(geotypes,geoset));
        Data = FI.features.data(2,ifeature(idx{M_}));
    case 'AutoCAD DXF'
        if strcmp(Props.Coords,'xyz')
            Data=FI.Lines(1:3,:)';
        else
            Data=FI.Lines(1:2,:)';
        end
    case 'BNA File'
        already_selected = 1;
        Data = bna('readc',FI,idx{M_});
    case 'ArcInfoUngenerate'
        already_selected = 1;
        Data = ai_ungen('readc',FI,idx{M_});
    case 'ESRI-Shape'
        already_selected = 1;
        if strcmp(Props.Geom,'PNT')
            Data = shape('read',FI,idx{M_},'points');
        else
            Data = shape('read',FI,idx{M_},'lines');
        end
        if Props.NVal>0
            val1 = dbase('read',FI.dBase,idx{M_},Props.Select);
            val1 = val1{1};
            % miss=isnan(Obj);
            % expand val1 array to match size of Obj/Data.
%             [lia,idxM] = ismember(Obj,idx{M_});
%             miss = ~lia;
%             idxM(miss) = 1;
%             val1 = val1(idxM);
%             if iscell(val1)
%                 % cell array of strings
%                 val1(miss)={''};
%             elseif ischar(val1)
%                 % array of characters
%                 val1(miss)=' ';
%             else
%                 val1(miss)=NaN;
%             end
        end
    case {'DelwaqTimFile','LexYacc_TimeTable'}
        Data=repmat(NaN,sz(T_),sz(ST_)+1);
        x=repmat(NaN,1,sz(ST_));
        y=repmat(NaN,1,sz(ST_));
        Data(:,1)=readtim(FI,Props);
        T=Data(:,1);
        %
        j=0;
        for i=1:length(FI.Table)
            c=strmatch(Props.Name,{FI.Table(i).Parameter(2:end).Name},'exact');
            if ~isempty(c)
                j=j+1;
                %
                T0 = FI.Table(i).Data(:,1);
                if isfield(FI.Table(i),'RefTime') && ~isempty(FI.Table(i).RefTime)
                    T0=FI.Table(i).RefTime+T0*FI.Table(i).TimeUnit;
                end
                %
                if isfield(FI.Table(i),'MetricCoords') && ~isempty(FI.Table(i).MetricCoords)
                    x(j)=FI.Table(i).MetricCoords(1);
                    y(j)=FI.Table(i).MetricCoords(2);
                end
                DT = repmat(inf,size(T));
                ifrom = zeros(size(T));
                for it = 1:length(T0)
                    DT0 = abs(T-T0(it));
                    itt = DT0<DT;
                    ifrom(itt) = it;
                    DT(itt) = DT0(itt);
                end
                %
                if isfield(FI,'DTmax')
                    DTmax = FI.DTmax;
                else
                    DTmax = 0;
                end
                %
                ito = DT<=DTmax;
                ifrom = ifrom(ito);
                Data(ito,1+j)=FI.Table(i).Data(ifrom,c+1);
            end
        end
    case {'WaterML2'}
        Data = FI.TimeSeries.Series{Props.Block};
    case {'NOOS'}
        Data = FI.Series(Props.Block).val';
end

% return grid ...
Ann=0;
if isequal(FI.FileType,'tekal')
    Ann=strcmp(FI.Field(blck).DataTp,'annotation');
end
SToutofrange=[];
if XYRead
    if Ann
        if Props.DimFlag(ST_)
            x=Data{1}(idx{ST_},1);
            y=Data{1}(idx{ST_},2);
        else
            x=Data{1}(idx{M_},1);
            y=Data{1}(idx{M_},2);
        end
    elseif DimFlag(M_) && DimFlag(N_) && DimFlag(K_)
        x=Data(idx{[M_ N_ K_]},1);
        y=Data(idx{[M_ N_ K_]},2);
        z=Data(idx{[M_ N_ K_]},3);
    elseif DimFlag(K_)
        z=Data(idx{K_},1);
    elseif DimFlag(M_) && DimFlag(N_)
        if DataInCell
            x=Data(gidx{[M_ N_]},3);
            y=Data(gidx{[M_ N_]},4);
            x(x==0 & y==0) = NaN;
            y(isnan(x))    = NaN;
        else
            x=Data(idx{[M_ N_]},1);
            y=Data(idx{[M_ N_]},2);
        end
    elseif DimFlag(M_)
        if already_selected
            if iscell(Data)
                xy=Data;
            else
                x=Data(:,1);
                if strcmp(Props.Coords,'xy')
                    y=Data(:,2);
                end
                if size(Data,2)==3
                    z=Data(:,3);
                end
            end
        else
            x=Data(idx{M_},1);
            if strcmp(Props.Coords,'xy')
                y=Data(idx{M_},2);
            elseif strcmp(Props.Coords,'xyz')
                y=Data(idx{M_},2);
                z=Data(idx{M_},3);
            elseif isempty(Props.Coords)
                xname=FI.Field(blck).ColLabels{1};
            end
        end
    elseif DimFlag(ST_) && ~ischar(x)
        SToutofrange = idx{ST_}>sz(ST_);
        idx{ST_}(SToutofrange)=1;
        x=x(idx{ST_});
        y=y(idx{ST_});
        x(SToutofrange)=NaN;
        y(SToutofrange)=NaN;
    end
end
if ~ischar(x)
    x(x==-999)=NaN;
end
if ~ischar(y)
    y(y==-999)=NaN;
end
if ~ischar(z)
    z(z==-999)=NaN;
end

% return data ...
if Props.NVal==0
    % no data
elseif Ann
    if Props.DimFlag(ST_)
        val1=Data{2}(idx{ST_});
    else
        val1=Data{2}(idx{M_});
    end
    for i = length(val1):-1:2
        if x(i)==999.999 && y(i)==999.999
            val1{i-1} = [val1{i-1} val1{i}];
            val1{i} = '';
            x(i) = NaN;
            y(i) = NaN;
        end
    end
elseif Props.Time
    switch Props.Time
        case -1 % time obtained via explicit read
            OutTime = readtim(FI,Props);
            val1 = Data(idx{T_},idx{ST_});
            val1(:,SToutofrange)=NaN;
        case {1,1.1,1.2,1.3} % time in seconds/minutes/hours/days
            switch Props.Time
                case 1 % seconds
                    fac = 3600*24;
                case 1.1 % minutes
                    fac = 60*24;
                case 1.2 % hours
                    fac = 24;
                case 1.3 % days
                    fac = 1;
            end
            OutTime=Data(idx{T_},1)/fac;
            val1=Data(idx{T_},idx{ST_}+1);
            val1(:,SToutofrange)=NaN;
        case 2 % (yy)yymmdd hhmmss
            OutTime=tdelft3d(Data(idx{T_},1),Data(idx{T_},2));
            val1=Data(idx{T_},idx{ST_}+2);
            val1(:,SToutofrange)=NaN;
    end
elseif DimFlag(M_) && DimFlag(N_) && DimFlag(K_)
    val1=Data(idx{[M_ N_ K_]},idx{ST_}+3);
elseif DimFlag(K_)
    val1=Data(idx{K_},idx{ST_}+1);
    val1(val1==-999)=NaN;
elseif DimFlag(M_) && DimFlag(N_)
    if Props.NVal==2 && isempty(idx{ST_})
        idx{ST_}=[1 2];
    end
    val1=Data(idx{[M_ N_]},idx{ST_}+2);
    if size(val1,3)>1
        val2=val1(:,:,2);
        val1=val1(:,:,1);
    end
    iKCS=strmatch('KCS',FI.Field(blck).ColLabels,'exact');
    if ~isempty(iKCS)
        act=logical(Data(idx{[M_ N_]},iKCS));
        val1(~act)=NaN;
        if ~isempty(val2)
            val2(~act)=NaN;
        end
        if XYRead && ~DataInCell
            x(~act)=NaN;
            y(~act)=NaN;
        end
    end
    val1(val1==-999)=NaN;
    val2(val2==-999)=NaN;
elseif DimFlag(M_)
    switch FI.FileType
        case 'tekal'
            if ~already_selected
                if isempty(idx{ST_}) % polygon case
                    val1=Data(idx{M_},3);
                else
                    val1=Data(idx{M_},idx{ST_}+1);
                end
            end
        otherwise
            if isempty(val1)
                if ~already_selected
                    val1=Data(idx{M_},2);
                end
            else
                if ~already_selected
                    val1=val1(idx{M_},1);
                end
            end
    end
end

%========================= GENERAL CODE =======================================

% generate output ...
if XYRead
    if ~ischar(xy)
        Ans.XY = xy;
    end
    if ~ischar(x)
        Ans.X=x;
        if ~isempty(xname)
            Ans.XName = xname;
        end
    end
    if ~ischar(y)
        Ans.Y=y;
    end
    if ~ischar(z)
        Ans.Z=z;
    end
    if strcmp(FI.FileType,'tekal')
        unit = '';
        for blck = [blck 1]
            Cmnt = FI.Field(blck).Comments;
            for i=1:length(Cmnt)
                [Tk,Rm]=strtok(Cmnt{i}(2:end));
                if strcmpi(Tk,'coordinate')
                    [Tk,Rm]=strtok(Rm);
                    if strcmpi(Tk,'system')
                        [a,c,err,idx]=sscanf(Rm,'%*[ :=]%c',1);
                        cs = deblank(Rm(idx-1:end));
                        if strcmpi(cs,'spherical')
                            unit = 'deg';
                            break
                        elseif strcmpi(cs,'cartesian')
                            unit = 'm';
                            break
                        end
                    end
                end
            end
            if ~isempty(unit)
                break
            end
        end
        if ~isempty(unit)
            if isfield(Ans,'X')
                Ans.XUnits = unit;
            end
            if isfield(Ans,'Y')
                Ans.YUnits = unit;
            end
        end
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end
if isnan(OutTime)
    Ans.Time=[];
else
    Ans.Time=OutTime;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                       'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'Block' 'Time' 'ClosedPoly' 'SubFld' 'Select'};
DataProps={'field X'                    ''     ''       [0 0 0 0 0]    0          1       0       0       0          []      {}  };
switch FI.FileType
    case 'tekal'
        if isfield(FI,'plotonpoly')
            DataProps={'polygon'           'POLYL' 'xy'    [0 0 1 0 0]   0           0       1       0       1          []      {}  };
            DataProps=repmat(DataProps,2+length(FI.Field.ColLabels),1);
            DataProps{2,1} = '-------';
            DataProps(3:end,1)=FI.Field.ColLabels;
            DataProps(3:end,6)={1};
            for i=1:length(FI.Field.ColLabels)
                DataProps(2+i,end) = {i};
            end
        elseif isfield(FI,'combinelines') && FI.combinelines
            if FI.Field(1).Size(2)==3 % pliz-file
                DataProps={'line'              'POLYL' 'xy'    [0 0 1 0 0]   0           0       0       0       1          []      {}
                           'lines'             'POLYL' 'xy'    [0 0 1 0 0]   0           0       0       0       1          []      {}
                           'labels'            'PNT'   'xy'    [0 0 1 0 0]   0           4       0       0       1          []      {}
                           'column 3'          'POLYL' 'xy'    [0 0 1 0 0]   0           1       0       0       0          []      {}  };
                if ~isempty(FI.Field(1).ColLabels{3})
                    DataProps{4,1} = FI.Field(1).ColLabels{3};
                else
                    [p,f,e]=fileparts(FI.FileName);
                    if strcmpi(e,'.pliz')
                        DataProps{4,1} = 'elevation';
                    end
                end
            else
                DataProps={'line'              'POLYL' 'xy'    [0 0 1 0 0]   0           0       0       0       1          []      {}
                           'lines'             'POLYL' 'xy'    [0 0 1 0 0]   0           0       0       0       1          []      {}
                           'labels'            'PNT'   'xy'    [0 0 1 0 0]   0           4       0       0       1          []      {}  };
            end
        else
            [p,f,e]=fileparts(FI.FileName);
            for i=1:length(FI.Field)
                switch FI.Field(i).DataTp
                    case 'numeric'
                        switch length(FI.Field(i).Size)
                            case 2 % 1D
                                Col1 = lower(FI.Field(i).ColLabels{1});
                                Col2 = '';
                                if length(FI.Field(i).ColLabels)>=2
                                    Col2 = lower(FI.Field(i).ColLabels{2});
                                    if isequal(Col1,'date') && isequal(Col2,'time')
                                        Col1='date and time';
                                    elseif isequal(Col1,'yymmdd') && isequal(Col2,'hhmmss')
                                        Col1='date and time';
                                    elseif isequal(Col1,'yyyymmdd') && isequal(Col2,'hhmmss')
                                        Col1='date and time';
                                    end
                                end
                                if strncmpi(Col1,'x coord',7) || strncmpi(Col1,'x-coord',7)
                                    Col1 = 'x-coordinate';
                                    if strncmpi(Col2,'y coord',7) || strncmpi(Col2,'y-coord',7)
                                        Col1 = 'x- and y-coordinate';
                                    end
                                end
                                if strncmpi(Col1,'z coord',7) || strncmpi(Col1,'z-coord',7)
                                    Col1 = 'z-coordinate';
                                end
                                switch Col1
                                    case {'date and time'}
                                        DP={'field X'    'PNT'  ''  [1 5 0 0 0]  0          1       i       2       0          []      {}  };
                                        DP{1}=sprintf('%s',FI.Field(i).Name);
                                        DataProps(end+1,:)=DP;
                                    case {'time in seconds','time in minutes','time in hours','time in days'}
                                        switch Col1
                                            case 'time in seconds'
                                                tim = 1;
                                            case 'time in minutes'
                                                tim = 1.1;
                                            case 'time in hours'
                                                tim = 1.2;
                                            case 'time in days'
                                                tim = 1.3;
                                        end
                                        DP={'field X'    'PNT'  ''  [3 5 0 0 0]  0          1       i       tim     0          []      {}  };
                                        DP{1}=sprintf('%s',FI.Field(i).Name);
                                        DataProps(end+1,:)=DP;
                                    case {'z-coordinate'}
                                        DP={'field X'    'PNT+' 'z'  [0 5 0 0 1]  0          1       i       0       0          []      {}  };
                                        DP{1}=sprintf('%s',FI.Field(i).Name);
                                        DataProps(end+1,:)=DP;
                                    case {'x- and y-coordinate'}
                                        DP={'field X'    'PNT' 'xy'  [0 5 1 0 0]  0          1       i       0       0          []      {}  };
                                        DP{1}=sprintf('%s',FI.Field(i).Name);
                                        DataProps(end+1,:)=DP;
                                    otherwise
                                        DP={'field X'    'sSEG' 'x' [0 5 1 0 0]  0          1       i       0       0          []      {}  };
                                        DP{1}=sprintf('%s',FI.Field(i).Name);
                                        if FI.Field(i).Size(2)==2
                                            if strcmpi(e,'.ldb') || strcmpi(e,'.pol')
                                                DP([2 3 4 6 9])={'POLYL' 'xy' [0 0 1 0 0] 0 1};
                                            else
                                                DP{9}=1;
                                            end
                                        end
                                        DataProps(end+1,:)=DP;
                                end
                            case 4 % 2D
                                DP={'field X'        'sQUAD' 'xy' [0 5 1 1 0]  0          1       i       0       0          []      {}  };
                                Fourier = strmatch('* Results fourier analysis on:',FI.Field(i).Comments);
                                Elliptic = strmatch('* Elliptic parameters of',FI.Field(i).Comments);
                                if ~isempty(Fourier)
                                    Quant = strtrim(FI.Field(i).Comments{Fourier}(31:end));
                                    DP{10}=1;
                                    switch FI.Field(i).Size(2)
                                        case 13
                                            %
                                            % Vector quantity (amplitudes and phases)
                                            %
                                            DP{11}={5,7,6,8};
                                        case 12
                                            %
                                            % Vector quantity (minimum or maximum) - component wise and magnitude 
                                            %
                                            DP{11}={5,6,7};
                                        otherwise
                                            %
                                            % Scalar quantity (amplitude and phase)
                                            %
                                            DP{11}={5,6};
                                    end
                                    DP{4}=[0 0 1 1 0];
                                    DP{5}=1;
                                    Freq = strmatch('* Frequency [degrees/hour]   :',FI.Field(i).Comments);
                                    if length(FI.Field(i).ColLabels)>=7 && strcmp(FI.Field(i).ColLabels(7),'Maximum value')
                                        DP{1}=sprintf('%s, maximum',Quant);
                                        %
                                        % use amplitude column only - scalar quantity
                                        %
                                        DP{11}=DP{11}(1);
                                    elseif length(FI.Field(i).ColLabels)>=7 && strcmp(FI.Field(i).ColLabels(7),'Minimum value')
                                        DP{1}=sprintf('%s, minimum',Quant);
                                        %
                                        % use amplitude column only - scalar quantity
                                        %
                                        DP{11}=DP{11}(1);
                                    elseif length(FI.Field(i).ColLabels)>=9 && strcmp(FI.Field(i).ColLabels(9),'Maximum magnitude')
                                        DP{1}=sprintf('%s, maximum',Quant);
                                    elseif length(FI.Field(i).ColLabels)>=9 && strcmp(FI.Field(i).ColLabels(9),'Minimum magnitude')
                                        DP{1}=sprintf('%s, minimum',Quant);
                                    elseif ~isempty(Freq)
                                        Freq = sscanf(FI.Field(i).Comments{Freq}(31:end),'%f',1);
                                        if Freq==0
                                            DP{1}=sprintf('%s, mean',Quant);
                                            %
                                            % use amplitude column(s) only
                                            %
                                            DP{11}=DP{11}(1:length(DP{11})/2);
                                            %
                                            if length(DP{11})==2
                                                DP{6} = 2;
                                                DP{10}=[];
                                                DP{11} = {cat(2,DP{11}{:})}; % put the two amplitudes into a cell array to match the correct subfields switch
                                            end
                                        else
                                            DP{1}=sprintf('%s, %g deg/hr',Quant,Freq);
                                        end
                                    else
                                        DP{1}=sprintf('%s',FI.Field(i).Name);
                                    end
                                elseif ~isempty(Elliptic)
                                    Quant = strtrim(FI.Field(i).Comments{Elliptic}(32:end));
                                    DP{10}=2;
                                    DP{11}={5,6,7,8};
                                    DP{4}=[0 0 1 1 0];
                                    Freq = strmatch('* Frequency [degrees/hour]    :',FI.Field(i).Comments);
                                    if ~isempty(strmatch('* column    7 : Maximum value',FI.Field(i).Comments))
                                        DP{1}=sprintf('%s, maximum',Quant);
                                    elseif ~isempty(strmatch('* column    7 : Minimum value',FI.Field(i).Comments))
                                        DP{1}=sprintf('%s, minimum',Quant);
                                    elseif ~isempty(Freq)
                                        Freq = sscanf(FI.Field(i).Comments{Freq}(32:end),'%f',1);
                                        if Freq==0
                                            DP{1}=sprintf('%s, mean',Quant);
                                        else
                                            DP{1}=sprintf('%s, %g deg/hr',Quant,Freq);
                                        end
                                    else
                                        DP{1}=sprintf('%s',FI.Field(i).Name);
                                    end
                                    %
                                    if size(DataProps,1)>0 && strcmp(DataProps{end,1},DP{1})
                                        if DataProps{end,6}~=1
                                            continue
                                        end
                                        blck = DataProps{end,7};
                                        DataProps{end,7} = [];
                                        fl = DataProps{end,11};
                                        nfl = length(fl);
                                        for is=1:nfl
                                            fl{is}=[blck fl{is}];
                                        end
                                        blck2 = DP{7};
                                        fl2 = DP{11};
                                        for is=1:length(fl2)
                                            fl{nfl+is}=[blck2 fl2{is}];
                                        end
                                        DataProps{end,11} = fl;
                                        %
                                        DP{1} = [DP{1} ', tidal ellipses'];
                                        DP{6} = -1;
                                    end
                                else
                                    if ~isempty(FI.Field(i).Time)
                                        DP{4}(1) = 1;
                                    end
                                    DP{1}=FI.Field(i).Name;
                                    if FI.Field(i).Size(2)==4 && ...
                                            strncmpi(FI.Field(i).ColLabels{3},'x component',11) && ...
                                            strncmpi(FI.Field(i).ColLabels{4},'y component',11)
                                        DP{6}=2;
                                        DP{4}(2)=0;
                                        if strncmpi(FI.Field(i).ColLabels{3},'x component of ',15)
                                            DP{1}=FI.Field(i).ColLabels{3}(16:end);
                                        end
                                    end
                                end
                                DataProps(end+1,:)=DP;
                            case 5 % 3D
                                %DP={'field X'    'sHEX'   'xyz'  [0 5 1 1 1]  0          1       i       0       0          []      {}  };
                                DP={'field X'     'sQUAD+' 'xy+z' [0 5 1 1 1]  0          1       i       0       0          []      {}  };
                                DP{1}=sprintf('%s',FI.Field(i).Name);
                                DataProps(end+1,:)=DP;
                        end
                    case 'annotation'
                        %DP={'field X'                          [0 5 1 1 0]  0          0       i       0       0          []      {}  };
                        %DP{1}=sprintf('%s (%i)',FI.Field(i).Name,i);
                        %DataProps(end+1,:)=DP;
                        DP={'field X'              'sQUAD' 'xy' [0 0 1 1 0]  0          4       i       0       0          []      {}  };
                        DP{1}=sprintf('%s',FI.Field(i).Name);
                        DataProps(end+1,:)=DP;
                end
            end
            DataProps(1,:)=[];
            [UniqueName,I,J] = unique(DataProps(:,1));
            if length(I)<length(J)
                for i=1:length(I)
                    ii=find(J==i);
                    if length(ii)>1
                        for j=ii'
                            DataProps{j,1}=sprintf('%s (%i)',DataProps{j,1},j);
                        end
                    end
                end
            end
        end
    case 'GeoJSON'
        DataProps={'line'                      'POLYL' 'xy'  [0 0 1 0 0]  0          0       0       0       1          []      {}
                   'point'                     'PNT'   'xy'  [0 0 1 0 0]  0          0       0       0       1          []      {}  };
        geotypes = unique(FI.features.data(1,:));
        if ~ismember('MultiLineString',geotypes)
            DataProps(strcmp('line',DataProps(:,1)),:) = [];
        end
        if ~ismember('Point',geotypes)
            DataProps(strcmp('point',DataProps(:,1)),:) = [];
        end
    case {'BNA File','ArcInfoUngenerate','AutoCAD DXF'}
        if isfield(FI,'SubType') && ismember(FI.SubType,{'line','polygon'})
            ncoords = size(FI.Seg(1).Coord,2);
        elseif isfield(FI,'Lines')
            ncoords = size(FI.Lines,1);
        else
            ncoords = 2;
        end
        if ncoords == 3
            DataProps={'line'                      'POLYL' 'xyz' [0 0 1 0 0]  0          0       0       0       1          []      {}  };
        else
            DataProps={'line'                      'POLYL' 'xy'  [0 0 1 0 0]  0          0       0       0       1          []      {}  };
        end
    case 'ESRI-Shape'
        DataProps={'line'                      'POLYL' 'xy' [0 0 6 0 0]  0          0       0       0       1          []      {}  };
        switch FI.ShapeTpName
            case {'polygon','polygonz','polygonm'}
                DataProps([1:2 5])={'polygon' 'POLYG' 2};
            case {'point','pointz','pointm'}
                DataProps([1:2 9])={'point' 'PNT' 0};
        end
        if isfield(FI,'dBase')
            dBaseFlds={FI.dBase.Fld.Name};
            for i=1:length(dBaseFlds)
                DataProps(i+1,:)=DataProps(1,:);
                DataProps{i+1,1}=[DataProps{i+1,1} ':' dBaseFlds{i}];
                switch FI.dBase.Fld(i).Type
                    case {'2','4','8','D','N','F'}
                        DataProps{i+1,6}=1;
                    otherwise
                        DataProps{i+1,6}=4;
                end
                DataProps{i+1,11}=i;
            end
        end
    case {'DelwaqTimFile','LexYacc_TimeTable'}
        DP = {'field X'                       'PNT'   'xy' [1 3 0 0 0]  0          1     NaN       1       0          []      {}  };
        Qnts={};
        for i=1:length(FI.Table)
            Qnts=union(Qnts,{FI.Table(i).Parameter(2:end).Name});
        end
        DataProps = repmat(DP,length(Qnts),1);
        for i=1:length(Qnts)
            DataProps{i,1} = Qnts{i};
            DataProps{i,7} = i;
        end
    case {'WaterML2'}
        PropNames{end+1} = 'Units';
        DP = {'field X'                       'PNT'     '' [1 5 0 0 0]  0          1     NaN       1       0          []      {}  ''};
        if isempty(FI.TimeSeries)
            DataProps = DP;
            DataProps(1,:) = [];
        else
            Qnts = FI.TimeSeries.QuantityName;
            units = FI.TimeSeries.QuantityUnit;
            DataProps = repmat(DP,length(Qnts),1);
            for i=1:length(Qnts)
                DataProps{i,1} = Qnts{i};
                DataProps{i,7} = i;
                DataProps{i,end} = units{i};
            end
        end
    case {'NOOS'}
        DP = {'field X'                       'PNT'   'xy' [1 5 0 0 0]  0          1       -1      -1      0          []      {}  };
        nseries = length(FI.Series);
        DataProps = repmat(DP,nseries,1);
        for i=1:nseries
            DataProps{i,1} = FI.Series(i).unit;
            DataProps{i,7} = i;
        end
end

%======================== SPECIFIC CODE DIMENSIONS ============================
    Out=cell2struct(DataProps,PropNames,2);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if isempty(Props.SubFld)
    return
end
switch length(Props.Select)
    case 1
        %subf={'amplitude'};
        return
    case 2
        subf={'amplitude','phase'};
    case 3
        right_part = fliplr(strtok(fliplr(Props.Name)));
        subf={['u component ' right_part],['v component ' right_part],['magnitude ' right_part]};
    case 4
        if Props.NVal==-1
            subf={'ellipsephase','ellipsephasevec','ellipse','cross'};
        elseif Props.SubFld==2
            subf={'amplitude','eccentricity','phase','inclination'};
        else
            subf={'x component, amplitude','y component, amplitude', ...
                'x component, phase','y component, phase'};
        end
    case 6
        subf={'x component','y component', ...
            'amplitude','eccentricity','phase','inclination'};
    case 8
        subf={'x component, amplitude','y component, amplitude', ...
            'x component, phase','y component, phase', ...
            'amplitude','eccentricity','phase','inclination'};
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
switch FI.FileType
    case 'tekal'
        blck = Props.Block;
        if isempty(blck)
            blck = Props.Select{1}(1);
        end
        if isfield(FI,'plotonpoly')
            sz(M_)=FI.Field.Size(1);
        elseif isfield(FI,'combinelines') && FI.combinelines
            switch Props.Name
                case {'lines','labels'}
                    sz(M_) = length(FI.Field);
                otherwise
                    szi=cat(1,FI.Field.Size);
                    sz(M_)=sum(szi(:,1))+length(FI.Field)-1;
            end
        elseif strcmp(FI.Field(blck).DataTp,'annotation')
            if Props.DimFlag(ST_)
                sz(ST_)=FI.Field(blck).Size(1);
                sz([M_ N_])=1;
            else
                sz(M_)=FI.Field(blck).Size(1);
                sz(N_)=1;
            end
        else
            if Props.DimFlag(K_)
                sz(K_)=FI.Field(blck).Size(1);
            elseif Props.DimFlag(M_) && Props.DimFlag(N_)
                sz(M_)=FI.Field(blck).Size(3);
                sz(N_)=FI.Field(blck).Size(4);
                if Props.DimFlag(K_)
                    sz(K_)=FI.Field(blck).Size(5);
                end
            elseif Props.DimFlag(M_)
                sz(M_)=FI.Field(blck).Size(1);
            end
        end
        if Props.DimFlag(T_)
            if Props.Time
                sz(T_)=FI.Field(blck).Size(1);
            else
                sz(T_)=1;
            end
        end
        if Props.DimFlag(ST_)
            if Props.Time
                sz(ST_)=FI.Field(blck).Size(2)-Props.Time;
            else
                sz(ST_)=FI.Field(blck).Size(2)-sum(Props.DimFlag([M_ N_ K_])~=0);
            end
        end
    case 'GeoJSON'
        geotypes = FI.features.data(1,:);
        switch Props.Name
            case 'line'
                geoset = {'MultiLineString'};
            case 'point'
                geoset = {'Point'};
        end
        sz(M_) = sum(ismember(geotypes,geoset));
    case {'BNA File','ArcInfoUngenerate'}
        sz(M_)=length(FI.Seg);
    case {'AutoCAD DXF'}
        sz(M_)=size(FI.Lines,2);
    case {'ESRI-Shape'}
        if strcmp(Props.Geom,'PNT')
            sz(M_)=FI.NPnt;
        else
            sz(M_)=FI.NShapes;%FI.NPnt+FI.NPrt-1;
        end
    case {'DelwaqTimFile','LexYacc_TimeTable'}
        St=0;
        T=readtim(FI,Props);
        for i=1:length(FI.Table)
            if ismember(Props.Name,{FI.Table(i).Parameter.Name})
                St=St+1;
            end
        end
        sz(T_)=length(T);
        sz(ST_)=St;
    case {'WaterML2'}
        sz(T_)=size(FI.TimeSeries.Series{Props.Block},1);
        sz(ST_)=1;
    case {'NOOS'}
        sz(T_)  = length(FI.Series(Props.Block).times);
        sz(ST_) = 1;
end
% -----------------------------------------------------------------------------

function [TZshift,TZstr]=gettimezone(FI,domain,Props)
TZshift = NaN;
TZstr   = '';
if Props.Time
    switch FI.FileType
        case 'WaterML2'
            TZshift = 0;
            TZstr   = 'UTC';
    end
end

% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
T=[];
if Props.Time
    switch FI.FileType
        case 'tekal'
            Data=tekal('read',FI,Props.Block);
            switch Props.Time
                case 1 % time in seconds
                    T=Data(:,1)/3600/24;
                case 1.1 % time in minutes
                    T=Data(:,1)/60/24;
                case 1.2 % time in hours
                    T=Data(:,1)/24;
                case 1.3 % time in days
                    T=Data(:,1);
                case 2 % (yy)yymmdd and hhmmss
                    T=tdelft3d(Data(:,1),Data(:,2));
            end
        case {'DelwaqTimFile','LexYacc_TimeTable'}
            if isfield(FI,'Times')
                T = FI.Times;
            else
                T = [];
                for i=1:length(FI.Table)
                    T0 = FI.Table(i).Data(:,1);
                    if isfield(FI.Table(i),'RefTime') && ~isempty(FI.Table(i).RefTime)
                        T0=FI.Table(i).RefTime+T0*FI.Table(i).TimeUnit;
                    end
                    T = union(T,T0);
                end
            end
        case {'WaterML2'}
            T = FI.TimeSeries.Series{Props.Block}(:,1);
        case {'NOOS'}
            T = FI.Series(Props.Block).times;
    end
    if nargin>2 && ~isequal(t,0)
        T=T(t);
    end
    T=T(:);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
switch FI.FileType
    case 'tekal'
        if strcmp(FI.Field(Props.Block).DataTp,'annotation')
            Data=tekal('read',FI,Props.Block);
            S=Data{2};
        else
            i0=sum(Props.DimFlag([M_ N_ K_])~=0);
            if Props.Time
                i0=floor(Props.Time);
            end
            S=FI.Field(Props.Block).ColLabels(i0+1:end);
            for i=1:length(S)
                if isempty(S{i})
                    S{i}=sprintf('column %i',i+i0);
                end
            end
        end
    case {'DelwaqTimFile','LexYacc_TimeTable'}
        S={};
        for i=1:length(FI.Table)
            if ismember(Props.Name,{FI.Table(i).Parameter.Name})
                S{end+1}=FI.Table(i).Location;
            end
        end
    case {'WaterML2'}
        S=FI.TimeSeries.LocationName(Props.Block);
    case 'NOOS'
        S={FI.Series(Props.Block).location};
end
if nargin>2 && ~isequal(t,0)
    S=S{t};
end
% -----------------------------------------------------------------------------


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
        OK=optfig(mfig);
        if FI.can_be_ldb
            f=findobj(mfig,'tag','combinelines');
            set(f,'value',FI.combinelines,'enable','on')
        end
    case 'combinelines'
        f=findobj(mfig,'tag','combinelines');
        if nargin>3
            Log=varargin{1};
            if ~isequal(Log,0) && ~isequal(Log,1)
                error('Invalid argument specified for %s.',cmd)
            end
            combinelines=Log;
        else
            combinelines=get(f,'value');
        end
        set(f,'value',combinelines)
        NewFI.combinelines=combinelines;
        cmdargs={cmd combinelines};
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function OK=optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
width=FigPos(3)-20;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions combinelines', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Combine all lines', ...
    'Enable','off', ...
    'Tag','combinelines');
OK=1;
% -----------------------------------------------------------------------------
