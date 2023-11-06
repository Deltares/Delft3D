function varargout=mikezerofil(FI,domain,field,cmd,varargin)
%MIKEZEROFIL QP support for Mike 11, 21 and 3 files.
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
        varargout={readsts(FI,Props,varargin{:})};
        return
    case 'subfields'
        varargout={{}};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        Parent=varargin{1};
        Ops=varargin{2};
        hOld=varargin{3};
        Station=varargin{4};
        %
        LocationStr=readsts(FI,Props,Station);
        switch Props.Name
            case 'cross sectional profile'
                YZ=FI.CrossSection.YZ{Station};
                xtyp='Distance';
                xval='';
                xunit='m';
            case 'area(z) plot'
                YZ=FI.CrossSection.ZARP{Station}([2 1],:);
                xtyp='Val';
                xval='area';
                xunit='m^2';
            case 'hydraulic radius(z) plot'
                YZ=FI.CrossSection.ZARP{Station}([3 1],:);
                xtyp='Val';
                xval='hydraulic radius';
                xunit='m';
            case 'wetted perimeter(z) plot'
                YZ=FI.CrossSection.ZARP{Station}([4 1],:);
                xtyp='Val';
                xval='wetted perimeter';
                xunit='m';
        end
        if ~isempty(hOld{1})
            hNew = hOld{1};
            set(hNew,'xdata',YZ(1,:),'ydata',YZ(2,:))
        else
            hNew=line(YZ(1,:),YZ(2,:),'parent',Parent,'color',Ops.colour);
        end
        setaxesprops(Parent,[xtyp '-Z'],{xval 'elevation'},{xunit 'm'})
        %
        set(get(Parent,'title'),'string',LocationStr,'interpreter','none')
        %
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
gidx={[] [] 0 0 0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

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
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) | isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            if DataInCell, gidx{i}=1:sz(i)+1; end
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        elseif DataInCell
            gidx{i}=idx{i}(1):(idx{i}(end)+1);
        end
    end
end
if ~DataInCell
    gidx=idx;
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

% read grid ...
x=[];
y=[];
z=[];
triangular = 0;
if XYRead && DimFlag(M_)
    if isfield(FI,'DataType') && strcmp(FI.DataType,'unstructured')
        triangular = 1;
        xid = strmatch('X-coord',{FI.Item.Name},'exact');
        x=mike('read',FI,xid,1);
        yid = strmatch('Y-coord',{FI.Item.Name},'exact');
        y=mike('read',FI,yid,1);
        zid = strmatch('Z-coord',{FI.Item.Name},'exact');
        if ~isempty(zid)
            z=mike('read',FI,zid,1);
        end
        tri = strmatch('Connectivity',{FI.Item.Name},'exact');
        TRI=mike('read',FI,tri,1);
        nnd = strmatch('No of nodes',{FI.Item.Name},'exact');
        NND=mike('read',FI,nnd,1);
        TRI = reshape(TRI,[NND(1) length(TRI)/NND(1)])';
        if NND(1)==6
            TRI = reshape(TRI,[FI.NumLayers FI.NumCells NND(1)]);
            TRI = squeeze(TRI(1,:,1:3));
            inodes = unique(TRI(:));
            map(inodes)=1:FI.NumNodes;
            TRI = map(TRI);
            %
            x = reshape(x,[FI.NumLayers+1 FI.NumNodes])';
            y = reshape(y,[FI.NumLayers+1 FI.NumNodes])';
            z = reshape(z,[FI.NumLayers+1 FI.NumNodes])';
            x = x(:,idx{K_});
            y = y(:,idx{K_});
            z = z(:,idx{K_});
        end
    else
        if isfield(FI,'Grid') % Mike21C grid
            x=mike('read',FI.Grid,1,1);
            y=mike('read',FI.Grid,2,1);
        else
            G=mike('read',FI,Props.Index,-1);
            if isfield(G,'X')
                x=G.X;
            end
            if isfield(G,'Y')
                y=G.Y;
            end
            if isfield(G,'Z')
                z=G.Z;
            end
        end
        if ~DataInCell
            if isempty(y)
                x=transpose(conv(x,[.5 .5]));
            else
                [x,y,z]=corner2center(x,y,z);
            end
        end
    end
end

% read data ...
val1=mike('read',FI,Props.Index,idx{T_});
if triangular
    if sz(K_)==0
        val1 = reshape(val1,[length(idx{T_}) 1 sz(M_)]);
    else
        val1 = reshape(val1,[length(idx{T_}) sz(K_) sz(M_)]);
        val1 = val1(:,idx{K_},:);
        val1 = permute(val1,[1 3 2]);
    end
else
    elidx=idx([M_ N_ K_]);
    elgidx=gidx([M_ N_ K_]);
    for i=1:3, if isequal(elidx{i},0), elidx{i}=1; end, end
    for i=1:3, if isequal(elgidx{i},0), elgidx{i}=1; end, end

    if ~isempty(x)
        x=squeeze(x(elgidx{:}));
    end
    if ~isempty(y)
        y=squeeze(y(elgidx{:}));
    end
    if ~isempty(z)
        z=squeeze(z(elgidx{:}));
    end
    if length(idx{T_})~=1
        val1=squeeze(val1(:,elidx{:}));
    else
        val1=squeeze(val1(elidx{:}));
    end
end

% generate output ...
if XYRead
    if triangular
        Ans.FaceNodeConnect=TRI;
        Ans.X = x;
        Ans.Y = y;
        if ~isempty(z)
            Ans.Z = z;
        end
    else
        if ~isempty(x)
            Ans.X=x;
            Ans.XUnits = 'm';
        end
        if ~isempty(y)
            Ans.Y=y;
            Ans.YUnits = 'm';
        end
        if ~isempty(z)
            Ans.Z=z;
            Ans.ZUnits = 'm';
        end
    end
end

switch Props.NVal
    case 0
    case 1
        Ans.Val=val1;
        if triangular
            if sz(M_)==FI.NumCells
                Ans.ValLocation = 'FACE';
            else
                Ans.ValLocation = 'NODE';
            end
        end
end

% read time ...
Ans.Time=readtim(FI,Props,idx{T_});

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                         'DimFlag'    'NVal' 'DataInCell' 'Index' 'UseGrid'};
DataProps={'data field'                    [1 0 0 0 0]  1           1       0          1    };
Out=cell2struct(DataProps,PropNames,2);
if isfield(FI,'DataType')
    DT = FI.DataType;
else
    DT = 'structured';
end
unstructured = false;
switch DT
    case 'unstructured'
        unstructured = true;
        %
        iElmType = strcmp('Element type',{FI.Item.Name});
        nElements = FI.Item(iElmType).MatrixSize;
        %
        fm=strmatch('MIKE_FM',{FI.Attrib.Name},'exact');
        Out(1).Geom = 'UGRID2D-FACE';
        if FI.Attrib(fm).Data(3)==3
            Out(1).DimFlag=[1 0 1 1 1];
            Out(1).Coords = 'xyz';
        else
            Out(1).DimFlag=[1 0 1 0 0];
            Out(1).Coords = 'xy';
        end
    case 'crosssections'
        Out(1).Name = 'cross sectional profile';
        Out(1).DimFlag=[0 5 0 0 0];
        Out(1).NVal = -1;
        Out(2) = Out(1);
        Out(2).Name = 'area(z) plot';
        Out(3) = Out(2);
        Out(3).Name = 'hydraulic radius(z) plot';
        Out(4) = Out(3);
        Out(4).Name = 'wetted perimeter(z) plot';
        return
    otherwise
        switch FI.NumCoords
            case 1
                Out(1).DimFlag=[1 0 1 0 0];
                Out(1).UseGrid=0;
            case 2
                Out(1).DimFlag=[1 0 1 1 0];
            case 3
                Out(1).DimFlag=[1 0 1 1 1];
        end
end
if ~isempty(FI.Item)
    for i = length(FI.Item):-1:1
        if strcmp(FI.Item(i).Name,'Connectivity')
            if i < length(FI.Item)
                Out(i) = [];
            end
            continue
        end
        %
        Out(i)=Out(1);
        if isfield(FI.Item,'EUMTypeStr') && ~strcmp(FI.Item(i).EUMTypeStr,'Undefined')
            if 0
                Out(i).Name  = [strtrim(FI.Item(i).Name) ' <' FI.Item(i).EUMTypeStr '>'];
            else
                Out(i).Name  = strtrim(FI.Item(i).Name);
            end
            Out(i).Units = FI.Item(i).UnitStr;
        else
            Out(i).Name  = strtrim(FI.Item(i).Name);
            Out(i).Units = '';
        end
        if unstructured
            if FI.Item(i).MatrixSize ~= nElements
                Out(i).Geom = 'UGRID2D-NODE';
                Out(i).DataInCell = 0;
            end
        end
        Out(i).Index=i;
    end
else
    Out(:,1)=[];
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
switch FI.FileType
    case 'MikeCTDT'
        if Props.DimFlag(M_)
            sz(M_)=FI.DataDim(1)+1;
        end
        if Props.DimFlag(N_)
            sz(N_)=FI.DataDim(2)+1;
        end
        if Props.DimFlag(K_)
            sz(K_)=FI.DataDim(3)+1;
        end
        if Props.DimFlag(T_)
            sz(T_)=max(1,FI.NumTimeSteps);
        end
    case 'MikeDFS'
        idx=Props.Index;
        if isfield(Props,'Geom')
            szM = FI.Item(Props.Index).MatrixSize;
            sz(N_)=1;
            if szM == FI.NumCells*FI.NumLayers
                sz(M_)=FI.NumCells;
                if Props.DimFlag(K_)
                    sz(K_)=FI.NumLayers;
                end
            elseif szM == FI.NumNodes*(FI.NumLayers+1)
                sz(M_)=FI.NumNodes;
                if Props.DimFlag(K_)
                    sz(K_)=FI.NumLayers+1;
                end
            elseif szM == FI.NumNodes*FI.NumLayers
                sz(M_)=FI.NumNodes;
                if Props.DimFlag(K_)
                    sz(K_)=FI.NumLayers;
                end
            end
        else
            if Props.DimFlag(M_)
                sz(M_)=FI.Item(idx).MatrixSize(1);
            end
            if Props.DimFlag(N_)
                sz(N_)=FI.Item(idx).MatrixSize(2);
            end
            if Props.DimFlag(K_)
                sz(K_)=FI.Item(idx).MatrixSize(3);
            end
        end
        if Props.DimFlag(T_)
            if FI.Item(idx).Static
                sz(T_)=1;
            else
                sz(T_)=FI.NumTimeSteps;
            end
        end
    case 'MikeXFS'
        sz(ST_) = length(FI.CrossSection.Name);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)

%======================== SPECIFIC CODE =======================================
switch FI.FileType
    case 'MikeCTDT'
        if isequal(t,0)
            t=1:max(1,FI.NumTimeSteps);
        end
        T=FI.RefDate+FI.TimeStep*(t-1+FI.StartTimeStep);
    case 'MikeDFS'
        idx=Props.Index;
        if FI.Item(idx).Static
            T=FI.RefDate;
        else
            if isequal(t,0)
                t=1:FI.NumTimeSteps;
            end
            T=FI.RefDate+FI.Times(t);
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=[];
cmd=lower(cmd);
cmdargs={};
switch cmd,
    case 'initialize'
        OK=optfig(mfig);
    case 'selgrid'
        Grd=mike('open');
        if ~isempty(Grd) & isfield(Grd,'Check') & isequal(Grd.Check,'OK') & Grd.NumItems==2
            NewFI=FI;
            NewFI.Grid=Grd;
        end
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
if nargin<3 || t==0
    t = 1:length(FI.CrossSection.Name);
end
S = cell(length(t),1);
for i = 1:length(t)
    if isempty(FI.CrossSection.Name{t(i)})
        S{i} = sprintf('%s@%g', ...
            FI.CrossSection.Branch{t(i)}, ...
            FI.CrossSection.Offset(t(i)));
    else
        S{i} = sprintf('%s (%s@%g)', ...
            FI.CrossSection.Name{t(i)}, ...
            FI.CrossSection.Branch{t(i)}, ...
            FI.CrossSection.Offset(t(i)));
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function OK=optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selgrid', ...
    'Position',[11 341 160 20], ...
    'String','select grid ...', ...
    'Tag','Pushbutton1');
OK=1;
% -----------------------------------------------------------------------------
