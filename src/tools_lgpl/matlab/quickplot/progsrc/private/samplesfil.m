function varargout=samplesfil(FI,domain,field,cmd,varargin)
%SAMPLESFIL QP support for XYZ sample files.
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
        % integrated below
        hParent = varargin{1};
        Ops = varargin{2};
        hOld=varargin{3};
        varargin = varargin(4:end);
        XYRead = 1;
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
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
elseif ~isempty(FI.Time)
    idx{T_} = 1;
end
nTim = max(1,length(idx{T_}));
nLoc = max([sz(M_) sz(ST_)]);
if DimFlag(ST_)
    idxM = idx{ST_};
    szM = sz(ST_);
else
    idxM = idx{M_};
    szM = sz(M_);
end
if isempty(FI.Time)
    if isequal(idxM,0)
        dim1 = ':';
    else
        dim1 = idxM;
        nLoc = length(idxM);
    end
else
    dim1 = ismember(FI.iTime,idx{T_});
    %
    if ~isequal(idxM,0) % can only happen if number of locations is constant, i.e. FI.nLoc scalar
        z = zeros(szM,1);
        z(idxM) = 1;
        dim1 = dim1 & repmat(z,[sz(T_) 1]);
        nLoc = length(idxM);
    end
end

% generate output ...
if XYRead
    switch Props.Coords
        case {'x','d'}
            Ans.X = FI.XYZ(dim1,FI.X)';
        case 'y'
            Ans.Y = FI.XYZ(dim1,FI.Y)';
        case 'xy'
            if iscell(FI.XYZ)
                xyz = zeros(nLoc,2);
                nc = 0;
                done = 0;
                for c = 1:length(FI.XYZ)
                    nq = size(FI.XYZ{c},2);
                    if FI.X>nc && FI.X<=nc+nq
                        xyz(:,1) = FI.XYZ{c}(dim1,FI.X-nc);
                        done = done+1;
                    end
                    if FI.Y>nc && FI.Y<=nc+nq
                        xyz(:,2) = FI.XYZ{c}(dim1,FI.Y-nc);
                        done = done+1;
                    end
                    if done==2
                        break
                    end
                    nc = nc+nq;
                end
            else
                xyz = FI.XYZ(dim1,[FI.X FI.Y]);
            end
            nPnt=size(xyz,1)/nTim;
            nCrd=size(xyz,2);
            Ans.XYZ=reshape(xyz,[nTim nPnt 1 nCrd]);
            switch Props.Geom
                case 'TRI'
                    if isfield(FI,'TRI')
                        Ans.TRI=FI.TRI;
                    elseif ~isempty(FI.X) && ~isempty(FI.Y)
                        try
                            [xy,I]=unique(xyz,'rows');
                            tri=delaunay(xy(:,1),xy(:,2));
                            Ans.TRI=I(tri);
                            if length(FI.nLoc)==1
                                FI.TRI=Ans.TRI;
                            end
                        catch
                            Ans.TRI=zeros(0,3);
                        end
                    end
                case 'sSEG'
                    Ans.X = Ans.XYZ(:,:,1,1);
                    Ans.Y = Ans.XYZ(:,:,1,2);
                    Ans = rmfield(Ans, 'XYZ');
                case 'PNT'
                    Ans.TRI=zeros(0,3);
            end
    end
    %
    if ~isempty(FI.X) && isfield(FI,'ParamUnits')
        HUnits = FI.ParamUnits{FI.X};
        if ~isempty(HUnits)
            if strcmp(HUnits,'degr')
                HUnits = 'deg';
            end
            Ans.XUnits = HUnits;
            if isfield(Ans,'TRI')
                Ans.YUnits = HUnits;
            end
        end
    end
end

% read time ...
T=readtim(FI,Props,idx{T_});

% collect data ...
switch Props.NVal
    case -1
        Min = FI.XYZ(dim1,Props.SubFld(1));
        Max = FI.XYZ(dim1,Props.SubFld(3));
        %
        dx = (max(FI.Times)-min(FI.Times))/(length(FI.Times)-1);
        x = repmat(T',[3 1]);
        x(end,:) = NaN;
        x = x(:);
        %
        y = [Min';repmat(Max',[2 1])];
        y(end,:) = NaN;
        y = y(:);
        %
        hNew = line(x,y, ...
            'parent',hParent, ...
            'color',Ops.colour, ...
            'linestyle',Ops.linestyle, ...
            'linewidth',Ops.linewidth, ...
            'marker',Ops.marker, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
        if Props.SubFld(2)>0
            Mean = FI.XYZ(dim1,Props.SubFld(2));
            hNew(2) = line(T,Mean, ...
                'parent',hParent, ...
                'color',Ops.colour, ...
                'linewidth',Ops.linewidth, ...
                'marker','o', ...
                'markerfacecolor',Ops.colour, ...
                'linestyle','none');
        end
        varargout = {hNew FI};
        return
    case 0
    case {1,4}
        if iscell(FI.XYZ)
            nc = 0;
            for c = 1:length(FI.XYZ)
                nq = size(FI.XYZ{c},2);
                if Props.SubFld>nc && Props.SubFld<=nc+nq
                    if nLoc==0
                        Ans.Val = FI.XYZ{c}(dim1,Props.SubFld-nc)';
                    else
                        Ans.Val = reshape(FI.XYZ{c}(dim1,Props.SubFld-nc),[nTim nLoc]);
                    end
                    break
                end
                nc = nc+nq;
            end
        elseif nLoc==0 % if variable number of locations, then nTim==1
            Ans.Val=FI.XYZ(dim1,Props.SubFld)';
        else
            Ans.Val=reshape(FI.XYZ(dim1,Props.SubFld),[nTim nLoc]);
        end
    otherwise
        if nLoc==0 % if variable number of locations, then nTim==1
            Ans.XComp=FI.XYZ(dim1,Props.SubFld(1))';
            Ans.YComp=FI.XYZ(dim1,Props.SubFld(2))';
        else
            Ans.XComp=reshape(FI.XYZ(dim1,Props.SubFld(1)),[nTim nLoc]);
            Ans.YComp=reshape(FI.XYZ(dim1,Props.SubFld(2)),[nTim nLoc]);
        end
        %
        if strcmp(Props.VecType,'naut')
            M = Ans.XComp;
            A = 90-Ans.YComp;
            Ans.XComp = M.*cosd(A);
            Ans.YComp = M.*sind(A);
        end
end
if DimFlag(T_)
    Ans.Time=T;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)

PropNames={'Name'                       'Units' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Geom'  'Coords' 'SubFld'};
DataProps={'locations'                  ''       [0 0 1 0 0]  0          0     ''        ''    ''      'PNT'   'xy'      []
    'locations as line'                 ''       [0 0 1 0 0]  0          0     ''        ''    ''      'sSEG'  'xy'      []
    'triangulated locations'            ''       [0 0 1 0 0]  0          0     ''        ''    ''      'TRI'   'xy'      []
    '-------'                           ''       [0 0 0 0 0]  0          0     ''        ''    ''      ''      ''        []
    'sample data'                       ''       [0 0 1 0 0]  0          1     ''        ''    ''      'TRI'   'xy'      -999};

Out=cell2struct(DataProps,PropNames,2);

params = 1:length(FI.Params);
params = setdiff(params,[FI.X FI.Y FI.Time]);
if ~isempty(FI.Times)
    if length(FI.nLoc)==1
        f3 = 1;
    else
        f3 = inf; % variable number of nodes
    end
    %
    Out(end).DimFlag(1) = 1;
    for i = 1:length(Out)
        if ~strcmp(Out(i).Name, '-------')
            Out(i).DimFlag(3) = f3;
        end
    end
end

% Expand parameters
NPar=length(params);
if NPar>0
    NFixed = length(Out)-1;
    Out = cat(1,Out(1:end-1),repmat(Out(end),NPar,1));
    for i = 1:NPar
        Out(NFixed+i).SubFld = params(i);
        Out(NFixed+i).Name   = FI.Params{params(i)};
        if isfield(FI,'ParamUnits')
            Out(NFixed+i).Units  = FI.ParamUnits{params(i)};
        end
        if iscell(FI.XYZ) % TODO: check which column contains chars.
            Out(NFixed+i).NVal = 4;
        end
    end
else
    Out=Out(1:end-2);
end

% No triangulation possible if only one or two points, or only one
% coordinate, or if all data are strings
if (length(FI.nLoc)==1 && FI.nLoc<2) || isempty(FI.Y) || isempty(FI.X) || iscell(FI.XYZ)
    Out(2:3)=[];
    for i=1:NPar
        if isempty(FI.X)
            if isempty(FI.Y)
                Out(i+2).Geom='PNT';
                Out(i+2).Coords='';
            else
                Out(i+2).Geom='sSEG';
                Out(i+2).Coords='y';
            end
        elseif isempty(FI.Y)
            Out(i+2).Geom='sSEG';
            if isfield(FI,'Params') && strcmpi(FI.Params{FI.X},'distance')
                Out(i+2).Coords='d';
            else
                Out(i+2).Coords='x';
            end
        else
            Out(i+2).Geom='PNT';
        end
    end
end

if isempty(FI.X) || isempty(FI.Y)
    if (isempty(FI.X) && isempty(FI.Y) && FI.nLoc>1) || ...
            (isfield(FI,'Table') && strcmp(FI.Table,'points'))
        for i=1:NPar
            Out(i+2).DimFlag(2) = 5;
            Out(i+2).DimFlag(3) = 0;
        end
    end
    %
    vars = {Out.Name};
    Mean = find(strcmpi('mean',vars));
    Minm = find(strcmpi('minimum',vars));
    Maxm = find(strcmpi('maximum',vars));
    if ~isempty(Minm) && ~isempty(Maxm) && Out(end).DimFlag(2)~=5
        Out(1) = Out(Minm);
        Out(1).Name = 'error bars';
        Out(1).NVal = -1;
        Out(1).AxesType = 'Time-Val';
        Out(1).SubFld = [Out(Minm).SubFld -999 Out(Maxm).SubFld];
        if ~isempty(Mean)
            Out(1).SubFld(2) = Out(Mean).SubFld;
        end
    else
        Out(1:2) = [];
    end
else
    vars = {Out.Name};
    U = find(strcmpi('u',vars));
    V = find(strcmpi('v',vars));
    if ~isempty(U) && ~isempty(V)
        Out(U).Name = 'VectorUV';
        Out(U).NVal = 2;
        Out(U).SubFld = [Out([U V]).SubFld];
        Out(V) = [];
    end
    %
    if NPar==2
        if (strcmpi(vars{end-1},'parameter 3') && ...
                strcmpi(vars{end},'parameter 4')) || ... % matlab routine starts numbering parameters in column 1
                (strcmpi(vars{end-1},'parameter 1') && ...
                strcmpi(vars{end},'parameter 2')) % gpp started numbering parameters in column 3
            Out(end-1).Name = 'vector';
            Out(end-1).NVal = 2;
            Out(end-1).SubFld = [Out([end-1 end]).SubFld];
            %Out(end).Name = 'vector-nautical';
            %Out(end).NVal = 2;
            %Out(end).VecType = 'naut';
            %Out(end).SubFld = Out(end-1).SubFld;
            Out(end) = [];
        end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
%======================== SPECIFIC CODE =======================================
if Props.DimFlag(T_)
    sz(T_) = length(FI.Times);
end
if Props.DimFlag(M_) && length(FI.nLoc)==1
    sz(M_) = FI.nLoc;
elseif Props.DimFlag(ST_) && length(FI.nLoc)==1
    sz(ST_) = FI.nLoc;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if isempty(FI.Time)
    if ~isempty(FI.Times)
        T = FI.Times;
    else
        T = NaN;
    end
else
    T=FI.Times;
    if t~=0
        T = T(t);
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if nargin<3 || t==0
    S = cell(1,FI.nLoc);
    t=1:FI.nLoc;
else
    S = cell(1,length(t));
end
for i=1:length(t)
    XUnit = '';
    YUnit = '';
    if isfield(FI,'ParamUnits')
        if ~isempty(FI.X)
            XUnit = [' ' FI.ParamUnits{FI.X}];
        end
        if ~isempty(FI.Y)
            YUnit = [' ' FI.ParamUnits{FI.Y}];
        end
    end
    %
    if isempty(FI.X) && isempty(FI.Y)
        S{i} = sprintf('point %i',i);
    elseif ~isempty(FI.X) && isempty(FI.Y)
        S{i} = sprintf('x = %g%s',FI.XYZ(t(i),FI.X),XUnit);
    elseif isempty(FI.X) && ~isempty(FI.Y)
        S{i} = sprintf('y = %g%s',FI.XYZ(t(i),FI.Y),YUnit);
    else
        S{i} = sprintf('(x,y) = (%g%s,%g%s)',FI.XYZ(t(i),FI.X),XUnit,FI.XYZ(t(i),FI.Y),YUnit);
    end
end
% -----------------------------------------------------------------------------
