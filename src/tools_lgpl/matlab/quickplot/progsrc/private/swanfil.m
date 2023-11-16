function varargout=swanfil(FI,domain,field,cmd,varargin)
%SWANFIL QP support for SWAN spectral files.
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
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

% expand indices ...
sz=getsize(FI,Props);
for i=1:5
    if isequal(idx{i},0)
        idx{i} = 1:sz(i);
    end
end

if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
else
    idx{T_} = 1;
end
% load data ...
% remove locations at which all quantities at all times are non-specified
if Props.NVal == 0
elseif isfield(FI,'Directions')
    % 2D spectrum
    Empty = all(all(cellfun('isempty',FI.Spectrum(Props.Fld,:)),3),1);
    stat = find(~Empty);
    val = {FI.Spectrum{Props.Fld,stat(idx{ST_}),idx{T_}}(idx{M_},idx{N_})};
else
    % 1D spectrum
    Empty = all(cellfun('isempty',FI.Spectrum),2);
    stat = find(~Empty);
    val = {FI.Spectrum{stat(idx{ST_}),idx{T_}}(idx{M_},Props.Fld)};
end

% generate output ...
if XYRead
    if Props.NVal == 0
        Ans.X = FI.LocationsXY(:,1);
        Ans.XUnits = FI.UnitXY;
        Ans.Y = FI.LocationsXY(:,2);
        Ans.YUnits = FI.UnitXY;
    else
        Ans.X  = FI.Frequencies(idx{M_});
        Ans.XUnits = 'Hz';
        if Props.DimFlag(N_)
            Ans.X  = repmat(Ans.X',1,length(idx{N_}));
            Ans.Y = repmat(FI.Directions(idx{N_}),length(idx{M_}),1);
            Ans.YUnits = 'deg';
        end
    end
end

if Props.NVal==1
    Ans.Val=val{1};
    Ans.Units=Props.Units;
end

% read time ...
if DimFlag(T_)
    Ans.Time = FI.Time(idx{T_});
else
    Ans.Time=[];
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'          'Units'   'DimFlag' 'DataInCell' 'NVal' 'Fld'};
DataProps={'spectrum'      ''        [0 5 1 0 0]  0          1     0    };
%======================== SPECIFIC CODE DIMENSIONS ============================
qnts = {FI.Quant.Name}';
units = {FI.Quant.Unit}';
N = length(qnts);

%
Out=cell2struct(DataProps,PropNames,2);
Out.MName = 'Frequency';
%
if isfield(FI,'Directions')
    Out.DimFlag(N_) = 1;
    Out.NName = 'Direction';
end
%
Out(1:N) = Out;
%
TranslationTable = {
    'EnDens'   'energy densities'
    'NDIR'     'average nautical direction'
    'DSPRDEGR' 'directional spreading'
    'VaDens'   'variance densities'};
%
for i=1:N
    ii = strmatch(qnts{i},TranslationTable(:,1),'exact');
    if length(ii)==1
        qnts(i) = TranslationTable(ii,2);
    end
end
%
time_dependent = isfield(FI,'Time');
for i=1:N
    Out(i).Name = qnts{i};
    Out(i).Units = units{i};
    Out(i).Fld = i;
    if time_dependent
        if all(FI.Time>0 & FI.Time<1)
            Out(i).DimFlag(T_) = 3;
        else
            Out(i).DimFlag(T_) = 1;
        end
    end
end
%
Out(end+1).Name = '-------';
%
Out(end+1).Name = 'data locations';
Out(end).Units = '';
Out(end).DimFlag = [0 3 0 0 0];
Out(end).DataInCell = 0;
Out(end).NVal = 0;
%
Out = Out([end end-1 1:end-2]);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
%======================== SPECIFIC CODE =======================================
if isfield(FI,'Directions')
    if ~isempty(Props.Fld)
        % remove locations at which all quantities at all times are non-specified
        Empty = all(all(cellfun('isempty',FI.Spectrum(Props.Fld,:)),3),1);
        sz(ST_) = size(FI.LocationsXY(~Empty,:),1);
    else
        sz(ST_) = size(FI.LocationsXY,1);
    end
else
    Empty = all(cellfun('isempty',FI.Spectrum),2);
    sz(ST_) = size(FI.LocationsXY(~Empty,:),1);
end
if isfield(FI,'Time')
    sz(T_) = length(FI.Time);
end
sz(M_) = length(FI.Frequencies);
if Props.DimFlag(N_)
    sz(N_) = length(FI.Directions);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if nargin==2
    t=':';
end
if isfield(FI,'Directions')
    if ~isempty(Props.Fld)
        % remove locations at which all quantities at all times are non-specified
        Empty = all(all(cellfun('isempty',FI.Spectrum(Props.Fld,:)),3),1);
        Locs = FI.LocationsXY(~Empty,:);
    else
        Locs = FI.LocationsXY;
    end
else
    Empty = all(cellfun('isempty',FI.Spectrum),2);
    Locs = FI.LocationsXY(~Empty,:);
end
N = size(Locs,1);
S=cell(N,1);
if isfield(FI,'UnitXY') && strcmp(FI.UnitXY,'deg')
    S = degstr(Locs,'lonlat','cell');
else
    for i = 1:N
        S{i} = sprintf('(%.2f m,%.2f m)',Locs(i,:));
    end
end
S=S(t);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
T = FI.Time;
if t~=0
    T = T(t);
end
% -----------------------------------------------------------------------------
