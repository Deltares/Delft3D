function varargout=flsfil(FI,domain,field,cmd,varargin)
%FLSFIL QP support for FLS and SOBEK incremental files.
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
        varargout={readsts(FI,domain,Props,0)};
        return
    case 'subfields'
        varargout={{}};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

% select appropriate timestep ...
sz=getsize(FI,domain,Props);
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
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end;

muldom=0;
switch FI.FileType,
    case 'FLS-inc'
        %
        % Backward compatible addition of Domain argument ...
        %
        if ~isempty(Props.SubFld1)
            domain=Props.SubFld1;
        end
        %
        %
        if length(idx{T_})>1
            error('Only one timestep supported for FLS-inc files.');
        end
        %
        % load data ...
        %
        elidx=idx(2:end);
        elidx(~DimFlag(2:end))=[];
        [Val1,FI]=fls('inc',FI,Props.SubFld2,idx{T_}/60);
        %
        % Loop over domains if necessary ...
        %
        muldom = domain<1 | domain>length(FI.Domain);
        if muldom
            Domains=1:length(FI.Domain);
        else
            Domains=domain;
        end
        for d=Domains
            % determine grid ...
            x=[];
            y=[];
            z=[];
            if XYRead & DataInCell
                eidx=idx;
                eidx{M_}(end+1)=eidx{M_}(end)+1;
                eidx{N_}(end+1)=eidx{N_}(end)+1;
                x=transpose(FI.Domain(d).XCorner+((1:FI.Domain(d).NRows+1)-1.5)*FI.Domain(d).XCellSize)*ones(1,FI.Domain(d).NCols+1);
                y=ones(FI.Domain(d).NRows+1,1)*(FI.Domain(d).YCorner+(-0.5:1:(FI.Domain(d).NCols-0.5))*FI.Domain(d).YCellSize);
                if ~muldom
                    x=x(eidx{[M_ N_]});
                    y=y(eidx{[M_ N_]});
                end
            else
                x=transpose(FI.Domain(d).XCorner+((1:FI.Domain(d).NRows)-1)*FI.Domain(d).XCellSize)*ones(1,FI.Domain(d).NCols);
                y=ones(FI.Domain(d).NRows,1)*(FI.Domain(d).YCorner+(0:(FI.Domain(d).NCols-1))*FI.Domain(d).YCellSize);
                if ~muldom
                    x=x(idx{[M_ N_]});
                    y=y(idx{[M_ N_]});
                end
            end
            if iscell(Val1) % select domain if necessary
                val1=Val1{d};
            else
                val1=Val1;
            end
            val1(val1==0)=NaN;
            if ~muldom
                val1=val1(idx{[M_ N_]});
                val1=reshape(val1,[1 size(val1)]);
                val2=[];
            else
                Ans(d).X=x;
                Ans(d).Y=y;
                Ans(d).Val=val1;
            end
        end

        % read time ...
        T=readtim(FI,domain,Props,idx{T_});
    case 'FLS-bin'
        x=[];
        y=[];
        val1=fls('bin',FI,Props.SubFld1,idx{ST_},idx{T_});
        val2=[];
        if ~isempty(Props.SubFld2)
            val2=fls('bin',FI,Props.SubFld2,idx{ST_},idx{T_});
        end

        % read time ...
        T=readtim(FI,domain,Props,idx{T_});
    case 'FLS-his'
        x=[];
        y=[];
        data=fls('his',FI,idx{ST_});
        val1=data(idx{T_},Props.SubFld1);
        val2=[];
        if ~isempty(Props.SubFld2)
            val2=data(idx{T_},Props.SubFld2);
        end

        % extract time ...
        T=data(idx{T_},1)/24;
    case 'FLS-cross'
        x=[];
        y=[];
        data=fls('cross',FI,idx{ST_});
        val1=data(idx{T_})';
        val2=[];

        % read time ...
        T=readtim(FI,domain,Props,idx{T_});
end


%========================= GENERAL CODE =======================================

% reshape if a single timestep is selected ...
if DimFlag(ST_)
    sz=[size(val1) 1]; sz(2)=[];
    if isempty(val2)
        val1=reshape(val1,sz);
    else
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
end

% reshape if a single timestep is selected ...
if DimFlag(T_) & isequal(size(idx{T_}),[1 1]) & ~muldom
    sz=size(val1); sz=[sz(2:end) 1];
    if isempty(val2)
        val1=reshape(val1,sz);
    else
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
end

% generate output ...
if ~muldom
    if XYRead
        Ans.X=x;
        Ans.Y=y;
        Ans.XUnit = 'm';
        Ans.YUnit = 'm';
    end
    if isempty(val2)
        Ans.Val=val1;
    else
        Ans.XComp=val1;
        Ans.YComp=val2;
    end
end
[Ans(:).Time]=deal(T);

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Domains=domains(FI);
switch FI.FileType,
    case 'FLS-inc'
        if length(FI.Domain)>1
            Domains={FI.Domain.Id};
            Domains{end+1}='all domains';
        else
            Domains={};
        end
    otherwise
        Domains={};
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'              'Units'  'DimFlag' 'DataInCell' 'NVal' 'VecType' 'SubFld1' 'SubFld2'};
switch FI.FileType,
    case 'FLS-inc'
        DataProps={'classified'      '-'         [4 0 1 1 0]  1         1     ''         []        []};
    case 'FLS-bin'
        DataProps={'waterdepth'      'm'         [3 5 0 0 0]  1         1     ''         'H'        []
            'waterlevel'      'm'         [3 5 0 0 0]  1         1     ''         'S'        []
            'velocity'        'm/s'       [3 5 0 0 0]  1         2     ''         'U'       'V'};
    case 'FLS-his'
        DataProps={'waterdepth'      'm'         [3 5 0 0 0]  1         1     ''          2         []
            'waterlevel'      'm'         [3 5 0 0 0]  1         1     ''          4         []
            'velocity'        'm/s'       [3 5 0 0 0]  1         2     ''          5         6 };
    case 'FLS-cross'
        DataProps={'discharge'       'm^3/s'     [3 5 0 0 0]  1         1     ''          []        []};
    otherwise
        DataProps={''                ''          [0 0 0 0 0]  0         1     ''          []        []};
        Out=cell2struct(DataProps,PropNames,2);
        Out(1,:)=[];
        return
end
%======================== SPECIFIC CODE DIMENSIONS ============================

Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE REMOVE ================================

%======================== SPECIFIC CODE ADD ===================================
switch FI.FileType,
    case 'FLS-inc'
        X=Out;
        if isfield(FI,'StartTime')
            X.DimFlag(T_)=1;
        end
        k=0;
        Strs={'waterdepth','velocity','waterlevel','u-velocity','v-velocity'};
        for j=1:length(FI.Quant)
            if ~isempty(FI.Quant(j).Class)
                k=k+1;
                Out(k)=X;
                Out(k).Name=[Strs{j} ' class'];
                Out(k).SubFld1=[];
                Out(k).SubFld2=j;
                if domain<1 | domain>length(FI.Domain)
                    Out(k).DimFlag([M_ N_])=inf;
                end
            end
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,domain,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
switch FI.FileType,
    case 'FLS-inc'
        sz(T_)=round(FI.End*60); % time step: one minute
        %
        % Backward compatible addition of Domain argument ...
        %
        if ~isempty(Props.SubFld1)
            domain=Props.SubFld1;
        end
        if domain>0 & domain<=length(FI.Domain)
            sz(M_)=FI.Domain(domain).NRows;
            sz(N_)=FI.Domain(domain).NCols;
        else
            sz(M_)=999;
            sz(N_)=999;
        end
    case 'FLS-bin'
        sz(T_)=FI.NumTimes;
        sz(ST_)=FI.NumSta;
    case 'FLS-his'
        sz(T_)=min(FI.NTimes);
        sz(ST_)=FI.NumSta;
    case 'FLS-cross'
        sz(T_)=FI.NumTimes;
        sz(ST_)=size(FI.M,1);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,domain,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
switch FI.FileType,
    case 'FLS-inc'
        if isfield(FI,'StartTime')
            t0=datenum(FI.StartTime(1),FI.StartTime(2),FI.StartTime(3),FI.StartTime(4),FI.StartTime(5),FI.StartTime(6));
        else
            t0=1;
        end
        if isequal(t,0),
            sz=getsize(FI,domain,Props);
            T=t0+transpose(1:sz(T_))/(24*60);
        else
            T=t0+t(:)/(24*60);
        end
    case 'FLS-bin'
        T=fls('bin',FI,'T',t)/24;
    case 'FLS-his'
        T=fls('his',FI,1);
        T=T(:,1)/24;
        if ~isequal(t,0)
            T=T(t);
        end
    case 'FLS-cross'
        T=fls('cross',FI,0);
        T=T(:)/24;
        if ~isequal(t,0)
            T=T(t);
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,domain,Props,t)
S={};
%======================== SPECIFIC CODE =======================================
switch FI.FileType,
    case 'FLS-inc'
        S={};
    case {'FLS-bin','FLS-his'}
        S={};
        for i=1:FI.NumSta,
            S{i}=sprintf('(%i,%i)',FI.M(i),FI.N(i));
        end
    case 'FLS-cross'
        S={};
        for i=1:size(FI.M)
            S{i}=sprintf('(%i,%i)-(%i,%i)',FI.M(i,1),FI.N(i,1),FI.M(i,2),FI.N(i,2));
        end
end

S=cellstr(S);
% -----------------------------------------------------------------------------
