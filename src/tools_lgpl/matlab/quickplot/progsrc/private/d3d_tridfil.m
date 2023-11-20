function varargout=d3d_tridfil(FI,domain,field,cmd,varargin)
%D3D_TRIDFIL QP support for Delft3D-FLOW drogue output files.
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
sz=getsize(FI,Props);
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
end

%========================= GENERAL CODE =======================================
if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

elidx=idx(2:end);
elidx(~DimFlag(2:end))=[];

% read grid ...
x=[];
y=[];
z=[];
if XYRead

    %======================== SPECIFIC CODE =======================================
    [x,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,{0 elidx{1}},'quiet');
    szx=size(x);
    if length(szx)==2
        szx=[szx(1) 1];
    else
        szx=szx([1 3]);
    end
    plsmisval = single(999.999);
    negmisval = single(-999.999);
    switch Props.Val1
        case 'XYDRO'
            y=reshape(x(:,2,:),szx);
            x=reshape(x(:,1,:),szx);
        case 'XYZTRK'
            z=reshape(x(:,3,:),szx);
            y=reshape(x(:,2,:),szx);
            x=reshape(x(:,1,:),szx);
            x((x == plsmisval & y == plsmisval) | (x == negmisval & y == negmisval) | (x==0 & y==0))=NaN;
            y(isnan(x))=NaN;
            z(isnan(x))=NaN;
    end
    %========================= GENERAL CODE =======================================
end

% load data ...
if DataRead
    val1=[]; val2=[];
    switch Props.Name
        case {'drogue velocity'}
            val1=x;
            val2=y;
            val1=gradient(val1);
            val2=gradient(val2);
    end
else
    Props.NVal=0;
end
% read time ...
T=readtim(FI,Props,idx{T_});

%========================= GENERAL CODE =======================================

% reshape if a single timestep is selected ...
if DimFlag(ST_)
    sz=[size(val1) 1]; sz(2)=[];
    switch Props.NVal
        case 1
            val1=reshape(val1,sz);
        case 2
            val1=reshape(val1,sz);
            val2=reshape(val2,sz);
    end
end

% reshape if a single timestep is selected ...
if DimFlag(T_) & isequal(size(idx{T_}),[1 1])
    sz=size(val1);
    sz=[sz(2:end) 1];
    switch Props.NVal
        case 1
            val1=reshape(val1,sz);
        case 2
            val1=reshape(val1,sz);
            val2=reshape(val2,sz);
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    Ans.XUnits='m';
    Ans.YUnits='m';
    Info=vs_disp(FI,Props.Group,Props.Val1);
    if isfield(Info,'ElmUnits') & isequal(Info.ElmUnits,'[  DEG  ]')
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    end
    if ~isempty(z)
        Ans.Z=z;
        Ans.ZUnits='m';
    end
end
switch Props.NVal
    case 1
        Ans.Val=val1;
    case 2
        Ans.XComp=val1;
        Ans.YComp=val2;
end
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'   'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'     'Val2'    'SubFld' 'MNK'};
DataProps={'drogue track'              ''     'PNT'  'xy'    [1 6 0 0 0]  0         0     ''       'z'   'z'       ''      'dro-series'     'XYDRO'    ''         []       0
    'particle track'            ''     'PNT'  'xyz'   [1 6 0 0 0]  0         0     ''       'z'   'z'       ''      'trk-series'     'XYZTRK'   ''         []       0};
%          'drogue velocity'           ''     'PNT'  'xy'    [1 5 0 0 0]  0         2     ''       'z'   'z'       ''      'dro-series'     'XYDRO'    ''         []       0};
Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE REMOVE ================================
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isstruct(Info)
        % remove references to non-stored data fields
        Out(i,:)=[];
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
%if Props.DimFlag(M_) & Props.DimFlag(N_)
%  Info=vs_disp(FI,'his-const','XCOR');
%  sz([N_ M_])=Info.SizeDim;
%end;
if Props.DimFlag(ST_)
    Info=vs_disp(FI,'dro-const','NAMDRO');
    if isstruct(Info)
        sz(ST_)=Info.SizeDim;
    else
        Info=vs_disp(FI,'trk-const','NAMTRK');
        sz(ST_)=Info.SizeDim;
    end
end
if Props.DimFlag(T_)
    Info=vs_disp(FI,Props.Group,[]);
    sz(T_)=Info.SizeDim;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
DRO='DRO';
switch vs_type(FI)
    case 'Delft3D-track'
        DRO='TRK';
end
dro=lower(DRO);
[Dt,Chk]=vs_get(FI,[dro '-const'],'DT','quiet');
[Tunit,Chk]=vs_get(FI,[dro '-const'],'TUNIT','quiet');
[Date,Chk]=vs_get(FI,[dro '-const'],'ITDATE','quiet');
d0=tdelft3d(Date(1),Date(2));
switch Props.Group,
    case {'dro-series','trk-series'}
        [T,Chk]=vs_let(FI,[dro '-info-series'],{t},['IT' DRO 'C'],'quiet');
        T=d0+T*Dt*Tunit/(24*3600);
    otherwise % ONE FIELD
        T=d0;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)

%======================== SPECIFIC CODE =======================================
switch vs_type(FI)
    case 'Delft3D-track'
        [S,Chk]=vs_get(FI,'trk-const','NAMTRK','quiet');
    otherwise
        [S,Chk]=vs_get(FI,'dro-const','NAMDRO','quiet');
end
if t~=0
    S=S(t,:);
end
S=cellstr(S);
% -----------------------------------------------------------------------------
