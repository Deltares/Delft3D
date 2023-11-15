function varargout=d3d_trahfil(FI,domain,field,cmd,varargin)
%D3D_TRAHFIL QP support for Delft3D-MOR transport history files.
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

%================== NEFIS SPECIFIC CODE =======================================
if DimFlag(M_)& DimFlag(N_)
    sz([M_ N_])=sz([N_ M_]);
    idx([M_ N_])=idx([N_ M_]);
end

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0)
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(size(idx{i}),[1 1])
            error('Only scalars allowed for index %i',i)
        else
            idx{i}=[max(1,idx{i}-1) idx{i}];
        end
    end
end

% read grid ...
x=[];
y=[];
z=[];
if XYRead

    %======================== SPECIFIC CODE =======================================

    %========================= GENERAL CODE =======================================
end

% grid interpolation in z direction ...

% load data ...
if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end;

if DataRead
    %================== NEFIS SPECIFIC CODE =======================================
    elidx=idx(2:end);
    switch Props.Name
        case {'concentration'}
            elidx(end+1)={1}; % third dimension automatically dropped after reading
    end
    elidx(~DimFlag(2:end))=[];

    if ~isempty(Props.SubFld) % sed.fraction
        elidx(end+1)={Props.SubFld}; % extra dimension automatically dropped after reading
    end

    [val1,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,elidx,'quiet');
    if isempty(Props.Val2)
        val2=[];
    else
        [val2,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val2,elidx,'quiet');
    end

    switch Props.Name
        case {'bed level'}
            val1=-val1;
    end

    if DataInCell & isequal(Props.ReqLoc,'d')
        Props.ReqLoc='z';
    end
    % combine vectors components ...
    if isequal(Props.VecType,'m')
        [val1,val2]=dir2uv(val1,val2);
    end
    % data interpolation ...
    if isequal(Props.Loc,'d') & isequal(Props.ReqLoc,'z')
        val1=interp2cen(val1,'t');
        if ~isempty(val2)
            val2=interp2cen(val2,'t');
        end
    elseif isequal(Props.Loc,'u') & isequal(Props.ReqLoc,'z')
        [val1,val2]=uv2cen(val1,val2);
    end
    % combine vectors components ...
    if isequal(Props.VecType,'u') & Props.MNK<=1
        % rotate n,m components into x,y direction ...
        %  [alf,Chk] = vs_get(FI,'his-const','ALFAS',idx(ST_),'quiet');
        %  alf = alf*pi/180;
        %  [val1,val2]=cur2ca(val1,val2,alf);
    end
else
    Props.NVal=0;
end
%======================== SPECIFIC CODE =======================================
% select active points ...
%   always active
%========================= GENERAL CODE =======================================
%val(act<=0)=NaN;
%if XYRead
%  x(act<=0)=NaN;
%  y(act<=0)=NaN;
%end

% read time ...
T=readtim(FI,Props,idx{T_});

% select subrange if necessary ... M,N,K only
DimMask=[0 0 1 1 1];
if ~all(allidx(DimMask & DimFlag))
    ind=cell(1,5);
    ind{2}=1;
    for i=[M_ N_ K_]
        if DimFlag(i)
            if ~allidx(i)
                ind{i}=1+(idx{i}(1)~=idx{i}(2));
            else
                ind{i}=idx{i};
            end
        end
    end
    if XYRead
        if DimFlag(M_) & DimFlag(N_)
            x=x(ind{[M_ N_]});
            y=y(ind{[M_ N_]});
        end
    end
    DimMask=[0 1 1 1 1];
    ind=ind(DimMask & DimFlag);
    if isempty(val2)
        val1=val1(:,ind{:});
    else
        val1=val1(:,ind{:});
        val2=val2(:,ind{:});
    end
end

%================== NEFIS SPECIFIC CODE =======================================
% permute n and m dimensions into m and n if necessary
if DimFlag(M_) & DimFlag(N_)
    perm=[2 1 3];
    if XYRead
        x=permute(x,perm);
        y=permute(y,perm);
    end
    if isempty(val2)
        val1=permute(val1,[1 1+perm]);
    else
        val1=permute(val1,[1 1+perm]);
        val2=permute(val2,[1 1+perm]);
    end
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
if DimFlag(T_) & isequal(size(idx{T_}),[1 1])
    sz=size(val1); sz=[sz(2:end) 1];
    if isempty(val2)
        val1=reshape(val1,sz);
    else
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
end
if Props.NVal==0
elseif Props.NVal==1
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'     'Val2'    'SubFld' 'MNK'};
DataProps={'water level'               'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'ZSL'      ''         []       0
    'velocity'                  'm/s'    [1 5 0 0 1]  0         2     ''       'z'   'z'       ''      'HISTRAN'        'ZU'       'ZV'       []       0
    'u roughness Chezy C'    'm^{1/2}/s' [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'ZCZU'     ''         []       0
    'v roughness Chezy C'    'm^{1/2}/s' [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'ZCZV'     ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'sediment transport of a fraction' ...
    'm^3/sm' [1 5 0 0 0]  0         2     ''       'z'   'z'       ''      'HISTRAN'        'ZSEDX'    'ZSEDY'    []       0
    'sediment transport magnitude of a fraction' ...
    'm^3/sm' [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'ZSEDR'    ''         []       0
    'sediment transport'        'm^3/sm' [1 5 0 0 0]  0         2     ''       'z'   'z'       ''      'HISTRAN'        'ZSEDTX'   'ZSEDTY'   []       0
    'transport magnitude'       'm^3/sm' [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'ZSEDTR'   ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'concentration'            'm^3/m^3' [1 5 0 0 1]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'CONCED'   ''         []       0
    'bed level'                 'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'ZZB'      ''         []       0
    'source term'             'm^3/m^3s' [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'RLSILS'   ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'transport layer'           '-'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'PTRLA'    ''         []       0
    'exchange layer'            '-'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'PEXLA'    ''         []       0
    'underlayer'                '-'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'P0LA'     ''         []       0
    'transport layer thickness' 'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'DDEF'     ''         []       0
    'D10'                       'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'D10AR'    ''         []       1
    'D50'                       'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'D50AR'    ''         []       1
    'D90'                       'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'D90AR'    ''         []       1
    'median diameter'           'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAG'        'DMEDAR'   ''         []       1
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'U cross-sec. sed. transport of a fraction' ...
    'm^3/s'  [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'SXRA'     ''         []       0
    'U cross-sec. sed. transport' ...
    'm^3/s'  [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'STXRA'    ''         []       0
    'V cross-sec. sed. transport of a fraction' ...
    'm^3/s'  [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'SYRA'     ''         []       0
    'V cross-sec. sed. transport' ...
    'm^3/s'  [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISTRAN'        'STYRA'    ''         []       0};
Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE REMOVE ================================
[N,Chk]=vs_get(FI,'HISLTRAB','*','quiet');
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info)
        % remove references to non-stored data fields
        Out(i)=[];
    else
        switch Out(i).Val1
            case {'ZSEDX','ZSEDR','ZSL','ZZB','ZU','ZCZU','ZCZV'}
                if N.NOSED==0, Out(i)=[]; end
            case {'ZSEDTX','ZSEDTR'}
                if N.NOSEDT==0, Out(i)=[]; end
            case {'SXRA'}
                if N.NTRAU==0, Out(i)=[]; end
            case {'SYRA'}
                if N.NTRAV==0, Out(i)=[]; end
            case {'STXRA'}
                if N.NTRAUT==0, Out(i)=[]; end
            case {'STYRA'}
                if N.NTRAVT==0, Out(i)=[]; end
            case {'CONSED'}
                if N.KPOL==0, Out(i)=[]; end
            case {'RLSILS'}
                if N.NOSIL==0, Out(i)=[]; end
            case {'PTRLA','PEXLA','P0LA','DDEF','D10AR','D50AR','D90AR','DMEDAR'}
                if Info.SizeDim(1)==0
                    Out(i)=[];
                end
        end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if ~isempty(strmatch(Props.Name,{'sediment transport of a fraction','sediment transport magnitude of a fraction', ...
        'sediment transport','transport magnitude','U cross-sec. sed. transport of a fraction', ...
        'V cross-sec. sed. transport of a fraction','transport layer','exchange layer','underlayer'},'exact'))
    Info=vs_disp(FI,Props.Group,Props.Val1);
    if isfield(Info,'NDim') & (Info.NDim>=2)
        subf=cellstr(multiline(sprintf('fraction %i\n',1:Info.SizeDim(2))));
        subf=subf(1:end-1);
    end
end
if nargin>2 & f~=0
    subf=subf(f);
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
Info=vs_disp(FI,Props.Group,Props.Val1);
switch Props.Val1
    case {'ZSEDX','ZSEDR','ZSL','ZZB','ZU','ZCZU','ZCZV'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','NOSED','quiet');
    case {'ZSEDTX','ZSEDTR'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','NOSEDT','quiet');
    case {'SXRA'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','NTRAU','quiet');
    case {'SYRA'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','NTRAV','quiet');
    case {'STXRA'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','NTRAUT','quiet');
    case {'STYRA'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','NTRAVT','quiet');
    case {'CONSED'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','KPOL','quiet');
    case {'RLSILS'}
        [sz(ST_),Chk]=vs_get(FI,'HISLTRAB','NOSIL','quiet');
    case {'PTRLA','PEXLA','P0LA','DDEF','D10AR','D50AR','D90AR','DMEDAR'}
        sz(ST_)=Info.SizeDim(1);
end
if Props.DimFlag(K_)
    sz(K_)=Info.SizeDim(2);
end
if Props.DimFlag(T_)
    Info=vs_disp(FI,Props.Group,[]);
    sz(T_)=Info.SizeDim;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[pars,Chk]=vs_get(FI,'HISLTRAB','*','quiet');
d0=tdelft3d(pars.ITO1,pars.ITO2);
switch Props.Group,
    case {'HISTRAN','HISTRAG'}
        [T,Chk]=vs_let(FI,'HISTRAN',{t},'HISTIME','quiet');
        T=d0+T*pars.TSCALE/(24*3600);
    otherwise, % ONE FIELD
        T=d0;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)

%======================== SPECIFIC CODE =======================================
switch Props.Val1
    case {'ZSEDX','ZSEDR','ZSL','ZZB','ZU','ZCZU','ZCZV'}
        [M,Chk]=vs_get(FI,'HISLTRAN','MC','quiet');
        [N,Chk]=vs_get(FI,'HISLTRAN','NC','quiet');
        %  [L,Chk]=vs_get(FI,'HISLTRAN','LC','quiet');
        %  S=sprintf('(%i,%i) fraction %i\n',transpose([M N L]));
        S=sprintf('(%i,%i)\n',transpose([M N]));
    case {'ZSEDTX','ZSEDTR'}
        [M,Chk]=vs_get(FI,'HISLTRAN','MCT','quiet');
        [N,Chk]=vs_get(FI,'HISLTRAN','NCT','quiet');
        S=sprintf('(%i,%i)\n',transpose([M N]));
    case {'SXRA'}
        [M,Chk] =vs_get(FI,'HISLTRAN','MITX','quiet');
        [N1,Chk]=vs_get(FI,'HISLTRAN','NIT1','quiet');
        [N2,Chk]=vs_get(FI,'HISLTRAN','NIT2','quiet');
        [L,Chk] =vs_get(FI,'HISLTRAN','LITX','quiet');
        S=sprintf('(%i,%i)-(%i,%i)\n',transpose([M N1 M N2]));
    case {'SYRA'}
        [N,Chk] =vs_get(FI,'HISLTRAN','NITY','quiet');
        [M1,Chk]=vs_get(FI,'HISLTRAN','MIT1','quiet');
        [M2,Chk]=vs_get(FI,'HISLTRAN','MIT2','quiet');
        [L,Chk] =vs_get(FI,'HISLTRAN','LITY','quiet');
        S=sprintf('(%i,%i)-(%i,%i)\n',transpose([M1 N M2 N]));
    case {'STXRA'}
        [M,Chk] =vs_get(FI,'HISLTRAN','MITTY','quiet');
        [N1,Chk]=vs_get(FI,'HISLTRAN','NITT1','quiet');
        [N2,Chk]=vs_get(FI,'HISLTRAN','NITT2','quiet');
        S=sprintf('(%i,%i)-(%i,%i)\n',transpose([M N1 M N2]));
    case {'STYRA'}
        [N,Chk] =vs_get(FI,'HISLTRAN','NITTY','quiet');
        [M1,Chk]=vs_get(FI,'HISLTRAN','MITT1','quiet');
        [M2,Chk]=vs_get(FI,'HISLTRAN','MITT2','quiet');
        S=sprintf('(%i,%i)-(%i,%i)\n',transpose([M1 N M2 N]));
    case {'CONSED'}
        [M,Chk] =vs_get(FI,'HISLTRAN','MPOL','quiet');
        [N,Chk] =vs_get(FI,'HISLTRAN','NPOL','quiet');
        S=sprintf('(%i,%i)\n',transpose([M N]));
    case {'RLSILS'}
        [M,Chk] =vs_get(FI,'HISLTRAN','MSIL','quiet');
        [N,Chk] =vs_get(FI,'HISLTRAN','NSIL','quiet');
        S=sprintf('(%i,%i)\n',transpose([M N]));
    otherwise
        S={};
end
if ~iscell(S)
    S=multiline(S(1:end-1));
    S=cellstr(S);
end
% -----------------------------------------------------------------------------
