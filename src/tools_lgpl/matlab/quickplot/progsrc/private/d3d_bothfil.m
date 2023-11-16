function varargout=d3d_bothfil(FI,domain,field,cmd,varargin)
%D3D_BOTHFIL QP support for Delft3D-MOR bottom module history files.
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
        return;
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

    elidx(~DimFlag(2:end))=[];
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
    end

    %======================== SPECIFIC CODE =======================================
    % select active points ...
    %   always active
    %========================= GENERAL CODE =======================================
else
    Props.NVal=0;
end
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
DataProps={'bedload sediment transport' 'm^3/m' [1 5 0 0 0]  0         2     ''       'z'   'z'       ''      'HISBOTTIM'      'TXHIS'    'TYHIS'    []       0
    'entrainment'                'm'     [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISBOTTIM'      'ENHIS'    ''         []       0
    '-------'                    ''      [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'bed level'                  'm'     [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'HISBOTTIM'      'DPHIS'    ''         []       0};
Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE REMOVE ================================
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info)
        % remove references to non-stored data fields
        Out(i)=[];
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
    Info=vs_disp(FI,Props.Group,Props.Val1);
    sz(ST_)=Info.SizeDim;
end
if Props.DimFlag(T_)
    Info=vs_disp(FI,Props.Group,[]);
    sz(T_)=Info.SizeDim;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[tscale,Chk]=vs_get(FI,'HISBOT','TSCALE','quiet');
%d0=tdelft3d(pars.IT01,pars.IT02);
d0=1; % no reference date in BOTH-file
switch Props.Group,
    case 'HISBOTTIM'
        [T,Chk]=vs_let(FI,'HISBOTTIM',{t},'ITBODE','quiet');
        T=d0+T*tscale/(24*3600);
    otherwise, % ONE FIELD
        T=d0;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)

%======================== SPECIFIC CODE =======================================
[S,Chk]=vs_get(FI,'HISBOT','NAMSTD','quiet');
S=cellstr(S);
% -----------------------------------------------------------------------------
