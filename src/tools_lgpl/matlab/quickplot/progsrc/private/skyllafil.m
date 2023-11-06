function varargout=skyllafil(FI,domain,field,cmd,varargin)
%SKYLLAFIL QP support for Skylla files.
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

% select appropriate spatial indices ...

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
            %    elseif i~=K_
            %      idx{i}=[max(1,idx{i}(1)-1) idx{i}];
        end
    end
end

% load data ...
if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end;

if DimFlag(ST_)
    [Station,Chk]=vs_let(FI,'RAAINAMES',{idx{ST_}},'NAME','quiet');
    Station=deblank(Station);
end

% read grid ...
x=[];
y=[];
z=[];
if XYRead
    %======================== SPECIFIC CODE =======================================
    if DimFlag(M_) && DimFlag(K_)
        [x,Chk]=vs_let(FI,'GEOMETRY','COOR_Xc','quiet');
        [y,Chk]=vs_let(FI,'GEOMETRY','COOR_Yc','quiet');
        x=x(idx{[M_ K_]});
        y=y(idx{[M_ K_]});
        x=reshape(x,[size(x,1) 1 size(x,2)]);
        y=reshape(y,[size(y,1) 1 size(y,2)]);
    else
        [x,Chk]=vs_let(FI,Station,{idx{K_} 1},'X-coord','quiet');
        [y,Chk]=vs_let(FI,Station,{idx{K_} 1},'Y-coord','quiet');
        x=reshape(x,[1 1 size(x,1)]);
        y=reshape(y,[1 1 size(y,1)]);
    end

    %========================= GENERAL CODE =======================================
end

%================== NEFIS SPECIFIC CODE =======================================
elidx=idx(2:end);
elidx(~DimFlag(2:end))=[];
if DimFlag(ST_)
    [val1,Chk]=vs_let(FI,Props.Group1,{elidx{2} idx{T_}},Props.Val1,'quiet');
    val1=val1';
elseif isequal(size(elidx{1}),[1 1]) && isequal(size(elidx{2}),[1 1])
    [val1,Chk]=vs_let(FI,Props.Group1,{elidx{:} idx{T_}},Props.Val1,'quiet');
    val1=reshape(val1,size(val1,3),1,1);
else
    [val1,Chk]=vs_let(FI,Props.Group1,{0 0 idx{T_}},Props.Val1,'quiet');
    val1=val1(elidx{:},:); val1(1,1,1,2)=1;
    val1=permute(val1,[3 1 2 4]); %This statement is bugged in R13 due to JIT acceleration
    val1(:,:,:,2)=[];
    val1=reshape(val1,[size(val1,1) size(val1,2) 1 size(val1,3)]);
end

if isempty(Props.Group2)
    val2=[];
else
    if DimFlag(ST_)
        [val2,Chk]=vs_let(FI,Props.Group1,{elidx{2} idx{T_}},Props.Val1,'quiet');
        val2=val2';
    elseif isequal(size(elidx{1}),[1 1]) && isequal(size(elidx{2}),[1 1])
        [val2,Chk]=vs_let(FI,Props.Group2,{elidx{:} idx{T_}},Props.Val2,'quiet');
        val2=reshape(val2,size(val2,3),1,1);
    else
        [val2,Chk]=vs_let(FI,Props.Group2,{0 0 idx{T_}},Props.Val2,'quiet');
        val2=val2(elidx{:},:); val2(1,1,1,2)=1;
        val2=permute(val2,[3 1 2 4]); %This statement is bugged in R13 due to JIT acceleration
        val2(:,:,:,2)=[];
        val2=reshape(val2,[size(val2,1) size(val2,2) 1 size(val2,3)]);
    end
end

% combine vectors components ...
if isequal(Props.VecType,'u')
    val1=(val1(:,[1 1:end-1],1,:)+val1)/2;
    val2=(val2(:,:,1,[1 1:end-1])+val2)/2;
end

%======================== SPECIFIC CODE =======================================
% filter only vector quantities based on fraction ...
if XYRead && ~isempty(val2) && ~DimFlag(ST_)
    if isequal(size(elidx{1}),[1 1]) && isequal(size(elidx{2}),[1 1])
        [act,Chk]=vs_let(FI,'F_SURFACE',{elidx{:} idx{T_}},'Fij','quiet');
        act=reshape(act,size(act,3),1,1);
    else
        [act,Chk]=vs_let(FI,'F_SURFACE',{0 0 idx{T_}},'Fij','quiet');
        act=act(elidx{:},:); act(1,1,1,2)=1;
        act=permute(act,[3 1 2 4]); %This statement is bugged in R13 due to JIT acceleration
        act(:,:,:,2)=[];
        act=reshape(act,[size(act,1) size(act,2) 1 size(act,3)]);
    end
    x(act<=0)=NaN;
    y(act<=0)=NaN;
end
%========================= GENERAL CODE =======================================
val1(:,isnan(x))=NaN;
if ~isempty(val2)
    val2(:,isnan(x))=NaN;
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
                %        if i~=K_
                %          ind{i}=(1:(length(idx{i})-1))+(idx{i}(1)~=idx{i}(2));
                %        else % i==K_
                ind{i}=1:length(idx{i});
                %        end
            else
                ind{i}=idx{i};
            end
        end
    end
    %  if XYRead
    %    if DimFlag(M_) & DimFlag(N_)
    %      x=x(ind{[M_ N_]});
    %      y=y(ind{[M_ N_]});
    %    end
    %  end
    DimMask=[0 1 1 1 1];
    ind=ind(DimMask & DimFlag);
    if isempty(val2)
        val1=val1(:,ind{:});
    else
        val1=val1(:,ind{:});
        val2=val2(:,ind{:});
    end
end

%========================= GENERAL CODE =======================================

% reshape if a single timestep is selected ...
if DimFlag(T_) && isequal(size(idx{T_}),[1 1])
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
    Ans.Z=y;
    I = vs_disp(FI,'GEOMETRY','COOR_Xc');
    Ans.XUnits = I.ElmUnits(2:end-1);
    I = vs_disp(FI,'GEOMETRY','COOR_Yc');
    Ans.ZUnits = I.ElmUnits(2:end-1);
end
if isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.ZComp=val2;
end
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                       'Units' 'DimFlag'   'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Group1'    'Val1'       'Group2'    'Val2'       'SubFld'};
DataProps={'cell fill rate'             ''       [5 0 1 0 1]      0         1     ''        'z'   'z'      'F_SURFACE' 'Fij'        ''          ''           []
    'pressure/density'          'm^2/s^2' [5 0 1 0 1]      0         1     ''        'z'   'z'      'PRESSURE'  'Pij'        ''          ''           []
    'rotation'                   '1/s'    [5 0 1 0 1]      0         1     ''        'z'   'z'      'ROTATION'  'Rij'        ''          ''           []
    'velocity'                   'm/s'    [5 0 1 0 1]      0         'xz'  'u'       ''    'z'      'U_VELOC'   'Uij'        'V_VELOC'   'Vij'        []
    '-------'                    ''       [0 0 0 0 0]      0         0     ''        ''    ''       ''          ''           ''          ''           []
    'pressure at station'        'Pa'     [3 5 0 0 1]      0         1     ''        'z'   'z'      'raai'      'Pressure'   ''          ''           []
    'velocity at station'        'm/s'    [3 5 0 0 1]      0         'xz'  ''        'z'   'z'      'raai'      'U-velocity' 'raai'      'V-velocity' []};

%======================== SPECIFIC CODE DIMENSIONS ============================

Out=cell2struct(DataProps,PropNames,2);

Info=vs_disp(FI,'RAAINAMES','NAME');
if isstruct(Info)
    ri=deblank(vs_get(FI,'RAAINAMES',{1},'NAME','quiet')');
    for i=1:size(Out,1)
        if strcmp(Out(i).Group1,'raai')
            Out(i).Group1=ri;
            if strcmp(Out(i).Group2,'raai')
                Out(i).Group2=ri;
            end
        end
    end
end
%======================== SPECIFIC CODE REMOVE ================================
for i=size(Out,1):-1:1
    if ~strcmp(Out(i).Name,'-------')
        Info1=vs_disp(FI,Out(i).Group1,Out(i).Val1);
        if ~isempty(Out(i).Group2)
            Info2=vs_disp(FI,Out(i).Group2,Out(i).Val2);
        else
            Info2=[];
        end
        if ~isstruct(Info1)
            % remove references to non-stored data fields
            Out(i,:)=[];
        elseif ~isempty(Out(i).Group2) & ~isstruct(Info2)
            % remove references to non-stored data fields
            Out(i,:)=[];
        else
            switch Out(i).Name,
                case 'raai',
            end
        end
    end
end

%======================== SPECIFIC CODE ADD ===================================
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(ST_)
    Info=vs_disp(FI,Props.Group1,[]);
    sz(T_)=Info.SizeDim(2);
    sz(K_)=Info.SizeDim(1);
    Info=vs_disp(FI,'RAAINAMES',[]);
    sz(ST_)=Info.SizeDim(1);
elseif Props.DimFlag(M_)
    Info=vs_disp(FI,Props.Group1,[]);
    sz(T_)=Info.SizeDim(3);
    sz(M_)=Info.SizeDim(1);
    sz(K_)=Info.SizeDim(2);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(ST_)
    [T,Chk]=vs_let(FI,Props.Group1,{1 t},'Time','quiet');
    T=1+T/24/3600;
else
    if isequal(t,0),
        sz=getsize(FI,Props);
        T=(1:sz(T_))';
    else
        T=t;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)

%======================== SPECIFIC CODE =======================================
[S,Chk]=vs_let(FI,'RAAINAMES','NAME','quiet');
S=cellstr(S);
% -----------------------------------------------------------------------------
