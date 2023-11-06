function varargout=d3d_tramfil(FI,domain,field,cmd,varargin)
%D3D_TRAMFIL QP support for Delft3D-MOR transport map files.
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
        varargout={{}};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;
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
ind=cell(1,5);
ind{2}=1;
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) | isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
        if i~=K_
            idx{i} = [max(1,idx{i}(1)-1) idx{i}];
            ind{i}=2:length(idx{i});
        else % i==K_
            ind{i}=1:length(idx{i});
        end
    end
end

% read grid ...
x=[];
y=[];
z=[];
if XYRead

    %======================== SPECIFIC CODE =======================================
    if DimFlag(M_) & DimFlag(N_)
        [x,Chk]=vs_get(FI,'GRID','XCOR',idx([M_ N_]),'quiet');
        [y,Chk]=vs_get(FI,'GRID','YCOR',idx([M_ N_]),'quiet');
        x((x==0) & (y==0)) = NaN;
        y(isnan(x))=NaN;
    end

    if DimFlag(K_)
        [dp,Chk]=vs_get(FI,'MAPTTRAN',{idx{T_}},'ZB',idx([M_ N_]),'quiet');
        if Chk
            dp(dp==-999)=NaN;
            dp=interp2cen(-dp);
            dp=reshape(dp,[1 size(dp)]);
            [h,Chk]=vs_let(FI,'MAPTTRAN',{idx{T_}},'SL',idx([M_ N_]),'quiet');
        end
        if ~Chk
            dp=zeros(length(idx{T_}),length(idx{M_}),length(idx{N_}));
            h=ones(length(idx{T_}),length(idx{M_}),length(idx{N_}));
        end
        if size(dp,1)==1,
            for i=1:size(h,1)
                h(i,:,:)=h(i,:,:)-dp;
            end
        else
            h=h-dp;
        end
        [thk,Chk]=vs_let(FI,'GRID','THICK','quiet');
        if strcmp(Props.Loc3D,'i')
            if DataInCell
                cthk=[0 cumsum(thk)-thk/2 1];
            else
                cthk=cumsum([0 thk]);
            end
        else % 'c'
            if DataInCell
                cthk=cumsum([0 thk]);
            else
                cthk=cumsum(thk)-thk/2;
            end
        end
        if DataInCell
            idxK_=[idx{K_} idx{K_}(end)+1];
        else
            idxK_=idx{K_};
        end
        cthk=cthk(idxK_);
        z=zeros([size(h) length(cthk)]);
        if size(dp,1)==1,
            for i=1:size(h,1)
                for k=1:length(cthk)
                    z(i,:,:,k)=dp+(1-cthk(k))*h(i,:,:);
                end
            end
        else
            for k=1:length(cthk)
                z(:,:,:,k)=dp+(1-cthk(k))*h;
            end
        end
        x=reshape(x,[1 size(x)]);
        x=repmat(x,[1 1 1 length(cthk)]);
        y=reshape(y,[1 size(y)]);
        y=repmat(y,[1 1 1 length(cthk)]);
    end
    %========================= GENERAL CODE =======================================
end

% grid interpolation ...
[x,y]=gridinterp(DataInCell,DimFlag(K_),Props.ReqLoc,x,y);

% load data ...
if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end;

if DataRead
    %================== NEFIS SPECIFIC CODE =======================================
    elidx=idx(2:end);
    switch Props.Name
        case {'depth averaged velocity'}
            elidx{end+1}=0;
        case {'velocity in depth averaged flow direction','velocity normal to depth averaged flow direction'}
            % always load 3D field to determine depth averaged flow direction
            elidx{K_-1}=0;
    end

    elidx(~DimFlag(2:end))=[];
    if ~isempty(Props.SubFld) % sed.fraction
        if iscell(Props.SubFld)
            elidx=cat(2,elidx,Props.SubFld); % last dimensions automatically dropped after reading
        else
            elidx(end+1)={Props.SubFld}; % last dimension automatically dropped after reading
        end
    end

    if Props.NVal==0
        val1=[];
    else
        [val1,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,elidx,'quiet');
    end
    if isempty(Props.Val2)
        val2=[];
    else
        [val2,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val2,elidx,'quiet');
    end

    switch Props.Name
        case {'bed level increment','time-varying bed level'}
            val1=-val1;
        case 'depth averaged velocity'
            if size(val1,4)>1,
                [thk,Chk]=vs_let(FI,'GRID','THICK','quiet');
                for k=1:length(thk)
                    val1(:,:,:,k)=val1(:,:,:,k)*thk(k);
                    val2(:,:,:,k)=val2(:,:,:,k)*thk(k);
                end
                val1=sum(val1,4);
                val2=sum(val2,4);
            end
        case 'water depth'
            [dp,Chk]=vs_let(FI,'MAPTTRAN',{idx{T_}},'ZB',elidx,'quiet');
            dp=interp2cen(-dp);
            %dp=reshape(dp,[1 size(dp)]);
            if size(dp,1)==1,
                for i=1:size(val1,1)
                    val1(i,:,:)=val1(i,:,:)-dp;
                end
            else
                val1=val1-dp;
            end
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
        [alf,Chk] = vs_get(FI,'GRID','ALFAS',idx([M_ N_]),'quiet');
        alf = alf*pi/180;
        [val1,val2]=cur2ca(val1,val2,alf);
    end

    switch Props.Name
        case {'velocity in depth averaged flow direction'}
            [thk,Chk]=vs_let(FI,'GRID','THICK','quiet');
            sz=size(val1);
            dav1=zeros(sz(1:3));
            dav2=dav1;
            for k=1:length(thk)
                dav1=dav1+val1(:,:,:,k)*thk(k);
                dav2=dav2+val2(:,:,:,k)*thk(k);
            end
            dvl=sqrt(dav1.^2+dav2.^2); dvl(dvl==0)=1;
            dav1=dav1./dvl; dav2=dav2./dvl;
            for k=1:length(thk)
                val1(:,:,:,k)=val1(:,:,:,k).*dav1+val2(:,:,:,k).*dav2;
            end
            val2=[];
        case {'velocity normal to depth averaged flow direction'}
            [thk,Chk]=vs_let(FI,'GRID','THICK','quiet');
            sz=size(val1);
            dav1=zeros(sz(1:3));
            dav2=dav1;
            for k=1:length(thk)
                dav1=dav1+val1(:,:,:,k)*thk(k);
                dav2=dav2+val2(:,:,:,k)*thk(k);
            end
            dvl=sqrt(dav1.^2+dav2.^2); dvl(dvl==0)=1;
            dav1=dav1./dvl; dav2=dav2./dvl;
            for k=1:length(thk)
                val1(:,:,:,k)=val1(:,:,:,k).*dav2-val2(:,:,:,k).*dav1;
            end
            val2=[];
    end
else
    Props.NVal=0;
end
%======================== SPECIFIC CODE =======================================
% select active points ...
switch Props.ReqLoc
    case 'd'
        [act,Chk]=vs_get(FI,'TEMPOUT','CODB',idx([M_ N_]),'quiet');
    otherwise
        [act,Chk]=vs_get(FI,'TEMPOUT','CODW',idx([M_ N_]),'quiet');
end
if DataInCell
    [gridact,Chk]=vs_get(FI,'TEMPOUT','CODB',idx([M_ N_]),'quiet');
else
    gridact=act;
end

%========================= GENERAL CODE =======================================

if XYRead
    if DimFlag(K_)
        szx=[size(x) 1]; % extent szx for the case that dataset in K dir. is 1
        szx1=szx([1:2 4:end]);
        szx1(2)=szx(2)*szx(3);
        x=reshape(x,szx1);
        x(:,gridact~=1,:)=NaN;
        x=reshape(x,szx);
        y=reshape(y,szx1);
        y(:,gridact~=1,:)=NaN;
        y=reshape(y,szx);
        %---
        szz=[size(z) 1]; % extent szx for the case that dataset in K dir. is 1
        szz1=szz([1:2 4:end]);
        szz1(2)=szz(2)*szz(3);
        z=reshape(z,szz1);
        z(:,act~=1,:)=NaN;
        z=reshape(z,szz);
    else
        x(gridact~=1)=NaN;
        y(gridact~=1)=NaN;
    end
end

if Props.NVal>0 & ~strcmp(Props.Loc,'NA')
    szz=[size(val1) 1]; % extent szx for the case that dataset in K dir. is 1
    szz1=szz([1:2 4:end]);
    szz1(2)=szz(2)*szz(3);
    val1=reshape(val1,szz1);
    val1(:,act~=1,:)=NaN;
    val1=reshape(val1,szz);
    if ~isempty(val2)
        val2(isnan(val1))=NaN;
    end
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
if 1%~all(allidx(DimMask & DimFlag))
    if XYRead
        if DataInCell
            if DimFlag(M_) & DimFlag(N_) & DimFlag(K_)
                z=z(:,ind{[M_ N_]},:);
            end
        else
            if DimFlag(M_) & DimFlag(N_)
                if DimFlag(K_)
                    x=x(:,ind{[M_ N_]},:);
                    y=y(:,ind{[M_ N_]},:);
                    z=z(:,ind{[M_ N_]},:);
                else
                    x=x(ind{[M_ N_]});
                    y=y(ind{[M_ N_]});
                end
            end
        end
    end
    DimMask=[0 1 1 1 1];
    ind=ind(DimMask & DimFlag);
    switch Props.NVal
        case {1,5,6}
            val1=val1(:,ind{:});
        case 2
            val1=val1(:,ind{:});
            val2=val2(:,ind{:});
    end
end

%================== NEFIS SPECIFIC CODE =======================================
% permute n and m dimensions into m and n if necessary
if DimFlag(M_) & DimFlag(N_)
    perm=[2 1 3];
    if XYRead
        if DimFlag(K_)
            x=permute(x,[1 1+perm]);
            y=permute(y,[1 1+perm]);
            z=permute(z,[1 1+perm]);
        else
            x=permute(x,perm);
            y=permute(y,perm);
        end
    end
    if Props.NVal==0
    elseif Props.NVal==1
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
if ~DimFlag(T_) | (DimFlag(T_) & isequal(size(idx{T_}),[1 1]))
    sz=size(x); sz=[sz(2:end) 1];
    if DimFlag(K_)
        x=reshape(x,sz);
        y=reshape(y,sz);
        if DimFlag(K_)
            sz=size(z); sz=[sz(2:end) 1];
            z=reshape(z,sz);
        end
    end
    if DataRead
        sz=size(val1); sz=[sz(2:end) 1];
        if Props.NVal==1
            val1=reshape(val1,sz);
        elseif Props.NVal==2
            val1=reshape(val1,sz);
            val2=reshape(val2,sz);
        end
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    if DimFlag(K_)
        Ans.Z=z;
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end

% read time ...
T=readtim(FI,Props,idx{T_});
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'     'Val2'    'SubFld' 'MNK'};
DataProps={'morphologic grid'          ''       [0 0 1 1 0]  0         0     ''       'd'   'd'       ''      'GRID'           'XCOR'     ''         []       0
    'hydrodynamic grid'         ''       [1 0 1 1 1]  0         0     ''       'z'   'z'       'i'     'MAPTTRAN'       'SL'       ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'water level'               'm'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'MAPTTRAN'       'SL'       ''         []       0
    'water depth'               'm'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'MAPTTRAN'       'SL'       ''         []       0
    'velocity (horizontal)'     'm/s'    [1 0 1 1 1]  1         2     'u'      'u'   'z'       'c'     'MAPTTRAN'       'U'        'V'        []       1
    'velocity in depth averaged flow direction' ...
    'm/s'    [1 0 1 1 1]  1         1     'u'      'u'   'z'       'c'     'MAPTTRAN'       'U'        'V'        []       0
    'velocity normal to depth averaged flow direction' ...
    'm/s'    [1 0 1 1 1]  1         1     'u'      'u'   'z'       'c'     'MAPTTRAN'       'U'        'V'        []       0
    'depth averaged velocity'   'm/s'    [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'MAPTTRAN'       'U'        'V'        []       1
    'spiral flow intensity'     'm/s'    [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'MAPTTRAN'       'RSP'      ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'u roughness Chezy C'    'm^{1/2}/s' [1 0 1 1 0]  1         1     ''       'u'   'u'       ''      'MAPTTRAN'       'CZU'      ''         []       0
    'v roughness Chezy C'    'm^{1/2}/s' [1 0 1 1 0]  1         1     ''       'v'   'v'       ''      'MAPTTRAN'       'CZV'      ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'initial bedload transport' 'm^3/m'  [1 0 1 1 0]  1         2     'u'      'z'   'z'       ''      'MAPATRAN'       'TTXI'     'TTYI'     []       1
    'avg bedload transport'    'm^3/m/s' [1 0 1 1 0]  1         2     'u'      'z'   'z'       ''      'MAPATRAN'       'TTXA'     'TTYA'     []       1
    'bedload transport per frac' 'm^3/m/s' [1 0 1 1 0]  1         2     'u'      'z'   'z'       ''      'MAPTTRAN'       'SX'       'SY'       []       1
    'total bedload transport'  'm^3/m/s' [1 0 1 1 0]  1         2     'u'      'z'   'z'       ''      'MAPTTRAN'       'STX'      'STY'      []       1
    'initial susp. transport'   'm^3/m'  [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'MAPATRAN'       'TTXSI'    'TTYSI'    []       1
    'avg susp. transport'      'm^3/m/s' [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'MAPATRAN'       'TTXSA'    'TTYSA'    []       1
    'susp. transport per frac' 'm^3/m/s' [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'MAPTTRAN'       'SXS'      'SYS'      []       1
    'total susp. transport'    'm^3/m/s' [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'MAPTTRAN'       'STXS'     'STYS'     []       1
    'concentration'            'm^3/m^3' [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'MAPTTRAN'       'CONC'     ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'bed exchange contribution' 'm^3/m^3' [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAN'       'RLSILT'   ''         []       0
    'bed level increment'       'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPATRAN'       'DZBSIL'   ''         []       0
    'time-varying bed level'    'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAN'       'ZB'       ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'D50 (char. diam. 1)'       'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPITRAN'       'DD50'     ''         1        0
    'D90 (char. diam. 2)'       'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPITRAN'       'DD90'     ''         1        0
    'transport layer thickness' 'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAG'       'DEFF'     ''         []       0
    'median grainsize'          'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAG'       'DMED'     ''         1        0
    'D10'                       'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAG'       'D10'      ''         1        0
    'D50'                       'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAG'       'D50'      ''         1        0
    'D90'                       'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAG'       'D90'      ''         1        0
    'transport layer'           ''       [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAG'       'PTRLA'    ''         []       0
    'exchange layer'            ''       [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'MAPTTRAG'       'PEXLA'    ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'GUU grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'u'   'u'       ''      'GRID'           'GUU'      ''         []       0
    'GVU grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'u'   'u'       ''      'GRID'           'GVU'      ''         []       0
    'GVV grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'v'   'v'       ''      'GRID'           'GVV'      ''         []       0
    'GUV grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'v'   'v'       ''      'GRID'           'GUV'      ''         []       0
    'cell area water level point' 'm^2'  [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'GRID'           'GSQS'     ''         []       0
    'cell area bottom point'    'm^2'    [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'GRID'           'GSQD'     ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0};

%============================= AUTODETECTION ==================================
Info=vs_disp(FI,'GRID','XCOR');
nm=Info.SizeDim([1 2]);
Info=vs_disp(FI,'GRID','THICK');
k=Info.SizeDim(1);
SkipGroup={'GRID','TEMPOUT'};
DataProps=auto_map_detect(FI,DataProps,nm,k,SkipGroup);

Info=vs_disp(FI,'MAPTTRAN','U');
if isfield(Info,'SizeDim') & (length(Info.SizeDim)==2 | (Info.SizeDim(3)==1))
    id=strmatch('velocity (horizontal)',DataProps(:,1),'exact');
    DataProps(id,:)=[];
    id=strmatch('velocity in depth averaged flow direction',DataProps(:,1),'exact');
    DataProps(id,:)=[];
    id=strmatch('velocity normal to depth averaged flow direction',DataProps(:,1),'exact');
    DataProps(id,:)=[];
end;

Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE REMOVE ================================
[DTSI,Chk]=vs_get(FI,'MAPATRAN',{1},'DTSI','quiet');
if ~Chk, DTSI=0; end
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info)
        % remove references to non-stored data fields
        Out(i,:)=[];
    elseif isequal(Info.SizeDim,1)
        % remove references to non-stored data fields
        Out(i,:)=[];
    elseif strmatch(Out(i).Name,{'initial bedload transport','initial susp. transport'},'exact') & ~DTSI
        Out(i,:)=[];
    end
end

[NQ,Chk]=vs_get(FI,'MAPTTRANNTRM','MAPTCH','quiet');
for i=size(Out,1):-1:1
    switch Out(i).Name,
        case 'u roughness Chezy C'
            Ch=1;
        case 'v roughness Chezy C'
            Ch=2;
        case 'velocity (horizontal)'
            Ch=[1 2];
        case 'depth averaged velocity'
            Ch=[1 2 3];
        case {'water level','water depth','time-varying bed level','spiral flow intensity'}
            Ch=3;
        case {'concentration','bed exchange contribution'}
            Ch=12;
        case 'bedload transport per frac'
            Ch=[4 5];
        case 'total bedload transport'
            Ch=[6 7];
        case 'susp. transport per frac'
            Ch=[8 9];
        case 'total susp. transport'
            Ch=[10 11];
        otherwise,
            Ch=[];
    end
    if ~isempty(Ch) & any(~NQ(Ch)), Out(i,:)=[]; end
end

[NQ,Chk]=vs_get(FI,'MAPATRANNTR','NQUALT','quiet');
for i=size(Out,1):-1:1
    switch Out(i).Name,
        case 'initial bedload transport'
            Ch=[1 2];
        case 'avg bedload transport'
            Ch=[3 4];
        case 'initial susp. transport'
            Ch=[5 6];
        case 'avg susp. transport'
            Ch=[7 8];
        case 'bed level increment'
            Ch=[9];
        otherwise,
            Ch=[];
    end
    if ~isempty(Ch) & any(~NQ(Ch)), Out(i,:)=[]; end
end

%======================= SET USEGRID OPTIONS ==================================
for i=1:length(Out)
    switch Out(i).ReqLoc
        case 'd'
            Out(i).UseGrid=1;
        case 'z'
            Out(i).UseGrid=2;
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if ~isempty(strmatch(Props.Name,{'initial bedload transport','avg bedload transport','bedload transport per frac', ...
        'total bedload transport','transport layer','exchange layer'},'exact'))
    Info=vs_disp(FI,Props.Group,Props.Val1);
    if isfield(Info,'NDim') & (Info.NDim>=3)
        subf=cellstr(multiline(sprintf('fraction %i\n',1:Info.SizeDim(3))));
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
if Props.DimFlag(M_) & Props.DimFlag(N_)
    Info=vs_disp(FI,'GRID','XCOR');
    sz([N_ M_])=Info.SizeDim;
end;
if Props.DimFlag(K_)
    Info=vs_disp(FI,'GRID','THICK');
    sz(K_)=Info.SizeDim;
end
if Props.DimFlag(T_)
    Info=vs_disp(FI,Props.Group,[]);
    sz(T_)=Info.SizeDim;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[pars,Chk]=vs_get(FI,'MAPATRANNTR','*','quiet');
d0=tdelft3d(pars.ITO1,pars.ITO2);
switch Props.Group,
    case {'MAPTTRAN','MAPTTRAG'}
        [T,Chk]=vs_let(FI,Props.Group,{t},'T-TRAN','quiet');
        T=d0+T*pars.TSCALE/(24*3600);
    case 'MAPATRAN'
        [T1,Chk]=vs_let(FI,'MAPATRAN',{t},'TSEDB','quiet');
        [T2,Chk]=vs_let(FI,'MAPATRAN',{t},'TSEDE','quiet');
        T=d0+0.5*(T1+T2)*pars.TSCALE/(24*3600);
    otherwise, % ONE FIELD
        T=d0;
end
% -----------------------------------------------------------------------------
