function varargout=d3d_trihfil(FI,domain,field,cmd,varargin)
%D3D_TRIHFIL QP support for Delft3D-FLOW history files.
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
    case 'dimlabels'
        varargout={getlabels(FI,Props)};
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

subf=getsubfields(FI,Props);
subforig = [];
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    subforig = Props.SubFld;
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
if DimFlag(ST_)
    if isequal(idx{ST_},0)
        idx{ST_}=1:sz(ST_);
    end
end

% select appropriate spatial indices ...
if any(DimFlag==7)
    dimlabels = getlabels(FI,Props);
    for i=1:length(DimFlag)
        if DimFlag(i)==7
            idx{i}=find(idx{i}==dimlabels{i});
        end
    end
end

%================== NEFIS SPECIFIC CODE =======================================
if DimFlag(M_)&& DimFlag(N_)
    sz([M_ N_])=sz([N_ M_]);
    idx([M_ N_])=idx([N_ M_]);
end

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        elseif i~=K_
            idx{i}=[max(1,idx{i}(1)-1) idx{i}];
        end
    end
end

multiturbine=0;
switch Props.Val1
    case {'INST_SIMTHRUST','CUM_SIMTHRUST','INST_THRUST','CUM_THRUST','INST_POWER','CUM_POWER'}
        if idx{ST_}==sz(ST_) && sz(ST_)>1
            idx{ST_} = 1:sz(ST_)-1;
            multiturbine=1;
        end
end

% read grid ...
x=[];
y=[];
z=[];
Info=vs_disp(FI,'his-const','ZK');
fixedlayers=0;
if isstruct(Info)
    fixedlayers=Info.SizeDim>1;
end
%
[NTRUV,Chk]=vs_get(FI,'his-const','NTRUV','quiet');
imatch=strmatch(Props.Name,{'instantaneous discharge','cumulative discharge'},'exact');
if ~isempty(imatch)
    if idx{ST_}>NTRUV
        idx{ST_}=idx{ST_}-NTRUV;
        Props.Group='his-dis-series';
        Val1s={'ZQ','ZQ_SUM'};
        Props.Val1=Val1s{imatch};
    end
end
%
XYRead = XYRead & ~strcmp(Props.Loc,'NA') & ~strcmp(Props.Group,'his-dad-series') & ~strcmp(Props.Group,'his-dis-series');
%
computeDZ=0;
switch Props.Name
    case {'depth averaged velocity','depth averaged discharge','froude number','head'}
        if fixedlayers
            computeDZ=1;
        end
end
if XYRead
    
    %======================== SPECIFIC CODE =======================================
    if ~isempty(Props.Loc)
        if strcmp(Props.Loc,'turbines')
            if multiturbine
                x=[];
                y=[];
            else
                [xy,Chk]=vs_get(FI,'his-const','XYTURBINES',{idx{ST_} 0},'quiet');
                x = xy(:,1);
                y = xy(:,2);
            end
        elseif isstruct(vs_disp(FI,'his-series','XYSTAT'))
            [xy,Chk]=vs_let(FI,'his-series',{idx{T_}},'XYSTAT',{0 idx{ST_}},'quiet');
            x=reshape(xy(:,1,:),[size(xy,1) size(xy,3)]);
            y=reshape(xy(:,2,:),[size(xy,1) size(xy,3)]);
        else
            [xy,Chk]=vs_get(FI,'his-const','XYSTAT',{0 idx{ST_}},'quiet');
            x=xy(1,:);
            y=xy(2,:);
        end
    else
        [xy,Chk]=vs_get(FI,'his-const','XYTRA',{0 idx{ST_}},'quiet');
        x=xy([1 3],:);
        y=xy([2 4],:);
    end
    
    if DimFlag(K_) || computeDZ
        zb = readzb(FI,idx);
        %
        if fixedlayers
            [h,Chk]=vs_let(FI,'his-const','ZK','quiet');
            h(1)=-inf;
            h(end)=inf;
            [s,Chk]=vs_let(FI,'his-series',{idx{T_}},'ZWL',idx(ST_),'quiet');
            nt=length(idx{T_});
            ns=length(idx{ST_});
            nk=length(h);
            z=repmat(reshape(h,[1 1 nk]),[nt ns]);
            for i=1:nk
                z(:,:,i)=min(z(:,:,i),s);
            end
            zb=repmat(reshape(zb,[nt ns]),[1 1 nk]);
            z = max(z,zb);
            if computeDZ
                dz=diff(z,1,3);
            end
            if strcmp(Props.Loc3D,'c')
                z=(z(:,:,1:end-1)+z(:,:,2:end))/2;
            end
            idxK_=idx{K_};
            if DimFlag(K_)
                z=z(:,:,idxK_);
                x=repmat(x,[1 1 length(idxK_)]);
                y=repmat(y,[1 1 length(idxK_)]);
            end
        else
            [h,Chk]=vs_let(FI,'his-series',{idx{T_}},'ZWL',idx(ST_),'quiet');
            h=h-zb;
            [thk,Chk]=vs_let(FI,'his-const','THICK','quiet');
            switch Props.Loc3D
                case 'i'
                    cthk=cumsum([0 thk]);
                    idxK_=idx{K_};
                case 'c'
                    if DataInCell
                        cthk=cumsum([0 thk]);
                        idxK_=[idx{K_} idx{K_}(end)+1];
                    else
                        cthk=cumsum(thk)-thk/2;
                        idxK_=idx{K_};
                    end
            end
            cthk=cthk(idxK_);
            z=zeros([size(h) length(cthk)]);
            for k=1:length(cthk)
                z(:,:,k)=zb+(1-cthk(k))*h;
            end
            x=repmat(x,[1 1 length(cthk)]);
            y=repmat(y,[1 1 length(cthk)]);
        end
    end
    %========================= GENERAL CODE =======================================
end

% grid interpolation in z direction ...

% load data ...
if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

if DataRead
    %================== NEFIS SPECIFIC CODE =======================================
    elidx=idx(2:end);
    switch Props.Name
        case {'depth averaged velocity','depth averaged discharge'}
            elidx{K_-1}=0;
            DimFlag(K_)=1;
            kidx=sum(DimFlag~=0);
        case {'froude number','head'}
            elidx{K_-1}=0;
            DimFlag(K_)=1;
            kidx=sum(DimFlag~=0);
            Props.NVal=2;
        case {'location tidal turbines'}
            PropC = Props;
            Props.Group = 'his-const';
            Props.Val1 = 'NAMTURBINES';
            idxT = idx{T_};
        case {'location observation points'}
            PropC = Props;
            Props.Group = 'his-const';
            Props.Val1 = 'NAMST';
            idxT = idx{T_};
            idx{T_} = 1;
        case {'cumulative dredged material'}
            %
            % Cumulative dredged material: fluxes, dredged totals, dumped totals.
            %
            loc=elidx{ST_-1};
            Info=vs_disp(FI,'his-dad-const','LINK_DEF');
            if loc<=Info.SizeDim(1)
                %
                % Dredge flux from dredge location to dump location:
                % loc represents the link number.
                %
            else
                loc=loc-Info.SizeDim(1);
                Info=vs_disp(FI,'his-dad-const','DREDGE_AREAS');
                if loc<=Info.SizeDim
                    %
                    % Cumulative dredged material: search for loc to sum the
                    % fluxes leaving the dredge location indicated by loc.
                    %
                    [Links,Chk]=vs_get(FI,'his-dad-const','LINK_DEF','quiet');
                    loc=find(Links(:,1)==loc);
                else
                    loc=loc-Info.SizeDim;
                    %
                    % Cumulative dumped material: search for loc to sum the
                    % fluxes entering the dump location indicated by loc.
                    %
                    [Links,Chk]=vs_get(FI,'his-dad-const','LINK_DEF','quiet');
                    loc=find(Links(:,2)==loc);
                end
            end
            elidx{ST_-1}=loc;
    end
    switch Props.Val1
        case {'BALFLUX','BALR1FLUX'}
            if Props.SubFld<3
                elidx = [{Props.SubFld} elidx];
            else
                elidx = [{[1 2]} elidx];
            end
            DimFlag(3:6)=DimFlag(2:5);
            DimFlag(2) = 1;
            Props.SubFld = [];
        case {'BALSDFLUX'}
            flux = mod(Props.SubFld-1,3)+1;
            frac = (Props.SubFld-flux)/3+1;
            Props.Xtra = frac;
            if flux<3
                elidx = [{flux} elidx];
            else
                elidx = [{[1 2]} elidx];
            end
            DimFlag(3:6)=DimFlag(2:5);
            DimFlag(2) = 1;
            Props.SubFld = [];
    end
    if isequal(subforig,'s') || isequal(subforig,'sb')
        if isequal(Props.SubFld,length(subf)) && length(subf)>1
            Props.SubFld = 1:length(subf)-1;
        end
    end
    if ~isempty(Props.SubFld) && isnumeric(Props.SubFld)
        elidx(end+1)={Props.SubFld}; % last dimension automatically dropped after reading
    end
    if ~isempty(Props.Xtra)
        elidx(end+1)={Props.Xtra}; % last dimension automatically dropped after reading
    end
    
    elidx(~DimFlag(2:end))=[];
    [val1,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,elidx,'quiet');
    if ~Chk
        error(val1)
    elseif multiturbine
        val1 = sum(val1,2);
    end
    if isempty(Props.Val2)
        val2=[];
    else
        [val2,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val2,elidx,'quiet');
    end
    
    switch Props.Val1
        case {'BALFLUX','BALR1FLUX','BALSDFLUX'}
            if length(elidx{1})==2
                val1 = val1 * [1;-1];
            end
            elidx(1)=[];
            DimFlag =DimFlag([1 3:end]);
    end
    switch Props.Name
        case {'location observation points','location tidal turbines'}
            sz=size(val1);
            val1=cellstr(reshape(val1,sz(2:3)));
            Props = PropC;
            idx{T_} = idxT;
        case 'water depth'
            zb = readzb(FI,idx);
            if size(zb,1)==1
                for i=1:size(val1,1)
                    val1(i,:,:)=val1(i,:,:)-zb;
                end
            else
                val1=val1-zb;
            end
        case {'bed level'}
            val1(val1==-999)=NaN; % filter missing values for moving observation points
            val1=-val1;
        case {'cum. erosion/sedimentation'}
            [dp0,Chk]=vs_let(FI,'his-sed-series',{1},'ZDPS',elidx,'quiet');
            val1=-val1+repmat(dp0,size(val1,1),1);
        case 'total transport'
            [val1r,Chk]=vs_let(FI,Props.Group,{idx{T_}},'ZSBU',elidx,'quiet');
            val1=val1+val1r;
            val1r=[];
            [val2r,Chk]=vs_let(FI,Props.Group,{idx{T_}},'ZSBV',elidx,'quiet');
            val2=val2+val2r;
            val2r=[];
        case {'momentary bed load transport', 'momentary suspended transport', 'momentary total transport', ...
                'instantaneous bed load transport', 'instantaneous suspended transport', 'instantaneous total transport', ...
                'cumulative bed load transport', 'cumulative suspended transport', 'cumulative total transport'}
            if ~isempty(strfind(Props.Name,'total transport'))
                val1=val1+val2;
                val2=[];
            end
            val1=sum(val1,3); % sum over fractions
        case {'depth averaged velocity','depth averaged discharge','froude number','head'}
            if fixedlayers
                sz=size(val1);
                h=zeros(sz(1:2));
                val1(val1==-999)=0;
                val2(val2==-999)=0;
                val1=val1.*dz;
                val2=val2.*dz;
                h=sum(dz,3);
                val1(h==0)=NaN; val2(h==0)=NaN; h(h==0)=1;
                val1=sum(val1,3); val1=val1./h;
                val2=sum(val2,3); val2=val2./h;
            else
                [thk,Chk]=vs_let(FI,'his-const','THICK','quiet');
                for k=length(thk):-1:1
                    val1(:,:,k)=val1(:,:,k)*thk(k);
                    val2(:,:,k)=val2(:,:,k)*thk(k);
                end
                val1=sum(val1,3);
                val2=sum(val2,3);
            end
            DimFlag(K_)=0;
            switch Props.Name
                case 'head'
                    val3=vs_let(FI,'his-series',idx(T_),'ZWL',idx(ST_),'quiet!');
                case 'froude number'
                    val3=vs_let(FI,'his-series',idx(T_),'ZWL',idx(ST_),'quiet!');
                    zb = readzb(FI,idx);
                    if size(zb,1)==1,
                        for i=1:size(val3,1)
                            val3(i,:,:)=val3(i,:,:)-zb;
                        end
                    else
                        val3=val3-zb;
                    end
                    val3(val3==0)=inf;
            end
            %
            [gravity,Success]=vs_let(FI,'his-const','GRAVITY','quiet');
            if ~Success
                gravity = 9.81;
            end
        case {'cumulative dredged material'}
            val1=sum(val1,2);
        otherwise
            if ~isempty(strfind(Props.Name,'total transport')) && ~isempty(val2) && Props.NVal==1
                val1=val1+val2;
                val2=[];
            end
    end
    if isequal(subforig,'s') || isequal(subforig,'sb')
        if length(Props.SubFld)>1
            val1=sum(val1,ndims(val1)); % sum of all fractions
            val2=sum(val2,ndims(val2)); % sum of all fractions
        end
    end
    
    if fixedlayers && Props.NVal~=4
        if ~isempty(val1)
            val1(val1==-999)=NaN;
        end
        if ~isempty(val2)
            val2(val2==-999)=NaN;
        end
    end
    
    switch Props.Name
        case 'froude number'
            val1 = sqrt(val1.^2+val2.^2)./sqrt(gravity*val3);
            val2 = [];
            val3 = [];
            Props.NVal = 1;
            Props.VecType = '';
        case 'head'
            val1 = val3+(val1.^2+val2.^2)/(2*gravity);
            val2 = [];
            val3 = [];
            Props.NVal = 1;
            Props.VecType = '';
    end

    if DataInCell && isequal(Props.ReqLoc,'d')
        Props.ReqLoc='z';
    end
    % combine vectors components ...
    if isequal(Props.VecType,'m')
        [val1,val2]=dir2uv(val1,val2);
    end
    % data interpolation ...
    if isequal(Props.Loc,'d') && isequal(Props.ReqLoc,'z')
        val1=interp2cen(val1,'t');
        if ~isempty(val2)
            val2=interp2cen(val2,'t');
        end
    elseif isequal(Props.Loc,'u') && isequal(Props.ReqLoc,'z')
        [val1,val2]=uv2cen(val1,val2);
    end
    % combine vectors components ...
    if isequal(Props.VecType,'u') && Props.MNK<=1
        % rotate n,m components into x,y direction ...
        [alf,Chk] = vs_get(FI,'his-const','ALFAS',idx(ST_),'quiet');
        alf = alf*pi/180;
        [val1,val2]=cur2ca(val1,val2,alf);
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
    ind{2}=1:length(idx{2});
    for i=[M_ N_ K_]
        if DimFlag(i)
            if ~allidx(i)
                if i~=K_
                    ind{i}=(1:(length(idx{i})-1))+(idx{i}(1)~=idx{i}(2));
                else % i==K_
                    ind{i}=1:length(idx{i});
                end
            else
                ind{i}=idx{i};
            end
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

%========================= GENERAL CODE =======================================


% reshape if a single timestep is selected ...
if DimFlag(T_) && isequal(size(idx{T_}),[1 1])
    sz=size(val1);
    sz=[sz(1) 1 sz(end)];
    if isempty(val2)
        val1=reshape(val1,sz);
    else
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
    if XYRead && any(DimFlag([M_ N_ K_]))
        x=reshape(x,sz);
        y=reshape(y,sz);
        if DimFlag(K_)
            z=reshape(z,sz);
        end
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    Ans.XUnits='m';
    Ans.YUnits='m';
    Info=vs_disp(FI,'his-const','XYSTAT');
    if isstruct(Info) && isequal(Info.ElmUnits,'[  DEG  ]')
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    end
    if DimFlag(K_)
        Ans.Z=z;
        Ans.ZUnits='m';
    end
end
if Props.NVal==0
elseif Props.NVal==1 || Props.NVal==4
    Ans.Val=val1;
    if strcmp(Props.Name,'temperature')
        Ans.AbsoluteUnits = true;
    end
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
TRB='turbines'; BAR='barriers';
PropNames={'Name'            'Units'   'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'     'Val2'    'SubFld' 'MNK'};
DataProps={'location observation points'   ''   [1 6 0 0 0]  0         4     ''       'z'   'z'       ''      'his-series'     'XYSTAT'   ''  []       0
    'location tidal turbines'   ''       [1 3 0 0 0]  0         4     ''       TRB   'z'       ''      'his-const'      'XYTURBINES'   ''     []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'wind speed'                'm/s'    [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'ZWNDSPD'  ''         []       0
    'wind direction'            'deg'    [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'ZWNDDIR'  ''         []       0
    'wind drag coefficient'     '-'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'ZWNDCD'   ''         []       0
    'air pressure'              'Pa'     [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'PATM'     ''         []       0
    'precipitation rate'        'mm/h'   [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'ZPRECP'   ''         []       0
    'evaporation rate'          'mm/h'   [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'ZEVAP'    ''         []       0
    'evaporation heat flux'            'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZQEVA'    ''         []       0
    'heat flux of forced convection'   'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZQCO'     ''         []       0
    'nett back radiation'              'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZQBL'     ''         []       0
    'nett solar radiation'             'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZQIN'     ''         []       0
    'total nett heat flux'             'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZQNET'    ''         []       0
    'free convection of sensible heat' 'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZHFREE'   ''         []       0
    'free convection of latent heat'   'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZEFREE'   ''         []       0
%    'computed minus derived heat flux' 'W/m^2'  [1 5 0 0 0]  0  1     ''       'z'   'z'       ''      'his-series'     'ZQMIS'    ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'significant wave height'   'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-wav-series' 'ZHS'      ''         []       0
    'peak wave period'          's'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-wav-series' 'ZTP'      ''         []       0
    'wave direction'            'deg'    [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-wav-series' 'ZDIR'     ''         []       0
    'wave length'               'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-wav-series' 'ZRLABD'   ''         []       0
    'peak near-bed orbital velocity' 'm/s' [1 5 0 0 0] 0        1     ''       'z'   'z'       ''      'his-wav-series' 'ZUORB'    ''         []       0
    'peak near-bed orbital velocity' 'm/s' [1 5 0 0 0] 0        1     ''       'z'   'z'       ''      'his-wav-series' 'ZUWB'     ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'water level'               'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'ZWL'      ''         []       0
    'water depth'               'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-series'     'ZWL'      ''         []       0
    'depth averaged velocity'   'm/s'    [1 5 0 0 0]  0         2     'u'      'z'   'z'       ''      'his-series'     'ZCURU'    'ZCURV'    []       1
    'horizontal velocity'       'm/s'    [1 5 0 0 1]  0         2     'u'      'z'   'z'       'c'     'his-series'     'ZCURU'    'ZCURV'    []       1
    'vertical velocity'         'm/s'    [1 5 0 0 1]  0         1     ''       'w'   'w'       'c'     'his-series'     'ZCURW'     ''        []       0
    'depth averaged discharge'  'm^3/s'  [1 5 0 0 0]  0         2     'u'      'z'   'z'       'c'     'his-series'     'ZQXK'     'ZQYK'     []       1
    'discharge'                 'm^3/s'  [1 5 0 0 1]  0         2     'u'      'z'   'z'       'c'     'his-series'     'ZQXK'     'ZQYK'     []       1
    'froude number'             '-'      [1 5 0 0 0]  0         1     'u'      'z'   'z'       ''      'his-series'     'ZCURU'    'ZCURV'    []       0
    'head'                      'm'      [1 5 0 0 0]  0         1     'u'      'z'   'z'       ''      'his-series'     'ZCURU'    'ZCURV'    []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'density'                   'kg/m^3' [1 5 0 0 1]  0         1     ''       'z'   'z'       'c'     'his-series'     'ZRHO'     ''         []       0
    'non-hydrostatic pressure'  ''       [1 5 0 0 1]  0         1     ''       'z'   'z'       'c'     'his-series'     'HYDPRES'  ''         []       0
    '--constituents'            ''       [1 5 0 0 1]  0         1     ''       'z'   'z'       'c'     'his-series'     'GRO'      ''         []       0
    '--turbquant'               ''       [1 5 0 0 1]  0         1     ''       'z'   'z'       'i'     'his-series'     'ZTUR'     ''         []       0
    'vertical eddy viscosity'   'm^2/s'  [1 5 0 0 1]  0         1     ''       'z'   'z'       'i'     'his-series'     'ZVICWW'   ''         []       0
    'vertical eddy diffusivity' 'm^2/s'  [1 5 0 0 1]  0         1     ''       'z'   'z'       'i'     'his-series'     'ZDICWW'   ''         []       0
    'richardson number'         '-'      [1 5 0 0 1]  0         1     ''       'z'   'z'       'i'     'his-series'     'ZRICH'    ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'settling velocity'         'm/s'    [1 5 0 0 1]  0         1     ''       'z'   'z'       'i'     'his-sed-series' 'ZWS'      ''         's1'     0
    'equilibrium concentrations' 'kg/m^3' [1 5 0 0 1] 0         1     ''       'z'   'z'       'c'     'his-sed-series' 'ZRSDEQ'   ''         'sb'     0
    'available mass in fluff layer' 'kg/m^2' [1 5 0 0 0]  0     1    ''        'z'   'z'       ''      'his-sed-series' 'MFLUFF'   ''         's'      0
    'available mass of sediment' 'kg/m^2' [1 5 0 0 0] 0         1     ''       'z'   'z'       ''      'his-sed-series' 'ZBDSED'   ''         'sb'     0
    'bed load transport'        'm^3/s/m' [1 5 0 0 0]  0        2     'u'      'z'   'z'       ''      'his-sed-series' 'ZSBU'     'ZSBV'     'sb'     1
    'd.a. suspended transport'  'm^3/s/m' [1 5 0 0 0]  0        2     'u'      'z'   'z'       ''      'his-sed-series' 'ZSSU'     'ZSSV'     'sb'     1
    'total transport'           'm^3/s/m' [1 5 0 0 0]  0        2     'u'      'z'   'z'       ''      'his-sed-series' 'ZSUU'     'ZSSV'     'sb'     1
    'source term suspended sediment fractions' ...
                                'kg/m^3/s' [1 5 0 0 0] 0        1     ''       'z'   'z'       ''      'his-sed-series' 'ZSOURSE'  ''         's'      0
    'sink term suspended sediment fractions'   ...
                                '1/s'    [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sed-series' 'ZSINKSE'  ''         's'      0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'bed shear stress'          'N/m^2'  [1 5 0 0 0]  0         2     'u'      'z'   'z'       ''      'his-series'     'ZTAUKS'   'ZTAUET'   []       1
    'bed shear stress for morphology' 'N/m^2'  [1 5 0 0 0]  0   1     ''       'z'   'z'       ''      'his-sed-series' 'ZTAUB'    ''         []       0
    'bed level'                  'm'     [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-const'      'DPS'      ''         []       0
    'cum. erosion/sedimentation' 'm'     [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sed-series' 'ZDPS'     ''         []       0
    'morphological acceleration factor' '-' [1 0 0 0 0] 0       1     ''       'NA'  ''        ''    'his-infsed-serie' 'MORFAC'   ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'sediment fraction in top layer'  '-'  [1 5 0 0 0]  0       1     ''       'z'   'z'       ''      'his-sed-series' 'ZFRAC'    ''         'sb1'    0
    'mud fraction in top layer'       '-'  [1 5 0 0 0]  0       1     ''       'z'   'z'       ''      'his-sed-series' 'ZMUDFRAC' ''         []       0
    'sand fraction in top layer'      '-'  [1 5 0 0 0]  0       1     ''       'z'   'z'       ''      'his-sed-series' 'ZSANDFRAC' ''        []       0
    'hiding and exposure'             '-'  [1 5 0 0 0]  0       1     ''       'z'   'z'       ''      'his-sed-series' 'ZHIDEXP'  ''         'sb1'    0
    'reduction factor due to limited sediment thickness' ...
                                      '-'  [1 5 0 0 0]  0       1     ''       'z'   'z'       ''      'his-sed-series' 'ZFIXFAC'  ''         'sb1'    0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'instantaneous discharge'   'm^3/s'  [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-series'     'CTR'      ''         []       0
    'cumulative discharge'      'm^3'    [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-series'     'FLTR'     ''         []       0
    'concentration'             ''       [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-dis-series' 'RINT'     ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'barrier height'            'm'      [1 5 0 0 0]  0         1     ''       BAR   ''        ''      'his-series'     'ZBAR'     ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'advective transport'       ''       [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-series'     'ATR'      ''         []       0
    'dispersive transport'      ''       [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-series'     'DTR'      ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'instantaneous bed load transport'  '*' [1 5 0 0 0] 0       1     ''       ''    ''        ''      'his-sed-series' 'SBTR'     ''         'sb'     0
    'instantaneous suspended transport' '*' [1 5 0 0 0] 0       1     ''       ''    ''        ''      'his-sed-series' 'SSTR'     ''         's'      0
    'instantaneous total transport'     '*' [1 5 0 0 0] 0       1     ''       ''    ''        ''      'his-sed-series' 'SSTR'     'SBTR'     's'      0
    'cumulative bed load transport'  '*' [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-sed-series' 'SBTRC'    ''         'sb'     0
    'cumulative suspended transport' '*' [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-sed-series' 'SSTRC'    ''         's'      0
    'cumulative total transport'     '*' [1 5 0 0 0]  0         1     ''       ''    ''        ''      'his-sed-series' 'SSTRC'    'SBTRC'    's'      0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         's'      0
    'dimensionless sediment diameter' '-' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZDSTAR'   ''         's'      0
    'current-related bed roughness height' 'm' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZRC'      ''         's'      0
    'wave-related bed roughness height' 'm' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZRW'      ''         's'      0
    'critical bed shear stress' 'N/m^2'  [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZTAUCR'   ''         's'      0
    'current-related bed shear stress' 'N/m^2' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZTAUC'    ''         's'      0
    'wave-related bed shear stress' 'N/m^2' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZTAUWV'   ''         's'      0
    'efficiency factor - current' '-' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZMUC'     ''         's'      0
    'efficiency factor - waves' '-' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZMUWA'    ''         's'      0
    'thickness of near-bed sediment mixing layer' 'm' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZDELM'    ''         's'      0
    'sediment reference height' 'm'      [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZAKS'     ''         's'      0
    'dimensionless sediment suspension parameter' '-' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZTA'      ''         's'      0
    'near-bed reference concentration of sediment' 'kg/m^3' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZRCA'     ''         's'      0
    'near-bed reference concentration of sediment' 'kg/m^3' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZRCA'     ''         's'      0                                         
    'suspended sediment particle size' 'm' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZDSS'     ''         's'      0
    'vertical sediment diffusion coefficient' 'm^2/s' ...
                                         [1 5 0 0 1]  0         1     ''       'z'   'z'       'i'     'his-sdx-series' 'ZSEDDF'   ''         's'      0
    'wave orbital velocity in onshore direction' 'm/s' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZUON'     ''         's'      0
    'wave orbital velocity in offshore direction' 'm/s' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZUOFF'    ''         's'      0
    'magnitude of (unadjusted) bedload transport vector' 'm^3/m/s' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZQBT'     ''         's'      0
    'magnitude of current-related bedload transport vector' 'm^3/m/s' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZQBC'     ''         's'      0
    'magnitude of wave-related bedload transport vector' 'm^3/m/s' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZQBW'     ''         's'      0
    'magnitude of wave-related near-bed transport vector' 'm^3/m/s' ...
                                         [1 5 0 0 0]  0         1     ''       'z'   'z'       ''      'his-sdx-series' 'ZQSW'     ''         's'      0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'area'                      'm^2'    [1 5 0 0 0]  0         1     ''       'NA'  ''        ''      'his-bal-const'  'BALAREAS' ''         []       0
    'volume'                    'm^3'    [1 5 0 0 0]  0         1     ''       'NA'  ''        ''      'his-bal-series' 'BALVOLUME' ''        []       0
    '--avgconstituents'         ''       [1 5 0 0 0]  0         1     ''       'NA'  ''        ''      'his-bal-series' 'BALR1CONC' ''        []       0
    'average bed level'         'm'      [1 5 0 0 0]  0         1     ''       'NA'  ''        ''      'his-bal-series' 'BALDPS'   ''         []       0
    'cumulative flux'           'm^3'    [1 5 0 0 0]  0         1     ''       'NA'  ''        ''      'his-bal-series' 'BALFLUX'  ''         'f'      0
    '--cumconsflux'             ''       [1 5 0 0 0]  0         1     ''       'NA'  ''        ''      'his-bal-series' 'BALR1FLUX' ''        'f'      0
    % note 1: 'cumulative total transp.' needs to differ from 'cumulative total transport' at transects used above
    % note 2: 'cumulative total transp.' includes morfac (and bedload) whereas cumulative flux does not include either
    'cumulative total transp.'  'kg'     [1 5 0 0 0]  0         1     ''       'NA'  ''        ''      'his-bal-series' 'BALSDFLUX' ''        'fs'     0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'reference velocity'        'm/s'    [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'UTURBINES'   ''      []       0
    'power coefficient'         '-'      [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'POWERCOEF'   ''      []       0
    'thrust coefficient'        '-'      [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'THRUSTCOEF'  ''      []       0
    'loss coefficient'          '-'      [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'LOSSCOEF'    ''      []       0
    'simulated thrust'          'N'      [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'INST_SIMTHRUST' ''   []       0
    'thrust'                    'N'      [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'INST_THRUST' ''      []       0
    'power'                     'W'      [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'INST_POWER'  ''      []       0
    'cumulative simulated thrust' 'N*s'  [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'CUM_SIMTHRUST' ''    []       0
    'cumulative thrust'         'N*s'    [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'CUM_THRUST'  ''      []       0
    'cumulative power'          'W*s'    [1 5 0 0 0]  0         1     ''       TRB   ''        ''      'his-series'     'CUM_POWER'   ''      []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'time fraction ploughing'        '-'     [1 5 0 0 0]  0     1     ''       ''    ''        ''      'his-dad-series' 'PLOUGH_TFRAC' ''     []       0
    'time fraction dredging'         '-'     [1 5 0 0 0]  0     1     ''       ''    ''        ''      'his-dad-series' 'DREDGE_TFRAC' ''     []       0
    'cumulative dredged material'    'm^3'   [1 5 0 0 0]  0     1     ''       ''    ''        ''      'his-dad-series' 'LINK_SUM' ''         'sb'     0};

%  'vorticity'                 '1/s'    [1 5 0 0 5]  0         1     ''       'd'   'd'       'c'     'his-series'     'ZVORT'    ''         []       0
%  'enstrophy'                 '1/s^2'  [1 5 0 0 5]  0         1     ''       'd'   'd'       'c'     'his-series'     'ZENST'    ''         []       0
Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE DIMENSIONS ============================
% Check whether the number of layers on the output file has been reduced
Info=vs_disp(FI,'his-const','OUTPUT_LAYERS');
if isstruct(Info)
    Info2=vs_disp(FI,'his-const','THICK');
    outputLayers = ~isequal(Info.SizeDim,Info2.SizeDim+1);
else
    outputLayers = 0;
end

%
Info=vs_disp(FI,'his-const','THICK');
k=Info.SizeDim(1);
if k==1
    Out = removequant(Out, ...
        {'vertical velocity'
        'horizontal velocity'
        'discharge'});
elseif outputLayers
    Out = removequant(Out, ...
        {'depth averaged velocity'
        'depth averaged discharge'});
    for i=1:length(Out)
        if Out(i).DimFlag(K_) && any(strcmp(Out(i).Loc3D,{'i','c'}))
            Out(i).DimFlag(K_) = 7;
        end
    end
end

[Out.Xtra] = deal([]);
[Out.Geom] = deal('');
[Out.Coords] = deal('');
i=strmatch('location observation points',{Out.Name});
Out(i).Geom = 'PNT';
Out(i).Coords = 'xy';
i=strmatch('location tidal turbines',{Out.Name});
Out(i).Geom = 'PNT';
Out(i).Coords = 'xy';
%======================== SPECIFIC CODE CHANGE ================================

Info=vs_disp(FI,'his-series','DPS');
if isstruct(Info)
    i=strmatch('bed level',{Out.Name});
    Out(i).Group='his-series';
    Out(i).Val1='DPS';
end

Info=vs_disp(FI,'his-sed-series',[]);
if isstruct(Info)
    i=strmatch('bed level',{Out.Name});
    Out(i).Group='his-sed-series';
    Out(i).Val1='ZDPS';
else
    i=strmatch('cum. erosion/sedimentation',{Out.Name});
    Out(i)=[];
end

%======================== SPECIFIC CODE REMOVE ================================
[NSt,Chk]=vs_get(FI,'his-const','NOSTAT','quiet');
if Chk==0
    NSt=0;
end
[NTr,Chk]=vs_get(FI,'his-const','NTRUV','quiet');
if Chk==0
    NTr=0;
end
NDis=0;
Info=vs_disp(FI,'his-dis-const','DISCHARGES');
if isstruct(Info)
    NDis=Info.SizeDim(1);
end
NTurbines=0;
Info=vs_disp(FI,'his-const','NAMTURBINES');
if isstruct(Info)
    NTurbines=Info.SizeDim(1);
end
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info) && ...
            ~strcmp(Out(i).Name,'cumulative discharge') && ... % exception because of culverts
            ~strcmp(Out(i).Name,'instantaneous discharge')     % exception because of culverts
        % remove references to non-stored data fields
        Out(i)=[];
        %  ---> if isequal(Info.SizeDim,1) this might be due to the fact
        %       that there is just one station or because the data has not
        %       been written
    elseif strcmp(Out(i).Name,'froude number') || strcmp(Out(i).Name,'head')
        Info2=vs_disp(FI,Out(i).Group,'ZWL');
        if ~isstruct(Info2)
            Out(i)=[];
        end
    else
        switch Out(i).Val1
            case {'FLTR','CTR'}
                % cross-section or discharges
                if NTr+NDis==0
                    Out(i)=[];
                elseif NTr>1 && (~isstruct(Info) || isequal(Info.SizeDim,1))
                    Out(i)=[];
                end
            case {'ATR','DTR','SBTR','SSTR','SBTRC','SSTRC'}
                % cross-section
                if NTr==0
                    Out(i)=[];
                elseif NTr>1 && isequal(Info.SizeDim,1)
                    Out(i)=[];
                end
            case {'CUM_SIMTHRUST','CUM_THRUST','CUM_POWER','INST_SIMTHRUST','INST_THRUST','INST_POWER','XYTURBINES','UTURBINES','POWERCOEF','THRUSTCOEF','LOSSCOEF'}
                % turbines
                if NTurbines==0
                    Out(i)=[];
                end
            case {'LINK_SUM','MORFAC','RINT'}
            case {'ZBAR'}
            otherwise
                % point
                if NSt==0
                    Out(i)=[];
                elseif NSt>1 && isequal(Info.SizeDim,1)
                    Out(i)=[];
                elseif Out(i).DimFlag(K_) && isequal(Info.SizeDim,1)
                    Out(i)=[];
                end
        end
    end
end

Info=vs_disp(FI,'his-dad-series','LINK_SUM');
if isstruct(Info) && Info.NDim==1
    id=strmatch('cumulative dredged material',{Out.Name},'exact');
    Out(id).SubFld=[];
end

%======================== SPECIFIC CODE ADD ===================================
sednames = lower(getsedimentnames(FI,'suspended'));
%
[names,Chk]=vs_get(FI,'his-const','NAMCON','quiet');
names=lower(cellstr(names));
[lstci,Chk]=vs_get(FI,'his-const','LSTCI','quiet');
if ischar(lstci)
    lstci=0;
end
[ltur,Chk]=vs_get(FI,'his-const','LTUR','quiet');
if ischar(ltur)
    ltur=0;
end
if lstci==0
    i=strmatch('--constituents',{Out.Name});
    Out(i)=[];
    i=strmatch('--avgconstituents',{Out.Name});
    Out(i)=[];
    i=strmatch('--cumconsflux',{Out.Name});
    Out(i)=[];
    i=strmatch('concentration',{Out.Name});
    Out(i)=[];
    i=strmatch('advective transport',{Out.Name});
    Out(i)=[];
    i=strmatch('dispersive transport',{Out.Name});
    Out(i)=[];
else
    SubsRepl={'--constituents'       '%s'                        ''
        '--avgconstituents'    'average %s'                      ''
        '--cumconsflux'        'cumulative flux of %s'           '*m^3'
        'concentration'        '%s concentration'                ''
        'advective transport'  'cum. advective transport of %s'  '*m^3'
        'dispersive transport' 'cum. dispersive transport of %s' '*m^3'};
    for iRepl=1:size(SubsRepl,1)
        i=strmatch(SubsRepl{iRepl,1},{Out.Name});
        if ~isempty(i) % will be empty if no stations/transect/discharges exist
            Ins=Out(i*ones(lstci,1));
            for j=1:lstci
                Ins(j).Name=sprintf(SubsRepl{iRepl,2},names{j});
                Ins(j).Xtra=j;
                Ins(j).Units=getunits(names{j},sednames);
                if ~isempty(Ins(j).Units) & ~isempty(SubsRepl{iRepl,3})
                    Ins(j).Units=[Ins(j).Units SubsRepl{iRepl,3}];
                end
            end
            Out=insstruct(Out,i,Ins);
        end
    end
end
FV_info=vs_disp(FI,'his-version','FILE-VERSION');
if isstruct(FV_info)
    hisversionstr = vs_get(FI,'his-version','FILE-VERSION','quiet!');
    hisversion = str2num(strrep(hisversionstr,'.',' '))*[1;0.01;.0001];
else
    hisversion = 0;
end
if hisversion<3.5211
    for i=1:length(Out)
        switch Out(i).Name
            case 'precipitation rate'
                Out(i).Units = 'm/s';
            case 'evaporation rate'
                Out(i).Units = 'kg/m^2/s';
        end
    end
end
for i=1:length(Out)
    if isequal(Out(i).Units,'*')
        Info = vs_disp(FI,Out(i).Group,Out(i).Val1);
        eUnit = strtrim(Info.ElmUnits(2:end-1));
        switch lower(eUnit)
            case 'm3'
                eUnit = 'm^3';
            case 'm3/s'
                eUnit = 'm^3/s';
            case 'kg'
                eUnit = 'kg';
            case 'kg/s'
                eUnit = 'kg/s';
            case ''
                Info = vs_disp(FI,Out(i).Group,'SBTR');
                eUnit = strtrim(Info.ElmUnits(2:end-1));
                switch lower(eUnit)
                    case 'm3/s'
                        eUnit = 'm^3';
                    case 'kg/s'
                        eUnit = 'kg';
                end
        end
        Out(i).Units = eUnit;
    end
end
if ltur==0
    i=strmatch('--turbquant',{Out.Name});
    Out(i)=[];
else
    i=strmatch('--turbquant',{Out.Name});
    Ins=Out(i*ones(ltur,1));
    for j=1:ltur
        Ins(j).Name=names{lstci+j};
        Ins(j).SubFld=j;
    end
    Out=insstruct(Out,i,Ins);
end

[Out.TemperatureType] = deal('unspecified');
[Out(strcmp({Out.Name},'temperature')).TemperatureType] = deal('absolute');
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if ischar(Props.SubFld)
    switch Props.SubFld
        case {'s','sb','s1','sb1'}
            Info=vs_disp(FI,'his-const','NAMSED');
            if isstruct(Info) && ismember(Props.SubFld,{'sb','sb1'})
                [names,Chk]=vs_get(FI,'his-const','NAMSED','quiet');
                subf=cellstr(names);
            else
                [names,Chk]=vs_get(FI,'his-const','NAMCON','quiet');
                names=cellstr(names);
                lnames=lower(names);
                i_sed=strmatch('sediment',lnames);
                subf=names(i_sed);
            end
            if length(subf)>1 && Props.SubFld(end)~='1'
                subf{end+1}='sum of all fractions';
            end
        case 'f'
            subf = {'from-to only','to-from only','two way'};
        case 'fs'
            Props.SubFld='s';
            subf1 = getsubfields(FI,Props);
            subf2 = {'from-to only','to-from only','two way'};
            subf = cell(1,numel(subf1)*numel(subf2));
            k = 0;
            for i = 1:length(subf1)
                for j = 1:length(subf2)
                    k = k+1;
                    subf{k} = sprintf('%s, %s',subf1{i},subf2{j});
                end
            end
    end
end
if nargin>2 && f~=0
    subf=subf(f);
end
% -----------------------------------------------------------------------------


% -------------------------------------------------------------------------
function DL=getlabels(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
DL={[] [] [] [] []};
if Props.DimFlag(K_)==7
    layers = vs_get(FI,'his-const','OUTPUT_LAYERS','quiet!');
    if strcmp(Props.Loc3D,'í')
        DL{K_} = layers;
    else
        DL{K_} = layers(layers~=0)';
    end
end
% -------------------------------------------------------------------------


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
    switch Props.Val1
        case {'XYTURBINES','UTURBINES','POWERCOEF','THRUSTCOEF','LOSSCOEF'}
            % turbines
            Info=vs_disp(FI,Props.Group,Props.Val1);
            sz(ST_)=Info.SizeDim(1);
        case {'INST_SIMTHRUST','CUM_SIMTHRUST','INST_THRUST','CUM_THRUST','INST_POWER','CUM_POWER'}
            % turbines
            Info=vs_disp(FI,Props.Group,Props.Val1);
            sz(ST_)=Info.SizeDim(1)+1;
        case {'RINT','ZQ_SUM','ZQ'}
            % discharges
            Info=vs_disp(FI,'his-dis-series',Props.Val1);
            sz(ST_)=Info.SizeDim(1);
        case {'ZBAR'}
            % barriers
            Info=vs_disp(FI,'his-const','NAMBAR');
            sz(ST_)=Info.SizeDim(1);
        case {'FLTR','CTR'}
            % cross-section and discharges
            [sz(ST_),Chk]=vs_get(FI,'his-const','NTRUV','quiet');
            Info=vs_disp(FI,'his-dis-const','DISCHARGES');
            if isstruct(Info)
                sz(ST_)=sz(ST_)+Info.SizeDim(1);
            end
        case {'ATR','DTR','SBTR','SSTR','SBTRC','SSTRC'}
            % cross-section
            [sz(ST_),Chk]=vs_get(FI,'his-const','NTRUV','quiet');
        case {'PLOUGH_TFRAC','DREDGE_TFRAC'}
            % dreding area
            Info=vs_disp(FI,'his-dad-const','DREDGE_AREAS');
            sz(ST_)=Info.SizeDim;
        case {'LINK_SUM'}
            % dredging link
            Info=vs_disp(FI,'his-dad-const','LINK_DEF');
            sz(ST_)=Info.SizeDim(1);
            Info=vs_disp(FI,'his-dad-const','DREDGE_AREAS');
            sz(ST_)=sz(ST_)+Info.SizeDim;
            Info=vs_disp(FI,'his-dad-const','DUMP_AREAS');
            sz(ST_)=sz(ST_)+Info.SizeDim;
        case {'BALVOLUME','BALAREAS','BALDPS','BALR1CONC'}
            % polygon
            Info=vs_disp(FI,'his-bal-const','BALAREAS');
            sz(ST_)=Info.SizeDim(1); % exclude non-polygons
        case {'BALFLUX','BALR1FLUX','BALSDFLUX'}
            % polygon interfaces
            FI.ElmDef(strcmp('BALNEIGHB',{FI.ElmDef.Name})).Type=3; % make sure BALNEIGB data is read as integers
            Info=vs_disp(FI,'his-bal-const','BALNEIGHB');
            sz(ST_)=Info.SizeDim(2);
        otherwise
            % point
            [sz(ST_),Chk]=vs_get(FI,'his-const','NOSTAT','quiet');
    end
end
if Props.DimFlag(K_)
    %  Info=vs_disp(FI,'his-const','THICK');
    %  sz(K_)=Info.SizeDim;
    Info=vs_disp(FI,Props.Group,Props.Val1);
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
FI = guarantee_options(FI);
[Dt,Chk]=vs_get(FI,'his-const','DT','quiet');
[Tunit,Chk]=vs_get(FI,'his-const','TUNIT','quiet');
[Date,Chk]=vs_get(FI,'his-const','ITDATE','quiet');
d0=tdelft3d(Date(1),Date(2));
if isequal(Props.Group,'his-const') || isequal(Props.Group,'his-bal-const')
    T=d0;
else
    switch lower(qp_option(FI,'displaytime'))
        case 'hydrodynamic time'
            grp = 'his-info-series';
            Info = vs_disp(FI,grp,[]);
            if ~isstruct(Info)
                grp = Props.Group;
            end
            %
            [T,Chk]=vs_let(FI,grp,{t},'ITHISC','quiet');
            if Chk==0
                error(T)
            end
            T=d0+T*Dt*Tunit/(24*3600);
        case 'morphologic time'
            [T,Chk]=vs_let(FI,'his-infsed-serie',{t},'MORFT','quiet');
            if Chk==0
                error(T)
            end
            T=d0+T;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
switch Props.Val1
    case {'XYTURBINES','UTURBINES','POWERCOEF','THRUSTCOEF','LOSSCOEF'}
        [S,Chk]=vs_get(FI,'his-const','NAMTURBINES','quiet');
    case {'INST_SIMTHRUST','CUM_SIMTHRUST','INST_THRUST','CUM_THRUST','INST_POWER','CUM_POWER'}
        [S,Chk]=vs_get(FI,'his-const','NAMTURBINES','quiet');
        S=cellstr(S);
        if length(S)>1
            S{end+1}='sum over all turbines';
        end
    case {'RINT','ZQ_SUM','ZQ'}
        [S,Chk]=vs_get(FI,'his-dis-const','DISCHARGES','quiet');
    case {'ZBAR'}
        [S,Chk]=vs_get(FI,'his-const','NAMBAR','quiet');
    case {'FLTR','CTR'}
        [NSt,Chk]=vs_get(FI,'his-const','NTRUV','quiet');
        if (NSt==0)
            S1='';
        else
            [S1,Chk]=vs_get(FI,'his-const','NAMTRA','quiet');
        end
        [S2,Chk]=vs_get(FI,'his-dis-const','DISCHARGES','quiet');
        if Chk
            S=strvcat(S1,S2);
        else
            S=S1;
        end
    case {'ATR','DTR','SBTR','SSTR','SBTRC','SSTRC'}
        % cross-section
        [S,Chk]=vs_get(FI,'his-const','NAMTRA','quiet');
    case {'DREDGE_TFRAC','PLOUGH_TFRAC'}
        [DredgeArea,Chk]=vs_get(FI,'his-dad-const','DREDGE_AREAS','quiet');
        S=cellstr(DredgeArea);
    case {'LINK_SUM'}
        % dredging link
        [ij,Chk]=vs_get(FI,'his-dad-const','LINK_DEF','quiet');
        [DredgeArea,Chk]=vs_get(FI,'his-dad-const','DREDGE_AREAS','quiet');
        DredgeArea=cellstr(DredgeArea);
        [DumpArea,Chk]=vs_get(FI,'his-dad-const','DUMP_AREAS','quiet');
        DumpArea=cellstr(DumpArea);
        S={};
        for i=1:size(ij,1)
            S{i,1}=sprintf('from %s to %s',DredgeArea{ij(i,1)},DumpArea{ij(i,2)});
        end
        for i=1:length(DredgeArea)
            S{end+1,1}=sprintf('dredged at %s',DredgeArea{i});
        end
        for i=1:length(DumpArea)
            S{end+1,1}=sprintf('dumped at %s',DumpArea{i});
        end
    case {'BALVOLUME','BALAREAS','BALDPS','BALR1CONC'}
        % polygon
        Info=vs_disp(FI,'his-bal-const','BALAREAS');
        [S,Chk]=vs_get(FI,'his-bal-const','BALVOLNAMES',{1:Info.SizeDim(1)},'quiet'); % exclude non-polygons
    case {'BALFLUX','BALR1FLUX','BALSDFLUX'}
        % polygon interfaces
        [BS,Chk]=vs_get(FI,'his-bal-const','BALVOLNAMES','quiet');
        BS = cellstr(BS);
        FI.ElmDef(strcmp('BALNEIGHB',{FI.ElmDef.Name})).Type=3; % make sure BALNEIGB data is read as integers
        [NB,Chk]=vs_get(FI,'his-bal-const','BALNEIGHB','quiet');
        nInt = size(NB,2);
        S = cell(1,nInt);
        for i = 1:nInt
            S{i} = sprintf('from %s to %s',BS{NB(1,i)},BS{NB(2,i)});
        end
    otherwise
        % point
        [S,Chk]=vs_get(FI,'his-const','NAMST','quiet');
end
if ~iscell(S)
    S=cellstr(S);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function zb = readzb(FI,idx)
T_=1; ST_=2; M_=3; N_=4; K_=5;

if isstruct(vs_disp(FI,'his-sed-series','ZDPS'))
    [dp,Chk]=vs_let(FI,'his-sed-series',idx(T_),'ZDPS',idx(ST_),'quiet');
elseif isstruct(vs_disp(FI,'his-series','DPS'))
    [dp,Chk]=vs_let(FI,'his-series',idx(T_),'DPS',idx(ST_),'quiet');
else
    [dp,Chk]=vs_let(FI,'his-const','DPS',idx(ST_),'quiet');
    dp=repmat(dp,length(idx{T_}),1);
end
dp(dp==-999) = NaN; % filter missing values for moving observation points
zb = -dp;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
FI = guarantee_options(FI);
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd,
    case 'initialize'
        OK=optfig(mfig);
        Info=vs_disp(FI,'his-infsed-serie','MORFT');
        if isstruct(Info)
            set(findobj(mfig,'tag','displaytime'),'enable','on');
            Hdispt=findobj(mfig,'tag','displaytime=?');
            dispnr=ustrcmpi(qp_option(FI,'displaytime'),get(Hdispt,'string'));
            set(Hdispt,'enable','on','backgroundcolor',Active,'value',dispnr);
        end
    case 'displaytime'
        Hdispt=findobj(mfig,'tag','displaytime=?');
        dispts=get(Hdispt,'string');
        if nargin>3
            dispstr=varargin{1};
            dispnr=find(strcmpi(dispstr,dispts));
            if isempty(dispnr)
                dispnr=1;
            end
            set(Hdispt,'value',dispnr)
        else
            dispnr=get(Hdispt,'value');
        end
        dispstr=dispts{dispnr};
        NewFI.QP_Options.displaytime=dispstr;
        cmdargs={cmd dispstr};
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
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 70 20], ...
    'String','Display Time', ...
    'Horizontalalignment','left', ...
    'Enable','off', ...
    'Tag','displaytime');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions displaytime', ...
    'Position',[91 voffset 240 20], ...
    'String',{'Hydrodynamic Time','Morphologic Time'}, ...
    'Enable','off', ...
    'Tag','displaytime=?');
OK=1;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function units=getunits(par,sednames)
switch lower(par)
    case 'temperature'
        units='°C';
    case 'salinity'
        units='ppt';
    case 'secondary flow'
        units='m/s';
    case 'turbulent energy' % turbulent kinetic energy
        units='m^2/s^2';
    case 'energy dissipation'
        units='m^2/s^3';
    case sednames
        units='kg/m^3';
    otherwise
        units='';
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sediments = getsedimentnames(FI,typ)
if nargin<2
    typ='all';
end
%
[names,Chk] = vs_get(FI,'his-const','NAMCON','quiet');
constituents = cellstr(names);
%
Info = vs_disp(FI,'his-const','NAMSED');
if isstruct(Info)
    [names,Chk] = vs_get(FI,'his-const','NAMSED','quiet');
    sediments = cellstr(names);
else
    L_constituents = lower(constituents);
    i_sed = strmatch('sediment',L_constituents);
    sediments = constituents(i_sed);
end
%
switch typ
    case 'all'
    case 'suspended'
        sediments = intersect(sediments,constituents);
    case 'bedload'
        sediments = setdiff(sediments,constituents);
end
% -----------------------------------------------------------------------------

function FI = guarantee_options(FI)
defopt = {'displaytime' 'hydrodynamic time'};
for i = 1:size(defopt,1)
    opt = defopt{i,1};
    val = defopt{i,2};
    if isequal(qp_option(FI,opt),[])
        FI = qp_option(FI,opt,val);
    end
end