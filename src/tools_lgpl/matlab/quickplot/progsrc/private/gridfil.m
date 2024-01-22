function varargout=gridfil(FI,domain,field,cmd,varargin)
%GRIDFIL QP support for Delft3D grid and attribute files.
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

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
ind=cell(1,5);
ind{2}=1;
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
        if i~=K_ && DimFlag(M_) && DimFlag(N_)
            if DataInCell && isequal(idx{i},1)
                idx{i}=[1 2];
                ind{i}=2;
            elseif DataInCell && idx{i}(1)==1
                ind{i}=2:length(idx{i});
            else
                idx{i}=[max(1,idx{i}(1)-1) idx{i}];
                % WARNING: the == sign in the following differs intentionally from the ~= sign
                % used in the equivalent line in the other routines! This results from a difference
                % in behaviour between the VS_LET routines and the methodology followed here.
                ind{i}=(1:(length(idx{i})-1))+1;%(idx{i}(1)==idx{i}(2));
            end
            allidx(i)=0;
        else % i==K_
            ind{i}=1:length(idx{i});
        end
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

% read grid ...
x=[];
y=[];
xy=[];
z=[];
dataongrid = DimFlag(M_) & DimFlag(N_);
if XYRead
    if dataongrid
        x=FI.X;
        y=FI.Y;
        %
        if strcmp(Props.Name,'clipped grid')
            [x,y]=enclosure('apply',FI.Enclosure,x,y);
        end
        %
        x=x(idx{[M_ N_]});
        y=y(idx{[M_ N_]});
        if idx{M_}(1)==idx{M_}(2)
            x(1,:)=NaN;
            y(1,:)=NaN;
        end
        if idx{N_}(1)==idx{N_}(2)
            x(:,1)=NaN;
            y(:,1)=NaN;
        end
    end
end

% prestore grid activity of depth points ...
act=~isnan(x)&~isnan(y);

if XYRead
    if dataongrid
        if DimFlag(K_)
            if DataInCell
                kmax = sz(K_)+1;
                switch Props.Loc3D
                    case 'i'
                        sigma = -[0 0.5:kmax-2.5 kmax-2];
                    otherwise
                        sigma = -(0:kmax-1);
                end
            else
                kmax = sz(K_);
                switch Props.Loc3D
                    case 'i'
                        sigma = -(0:kmax);
                    otherwise
                        sigma = -(0:kmax)-0.5;
                end
            end
            x = repmat(x,[1 1 length(idx{K_})]);
            y = repmat(y,[1 1 length(idx{K_})]);
            z = y;
            for k = 1:length(idx{K_})
                z(:,:,k) = sigma(idx{K_}(k));
            end
            x = reshape(x,[1 size(x)]);
            y = reshape(y,[1 size(y)]);
            z = reshape(z,[1 size(z)]);
        end
    end
end

% grid interpolation ...
[x,y]=gridinterp(DataInCell,DimFlag(K_),Props.ReqLoc,x,y);

% load data ...
%================== NEFIS SPECIFIC CODE =======================================
elidx=idx(2:end);
elidx(~DimFlag(2:end))=[];
val={};
ThinDam=0;
Dpsopt='mean';
if Props.File~=0
    if Props.File==-1
        Attrib.Data = FI.Enclosure;
        Attrib.FileType = 'enclosure';
        Attrib.QP_Options = [];
    else
        Fld=abs(Props.Fld);
        Attribs = qp_option(FI,'AttribFiles');
        Attrib = qp_unwrapfi(Attribs(Props.File));
    end
    filetp=Attrib.FileType;
    switch filetp
        case {'wldep','wlfdep','trirst','boxfile'}
            Dpsopt=qp_option(Attrib,'Dpsopt','default','max');
            if ~isempty(strmatch('velocity',Props.Name))
                val{1}=Attrib.Data{Fld(1)};
                val{2}=Attrib.Data{Fld(2)};
            elseif ~isempty(strmatch('horizontal velocity',Props.Name))
                k=length(Fld)/2;
                val{1}=cat(3,Attrib.Data{Fld(1:k)});
                val{2}=cat(3,Attrib.Data{Fld(k+(1:k))});
            else
                val{1}=cat(3,Attrib.Data{Fld});
            end
            for iv=1:length(val)
                if isfield(Attrib,'DOrder')
                    switch Attrib.DOrder
                        case 1
                            sz=size(val{iv}); sz(1:2)=sz([2 1]);
                            val{iv}=reshape(val{iv},sz);
                            val{iv}=permute(val{iv},[2 1 3:length(sz)]);
                        case 2
                            % default case
                        case 3
                            sz=size(val{iv}); sz(1:2)=sz([2 1]);
                            val{iv}=reshape(val{iv},sz);
                            val{iv}=permute(val{iv},[2 1 3:length(sz)]);
                            val{iv}=val{iv}(end:-1:1,:,:);
                        case 4
                            val{iv}=val{iv}(end:-1:1,:,:);
                        case 5
                            sz=size(val{iv}); sz(1:2)=sz([2 1]);
                            val{iv}=reshape(val{iv},sz);
                            val{iv}=permute(val{iv},[2 1 3:length(sz)]);
                            val{iv}=val{iv}(:,end:-1:1,:);
                        case 6
                            val{iv}=val{iv}(:,end:-1:1,:);
                        case 7
                            sz=size(val{iv}); sz(1:2)=sz([2 1]);
                            val{iv}=reshape(val{iv},sz);
                            val{iv}=permute(val{iv},[2 1 3:length(sz)]);
                            val{iv}=val{iv}(end:-1:1,end:-1:1,:);
                        case 8
                            val{iv}=val{iv}(end:-1:1,end:-1:1,:);
                    end
                end
            end
        case 'SWAN-output'
            val{1}=Attrib.Data(:,:,Props.Fld(1));
            if length(Props.Fld)==2
                val{2}=Attrib.Data(:,:,Props.Fld(2));
            end
        case {'fls','FLS-inc'}
            if isfield(Attrib,'Times')
                T = Attrib.Times(idx{T_});
            else
                T = idx{T_}/60;
            end
            [val{1},Fls]=incremental('read',Attrib,Fld,T);
            val{1}(val{1}==0)=NaN;
            Attribs(Props.File) = qp_wrapfi(Fls,Attribs(Props.File));
            FI=qp_option(FI,'AttribFiles',Attribs);
        case 'bagmap'
            val{1}=bagmap('read',Attrib,idx{T_},Props.Fld);
        case {'weir','weir-waqua','thindam','thindam-waqua','enclosure','3dgate','rigidsheet'}
            ThinDam=1;
            bedsigncorrection=-1;
            weirheight=2;
            if isequal(filetp,'weir-waqua')
                bedsigncorrection=1;
                weirheight=1;
            end
            val{1}=zeros(size(FI.X));
            val{2}=zeros(size(FI.X));
            %val{2}=val{1}; resulteert in 4 keer dezelfde matrix in de gecompileerde versie,
            %er wordt dus geen goed onderscheid gemaakt tussen de verschillende kopieen!
            DamVal=0;
            if isfield(Attrib,'CHARu')
                DamVal=size(Attrib.CHARu,2)>=3;
            end
            if DamVal
                Props.NVal=2;
                val{3}=zeros(size(FI.X));
                val{4}=zeros(size(FI.X));
            end
            if isfield(Attrib,'MNu')
                MNu=Attrib.MNu;
            elseif isfield(Attrib,'MNKu')
                MNKu=Attrib.MNKu;
                MNu = MNKu(MNKu(:,5)<=idx{K_} & MNKu(:,6)>=idx{K_}, 1:4);
            else
                [MNu,MNv]  =enclosure('thindam',Attrib.Data);
            end
            if ~isempty(MNu)
                MNu(:,[3 4]) = MNu(:,[3 4])-MNu(:,[1 2]);
                if any( ~( (abs(MNu(:,3))==abs(MNu(:,4))) | ...
                        abs(MNu(:,3))==0              | ...
                        abs(MNu(:,4))==0              ) )
                    error('Invalid combination of MNu coordinates.')
                end
                xx=max(abs(MNu(:,[3 4]))+1,[],2);
                n=max(xx);
                M=repmat(MNu(:,1),1,n)+sign(MNu(:,3))*(0:(n-1));
                N=repmat(MNu(:,2),1,n)+sign(MNu(:,4))*(0:(n-1));
                if size(M,2)>1
                    M=M';
                    N=N';
                end
                I=repmat(0:(n-1),size(MNu,1),1)<repmat(xx,1,n); I=I';
                Indu=repmat(1:size(MNu,1),n,1);
                Indu=Indu(I);
                M=M(I);
                N=N(I);
                val{1}(sub2ind(size(val{1}),M,N))=1;
                %      val{1}(:)=0;
                if DamVal
                    val{3}(sub2ind(size(val{1}),M,N))=bedsigncorrection*Attrib.CHARu(Indu,weirheight);
                end
            end
            if isfield(Attrib,'MNv')
                MNv=Attrib.MNv;
            elseif isfield(Attrib,'MNKv')
                MNKv=Attrib.MNKv;
                MNv=MNKv(MNKv(:,5)<=idx{K_} & MNKv(:,6)>=idx{K_},1:4);
            else
                % MNv already defined above
            end
            if ~isempty(MNv)
                MNv(:,[3 4])=MNv(:,[3 4])-MNv(:,[1 2]);
                if any( ~( (abs(MNv(:,3))==abs(MNv(:,4))) | ...
                        abs(MNv(:,3))==0              | ...
                        abs(MNv(:,4))==0              ) )
                    error('Invalid combination of MNv coordinates.')
                end
                xx=max(abs(MNv(:,[3 4]))+1,[],2);
                n=max(xx);
                M=repmat(MNv(:,1),1,n)+sign(MNv(:,3))*(0:(n-1)); M=M';
                N=repmat(MNv(:,2),1,n)+sign(MNv(:,4))*(0:(n-1)); N=N';
                I=repmat(0:(n-1),size(MNv,1),1)<repmat(xx,1,n); I=I';
                Indv=repmat(1:size(MNv,1),n,1);
                Indv=Indv(I);
                M=M(I);
                N=N(I);
                val{2}(sub2ind(size(val{2}),M,N))=1;
                %      val{2}(:)=0;
                if DamVal
                    val{4}(sub2ind(size(val{2}),M,N))=bedsigncorrection*Attrib.CHARv(Indv,weirheight);
                end
            end
            if DimFlag(K_)
                %DimFlag(K_)=0;
                idx{K_}=[];
                elidx(K_-2)=[];
            end
        case {'trtarea'}
            val{1}=zeros(size(FI.X));
            ReqCode=Attrib.RoughnessIDs(Props.SubFld);
            records=Attrib.Records(Attrib.Records(:,5)==ReqCode,:);
            val{1}=val{1}+sparse(records(:,2),records(:,1),records(:,6),size(val{1},1),size(val{1},2));
        case {'cross-sections','barriers'}
            MN = Attrib.MNMN;
            if strcmp(Props.Geom,'POLYL')
                i=idx{ST_};
                if MN(i,1)==MN(i,3)
                    m = MN(i,1);
                    n = min(MN(i,[2 4]))-1:max(MN(i,[2 4]));
                    xy = {[FI.X(m,n)' FI.Y(m,n)']};
                else
                    m = min(MN(i,[1 3]))-1:max(MN(i,[1 3]));
                    n = MN(i,2);
                    xy = {[FI.X(m,n) FI.Y(m,n)]};
                end
            else
                ThinDam=1;
                val{1}=zeros(size(FI.X));
                val{2}=zeros(size(FI.X));
                for i=1:size(MN,1)
                    if MN(i,1)==MN(i,3)
                        val{1}(MN(i,1),min(MN(i,[2 4])):max(MN(i,[2 4])))=1;
                    else
                        val{2}(min(MN(i,[1 3])):max(MN(i,[1 3])),MN(i,2))=1;
                    end
                end
            end
        case {'openboundary'}
            MN = Attrib.MN;
            if strcmp(Props.Geom,'POLYL')
                j=find(Attrib.BndType==Props.Fld);
                i=j(idx{ST_});
                %
                if MN(i,2)~=MN(i,4) && MN(i,1)==MN(i,3)
                    m = MN(i,1);
                    n = max(min(MN(i,[2 4]))-1,1):max(MN(i,[2 4]));
                    xy = {[FI.X(m,n)' FI.Y(m,n)']};
                    if any(isnan(xy{1}(:))) && m>1
                        xy = {[FI.X(m-1,n)' FI.Y(m-1,n)']};
                    end
                elseif MN(i,1)~=MN(i,3) && MN(i,2)==MN(i,4)
                    m = max(min(MN(i,[1 3]))-1,1):max(MN(i,[1 3]));
                    n = MN(i,2);
                    xy = {[FI.X(m,n) FI.Y(m,n)]};
                    if any(isnan(xy{1}(:))) && n>1
                        xy = {[FI.X(m,n-1) FI.Y(m,n-1)]};
                    end
                elseif MN(i,1)~=MN(i,3) && MN(i,2)~=MN(i,4) % diagonal water level boundary
                    dMN = MN(i,[3 4]) - MN(i,[1 2]);
                    DM  = abs(dMN(1));
                    dMN = sign(dMN);
                    %
                    % number of water level boundary points = DM+1
                    % number of grid points representing boundary = 2*(DM+1)+1
                    %
                    xy = NaN(2*(DM+1)+1,2);
                    dMN0 = [0 0];
                    dMN0(dMN>0) = -1;
                    j1 = 1;
                    m = MN(i,1)+dMN0(1);
                    n = MN(i,2)+dMN0(2);
                    for j = 0:DM
                        xy(j1,:) = [FI.X(m,n) FI.Y(m,n)];
                        %
                        m = m+dMN(1);
                        xy(j1+1,:) = [FI.X(m,n) FI.Y(m,n)];
                        if any(isnan(xy(j1+1,:)))
                            xy(j1+1,:) = [FI.X(m-dMN(1),n+dMN(2)) FI.Y(m-dMN(1),n+dMN(2))];
                        end
                        n = n+dMN(2);
                        j1= j1+2;
                        if j==DM
                            xy(j1,:) = [FI.X(m,n) FI.Y(m,n)];
                        end
                    end
                    xy = {xy};
                else % single point
                    if MN(i,1)>1
                        m = MN(i,1)+(-1:0);
                        n = MN(i,2);
                        xy = {[FI.X(m,n) FI.Y(m,n)]};
                    else
                        xy = {[NaN NaN]};
                    end
                    if any(isnan(xy{1}(:))) && MN(i,2)>1
                        m = MN(i,1);
                        n = MN(i,2)+(-1:0);
                        xy = {[FI.X(m,n)' FI.Y(m,n)']};
                    end
                    if any(isnan(xy{1}(:))) && MN(i,1)>1 && MN(i,2)>1
                        m = MN(i,1)+(-1:0);
                        n = MN(i,2)-1;
                        xy = {[FI.X(m,n) FI.Y(m,n)]};
                    end
                    if any(isnan(xy{1}(:))) && MN(i,1)>1 && MN(i,2)>1
                        m = MN(i,1)-1;
                        n = MN(i,2)+(-1:0);
                        xy = {[FI.X(m,n)' FI.Y(m,n)']};
                    end
                end
            else
                ThinDam=1;
                val{1}=zeros(size(FI.X));
                val{2}=zeros(size(FI.X));
                for i=1:length(Attrib.Name)
                    if strcmp(Attrib.BndType(i),Props.Fld)
                        if MN(i,2)~=MN(i,4) && MN(i,1)==MN(i,3)
                            val{1}(MN(i,1),min(MN(i,[2 4])):max(MN(i,[2 4])))=1;
                            if MN(i,1)>1
                                val{1}(MN(i,1)-1,min(MN(i,[2 4])):max(MN(i,[2 4])))=1;
                            end
                        elseif MN(i,1)~=MN(i,3) && MN(i,2)==MN(i,4)
                            val{2}(min(MN(i,[1 3])):max(MN(i,[1 3])),MN(i,2))=1;
                            if MN(i,2)>1
                                val{2}(min(MN(i,[1 3])):max(MN(i,[1 3])),MN(i,2)-1)=1;
                            end
                        elseif MN(i,1)~=MN(i,3) && MN(i,2)~=MN(i,4) % diagonal water level boundary
                            dMN = MN(i,[3 4]) - MN(i,[1 2]);
                            DM  = abs(dMN(1));
                            dMN = sign(dMN);
                            for j = 0:DM
                                val{1}(MN(i,1)+dMN(1)*j,MN(i,2)+dMN(2)*j)=1;
                                val{2}(MN(i,1)+dMN(1)*j,MN(i,2)+dMN(2)*j)=1;
                                if MN(i,1)+dMN(1)*j>1
                                    val{1}(MN(i,1)+dMN(1)*j-1,MN(i,2)+dMN(2)*j)=1;
                                end
                                if MN(i,2)+dMN(2)*j>1
                                    val{2}(MN(i,1)+dMN(1)*j,MN(i,2)+dMN(2)*j-1)=1;
                                end
                            end
                        else % single point
                            val{1}(MN(i,1),MN(i,2))=1;
                            if MN(i,1)>1
                                val{1}(MN(i,1)-1,MN(i,2))=1;
                            end
                            val{2}(MN(i,1),MN(i,2))=1;
                            if MN(i,2)>1
                                val{2}(MN(i,1),MN(i,2)-1)=1;
                            end
                        end
                    end
                end
            end
        case {'observation points','discharge stations'}
            switch filetp
                case 'observation points'
                    MN=Attrib.MN(idx{M_},:);
                case 'discharge stations'
                    switch Props.Fld
                        case 1
                            MN=Attrib.MNK(idx{M_},[1 2]);
                        case 2
                            MN=Attrib.MNK_out(idx{M_},[1 2]);
                    end
            end
            linidx=sub2ind(size(FI.X),MN(:,1),MN(:,2));
            [x,y]=gridinterp(0,0,'z',FI.X,FI.Y);
            x=x(linidx);
            y=y(linidx);
            val{1}=Attrib.Name; % (idx{M_}) indexing done after this switch statement
        case {'drypoint'}
            val{1}=NaN(size(FI.X));
            for i=1:size(Attrib.MN,1)
                i1 = Attrib.MN(i,[1 3]);
                i2 = Attrib.MN(i,[2 4]);
                val{1}(min(i1):max(i1),min(i2):max(i2))=1;
            end
        otherwise
            error('Reading data from %s file not yet implemented.',Attribs(Props.File).FileType)
    end
    for i=1:length(val)
        val{i}=val{i}(elidx{:});
        val{i}=reshape(val{i},[1 size(val{i})]);
    end
else
    switch(Props.Name)
        case {'nm cell index','nm cell index (DD simulation)','nm node index','nm node index (DD simulation)'}
            if length(Props.Name)>14 && strcmp(Props.Name(end-14:end),'(DD simulation)')
                ddb = 1;
            else
                ddb = 0;
            end
            %
            nmax = sz(N_); % nmaxus
            if nmax == 2*round(nmax/2)
                nmax = nmax+1; % nmax always odd
            end
            mmax = sz(M_);
            nlb = 1-ddb;
            nub = nmax+ddb;
            mlb = -1-ddb;
            mub = mmax+2+ddb;
            nmmax = (mmax+2*ddb) * (nmax+2*ddb);
            nmlb = 1 - 2*(nmax+2*ddb);
            nmub = nmmax + 2*(nmax+2*ddb);
            full = reshape(nmlb:nmub,[nub-nlb+1 mub-mlb+1]);
            full = permute(full(1+ddb:end-ddb,3+ddb:end-2-ddb),[3 2 1]); % permute M and N and add 1 as time dimension in front
            val{1}=full(1,elidx{:});
        case {'(m,n) cell indices','(m,n) node indices'}
            val{1}=cell(1, length(elidx{1}), length(elidx{2}));
            for m = 1:length(elidx{1})
                for n = 1:length(elidx{2})
                    val{1}{1,m,n} = sprintf('(%i,%i)',elidx{1}(m),elidx{2}(n));
                end
            end
    end
end

if dataongrid
    % data interpolation ...
    if DataInCell && isequal(Props.ReqLoc,'d') && Props.NVal>0
        Props.ReqLoc='z';
    end
    if isequal(Props.Loc,'d') && isequal(Props.ReqLoc,'z')
        val{1}=interp2cen(val{1},'t',Dpsopt);
    elseif isequal(Props.Loc,'u') && isequal(Props.ReqLoc,'z')
        [val{1},val{2}]=uv2cen(val{1},val{2});
    end
    
    % combine vectors components ...
    if isequal(Props.VecType,'m')
        [val{1},val{2}]=dir2uv(val{1},val{2});
    elseif isequal(Props.VecType,'u')
        % rotate n,m components into x,y direction ...
        % [alf,Chk] = vs_get(FI,'GRID','ALFAS',idx([M_ N_]),'quiet');
        xD=FI.X(idx{[M_ N_]});
        yD=FI.Y(idx{[M_ N_]});
        
        n=2:size(xD,2);
        xU(:,n)=(xD(:,n)+xD(:,n-1))/2;
        xU(:,1)=NaN;
        yU(:,n)=(yD(:,n)+yD(:,n-1))/2;
        yU(:,1)=NaN;
        m=2:size(xD,1);
        DxU(m,:)=xU(m,:)-xU(m-1,:);
        DyU(m,:)=yU(m,:)-yU(m-1,:);
        
        alf=atan2(DyU,DxU);
        
        %alf=0;
        %alf = alf*pi/180;
        [val{1},val{2}]=cur2ca(val{1},val{2},alf);
    end
    
    %======================== SPECIFIC CODE =======================================
    %select active points ...
    gridact=act;
    switch Props.ReqLoc
        case 'd'
            % done above:
            % act=~isnan(x)&~isnan(y);
        otherwise
            % done above:
            % act=~isnan(x)&~isnan(y);
            if size(act,1)>2 && size(act,2)>2
                act=conv2(double(act),[0 0 0; 0 1 1;0 1 1],'same')>3;
            elseif size(act,1)>2
                act=conv2(double(act),[0;1;1],'same')==2;
            elseif size(act,2)>2
                act=conv2(double(act),[0 1 1],'same')==2;
            end
    end
    %if ~DataInCell % overrule initialised gridact dataset ...
    %  gridact=act;
    %end
    
    %========================= GENERAL CODE =======================================
    if XYRead
        if DimFlag(K_)
            szx=[size(x) 1]; % extent szx for the case that dataset in K dir. is 1
            szx1=szx([1:2 4:end]);
            szx1(2)=szx(2)*szx(3);
            x=reshape(x,szx1);
            x(:,gridact<=0,:)=NaN;
            x=reshape(x,szx);
            y=reshape(y,szx1);
            y(:,gridact<=0,:)=NaN;
            y=reshape(y,szx);
            %---
            szz=[size(z) 1]; % extent szx for the case that dataset in K dir. is 1
            szz1=szz([1:2 4:end]);
            szz1(2)=szz(2)*szz(3);
            z=reshape(z,szz1);
            z(:,act<=0,:)=NaN;
            z=reshape(z,szz);
        else
            x(gridact<=0)=NaN;
            y(gridact<=0)=NaN;
        end
    end
    if Props.NVal>0 && Props.NVal~=4
        szz=[size(val{1}) 1]; % extent szx for the case that dataset in K dir. is 1
        szz1=szz([1:2 4:end]);
        szz1(2)=szz(2)*szz(3);
        val{1}=reshape(val{1},szz1);
        val{1}(:,act<=0,:)=NaN;
        val{1}=reshape(val{1},szz);
        for i=2:length(val),
            val{i}(:,isnan(val{1}))=NaN;
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
    if ~all(allidx(DimMask & DimFlag))
        if XYRead
            if DataInCell
                if DimFlag(M_) && DimFlag(N_) && DimFlag(K_)
                    z=z(:,ind{[M_ N_]},:);
                end
            else
                if DimFlag(M_) && DimFlag(N_)
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
        for i=1:length(val)
            val{i}=val{i}(:,ind{:});
        end
    end
    
    %========================= GENERAL CODE =======================================
    
    % reshape if a single timestep is selected ...
    if DimFlag(ST_)
        sz=[size(val{1}) 1]; sz(2)=[];
        for i=1:length(val)
            val{i}=reshape(val{i},sz);
        end
    end
    
    % reshape if a single timestep is selected ...
    if ~DimFlag(T_) || (DimFlag(T_) && isequal(size(idx{T_}),[1 1]))
        sz=size(x); sz=[sz(2:end) 1];
        if DimFlag(K_)
            x=reshape(x,sz);
            y=reshape(y,sz);
            if DimFlag(K_)
                sz=size(z); sz=[sz(2:end) 1];
                z=reshape(z,sz);
            end
        end
        if length(val)>0
            sz=size(val{1}); sz=[sz(2:end) 1];
            for i=1:length(val)
                val{i}=reshape(val{i},sz);
            end
        end
    end
    
    % apply minus sign if requested
    for iv=1:length(val)
        if Props.Fld<0
            val{iv}=-val{iv};
        end
    end
end

% generate output ...
if XYRead
    if ~isempty(xy)
        Ans.XY = xy;
    else
        Ans.X=x;
        Ans.Y=y;
        Ans.XUnits='m';
        Ans.YUnits='m';
    end
    if isfield(FI,'CoordinateSystem') && isequal(lower(FI.CoordinateSystem),'spherical')
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    end
    if DimFlag(K_)
        Ans.Z=z;
        Ans.ZUnits='m';
    end
end

if Props.NVal==0
    if ThinDam
        Ans.XDam=val{1};
        Ans.YDam=val{2};
    end
elseif Props.NVal==1 || Props.NVal==4 || Props.NVal==5
    Ans.Val=val{1};
else
    if ThinDam
        Ans.XDam=val{1};
        Ans.YDam=val{2};
        if length(val)>2
            Ans.XDam=Ans.XDam==1;
            Ans.YDam=Ans.YDam==1;
            Ans.XDamVal=val{3};
            Ans.YDamVal=val{4};
        end
    else
        Ans.XComp=val{1};
        Ans.YComp=val{2};
    end
end

% read time ...
T=readtim(FI,Props,idx{T_});
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%Str=sprintf('%s (%s)',Attribs(i).FileType,AttribName);
[p,f]=fileparts(FI.FileName);
Str = sprintf('enclosure (%s.enc)',f);
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                    'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Loc3D' 'File' 'Fld'};
DataProps={'morphologic grid'         'sQUAD' 'xy'    [0 0 1 1 0]  0          0     ''        'd'   'd'      ''      0      0
    'hydrodynamic grid'               'sQUAD' 'xy'    [0 0 1 1 0]  0          0     ''        'z'   'z'      'i'     0      0
    Str                               'sQUAD' 'xy'    [0 0 1 1 0]  0          0     ''        'd'   'd'      ''     -1      1
    'clipped grid'                    'sQUAD' 'xy'    [0 0 1 1 0]  0          0     ''        'd'   'd'      ''      0      0
    '-------'                         ''      ''      [0 0 0 0 0]  0          0     ''        ''    ''       ''      0      0
    '(m,n) node indices'              'sQUAD' 'xy'    [0 0 1 1 0]  0          4     ''        'd'   'd'      'i'     0      0
    'nm node index'                   'sQUAD' 'xy'    [0 0 1 1 0]  0          1     ''        'd'   'd'      'i'     0      0
    'nm node index (DD simulation)'   'sQUAD' 'xy'    [0 0 1 1 0]  0          1     ''        'd'   'd'      'i'     0      0
    '-------'                         ''      ''      [0 0 0 0 0]  0          0     ''        ''    ''       ''      0      0
    '(m,n) cell indices'              'sQUAD' 'xy'    [0 0 1 1 0]  1          4     ''        'z'   'z'      'i'     0      0
    'nm cell index'                   'sQUAD' 'xy'    [0 0 1 1 0]  1          1     ''        'z'   'z'      'i'     0      0
    'nm cell index (DD simulation)'   'sQUAD' 'xy'    [0 0 1 1 0]  1          1     ''        'z'   'z'      'i'     0      0   };

%======================== SPECIFIC CODE DIMENSIONS ============================
Attribs = qp_option(FI,'AttribFiles');
if ~isempty(Attribs)
    l=size(DataProps,1);
    for i=1:length(Attribs)
        Attrib = qp_unwrapfi(Attribs(i));
        AttribName = relativepath(Attribs(i).Name,FI.FileName);
        switch Attribs(i).FileType
            case {'wldep','wlfdep','boxfile'}
                for j=1:length(Attrib.Data)
                    l=l+1;
                    if length(Attrib.Data)>1
                        Str=sprintf('field %i of %s',j,AttribName);
                    else
                        Str=AttribName;
                    end
                    DataProps(l,:)={Str   'sQUAD' 'xy'    [0 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      j   };
                    L=l;
                    if length(Attrib.Data)==1
                        l=l+1;
                        DataProps(l,:)= ...
                            {strcat('-',Str) 'sQUAD' 'xy'   [0 0 1 1 0]  1          1     ''        'd'   'd'      ''      i     -j   };
                        L=[L l];
                    end
                    if isequal(qp_option(Attribs(i),'DLocation'),'cell centres')
                        DataProps(L,8:9)={'z'};
                    end
                end
            case 'SWAN-output'
                nd = ndims(Attrib.Data);
                j = 1;
                while j<size(Attrib.Data,nd)
                    l=l+1;
                    Str = Attrib.Parameters{j};
                    if isequal(Attrib.Parameters{j}(end-1:end),'-X')
                        DataProps(l,:)={Str(1:end-2)   'sQUAD' 'xy'    [0 0 1 1 0]  1          2     ''        'd'   'd'      ''      i      [j j+1]   };
                        j = j+2;
                    else
                        DataProps(l,:)={Str   'sQUAD' 'xy'    [0 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      j   };
                        j = j+1;
                    end
                end
            case 'trirst'
                l=l+1;
                Str=sprintf('water level from %s',AttribName);
                DataProps(l,:)={Str       'sQUAD' 'xy'   [0 0 1 1 0]  1          1     ''        'z'   'z'      ''      i      1   };
                [NLyr,NSubs,NTurb,NRem]=DetectFld(Attribs(i));
                l=l+1;
                if NLyr==1
                    Str=sprintf('velocity from %s',AttribName);
                    DataProps(l,:)={Str    'sQUAD' 'xy'   [0 0 1 1 0]  1          2     'u'       'u'   'z'      ''      i      1+(1:(2*NLyr))   };
                else
                    Str=sprintf('horizontal velocity from %s',AttribName);
                    DataProps(l,:)={Str    'sQUAD+' 'xy+z' [0 0 1 1 1]  1          2     'u'       'u'   'z'      'c'     i      1+(1:(2*NLyr))   };
                end
                offset=1+2*NLyr;
                for j=1:NSubs
                    l=l+1;
                    Str=sprintf('substance %i of %s',j,AttribName);
                    DataProps(l,:)={Str    'sQUAD' 'xy'   [0 0 1 1 0]  1          1     ''        'z'   'z'      'c'      i      offset+(j-1)*NLyr+(1:NLyr)   };
                    if NLyr>1
                        DataProps(l,2:4) = {'sQUAD+' 'xy+z' [0 0 1 1 1]};
                    end
                end
                offset=offset+NSubs*NLyr;
                for j=1:NTurb
                    l=l+1;
                    Str=sprintf('turb.quant. %i of %s',j,AttribName);
                    DataProps(l,:)={Str    'sQUAD+' 'xy+z' [0 0 1 1 1]  1          1     ''        'z'   'z'      'i'      i      offset+(j-1)*(NLyr+1)+(1:(NLyr+1))   };
                end
                offset=offset+NTurb*(NLyr+1);
                for j=1:NRem
                    l=l+1;
                    Str=sprintf('field %i of %s',j,AttribName);
                    DataProps(l,:)={Str    'sQUAD' 'xy'   [0 0 1 1 0]  1          1     ''        'z'   'z'      ''      i      offset+j   };
                end
            case 'trtarea'
                l=l+1;
                DataProps(l,:)={'area fraction' ...
                    'sQUAD'     'xy'                   [1 0 1 1 0]  1          1     ''        'z'   'z'      ''      i      1   };
            case {'fls','FLS-inc'}
                if isfield(Attrib,'StartTime')
                    it = 1;
                else
                    it = 3;
                end
                for j=1:length(Attrib.Quant)
                    if isempty(Attrib.Quant(j).Class)
                        continue
                    end
                    switch Attrib.Quant(j).Name
                        case {'H','Waterdepth(m)'}
                            Name = 'classified water depth';
                        case {'C','Velocity(m/s)','M'}
                            Name = 'classified velocity magnitude';
                        case {'Z','Waterlevel(m)'}
                            Name = 'classified water level';
                        case {'U','U-velocity(m/s)'}
                            Name = 'classified u velocity';
                        case {'V','V-velocity(m/s)'}
                            Name = 'classified v velocity';
                        case 'A'
                            Name = 'classified velocity angle';
                        otherwise
                            Name = Attrib.Quant(j).Name;
                            if isempty(Name)
                                Name = sprintf('Quantity %i',j);
                            end
                    end
                    l=l+1;
                    DataProps(l,:)={Name ...
                        'sQUAD'     'xy'                   [it 0 1 1 0]  1          1     ''        'z'   'z'      ''      i      j   };
                end
            case {'weir','weir-waqua','thindam','thindam-waqua'}
                type=Attribs(i).FileType;
                if length(type)>6 && isequal(type(end-5:end),'-waqua')
                    type=type(1:end-6);
                end
                if strcmp(type,'thindam')
                    type = 'thin dam';
                end
                l=l+1;
                Str=sprintf('%ss (%s)',type,AttribName);
                DamVal=size(Attrib.CHARu,2)>=3;
                NVal=0.5*DamVal;
                DataProps(l,:)={Str       'sQUAD' 'xy'   [0 0 1 1 0]  0       NVal    ''        'd'   'd'      ''      i      1   };
            case {'3dgate'}
                l=l+1;
                Str=sprintf('3D gate (%s)',AttribName);
                DataProps(l,:)={Str       'sQUAD' 'xy'   [0 0 1 1 5]  0        0      ''        'd'   'd'      ''      i      1   };
            case {'rigidsheet'}
                l=l+1;
                Str=sprintf('Sheet or plate (%s)',AttribName);
                DataProps(l,:)={Str       'sQUAD' 'xy'   [0 0 1 1 5]  0        0      ''        'd'   'd'      ''      i      1   };
            case {'observation points'}
                l=l+1;
                Str=sprintf('observation points (%s)',AttribName);
                DataProps(l,:)={...
                    Str 'sQUAD' 'xy'  [0 0 1 0 0]  0       4    ''        ''    ''       ''      i      1   };
            case {'discharge stations'}
                l=l+1;
                Str=sprintf('discharge stations (%s)',AttribName);
                DataProps(l,:)={...
                    Str 'sQUAD' 'xy'  [0 0 1 0 0]  0       4    ''        ''    ''       ''      i      1   };
                if isfield(Attrib,'MNK_out')
                    l=l+1;
                    Str=sprintf('discharge station outlets (%s)',AttribName);
                    DataProps(l,:)={...
                        Str 'sQUAD' 'xy'  [0 0 1 0 0]  0       4    ''        ''    ''       ''      i      2   };
                end
            case {'drypoint'}
                l=l+1;
                Str=sprintf('dry points (%s)',AttribName);
                DataProps(l,:)={...
                    Str 'sQUAD' 'xy'  [0 0 1 1 0]  2       5    ''        ''    ''       ''      i      1   };
            case {'openboundary'}
                Types = unique(Attrib.BndType);
                for bndTyp=1:length(Types)
                    l=l+1;
                    switch Types(bndTyp)
                        case 'Z'
                            name = 'water level';
                        case 'C'
                            name = 'normal velocity';
                        case 'Q'
                            name = 'discharge per cell';
                        case 'T'
                            name = 'total discharge';
                        case 'R'
                            name = 'Riemann';
                        case 'N'
                            name = 'Neumann';
                        otherwise
                            name = Types(bndTyp);
                    end
                    Str=sprintf('%s boundaries (%s)',name,AttribName);
                    Str2=sprintf('%s boundary (%s)',name,AttribName);
                    DataProps(l,:)={Str ...
                        'sQUAD' 'xy'  [0 0 1 1 0]  0       0    ''        'd'   'd'      ''      i      Types(bndTyp)   };
                    l=l+1;
                    DataProps(l,:)={Str2 ...
                        'POLYL' 'xy'  [0 5 0 0 0]  0       0    ''        'd'   'd'      ''      i      Types(bndTyp)   };
                end
            case {'cross-sections','barriers'}
                type=Attribs(i).FileType;
                l=l+1;
                Str=sprintf('%s (%s)',type,AttribName);
                Str2=sprintf('%s (%s)',type(1:end-1),AttribName);
                DataProps(l,:)={Str ...
                    'sQUAD' 'xy'  [0 0 1 1 0]  0       0    ''        'd'   'd'      ''      i      1   };
                l=l+1;
                DataProps(l,:)={Str2 ...
                    'POLYL' 'xy'  [0 5 0 0 0]  0       0    ''        'd'   'd'      ''      i      1   };
            case 'bagmap'
                switch Attrib.Quantity
                    case 'results dredging volumes'
                        l=l+1;
                        DataProps(l,:)={'dredged volume' ...
                            'sQUAD' 'xy' [5 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      1   };
                        l=l+1;
                        DataProps(l,:)={'dumped volume' ...
                            'sQUAD' 'xy' [5 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      2   };
                    case 'results cumulative dredging volumes'
                        l=l+1;
                        DataProps(l,:)={'cumulative dredged volume' ...
                            'sQUAD' 'xy' [5 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      1   };
                        l=l+1;
                        DataProps(l,:)={'cumulative dumped volume' ...
                            'sQUAD' 'xy' [5 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      2   };
                    case {'bed increment over one time step','results bed increment over one time step'}
                        l=l+1;
                        DataProps(l,:)={'bed level increment' ...
                            'sQUAD' 'xy' [5 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      1   };
                    otherwise
                        for j=1:size(Attrib.Offset,1)
                            l=l+1;
                            Str=sprintf('%s: field %i',Attrib.Quantity,j);
                            DataProps(l,:)={Str 'sQUAD' 'xy' [5 0 1 1 0]  1          1     ''        'd'   'd'      ''      i      j   };
                        end
                end
            case 'enclosure'
                l=l+1;
                Str=sprintf('%s (%s)',Attribs(i).FileType,AttribName);
                DataProps(l,:)={Str      'sQUAD'     'xy' [0 0 1 1 0]  0          0      ''        'd'   'd'      ''      i      1   };
            otherwise
        end
    end
end
Out=cell2struct(DataProps,PropNames,2);

%--- set UseGrid options ...
RL={Out(:).ReqLoc};
[Out(strcmp('d',RL)).UseGrid]=deal(1);
[Out(strcmp('z',RL)).UseGrid]=deal(2);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if Props.File>0
    Attribs = qp_option(FI,'AttribFiles');
    Attrib=qp_unwrapfi(Attribs(Props.File));
    switch Attrib.FileType
        case 'trtarea'
            nrid=length(Attrib.RoughnessIDs);
            subf=cell(nrid,1);
            for i=1:nrid
                subf{i}=sprintf('roughness code %i',Attrib.RoughnessIDs(i));
            end
        otherwise
            % default no subfields
    end
end
if nargin>2 && f~=0
    subf=subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
%======================== SPECIFIC CODE =======================================
Attribs = qp_option(FI,'AttribFiles');
if Props.File>0
    Attrib = qp_unwrapfi(Attribs(Props.File));
end
if Props.DimFlag(M_) && Props.DimFlag(N_)
    sz(M_)=size(FI.X,1);
    sz(N_)=size(FI.X,2);
else
    switch Attribs(Props.File).FileType
        case {'observation points'}
            sz(M_)=size(Attrib.MN,1);
        case {'discharge stations'}
            sz(M_)=size(Attrib.MNK,1);
        case {'openboundary'}
            sz(ST_)=sum(Attrib.BndType==Props.Fld);
        case {'cross-sections','barriers'}
            sz(ST_)=length(Attrib.Name);
    end
end
if Props.DimFlag(K_)
    if isfield(Attrib,'KMax')
        sz(K_)=Attrib.KMax;
    else
        sz(K_)=length(Props.Fld)/Props.NVal;
    end
end
if Props.DimFlag(T_)
    switch Attribs(Props.File).FileType
        case {'fls','FLS-inc'}
            if isfield(Attrib,'Times')
                sz(T_) = length(Attrib.Times);
            else
                sz(T_)=round(Attrib.End*60); % time step: one minute
            end
        case {'bagmap'}
            sz(T_)=length(Attrib.Time);
        otherwise
            sz(T_)=1;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
if Props.File>0
    Attribs = qp_option(FI,'AttribFiles');
    Attrib = qp_unwrapfi(Attribs(Props.File));
    switch Attribs(Props.File).FileType
        case {'fls','FLS-inc'}
            if isfield(Attrib,'Times')
                T = Attrib.Times/24;
                if ~isequal(t,0)
                    T = T(t);
                end
            else
                if isfield(Attrib,'StartTime')
                    t0=datenum(Attrib.StartTime(1),Attrib.StartTime(2),Attrib.StartTime(3),Attrib.StartTime(4),Attrib.StartTime(5),Attrib.StartTime(6));
                else
                    t0=1;
                end
                if isequal(t,0),
                    sz=getsize(FI,Props);
                    T=t0+transpose(1:sz(T_))/(24*60);
                else
                    T=t0+t(:)/(24*60);
                end
            end
        case {'bagmap'}
            if isequal(t,0),
                T=Attrib.Time;
            else
                T=Attrib.Time(t);
            end
            T=T(:);
        otherwise
            T=t(:);
    end
else
    T=t(:);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
if Props.File<=0
    S = {};
    return
end
Attribs = qp_option(FI,'AttribFiles');
Attrib = qp_unwrapfi(Attribs(Props.File));
switch Attrib.FileType
    case {'openboundary'}
        S = Attrib.Name(Attrib.BndType==Props.Fld);
    case {'cross-sections','barriers'}
        S = Attrib.Name;
end
if nargin>2
    S=S(t);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
FI=[];
cmd=lower(cmd);
cmdargs={};

Attribs = qp_option(NewFI,'AttribFiles');

switch cmd
    case 'initialize'
        OK=optfig(mfig);
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        if ~isempty(Attribs)
            Str={};
            for i=1:length(Attribs)
                Str{i}=abbrevfn(Attribs(i).Name,60);
            end
            set(Handle_SelectFile,'string',Str,'value',1,'enable','on','backgroundcolor',Active);
            Handle_CloseFile=findobj(mfig,'tag','closefile');
            set(Handle_CloseFile,'enable','on');
            NewFI=options(NewFI,mfig,'selectfile');
        end
        if isfield(NewFI,'Orient') && strcmp(NewFI.Orient,'clockwise')
            Handle_SaveCCW=findobj(mfig,'tag','saveccwgrid');
            set(Handle_SaveCCW,'enable','on')
        end
        
    case 'saveccwgrid'
        [f,p]=uiputfile(NewFI.FileName,'Save as ...');
        if ischar(f)
            CCWFI = NewFI;
            CCWFI.X = CCWFI.X(1:end-1,1:end-1).';
            CCWFI.Y = CCWFI.Y(1:end-1,1:end-1).';
            if isfield(CCWFI,'Enclosure')
               %what if the enclosure does not match the full grid?
               %would this work?
               %CCWFI.Enclosure = flipud(CCWFI.Enclosure(:,[2 1]));
               %or would this give problems with holes?
               %ignore this issue for the time being.
               CCWFI = rmfield(CCWFI,'Enclosure');
               opt = {'AutoEnclosure'};
            else
               opt = {};
            end
            wlgrid('write',CCWFI,'filename',[p f],opt{:})
        end
        
    case 'openfile'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        sel=get(Handle_SelectFile,'value');
        Str=get(Handle_SelectFile,'string');
        if nargin>3
            targetdir=varargin{1}; % FileName specified
            lcl_cmd = 'grid_open';
        elseif ~isempty(Attribs)
            targetdir=fileparts(Attribs(sel).Name);
            lcl_cmd = 'grid_opennew';
        else
            targetdir=fileparts(NewFI.FileName);
            lcl_cmd = 'grid_opennew';
        end
        %
        [Attrib,FileName] = qp_proxy(lcl_cmd,targetdir,size(NewFI.X));
        if isempty(Attrib)
            return
        end
        %
        if isempty(Attribs)
            Str={abbrevfn(FileName,60)};
            Attribs = Attrib;
            NrInList=1;
        else
            NrInList=find(strcmp(FileName,{Attribs.Name}));
            if isempty(NrInList)
                NrInList=length(Attribs)+1;
            end
            Str{NrInList}=abbrevfn(FileName,60);
            Attribs(NrInList) = Attrib;
        end
        set(Handle_SelectFile,'string',Str,'value',NrInList,'enable','on','backgroundcolor',Active);
        Handle_CloseFile=findobj(mfig,'tag','closefile');
        set(Handle_CloseFile,'enable','on');
        NewFI = qp_option(NewFI,'AttribFiles',Attribs);
        NewFI=options(NewFI,mfig,'selectfile');
        cmdargs={cmd FileName};
        
    case {'rstpc','rstunix','rstascii'}
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        NrInList=get(Handle_SelectFile,'value');
        Name  = Attribs(NrInList).Name;
        Attrib = qp_unwrapfi(Attribs(NrInList));
        Slash=sort([strfind(Name,'/') strfind(Name,'\')]);
        if strcmp(cmd,'rstascii')
            RstName = [Name(1:max(Slash)) '*.ini'];
        else
            Dot=strfind(Name,'.');
            Dot(Dot<max(Slash)) = [];
            if isempty(Dot)
                Dot=length(Name);
            end
            RstName = [Name(1:Dot(1)) '*'];
        end
        [f,p]=uiputfile(RstName,'Save as ...');
        if ischar(f)
            NLyr  = qp_option(Attrib,'NLyr');
            NSubs = qp_option(Attrib,'NSubs');
            NTurb = qp_option(Attrib,'NTurb');
            %
            h_nelyr = findobj(mfig,'tag','nelyr');
            NELyr = get(h_nelyr,'userdata');
            %
            % water level
            %
            data = Attrib.Data;
            zw = data{1};
            %
            % velocity components and constituents
            %
            offset = 1;
            c = cell(1,NSubs);
            index = ceil((1:NELyr)*NLyr/NELyr);
            for i = 1:NSubs+2
                c{i} = cat(3,data{offset+index});
                offset = offset+NLyr;
            end
            %
            % turbulent quantities
            %
            t = cell(1,NTurb);
            index = 1+round((0:NELyr)*NLyr/NELyr);
            for i = 1:NTurb
                t{i} = cat(3,data{offset+index});
                offset = offset+NLyr+1;
            end
            %
            % remaining 2D fields
            %
            if strcmp(cmd,'rstascii')
                if isempty(strfind(f,'.'))
                    f = [f '.ini'];
                end
                if ~isempty(t)
                    ui_message('warning','Turbulent quantities are not supported in ASCII restart files; they will be skipped.');
                end
                if ~isempty(data)
                    ui_message('warning','Additional 2D quantities are not supported in ASCII restart files; they will be skipped.');
                end
                wldep('write',[p f],zw,c{:});
            else
                trirst('write',[p f],cmd(4:end),zw,c{:},t{:},data{offset+1:end});
            end
        end
        
    case 'closefile'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        Str=get(Handle_SelectFile,'string');
        NrInList=get(Handle_SelectFile,'value');
        Attribs(NrInList)=[];
        Str(NrInList)=[];
        if isempty(Attribs)
            set(Handle_SelectFile,'enable','off','backgroundcolor',Inactive);
            Handle_CloseFile=findobj(mfig,'tag','closefile');
            set(Handle_CloseFile,'enable','off');
            Str=' ';
        else
            NrInList=min(NrInList,length(Attribs));
        end
        set(Handle_SelectFile,'string',Str,'value',NrInList);
        NewFI = qp_option(NewFI,'AttribFiles',Attribs);
        NewFI=options(NewFI,mfig,'selectfile');
        cmdargs={cmd};
        
    case 'selectfile'
        if isempty(mfig)
            return
        end
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        if nargin>3
            FileName=varargin{1};
            Files={Attribs(:).Name};
            NrInList=strmatch(FileName,Files,'exact');
            if ~isequal(size(NrInList),[1 1])
                NrInList=get(Handle_SelectFile,'value');
            else
                cmdargs={cmd FileName};
            end
        else
            NrInList=get(Handle_SelectFile,'value');
            if ~isempty(Attribs)
                FileName=Attribs(NrInList).Name;
                cmdargs={cmd FileName};
            end
        end
        if ~isempty(NrInList)
            set(Handle_SelectFile,'value',NrInList)
            if ~isempty(Attribs)
                set(Handle_SelectFile,'tooltip',Attribs(NrInList).Name)
            end
        end
        if ~isempty(Attribs) && strcmp(Attribs(NrInList).FileType,'trirst')
            Attrib = qp_unwrapfi(Attribs(NrInList));
            set(findobj(mfig,'tag','nelyr'),'enable','on','backgroundcolor',Active,'string','')
            set(findobj(mfig,'tag','nelyrtxt'),'enable','on')
            set(findobj(mfig,'tag','rstunix'),'enable','on')
            set(findobj(mfig,'tag','rstpc'),'enable','on')
            set(findobj(mfig,'tag','rstascii'),'enable','on')
            %
            set(findobj(mfig,'tag','nlyr'),'enable','on','backgroundcolor',Active,'string','')
            set(findobj(mfig,'tag','nlyrtxt'),'enable','on')
            NewFI=options(NewFI,mfig,'nlyr',NaN);
            set(findobj(mfig,'tag','nsubs'),'enable','on','backgroundcolor',Active,'string','')
            set(findobj(mfig,'tag','nsubstxt'),'enable','on')
            NewFI=options(NewFI,mfig,'nsubs',NaN);
            if Attrib.NLyr>1
                set(findobj(mfig,'tag','nturb'),'enable','on','backgroundcolor',Active,'string','')
                set(findobj(mfig,'tag','nturbtxt'),'enable','on')
                NewFI=options(NewFI,mfig,'nturb',NaN);
            else
                set(findobj(mfig,'tag','nturb'),'enable','off','backgroundcolor',Inactive,'string','')
                set(findobj(mfig,'tag','nturbtxt'),'enable','off')
                NewFI.Data(NrInList).Data.NTurb=0;
            end
        else
            set(findobj(mfig,'tag','rstunix'),'enable','off')
            set(findobj(mfig,'tag','rstpc'),'enable','off')
            set(findobj(mfig,'tag','rstascii'),'enable','off')
            if ~isempty(Attribs) && strcmp(Attribs(NrInList).FileType,'3dgate')
                set(findobj(mfig,'tag','nlyr'),'enable','on','backgroundcolor',Active,'string','')
                set(findobj(mfig,'tag','nlyrtxt'),'enable','on')
                NewFI=options(NewFI,mfig,'nlyr',NaN);
            else
                set(findobj(mfig,'tag','nlyr'),'enable','off','backgroundcolor',Inactive,'string','')
                set(findobj(mfig,'tag','nlyrtxt'),'enable','off')
            end
            set(findobj(mfig,'tag','nelyr'),'enable','off','backgroundcolor',Inactive,'string','')
            set(findobj(mfig,'tag','nelyrtxt'),'enable','off')
            set(findobj(mfig,'tag','nsubs'),'enable','off','backgroundcolor',Inactive,'string','')
            set(findobj(mfig,'tag','nsubstxt'),'enable','off')
            set(findobj(mfig,'tag','nturb'),'enable','off','backgroundcolor',Inactive,'string','')
            set(findobj(mfig,'tag','nturbtxt'),'enable','off')
            set(findobj(mfig,'tag','nfld'),'enable','off','backgroundcolor',Inactive,'string','')
            set(findobj(mfig,'tag','nfldtxt'),'enable','off')
        end
        %
        if isempty(Attribs) || (~strcmp(Attribs(NrInList).FileType,'wldep') && ...
                ~strcmp(Attribs(NrInList).FileType,'boxfile'))
            set(findobj(mfig,'tag','dlocation'),'enable','off','backgroundcolor',Inactive)
            set(findobj(mfig,'tag','dlocationtxt'),'enable','off')
            set(findobj(mfig,'tag','dpsopt'),'enable','off','backgroundcolor',Inactive)
            set(findobj(mfig,'tag','dpsopt'),'enable','off')
        else
            DLoc = qp_option(Attribs(NrInList),'DLocation');
            if isempty(DLoc)
                DLoc='grid points';
            end
            hDLoc=findobj(mfig,'tag','dlocation');
            supportedLocations = get(hDLoc,'string');
            iDLoc = ustrcmpi(DLoc,supportedLocations);
            if iDLoc>0
                DLoc = iDLoc;
            else
                % maybe an old location name was used ...
                iDLoc = ustrcmpi(DLoc,{'bed level','water level'});
                if iDLoc>0
                    DLoc = iDLoc;
                else
                    DLoc = 1; % default 'grid points'
                end
            end
            set(hDLoc,'enable','on','backgroundcolor',Active,'value',DLoc)
            set(findobj(mfig,'tag','dlocationtxt'),'enable','on')
            %
            if isequal(DLoc,2) % 'water level'
                set(findobj(mfig,'tag','dpsopt'),'enable','off','backgroundcolor',Inactive)
                set(findobj(mfig,'tag','dpsopttxt'),'enable','off')
            else
                Dpsopt = qp_option(Attribs(NrInList),'Dpsopt');
                if isempty(Dpsopt)
                    Dpsopt='max';
                end
                hDpsopt=findobj(mfig,'tag','dpsopt');
                Dpsopt=strmatch(Dpsopt,get(hDpsopt,'string'));
                set(hDpsopt,'enable','on','backgroundcolor',Active,'value',Dpsopt)
                set(findobj(mfig,'tag','dpsopttxt'),'enable','on')
            end
        end
        %
        if isempty(Attribs) || ~strcmp(Attribs(NrInList).FileType,'wldep')
            set(findobj(mfig,'tag','dataorder'),'enable','off','backgroundcolor',Inactive)
            set(findobj(mfig,'tag','dataordertxt'),'enable','off')
        else
            hDOrd=findobj(mfig,'tag','dataorder');
            DOrd = qp_option(Attribs(NrInList),'DOrder');
            if isempty(DOrd)
                DOrd=2; % (d(m,n) m=1:M) n=1:N
            end
            set(hDOrd,'enable','on','backgroundcolor',Active,'value',DOrd)
            set(findobj(mfig,'tag','dataordertxt'),'enable','on')
        end
        
    case 'nelyr'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        NrInList=get(Handle_SelectFile,'value');
        %
        h_nelyr = findobj(mfig,'tag',cmd);
        if nargin>3
            N = varargin{1};
        else
            N = get(h_nelyr,'string');
        end
        if ischar(N)
            N = str2num(N);
        end
        if isempty(N)
            N = get(h_nelyr,'userdata');
        else
            N = floor(N(1));
        end
        NLyr = Attrib.NLyr;
        N = max(1,floor(N/NLyr))*NLyr;
        set(h_nelyr,'userdata',N,'string',sprintf('%i',N))
        %
        cmdargs={cmd N};
        
    case {'nlyr','nsubs','nturb'}
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        NrInList=get(Handle_SelectFile,'value');
        %
        Field=cmd;
        Field(1:2)=upper(Field(1:2));
        %
        Attrib = qp_unwrapfi(Attribs(NrInList));
        switch Attribs(NrInList).FileType
            case 'trirst'
                switch cmd
                    case 'nlyr'
                        N_LowerLim=1;
                        N_0=0;
                        N_UpperLim=floor((length(Attrib.Data)-1)/2);
                    case 'nsubs'
                        N_LowerLim=0;
                        N_0=0;
                        NLyr=qp_option(Attribs(NrInList),'NLyr');
                        N_UpperLim=floor((length(Attrib.Data)-NLyr*2-1)/NLyr);
                    case 'nturb'
                        N_LowerLim=0;
                        N_0=0;
                        NLyr=qp_option(Attribs(NrInList),'NLyr');
                        NSubs=qp_option(Attribs(NrInList),'NSubs');
                        N_UpperLim=floor((length(Attrib.Data)-NLyr*(2+NSubs)-1)/(NLyr+1));
                    otherwise
                        N_LowerLim=0;
                        N_0=0;
                        N_UpperLim=0;
                end
            case '3dgate'
                N_0=max(max(max(Attrib.MNKu(:,5:6))), ...
                    max(max(Attrib.MNKv(:,5:6))));
                N_LowerLim=N_0;
                N_UpperLim=1e6;
        end
        N_1 = qp_option(Attribs(NrInList),Field);
        if ~isempty(N_1)
            N_0=N_1;
        end
        Handle_N_=findobj(mfig,'tag',cmd);
        if nargin>3
            N_=varargin{1};
        else
            N_=get(Handle_N_,'string');
        end
        if ischar(N_)
            N_=str2num(N_);
        end
        if isempty(N_)
            N_=N_0;
        else
            N_=N_(1);
            if N_~=round(N_) || N_<N_LowerLim || N_>N_UpperLim
                N_=N_0;
            end
            if N_>N_UpperLim
                N_=N_UpperLim;
            elseif N_<N_LowerLim
                N_=N_LowerLim;
            end
        end
        set(Handle_N_,'string',sprintf('%i',N_))
        Attribs(NrInList) = qp_option(Attribs(NrInList),Field,N_);
        %
        switch cmd
            case 'nlyr'
                Attribs(NrInList) = qp_option(Attribs(NrInList),'NSubs',-1);
                Attribs(NrInList) = qp_option(Attribs(NrInList),'NTurb',-1);
                [k,NSubs,NTurb,NRem]=DetectFld(Attribs(NrInList));
                Attribs(NrInList) = qp_option(Attribs(NrInList),'NSubs',NSubs);
                Attribs(NrInList) = qp_option(Attribs(NrInList),'NTurb',NTurb);
                %
                set(findobj(mfig,'tag','nelyr'),'enable','on','backgroundcolor',Active,'string',sprintf('%i',k),'userdata',k)
                set(findobj(mfig,'tag','nsubs'),'enable','on','backgroundcolor',Active,'string',sprintf('%i',NSubs))
                set(findobj(mfig,'tag','nsubstxt'),'enable','on')
                if qp_option(Attribs(NrInList),'NLyr')>1
                    set(findobj(mfig,'tag','nturb'),'enable','on','backgroundcolor',Active,'string',sprintf('%i',NTurb))
                    set(findobj(mfig,'tag','nturbtxt'),'enable','on')
                else
                    set(findobj(mfig,'tag','nturb'),'enable','off','backgroundcolor',Inactive,'string','')
                    set(findobj(mfig,'tag','nturbtxt'),'enable','off')
                end
                set(findobj(mfig,'tag','nfld'),'enable','inactive','backgroundcolor',Inactive,'string',sprintf('%i',NRem))
                set(findobj(mfig,'tag','nfldtxt'),'enable','on')
            case 'nsubs'
                Attribs(NrInList) = qp_option(Attribs(NrInList),'NTurb',-1);
                [k,NSubs,NTurb,NRem]=DetectFld(Attribs(NrInList));
                Attribs(NrInList) = qp_option(Attribs(NrInList),'NTurb',NTurb);
                if qp_option(Attribs(NrInList),'NLyr')>1
                    set(findobj(mfig,'tag','nturb'),'enable','on','backgroundcolor',Active,'string',sprintf('%i',NTurb))
                    set(findobj(mfig,'tag','nturbtxt'),'enable','on')
                end
                set(findobj(mfig,'tag','nfld'),'enable','inactive','backgroundcolor',Inactive,'string',sprintf('%i',NRem))
                set(findobj(mfig,'tag','nfldtxt'),'enable','on')
            case 'nturb'
                [k,NSubs,NTurb,NRem]=DetectFld(Attribs(NrInList));
                set(findobj(mfig,'tag','nfld'),'enable','inactive','backgroundcolor',Inactive,'string',sprintf('%i',NRem))
                set(findobj(mfig,'tag','nfldtxt'),'enable','on')
        end
        %
        NewFI = qp_option(NewFI,'AttribFiles',Attribs);
        cmdargs={cmd N_};
        
    case {'dlocation','dpsopt'}
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        NrInList=get(Handle_SelectFile,'value');
        Lbl='DLocation';
        Default='grid points';
        if strcmp(cmd,'dpsopt')
            Lbl='Dpsopt';
            Default='max';
        end
        Val0 = qp_option(Attribs(NrInList),Lbl);
        if isempty(Val0)
            Val0=Default;
        end
        Handle_Val=findobj(mfig,'tag',cmd);
        ValStr=get(Handle_Val,'string');
        if nargin>3
            Val=varargin{1};
            ValNr=strmatch(Val,ValStr);
        else
            ValNr=get(Handle_Val,'value');
        end
        if isequal(size(ValNr),[1 1])
            Val=ValStr{ValNr};
        else
            Val=Val0;
            ValNr=strmatch(Val0,ValStr);
        end
        set(Handle_Val,'value',ValNr)
        Attribs(NrInList) = qp_option(Attribs(NrInList),Lbl,Val);
        NewFI = qp_option(NewFI,'AttribFiles',Attribs);
        NewFI=options(NewFI,mfig,'selectfile');
        cmdargs={cmd Val};
        
    case 'dataorder'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        NrInList=get(Handle_SelectFile,'value');
        Handle_DOrd=findobj(mfig,'tag','dataorder');
        DOrdStr=get(Handle_DOrd,'string');
        DOrd0 = qp_option(Attribs(NrInList),'DOrder');
        if isempty(DOrd0)
            DOrd0=2; % (d(m,n) m=1:M) n=1:N
        end
        if nargin>3
            DOrd=varargin{1};
            if ischar(DOrd)
                DOrdNr=str2num(DOrd); % check for '2'
                if isempty(DOrdNr) % should be something like '(d(m,n) ...'
                    DOrdNr=strmatch(DOrd,DOrdStr);
                end
            else
                DOrdNr=DOrd;
            end
        else
            DOrdNr=get(Handle_DOrd,'value');
        end
        if isequal(size(DOrdNr),[1 1])
            DOrd=DOrdNr;
        else
            DOrd=DOrd0;
        end
        set(Handle_DOrd,'value',DOrdNr)
        Attribs(NrInList) = qp_option(Attribs(NrInList),'DOrder',DOrd);
        NewFI = qp_option(NewFI,'AttribFiles',Attribs);
        cmdargs={cmd DOrdStr{DOrdNr}};
        
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
%
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions openfile', ...
    'Position',[11 voffset 160 20], ...
    'String','Add Dataset', ...
    'Tag','openfile')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions closefile', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'String','Remove Dataset', ...
    'Tag','closefile')
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selectfile', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',' ', ...
    'Style','popupmenu', ...
    'Tag','selectfile', ...
    'Value',1)
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 160 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','Data Location', ...
    'Tag','dlocationtxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions dlocation', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'Style','popupmenu', ...
    'horizontalalignment','right', ...
    'String',{'grid points','cell centres'}, ...
    'Tag','dlocation')
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 160 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','Data Order', ...
    'Tag','dataordertxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions dataorder', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'Style','popupmenu', ...
    'horizontalalignment','right', ...
    'String',{'(d(m,n) n=1:N) m=1:M', ...
    '(d(m,n) m=1:M) n=1:N', ...
    '(d(m,n) n=1:N) m=M:-1:1', ...
    '(d(m,n) m=M:-1:1) n=1:N', ...
    '(d(m,n) n=N:-1:1) m=1:M', ...
    '(d(m,n) m=1:M) n=N:-1:1', ...
    '(d(m,n) n=N:-1:1) m=M:-1:1', ...
    '(d(m,n) m=M:-1:1) n=N:-1:1'}, ...
    'Tag','dataorder')
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 160 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','Dpsopt', ...
    'Tag','dpsopttxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions dpsopt', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'Style','popupmenu', ...
    'horizontalalignment','right', ...
    'String',{'mean','max','min'}, ...
    'Tag','dpsopt')
%
%------------------------------
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 120 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','Number of Layers', ...
    'Tag','nlyrtxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions nlyr', ...
    'Enable','off', ...
    'Position',[121 voffset 50 20], ...
    'Style','edit', ...
    'horizontalalignment','right', ...
    'String','', ...
    'Tag','nlyr')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[181 voffset 120 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','# Export Layers', ...
    'Tag','nelyrtxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions nelyr', ...
    'Enable','off', ...
    'Position',[281 voffset 50 20], ...
    'Style','edit', ...
    'horizontalalignment','right', ...
    'String','', ...
    'Tag','nelyr')
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 120 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','# Substances', ...
    'Tag','nsubstxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions nsubs', ...
    'Enable','off', ...
    'Position',[121 voffset 50 20], ...
    'Style','edit', ...
    'horizontalalignment','right', ...
    'String','', ...
    'Tag','nsubs')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions rstpc', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'String','Save PC Format', ...
    'Tag','rstpc')
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 120 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','# Turb. Quant', ...
    'Tag','nturbtxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions nturb', ...
    'Enable','off', ...
    'Position',[121 voffset 50 20], ...
    'Style','edit', ...
    'horizontalalignment','right', ...
    'String','', ...
    'Tag','nturb')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions rstunix', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'String','Save UNIX Format', ...
    'Tag','rstunix')
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 120 18], ...
    'Style','text', ...
    'horizontalalignment','left', ...
    'String','# 2D Fields', ...
    'Tag','nfldtxt')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','', ...
    'Enable','off', ...
    'Position',[121 voffset 50 20], ...
    'Style','edit', ...
    'horizontalalignment','right', ...
    'String','', ...
    'Tag','nfld')
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions rstascii', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'String','Save ASCII Format', ...
    'Tag','rstascii')
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions saveccwgrid', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'String','Save CCW Grid', ...
    'Tag','saveccwgrid')
%
OK=1;
% -----------------------------------------------------------------------------


function [k,NSubs,NTurb,NRem]=DetectFld(Rst)
ActualRst = qp_unwrapfi(Rst);
if ~isfield(ActualRst,'Data')
    k = 0;
    NSubs = 0;
    NTurb = 0;
    NRem = 0;
    return
end
N=length(ActualRst.Data);
%
NLyr = qp_option(Rst,'NLyr');
if ~isempty(NLyr)
    maxk=floor((N-1)/2);
    k=max(min(NLyr,maxk),1);
else
    k=1;
end
%
maxsubs=floor((N-1-k*2)/k);
NSubs = qp_option(Rst,'NSubs');
if ~isempty(NSubs)
    NSubs=min(NSubs,maxsubs);
else
    NSubs=-1;
end
%
if NSubs<0 % Number of substances not specified
    if k==1
        NTurb=0;
        NSubs=maxsubs;
        NRem=0;
    else
        maxturb=floor((N-1-k*2)/(k+1));
        NTurb=min(2,maxturb);
        NSubs=floor((N-1-k*2-(k+1)*NTurb)/k);
        NRem=N-1-k*(2+NSubs)-(k+1)*NTurb;
        if NRem==1 && NTurb>0
            NTurb=NTurb-1;
            NSubs=NSubs+1;
            NRem=NRem+1;
        end
    end
else % Number of substances specified
    if k==1
        NTurb=0;
    else
        maxturb=floor((N-1-k*(2+NSubs))/(k+1));
        NTurb = qp_option(Rst,'NTurb');
        if ~isempty(NTurb)
            NTurb=min(NTurb,maxturb);
        else
            NTurb=-1;
        end
    end
    %
    if NTurb<0 % Number of turbulence quantities not specified
        NTurb = min(2,maxturb);
        NRem = N-1-k*(2+NSubs)-(k+1)*NTurb;
    else % Number of turbulence quantities specified
        NRem = N-1-k*(2+NSubs)-(k+1)*NTurb;
    end
end
% -----------------------------------------------------------------------------
