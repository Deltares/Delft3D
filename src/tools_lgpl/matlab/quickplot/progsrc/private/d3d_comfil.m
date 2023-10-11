function varargout=d3d_comfil(FI,domain,field,cmd,varargin)
%D3D_COMFIL QP support for Delft3D communication files.
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
end

OrigFI = FI;
nPartitions = 1; % single partition
mergeParts = false;
mergeDim = -1;
if isfield(FI,'NEFIS')
    nPartitions = length(FI.NEFIS); 
    if domain <= nPartitions
        FI = FI.NEFIS(domain);
        nPartitions = 1;
    else
        mergeParts = (domain == nPartitions+2); % merged partitions
        switch FI.Merge.Dimension
            case 'M'
                mergeDim = M_;
            case 'N'
                mergeDim = N_;
        end
        FI = FI.NEFIS(1);
    end
    if isfield(OrigFI,'QP_Options')
        FI.QP_Options = OrigFI.QP_Options;
    end
end

if nargin==2
    varargout={infile(FI,domain)};
    if nPartitions > 1 && ~mergeParts
        Out = varargout{1};
        for i = 1:length(Out)
            if Out(i).DimFlag(mergeDim)
                Out(i).DimFlag(mergeDim) = inf;
            end
        end
        varargout{1} = Out;
    end
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(OrigFI,cmd,varargin{:});
        case 'domains'
            varargout={domains(OrigFI)};
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
        if mergeParts && Props.DimFlag(mergeDim)
            varargout{1}(mergeDim) = OrigFI.Merge.Length;
        end
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
%
if strcmp(Props.Name,'grid')
    DataInCell = 1;
end

hasSubfields = ~isempty(getsubfields(OrigFI,Props));
[Ans,OrigFI] = merge_trim_com(OrigFI,domain,Props,XYRead,DataRead,DataInCell,@get_single_partition,nPartitions,mergeParts,mergeDim,hasSubfields,varargin{:});

varargout{2} = OrigFI;
varargout{1} = Ans;

function Domains = domains(FI)
if isfield(FI,'Partitions')
    Domains = multiline(sprintf('partition %3.3d-',1:FI.Partitions{1}+1),'-','cell');
    Domains{end-1} = 'all partitions';
    Domains{end} = 'merged partitions';
else
    Domains = {};
end

function [Ans,FI] = get_single_partition(FI,domain,Props,XYRead,DataRead,DataInCell,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;

DimFlag=Props.DimFlag;
idx={[] [] 0 0 0};
fidx=find(DimFlag);

subf=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    Props.SubFld=cat(2,varargin{1},Props.SubFld);
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
        if isempty(idx{i}) || isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
        if i~=K_
            if length(idx{i})==1 && idx{i}(1)==1
                idx{i} = [1 2];
                ind{i} = 1;
            elseif idx{i}(1)>1
                idx{i} = [idx{i}(1)-1 idx{i}];
                ind{i} = 2:length(idx{i});
            else
                if DataInCell
                    ind{i} = 2:length(idx{i});
                else
                    ind{i} = 1:length(idx{i});
                end
            end
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
z=[];
Info=vs_disp(FI,'GRID','ZK');
fixedlayers=0;
if isstruct(Info)
    fixedlayers=Info.SizeDim>1;
end
%
computeDZ=0;
switch Props.Name
    case {'depth averaged velocity','d.a. velocity fluctuations','velocity in depth averaged flow direction','velocity normal to depth averaged flow direction','head','froude number'}
        if fixedlayers
            computeDZ=1;
        end
end
if XYRead

    %======================== SPECIFIC CODE =======================================
    if DimFlag(M_) & DimFlag(N_)
        [x,Chk]=vs_get(FI,'GRID','XCOR',idx([M_ N_]),'quiet');
        [y,Chk]=vs_get(FI,'GRID','YCOR',idx([M_ N_]),'quiet');
        x((x==0) & (y==0)) = NaN;
        %
        missingvalue = clip2single(999.999);
        x((x==missingvalue) & (y==missingvalue)) = NaN;
        y(isnan(x))=NaN;
    end

    if DimFlag(K_) | computeDZ
        Info=vs_disp(FI,'BOTTIM',[]);
        if isstruct(Info)
            [dp,Chk]=vs_let(FI,'BOTTIM',{Info.SizeDim},'DP',idx([M_ N_]),'quiet');
            dp(dp==-999)=NaN;
        else
            [dp,Chk]=vs_let(FI,'INITBOT',{1},'DP0',idx([M_ N_]),'quiet');
        end
        dp=-interpdp(FI,dp);
        %
        if DataInCell
            idxK_=[idx{K_} idx{K_}(end)+1];
        else
            idxK_=idx{K_};
        end
        %
        if fixedlayers
            %
            % Z-MODEL
            %
            [s,Chk]=vs_let(FI,'CURTIM',{idx{T_}},'S1',idx([M_ N_]),'quiet');
            [h,Chk]=vs_let(FI,'GRID','ZK','quiet');
            h(1)=-inf;
            h(end)=inf;
            %
            szz=[size(s) length(h)];
            z=repmat(reshape(h,[1 1 1 length(h)]),[szz(1:3),1]);
            z=reshape(z,[prod(szz(1:3)) szz(4)]);
            s=s(:);
            for i=1:szz(4)
                fl=z(:,i)>s;
                z(fl,i)=s(fl);
            end
            %
            if size(dp,1)==1
                z=reshape(z,[szz(1) prod(szz(2:3)) szz(4)]);
                dp=reshape(dp,[1 prod(szz(2:3))]);
                for t=1:szz(1)
                    for i=1:szz(4)
                        fl=z(t,:,i)<dp;
                        if any(fl)
                            z(t,fl,i)=dp(1,fl);
                        end
                    end
                end
            else
                dp=dp(:);
                for i=1:szz(4)
                    fl=z(:,i)<dp;
                    if any(fl)
                        z(fl,i)=dp(fl);
                    end
                end
            end
            z=reshape(z,szz);
            if computeDZ
                dz=diff(z,1,4);
            end
            if DimFlag(K_)
                if strcmp(Props.Loc3D,'i')
                    if DataInCell
                        z=cat(4,z(:,:,:,1),(z(:,:,:,1:end-1)+z(:,:,:,2:end))/2,z(:,:,:,end));
                    end
                else % 'c'
                    if ~DataInCell
                        z=(z(:,:,:,1:end-1)+z(:,:,:,2:end))/2;
                    end
                end
                z=z(:,:,:,idxK_);
            end
        else
            %
            % SIGMA-MODEL
            %
            [h,Chk]=vs_let(FI,'CURTIM',{idx{T_}},'S1',idx([M_ N_]),'quiet');
            if size(dp,1)==1
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
            cthk=cthk(idxK_);
            szh=size(h);
            if length(szh)<3
                szh(3)=1;
            end
            z=zeros([szh length(cthk)]);
            if size(dp,1)==1
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
        end
        if DimFlag(K_)
            x=reshape(x,[1 size(x)]);
            x=repmat(x,[1 1 1 length(idxK_)]);
            y=reshape(y,[1 size(y)]);
            y=repmat(y,[1 1 1 length(idxK_)]);
        end
    end
    %========================= GENERAL CODE =======================================
end

% grid interpolation ...
[x,y]=gridinterp(DataInCell,DimFlag(K_),Props.ReqLoc,x,y);

% load data ...
if DataRead
    %================== NEFIS SPECIFIC CODE =======================================
    elidx=idx(2:end);
    ThinDam=0;
    if Props.NVal==1.9
        Props.NVal=2;
        ThinDam=2;
    end
    switch Props.Name
        case {'depth averaged velocity','depth averaged discharge','depth averaged unit discharge','discharge potential'}
            Info=vs_disp(FI,Props.Group,Props.Val1);
            Flag3D=isequal(size(Info.SizeDim),[1 3]);
            if Flag3D
                elidx{end+1}=0;
            end
        case {'velocity in depth averaged flow direction','velocity normal to depth averaged flow direction'}
            % always load 3D field to determine depth averaged flow direction
            elidx{K_-1}=0;
        case {'thin dams','temporarily inactive velocity points'}
            Props.NVal=2;
            ThinDam=1;
    end

    elidx(~DimFlag(2:end))=[];
    if ~isempty(Props.SubFld) % e.g. sed.fraction
       % last dimensions automatically dropped after reading
       if iscell(Props.SubFld)
          elidx=cat(2,elidx,Props.SubFld);
       else
          for i=1:length(Props.SubFld)
             elidx(end+1)={Props.SubFld(i)};
           end
        end
    end

    if Props.NVal==0
        val1=[];
    elseif strcmp(Props.Name,'spiral flow intensity (effective)')
        if length(idx{T_})~=1
            error('%s cannot be determined for one timestep only.',Props.Name);
        end
        val1=spirint([],FI,idx{T_});
        val1=reshape(val1,[1 size(val1)]);
        val1=val1(1,elidx{1},elidx{2});
    else
        [val1,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,elidx,'quiet');
        if ~Chk
            error(val1)
        end
    end
    if isempty(Props.Val2)
        val2=[];
    else
        [val2,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val2,elidx,'quiet');
        if ~Chk
            error(val2)
        end
    end

    if fixedlayers && ~ThinDam
        val1(val1==0)=-999;
        val2(val2==0)=-999;
    end

    switch Props.Name
        case {'initial bed level','fixed layer','time-varying bed level'}
            if DataInCell
                val1(val1==-999)=NaN;
                val1=interpdp(FI,val1);
                Props.Loc='z';
            end
            val1=-val1;
        case {'sed. layer above fixed bed'}
            if isstruct(vs_disp('INITBOT','DPF'))
                dpf='DPF';
            else
                dpf='DPFIX';
            end
            [refdp,Chk]=vs_let(FI,'INITBOT',dpf,elidx,'quiet');
            if DataInCell
                val1(val1==-999)=NaN;
                val1=interpdp(FI,val1);
                refdp(refdp==-999)=NaN;
                refdp=interpdp(FI,refdp);
                Props.Loc='z';
            end
            for t=1:size(val1,1),
                val1(t,:)=refdp(1,:)-val1(t,:); % refdp more positive (deeper) than val1 -> positive thickness
            end;
        case {'cum. erosion/sedimentation'}
            [refdp,Chk]=vs_let(FI,'BOTTIM',{1},'DP',elidx,'quiet');
            if DataInCell
                val1(val1==-999)=NaN;
                val1=interpdp(FI,val1);
                refdp(refdp==-999)=NaN;
                refdp=interpdp(FI,refdp);
                Props.Loc='z';
            end
            for t=1:size(val1,1),
                val1(t,:)=refdp(1,:)-val1(t,:); % erosion negative, deposition positive
            end;
        case 'depth averaged velocity'
            if Flag3D
                if fixedlayers
                    sz=size(val1);
                    val1(val1==-999)=0;
                    val2(val2==-999)=0;
                    val1=val1.*dz;
                    val2=val2.*dz;
                    h=sum(dz,4);
                    val1(h==0)=0; val2(h==0)=0; h(h==0)=1;
                    val1=sum(val1,4); val1=val1./h;
                    val2=sum(val2,4); val2=val2./h;
                else
                    [thk,Chk]=vs_let(FI,'GRID','THICK','quiet');
                    for k=1:length(thk)
                        val1(:,:,:,k)=val1(:,:,:,k)*thk(k);
                        val2(:,:,:,k)=val2(:,:,:,k)*thk(k);
                    end
                    val1=sum(val1,4);
                    val2=sum(val2,4);
                end
                elidx(end)=[]; % don't read K_ in case of U/VMNLDF
            end
        case {'depth averaged discharge','depth averaged unit discharge','discharge potential'}
            if Flag3D
                if fixedlayers
                    val1(val1==-999)=0;
                    val2(val2==-999)=0;
                end
                val1=sum(val1,4);
                val2=sum(val2,4);
            end
            switch Props.Name
                case 'discharge potential'
                    %val1=cumsum(val1,2)-repmat(cumsum(val2(:,1,:),3),[1,size(val1,2),1]);
                    %val1=cumsum(val2,3)-repmat(cumsum(val1(:,:,1),2),[1,1,size(val2,3)]);
                    szv=size(val1);
                    side_inflow = val2(sub2ind(szv,...
                        repmat((1:szv(1))',[1 1 szv(3)]),...
                        min(sum(cumprod(double(val2==0),2),2)+1,szv(2)),...
                        repmat(reshape(1:szv(3),[1 1 szv(3)]),[szv(1) 1 1])...
                        ));
                    val1=cumsum(val1,2)-repmat(cumsum(side_inflow,3),[1,size(val1,2),1]);
                    val2=[];
                case 'depth averaged unit discharge'
                    [GUU,Chk]=vs_let(FI,'GRID','GUU',elidx(1:2),'quiet'); GUU(GUU==0)=1;
                    [GVV,Chk]=vs_let(FI,'GRID','GVV',elidx(1:2),'quiet'); GVV(GVV==0)=1;
                    for t=1:size(val1,1)
                        val1(t,:,:)=val1(t,:,:)./GUU;
                        val2(t,:,:)=val2(t,:,:)./GVV;
                    end
            end
        case {'unit discharge (horizontal)'}
            [GUU,Chk]=vs_let(FI,'GRID','GUU',elidx(1:2),'quiet'); GUU(GUU==0)=1;
            [GVV,Chk]=vs_let(FI,'GRID','GVV',elidx(1:2),'quiet'); GVV(GVV==0)=1;
            for t=1:size(val1,1)
                for k=1:size(val1,4)
                    val1(t,:,:,k)=val1(t,:,:,k)./GUU;
                    val2(t,:,:,k)=val2(t,:,:,k)./GVV;
                end
            end
    end

    if fixedlayers
        val1(val1==-999)=NaN;
        val2(val2==-999)=NaN;
    end

    % rotate direction as appropriate ...
    if isequal(Props.VecType,'m')
        [alf,Chk] = vs_let(FI,'GRID','ALFAS',idx([M_ N_]),'quiet');
        val2=val2+repmat(alf,[size(val2,1),1,1]);
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
        if fixedlayers
            val1(val1==0)=NaN;
            val2(val2==0)=NaN;
        end
        if 0 && isstruct(vs_disp(FI,'KENMTIM','KFU'))
            kfu=vs_let(FI,Props.Group,idx(T_),'KFU',elidx,'quiet!');
            kfv=vs_let(FI,Props.Group,idx(T_),'KFV',elidx,'quiet!');
            szv1 = size(val1);
            szv1(end+1) = 1;
            val1 = reshape(val1,[prod(szv1(1:3)) szv1(4:end)]);
            val2 = reshape(val2,[prod(szv1(1:3)) szv1(4:end)]);
            val1(kfu==0,:) = NaN;
            val2(kfv==0,:) = NaN;
            val1 = reshape(val1,szv1);
            val2 = reshape(val2,szv1);
        end
        [val1,val2]=uv2cen(val1,val2);
    end
    % combine vectors components ...
    if isequal(Props.VecType,'u') & Props.MNK<=1
        % rotate n,m components into x,y direction ...
        [alf,Chk] = vs_get(FI,'GRID','ALFAS',idx([M_ N_]),'quiet');
        if Chk
            alf = alf*pi/180;
            [val1,val2]=cur2ca(val1,val2,alf);
            %else % POM ?
            %t=val2; val2=val1; val1=-t; t=[];
        end
    end

    switch Props.Name
        case {'velocity in depth averaged flow direction','velocity normal to depth averaged flow direction'}
            if fixedlayers
                sz=size(val1);
                dav1=zeros(sz(1:3));
                dav2=dav1;
                %division by water depth h is not necessary to determine direction ...
                %h=dav1;
                for k=1:size(val1,4)
                    dav1=dav1+val1(:,:,:,k).*dz(:,:,:,k);
                    dav2=dav2+val2(:,:,:,k).*dz(:,:,:,k);
                    %h=h+dz(:,:,:,k);
                end
                %dav1=dav1./h; dav2=dav2./h;
            else
                [thk,Chk]=vs_let(FI,'GRID','THICK','quiet');
                sz=size(val1);
                dav1=zeros(sz(1:3));
                dav2=dav1;
                for k=1:length(thk)
                    dav1=dav1+val1(:,:,:,k)*thk(k);
                    dav2=dav2+val2(:,:,:,k)*thk(k);
                end
            end
            dvl=sqrt(dav1.^2+dav2.^2); dvl(dvl==0)=1;
            dav1=dav1./dvl; dav2=dav2./dvl;
            switch Props.Name
                case {'velocity in depth averaged flow direction'}
                    for k=1:size(val1,4)
                        val1(:,:,:,k)=val1(:,:,:,k).*dav1+val2(:,:,:,k).*dav2;
                    end
                case {'velocity normal to depth averaged flow direction'}
                    for k=1:size(val1,4)
                        val1(:,:,:,k)=val1(:,:,:,k).*dav2-val2(:,:,:,k).*dav1;
                    end
            end
            val2=[];
            val1=val1(:,:,:,idx{K_});
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
        [act,Chk]=vs_get(FI,'KENMCNST','KCS',idx([M_ N_]),'quiet');
        act(act>1e9)=1;
end
if ~Chk
    if XYRead
        act=ones(size(x,1),size(x,2));
    else
        act=ones(size(val1,2),size(val1,3));
    end
end
if DataInCell
    [gridact,Chk]=vs_get(FI,'TEMPOUT','CODB',idx([M_ N_]),'quiet');
    if ~Chk
        gridact=ones(size(val1,2),size(val1,3));
    end
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
if Props.NVal>0
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
    if Props.NVal==1
        val1=val1(:,ind{:});
    elseif Props.NVal==2
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
    if Props.NVal>0
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
    Ans.XUnits='m';
    Ans.YUnits='m';
    Info1=vs_disp(FI,'GRID','XCOR');
    Info2=vs_disp(FI,'INITBOT','ALTITUDE');
    if isstruct(Info1) & isequal(Info1.ElmUnits,'[  DEG  ]')
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    elseif  isstruct(Info2)
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    end
    if DimFlag(K_)
        Ans.Z=z;
        Ans.ZUnits='m';
    end
end
if Props.NVal==0
elseif ThinDam==2
    Ans.XDam=~isnan(val1);
    Ans.YDam=~isnan(val2);
    Ans.XDamVal=val1;
    Ans.YDamVal=val2;
elseif ThinDam
    val1(isnan(val1))=1;
    val2(isnan(val2))=1;
    Ans.XDam=~val1;
    Ans.YDam=~val2;
elseif isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end

% read time ...
[T,Tsc]=readtim(FI,Props,idx{T_});
Ans.Time=T;
Ans.XInfo.Tscale=Tsc;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'    'Val2'  'SubFld' 'MNK'};
DataProps={'morphologic grid'          ''       [0 0 1 1 0]  0         0     ''       'd'   'd'       ''      'GRID'           'XCOR'    ''       []       0
    'hydrodynamic grid'         ''       [1 0 1 1 1]  0         0     ''       'z'   'z'       'i'     'CURTIM'         'S1'      ''       []       0
    'grid'                      ''       [1 0 1 1 1]  0         0     ''       'z'   'z'       ''      'CURTIM'         'S1'      ''       []       0
    'inactive water level points' ...
    ''       [1 0 1 1 0]  2         1     ''       'z'   'z'       ''      'KENMCNST'       'KCS'     ''       []       0
    'thin dams'                 ''       [1 0 1 1 0]  0         0     ''       'd'   'd'       ''      'KENMCNST'       'KCU'     'KCV'    []       0
    'temporarily inactive velocity points' ...
    ''       [1 0 1 1 0]  0         0     ''       'd'   'd'       ''      'KENMTIM'        'KFU'     'KFV'    []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'wind velocity'             'm/s'    [1 0 1 1 0]  1         2     'x'      'z'   'z'       ''      'WIND'           'WINDU'   'WINDV'  []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'hrms wave height'          'm'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'WAVTIM'         'HRMS'    ''       []       0
%   'wave direction'            'deg'    [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'WAVTIM'         'DIR'     ''       []       0
    'hrms wave vector'          'm'      [1 0 1 1 0]  1         2     'm'      'z'   'z'       ''      'WAVTIM'         'HRMS'    'DIR'    []       0
    'tp wave period'            's'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'WAVTIM'         'TP'      ''       []       0
    'smoothed peak wave period' 's'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'WAVTIM'         'TPS'     ''       []       0
    'mean wave length'          'm'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'WAVTIM'         'WLEN'    ''       []       0
    'orbital velocity near bed' 'm/s'    [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'WAVTIM'         'UBOT'    ''       []       0
    'wave dissipation'          'W/m^2'  [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'WAVTIM'         'DISS'    ''       []       0
    'wave force'                'N/m^2'  [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'WAVTIM'         'FX'      'FY'     []       1
    'mass flux'                 'm^3/sm' [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'WAVTIM'         'MX'      'MY'     []       1
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'water level'               'm'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'CURTIM'         'S1'      ''       []       0
    'velocity (horizontal)'     'm/s'    [1 0 1 1 1]  1         2     'u'      'u'   'z'       'c'     'CURTIM'         'U1'      'V1'     []       1
    'velocity in depth averaged flow direction' ...
    'm/s'    [1 0 1 1 1]  1         1     'u'      'u'   'z'       'c'     'CURTIM'         'U1'      'V1'     []       0
    'velocity normal to depth averaged flow direction' ...
    'm/s'    [1 0 1 1 1]  1         1     'u'      'u'   'z'       'c'     'CURTIM'         'U1'      'V1'     []       0
    'depth averaged velocity'   'm/s'    [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'CURTIM'         'U1'      'V1'     []       1
    'unit discharge (horizontal)'    'm^2/s'  [1 0 1 1 1]  1         2     'u'      'u'   'z'       'c'     'CURTIM'         'QU'      'QV'     []       1
    'depth averaged unit discharge'  'm^2/s'  [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'CURTIM'         'QU'      'QV'     []       1
    'depth averaged discharge'  'm^3/s'  [1 0 1 1 0]  1         1.9     ''       'd'   'd'       ''      'CURTIM'         'QU'      'QV'     []       1
    'discharge potential'       'm^3/s'  [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'CURTIM'         'QU'      'QV'     []       0
    'spiral flow intensity'     'm/s'    [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'CURTIM'         'RSP'     ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'u roughness parameter'     ''       [1 0 1 1 0]  1         1     ''       'u'   'u'       ''      'ROUGHNESS'      'CFUROU'  ''       []       0
    'v roughness parameter'     ''       [1 0 1 1 0]  1         1     ''       'v'   'v'       ''      'ROUGHNESS'      'CFVROU'  ''       []       0
    'max. bottom friction'      'N/m^2'  [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'TAUTIM'         'TAUMAX'  ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'salinity'                  'ppt'    [1 0 1 1 1]  1         1     ''       'z'   'z'       'c'     'DWQTIM'         'RSAL'    ''       []       0
    'temperature'               '°C'     [1 0 1 1 1]  1         1     ''       'z'   'z'       'c'     'DWQTIM'         'RTEM'    ''       []       0
    'vertical eddy diffusivity' 'm^2/s'  [1 0 1 1 1]  1         1     ''       'z'   'z'       'c'     'DWQTIM'         'DICWW'   ''       []       0
    'horizontal diffusivity'    'm^2/s'  [1 0 1 1 1]  1         1    ''        'z'   'z'       'c'     'DWQTIM'         'DICUV'   ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'initial bedload transport' 'm^3/m'  [1 0 1 1 0]  1         2     'u'      'z'   'z'       ''      'TRANSTIM'       'TTXI'    'TTYI'   []       1
    'avg bedload transport'     'm^3/sm' [1 0 1 1 0]  1         2     'u'      'z'   'z'       ''      'TRANSTIM'       'TTXA'    'TTYA'   []       1
    'initial susp. transport'   'm^3/m'  [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'TRANSTIM'       'TTXSI'   'TTYSI'  []       1
    'avg susp. transport'       'm^3/sm' [1 0 1 1 0]  1         2     'u'      'u'   'z'       ''      'TRANSTIM'       'TTXSA'   'TTYSA'  []       1
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'initial bed level'         'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'INITBOT'        'DP0'     ''       []       0
    'fixed layer'               'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'INITBOT'        'DPF'     ''       []       0
    'fixed layer'               'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'INITBOT'        'DPFIX'   ''       []       0
    'sed. layer above fixed bed' ...
    'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'BOTTIM'         'DP'      ''       []       0
    'time-varying bed level'    'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'BOTTIM'         'DP'      ''       []       0
    'cum. erosion/sedimentation' ...
    'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'BOTTIM'         'DP'      ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'transport layer thickness' 'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'GRASEDTIM'      'DEFF'    ''       1        0
    'transp. l. thickness (wlvl)' ...
    'm'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'GRASEDTIM'      'DEFFH'   ''       1        0
    'median grainsize'          'm'      [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'GRASEDTIM'      'DMED0'   ''       []       0
    'transport layer'           ''       [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'GRASEDTIM'      'PTRLA'   ''       1        0
    'transport layer (wlvl)'    ''       [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'GRASEDTIM'      'PTRLAH'  ''       1        0
    'exchange layer'            ''       [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'GRASEDTIM'      'PEXLA'   ''       1        0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0
    'GUU grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'u'   'u'       ''      'GRID'           'GUU'     ''       []       0
    'GVU grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'u'   'u'       ''      'GRID'           'GVU'     ''       []       0
    'GVV grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'v'   'v'       ''      'GRID'           'GVV'     ''       []       0
    'GUV grid distance'         'm'      [1 0 1 1 0]  1         1     ''       'v'   'v'       ''      'GRID'           'GUV'     ''       []       0
    'cell area water level point' ...
    'm^2'    [1 0 1 1 0]  1         1     ''       'z'   'z'       ''      'GRID'           'GSQS'    ''       []       0
    'cell area bottom point'    'm^2'    [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'GRID'           'GSQD'    ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''        ''       []       0};

%============================= AUTODETECTION ==================================
Info=vs_disp(FI,'GRID','XCOR');
nm=Info.SizeDim([1 2]);
Info=vs_disp(FI,'GRID','THICK');
k=Info.SizeDim(1);
SkipGroup={'GRID','KENMTIM','TEMPOUT','KENMCNST','INITBOT'};
DataProps=auto_map_detect(FI,DataProps,nm,k,SkipGroup);
%
Info=vs_disp(FI,'GRID','ZK');
fixedlayers=0;
if isstruct(Info)
    fixedlayers=Info.SizeDim>1;
end
%======================== SPECIFIC CODE DIMENSIONS ============================
Info=vs_disp(FI,'CURTIM','U1');
if isfield(Info,'NDim') & (Info.NDim==2)
    id=strmatch('velocity (horizontal)',DataProps(:,1),'exact');
    DataProps(id,:)=[];
    id=strmatch('unit discharge (horizontal)',DataProps(:,1),'exact');
    DataProps(id,:)=[];
    id=strmatch('velocity in depth averaged flow direction',DataProps(:,1),'exact');
    DataProps(id,:)=[];
    id=strmatch('velocity normal to depth averaged flow direction',DataProps(:,1),'exact');
    DataProps(id,:)=[];
end

[RouFlo,Chk]=vs_get(FI,'ROUGHNESS','ROUFLO','quiet');
if Chk
    switch RouFlo
        case 'MANN'
            RouFlo='Manning n';
            RouUnits='s/m^{1/3}';
        case 'WHIT'
            RouFlo='White-Colebrook/Nikuradse k';
            RouUnits='m';
        case 'CHEZ'
            RouFlo='Chezy C';
            RouUnits='m^{1/2}/s';
        case 'Z   '
            RouFlo='z_0';
            RouUnits='m';
    end
else
    RouFlo='parameter';
    RouUnits='';
end

Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE REMOVE ================================
for i=size(Out,1):-1:1

    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info)
        % remove references to non-stored data fields
        if strcmp(Out(i).Name,'grid') % S1 not available on file, so convert grid to 2D time-independent quantity.
            Out(i).DimFlag(1) = 0;
            Out(i).DimFlag(5) = 0;
        else
            Out(i)=[];
        end
    elseif isequal(Info.SizeDim,1)
        % remove references to non-stored data fields
        Out(i)=[];
    else
        switch Out(i).Name
            case {'u roughness parameter','v roughness parameter'},
                Out(i).Name=sprintf('%s roughness %s',Out(i).Name(1),RouFlo);
                Out(i).Units=RouUnits;
            case {'fixed layer','sed. layer above fixed bed'},
                [Pres,Chk]=vs_get(FI,'INITBOT','NVASTI','quiet');
                if ~isequal(Pres,1)
                    Out(i)=[];
                end
                % based on TRANNT ITOTAT, one can select bedload / suspended load
            case {'initial bedload transport','avg bedload transport'},
                [Pres,Chk]=vs_get(FI,'TRANNT','ITOTAT','quiet');
                if ~isequal(Pres,3) & ~isequal(Pres,2)
                    Out(i).Name=strrep(Out(i).Name,'bedload','total load');
                end
                % based on TRANNT ITOTAT, one can select bedload / suspended load
            case {'initial susp. transport','avg susp. transport'},
                [Pres,Chk]=vs_get(FI,'TRANNT','ITOTAT','quiet');
                if ~isequal(Pres,3) & ~isequal(Pres,2)
                    Out(i)=[];
                end
                % based on GRASEDTIM NLAYER, one can deselect exchange layer
            case {'exchange layer'},
                [Pres,Chk]=vs_get(FI,'GRASEDTIM',{1},'NLAYER','quiet');
                if ~isequal(Pres,2)
                    Out(i)=[];
                end
            case {'spiral flow intensity'},
                Info=vs_disp(FI,'GRID','THICK');
                if Info.SizeDim>1
                    % Out(i)=[];
                    Out(i).Name='spiral flow intensity (effective)';
                end
                if fixedlayers
                    Out(i)=[];
                end
            case {'salinity','temperature'},
                Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
                if Info.NDim==2
                    Out(i).DimFlag(K_)=0;
                end
        end
    end
end

% based on TRANSTIM DTSI = 0, one can deselect initial transport
[Pres,Chk]=vs_get(FI,'TRANSTIM',{1},'DTSI','quiet');
if isequal(Pres,0)
    for i=size(Out,1):-1:1
        switch Out(i).Name
            case {'initial bedload transport','initial total load transport','initial susp. transport'},
                Out(i,:)=[];
        end
    end
end

%======================== SPECIFIC CODE ADD ===================================
Strs={};%'initial total load transport','avg total load transport','transport layer','exchange layer','transport layer (wlvl)'};
for fld=1:length(Strs),
    Str=Strs{fld};
    i=strmatch(Str,{Out.Name},'exact');
    if ~isempty(i)
        Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
        if isfield(Info,'NDim') & (Info.NDim>=3)
            names=cellstr(multiline(sprintf(['fraction %i ' Str '\n'],1:Info.SizeDim(3))));
            names=names(1:end-1);
            Ins=Out(i*ones(length(names),1));
            for j=1:length(names)
                Ins(j).Name=names{j};
                if isempty(Ins(j).SubFld)
                    Ins(j).SubFld=j;
                else
                    Ins(j).SubFld=cat(2,j,Ins(j).SubFld);
                end
            end
            Out=insstruct(Out,i,Ins);
        end
    end
end

%======================= SET USEGRID OPTIONS ==================================
for i=1:length(Out)
    switch Out(i).ReqLoc
        case 'd'
            Out(i).UseGrid=3;%1;
            Out(i).Geom='SGRID-NODE';
        case 'z'
            Out(i).UseGrid=3;%2;
            Out(i).Geom='SGRID-FACE';
    end
    Out(i).Coords='xy';
end

[Out.TemperatureType] = deal('unspecified');
[Out(strcmp({Out.Name},'temperature')).TemperatureType] = deal('absolute');
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if ~isempty(strmatch(Props.Name,{'initial total load transport','avg total load transport', ...
        'transport layer','exchange layer','transport layer (wlvl)'},'exact'))
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
end
if Props.DimFlag(K_)
    Info=vs_disp(FI,'GRID','THICK');
    sz(K_)=Info.SizeDim(1);
    if strcmp(Props.Loc3D,'i')
        sz(K_)=sz(K_)+1;
    end
end
if Props.DimFlag(T_)
    Info=vs_disp(FI,Props.Group,[]);
    sz(T_)=Info.SizeDim;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [T,Tsc]=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[pars,Chk]=vs_get(FI,'PARAMS','*','quiet');
d0=tdelft3d(pars.IT01,pars.IT02);
switch Props.Group,
    case 'WAVTIM'
        [Tsc,Chk]=vs_let(FI,'WAVTIM',{t},'TIMWAV','quiet');
        T=d0+Tsc*pars.TSCALE/(24*3600);
    case {'CURTIM','TAUTIM','DWQTIM','KENMTIM','WIND'}
        [Tsc,Chk]=vs_let(FI,'CURTIM',{t},'TIMCUR','quiet');
        Info=vs_disp(FI,'INITBOT','ALTITUDE');
        if isstruct(Info)
            T=d0+Tsc/(24*3600);
        else
            T=d0+Tsc*pars.TSCALE/(24*3600);
        end
    case 'TRANSTIM'
        [Tsc,Chk]=vs_let(FI,'TRANSTIM',{t},'TSEDE','quiet');
        T=d0+Tsc*pars.TSCALE/(24*3600);
    case 'BOTTIM'
        [Tsc,Chk]=vs_let(FI,'BOTTIM',{t},'TIMBOT','quiet');
        T=d0+Tsc*pars.TSCALE/(24*3600);
    otherwise, % Unknown time definition or one field
        if isequal(t,0)
            Info=vs_disp(FI,Props.Group,[]);
            t=1:Info.SizeDim;
        end
        T=d0+zeros(size(t));
        Tsc=NaN*T;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd,
    case 'initialize'
        OK=optfig(mfig);
    case 'simsteps'
        Txt=simsteps(FI);
        ui_message('error',Txt);
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
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions simsteps', ...
    'Position',[11 voffset 160 20], ...
    'String','Check Time Step', ...
    'Tag','Pushbutton1');
OK=1;
% -----------------------------------------------------------------------------



function val1=interpdp(FI,val1)
[nfltyp,Chk]=vs_get(FI,'PARAMS','NFLTYP','quiet');
switch nfltyp
    case {0,2} % MAX
        dp22=val1(:,2:end,2:end);
        dp22=max(val1(:,1:end-1,1:end-1),dp22);
        dp22=max(val1(:,2:end,1:end-1),dp22);
        dp22=max(val1(:,1:end-1,2:end),dp22);
        val1(:,2:end,2:end)=dp22;
    case 3 % MIN
        dp22=val1(:,2:end,2:end);
        dp22=min(val1(:,1:end-1,1:end-1),dp22);
        dp22a=val1(:,2:end,1:end-1);
        dp22a=min(val1(:,1:end-1,2:end),dp22a);
        val1(:,2:end,2:end)=(dp22+dp22a)/2;
    case 1 % MEAN
        val1=interp2cen(val1,'t');
end

