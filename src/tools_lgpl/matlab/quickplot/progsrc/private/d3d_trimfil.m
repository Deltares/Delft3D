function varargout=d3d_trimfil(FI,domain,field,cmd,varargin)
%D3D_TRIMFIL QP support for Delft3D-FLOW map files.
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
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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

%========================= GENERAL CODE ===================================
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
        varargout={{}};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        hNew = plotthis(FI,Props,varargin{:});
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

if DataInCell==0.5 && strcmp(Props.ReqLoc,'z')
    DataInCell = 1;
elseif strcmp(Props.Name,'grid')
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

% select appropriate spatial indices ...
if any(DimFlag==7)
    dimlabels = getlabels(FI,Props);
    for i=1:length(DimFlag)
        if DimFlag(i)==7
            idx{i}=find(idx{i}==dimlabels{i});
        end
    end
end

if DimFlag(M_)&& DimFlag(N_)
    sz([M_ N_])=sz([N_ M_]);
    idx([M_ N_])=idx([N_ M_]);
end

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
Ans=[];
Info=vs_disp(FI,'map-const','ZK');
zlayermodel=0;
if isstruct(Info)
    zlayermodel=Info.SizeDim>1;
end
computeDZ=0;
switch Props.Name
    case {'relative hydrostatic pressure','relative total pressure','total pressure','hydrostatic pressure'}
        computeDZ=1;
    case {'depth averaged velocity','staggered depth averaged velocities','d.a. velocity fluctuations','velocity in depth averaged flow direction','velocity normal to depth averaged flow direction','head','froude number','time-aver. depth-aver. velocity'}
        if zlayermodel, computeDZ=1; end
end
XYRead = XYRead & ~strcmp(Props.Loc,'NA');

compute_unitvalue = (strcmp(Props.Val1,'R1FLX_UU') || strcmp(Props.Val1,'R1FLX_UUC')) && length(Props.Name)>9 && strcmp(Props.Name(end-8:end),'unit flux');
[coordtype,ok]=vs_get(FI,'map-const','COORDINATES','quiet');
Info=vs_disp(FI,'map-const','XZ');
coord_spherical = (isstruct(Info) && isequal(Info.ElmUnits,'[  DEG  ]')) || (ok && strcmp(deblank(coordtype),'SPHERICAL'));

if XYRead || compute_unitvalue || computeDZ

    if DimFlag(M_) && DimFlag(N_)
        x=vs_get(FI,'map-const','XCOR',idx([M_ N_]),'quiet!');
        y=vs_get(FI,'map-const','YCOR',idx([M_ N_]),'quiet!');
        %    x((x==0) & (y==0)) = NaN;
        %y(isnan(x))=NaN;
    end
    if strcmp(Props.Group,'map-sedgs-series')
        if strcmp(Props.Loc3D,'c')
            idxK_ = [idx{K_} idx{K_}(end)+1];
        else
            idxK_ = idx{K_};
        end
        z=vs_let(FI,'map-sedgs-series',idx(T_),'Z_INTERF',[idx([M_ N_]) {idxK_}],'quiet!');
        if strcmp(Props.Loc3D,'i')
            if DataInCell
                z=cat(4,z(:,:,:,1),(z(:,:,:,1:end-1)+z(:,:,:,2:end))/2,z(:,:,:,end));
            end
        else % 'c'
            if ~DataInCell
                z=(z(:,:,:,1:end-1)+z(:,:,:,2:end))/2;
            end
        end
        nk = size(z,4);
        x=reshape(x,[1 size(x)]);
        x=repmat(x,[1 1 1 nk]);
        y=reshape(y,[1 size(y)]);
        y=repmat(y,[1 1 1 nk]);
    elseif zlayermodel && (DimFlag(K_) || computeDZ)
        if isstruct(vs_disp(FI,'map-series','LAYER_INTERFACE'))
            z=vs_let(FI,'map-series',idx(T_),'LAYER_INTERFACE',[idx([M_ N_]) {0}],'quiet!');
            z(z==-999) = NaN;
        else
            h=vs_let(FI,'map-const','ZK','quiet!');
            h(1)=-inf;
            h(end)=inf;
            s=vs_let(FI,'map-series',idx(T_),'S1',idx([M_ N_]),'quiet!');
            szz=[size(s) length(h)];
            z=repmat(reshape(h,[1 1 1 length(h)]),[szz(1:3),1]);
            z=reshape(z,[prod(szz(1:3)) szz(4)]);
            s=s(:);
            for i=1:szz(4)
                fl=z(:,i)>s;
                z(fl,i)=s(fl);
            end
            dp=readdps(FI,idx);
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
        end
        if computeDZ
            dz=diff(z,1,4);
        end
        if strcmp(Props.Loc3D,'i')
            if DataInCell
                z=cat(4,z(:,:,:,1),(z(:,:,:,1:end-1)+z(:,:,:,2:end))/2,z(:,:,:,end));
            end
        else % 'c'
            if ~DataInCell
                z=(z(:,:,:,1:end-1)+z(:,:,:,2:end))/2;
            end
        end
        if DataInCell
            idxK_=[idx{K_} idx{K_}(end)+1];
        else
            idxK_=idx{K_};
        end
        if DimFlag(K_)
            z=z(:,:,:,idxK_);
            x=reshape(x,[1 size(x)]);
            x=repmat(x,[1 1 1 length(idxK_)]);
            y=reshape(y,[1 size(y)]);
            y=repmat(y,[1 1 1 length(idxK_)]);
        end
    elseif DimFlag(K_) && strcmp(Props.Loc3D,'b')
        dp=readdps(FI,idx);
        I=vs_disp(FI,'map-sed-series','DP_BEDLYR');
        idxK_=[idx{K_} idx{K_}(end)+1];
        if isstruct(I)
            dz=vs_let(FI,'map-sed-series',idx(T_),'DP_BEDLYR',[idx([M_ N_]) {0}],'quiet!');
        else
            dz=vs_let(FI,'map-sed-series',idx(T_),'THLYR',[idx([M_ N_]) {0}],'quiet!');
            dz=cumsum(dz,4);
            idxK_ = idxK_-1;
        end
        szdp=size(dp);
        if length(szdp)<3
            szh(3)=1;
        end
        if DataInCell
            z=repmat(dp,[1 1 1 length(idxK_)]);
            for k=find(idxK_>0)
                z(:,:,:,k)=dp-dz(:,:,:,idxK_(k));
            end
        else
            z=repmat(dp,[1 1 1 length(idxK_)-1]);
            for k = 1:length(idxK_)-1
                if idxK_(k)==0
                    z(:,:,:,k)=dp - (dz(:,:,:,idxK_(k+1)))/2;
                else
                    z(:,:,:,k)=dp - (dz(:,:,:,idxK_(k)) + dz(:,:,:,idxK_(k+1)))/2;
                end
            end
        end
        x=reshape(x,[1 size(x)]);
        x=repmat(x,[1 1 1 size(z,4)]);
        y=reshape(y,[1 size(y)]);
        y=repmat(y,[1 1 1 size(z,4)]);
    elseif DimFlag(K_) || computeDZ
        dp=readdps(FI,idx);
        [h,Chk_h]=vs_let(FI,'map-series',idx(T_),'S1',idx([M_ N_]),'quiet');
        if Chk_h
            if size(dp,1)==1
                for i=1:size(h,1)
                    h(i,:,:)=h(i,:,:)-dp;
                end
            else
                h=h-dp;
            end
        end
        thk=vs_let(FI,'map-const','THICK','quiet!');
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
        if computeDZ
            dz=repmat(h,[1 1 1 length(thk)]);
            for k = 1:length(thk)
                dz(:,:,:,k) = dz(:,:,:,k)*thk(k);
            end
        end
        if DataInCell
            idxK_=[idx{K_} idx{K_}(end)+1];
        else
            idxK_=idx{K_};
        end
        cthk=cthk(idxK_);
        if Chk_h
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
        else
            z=reshape(cthk,[1 1 1 length(cthk)]);
            z=repmat(z,[1 size(x)]);
        end
        x=reshape(x,[1 size(x)]);
        x=repmat(x,[1 1 1 length(cthk)]);
        y=reshape(y,[1 size(y)]);
        y=repmat(y,[1 1 1 length(cthk)]);
    end

    % grid interpolation ...
    if compute_unitvalue
        if coord_spherical
            error('unit vector not yet supported for spherical coordinates')
        end % assuming grid locations don't vary in time
        guu = sqrt(diff(x(1,[1 1:end],:,1),1,2).^2 + diff(y(1,[1 1:end],:,1),1,2).^2);
        gvv = sqrt(diff(x(1,:,[1 1:end],1),1,3).^2 + diff(y(1,:,[1 1:end],1),1,3).^2);
    end
    [x,y]=gridinterp(DataInCell,DimFlag(K_),Props.ReqLoc,x,y);
    %
    if ~DataInCell && Props.NVal==2 && isstruct(vs_disp(FI,'map-series','Ndry_GRS')) && length(idx{T_})==1 % if not length(idx{:})==1 I have more than 1 time step and kfscc in function xy_cutcell becomes a cell.
        [x,y] = xy_cutcell(squeeze(x),squeeze(y),FI,idx);
        if (DimFlag(K_))
            x = reshape(x,1,size(x,1),size(x,2));
            y = reshape(y,1,size(y,1),size(y,2));
        end
    end
end

% load data ...
if DataRead
    elidx=idx(2:end);
    ThinDam=0;
    DepthInZeta=0;
    if Props.NVal==1.9
        Props.NVal=2;
        ThinDam=2;
    end
    switch Props.Name
        case {'relative hydrostatic pressure','relative total pressure','total pressure','hydrostatic pressure'}
            selectK = elidx{4};
            elidx{4}=0;
        case {'depth averaged velocity','staggered depth averaged velocities','d.a. velocity fluctuations','time-aver. depth-aver. velocity'}
            Info=vs_disp(FI,Props.Group,Props.Val1);
            Flag3D=isequal(size(Info.SizeDim),[1 3]);
            if Flag3D
                elidx{end+1}=0;
            end
        case {'depth averaged concentration','time-aver. depth-aver. concentr.'}
            Flag3D = 1;
            elidx{end+1}=0;
        case {'froude number','head'}
            Info=vs_disp(FI,Props.Group,Props.Val1);
            Flag3D=isequal(size(Info.SizeDim),[1 3]);
            if Flag3D
                elidx{end+1}=0;
            end
            Props.NVal=2;
        case {'velocity in depth averaged flow direction','velocity normal to depth averaged flow direction'}
            % always load 3D field to determine depth averaged flow direction
            elidx{K_-1}=0;
        case {'thin dams','temporarily inactive velocity points'}
            Props.NVal=2;
            ThinDam=1;
        case {'domain decomposition boundaries','open boundaries','closed boundaries','partition boundaries'}
            Props.NVal=2;
            ThinDam=1;
            % These quantities are determined by comparing a cell value
            % with a neighbouring cell value. To make sure that this works
            % properly for the high M/N boundary, we need to make sure that
            % the neighbour on those sides is included when requesting the
            % data.
            if elidx{2}(end)<sz(3) % M_
                elidx{2}(end+1) = elidx{2}(end)+1;
            end
            if elidx{3}(end)<sz(4) % N_
                elidx{3}(end+1) = elidx{3}(end)+1;
            end
        case {'sediment thickness','base level of sediment layer'}
            switch Props.Val1
                case 'THLYR'
                    elidx{end+1}=0;
                case 'DP_BEDLYR'
                    I=vs_disp(FI,Props.Group,Props.Val1);
                    elidx{end+1}=I.SizeDim(3);
            end
        case {'cum. erosion/sedimentation','initial bed level','bed level in water level points','cumulative mass error'}
            DepthInZeta=DataInCell | strcmp(Props.ReqLoc,'z');
        case {'Shepard deposit classification','USDA deposit classification'}
            elidx{end+1}=0;
    end
    if Props.NVal==0.9
            Props.NVal=2;
            ThinDam=2;
    end
    if isequal(subforig,'s') || isequal(subforig,'sb')
        if isequal(Props.SubFld,length(subf)) && length(subf)>1
            Props.SubFld = 1:length(subf)-1;
        end
    end
    if ~isempty(Props.SubFld)
        if iscell(Props.SubFld)
            elidx=cat(2,elidx,Props.SubFld); % last dimensions automatically dropped after reading
        else
            elidx(end+1)={Props.SubFld}; % last dimension automatically dropped after reading
        end
    end
    if ~DimFlag(T_)
        idx{T_} = 1;
    end
    elidx(~DimFlag(2:end))=[];
    if (Props.NVal==0) || DepthInZeta
        val1=[];
    else
        if strcmp('time-aver. depth-aver. velocity',Props.Name) || strcmp('time-averaged bed',Props.Name) || strcmp('time-av. bed minus cr.sect.aver.',Props.Name) || strcmp('time-averaged water depth',Props.Name)  || strcmp('time-aver. depth-aver. concentr.',Props.Name) 
            val1=vs_let(FI,Props.Group,{0},Props.Val1,elidx,'quiet!'); %load all data
            Nsteps = size(val1,1);    
            useFFTforFINDINGrange = 1; %0: uses a prescribed value of steps NstAV before and after to do the average 1: I use the fft to find the dominant frequency
            mm=40; %to be changed any time
            nn=12; %to be changed any time
            multiplier = 1; %I use multiplier*NstAV values before and after
            if useFFTforFINDINGrange  
                Fs = 1; %sampling frequency
                curve = val1(1:Nsteps,nn,mm);  %double check if first index is mm or first is nn
                curve = curve - mean(curve);
                xdft = fft(curve,Nsteps);
                maxAmp = max(abs(xdft));                
                freq = [0:Nsteps-1].*(Fs/Nsteps); %This is your total freq-axis
                freqsYouCareAbout = freq(freq < Fs/2);  %You only care about either the pos or neg frequencies, since they are redundant for a real signal.
                xdftYouCareAbout = abs(xdft(1:round(Nsteps/2))); %Take the absolute magnitude.
                [maxVal, index] = max(xdftYouCareAbout); %maxVal is your (un-normalized) maximum amplitude
                maxFreq = freqsYouCareAbout(index); % This is the frequency of your dominant signal.
                maxT = 1/maxFreq;
                NstAV = int32(maxT/2);%half period before and half after
            else
                NstAV = 10; % I average using NstAV time steps before and NstAV after
            end            
            Time_num = cell2mat(idx(T_));
            meanArray = mean(val1(max(Time_num-NstAV,1):min(Time_num+NstAV,Nsteps),:,:,:),1);
            val1 = meanArray;
        else
            val1=vs_let(FI,Props.Group,idx(T_),Props.Val1,elidx,'quiet!');
        end
    end
    if isempty(Props.Val2)
        val2=[];
    else
        if strmatch('time-aver. depth-aver. velocity',Props.Name)
            val2=vs_let(FI,Props.Group,{0},Props.Val2,elidx,'quiet!'); %load all data
            Nsteps = size(val2,1);
            %Note: NstAV here is the one computed (or prescribed) for the x component
            meanArray =  mean(val2(max(Time_num-NstAV,1):min(Time_num+NstAV,Nsteps),:,:,:),1);
            val2 = meanArray;
        else
            val2=vs_let(FI,Props.Group,idx(T_),Props.Val2,elidx,'quiet!');
        end
    end
    val3=[];

    [gravity,Success]=vs_let(FI,'map-const','GRAVITY','quiet');
    if ~Success
        gravity = 9.81;
    end

    if compute_unitvalue
        guu(guu==0) = 1;
        gvv(gvv==0) = 1;
        for t = size(val1,1):-1:1
            for k = size(val1,4):-1:1
                val1(t,:,:,k) = val1(t,:,:,k)./guu;
                val2(t,:,:,k) = val2(t,:,:,k)./gvv;
            end
        end
    end
    switch Props.Name
        case 'high and low bed levels'
            val1=-val1;
            val2=-val2;
            val3=vs_let(FI,Props.Group,idx(T_),'kfs_cc',elidx,'quiet!');
            Props.NVal=3;
        case 'total transport'
            val1r=vs_let(FI,Props.Group,idx(T_),'SBUU',elidx,'quiet!');
            val1=val1+val1r;
            clear val1r
            val2r=vs_let(FI,Props.Group,idx(T_),'SBVV',elidx,'quiet!');
            val2=val2+val2r;
            clear val2r
        case 'mean total transport'
            val1r=vs_let(FI,Props.Group,idx(T_),'SBUUA',elidx,'quiet!');
            val1=val1+val1r;
            clear val1r
            val2r=vs_let(FI,Props.Group,idx(T_),'SBVVA',elidx,'quiet!');
            val2=val2+val2r;
            clear val2r
        case {'Shepard deposit classification','USDA deposit classification'}
            Props.SubFld='sb1';
            subf=lower(getsubfields(FI,Props));
            isand = wildstrmatch('*sand*',subf);
            isilt = wildstrmatch('*silt*',subf);
            iclay = wildstrmatch('*clay*',subf);
            sand = sum(val1(:,:,:,:,isand),5);
            silt = sum(val1(:,:,:,:,isilt),5);
            clay = sum(val1(:,:,:,:,iclay),5);
            [val1,Ans.Classes] = classify_Sediment(sand,silt,clay,strtok(Props.Name));
    end
    if isequal(subforig,'s') || isequal(subforig,'sb')
        if length(Props.SubFld)>1
            val1=sum(val1,ndims(val1)); % sum of all fractions
            val2=sum(val2,ndims(val2)); % sum of all fractions
        end
    end
    switch Props.Name
        case {'relative hydrostatic pressure','relative total pressure','total pressure','hydrostatic pressure'}
            [rhoconst,Success]=vs_let(FI,'map-const','RHOCONST','quiet');
            if ~Success
                rhoconst = 1000;
            end
            %
            if zlayermodel
                val1 = dz.*val1*gravity;
                val1 = val1(:,:,:,end:-1:1);
                val1 = cumsum(val1,4) - val1/2;
                val1 = val1(:,:,:,end:-1:1);
            else
                val1 = dz.*val1*gravity;
                val1 = cumsum(val1,4) - val1/2;
            end
            val1 = val1(:,:,:,selectK);
            %
            if strcmp(Props.Name(1:8),'relative')
                if size(z,4)==size(val1,4)
                    val1 = val1 + gravity*rhoconst*z;
                else
                    val1 = val1 + gravity*rhoconst*(z(:,:,:,1:end-1)+z(:,:,:,2:end))/2;
                end
            end
            if ~isempty(strfind(Props.Name,'total'))
                val1 = val1 + val2(:,:,:,selectK);
                val2 = [];
            end
        case {'grid cell surface area'}
            xc=vs_get(FI,'map-const','XCOR',idx([M_ N_]),'quiet!');
            yc=vs_get(FI,'map-const','YCOR',idx([M_ N_]),'quiet!');
            val1(:) = NaN;
            lidx{1} = 2:length(idx{M_});
            lidx{2} = 2:length(idx{N_});
            val1(1,lidx{:}) = reshape(cellarea(xc,yc),[1 size(xc)-1]);
        case {'water level','temporarily inactive water level points'}
            kfu=vs_let(FI,Props.Group,idx(T_),'KFU',elidx,'quiet!');
            kfv=vs_let(FI,Props.Group,idx(T_),'KFV',elidx,'quiet!');
            if strcmp(Props.Name,'water level')
                val1(kfv(:,[1 1:end-1],:)==0 & kfv==0 & kfu(:,:,[1 1:end-1])==0 & kfu==0)=NaN;
            else
                Mask = kfv(:,[1 1:end-1],:)==0 & kfv==0 & kfu(:,:,[1 1:end-1])==0 & kfu==0;
                val1(Mask) = 1;
                val1(~Mask) = NaN;
            end
        case {'domain decomposition boundaries','open boundaries','closed boundaries'}
            switch Props.Name
                case 'domain decomposition boundaries'
                    bval = 3;
                case 'open boundaries'
                    bval = 2;
                case 'closed boundaries'
                    bval = 0;
            end
            %
            % A boundary is a transition from internal (1) to boundary type (2
            % for open boundaries, 3 for DD boundaries, 0 for closed
            % boundaries). The current implementation follows the KFU
            % convention (i.e. 1 is no marker, 0 is marker), so the condition
            % contains all kinds of negations. The N and M direction are still
            % reversed, so val2 is associated with the first dimension of the
            % data and val1 is associated with the second dimension of the
            % data.
            %
            val2 = double((val1(:,1:end,:)~=bval | val1(:,[2:end end],:)~=1) & (val1(:,1:end,:)~=1 | val1(:,[2:end end],:)~=bval));
            val1 = double((val1(:,:,1:end)~=bval | val1(:,:,[2:end end])~=1) & (val1(:,:,1:end)~=1 | val1(:,:,[2:end end])~=bval));
        case {'partition boundaries'}
            %
            % A partition boundary is a change in partition number. The
            % current implementation follows the KFU convention (i.e. 1 is
            % no marker, 0 is marker), so we check whether the partition
            % numbers are equal. The N and M direction are still reversed,
            % so val2 is associated with the first dimension of the data
            % and val1 is associated with the second dimension of the data.
            %
            val2 = double(val1(:,1:end,:) == val1(:,[2:end end],:));
            val1 = double(val1(:,:,1:end) == val1(:,:,[2:end end]));
        case {'velocity'}
            [val3,Chk]=vs_let(FI,Props.Group,idx(T_),'WPHY',elidx,'quiet');
            if ~Chk
                val3=[];
                Props.NVal=2;
            end
        case 'sediment thickness'
            if size(val1,4)>1
                val1=sum(val1,4);
            end
        case 'base level of sediment layer'
            if size(val1,4)>1
                val1=sum(val1,4);
            end
            val1=-val1+readdps(FI,idx,0);
        case {'high bed level','low bed level'}
            val1 = -val1;
        case {'initial bed level','bed level in water level points'}
            if DepthInZeta %strcmp(Props.Val1,'DPSED') || DataInCell
                val1=readdps(FI,idx,strcmp(Props.Name,'initial bed level'));
                Props.Loc='z';
            else
                val1=-val1;
            end
            val1(val1==999)=NaN;
        case {'cum. erosion/sedimentation','cumulative mass error'}
            [FI,val1] = eros_sed(FI,Props.Name,idx);
        case {'depth averaged velocity','staggered depth averaged velocities','d.a. velocity fluctuations','froude number','head','time-aver. depth-aver. velocity'}
            if Flag3D
                if zlayermodel
                    val1(val1==-999)=0;
                    val2(val2==-999)=0;
                    dz(isnan(dz))=0;
                    val1=val1.*dz;
                    val2=val2.*dz;
                    h=sum(dz,4);
                    val1(h==0)=0; val2(h==0)=0; h(h==0)=1;
                    val1=sum(val1,4); val1=val1./h;
                    val2=sum(val2,4); val2=val2./h;
                else
                    thk=vs_let(FI,'map-const','THICK','quiet!');
                    for k=1:length(thk)
                        val1(:,:,:,k)=val1(:,:,:,k)*thk(k);
                        val2(:,:,:,k)=val2(:,:,:,k)*thk(k);
                    end
                    val1=sum(val1,4);
                    val2=sum(val2,4);
                end
                elidx(end)=[]; % don't read K_ in case of U/VMNLDF
            end
            switch Props.Name
                case 'd.a. velocity fluctuations'
                    val1r=vs_let(FI,Props.Group,idx(T_),'UMNLDF',elidx,'quiet!');
                    val1=val1-val1r;
                    val1r=[];
                    val2r=vs_let(FI,Props.Group,idx(T_),'VMNLDF',elidx,'quiet!');
                    val2=val2-val2r;
                    val2r=[];
                case 'head'
                    val3=vs_let(FI,Props.Group,idx(T_),'S1',elidx,'quiet!');
                case 'froude number'
                    val3=vs_let(FI,Props.Group,idx(T_),'S1',elidx,'quiet!');
                    dp=readdps(FI,idx);
                    if size(dp,1)==1,
                        for i=1:size(val3,1)
                            val3(i,:,:)=val3(i,:,:)-dp;
                        end
                    else
                        val3=val3-dp;
                    end
                    val3(val3==0)=inf;
            end
        case {'depth averaged concentration','time-aver. depth-aver. concentr.'}
            if Flag3D
                if zlayermodel
                    val1(val1==-999)=0;
                    val1=val1.*dz;
                    h=sum(dz,4);
                    val1(h==0)=0; h(h==0)=1;
                    val1=sum(val1,4); val1=val1./h;
                else
                    thk=vs_let(FI,'map-const','THICK','quiet!');
                    for k=1:length(thk)
                        val1(:,:,:,k)=val1(:,:,:,k)*thk(k);
                    end
                    val1=sum(val1,4);
                end
                elidx(end)=[]; % don't read K_ in case of U/VMNLDF
            end
        case 'water depth'
            dp=readdps(FI,idx);
            if size(dp,1)==1,
                for i=1:size(val1,1)
                    val1(i,:,:)=val1(i,:,:)-dp;
                end
            else
                val1=val1-dp;
            end
        case {'bed level in velocity points'}
            val1 = -val1;
            val2 = -val2;
    end

    if zlayermodel
        val1(val1==-999)=NaN;
        val2(val2==-999)=NaN;
        val3(val3==-999)=NaN;
    end

    if DataInCell && isequal(Props.ReqLoc,'d')
        Props.ReqLoc='z';
    end
    % combine vectors components ...
    if isequal(Props.VecType,'m')
        [val1,val2]=dir2uv(val1,val2);
    elseif isequal(Props.VecType,'mr')
        [alf,Chk] = vs_let(FI,'map-const','ALFAS',idx([M_ N_]),'quiet');
	if ~Chk
	    ui_message('warning','No direction information in file. Vector direction probably incorrect.');
	    alf=0;
	end
	for t = 1:size(val2,1)
	    val2(t,:) = val2(t,:) + alf(1,:);
	end
	[val1,val2]=dir2uv(val1,val2);
    end
    % data interpolation ...
    if isequal(Props.Loc,'d') && isequal(Props.ReqLoc,'z')
        val1=interp2cen(val1,'t');
        if ~isempty(val2)
            val2=interp2cen(val2,'t');
        end
    elseif isequal(Props.Loc,'u') && isequal(Props.ReqLoc,'z')
        if zlayermodel
            val1(val1==0)=NaN;
            val2(val2==0)=NaN;
        end
        [val1,val2]=uv2cen(val1,val2);
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

    % combine vectors components ...
    if isequal(Props.VecType,'u') && Props.MNK<=1
        % rotate n,m components into x,y direction ...
        [alf,Chk] = vs_get(FI,'map-const','ALFAS',idx([M_ N_]),'quiet');
        if ~Chk
            ui_message('warning','No direction information in file. Vector direction probably incorrect.');
            alf=0;
        end
        alf = alf*pi/180;
        if Props.MNK<0
            [val1,val2]=cur2ca(val1,1i*val2,alf);
        else
            [val1,val2]=cur2ca(val1,val2,alf);
        end
    end

    switch Props.Name
        case {'velocity in depth averaged flow direction','velocity normal to depth averaged flow direction'}
            if zlayermodel
                szv1=size(val1);
                dav1=zeros(szv1(1:3));
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
                thk=vs_let(FI,'map-const','THICK','quiet!');
                szv1=size(val1);
                dav1=zeros(szv1(1:3));
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

% select active points ...
idx1 = idx;
idx1{M_}(end+1) = min(idx1{M_}(end)+1,sz(M_));
idx1{N_}(end+1) = min(idx1{N_}(end)+1,sz(N_));
act=abs(vs_get(FI,'map-const','KCS',idx1([M_ N_]),'quiet!'));
switch Props.ReqLoc
    case 'd'
        %  act=vs_get(FI,'TEMPOUT','CODB',idx([M_ N_]),'quiet!');
        act=conv2(double(act==1),[1 1;1 1],'valid')>0;
        gridact=act;
    otherwise % 'z', always if DataInCell
        if DataInCell
            %gridact=vs_get(FI,'TEMPOUT','CODB',idx([M_ N_]),'quiet!');
            gridact=conv2(double(act==1),[1 1;1 1],'valid')>0;
            act=act(1:end-1,1:end-1);
        else
            act=act(1:end-1,1:end-1);
            gridact=act;
        end
end

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

if Props.NVal>0 && ~strcmp(Props.Loc,'NA')
    szz=[size(val1) 1]; % extent szx for the case that dataset in K dir. is 1
    szz1=szz([1:2 4:end]);
    szz1(2)=szz(2)*szz(3);
    val1=reshape(val1,szz1);
    val1(:,act~=1,:)=NaN;
    val1=reshape(val1,szz);
    if ~isempty(val2)
        val2(isnan(val1))=NaN;
    end
    if ~isempty(val3)
        val3(isnan(val1))=NaN;
    end
end

% select subrange if necessary ... M,N,K only
if DataInCell
    for i=[M_ N_ K_]
        if DimFlag(i)
            allidx(i)=0;
        end
    end
end
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
switch Props.NVal
    case {1,5,6}
        val1=val1(:,ind{:});
    case 2
        val1=val1(:,ind{:});
        val2=val2(:,ind{:});
    case 3
        val1=val1(:,ind{:});
        val2=val2(:,ind{:});
        val3=val3(:,ind{:});
end

% permute n and m dimensions into m and n if necessary
if DimFlag(M_) && DimFlag(N_)
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
    switch Props.NVal
        case {1,5,6}
            val1=permute(val1,[1 1+perm]);
        case 2
            val1=permute(val1,[1 1+perm]);
            val2=permute(val2,[1 1+perm]);
        case 3
            val1=permute(val1,[1 1+perm]);
            val2=permute(val2,[1 1+perm]);
            val3=permute(val3,[1 1+perm]);
    end
end

% reshape if a single timestep is selected ...
if DimFlag(ST_)
    sz=[size(val1) 1]; sz(2)=[];
    switch Props.NVal
        case {1,5,6}
            val1=reshape(val1,sz);
        case 2
            val1=reshape(val1,sz);
            val2=reshape(val2,sz);
        case 3
            val1=reshape(val1,sz);
            val2=reshape(val2,sz);
            val3=reshape(val3,sz);
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
    if Props.NVal>0
        sz=size(val1); sz=[sz(2:end) 1];
        switch Props.NVal
            case {1,5,6}
                val1=reshape(val1,sz);
            case 2
                val1=reshape(val1,sz);
                val2=reshape(val2,sz);
            case 3
                val1=reshape(val1,sz);
                val2=reshape(val2,sz);
                val3=reshape(val3,sz);
        end
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    if coord_spherical
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    else
        Ans.XUnits='m';
        Ans.YUnits='m';
    end
    if DimFlag(K_)
        Ans.Z=z;
        Ans.ZUnits='m';
    end
end
if strcmp(Props.Name,'high and low bed levels')
    %
    % 1) create the polygons for the non-cut cells ...
    %
    x1 = cat(3,x(1:end-1,1:end-1),x(2:end,1:end-1),x(2:end,2:end),x(1:end-1,2:end),x(1:end-1,1:end-1),x(1:end-1,1:end-1));
    y1 = cat(3,y(1:end-1,1:end-1),y(2:end,1:end-1),y(2:end,2:end),y(1:end-1,2:end),y(1:end-1,1:end-1),y(1:end-1,1:end-1));
    x1 = permute(x1,[3 2 1]);
    y1 = permute(y1,[3 2 1]);
    val1 = permute(val1,[3 2 1]);
    val2 = permute(val2,[3 2 1]);
    val3 = permute(val3,[3 2 1]);
    ok = none(isnan(x1) | isnan(y1),1) & abs(val3)>1;
    x1(6,:) = NaN;
    y1(6,:) = NaN;
    v = repmat(val2,[6 1 1]);
    v(6,:) = NaN;
    x = x1(:,ok);
    y = y1(:,ok);
    v  = v(:,ok);
    x = x(:);
    y = y(:);
    v = v(:);
    %
    % 2) create the polygons for the cut cells (high part) ...
    %
    Time = idx(T_);
    idxN = idx{M_}(ind{1});
    idxM = idx{N_}(ind{2});
    xpoly = vs_get(FI,'map-series',Time,'INTx_GRS',{0 idxN idxM},'quiet');
    ypoly = vs_get(FI,'map-series',Time,'INTy_GRS',{0 idxN idxM},'quiet');
    ndryp = vs_get(FI,'map-series',Time,'Ndry_GRS',{idxN idxM},'quiet');
    icut = abs(val3)<=1;
    xpoly = xpoly(:,icut);
    ypoly = ypoly(:,icut);
    ndryp = ndryp(icut);
    for i = 1:size(xpoly,2)
        xpoly(ndryp(i)+1,i)   = xpoly(1,i);
        ypoly(ndryp(i)+1,i)   = ypoly(1,i);
        xpoly(ndryp(i)+2:7,i) = NaN;
        ypoly(ndryp(i)+2:7,i) = NaN;
    end
    v2 = val1(icut)';
    v2 = repmat(v2,[7 1]);
    x = [x;xpoly(:)];
    y = [y;ypoly(:)];
    v = [v;v2(:)];
    %
    % 3) create the polygons for the cut cells (low part) ...
    %
    x1 = x1(:,icut);
    y1 = y1(:,icut);
    XCOR_info = vs_disp(FI,'map-const','XCOR');
    INTx_GRS_info = vs_disp(FI,'map-series','INTx_GRS');
    if XCOR_info.NByteVal > INTx_GRS_info.NByteVal
        x1 = double(single(x1));
        y1 = double(single(y1));
    end
    xcomp = repmat(NaN,size(xpoly));
    ycomp = repmat(NaN,size(ypoly));
    for i = 1:size(xpoly,2)
        % xpoly,ypoly --> clockwise or counterclockwise
        np = ndryp(i);
        if clockwise(xpoly(1:np,i),ypoly(1:np,i))<0
            xpoly(1:np,i) = xpoly(np:-1:1,i);
            ypoly(1:np,i) = ypoly(np:-1:1,i);
        end
        % xpoly,ypoly --> clockwise
        % x1,y1 --> counterclockwise
        %
        % first point is boundary point
        xcomp(1,i) = xpoly(1,i);
        ycomp(1,i) = ypoly(1,i);
        % determine first corner point in common
        % first check if first point is a corner point
        j = 1;
        while j<5 && (x1(j,i)~=xpoly(1,i) || y1(j,i)~=ypoly(1,i))
            j = j+1;
        end
        if j==5
            % first point is not a corner point
            % second point is always a corner point, now find it
            j = 1;
            while x1(j,i)~=xpoly(2,i) || y1(j,i)~=ypoly(2,i)
                j = j+1;
            end
        end
        % loop through the corners x1,y1 of the cell in counterclockwise
        % and add them to the xcomp,ycomp array
        for k = 1:4
            jk = mod(j+k-1,4)+1;
            % until we find a corner point that is again in coFon
            if (x1(jk,i)==xpoly(np,i) && y1(jk,i)==ypoly(np,i)) || ...
                    (x1(jk,i)==xpoly(np-1,i) && y1(jk,i)==ypoly(np-1,i))
                break
            end
            xcomp(k+1,i) = x1(jk,i);
            ycomp(k+1,i) = y1(jk,i);
        end
        % now add last point (also boundary point)
        xcomp(k+1,i) = xpoly(np,i);
        ycomp(k+1,i) = ypoly(np,i);
        % and close the polygon
        xcomp(k+2,i) = xpoly(1,i);
        ycomp(k+2,i) = ypoly(1,i);
    end
    v2 = val2(icut)';
    v2 = repmat(v2,[7 1]);
    x = [x;xcomp(:)];
    y = [y;ycomp(:)];
    v = [v;v2(:)];
    %
    Ans.X=x;
    Ans.Y=y;
    Ans.Val=v;
    Props.NVal = 0;
end
switch Props.NVal
    case {1,5,6}
        Ans.Val=val1;
    case 2
        if ThinDam==2
            Ans.XDam=~isnan(val1);
            Ans.YDam=~isnan(val2);
            Ans.XDamVal=val1;
            Ans.YDamVal=val2;
        elseif ThinDam
            val1(isnan(val1))=1;
            val2(isnan(val2))=1;
            Ans.XDam=~val1;
            Ans.YDam=~val2;
        else
            Ans.XComp=val1;
            Ans.YComp=val2;
        end
    case 3
        Ans.XComp=val1;
        Ans.YComp=val2;
        Ans.ZComp=val3;
end

% read time ...
if DimFlag(T_)
    Ans.Time=readtim(FI,Props,idx{T_});
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function [TZshift,TZstr]=gettimezone(FI,domain,Props)
TZstr = '';
Info = vs_disp(FI,'map-const','TZONE');
if isstruct(Info)
    TZshift = vs_get(FI,'map-const','TZONE','quiet');
else
    TZshift = NaN;
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function Out=infile(FI,domain)
FI = guarantee_options(FI);
%======================== SPECIFIC CODE ===================================
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'Geom'  'Coords' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'    'Val2'  'SubFld' 'MNK' };
DataProps={'morphologic grid'          ''       [0 0 1 1 0]  0         0    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'XCOR'    ''       []       0
    'hydrodynamic grid'                ''       [1 0 1 1 1]  0         0    'sQUAD' 'xy'     ''        'z'   'z'       'i'     'map-series'     'S1'      ''       []       0
    'grid'                             ''       [1 0 1 1 1]  0         0    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'S1'      ''       []       0
    'domain decomposition boundaries'  ''       [0 0 1 1 0]  0         0    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'KCS'     ''       []       0
    'open boundaries'                  ''       [0 0 1 1 0]  0         0    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'KCS'     ''       []       0
    'closed boundaries'                ''       [0 0 1 1 0]  0         0    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'KCS'     ''       []       0
    'thin dams'                        ''       [0 0 1 1 0]  0         0    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'KCU'     'KCV'    []       0
    'temporarily inactive water level points' ...
                                       ''       [1 0 1 1 0]  2         5    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'KFU'     ''       []       0
    'temporarily inactive velocity points' ...
                                       ''       [1 0 1 1 0]  0         0    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'KFU'     'KFV'    []       0
    'top active layer at water level point (kfsmax)' ...
                                       ''       [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'KFSMAX'  ''       []       0
    'top active layer at velocity points (kfu/vmax)' ...
                                       ''       [1 0 1 1 0]  1         0.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'KFUMAX'  'KFVMAX' []       0
    'bottom active layer at water level point (kfsmin)' ...
                                       ''       [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'KFSMIN'  ''       []       0
    'bottom active layer at velocity points (kfu/vmin)' ...
                                       ''       [1 0 1 1 0]  1         0.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'KFUMIN'  'KFVMIN' []       0
    'parallel partition numbers'       ''       [0 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-const'      'PPARTITION'  ''       []       0
    'partition boundaries'             ''       [0 0 1 1 0]  0         0    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'PPARTITION' ''    []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'air pressure'                     'N/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'PATM'    ''       []       0
    'air temperature'                  '°C'     [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'AIRTEM'  ''       []       0
    'cloud coverage'                   '%'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'CLOUDS'  ''       []       0
    'relative air humidity'            '%'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'AIRHUM'  ''       []       0
    'wind speed'                       'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'x'       'z'   'z'       ''      'map-series'     'WINDU'   'WINDV'  []       0
    'precipitation rate'               'mm/h'   [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'PRECIP'  ''       []       0
    'evaporation rate'                 'mm/h'   [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'EVAP'    ''       []       0
    'evaporation heat flux'            'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'QEVA'    ''       []       0
    'heat flux of forced convection'   'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'QCO'     ''       []       0
    'nett back radiation'              'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'QBL'     ''       []       0
    'nett solar radiation'             'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'QIN'     ''       []       0
    'total nett heat flux'             'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'QNET'    ''       []       0
    'free convection of sensible heat' 'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'HFREE'   ''       []       0
    'free convection of latent heat'   'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'EFREE'   ''       []       0
    'computed minus derived heat flux' 'W/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'QMIS'    ''       []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''                 ''        ''       []       0    
    'fraction high ground'             ''       [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'       'poros'   ''       []       0    
    'high bed level'                   'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'       'dpH'     ''       []       0
    'low bed level'                    'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'       'dpL'     ''       []       0
    'high and low bed levels'          'm'      [1 0 1 1 0]  2         1    'POLYG' 'xy'     ''        'z'   'z'       ''      'map-series'       'dpH'     'dpL'    []       0
    'type of cut cell'                 ''       [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'       'kfs_cc'  ''       []       0
    'cut cell patches'                 ''       [1 0 1 1 0]  0        -1    ''      ''       ''        ''    ''        ''      'map-series'       'INTx_GRS' 'INTy_GRS' []    0
    'cut cell patches with line'       ''       [1 0 1 1 0]  0        -1    ''      ''       ''        ''    ''        ''      'map-series'       'INTx_GRS' 'INTy_GRS' []    0
    'cut cell patches only line'       ''       [1 0 1 1 0]  0        -1    ''      ''       ''        ''    ''        ''      'map-series'       'INTx_GRS' 'INTy_GRS' []    0    
   %'reconstructed bankline'           ''       [1 0 1 1 0]  0        -1    ''      ''       ''        ''    ''        ''      'map-series'       'kfs_cc'  ''       []       0
    'ghost u-point reconstruction'     ''       [1 0 1 1 0]  0        -1    ''      ''       ''        ''    ''        ''      'map-series'       'mGPu1'   ''       []       0
    'ghost v-point reconstruction'     ''       [1 0 1 1 0]  0        -1    ''      ''       ''        ''    ''        ''      'map-series'       'mGPv1'   ''       []       0
    'ghost s-point reconstruction'     ''       [1 0 1 1 0]  0        -1    ''      ''       ''        ''    ''        ''      'map-series'       'mGPs1'   ''       []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'wave height'                      'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-trit-series'  'WAVE_HEIGHT' ''  []       0
    'significant wave height'          'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-rol-series'   'HS'     ''       []       0
    'wave vector'                      'm'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'm'       'z'   'z'       ''      'map-trit-series'  'WAVE_HEIGHT' 'DIR' []     0
    'wave vector'                      'm'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'mr'      'z'   'z'       ''      'map-rol-series'   'HS'     'TETA'   []       0
    'orbital velocity amplitude'       'm/s'    [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-trit-series'  'UORB'   ''       []       0
    'orbital velocity amplitude'       'm/s'    [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-rol-series'   'UORB'   ''       []       0
    'wave period'                      's'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-trit-series'  'PERIOD' ''       []       0
    'peak wave period'                 's'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-rol-series'   'TP' ''       []       0
    'wave length'                      'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-trit-series'  'WAVE_LENGTH' ''  []       0
    'wave length'                      'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-rol-series'   'LAMBDA'  ''  []       0
    'short-wave energy'                'J/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-rol-series'   'EWAVE1'  ''       []       0
    'roller energy'                    'J/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-rol-series'   'EROLL1'  ''       []       0
    'transport velocity of roller energy' ...
                                       'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-rol-series'   'QXKR'    'QYKR'   []       0
    'transport velocity of wave energy' ...
                                       'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-rol-series'   'QXKW'    'QYKW'   []       0
    'wave force'                       'N/m^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-rol-series'   'FXW'     'FYW'    []       0
    'wave force'                       'N/m^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-trit-series'  'WAVE_FORCE_X' 'WAVE_FORCE_Y' []      0
    'roller force'                     'N/m^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-rol-series'   'WSU'     'WSV'    []       0
    'roller force'                     'N/m^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-trit-series'  'ROLLER_FORCE_X' 'ROLLER_FORCE_Y'  [] 0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''                 ''         ''      []       0
    'snow thickness on ice cover'      'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'       'H_SNOW'   ''      []       0
    'area fraction covered by ice'     '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-ice-series'   'ICEFRAC'  ''      []       0
    'area fraction covered by ice'     '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'       'A_ICE'    ''      []       0
    'thickness of ice cover'           'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-ice-series'   'ICETHCK'  ''      []       0
    'thickness of ice cover'           'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'       'H_ICE'    ''      []       0
    'pressure of ice cover'            'Pa'     [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-ice-series'   'ICEPRESS' ''      []       0
    'ice drift velocity'               'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'       'U_ICE'    'V_ICE' []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'water level (when dry: bed level)' 'm'     [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'S1'      ''       []       0
    'water level'                      'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'S1'      ''       []       0
    'water depth'                      'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'S1'      ''       []       0
    'depth averaged velocity'          'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'U1'      'V1'     []       1
    'staggered depth averaged velocities' 'm/s' [1 0 1 1 0]  1         1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'U1'      'V1'     []       2
    'horizontal velocity'              'm/s'    [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'U1'      'V1'     []       1
    'staggered horizontal velocity'    'm/s'    [1 0 1 1 5]  1         1.9  'sQUAD' 'xy'     ''        'd'   'd'       'c'     'map-series'     'U1'      'V1'     []       1
    'velocity'                         'm/s'    [1 0 1 1 1]  1         3    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'U1'      'V1'     []       1
    'vertical velocity'                'm/s'    [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'w'   'z'       'c'     'map-series'     'WPHY'    ''       []       0
    'velocity in depth averaged flow direction' ...
                                       'm/s'    [1 0 1 1 1]  1         1    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'U1'      'V1'     []       0
    'velocity normal to depth averaged flow direction' ...
                                       'm/s'    [1 0 1 1 1]  1         1    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'U1'      'V1'     []       0
    'filtered depth averaged velocity' 'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'UMNLDF'  'VMNLDF' []       1
    'd.a. velocity fluctuations'       'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'UMNLDF'  'VMNLDF' []       1
    'froude number'                    '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'U1'      'V1'     []       0
    'head'                             'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'U1'      'V1'     []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'acceleration (GLM coordinates)'   'm/s^2'  [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_DUDT'        'MOM_DVDT'        []       1
    'acc. due to density'              'm/s^2'  [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_UDENSITY'    'MOM_VDENSITY'    []       1
    'acc. due to flow resistance'      'm/s^2'  [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_URESISTANCE' 'MOM_VRESISTANCE' []       1
    'acc. due to coriolis'             'm/s^2'  [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_UCORIOLIS'   'MOM_VCORIOLIS'   []       1
    'acc. due to viscosity'            'm/s^2'  [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_UVISCO'      'MOM_VVISCO'      []       1
    'acc. due to hydrostatic pressure' 'm/s^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'MOM_UPRESSURE'   'MOM_VPRESSURE'   []       1
    'acc. due to tide gen. forces'     'm/s^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'MOM_UTIDEGEN'    'MOM_VTIDEGEN'    []       1
    'acc. due to wind force (top layer)' 'm/s^2' [1 0 1 1 0] 1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'MOM_UWINDFORCE'  'MOM_VWINDFORCE'  []       1
    'acc. due to bed shear (bottom layer)' 'm/s^2' [1 0 1 1 0] 1       2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'MOM_UBEDSHEAR'   'MOM_VBEDSHEAR'   []       1
    'acc. due to waves'                'm/s^2'  [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_UWAVES'      'MOM_VWAVES'      []       1
    'acc. due to streamw. momentum transp.' 'm/s^2' [1 0 1 1 1] 1      2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_UDUDX'       'MOM_VDVDY'       []       1
    'acc. due to lateral momentum transp.'  'm/s^2' [1 0 1 1 1] 1      2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'MOM_VDUDY'       'MOM_UDVDX'       []       1
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'density'                          'kg/m^3' [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'RHO'     ''       []       0
    'hydrostatic pressure'             'Pa'     [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'RHO'     ''       []       0
    'non-hydrostatic pressure'         'Pa'     [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'HYDPRES' ''       []       0
    'total pressure'                   'Pa'     [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'RHO'     'HYDPRES' []      0
    'relative hydrostatic pressure'    'Pa'     [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'RHO'     ''       []       0
    'relative total pressure'          'Pa'     [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'RHO'     'HYDPRES' []      0
    'concentration'                    'kg/m^3' [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-sedgs-series' 'RZED1' ''       's'      0
    '--constituents'                   ''       [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'R1'      ''       []       0
    '--constituents flux staggered'    ''       [1 0 1 1 1]  1         1.9  'sQUAD' 'xy'     ''        'd'   'd'       'c'     'map-series'     'R1FLX_UU' 'R1FLX_VV' []    2
    '--constituents unit flux'         ''       [1 0 1 1 1]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'R1FLX_UU' 'R1FLX_VV' []    0
    '--constituents cumulative flux staggered' '' [1 0 1 1 1] 1        1.9  'sQUAD' 'xy'     ''        'd'   'd'       'c'     'map-series'     'R1FLX_UUC' 'R1FLX_VVC' []  2
    '--constituents cumulative unit flux'      '' [1 0 1 1 1] 1        2    'sQUAD' 'xy'     'u'       'u'   'z'       'c'     'map-series'     'R1FLX_UUC' 'R1FLX_VVC' []  0
    '--turbquant'                      ''       [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'i'     'map-series'     'RTUR1'   ''       []       0
    'vertical eddy viscosity'          'm^2/s'  [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'i'     'map-series'     'VICWW'   ''       []       0
    'vertical eddy diffusivity'        'm^2/s'  [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'i'     'map-series'     'DICWW'   ''       []       0
    'horizontal viscosity'             'm^2/s'  [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'VICUV'   ''       []       0
    'richardson number'                '-'      [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'i'     'map-series'     'RICH'    ''       []       0
    'vorticity'                        '1/s'    [1 0 1 1 5]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'VORTIC'  ''       []       0
    'enstrophy'                        '1/s^2'  [1 0 1 1 5]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'c'     'map-series'     'ENSTRO'  ''       []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'characteristic velocity'          'm/s'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'z'   'z'       ''      'map-sed-series' 'UUU'     'VVV'    []       1
    'characteristic velocity magnitude' 'm/s'   [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'UMOD'    ''       []       0
    'height above bed for characteristic velocity' ...
                                       'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'ZUMOD'   ''       []       0
    'bed shear velocity magnitude'     'm/s'    [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'USTAR'   ''       []       0
    'settling velocity'                'm/s'    [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        ''    'z'       'i'     'map-sedgs-series' 'WSS'   ''       's1'     0
    'settling velocity'                'm/s'    [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        ''    'z'       'i'     'map-sed-series' 'WS'      ''       's1'     0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'equilibrium concentration'        'kg/m^3' [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        ''    'z'       'c'     'map-sed-series' 'RSEDEQ'  ''       's1'     0
    'bed load transport due to currents (zeta point)' ...
                                       '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'z'   'z'       ''      'map-sed-series' 'SBCU'    'SBCV'   'sb'     1
    'bed load transport due to currents' '*'    [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'SBCUU'   'SBCVV'  'sb'     1
    'staggered bed load transp. due to currents' '*' [1 0 1 1 0] 1     1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-sed-series' 'SBCUU'   'SBCVV'  'sb'     2
    'bed load transport due to waves (zeta point)' ...
                                       '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'z'   'z'       ''      'map-sed-series' 'SBWU'    'SBWV'   'sb'     1
    'bed load transport due to waves'  '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'SBWUU'   'SBWVV'  'sb'     1
    'staggered bed load transp. due to waves' '*' [1 0 1 1 0] 1        1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-sed-series' 'SBWUU'   'SBWVV'  'sb'     2
    'suspended transport due to waves (zeta point)' ...
                                       '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'z'   'z'       ''      'map-sed-series' 'SSWU'    'SSWV'   'sb'     1
    'suspended transport due to waves' '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'SSWUU'   'SSWVV'  'sb'     1
    'staggered suspended transp. due to waves' '*' [1 0 1 1 0] 1       1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-sed-series' 'SSWUU'   'SSWVV'  'sb'     2
    'bed load transport'               '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'SBUU'    'SBVV'   'sb'     1
    'staggered bedload transport'      '*'      [1 0 1 1 0]  1         1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-sed-series' 'SBUU'    'SBVV'   'sb'     2
    'near-bed transport correction'    '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'SUCOR'   'SVCOR'  's'      1
    'staggered near-bed transport correction' '*' [1 0 1 1 0]  1       1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-sed-series' 'SUCOR'   'SVCOR'  's'      2
    'd.a. suspended transport'         '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'SSUU'    'SSVV'   's'      1
    'staggered d.a. suspended transport' '*'    [1 0 1 1 0]  1         1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-sed-series' 'SSUU'    'SSVV'   's'      2
    'total transport'                  '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'SSUU'    'SSVV'   's'      1
    'mean bed load transport'          '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-avg-series' 'SBUUA'   'SBVVA'  'sb'     1
    'mean d.a. suspended transport'    '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-avg-series' 'SSUUA'   'SSVVA'  's'      1
    'mean total transport'             '*'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-avg-series' 'SSUUA'   'SSVVA'  's'      1
    'source term suspended sediment fractions' ...
                                      'kg/m^3/s' [1 0 1 1 0] 1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'SOURSE'  ''       's'      1
    'sink term suspended sediment fractions'   ...
                                       '1/s'    [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'SINKSE'  ''       's'      1
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'near bed reference concentration' 'kg/m^3' [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'RCA'     ''       's'      0
    'bed shear stress'                 'N/m^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-series'     'TAUKSI'  'TAUETA' []       1
    'staggered bed shear stress'       'N/m^2'  [1 0 1 1 0]  1         1.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'TAUKSI'  'TAUETA' []       1
    'maximum bed shear stress'         'N/m^2'  [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-series'     'TAUMAX'  ''       []       0
    'bed shear stress for morphology'  'N/m^2'  [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'TAUB'    ''       []       0
    'excess bed shear ratio'           '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'TAURAT'  ''       'sb1'    0
    'Chezy roughness'                  'm^{1/2}/s' [1 0 1 1 0]  1      0.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'CFUROU'  'CFVROU'  []      0
    'bed roughness'                    ''       [1 0 1 1 0]  1         0.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'ROUMETU' 'ROUMETV' []      0
    'currents related z0 roughness'    'm'      [1 0 1 1 0]  1         0.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'Z0UCUR'  'Z0VCUR' []       0
    'wave enhanced z0 roughness'       'm'      [1 0 1 1 0]  1         0.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-series'     'Z0UROU'  'Z0VROU' []       0
    'initial bed level'                'm'      [0 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'DP0'     ''       []       0
    'bed level in water level points'  'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'd'   'z'       ''      'map-const'      'DP0'     ''       []       0
    'bed level in velocity points'     'm'      [1 0 1 1 0]  1         0.9  'sQUAD' 'xy'     ''        'd'   'd'       ''      'map-const'      'DPU0'    'DPV0'   []       0
    'bed slope'                        '-'      [1 0 1 1 0]  1         2    'sQUAD' 'xy'     'u'       'u'   'z'       ''      'map-sed-series' 'DZDUU'   'DZDVV'  []       1
    'cum. erosion/sedimentation'       'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'd'   'z'       ''      'map-const'      'DP0'     ''       []       0
    'morphological acceleration factor' '-'     [1 0 0 0 0]  0         1    ''      ''       ''        'NA'  ''        ''    'map-infsed-serie' 'MORFAC'  ''       []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'available mass in fluff layer'    'kg/m^2' [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        ''    'z'       ''      'map-sed-series' 'MFLUFF'  ''       's'      0
    'available mass of sediment'       'kg/m^2' [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        ''    'z'       'b'     'map-sed-series' 'MSED'    ''       'sb'     0
    'available mass of sediment'       'kg/m^2' [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        ''    'z'       ''      'map-sed-series' 'BODSED'  ''       'sb'     0
    'available mass of sediment'       'kg/m^2' [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        ''    'z'       ''      'map-mor-series' 'BODSED'  ''       'sb'     0
    'sediment fraction in top layer'   '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'FRAC'    ''       'sb1'    0
    'mud fraction in top layer'        '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'MUDFRAC' ''       []       0
    'sediment fraction'                '-'      [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'b'     'map-sed-series' 'LYRFRAC' ''       'sb1'    0
    'cumulative mass error'            'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'LYRFRAC' ''       []       0
    'Shepard deposit classification'   ''       [1 0 1 1 1]  1         6    'sQUAD' 'xy'     ''        'z'   'z'       'b'     'map-sed-series' 'LYRFRAC' ''       []       0
    'USDA deposit classification'      ''       [1 0 1 1 1]  1         6    'sQUAD' 'xy'     ''        'z'   'z'       'b'     'map-sed-series' 'LYRFRAC' ''       []       0
    'bed porosity'                     '-'      [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'b'     'map-sed-series' 'EPSPOR'  ''       []       0
    'maximum historical load'          'kg/m^2' [1 0 1 1 1]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       'b'     'map-sed-series' 'PRELOAD' ''       []       0
    'arithmetic mean sediment diameter' 'm'     [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'DM'      ''       []       0
    'geometric mean sediment diameter' 'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'DG'      ''       []       0
    'hiding and exposure'              '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'HIDEXP'  ''       'sb1'    0
    'reduction factor due to limited sediment thickness' ...
                                       '-'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'FIXFAC'  ''       'sb1'    0
    'sediment thickness'               'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'DPSED'   ''       []       0
    'sediment thickness'               'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'THLYR'   ''       []       0
    'sediment thickness'               'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'DP_BEDLYR' ''       []       0
    'base level of sediment layer'     'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'DP_BEDLYR' ''       []       0
    'base level of sediment layer'     'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'THLYR'   ''       []       0
    'base level of sediment layer'     'm'      [1 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-sed-series' 'DPSED'   ''       []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0
    'grid cell surface area'           'm^2'    [0 0 1 1 0]  1         1    'sQUAD' 'xy'     ''        'z'   'z'       ''      'map-const'      'XCOR'    ''       []       0
    '-------'                          ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''               ''        ''       []       0};

%============================= AUTODETECTION ==============================
Info=vs_disp(FI,'map-const','XCOR');
nm=Info.SizeDim([1 2]);
Info=vs_disp(FI,'map-const','THICK');
k=Info.SizeDim(1);
SkipGroup={'map-const'};
SkipElem={'THLYR','DP_BEDLYR'};
DataProps=auto_map_detect(FI,DataProps,nm,k,SkipGroup,SkipElem);

% Check whether the number of layers on the output file has been reduced
Info=vs_disp(FI,'map-const','OUTPUT_LAYERS');
if isstruct(Info)
    Info2=vs_disp(FI,'map-const','THICK');
    outputLayers = ~isequal(Info.SizeDim,Info2.SizeDim+1);
else
    outputLayers = 0;
end

%======================== SPECIFIC CODE DIMENSIONS ========================
Info=vs_disp(FI,'map-sed-series','LYRFRAC');
if isstruct(Info)
    id=strmatch('sediment fraction',DataProps(:,1),'exact');
    DataProps{id,1} = sprintf('sediment %s fraction', ...
        lower(strtok(Info.ElmDescription)));
end

Out=cell2struct(DataProps,PropNames,2);
%
if k==1
    Out = removequant(Out, ...
        {'velocity'
        'horizontal velocity'});
elseif outputLayers
    Out = removequant(Out, ...
        {'depth averaged velocity'
        'staggered depth averaged velocities'
        'd.a. velocity fluctuations'
        'froude number'
        'head'
        'hydrostatic pressure'
        'total pressure'
        'relative hydrostatic pressure'
        'relative total pressure'
        'time-aver. depth-aver. velocity'});
    K_=5;
    for i=1:length(Out)
        if Out(i).DimFlag(K_) && any(strcmp(Out(i).Loc3D,{'i','c'}))
            Out(i).DimFlag(K_) = 7;
        end
    end
end
if k==1 || outputLayers
    Out = removequant(Out, ...
        {'velocity in depth averaged flow direction'
        'velocity normal to depth averaged flow direction'});
end

%======================== SPECIFIC CODE CHANGE ============================
Info=vs_disp(FI,'map-sed-series',[]);
if isstruct(Info)
    i=strcmp('bed level in water level points',{Out.Name});
    Out(i).Loc='z';
    Out(i).Group='map-sed-series';
    Out(i).Val1='DPS';
    if ~isstruct(vs_disp(FI,Out(i).Group,Out(i).Val1))
        Out(i).Val1='DPSED';
    end
    i=strcmp('cum. erosion/sedimentation',{Out.Name});
    Out(i).Loc='z';
    Out(i).Group='map-sed-series';
    Out(i).Val1='DPS';
    if ~isstruct(vs_disp(FI,Out(i).Group,Out(i).Val1))
        Out(i).Val1='DPSED';
    end
else
    Out(strcmp('cum. erosion/sedimentation',{Out.Name}))=[];
end

%======================== SPECIFIC CODE REMOVE ============================
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if Out(i).NVal>1
        Info2=vs_disp(FI,Out(i).Group,Out(i).Val2);
    else
        Info2=[];
    end
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info) || any(Info.SizeDim==0)
        % remove references to non-stored data fields
        if strcmp(Out(i).Name,'grid') % S1 not available on file, so convert grid to 2D time-independent quantity.
            Out(i).DimFlag(1) = 0;
            Out(i).DimFlag(5) = 0;
        else
            Out(i)=[];
        end
    elseif Out(i).NVal>1 && Out(i).NVal<4 && ~isstruct(Info2)
        % remove references to non-stored data fields
        Out(i)=[];
    elseif isequal(Info.SizeDim,1) && ~strcmp(Out(i).Loc,'NA')
        % remove references to non-stored data fields
        Out(i)=[];
    elseif strcmp(Out(i).Name,'froude number') || strcmp(Out(i).Name,'head')
        Info2=vs_disp(FI,Out(i).Group,'S1');
        if ~isstruct(Info2)
            Out(i)=[];
        end
    elseif strcmp(Out(i).Name,'hydrostatic pressure') || strcmp(Out(i).Name,'relative hydrostatic pressure')
        Info2=vs_disp(FI,Out(i).Group,'RHO');
        if Info2.SizeDim(3)==1 % not for 2D simulations
            Out(i)=[];
        end
    elseif strcmp(Out(i).Name,'total pressure') || strcmp(Out(i).Name,'relative total pressure')
        Info2=vs_disp(FI,Out(i).Group,'HYDPRES');
        if ~isstruct(Info2)
            Out(i)=[];
        end
    elseif strcmp(Out(i).Name,'energy dissipation')
        % turbulent kinetic energy automatically caught by previous case
        % epsilon only present as second field in RTUR1: RTUR1(:,:,:,2)
        if ~isequal(Info.SizeDim(4),2)
            Out(i)=[];
        end
    elseif strcmp(Out(i).Name,'d.a. velocity fluctuations')
        Info=vs_disp(FI,'map-series','U1');
        if ~isstruct(Info) || length(Info.SizeDim)<3
            Out(i)=[];
        elseif ~isequal(Info.SizeDim(3),1) % only for kmax==1
            Out(i)=[];
        else
            Out(i).Val1='U1';
            Out(i).Val2='V1';
        end
    elseif strcmp(Out(i).Name,'initial bed level') || strcmp(Out(i).Name,'bed level in water level points')
        nfltp=vs_get(FI,'map-const','DRYFLP','quiet!');
        nfltp=lower(deblank(nfltp));
        if isequal(nfltp,'dp')
            Out(i).Loc='z';
            Out(i).ReqLoc='z';
        elseif isequal(qp_option(FI,'dps'),'dp')
            Out(i).Loc='z';
            Out(i).ReqLoc='z';
        end
    elseif strcmp(Out(i).Name,'Shepard deposit classification') || strcmp(Out(i).Name,'USDA deposit classification')
        Props = Out(i);
        Props.SubFld='sb1';
        subf=lower(getsubfields(FI,Props));
        isand = wildstrmatch('*sand*',subf);
        isilt = wildstrmatch('*silt*',subf);
        iclay = wildstrmatch('*clay*',subf);
        if ~all(isand|isilt|iclay)
            Out(i)=[];
        end
    elseif strcmp(Out(i).Name,'bed roughness')
        switch strtok(Info.ElmDescription)
            case 'Chezy'
                Out(i).Name = 'Chezy C roughness';
                Out(i).Units = 'm^{1/2}/s';
            case 'Nikuradse'
                Out(i).Name = 'White-Colebrook/Nikuradse k roughness';
                Out(i).Units = 'm';
            case 'Manning'
                Out(i).Name = 'Manning n roughness';
                Out(i).Units = 's/m^{1/3}';
            case 'Z0'
                Out(i).Name = 'z_0 roughness';
                Out(i).Units = 'm';
        end
    end
end

kcs=vs_get(FI,'map-const','KCS','quiet!');
if all(kcs~=2)
    Out(strcmp('open boundaries',{Out.Name}))=[];
end
if all(kcs~=3)
    Out(strcmp('domain decomposition boundaries',{Out.Name}))=[];
end
%======================== SPECIFIC CODE ADD ===============================
sednames = getsedimentnames(FI,'suspended');
%
names=vs_get(FI,'map-const','NAMCON','quiet!');
names=lower(cellstr(names));
lstci=vs_get(FI,'map-const','LSTCI','quiet!');
if ischar(lstci), lstci=0; end
ltur=vs_get(FI,'map-const','LTUR','quiet!');
if ischar(ltur), ltur=0; end
i=strcmp('concentration',{Out.Name});
if any(i)
    % general sigma
    txt_append = ' (averaged to flow layers)';
    i=find(strcmp('settling velocity',{Out.Name}) & strcmp('map-sed-series',{Out.Group}));
    Out(i).Name = [Out(i).Name txt_append];
else
    txt_append = '';
end
i = find(strcmp('--constituents',{Out.Name}));
if (lstci>0) && ~isempty(i)
    Ins = Out(i*ones(lstci,1));
    for j = 1:lstci
        Ins(j).Name   = [names{j} txt_append];
        Ins(j).SubFld = j;
        Ins(j).Units  = getunit(names{j},sednames);
    end
    Out=insstruct(Out,i,Ins);
end
for c = {{'--constituents flux staggered','m^3/s'}, ...
        {'--constituents unit flux','m^2/s'} ...
        {'--constituents cumulative flux staggered','m^3'}, ...
        {'--constituents cumulative unit flux','m^2'}}
    nm = c{1}{1};
    un = c{1}{2};
    i=find(strcmp(nm,{Out.Name}));
    if (lstci>0) && ~isempty(i)
        Ins=Out(i*ones(lstci,1));
        for j=1:lstci
            Ins(j).Name=[names{j} nm(15:end)];
            Ins(j).SubFld=j;
            Ins(j).Units=getunit(names{j},sednames);
            if ~isempty(Ins(j).Units)
                Ins(j).Units = [Ins(j).Units '*' un];
            end
        end
        Out=insstruct(Out,i,Ins);
    end
end
i=find(strcmp('--turbquant',{Out.Name}));
if (ltur>0) && ~isempty(i)
    Ins=Out(i*ones(ltur,1));
    for j=1:ltur
        Ins(j).Name=names{lstci+j};
        Ins(j).SubFld=j;
        Ins(j).Units=getunit(names{lstci+j},sednames);
    end
    Out=insstruct(Out,i,Ins);
end
for i=1:length(Out)
    if isequal(Out(i).Units,'*')
        Info = vs_disp(FI,Out(i).Group,Out(i).Val1);
        eUnit = strtrim(Info.ElmUnits(2:end-1));
        switch lower(eUnit)
            case {'m3/sm','m3/s/m'}
                eUnit = 'm^3/s/m';
            case 'kg/sm'
                eUnit = 'kg/s/m';
        end
        Out(i).Units = eUnit;
    end
end

%======================= SET USEGRID OPTIONS ==============================
for i=1:length(Out)
    if isequal(Out(i).Geom,'sQUAD')
        switch Out(i).ReqLoc
            case 'd'
                switch Out(i).NVal
                    case {0.9, 1.9}
                        Out(i).UseGrid=3;%1;
                        Out(i).Geom='SGRID-EDGE';
                    otherwise
                        Out(i).UseGrid=3;%1;
                        Out(i).Geom='SGRID-NODE';
                end
            case 'z'
                Out(i).UseGrid=3;%2;
                Out(i).Geom='SGRID-FACE';
        end
    end
    Out(i).Coords='xy';
end

[Out.TemperatureType] = deal('unspecified');
[Out(strcmp({Out.Name},'temperature')).TemperatureType] = deal('absolute');
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
function DL=getlabels(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
DL={[] [] [] [] []};
if Props.DimFlag(K_)==7
    layers = vs_get(FI,'map-const','OUTPUT_LAYERS','quiet!');
    if strcmp(Props.Loc3D,'í')
        DL{K_} = layers;
    else
        DL{K_} = layers(layers~=0)'; %row vector is easier
    end
end
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE ===================================
if Props.DimFlag(M_) && Props.DimFlag(N_)
    Info=vs_disp(FI,'map-const','XCOR');
    sz([N_ M_])=Info.SizeDim;
end
if Props.DimFlag(K_)
    if Props.NVal==0
        Info=vs_disp(FI,'map-const','THICK');
        sz(K_)=Info.SizeDim(1);
        if strcmp(Props.Loc3D,'i')
            sz(K_)=sz(K_)+1;
        end
    else
        switch Props.Loc3D
            case 'c'
                if 0 % dummyMissingLayers
                    Info=vs_disp(FI,'map-const','THICK');
                    sz(K_)=Info.SizeDim;
                else
                    Info=vs_disp(FI,Props.Group,Props.Val1);
                    sz(K_)=Info.SizeDim(3);
                end
            case 'i'
                if 0 % dummyMissingLayers
                    Info=vs_disp(FI,'map-const','THICK');
                    sz(K_)=Info.SizeDim+1;
                else
                    Info=vs_disp(FI,Props.Group,Props.Val1);
                    sz(K_)=Info.SizeDim(3);
                end
            otherwise
                Info=vs_disp(FI,Props.Group,Props.Val1);
                sz(K_)=Info.SizeDim(3);
        end
    end
end
if Props.DimFlag(T_)
    Info=vs_disp(FI,Props.Group,[]);
    sz(T_)=Info.SizeDim;
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function Quantities = quantities(FI)
FI = guarantee_options(FI);
PropNames = {'Name'                                      'Unit'   'ValType'     'Time' 'Stagger'  'SubFld' 'VCoord' 'Group'            'Val1'           'Val2'  };
DataProps = {...
    'x coordinate'                                       'm'      'float'       ''     'Nodes2D'  ''       ''       'map-const'        'XCOR'           ''
    'y coordinate'                                       'm'      'float'       ''     'Nodes2D'  ''       ''       'map-const'        'YCOR'           ''
    'z coordinate'                                       'm'      'level'       'Time' 'HFaces3D' ''       'F'      'map-series'       'S1'             ''
    'domain decomposition boundaries'                    ''       'logical'     ''     'Edges2D'  ''       ''       'map-const'        'KCS'            ''
    'open boundaries'                                    ''       'logical'     ''     'Edges2D'  ''       ''       'map-const'        'KCS'            ''
    'closed boundaries'                                  ''       'logical'     ''     'Edges2D'  ''       ''       'map-const'        'KCS'            ''
    'thin dams'                                          ''       'logical'     ''     'Edges2D'  ''       ''       'map-const'        'KCU'            'KCV'
    'temporarily inactive water level points'            ''       'logical'     'Time' 'Faces2D'  ''       ''       'map-series'       'KFU'            'KFV'
    'temporarily inactive velocity points'               ''       'logical'     'Time' 'Edges2D'  ''       ''       'map-series'       'KFU'            'KFV'
    'parallel partition numbers'                         ''       'float'       ''     'Faces2D'  ''       ''       'map-const'        'PPARTITION'     ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'air pressure'                                       'N/m^2'  'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'PATM'           ''
    'air temperature'                                    '°C'     'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'AIRTEM'         ''
    'cloud coverage'                                     '%'      'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'CLOUDS'         ''
    'relative air humidity'                              '%'      'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'AIRHUM'         ''
    'wind speed'                                         'm/s'    'vector(xy)'  'Time' 'Faces2D'  ''       ''       'map-series'       'WINDU'          'WINDV'
    'precipitation rate'                                 'mm/h'   'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'PRECIP'         ''
    'evaporation rate'                                   'mm/h'   'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'EVAP'           ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'wave height'                                        'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-trit-series'  'WAVE_HEIGHT'    ''
    'significant wave height'                            'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-rol-series'   'HS'             ''
    'wave vector'                                        'm'      'vector(xy)'  'Time' 'Faces2D'  ''       ''       'map-trit-series'  'WAVE_HEIGHT'    'DIR'
    'orbital velocity amplitude'                         'm/s'    'float'       'Time' 'Faces2D'  ''       ''       'map-trit-series'  'UORB'           ''
    'wave period'                                        's'      'float'       'Time' 'Faces2D'  ''       ''       'map-trit-series'  'PERIOD'         ''
    'wave length'                                        'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-trit-series'  'WAVE_LENGTH'    ''
    'short-wave energy'                                  'J/m^2'  'float'       'Time' 'Faces2D'  ''       ''       'map-rol-series'   'EWAVE1'         ''
    'roller energy'                                      'J/m^2'  'float'       'Time' 'Faces2D'  ''       ''       'map-rol-series'   'EROLL1'         ''
    'transport velocity of roller energy'                'm/s'    'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-rol-series'   'QXKR'           'QYKR'
    'transport velocity of wave energy'                  'm/s'    'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-rol-series'   'QXKW'           'QYKW'
    'wave force'                                         'N/m^2'  'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-rol-series'   'FXW'            'FYW'
    'wave force'                                         'N/m^2'  'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-trit-series'  'WAVE_FORCE_X'   'WAVE_FORCE_Y'
    'roller force'                                       'N/m^2'  'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-rol-series'   'WSU'            'WSV'
    'roller force'                                       'N/m^2'  'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-trit-series'  'ROLLER_FORCE_X' 'ROLLER_FORCE_Y'
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'water level (when dry: bed level)'                  'm'      'level'       'Time' 'Faces2D'  ''       ''       'map-series'       'S1'             ''
    'water level'                                        'm'      'level'       'Time' 'Faces2D'  ''       ''       'map-series'       'S1'             ''
    'water depth'                                        'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'S1'             ''
    'depth averaged velocity'                            'm/s'    'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-series'       'U1'             'V1'
    'horizontal velocity'                                'm/s'    'vector(ij)'  'Time' 'VFaces3D' ''       'F'      'map-series'       'U1'             'V1'
    'velocity'                                           'm/s'    'vector(ijk)' 'Time' 'Voxels3D' ''       'F'      'map-series'       'U1'             'V1'
    'vertical velocity'                                  'm/s'    'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'WPHY'           ''
    'velocity in depth averaged flow direction'          'm/s'    'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'U1'             'V1'
    'velocity normal to depth averaged flow direction'   'm/s'    'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'U1'             'V1'
    'filtered depth averaged velocity'                   'm/s'    'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-series'       'UMNLDF'         'VMNLDF'
    'd.a. velocity fluctuations'                         'm/s'    'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-series'       'UMNLDF'         'VMNLDF'
    'froude number'                                      '-'      'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'U1'             'V1'
    'head'                                               'm'      'level'       'Time' 'Faces2D'  ''       ''       'map-series'       'U1'             'V1'
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'density'                                            'kg/m^3' 'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'RHO'            ''
    'hydrostatic pressure'                               'Pa'     'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'RHO'            ''
    'non-hydrostatic pressure'                           'Pa'     'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'HYDPRES'        ''
    '--constituents'                                     ''       'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'R1'             ''
    '--constituents flux'                                ''       'vector(ij)'  'Time' 'VFaces3D' ''       'F'      'map-series'       'R1FLX_UU'       'R1FLX_VV'
    '--constituents cumulative flux'                     ''       'vector(ij)'  'Time' 'VFaces3D' ''       'F'      'map-series'       'R1FLX_UUC'      'R1FLX_VVC'
    '--turbquant'                                        ''       'float'       'Time' 'HFaces3D' ''       'F'      'map-series'       'RTUR1'          ''
    'vertical eddy viscosity'                            'm^2/s'  'float'       'Time' 'HFaces3D' ''       'F'      'map-series'       'VICWW'          ''
    'vertical eddy diffusivity'                          'm^2/s'  'float'       'Time' 'HFaces3D' ''       'F'      'map-series'       'DICWW'          ''
    'horizontal viscosity'                               'm^2/s'  'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'VICUV'          ''
    'richardson number'                                  '-'      'float'       'Time' 'HFaces3D' ''       'F'      'map-series'       'RICH'           ''
    'vorticity'                                          '1/s'    'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'VORTIC'         ''
    'enstrophy'                                          '1/s^2'  'float'       'Time' 'Voxels3D' ''       'F'      'map-series'       'ENSTRO'         ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'characteristic velocity'                            'm/s'    'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-sed-series'   'UUU'            'VVV'
    'characteristic velocity magnitude'                  'm/s'    'float'       'Time' 'Faces2D'  ''       ''       'map-sed-series'   'UMOD'           ''
    'height above bed for characteristic velocity'       'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-sed-series'   'ZUMOD'          ''
    'bed shear velocity magnitude'                       'm/s'    'float'       'Time' 'Faces2D'  ''       ''       'map-sed-series'   'USTAR'          ''
    'settling velocity'                                  'm/s'    'float'       'Time' 'HFaces3D' 's1'     'F'      'map-sed-series'   'WS'             ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'equilibrium concentration'                          'kg/m^3' 'float'       'Time' 'Voxels3D' 's1'     'F'      'map-sed-series'   'RSEDEQ'         ''
    'bed load transport due to currents (zeta point)'    '*'      'vector(ij)'  'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'SBCU'           'SBCV'
    'bed load transport due to currents'                 '*'      'vector(ij)'  'Time' 'Edges2D'  'sb1'    ''       'map-sed-series'   'SBCUU'          'SBCVV'
    'bed load transport due to waves (zeta point)'       '*'      'vector(ij)'  'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'SBWU'           'SBWV'
    'bed load transport due to waves'                    '*'      'vector(ij)'  'Time' 'Edges2D'  'sb1'    ''       'map-sed-series'   'SBWUU'          'SBWVV'
    'suspended transport due to waves (zeta point)'      '*'      'vector(ij)'  'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'SSWU'           'SSWV'
    'suspended transport due to waves'                   '*'      'vector(ij)'  'Time' 'Edges2D'  'sb1'    ''       'map-sed-series'   'SSWUU'          'SSWVV'
    'bed load transport'                                 '*'      'vector(ij)'  'Time' 'Edges2D'  'sb1'    ''       'map-sed-series'   'SBUU'           'SBVV'
    'near-bed transport correction'                      '*'      'vector(ij)'  'Time' 'Edges2D'  's1'     ''       'map-sed-series'   'SUCOR'          'SVCOR'
    'd.a. suspended transport'                           '*'      'vector(ij)'  'Time' 'Edges2D'  's1'     ''       'map-sed-series'   'SSUU'           'SSVV'
    'total transport'                                    '*'      'vector(ij)'  'Time' 'Edges2D'  's1'     ''       'map-sed-series'   'SSUU'           'SSVV'
    'mean bed load transport'                            '*'      'vector(ij)'  'Time' 'Edges2D'  'sb1'    ''       'map-avg-series'   'SBUUA'          'SBVVA'
    'mean d.a. suspended transport'                      '*'      'vector(ij)'  'Time' 'Edges2D'  's1'     ''       'map-avg-series'   'SSUUA'          'SSVVA'
    'mean total transport'                               '*'      'vector(ij)'  'Time' 'Edges2D'  's1'     ''       'map-avg-series'   'SSUUA'          'SSVVA'
    'source term suspended sediment fractions'         'kg/m^3/s' 'float'       'Time' 'Faces2D'  's1'     ''       'map-sed-series'   'SOURSE'         ''
    'sink term suspended sediment fractions'           '1/s'      'float'       'Time' 'Faces2D'  's1'     ''       'map-sed-series'   'SINKSE'         ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'z coordinate gensig'                                'm'      'level'       'Time' 'HFaces3D' ''       'G'      'map-sedgs-series' 'Z_INTERF'       ''
    'concentration'                                      'kg/m^3' 'float'       'Time' 'Voxels3D' 's1'     'G'      'map-sedgs-series' 'RZED1'          ''
    'settling velocity'                                  'm/s'    'float'       'Time' 'HFaces3D' 's1'     'G'      'map-sedgs-series' 'WSS'            ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'near bed reference concentration'                   'kg/m^3' 'float'       'Time' 'Faces2D'  's1'     ''       'map-sed-series'   'RCA'            ''
    'bed shear stress'                                   'N/m^2'  'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-series'       'TAUKSI'         'TAUETA'
    'maximum bed shear stress'                           'N/m^2'  'float'       'Time' 'Faces2D'  ''       ''       'map-series'       'TAUMAX'         ''
    'excess bed shear ratio'                             '-'      'float'       'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'TAURAT'         ''
    'initial bed level'                                  'm'      'level'       ''     'Nodes2D'  ''       ''       'map-const'        'DP0'            ''
    'bed level in cell centers'                          'm'      'level'       'Time' 'Faces2D'  ''       ''       'map-const'        'DP0'            ''
    'bed slope'                                          '-'      'vector(ij)'  'Time' 'Edges2D'  ''       ''       'map-sed-series'   'DZDUU'          'DZDVV'
    'cum. erosion/sedimentation'                         'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-const'        'DP0'            ''
    'morphological acceleration factor'                  '-'      'float'       'Time' ''         ''       ''       'map-infsed-serie' 'MORFAC'         ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'sediment fraction in top layer'                     '-'      'float'       'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'FRAC'           ''
    'mud fraction in top layer'                          '-'      'float'       'Time' 'Faces2D'  ''       ''       'map-sed-series'   'MUDFRAC'        ''
    'cumulative mass error'                              'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-sed-series'   'LYRFRAC'        ''
    'arithmetic mean sediment diameter'                  'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-sed-series'   'DM'             ''
    'geometric mean sediment diameter'                   'm'      'float'       'Time' 'Faces2D'  ''       ''       'map-sed-series'   'DG'             ''
    'hiding and exposure'                                '-'      'float'       'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'HIDEXP'         ''
    'reduction factor due to limited sediment thickness' '-'      'float'       'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'FIXFAC'         ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    'available mass of sediment'                         'kg/m^2' 'float'       'Time' 'Faces2D'  'sb1'    ''       'map-sed-series'   'BODSED'         ''
    'available mass of sediment'                         'kg/m^2' 'float'       'Time' 'Faces2D'  'sb1'    ''       'map-mor-series'   'BODSED'         ''
    'z coordinate bed'                                   'm'      'level'       'Time' 'HFaces3D' ''       'B'      'map-sed-series'   'THLYR'          ''
    'sediment fraction'                                  '-'      'float'       'Time' 'Voxels3D' 'sb1'    'B'      'map-sed-series'   'LYRFRAC'        ''
    'bed porosity'                                       '-'      'float'       'Time' 'Voxels3D' ''       'B'      'map-sed-series'   'EPSPOR'         ''
    'maximum historical load'                            'kg/m^2' 'float'       'Time' 'Voxels3D' ''       'B'      'map-sed-series'   'PRELOAD'        ''
    %   '-------'                                            ''       ''            ''     ''         ''       ''       ''                 ''               ''
    };

% Autodetect any additional 2D or 3D data fields.

Info=vs_disp(FI,'map-const','XCOR');
nm=Info.SizeDim([1 2]);
Info = vs_disp(FI,'map-const','THICK');
k = Info.SizeDim(1);
SkipGroup={'map-const'};
SkipElem={'THLYR','DP_BEDLYR'};
DataProps=auto_map_detect(FI,DataProps,nm,k,SkipGroup,SkipElem);

%======================== SPECIFIC CODE DIMENSIONS ========================
if k==1
    id = strmatch('velocity',DataProps(:,1),'exact');
    DataProps(id,:) = [];
    id = strmatch('horizontal velocity',DataProps(:,1),'exact');
    DataProps(id,:) = [];
    id = strmatch('velocity in depth averaged flow direction',DataProps(:,1),'exact');
    DataProps(id,:) = [];
    id = strmatch('velocity normal to depth averaged flow direction',DataProps(:,1),'exact');
    DataProps(id,:) = [];
end

Out = cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE CHANGE ============================
Info = vs_disp(FI,'map-sed-series',[]);
if isstruct(Info)
    i = strmatch('bed level in cell centers',{Out.Name});
    Out(i).Stagger = 'Faces2D';
    Out(i).Group   = 'map-sed-series';
    Out(i).Val1    = 'DPS';
    if ~isstruct(vs_disp(FI,Out(i).Group,Out(i).Val1))
        Out(i).Val1 = 'DPSED';
    end
    %
    i = strmatch('cum. erosion/sedimentation',{Out.Name});
    Out(i).Stagger = 'Faces2D';
    Out(i).Group   = 'map-sed-series';
    Out(i).Val1    = 'DPS';
    if ~isstruct(vs_disp(FI,Out(i).Group,Out(i).Val1))
        Out(i).Val1 = 'DPSED';
    end
else
    i = strmatch('bed level in cell centers',{Out.Name});
    Out(i).Time = '';
    %
    i = strmatch('cum. erosion/sedimentation',{Out.Name});
    Out(i) = [];
end

%======================== SPECIFIC CODE REMOVE ============================
for i = size(Out,1):-1:1
    Info = vs_disp(FI,Out(i).Group,Out(i).Val1);
    if isempty(Out(i).Val2)
        Info2 = [];
    else
        Info2 = vs_disp(FI,Out(i).Group,Out(i).Val2);
    end
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info) || any(Info.SizeDim==0)
        % remove references to non-stored data fields
        Out(i) = [];
    elseif ~isempty(Out(i).Val2) && ~isstruct(Info2)
        % remove references to non-stored data fields
        Out(i) = [];
    elseif isequal(Info.SizeDim,1) && ~isempty(Out(i).Stagger)
        % remove references to non-stored data fields
        Out(i) = [];
    elseif strcmp(Out(i).Name,'froude number') || strcmp(Out(i).Name,'head')
        Info2 = vs_disp(FI,Out(i).Group,'S1');
        if ~isstruct(Info2)
            Out(i) = [];
        end
    elseif strcmp(Out(i).Name,'energy dissipation')
        % turbulent kinetic energy automatically caught by previous case
        % epsilon only present as second field in RTUR1: RTUR1(:,:,:,2)
        if ~isequal(Info.SizeDim(4),2)
            Out(i) = [];
        end
    elseif strcmp(Out(i).Name,'d.a. velocity fluctuations')
        Info = vs_disp(FI,'map-series','U1');
        if ~isstruct(Info) || length(Info.SizeDim)<3
            Out(i) = [];
        elseif ~isequal(Info.SizeDim(3),1) % only for kmax==1
            Out(i) = [];
        else
            Out(i).Val1 = 'U1';
            Out(i).Val2 = 'V1';
        end
    elseif strcmp(Out(i).Name,'initial bed level') || strcmp(Out(i).Name,'bed level in cell centers')
        nfltp = vs_get(FI,'map-const','DRYFLP','quiet!');
        nfltp = lower(deblank(nfltp));
        if isequal(nfltp,'dp')
            Out(i).Stagger = 'Faces2D';
        elseif isequal(qp_option(FI,'dps'),'dp')
            Out(i).Stagger = 'Faces2D';
        end
    end
end

try
    kcs = vs_get(FI,'map-const','KCS','quiet');
catch
    kcs = [];
end
if all(kcs ~= 2)
    i = strmatch('open boundaries',{Out.Name},'exact');
    Out(i) = [];
end
if all(kcs ~= 3)
    i = strmatch('domain decomposition boundaries',{Out.Name},'exact');
    Out(i) = [];
end
%======================== SPECIFIC CODE ADD ===============================
sednames = getsedimentnames(FI,'suspended');
%
names = vs_get(FI,'map-const','NAMCON','quiet!');
names = lower(cellstr(names));
[lstci,Chk] = vs_get(FI,'map-const','LSTCI','quiet');
if ~Chk
    lstci = 0;
end
[ltur,Chk] = vs_get(FI,'map-const','LTUR','quiet');
if ~Chk
    ltur = 0;
end
i = strmatch('--constituents',{Out.Name},'exact');
if (lstci>0) && ~isempty(i)
    Ins = Out(i*ones(lstci,1));
    for j = 1:lstci
        Ins(j).Name = names{j};
        Ins(j).SubFld = j;
        Ins(j).Unit = getunit(names{j},sednames);
    end
    Out = insstruct(Out,i,Ins);
end
i = strmatch('--constituents flux',{Out.Name},'exact');
if (lstci>0) && ~isempty(i)
    Ins = Out(i*ones(lstci,1));
    for j = 1:lstci
        Ins(j).Name = [names{j} ' flux'];
        Ins(j).SubFld = j;
        Ins(j).Unit = getunit(names{j},sednames);
        if ~isempty(Ins(j).Units)
            Ins(j).Unit = [Ins(j).Units '*m^3/s'];
        end
    end
    Out = insstruct(Out,i,Ins);
end
i = strmatch('--constituents cumulative flux',{Out.Name},'exact');
if (lstci>0) && ~isempty(i)
    Ins = Out(i*ones(lstci,1));
    for j = 1:lstci
        Ins(j).Name = [names{j} ' cumulative flux'];
        Ins(j).SubFld = j;
        Ins(j).Unit = getunit(names{j},sednames);
        if ~isempty(Ins(j).Units)
            Ins(j).Unit = [Ins(j).Units '*m^3'];
        end
    end
    Out = insstruct(Out,i,Ins);
end
i = strmatch('--turbquant',{Out.Name});
if (ltur>0) && ~isempty(i)
    Ins = Out(i*ones(ltur,1));
    for j = 1:ltur
        Ins(j).Name = names{lstci+j};
        Ins(j).SubFld = j;
        Ins(j).Unit = getunit(names{lstci+j},sednames);
    end
    Out = insstruct(Out,i,Ins);
end
for i = 1:length(Out)
    if isequal(Out(i).Unit,'*')
        Info = vs_disp(FI,Out(i).Group,Out(i).Val1);
        eUnit = strtrim(Info.ElmUnits(2:end-1));
        switch lower(eUnit)
            case {'m3/sm','m3/s/m'}
                eUnit = 'm^3/s/m';
            case 'kg/sm'
                eUnit = 'kg/s/m';
        end
        Out(i).Unit = eUnit;
    end
end

Info=vs_disp(FI,'map-const','XZ');
if isstruct(Info) && isequal(Info.ElmUnits,'[  DEG  ]')
    Out(1).Name = 'longitude';
    Out(1).Unit = 'deg';
    Out(2).Name = 'latitude';
    Out(2).Unit = 'deg';
end

for i = 1:length(Out)
    k = Out(i).VCoord;
    if isequal(k,'F')
        k = '';
    end
    switch Out(i).Stagger
        case 'Nodes2D'
            dims = {'MI' 'NI' };
        case 'Faces2D'
            dims = {'M'  'N'  };
        case 'Edges2D'
            dims = {'MI' 'N'  ;'M'  'NI' };
        case 'VEdges3D'
            dims = {'MI' 'NI' ['K' k]  };
        case 'HFaces3D'
            dims = {'M'  'N'  ['K' k 'I']};
        case 'VFaces3D'
            dims = {'MI' 'N'  ['K' k]  ;'M'  'NI' ['K' k]  };
        case 'Voxels3D'
            dims = {'M'  'N'  ['K' k]  };
        otherwise
            dims = cell(1,0);
    end
    if ~isempty(Out(i).Time)
        dims = [repmat({Out(i).Time},size(dims,1),1) dims];
    end
    if ~isempty(Out(i).SubFld)
        dims(:,end+1) = {['SUBF_' Out(i).SubFld]};
    end
    Out(i).Dimensions = dims;
    if isempty(Out(i).Stagger)
        Out(i).Location = '';
    else
        Out(i).Location = '<unnamed location>';
    end
end
Quantities = Out;
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function Dimensions = dimensions(FI)
Props = quantities(FI);
%
Dimensions = [];
t = strmatch('Time',{Props.Time},'exact');
if ~isempty(t)
    t = t(1);
    Dimensions = adddimension(Dimensions,'Time', ...
        'simulation time hydrodynamics', ...
        'discrete-time','matlabdate',readtim(FI,Props(t),0)');
end
%
Info = vs_disp(Props(1).Group,Props(1).Val1);
Dimensions = adddimension(Dimensions,'M', ...
    'index first grid direction','discrete','',1:Info.SizeDim(2));
Dimensions = adddimension(Dimensions,'MI', ...
    'index first grid direction','discrete','',1:Info.SizeDim(2));
%
Dimensions = adddimension(Dimensions,'N', ...
    'index second grid direction','discrete','',1:Info.SizeDim(1));
Dimensions = adddimension(Dimensions,'NI', ...
    'index second grid direction','discrete','',1:Info.SizeDim(1));
%
vcoords = {Props.VCoord};
z = strmatch('G',vcoords,'exact');
if ~isempty(z)
    z = z(1);
    Info = vs_disp(FI,Props(z).Group,Props(z).Val1);
    szk = Info.SizeDim(3);
    switch Props(z).Stagger
        case 'HFaces3D'
            szk = szk-1;
    end
    Dimensions = adddimension(Dimensions,'KG', ...
        'layer index 3D generalized sigma grid','discrete','',1:szk);
    Dimensions = adddimension(Dimensions,'KGI', ...
        'interface layer index 3D generalized sigma grid','discrete','',0:szk);
end
%
z = strmatch('F',vcoords,'exact');
if isequal(Props(z(1)).Name,'z coordinate')
    z(1)=[];
end
if ~isempty(z)
    z = z(1);
    Info = vs_disp(FI,Props(z).Group,Props(z).Val1);
    szk = Info.SizeDim(3);
    switch Props(z).Stagger
        case 'HFaces3D'
            szk = szk-1;
    end
    Dimensions = adddimension(Dimensions,'K', ...
        'layer index 3D flow grid','discrete','',1:szk);
    Dimensions = adddimension(Dimensions,'KI', ...
        'interface layer index 3D flow grid','discrete','',0:szk);
end
%
z = strmatch('B',vcoords,'exact');
if ~isempty(z)
    z = z(1);
    Info = vs_disp(FI,Props(z).Group,Props(z).Val1);
    szk = Info.SizeDim(3);
    if isequal(Props(z(1)).Name,'z coordinate bed')
        szk = szk+1;
    end
    switch Props(z).Stagger
        case 'HFaces3D'
            szk = szk-1;
    end
    Dimensions = adddimension(Dimensions,'KB', ...
        'layer index for bed composition grid','discrete','',1:szk);
    Dimensions = adddimension(Dimensions,'KBI', ...
        'interface layer index for bed composition grid','discrete','',0:szk);
end
%
sfdep = find(~cellfun('isempty',{Props.SubFld}));
subf  = {Props(sfdep).SubFld};
sfdep = sfdep(cellfun('isclass',subf,'char'));
subf  = {Props(sfdep).SubFld};
usubf = unique(subf);
for s = 1:length(usubf)
    ii = strmatch(usubf{s},subf,'exact');
    sfName = sprintf('SUBF_%s',usubf{s});
    Dimensions = adddimension(Dimensions,sfName, ...
        sfName,'discrete','',getsubfields(FI,Props(sfdep(ii(1))),0));
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function Locations = locations(FI)
Props = quantities(FI);
vcoords = {Props.VCoord};
%
Locations = [];
LocDims = {'M' 'MI' 'DanglingLowCell' 'N' 'NI' 'DanglingLowCell'};
XCoord = Props(1);
YCoord = Props(2);
ZCoords = Props(1:0);
%
if ismember('F',vcoords)
    LocDims = [LocDims {'K' 'KI' 'WholeCells'}];
    z = strmatch('z coordinate',{Props.Name},'exact');
    if ~isempty(z)
        ZCoords(end+1) = Props(z);
    end
end
if ismember('G',vcoords)
    LocDims = [LocDims {'KG' 'KGI' 'WholeCells'}];
    z = strmatch('z coordinate gensig',{Props.Name},'exact');
    if ~isempty(z)
        ZCoords(end+1) = Props(z);
    end
end
if ismember('B',vcoords)
    LocDims = [LocDims {'KB' 'KBI' 'WholeCells'}];
    z = strmatch('z coordinate bed',{Props.Name},'exact');
    if ~isempty(z)
        ZCoords(end+1) = Props(z);
    end
end
%
Locations = addlocation(Locations,'<unnamed location>', ...
    'Struct2D+',LocDims,'Time',1,XCoord,YCoord,ZCoords);
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function [Data,NewFI] = getdata(FI,Q,DimSelection)
NewFI = FI;
%
Info=vs_disp(FI,'map-const','ZK');
zlayermodel=0;
if isstruct(Info) && Info.SizeDim>1
    zlayermodel = 1;
end
%
dNames = Q.Dimensions;
DimCell = cell(size(dNames));
for i = 1:numel(DimCell)
    if isfield(DimSelection,dNames{i})
        DimCell{i} = DimSelection.(dNames{i});
    else
        DimCell{i} = 0;
    end
end
%
% split group and element dimensions
%
if isequal(dNames{1},'Time')
    gDims = DimCell(1);
    eDims = DimCell(:,2:end);
    removedim1 = 0;
else
    gDims = {1};
    eDims = DimCell;
    removedim1 = 1;
end
%
% flip M and N dimensions
%
if ~isequal(Q.Name,'morphological acceleration factor')
    eDims(:,1:2) = eDims(:,[2 1]);
end
%
% handle special cases
%
done = false;
switch Q.Name
    case {'depth averaged velocity', ...
            'd.a. velocity fluctuations', ...
            'head',...
            'time-aver. depth-aver. velocity'}
        eDims = {...
            DimSelection.N  DimSelection.MI 0
            DimSelection.NI DimSelection.M  0};
    case 'z coordinate bed'
        zDim = eDims{3};
        eDims{3} = [1 1:max(zDim)-1];
    case {'cumulative erosion/sedimentation', ...
            'cumulative mass error'}
        idx = {gDims{1} [] eDims{1} eDims{2} []};
        [FI,val1] = eros_sed(FI,Q.Name,idx);
        Data = {val1};
        done = true;
    case 'z coordinate'
        [zw,Success] = vs_let(FI,Q.Group,gDims,Q.Val1,eDims(1:2),'quiet');
        if ~Success, error(zw), end
        %
        idx = {gDims{1} [] eDims{1} eDims{2} []};
        zb = readdps(FI,idx);
        if size(zb,1)==1
            zb = repmat(zb,[size(zw,1) 1 1 1]);
        end
        %
        if zlayermodel
            [zk,Success] = vs_let(FI,'map-const','ZK','quiet');
            if ~Success, error(zk), end
            zk(1) = -inf;
            zk(end) = inf;
            zk = zk(eDims{3});
            %
            z = zeros([size(zw) length(zk)]);
            for k = 1:length(zk)
                z(:,:,:,k) = min(zw,max(zb,zk(k)));
            end
        else
            [th,Success] = vs_let(FI,'map-const','THICK','quiet');
            if ~Success, error(th), end
            sg = [0 cumsum(th)];
            sg = sg(eDims{3});
            %
            z = zeros([size(zw) length(sg)]);
            for k = 1:length(sg)
                z(:,:,:,k) = zw-sg(k)*(zw-zb);
            end
        end
        Data = {z};
        done = true;
    case {'water depth','froude number'}
        idx = {gDims{1} [] eDims{1} eDims{2} []};
        zb = readdps(FI,idx);
        %
        [zw,Success] = vs_let(FI,Q.Group,gDims,'S1',eDims(1:2),'quiet');
        if ~Success, error(zw), end
        %
        tb = min(1,1:size(zw,1));
        h = zw-zb(tb,:,:);
        if strcmp(Q.Name,'water depth')
            Data = {h};
            done = true;
        elseif strcmp(Q.Name,'froude number')
            %
            % h will be used further down.
            %
            eDims = {...
                DimSelection.N  DimSelection.MI 0
                DimSelection.NI DimSelection.M  0};
        end
    case {'domain decomposition boundaries','open boundaries','closed boundaries'}
        switch Q.Name
            case 'domain decomposition boundaries'
                bval = 3;
            case 'open boundaries'
                bval = 2;
            case 'closed boundaries'
                bval = 0;
        end
        %
        eInfo = vs_disp(FI,Q.Group,Q.Val1);
        xDims = eDims(2:3);
        xDims{1} = [xDims{1} min(max(xDims{1})+1,eInfo.SizeDim(1))];
        xDims{2} = [xDims{2} min(max(xDims{2})+1,eInfo.SizeDim(2))];
        [kcs,Success] = vs_let(FI,Q.Group,Q.Val1,xDims(1:2),'quiet');
        if ~Success, error(kcs), end
        %
        % A boundary is a transition from internal (1) to boundary type (2
        % for open boundaries, 3 for DD boundaries, 0 for closed
        % boundaries).
        %
        Nu = 2:length(xDims{1})-1;
        Mu = 1:length(xDims{2})-1;
        Nv = 1:length(xDims{1})-1;
        Mv = 2:length(xDims{2})-1;
        val1 = (kcs(:,Nu,Mu)==bval & kcs(:,Nu,Mu+1)==1) | (kcs(:,Nu,Mu)==1 & kcs(:,Nu,Mu+1)==bval);
        val2 = (kcs(:,Nv,Mv)==bval & kcs(:,Nv+1,Mv)==1) | (kcs(:,Nv,Mv)==1 & kcs(:,Nv+1,Mv)==bval);
        Data = {val1 val2};
        done = true;
    case {'temporarily inactive water level points'}
        xNDim = [max(min(eDims{1})-1,1) eDims{1}];
        xMDim = [max(min(eDims{2})-1,1) eDims{2}];
        [kfu,Success] = vs_let(FI,Q.Group,gDims,Q.Val1,{eDims{1} xMDim},'quiet');
        if ~Success, error(kfu), end
        [kfv,Success] = vs_let(FI,Q.Group,gDims,Q.Val2,{xNDim eDims{2}},'quiet');
        if ~Success, error(kfv), end
        %
        N = 1:length(eDims{1});
        M = 1:length(eDims{2});
        val1 = kfu(:,N,M)==0 & kfu(:,N,M+1)==0 & kfv(:,N,M)==0 & kfv(:,N+1,M)==0;
        Data = {val1};
        done = true;
    case {'velocity','velocity in depth averaged flow direction','velocity normal to depth averaged flow direction'}
        xNDim = [max(min(eDims{1})-1,1) eDims{1}];
        xMDim = [max(min(eDims{2})-1,1) eDims{2}];
        [u,Success] = vs_let(FI,Q.Group,gDims,Q.Val1,{eDims{1} xMDim 0},'quiet');
        if ~Success, error(u), end
        [v,Success] = vs_let(FI,Q.Group,gDims,Q.Val2,{xNDim eDims{2} 0},'quiet');
        if ~Success, error(v), end
        %
        N = 1:length(eDims{1});
        M = 1:length(eDims{2});
        u = (u(:,N,M,:)+u(:,N,M+1,:))/2;
        v = (v(:,N,M,:)+v(:,N+1,M,:))/2;
        %
        if strcmp(Q.Name,'velocity')
            [w,Success] = vs_let(FI,Q.Group,gDims,'WPHY',eDims,'quiet');
            if ~Success, error(w), end
            Data = {u v w};
        else
            if zlayermodel
                [zw,Success] = vs_let(FI,Q.Group,gDims,'S1',eDims(1:2),'quiet');
                if ~Success, error(zw), end
                %
                idx = {gDims{1} [] eDims{1} eDims{2} []};
                zb = readdps(FI,idx);
                if size(zb,1)==1
                    zb = repmat(zb,[size(zw,1) 1 1 1]);
                end
                %
                [zk,Success] = vs_let(FI,'map-const','ZK','quiet');
                if ~Success, error(zk), end
                zk(1) = -inf;
                zk(end) = inf;
                %
                u2d = 0;
                v2d = 0;
                z1 = min(zw,max(zb,zk(1)));
                for k = 1:length(zk)-1
                    z2 = min(zw,max(zb,zk(k+1)));
                    dz = z2-z1;
                    u2d = u2d + u(:,:,:,k).*dz;
                    v2d = v2d + v(:,:,:,k).*dz;
                    z1 = z2;
                end
                %division by water depth h is not necessary to determine direction ...
            else
                [th,Success] = vs_let(FI,'map-const','THICK','quiet');
                if ~Success, error(th), end
                %
                u2d = 0;
                v2d = 0;
                for k = 1:length(th)
                    u2d = u2d + u(:,:,:,k)*th(k);
                    v2d = v2d + v(:,:,:,k)*th(k);
                end
            end
            umg = sqrt(u2d.^2+v2d.^2);
            umg(umg==0) = 1;
            u2d = u2d./umg;
            v2d = v2d./umg;
            %
            switch Q.Name
                case 'velocity in depth averaged flow direction'
                    for k = eDims{3}
                        u(:,:,:,k)=u(:,:,:,k).*u2d+v(:,:,:,k).*v2d;
                    end
                case 'velocity normal to depth averaged flow direction'
                    for k = eDims{3}
                        u(:,:,:,k)=u(:,:,:,k).*v2d-v(:,:,:,k).*u2d;
                    end
            end
            u = u(:,:,:,eDims{3});
            Data = {u};
        end
        done = true;
end
%
% get data
%
if ~done
    [Val1,Success] = vs_let(FI,Q.Group,gDims,Q.Val1,eDims(1,:),'quiet');
    if ~Success, error(Val1), end
    if isempty(Q.Val2)
        Data = {Val1};
    else
        e2 = min(2,size(eDims,1));
        [Val2,Success] = vs_let(FI,Q.Group,gDims,Q.Val2,eDims(e2,:),'quiet');
        if ~Success, error(Val2), end
        Data = {Val1 Val2};
    end
end
%
% handle special cases
%
switch Q.Name
    case {'x coordinate','y coordinate'}
        eInfo = vs_disp(FI,Q.Group,Q.Val1);
        xDims = eDims;
        xDims{1} = [xDims{1} min(max(xDims{1})+1,eInfo.SizeDim(1))];
        xDims{2} = [xDims{2} min(max(xDims{2})+1,eInfo.SizeDim(2))];
        [kcs,Success] = vs_let(FI,'map-const','KCS',xDims,'quiet');
        if ~Success, error(kcs), end
        %
        inact = kcs~=1;
        inact = inact(1:end-1,1:end-1) ...
            & inact(2:end,1:end-1) ...
            & inact(1:end-1,2:end) ...
            & inact(2:end,2:end);
        %
        Data{1}(inact) = NaN;
    case 'wave vector'
        [Data{:}]=dir2uv(Data{:});
    case 'z coordinate bed'
        Data{1} = -Data{1};
        %
        idx = {gDims{1} [] eDims{1} eDims{2} []};
        Data{1}(:,:,:,1) = readdps(FI,idx);
        %
        Data{1} = cumsum(Data{1},4);
        Data{1} = Data{1}(:,:,:,zDim);
    case {'depth averaged velocity', ...
            'd.a. velocity fluctuations', ...
            'froude number', ...
            'head',...
            'time-aver. depth-aver. velocity'}
        u = Data{1};
        v = Data{2};
        if zlayermodel
            eInfo = vs_disp(FI,Q.Group,Q.Val1);
            xDims = eDims(2:3);
            xDims{1} = [xDims{1} min(max(xDims{1})+1,eInfo.SizeDim(1))];
            xDims{2} = [xDims{2} min(max(xDims{2})+1,eInfo.SizeDim(2))];
            %
            [zw,Success] = vs_let(FI,Q.Group,gDims,'S1',xDims,'quiet');
            if ~Success, error(zw), end
            %
            idx = {gDims{1} [] xDims{:} []};
            zb = readdps(FI,idx);
            if size(zb,1)==1
                zb = repmat(zb,[size(zw,1) 1 1 1]);
            end
            %
            [zk,Success] = vs_let(FI,'map-const','ZK','quiet');
            if ~Success, error(zk), end
            zk(1) = -inf;
            zk(end) = inf;
            %
            Nu = 1+(min(eDims{1,1})>1):length(xDims{1})-1;
            Mu = 1:length(xDims{2})-1;
            Nv = 1:length(xDims{1})-1;
            Mv = 1+(min(eDims{2,2})>1):length(xDims{2})-1;
            %
            u2d = 0;
            v2d = 0;
            hu = 0;
            hv = 0;
            z1 = min(zw,max(zb,zk(1)));
            for k = 1:length(zk)-1
                z2 = min(zw,max(zb,zk(k+1)));
                %
                % The bed level at the velocity point is the highest of the
                % two. The water level at the velocity points is actually
                % the upwind one (based on depth averaged velocity), but we
                % don't have the depth averaged velocity; that is just the
                % quantity that we want to compute.
                %
                zwu = (z2(:,Nu,Mu)+z2(:,Nu,Mu+1))/2;
                zbu = max(z1(:,Nu,Mu),z1(:,Nu,Mu+1));
                dzu = max(0,zwu-zbu);
                %
                zwv = (z2(:,Nv,Mv)+z2(:,Nv+1,Mv))/2;
                zbv = max(z1(:,Nv,Mv),z1(:,Nv+1,Mv));
                dzv = max(0,zwv-zbv);
                %
                u2d = u2d + u(:,:,:,k).*dzu;
                v2d = v2d + v(:,:,:,k).*dzv;
                hu = hu + dzu;
                hv = hv + dzv;
                %
                z1 = z2;
            end
            hu(hu==0) = 1;
            u2d = u2d./hu;
            hv(hv==0) = 1;
            v2d = v2d./hv;
        else
            [th,Success] = vs_let(FI,'map-const','THICK','quiet');
            if ~Success, error(th), end
            %
            u2d = 0;
            v2d = 0;
            for k = 1:length(th)
                u2d = u2d + u(:,:,:,k)*th(k);
                v2d = v2d + v(:,:,:,k)*th(k);
            end
        end
        %
        [gravity,Success]=vs_let(FI,'map-const','GRAVITY','quiet');
        if ~Success
            gravity = 9.81;
        end
        switch Q.Name
            case {'depth averaged velocity','time-aver. depth-aver. velocity'}
                Data = {u2d v2d};
            case 'd.a. velocity fluctuations'
                [umean,Success]=vs_let(FI,Q.Group,gDims,'UMNLDF',eDims(1,1:2),'quiet');
                if ~Success, error(umean), end
                [vmean,Success]=vs_let(FI,Q.Group,gDims,'VMNLDF',eDims(2,1:2),'quiet');
                if ~Success, error(vmean), end
                Data = {u2d-umean v2d-vmean};
            case 'froude number'
                %froude = sqrt(u.^2+v.^2)/sqrt(gh)
                %
                % h has already been determined before the velocities were
                % read.
                %
                u2d = (u2d(:,:,1:end-1)+u2d(:,:,2:end))/2;
                v2d = (v2d(:,1:end-1,:)+v2d(:,2:end,:))/2;
                u2d = u2d.^2+v2d.^2;
                %
                h = sqrt(u2d./(gravity*max(1e-4,h)));
                Data = {h};
            case 'head'
                %head = zw+(u.^2+v.^2)/(2g);
                u2d = (u2d(:,:,1:end-1)+u2d(:,:,2:end))/2;
                v2d = (v2d(:,1:end-1,:)+v2d(:,2:end,:))/2;
                u2d = u2d.^2+v2d.^2;
                %
                [zw,Success]=vs_let(FI,Q.Group,gDims,'S1',eDims([1 4]),'quiet');
                if ~Success, error(zw), end
                %
                zw = zw + u2d/(2*gravity);
                Data = {zw};
        end
    case {'thin dams','temporarily inactive velocity points'}
        Data{1} = Data{1}==0;
        Data{2} = Data{2}==0;
    case 'total transport'
        [val1,Success]=vs_let(FI,Q.Group,gDims,'SBUU',eDims(1,:),'quiet');
        if ~Success, error(val1), end
        Data{1} = Data{1}+val1;
        [val1,Success]=vs_let(FI,Q.Group,gDims,'SBVV',eDims(2,:),'quiet');
        if ~Success, error(val1), end
        Data{2} = Data{2}+val1;
    case 'mean total transport'
        [val1,Success]=vs_let(FI,Q.Group,gDims,'SBUUA',eDims(1,:),'quiet');
        if ~Success, error(val1), end
        Data{1} = Data{1}+val1;
        [val1,Success]=vs_let(FI,Q.Group,gDims,'SBVVA',eDims(2,:),'quiet');
        if ~Success, error(val1), end
        Data{2} = Data{2}+val1;
end
%
% flip M and N dimensions
%
for i=1:length(Data)
    Data{i} = permute(Data{i},[1 3 2 4:ndims(Data{i})]);
end
%
% remove group dimension if it does not represent time
%
if removedim1
    for i=1:length(Data)
        sData = size(Data{i});
        sData = [sData(2:end) 1];
        Data{i} = reshape(Data{i},sData);
    end
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if ischar(Props.SubFld)
    switch Props.SubFld
        case {'s','sb','s1','sb1'}
            Info=vs_disp(FI,'map-const','NAMSED');
            if isstruct(Info) && ismember(Props.SubFld,{'sb','sb1'})
                names=vs_get(FI,'map-const','NAMSED','quiet!');
                subf=cellstr(names);
            else
                names=vs_get(FI,'map-const','NAMCON','quiet!');
                names=cellstr(names);
                lnames=lower(names);
                i_sed=strmatch('sediment',lnames);
                subf=names(i_sed);
            end
            if length(subf)>1 && Props.SubFld(end)~='1'
                subf{end+1}='sum of all fractions';
            end
    end
end
if nargin>2 && f~=0
    subf=subf(f);
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE ===================================
FI = guarantee_options(FI);
Dt=vs_get(FI,'map-const','DT','quiet!');
Tunit=vs_get(FI,'map-const','TUNIT','quiet!');
Date=vs_get(FI,'map-const','ITDATE','quiet!');
d0=tdelft3d(Date(1),Date(2));
%
TLikeFlow=0;
switch Props.Group
    case {'map-series','map-rol-series','map-sed-series','map-infsed-serie','map-sedgs-series'}
        TLikeFlow=1;
    case 'map-avg-series'
        T=vs_let(FI,'map-infavg-serie',{t},'ITAVGS','quiet!');
        T=d0+T*Dt*Tunit/(24*3600);
    otherwise % ONE FIELD
        Info = vs_disp(FI,Props.Group,[]);
        if Info.SizeDim==1
            T=d0;
        else
            TLikeFlow=1;
        end
end
%
if TLikeFlow
    %
    switch lower(qp_option(FI,'displaytime'))
        case 'hydrodynamic time'
            %
            switch Props.Group
                case 'map-rol-series'
                    T=vs_let(FI,'map-infrol-serie',{t},'ITMAPS','quiet!');
                case {'map-sed-series','map-infsed-serie'}
                    T=vs_let(FI,'map-infsed-serie',{t},'ITMAPS','quiet!');
                otherwise %case {'map-series','map-sedgs-series'} % and other groups
                    T=vs_let(FI,'map-info-series',{t},'ITMAPC','quiet!');
            end
            T=d0+T*Dt*Tunit/(24*3600);
            %
        case 'morphologic time'
            Info=vs_disp(FI,'map-infsed-serie','MORFT');
            if isstruct(Info)
                T=vs_let(FI,'map-infsed-serie',{t},'MORFT','quiet!');
                T=d0+T;
            else
                % The following approach assumes Tstart equal to 0.
                T=vs_let(FI,'map-info-series',{t},'ITMAPC','quiet!');
                T=T*Dt*Tunit/60; % Determine relative time in minutes
                morstt = qp_option(FI,'morstt');
                morfac = qp_option(FI,'morfac');
                if ~isempty(morstt)
                    T=max(0,T-morstt);
                end
                if ~isempty(morfac)
                    T=T*morfac;
                end
                T=d0+T/(24*60);
            end
    end
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function [FI,val1] = eros_sed(FI,Name,idx)
T_=1; ST_=2; M_=3; N_=4; K_=5;

tmpidx=idx;
tmpidx{1}=1;
%
% For efficiency reasons the way in which DPS0 can be determined
% is stored as a field called 'dps0' of the FI. Note that it is
% stored only in a local copy of FI, so, that is less efficient
% than storing it in the main FI. However, because the user may
% change the qp_option dps value of the main FI, storing it
% locally is the only correct thing to do.
%
dps0 = qp_option(FI,'dps0');
if isempty(dps0)
    if strcmp(Name,'cum. erosion/sedimentation')
        dps0=0;
    else
        %
        % In case of cumulative mass error, the bed composition is
        % always based on the first time step. So, also use that bed
        % level otherwise that will introduce a consistent invalid mass
        % error.
        %
        dps0=1;
    end
end
%
% There are two ways for determining the initial bed level. The
% first option is to read the first field of the DPS array on the
% TRIM-file. However, the first bed level on the file may not
% represent the real initial bed level ... so, we cannot rely on
% it. If we do, we may come to incorrect conclusions.
%
if dps0==0 || dps0==1
    dp0a=readdps(FI,tmpidx);
end
%
% The second option is to use the DP0 array and combine it with
% the DRYFLP option. However, in case of DPSOPT equal to MEAN (and
% maybe also in case of MIN) the averaging carried out using
% MATLAB is generally not exactly equal to the DPS values computed
% by Delft3D. This results in minor scattered differences that may
% disturb the user ...
%
if dps0==0 || dps0==2
    dp0b=readdps(FI,tmpidx,1);
end
%
% If the differences are small (within single precision range)
% then accept the first DPS field on the map file. Otherwise, use
% the one obtained from the DP0 array.
%
switch dps0
    case 0
        if all(abs(dp0a(:)-dp0b(:))<=2.4e-7*abs(dp0a(:)) | isnan(dp0b(:)))
            dp0=dp0a;
            FI = qp_option(FI,'dps0',1);
        else
            dp0=dp0b;
            FI = qp_option(FI,'dps0',2);
        end
    case 1
        dp0=dp0a;
    case 2
        dp0=dp0b;
end
%
val1=readdps(FI,idx);
for i=1:size(val1,1)
    val1(i,:)=val1(i,:)-dp0(1,:);
end
if strcmp(Name,'cumulative mass error')
    %
    % This part of the code needs DP0 to be equal to dps(1) !!
    %
    I = vs_disp(FI,'map-sed-series','DP_BEDLYR');
    if isstruct(I)
        [ddzt,Success]=vs_let(FI,'map-sed-series',idx(T_),'DP_BEDLYR',[idx([M_ N_]) {I.SizeDim(3)}],'quiet');
        [ddz1,Success]=vs_let(FI,'map-sed-series',{1},'DP_BEDLYR',[idx([M_ N_]) {I.SizeDim(3)}],'quiet');
    else
        [dz,Success]=vs_let(FI,'map-sed-series',idx(T_),'THLYR',[idx([M_ N_]) {0}],'quiet');
        if ~Success, error(dz), end
        ddzt=sum(dz,4);
        [dz,Success]=vs_let(FI,'map-sed-series',{1},'THLYR',[idx([M_ N_]) {0}],'quiet');
        if ~Success, error(dz), end
        ddz1=sum(dz,4);
    end
    for i=1:size(val1,1)
        val1(i,:)=val1(i,:)+ddz1(1,:)-ddzt(i,:);
    end
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function dp=readdps(FI,idx,DP0)
T_=1; ST_=2; M_=3; N_=4; K_=5;
FI = guarantee_options(FI);
if nargin==2, DP0=0; end
Info=vs_disp(FI,'map-sed-series','DPS');
Info2=vs_disp(FI,'map-sed-series','DPSED');
%
% Do not rely on DPS for the initial bed level since map-sed-series may
% not contain the actual first bed level of the simulation! One may say
% that the latter case could easily be checked by checking whether the
% first time of the map-sed-series group is equal to 0, but that works only
% if tstart equals 0 and, thus, an alternative is still needed.
%
if isstruct(Info) && ~DP0
    %
    % Simple case: read DPS directly
    %
    dp=vs_let(FI,'map-sed-series',idx(T_),'DPS',idx([M_ N_]),'quiet!');
    dp=-dp;
else
    %
    % DPS not available (or initial bed level requested)
    % First step: determine DPS0
    %
    Info=vs_disp(FI,'map-const','DPS0');
    if isstruct(Info)
        %
        % DPS0 available on file
        %
        dp=vs_get(FI,'map-const','DPS0',idx([M_ N_]),'quiet!');
        dp=-dp;
    else
        %
        % DPS0 not available: reconstruct from DP0 and DPSOPT(=DRYFLP)
        %
        dp=vs_get(FI,'map-const','DP0',idx([M_ N_]),'quiet!');
        dp(dp==-999)=NaN;
        nfltp=qp_option(FI,'dps');
        if isempty(nfltp)
            nfltp=vs_get(FI,'map-const','DRYFLP','quiet!');
            nfltp=lower(deblank(nfltp));
        end
        switch nfltp
            case {'mean','mean_dpd','mean_dp'}
                dp=interp2cen(-dp);
            case {'min','min_dpd','min_dp'}
                dp22=dp(2:end,2:end);
                dp22=min(dp(1:end-1,1:end-1),dp22);
                dp22a=dp(2:end,1:end-1);
                dp22a=min(dp(1:end-1,2:end),dp22a);
                dp(2:end,2:end)=(dp22+dp22a)/2; dp=-dp;
            case 'dp'
                dp=-dp; % DP0 is actually DPS !
            otherwise % default {'max','max_dpd','max_dp'}
                dp22=dp(2:end,2:end);
                dp22=max(dp(1:end-1,1:end-1),dp22);
                dp22=max(dp(2:end,1:end-1),dp22);
                dp22=max(dp(1:end-1,2:end),dp22);
                dp(2:end,2:end)=dp22; dp=-dp;
        end
    end
    dp=reshape(dp,[1 size(dp)]);
    %
    % Don't try perform any corrections if the initial bed level is
    % requested. Otherwise, i.e. if DPS really does not exist and DPSED does
    % exist, then we have a very old, rare, TRIM file. In that case use the
    % following approach.
    %
    if isstruct(Info2) && ~DP0
        dp0=dp;
        [dp,Chk]=vs_let(FI,'map-sed-series',idx(T_),'DPSED',idx([M_ N_]),'quiet');
        if Chk
            for i=1:size(dp,1)
                %
                % (1,1) is a dummy waterlevel point, so, (1,1) should always
                % contain the initial DPSED value IF the simulation uses a
                % uniform initial bed composition (not a DEP file). So, this
                % implementation may fail (at least if the option of
                % prescribing a DEP file was possible in those early versions
                % of onlineMOR). The alternative of using the DPSED field of
                % the first time step available from the TRIM file, may also
                % produce an invalid result. There is no procedure that gives a
                % guaranteed correct result! Since we can only get here for old
                % files, don't worry about this problem any further.
                %
                dp(i,:)=dp0(1,:)+dp(i,:)-dp(i,1);
            end
        end
    end
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE ===================================
FI = guarantee_options(FI);
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd,
    case 'initialize'
        OK=optfig(mfig);
        dpstxt=findobj(mfig,'tag','dpstxt');
        dpslst=findobj(mfig,'tag','dpslst');
        dpsops=get(dpslst,'string');
        if isempty(qp_option(FI,'dps'))
            nfltp=vs_get(FI,'map-const','DRYFLP','quiet!');
            nfltp=lower(deblank(nfltp));
            %if isempty(nfltp)
            dpsops = get(dpslst,'string');
            idpsops = strmatch(deblank(lower(nfltp)),dpsops,'exact');
            if isempty(idpsops)
                set(dpslst,'enable','on','backgroundcolor',Active)
                set(dpstxt,'enable','on')
            else
                set(dpslst,'value',idpsops)
            end
        else
            set(dpslst,'value',strmatch(qp_option(FI,'dps'),dpsops),'enable','on','backgroundcolor',Active)
            set(dpstxt,'enable','on')
        end
        Info=vs_disp(FI,'map-series',[]);
        if isstruct(Info)
            Nt=Info.SizeDim;
            if Nt>0
                set(findobj(mfig,'tag','Trestart'),'enable','on');
                set(findobj(mfig,'tag','Wrestart'),'enable','on');
                Ht=findobj(mfig,'tag','Erestart');
                set(Ht,'string',sprintf('%i',Nt),'userdata',Nt,'enable','on','backgroundcolor',Active)
                %
                set(findobj(mfig,'tag','reduce_optxt'),'enable','on')
                set(findobj(mfig,'tag','reduce_operator'),'enable','on','backgroundcolor',Active)
                set(findobj(mfig,'tag','reduce_trim'),'enable','on')
            end
            set(findobj(mfig,'tag','checktstep'),'enable','on');
        end
        Info=vs_disp(FI,'map-sed-series','LYRFRAC');
        if isstruct(Info)
            set(findobj(mfig,'tag','Tdz'),'enable','on');
            set(findobj(mfig,'tag','Edz'),'enable','on','backgroundcolor',Active,'string','Auto','userdata',[]);
            set(findobj(mfig,'tag','map2golder'),'enable','on');
        end
        Info=vs_disp(FI,'map-infsed-serie',[]);
        if isstruct(Info)
            set(findobj(mfig,'tag','displaytime'),'enable','on');
            Hdispt=findobj(mfig,'tag','displaytime=?');
            dispnr=ustrcmpi(qp_option(FI,'displaytime'),get(Hdispt,'string'));
            set(Hdispt,'enable','on','backgroundcolor',Active,'value',dispnr);
            Info=vs_disp(FI,'map-infsed-serie','MORFAC');
            if ~isstruct(Info)
                set(findobj(mfig,'tag','Tmorfac'),'enable','on')
                set(findobj(mfig,'tag','Emorfac'),'enable','on','backgroundcolor',Active,'string',num2str(qp_option(FI,'morfac')))
                set(findobj(mfig,'tag','Tmorstt'),'enable','on')
                set(findobj(mfig,'tag','Emorstt'),'enable','on','backgroundcolor',Active,'string',num2str(qp_option(FI,'morstt')))
                set(findobj(mfig,'tag','Umorstt'),'enable','on')
            end
        end
    case 'writerestart'
        cwd = pwd;
        if isempty(FI.FileName)
            p = fileparts(FI.DatExt);
        else
            p = fileparts(FI.FileName);
        end
        cd(p)
        if matlabversionnumber<6
            filterspec='tri-rst.*';
        else
            filterspec = ...
                {'tri-rst.*' 'restart binary file (tri-rst.*)'
                'trim-*.dat' 'restart map file - NEFIS format (trim-*.dat)'
                'trim-*.nc'  'restart map file - NetCDF format (trim-*.nc)'
                '*.ini' 'initial conditions file (*.ini)'};
        end
        [f,p]=uiputfile(filterspec,'Specify restart file name');
        cd(cwd)
        if ischar(f)
            pf = [p f];
            [p,f,e] = fileparts(pf);
            if isequal(e,'.*')
                e = '';
                pf = fullfile(p,f);
            end
            t=get(findobj(mfig,'tag','Erestart'),'userdata');
            if ustrcmpi('tri-rst',f)>0 || (ustrcmpi('.dat',e)<0 && ustrcmpi('.nc',e)<0 && ~isempty(e))
                if isfield(FI,'NEFIS')
                    for ip = 1:length(FI.NEFIS)
                        pfPart = [pf sprintf('-%3.3i',ip)];
                        trim2rst(FI.NEFIS(ip),t,pfPart);
                    end
                else
                    trim2rst(FI,t,pf);
                end
            elseif ustrcmpi('.nc',e)>0
                if isfield(FI,'NEFIS')
                    for ip = 1:length(FI.NEFIS)
                        pfPart = [fullfile(p,f),sprintf('-%3.3i',ip),e];
                        write_nctrimfile(FI,pfPart,t);
                    end
                else
                    write_nctrimfile(FI,pf,t);
                end
            else
                pf = fullfile(p,f); % get rid of .dat extension
                if isfield(FI,'NEFIS')
                    for ip = 1:length(FI.NEFIS)
                        pfPart = [pf,sprintf('-%3.3i',ip)];
                        write_trimfile(FI,pfPart,t);
                    end
                else
                    write_trimfile(FI,pf,t);
                end
            end
        end
    case 'reduce_trim'
        hOp = findobj(mfig,'tag','reduce_operator');
        Ops = get(hOp,'string');
        if nargin>3
            op = varargin{1};
            iop = ustrcmpi(op,Ops);
            if iop<0
                ui_message('error',['Invalid operator specified in reduce_trim call: ',var2str(op)])
                return
            end
        else
            iop = get(hOp,'value');
        end
        op = Ops{iop};
        %
        if nargin>4
            pf = varargin{2};
            [p,f,e] = fileparts(pf);
        else
            cwd = pwd;
            if isempty(FI.FileName)
                p = fileparts(FI.DatExt);
            else
                p = fileparts(FI.FileName);
            end
            cd(p)
            if matlabversionnumber<6
                filterspec='trim-*.dat';
            else
                filterspec = ...
                    {'trim-*.dat' 'map file (trim-*.dat)'};
            end
            [f,p]=uiputfile(filterspec,'Specify file name');
            cd(cwd)
        end
        if ischar(f)
            [tmp,f,e] = fileparts(f); % get rid of .dat extension
            pf = [p f];
            t=get(findobj(mfig,'tag','Erestart'),'userdata');
            switch op
                case 'compress'
                    if isfield(FI,'NEFIS')
                        for ip = 1:length(FI.NEFIS)
                            pfPart = [pf,sprintf('-%3.3i',ip)];
                            write_trimfile(FI,pfPart,t);
                        end
                    else
                        write_trimfile(FI,pf,t);
                    end
                otherwise
                    if isfield(FI,'NEFIS')
                        for ip = 1:length(FI.NEFIS)
                            pfPart = [pf,sprintf('-%3.3i',ip)];
                            average_trim(FI,pfPart,t,op)
                        end
                    else
                        average_trim(FI,pf,t,op)
                    end
            end
        end
    case 'simsteps'
        t=get(findobj(mfig,'tag','Erestart'),'userdata');
        Txt=simsteps(FI,t);
        d3d_qp refreshfigs
        ui_message('error',Txt);
    case 'dz'
        Hdz=findobj(mfig,'tag','Edz');
        Txt=get(Hdz,'string');
        dz=sscanf(Txt,'%f',1);
        if isempty(dz)
            set(Hdz,'string','Auto','userdata',[])
        else
            set(Hdz,'string',num2str(dz),'userdata',dz)
        end
    case 'map2golder'
        Hdz=findobj(mfig,'tag','Edz');
        dz=get(Hdz,'userdata');
        Ht=findobj(mfig,'tag','Erestart');
        t=get(Ht,'userdata');
        try
            [f,p] = uiputfile('*.txt','Export to Golder');
            if ischar(f)
                Txt = map2golder(FI,t,dz,[p f]);
            end
            ui_message('error',Txt);
        catch
            ui_message('error',lasterr);
        end
    case {'t_select','t_restart'}
        Info=vs_disp(FI,'map-series',[]);
        Nt=Info.SizeDim;
        %
        Ht=findobj(mfig,'tag','Erestart');
        if nargin>3
            t=varargin{1};
            if ischar(t), t=str2vec(t,'range',[1 Nt],'applylimit'); end
        else
            t=str2vec(get(Ht,'string'),'range',[1 Nt],'applylimit');
        end
        t=round(unique(t));
        if isempty(t)
            t=get(Ht,'userdata');
        end
        set(Ht,'string',vec2str(t,'nobrackets'),'userdata',t)
        %
        if length(t)==1
           e = 'on';
           eg = get(findobj(mfig,'tag','Edz'),'enable');
        else
           e = 'off';
           eg = 'off';
        end
        set(findobj(mfig,'tag','Wrestart'),'enable',e)
        set(findobj(mfig,'tag','checktstep'),'enable',e)
        set(findobj(mfig,'tag','map2golder'),'enable',eg)
    case {'morfac','morstt'}
        Hv=findobj(mfig,'tag',['E' cmd]);
        if nargin>3
            v=varargin{1};
            if ischar(v)
                v=str2double(v);
            end
        else
            v=str2double(get(Hv,'string'));
        end
        if isnan(v)
            v=get(Hv,'userdata');
        end
        NewFI = qp_option(NewFI,cmd,v);
        set(Hv,'string',sprintf('%g',v),'userdata',v)
    case 'dps'
        dpslst=findobj(mfig,'tag','dpslst');
        dpsops=get(dpslst,'string');
        if nargin>3
            dps=varargin{1};
            dps=strmatch(lower(dps),dpsops,'exact');
            if isempty(dps), dps=1; end
            set(dpslst,'value',dps)
        else
            dps=get(dpslst,'value');
        end
        dpsname=dpsops{dps};
        NewFI = qp_option(NewFI,'dps',dpsname);
        cmdargs={cmd dpsname};
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
        NewFI = qp_option(NewFI,'displaytime',dispstr);
        cmdargs={cmd dispstr};
    otherwise
        error(['Unknown option command: ',cmd])
end
% -------------------------------------------------------------------------

function [FORMAT, shuffle, deflateLevel, chunk2D] = get_netcdfsettings(cmd)
if nargin==0
    FORMAT = '64BIT_OFFSET'; % 'NC_NETCDF4'; %
    shuffle = false;
    deflateLevel = 3;
    chunk2D = 20;
    %
    %H = qp_uifigure('xx','','ncsettings',[100 100 100 100],@get_netcdfsettings);
else
    %switch cmd
    %    case ....
    %end
end
% get export settings using H=qp_uifigure(Name,closecom,tag,pos,callbackfcn)

function write_nctrimfile(FI,pf,t)
[FORMAT, shuffle, deflateLevel, chunk2D] = get_netcdfsettings;
%
% Get the data
%
MC = vs_get(FI,'map-const','*','quiet!');
MIT = vs_get(FI,'map-info-series',{t},'*','quiet!');
MT = vs_get(FI,'map-series',{t},'*','quiet!');
MIST = vs_get(FI,'map-infsed-serie',{t},'*','quiet!');
MST = vs_get(FI,'map-sed-series',{t},'*','quiet!');
MRT = vs_get(FI,'map-rol-series',{t},'*','quiet!');
if isfield(MST,'MSED')
    nlyr = size(MST.MSED,3);
    lsedtot = size(MST.MSED,4);
elseif isfield(MST,'LYRFRAC')
    nlyr = size(MST.LYRFRAC,3);
    lsedtot = size(MST.LYRFRAC,4);
elseif isfield(MST,'BODSED')
    nlyr = 1;
    lsedtot = size(MST.BODSED,3);
else
    nlyr = 0;
    lsedtot = 0;
end
if isfield(MST,'MFLUFF')
    lsed = size(MST.MFLUFF,3);
else
    lsed = 0;
end
%
mmax = MC.MMAX;
nmax = MC.NMAX;
kmax = MC.KMAX;
lstsci = MC.LSTCI;
ltur = MC.LTUR;
if kmax>0
    if isfield(MC,'LAYER_MODEL')
        layer_model = MC.LAYER_MODEL;
        zmodel = layer_model(1)=='Z';
    else
        layer_model = 'SIGMA-MODEL';
        zmodel = 0;
    end
end
time = MIT.ITMAPC * MC.DT * MC.TUNIT;
%
% Create the new file and define dimensions and variables.
%
CLOBBER = netcdf.getConstant('CLOBBER');
UNLIMITED = netcdf.getConstant('UNLIMITED');
GLOBAL = netcdf.getConstant('GLOBAL');
PREC = 'double';
%
mode = bitor(CLOBBER, netcdf.getConstant(FORMAT));
ncid = netcdf.create(pf,mode);
if ~strcmp(FORMAT,'NC_NETCDF4')
    shuffle = false;
    deflateLevel = 0;
    chunk2D = [];
else
    if length(chunk2D)==1
        chunk2D = chunk2D*[1 1];
    end
    chunk2D = min(chunk2D,[nmax mmax]);
end
%
dim_t  = netcdf.defDim(ncid, 'time', UNLIMITED);
dim_m  = netcdf.defDim(ncid, 'M', mmax); % center
dim_mc = netcdf.defDim(ncid, 'MC', mmax); % corner
dim_n  = netcdf.defDim(ncid, 'N', nmax); % center
dim_nc = netcdf.defDim(ncid, 'NC', nmax); % corner
if kmax>0
    if zmodel
        dim_k  = netcdf.defDim(ncid, 'K_LYR', kmax);
        dim_k1 = netcdf.defDim(ncid, 'K_INTF', kmax+1);
    else
        dim_k  = netcdf.defDim(ncid, 'SIG_LYR', kmax);
        dim_k1 = netcdf.defDim(ncid, 'SIG_INTF', kmax+1);
    end
end
if lstsci>0
    dim_lstsci = netcdf.defDim(ncid, 'LSTSCI', lstsci);
    dim_20 = netcdf.defDim(ncid, 'strlen20', 20);
end
if ltur>0
    dim_ltur   = netcdf.defDim(ncid, 'LTUR'  , ltur  );
end
if lsedtot>0
    dim_lsedtot = netcdf.defDim(ncid, 'LSEDTOT', lsedtot);
end
if lsed>0
    dim_lsed = netcdf.defDim(ncid, 'LSED', lsed);
end
if nlyr>0
    dim_nlyr  = netcdf.defDim(ncid, 'nlyr', nlyr);
    dim_nlyr1 = netcdf.defDim(ncid, 'nlyrp1', nlyr+1);
end
%
if isempty(chunk2D)
    chunk3D               = [];
    chunk3D_lstsci        = [];
    chunk3Di_ltur         = [];
    chunk2D_nlyr_lsedtot  = [];
    chunk2D_lsedtot       = [];
    chunk2D_nlyrp1        = [];
    chunk2D_nlyr          = [];
    chunk2D_lsed          = [];
else
    chunk3D               = [chunk2D kmax 1];
    chunk3D_lstsci        = [chunk2D kmax lstsci 1];
    chunk3Di_ltur         = [chunk2D kmax+1 ltur 1];
    chunk2D_nlyr_lsedtot  = [chunk2D nlyr lsedtot 1];
    chunk2D_lsedtot       = [chunk2D lsedtot 1];
    chunk2D_nlyrp1        = [chunk2D nlyr+1 1];
    chunk2D_nlyr          = [chunk2D nlyr 1];
    chunk2D_lsed          = [chunk2D lsed 1];
    chunk2D               = [chunk2D 1];
end
%
var_t = netcdf.defVar(ncid,'time','double',dim_t);
Y = floor(MC.ITDATE(1)/10000);
M = floor(MC.ITDATE(1)/100-100*Y);
D = floor(MC.ITDATE(1)-10000*Y-100*M);
netcdf.putAtt(ncid,var_t,'units',sprintf('seconds since %i-%0.2i-%0.2i 00:00:00',Y,M,D));
var_s1 = netcdf_defVar(ncid,'S1',PREC,[dim_n dim_m dim_t],false,deflateLevel,chunk2D);
var_u1 = netcdf_defVar(ncid,'U1',PREC,[dim_n dim_mc dim_k dim_t],false,deflateLevel,chunk3D);
var_v1 = netcdf_defVar(ncid,'V1',PREC,[dim_nc dim_m dim_k dim_t],false,deflateLevel,chunk3D);
if lstsci>0 && isfield(MT,'R1')
    var_nc = netcdf.defVar(ncid,'NAMCON','char',[dim_20 dim_lstsci]);
    var_r1 = netcdf_defVar(ncid,'R1',PREC,[dim_n dim_m dim_k dim_lstsci dim_t],false,deflateLevel,chunk3D_lstsci);
end
if ltur>0 && isfield(MT,'RTUR1')
    var_rtur1 = netcdf_defVar(ncid,'RTUR1',PREC,[dim_n dim_m dim_k1 dim_ltur dim_t],false,deflateLevel,chunk3Di_ltur);
end
var_ku = netcdf_defVar(ncid,'KFU','NC_INT',[dim_n dim_mc dim_t],shuffle,deflateLevel,chunk2D);
var_kv = netcdf_defVar(ncid,'KFV','NC_INT',[dim_nc dim_m dim_t],shuffle,deflateLevel,chunk2D);
if isfield(MT,'UMNLDF')
    var_um = netcdf_defVar(ncid,'UMNLDF',PREC,[dim_n dim_mc dim_t],false,deflateLevel,chunk2D);
    var_vm = netcdf_defVar(ncid,'VMNLDF',PREC,[dim_nc dim_m dim_t],false,deflateLevel,chunk2D);
end
%
if isfield(MIST,'MORFT')
    var_mt = netcdf.defVar(ncid,'MORFT','double',dim_t);
end
if isfield(MST,'DPS')
    var_dps = netcdf_defVar(ncid,'DPS',PREC,[dim_n dim_m dim_t],false,deflateLevel,chunk2D);
end
if isfield(MST,'MSED')
    var_msed = netcdf_defVar(ncid,'MSED',PREC,[dim_n dim_m dim_nlyr dim_lsedtot dim_t],false,deflateLevel,chunk2D_nlyr_lsedtot);
elseif isfield(MST,'LYRFRAC')
    var_lyrfrac = netcdf_defVar(ncid,'LYRFRAC',PREC,[dim_n dim_m dim_nlyr dim_lsedtot dim_t],false,deflateLevel,chunk2D_nlyr_lsedtot);
elseif isfield(MST,'BODSED')
    var_bodsed = netcdf_defVar(ncid,'BODSED',PREC,[dim_n dim_m dim_lsedtot dim_t],false,deflateLevel,chunk2D_lsedtot);
end
if isfield(MST,'DP_BEDLYR')
    var_dpb = netcdf_defVar(ncid,'DP_BEDLYR',PREC,[dim_n dim_m dim_nlyr1 dim_t],false,deflateLevel,chunk2D_nlyrp1);
elseif isfield(MST,'THLYR')
    var_thlyr = netcdf_defVar(ncid,'THLYR',PREC,[dim_n dim_m dim_nlyr dim_t],false,deflateLevel,chunk2D_nlyr);
end
if isfield(MST,'MFLUFF')
    var_mfl = netcdf_defVar(ncid,'MFLUFF',PREC,[dim_n dim_m dim_lsed dim_t],false,deflateLevel,chunk2D_lsed);
end
if isfield(MST,'DUNEHEIGHT')
    var_dh = netcdf_defVar(ncid,'DUNEHEIGHT',PREC,[dim_n dim_m dim_t],false,deflateLevel,chunk2D);
    var_dl = netcdf_defVar(ncid,'DUNELENGTH',PREC,[dim_n dim_m dim_t],false,deflateLevel,chunk2D);
end
%
if isfield(MRT,'HS')
    var_hs = netcdf_defVar(ncid,'HS',PREC,[dim_n dim_m dim_t],false,deflateLevel,chunk2D);
end
if isfield(MRT,'EWAVE1')
    var_ew = netcdf_defVar(ncid,'EWAVE1',PREC,[dim_n dim_m dim_t],false,deflateLevel,chunk2D);
    var_er = netcdf_defVar(ncid,'EROLL1',PREC,[dim_n dim_m dim_t],false,deflateLevel,chunk2D);
    var_qxkr = netcdf_defVar(ncid,'QXKR',PREC,[dim_n dim_mc dim_t],false,deflateLevel,chunk2D);
    var_qykr = netcdf_defVar(ncid,'QYKR',PREC,[dim_nc dim_m dim_t],false,deflateLevel,chunk2D);
    var_qxkw = netcdf_defVar(ncid,'QXKW',PREC,[dim_n dim_mc dim_t],false,deflateLevel,chunk2D);
    var_qykw = netcdf_defVar(ncid,'QYKW',PREC,[dim_nc dim_m dim_t],false,deflateLevel,chunk2D);
end
if isfield(MRT,'FXW')
    var_fxw = netcdf_defVar(ncid,'FXW',PREC,[dim_n dim_mc dim_t],false,deflateLevel,chunk2D);
    var_fyw = netcdf_defVar(ncid,'FYW',PREC,[dim_nc dim_m dim_t],false,deflateLevel,chunk2D);
    var_wsu = netcdf_defVar(ncid,'WSU',PREC,[dim_n dim_mc dim_t],false,deflateLevel,chunk2D);
    var_wsv = netcdf_defVar(ncid,'WSV',PREC,[dim_nc dim_m dim_t],false,deflateLevel,chunk2D);
end
%
netcdf.putAtt(ncid,GLOBAL,'LAYER_MODEL',layer_model);
netcdf.endDef(ncid);
%
% End of definition, now write the data.
%
netcdf.putVar(ncid,var_t,0,time);
netcdf.putVar(ncid,var_s1,[0 0 0],[nmax mmax 1],MT.S1);
netcdf.putVar(ncid,var_u1,[0 0 0 0],[nmax mmax kmax 1],MT.U1);
netcdf.putVar(ncid,var_v1,[0 0 0 0],[nmax mmax kmax 1],MT.V1);
if lstsci>0 && isfield(MT,'R1')
    netcdf.putVar(ncid,var_nc,[0 0],[20 lstsci],MC.NAMCON(:,1:lstsci));
    netcdf.putVar(ncid,var_r1,[0 0 0 0 0],[nmax mmax kmax lstsci 1],MT.R1);
end
if ltur>0 && isfield(MT,'RTUR1')
    netcdf.putVar(ncid,var_rtur1,[0 0 0 0 0],[nmax mmax kmax+1 ltur 1],MT.RTUR1);
end
netcdf.putVar(ncid,var_ku,[0 0 0],[nmax mmax 1],MT.KFU);
netcdf.putVar(ncid,var_kv,[0 0 0],[nmax mmax 1],MT.KFV);
if isfield(MT,'UMNLDF')
    netcdf.putVar(ncid,var_um,[0 0 0],[nmax mmax 1],MT.UMNLDF);
    netcdf.putVar(ncid,var_vm,[0 0 0],[nmax mmax 1],MT.VMNLDF);
end
%
if isfield(MIST,'MORFT')
    netcdf.putVar(ncid,var_mt,0,1,MIST.MORFT);
end
if isfield(MST,'DPS')
    netcdf.putVar(ncid,var_dps,[0 0 0],[nmax mmax 1],MST.DPS);
end
if isfield(MST,'MSED')
    netcdf.putVar(ncid,var_msed,[0 0 0 0 0],[nmax mmax nlyr lsedtot 1],MST.MSED);
elseif isfield(MST,'LYRFRAC')
    netcdf.putVar(ncid,var_lyrfrac,[0 0 0 0 0],[nmax mmax nlyr lsedtot 1],MST.LYRFRAC);
elseif isfield(MST,'BODSED')
    netcdf.putVar(ncid,var_bodsed,[0 0 0 0],[nmax mmax lsedtot 1],MST.BODSED);
end
if isfield(MST,'DP_BEDLYR')
    netcdf.putVar(ncid,var_dpb,[0 0 0 0],[nmax mmax nlyr+1 1],MST.DP_BEDLYR);
elseif isfield(MST,'THLYR')
    netcdf.putVar(ncid,var_thlyr,[0 0 0 0],[nmax mmax nlyr 1],MST.THLYR);
end
if isfield(MST,'MFLUFF')
    netcdf.putVar(ncid,var_mfl,[0 0 0 0],[nmax mmax lsed 1],MST.MFLUFF);
end
if isfield(MST,'DUNEHEIGHT')
    netcdf.putVar(ncid,var_dh,[0 0 0],[nmax mmax 1],MST.DUNEHEIGHT);
    netcdf.putVar(ncid,var_dl,[0 0 0],[nmax mmax 1],MST.DUNELENGTH);
end
%
if isfield(MRT,'HS')
    netcdf.putVar(ncid,var_hs,[0 0 0],[nmax mmax 1],MRT.HS);
end
if isfield(MRT,'EWAVE1')
    netcdf.putVar(ncid,var_ew,[0 0 0],[nmax mmax 1],MRT.HS);
    netcdf.putVar(ncid,var_er,[0 0 0],[nmax mmax 1],MRT.HS);
    netcdf.putVar(ncid,var_qxkr,[0 0 0],[nmax mmax 1],MRT.QXKR);
    netcdf.putVar(ncid,var_qykr,[0 0 0],[nmax mmax 1],MRT.QYKR);
    netcdf.putVar(ncid,var_qxkw,[0 0 0],[nmax mmax 1],MRT.QXKW);
    netcdf.putVar(ncid,var_qykw,[0 0 0],[nmax mmax 1],MRT.QYKW);
end
if isfield(MRT,'FXW')
    netcdf.putVar(ncid,var_fxw,[0 0 0],[nmax mmax 1],MRT.FXW);
    netcdf.putVar(ncid,var_fyw,[0 0 0],[nmax mmax 1],MRT.FYW);
    netcdf.putVar(ncid,var_wsu,[0 0 0],[nmax mmax 1],MRT.WSU);
    netcdf.putVar(ncid,var_wsv,[0 0 0],[nmax mmax 1],MRT.WSV);
end
netcdf.close(ncid);

function varid = netcdf_defVar(ncid,varname,xtype,dimids,shuffle,deflateLevel,chunkDims)
varid = netcdf.defVar(ncid,varname,xtype,dimids);
if shuffle || deflateLevel>0
    netcdf.defVarDeflate(ncid,varid,shuffle,deflateLevel>0,deflateLevel)
end
if ~isempty(chunkDims)
    netcdf.defVarChunking(ncid,varid,'CHUNKED',chunkDims);
end

function write_trimfile(FI,pf,t)
grps = {'map-series','map-info-series', ...
    'map-sed-series','map-infsed-serie','map-sedgs-series', ...
    'map-rol-series','map-infrol-serie', ...
    'map-trit-series','map-inftri-serie'};
%
% The average group for sediment transport rates may be time-varying or not.
%
agrps = {'map-avg-series' 'map-infavg-serie'};
Info_agrps = vs_disp(FI,agrps{1},[]);
if ~isstruct(Info_agrps)
    agrps = {};
elseif Info_agrps.SizeDim>1
    grps = [grps agrps];
    agrps = {};
end
%
% Remove any groups that are not in the file.
%
grps(2,:) = {{t}};
for i=size(grps,2):-1:1
    if ~isstruct(vs_disp(FI,grps{1,i},[]))
        grps(:,i) = [];
    end
end
%
% Create the new file.
%
NFS2 = vs_ini([pf '.dat'],[pf '.def']);
vs_copy(FI,NFS2,'*',[],'map-const','map-version',grps{:},agrps{:},'progressbar');

% -------------------------------------------------------------------------
function OK=optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset 70 18], ...
    'String','Dpsopt', ...
    'Enable','off', ...
    'Tag','dpstxt');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions dps', ...
    'Position',[91 voffset 80 20], ...
    'String',{'max','min','mean','dp'}, ...
    'Value',1, ...
    'Enable','off', ...
    'Tag','dpslst');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset 70 18], ...
    'String','Time Index', ...
    'Enable','off', ...
    'Tag','Trestart');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','right', ...
    'Callback','d3d_qp fileoptions t_select', ...
    'Position',[91 voffset 80 20], ...
    'String','', ...
    'Value',0, ...
    'Enable','off', ...
    'Tag','Erestart');
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions writerestart', ...
    'Position',[181 voffset 150 20], ...
    'String','Write Restart File', ...
    'Enable','off', ...
    'Tag','Wrestart');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions simsteps', ...
    'Position',[181 voffset 150 20], ...
    'String','Check Time Step', ...
    'Enable','off', ...
    'Tag','checktstep');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[91 voffset 30 18], ...
    'String','Dz', ...
    'Enable','off', ...
    'Tag','Tdz');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','right', ...
    'Callback','d3d_qp fileoptions dz', ...
    'Position',[131 voffset 40 20], ...
    'String','', ...
    'Value',0, ...
    'Enable','off', ...
    'Tag','Edz');
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions map2golder', ...
    'Position',[181 voffset 150 20], ...
    'String','Export to Golder', ...
    'Enable','off', ...
    'Tag','map2golder');
voffset=voffset-30;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset 70 18], ...
    'String','Operator', ...
    'Enable','off', ...
    'Tag','reduce_optxt');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','right', ...
    'Position',[91 voffset 80 20], ...
    'String',{'compress','min','mean','max','std','median'}, ...
    'Value',1, ...
    'Enable','off', ...
    'Tag','reduce_operator');
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions reduce_trim', ...
    'Position',[181 voffset 150 20], ...
    'String','Write Reduced Trim File', ...
    'Enable','off', ...
    'Tag','reduce_trim');
voffset=voffset-30;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 70 18], ...
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
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset 160 18], ...
    'String','Morphological Scale Factor', ...
    'Enable','off', ...
    'Tag','Tmorfac');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','right', ...
    'Callback','d3d_qp fileoptions morfac', ...
    'Position',[181 voffset 80 20], ...
    'String','Automatic', ...
    'Value',1, ...
    'Enable','off', ...
    'Tag','Emorfac');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset 160 18], ...
    'String','Spin-up Interval Morphology', ...
    'Enable','off', ...
    'Tag','Tmorstt');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','right', ...
    'Callback','d3d_qp fileoptions morstt', ...
    'Position',[181 voffset 80 20], ...
    'String','Automatic', ...
    'Value',0, ...
    'Enable','off', ...
    'Tag','Emorstt');
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[271 voffset 80 18], ...
    'String','min', ...
    'Enable','off', ...
    'Tag','Umorstt');
OK=1;
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function unit = getunit(par,sednames)
switch lower(par)
    case 'temperature'
        unit = '°C';
    case 'salinity'
        unit = 'ppt';
    case 'secondary flow'
        unit = 'm/s';
    case 'turbulent energy' % turbulent kinetic energy
        unit = 'm^2/s^2';
    case 'energy dissipation'
        unit = 'm^2/s^3';
    case sednames
        unit = 'kg/m^3';
    otherwise
        unit = '';
end
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
function sediments = getsedimentnames(FI,typ)
if nargin<2
    typ='all';
end
%
names = vs_get(FI,'map-const','NAMCON','quiet!');
constituents = lower(cellstr(names));
%
Info = vs_disp(FI,'map-const','NAMSED');
if isstruct(Info)
    names = vs_get(FI,'map-const','NAMSED','quiet!');
    sediments = lower(cellstr(names));
else
    sediments = constituents(strncmpi('sediment',constituents,8));
end
%
switch typ
    case 'all'
    case 'suspended'
        sediments = intersect(sediments,constituents);
    case 'bedload'
        sediments = setdiff(sediments,constituents);
end
% -------------------------------------------------------------------------


function FI = guarantee_options(FI)
defopt = {'morfac' 1
    'morstt' 0
    'dps' ''
    'displaytime' 'hydrodynamic time'};
for i = 1:size(defopt,1)
    opt = defopt{i,1};
    val = defopt{i,2};
    if isequal(qp_option(FI,opt),[])
        FI = qp_option(FI,opt,val);
    end
end
% -------------------------------------------------------------------------

function [class,classes] = classify_Sediment(sand,silt,clay,method)
% classify_Sediment determines the sediment type according to the USDA
% classification in 12 major classes or Shepard's classification in 10 
% classes(see below)
%
%   Input: sand/silt/clay fraction (not %), each a matrix with the same
%   dimensions
%
%   Output: class       
%
%   Example: 
%     class = classify_Sediment(0.6,0.3,0.1,'Shepard'); %determines the class for a
%     sediment sample with 60% sand, 30% silt and 10% clay, returning '3'
%     (=SANDY LOAM)
%
% USDA classification in 12 major classes:
% SAND = 1: 
% LOAMY SAND = 2: 
% SANDY LOAM = 3:
% LOAM = 4:
% SILT LOAM = 5:
% SILT = 6:
% SANDY CLAY LOAM = 7:
% CLAY LOAM = 8:
% SILTY CLAY LOAM = 9:
% SANDY CLAY = 10:
% SILTY CLAY = 11:
% CLAY = 12:
% See also: http://soils.usda.gov/technical/aids/investigations/texture/
% See also: http://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/?cid=nrcs142p2_054167

% Shepard's classification: 
% SAND = 1
% SILTY SAND = 2
% CLAYEY SAND = 3
% SILT = 4
% SANDY SILT = 5
% CLAYEY SILT = 6
% CLAY = 7
% SANDY CLAY = 8
% SILTY CLAY = 9
% SAND-SILT-CLAY = 10
% See also: http://pubs.usgs.gov/of/2004/1003/htmldocs/figures/shepfig.htm

class = nan(size(sand));

if strcmp(method,'USDA')
    classes = {'sand','loamy sand','sandy loam','loam','silt loam','silt','sandy clay loam','clay loam','silty clay loadm','sandy clay','silty clay','clay'};
    % SAND = 1: 
    ind = sand>=.85 & (silt + clay*1.5)<0.15;
    class(ind)=1;
    % LOAMY SAND = 2: 
    ind = sand>=.7 & sand<=.9 & (silt + clay*1.5)>=0.15 & (silt + clay*2)<=0.3;
    class(ind)=2;
    % SANDY LOAM = 3:
    ind  = clay<=.2 & ((silt + clay*2)>0.3) & sand>=.52;
    class(ind)=3;
    ind = clay<.07 & silt<.5 & sand>.43 & sand<.53;
    class(ind)=3;
    % LOAM = 4:
    ind = clay>=.07 & clay<=.27 & silt>=.28 & silt<.5 & sand<.52;
    class(ind)=4;
    % SILT LOAM = 5:
    ind = silt>=.5 & clay>=.12 & clay<=.27;
    class(ind)=5;
    ind = silt>=.5 & silt<.8 & clay<=.12;
    class(ind)=5;
    % SILT = 6:
    ind = silt>=.8 & clay<.12;
    class(ind)=6;
    % SANDY CLAY LOAM = 7:
    ind  = clay>.2 & clay<.35 & silt<.28 & sand>=.45;
    class(ind)=7;
    % CLAY LOAM = 8:
    ind  = clay>.27 & clay<.4 & sand>.2 & sand<.45;
    class(ind)=8;
    % SILTY CLAY LOAM = 9:
    ind  = clay>.27 & clay<.4 & sand<=.2;
    class(ind)=9;
    % SANDY CLAY = 10:
    ind  = clay>=.35 & sand>=.45;
    class(ind)=10;
    % SILTY CLAY = 11:
    ind  = clay>=.4 & silt>=.4;
    class(ind)=11;
    % CLAY = 12:
    ind  = clay>=.4 & sand<.45 & silt<.4;
    class(ind)=12;
elseif strcmp(method,'Shepard')
    classes = {'sand','silty sand','clayey sand','silt','sandy silt','clayey silt','clay','sandy clay','silty clay','sand-silt-clay'};
    % SAND = 1
    ind = sand>=.75;
    class(ind)=1;
    % SILTY SAND = 2
    ind = sand>=silt & sand<.75 & clay<=silt;
    class(ind)=2;
    % CLAYEY SAND = 3
    ind = sand>=clay & sand<.75 & clay>silt;
    class(ind)=3;
    % SILT = 4
    ind = silt>=.75;
    class(ind)=4;
    % SANDY SILT = 5
    ind = silt>sand & silt<.75 & clay<=sand;
    class(ind)=5;
    % CLAYEY SILT = 6
    ind = silt>=clay & silt<.75 & clay>sand;
    class(ind)=6;
    % CLAY = 7
    ind = clay>=.75;
    class(ind)=7;
    % SANDY CLAY = 8
    ind = clay>sand & clay<.75 & sand>=silt;
    class(ind)=8;
    % SILTY CLAY = 9
    ind = clay>silt & clay<.75 & sand<silt;
    class(ind)=9;
    % SAND-SILT-CLAY = 10
    ind = sand>.2 & silt >.2 & clay>.2;
    class(ind)=10;    
end
% -------------------------------------------------------------------------
function hNew = plotthis(FI,Props,Parent,Ops,hOld,varargin)
hNew = [];
switch Props.Name
    case {'cut cell patches','cut cell patches with line','cut cell patches only line'}
        Time = varargin(1);
        idxN = varargin(3);
        idxM = varargin(2);
        xpoly = vs_get(FI,'map-series',Time,'INTx_GRS',[{0} idxN idxM],'quiet!');
        ypoly = vs_get(FI,'map-series',Time,'INTy_GRS',[{0} idxN idxM],'quiet!');
        ndryp = vs_get(FI,'map-series',Time,'Ndry_GRS',[idxN idxM],'quiet!');
        kfscc = vs_get(FI,'map-series',Time,'kfs_cc',[idxN idxM],'quiet!');
        if ~isempty(ndryp)
            icut = abs(kfscc)<=1;
            ndryp = ndryp(icut);
            xpoly = xpoly(:,icut);
            ypoly = ypoly(:,icut);
            for i = 1:length(ndryp)
                if ndryp(i)==3
                    xpoly(4:5,i) = xpoly(3,i);
                    ypoly(4:5,i) = ypoly(3,i);
                elseif ndryp(i)==4
                    xpoly(5,i) = xpoly(4,i);
                    ypoly(5,i) = ypoly(4,i);
                end
            end
            c = get(Parent,'color');
            if isequal(c,'none')
                c = get(get(Parent,'parent'),'color');
            end
            switch Props.Name
                case  'cut cell patches'
                    hNew = patch(xpoly,ypoly,1,'parent',Parent,'facecolor',c,'linestyle','none','cdata',[]);
                case   'cut cell patches with line'
                    hNew = patch(xpoly,ypoly,1,'parent',Parent,'facecolor',c,'edgecolor',Ops.colour,'cdata',[]);
                case   'cut cell patches only line'
                    hNew = patch(xpoly,ypoly,1,'parent',Parent,'facecolor','none','linestyle','-','edgecolor',Ops.colour,'cdata',[]);
            end
        end
    case {'ghost u-point reconstruction','ghost v-point reconstruction','ghost s-point reconstruction'}
        p = [Props.Name(7) '1'];
        Time = varargin(1);
        idxN = varargin{3};
        idxM = varargin{2};
        %
        plotTEXT = 0 % 0:NO TEXT;  1:GP  2:BI;  3:IP;  
        %
        nGHOST = vs_get(FI,'map-series',Time,['totGHOST' p],'quiet!');
        points = {1:nGHOST};
        mGP = vs_get(FI,'map-series',Time,['mGP' p],points,'quiet!');
        nGP = vs_get(FI,'map-series',Time,['nGP' p],points,'quiet!');
        xBI = vs_get(FI,'map-series',Time,['xBI' p],points,'quiet!');
        yBI = vs_get(FI,'map-series',Time,['yBI' p],points,'quiet!');
        mBI = vs_get(FI,'map-series',Time,['mBI' p],points,'quiet!');
        nBI = vs_get(FI,'map-series',Time,['nBI' p],points,'quiet!');        
        xIP = vs_get(FI,'map-series',Time,['xIP' p],points,'quiet!');
        yIP = vs_get(FI,'map-series',Time,['yIP' p],points,'quiet!');
        mIP = vs_get(FI,'map-series',Time,['mIP' p],points,'quiet!');
        nIP = vs_get(FI,'map-series',Time,['nIP' p],points,'quiet!');
        %
        inside = (ismember(mGP,idxM) | idxM==0) & (ismember(nGP,idxN) | idxN==0);
        mGP(~inside) = [];
        nGP(~inside) = [];
        xBI(~inside) = [];
        yBI(~inside) = [];
        xIP(~inside) = [];
        yIP(~inside) = [];
        mIP(~inside) = [];
        nIP(~inside) = [];
        %
        xc=vs_get(FI,'map-const','XCOR','quiet!');
        yc=vs_get(FI,'map-const','YCOR','quiet!');
        nmax = size(xc,1);
        nmax = nmax -1;
        mmax = size(xc,2)-1;
        % create xcor0, ycor0 (containing boundary cells and immaginary nodes), since s1 ghost can be outside the domain
        xcor0(2:nmax+1,2:mmax+1) = xc(1:nmax,1:mmax);
        ycor0(2:nmax+1,2:mmax+1) = yc(1:nmax,1:mmax);
        xcor0(2:nmax+1,1) = xcor0(2:nmax+1,2) - (xcor0(2:nmax+1,3)-xcor0(2:nmax+1,2));
        ycor0(2:nmax+1,1) = ycor0(2:nmax+1,2) - (ycor0(2:nmax+1,3)-ycor0(2:nmax+1,2)) ;
        xcor0(2:nmax+1,mmax+2) = xcor0(2:nmax+1,mmax+1) + (xcor0(2:nmax+1,mmax+1)-xcor0(2:nmax+1,mmax)) ;
        ycor0(2:nmax+1,mmax+2) = ycor0(2:nmax+1,mmax+1) + (ycor0(2:nmax+1,mmax+1)-ycor0(2:nmax+1,mmax))  ;
        xcor0(1,2:mmax+1) = xcor0(2,2:mmax+1) - (xcor0(3,2:mmax+1) - xcor0(2,2:mmax+1));
        ycor0(1,2:mmax+1) = ycor0(2,2:mmax+1) - (ycor0(3,2:mmax+1) - ycor0(2,2:mmax+1));
        xcor0(nmax+2,2:mmax+1) = xcor0(nmax+1,2:mmax+1) +  (xcor0(nmax+1,2:mmax+1) - xcor0(nmax,2:mmax+1));
        ycor0(nmax+2,2:mmax+1) = ycor0(nmax+1,2:mmax+1) +  (ycor0(nmax+1,2:mmax+1) - ycor0(nmax,2:mmax+1));
        %4 corners points should never be needed since that cannot be a corner ghost cell
        xcor0(1,1) = xcor0(2,1) - (xcor0(3,1) - xcor0(2,1));
        ycor0(1,1) = ycor0(2,1) - (ycor0(3,1) - ycor0(2,1));
        xcor0(nmax+2,1) = xcor0(nmax+2,2) - (xcor0(nmax+2,3) - xcor0(nmax+2,2));
        ycor0(nmax+2,1) = ycor0(nmax+2,2) - (ycor0(nmax+2,3) - ycor0(nmax+2,2));
        xcor0(1,mmax+2) = xcor0(1,mmax+2) + (xcor0(1,mmax+1) - xcor0(1,mmax));
        ycor0(1,mmax+2) = ycor0(1,mmax+2) + (ycor0(1,mmax+1) - ycor0(1,mmax));
        xcor0(nmax+2,mmax+2) = xcor0(nmax+2,mmax+1) + (xcor0(nmax+2,mmax+1) - xcor0(nmax+2,mmax));
        ycor0(nmax+2,mmax+2) = ycor0(nmax+2,mmax+1) + (ycor0(nmax+2,mmax+1) - ycor0(nmax+2,mmax));
        nmax0 = nmax+2;
        lGP = sub2ind(size(xcor0),nGP,mGP);         
        %
        switch p
            case 's1'
                xGP = (xcor0(lGP)+xcor0(lGP+1)+xcor0(lGP+nmax0)+xcor0(lGP+nmax0+1))/4;
                yGP = (ycor0(lGP)+ycor0(lGP+1)+ycor0(lGP+nmax0)+ycor0(lGP+nmax0+1))/4;                   
            case 'u1'            
                xGP = (xcor0(lGP+nmax0+1)+xcor0(lGP+nmax0))/2;
                yGP = (ycor0(lGP+nmax0+1)+ycor0(lGP+nmax0))/2;
            case 'v1'                      
                xGP = (xcor0(lGP+1)+xcor0(lGP+nmax0+1))/2;
                yGP = (ycor0(lGP+1)+ycor0(lGP+nmax0+1))/2;
        end
        %
        x = [xGP xBI xIP xIP]';
        y = [yGP yBI yIP yIP]';
        x(4,:) = NaN;
        y(4,:) = NaN;
        if isempty(x)
            hNew = [];
        else
            hNew(4) = line(x(:),y(:),'parent',Parent,'color','k','linestyle','-','marker','none','LineWidth' ,2);
            hNew(3) = line(xGP,yGP,'parent',Parent,'markeredgecolor','k','markerfacecolor',[128 0 0]/255,'linestyle','none','marker','o','MarkerSize',5,'LineWidth' ,2);
            hNew(2) = line(xBI,yBI,'parent',Parent,'markeredgecolor','k','markerfacecolor','w','linestyle','none','marker','o','MarkerSize',5,'LineWidth' ,2);
            hNew(1) = line(xIP,yIP,'parent',Parent,'markeredgecolor','k','markerfacecolor','k','linestyle','none','marker','o','MarkerSize',5,'LineWidth' ,2);
            if plotTEXT==1
                for i = length(mIP):-1:1
                    hNew(4+i) = text(xGP(i),yGP(i),sprintf('(%i,%i)',mGP(i),nGP(i)),'parent',Parent,'horizontalalignment','left','verticalalignment','bottom','clipping','on');
                end
            elseif plotTEXT==2
                for i = length(mIP):-1:1
                    hNew(4+i) = text(xBI(i),yBI(i),sprintf('(%i,%i)',mBI(i),nBI(i)),'parent',Parent,'horizontalalignment','left','verticalalignment','bottom','clipping','on');
                end         
            elseif plotTEXT==3
                for i = length(mIP):-1:1
                    hNew(4+i) = text(xIP(i),yIP(i),sprintf('(%i,%i)',mIP(i),nIP(i)),'parent',Parent,'horizontalalignment','left','verticalalignment','bottom','clipping','on');
                end                       
            end

        end
end


function [x,y] = xy_cutcell(x,y,FI,idx)
T_=1; ST_=2; N_=3; M_=4; K_=5;
%
xc=vs_get(FI,'map-const','XCOR',idx([N_ M_]),'quiet!');
yc=vs_get(FI,'map-const','YCOR',idx([N_ M_]),'quiet!');
area0 = repmat(NaN,size(xc));
area0(2:end,2:end) = cellarea(xc,yc);
%
Time = idx(T_);
xpoly = vs_get(FI,'map-series',Time,'INTx_GRS',[{0} idx([N_ M_])],'quiet!');
ypoly = vs_get(FI,'map-series',Time,'INTy_GRS',[{0} idx([N_ M_])],'quiet!');
ndryp = vs_get(FI,'map-series',Time,'Ndry_GRS',idx([N_ M_]),'quiet!');
kfscc = vs_get(FI,'map-series',Time,'kfs_cc',idx([N_ M_]),'quiet!');
%
for m = 1:size(kfscc,2)
    for n = 1:size(kfscc,1)
        if kfscc(n,m) == 0
            np = ndryp(n,m);
            xp = xpoly(1:np,n,m);
            yp = ypoly(1:np,n,m);
            %
            A0 = area0(n,m); % area of whole grid cell
            A1 = 0; % area of high part
            x1 = 0; % x coordinate of centroid of high part
            y1 = 0; % y coordinate of centroid of high part
            for i = 0:np-1
                if i>0
                    c = (xp(i)*yp(i+1)-xp(i+1)*yp(i));
                    x1 = x1 + (xp(i)+xp(i+1))*c;
                    y1 = y1 + (yp(i)+yp(i+1))*c;
                else
                    c = (xp(np)*yp(1)-xp(1)*yp(np));
                    x1 = x1 + (xp(np)+xp(1))*c;
                    y1 = y1 + (yp(np)+yp(1))*c;
                end
                A1 = A1 + c;
            end
            A1 = A1/2;
            x1 = x1/(6*A1);
            y1 = y1/(6*A1);
            A1 = abs(A1);
            %
            x(n,m) = (x(n,m)*A0 - x1*A1)/(A0-A1);  %these gives rounding error for small cut cells 
            y(n,m) = (y(n,m)*A0 - y1*A1)/(A0-A1);  %these gives rounding error for small cut cells 
        end
    end
end