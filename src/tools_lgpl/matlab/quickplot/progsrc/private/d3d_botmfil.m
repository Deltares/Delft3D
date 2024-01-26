function varargout=d3d_botmfil(FI,domain,field,cmd,varargin)
%D3D_BOTMFIL QP support for Delft3D-MOR bottom module map files.
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
        varargout={{}};
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

        mvgrid=isstruct(vs_disp(FI,'GRIDTIM',[]));
        if mvgrid
            [x,Chk]=vs_get(FI,'GRIDTIM',idx(T_),'XCOR',idx([M_ N_]),'quiet');
            [y,Chk]=vs_get(FI,'GRIDTIM',idx(T_),'YCOR',idx([M_ N_]),'quiet');
        else

            [x,Chk]=vs_get(FI,'GRID','XCOR',idx([M_ N_]),'quiet');
            [y,Chk]=vs_get(FI,'GRID','YCOR',idx([M_ N_]),'quiet');

        end

        x((x==0) & (y==0)) = NaN;
        y(isnan(x))=NaN;
    end

    %========================= GENERAL CODE =======================================
end

% grid interpolation ...
[x,y]=gridinterp(DataInCell,0,Props.ReqLoc,x,y);

% load data ...
if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end;

if DataRead
    %================== NEFIS SPECIFIC CODE =======================================
    elidx=idx(2:end);

    elidx(~DimFlag(2:end))=[];
    if ~isempty(Props.SubFld)
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
        case {'bed level'}
            val1=-val1;
        case {'cum. erosion/sedimentation'}
            [refdp,Chk]=vs_let(FI,'MAPBOTTIM',{1},'DP',elidx,'quiet');
            for t=1:size(val1,1),
                val1(t,:)=refdp(1,:)-val1(t,:); % erosion negative, deposition positive
            end;
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
        x=permute(x,perm);
        y=permute(y,perm);
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
elseif Props.NVal==1
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
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'    'Val2'  'SubFld' 'MNK'};
DataProps={'morphologic grid'          ''       [0 0 1 1 0]  0         0    ''        'd'   'd'       ''      'GRID'           'XCOR'    ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0    ''        ''    ''        ''      ''                ''       ''       []       0
    'bedload transport'         'm^3/m'  [1 0 1 1 0]  1         2    'u'       'z'   'z'       ''      'MAPBOTTIM'      'TX'      'TY'     []       0
    'entrainment'               'm'      [1 0 1 1 0]  1         1    ''        'd'   'd'       ''      'MAPBOTTIM'      'EN'      ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0    ''        ''    ''        ''      ''                ''       ''       []       0
    'bed level'                 'm'      [1 0 1 1 0]  1         1    ''        'd'   'd'       ''      'MAPBOTTIM'      'DP'      ''       []       0
    'cum. erosion/sedimentation' ...
    'm'      [1 0 1 1 0]  1         1    ''        'd'   'd'       ''      'MAPBOTTIM'      'DP'      ''       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0    ''        ''    ''        ''      ''                ''       ''       []       0
    'GUU grid distance'         'm'      [1 0 1 1 0]  1         1    ''        'u'   'u'       ''      'GRID'           'GUU'     ''       []       0
    'GVU grid distance'         'm'      [1 0 1 1 0]  1         1    ''        'u'   'u'       ''      'GRID'           'GVU'     ''       []       0
    'GVV grid distance'         'm'      [1 0 1 1 0]  1         1    ''        'v'   'v'       ''      'GRID'           'GVV'     ''       []       0
    'GUV grid distance'         'm'      [1 0 1 1 0]  1         1    ''        'v'   'v'       ''      'GRID'           'GUV'     ''       []       0
    'cell area water level point' ...
    'm^2'    [1 0 1 1 0]  1         1    ''        'z'   'z'       ''      'GRID'           'GSQS'    ''       []       0
    'cell area bottom point'    'm^2'    [1 0 1 1 0]  1         1    ''        'd'   'd'       ''      'GRID'           'GSQD'    ''       []       0};

%============================= AUTODETECTION ==================================
Info=vs_disp(FI,'GRID','XCOR');
nm=Info.SizeDim([1 2]);
Info=vs_disp(FI,'GRID','THICK');
k=Info.SizeDim(1);
SkipGroup={'GRID','KENMTIM','TEMPOUT','KENMCNST','INITBOT','GEOM'};
DataProps=auto_map_detect(FI,DataProps,nm,k,SkipGroup);

Out=cell2struct(DataProps,PropNames,2);


mvgrid=isstruct(vs_disp(FI,'GRIDTIM',[]));
if mvgrid
    Out(1).Group='GRIDTIM';
    Out(1).DimFlag=[1 0 1 1 0];
end

%======================== SPECIFIC CODE REMOVE ================================
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info)
        % remove references to non-stored data fields
        Out(i,:)=[];
    elseif isequal(Info.SizeDim,1)
        % remove references to non-stored data fields
        Out(i,:)=[];
        %  elseif strcmp(Out(i).Name,'fixed layer'),
        %    [Pres,Chk]=vs_get(FI,'INITBOT','NVASTI','quiet');
        %    if ~isequal(Pres,1)
        %      Out(i,:)=[];
        %    end
    end
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
[tscale,Chk]=vs_get(FI,'MAPBOT','TSCALE','quiet');
%d0=tdelft3d(pars.IT01,pars.IT02);
d0=1; % no reference date in BOTM-file
switch Props.Group,
    case 'MAPBOTTIM'
        [T,Chk]=vs_let(FI,'MAPBOTTIM',{t},'ITBODE','quiet');
        T=d0+T*tscale/(24*3600);
    otherwise, % ONE FIELD
        T=d0;
end
% -----------------------------------------------------------------------------
