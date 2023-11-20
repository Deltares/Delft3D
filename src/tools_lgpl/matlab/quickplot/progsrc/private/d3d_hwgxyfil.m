function varargout=d3d_hwgxyfil(FI,domain,field,cmd,varargin)
%D3D_HWGXYFIL QP support for Delft3D-WAVE files.
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
if ~strcmp(FI.SubType,'Delft3D-hwgxy') && DimFlag(M_) && DimFlag(N_)
    sz([M_ N_])=sz([N_ M_]);
    idx([M_ N_])=idx([N_ M_]);
end

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
        if i~=K_
            idx{i} = [max(1,idx{i}(1)-1) idx{i}];
            ind{i}=2:length(idx{i});
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
if XYRead

    %======================== SPECIFIC CODE =======================================
    if DimFlag(M_) && DimFlag(N_)
        %    [x,Chk]=vs_get(FI,'map-series',{t},'XP',idx([M_ N_]),'quiet');
        %    [y,Chk]=vs_get(FI,'map-series',{t},'YP',idx([M_ N_]),'quiet');
        if DimFlag(T_)
            tgrid = idx{T_}(1);
        else
            tgrid = 1;
        end
        [x,Chk]=vs_get(FI,'map-series',{tgrid},'XP',idx([M_ N_]),'quiet');
        [y,Chk]=vs_get(FI,'map-series',{tgrid},'YP',idx([M_ N_]),'quiet');
        x((x==0) & (y==0)) = NaN;
        x((x==-999) & (y==-999)) = NaN;
        y(isnan(x))=NaN;
    end

    %========================= GENERAL CODE =======================================
end

% grid interpolation ...
[x,y]=gridinterp(DataInCell,DimFlag(K_),Props.ReqLoc,x,y);

% load data ...
val1=[];
val2=[];
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

    if Props.NVal~=0
        [val1,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,elidx,'quiet');
        switch Props.Name
            case {'total wave energy'}
                % val1 = significant wave height
                rho = 1000;
                g   = 9.8;
                val1 = (1/16) * rho * g * val1.^2;
            case {'total wave variance'}
                % val1 = significant wave height
                val1 = (1/16) * val1.^2;
        end
    end
    if ~isempty(Props.Val2)
        [val2,Chk]=vs_let(FI,Props.Group,{idx{T_}},Props.Val2,elidx,'quiet');
    end
    if isequal(Props.Val1,'DEPTH')
        val1(val1==-99)=NaN;
    else
        [act,Chk]=vs_let(FI,'map-series',{idx{T_}},'DEPTH',elidx,'quiet');
        if ~isempty(Props.Val2)
            val2(act==-99)=NaN;
        end
        val1(act==-99)=NaN;
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
        error('No ALFAS available for HWGXY file.');
        %  [alf,Chk] = vs_get(FI,'GRID','ALFAS',idx([M_ N_]),'quiet');
        %  alf = alf*pi/180;
        %  [val1,val2]=cur2ca(val1,val2,alf);
    end
else
    Props.NVal=0;
end
%======================== SPECIFIC CODE =======================================
% select active points ...
% if Props.NVal>0 & isequal(Props.ReqLoc,'d')
%     %  [act,Chk]=vs_get(FI,'map-series',{t},'CODE',idx([M_ N_]),'quiet');
%     %  [act,Chk]=vs_get(FI,'map-series',{1},'CODE',idx([M_ N_]),'quiet');
%     [act,Chk]=vs_get(FI,'map-series',{idx{T_}},'DEPTH',idx([M_ N_]),'quiet');
%     act=act~=-99;
% elseif DataRead
%     act=ones(size(val1));
% else
%     act=[];
% end
% if DataInCell
%     % [gridact,Chk]=vs_get(FI,'map-series',{1},'CODE',idx([M_ N_]),'quiet');
%     [gridact,Chk]=vs_get(FI,'map-series',{idx{T_}},'DEPTH',idx([M_ N_]),'quiet');
%     gridact=gridact~=-99;
% else
%     gridact=act;
% end
% 
% %========================= GENERAL CODE =======================================
% if XYRead
%     if DimFlag(K_)
%         szx=[size(x) 1]; % extent szx for the case that dataset in K dir. is 1
%         szx1=szx([1:2 4:end]);
%         szx1(2)=szx(2)*szx(3);
%         x=reshape(x,szx1);
%         x(:,gridact<=0,:)=NaN;
%         x=reshape(x,szx);
%         y=reshape(y,szx1);
%         y(:,gridact<=0,:)=NaN;
%         y=reshape(y,szx);
%         %---
%         szz=[size(z) 1]; % extent szx for the case that dataset in K dir. is 1
%         szz1=szz([1:2 4:end]);
%         szz1(2)=szz(2)*szz(3);
%         z=reshape(z,szz1);
%         z(:,act<=0,:)=NaN;
%         z=reshape(z,szz);
%     else
%         x(gridact<=0)=NaN;
%         y(gridact<=0)=NaN;
%     end
% end
% if Props.NVal>0
%     szz=[size(val1) 1]; % extent szx for the case that dataset in K dir. is 1
%     szz1=szz([1:2 4:end]);
%     szz1(2)=szz(2)*szz(3);
%     val1=reshape(val1,szz1);
%     val1(:,act<=0,:)=NaN;
%     val1=reshape(val1,szz);
%     if ~isempty(val2)
%         val2(:,isnan(val1))=NaN;
%     end
% end

% select subrange if necessary ... M,N,K only
DimMask=[0 0 1 1 1];
if DataInCell
    for i=[M_ N_ K_]
        if DimFlag(i)
            allidx(i)=0;
        end
    end
end
if 1 %~all(allidx(DimMask & DimFlag))
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
    if Props.NVal==1
        val1=val1(:,ind{:});
    elseif Props.NVal==2
        val1=val1(:,ind{:});
        val2=val2(:,ind{:});
    end
end

%================== NEFIS SPECIFIC CODE =======================================
% permute n and m dimensions into m and n if necessary
if ~strcmp(FI.SubType,'Delft3D-hwgxy') && DimFlag(M_) && DimFlag(N_)
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
    Info1=vs_disp(FI,'map-series','XP');
    if isstruct(Info1) && isequal(Info1.ElmUnits,'[  DEG  ]')
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    end
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
function Out=infile(FI,domain)

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'     'Val2'    'SubFld' 'MNK'};
DataProps={'wave grid'          ''       [0 0 1 1 0]  0         0     ''       'd'   'd'       ''      'map-series'     'XP'       'YP'       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'wind velocity'             'm/s'    [1 0 1 1 0]  1         2     'x'      'd'   'd'       ''      'WIND'           'WINDU'    'WINDV'    []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'total wave energy'         'J/m^2'  [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'HSIGN'    ''         []       0
    'total wave variance'       'm2'     [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'HSIGN'    ''         []       0
    'hsig wave height'          'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'HSIGN'    ''         []       0
    'hsig wave vector (mean direction)' ...
                                'm'      [1 0 1 1 0]  1         2     'm'      'd'   'd'       ''      'map-series'     'HSIGN'    'DIR'      []       0
    'hsig wave vector (peak direction)' ...
                                'm'      [1 0 1 1 0]  1         2     'm'      'd'   'd'       ''      'map-series'     'HSIGN'    'PDIR'     []       0
    'difference in significant wave height (last iterations)' ...
                                'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'DHSIGN'   ''         []       0
    'mean absolute wave period T_{m-1,0}' ....
                                's'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'TMM10'    ''         []       0
    'mean absolute zero-crossing period T_{m02}' ...
                                's'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'TM02'     ''         []       0
    'mean wave period T_{m01}'  's'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'PERIOD'   ''         []       0
    'peak wave period'          's'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'TP'       ''         []       0
    'relative peak wave period' 's'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'RTP'      ''         []       0
    'smoothed peak period'      's'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'TPS'      ''         []       0
    'difference in mean wave period (last iterations)' ...
                                's'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'DRTM01'   ''         []       0
    'mean wave steepness'       '-'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'STEEPW'   ''         []       0
    'mean wave length'          'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'WLENGTH'  ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'directional spreading'     'deg'    [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'DSPR'     ''         []       0
    'dissipation'               'N/ms'   [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'DISSIP'   ''         []       0
    'leakage'                   'J/m^2s' [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'LEAK'     ''         []       0
    'fraction breaking'         '-'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'QB'       ''         []       0
    'energy transport'          'W/m'    [1 0 1 1 0]  1         2     'x'      'd'   'd'       ''      'map-series'     'TRANSP-X' 'TRANSP-Y' []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'set-up due to waves'       'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'SETUP'    ''         []       0
    'water depth'               'm'      [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'DEPTH'    ''         []       0
    'current velocity'          'm/s'    [1 0 1 1 0]  1         2     'x'      'd'   'd'       ''      'map-series'     'VELOC-X'  'VELOC-Y'  []       0
    'wave induced force'        'N/m^2'  [1 0 1 1 0]  1         2     'x'      'd'   'd'       ''      'map-series'     'FX'       'FY'       []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0
    'orbital velocity near bottom' ...
                                'm/s'    [1 0 1 1 0]  1         1     ''       'd'   'd'       ''      'map-series'     'UBOT'     ''         []       0
    '-------'                   ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''         ''         []       0};

%============================= AUTODETECTION ==================================
Info=vs_disp(FI,'map-series','XP');
nm=Info.SizeDim([1 2]);
k=1;
SkipGroup={};
SkipElem={'CODE'};
DataProps=auto_map_detect(FI,DataProps,nm,k,SkipGroup,SkipElem);
%
% shift autodetect fields from z to d location ...
autoflds=strmatch('z',DataProps(:,7));
DataProps(autoflds,7:8)={'d'};

Out=cell2struct(DataProps,PropNames,2);

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
    elseif ~isempty(Out(i).Val2)
        Info=vs_disp(FI,Out(i).Group,Out(i).Val2);
        if ~isstruct(Info)
            % remove references to non-stored data fields
            Out(i,:)=[];
        elseif isequal(Info.SizeDim,1)
            % remove references to non-stored data fields
            Out(i,:)=[];
        end
    end
end

%======================= SET USEGRID OPTIONS ==================================
for i=1:length(Out)
    switch Out(i).ReqLoc
        case 'd'
            Out(i).UseGrid=1;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(M_) && Props.DimFlag(N_)
    Info=vs_disp(FI,'map-series','XP');
    sz([M_ N_])=Info.SizeDim;
    if ~strcmp(FI.SubType,'Delft3D-hwgxy')
        sz([M_ N_])=sz([N_ M_]);
    end
end;
%if Props.DimFlag(K_)
%  Info=vs_disp(FI,'GRID','THICK');
%  sz(K_)=Info.SizeDim;
%end
if Props.DimFlag(T_)
    Info=vs_disp(FI,Props.Group,[]);
    sz(T_)=Info.SizeDim;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[pars,Chk]=vs_get(FI,'PARAMS','*','quiet');
d0=tdelft3d(pars.IT01,pars.IT02);
if isequal(Props.Group,'map-series')
    Time = 'map-series';
else
    Time = 'reference-time';
    Info1 = vs_disp(FI,'map-series',[]);
    Info2 = vs_disp(FI,Props.Group,[]);
    if isstruct(Info1) && isstruct(Info2) && isequal(Info1.SizeDim,Info2.SizeDim)
        Time = 'map-series';
    end
end
%
switch Time
    case 'map-series'
        [T,Chk] = vs_let(FI,'map-series',{t},'TIME','quiet');
        T = d0+T*pars.TSCALE/(24*3600);
    otherwise
        T = d0;
end
% -----------------------------------------------------------------------------
