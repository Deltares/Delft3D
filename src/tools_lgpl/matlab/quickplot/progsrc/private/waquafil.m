function varargout=waquafil(FI,domain,field,cmd,varargin)
%WAQUAFIL QP support for SIMONA (WAQUA/TRIWAQ) files.
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
        if Props.DimFlag(ST_)~=0
            S=readsts(FI,Props);
            varargout={S};
        else
            varargout={{}};
        end
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

if length(idx{T_})>1 && ~DimFlag(ST_)
    error('Loading data for multiple timesteps is currently not yet supported for Waqua SDS files.');
end

if DimFlag(ST_)
    if isequal(idx{ST_},0)
        idx{ST_}=1:sz(ST_);
    end
end

% select appropriate spatial indices ...

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
        end
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

gidx = idx;
if DataInCell
    for i=[M_ N_ K_]
        if DimFlag(i)
            if isequal(idx{i},1)
                gidx{i}=[1 2];
                idx{i}=1;
            elseif idx{i}(1)==1
                gidx{i}=idx{i};
                idx{i}=idx{i}(2:end);
            else
                gidx{i}=[idx{i}(1)-1 idx{i}];
            end
        end
    end
else
    gidx=idx;
end

% read grid ...
x=[];
y=[];
z=[];
val={};
if XYRead
    %======================== SPECIFIC CODE =======================================
    if DimFlag(M_) && DimFlag(N_)
        if DimFlag(K_)
            zgidx = gidx;
            zgidx([M_ N_]) = idx([M_ N_]);
            if strcmp(Props.Loc3D,'c') && ~DataInCell
                zgrid = 'zgrid3dc';
            else
                zgrid = 'zgrid3di';
            end
            [x,y,z,val]=waquaio(FI,Props.Exper,zgrid,zgidx{DimFlag~=0});
            val={val};
            z=reshape(z,[1 size(z)]);
            if DataInCell
                DimFlagD=DimFlag;
                DimFlagD([1 5])=0;
                [x,y]=waquaio(FI,Props.Exper,'dgrid',gidx{DimFlagD~=0});
            end
            x=reshape(x,[1 size(x)]);
            y=reshape(y,[1 size(y)]);
            if DataInCell
                x=repmat(x,[1 1 1 length(gidx{K_})]);
                y=repmat(y,[1 1 1 length(gidx{K_})]);
            end
            %z=z(:,idx{M_},idx{N_},gidx{K_}); % Note: idx{MN_} instead of gidx{MN_} for DataInCell
            %x=x(:,gidx{M_},gidx{N_},:);
            %y=y(:,gidx{M_},gidx{N_},:);
        else
            GLoc=Props.ReqLoc;
            if DataInCell, GLoc='d'; end
            [x,y]=waquaio(FI,Props.Exper,[GLoc 'grid'],gidx{M_},gidx{N_});
            %x=x(gidx{M_},gidx{N_});
            %y=y(gidx{M_},gidx{N_});
        end
    elseif DimFlag(K_)
        if strcmp(Props.Loc3D,'i')
            zgrid = [Props.zWaqIO 'i'];
        else
            zgrid = [Props.zWaqIO 'c'];
        end
        z=waquaio(FI,Props.Exper,zgrid,idx{T_},idx{ST_},idx{K_});
    end
    %========================= GENERAL CODE =======================================
end

% load data ...
T=[];
if DataRead
    %================== NEFIS SPECIFIC CODE =======================================
    elidx=idx(3:end);
    elidx(~DimFlag(3:end))=[];
    Name=Props.Name;
    ThinDam=0;
    ThinDamVal_in_same_call=0;
    switch Name
        case 'bed level'
            if DataInCell && waqua('exists',FI,Props.Exper,'DPS_FLOW')
                Props.WaqIO = 'height_wl_points';
                Props.Loc = 'z';
                Props.ReqLoc = 'z';
            end
        case 'thin dams'
            Props.NVal=2;
            ThinDam=1;
            DimFlag(T_)=1;
            idx{T_}=1;
        case {'weir crest level', 'weir sill level (low M/N)', 'weir sill level (high M/N)', 'weir crest length', 'weir slope (low M/N)', 'weir slope (high M/N)', 'weir vegetation height', 'weir vegetation density', 'weir vegetation drag'}
            Props.NVal=4;
            ThinDamVal_in_same_call=1;
            ThinDam=1;
            idx{T_}=1;
        case 'weir flow condition' 
            Props.NVal=6;
            ThinDamVal_in_same_call=1;
            ThinDam=1;
        case {'weir unit discharge', 'weir overflow depth', 'weir velocity', 'weir head loss'}
            Props.NVal=4;
            ThinDamVal_in_same_call=1;
            ThinDam=1;
        case 'temporarily inactive velocity points'
            Props.NVal=2;
            ThinDam=1;
        case 'roughness Chezy C'
            Props.NVal=4;
            ThinDam=1;
        case {'water level stations','velocity stations'}
            Props.NVal=2;
        case {'velocity','horizontal velocity'}
            if Props.MNK>1,
                Props.WaqIO='veloc';
            end
    end
    readT = DimFlag(T_);
    switch Props.NVal
        case 0
            %nothing
        case 1
            [val{1:1+readT}]=waquaio(FI,Props.Exper,Props.WaqIO,idx{DimFlag~=0});
        case 2
            [val{1:2+readT}]=waquaio(FI,Props.Exper,Props.WaqIO,idx{DimFlag~=0});
        case 3
            if strcmp(Props.WaqIO,'veloc') && isfield(Props,'MNK') && (Props.MNK>1)
                [val{1:3+readT}]=waquaio(FI,Props.Exper,'veloc0',idx{DimFlag~=0},1);
            else
                [val{1:3+readT}]=waquaio(FI,Props.Exper,Props.WaqIO,idx{DimFlag~=0});
            end
        case {4,6}
            if ThinDamVal_in_same_call
                [val{1:4+readT}]=waquaio(FI,Props.Exper,Props.WaqIO,idx{DimFlag~=0});
            else
                [val{1:2+readT}]=waquaio(FI,Props.Exper,'veloc0',idx{DimFlag~=0},1);
                val{1}=~(isnan(val{1}) | val{1}==0);
                val{2}=~(isnan(val{2}) | val{2}==0);
                [val{3:4+readT}]=waquaio(FI,Props.Exper,Props.WaqIO,idx{DimFlag~=0});
            end
    end
    switch Props.Name
        case {'water level stations','velocity stations'}
            x=val{1};
            y=val{2};
            CHAR = ['flowstat-' Props.WaqIO(1:2)];
            val={deblank(waquaio(FI,Props.Exper,CHAR,idx{DimFlag~=0}))};
            Props.NVal=1;
    end
    if readT
        T=val(end);
        val(end)=[];
    else
        T={};
    end
    for i=1:length(val)
        val{i}=reshape(val{i},[1 size(val{i})]);
    end
    if Props.NVal==1
        % data interpolation ...
        if DataInCell
            % data in z-points
            Props.ReqLoc='z';
        end
        if isequal(Props.Loc,'d') && isequal(Props.ReqLoc,'z')
            val{1}=interp2cen(val{1},'t');
        end
    end
    %for i=1:length(val)
    %   val{i}=val{i}(:,elidx{:});
    %end
    switch Name
        case 'thin dams'
            val{1}=val{1}==0;
            val{2}=val{2}==0;
        case 'temporarily inactive velocity points'
            val{1}=val{1}<=0;
            val{2}=val{2}<=0;
    end
else
    Props.NVal=0;
end
%========================= GENERAL CODE =======================================
%val1(:,isnan(x))=NaN;
%if ~isempty(val2)
%  val2(:,isnan(x))=NaN;
%end

%================== NEFIS SPECIFIC CODE =======================================
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
    if Props.NVal>0
        for i=1:length(val)
            val{i}=permute(val{i},[1 1+perm]);
        end
    end
end
%=====================  ==== GENERAL CODE =======================================

% reshape if a single timestep is selected ...
if DimFlag(ST_)
    sz=[size(val{1}) 1];
    sz = [sz(2) 1 sz(3)];
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
    if DataRead && Props.NVal>0
        sz=size(val{1}); sz=[sz(2:end) 1];
        for i=1:length(val)
            val{i}=reshape(val{i},sz);
        end
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    Ans.XUnits='m';
    Ans.YUnits='m';
    if waquaio(FI,Props.Exper,'curvl')>=2
        Ans.X = Ans.X*(180/pi);
        Ans.Y = Ans.Y*(180/pi);
        Ans.XUnits='deg';
        Ans.YUnits='deg';
    end
    if DimFlag(K_)
        Ans.Z=z;
        Ans.ZUnits='m';
    end
end
switch Props.NVal
    case 1
        Ans.Val=val{1};
    case 2
        if ThinDam
            Ans.XDam=val{1};
            Ans.YDam=val{2};
        else
            Ans.XComp=val{1};
            Ans.YComp=val{2};
        end
    case 3
        Ans.XComp=val{1};
        Ans.YComp=val{2};
        Ans.ZComp=val{3};
    case {4,6}
        Ans.XDam=val{1};
        Ans.YDam=val{2};
        if Props.NVal==6
            switch Props.Name
                case 'weir flow condition'
                    transform = [1 2 0 0 3 0 0 4 5];
                    val{3} = transform(val{3}+5);
                    val{4} = transform(val{4}+5);
                    Ans.Classes = {'subcritical flow in negative direction', 'critical flow in negative direction', 'no flow across weir', 'critical flow in positive direction', 'subcritical flow in positive direction'};
            end
        end
        Ans.XDamVal=val{3};
        Ans.YDamVal=val{4};
end
if iscell(T)
    if isempty(T)
        T=[];
    else
        T=T{1};
    end
else
    T=[];
    if Props.DimFlag(T_)
        T=readtim(FI,Props,idx{T_});
    end
end
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Domains=domains(FI)
Domains={FI.Experiment.Name};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                    'Units' 'DimFlag' 'DataInCell' 'MNK' 'NVal' 'Loc' 'ReqLoc' 'Loc3D' 'Exper' 'WaqIO'     'zWaqIO'   'Char'};
DataProps={'depth grid'               ''       [0 0 1 1 0]  0        0      0     'd'   'd'       ''     ''      ''          ''         'MESH_CURVIL'
    'hydrodynamic grid'               ''       [1 0 1 1 1]  0        0      0     'z'   'z'       ''     ''      ''          ''         'LAYER_INTERFACES'
    'thin dams'                       ''       [0 0 1 1 0]  0        0      0     'd'   'd'       ''     ''      'drywet'    ''         'SOLUTION_DRYWET'
    'temporarily inactive velocity points' ...
    ''       [1 0 1 1 0]  0        0      0     'd'   'd'       ''     ''      'drywet'    ''         'SOLUTION_DRYWET'
    'weir crest level'                'm'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs-cresth'   ''    'MESH_WEIPOS'
    'weir sill level (low M/N)'       'm'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs-sill1'    ''    'MESH_WEIPOS'
    'weir sill level (high M/N)'      'm'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs-sill2'    ''    'MESH_WEIPOS'
    'weir crest level'                'm'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-cresth ' ''    'MESH_WEIRS'
    'weir sill level (low M/N)'       'm'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-sill1'   ''    'MESH_WEIRS'
    'weir sill level (high M/N)'      'm'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-sill2'   ''    'MESH_WEIRS'
    'weir crest length'               ''       [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-crestl'  ''    'MESH_WEIRS'
    'weir slope (low M/N)'            '-'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-talud1'  ''    'MESH_WEIRS'
    'weir slope (high M/N)'           '-'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-talud2'  ''    'MESH_WEIRS'
    'weir vegetation height'          'm'      [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-vegh'    ''    'MESH_WEIRS'
    'weir vegetation density'         ''       [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-vegdens' ''    'MESH_WEIRS'
    'weir vegetation drag'            ''       [0 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-vegdrag' ''    'MESH_WEIRS'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'wind'                            'm/s'    [1 0 1 1 0]  1        0      2     'z'   'z'       ''     ''      'wind'      ''         'SOLUTION_WIND'
    'wind'                            'm/s'    [1 0 1 1 0]  1        0      2     'z'   'z'       ''     ''      'wind'      ''         'FORCINGS_SVWP_WINDU'
    'pressure'                        'Pa'     [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'press'     ''         'SOLUTION_PRESS'
    'pressure'                        'Pa'     [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'press'     ''         'FORCINGS_SVWP_PRESSURE'
    'Charnock parameter'              '-'      [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'charnock'  ''         'SOLUTION_CHARNOCK'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'wave height'                     'm'      [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'hrms'      ''         'COEFF_FLOW_WAVES'
    'wave vector'                     'm'      [1 0 1 1 0]  1        0      2     'z'   'z'       ''     ''      'wvec'      ''         'COEFF_FLOW_WAVES'
    'wave period'                     's'      [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'peakperiod' ''        'COEFF_FLOW_WAVES'
    'wave number'                     ''       [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'wavenumber' ''        'SOLUTION_WAVES_WAVENUMBER'
    'wave force'                      'N/m^2'  [1 0 1 1 0]  1        0      2     'z'   'z'       ''     ''      'wforce'    ''         'COEFF_FLOW_WAVES'
    'stokes drift velocity'           'm/s'    [1 0 1 1 0]  1        0      2     'z'   'z'       ''     ''      'stokes'    ''         'SOLUTION_WAVES_STOKES'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'water level'                     'm'      [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'wlvl'      ''         'SOLUTION_FLOW_SEP'
    'water depth'                     'm'      [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'wdepth'    ''         'SOLUTION_FLOW_SEP'
    'horizontal velocity'             'm/s'    [1 0 1 1 1]  1        1      2     'z'   'z'       'c'    ''      'xyveloc'   ''         'SOLUTION_FLOW_UP'
    'velocity'                        'm/s'    [1 0 1 1 1]  1        1      3     'z'   'z'       'c'    ''      'xyveloc'   ''         'SOLUTION_FLOW_UP'
    'unit discharge'                  'm^2/s'  [1 0 1 1 0]  1        0      2     'z'   'z'       'c'    ''      'xyudisch'  ''         'SOLUTION_FLOW_UP'
    'discharge potential'             'm^3/s'  [1 0 1 1 0]  1        0      1     'd'   'd'       ''     ''      'dischpot'  ''         'SOLUTION_FLOW_UP'
    'roughness Chezy C'            'm^{1/2}/s' [1 0 1 1 0]  0        0     0.9    'd'   'd'       ''     ''      'chezy'     ''         'SOLUTION_FLOW_CZ'
    'head'                            'm'      [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'head'      ''         'SOLUTION_FLOW_SEP'
    'horizontal viscosity'            'm^2/s'  [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      'hvisco'    ''         'SOLUTION_FLOW_TOTALHORVISC'
    %'weir flow condition'             ''       [1 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-fc' ''         'SOLUTION_FLOW_KCONDW'
    'weir unit discharge'             'm^3/m'  [1 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-q'  ''         'SOLUTION_FLOW_WEIRQ'
    'weir overflow depth'             'm'      [1 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-h'  ''         'SOLUTION_FLOW_WEIRH'
    'weir velocity'                   'm/s'    [1 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-u'  ''         'SOLUTION_FLOW_WEIRVL'
    'weir head loss'                  'm'      [1 0 1 1 0]  0        0     0.6    'd'   'd'       ''     ''      'weirs2-de' ''         'SOLUTION_FLOW_WEIRDE'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'non-hydrostatic pressure'        'Pa'     [1 0 1 1 1]  1        0      1     'z'   'z'       'c'    ''      'pressure'  ''         'SOLUTION_HYDRODYNAMIC_PRESSURE'
    '--substances'                    ''       [1 0 1 1 1]  1        0      1     'z'   'z'       'c'    ''      ''          ''         'SOLUTION_TRANS'
    'turbulent kinetic energy'       'm^2/s^2' [1 0 1 1 1]  1        0      1     'z'   'z'       'i'    ''      'energy'    ''         'SOLUTION_TURB_ENERGY'
    'energy dissipation'             'm^2/s^3' [1 0 1 1 1]  1        0      1     'z'   'z'       'i'    ''      'dissip'    ''         'SOLUTION_TURB_DISSIP'
    'vertical eddy diffusivity'       'm^2/s'  [1 0 1 1 1]  1        0      1     'z'   'z'       'i'    ''      'vdiffu'    ''         'SOLUTION_DIFCWM'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--inc'                           ''       [1 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'INCREMENTAL_OUTPUT_TIMIDX'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MAXVALUES_SEP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MAXVALUES_UP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MAXVALUES_VP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MAXVALUES_MGN'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MAXVALUES_SAL'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MAXVALUES_TEMP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MAXVALUES_RP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MINVALUES_SEP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MINVALUES_UP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MINVALUES_VP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MINVALUES_MGN'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MINVALUES_SAL'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MINVALUES_TEMP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    '--maxmin'                        ''       [0 0 1 1 0]  1        0      1     'z'   'z'       ''     ''      ''          ''         'SOLUTION_DERIVED_MINVALUES_RP'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'bed shear stress'                'N/m^2'  [1 0 1 1 0]  1        0      2     'z'   'z'       ''     ''      'wstress'   ''         'SOLUTION_WAVES_STRESS'
    'bed level'                       'm'      [0 0 1 1 0]  1        0      1     'd'   'd'       ''     ''      'height'    ''         'MESH_H'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'water level stations'            ''       [0 0 1 0 0]  0        0      4     ''    ''        ''     ''      'wl-xy'     ''         'CHECKPOINTS_FLOW_IWLPT'
    'velocity stations'               ''       [0 0 1 0 0]  0        0      4     ''    ''        ''     ''      'uv-xy'     ''         'CHECKPOINTS_FLOW_ICURPT'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'water level (station)'           'm'      [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'wl-stat'   ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'horizontal velocity (station)'   'm/s'    [1 5 0 0 1]  0        0      2     ''    ''        'c'    ''      'uv-stat'   'z-stat'   'TIMEHISTORIES_FLOW_TIMHIS'
    'vertical velocity (station)'     'm/s'    [1 5 0 0 1]  0        0      1     ''    ''        'c'    ''      'w-stat'    'z-stat'   'TIMEHISTORIES_FLOW_TIMHIS'
    '--substances (station)'          ''       [1 5 0 0 1]  0        0      1     ''    ''        'c'    ''      ''          'z-sbstat' 'TIMEHISTORIES_TRANS'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'instantaneous discharge (transect)' 'm^3/s' [1 5 0 0 0] 0       0      1     ''    ''        ''     ''      'mq-stat'   ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'cumulative discharge (transect)' 'm^3'    [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'cq-stat'   ''         'TIMEHISTORIES_FLOW_TIMHIS'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''
    'gate level (barrier)'            'm'      [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'gl-bar'    ''         'TIMEHISTORIES_FLOW_RRSBAH'
    'water level (barrier high M/N side)' 'm'  [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'wl-hbarp'  ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'water level (barrier low M/N side)' 'm'   [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'wl-lbarp'  ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'sill level (barrier)'            'm'      [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'sl-bar'    ''         'TIMEHISTORIES_FLOW_RRSBAH'
    'flow-through height (barrier)'   'm'      [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'hg-barp'   ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'width (barrier)'                 'm'      [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'wd-bar'    ''         'TIMEHISTORIES_FLOW_RRSBAH'
    'discharge (barrier)'             'm^3/s'  [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'q-barp'    ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'velocity (barrier high M/N side)' 'm/s'   [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'vel-hbarp' ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'velocity (barrier)'              'm/s'    [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'vel-barp'  ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'velocity (barrier low M/N side)' 'm/s'    [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'vel-lbarp' ''         'TIMEHISTORIES_FLOW_TIMHIS'
    'energy loss (barrier)'           ''       [1 5 0 0 0]  0        0      1     ''    ''        ''     ''      'enl-barp'  ''         'TIMEHISTORIES_FLOW_TIMHIS'
    '-------'                         ''       [0 0 0 0 0]  0        0      0     ''    ''        ''     ''      ''          ''         ''};
Out1=cell2struct(DataProps,PropNames,2);

Out=Out1(1); Out(:,1)=[];
if domain==0, domain=1; end
e=domain;
i=0;
eName=FI.Experiment(e).Name;
chars={FI.Experiment(e).Char.Name};
if waqua('exists',FI,eName,'MESH_IDIMEN')
    dimen=waqua('readsds',FI,eName,'MESH_IDIMEN');
    kflag = dimen(18)==1;
    sdstype='TRIWAQ';
elseif waqua('exists',FI,eName,'MESH01_SPECIFIC_IDIMEN')
    kflag = 0;
    sdstype='WAQWND';
else
    error('Unknown SDS type: cannot locate grid dimension characteristics.')
end
%
stagwflag = 11;
if waqua('exists',FI,eName,'MESH01_GENERAL_DIMENSIONS')
    mgd = waqua('read',FI,eName,'MESH01_GENERAL_DIMENSIONS');
    stagwflag = mgd(8);
end
if stagwflag~=11 && stagwflag~=13
    ui_message('warning','Unexpected value (%i) for MESH01_GENERAL_DIMENSIONS(8): 11 and 13 supported',stagwflag)
    stagwflag = 11;
end
%
% 0 = wind speed in the svwp file
% 1 = wind stress in the svwp file
% 2 = cumulative wind stress in the svwp file
% 3 = cumulative wind stress in the svwp file, conversion to times in between.
% 4 = external forces in the svwp file --> SOLUTION_EXTFORCE instead of SOLUTION_WIND
istress = 0;
if waqua('exists',FI,eName,'CONTROL_SVWP_ICWINA')
    csi = waqua('read',FI,eName,'CONTROL_SVWP_ICWINA');
    istress = csi(2);
end
%
maxmin_quant = {'time','water level','velocity in m direction','velocity in n direction','velocity magnitude','salinity','temperature','constituent concentration','velocity in x direction','velocity in y direction'};
maxmin_var = {'','SEP','UP','VP','MGN','SAL','TEMP','RP'};
maxmin_qcons = 8;
maxmin_unit = {'min','m','m/s','m/s','m/s','kg/m^3','degC','','m/s','m/s'};
nflds = 0;
if strcmp(sdstype,'TRIWAQ')
    [Subs,SubsUnit]=waquaio(FI,eName,'substances');
    if waqua('exists',FI,eName,'CONTROL_DERIVED_MAXVALUES_INDICES')
        maxmin = waqua('read',FI,eName,'CONTROL_DERIVED_MAXVALUES_INDICES');
        nflds = maxmin(1);
        if nflds~=7
            ui_message('warning',{'Number of fields in MIN/MAX data unexpected.','Data may be interpreted incorrectly!'})
        end
        maxmin = reshape(maxmin(21:end),[nflds+4 nflds 2]);
        % for some SDS files, maxmin(1:nflds+3,:,:) seems to contain the
        % indices of the fields within the CHAR arrays, however, for other
        % SDS files the variable seems to contain only 0/1 flags indicating
        % whether a quantity is included. So, we cannot rely on the values
        % to be present. First reset indices, and then recalculate them.
        maxmin = maxmin(1:nflds+3,:,:)~=0;
        maxmin = cumsum(maxmin,1).*maxmin;
    end
end
%
for j=1:length(Out1)
    OutIn=Out1(j);
    OutIn.Exper=eName;
    if isempty(OutIn.Char)
    elseif ~any(strcmp(OutIn.Char,chars)) % waqua('exists',FI,FI.Experiment(e).Name,OutIn.Char)
        OutIn=[];
    else
        switch OutIn.Name
            case {'weir crest level', 'weir sill level (low M/N)', 'weir sill level (high M/N)'}
                % remove old style weirs if new style is present
                if ~strcmp(OutIn.Char,'MESH_WEIRS')  && any(strcmp('MESH_WEIRS',chars))
                    OutIn=[];
                end
            case {'weir flow condition', 'weir unit discharge', 'weir overflow depth', 'weir velocity', 'weir head loss'}
                % remove time varying weir quantities if the weir location information is missing
                if ~any(strcmp('MESH_WEIRS',chars))
                    OutIn=[];
                end
            case {'wind','pressure'}
                switch stagwflag
                    case 11
                        OutIn.Loc = 'd';
                        OutIn.ReqLoc = 'd';
                    case 13
                        % WIND at u/v, PRESSURE at z - u/v to z averaging done in waquaio.m
                    otherwise
                        % caught above
                end
                if strcmp(OutIn.Name,'wind') && istress==1
                    OutIn.Name = 'wind stress';
                    OutIn.Units = 'N/m2';
                end
            case 'horizontal velocity (station)'
                if kflag
                    OutIn.DimFlag(K_)=0;
                    OutIn.Name='velocity (station)';
                end
            case 'horizontal velocity'
                if kflag
                    OutIn.DimFlag(K_)=0;
                    OutIn.Name='velocity';
                end
            case 'hydrodynamic grid'
                if kflag
                    OutIn.DimFlag(T_)=0;
                    OutIn.DimFlag(K_)=0;
                end
            case {'velocity','vertical velocity (station)'}
                if kflag
                    OutIn=[];
                end
            case {'discharge potential','unit discharge','head'}
                if dimen(18)>1
                    OutIn=[];
                end
            case {'--inc'}
                NClass = waqua('read',FI,eName,'CONTROL_FLOW_INCREMENTAL_CLASSNBND');
                Classes = waqua('read',FI,eName,'CONTROL_FLOW_INCREMENTAL_CLASSBND');
                Classes = reshape(Classes,[max(NClass) length(NClass)]);
                Quants = {'classified water depth','classified water level','classified u velocity', ...
                        'classified v velocity','classified velocity magnitude','classified velocity angle'};
                if NClass>6
                    ui_message('More incremental classes than expected: new SIMONA feature?')
                    NClass = 6;
                end
                for c = 1:length(NClass)
                    if NClass(c)>0
                        OutIn.Name = Quants{c};
                        OutIn.WaqIO = sprintf('%s-%i',OutIn.Char,c);
                        i=i+1;
                        Out(i) = OutIn;
                    end
                end
                OutIn = [];
            case {'--maxmin'}
                if nflds>0
                    var = OutIn.Char(28:end);
                    f = strmatch(var,maxmin_var,'exact');
                    MAMI_upper = OutIn.Char(18:20);
                    MAMI_lower = lower(OutIn.Char(18:20));
                    MAMI_index = strmatch(MAMI_upper,{'MAX','MIN'});
                    for f2 = 1:nflds+3
                        if f2==maxmin_qcons
                            cf2 = length(Subs);
                        else
                            cf2 = 1;
                        end
                        for c2 = 1:cf2
                            if maxmin(f2,f-1,MAMI_index)>0 % note shift in second dimension because 'time' is not included in second dimension
                                if f==f2
                                    if f==maxmin_qcons
                                        if c==c2
                                            OutIn.Name  = [MAMI_lower,'imum ',Subs{c}];
                                            OutIn.Units = SubsUnit{c};
                                        else
                                            OutIn.Name  = [Subs{c2},' at ',MAMI_lower,'imum ',Subs{c}];
                                            OutIn.Units = SubsUnit{c2};
                                        end
                                    else
                                        OutIn.Name  = [MAMI_lower,'imum ',maxmin_quant{f}];
                                        OutIn.Units = maxmin_unit{f};
                                    end
                                else
                                    if f==maxmin_qcons
                                        OutIn.Name  = [maxmin_quant{f2},' at ',MAMI_lower,'imum ',Subs{c}];
                                        OutIn.Units = maxmin_unit{f2};
                                    elseif f2==maxmin_qcons
                                        OutIn.Name  = [Subs{c2},' at ',MAMI_lower,'imum ',maxmin_quant{f}];
                                        OutIn.Units = SubsUnit{c2};
                                    else
                                        OutIn.Name  = [maxmin_quant{f2},' at ',MAMI_lower,'imum ',maxmin_quant{f}];
                                        OutIn.Units = maxmin_unit{f2};
                                    end
                                end
                                OutIn.WaqIO = sprintf('%s-%i',OutIn.Char,maxmin(f2,f-1,MAMI_index));
                                i=i+1;
                                Out(i)=OutIn;
                            end
                        end
                    end
                end
                OutIn=[];
            case {'--substances','--substances (station)'}
                if kflag
                    OutIn.DimFlag(K_)=0;
                end
                if isempty(Subs)
                    OutIn=[];
                else
                    ct=OutIn.Name(13:end);
                    nm='subst:';
                    if strcmp(ct,' (station)')
                        nm='stsubst:';
                    end
                    for s=1:length(Subs)-1
                        OutIn.WaqIO=[nm Subs{s}];
                        OutIn.Name=[strtrim(lower(Subs{s})) ct];
                        OutIn.Exper=eName;
                        i=i+1;
                        Out(i)=OutIn;
                    end
                    OutIn.WaqIO=[nm Subs{end}];
                    OutIn.Name=[strtrim(lower(Subs{end})) ct];
                end
            case {'velocity (barrier)','flow-through height (barrier)','energy loss (barrier)'}
                dimen=waqua('readsds',FI,eName,'MESH_IDIMEN');
                % 18: KMAX
                % 24: NBARUV = NumBarrierPoints
                kmax=dimen(18);
                nbaruv=dimen(24);
                %
                iconta=waqua('readsds',FI,eName,'CONTROL_FLOW_ICONTA');
                %  6: NOWL = num waterlevel stat
                %  7: NOCUR = num current stat
                %  8: NTRA = num u-transp cross
                %  9: NTRAV = num v-transp cross
                nowl=iconta(6);
                nocur=iconta(7);
                ntra=iconta(8);
                ntrav=iconta(9);
                %
                if waqua('size',FI,eName,'TIMEHISTORIES_FLOW_TIMHIS') == ...
                        nowl+nocur*kmax*3+ntra*2+ntrav*2+nbaruv*5+nocur*(3*kmax+2)
                    OutIn=[];
                end
        end
        if ~isempty(OutIn)
            sz=getsize(FI,OutIn);
            if sz(ST_)<1 && OutIn.DimFlag(ST_)
                OutIn=[];
            end
            if sz(T_)<1 && ~isempty(OutIn) && OutIn.DimFlag(T_)
                OutIn=[];
            end
        end
    end
    if ~isempty(OutIn)
        i=i+1;
        Out(i)=OutIn;
    end
end

%======================= SET USEGRID OPTIONS ==================================
if length(FI.Experiment)==1
    for i=1:length(Out)
        switch Out(i).ReqLoc
            case 'd'
                Out(i).UseGrid=1;
            case 'z'
                Out(i).UseGrid=2;
            otherwise
                Out(i).UseGrid=0;
        end
    end
end

%---- set SpatType field
Out=spatiallystructured(Out);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if strcmp(Props.Name,'water level stations') || ...
        strcmp(Props.Name,'velocity stations')
    CHAR = ['flowstat-' Props.WaqIO(1:2)];
    S=deblank(waquaio(FI,Props.Exper,CHAR));
    sz(M_)=length(S);
elseif Props.DimFlag(M_) || Props.DimFlag(N_) || Props.DimFlag(K_)
    if waqua('exists',FI,Props.Exper,'MESH_IDIMEN')
        dimen=waqua('readsds',FI,Props.Exper,'MESH_IDIMEN');
        if Props.DimFlag(M_)
            sz(M_)=dimen(2);
        end
        if Props.DimFlag(N_)
            sz(N_)=dimen(3);
        end
        if Props.DimFlag(K_)
            sz(K_)=dimen(18)+strcmp(Props.Loc3D,'i');
        end
    else
        dimen=waqua('readsds',FI,Props.Exper,'MESH01_SPECIFIC_IDIMEN');
        if Props.DimFlag(M_)
            sz(M_)=dimen(1);
        end
        if Props.DimFlag(N_)
            sz(N_)=dimen(2);
        end
    end
end
if Props.DimFlag(T_)
    ExpName=Props.Exper;
    e=strcmp(ExpName,{FI.Experiment.Name});
    CharName={FI.Experiment(e).Char.Name}';
    c=strcmp(Props.Char,CharName);
    if strcmp(Props.Char,'INCREMENTAL_OUTPUT_TIMIDX')
        sz(T_)=FI.Entry(FI.Experiment(e).Char(c).DataIndex).BlockSize/3;
    else
        sz(T_)=FI.Entry(FI.Experiment(e).Char(c).DataIndex).NumIter;
    end
end
if Props.DimFlag(ST_)
    sz(ST_)=length(readsts(FI,Props));
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)

%======================== SPECIFIC CODE =======================================
refdate = waquaio(FI,Props.Exper,'refdate');
%
switch Props.Char
    case {'SOLUTION_WIND','SOLUTION_PRESS'}
        T = refdate + waqua('read',FI,Props.Exper,'CONTROL_SVWP_WINTIM')/1440;
        if ~isequal(t,0)
            T = T(t);
        end
    otherwise
        if strcmp(Props.Char,'INCREMENTAL_OUTPUT_TIMIDX')
            Info=waqua('read',FI,Props.Exper,'CONTROL_FLOW_INCREMENTAL_TIMES');
            T = Info(1):Info(2):Info(3);
            if isequal(t,0)
                T=refdate+T/1440;
            else
                T=refdate+T(t)/1440;
            end
        else
            Info=waqua('read',FI,Props.Exper,Props.Char,[]);
            if isequal(t,0)
                T=refdate+Info.SimTime/1440;
            else
                T=refdate+Info.SimTime(t)/1440;
            end
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props)

%======================== SPECIFIC CODE =======================================
switch Props.WaqIO
    case {'wlstat','wl-stat'}
        S=deblank(waquaio(FI,Props.Exper,'flowstat-wl'));
    case {'umag-stat','u-stat','v-stat','uv-stat','z-stat','w-stat'}
        S=deblank(waquaio(FI,Props.Exper,'flowstat-uv'));
    case {'mq-stat','cq-stat'}
        Su=deblank(waquaio(FI,Props.Exper,'flowcrs-u'));
        Sv=deblank(waquaio(FI,Props.Exper,'flowcrs-v'));
        S=cat(1,Su,Sv);
    case {'q-barp','wl-lbarp','vel-lbarp','wl-hbarp','vel-hbarp','vel-barp','hg-barp','enl-barp'}
        S=deblank(waquaio(FI,Props.Exper,'barrierpoints'));
    case {'sl-bar','gl-bar','wd-bar'}
        S=deblank(waquaio(FI,Props.Exper,'barriers'));
    otherwise
        if ~any(strcmp('stsubst',Props.WaqIO))
            S=deblank(waquaio(FI,Props.Exper,'transtat'));
        end
end
% -----------------------------------------------------------------------------
