function varargout=nfs_tritonfil(FI,domain,field,cmd,varargin)
%NFS_TRITONFIL QP support for TRITON output files.
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

F_ = [];
subf=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    F_ = length(idx)+1;
    idx{F_}=varargin{1};
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

% select appropriate timestep ...
sz=getsize(FI,Props);
for i=1:length(sz)
    if DimFlag(i)
        if isequal(idx{i},0)
            idx{i}=1:sz(i);
        end
    end
end
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
elseif ~Props.DimFlag(T_)
    idx{T_} = 1;
end

% read grid ...
x=[];
y=[];
if XYRead
    switch Props.Group
        case {'GRID','BOTTOMS'}
            % grid
            gidx = idx;
            gidx{M_} = [gidx{M_} gidx{M_}(end)+1];
            nM = length(gidx{M_});
            gidx{N_} = [gidx{N_} gidx{N_}(end)+1];
            nN = length(gidx{N_});
            %
            x = vs_get(FI,'GRID','X',gidx(M_),'quiet');
            if ~DataInCell & Props.NVal>0
                x = (x(1:end-1)+x(2:end))/2;
                nN = nN-1;
                nM = nM-1;
            end
            x = repmat(x,1,nN);
            %
            y = vs_get(FI,'GRID','Y',gidx(N_),'quiet')';
            if ~DataInCell & Props.NVal>0
                y = (y(1:end-1)+y(2:end))/2;
            end
            y = repmat(y,nM,1);
            %
        case {'COARSEGRID','COARSEGRIDTS','COARSEGRIDTD','COARSEGRIDSD'}
            % coarse grid
            x = vs_get(FI,'COARSEGRID','X-CG',idx(M_),'quiet');
            x = repmat(x,1,length(idx{N_}));
            y = vs_get(FI,'COARSEGRID','Y-CG',idx(N_),'quiet')';
            y = repmat(y,length(idx{M_}),1);
        case {'POINTS','POINTSTS','POINTSTD','POINTSSD'}
            % points
            x = vs_get(FI,'POINTS','X-PTS',idx(ST_),'quiet');
            y = vs_get(FI,'POINTS','Y-PTS',idx(ST_),'quiet');
        otherwise
            % rays
            rayid = Props.Group(end-1:end);
            x = vs_get(FI,['RAY-' rayid],['XRAY-' rayid],idx(M_),'quiet');
            y = vs_get(FI,['RAY-' rayid],['YRAY-' rayid],idx(M_),'quiet');
    end
    %
    % rotate grid
    %
    Info = vs_disp(FI,'GRIDSETTINGS','ANGLE');
    if isstruct(Info)
        Info = vs_get(FI,'GRIDSETTINGS','*','quiet');
        xtmp = Info.XPC + x*cos(Info.ANGLE)-y*sin(Info.ANGLE);
        y    = Info.YPC + y*cos(Info.ANGLE)+x*sin(Info.ANGLE);
        x    = xtmp;
        xtmp = [];
    end
end

val1=[];
val2=[];
if DataRead & Props.NVal>0
    switch Props.Group
        case {'GRID','BOTTOMS'}
            % grid
            eIndex = idx([M_ N_ F_]);
        case {'COARSEGRID','COARSEGRIDTS','COARSEGRIDTD','COARSEGRIDSD'}
            % coarse grid
            eIndex = idx([M_ N_ F_]);
        case {'POINTS','POINTSTS','POINTSTD','POINTSSD'}
            % points
            eIndex = idx([ST_ F_]);
        otherwise
            % rays
            eIndex = idx([M_ F_]);
    end
    val1 = vs_let(FI,Props.Group,idx(T_),Props.Val1,eIndex,'quiet');
    if ~isempty(Props.Val2)
        val2 = vs_let(FI,Props.Group,idx(T_),Props.Val2,eIndex,'quiet');
    end
end

if isequal(Props.Name,'bed level')
    val1 = -val1;
end

%========================= GENERAL CODE =======================================

% reshape if a single timestep is selected ...
if ~DimFlag(T_) | (DimFlag(T_) & isequal(size(idx{T_}),[1 1]))
    sz=size(x); sz=[sz(2:end) 1];
    if Props.NVal>0
        sz=size(val1); sz=[sz(2:end) 1];
        switch Props.NVal
            case 1
                val1=reshape(val1,sz);
            case 2
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
end
switch Props.NVal
    case 1
        Ans.Val=val1;
    case 2
        Ans.XComp=val1;
        Ans.YComp=val2;
end

% read time ...
if DimFlag(T_)
    T=readtim(FI,Props,idx{T_});
    Ans.Time=T;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Domains=domains(FI)
Domains = {};
if isstruct(vs_disp(FI,'GRID',[]))
    Domains{end+1,1} = 'grid';
end
if isstruct(vs_disp(FI,'COARSEGRID',[]))
    Domains{end+1,1} = 'coarse grid';
end
if isstruct(vs_disp(FI,'POINTS',[]))
    Domains{end+1,1} = 'points';
end
i = 1;
while isstruct(vs_disp(FI,sprintf('RAY-%2.2i',i),[]))
    Domains{end+1,1} = deblank(vs_get(FI,sprintf('RAY-%2.2i',i),sprintf('NAMERAY-%2.2i',i),'quiet'));
    i = i+1;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'Group'          'Val1'    'Val2'  'SubFld'};
%
dms = domains(FI);
if domain == 0
    domain = length(dms);
end
switch dms{domain}
    case 'grid'
        DataProps={...
            'grid'                        ''       [0 0 1 1 0]  0         0    'GRID'           'X'       'Y'      []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'bed level'                  'm'       [0 0 1 1 0]  1         1    'BOTTOMS'        'BOTTOM'  ''       []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []      };
    case 'coarse grid'
        DataProps={...
            'grid'                        ''       [0 0 1 1 0]  0         0    'COARSEGRID'     'X-CG'    'Y-CG'   []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'water level'                'm'       [7 0 1 1 0]  0         1    'COARSEGRIDTS'   'ZETA-CG' ''       []
            'velocity'                  'm/s'      [7 0 1 1 0]  0         2    'COARSEGRIDTS'   'U-CG'    'V-CG'   []
            'bed level'                  'm'       [7 0 1 1 0]  0         1    'COARSEGRIDTS'   'BOTTOM-CG' ''     []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'H_{1/3}'                    'm'       [0 0 1 1 0]  0         1    'COARSEGRIDTD'   'H13-CG'  ''       []
            'H_{rms}'                    'm'       [0 0 1 1 0]  0         1    'COARSEGRIDTD'   'HRMS-CG' ''       []
            'H_{1%}'                     'm'       [0 0 1 1 0]  0         1    'COARSEGRIDTD'   'H1PERCENT-CG' ''  []
            'H_{2%}'                     'm'       [0 0 1 1 0]  0         1    'COARSEGRIDTD'   'H2PERCENT-CG' ''  []
            'T_m'                        's'       [0 0 1 1 0]  0         1    'COARSEGRIDTD'   'TM-CG'   ''       []
            'T_{1/3}'                    's'       [0 0 1 1 0]  0         1    'COARSEGRIDTD'   'T13-CG'  ''       []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'HM0'                        'm'       [0 0 1 1 0]  0         1    'COARSEGRIDSD'   'HM0-CG'  ''       []
            'T_{m-1,0}'                  's'       [0 0 1 1 0]  0         1    'COARSEGRIDSD'   'TM-10-CG' ''      []
            'T_{m01}'                    's'       [0 0 1 1 0]  0         1    'COARSEGRIDSD'   'TM01-CG' ''       []
            'T_p'                        's'       [0 0 1 1 0]  0         1    'COARSEGRIDSD'   'TP-CG'   ''       []
            'S_{energy}'               'J/m^2/Hz'  [0 0 1 1 0]  0         1    'COARSEGRIDSD'   'Senergy-CG' ''    'f'
            'S_{var}'                    'm^2/Hz'  [0 0 1 1 0]  0         1    'COARSEGRIDSD'   'Svar-CG' ''       'f'
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []      };
    case 'points'
        DataProps={...
            'point'                       ''       [0 3 0 0 0]  0         0    'POINTS'         'X-PTS'   'Y-PTS'  []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'water level'                'm'       [7 3 0 0 0]  0         1    'POINTSTS'       'ZETA-PTS' ''      []
            'velocity'                  'm/s'      [7 3 0 0 0]  0         2    'POINTSTS'       'U-PTS'   'V-PTS'  []
            'bed level'                  'm'       [7 3 0 0 0]  0         1    'POINTSTS'       'BOTTOM-PTS' ''    []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'H_{1/3}'                    'm'       [0 3 0 0 0]  0         1    'POINTSTD'       'H13-PTS' ''       []
            'H_{rms}'                    'm'       [0 3 0 0 0]  0         1    'POINTSTD'       'HRMS-PTS' ''      []
            'H_{1%}'                     'm'       [0 3 0 0 0]  0         1    'POINTSTD'       'H1PERCENT-PTS' '' []
            'H_{2%}'                     'm'       [0 3 0 0 0]  0         1    'POINTSTD'       'H2PERCENT-PTS' '' []
            'T_m'                        's'       [0 3 0 0 0]  0         1    'POINTSTD'       'TM-PTS'  ''       []
            'T_{1/3}'                    's'       [0 3 0 0 0]  0         1    'POINTSTD'       'T13-PTS' ''       []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'HM0'                        'm'       [0 3 0 0 0]  0         1    'POINTSSD'       'HM0-PTS' ''       []
            'T_{m-1,0}'                  's'       [0 3 0 0 0]  0         1    'POINTSSD'       'TM-10-PTS' ''     []
            'T_{m01}'                    's'       [0 3 0 0 0]  0         1    'POINTSSD'       'TM01-PTS' ''      []
            'T_p'                        's'       [0 3 0 0 0]  0         1    'POINTSSD'       'TP-PTS'  ''       []
            'S_{energy}'               'J/m^2/Hz'  [0 3 0 0 0]  0         1    'POINTSSD'       'Senergy-PTS' ''   'f'
            'S_{var}'                    'm^2/Hz'  [0 3 0 0 0]  0         1    'POINTSSD'       'Svar-PTS' ''      'f'
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []      };
    otherwise
        i = 1;
        while ismember(dms{i},{'grid','coarse grid','points'})
            i = i+1;
        end
        raynr = domain-i+1;
        %
        DataProps={...
            'grid'                        ''       [0 0 1 1 0]  0         0    'RAY-xx'         'XRAY-xx' 'YRAY-xx' []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'water level'                'm'       [7 0 1 1 0]  0         1    'RAYTS-xx'       'ZETARAY-xx' ''    []
            'velocity'                  'm/s'      [7 0 1 1 0]  0         2    'RAYTS-xx'       'URAY-xx' 'VRAY-xx' []
            'bed level'                  'm'       [7 0 1 1 0]  0         1    'RAYTS-xx'       'BOTTOMRAY-xx' ''  []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'H_{1/3}'                    'm'       [0 0 1 1 0]  0         1    'RAYTD-xx'       'H13-xx'  ''       []
            'H_{rms}'                    'm'       [0 0 1 1 0]  0         1    'RAYTD-xx'       'HRMS-xx' ''       []
            'H_{1%}'                     'm'       [0 0 1 1 0]  0         1    'RAYTD-xx'       'H1PERCENT-xx' ''  []
            'H_{2%}'                     'm'       [0 0 1 1 0]  0         1    'RAYTD-xx'       'H2PERCENT-xx' ''  []
            'T_m'                        's'       [0 0 1 1 0]  0         1    'RAYTD-xx'       'TM-xx'   ''       []
            'T_{1/3}'                    's'       [0 0 1 1 0]  0         1    'RAYTD-xx'       'T13-xx'  ''       []
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []
            'HM0'                        'm'       [0 0 1 1 0]  0         1    'RAYSD-xx'       'HM0-xx'  ''       []
            'T_{m-1,0}'                  's'       [0 0 1 1 0]  0         1    'RAYSD-xx'       'TM-10-xx' ''      []
            'T_{m01}'                    's'       [0 0 1 1 0]  0         1    'RAYSD-xx'       'TM01-xx' ''       []
            'T_p'                        's'       [0 0 1 1 0]  0         1    'RAYSD-xx'       'TP-xx'   ''       []
            'S_{energy}'               'J/m^2/Hz'  [0 0 1 1 0]  0         1    'RAYSD-xx'       'Senergy-xx' ''    'f'
            'S_{var}'                    'm^2/Hz'  [0 0 1 1 0]  0         1    'RAYSD-xx'       'Svar-xx' ''       'f'
            '-------'                     ''       [0 0 0 0 0]  0         0    ''               ''        ''       []      };
        %
        for c = 1:length(DataProps(:))
            if ischar(DataProps{c})
                Str = DataProps{c};
                if length(Str)>2 & strcmp(Str(end-1:end),'xx')
                    Str(end-1:end) = sprintf('%2.2i',raynr);
                end
                DataProps{c} = Str;
            end
        end
end

Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE ADD ================================
AllGroups = {Out.Group};
Info = vs_disp(FI,Out(1).Group,Out(1).Val1);
SzGrid = Info.SizeDim;
if strcmp(Out(1).Group,'GRID')
    Info = vs_disp(FI,Out(1).Group,Out(1).Val2);
    SzGrid(1,2) = Info.SizeDim;
    SzGrid = SzGrid-1;
elseif strcmp(Out(1).Group,'COARSEGRID')
    Info = vs_disp(FI,Out(1).Group,Out(1).Val2);
    SzGrid(1,2) = Info.SizeDim;
end
UniGroups = unique(AllGroups);
for ig=1:length(UniGroups)
    Grp = UniGroups{ig};
    I = strmatch(Grp,AllGroups,'exact');
    ElmsUsed = unique(cat(2,{Out(I).Val1},{Out(I).Val2}));
    if ~isempty(Grp)
        %
        % If group name is not empty (i.e. not a separator) ...
        %
        ElmsFound = vs_disp(FI,Grp);
        if iscell(ElmsFound)
            %
            % If group exists ...
            %
            AutoDetect = setdiff(ElmsFound,ElmsUsed);
            for ie = 1:length(AutoDetect)
                Info = vs_disp(FI,Grp,AutoDetect{ie});
                if isequal(Info.SizeDim,SzGrid) & ismember(Info.TypeVal,[3 4 5])
                    Out(end+1) = Out(3);
                    %
                    % Set name ...
                    %
                    if ~isempty(Info.ElmDescription)
                        Out(end).Name = Info.ElmDescription;
                    else
                        Out(end).Name = AutoDetect{ie};
                    end
                    %
                    % Set unit ...
                    %
                    if ~isempty(Info.ElmUnits)
                        Out(end).Units = strtrim(Info.ElmUnits(2:end-1));
                    else
                        Out(end).Units = '';
                    end
                    %
                    % Set group and element names ...
                    %
                    Out(end).Group = Grp;
                    Out(end).Val1 = AutoDetect{ie};
                    %
                    % Remove time dimension ...
                    %
                    if ~isequal(Grp,Out(3).Group)
                        Out(end).DimFlag(T_) = 0;
                    end
                end
            end
        end
    end
end

%======================== SPECIFIC CODE CHANGE ================================
for i=size(Out,1):-1:1
    InfoG=vs_disp(FI,Out(i).Group,[]);
    Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info)
        % remove references to non-stored data fields
        Out(i)=[];
    elseif InfoG.SizeDim==0
        Out(i)=[];
    end
end

%======================= SET USEGRID OPTIONS ==================================
if ~strcmp(dms{domain},'points')
    [Out(:).UseGrid]=deal(1);
else
    [Out(:).UseGrid]=deal([]);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
%======================== SPECIFIC CODE =======================================
if ~isempty(Props.SubFld)
    switch Props.Group
        case {'COARSEGRIDSD'}
            Freqs = vs_get(FI,'COARSEGRIDSD','FREQ-CG','quiet');
        case {'POINTSSD'}
            Freqs = vs_get(FI,'POINTSSD','FREQ-PTS','quiet');
        otherwise
            Freqs = vs_get(FI,Props.Group,['FREQ-' Props.Val1(end-1:end)],'quiet');
    end
    if nargin==3 & f~=0
        Freqs = Freqs(f);
    end
    NFrq = length(Freqs);
    for i = NFrq:-1:1
        if Freqs(i)<0.1
            subf{i}=sprintf('%g mHz',1000*Freqs(i));
        else
            subf{i}=sprintf('%g Hz',Freqs(i));
        end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
%======================== SPECIFIC CODE =======================================
switch Props.Group
    case {'GRID','BOTTOMS'}
        Info = vs_disp(FI,'GRID','X');
        sz(M_) = Info.SizeDim-1;
        Info = vs_disp(FI,'GRID','Y');
        sz(N_) = Info.SizeDim-1;
    case {'COARSEGRID','COARSEGRIDTS','COARSEGRIDTD','COARSEGRIDSD'}
        Info = vs_disp(FI,'COARSEGRID','X-CG');
        sz(M_) = Info.SizeDim;
        Info = vs_disp(FI,'COARSEGRID','Y-CG');
        sz(N_) = Info.SizeDim;
    case {'POINTS','POINTSTS','POINTSTD','POINTSSD'}
        Info = vs_disp(FI,'POINTS','X-PTS');
        sz(ST_) = Info.SizeDim;
    otherwise
        Info = vs_disp(FI,Props.Group,Props.Val1);
        sz(M_) = Info.SizeDim(1);
        sz(N_) = 1;
end
if Props.DimFlag(T_)
    Info = vs_disp(FI,Props.Group,[]);
    sz(T_) = Info.SizeDim;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
T=[];
if Props.DimFlag(T_)
    switch Props.Group
        case {'COARSEGRIDTS'}
            TElm = 'TIME-CG';
        case {'POINTSTS'}
            TElm = 'TIME-PTS';
        otherwise
            TElm = ['TIMERAY-' Props.Group(end-1:end)];
    end
    T = vs_let(FI,Props.Group,{t},TElm,'quiet')/(24*3600);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
switch Props.Group
    case {'POINTS','POINTSTS','POINTSTD','POINTSSD'}
        S = vs_get(FI,'POINTS','NAME-PTS','quiet');
end
S=cellstr(S);
% -----------------------------------------------------------------------------
