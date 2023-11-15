function varargout=pharosfil(FI,domain,field,cmd,varargin)
%PHAROSFIL QP support for Pharos files.
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
            [varargout{1:2}]=ph_options(FI,cmd,varargin{:});
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
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        Parent=varargin{1};
        Ops=varargin{2};
        hOld=varargin{3};
        Station=varargin{4};
        Location = ph_get(FI,'SEICH_loc',{Station},'Point_nr','quiet');
        %
        Freqs = ph_let(FI,'SEICH_def','FREQ','quiet');
        Freqs = Freqs(Freqs>0)';
        %
        Name = Props.Data.Name;
        %Get data for Location+1 because the Location is zero-based (writing
        %program is a C-program).
        data = qpread(FI,Props.Data,'data',0,Location+1);
        %
        hNew=line(Freqs,data.Val,'color',Ops.colour, ...
            'linestyle',Ops.linestyle, ...
            'linewidth',Ops.linewidth, ...
            'marker',Ops.marker, ...
            'markersize',Ops.markersize, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
        setappdata(Parent,'AxesType','<blocking>')
        LocationStr=readsts(FI,Props,Station);
        set(get(Parent,'title'),'string',LocationStr,'interpreter','none')
        set(get(Parent,'xlabel'),'string','frequency (Hz) \rightarrow')
        if isempty(Props.Units)
            set(get(Parent,'ylabel'),'string',[Name,' \rightarrow'])
        else
            set(get(Parent,'ylabel'),'string',[Name,' (',Props.Units,') \rightarrow'])
        end
        varargout={hNew FI};
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
NRun = 1;
if ~DimFlag(T_)
    if ~isempty(Props.SubFld)
        idx{T_}=Props.SubFld;
    else
        idx{T_}=1;
    end
else
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
    switch Props.Name
        case 'wave image'
            t = idx{T_};
            if ~isempty(Props.SubFld)
                Info = ph_disp(FI,Props.Group,[]);
                NRun = Info.SizeDim;
                if Props.SubFld>NRun
                    idx{T_}=1:NRun;
                else
                    idx{T_}=Props.SubFld;
                end
            else
                idx{T_}=1;
            end
    end
end

% generate output ...
if XYRead
    if strcmp(Props.Geom,'PNT')
        % skip until after reading the data
    else
        X=ph_let(FI,'GRID_coor',{0},'X_coor','quiet');
        Y=ph_let(FI,'GRID_coor',{0},'Y_coor','quiet');
        XYZ=[X Y];
        %
        PointIndices=ph_let(FI,'MESH_2','KMESHC',{0},'quiet');
        INPELM=ph_let(FI,'MESH_1','INPELM',{0},'quiet');
        INELEM=ph_let(FI,'MESH_1','INELEM',{0},'quiet');
        %
        % INPELM contains the number of points per element
        % INELEM contains the number of elements
        % The last "element set" contains the triangles.
        % The other "element sets" contain the boundary conditions.
        %
        switch Props.Name
            case {'closed boundaries','open boundary','transmission boundaries'}
                LELNR=ph_let(FI,'GRID_adm',{1},'LELNR',{0},'quiet');
                LELNR=LELNR(cumsum(INELEM))';
                elm={};
                ind=0;
                for i=1:length(INPELM)-1
                    if LELNR(i)==6 & strcmp(Props.Name,'closed boundaries')
                        elm{i}=transpose(reshape(PointIndices(ind+(1:INPELM(i)*INELEM(i))),[INPELM(i) INELEM(i)]));
                    elseif LELNR(i)==3 & strcmp(Props.Name,'open boundary')
                        bnd=reshape(PointIndices(ind+(1:INPELM(i))),[INPELM(i) 1]);
                        elm{i}=[bnd(1:end-1) bnd(2:end)];
                    elseif LELNR(i)==4 & strcmp(Props.Name,'transmission boundaries')
                        bnd=reshape(PointIndices(ind+(1:INPELM(i))),[INPELM(i) 1]);
                        elm{i}=[bnd(1:end-1) bnd(2:end)];
                    else
                        elm{i}=zeros(0,2);
                    end
                    ind=ind+INPELM(i)*INELEM(i);
                end
                %
                Ans.XY=XYZ;
                Ans.SEG=cat(1,elm{:});
            otherwise
                if idx{M_}~=0
                    XYZ = XYZ(idx{M_},:);
                end
                Ans.X = XYZ(:,1);
                Ans.Y = XYZ(:,2);
                %
                ind=sum(INPELM(1:end-1).*INELEM(1:end-1));
                TRI=transpose(reshape(PointIndices(ind+(1:INPELM(end)*INELEM(end))),[INPELM(end) INELEM(end)]));
                %
                if idx{M_}~=0
                    Translate=zeros(sz(M_),1);
                    Translate(idx{M_})=1:length(idx{M_});
                    TRI = Translate(TRI);
                    TRI = TRI(all(TRI,2),:);
                end
                Ans.FaceNodeConnect=TRI;
                Ans.ValLocation='NODE';
        end
    end
end

switch Props.Name
    case 'wave image'
        % need all points for wave image to determine phase associated with
        % the maximum amplitude in the whole field
        m = idx{M_};
        idx{M_}=0;
end

RPAR=ph_get(FI,'INFO',{1},'RPAR',{0},'quiet');
WL = RPAR(4);
switch Props.NVal
    case 0
    case 1
        if strcmp(Props.Geom,'PNT')
            Location = ph_get(FI,'SEICH_loc',idx(ST_),'Point_nr','quiet');
            %
            Freqs = ph_let(FI,'SEICH_def','FREQ','quiet');
            Freqs = Freqs(Freqs>0)'; % TODO: Add Freqs to Ans and plot ...
            %
            %Get data for Location+1 because the Location is zero-based
            %(writing program is a C-program).
            Ans = qpread(FI,Props.Data,'data',0,Location+1);
            Ans.X = ph_get(FI,'SEICH_loc',idx(ST_),'Xp','quiet');
            Ans.Y = ph_get(FI,'SEICH_loc',idx(ST_),'Yp','quiet');
        else
            %H=ph_get(FI,'GRID','H_depth');
            switch Props.Name
                case 'relative breaking intensity'
                    Val1=ph_let(FI,Props.Group,{idx{T_}},Props.Val1,{0},'quiet');
                otherwise
                    Val1=ph_let(FI,Props.Group,{idx{T_}},Props.Val1,{idx{M_}},'quiet');
            end
            Val2=[];
            if ~isempty(Props.Val2)
                Val2=ph_let(FI,Props.Group,{idx{T_}},Props.Val2,{idx{M_}},'quiet');
            end
            switch Props.Name
                case 'water level'
                    Ans.Val = Val1;
                    Ans.Val(:) = WL;
                case 'bed level'
                    Ans.Val = WL - Val1;
                case 'wave height'
                    Amp = sqrt(Val1.^2 + Val2.^2);
                    Ans.Val = 2*Amp;
                case 'wave image'
                    Amp   = sqrt(Val1.^2 + Val2.^2);
                    Phase = atan2(Val2,Val1);
                    if NRun>1
                        Info = ph_disp(FI,'SPECTRAL-INFO',[]);
                        if isstruct(Info) && Info.SizeDim>0
                            Freqs = ph_let(FI,'SPECTRAL-INFO',{0},'SPECTRAL-RPAR',{1},'quiet');
                            Weights = ph_let(FI,'SPECTRAL-INFO',{0},'WEIGHTS',{0},'quiet')';
                            Weights = Weights(:);
                            if size(Amp,1)~=NRun
                                Weights = Weights(idx{T_});
                                Weights(:)=1; % reset weights to one, to generate same wave image from combined file as from individual file.
                            end
                            for i=1:size(Amp,1)
                                Amp(i,:) = Weights(i)*Amp(i,:);
                            end
                        else
                            error('Combination not implemented.');
                            Freqs = ph_let(FI,'INFO','RPAR',{1},'quiet');
                        end
                        Per = 1./Freqs;
                        Per = repmat(Per(:)',NRun/length(Per),1);
                        Per = Per(:);
                        if size(Amp,1)~=NRun
                            Per = Per(idx{T_});
                        end
                        SeicheSpecial=0;
                    else
                        Per=Props.Period;
                        SeicheSpecial=size(Amp,1)>1;
                    end
                    %
                    if SeicheSpecial
                        %
                        [Ampmax,i] = max(Amp,[],2);
                        ii = sub2ind(size(Phase),1:size(Phase,1),i');
                        PhaseRef   = Phase(ii)';
                        % apply selection
                        if m~=0
                            Amp = Amp(:,m);
                            Phase = Phase(:,m);
                        end
                        Ans.Val = zeros(length(ii),size(Amp,2));
                        for i=1:length(ii)
                            Ans.Val(i,:) = Amp(i,:).*cos(Phase(i,:)-PhaseRef(i));
                        end
                    else
                        SumAmp=sum(Amp,1);
                        [Ampmax,i] = max(SumAmp);
                        PhaseRef   = Phase(:,i);
                        % apply selection
                        if m~=0
                            Amp = Amp(:,m);
                            Phase = Phase(:,m);
                        end
                        if ~DimFlag(T_)
                            t=0;
                        end
                        Ans.Val = zeros(length(t),size(Amp,2));
                        deltaT = Props.Period/Props.NSamples;
                        for i=1:length(t)
                            for f=1:size(Amp,1)
                                Ans.Val(i,:) = Ans.Val(i,:) + Amp(f,:).*cos(Phase(f,:)-PhaseRef(f)-2*pi*t(i)*deltaT/Per(f));
                            end
                        end
                        idx{T_} = t;
                    end
                case 'wave phase'
                    Phase   = atan2(Val2,Val1);
                    Ans.Val = Phase;
                case 'relative breaking intensity'
                    mVal1 = max(Val1(:));
                    if ~isequal(idx{M_},0)
                        Val1 = Val1(:,idx{M_});
                    end
                    Ans.Val = Val1/mVal1;
                otherwise
                    Ans.Val = Val1;
            end
        end
    otherwise
        Val1=ph_let(FI,Props.Group,{idx{T_}},Props.Val1,{idx{M_}},'quiet');
        Val2=ph_let(FI,Props.Group,{idx{T_}},Props.Val2,{idx{M_}},'quiet');
        switch Props.Name
            case 'maximum velocity'
                Ans.XComp=Val1.*cos(Val2);
                Ans.YComp=Val1.*sin(Val2);
            otherwise
                Ans.XComp=Val1;
                Ans.YComp=Val2;
        end
end

% read time ...
if DimFlag(T_)
    Ans.Time=readtim(FI,Props,idx{T_});
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)

%======================== SPECIFIC CODE =======================================
PropNames={'Name'             'Units'    'Geom'         'Coords' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Group'      'Val1'           'Val2'      'UseGrid' 'SubFld'};
DataProps={'grid'             ''         'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_coor'   'X_coor'         ''           1         ''
    'open boundary'            ''        'SEG'          'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_adm'   'LELNR'          ''           2         ''
    'transmission boundaries'  ''        'SEG'          'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_adm'   'LELNR'          ''           2         ''
    'closed boundaries'        ''        'SEG'          'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_adm'   'LELNR'          ''           2         ''
    '-------'                  ''        ''             ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'water level'              'm'       'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'H_depth'        ''           1         ''
    'bed level'                'm'       'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'H_depth'        ''           1         ''
    'water depth'              'm'       'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'H_depth'        ''           1         ''
    'velocity'                 'm/s'     'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          2     ''        ''    ''       'CURRENT'    'Ux_veloc'       'Uy_veloc'   1         ''
    %'relative radiation frequency' '1/s' 'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'CURRENT'    'Omega_rel'      ''           1         ''
    '-------'                  ''        ''             ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'relative breaking intensity' ''     'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'BREAKING'   'Gamma_b'        ''           1         'nfreq'
    'weighted mean wave height dir.' 'm' 'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'HS_dir'     'HS_directional' ''           1         'md'
    'wave height'              'm'       'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          'PHI_i'      1         'nrun'
    'wave height'              'm'       'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         'PHIs_i'     1         'sf'
    'wave phase'               ''        'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          'PHI_i'      1         'nrun'
    'wave phase'               ''        'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         'PHIs_i'     1         'sf'
    'wave image'               'm'       'UGRID2D-NODE' 'xy'     [7 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          'PHI_i'      1         'nrun'
    'wave image'               'm'       'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         'PHIs_i'     1         'sf'
    'maximum velocity'         'm/s'     'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'UMAX'           ''           1         'sf'
    'maximum velocity direction' ...
                               'radians' 'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'UDIR'           ''           1         'sf'
    %   'maximum velocity'     'm/s'     'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          2     ''        ''    ''       'SEICH_res'  'UMAX'           'UDIR'       1         'sf'
    'minimum velocity'         'm/s'     'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'UMIN'           ''           1         'sf'
    '-------'                  ''        ''             ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'wave number'              ''        'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'K_wave'         ''           1         'nfreq'
    'phase velocity'           'm/s'     'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'C_wave'         ''           1         'nfreq'
    'group velocity'           'm/s'     'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'Cg_wave'        ''           1         'nfreq'
    'potential (real part)'    ''        'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          ''           1         'nrun'
    'potential (imag part)'    ''        'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_i'          ''           1         'nrun'
    'seiches potential (real part)' ''   'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         ''           1         'sf'
    'seiches potential (imag part)' ''   'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_i'         ''           1         'sf'
    '-------'                  ''        ''             ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'weighted period Tm-1,0'   's'       'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SPECTRAL-PAR' 'TM10'         ''           1         ''
    'wave number based on Tm-1,0' ...
                               ''        'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SPECTRAL-PAR' 'KTM10'        ''           1         ''
    'radial frequency based on Tm-1,0' ...
                               ''        'UGRID2D-NODE' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SPECTRAL-PAR' 'OMEGATM10'    ''           1         ''
    '-------'                  ''        ''             ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''};
%======================== SPECIFIC CODE DIMENSIONS ============================
SkipGroup={};
SkipElem={'RKN_roughness'};
%
Info = ph_disp(FI,'GRID_coor',[]);
npnt = Info.SizeDim;
%
% Don't want to sort the Grps and therefore I don't use setdiff here ...
%
Grps=ph_disp(FI,[]);
Grps(ismember(Grps,SkipGroup))=[];
%
dpGrp=strmatch('Group',PropNames,'exact');
dpVal1=strmatch('Val1',PropNames,'exact');
dpVal2=strmatch('Val2',PropNames,'exact');
fld=size(DataProps,1);
for i=1:length(Grps)
    Grpi=strmatch(Grps{i},DataProps(:,dpGrp),'exact');
    InfoG=ph_disp(FI,Grps{i},[]);
    if ~isstruct(InfoG) || any(InfoG.SizeDim==0)
        continue
    end
    %
    % Don't want to sort the Elms and therefore I don't use setdiff here ...
    %
    Elms=ph_disp(FI,Grps{i});
    Elms(ismember(Elms,SkipElem))=[];
    %
    for j=1:length(Elms)
        Elmi=strmatch(Elms{j},DataProps(Grpi,dpVal1),'exact');
        Elmi=[Elmi;strmatch(Elms{j},DataProps(Grpi,dpVal2),'exact')];
        Info=ph_disp(FI,Grps{i},Elms{j});
        if (isempty(Elmi) || isempty(Grpi)) && isstruct(Info)
            if isequal(Info.SizeDim,npnt)
                if isempty(Info.ElmDescription)
                    edescr=sprintf('%s of %s',Elms{j},Grps{i});
                else
                    edescr=Info.ElmDescription;
                end
                eunit='';
                if ~isempty(Info.ElmUnits)
                    eunit=strtrim(Info.ElmUnits);
                    if isequal(eunit([1 end]),'[]')
                        eunit=eunit(2:end-1);
                    end
                end
                subf='';
                if ~isequal(InfoG.SizeDim,1)
                    subf='agd';
                end
                fld=fld+1;
                DataProps(fld,:)={edescr            eunit  'UGRID2D-NODE' 'xy' [0 0 6 0 0]  0          1     ''        ''    ''       Grps{i}      Elms{j}          ''           1         subf};
            end
        end
    end
    fld=fld+1;
    DataProps(fld,:)={'-------'                  ''   ''  ''   [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''};
end
%======================== DataProps conversion ================================
Out=cell2struct(DataProps,PropNames,2);
%======================== SPECIFIC CODE REMOVE ================================
for i=size(Out,1):-1:1
    if ~isempty(strmatch('---',Out(i).Name))
        % skip separators
        continue
    end
    InfoGrp=ph_disp(FI,Out(i).Group,[]);
    InfoElm=ph_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isstruct(InfoElm) || InfoGrp.SizeDim==0
        % remove references to non-stored data fields
        Out(i)=[];
    elseif strcmp(Out(i).Name,'relative breaking intensity')
        % old Pharos files have only one breaking intensity field: don't
        % use it
        InfoGrp=ph_disp(FI,'SPECTRAL-INFO',[]);
        InfoElm=ph_disp(FI,Out(i).Group,[]);
        if isstruct(InfoGrp) && InfoGrp.SizeDim~=InfoElm.SizeDim
            Out(i).SubFld='';
        end
    end
end

Info = ph_disp(FI,'SEICH_def',[]);
if isstruct(Info)
    %seiches
    Period=1; % arbitrary finite, non-zero value
    NSamp=1;
    for i=length(Out):-1:1
        if isequal(Out(i).SubFld,'nfreq')
            Out(i)=[];
        end
    end
else
    % not seiches
    Info = ph_disp(FI,'SPECTRAL-INFO',[]);
    if isstruct(Info) & Info.SizeDim>0
        % spectral model
        for i=1:length(Out)
            if strcmp(Out(i).Name,'weighted mean wave height dir.')
                Out(i).Name = 'wave height spectral model';
                Out(i).SubFld = [];
            end
        end
        %
        Freqs = ph_let(FI,'SPECTRAL-INFO',{0},'SPECTRAL-RPAR',{1},'quiet');
    else
        Freqs = ph_let(FI,'INFO','RPAR',{1},'quiet');
    end
    iszero = Freqs==0;
    Freqs(iszero) = 1;
    Per = 1./Freqs;
    Per(iszero) = 1000;
    %
    Pmin = min(Per);
    Pmax = max(Per);
    nPmin = Pmin:Pmin:max(100*Pmin,10*Pmax);
    dP = 0;
    for i = 1:length(Per)
        dP = max(dP,abs(max(1,round(nPmin/Per(i)))*Per(i)-nPmin));
    end
    [dPmin,n] = min(dP);
    Period = n*Pmin;
    %
    NSamp = 100*n;
end
for i=1:length(Out)
    Out(i).Period   = Period;
    Out(i).NSamples = NSamp;
end
Info = ph_disp(FI,'SEICH_loc',[]);
if isstruct(Info) & Info.SizeDim>0
    SeichFreq = strmatch('sf',{Out.SubFld});
    if ~isempty(SeichFreq)
        Out(end+1)=Out(end);
        for i=SeichFreq'
            Out(end+1) = Out(i);
            Out(end).Name = cat(1,[Out(end).Name ' (frequency graph)']);
            Out(end).SubFld = '';
            Out(end).DimFlag = [0 5 0 0 0];
            Out(end).NVal = -1;
            Out(end).Geom = 'PNT';
            Out(end).Data = Out(i);
        end
    end
end

% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if isempty(Props.SubFld)
    return
end
switch Props.SubFld
    case 'agd'
        InfoG = ph_disp(FI,Props.Group,[]);
        for i=InfoG.SizeDim:-1:1
            subf{i}=sprintf('field %i',i);
        end
    case 'md' % main directions in case of directional spreading
        MainDirs = ph_let(FI,'HS_dir','QH_inc',{1},'quiet')*180/pi;
        for i=length(MainDirs):-1:1
            subf{i}=sprintf('%g deg',MainDirs(i));
        end
    case {'sf','nfreq','nrun'} % frequencies / periods
        freq = 0;
        switch Props.SubFld
            case 'sf'
                freq = 1;
                Freqs = ph_let(FI,'SEICH_def','FREQ',{0},'quiet');
            case {'nfreq','nrun'}
                Info = ph_disp(FI,'SPECTRAL-INFO',[]);
                if isstruct(Info) && Info.SizeDim>0
                    Freqs = ph_let(FI,'SPECTRAL-INFO',{0},'SPECTRAL-RPAR',{1},'quiet');
                else
                    Freqs = ph_let(FI,'INFO','RPAR',{1},'quiet');
                end
        end
        Freqs = Freqs(Freqs>0);
        NFreq = length(Freqs);
        for i=NFreq:-1:1
            if freq
                if Freqs(i)<0.1
                    subf{i}=sprintf('%g mHz',1000*Freqs(i));
                else
                    subf{i}=sprintf('%g Hz',Freqs(i));
                end
            else
                subf{i}=sprintf('%g s',1/Freqs(i));
            end
        end
        if NFreq==0
            subf={};
        end
        if strcmp(Props.SubFld,'nrun')
            freq = subf;
            Info = ph_disp(FI,'POTENTIALS',[]);
            NRun = Info.SizeDim;
            NDir = NRun/NFreq;
            Dirs = ph_let(FI,'GRID_adm',{0},'QH_incident',{1},'quiet')*180/pi;
            for direction=NDir:-1:1
                dirstr{direction} = sprintf('%g deg',Dirs(direction));
            end
            direction = NDir;
            frequency = NFreq;
            for run=NRun:-1:1
                subf{run}=sprintf('%s - %s',freq{frequency},dirstr{direction});
                direction = direction-1;
                if direction<1
                    frequency = frequency-1;
                    direction = NDir;
                end
            end
            %
            % Don't include "combined" wave image since it guarantees only to
            % reproduce maximum for point with maximum amplitude and not for
            % any other point.
            %
            %if NRun>1 & strcmp(Props.Name,'wave image') % & ~spectral
            %    subf{end+1}='combined';
            %end
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
if Props.DimFlag(M_)
    Info=ph_disp(FI,'GRID_coor',[]);
    sz(M_)=Info.SizeDim;
end
if Props.DimFlag(ST_)
    Info=ph_disp(FI,'SEICH_loc',[]);
    sz(ST_)=Info.SizeDim;
end
if Props.DimFlag(T_)
    switch Props.Name
        case 'wave image'
            sz(T_)=Props.NSamples;
        otherwise
            Info=ph_disp(FI,Props.Group,[]);
            sz(T_)=Info.SizeDim;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
if isequal(t,0)
    t=1:Props.NSamples;
end
switch Props.Name
    case 'wave image'
        T=Props.Period*t/Props.NSamples/24/3600;
    otherwise
        T=zeros(length(t),1);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,s)
%======================== SPECIFIC CODE =======================================
if nargin<3
    s=0;
end
Sx=ph_let(FI,'SEICH_loc',{s},'DESCR','quiet');
S=cell(size(Sx,1),1);
for i=1:size(Sx,1)
    Name = Sx(i,:);
    j = min(find(Name==0));
    if ~isempty(j)
        S{i} = Name(1:j-1);
    else
        S{i} = deblank(Name);
    end
end
% -----------------------------------------------------------------------------

function varargout = ph_get(FI, varargin)
if isfield(FI, 'GrpDat')
    [varargout{1:nargout}] = vs_get(FI, varargin{:});
else
    from_let = ph_ncget(FI, varargin{:});
    sz_let = size(from_let);
    varargout = {reshape(from_let, [sz_let(2:end), 1])};
end

function varargout = ph_let(FI, varargin)
if isfield(FI, 'GrpDat')
    [varargout{1:nargout}] = vs_let(FI, varargin{:});
else
    varargout = {ph_ncget(FI, varargin{:})};
end

function data = ph_ncget(FI, varargin)
Grp = varargin{1};
gDim = {};
Elm = varargin{2};
eDim = {};
if iscell(Elm)
    gDim = Elm;
    Elm = varargin{3};
    if iscell(varargin{4})
        eDim = varargin{4};
    end
elseif iscell(varargin{3})
    eDim = varargin{3};
end
ncVar = Elm2ncVar(FI, Elm);
switch ncVar
    case 'SPEC_PHI_R'
        eDim{2} = 1;
    case 'SPEC_PHI_I'
        eDim{2} = 1;
    case 'SPECTRAL-WEIGHTS'
        gDim = [{1} gDim];
    case 'QH_incident'
        gDim = {};
    case 'LELNR'
        gDim = {};
    case 'RPAR'
        gDim = {};
    otherwise
        switch Grp
            case {'HS_dir','GRID'}
                gDim= {};
        end
end
eStart = zeros(size(eDim));
eCount = zeros(size(eDim));
gStart = zeros(size(gDim));
gCount = zeros(size(gDim));
for i = length(eDim):-1:1
    if isequal(eDim{i}, 0)
        eDim{i} = ':';
        eStart(i) = 0;
        eCount(i) = inf;
    else
        eStart(i) = min(eDim{i}) - 1;
        eCount(i) = max(eDim{i}) - eStart(i);
        eDim{i} = eDim{i} - eStart(i);
    end
end
for i = length(gDim):-1:1
    if isequal(gDim{i},0)
        gDim{i} = ':';
        gStart(i) = 0;
        gCount(i) = inf;
    else
        gStart(i) = min(gDim{i}) - 1;
        gCount(i) = max(gDim{i}) - gStart(i);
        gDim{i} = gDim{i} - gStart(i);
    end
end
iVar = ustrcmpi(ncVar,{FI.Dataset.Name});
szVar = FI.Dataset(iVar).Size;
% if strcmp(Grp, 'GRID_adm')
%     fprintf('%s: %s -> %s (%i)\n',Grp,Elm,ncVar,iVar);
%     fprintf('size = %s\n', vec2str(szVar));
%     fprintf('index = %s %s\n', vec2str(gCount), vec2str(eCount));
% end
if ismember(ncVar,{'SPECTRAL-RPAR','RPAR','PHI_R','PHI_I'})
    % variables with group dimension last
    start = [eStart gStart];
    count = min([eCount gCount], szVar);
    data = nc_varget(FI.Filename, ncVar, start, count);
    if length(count) > 1
        data = reshape(data, count);
    end
    data = data(eDim{:},gDim{:});
    % make sure to return data with group dimension first
    if length(count) > 1
        data = permute(data, [length(eDim)+(1:length(gDim)) 1:length(eDim)]); % group dimensions should go first
    end
else
    % variables with group dimension first
    start = [gStart eStart];
    count = min([gCount eCount], szVar);
    data = nc_varget(FI.Filename, ncVar, start, count);
    if length(count) > 1
        data = reshape(data, count);
    end
    data = data(gDim{:},eDim{:});
end
switch ncVar
    case 'SPECTRAL-WEIGHTS'
        data = permute(data, [2:ndims(data) 1]);
    otherwise
        % if group dimension was dropped, add it again.
        if isempty(gDim)
            data = reshape(data, [1, size(data)]);
        end
end

function varargout = ph_disp(FI, varargin)
if isfield(FI, 'GrpDat')
    [varargout{1:nargout}] = vs_disp(FI, varargin{:});
else
    Grp = varargin{1};
    if nargin == 2
        if isempty(Grp)
            % query group names
            Info = {'POTENTIALS', 'MESH_2', 'SEICH_loc', 'MESH_curv', 'MESH_1', 'BOUND_cond', 'WAVES_def', 'HS_dir', 'GRID_adm', 'INFO', 'GRID_coor', 'GRID', 'SPECTRAL-PAR', 'SPECTRAL-INFO'};
        else
            % query elements in specified group
            switch Grp
                %case 'POTENTIALS'
                %    Info = {'PHI_r', 'PHI_i'};
                %case 'GRID'
                %    Info = {'H_depth', 'K_wave', 'C_wave', 'Cg_wave'};
                otherwise
                    Info = {};
            end
        end
    else
        Elm = varargin{2};
        if isempty(Elm)
            % query group properties
            Info.Name = Grp;
            Info.SizeDim = Grp2Size(FI, Grp);
            if Info.SizeDim < 0
                varargout = {-1};
                return
            end
        else
            % query element properties
            if Grp2Size(FI, Grp) < 0
                varargout = {-1};
                return
            end
            %
            ncVar = Elm2ncVar(FI, Elm);
            iVar = ustrcmpi(ncVar, {FI.Dataset.Name});
            if iVar < 0
                varargout = {-1};
                return
            end
            dVar = FI.Dataset(iVar);
            %
            Info.GrpName = Grp;
            Info.ElmName = Elm;
            Info.SizeDim = dVar.Size;
            attribs = {dVar.Attribute.Name};
            %
            iLN = ustrcmpi('long_name', attribs);
            if iLN > 0
                Info.ElmDescription = dVar.Attribute(iLN).Value;
            else
                Info.ElmDescription = '';
            end
            %
            iUN = ustrcmpi('unit', attribs);
            if iUN > 0
                Info.ElmUnits = dVar.Attribute(iUN).Value;
            else
                Info.ElmUnits = '';
            end
        end
    end
    varargout{1} = Info;
end

function szGrp = Grp2Size(FI, Grp)
switch Grp
    case 'GRID_coor'
        iDimNodes = ustrcmpi('nMesh2D_node',{FI.Dimension.Name});
        szGrp = FI.Dimension(iDimNodes).Length;
    case 'HS_dir'
        switch FI.simtype
            case 'dir_spread'
                szGrp = 1;
            otherwise
                szGrp = 0;
        end
    case 'POTENTIALS'
        switch FI.simtype
            case 'spectral'
                Phi_r = ustrcmpi('SPEC_PHI_R',{FI.Dataset.Name});
                szGrp = FI.Dataset(Phi_r).Size(1);
            case 'seiching'
                szGrp = -1;
            case 'dir_spread'
                szGrp = 0;
            otherwise
                szGrp = 1;
        end
    case 'SEICH_def'
        switch FI.simtype
            case 'seiching'
                szGrp = 1;
            otherwise
                szGrp = -1;
        end
    case 'SEICH_loc'
        % not sure whether this data is actually written to netCDF file.
        szGrp = 0;
    case {'SPECTRAL-INFO', 'SPECTRAL-PAR'}
        switch FI.simtype
            case 'spectral'
                szGrp = 1;
            otherwise
                szGrp = 0;
        end
    otherwise
        szGrp = 1;
end


function ncVar = Elm2ncVar(FI, Elm)
switch Elm
    case 'PHI_r'
        if strcmp(FI.simtype,'spectral')
            ncVar = 'SPEC_PHI_R';
        else
            ncVar = 'PHI_R';
        end
    case 'PHI_i'
        if strcmp(FI.simtype,'spectral')
            ncVar = 'SPEC_PHI_I';
        else
            ncVar = 'PHI_I';
        end
    case 'QH_incident'
        if strcmp(FI.simtype,'spectral')
            ncVar = 'SPEC_QH_incident';
        else
            ncVar = 'QH_incident';
        end
    case 'X_coor'
        ncVar = 'Mesh2D_node_x';
    case 'Y_coor'
        ncVar = 'Mesh2D_node_y';
    case 'QH_inc'
        ncVar = 'QH_INC';
    case 'FREQ'
        ncVar = 'SEICHDEF_FREQ';
    case 'PHIs_r'
        ncVar = 'SEICH-PHIS_R';
    case 'PHIs_i'
        ncVar = 'SEICH-PHIS_I';
    case 'UMAX'
        ncVar = 'SEICH-UMAX';
    case 'UDIR'
        ncVar = 'SEICH-UDIR';
    case 'UMIN'
        ncVar = 'SEICH-UMIN';
    case 'TM10'
        ncVar = 'SPECTRAL-TM10';
    case 'KTM10'
        ncVar = 'SPECTRAL-KTM10';
    case 'OMEGATM10'
        ncVar = 'SPECTRAL-OMEGATM10';
    case 'HS_directional'
        ncVar = 'HS_DIRECTIONAL';
    case 'WEIGHTS'
        ncVar = 'SPECTRAL-WEIGHTS';
    otherwise
        ncVar = Elm;
end

function [NewFI,cmdargs]=ph_options(FI,mfig,cmd,varargin)
if isfield(FI, 'GrpDat')
    [NewFI,cmdargs]=options(FI,mfig,cmd,varargin{:});
else
    [NewFI,cmdargs]=netcdffil(FI,1,'options',mfig,cmd,varargin{:});
end
