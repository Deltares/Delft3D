function varargout=waquaio(sds,exper,field,varargin)
%WAQUAIO Read SIMONA SDS file.
%   [...]=WAQUAIO(SDS,'Exp','Field',TStep,Station,N,M,K)
%   supported fields and associated output arguments
%
%     FIELD    | OUTPUT ARGUMENTS
%   -----------------------------------------------------------------------
%   * refdate  : refdate
%   * grid     : x,y (depth points)
%                -->  dgrid, zgrid, ugrid, vgrid (DP,ZETA,U,V points)
%   * zgrid3d  : x,y,z (waterlevel points)
%                -->  zgrid3d, ugrid3d, vgrid3d (ZETA,U,V points)
%   * drywet   : udam,vdam ((temporary) thindams)
%
%   * depth    : bed level (positive down)
%   * height   : bed level (positive up)
%
%   * wlvl     : water level, time
%   * wdepth   : water depth, time
%   * head     : head (energie hoogte), time
%   * xyveloc  : u,v,time or u,v,w,time
%                (U,V components in X,Y direction)
%   * xyudisch : qu,qv,time (qu=u*hu, qv=v*hv)
%                (QU,QV components in X,Y direction)
%   * veloc    : u,v,time or u,v,w,time
%                (U,V components in KSI,ETA direction in waterlevel points)
%   * veloc0   : unprocessed velocities in velocity points
%   * udisch   : unit discharge (u*H): qu,qv,time or qu,qv,w,time
%                (qu,qv components in KSI,ETA direction in waterlevel points)
%   * udisch0  : unprocessed unit discharges in velocity points
%   * disch    : Qu,Qv,time or Qu,Qv,w,time
%                (QU,QV components in KSI,ETA direction in waterlevel points)
%   * disch0   : unprocessed discharges in velocity points
%   * dischpot : discharge potential,time
%   * chezy    : Chézy KSI direction at U point,Chézy ETA direction at V point
%
%   * energy   : turbulent kinetic energy, time
%   * dissip   : energy dissipation, time
%   * vdiffu   : vertical eddy diffusivity, time
%
%   * subst:<substance name>  : substance field, time
%
%   * weirs    : udam,vdam,uhgh,vhgh: weir locations and weir crest heights
%     weirs2-cresth  : udam,vdam,uhgh,vhgh: weir locations and weir crest heights
%     weirs2-sill1   : udam,vdam,uhgh,vhgh: weir locations and weir sill heights (low M/N)
%     weirs2-sill2   : udam,vdam,uhgh,vhgh: weir locations and weir sill heights (high M/N)
%   * weirs2   : udam,vdam: locations of weirs - new style
%     weirs2-cresth  : udam,vdam,uhgh,vhgh: weir locations and weir crest heights - new style
%     weirs2-sill1   : udam,vdam,uhgh,vhgh: weir locations and weir sill heights (low M/N) - new style
%     weirs2-sill2   : udam,vdam,uhgh,vhgh: weir locations and weir sill heights (high M/N) - new style
%     weirs2-crestl  : udam,vdam,uhgh,vhgh: weir locations and weir crest lengths - new style
%     weirs2-talud1  : udam,vdam,uhgh,vhgh: weir locations and weir slopes (low M/N) - new style
%     weirs2-talud2  : udam,vdam,uhgh,vhgh: weir locations and weir slopes (high M/N) - new style
%     weirs2-calib1  : udam,vdam,uhgh,vhgh: weir locations and weir calibration factor 1 - new style
%     weirs2-calib2  : udam,vdam,uhgh,vhgh: weir locations and weir calibration factor 2 - new style
%     weirs2-vegh    : udam,vdam,uhgh,vhgh: weir locations and vegetation height at weir - new style
%     weirs2-vegdens : udam,vdam,uhgh,vhgh: weir locations and vegetation density at weir - new style
%     weirs2-vegdrag : udam,vdam,uhgh,vhgh: weir locations and vegetation drag at weir - new style
%     weirs2-fc      : udam,vdam,uhgh,vhgh: weir locations and flow conditions - new style
%     weirs2-q       : udam,vdam,uhgh,vhgh: weir locations and unit discharge at weir - new style
%     weirs2-h       : udam,vdam,uhgh,vhgh: weir locations and overflow depth at weir - new style
%     weirs2-u       : udam,vdam,uhgh,vhgh: weir locations and velocity at weir - new style
%     weirs2-de      : udam,vdam,uhgh,vhgh: weir locations and head loss at weir - new style
%
%   * wind     : wind vector, time
%   * press    : pressure, time
%
%   * flowstat-wl : water level station names
%   * xy-wl       : water level station xy coordinates
%   * mn-wl       : water level station mn coordinates
%   * wlstat      : water level at station
%   * flowstat-uv : current station names
%   * xy-uv       : current station xy coordinates
%   * mn-uv       : current station mn coordinates
%   * uv-stat     : velocity at current station
%                   (U,V components in X,Y direction)
%   * uv0-stat    : velocity at current station
%                   (U,V components direction as on file - depends on
%                    NO_BACKTRANSFORM keyword in SIMINP file)
%   * flowcrs-u   : u-discharge crosssection names
%   * flowcrs-v   : v-discharge crosssection names
%   * mq-stat     : instantaneous discharge
%   * cq-stat     : cumulative discharge
%
%   * substances  : substance names,substance units
%   * transtat    : concentration station names
%   * trancrs-u   : u-transport crosssection names
%   * trancrs-v   : v-transport crosssection names
%   * mn-transtat : mn-coordinates of conc. station names
%   * mn-trancrs-u: mn-coordinates of u-transport crosssec.
%   % mn-trancrs-v: mn-coordinates of v-transport crosssec.

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

if nargin<3
    error('Not enough input arguments.')
end

%
% Obtain experiment name when it was not specified by the user.
%
if isempty(exper)
    if length(sds.Experiment)==1
        exper = sds.Experiment.Name;
    else
        error('Missing experiment name.')
    end
end

%
% Read reference date ...
%
refdate = waqua_refdate(sds,exper);

if strcmp(field,'refdate')
    varargout = {refdate};
    return
end
%
% Retrieve all relevant dimensions from MESH_IDIMEN array.
%
if waqua('exists',sds,exper,'MESH_IDIMEN')
    dimen=waqua('readsds',sds,exper,'MESH_IDIMEN');
    dimen(100)=0;
    %  1: NDIM 1,2,3
    %  2: MMAX
    %  3: NMAX
    %  4: MNMAX = max(NMAX,MMAX)
    %  5: MNMAXK = 1 + NumComputationalPoints
    %  6: NENCLO = NumEnclosurePoints
    %  7: LDAM = NumThinDams
    %  8: NOCOLS = NumGridCols
    %  9: NOROCO = NumGridRows+NumGridCols
    % 10: NOROWS = NumGridRows
    % 11: NSLU = NumUBarrier
    % 12: NSLUV = NumUVBarrier
    % 13: NSLV = NumVBarrier
    % 14: NTO = TotalNumTideOpening
    % 15: IADLND = AddressInactivePoint
    % 16: KURFLG = 0 (Rect), 1 (Curv), 2 (RectOnSphe), 3 (CurvOnSphe)
    % 17: NROU = NumWeirs
    % 18: KMAX
    % 19: ISILOR = 0 (pos.down), 1 (pos.up) 
    % 20: IDEPO = 0 (pos.down), 1 (pos.up)
    % 21: IRLFLG = 0 (no duplic.spher), 1 (duplic.spher)
    % 22: NBARU = NumUBarrierPoints
    % 23: NBARV = NumVBarrierPoints
    % 24: NBARUV = NumBarrierPoints
    % 25: NTOPT = NumOpeningPoints
    dim.mmax=dimen(2);
    dim.nmax=dimen(3);
    dim.npnt=dimen(5);
    dim.num_irogeo_rows=dimen(10);
    dim.nslu=dimen(11);
    dim.nsluv=dimen(12);
    dim.inact=dimen(15);
    curvl=dimen(16);
    dim.spheric=dimen(16)>2;
    dim.sph_dupl=0; %dimen(21); % do not use the second grid, it is just
    % counting grid points (ref: DCSM98a)
    dim.kmax=dimen(18);
    dim.nbaruv=dimen(24);
    dim.nweiu=dimen(38);
    dim.sz=[dim.nmax dim.mmax];
elseif waqua('exists',sds,exper,'MESH01_GENERAL_DIMENSIONS')
    dimen=waqua('readsds',sds,exper,'MESH01_GENERAL_DIMENSIONS');
    %  1: typgrd
    %  2: ndim2
    %  3: mmax
    %  4: nmax
    %  5: kmax
    %  6: npoint
    %  7: ncel
    %  8: typcel
    %  9: typcoor
    % 10: inpelm
    % 11: mettyp : 0=cart (planar), 1=sph, 10=rot pole
    % 13: itmzon
    % 14: isumtm
    % 20: nuspnt
    % 21: ncurvs
    % 22: nsurfs
    % 23: nvols
    % 24: nelgrp
    % 25: unique
    % 26: lennam
    % 27: namuse
    % 40: iadlnd
    % 41: conditions
    % 42: poscon
    dim.nmax=dimen(4);
    dim.mmax=dimen(3);
    dim.npnt=dimen(3)*dimen(4);
    dim.spheric=dimen(11);
    dim.sph_dupl=0;
    curvl = 1+2*min(1,dim.spheric);
else
    dimen=waqua('readsds',sds,exper,'MESH01_SPECIFIC_IDIMEN');
    %  1: MMAX
    %  2: NMAX
    dim.nmax=dimen(2);
    dim.mmax=dimen(1);
    curvl = 1;
end

%
% Switch name of data field to lower case
%
field=lower(field);

%
% If non-curvilinear, simplify xyveloc to veloc
%
if curvl~=1 && curvl~=3 && strcmp(field,'xyveloc')
    field='veloc';
end

%
% Determine whether we are dealing with a 2D or 3D array or a variable at a
% station. But first, handle the special cases ...
%
if isequal(field,'flowstat-cur')
    field = 'flowstat-uv';
end
if isequal(field,'curvl')
    varargout = {curvl};
    return
elseif isequal(field,'substances')
    if waqua('exists',sds,exper,'CONTROL_TRANS_ICONTA');
        iconta=waqua('readsds',sds,exper,'CONTROL_TRANS_ICONTA');
        %  1: NOPOW = num discharge sources power stations
        %  2: NSRC = num discharge sources
        %  3: NOPOL = num constituent stations
        %  4: NTRA = num u-transp. cross
        %  5: NTRAV = num v-transp. cross
        %  6: NTOPT = num opening points
        %  7: NOTTID = num times in opening data
        %  8: NOTDIS = num times in disch. data
        %  9: NOTCDI = num times in conc. data at disch. sources
        % 10: NOTQNR = num times in incoming flux data
        % 11: NOTDRY = num times in dry air bulb data
        % 12: NOTRHU = num times in rel. humidity data
        % 13: NOTTWM = num times in background water temp data
        % 14: NOTQSC = num times in solar rad. data
        % 15: IBLHIT = 1 (blocked transport history data present), 0 (not present)
        % 16: NUTHBT = num time instances in blocked hist. data
        nsrc=iconta(2);
        
        icontb=waqua('readsds',sds,exper,'CONTROL_TRANS_ICONTB');
        %  1: KTEMP = temperature model type (1: rad.according to model,
        %                                2: rad. time series, 3:excess temp.)
        %  2: LMAX = num constituents
        %  3: LSAL = const num salinity
        %  4: LTEMP = const num temperature
        %  5: LERG = 0 (no energy), L>0 (energy in computation)
        %  6: LEPS = 0 (no dissip), L>0 (dissip in computation)
        %  7: LDIFCO = 1 (yes) / 0 (no) constant vert. eddy visc
        %  8: IFLVAL = flag fall velocities user specified
        %  9: LTUR = num turbulence variables
        % 10: NWRTUR = write flag turb. quantities SDS file (1 = yes)
        lmax=icontb(2);
        
        data=waqua('readsds',sds,exper,'PROBLEM_TRANS_NAMPRB');
        nams=data(1+nsrc+(1:lmax))';
        unt=data(1+nsrc+lmax+(1:lmax))';
        varargout={nams unt};
    else
        varargout={{} {}};
    end
    return
elseif ismember(field,{'transtat','trancrs-u','trancrs-v', ...
        'flowstat-wl','flowstat-uv','flowcrs-u','flowcrs-v', ...
        'wlstat','wl-stat','u-stat','v-stat','uv-stat','w-stat', ...
        'mq-stat','cq-stat','z-stat','z-stati','z-statc', ...
        'z-sbstat','z-sbstati','z-sbstatc','wl-xy','uv-xy', ...
        'wl-mn'   ,'uv-mn',                                 ...
        'q-barp','wl-lbarp','vel-lbarp','wl-hbarp','vel-hbarp', ...
        'vel-barp','hg-barp','enl-barp','sl-bar','gl-bar','wd-bar', ...
        'barriers','barrierpoints','mn-transtat','mn-trancrs-u', ...
        'mn-trancrs-v','uv0-stat'})
    stationdata = 1;
elseif length(field)>8 && strcmp(field(1:8),'stsubst:')
    stationdata = 1;
else
    stationdata = 0;
end

%
% If station data ...
%
if stationdata
    [varargout{1:nargout}] = waqua_get_station(sds,exper,field,dim,refdate,varargin);
else
    [varargout{1:nargout}] = waqua_get_spatial(sds,exper,field,dim,refdate,varargin);
end

function refdate = waqua_refdate(sds,exper)
if waqua('exists',sds,exper,'PROBLEM_FLOW_NAMPRB')
    names=waqua('readsds',sds,exper,'PROBLEM_FLOW_NAMPRB');
    %  1: ITDATE
    %  2: NAMDIS = discharge source names
    itdate=names{1};
else
    names = waqua('read',sds,exper,'IDENTIFICATION');
    %  1  : verlds(1)
    %  2-4: idtext(3)
    %  5  : refdat(1)
    %  6  : (16)
    itdate = names{5};
end
%
[day,r]=strtok(itdate);
day=str2double(day);
[month,r]=strtok(r);
month=ustrcmpi(month,{'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'});
year=strtok(r);
year=str2double(year);
if ~(isempty(day) || isempty(month) || isempty(year))
    refdate=datenum(year,month,day);
else
    refdate=1;
end

%==========================================================================
% STATION DATA
%==========================================================================
%
% TIMEHISTORIES_FLOW_TIMHIS:
% ZWL   : NOWL         (waterlevel)
% ZCUR  : NOCUR,KMAX   (vel.mag.)
% ZCURU : NOCUR,KMAX   (u vel.)
% ZCURV : NOCUR,KMAX   (v vel.)
% CTR   : NTRA         (disch u dir.)
% FLTR  : NTRA         (cumm. disch u dir.)
% CTRV  : NTRAV        (disch v dir.)
% FLTRV : NTRAV        (cumm. disch v dir.)
% BARQ  : NBARUV       (transp at barrier)
% BASEPA: NBARUV       (waterlevel low M/N)
% BASEPB: NBARUV       (waterlevel high M/N)
% BAVELA: NBARUV       (velocity low M/N)
% BAVELB: NBARUV       (velocity high M/N)
% BARFLW: NBARUV       (vel. at barrier location) [included in SDS since revision 152 - 09/06/2006!]
% BARH:   NBARUV       (height at barrier location) [included in SDS since revision 152 - 09/06/2006!]
% BARDE:  NBARUV       (energy loss at barrier location) [included in SDS since revision 152 - 09/06/2006!]
% ZCURW:  NOCUR,KMAX+1 (omega vel, TRIWAQ only)
% ZCURWP: NOCUR,KMAX   (phys.w vel, TRIWAQ only)
% ZKCUR:  NOCUR,KMAX+1 (layer interface, TRIWAQ only)
%
% TIMEHISTORIES_TRANS:
% GRO:    LMAX,NOPOL,KMAX   (constit stat)
% ADTR:   LMAX,NTRA         (total const u-cross)
% ATR:    LMAX,NTRA         (total advec const u-cross)
% DTR:    LMAX,NTRA         (total diff.mass transp u-cross)
% ADTRV:  LMAX,NTRAV        (total const v-cross)
% ATRV:   LMAX,NTRAV        (total advec const v-cross)
% DTRV:   LMAX,NTRAV        (total diff.mass transp v-cross)
% ZKPOL:  NOPOL,KMAX+1      (layer interface, TRIWAQ only)
% GRKE:   LTUR,NOPOL,KMAX+1 (turb.quant., TRIWAQ only)
%
%==========================================================================
%
function varargout = waqua_get_station(sds,exper,field,dim,refdate,argin)
kmax = dim.kmax;
nbaruv = dim.nbaruv;
switch field
    %
    % -------------------------------------------------------------------
    %
    case {'mn-transtat','mn-trancrs-u','mn-trancrs-v'}
        switch field
            case 'mn-transtat'
                MN=waqua('readsds',sds,exper,'CHECKPOINTS_TRANS_IPOLPT');
                MN=reshape(MN,[length(MN)/2 2]);
            case 'mn-trancrs-u'
                MN=waqua('readsds',sds,exper,'CHECKPOINTS_TRANS_ICROSU');
                MN=reshape(MN,[length(MN)/3 3]);
                MN=MN(:,[1 2 1 3]);
            case 'mn-trancrs-v'
                MN=waqua('readsds',sds,exper,'CHECKPOINTS_TRANS_ICROSV');
                MN=reshape(MN,[length(MN)/3 3]);
                MN=MN(:,[2 1 3 1]);
        end
        varargout = {MN};
        return
    case {'transtat','trancrs-u','trancrs-v'}
        iconta=waqua('readsds',sds,exper,'CONTROL_TRANS_ICONTA');
        % ref. "substances"
        nopol=iconta(3);
        ntra=iconta(4);
        ntrav=iconta(5);
        
        data=waqua('readsds',sds,exper,'CHECKPOINTS_TRANS_NAMCHK');
        switch field
            case 'transtat'
                data=deblank(data(1:nopol));
                empty=cellfun('isempty',data);
                if any(empty)
                    MN=waqua('readsds',sds,exper,'CHECKPOINTS_TRANS_IPOLPT');
                    MN=reshape(MN,[length(MN)/2 2]);
                    for i=find(empty)
                        data{i}=sprintf('(M=%i,N=%i)',MN(i,:));
                    end
                end
            case 'trancrs-u'
                data=data(nopol+(1:ntra));
            case 'trancrs-v'
                data=data(nopol+ntra+(1:ntrav));
        end
        data=data';
        varargout={data};
        
    case {'flowstat-wl','flowstat-uv','flowcrs-u','flowcrs-v','wlstat', ...
            'wl-stat','u-stat','v-stat','uv-stat','w-stat','mq-stat', ...
            'cq-stat','z-stat','z-stati','z-statc','uv0-stat', ...
            'z-sbstat','z-sbstati','z-sbstatc','wl-xy','uv-xy', ...
            'wl-mn'   ,'uv-mn'    ,                             ...
            'q-barp','wl-lbarp','vel-lbarp','wl-hbarp','vel-hbarp', ...
            'vel-barp','hg-barp','enl-barp','sl-bar','gl-bar','wd-bar', ...
            'barriers','barrierpoints'}
        [tstep,stationi,k]=local_argin(argin);
        iconta=waqua('readsds',sds,exper,'CONTROL_FLOW_ICONTA');
        %  1: NOPOW = num discharge sources power stations
        %  2: NSRC = num discharge sources
        %  3: NTOF = num Fourier openings
        %  4: NTOT = num tide openings
        %  5: KC = num Fourier comp
        %  6: NOWL = num waterlevel stat
        %  7: NOCUR = num current stat
        %  8: NTRA = num u-transp cross
        %  9: NTRAV = num v-transp cross
        % 10: NOTBAR = num times in barrier data
        % 11: NOTTID = num times in open bound data
        % 12: NOTDIS = num times in disch data
        % 13: NTOQH = num QH rel. driven bound
        % 14: NTOQAD = num disch, bound with autom. distrib.
        % 15: NUMQHP = num QH pairs for QH rels
        % 16: IBLHIF = 1 (blocked flow history data present), 0 (not present)
        % 17: NUTHBF = num time instances in blocked hist. data
        % 18: NIKURA = Nikuradse roughness active (1) or not (0)
        % 19: NARU = size AREAU table
        % 20: NARV = size AREAV table
        % 21: NROUGHK = size ROUGK table
        % 22-23: KALMAN param.
        nowl=iconta(6);
        nocur=iconta(7);
        ntra=iconta(8);
        ntrav=iconta(9);
        
        factor=1;
        ARRAY='TIMEHISTORIES_FLOW_TIMHIS';
        if waqua('size',sds,exper,'TIMEHISTORIES_FLOW_TIMHIS') > ...
                nowl+nocur*kmax*3+ntra*2+ntrav*2+nbaruv*5+nocur*(3*kmax+2)
            % BARFLW, BARH and BARDE are included in the SDS file since
            % revision 152 dated 09/06/2006
            nbarpar = 8;
        else
            nbarpar = 5;
        end
        barrierfields = {'q-barp','wl-lbarp','wl-hbarp','vel-lbarp','vel-hbarp', ...
            'vel-barp','hg-barp','enl-barp'};
        statmax=-1;
        stationi_org = stationi;
        switch field
            case 'flowstat-wl'
                statmax=nowl;
            case 'flowstat-uv'
                statmax=nocur;
            case 'flowcrs-u'
                statmax=ntra;
            case 'flowcrs-v'
                statmax=ntrav;
            case 'barriers'
                statmax=dim.nsluv;
            case 'barrierpoints'
                statmax=nbaruv;
            case {'wlstat','wl-stat','wl-xy','uv-xy','wl-mn','uv-mn'}
                stoffset=0;
                krange=1;
                statmax=nowl;
            case 'umag-stat'
                stoffset=nowl;
                krange=1+(0:kmax-1)*nocur;
                statmax=nocur;
            case {'u-stat','uv-stat','uv0-stat'}
                stoffset=nowl+nocur*kmax;
                krange=1+(0:kmax-1)*nocur;
                statmax=nocur;
            case 'v-stat'
                stoffset=nowl+nocur*kmax*2;
                krange=1+(0:kmax-1)*nocur;
                statmax=nocur;
            case 'mq-stat'
                stoffset=nowl+nocur*kmax*3;
                krange=1;
                %
                % ntra instant, ntra cum, ntrav instant, ntrav cum
                %^ current offset location
                %                        ^ offset for stationi-ntra
                %
                % the instaneous v cross-sections follow after the
                % cumulative u cross-sections. Add the ntra skip to
                % the indices for v cross-sections.
                if isequal(stationi,':')
                    stationi = 1:ntra+ntrav;
                end
                stationi(stationi>ntra) = stationi(stationi>ntra)+ntra;
                statmax=ntra+ntrav;
            case 'cq-stat'
                stoffset=nowl+nocur*kmax*3+ntra;
                krange=1;
                %
                % ntra instant, ntra cum, ntrav instant, ntrav cum
                %              ^ current offset location
                %              offset for stationi-ntra ^
                %
                % the cumulative v cross-sections follow after the
                % instaneous v cross-sections. Add the ntrav skip to
                % the indices for v cross-sections.
                if isequal(stationi,':')
                    stationi = 1:ntra+ntrav;
                end
                stationi(stationi>ntra) = stationi(stationi>ntra)+ntrav;
                statmax=ntra+ntrav;
            case barrierfields
                ibar = find(strcmp(field,barrierfields));
                stoffset=nowl+nocur*kmax*3+ntra*2+ntrav*2+nbaruv*(ibar-1);
                krange=1;
                field='barrierdata';
                statmax=nbaruv;
            case {'sl-bar','gl-bar','wd-bar'}
                switch field
                    case 'psl-bar'
                        stoffset=1;
                        factor=-1;
                    case 'sl-bar'
                        stoffset=2;
                        factor=-1;
                    case 'pgl-bar'
                        stoffset=3;
                    case 'gl-bar'
                        stoffset=4;
                    case 'pwd-bar'
                        stoffset=5;
                    case 'wd-bar'
                        stoffset=6;
                end
                stationi=(stationi-1)*7+1;
                krange=1;
                field='barrierdata';
                ARRAY='TIMEHISTORIES_FLOW_RRSBAH';
                statmax=nbaruv;
            case 'w-stat'
                stoffset=nowl+nocur*kmax*3+ntra*2+ntrav*2+nbaruv*nbarpar+nocur*(kmax+1);
                krange=1+(0:kmax-1)*nocur;
                statmax=nocur;
            case {'z-stat','z-stati','z-statc'}
                stoffset=nowl+nocur*kmax*3+ntra*2+ntrav*2+nbaruv*nbarpar+nocur*(2*kmax+1);
                krange=1+(0:kmax)*nocur;
                statmax=nocur;
            case {'z-sbstat','z-sbstati','z-sbstatc'}
                iconta=waqua('readsds',sds,exper,'CONTROL_TRANS_ICONTA');
                icontb=waqua('readsds',sds,exper,'CONTROL_TRANS_ICONTB');
                % ref. "substances"
                nopol=iconta(3);
                ntra=iconta(4);
                ntrav=iconta(5);
                lmax=icontb(2);
                %
                ARRAY='TIMEHISTORIES_TRANS';
                stoffset=lmax*(nopol*kmax+3*ntra+3*ntrav);
                krange=1+(0:kmax)*nopol;
                statmax=nopol;
        end
        if ~isequal(stationi_org,':')
            if any(stationi_org<1 | stationi_org>statmax | stationi_org~=round(stationi_org))
                if statmax<0
                    error('The variable statmax has not been set for ''%s''',field)
                elseif statmax==0
                    error('Station index %g invalid: no stations for ''%s'' in data file',stationi_org,field)
                elseif statmax==1
                    error('Station index %g invalid: only one station in data file for ''%s''',stationi_org,field)
                else
                    error('Station index %g invalid: should be integer in range 1:%i',stationi_org,statmax)
                end
            end
        end
        switch field
            % nowl       : waterlevels
            % nocur*k    : velocity mag, u comp., v comp.
            % ntra       : ctr, fltr
            % ntrav      : ctrv, fltrv
            % nbaruv     : barq, basepa, basepb, bavela, bavelb, barflw,
            %              barh, barde
            % nocur*(k+1): zcurw
            % nocur*k    : zcurwp
            % nocur*(k+1): zkcur
            case {'wl-xy','uv-xy'}
                switch field
                    case 'wl-xy'
                        ARRAY='CHECKPOINTS_FLOW_IWLPT';
                    case 'uv-xy'
                        ARRAY='CHECKPOINTS_FLOW_ICURPT';
                end
                stationi=local_argin(argin);
                MN=waqua('readsds',sds,exper,ARRAY);
                MN=reshape(MN,[length(MN)/2 2]);
                MN=MN(stationi,:);
                [zgx,zgy]=waqua_get_spatial(sds,exper,'zgrid',dim,refdate,{});
                mn = sub2ind(size(zgx),MN(:,2),MN(:,1));
                varargout={zgx(mn) zgy(mn)};
            case {'wl-mn','uv-mn'}
                switch field
                    case 'wl-mn'
                        ARRAY='CHECKPOINTS_FLOW_IWLPT';
                    case 'uv-mn'
                        ARRAY='CHECKPOINTS_FLOW_ICURPT';
                end
                stationi=local_argin(argin);
                MN=waqua('readsds',sds,exper,ARRAY);
                MN=reshape(MN,[length(MN)/2 2]);
                varargout = {MN}; 
            case {'wlstat','wl-stat','umag-stat','u-stat','v-stat', ...
                    'mq-stat','cq-stat','w-stat','z-stat','z-stati','z-statc', ...
                    'z-sbstat','z-sbstati','z-sbstatc','barrierdata'}
                data=waqua('readsds',sds,exper,ARRAY,tstep);
                switch field
                    case {'z-stat','z-statc','z-sbstat','z-sbstatc'}
                        z_int = data.Data(:,stoffset+(stationi-1)+krange(:));
                        z     = (data.Data(:,stoffset+(stationi-1)+krange(1:kmax)) + ...
                            data.Data(:,stoffset+(stationi-1)+krange(2:kmax+1)))/2;
                        varargout={z refdate+data.SimTime/1440 z_int};
                    otherwise
                        varargout={factor*data.Data(:,stoffset+(stationi-1)+krange(k)) refdate+data.SimTime/1440};
                end
            case {'uv-stat','uv0-stat'}
                data=waqua('readsds',sds,exper,'TIMEHISTORIES_FLOW_TIMHIS',tstep);
                U = data.Data(:,stoffset+(stationi-1)+krange(k));
                V = data.Data(:,stoffset+nocur*kmax+(stationi-1)+krange(k));
                Time = refdate+data.SimTime/1440;
                % U and V are given in X and Y direction if itraflg=1
                % otherwise U and V are in M and N direction
                icontb=waqua('readsds',sds,exper,'CONTROL_FLOW_ICONTB');
                itraflg = icontb(7);
                if itraflg~=1 && strcmp(field,'uv-stat')
                    % convert U and V from M and N directions into X and Y
                    % directions; for this we'll need to determine the
                    % local grid orientation. Let's first get the
                    % surrounding grid coordinates ...
                    MN = waqua('readsds',sds,exper,'CHECKPOINTS_FLOW_ICURPT');
                    MN = reshape(MN,[nocur 2]);
                    M = MN(stationi,1);
                    N = MN(stationi,2);
                    [x,y]=waqua_get_spatial(sds,exper,'dgrid',dim,refdate,{[N-1 N] [M-1 M]});
                    % now determine the orientation of the grid; this
                    % computation is (like in Delft3D) based on only the
                    % direction of the M-line assuming an ortogonal grid.
                    dx = mean(x(:,2) - x(:,1));
                    dy = mean(y(:,2) - y(:,1));
                    len = sqrt(dx^2+dy^2);
                    dx = dx/len;
                    dy = dy/len;
                    % and finally rotate the velocity vectors
                    Ux = U*dx-V*dy;
                    V  = V*dx+U*dy;
                    U  = Ux;
                end
                varargout={U V Time};
            case {'barriers','barrierpoints'}
                stationi=local_argin(argin);
                %
                nsluv=dim.nsluv;
                if nsluv==0
                    varargout={{}};
                    return
                end
                bnam=waqua('readsds',sds,exper,'MESH_NAMMSH');
                bnam=bnam(1:nsluv);
                bnam=deblank(bnam)';
                if strcmp(field,'barriers')
                    varargout={bnam(stationi)};
                else
                    bloc=waqua('readsds',sds,exper,'MESH_BARPOS');
                    bloc=reshape(bloc,[nsluv length(bloc)/nsluv]);
                    %
                    mstart = bloc(:,1);
                    nstart = bloc(:,2);
                    linebarrier = bloc(:,3)==5;
                    %
                    nslu = dim.nslu;
                    uline = linebarrier & ((1:nsluv)'<=nslu);
                    vline = linebarrier & ~uline;
                    %
                    mend = mstart;
                    nend = nstart;
                    nend(uline) = bloc(uline,4);
                    mend(vline) = bloc(vline,4);
                    %
                    sts = cell(dim.nbaruv,1);
                    ibarp = 0;
                    for ibar=1:nsluv
                        if linebarrier(ibar)
                            % split
                            if ibar<=nslu
                                % N
                                ninc = sign(nend(ibar)-nstart(ibar)+1);
                                for n = nstart(ibar):ninc:nend(ibar)
                                    ibarp = ibarp+1;
                                    sts{ibarp} = sprintf('%s (%i,%i)',bnam{ibar},mstart(ibar),n);
                                end
                            else
                                % M
                                minc = sign(mend(ibar)-mstart(ibar)+1);
                                for m = mstart(ibar):minc:mend(ibar)
                                    ibarp = ibarp+1;
                                    sts{ibarp} = sprintf('%s (%i,%i)',bnam{ibar},m,nstart(ibar));
                                end
                            end
                        else
                            ibarp = ibarp+1;
                            sts(ibarp) = bnam(ibar);
                        end
                    end
                    %
                    varargout={sts(stationi)};
                end
            otherwise
                stationi=local_argin(argin);
                sts=waqua('readsds',sds,exper,'CHECKPOINTS_FLOW_NAMCHK');
                switch field
                    case 'flowstat-wl'
                        sts=deblank(sts(1:nowl));
                        empty=cellfun('isempty',sts);
                        if any(empty)
                            MN=waqua('readsds',sds,exper,'CHECKPOINTS_FLOW_IWLPT');
                            MN=reshape(MN,[length(MN)/2 2]);
                            for i=find(empty)
                                sts{i}=sprintf('(M=%i,N=%i)',MN(i,:));
                            end
                        end
                    case 'flowstat-uv'
                        sts=deblank(sts(nowl+(1:nocur)));
                        empty=cellfun('isempty',sts);
                        if any(empty)
                            MN=waqua('readsds',sds,exper,'CHECKPOINTS_FLOW_ICURPT');
                            MN=reshape(MN,[length(MN)/2 2]);
                            for i=find(empty)
                                sts{i}=sprintf('(M=%i,N=%i)',MN(i,:));
                            end
                        end
                    case 'flowcrs-u'
                        sts=sts(nowl+nocur+(1:ntra));
                    case 'flowcrs-v'
                        sts=sts(nowl+nocur+ntra+(1:ntrav));
                    otherwise
                        error('Unexpected station type name ''%s''',field)
                end
                sts=sts';
                %
                varargout={sts(stationi)};
        end
        
    otherwise
        if length(field)>8 && strcmp(field(1:8),'stsubst:')
            Subs=lower(waquaio(sds,exper,'substances'));
            sbs=strtrim(field(9:end));
            for i_sub = 1: length(Subs)
                Subs{i_sub} = strtrim(Subs{i_sub});
            end
            s=ustrcmpi(sbs,Subs);
            if s>0
                iconta=waqua('readsds',sds,exper,'CONTROL_TRANS_ICONTA');
                icontb=waqua('readsds',sds,exper,'CONTROL_TRANS_ICONTB');
                % ref. "substances"
                nopol=iconta(3);
                lmax=icontb(2);
                %
                [tstep,stationi,k]=local_argin(argin);
                %
                stoffset=(stationi-1)*lmax+(s-1);
                krange=1+(0:kmax-1)*nopol*lmax;
                %
                data=waqua('readsds',sds,exper,'TIMEHISTORIES_TRANS',tstep);
                varargout={data.Data(:,stoffset+krange(k)) refdate+data.SimTime/1440};
            else
                error('Invalid substance name: %s',sbs)
            end
        else
            error('Unknown field: %s',field)
        end
end

%==========================================================================
% SPATIAL DATA
%==========================================================================
% 'MESH_H' 'real'
%           'h         (p:lgrid=mnmaxk)' 'Z'
% 'MESH_CURVIL' 'real'
%           'guu       (p:lgrid=mnmaxk)' 'U'
%           'gvv       (p:lgrid=mnmaxk)' 'V'
%           'xdep      (p:lgrid=mnmaxk)' 'D'
%           'ydep      (p:lgrid=mnmaxk)' 'D'
%           'xzeta     (p:lgrid=mnmaxk)' 'Z'
%           'yzeta     (p:lgrid=mnmaxk)' 'Z'
%           'xu        (p:lgrid=mnmaxk)' 'U'
%           'yu        (p:lgrid=mnmaxk)' 'U'
%           'xv        (p:lgrid=mnmaxk)' 'V'
%           'yv        (p:lgrid=mnmaxk)' 'V'
%           'rguu      (irlflg,p:lgrid=mnmaxk)' 'U'
%           'rgvv      (irlflg,p:lgrid=mnmaxk)' 'V'
%           'rxdep     (irlflg,p:lgrid=mnmaxk)' 'D'
%           'rydep     (irlflg,p:lgrid=mnmaxk)' 'D'
%           'rxzeta    (irlflg,p:lgrid=mnmaxk)' 'Z'
%           'ryzeta    (irlflg,p:lgrid=mnmaxk)' 'Z'
%           'rxu       (irlflg,p:lgrid=mnmaxk)' 'U'
%           'ryu       (irlflg,p:lgrid=mnmaxk)' 'U'
%           'rxv       (irlflg,p:lgrid=mnmaxk)' 'V'
%           'ryv       (irlflg,p:lgrid=mnmaxk)' 'V'
% 'MESH_DEPTH_DPS' 'real'
%           'dps       (p:lgrid=mnmaxk)' 'Z'
% 'MESH_DEPTH_DPD' 'real'
%           'dpd       (p:lgrid=mnmaxk)' 'D'
% 'MESH_DEPTH_DPU' 'real'
%           'dpu       (p:lgrid=mnmaxk)' 'U'
% 'MESH_DEPTH_DPV' 'real'
%           'dpv       (p:lgrid=mnmaxk)' 'V'
% 'MESH_KCS' 'int'
%           'kcs       (p:lgrid=mnmaxk)' 'Z'
% 'MESH_KCD' 'int'
%           'kcd       (p:lgrid=mnmaxk)' 'D'
%
% 'DPS_FLOW' 'real'
%           'dps       (p:lgrid=mnmaxk)' 'Z'
%
% 'SOLUTION_DRYWET' 'int'
%           'khu       (p:lgrid=mnmaxk)' 'U'
%           'khv       (p:lgrid=mnmaxk)' 'V'
%
% 'LAYER_INTERFACES' 'real'
%           'zk        (p:lgrid=mnmaxk,layer=0:kmax)' 'Z'
%           'zku       (p:lgrid=mnmaxk,layer=0:kmax)' 'U'
%           'zkv       (p:lgrid=mnmaxk,layer=0:kmax)' 'V'
%
% 'COEFF_GENERAL_DIFCO' 'real'
%           'difco     (p:lgrid=mnmaxk,layer=kmax)' 'Z' %Horizontal difco
% 'COEFF_GENERAL_KCD' 'real'
%           'wstr      (p:lgrid=mnmaxk)' 'Z' %Wind stress
% 'COEFF_GENERAL_KCDA' 'real'
%           'cdv1      (p:lgrid=mnmaxk)' 'Z' %Wind drag coeff
% 'COEFF_GENERAL_KCDB' 'real'
%           'cdv2      (p:lgrid=mnmaxk)' 'Z' %Wind drag coeff
% 'COEFF_GENERAL_FFZETA' 'real'
%           'ffzeta    (p:lgrid=mnmaxk)' 'Z' %Coriolis
%
% 'COEFF_FLOW_CMAN' 'real'
%           'cmanur    (p:lgrid=mnmaxk)' 'U' %Normal/flood rough
%           'cmanvr    (p:lgrid=mnmaxk)' 'V' %Normal/flood rough
%           'cmanue    (p:lgrid=mnmaxk)' 'U' %Ebb rough
%           'cmanve    (p:lgrid=mnmaxk)' 'V' %Ebb rough
% 'COEFF_FLOW_VICOWG' 'real'
%           'vicowg    (p:lgrid=mnmaxk,layer=0:kmax)' 'Z' %Wght matrix vico
% 'COEFF_FLOW_METROU' 'int'
%           'metrou    (p:lgrid=mnmaxk)' 'U=V' %Roughness method
% 'COEFF_FLOW_WAVES' 'real'
%           'wforcx    (p:lgrid=mnmaxk)' 'U' %Wave force u-comp
%           'wforcy    (p:lgrid=mnmaxk)' 'V' %Wave force v-comp
%           'hrms      (p:lgrid=mnmaxk)' 'Z' %Wave height
%           'period    (p:lgrid=mnmaxk)' 'Z' %Wave peak period
%           'dirx      (p:lgrid=mnmaxk)' 'U' %Wave unit vector u-comp
%           'diry      (p:lgrid=mnmaxk)' 'V' %Wave unit vector v-comp
% 'COEFF_FLOW_HORVISC' 'real'
%           'horvsc    (p:lgrid=mnmaxk)' 'Z' %Horizontal vico
%
% 'SOLUTION_FLOW_SEP' 'real'
%           'sep       (p:lgrid=mnmaxk)' 'Z' %Water level
% 'SOLUTION_FLOW_UP' 'real'
%           'up        (p:lgrid=mnmaxk,layer=kmax)' 'U' %U-velocity
% 'SOLUTION_FLOW_VP' 'real'
%           'vp        (p:lgrid=mnmaxk,layer=kmax)' 'V' %v-velocity
% 'SOLUTION_FLOW_W' 'real'
%           'w         (p:lgrid=mnmaxk,layer=0:kmax)' 'Z' %Omega-velocity
% 'SOLUTION_FLOW_WPHYS' 'real'
%           'wphys     (p:lgrid=mnmaxk,layer=kmax)' 'W' %W-velocity
% 'SOLUTION_FLOW_CZ' 'real'
%           'czu       (p:lgrid=mnmaxk)' 'U' %U-chezy
%           'czv       (p:lgrid=mnmaxk)' 'V' %V-chezy
% 'SOLUTION_FLOW_HU' 'real'
%           'hu        (p:lgrid=mnmaxk)' 'U' %Water depth u-point
% 'SOLUTION_FLOW_HV' 'real'
%           'hv        (p:lgrid=mnmaxk)' 'V' %Water depth v-point
%
% 'SOLUTION_HYDRODYNAMIC_PRESSURE' 'real'
%           'pressure  (p:lgrid=mnmaxk,layer=kmax)' 'Z' %Non-hydropress
% 'SOLUTION_HYDRODYNAMIC_WPHYSI' 'real'
%           'wphysi    (p:lgrid=mnmaxk,layer=0:kmax)' 'Z' %W-veloc
%
% 'INTEGRAL_FLOW_SEPNOW' 'real'
%           'sepnow    (p:lgrid=mnmaxk)' 'Z'
% 'INTEGRAL_FLOW_DEPINT' 'real'
%           'depint    (p:lgrid=mnmaxk)' 'Z'
% 'INTEGRAL_FLOW_UPINT' 'real'
%           'upint     (p:lgrid=mnmaxk,layer=kmax)' 'U'
% 'INTEGRAL_FLOW_VPINT' 'real'
%           'vpint     (p:lgrid=mnmaxk,layer=kmax)' 'V'
% 'INTEGRAL_FLOW_DISUNT' 'real'
%           'disunt    (p:lgrid=mnmaxk,layer=kmax)' 'U'
% 'INTEGRAL_FLOW_DISVNT' 'real'
%           'disvnt    (p:lgrid=mnmaxk,layer=kmax)' 'V'
% 'INTEGRAL_FLOW_WPHINT' 'real'
%           'wphint    (p:lgrid=mnmaxk,layer=kmax)' 'Z'
% 'INTEGRAL_FLOW_WINT' 'real'
%           'wint      (p:lgrid=mnmaxk,layer=0:kmax)' 'Z'
% 'INTEGRAL_FLOW_ZKINT' 'real'
%           'zkint     (p:lntgrid=mnmaxk,layer=0:kmax)' 'Z'
% 'INTEGRAL_FLOW_XDLAGR' 'real'
%           'xdlagr    (p:lgrid=mnmaxk,layer=0:kmax)' 'D' % lagra. x-disp
% 'INTEGRAL_FLOW_YDLAGR' 'real'
%           'ydlagr    (p:lgrid=mnmaxk,layer=0:kmax)' 'D' % lagra. y-disp
% 'INTEGRAL_FLOW_ZDLAGR' 'real'
%           'zdlagr    (p:lgrid=mnmaxk,layer=0:kmax)' 'D' % lagra. z-disp
% 'INTEGRAL_FLOW_ZK' 'real'
%           'zk        (p:lgrid=mnmaxk,layer=0:kmax)' 'Z'
%
% 'RESTART_FLOW_VICOW' 'real'
%           'vicow     (p:lgrid=mnmaxk,layer=0:kmax)' 'Z'
% 'RESTART_FLOW_QX' 'real'
%           'qx        (p:lgrid=mnmaxk)' 'U'
% 'RESTART_FLOW_QY' 'real'
%           'qy        (p:lgrid=mnmaxk)' 'V'
%
% 'RESTART_HYDRODYNAMIC_PRECONDITIONER' 'real'
%           'precon    (coef=1:nconct,p:lgrid=mnmaxk,layer=1:kmax)' 'Z'
% 'RESTART_HYDRODYNAMIC_DPRES' 'real'
%           'dpres     (p:lgrid=mnmaxk,layer=1:kmax)' 'Z'
%
% 'ASTRONOM_SPLIT' 'real'
%           'asplt     (mnmaxk,8,2)' '?'
%
% 'HARMONIC_TIDE_AZERO' 'real'
%           'azero     (mnmaxk)' 'Z'
% 'HARMONIC_TIDE_AMPL' 'real'
%           'ampl      (mnmaxk,ntidcs)' 'Z'
% 'HARMONIC_TIDE_PHASE' 'real'
%           'phase     (mnmaxk,ntidcs)' 'Z'
%
% 'SOLUTION_TRANS' 'real'
%           'rp        (p:lgrid=mnmaxk,layer=kmax,constit=lmax)' 'Z'
%
% 'SOLUTION_DIFCWM' 'real'
%           'difcwm    (p:lgrid=mnmaxk,layer=0:kmax)' 'Z'
%
% 'SOLUTION_TURB_ENERGY' 'real'
%           'energy    (p:lgrid=mnmaxk,layer=0:kmax)' 'Z'
% 'SOLUTION_TURB_DISSIP' 'real'
%           'dissip    (p:lgrid=mnmaxk,layer=0:kmax)' 'Z'
%
% 'SOLUTION_HORTUR_ENERGY' 'real'
%           'hener     (p:lgrid=mnmaxk)' 'Z'
% 'SOLUTION_HORTUR_DISSIP' 'real'
%           'hdiss     (p:lgrid=mnmaxk)' 'Z'
%
% 'SOLUTION_WAVES_WAVENUMBER' 'real'
%           'waveno    (p:lgrid=mnmaxk)' 'Z'
% 'SOLUTION_WAVES_STOKES' 'real'
%           'ustoks    (p:lgrid=mnmaxk)' 'U'
%           'vstoks    (p:lgrid=mnmaxk)' 'V'
% 'SOLUTION_WAVES_STRESS' 'real'
%           'taumu     (p:lgrid=mnmaxk)' 'U'
%           'taumv     (p:lgrid=mnmaxk)' 'V'
%
% 'RESTART_TRANS_DIFCW' 'real'
%           'difcw     (p:lgrid=mnmaxk,layer=0:kmax)' 'z'
%
% 'USERDATA_TRANS_SPAINP' 'int'
%           'spainp    (p:lgrid=mnmaxk,leninp)' 'Z'
%
% 'USER_SPATIM_TRANS' 'real'
%           'spatim    (p:lgrid=mnmaxk,lentdf)' 'Z'
%
% 'SOLUTION_USER_TRANS' 'real'
%           'solusr    (p:lgrid=mnmaxk,lensol)' 'Z'
%
% 'SOLUTION_DERIVED_MAXVALUES_SEP' 'real'
%           'septim    (p:lgrid=mnmaxk,flag=mx_wl_tim)' 'Z'
%           'sepmax    (p:lgrid=mnmaxk,flag=mx_wl_wl)' 'Z'
%           'sepflwu   (p:lgrid=mnmaxk,flag=mx_wl_fu)' 'Z'
%           'sepflwv   (p:lgrid=mnmaxk,flag=mx_wl_fv)' 'Z'
%           'sepflwm   (p:lgrid=mnmaxk,flag=mx_wl_fm)' 'Z'
%           'sepsal    (p:lgrid=mnmaxk,flag=mx_wl_sal)' 'Z'
%           'septmp    (p:lgrid=mnmaxk,flag=mx_wl_tmp)' 'Z'
%           'seprp     (p:lgrid=mnmaxk,constit=mx_wl_rp*nrp)' 'Z'
%           'sepflwx   (p:lgrid=mnmaxk,flag=mx_wl_fx)' 'Z'
%           'sepflwy   (p:lgrid=mnmaxk,flag=mx_wl_fy)' 'Z'
% 'SOLUTION_DERIVED_MAXVALUES_UP' 'real'
%           'uptim     (p:lgrid=mnmaxk,flag=mx_u_tim)' 'Z'
%           'upsep     (p:lgrid=mnmaxk,flag=mx_u_wl)' 'Z'
%           'upmax     (p:lgrid=mnmaxk,flag=mx_u_fu)' 'Z'
%           'upflwv    (p:lgrid=mnmaxk,flag=mx_u_fv)' 'Z'
%           'upflwm    (p:lgrid=mnmaxk,flag=mx_u_fm)' 'Z'
%           'upsal     (p:lgrid=mnmaxk,flag=mx_u_sal)' 'Z'
%           'uptmp     (p:lgrid=mnmaxk,flag=mx_u_tmp)' 'Z'
%           'uprp      (p:lgrid=mnmaxk,constit=mx_u_rp*nrp)' 'Z'
%           'upflwx    (p:lgrid=mnmaxk,flag=mx_u_fx)' 'Z'
%           'upflwy    (p:lgrid=mnmaxk,flag=mx_u_fy)' 'Z'
% 'SOLUTION_DERIVED_MAXVALUES_VP' 'real'
%           'vptim     (p:lgrid=mnmaxk,flag=mx_v_tim)' 'Z'
%           'vpsep     (p:lgrid=mnmaxk,flag=mx_v_wl)' 'Z'
%           'vpflwu    (p:lgrid=mnmaxk,flag=mx_v_fu)' 'Z'
%           'vpmax     (p:lgrid=mnmaxk,flag=mx_v_fv)' 'Z'
%           'vpflwm    (p:lgrid=mnmaxk,flag=mx_v_fm)' 'Z'
%           'vpsal     (p:lgrid=mnmaxk,flag=mx_v_sal)' 'Z'
%           'vptmp     (p:lgrid=mnmaxk,flag=mx_v_tmp)' 'Z'
%           'vprp      (p:lgrid=mnmaxk,constit=mx_v_rp*nrp)' 'Z'
%           'vpflwx    (p:lgrid=mnmaxk,flag=mx_v_fx)' 'Z'
%           'vpflwy    (p:lgrid=mnmaxk,flag=mx_v_fy)' 'Z'
% 'SOLUTION_DERIVED_MAXVALUES_MGN' 'real'
%           'mgntim    (p:lgrid=mnmaxk,flag=mx_mgn_tim)' 'Z'
%           'mgnsep    (p:lgrid=mnmaxk,flag=mx_mgn_wl)' 'Z'
%           'mgnflwu   (p:lgrid=mnmaxk,flag=mx_mgn_fu)' 'Z'
%           'mgnflwv   (p:lgrid=mnmaxk,flag=mx_mgn_fv)' 'Z'
%           'mgnmax    (p:lgrid=mnmaxk,flag=mx_mgn_fm)' 'Z'
%           'mgnsal    (p:lgrid=mnmaxk,flag=mx_mgn_sal)' 'Z'
%           'mgntmp    (p:lgrid=mnmaxk,flag=mx_mgn_tmp)' 'Z'
%           'mgnrp     (p:lgrid=mnmaxk,constit=mx_mgn_rp*nrp)' 'Z'
%           'mgnflwx   (p:lgrid=mnmaxk,flag=mx_mgn_fx)' 'Z'
%           'mgnflwy   (p:lgrid=mnmaxk,flag=mx_mgn_fy)' 'Z'
% 'SOLUTION_DERIVED_MAXVALUES_SAL' 'real'
%           'saltim    (p:lgrid=mnmaxk,flag=mx_sal_tim)' 'Z'
%           'salsep    (p:lgrid=mnmaxk,flag=mx_sal_wl)' 'Z'
%           'salflwu   (p:lgrid=mnmaxk,flag=mx_sal_fu)' 'Z'
%           'salflwv   (p:lgrid=mnmaxk,flag=mx_sal_fv)' 'Z'
%           'salflwm   (p:lgrid=mnmaxk,flag=mx_sal_fm)' 'Z'
%           'salmax    (p:lgrid=mnmaxk,flag=mx_sal_sal)' 'Z'
%           'saltmp    (p:lgrid=mnmaxk,flag=mx_sal_tmp)' 'Z'
%           'salrp     (p:lgrid=mnmaxk,constit=mx_sal_rp*nrp)' 'Z'
%           'salflwx   (p:lgrid=mnmaxk,flag=mx_sal_fx)' 'Z'
%           'salflwy   (p:lgrid=mnmaxk,flag=mx_sal_fy)' 'Z'
% 'SOLUTION_DERIVED_MAXVALUES_TEMP' 'real'
%           'tmptim    (p:lgrid=mnmaxk,flag=mx_tmp_tim)' 'Z'
%           'tmpsep    (p:lgrid=mnmaxk,flag=mx_tmp_wl)' 'Z'
%           'tmpflwu   (p:lgrid=mnmaxk,flag=mx_tmp_fu)' 'Z'
%           'tmpflwv   (p:lgrid=mnmaxk,flag=mx_tmp_fv)' 'Z'
%           'tmpflwm   (p:lgrid=mnmaxk,flag=mx_tmp_fm)' 'Z'
%           'tmpsal    (p:lgrid=mnmaxk,flag=mx_tmp_sal)' 'Z'
%           'tmpmax    (p:lgrid=mnmaxk,flag=mx_tmp_tmp)' 'Z'
%           'tmprp     (p:lgrid=mnmaxk,constit=mx_tmp_rp*nrp)' 'Z'
%           'tmpflwx   (p:lgrid=mnmaxk,flag=mx_tmp_fx)' 'Z'
%           'tmpflwy   (p:lgrid=mnmaxk,flag=mx_tmp_fy)' 'Z'
% 'SOLUTION_DERIVED_MAXVALUES_RP' 'real'
%           'rptim     (p:lgrid=mnmaxk,constit=mx_rp_tim*nrp)' 'Z'
%           'rpsep     (p:lgrid=mnmaxk,constit=mx_rp_wl*nrp)' 'Z'
%           'rpflwu    (p:lgrid=mnmaxk,constit=mx_rp_fu*nrp)' 'Z'
%           'rpflwv    (p:lgrid=mnmaxk,constit=mx_rp_fv*nrp)' 'Z'
%           'rpflwm    (p:lgrid=mnmaxk,constit=mx_rp_fm*nrp)' 'Z'
%           'rpsal     (p:lgrid=mnmaxk,constit=mx_rp_sal*nrp)' 'Z'
%           'rptmp     (p:lgrid=mnmaxk,constit=mx_rp_tmp*nrp)' 'Z'
%           'rpmax     (p:lgrid=mnmaxk,constit=mx_rp_rp*nrp)' 'Z'
%           'rpflwx    (p:lgrid=mnmaxk,constit=mx_rp_fx*nrp)' 'Z'
%           'rpflwy    (p:lgrid=mnmaxk,constit=mx_rp_fy*nrp)' 'Z'
% 'SOLUTION_DERIVED_MINVALUES_SEP' 'real'
%           'septim    (p:lgrid=mnmaxk,flag=mn_wl_tim)' 'Z'
%           'sepmin    (p:lgrid=mnmaxk,flag=mn_wl_wl)' 'Z'
%           'sepflwu   (p:lgrid=mnmaxk,flag=mn_wl_fu)' 'Z'
%           'sepflwv   (p:lgrid=mnmaxk,flag=mn_wl_fv)' 'Z'
%           'sepflwm   (p:lgrid=mnmaxk,flag=mn_wl_fm)' 'Z'
%           'sepsal    (p:lgrid=mnmaxk,flag=mn_wl_sal)' 'Z'
%           'septmp    (p:lgrid=mnmaxk,flag=mn_wl_tmp)' 'Z'
%           'seprp     (p:lgrid=mnmaxk,constit=mn_wl_rp*nrp)' 'Z'
%           'sepflwx   (p:lgrid=mnmaxk,flag=mn_wl_fx)' 'Z'
%           'sepflwy   (p:lgrid=mnmaxk,flag=mn_wl_fy)' 'Z'
% 'SOLUTION_DERIVED_MINVALUES_UP' 'real'
%           'uptim     (p:lgrid=mnmaxk,flag=mn_u_tim)' 'Z'
%           'upsep     (p:lgrid=mnmaxk,flag=mn_u_wl)' 'Z'
%           'upmin     (p:lgrid=mnmaxk,flag=mn_u_fu)' 'Z'
%           'upflwv    (p:lgrid=mnmaxk,flag=mn_u_fv)' 'Z'
%           'upflwm    (p:lgrid=mnmaxk,flag=mn_u_fm)' 'Z'
%           'upsal     (p:lgrid=mnmaxk,flag=mn_u_sal)' 'Z'
%           'uptmp     (p:lgrid=mnmaxk,flag=mn_u_tmp' 'Z'
%           'uprp      (p:lgrid=mnmaxk,constit=mn_u_rp*nrp)' 'Z'
%           'upflwx    (p:lgrid=mnmaxk,flag=mn_u_fx)' 'Z'
%           'upflwy    (p:lgrid=mnmaxk,flag=mn_u_fy)' 'Z'
% 'SOLUTION_DERIVED_MINVALUES_VP' 'real'
%           'vptim     (p:lgrid=mnmaxk,flag=mn_v_tim)' 'Z'
%           'vpsep     (p:lgrid=mnmaxk,flag=mn_v_wl)' 'Z'
%           'vpflwu    (p:lgrid=mnmaxk,flag=mn_v_fu)' 'Z'
%           'vpmin     (p:lgrid=mnmaxk,flag=mn_v_fv)' 'Z'
%           'vpflwm    (p:lgrid=mnmaxk,flag=mn_v_fm)' 'Z'
%           'vpsal     (p:lgrid=mnmaxk,flag=mn_v_sal)' 'Z'
%           'vptmp     (p:lgrid=mnmaxk,flag=mn_v_tmp)' 'Z'
%           'vprp      (p:lgrid=mnmaxk,constit=mn_v_rp*nrp)' 'Z'
%           'vpflwx    (p:lgrid=mnmaxk,flag=mn_v_fx)' 'Z'
%           'vpflwy    (p:lgrid=mnmaxk,flag=mn_v_fy)' 'Z'
% 'SOLUTION_DERIVED_MINVALUES_MGN' 'real'
%           'mgntim    (p:lgrid=mnmaxk,flag=mn_mgn_tim)' 'Z'
%           'mgnsep    (p:lgrid=mnmaxk,flag=mn_mgn_wl)' 'Z'
%           'mgnflwu   (p:lgrid=mnmaxk,flag=mn_mgn_fu)' 'Z'
%           'mgnflwv   (p:lgrid=mnmaxk,flag=mn_mgn_fv)' 'Z'
%           'mgnmin    (p:lgrid=mnmaxk,flag=mn_mgn_fm)' 'Z'
%           'mgnsal    (p:lgrid=mnmaxk,flag=mn_mgn_sal)' 'Z'
%           'mgntmp    (p:lgrid=mnmaxk,flag=mn_mgn_tmp)' 'Z'
%           'mgnrp     (p:lgrid=mnmaxk,constit=mn_mgn_rp*nrp)' 'Z'
%           'mgnflwx   (p:lgrid=mnmaxk,flag=mn_mgn_fx)' 'Z'
%           'mgnflwy   (p:lgrid=mnmaxk,flag=mn_mgn_fy)' 'Z'
% 'SOLUTION_DERIVED_MINVALUES_SAL' 'real'
%           'saltim    (p:lgrid=mnmaxk,flag=mn_sal_tim)' 'Z'
%           'salsep    (p:lgrid=mnmaxk,flag=mn_sal_wl)' 'Z'
%           'salflwu   (p:lgrid=mnmaxk,flag=mn_sal_fu)' 'Z'
%           'salflwv   (p:lgrid=mnmaxk,flag=mn_sal_fv)' 'Z'
%           'salflwm   (p:lgrid=mnmaxk,flag=mn_sal_fm)' 'Z'
%           'salmin    (p:lgrid=mnmaxk,flag=mn_sal_sal)' 'Z'
%           'saltmp    (p:lgrid=mnmaxk,flag=mn_sal_tmp)' 'Z'
%           'salrp     (p:lgrid=mnmaxk,constit=mn_sal_rp*nrp)' 'Z'
%           'salflwx   (p:lgrid=mnmaxk,flag=mn_sal_fx)' 'Z'
%           'salflwy   (p:lgrid=mnmaxk,flag=mn_sal_fy)' 'Z'
% 'SOLUTION_DERIVED_MINVALUES_TEMP' 'real'
%           'tmptim    (p:lgrid=mnmaxk,flag=mn_tmp_tim)' 'Z'
%           'tmpsep    (p:lgrid=mnmaxk,flag=mn_tmp_wl)' 'Z'
%           'tmpflwu   (p:lgrid=mnmaxk,flag=mn_tmp_fu)' 'Z'
%           'tmpflwv   (p:lgrid=mnmaxk,flag=mn_tmp_fv)' 'Z'
%           'tmpflwm   (p:lgrid=mnmaxk,flag=mn_tmp_fm)' 'Z'
%           'tmpsal    (p:lgrid=mnmaxk,flag=mn_tmp_sal)' 'Z'
%           'tmpmin    (p:lgrid=mnmaxk,flag=mn_tmp_tmp)' 'Z'
%           'tmprp     (p:lgrid=mnmaxk,constit=mn_tmp_rp*nrp)' 'Z'
%           'tmpflwx   (p:lgrid=mnmaxk,flag=mn_tmp_fx)' 'Z'
%           'tmpflwy   (p:lgrid=mnmaxk,flag=mn_tmp_fy)' 'Z'
% 'SOLUTION_DERIVED_MINVALUES_RP' 'real'
%           'rptim     (p:lgrid=mnmaxk,constit=mn_rp_tim*nrp)' 'Z'
%           'rpsep     (p:lgrid=mnmaxk,constit=mn_rp_wl*nrp)' 'Z'
%           'rpflwu    (p:lgrid=mnmaxk,constit=mn_rp_fu*nrp)' 'Z'
%           'rpflwv    (p:lgrid=mnmaxk,constit=mn_rp_fv*nrp)' 'Z'
%           'rpflwm    (p:lgrid=mnmaxk,constit=mn_rp_fm*nrp)' 'Z'
%           'rpsal     (p:lgrid=mnmaxk,constit=mn_rp_sal*nrp)' 'Z'
%           'rptmp     (p:lgrid=mnmaxk,constit=mn_rp_tmp*nrp)' 'Z'
%           'rpmin     (p:lgrid=mnmaxk,constit=mn_rp_rp*nrp)' 'Z'
%           'rpflwx    (p:lgrid=mnmaxk,constit=mn_rp_fx*nrp)' 'Z'
%           'rpflwy    (p:lgrid=mnmaxk,constit=mn_rp_fy*nrp)' 'Z'
%
% 'PROBLEM_KALM_WIND_WEIGHT' 'real'
%           'wghtwu    (4,mnmaxk)' 'U' %Wind weight coefficients
%           'wghtwv    (4,mnmaxk)' 'V' %Wind weight coefficients
% 'PROBLEM_KALM_WIND_PARPT' 'int'
%           'ipparw    (4,mnmaxk)' 'U=V' %Wind index
% 'PROBLEM_KALM_VISC_WEIGHT' 'real'
%           'wghtv     (8,mnmaxk,layer=0:kmax)' %Visc noise wghts
% 'PROBLEM_KALM_VISC_PARPT' 'int'
%           'ipparv    (8,mnmaxk,layer=0:kmax)' %Visc noise index
%
% 'RESTART_KALM_STEADY_STATE' 'real'
%           'xup       (p:lgrid=mnmaxk)' 'U'
%           'xvp       (p:lgrid=mnmaxk)' 'V'
%           'xsep      (p:lgrid=mnmaxk)' 'Z'
%           'cirkal    (2,layer=kmax,noroco)' 'X'
%           'wukal     (p:lgrid=mnmaxk)' 'U'
%           'wvkal     (p:lgrid=mnmaxk)' 'V'
%
function varargout = waqua_get_spatial(sds,exper,field,dim,refdate,argin)
if ~waqua('exists',sds,exper,'MESH_IDIMEN')
    %
    %WAQWND
    %
    switch field
        case {'zgrid','dgrid'}
            [n,m]=local_argin(argin);
            coords = waqua('readsds',sds,exper,'MESH01_GENERAL_COOR');
            %  1 : dx
            %  2 : dy
            %  3 : dz
            %  4 : x0
            %  5 : y0
            %  6 : z0
            %  7 : grdang
            %  8 : latsp
            %  9 : lonsp
            x0 = coords(4);
            y0 = coords(5);
            dx = coords(1);
            dy = coords(2);
            if strcmp(field,'zgrid')
                shft = 1.5;
            else
                shft = 1;
            end
            x = x0+repmat(dx*(m-shft),length(n),1);
            y = y0+repmat(dy*(n'-shft),1,length(m));
            if strcmp(field,'zgrid')
                x(:,m==dim.mmax | m==1) = NaN;
                y(:,m==dim.mmax | m==1) = NaN;
                x(n==dim.nmax | n==1,:) = NaN;
                y(n==dim.nmax | n==1,:) = NaN;
            else
                x(:,m==dim.mmax) = NaN;
                y(:,m==dim.mmax) = NaN;
                x(n==dim.nmax,:) = NaN;
                y(n==dim.nmax,:) = NaN;
            end
            if dim.spheric==10
                latsp = coords(8);%*1000*pi/180;
                lonsp = coords(9);%*1000*pi/180;
                [x,y]=qp_proj_rotatepole(x,y,lonsp,latsp,0);
            end
            varargout = {x y};
        case {'wind','press','charnock'}
            [tstep,n,m]=local_argin(argin);
            nmfull = reshape(1:dim.mmax*dim.nmax,dim.mmax,dim.nmax)';
            nm = nmfull(n,m);
            %
            nmmax = dim.nmax*dim.mmax;
            switch field
                case 'wind'
                    % waqua wind file: wind in x/y direction
                    windu = waqua('readsds',sds,exper,'SOLUTION_WIND',tstep,1:nmmax);
                    time = refdate+windu.SimTime/1440;
                    windv = waqua('readsds',sds,exper,'SOLUTION_WIND',tstep,nmmax+(1:nmmax));
                    %
                    mgd = waqua('readsds',sds,exper,'MESH01_GENERAL_DIMENSIONS');
                    if mgd(8)==13
                        % 13: staggered components at u/v points
                        [windu,windv]=uv2xy(sds,exper,dim,n,m,nmfull,1,windu.Data,windv.Data,refdate);
                    else
                        % 11: collocated components at grid point
                        windu = windu.Data(nm);
                        windv = windv.Data(nm);
                    end
                    %
                    varargout = {windu windv time};
                case 'press'
                    press = waqua('readsds',sds,exper,'SOLUTION_PRESS',tstep,1:nmmax);
                    time = refdate+press.SimTime/1440;
                    press = press.Data(nm);
                    varargout = {press time};
                case 'charnock'
                    press = waqua('readsds',sds,exper,'SOLUTION_CHARNOCK',tstep,1:nmmax);
                    time = refdate+press.SimTime/1440;
                    press = press.Data(nm);
                    varargout = {press time};
            end
    end
    return
end
%
[nm,sact]=getspace(sds,exper,dim);
nmax = dim.nmax;
mmax = dim.mmax;
kmax = dim.kmax;
npnt = dim.npnt;
inact = dim.inact;
sph_dupl = dim.sph_dupl;
%
switch field
    case {'grid','dgrid','zgrid','ugrid','vgrid'}
        [n,m]=local_argin(argin);
        nm=nm(n,m);
        sznm=size(nm);
        %-----
        %conmsh=waqua('readsds',sds,exper,'MESH_CONMSH');
        %  1: DX = GridSize X dir.
        %  2: DY = GridSize Y dir.
        %  3: DKSI = GridSize Transformed Grid (=1.)
        %  4: ANGLAT = Latitude (Deg.)
        %  5: RLAMBD = East Longuitude Point(1,1) (Deg.)
        %  6: FI = Latitude Point(1,1) (Deg.)
        %  7: GRDANG = Clockwise Angle from Y to North (Deg.)
        %  8: DLAMBD = GridCellAngle X dir. (Deg.)
        %  9: DFI = GridCellAngle Y dir. (Deg.)
        % 10: REARTH = Radius of Earth
        
        %data=waqua('readsds',sds,exper,'MESH_CURVIL');
        % CURVIL: GUU, GVV, XDEP, YDEP, XZETA, YZETA, XU, YU, XV, YV
        switch field
            case {'grid','dgrid'}
                offset=2;
            case 'zgrid'
                offset=4;
            case 'ugrid'
                offset=6;
            case 'vgrid'
                offset=8;
        end
        %
        x=waqua('readsds',sds,exper,'MESH_CURVIL',0,(sph_dupl*10+offset)*npnt+(1:npnt));
        %x=data((sph_dupl*10+offset)*npnt+(1:npnt));
        y=waqua('readsds',sds,exper,'MESH_CURVIL',0,(sph_dupl*10+offset+1)*npnt+(1:npnt));
        %y=data((sph_dupl*10+offset+1)*npnt+(1:npnt));
        x(inact)=NaN;
        y(inact)=NaN;
        x((x==0)&(y==0))=NaN;
        y(isnan(x))=NaN;
        X=x(nm); X=reshape(X,sznm);
        Y=y(nm); Y=reshape(Y,sznm);
        X(n==nmax,:) = NaN;
        Y(n==nmax,:) = NaN;
        X(:,m==mmax) = NaN;
        Y(:,m==mmax) = NaN;
        varargout={X Y};
        
    case 'wind'
        [tstep,n,m]=local_argin(argin);
        % waqua result file: wind interpolated to u/v point
        windu = waqua('readsds',sds,exper,'FORCINGS_SVWP_WINDU',tstep);
        time = refdate+windu.SimTime/1440;
        windu = windu.Data(nm);
        windv = waqua('readsds',sds,exper,'FORCINGS_SVWP_WINDV',tstep);
        windv = windv.Data(nm);
        %
        mgd = waqua('readsds',sds,exper,'MESH01_GENERAL_DIMENSIONS');
        if mgd(8)==14 % 11: colocated components at grid point
            % staggered components at u/v points
            [windu,windv]=uv2xy(sds,exper,dim,n,m,nm,1,windu,windv,refdate);
        end
        varargout = {windu windv time};

    case 'press'
        [tstep,n,m]=local_argin(argin);
        nm=nm(n,m);
        press = waqua('readsds',sds,exper,'FORCINGS_SVWP_PRESSURE',tstep);
        time = refdate+press.SimTime/1440;
        press = press.Data(nm);
        varargout = {press time};
        
    case {'zgrid3d','ugrid3d','vgrid3d','zgrid3di','ugrid3di','vgrid3di','zgrid3dc','ugrid3dc','vgrid3dc'}
        [tstep,n,m,k]=local_argin(argin);
        nm=nm(n,m);
        sznm=size(nm);
        num_n = size(nm,1);
        num_m = size(nm,2);
        %
        if field(end)=='i'
            layer = 'i';
            if ischar(k), k=1:kmax+1; end
        else
            layer = 'c';
            if field(end)=='c'
                field = field(1:end-1);
            end
            if ischar(k), k=1:kmax; end
        end
        num_k = length(k);
        %-----
        %conmsh=waqua('readsds',sds,exper,'MESH_CONMSH');
        %  1: DX = GridSize X dir.
        %  2: DY = GridSize Y dir.
        %  3: DKSI = GridSize Transformed Grid (=1.)
        %  4: ANGLAT = Latitude (Deg.)
        %  5: RLAMBD = East Longuitude Point(1,1) (Deg.)
        %  6: FI = Latitude Point(1,1) (Deg.)
        %  7: GRDANG = Clockwise Angle from Y to North (Deg.)
        %  8: DLAMBD = GridCellAngle X dir. (Deg.)
        %  9: DFI = GridCellAngle Y dir. (Deg.)
        % 10: REARTH = Radius of Earth
        
        %data=waqua('readsds',sds,exper,'MESH_CURVIL');
        % CURVIL: GUU, GVV, XDEP, YDEP, XZETA, YZETA, XU, YU, XV, YV
        switch field
            case {'zgrid3d','zgrid3di'}
                offset=4;
                zoffset=0;
            case {'ugrid3d','ugrid3di'}
                offset=6;
                zoffset=1;
            case {'vgrid3d','vgrid3di'}
                offset=8;
                zoffset=2;
        end
        x=waqua('readsds',sds,exper,'MESH_CURVIL',0,(sph_dupl*10+offset)*npnt+(1:npnt));
        %x=data((sph_dupl*10+offset)*npnt+(1:npnt));
        y=waqua('readsds',sds,exper,'MESH_CURVIL',0,(sph_dupl*10+offset+1)*npnt+(1:npnt));
        %y=data((sph_dupl*10+offset+1)*npnt+(1:npnt));
        x(inact)=NaN; y(inact)=NaN;
        X=x(nm); X=reshape(X,sznm);
        Y=y(nm); Y=reshape(Y,sznm);
        data=waqua('readsds',sds,exper,'LAYER_INTERFACES',tstep);
        if iscell(data.Data)
            if isempty(data.Data{1})
                error('LAYER_INTERFACES not available for requested time step.')
            else
                z=data.Data{1}(zoffset*npnt*(kmax+1)+(1:(npnt*(kmax+1))));
            end
        else
            z=data.Data(zoffset*npnt*(kmax+1)+(1:(npnt*(kmax+1))));
        end
        z(inact+(0:kmax)*npnt)=NaN;
        Z=zeros(num_n,num_m,num_k);
        for i=1:num_k
            ik = k(i);
            if strcmp(layer,'i')
                Z(:,:,i) = z(nm+(ik-1)*npnt);
            else
                Z(:,:,i) = (z(nm+(ik-1)*npnt) + z(nm+ik*npnt))/2;
            end
        end
        X=repmat(X,[1 1 num_k]);
        Y=repmat(Y,[1 1 num_k]);
        varargout={X Y Z refdate+data.SimTime/1440};
        
    case {'depth','height','depth_wl_points','height_wl_points'}
        [n,m]=local_argin(argin);
        nm=nm(n,m);
        switch field
            case {'height_wl_points','depth_wl_points'}
                sact = sact(n,m);
                %DPS_FLOW only for Waqua
                data=waqua('readsds',sds,exper,'DPS_FLOW');
                data(inact)=NaN;
                data(data==-999)=NaN;
                Dep=data(nm);
                Dep(~sact)=NaN;
            otherwise
                sact = sact(n,m);
                dact = sact | sact([2:end end],:) | ...
                    sact(:,[2:end end]) | sact([2:end end],[2:end end]);
                data=waqua('readsds',sds,exper,'MESH_H');
                data(inact)=NaN;
                data(data==-999)=NaN;
                Dep=data(nm);
                Dep(~dact)=NaN;
        end
        switch field
            case {'height','height_wl_points'}
                varargout={-Dep};
            otherwise
                varargout={Dep};
        end
        
    case {'drywet'}
        [tstep,n,m]=local_argin(argin);
        nm=nm(n,m);
        %-----
        thd=waqua('readsds',sds,exper,'SOLUTION_DRYWET',tstep);
        if ~isempty(thd.Data)
            THDu=thd.Data(1:npnt);
            THDu=THDu(nm);
            %THDu=reshape(THDu,sznm);
            THDv=thd.Data(npnt+(1:npnt));
            THDv=THDv(nm);
            %THDv=reshape(THDv,sznm);
        else
            THDu=[];
            THDv=[];
        end
        varargout={THDu THDv refdate+thd.SimTime/1440};
        
    case {'veloc','veloc0','disch','disch0','udisch','udisch0','dischpot','xyveloc','xyudisch','wforce','wdir','wvec','wstress','stokes'}
        [tstep,n,m,k]=local_argin(argin);
        if ischar(n), n=1:nmax; end
        if ischar(m), m=1:mmax; end
        nmfull=nm;
        if ~strcmp(field,'dischpot')
            nm=nmfull(n,m);
            sact=sact(n,m);
        end
        sznm=size(nm);
        psznm=prod(sznm);
        %-----
        switch field
            case 'wforce'
                kmax=1;
                k=1;
                num_k=1;
                u=waqua('readsds',sds,exper,'COEFF_FLOW_WAVES',tstep,1:npnt);
                v=waqua('readsds',sds,exper,'COEFF_FLOW_WAVES',tstep,npnt+(1:npnt));
            case {'wdir','wvec'}
                kmax=1;
                k=1;
                num_k=1;
                u=waqua('readsds',sds,exper,'COEFF_FLOW_WAVES',tstep,4*npnt+(1:npnt));
                v=waqua('readsds',sds,exper,'COEFF_FLOW_WAVES',tstep,5*npnt+(1:npnt));
            case 'stokes'
                kmax=1;
                k=1;
                num_k=1;
                u=waqua('readsds',sds,exper,'SOLUTION_WAVES_STOKES',tstep,1:npnt);
                v=waqua('readsds',sds,exper,'SOLUTION_WAVES_STOKES',tstep,npnt+(1:npnt));
            case 'wstress'
                kmax=1;
                k=1;
                num_k=1;
                u=waqua('readsds',sds,exper,'SOLUTION_WAVES_STRESS',tstep,1:npnt);
                v=waqua('readsds',sds,exper,'SOLUTION_WAVES_STRESS',tstep,npnt+(1:npnt));
            otherwise
                if ischar(k), k=1:kmax; end
                num_k = length(k);
                u=waqua('readsds',sds,exper,'SOLUTION_FLOW_UP',tstep);
                v=waqua('readsds',sds,exper,'SOLUTION_FLOW_VP',tstep);
        end
        % SOLUTION_DRYWET (KHU,KHV: DRY/FLOODING VELOCITY POINTS)
        inact=inact+(0:kmax-1)*npnt;
        switch field
            case {'veloc0','xyveloc','xyudisch','wforce','wdir','wvec'}
                if strcmp(field,'xyudisch')
                    if kmax>=2
                        error('Discharge not supported for kmax>=2')
                    end
                    tmp=waqua('readsds',sds,exper,'SOLUTION_FLOW_HU',tstep);
                    U=u.Data.*tmp.Data; % U*HU
                    tmp=waqua('readsds',sds,exper,'SOLUTION_FLOW_HV',tstep);
                    V=v.Data.*tmp.Data; % V*HV
                else
                    U=u.Data;
                    V=v.Data;
                end
                if isempty(U) || isempty(V)
                    varargout={[] [] refdate+u.SimTime/1440};
                    return
                end
                U(inact)=NaN;
                V(inact)=NaN;
            case {'disch0','disch','dischpot','udisch0','udisch'}
                if kmax>=2
                    error('Discharge not supported for kmax>=2')
                end
                %data=waqua('readsds',sds,exper,'MESH_CURVIL');
                % CURVIL: GUU, GVV, XDEP, YDEP, XZETA, YZETA, XU, YU, XV, YV
                %
                % only for kmax==2
                %
                tmp=waqua('readsds',sds,exper,'SOLUTION_FLOW_HU',tstep);
                tmp=tmp.Data;
                switch field
                    case {'udisch','udisch0'}
                        UU=u.Data.*tmp; % U*HU
                    otherwise
                        guu = waqua('readsds',sds,exper,'MESH_CURVIL',0,(1:npnt)+(10*sph_dupl)*npnt);
                        UU=u.Data.*tmp.*guu'; % U*HU*GUU
                end
                tmp=waqua('readsds',sds,exper,'SOLUTION_FLOW_HV',tstep);
                tmp=tmp.Data;
                switch field
                    case {'udisch','udisch0'}
                        VV=v.Data.*tmp; % V*HV
                    otherwise
                        gvv = waqua('readsds',sds,exper,'MESH_CURVIL',0,(1:npnt)+(10*sph_dupl+1)*npnt);
                        VV=v.Data.*tmp.*gvv'; % V*HV*GVV
                end
                %
                % for all kmax
                %
                switch field
                    case {'disch0','udisch0'}
                        UU(inact)=NaN; VV(inact)=NaN;
                        U=UU(nm);
                        V=VV(nm);
                    case {'disch','udisch'}
                        UU(inact)=NaN;
                        VV(inact)=NaN;
                        ndm=nm([1 1:(nmax-1)],:);
                        nmd=nm(:,[1 1:(mmax-1)]);
                        U = zeros([sznm num_k]);
                        V = U;
                        for i=1:num_k
                            ik = k(i);
                            nms=(ik-1)*npnt;
                            U(:,:,i)=.5*( UU(nm+nms) + UU(nmd+nms) );
                            V(:,:,i)=.5*( VV(nm+nms) + VV(ndm+nms) );
                        end
                    case 'dischpot'
                        U=UU(nm);
                        V=VV(nm);
                        U(isnan(U))=0;
                        V(isnan(V))=0;
                        %
                        % if boundary lies on gridline 1, we can use ...
                        %
                        %   offset=cumsum(V(1,:),2);
                        %
                        % else we have to do something like ...
                        %
                        [mmax,i]=max(sact,[],1);
                        j=find(mmax~=0); i(mmax==0)=[];
                        ind=sub2ind(size(sact),i,j);
                        ind1=sub2ind(size(sact),i,max(j-1,1));
                        offset=zeros([1 size(U,2)]);
                        offset(j)=V(ind)+U(ind)-U(ind1);
                        offset=cumsum(offset);
                        %
                        % compute discharge potential ...
                        %
                        data=cumsum(U,1)-repmat(offset,[size(U,1) 1]);
                        %
                        % the same result should be obtained when doing the
                        % above in the alternate direction ...
                        %
                        %    data=cumsum(V,2)-repmat(cumsum(U(:,1),1),[1 size(V,2)]);
                        %
                        % finally, make sure dat the potential is positive ...
                        %
                        data=data-min(data(:));
                        data=data(n,m);
                        varargout={data refdate+u.SimTime/1440};
                        return
                    otherwise
                        U=UU(nm);
                        V=VV(nm);
                end
            case 'veloc'
                UU=u.Data;
                VV=v.Data;
                ndm=nmfull(max(1,n-1),m);
                nmd=nmfull(n,max(1,m-1));
                UU(inact)=NaN; VV(inact)=NaN;
                U = zeros([sznm num_k]);
                V = U;
                for i=1:num_k
                    ik=k(i);
                    nms=(ik-1)*npnt;
                    U(:,:,i)=.5*( UU(nm+nms) + UU(nmd+nms) );
                    V(:,:,i)=.5*( VV(nm+nms) + VV(ndm+nms) );
                end
        end
        %
        switch field
            case {'xyveloc','xyudisch','wforce','wdir','wvec'}
                [U,V]=uv2xy(sds,exper,dim,n,m,nmfull,k,U,V,refdate);
                %
                if strcmp(field,'wvec')
                    tmp=waqua('readsds',sds,exper,'COEFF_FLOW_WAVES',tstep,2*npnt+(1:npnt));
                    U = U.*tmp.Data(nm);
                    V = V.*tmp.Data(nm);
                end
        end
        %
        if strcmp(field,'veloc0') || (nargout==4)
            %
            % Define nmk if needed
            %
            nmk=repmat(nm,[1 1 num_k]);
            for i=1:num_k
                ik = k(i);
                nmk(:,:,i)=nmk(:,:,i)+(ik-1)*npnt;
            end
        end
        %
        if strcmp(field,'veloc0')
            U=U(nmk);
            V=V(nmk);
        else
            U=reshape(U,[psznm num_k]); U(~sact,:)=NaN; U=reshape(U,[sznm num_k]);
            V=reshape(V,[psznm num_k]); V(~sact,:)=NaN; V=reshape(V,[sznm num_k]);
        end
        %
        if nargout==4
            w=waqua('readsds',sds,exper,'SOLUTION_FLOW_WPHYS',tstep);
            W=w.Data;
            if ~isempty(W)
                W(inact)=NaN;
                W=W(nmk);
                W=reshape(W,[psznm num_k]); W(~sact,:)=NaN; W=reshape(W,[sznm num_k]);
            else
                W=[];
            end
            varargout={U V W refdate+u.SimTime/1440};
        else
            varargout={U V refdate+u.SimTime/1440};
        end
        
    case {'wlvl','waterlevel','head','wdepth','hrms','peakperiod','wavenumber'}
        %
        % Time dependent 2D quantities at cell centre
        %
        [tstep,n,m]=local_argin(argin);
        nm=nm(n,m);
        sact=sact(n,m);
        %-----
        switch field
            case {'wlvl','waterlevel','head','wdepth'}
                var=waqua('readsds',sds,exper,'SOLUTION_FLOW_SEP',tstep);
            case 'hrms'
                var=waqua('readsds',sds,exper,'COEFF_FLOW_WAVES',tstep,2*npnt+(1:npnt));
            case 'peakperiod'
                var=waqua('readsds',sds,exper,'COEFF_FLOW_WAVES',tstep,3*npnt+(1:npnt));
            case 'wavenumber'
                var=waqua('readsds',sds,exper,'SOLUTION_WAVES_WAVENUMBER',tstep);
            otherwise
                var=waqua('readsds',sds,exper,field,tstep);
        end
        VAR=var.Data;
        if strcmp(field,'wdepth')
            dps=waqua('readsds',sds,exper,'DPS_FLOW');
            dps(dps>400)=NaN;
            VAR = VAR+reshape(dps,size(VAR));
        end
        if ~isempty(VAR)
            VAR(inact)=NaN;
            VAR=VAR(nm);
        else
            VAR=[];
        end
        VAR(~sact)=NaN;
        %VAR=reshape(VAR,sznm);
        if strcmp(field,'head')
            [U,V]=waqua_get_spatial(sds,exper,'veloc',dim,refdate,argin);
            VAR=VAR+(U.^2+V.^2)/(2*9.81);
        end
        varargout={VAR refdate+var.SimTime/1440};
        
    case {'weirs','weirs-cresth','weirs-sill1','weirs-sill2'}
        [n,m]=local_argin(argin);
        nm=nm(n,m);
        sznm=size(nm);
        %-----
        wp=waqua('readsds',sds,exper,'MESH_WEIPOS');
        wp=reshape(wp,[length(wp)/4 4]);
        udam=zeros(sznm);
        vdam=zeros(sznm);
        %
        indomain = ismember(wp(:,1),m) & ismember(wp(:,2),n);
        wp = wp(indomain,:);
        %
        minn = min(n)-1;
        minm = min(m)-1;
        uflg=wp(:,3)>0; uind=sub2ind(sznm,wp(uflg,2)-minn,wp(uflg,1)-minm);
        vflg=wp(:,4)>0; vind=sub2ind(sznm,wp(vflg,2)-minn,wp(vflg,1)-minm);
        udam(uind)=1;
        udam(1,:)=0;
        udam(nm==1 | nm([1 1:end-1],:)==1)=0;
        vdam(vind)=1;
        vdam(:,1)=0;
        vdam(nm==1 | nm(:,[1 1:end-1])==1)=0;
        if nargout==2
            varargout={udam vdam};
        else
            uhgh=zeros(sznm);
            vhgh=zeros(sznm);
            %
            wh=waqua('readsds',sds,exper,'COEFF_FLOW_WEIDIM');
            % crest level u, sill level u, crest level v, sill level v
            % 1            , 2:3         , 4            , 5:6
            wh=reshape(wh,[length(wh)/6 6]);
            wh = wh(indomain,:);
            %
            switch field
                case {'weirs','weirs-cresth'}
                    uhgh(uind)=-wh(uflg,1);
                    vhgh(vind)=-wh(vflg,4);
                case 'weirs-sill1'
                    uhgh(uind)=wh(uflg,2);
                    vhgh(vind)=wh(vflg,5);
                case 'weirs-sill2'
                    uhgh(uind)=wh(uflg,3);
                    vhgh(vind)=wh(vflg,6);
            end
            varargout={udam vdam uhgh vhgh};
        end
        
    case {'weirs2','weirs2-cresth','weirs2-sill1','weirs2-sill2','weirs2-crestl','weirs2-talud1','weirs2-talud2','weirs2-calib1','weirs2-calib2','weirs2-vegh','weirs2-vegdens','weirs2-vegdrag','weirs2-fc','weirs2-q','weirs2-h','weirs2-u','weirs2-de'}
        switch field
            case {'weirs2-fc','weirs2-q','weirs2-h','weirs2-u','weirs2-de'}
                [tstep,n,m]=local_argin(argin);
            otherwise
                [n,m]=local_argin(argin);
        end
        nm=nm(n,m);
        sznm=size(nm);
        %-----
        wp=waqua('readsds',sds,exper,'MESH_WEIRS');
        wp=reshape(wp,[length(wp)/3 3]);
        udam=zeros(sznm);
        vdam=zeros(sznm);
        %
        indomain = ismember(wp(:,1),m) & ismember(wp(:,2),n);
        wp = wp(indomain,:);
        %
        minn = min(n)-1;
        minm = min(m)-1;
        uflg=wp(:,3)>0; uflg(dim.nweiu+1:end)=0; uind=sub2ind(sznm,wp(uflg,2)-minn,wp(uflg,1)-minm);
        vflg=wp(:,3)>0; vflg(1:dim.nweiu)=0;     vind=sub2ind(sznm,wp(vflg,2)-minn,wp(vflg,1)-minm);
        udam(uind)=1;
        udam(1,:)=0;
        udam(nm==1 | nm([1 1:end-1],:)==1)=0;
        vdam(vind)=1;
        vdam(:,1)=0;
        vdam(nm==1 | nm(:,[1 1:end-1])==1)=0;
        if nargout==2 || strcmp(field,'weirs2')
            varargout={udam vdam};
        else
            uhgh=zeros(sznm);
            vhgh=zeros(sznm);
            %
            flds = {'cresth','sill1','sill2','crestl','talud1','talud2','calib1','calib2','vegh','vegdens','vegdrag'};
            i = 1;
            sgn = 1;
            switch field(8:end)
                case flds
                    wh=waqua('readsds',sds,exper,'COEFF_FLOW_WEIRDIM');
                    % crest level, sill level(1,2), crest length, talud ramp (len/height; 1,2), calib (1,2), veg heigh, veg dens, veg drag
                    % 1          , 2:3            , 4           , 5:6                         , 7:8        , 9        , 10      , 11
                    %
                    wh=reshape(wh,[length(wh)/11 11]);
                    i = find(strcmp(field(8:end),flds));
                    switch i
                        case 1
                            sgn = -1;
                        otherwise
                            sgn = 1;
                    end
                    time=[];
                case 'fc'
                    wh=waqua('readsds',sds,exper,'SOLUTION_FLOW_KCONDW',tstep);
                    time=refdate+wh.SimTime/1440;
                    wh=reshape(wh.Data,[length(indomain) 1]);
                    % -4 : subcritical flow in negative direction,
                    % -3 : critical flow in negative direction,
                    % 0 : no flow across weir,
                    % 3 : critical flow in positive direction,
                    % 4 : subcritical flow in positive direction.
                case 'q'
                    wh=waqua('readsds',sds,exper,'SOLUTION_FLOW_WEIRQ',tstep);
                    time=refdate+wh.SimTime/1440;
                    wh=reshape(wh.Data,[length(indomain) 1]);
                case 'h'
                    wh=waqua('readsds',sds,exper,'SOLUTION_FLOW_WEIRH',tstep);
                    time=refdate+wh.SimTime/1440;
                    wh=reshape(wh.Data,[length(indomain) 1]);
                case 'u'
                    wh=waqua('readsds',sds,exper,'SOLUTION_FLOW_WEIRVL',tstep);
                    time=refdate+wh.SimTime/1440;
                    wh=reshape(wh.Data,[length(indomain) 1]);
                case 'de'
                    wh=waqua('readsds',sds,exper,'SOLUTION_FLOW_WEIRDE',tstep);
                    time=refdate+wh.SimTime/1440;
                    wh=reshape(wh.Data,[length(indomain) 1]);
            end
            wh = wh(indomain,:);
            uhgh(uind)=sgn*wh(uflg,i);
            vhgh(vind)=sgn*wh(vflg,i);
            if isempty(time)
                varargout={udam vdam uhgh vhgh};
            else
                varargout={udam vdam uhgh vhgh time};
            end
        end
        
    case {'dissip','energy','vdiffu','pressure'}
        [tstep,n,m,k]=local_argin(argin);
        nm=nm(n,m);
        sznm=size(nm);
        psznm=prod(sznm);
        sact=sact(n,m);
        %-----
        switch field
            case 'dissip'
                entry='SOLUTION_TURB_DISSIP';
                kmin=0;
            case 'energy'
                entry='SOLUTION_TURB_ENERGY';
                kmin=0;
            case 'vdiffu'
                entry='SOLUTION_DIFCWM';
                kmin=0;
            case 'pressure'
                entry='SOLUTION_HYDRODYNAMIC_PRESSURE';
                kmin=1;
        end
        if ischar(k), k=1:kmax+1-kmin; end
        num_k = length(k);
        %-----
        var=waqua('readsds',sds,exper,entry,tstep);
        nmk=repmat(nm,[1 1 num_k]);
        for i=1:num_k
            ik = k(i);
            nmk(:,:,i)=nmk(:,:,i)+(ik-1)*npnt;
        end
        inact=inact+(0:kmax-kmin)*npnt;
        VAR=var.Data;
        if ~isempty(VAR)
            VAR(inact)=NaN;
            VAR=VAR(nmk);
        else
            VAR=[];
        end
        VAR=reshape(VAR,[psznm num_k]);
        VAR(~sact,:)=NaN;
        VAR=reshape(VAR,[sznm num_k]);
        varargout={VAR refdate+var.SimTime/1440};
        
    case 'chezy'
        [tstep,n,m]=local_argin(argin);
        nm=nm(n,m);
        %-----
        rcgenb=waqua('readsds',sds,exper,'COEFF_GENERAL_RCGENB');
        %  1: AG = gravity accel
        %  2: DAIR = air density (for wind force)
        %  3: DWAT = water density (for wind force)
        %  4: WCONV = wind conversion
        %  5: WSTR = wind speed-independent drag coefficient
        %  6: WSCDV1 = 1st wind speed
        %  7: WSCDV2 = 2nd wind speed
        %  8: CDV1 = lower bound linear drag
        %  9: CDV2 = upper bound linear drag
        % 10: DYNVIS = dynamic viscosity water
        ag=rcgenb(1);
        
        rconta=waqua('readsds',sds,exper,'CONTROL_FLOW_RCONTA');
        %  1: DTMIN = (full) integration step in minutes
        %  3: TSTART = simulation start time (min)
        %  4: TSTOP = simulation stop time (min)
        %  6: EPS = conv. crit. continuity
        %  7: -- (harm analysis)
        %  8: TICVAL = time interval Chezy comput.
        % 11: VAR = marginal depth (dry/flood) = TRSH
        % 12: EPS1 = accuracy Lagrangian time-integral
        % 13: DCO = dry/flood parameter
        % 23: TLFSMO = tide smooth time
        % 24-26: TFBAR,TIBAR,TLBAR = first, inc, last time barrier steering
        % 34: RHOM = density sea water surrounding model (kg/liter)
        % 35: ALPH0 = press. gradient coefficient salinity
        % 36: TEMPW = water temp. state equation
        % 37: SALW = salinity state equation
        % 42: VICO = hor. viscosity
        % 43: THETAC = weighing factor Chezy at weirs
        % 44: RFELAG = red.factor weir-type groynes
        % 45: RFELNG = red.factor weir-type not groynes
        % 46: HKRDUM = dummy overflow height
        % 51: DEFVIV = default vert. visc.
        % 52: Z0 = roughness height veloc. log prof.
        % 53: TETA = implicit coefficient Euler momentum
        % 54: CMUKL = fact. parab. eddy visc.
        % 55: RKAPPA = Von Karman
        % 56: ESMOOT = constant log-layer smooth walls
        % 57-61: CMU, SIGMAK, SIGMAE, CEPS1, CEPS2 = constants k-eps model
        % 62-63: ETA0, GAMMA = constants RNG k-eps model
        % 64: CEPS3 = constant extended k-eps model
        % 65: VTURB = 0 (stnd), 1 (RNG), 2 (ext) k-eps model
        dtmin=rconta(1);
        
        data=waqua('readsds',sds,exper,'SOLUTION_FLOW_CZ',tstep);
        czx=data.Data(1:npnt);
        czy=data.Data(npnt+(1:npnt));
        czx(inact)=NaN; czy(inact)=NaN;
        czx=sqrt((ag*dtmin*60/2)./czx);
        czy=sqrt((ag*dtmin*60/2)./czy);
        CZX=czx(nm);
        CZY=czy(nm);
        CZX(CZX>1e7)=NaN;
        CZY(CZY>1e7)=NaN;
        varargout={CZX CZY refdate+data.SimTime/1440};
    otherwise
        if length(field)>25 && strcmp(field(1:25),'incremental_output_timidx') %e.g. incremental_output_timidx-3
            [tstep,n,m]=local_argin(argin);
            %-----
            Times = waqua('read',sds,exper,'CONTROL_FLOW_INCREMENTAL_TIMES');
            Index = waqua('read',sds,exper,'INCREMENTAL_OUTPUT_TIMIDX');
            Index = reshape(Index,[3 length(Index)/3]);
            Index = Index(:,1:tstep);
            maxbuf = Index(1,tstep);
            %-----
            separator = strfind(field,'-');
            Qplot = str2double(field(separator+1:end)); % incremental_output_timidx-3 --> 3
            VAL = zeros(nmax,mmax);
            for buf = 1:maxbuf
                %
                % Don't read all buffers at once because that quickly leads
                % to OUT OF MEMORY problems. Therefore, read one buffer at a
                % time.
                %
                Vals = waqua('read',sds,exper,'INCREMENTAL_OUTPUT_CLSVAL',buf);
                Vals = Vals.Data;
                Vals = reshape(Vals,[2 size(Vals,2)/2]);
                M = floor(Vals(1,:)/256); % m index
                N = floor(Vals(2,:)/256); % n index
                Q = Vals(1,:)-256*M; % quantity
                C = Vals(2,:)-256*N; % class
                %
                range = 1:max(Index(3,Index(1,:)==buf));
                %
                % select only those entries that refer to the requested
                % quantity.
                %
                range = range(Q(range)==Qplot);
                %
                mt = M(range);
                nt = N(range);
                ct = C(range);
                VAL(nt+nmax*(mt-1)) = ct;
            end
            VAL = VAL(n,m);
            VAL(VAL==0) = NaN;
            varargout={VAL refdate+(Times(1)+(tstep-1)*Times(2))/1440};
        elseif length(field)>17 && strcmp(field(1:17),'solution_derived_') %e.g. solution_derived_maxvalues_sep-3
            [n,m]=local_argin(argin);
            nm=nm(n,m);
            sact=sact(n,m);
            %-----
            separator = strfind(field,'-');
            i = str2double(field(separator+1:end)); % solution_derived_maxvalues_sep-3 --> 3
            field = upper(field(1:separator-1)); % solution_derived_maxvalues_sep-3 --> SOLUTION_DERIVED_MAXVALUES_SEP
            VAL=waqua('readsds',sds,exper,field,[],(i-1)*npnt+(1:npnt));
            if ~isempty(VAL)
                VAL(inact)=NaN;
                VAL=VAL(nm);
            else
                VAL=[];
            end
            VAL(~sact)=NaN;
            varargout={VAL};
        elseif length(field)>6 && strcmp(field(1:6),'subst:')
            Subs=lower(waquaio(sds,exper,'substances'));
            sbs=field(7:end);
            s=ustrcmpi(sbs,Subs);
            if s>0
                [tstep,n,m,k]=local_argin(argin);
                nm=nm(n,m);
                sact=sact(n,m);
                num_n = length(n);
                num_m = length(m);
                if ischar(k), k=1:kmax; end
                num_k = length(k);
                %-----
                subs=waqua('readsds',sds,exper,'SOLUTION_TRANS',tstep);
                nmk=repmat(nm,[1 1 num_k]);
                for i=1:num_k
                    ik = k(i);
                    nmk(:,:,i)=nmk(:,:,i)+(ik-1)*npnt;
                end
                inact=inact+(0:kmax-1)*npnt;
                SUBS=subs.Data(npnt*(s-1)*kmax+(1:kmax*npnt));
                if ~isempty(SUBS)
                    SUBS(inact)=NaN;
                    SUBS=SUBS(nmk);
                    SUBS=reshape(SUBS,[num_n*num_m num_k]);
                    SUBS(~sact,:)=NaN;
                    SUBS=reshape(SUBS,num_n,num_m,num_k);
                else
                    SUBS=[];
                end
                varargout={SUBS refdate+subs.SimTime/1440};
            else
                error('Invalid substance name: %s',sbs)
            end
        else
            error('Unknown field: %s',field)
        end
end
%==========================================================================
% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
%==========================================================================


function varargout=local_argin(argin)
if nargout>length(argin)
    argin(1,length(argin)+1:nargout)={':'};
end
varargout=argin(1:nargout);


function [nm,sact]=getspace(sds,exper,dim)
%
nm=waqua('readsds',sds,exper,'MESH_LGRID');
nm=reshape(nm,dim.sz);
%
sact=false(dim.sz);
irogeo=waqua('readsds',sds,exper,'MESH_IROGEO');
irogeo=reshape(irogeo,[3 length(irogeo)/3]);
for i=1:dim.num_irogeo_rows
    sact(irogeo(1,i),irogeo(2,i):irogeo(3,i))=1;
end


function S = subscriptreshape(U,nm)
% U(nm) has the same shape as nm except if U and nm are both vectors, in
% that case U(nm) is a row vector if U is a row vector and a column vector
% if U is a column vector.
S = reshape(U(nm),size(nm));


function [UU,VV]=uv2xy(sds,exper,dim,n,m,nmfull,k,U,V,refdate)
% ALGORITHM COPIED FROM
% simona\src\postproc\waqview\routines\velocity_zeta.F90
%--------------
%
% Extend index range to get all relevant geometry information ...
%
nm   = nmfull(n,m);
%
npnt = dim.npnt;
num_k = length(k);
sznm = size(nm);

% get coordinates of corner points ...
[xh,yh] = waqua_get_spatial(sds,exper,'dgrid',dim,refdate,{n,m});
x1 = 0.5*(xh([1 1:end-1],[1 1:end-1])+xh(:,[1 1:end-1]));
y1 = 0.5*(yh([1 1:end-1],[1 1:end-1])+yh(:,[1 1:end-1]));
x2 = 0.5*(xh([1 1:end-1],:)+xh(:,:));
y2 = 0.5*(yh([1 1:end-1],:)+yh(:,:));
dx = x2-x1;
dy = y2-y1;
if dim.spheric
    dx = dx.*cos((y1+y2)/2); % NOTE: lat/lon in radians in SIMONA
end
alf = atan2(dy,dx);
cosalf = cos(alf);
sinalf = sin(alf);
%
% Now generate the data for the range selected by the user ...
%
ndm = nmfull(max(1,n-1),m);
nmd = nmfull(n,max(1,m-1));
%
UU = zeros([sznm num_k]);
VV = UU;
for i=1:num_k
    ik = k(i);
    nms=(ik-1)*npnt;
    %
    uu = subscriptreshape(U,nm+nms);
    ud = subscriptreshape(U,nmd+nms);
    vv = subscriptreshape(V,nm+nms);
    vd = subscriptreshape(V,ndm+nms);
    %
    ugem = 0.5*(ud+uu);
    vgem = 0.5*(vd+vv);
    %
    UU(:,:,i) = cosalf.*ugem - sinalf.*vgem;
    VV(:,:,i) = sinalf.*ugem + cosalf.*vgem;
end
