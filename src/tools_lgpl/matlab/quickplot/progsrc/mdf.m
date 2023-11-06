function varargout = mdf(cmd,varargin)
%MDF Manipulate Delft3D-FLOW mdf files.
%   MDF_STRUCT = MDF('read',FILENAME) read mdf file and various attribute
%   files into structure.
%
%   MDF_OUT = MDF('rotate',MDF_IN,ANGLE) rotates the model administration
%   by ANGLE degrees; ANGLE can be 0, 90 (default), 180 or 270 degrees.
%
%   MDF('write',MDF_STRUCT,CASENAME,PATH) stores the data of the MDF_STRUCT
%   in an mdf file named CASENAME.mdf and all files are stored using the
%   same CASENAME combined with their default extension.
%
%   See also WLGRID, WLDEP, D3D_ATTRIB.

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

switch lower(cmd)
    case 'read'
        varargout{1} = masterread(varargin{:});
    case 'rotate'
        varargout{1} = mdfrotate(varargin{:});
    case 'clip'
        [varargout{1:max(1,nargout)}] = mdfclip(varargin{:});
    case 'write'
        masterwrite(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end


function varargout = mdfclip(MDF1,varargin)
if ~strcmp(MDF1.FileType,'Delft3D D-Flow2D3D')
    error('The mdf clip function is only available for Delft3D-FLOW model input.')
end
% MDFCLIP(MDF,MASK)
% MDFCLIP(MDF,MMIN,MMAX,NMIN,NMAX
% MDFCLIP(MDF,MLIM,NLIM)
%
mnkmax = propgetval(MDF1.mdf,'','MNKmax');
switch nargin
    case 2
        % MDFCLIP(MDF,MASK)
        mask = varargin{1};
        if ~isnumeric(mask) && ~islogical(mask)
            error('MASK array should be logical or numeric')
        elseif ~isequal(size(mask),mnkmax(1:2))
            error('MASK argument should be size %i x %i.',mnkmax(1:2))
        end
    case 3
        % MDFCLIP(MDF,MLIM,NLIM)
        mask = zeros(mnkmax(1:2));
        mlim = varargin{1};
        if ~isnumeric(mlim) || ~isequal(size(mlim),[1 2])
            error('Invalid MLIM argument.')
        end
        nlim = varargin{2};
        if ~isnumeric(nlim) || ~isequal(size(nlim),[1 2])
            error('Invalid NLIM argument.')
        end
        mask(mlim(1):mlim(2),nlim(1):nlim(2))=1;
    case 5
        % MDFCLIP(MDF,MMIN,MMAX,NMIN,NMAX
        mask = zeros(mnkmax(1:2));
        mmin = varargin{1};
        if ~isnumeric(mmin) || ~isequal(size(mmin),[1 1])
            error('Invalid MMIN argument.')
        end
        mmax = varargin{2};
        if ~isnumeric(mmax) || ~isequal(size(mmax),[1 1])
            error('Invalid MMAX argument.')
        end
        nmin = varargin{3};
        if ~isnumeric(nmin) || ~isequal(size(nmin),[1 1])
            error('Invalid NMIN argument.')
        end
        nmax = varargin{4};
        if ~isnumeric(nmax) || ~isequal(size(nmax),[1 1])
            error('Invalid NMAX argument.')
        end
        mask(mmin:mmax,nmin:nmax)=1;
    otherwise
        error('Invalid input arguments for mdf(''clip'',...)')
end
%
% find mask numbers; replace NaNs by 0; exclude 0 from domain number list
%
mask(isnan(mask)) = 0;
mask([1 end],:) = 0;
mask(:,[1 end]) = 0;
domains = unique(mask(:));
domains(domains==0)=[];
%
if nargout<length(domains)
    warning('More domains indicated in MASK array than output arguments.')
    domains = domains(1:nargout);
end
varargout = cell(1,length(domains));
for i = 1:length(domains)
    mask1 = mask==domains(i);
    MDF2 = MDF1;
    %
    MAct = find(any(mask1,2));
    mmin = MAct(1)-1;
    mmax = MAct(end)+1;
    NAct = find(any(mask1,1));
    nmin = NAct(1)-1;
    nmax = NAct(end)+1;
    %
    cmask = ~mask1(mmin:mmax,nmin:nmax);
    gmask = cmask([2:end end],[2:end end])&cmask(:,[2:end end])&cmask([2:end end],:)&cmask;
    %
    MDF2.mdf = inifile('seti',MDF2.mdf,'','MNKmax',[mmax-mmin+1 nmax-nmin+1 mnkmax(3)]);
    MDF2.grd.X = MDF2.grd.X(mmin:mmax-1,nmin:nmax-1);
    MDF2.grd.X(gmask(1:end-1,1:end-1))=MDF2.grd.MissingValue;
    MDF2.grd.Y = MDF2.grd.Y(mmin:mmax-1,nmin:nmax-1);
    MDF2.grd.Y(gmask(1:end-1,1:end-1))=MDF2.grd.MissingValue;
    MDF2.grd.Enclosure = enclosure('extract',MDF2.grd.X,MDF2.grd.Y);
    %
    if isfield(MDF2,'dep')
        MDF2.dep = MDF2.dep(mmin:mmax,nmin:nmax);
        %
        dpsopt = propget(MDF2.mdf,'','Dpsopt','DEFAULT');
        if strcmpi(dpsopt,'DP')
            MDF2.dep(cmask)=-999;
        else
            MDF2.dep(gmask)=-999;
        end
    end
    %
    varargout{i} = MDF2;
end


function [M2,N2] = rotate(M1,N1,MMAX)
if nargin==2
    MMAX = N1;
    N1 = M1(:,2);
    M1 = M1(:,1);
end
M2 = N1;
N2 = MMAX-M1+1;
if nargout<=1
    M2 = [M2 N2];
end


function varargout = fldrotateX(loc,varargin)
for i=1:nargin-1
    szA = size(varargin{i});
    varargin{i} = reshape(varargin{i},szA(2:end));
end
[varargout{1:nargout}] = fldrotate(loc,varargin{:});
for i=1:nargout
    szA = size(varargout{i});
    varargout{i} = reshape(varargout{i},[1 szA]);
end


function [U1,V1] = fldrotate(loc,U,V)
szU = size(U);
switch loc
    case 'center'
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            for k = 1:prod(szU(3:end))
                U1(:,:,k) = rot90(U(:,:,k),-1);
            end
        else
            U1 = rot90(U,-1);
        end
    case 'corner'
        szM = szU(1);
        szN = szU(2);
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            U1(:,end,:) = -999;
            U1(end,:,:) = -999;
            for k = 1:prod(szU(3:end))
                U1(1:end-1,1:end-1,k) = rot90(U(1:end-1,1:end-1,k),-1);
            end
        else
            U1 = [rot90(U(1:end-1,1:end-1),-1) repmat(-999,szN-1,1); repmat(-999,1,szM)];
        end
    case 'edges'
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            V1 = repmat(V(1),szU([2 1 3:end]));
            for k = 1:prod(szU(3:end))
                U1(:,:,k) = rot90(V([end 1:end-1],:,k),-1);
                V1(:,:,k) = rot90(U(:,:,k),-1);
            end
        else
            U1 = rot90(V([end 1:end-1],:),-1);
            V1 = rot90(U,-1);
        end
    case 'veloc'
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            V1 = repmat(V(1),szU([2 1 3:end]));
            for k = 1:prod(szU(3:end))
                U1(:,:,k) = rot90(V([end 1:end-1],:,k),-1);
                V1(:,:,k) = rot90(-U(:,:,k),-1);
            end
        else
            U1 = rot90(V([end 1:end-1],:),-1);
            V1 = rot90(-U,-1);
        end
end

function MDF2 = mdfrotate(MDF1,angle)
if ~strcmp(MDF1.FileType,'Delft3D D-Flow2D3D')
    error('The mdf rotate function is only available for Delft3D-FLOW model input.')
end
if nargin>1
    switch angle
        case 0
            MDF2 = MDF1;
            return
        case 90
            % going to do here ...
        case 180
            % rotate first 90 degrees
            MDF1 = mdfrotate(MDF1);
            % other 90 degrees going to do here ...
        case 270
            % rotate first 90 degrees
            MDF1 = mdfrotate(MDF1);
            % rotate second 90 degrees
            MDF1 = mdfrotate(MDF1);
            % final 90 degrees going to do here ...
        otherwise
            error('Can only rotate 0, 90, 180 and 270 degrees.')
    end
end
MDF2 = MDF1;
%
mnkmax = propgetval(MDF2.mdf,'','MNKmax');
MMAX = mnkmax(1);
MDF2.mdf = inifile('seti',MDF2.mdf,'','MNKmax',mnkmax([2 1 3]));
%
ccofu = propget(MDF2.mdf,'','Ccofu');
ccofv = propget(MDF2.mdf,'','Ccofv');
MDF2.mdf = inifile('seti',MDF2.mdf,'','Ccofu',ccofv);
MDF2.mdf = inifile('seti',MDF2.mdf,'','Ccofv',ccofu);
%
MDF2.grd.X = fliplr(MDF2.grd.X');
MDF2.grd.Y = fliplr(MDF2.grd.Y');
MDF2.grd.Enclosure = rotate(MDF2.grd.Enclosure,MMAX);
%
if isfield(MDF2,'dep')
    dpsopt = propget(MDF2.mdf,'','Dpsopt');
    if strcmpi(dpsopt,'DP')
        % data in cell centres: dummy row along all sides
        MDF2.dep = fldrotate('center',MDF2.dep);
    else
        % data at grid points: dummy row only at high M and N
        MDF2.dep = fldrotate('corner',MDF2.dep);
    end
end
%
if isfield(MDF2,'rgh')
    % data at velocity points
    [MDF2.rgh(1).Data,MDF2.rgh(2).Data] = fldrotate('edges',MDF2.rgh(1).Data,MDF2.rgh(2).Data);
end
%
if isfield(MDF2,'ini')
    switch MDF2.ini.FileType
        case {'trirst','ini'}
            % plain binary restart file or ascii initial conditions file
            %
            % water level: data at cell centres
            MDF2.ini.Data(1).Data = fldrotate('center',MDF2.ini.Data(1).Data);
            % velocities: data at velocity points
            for k = 1:mnkmax(3)
                iu = 1+k;
                iv = iu + mnkmax(3);
                [MDF2.ini.Data(iu).Data,MDF2.ini.Data(iv).Data] = fldrotate('veloc',MDF2.ini.Data(iu).Data,MDF2.ini.Data(iv).Data);
            end
            %
            if strcmp(MDF2.ini.FileType,'ini')
                % constituents: data at cell centres
                last3D = length(MDF2.ini.Data);
                has_umean = false;
            else
                % constituents and turbulent quantities: data at cell centres
                last3D = length(MDF2.ini.Data)-2;
                has_umean = true;
            end
            % constituents and turbulent quantities: data at cell centres
            for f = 2+2*mnkmax(3):last3D
                MDF2.ini.Data(f).Data = fldrotate('center',MDF2.ini.Data(f).Data);
            end
            % u/v mnldf: data at velocity points
            if has_umean
                iu = length(MDF2.ini.Data)-1;
                iv = length(MDF2.ini.Data);
                [MDF2.ini.Data(iu).Data,MDF2.ini.Data(iv).Data] = fldrotate('veloc',MDF2.ini.Data(iu).Data,MDF2.ini.Data(iv).Data);
            end
        case 'NEFIS'
            % NEFIS map-file
            oldsz = [MDF2.ini.Data.map_const.NMAX MDF2.ini.Data.map_const.MMAX];
            newsz = oldsz([2 1]);
            for i = 1:length(MDF2.ini.ElmDef)
                if length(MDF2.ini.ElmDef(i).Size)>2 && isequal(MDF2.ini.ElmDef(i).Size(2:3),oldsz)
                    MDF2.ini.ElmDef(i).Size(2:3) = newsz;
                end
            end
            %
            MDF2.ini.Data.map_const.NMAX = newsz(1);
            MDF2.ini.Data.map_const.MMAX = newsz(2);
            %
            for gc = fieldnames(MDF2.ini.Data)'
                g = gc{1};
                for ec = fieldnames(MDF2.ini.Data.(g))'
                    e = ec{1};
                    sz = size(MDF2.ini.Data.(g).(e));
                    if length(sz)>2 && isequal(sz(2:3),oldsz)
                        switch e
                            case {'XCOR','YCOR','DP0','CODB'} % DP0 only if not dpsopt=DP
                                % data at cell corners
                                MDF2.ini.Data.(g).(e) = fldrotateX('corner',MDF2.ini.Data.(g).(e));
                            case {'KFU','KCU'}
                                % data at cell edges
                                e2 = strrep(e,'U','V');
                                [MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2)] = fldrotateX('edges',MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2));
                            case {'U1','TAUKSI','UMNLDF','SBUU','SSUU','SBUUA','SSUUA'}
                                % data at cell edges - velocity components
                                if strcmp(e,'TAUKSI')
                                    e2 = 'TAUETA';
                                else
                                    e2 = strrep(e,'U','V');
                                end
                                [MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2)] = fldrotateX('veloc',MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2));
                            case {'KFV','KCV','V1','TAUETA','VMNLDF','SBVV','SSVV','SBVVA','SSVVA'}
                                % skip - treated with U component
                            otherwise
                                % data at cell centres
                                MDF2.ini.Data.(g).(e) = fldrotateX('center',MDF2.ini.Data.(g).(e));
                        end
                    end
                end
            end
        otherwise
            warning('Rotating %i data not yet supported',MDF2.ini.FileType)
    end
end
%
if isfield(MDF2,'dry')
    MDF2.dry.MN(:,1:2) = rotate(MDF2.dry.MN(:,1:2),MMAX);
    MDF2.dry.MN(:,3:4) = rotate(MDF2.dry.MN(:,3:4),MMAX);
end
%
for fldc = {'thd','bar','gat','cdw','w2d','lwl','ppl','rgs'}
    fld = fldc{1};
    if isfield(MDF2,fld)
        FLD = MDF2.(fld);
        %
        if isfield(FLD,'MNu')
            MNu = 'MNu';
            MNv = 'MNv';
        else
            MNu = 'MNKu';
            MNv = 'MNKv';
        end
        FLD.(MNu)(:,1:2) = rotate(FLD.(MNu)(:,1:2),MMAX);
        FLD.(MNu)(:,3:4) = rotate(FLD.(MNu)(:,3:4),MMAX);
        FLD.(MNv)(:,1:2) = rotate(FLD.(MNv)(:,1:2),MMAX);
        FLD.(MNv)(:,3:4) = rotate(FLD.(MNv)(:,3:4),MMAX);
        TMP_MN = FLD.(MNu);
        TMP_CHAR = FLD.CHARu;
        FLD.(MNu) = FLD.(MNv);
        FLD.CHARu = FLD.CHARv;
        FLD.(MNv) = TMP_MN;
        FLD.CHARv = TMP_CHAR;
        %
        MDF2.(fld) = FLD;
    end
end
%
if isfield(MDF2,'fls')
    MDF2.fls = fldrotate('center',MDF2.fls);
end
%
if isfield(MDF2,'bnd')
    MDF2.bnd.MN(:,1:2) = rotate(MDF2.bnd.MN(:,1:2),MMAX);
    MDF2.bnd.MN(:,3:4) = rotate(MDF2.bnd.MN(:,3:4),MMAX);
    %
    Active = enclosure('inside',MDF2.grd.Enclosure,size(MDF2.grd.X));
    %
    jH = 0;
    for i = 1:length(MDF2.bnd.Name)
        if MDF2.bnd.Forcing(i)=='H'
            jH = jH+1;
        end
        if any('CQTRN'==MDF2.bnd.BndType(i))
            if MDF2.bnd.BndType(i)=='R'
                warning('Riemann data may need adjustment: sign of velocity component changes')
            end
            % if along N axis, then change sign of flux
            if MDF2.bnd.MN(i,1) ~= MDF2.bnd.MN(i,3)
                nAligned = true;
            elseif MDF2.bnd.MN(i,2) ~= MDF2.bnd.MN(i,4)
                nAligned = false;
            else
                mn = MDF2.bnd.MN(i,1:2);
                if mn(2)>1 && Active(mn(1),mn(2)-1)
                    nAligned = true;
                elseif mn(2)<size(Active,2) && Active(mn(1),mn(2)+1)
                    nAligned = true;
                else
                    nAligned = false;
                end
            end
            if nAligned
                switch MDF2.bnd.Forcing(i)
                    case 'T'
                        for j = 1:length(MDF2.bct.Table)
                            if strcmp(MDF2.bct.Table(j).Location,MDF2.bnd.Name{i})
                                MDF2.bct.Table(j).Data(:,2:end) = -MDF2.bct.Table(j).Data(:,2:end);
                                break
                            end
                        end
                    case 'H'
                        nH = size(MDF2.bch.Amplitudes,1)/2;
                        MDF2.bch.Amplitudes(jH,:) = -MDF2.bch.Amplitudes(jH,:);
                        if MDF2.bnd.BndType(i) ~= 'T'
                            MDF2.bch.Amplitudes(nH+jH,:) = -MDF2.bch.Amplitudes(nH+jH,:);
                        end
                end
            end
        end
    end
end
%
if isfield(MDF2,'morini') && isfield(MDF2.morini,'field')
    for f = 1:length(MDF2.morini.field)
        % all fields in cell centres
        MDF2.morini.field(f).data = fldrotate('center',MDF2.morini.field(f).data);
    end
end
%
if isfield(MDF2,'sta')
    MDF2.sta.MN = rotate(MDF2.sta.MN,MMAX);
end
%
if isfield(MDF2,'crs')
    MDF2.crs.MNMN(:,1:2) = rotate(MDF2.crs.MNMN(:,1:2),MMAX);
    MDF2.crs.MNMN(:,3:4) = rotate(MDF2.crs.MNMN(:,3:4),MMAX);
end


function masterwrite(FI,caseid,path)
if nargin<3
    path = '';
end
switch FI.FileType
    case 'Delft3D D-Flow2D3D'
        mdfwrite(FI,caseid,path)
    case 'Delft3D D-Flow FM'
        mduwrite(FI,caseid,path)
end


function mduwrite(MDU,caseid,path)
iniFormat = 'pretty';
%
if isfield(MDU,'ExtForce') && ~isempty(MDU.ExtForce)
    filename = [caseid '_old_format.ext'];
    MDU.ExtForce = mdu_extold_write(MDU.ExtForce, fullfile(path, filename), iniFormat);
else
    filename = '';
end
MDU.mdu = inifile('seti', MDU.mdu, 'external forcing', 'ExtForceFile', filename);
%
if isfield(MDU,'ExtForceNew') && ~isempty(MDU.ExtForceNew)
    filename = [caseid '_new_format.ext'];
    MDU.ExtForceNew = mdu_extnew_write(MDU.ExtForceNew, fullfile(path, filename), iniFormat);
else
    filename = '';
end
MDU.mdu = inifile('seti', MDU.mdu, 'external forcing', 'ExtForceFileNew', filename);
%
if isfield(MDU,'Obs')
    filename = [caseid '_obs.ini'];
    MDU.Obs = mdu_obs_write(MDU.Obs, fullfile(path, filename), iniFormat);
else
    filename = '';
end
MDU.mdu = inifile('seti', MDU.mdu, 'output', 'ObsFile', filename);
%
if isfield(MDU,'Crs')
    filename = [caseid '_crs.ini'];
    MDU.Crs = mdu_crs_write(MDU.Crs, fullfile(path, filename), iniFormat);
else
    filename = '';
end
MDU.mdu = inifile('seti', MDU.mdu, 'output', 'CrsFile', filename);
%
if isfield(MDU,'Crossdef')
    filename = [caseid '_crossdef.ini'];
    MDU.Crossdef = mdu_crossdef_write(MDU.Crossdef, fullfile(path, filename), iniFormat);
else
    filename = '';
end
MDU.mdu = inifile('seti', MDU.mdu, 'geometry', 'CrossDefFile', filename);
%
if isfield(MDU,'Crossloc')
    filename = [caseid '_crossloc.ini'];
    MDU.Crossloc = mdu_crossloc_write(MDU.Crossloc, fullfile(path, filename), iniFormat);
else
    filename = '';
end
MDU.mdu = inifile('seti', MDU.mdu, 'geometry', 'CrossLocFile', filename);
%
if isfield(MDU,'Fric1d')
    filename = mdu_frc1d_write(MDU.Fric1d, path, caseid, iniFormat);
else
    filename = '';
end
MDU.mdu = inifile('seti', MDU.mdu, 'geometry', 'FrictFile', filename);
%
filename = [caseid '.mdu'];
inifile('write', fullfile(path,filename), MDU.mdu, iniFormat);


function Ext = mdu_extold_write(Ext, filename, iniFormat)
fprintf('Writing old ExtForce is not yet implemented.\n')


function Ext = mdu_extnew_write(Ext, filename, iniFormat)
fprintf('Writing NewExtForce is not yet implemented.\n')


function XDef = mdu_crossdef_write(XDef, filename, iniFormat)
XDef.FileName = filename;
xFile = inifile('new');
xFile = inifile('add', xFile, 'General');
xFile = inifile('set', xFile, 'General', 'fileVersion', '3.00');
xFile = inifile('set', xFile, 'General', 'fileType', 'crossDef');
for i = 1:length(XDef.Name)
    [xFile,igrp] = inifile('add', xFile, 'Definition');
    xFile = inifile('set', xFile, igrp, 'id', XDef.Name{i});
    xFile = inifile('set', xFile, igrp, 'type', XDef.Type{i});
    xFile = inifile('set', xFile, igrp, 'thalweg', sprintf('%.13g', XDef.Thalweg(i)));
    xDef = XDef.Definition{i};
    switch XDef.Type{i}
        case 'circle'
            xFile = inifile('set', xFile, igrp, 'diameter'  , xDef.Diameter);
        case 'rectangle'
            xFile = inifile('set', xFile, igrp, 'width' , xDef.Width);
            xFile = inifile('set', xFile, igrp, 'height', xDef.Height);
        case 'yz'
            if xDef.SingleZ
                xFile = inifile('set', xFile, igrp, 'singleValuedZ' , 'true');
            else
                xFile = inifile('set', xFile, igrp, 'singleValuedZ' , 'false');
            end
            xFile = inifile('set', xFile, igrp, 'yzCount'     , sprintf('%i', size(xDef.YZ, 1)));
            xFile = inifile('set', xFile, igrp, 'yCoordinates', sprintf('%.13g ', xDef.YZ(:,1)));
            xFile = inifile('set', xFile, igrp, 'zCoordinates', sprintf('%.13g ', xDef.YZ(:,2)));
            xFile = inifile('set', xFile, igrp, 'conveyance'  , xDef.Conveyance);
            xFile = inifile('set', xFile, igrp, 'sectionCount', sprintf('%i', length(xDef.fricPos)-1));
            xFile = inifile('set', xFile, igrp, 'frictionPositions', sprintf('%.13g ', xDef.fricPos));
            xFile = inifile('set', xFile, igrp, 'frictionIds', sprintf('%s ', xDef.fricIds{:}));
        case 'xyz'
            xFile = inifile('set', xFile, igrp, 'conveyance'  , xDef.Conveyance);
            xFile = inifile('set', xFile, igrp, 'xyzCount'    , sprintf('%i', size(xDef.XYZ, 1)));
            xFile = inifile('set', xFile, igrp, 'xCoordinates', sprintf('%.13g ', xDef.XYZ(:,1)));
            xFile = inifile('set', xFile, igrp, 'yCoordinates', sprintf('%.13g ', xDef.XYZ(:,2)));
            xFile = inifile('set', xFile, igrp, 'zCoordinates', sprintf('%.13g ', xDef.XYZ(:,3)));
            xFile = inifile('set', xFile, igrp, 'sectionCount', sprintf('%i', length(xDef.fricPos)-1));
            xFile = inifile('set', xFile, igrp, 'frictionPositions', sprintf('%.13g ', xDef.fricPos));
            xFile = inifile('set', xFile, igrp, 'frictionIds', sprintf('%s ', xDef.fricIds{:}));
        otherwise
            fprintf('Writing cross section definition "%s" not yet implemented.\n',XDef.Type{i});
    end
end
inifile('write', filename, xFile, iniFormat);


function XLoc = mdu_crossloc_write(XLoc, filename, iniFormat)
XLoc.FileName = filename;
xFile = inifile('new');
xFile = inifile('add', xFile, 'General');
xFile = inifile('set', xFile, 'General', 'fileVersion', '1.01');
xFile = inifile('set', xFile, 'General', 'fileType', 'crossLoc');
for i = 1:length(XLoc.Name)
    [xFile,igrp] = inifile('add', xFile, 'CrossSection');
    xFile = inifile('set', xFile, igrp, 'id', XLoc.Name{i});
    xFile = inifile('set', xFile, igrp, 'branchId', XLoc.BranchId{i});
    xFile = inifile('set', xFile, igrp, 'chainage', sprintf('%.13g',XLoc.Offset(i)));
    xFile = inifile('set', xFile, igrp, 'shift', sprintf('%.13g',XLoc.Shift(i)));
    xFile = inifile('set', xFile, igrp, 'definitionId', XLoc.XDefName{i});
end
inifile('write', filename, xFile, iniFormat);


function Obs = mdu_obs_write(Obs, filename, iniFormat)
obsFile = inifile('new');
obsFile = inifile('add', obsFile, 'General');
obsFile = inifile('set', obsFile, 'General', 'fileVersion', '2.00');
obsFile = inifile('set', obsFile, 'General', 'fileType', 'obsPoints');
for c = 1:length(Obs)
    obsList = Obs{c};
    for i = 1:length(obsList.Name)
        [obsFile,iChap] = inifile('add', obsFile, 'ObservationPoint');
        obsFile = inifile('set', obsFile, iChap, 'name', obsList.Name{i});
        if isempty(obsList.SnapTo{i})
            obsFile = inifile('set', obsFile, iChap, 'branchId', obsList.BranchId{i});
            obsFile = inifile('set', obsFile, iChap, 'chainage', sprintf('%.11g',obsList.Offset(i)));
        else
            obsFile = inifile('set', obsFile, iChap, 'locationType', obsList.SnapTo{i});
            obsFile = inifile('set', obsFile, iChap, 'x', sprintf('%.11g',obsList.XY(i,1)));
            obsFile = inifile('set', obsFile, iChap, 'y', sprintf('%.11g',obsList.XY(i,2)));
        end
    end
end
obsFile = inifile('write', filename, obsFile, iniFormat);
Obs = {obsFile};


function Crs = mdu_crs_write(Crs, filename, iniFormat)
crsFile = inifile('new');
crsFile = inifile('add', crsFile, 'General');
crsFile = inifile('set', crsFile, 'General', 'fileVersion', '2.00');
crsFile = inifile('set', crsFile, 'General', 'fileType', 'obsCross');
for c = 1:length(Crs)
    obsList = Crs{c};
    for i = 1:length(obsList.Name)
        [crsFile,iChap] = inifile('add', crsFile, 'ObservationCrossSection');
        %
        csName = obsList.Name{i};
        if length(csName) > 40
            fprintf('Clipping name of cross section "%s" to "%s".\n', csName, csName(1:40))
            csName = csName(1:40);
        end
        crsFile = inifile('set', crsFile, iChap, 'name', csName);
        if isempty(obsList.BranchId{i})
            crsFile = inifile('set', crsFile, iChap, 'numCoordinates', sprintf('%i', size(obsList.XY{i},1)));
            crsFile = inifile('set', crsFile, iChap, 'xCoordinates', sprintf('%.11g ',obsList.XY{i}(:,1)));
            crsFile = inifile('set', crsFile, iChap, 'yCoordinates', sprintf('%.11g ',obsList.XY{i}(:,2)));
        else
            crsFile = inifile('set', crsFile, iChap, 'branchId', obsList.BranchId{i});
            crsFile = inifile('set', crsFile, iChap, 'chainage', sprintf('%.11g ',obsList.Offset(i)));
        end
    end
end
crsFile = inifile('write', filename, crsFile, iniFormat);
Crs = {crsFile};


function frcNames = mdu_frc1d_write(Frc, path, caseid, iniFormat)
frcNames = Frc(:,1);
for f = 1:size(Frc,1)
    frcName = Frc{f,1};
    frc = Frc{f,2};
    frcFile = inifile('new');
    [frcFile, iChap] = inifile('add', frcFile, 'General');
    frcFile = inifile('set', frcFile, iChap, 'fileVersion', '3.01');
    frcFile = inifile('set', frcFile, iChap, 'fileType', 'roughness');
    [frcFile, iChap] = inifile('add', frcFile, 'Global');
    frcFile = inifile('set', frcFile, iChap, 'frictionId', frcName);
    frcFile = inifile('set', frcFile, iChap, 'frictionType', frc.Type);
    frcFile = inifile('set', frcFile, iChap, 'frictionValue', frc.Global);
    for b = 1:length(frc.Branch)
        bFrc = frc.Branch(b);
        [frcFile, iChap] = inifile('add', frcFile, 'Branch');
        frcFile = inifile('set', frcFile, iChap, 'branchId', bFrc.Id);
        frcFile = inifile('set', frcFile, iChap, 'frictionType', bFrc.Type);
        frcFile = inifile('set', frcFile, iChap, 'functionType', 'constant');
        if isempty(bFrc.Locs)
            frcFile = inifile('set', frcFile, iChap, 'frictionValues', sprintf('%.11g',bFrc.Values));
            frcFile = inifile('set', frcFile, iChap, 'numLocations', '1');
            frcFile = inifile('set', frcFile, iChap, 'chainage', '0.0');
        else
            frcFile = inifile('set', frcFile, iChap, 'numLocations', length(bFrc.Locs));
            frcFile = inifile('set', frcFile, iChap, 'chainage', sprintf('%.11g ',bFrc.Locs));
            frcFile = inifile('set', frcFile, iChap, 'frictionValues', sprintf('%.11g ',bFrc.Values));
        end
    end
    filename = [caseid '_friction_' frcName '.ini'];
    inifile('write', fullfile(path, filename), frcFile, iniFormat);
    frcNames{f} = filename;
end
frcNames = sprintf('%s ',frcNames{:});


function str = hstr(str)
str = ['#' str '#'];


function mdfwrite(MDF,caseid,path)
if isfield(MDF,'grd')
    filename = [caseid '.grd'];
    wlgrid('write',fullfile(path,filename),MDF.grd);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filcco',hstr(filename));
    %
    filename = [caseid '.enc'];
    MDF.mdf = inifile('seti',MDF.mdf,'','Filgrd',hstr(filename));
end
%
if isfield(MDF,'dep')
    filename = [caseid '.dep'];
    wldep('write',fullfile(path,filename),'',MDF.dep);
    MDF.mdf = inifile('seti',MDF.mdf,'','Fildep',hstr(filename));
end
%
if isfield(MDF,'rgh')
    filename = [caseid '.rgh'];
    wldep('write',fullfile(path,filename),MDF.rgh);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filrgh',hstr(filename));
end
%
if isfield(MDF,'ini')
    switch MDF.ini.FileType
        case 'trirst'
            % plain binary restart file (flow only)
            filename = ['tri-rst.' caseid];
            trirst('write',fullfile(path,filename),MDF.ini.Data);
            MDF.mdf = inifile('seti',MDF.mdf,'','Restid',hstr(caseid));
        case 'ini'
            % plain ascii initial conditions file (flow only)
            filename = [caseid '.ini'];
            wldep('write',fullfile(path,filename),MDF.ini.Data);
            MDF.mdf = inifile('seti',MDF.mdf,'','Filic',hstr(filename));
        case 'NEFIS'
            % NEFIS map-file
            filename = ['trim-restart-for-' caseid];
            TRIMnew = vs_ini(fullfile(path,[filename '.dat']),fullfile(path,[filename '.def']));
            vs_copy(MDF.ini,TRIMnew);
            MDF.mdf = inifile('seti',MDF.mdf,'','Restid',hstr(filename));
    end
end
%
if isfield(MDF,'dry')
    filename = [caseid '.dry'];
    d3d_attrib('write',fullfile(path,filename),MDF.dry);
    MDF.mdf = inifile('seti',MDF.mdf,'','Fildry',hstr(filename));
end
%
if isfield(MDF,'thd')
    filename = [caseid '.thd'];
    d3d_attrib('write',fullfile(path,filename),MDF.thd);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filtd',hstr(filename));
end
%
if isfield(MDF,'bnd')
    filename = [caseid '.bnd'];
    d3d_attrib('write',fullfile(path,filename),MDF.bnd);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filbnd',hstr(filename));
end
%
if isfield(MDF,'bct')
    filename = [caseid '.bct'];
    bct_io('write',fullfile(path,filename),MDF.bct);
    MDF.mdf = inifile('seti',MDF.mdf,'','FilbcT',hstr(filename));
end
%
if isfield(MDF,'bch')
    filename = [caseid '.bch'];
    bch_io('write',fullfile(path,filename),MDF.bch);
    MDF.mdf = inifile('seti',MDF.mdf,'','FilbcH',hstr(filename));
end
%
if isfield(MDF,'bcc')
    filename = [caseid '.bcc'];
    bct_io('write',fullfile(path,filename),MDF.bcc);
    MDF.mdf = inifile('seti',MDF.mdf,'','FilbcC',hstr(filename));
end
%
if isfield(MDF,'sed')
    filename = [caseid '.sed'];
    inifile('write',fullfile(path,filename),MDF.sed);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filsed',hstr(filename));
end
%
if isfield(MDF,'morini')
    %
    if isfield(MDF.morini,'field')
        for f = 1:length(MDF.morini.field)
            Fld = MDF.morini.field(f);
            filename = sprintf('%s_layer%i_key%i.frc',caseid,Fld.lyr,Fld.key);
            wldep('write',fullfile(path,filename),'',Fld.data);
            MDF.morini.inb = inifile('seti',MDF.morini.inb,Fld.chp,Fld.key,hstr(filename));
        end
    end
    %
    filename = [caseid '.inb'];
    inifile('write',fullfile(path,filename),MDF.morini.inb);
    MDF.mor = inifile('seti',MDF.mor,'Underlayer','IniComp',hstr(filename));
end
%
if isfield(MDF,'mor')
    filename = [caseid '.mor'];
    inifile('write',fullfile(path,filename),MDF.mor);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filmor',hstr(filename));
end
%
if isfield(MDF,'tra')
    filename = [caseid '.tra'];
    inifile('write',fullfile(path,filename),MDF.tra);
    MDF.mdf = inifile('seti',MDF.mdf,'','TraFrm',hstr(filename));
end
%
if isfield(MDF,'sta')
    filename = [caseid '.obs'];
    d3d_attrib('write',fullfile(path,filename),MDF.sta);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filsta',hstr(filename));
end
%
if isfield(MDF,'crs')
    filename = [caseid '.crs'];
    d3d_attrib('write',fullfile(path,filename),MDF.crs);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filcrs',hstr(filename));
end
%
keys = {'Filbar' 'bar' 'bar'
    'Filgat' 'gat' 'gat'
    'Filcdw' 'cdw' 'cdw'
    'Fil2dw' 'w2d' '2dw'
    'Fillwl' 'lwl' 'lwl'
    'Filppl' 'ppl' 'ppl'
    'Filrgs' 'rgs' 'rgs'};
for i = 1:size(keys,1)
    key = keys{i,1};
    fld = keys{i,2};
    ext = keys{i,3};
    %
    if isfield(MDF,fld)
        filename = [caseid '.' ext];
        d3d_attrib('write',fullfile(path,filename),MDF.(fld));
        MDF.mdf = inifile('seti',MDF.mdf,'',key,hstr(filename));
    end
end
%
if isfield(MDF,'fls')
    filename = [caseid '.fls'];
    wldep('write',fullfile(path,filename),'',MDF.fls);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filfls',hstr(filename));
end
%
filename = [caseid '.mdf'];
inifile('write',fullfile(path,filename),MDF.mdf);


function Val = propget(varargin)
Val = rmhash(inifile('geti',varargin{:})); %hgeti


function MFile = masterread(filename)
%MFile = ddbread(filename);
master = inifile('open',filename,'blank','default');
master_path = fileparts(filename);
%
UNSPECIFIED = 'UNSPECIFIED';
block = 'model';
Program = propget(master,block,'Program',UNSPECIFIED);
if iscell(Program) || isequal(Program,UNSPECIFIED)
    block = 'general';
    Program = propget(master,block,'Program',UNSPECIFIED);
end
%
if isequal(Program,UNSPECIFIED)
    if inifile('existsi',master,'WaveFileInformation')
        MFile.FileType = 'Delft3D D-Wave';
        MFile.mdw = master;
        MFile = mdwread(MFile,master_path);
    elseif inifile('existsi',master,'General','fileType')
        fileType = propget(master,'General','fileType');
        fversion = inifile('getstringi',master,'General','fileVersion','');
        if isempty(fversion)
            mversion = inifile('getstringi',master,'General','majorVersion','');
            if isempty(mversion)
                fversion = '1.0';
            else
                iversion = inifile('getstringi',master,'General','minorVeriosn','');
                fversion = [mversion '.' iversion];
            end
        end
        switch fileType
            case 'modelDef'
                MFile.FileType = 'Delft3D D-Flow1D';
                MFile.md1d = master;
                MFile = md1dread(MFile,master_path);
            case '1D2D'
                MFile.FileType = 'Delft3D Coupled Model';
                MFile.config = master;
                %
                typeModel = inifile('geti',master,'Model','type');
                nameModel = inifile('geti',master,'Model','name');
                dirModel  = inifile('geti',master,'Model','directory');
                mdfModel  = inifile('geti',master,'Model','modelDefinitionFile');
                nModels = length(typeModel);
                %
                MFile.Domains = cell(nModels,3);
                MFile.Domains(:,1) = typeModel;
                MFile.Domains(:,2) = nameModel;
                for i = 1:length(typeModel)
                    fName = relpath(master_path,[dirModel{i} filesep mdfModel{i}]);
                    MFile.Domains{i,3} = masterread(fName);
                end
                %
                mappingFile = inifile('geti',master,'Files','mappingFile');
                Domain.FileType = 'Delft3D 1D2D mapping';
                Domain.mapping = inifile('open',relpath(master_path,mappingFile));
                MFile.Domains(end+1,:) = {'1D2D','1D2Dmapping',Domain};
            otherwise
                MFile.FileType = fileType;
                MFile.FileVersion = fversion;
                MFile.file = master;
        end
    elseif inifile('existsi',master,'','MNKmax')
        MFile.FileType = 'Delft3D D-Flow2D3D';
        MFile.mdf = master;
        MFile = mdfread(MFile,master_path);
    else
        error('Unable to determine type of simulation for %s',filename)
    end
else
    switch Program
        case {'D-Flow FM', 'D-Flow Flexible Mesh', 'Unstruc', 'UNSTRUC'}
            MFile.FileType = 'Delft3D D-Flow FM';
            MFile.mdu = master;
            MFile.general = block;
            MFile = mduread(MFile,master_path);
        otherwise
            error('Unknown program type %s in %s',Program,filename)
    end
end


function DDB = ddbread(filename)
fid = fopen(filename,'r','n','US-ASCII');
DDB.DomainNames = {};
iLine = 0;
iBound = 0;
while ~feof(fid)
    iLine = iLine+1;
    Line = fgetl(fid);
    [Dom1,Rem] = strtok(Line);
    [val,n] = sscanf(Rem,' %d %d %d %d %s %d %d %d %d');
    if n<7
        fclose(fid);
        error('Invalid DD syntax in line %i of %s',iLine,filename)
    end
    Dom2 = char(val(5:end-4)');
    Idx = val([1:4 end-3:end])';
    iBound = iBound+1;
    DDB.DomainNrs(iBound,1:2) = {Dom1 Dom2};
    DDB.DomainMN(iBound,1:8)  = Idx;
end
fclose(fid);
[DDB.DomainNames,dummy,DDB.DomainNrs]=unique(DDB.DomainNrs);
DDB.DomainNrs = reshape(DDB.DomainNrs,[iBound 2]);


function MF = md1dread(MF,md_path)
ntwname = propget(MF.md1d,'Files','networkFile');
if ~isempty(ntwname)
    ntwname = relpath(md_path,ntwname);
    MF.ntw = inifile('open',ntwname);
else
    error('Unable to locate networkFile keyword in [Files] chapter.');
end
%
crsname = propget(MF.md1d,'Files','crossLocFile');
if ~isempty(crsname)
    crsname = relpath(md_path,crsname);
    MF.crsLoc = inifile('open',crsname);
else
    error('Unable to locate crossLocFile keyword in [Files] chapter.');
end
%
bndname = propget(MF.md1d,'Files','boundLocFile');
if ~isempty(bndname)
    bndname = relpath(md_path,bndname);
    MF.bndLoc = inifile('open',bndname);
else
    error('Unable to locate boundLocFile keyword in [Files] chapter.');
end
%
bndname = propget(MF.md1d,'Files','latDischargeLocFile');
if ~isempty(bndname)
    bndname = relpath(md_path,bndname);
    MF.latLoc = inifile('open',bndname);
else
    error('Unable to locate latDischargeLocFile keyword in [Files] chapter.');
end
%
crsname = propget(MF.md1d,'Files','crossDefFile');
if ~isempty(crsname)
    crsname = relpath(md_path,crsname);
    MF.crsDef = inifile('open',crsname);
else
    error('Unable to locate crossDefFile keyword in [Files] chapter.');
end
%
CT=inifile('geti',MF.crsDef,'Definition','type');
if ~iscell(CT)
    CT = {CT};
end
CID=inifile('cgetstringi',MF.crsDef,'Definition','id');
CDF=inifile('cgetstringi',MF.crsLoc,'CrossSection','definition');
[lDF,iDF]=ismember(CDF,CID);
if ~all(lDF)
    missingDF = unique(CDF(~lDF));
    error('Missing cross section definitions: %s',sprintf('%s ',missingDF{:}))
else
    % copy CrossSection type to CrossSection location data structure
    MF.crsLoc=inifile('seti',MF.crsLoc,'CrossSection','type',CT(iDF));
end
%
strname = propget(MF.md1d,'Files','structureFile');
if ~isempty(strname)
    strname = relpath(md_path,strname);
    MF.strucLoc = inifile('open',strname);
else
    error('Unable to locate structureFile keyword in [Files] chapter.');
end
%
obsname = propget(MF.md1d,'Files','obsPointsFile');
if ~isempty(obsname)
    obsname = relpath(md_path,obsname);
    MF.obs = inifile('open',obsname);
else
    error('Unable to locate obsPointsFile keyword in [Files] chapter.');
end
%


function MF = mduread(MF,md_path)
mshname = propget(MF.mdu,'geometry','NetFile');
if ~isempty(mshname)
    mshname = relpath(md_path,mshname);
    [NetFile,Q] = getmesh(mshname);
    MF.mesh.nc_file = NetFile;
    imeshquant = find(([Q.NVal]==0 & ~strcmp({Q.Name},'-------') & [Q.UseGrid]~=0) | [Q.NVal]==4);
    MF.mesh.quant = Q(imeshquant);
    %
    ismesh = cellfun(@iscell,{Q.varid}) & [Q.NVal]==0;
    for i = 1:length(ismesh)
        if ismesh(i)
            ismesh(i) = isequal(Q(i).varid{1},'node_index');
            if ismesh(i)
                if strcmp(Q(i).Geom, 'UGRID1D_NETWORK-NODE')
                    mesh1d_id = Q(i).varid{2};
                    Mesh1D = F.Dataset(mesh1d_id+1);
                    csp = ustrcmpi('coordinate_space', {Mesh1D.Attribute.Name});
                    Network = Mesh1D.Attribute(csp).Value;
                    network_id = ustrcmpi(Network,{F.Dataset.Name})-1;
                    for j = 1:length(ismesh)
                        if ismesh(j)
                            if isequal(Q(j).varid{2}, network_id)
                                ismesh(j) = false;
                                break
                            end
                        end
                    end
                end
            end
        end
    end
    imesh = find(ismesh);
    [~,MF.mesh.meshes] = ismember(imesh,imeshquant);
    %
    FirstPoint = netcdffil(NetFile,1,Q(1),'grid',1);
    MF.mesh.XYUnits = FirstPoint.XUnits;
else
    error('Unable to locate NetFile keyword in [geometry] chapter.');
end
%
attfiles = {...
   'external forcing','ExtForceFile','ExtForce'
   'external forcing','ExtForceFileNew','ExtForceNew'
   'geometry','IniFieldFile','IniField'
   'geometry','BedlevelFile','BedLevel'
   'geometry','DryPointsFile','DryPoints'
   'geometry','WaterLevIniFile','WaterLevIni'
   'geometry','LandBoundaryFile','LandBoundary'
   'geometry','ThinDamFile','ThinDam'
   'geometry','FixedWeirFile','FixedWeir'
   'geometry','ThindykeFile','Thindyke'
   'geometry','StructureFile','Structure'
   'geometry','VertplizFile','Vertpliz'
   'geometry','ProflocFile','Profloc'
   'geometry','ProfdefFile','Profdef'
   'geometry','ProfdefxyzFile','Profdefxyz'
   'geometry','ManholeFile','Manhole'
   'geometry','PartitionFile','Partition'
   'restart','RestartFile','Restart'
   'output','ObsFile','Obs'
   'output','CrsFile','Crs'
   'sediment','MorFile','Mor'
   'sediment','SedFile','Sed'};
for i = 1:size(attfiles,1)
    grp = attfiles{i,1};
    fld = attfiles{i,2};
    key = attfiles{i,3};
    %
    if strcmp(key,'BedLevel')
        Missing = -999;
        bltyp = propgetval(MF.mdu,grp,'BedlevType',Missing);
        if bltyp == Missing
            bltyp = propgetval(MF.mdu,grp,'BotlevType',Missing);
            if bltyp == Missing
                bltyp = 3;
            end
        end
        MF.BedLevelType = bltyp;
        %
        zkuni = -5;
        zkuni = propgetval(MF.mdu,grp,'BotLevUni',zkuni);
        zkuni = propgetval(MF.mdu,grp,'BedLevUni',zkuni);
        VNames = {NetFile.Dataset.Name}';
        %
        filename = '';
        if isfield(MF,'IniField')
            IniNames = {MF.IniField.Quantity.Name};
            ic = find(strcmpi('bedlevel',IniNames));
            if ~isempty(ic)
                filename = MF.IniField.Quantity.File;
            end
        end
        if isempty(filename)
            filename = propget(MF.mdu,grp,fld,''); % BedlevelFile
        end
        if isempty(filename)
            filename = propget(MF.mdu,grp,'BathymetryFile','');
        end
        % ... data from BathymetryFile/BedlevelFile
        if isempty(filename) % in FM: if file not exists ...
            if bltyp == 1
                % bed levels specified at faces
                % ... variable with standard name "altitude" at cell centres from mesh file
                % ... variable with name "mesh2d_flowelem_bl" from mesh file
                SNames = get_standard_names(NetFile);
                ibl2d = strcmp('altitude',SNames);
                if none(ibl2d)
                    ibl2d = strcmp('mesh2d_flowelem_bl',VNames);
                end
                loc = 'UGRID2D-FACE';
            elseif bltyp == 2
                % bed levels specified at edges
                % can't be read from mesh file
                ibl2d = [];
            else
                % bed levels specified at nodes
                % ... variable with name "node_z" or "NetNode_z" from mesh file
                ibl2d = strcmp('NetNode_z',VNames);
                if none(ibl2d)
                    ibl2d = strcmp('node_z',VNames);
                end
                loc = 'UGRID2D-NODE';
            end
            %
            ibl2d = find(ibl2d)-1;
            if ~isempty(ibl2d)
                % use bed level from mesh file
                iq = false(size(Q));
                for j = 1:length(iq)
                    if isnumeric(Q(j).varid) & ~isempty(Q(j).varid) & isscalar(Q(j).varid) & strcmp(Q(j).Geom,loc)
                        iq(j) = ismember(Q(j).varid,ibl2d);
                    end
                end
                MF.BedLevel = Q(iq);
                MF.BedLevelUni = zkuni;
            else
                % use uniform bed level
                MF.BedLevel = zkuni;
            end
            continue
        end
    else
        filename = propget(MF.mdu,grp,fld,'');
    end
    if ~isempty(filename)
        filenames = strsplit(filename,';');
        if length(filenames) == 1
            filename1 = relpath(md_path,filenames{1});
            if ~exist(filename1, 'file')
                filenames = strsplit(filename,' ');
            end
        end
        for ifile = 1:length(filenames)
            filename1 = relpath(md_path,filenames{ifile});
            if ~exist(filename1,'file')
                error('Unable to locate file(s) referred to by:%s',filename);
            end
        end
        Files = [];
        for ifile = length(filenames):-1:1
            filename = relpath(md_path,filenames{ifile});
            switch key
                case {'BedLevel','WaterLevIni'}
                    F = samples('read',filename);
                case 'Crs'
                    try
                        F = tekal('open',filename,'loaddata');
                        F = tekal2crs(F);
                    catch
                        F = inifile('open',filename);
                        F = ini2crs(F);
                    end
                case 'ExtForce'
                    ext_path = fileparts(filename);
                    %
                    F1 = inifile('open',filename); % open external forcings file as ini-file
                    K1 = inifile('keywords',F1,1); % keywords of first (and only) section
                    nK = length(K1);
                    Ks = false(1,nK);
                    for k = 1:nK
                        if isempty(K1{k}) % no =-sign in line
                            continue
                        elseif K1{k}(1)=='*' % comment line
                            continue
                        else
                            Ks(k) = true;
                        end
                    end
                    K1 = K1(Ks);
                    nQ = sum(strcmpi(K1,'quantity'));
                    F = [];
                    %
                    Ks = find(Ks);
                    q = 0;
                    F(nQ).Quantity = '';
                    for k = 1:length(K1)
                        val = inifile('geti',F1,1,Ks(k));
                        switch lower(K1{k})
                            case 'quantity'
                                q = q+1;
                                F(q).Quantity = val;
                            case 'filename'
                                F(q).FileName = relpath(ext_path,val);
                            case 'filetype'
                                if isscalar(val) && val>=1 && val<=12
                                    FileTypeList = {'uniform','unimagdir','svwp','arcinfo','spiderweb','curvi','triangulation','triangulation_magdir','polyline','inside_polygon','ncgrid','ncflow'};
                                    val = FileTypeList{val};
                                end
                                F(q).FileType = val;
                            case 'method'
                                if isscalar(val) && val>=0 && val<=9
                                    MethodList = {'provider','intp_space_and_time','intp_space_then_intp_time','save_weights','spatial_inside_polygon','spatial_triangulation','spatial_averaging','spatial_index_triangulation','spatial_smoothing','spatial_internal_diffusion'};
                                    val = MethodList{val+1};
                                end
                                F(q).Method = val;
                            case 'operand'
                                switch lower(val)
                                    case 'o'
                                        val = 'override';
                                    case '+'
                                        val = 'add';
                                    case '*'
                                        val = 'multiply';
                                    case 'a'
                                        val = 'apply_when_undefined';
                                end
                                F(q).Operand = val;
                            case 'value'
                                F(q).Value = val;
                            case 'factor'
                                F(q).Value = val;
                            otherwise
                                % unknown keyword - skip it or warn?
                                continue
                        end
                        F(q).File = [];
                    end
                    %
                    for q = 1:length(F)
                        switch F(q).FileType
                            case {'triangulation','triangulation_magdir'}
                                F(q).File = samples('read', F(q).FileName);
                            case 'curvi'
                                F(q).File = asciiwind('open', F(q).FileName);
                            otherwise
                                ui_message('warning','Unsupported external forcing file type %s in record %i of %s', F(q).FileType, q, filename)
                        end
                    end
                case 'ExtForceNew'
                    F = [];
                    F.File = inifile('open',filename);
                    ext_path = fileparts(filename);
                    if inifile('existsi',F.File,'boundary')>0
                        BndQuant = inifile('cgetstringi',F.File,'boundary','quantity');
                        %
                        [BndLines,pliBnd]  = inifile('cgetstringi',F.File,'boundary','locationfile');
                        [BndNodes,nodBnd] = inifile('cgetstringi',F.File,'boundary','nodeId');
                        BndLocs = cell(size(BndQuant));
                        BndType = BndLocs;
                        BndLocs(pliBnd) = BndLines;
                        BndType(pliBnd) = {'line'};
                        BndLocs(nodBnd) = BndNodes;
                        BndType(nodBnd) = {'node'};
                        %
                        BndForce = inifile('cgetstringi',F.File,'boundary','forcingFile');
                        uBndQuant = unique(BndQuant);
                        F.Bnd.Types = uBndQuant;
                        BndInd = cellfun(@(f)find(strcmp(f,BndQuant)),F.Bnd.Types,'uniformoutput',false);
                        %
                        [BndLocs,~,ic] = unique(BndLocs);
                        for iBL = length(BndLocs):-1:1
                            if strcmp(BndType{ic(iBL)},'line')
                                bndfilename = relpath(ext_path,BndLocs{iBL});
                                F.BndLoc.Files{iBL} = tekal('open',bndfilename,'loaddata');
                                [p,BndLocs{iBL}] = fileparts(BndLocs{iBL});
                            end
                        end
                        F.BndLoc.Names = BndLocs;
                        BndLocs = BndLocs(ic);
                        F.Bnd.Locs = cellfun(@(f)BndLocs(f),BndInd,'uniformoutput',false);
                        %
                        [uBndForce,~,ic] = unique(BndForce);
                        for iBF = length(uBndForce):-1:1
                            bndfilename = uBndForce{iBF};
                            if strcmp(bndfilename,'REALTIME')
                                % REALTIME in memory data exchange
                                F.BndForce.Files{iBF} = 'REALTIME';
                            else
                                bndfilename = relpath(ext_path,bndfilename);
                                F.BndForce.Files{iBF} = inifile('open',bndfilename);
                            end
                        end
                        BndForce = F.BndForce.Files(ic);
                        F.Bnd.Forcing = cellfun(@(f)BndForce(f),BndInd,'uniformoutput',false);
                        %
                        for iBT = 1:length(F.Bnd.Types)
                            BTp  = F.Bnd.Types{iBT};
                            Locs = F.Bnd.Locs{iBT};
                            for iBL = 1:length(Locs)
                                Loc = Locs{iBL};
                                ForceFile = F.Bnd.Forcing{iBT}{iBL};
                                %fprintf('Searching for %s at %s in %s\n',BTp,Loc,ForceFile.FileName);
                                if strcmp(ForceFile,'REALTIME')
                                    continue
                                end
                                [nForcings,iQ]=inifile('existsi',ForceFile,'forcing');
                                forcesThisLocAndType = false(size(ForceFile.Data,1));
                                for iFQ = 1:nForcings
                                    Name = inifile('getstringi',ForceFile,iQ(iFQ),'Name');
                                    if ~strncmp(Name,Loc,length(Loc))
                                        continue
                                    end
                                    Qnts = inifile('cgetstringi',ForceFile,iQ(iFQ),'Quantity');
                                    if ~strncmp(Qnts{end},BTp,length(BTp))
                                        continue
                                    end
                                   forcesThisLocAndType(iQ(iFQ)) = true;
                                end
                                ForceFile.Data = ForceFile.Data(forcesThisLocAndType,:);
                                F.Bnd.Forcing{iBT}{iBL} = ForceFile;
                            end
                        end
                    else
                        F.Bnd.Types = {};
                    end
                case 'IniField'
                    F = inifile('open',filename);
                    [ContainsIni, IndexChapter] = inifile('exists', F, 'Initial');
                    if ContainsIni
                        for iq = length(IndexChapter):-1:1
                            F.Quantity(iq).Name     = inifile('hgeti', F, IndexChapter(iq), 'quantity');
                            F.Quantity(iq).FileType = inifile('hgeti', F, IndexChapter(iq), 'dataFileType');
                            F.Quantity(iq).File     = inifile('hgeti', F, IndexChapter(iq), 'dataFile');
                            F.Quantity(iq).Interp   = inifile('hgeti', F, IndexChapter(iq), 'interpolationMethod');
                        end
                    end
                case 'Mor'
                    F = inifile('open',filename);
                case 'Obs'
                    try
                        fid = fopen(filename,'r','n','UTF-8');
                        XYName = textscan(fid,'%f %f %s');
                        fclose(fid);
                        for n = 1:length(XYName{3})
                            name = XYName{3}{n};
                            if length(name)<=1
                                continue
                            elseif name(1)=='''' && name(end)==''''
                                XYName{3}{n} = name(2:end-1);
                            end
                        end
                        F = xyn2obs(XYName{:});
                    catch
                        F = inifile('open',filename);
                        F = ini2obs(F);
                    end
                case 'Profdef'
                    F = profdefparser(filename);
                case 'Profdefxyz'
                    F = tekal('read',filename,'loaddata');
                case 'Profloc'
                    F = samples('read',filename);
                case 'Sed'
                    F = inifile('open',filename);
                case 'Structure'
                    F = inifile('open',filename);
                case 'FixedWeir'
                    F = landboundary('read',filename);
                otherwise
                    F = filename;
            end
            switch key
                case {'Obs','Crs'}
                    Files{ifile} = F;
                otherwise
                    Files = F;
            end
        end
        MF.(key) = Files;
    end
end


function F2 = xyn2obs(X,Y,Name)
F2.Name = Name;
sz = size(F2.Name);
F2.SnapTo = repmat({'all'}, sz);
F2.XY = cat(2,X,Y);
F2.BranchId = cell(sz);
F2.Offset = zeros(sz);


function F2 = ini2obs(F)
[~,igrp]=inifile('existsi',F,'ObservationPoint');
F2.Name = inifile('getstringi',F,igrp,'Name','');
F2.SnapTo = inifile('getstringi',F,igrp,'locationType','');
x = inifile('geti',F,igrp,'x',NaN);
y = inifile('geti',F,igrp,'y',NaN);
F2.XY = [x y];
F2.BranchId = inifile('getstringi',F,igrp,'branchId','');
F2.Offset = inifile('geti',F,igrp,'chainage',NaN);


function F2 = tekal2crs(F)
F2.Name = {F.Field.Name}';
sz = size(F2.Name);
F2.BranchId = cell(sz);
F2.Offset = zeros(sz);
F2.XY = {F.Field.Data}';


function F2 = ini2crs(F)
[~,igrp]=inifile('existsi',F,'ObservationCrossSection');
F2.Name = inifile('getstringi',F,igrp,'Name','');
F2.BranchId = inifile('getstringi',F,igrp,'branchId','');
F2.Offset = inifile('geti',F,igrp,'chainage',NaN);
x = inifile('cgeti',F,igrp,'xCoordinates',[]);
y = inifile('cgeti',F,igrp,'yCoordinates',[]);
F2.XY = cellfun(@(a,b)[a' b'],x,y,'UniformOutput',false);


function F = profdefparser(filename)
keys = {'PROFNR=','TYPE=','WIDTH=','HEIGHT=','ZMIN=','BASE=','TALUD=','FRCTP=','FRCCF='};
lkeys = cellfun(@length,keys);
N = zeros(100,9);
n = 0;
fid = fopen(filename,'n','US-ASCII');
while ~feof(fid)
    Line = fgetl(fid);
    if isempty(Line)
        continue
    elseif Line(1) == '*'
        continue
    end
    n = n+1;
    if n > size(N,1)
        N(2*size(N,1),:) = 0;
    end
    %
    for i = 1:9
        key = keys{i};
        ik = strfind(Line,key);
        if ~isempty(ik)
            N(n,i) = sscanf(Line(ik(1)+lkeys(i):end),'%f',1);
        elseif i == 4 % default height
            N(n,i) = 3000;
        elseif i == 5 || i == 8 || i == 9 % default zmin, frctp, frccf
            N(n,i) = -999;
        end
    end
end
fclose(fid);
N = N(1:n,:);
F.ProfNr = N(:,1);
F.ProfTp = N(:,2);
F.Width = N(:,3);
F.Height = N(:,4);
F.ZMin = N(:,5);
F.Base = N(:,6);
Taluds = (F.ProfTp == 6 | F.ProfTp == 7) & F.Base == 0 & N(:,7) > 0;
if any(Taluds)
    F.Base(Taluds) = max(0, F.Width(Taluds) - 2 * F.Height(Taluds) * N(Taluds,7));
end
F.FrcTp = N(:,8);
F.FrcCf = N(:,9);


function SNames = get_standard_names(F)
N = length(F.Dataset);
SNames = cell(N,1);
for i = 1:N
    if ~isempty(F.Dataset(i).Attribute)
        Att = {F.Dataset(i).Attribute.Name};
        SN = strcmp(Att,'standard_name');
        if any(SN)
            SNames{i} = F.Dataset(i).Attribute(SN).Value;
        end
    end
end


function [F,Q] = getmesh(mshname)
F = nc_interpret(mshname);
F.FileType = 'NetCDF';
Q = qpread(F);
if ~strcmp(Q(1).Geom,'UGRID1D-NODE') && ~strcmp(Q(1).Geom,'UGRID2D-NODE')
    % old mesh file: modify data structures such that it behaves like a
    % new ugrid file.
    %
    grdid = length(F.Dataset)+1;
    %
    F.Dataset(grdid).Name = 'Mesh2D';
    F.Dataset(grdid).Attribute(1).Name = 'edge_node_connectivity';
    F.Dataset(grdid).Attribute(1).Value = 'NetLink';
    F.Dataset(grdid).Mesh = {'ugrid' 2 grdid -1 'nNetNode' 'nNetLink' ''};
    F.Dataset(grdid).X = ustrcmpi({F.Dataset.Name},'NetNode_x');
    F.Dataset(grdid).Y = ustrcmpi({F.Dataset.Name},'NetNode_y');
    NL = ustrcmpi({F.Dataset.Name},'NetLink');
    F.Dataset(NL).Attribute(end+1).Name = 'start_index';
    F.Dataset(NL).Attribute(end).Value = 1;
    %
    Q = [];
    Q.Name = 'Mesh2D';
    Q.Units = '';
    Q.TemperatureType = 'unspecified';
    Q.Geom = 'UGRID1D-NODE';
    Q.Coords = 'xy';
    Q.DimFlag = [0 0 6 0 0];
    Q.DataInCell = 0;
    Q.NVal = 0;
    Q.SubFld = [];
    Q.MNK = 0;
    Q.varid = {'node_index' grdid-1};
    Q.DimName = {[]  []  'nNetNode'  []  []};
    Q.hasCoords = 1;
    Q.VectorDef = 0;
    Q.ClosedPoly = 0;
    Q.UseGrid = 1;
end


function MF = mdwread(MF,md_path)
grdname = inifile('geti',MF.mdw,'Domain','Grid');
if ~isempty(grdname)
    if iscell(grdname)
        numDomains = length(grdname);
        for idom = 1:numDomains
            grdname_loc = relpath(md_path,grdname{idom});
            [f,p] = fileparts(grdname_loc);
            MF.domain(idom).name = p;
            MF.domain(idom).grd  = wlgrid('read',grdname_loc);
        end
    else
        % numDomains = 1;
        idom = 1;
        grdname_loc = relpath(md_path,grdname);
        [f,p] = fileparts(grdname_loc);
        MF.domain(idom).name = p;
        MF.domain(idom).grd = wlgrid('read',grdname_loc);
    end
else
    error('Unable to locate Grid keyword in [Domain] chapter.');
end


function MF = mdfread(MF,md_path)
mnkmax = propgetval(MF.mdf,'','MNKmax');
SUB1   = propget(MF.mdf,'','Sub1','');
salin  = ~isempty(strfind(lower(SUB1),'s'));
tempa  = ~isempty(strfind(lower(SUB1),'t'));
secfl  = ~isempty(strfind(lower(SUB1),'i'));
SUB2   = propget(MF.mdf,'','Sub2','');
consti = ~isempty(strfind(lower(SUB2),'c'));
if consti
    consti = 0;
    for i = 1:99
        Namc = propget(MF.mdf,'',sprintf('Namc%i',i),'');
        if isempty(Namc)
            break
        else
            consti = consti+1;
        end
    end
end
lstsci = salin + tempa + secfl + consti;
nturb  = 0;
if mnkmax(3)>1
    TURB = propget(MF.mdf,'','Tkemod','Algebraic');
    switch lower(TURB)
        case 'k-l'
            nturb = 1;
        case 'k-epsilon'
            nturb = 2;
        otherwise % algebraic or constant
            nturb = 0;
    end
end
%
grdname = propget(MF.mdf,'','Filcco','');
if ~isempty(grdname)
    grdname = relpath(md_path,grdname);
    MF.grd = wlgrid('read',grdname);
else
    dxdy = propget(MF.mdf,'','DxDy');
    spherical = propget(MF.mdf,'','Sphere','N');
    GRD.X = dxdy(1) * repmat((1:mnkmax(1)-1)' - 0.5,1,mnkmax(2)-1);
    GRD.Y = dxdy(2) * repmat((1:mnkmax(2)-1) - 0.5,mnkmax(1)-1,1);
    GRD.Enclosure = [1 1; mnkmax(1) 1; mnkmax(1:2); 1 mnkmax(2); 1 1];
    GRD.FileName = '';
    GRD.CoordinateSystem = 'Cartesian';
    GRD.MissingValue = 0;
    GRD.Attributes = {};
    GRD.Type = 'RGF';
    GRD.Orient = 'anticlockwise';
    if strcmpi(spherical,'y')
        error('Can''t combine constant DxDy grid with Sphere = #Y#');
    end
    MF.grd = GRD;
end
if strcmp(MF.grd.CoordinateSystem,'Spherical')
   if inifile('existsi',MF.mdf,'','Grdang')
       MF.mdf = inifile('deletei',MF.mdf,'','Grdang');
   end
   if inifile('existsi',MF.mdf,'','Anglat')
       MF.mdf = inifile('deletei',MF.mdf,'','Anglat');
   end
   if inifile('existsi',MF.mdf,'','Anglon')
       MF.mdf = inifile('deletei',MF.mdf,'','Anglon');
   end
end
encname = get_single_key(MF.mdf, '', 'Filgrd', '');
if ~isempty(encname)
    encname = relpath(md_path,encname);
    MF.grd.Enclosure = enclosure('read',encname);
end
%
depname = get_single_key(MF.mdf, '', 'Fildep', '');
if ~isempty(depname)
    depname = relpath(md_path,depname);
    MF.dep = wldep('read',depname,MF.grd);
end
%
rghname = get_single_key(MF.mdf, '', 'Filrgh', '');
if ~isempty(rghname)
    rghname = relpath(md_path,rghname);
    MF.rgh = wldep('read',rghname,MF.grd,'multiple');
    if length(MF.rgh)~=2
        warning('Unexpected length of roughness file');
    end
end
%
ininame = get_single_key(MF.mdf, '', 'Restid', '');
if ~isempty(ininame)
    idate = propget(MF.mdf,'','Itdate');
    idate = idate([1:4 6:7 9:10]);
    %
    tunit = propget(MF.mdf,'','Tunit');
    switch lower(tunit)
        case 'w'
            tunit = 7;
        case 'd'
            tunit = 1;
        case 'h'
            tunit = 1/24;
        case 'm'
            tunit = 1/1440;
        case 's'
            tunit = 1/86400;
    end
    itime = propgetval(MF.mdf,'','TStart')*tunit;
    rdate = datenum(idate,'yyyymmdd')+itime;
    if itime>=1
        itime = datestr(rdate,'HHMMSS');
        idate = datestr(rdate,'yyyymmdd');
    else
        itime = datestr(itime,'HHMMSS');
    end
    %
    % try tri-rst.restid.YYYYMMDD.HHMMSS
    inicond = relpath(md_path,['tri-rst.' ininame '.' idate '.' itime]);
    try
        MF.ini.Data = trirst('read',inicond,MF.grd,'all');
        MF.ini.FileType = 'trirst';
    catch
        %
        % try tri-rst.restid
        inicond = relpath(md_path,['tri-rst.' ininame]);
        try
            MF.ini.Data = trirst('read',inicond,MF.grd,'all');
            MF.ini.FileType = 'trirst';
        catch
            try
                %
                % try restid as trim-nc
                inicond = relpath(md_path,[ininame '.nc']);
                ncid = netcdf.open(inicond);
                MF.ini.FileName = inicond;
                MF.ini.FileType = 'netCDF';
                netcdf.close(ncid)
            catch
                try
                    %
                    % try restid as trim-dat/def
                    inicond = relpath(md_path,ininame);
                    MF.ini = vs_use(inicond,'quiet');
                    %
                    times = qpread(MF.ini,'water level','times');
                    iMAP  = find(times==rdate);
                    %
                    for ig = 1:length(MF.ini.GrpDat)
                        g = MF.ini.GrpDat(ig).Name;
                        g_ = strrep(g,'-','_');
                        if MF.ini.GrpDat(ig).SizeDim>1
                            MF.ini.Data.(g_) = vs_let(MF.ini,g,{iMAP},'*','quiet');
                            MF.ini.GrpDat(ig).SizeDim=1;
                        else
                            MF.ini.Data.(g_) = vs_let(MF.ini,g,{1},'*','quiet');
                        end
                    end
                    %
                    MF.ini.FileName = 'IN MEMORY';
                    MF.ini.FileType = 'NEFIS';
                    MF.ini.DatExt = '';
                    MF.ini.DefExt = '';
                catch
                    ui_message('warning','Unable to locate restart file: %s',ininame)
                end
            end
        end
    end
    %
    if isfield(MF,'ini') && strcmp(MF.ini.FileType,'trirst')
        % plain binary restart file (flow only)
        nfields = length(MF.ini.Data);
        % water level, velocity, constituents, turbulent quantities, u/v mnldf
        nf_req  = 1 + 2*mnkmax(3) + lstsci*mnkmax(3) + nturb*(mnkmax(3)+1) + 2;
        if nfields ~= nf_req
            error('Number of fields in restart file (%i) does not match expect number of fields (%i)',nfields,nf_req)
        end
    end
else
    ininame = get_single_key(MF.mdf, '', 'Filic', '');
    if ~isempty(ininame)
        ininame = relpath(md_path,ininame);
        MF.ini.FileType = 'ini';
        MF.ini.Data = wldep('read',ininame,MF.grd,'multiple');
        %
        % plain ASCII initial conditions file (flow only)
        nfields = length(MF.ini.Data);
        % water level, velocity, constituents
        nf_req  = 1 + 2*mnkmax(3) + lstsci*mnkmax(3);
        if nfields ~= nf_req
            error('Number of fields in initial conditions file (%i) does not match expect number of fields (%i)',nfields,nf_req)
        end
    end
end
%
dryname = get_single_key(MF.mdf, '', 'Fildry', '');
if ~isempty(dryname)
    dryname = relpath(md_path,dryname);
    MF.dry = d3d_attrib('read',dryname);
end
%
thdname = get_single_key(MF.mdf,'','Filtd','');
if ~isempty(thdname)
    thdname = relpath(md_path,thdname);
    thd = d3d_attrib('read',thdname);
    if ~isempty(thd.MNu) || ~isempty(thd.MNv)
        MF.thd = thd;
    else
        warning('Filtd file specified, but it doesn''t specify and thin dams.')
    end
end
%
wndname = get_single_key(MF.mdf,'','Filwnd','');
if ~isempty(wndname)
    wndname = relpath(md_path,wndname);
    MF.wnd = readwnd(wndname);
end
%
wndname = get_single_key(MF.mdf,'','Filwp','');
if ~isempty(wndname)
    wndname = relpath(md_path,wndname);
    MF.grid_pres = asciiwind('open',wndname);
end
%
wndname = get_single_key(MF.mdf,'','Filwu','');
if ~isempty(wndname)
    wndname = relpath(md_path,wndname);
    MF.grid_windu = asciiwind('open',wndname);
end
%
wndname = get_single_key(MF.mdf,'','Filwv','');
if ~isempty(wndname)
    wndname = relpath(md_path,wndname);
    MF.grid_windv = asciiwind('open',wndname);
end
%
bndname = get_single_key(MF.mdf,'','Filbnd','');
if ~isempty(bndname)
    bndname = relpath(md_path,bndname);
    MF.bnd = d3d_attrib('read',bndname);
    %
    forcing_types = unique(upper(MF.bnd.Forcing));
    %
    bctname = get_single_key(MF.mdf,'','FilbcT','');
    if ismember('T',forcing_types)
        if ~isempty(bctname)
            bctname = relpath(md_path,bctname);
            MF.bct = bct_io('read',bctname);
        end
    elseif ~isempty(bctname)
        warning('FilbcT name specified, but no boundary with time series forcing specified. Skipping file.')
    end
    %
    bcaname = get_single_key(MF.mdf,'','Filana','');
    if ismember('A',forcing_types)
        if ~isempty(bcaname)
            bcaname = relpath(md_path,bcaname);
            MF.ana = readbca(bcaname);
        end
    elseif ~isempty(bcaname)
        warning('Filana name specified, but no boundary with astronomic forcing specified. Skipping file.')
    end
    %
    bchname = get_single_key(MF.mdf,'','FilbcH','');
    if ismember('H',forcing_types)
        if ~isempty(bchname)
            bchname = relpath(md_path,bchname);
            MF.bch = bch_io('read',bchname);
        end
    elseif ~isempty(bchname)
        warning('FilbcH name specified, but no boundary with harmonic forcing specified. Skipping file.')
    end
    %
    bcqname = get_single_key(MF.mdf,'','FilbcQ','');
    if ismember('Q',forcing_types)
        if ~isempty(bcqname)
            bcqname = relpath(md_path,bcqname);
            MF.bcq = bct_io('read',bcqname);
        end
    elseif ~isempty(bcqname)
        warning('FilbcQ name specified, but no boundary with QH forcing specified. Skipping file.')
    end
    %
    if salin || tempa || consti > 0
        bccname = get_single_key(MF.mdf,'','FilbcC','');
        if ~isempty(bccname)
            bccname = relpath(md_path,bccname);
            MF.bcc = bct_io('read',bccname);
        end
    elseif ~isempty(bcqname)
        %warning('FilbcC name specified, but no salinity, temperature or constituent specified. Skipping file.')
        MF.mdf = inifile('deletei',MF.mdf,'','FilbcC');
    end
end
%
sedname = get_single_key(MF.mdf,'','Filsed','');
if ~isempty(sedname)
    sedname = relpath(md_path,sedname);
    MF.sed = inifile('open',sedname);
end
%
morname = get_single_key(MF.mdf,'','Filmor','');
if ~isempty(morname)
    morname = relpath(md_path,morname);
    MF.mor = inifile('open',morname);
end
%
if isfield(MF,'mor')
    morininame = get_single_key(MF.mor,'Underlayer','IniComp','');
    if ~isempty(morininame)
        morininame = relpath(md_path,morininame);
        MF.morini.inb = inifile('open',morininame);
        Chaps = inifile('chapters',MF.morini.inb);
        f = 0;
        l = 0;
        for c = 1:length(Chaps)
            if strcmpi(Chaps{c},'layer')
                l = l+1;
                Keys = inifile('keywordsi',MF.morini.inb,c);
                for k = 1:length(Keys)
                    if ~strcmpi(Keys{k},'Type')
                        val = propget(MF.morini.inb,c,k);
                        if ischar(val)
                            f = f+1;
                            filename = relpath(md_path,val);
                            MF.morini.field(f).chp  = c;
                            MF.morini.field(f).lyr  = l;
                            MF.morini.field(f).key  = k;
                            MF.morini.field(f).data = wldep('read',filename,MF.grd);
                        end
                    end
                end
            end
        end
    end
end
%
traname = get_single_key(MF.mdf,'','TraFrm','');
if ~isempty(traname)
    traname = relpath(md_path,traname);
    MF.tra = readtra(traname);
end
%
staname = get_single_key(MF.mdf,'','Filsta','');
if ~isempty(staname)
    staname = relpath(md_path,staname);
    sta = d3d_attrib('read',staname);
    if ~isempty(sta.MN)
        MF.sta = sta;
    else
        warning('Filsta file specified, but it doesn''t define any observation points.')
    end
end
%
crsname = get_single_key(MF.mdf,'','Filcrs','');
if ~isempty(crsname)
    crsname = relpath(md_path,crsname);
    crs = d3d_attrib('read',crsname);
    if ~isempty(crs.MNMN)
        MF.crs = crs;
    else
        warning('Filcrs file specified, but it doesn''t define any observation cross sections.')
    end
end
%
keys = {'Filbar' 'bar'
    'Filgat' 'gat'
    'Filcdw' 'cdw'
    'Fil2dw' 'w2d'
    'Fillwl' 'lwl'
    'Filppl' 'ppl'
    'Filrgs' 'rgs'};
for i = 1:size(keys,1)
    key = keys{i,1};
    fld = keys{i,2};
    fldname = get_single_key(MF.mdf,'',key,'');
    if ~isempty(fldname)
        fldname = relpath(md_path,fldname);
        MF.(fld) = d3d_attrib('read',fldname);
    end
end
%
flsname = get_single_key(MF.mdf,'','Filfls','');
if ~isempty(flsname)
    flsname = relpath(md_path,flsname);
    MF.fls = wldep('read',flsname,MF.grd);
end


function filename = get_single_key(mdf, grp, key, varargin)
filename = propget(mdf, grp, key, varargin{:});
if iscell(filename)
    %warning('The key ''%s'' appears %i times in %s. Using only the first entry.',key,length(filename),mdf.FileName)
    filename = filename{1};
end

function val = propgetval(varargin)
str = propget(varargin{:});
if iscell(str)
    val = cell(size(str));
    for i = 1:length(str)
        val{i} = getval(str{i});
    end
elseif isnumeric(str)
    if isempty(str) && length(varargin)>3
        val = varargin{4};
    else
        val = str;
    end
else
    val = sscanf(str,'%f',[1 inf]);
end


function str = rmhash(str)
if iscell(str)
    for i = 1:length(str)
        str{i} = rmhash(str{i});
    end
elseif ischar(str)
    hashes = strfind(str,'#');
    if length(hashes)>1
        str1 = deblank(str(1:hashes(1)-1));
        if isempty(str1)
            str = str(hashes(1)+1:hashes(2)-1);
        else
            str = str1;
        end
    elseif length(hashes)==1
        str = str(1:hashes(1)-1);
    end
    str = deblank(str);
end


function pf = relpath(path,file)
if length(file)>1 && file(2)==':'
    pf = file;
else
    pf = fullfile(path,file);
end


function data = readwnd(filename)
fid = fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Can''t open file: %s.',filename)
end
try
    data = fscanf(fid,'%f %f %f\n',[3  inf]);
    fclose(fid);
catch e
    if fid>0
        fclose(fid);
    end
    rethrow(e)
end

function S = readbca(filename)
fid = fopen(filename,'r','n','US-ASCII');
S.FileName = filename;
S.FileType = 'Delft3D-FLOW BCA-file';
S.Location.Name = '';
S.Location.Components = cell(0,2);
Components = {'A0', 'SA', 'SSA', 'MSM', 'MM', 'MSF', 'MS0', 'MF', 'KO0', ...
    'MK0', 'SNU', 'SN', 'MSTM', 'MFM', '2SM', 'MSQM', 'MQM', '2SMN', '2OK1', ...
    '2Q1', 'NJ1', 'SIGMA1', 'MUK1', 'NUJ1', 'Q1', 'NK1', 'RO1', 'NUK1', 'O1', ...
    'TAU1', 'MP1', 'M1B', 'M1C', 'M1A', 'M1', 'NO1', 'CHI1', 'LP1', 'PI1', 'TK1', ...
    'P1', 'SK1', 'S1', 'K1', 'MO1', 'SP1', 'PSI1', 'RP1', 'FI1', 'KP1', 'THETA1', ...
    'LABDAO1', 'J1', 'MQ1', '2PO1', 'SO1', 'OO1', '2KO1', 'UPSILON1', 'KQ1', '2MN2S2', ...
    '3MKS2', '2NS2', '3MS2', 'OQ2', 'MNK2', 'EPSILON2', 'MNS2', '2ML2S2', 'MNUS2', ...
    'MNK2S2', '2MS2K2', 'O2', 'NLK2', '2MK2', '2N2', 'MU2', '2MS2', 'SNK2', 'NA2', ...
    'N2', 'KQ2', 'NB2', 'NU2', '3MSN2', '2KN2S2', 'OP2', 'MSK2', 'GAMMA2', 'ALFA2', ...
    'MPS2', 'MA2', 'M2', 'KO2', 'MSP2', 'MB2', 'DELTA2', 'MKS2', 'M2(KS)2', '2SN(MK)2', ...
    'LABDA2', 'SNM2', '2MN2', 'L2', 'L2A', 'L2B', '2SK2', 'T2', 'S2', 'KP2', 'R2', ...
    'K2', 'MSNU2', 'MSN2', 'ZETA2', 'ETA2', 'KJ2', 'MKN2', '2KM(SN)2', '2SM2', 'SKM2', ...
    '2MS2N2', '2SNU2', '2SN2', 'SKN2', 'MQ3', 'NO3', 'MO3', '2MK3', '2MP3', 'M3', 'NK3', ...
    'SO3', 'MP3', 'MK3', 'SP3', '2MQ3', 'SK3', '2SO3', 'K3', '4MS4', '2MNS4', '3MK4', ...
    'MNLK4', '3MS4', 'MSNK4', 'MN4', 'MNU4', '2MLS4', '2MSK4', 'M4', '2MKS4', 'SN4', ...
    '3MN4', '2SMK4', 'MS4', 'MK4', '2SNM4', '2MSN4', 'SL4', 'S4', 'SK4', '2SMN4', ...
    '3SM4', '2SKM4', 'MNO5', '3MK5', '3MP5', 'M5', 'MNK5', '2MP5', 'MSO5', '3MO5', ...
    'MSK5', '3KM5', '2(MN)S6', '3MNS6', '4MK6', '2NM6', '4MS6', '2MSNK6', '2MN6', ...
    '2MNU6', '3MSK6', 'M6', 'MSN6', 'MNK6', '4MN6', 'MKNU6', '2(MS)K6', '2MS6', ...
    '2MK6', '2SN6', '3MSN6', 'MKL6', '2SM6', 'MSK6', 'S6', '2MNO7', '2NMK7', ...
    'M7', '2MSO7', 'MSKO7', '2(MN)8', '3MN8', '3MNKS8', 'M8', '2MSN8', '2MNK8', ...
    '3MS8', '3MK8', '2SNM8', 'MSNK8', '2(MS)8', '2MSK8', '3SM8', '2SMK8', 'S8', ...
    '2(MN)K9', '3MNK9', '4MK9', '3MSK9', '4MN10', 'M10', '3MSN10', '4MS10', ...
    '2(MS)N10', '2MNSK10', '3M2S10', '4MSK11', 'M12', '4MSN12', '5MS12', ...
    '3MNKS12', '4M2S12'};
try
    i = 0;
    Line = fgetl(fid);
    while ischar(Line)
        Line = deblank(Line);
        if ~isempty(deblank(Line))
            [comp,Rem] = strtok(Line);
            if ~isempty(Rem)
                val = sscanf(Rem,'%f',[1 2]);
            else
                val = [];
            end
            if i == 0 || length(val) ~= 2
                i = i+1;
                S.Location(i).Name = Line;
            else
                S.Location(i).Components(end+1,1:2) = {comp, val};
            end
        end
        Line = fgetl(fid);
    end
    fclose(fid);
catch e
    if fid>0
        fclose(fid);
    end
    rethrow(e)
end


function varargout = bch_io(cmd,varargin)
switch lower(cmd)
    case 'read'
        O = readbch(varargin{:});
        if nargout>0
            varargout{1} = O;
        end
    case 'write'
        writebch(varargin{:})
    otherwise
        error('Unknown command: %s',cmd)
end

function writebch(filename,S)
fid = fopen(filename,'wt','n','US-ASCII');
Format = [repmat(' %15.7e',1,length(S.Freq)) '\n'];
fprintf(fid,Format,S.Freq);
fprintf(fid,'\n');
fprintf(fid,Format,S.Amplitudes');
fprintf(fid,'\n');
Format = [repmat(' ',1,16) repmat(' %15.7e',1,length(S.Freq)-1) '\n'];
fprintf(fid,Format,S.Phases(:,2:end)');
fclose(fid);


function S = readbch(filename)
S.FileName = filename;
S.FileType = 'Delft3D-FLOW BCH-file';
fid = fopen(filename,'r','n','US-ASCII');
%
lNr = 1;
Line = fgetl(fid);
Freq = {};
nFreq = [];
while 1
    [Fr,nF,err] = sscanf(Line,'%f');
    if ~isempty(err)
        fclose(fid);
        error('Only values expected in line %i of BCH file: "%s"',lNr,Line)
    elseif isempty(deblank(Line))
        break
    else
        Freq{lNr}  = Fr;
        nFreq(lNr) = nF;
    end
    %
    lNr = lNr+1;
    Line = fgetl(fid);
end
S.Freq = cat(1,Freq{:});
hasZeroFreq = S.Freq(1) == 0;
if hasZeroFreq
    phaseCorr = 1;
else
    phaseCorr = 0;
end
nFreqTot = sum(nFreq);
%
Data = zeros(0,nFreqTot);
iBnd = 0;
while 1
    nRem = nFreqTot;
    offset = 0;
    iBnd = iBnd+1;
    while nRem > 0
        lNr = lNr+1;
        Line = fgetl(fid);
        if isempty(deblank(Line)) && nRem == nFreqTot
            break
        end
        %
        [DataRow,nVal2,err] = sscanf(Line,'%f');
        if ~isempty(err)
            fclose(fid);
            error('Only values expected in line %i of BCH file: "%s"',lNr,Line)
        elseif nVal2 > nRem
            fclose(fid);
            error('Only %i values expected in line %i "%s"',nRem,lNr,Line)
        end
        Data(iBnd,offset+(1:nVal2)) = DataRow;
        offset = offset + nVal2;
        nRem = nRem - nVal2;
    end
    if isempty(deblank(Line))
        break
    end
end
nBnd = size(Data,1);
S.Amplitudes = Data;
%
Data(:) = NaN;
if nFreqTot > phaseCorr
    for iBnd = 1:nBnd
        nRem = nFreqTot - phaseCorr;
        offset = phaseCorr;
        while nRem > 0
            lNr = lNr+1;
            Line = fgetl(fid);
            %
            [DataRow,nVal2,err] = sscanf(Line,'%f');
            if ~isempty(err)
                fclose(fid);
                error('Only values expected in line %i of BCH file: "%s"',lNr,Line)
            elseif nVal2 > nRem
                fclose(fid);
                error('Only %i values expected in line %i "%s"',nRem,lNr,Line)
            end
            Data(iBnd,offset+(1:nVal2)) = DataRow;
            offset = offset + nVal2;
            nRem = nRem - nVal2;
        end
    end
end
S.Phases = Data;
%
while ~feof(fid)
    lNr = lNr+1;
    Line = fgetl(fid);
    if ischar(Line) && ~isempty(deblank(Line))
        fclose(fid);
        error('Unexpected data "%s" on line %i in file %s.',deblank(Line),lNr,filename)
    end
end
fclose(fid);

function I = readtra(filename)
I =inifile('open',filename);