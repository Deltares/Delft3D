function varargout=d3d_simfil(FI,idom,field,cmd,varargin)
%D3D_SIMFIL QP support for Delft3D simulation configuration files.
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

oFI = [];
if strcmp(FI.FileType,'Delft3D Coupled Model')
    if nargin<3 || (~strcmp(field,'domains') && ~strcmp(field,'options'))
        oFI = FI;
        odom = idom;
        %
        FI = FI.Domains{idom,3};
        idom = [];
    end
end

if nargin==2
    varargout={infile(FI,idom)};
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
        varargout={getsize(FI,idom,Props)};
        return
    case 'times'
        varargout={readtim(FI,idom,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,idom,Props);
        return
    case 'stations'
        varargout={readsts(FI,Props,0)};
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
    idx_subf = [];
    idx(fidx(1:length(varargin))) = varargin;
else
    idx_subf = varargin{1};
    idx(fidx(1:(length(varargin)-1))) = varargin(2:end);
end

sz = getsize(FI,idom,Props);
allidx=zeros(size(sz));
for i=1:length(sz)
    if DimFlag(i)
        if i == T_ && isempty(idx{i})
            idx{T_}=sz(T_);
        elseif i == ST_ && isempty(idx{i})
            % skip --> nothing
        elseif  isempty(idx{i}) || isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        end
    end
end

switch FI.FileType(9:end)
    case '1D2D mapping'
        switch Props.Name
            case {'1D2D links','1D2D link numbers'}
                XY1D = inifile('hcgeti',FI.mapping,'1d2dLink','XY_1D');
                XY2D = inifile('hcgeti',FI.mapping,'1d2dLink','XY_2D');
                for i = 1:length(XY1D)
                    XY1D{i}(2,:) = XY2D{i};
                end
                %
                Ans.XY = XY1D(idx{M_});
                if strcmp(Props.Name,'1D2D link numbers')
                    Ans.Val = idx{M_};
                end
        end
    case 'D-Flow1D'
        Name = Props.Name;
        if strcmp(Name,'water level boundary points') || ...
                strcmp(Name,'discharge boundary points') || ...
                strncmp(Name,'boundary points',15)
            Name = 'boundary points';
        elseif length(Name)>20 && strcmp(Name(end-19:end),'cross section points')
            Name = 'cross section points';
        elseif strncmp(Name,'structure points',16)
            Name = 'structure points';
        end
        switch Name
            case 'network'
                if ~isfield(FI,'ntwXY')
                    G = inifile('hcgeti',FI.ntw,'Branch','geometry');
                    for i = length(G):-1:1
                        XY{i} = geom2xy(G{i});
                    end
                    FI.ntwXY = XY;
                end
                Ans.XY = FI.ntwXY(idx{M_});
            case 'nodes'
                X = inifile('hcgeti',FI.ntw,'Node','x');
                Y = inifile('hcgeti',FI.ntw,'Node','y');
                Ans.X = [X{idx{M_}}];
                Ans.Y = [Y{idx{M_}}];
                Ans.Val = inifile('hgeti',FI.ntw,'Node','id');
                Ans.Val = Ans.Val(idx{M_});
            case 'xyz cross section lines'
                CDx = inifile('hcgeti', FI.crsDef, 'Definition', 'xCoors', []);
                CDy = inifile('hcgeti', FI.crsDef, 'Definition', 'yCoors', []);
                isXYZ = ~cellfun(@isempty, CDx);
                CDx = CDx(isXYZ);
                CDy = CDy(isXYZ);
                %
                CDx = CDx(idx{M_});
                CDy = CDy(idx{M_});
                for i = 1:length(CDx)
                    CDx{i} = [CDx{i}' CDy{i}'];
                end
                %
                Ans.XY = CDx;
            case 'cross section points'
                csId = inifile('hgeti',FI.crsLoc,'CrossSection','id');
                CT = inifile('hgeti',FI.crsLoc,'CrossSection','type');
                CT = find(strcmp(CT,Props.varid));
                iM = CT(idx{M_});
                %
                FI = check_crsXY(FI);
                %
                Ans.X = FI.crsXY(iM,1);
                Ans.Y = FI.crsXY(iM,2);
                Ans.Val = csId(iM);
            case 'lateral discharges'
                lId = inifile('hcgetstringi',FI.latLoc,'LateralDischarge','id');
                FI = check_latXY(FI);
                %
                Ans.X = FI.latXY(idx{M_},1);
                Ans.Y = FI.latXY(idx{M_},2);
                Ans.Val = lId(idx{M_});
            case 'grid'
                FI = check_gpXY(FI);
                Ans.X = FI.gpXY(idx{M_},1);
                Ans.Y = FI.gpXY(idx{M_},2);
                Ans.EdgeNodeConnect = FI.gpConnect;
            case 'grid points'
                FI = check_gpXY(FI);
                Ans.X = FI.gpXY(idx{M_},1);
                Ans.Y = FI.gpXY(idx{M_},2);
                Ans.Val = FI.gpId(idx{M_});
            case 'observation points'
                lId = inifile('hcgetstringi',FI.obs,'ObservationPoint','id');
                FI = check_obsXY(FI);
                %
                Ans.X = FI.obsXY(idx{M_},1);
                Ans.Y = FI.obsXY(idx{M_},2);
                Ans.Val = lId(idx{M_});
            case 'boundary points'
                F=inifile('hcgeti',FI.bndLoc,'Boundary','type');
                BT = [F{:}];
                BT = find(BT==Props.varid);
                %
                F=inifile('hcgeti',FI.bndLoc,'Boundary','nodeId');
                BNI = F(BT(idx{M_}));
                %
                NI = inifile('hcgetstringi',FI.ntw,'Node','id');
                [~,iBNI,iNI] = intersect(BNI,NI);
                ni = zeros(size(BNI));
                ni(iBNI)=iNI;
                %
                x = inifile('hcgeti',FI.ntw,'Node','x');
                y = inifile('hcgeti',FI.ntw,'Node','y');
                Ans.X   = [x{ni}]';
                Ans.Y   = [y{ni}]';
                Ans.Val = NI(ni);
            case 'structure points'
                sId = inifile('hcgeti',FI.strucLoc,'Structure','id');
                ST = inifile('hcgeti',FI.strucLoc,'Structure','type');
                ST = find(strcmp(ST,Props.varid));
                iM = ST(idx{M_});
                %
                FI = check_strucXY(FI);
                %
                Ans.X = FI.strucXY(iM,1);
                Ans.Y = FI.strucXY(iM,2);
                Ans.Val = sId(iM);
            otherwise
                switch Props.varid{1}
                    case {'calcdim','calcpnt','nodes_cr'}
                        FI = check_gpXY(FI);
                        %
                        % first N1 points are internal nodes
                        X = inifile('hcgeti',FI.ntw,'Node','x');
                        Y = inifile('hcgeti',FI.ntw,'Node','y');
                        N = inifile('hgetstringi',FI.ntw,'Node','id');
                        nodXY = [cat(1,X{:}) cat(1,Y{:})];
                        %
                        % next N2 points are the internal nodes of the branches
                        igpXY = FI.gpXY(FI.gpInternal,:);
                        %
                        % final N3 points are the boundary nodes in the
                        % order of the branches
                        bNodes = [inifile('hcgetstringi',FI.ntw,'Branch','FromNode') inifile('hcgetstringi',FI.ntw,'Branch','ToNode')]';
                        bNodes = bNodes(:);
                        B = inifile('hcgetstringi',FI.bndLoc,'Boundary','nodeId');
                        B = bNodes(ismember(bNodes,B));
                        [~,locB]=ismember(B,N);
                        bndXY = nodXY(locB,:);
                        %
                        % Finally remove the boundaries from the internal
                        % nodes list
                        nodXY(locB,:) = [];
                        %
                        Ans.X = cat(1,nodXY(:,1),igpXY(:,1),bndXY(:,1));
                        Ans.X = Ans.X(idx{M_});
                        Ans.Y = cat(1,nodXY(:,2),igpXY(:,2),bndXY(:,2));
                        Ans.Y = Ans.Y(idx{M_});
                        [time,data] = delwaq('read',FI.(Props.varid{1}),Props.varid{2},idx{M_},idx{T_});
                        Ans.Time = time;
                        Ans.Val = permute(data,[3 2 1]);
                    case 'morph_gr'
                        FI = check_gpXY(FI);
                        %
                        % all grid points (including fromNode and toNode)
                        % in order of the branches
                        XY = FI.gpXY;
                        %
                        Ans.X = XY(idx{M_},1);
                        Ans.Y = XY(idx{M_},2);
                        [time,data] = delwaq('read',FI.(Props.varid{1}),Props.varid{2},idx{M_},idx{T_});
                        Ans.Time = time;
                        Ans.Val = permute(data,[3 2 1]);
                    case 'qlat'
                        FI = check_latXY(FI);
                        Ans.X = FI.latXY(idx{M_},1);
                        Ans.Y = FI.latXY(idx{M_},2);
                        %
                        [time,data] = delwaq('read',FI.(Props.varid{1}),Props.varid{2},idx{M_},idx{T_});
                        Ans.Time = time;
                        Ans.Val = permute(data,[3 2 1]);
                    case 'qwb'
                        [time,data] = delwaq('read',FI.(Props.varid{1}),Props.varid{2},idx_subf,idx{T_});
                        Ans.Time = time;
                        Ans.Val = permute(data,[3 2 1]);
                    case {'reachdim','flowanal','reachseg','reach_cr','rsegsub'}
                        FI = check_reachXY(FI);
                        Ans.X = FI.reachXY(idx{M_},1);
                        Ans.Y = FI.reachXY(idx{M_},2);
                        %
                        [time,data] = delwaq('read',FI.(Props.varid{1}),Props.varid{2},idx{M_},idx{T_});
                        Ans.Time = time;
                        Ans.Val = permute(data,[3 2 1]);
                    case {'strucdim','struc','struc_cr'}
                        FI = check_strucXY(FI);
                        Ans.X = FI.strucXY(idx{M_},1);
                        Ans.Y = FI.strucXY(idx{M_},2);
                        %
                        [time,data] = delwaq('read',FI.(Props.varid{1}),Props.varid{2},idx{M_},idx{T_});
                        Ans.Time = time;
                        Ans.Val = permute(data,[3 2 1]);
                    case 'output'
                        nc = FI.output.(Props.varid{2});
                        var = nc.Dataset(Props.varid{3});
                        idxT = idx{T_};
                        switch Props.varid{2}
                            case 'gridpoints'
                                FI = check_gpXY(FI);
                                Ans.X = FI.gpXY(idx{M_},1);
                                Ans.Y = FI.gpXY(idx{M_},2);
                                Ans.EdgeNodeConnect = FI.gpConnect;
                                Ans.ValLocation = 'NODE';
                                idxLoc = idx{M_};
                            case 'reachsegments'
                                FI = check_gpXY(FI);
                                Ans.X = FI.gpXY(:,1);
                                Ans.Y = FI.gpXY(:,2);
                                Ans.EdgeNodeConnect = FI.gpConnect(idx{M_},:);
                                Ans.ValLocation = 'EDGE';
                                idxLoc = idx{M_};
                            case 'observations'
                                % TODO!
                                %lId = inifile('hcgetstringi',FI.obs,'ObservationPoint','id');
                                FI = check_obsXY(FI);
                                %
                                Ans.X = FI.obsXY(idx{ST_},1);
                                Ans.Y = FI.obsXY(idx{ST_},2);
                                %Ans.Val = lId(idx{M_});
                                %
                                idxLoc = idx{ST_};
                            otherwise
                                idxLoc = idx{M_};
                        end
                        Ans.Val = qp_netcdf_get(nc,var.Name,{'time','id'},{idxT idxLoc});
                        Ans.Time = readtim(FI,idom,Props,idx{T_});
                        if length(Ans.Time) == 1
                            Ans.Val = permute(Ans.Val, [2:ndims(Ans.Val) 1]);
                        end
                    otherwise
                        % only for debug purposes ...
                end
                if ~isequal(idx{M_},0) && ~strcmp(Props.varid{1},'output')
                    Ans.LocationName = FI.(Props.varid{1}).SegmentName(idx{M_});
                end
        end
    case 'D-Flow2D3D'
        switch Props.Name
            case 'grid'
                nM = length(idx{M_});
                nN = length(idx{N_});
                Ans.X = NaN(nM,nN);
                Ans.Y = NaN(nM,nN);
                if idx{M_}(end)==sz(M_)
                    idx{M_}(end) = [];
                    nM = nM-1;
                end
                if idx{N_}(end)==sz(N_)
                    idx{N_}(end) = [];
                    nN = nN-1;
                end
                Ans.X(1:nM,1:nN) = FI.grd.X(idx{M_},idx{N_});
                Ans.Y(1:nM,1:nN) = FI.grd.Y(idx{M_},idx{N_});
            case 'bed levels'
                F.X = FI.grd.X;
                F.Y = FI.grd.Y;
                F.X(end+1,:) = NaN;
                F.Y(end+1,:) = NaN;
                F.X(:,end+1) = NaN;
                F.Y(:,end+1) = NaN;
                if isfield(FI,'dep')
                    F.QP_Options.AttribFiles.Data = {FI.dep};
                    F.QP_Options.AttribFiles.FileType = 'wldep';
                    F.QP_Options.AttribFiles.QP_Options.Dpsopt = rmhash(inifile('hgeti',FI.mdf,'*','Dpsopt','#MEAN#'));
                    F.QP_Options.AttribFiles.QP_Options.DOrder = 2;
                    F.QP_Options.AttribFiles.QP_Options.DataLocation = 'TODO';
                    %
                    Props.VecType    = '';
                    if strcmp(F.QP_Options.AttribFiles.QP_Options.Dpsopt,'DP')
                        Props.Loc        = 'z';
                        Props.ReqLoc     = 'z';
                    else
                        Props.Loc        = 'd';
                        Props.ReqLoc     = 'd';
                    end
                    Props.Loc3D      = '';
                    Props.File       = 1;
                    Props.Fld        = -1;
                    Props.UseGrid    = 1;
                    % cmd is griddata or gridcelldata
                    Ans = gridfil(F,idom,Props,cmd,idx{M_},idx{N_});
                else
                    Ans = F;
                    depuni = inifile('hgeti',FI.mdf,'*','Depuni',NaN);
                    Ans.Val = repmat(depuni,size(F.X)); %size-1 if in cell centres?
                end
            case 'thin dams'
                F = FI.grd;
                F.X(end+1,:) = NaN;
                F.Y(end+1,:) = NaN;
                F.X(:,end+1) = NaN;
                F.Y(:,end+1) = NaN;
                F.QP_Options.AttribFiles = FI.thd;
                F.QP_Options.AttribFiles.FileType = 'thindam';
                %
                Props.VecType    = '';
                Props.Loc        = 'd';
                Props.ReqLoc     = 'd';
                Props.Loc3D      = '';
                Props.File       = 1;
                Props.Fld        = 1;
                Props.UseGrid    = 1;
                Ans = gridfil(F,idom,Props,cmd,idx{M_},idx{N_});
            case 'dry points'
                F = FI.grd;
                F.X(end+1,:) = NaN;
                F.Y(end+1,:) = NaN;
                F.X(:,end+1) = NaN;
                F.Y(:,end+1) = NaN;
                F.QP_Options.AttribFiles = FI.dry;
                F.QP_Options.AttribFiles.FileType = 'drypoint';
                %
                Props.VecType    = '';
                Props.Loc        = '';
                Props.ReqLoc     = '';
                Props.Loc3D      = '';
                Props.File       = 1;
                Props.Fld        = 1;
                Props.UseGrid    = 1;
                Ans = gridfil(F,idom,Props,cmd,idx{M_},idx{N_});
            case 'observation points'
                I0 = sub2ind(size(FI.grd.X),FI.sta.MN(idx{M_},1),FI.sta.MN(idx{M_},2));
                d1 = size(FI.grd.X,1);
                Ans.XY =  0;
                for i = [0 1 d1 d1+1]
                    I = max(I0-i,1); 
                    Ans.XY = Ans.XY + [FI.grd.X(I) FI.grd.Y(I)];
                end
                Ans.XY = Ans.XY/4;
                Ans.Val = FI.sta.Name(idx{M_});
            otherwise
                Ans = [];
        end
    case 'D-Flow FM'
        switch Props.Name
            case {FI.mesh.quant.Name}
                j = Props.varid;
                Ans = netcdffil(FI.mesh.nc_file,idom,FI.mesh.quant(j),'griddata',idx{M_});
            case 'initial water level samples'
                Ans.XY  = FI.WaterLevIni(idx{M_},1:2);
                Ans.Val = FI.WaterLevIni(idx{M_},3);
            case 'bed levels'
                Ans = netcdffil(FI.mesh.nc_file,idom,FI.BedLevel,'griddata',idx{M_});
                % use uniform value for locations without value assigned.
                Ans.Val(isnan(Ans.Val)) = FI.BedLevelUni;
            case {'bed level','bed level on 1D mesh','bed level on 2D mesh'}
                j = Props.varid;
                Ans = netcdffil(FI.mesh.nc_file,idom,FI.mesh.quant(j),'grid',idx{M_});
                Ans.Val = repmat(FI.BedLevel,size(Ans.X));
            case 'bed level samples'
                Ans.XY  = FI.BedLevel(idx{M_},1:2);
                Ans.Val = FI.BedLevel(idx{M_},3);
            case 'fixed weirs'
                Ans.XY  = FI.FixedWeir(:,1:2);
                Ans.Val = FI.FixedWeir(:,3);
            case 'observation points'
                Ans.XY  = zeros(sz(M_),2);
                Ans.Val = cell(sz(M_),1);
                offset = 0;
                network_loaded = false;
                for i = 1:length(FI.Obs)
                    nobj = length(FI.Obs{i}.Name);
                    Mask = idx{M_}>offset & idx{M_}<=offset+nobj;
                    if any(Mask)
                        iObj = idx{M_}(Mask)-offset;
                        Ans.Val(Mask)  = FI.Obs{i}.Name(iObj);
                        Ans.XY(Mask,:) = FI.Obs{i}.XY(iObj,:);
                        hasBranchId    = ~cellfun(@isempty, FI.Obs{i}.BranchId(iObj));
                        if any(hasBranchId)
                            if ~network_loaded
                                Network = netcdffil(FI.mesh.nc_file, idom, FI.mesh.quant(4), 'griddata');
                                network_loaded = true;
                            end
                            iObj = iObj(hasBranchId);
                            Ans.XY(offset+iObj, :) = branch_idoffset2xy(Network, FI.Obs{i}.BranchId(iObj), FI.Obs{i}.Offset(iObj));
                        end
                    end
                    offset = offset+nobj;
                end
            case 'observation cross sections'
                Ans.XY  = cell(sz(M_),1);
                Ans.Val = cell(sz(M_),1);
                offset = 0;
                network_loaded = false;
                for i = 1:length(FI.Crs)
                    nobj = length(FI.Crs{i}.Name);
                    Mask = idx{M_}>offset & idx{M_}<=offset+nobj;
                    if any(Mask)
                        iObj = idx{M_}(Mask)-offset;
                        Ans.Val(Mask) = FI.Crs{i}.Name(iObj);
                        Ans.XY(Mask)  = FI.Crs{i}.XY(iObj);
                        hasBranchId   = ~cellfun(@isempty, FI.Crs{i}.BranchId(iObj));
                        if any(hasBranchId)
                            if ~network_loaded
                                Network = netcdffil(FI.mesh.nc_file, idom, FI.mesh.quant(4), 'griddata');
                                network_loaded = true;
                            end
                            iObj = iObj(hasBranchId);
                            XYvec = branch_idoffset2xy(Network, FI.Crs{i}.BranchId(iObj), FI.Crs{i}.Offset(iObj));
                            Ans.XY(offset+iObj) = mat2cell(XYvec,ones(1,size(XYvec,1)),2);
                        end
                    end
                end
            case 'dambreak start points'
                [~,IndexChapter] = inifile('existsi',FI.Structure,'structure');
                types = inifile('hcgetstringi',FI.Structure,IndexChapter,'type');
                IndexChapter = IndexChapter(strcmp(types,'dambreak'));
                IndexChapter = IndexChapter(idx{ST_});
                %
                x = inifile('hcgeti',FI.Structure,IndexChapter,'startLocationX',NaN);
                y = inifile('hcgeti',FI.Structure,IndexChapter,'startLocationY',NaN);
                Ans.X = [x{:}];
                Ans.Y = [y{:}];
            otherwise
                if ~isempty(strfind(Props.Name,'open boundaries'))
                    ibtp = strcmp(FI.ExtForceNew.Bnd.Types,strtok(Props.Name));
                    bnds = FI.ExtForceNew.Bnd.Locs{ibtp};
                    bnds = bnds(idx{M_});
                    [~,ibloc] = ismember(bnds,FI.ExtForceNew.BndLoc.Names);
                    bfil = FI.ExtForceNew.BndLoc.Files(ibloc);
                    Ans.XY = cell(length(bfil),1);
                    for i = 1:length(bfil)
                        Ans.XY{i} = bfil{i}.Field.Data;
                    end
                    Ans.Val = bnds;
                elseif length(Props.Name)>11 && strcmp(Props.Name(end-10:end),' structures')
                    [~,IndexChapter] = inifile('existsi',FI.Structure,'structure');
                    types = inifile('hcgetstringi',FI.Structure,IndexChapter,'type');
                    IndexChapter = IndexChapter(strcmp(types,Props.Name(1:end-11)));
                    IndexChapter = IndexChapter(idx{ST_});
                    %
                    % location could be:
                    % - branchId, chainage: 1D structure
                    branchIds = inifile('hcgetstringi',FI.Structure,IndexChapter,'branchId','');
                    chainage = 'toread';
                    % - numCoordinates, xCoordinates, yCoordinates
                    nCoords = inifile('hgeti',FI.Structure,IndexChapter,'numCoordinates',NaN);
                    xCoords = 'toread';
                    % - polylinefile: v1.00 structure file
                    pli_files = inifile('hcgetstringi',FI.Structure,IndexChapter,'polylinefile','');
                    path_str = fileparts(FI.Structure.FileName);
                    for j = length(IndexChapter):-1:1
                        if ~isempty(branchIds{j})
                            % branchId, chainage
                            if isequal(chainage,'toread')
                                chainage = inifile('hgeti',FI.Structure,IndexChapter,'chainage',NaN);
                                Network = netcdffil(FI.mesh.nc_file, idom, FI.mesh.quant(4), 'griddata');
                            end
                            Ans.XY{j} = branch_idoffset2xy(Network, branchIds{j}, chainage(j));
                        elseif ~isnan(nCoords(j))
                            % numCoordinates, xCoordinates, yCoordinates
                            if isequal(xCoords,'toread')
                                xCoords = inifile('hcgeti',FI.Structure,IndexChapter,'xCoordinates',NaN);
                                yCoords = inifile('hcgeti',FI.Structure,IndexChapter,'yCoordinates',NaN);
                            end
                            Ans.XY{j} = [xCoords{j}' yCoords{j}'];
                        else
                            Ans.XY{j} = landboundary('read',relpath(path_str,pli_files{j}));
                        end
                    end
                    Ans.Val = inifile('hcgetstringi',FI.Structure,IndexChapter,'id','');
                else
                    Ans = [];
                end
        end
        if isfield(Ans,'XY') && ~isfield(Ans,'XUnits')
            Ans.XUnits = FI.mesh.XYUnits;
            Ans.YUnits = FI.mesh.XYUnits;
        end
    case 'D-Wave'
        switch Props.Name
            case 'grid'
                Ans.X = FI.domain(idom).grd.X(idx{M_},idx{N_});
                Ans.Y = FI.domain(idom).grd.Y(idx{M_},idx{N_});
            otherwise
                Ans = [];
        end
end

if ~isempty(oFI)
    oFI.Domains{odom,3} = FI;
    FI = oFI;
end
varargout={Ans FI};
% -----------------------------------------------------------------------------


function XY = geom2xy(G)
p = strfind(G,'(');
GeomType = G(1:p-1);
if strncmp(GeomType,'LINESTRING',10)
    xy = sscanf(G(p+1:end),'%f');
    switch length(xy)
        case 2
            XY = sscanf(G(p+1:end),'%f %f,',[2 inf]);
            XY = XY';
        case 3
            XY = sscanf(G(p+1:end),'%f %f %f,',[3 inf]);
            XY = XY(1:2,:)';
    end
else
    error('Geometry type "%s" not yet supported.',GeomType)
end

% -----------------------------------------------------------------------------
function Out=domains(FI)
switch FI.FileType
    case 'Delft3D D-Wave'
        Out = {FI.domain.name};
    case 'Delft3D Coupled Model'
        Out = FI.Domains(:,2);
    otherwise
        Out = {};
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,idom)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'SubFld' 'MNK' 'varid'  'DimName' 'hasCoords' 'VectorDef' 'ClosedPoly' 'UseGrid'};
DataProps={'-------'                ''      ''     ''      [0 0 0 0 0]  0           0      []       0     []          {}          0         0          0          0};
Out=cell2struct(DataProps,PropNames,2);
switch FI.FileType
    case 'Delft3D 1D2D mapping'
        Out(1).Name = '1D2D links';
        Out(1).Geom = 'POLYL';
        Out(1).Coords = 'xy';
        Out(1).DimFlag(M_) = 1;
        %
        Out(2) = Out(1);
        Out(2).Name = '1D2D link numbers';
        Out(2).NVal = 1;
    case 'Delft3D D-Flow1D'
        if isfield(FI,'bndLoc')
            F=inifile('hcgeti',FI.bndLoc,'Boundary','type');
            BT=[F{:}];
            uBT=unique(BT);
        else
            uBT={};
        end
        nBT=length(uBT);
        %
        if isfield(FI,'strucLoc') && inifile('exists',FI.strucLoc,'Structure')
            ST=inifile('hcgetstringi',FI.strucLoc,'Structure','type');
            uST=unique(ST);
        else
            uST={};
        end
        nST=length(uST);
        %
        if isfield(FI,'obs')
            Obs=inifile('hcgetstringi',FI.obs,'ObservationPoint','id');
        else
            Obs = {};
        end
        nObs=length(Obs);
        hasObs = nObs>0;
        %
        % CrossSection types have been copied from their definition records
        % to the location record in MDF.
        if isfield(FI,'crsLoc')
            CT=inifile('hcgetstringi',FI.crsLoc,'CrossSection','type');
            uCT=unique(CT);
        else
            uCT = {};
        end
        nCT=length(uCT);
        hasCxyz = any(strcmp('xyz',uCT));
        %
        try
            LAT=inifile('hcgetstringi',FI.latLoc,'LateralDischarge','id');
            hasLAT=1;
        catch
            hasLAT=0;
        end
        %
        nFLD = 0;
        flds = {'calcdim','calcpnt', ... % calculation points
            'morph_gr', ... % calculation points (morphology)
            'reachdim','flowanal','reachseg','rsegsub', ... % reach segments
            'strucdim','struc', ... % structures
            'qlat', ... % lateral discharges
            'qwb', ... % global water balance
            'measstat', 'nodes_cr','reach_cr','struc_cr'}; % node, calculation point, reach segment, structure states just before crash
        for i = 1:length(flds)
            if isfield(FI,flds{i})
                nFLD = nFLD+1+length(FI.(flds{i}).SubsName);
            end
        end
        %
        if isfield(FI,'output')
            netcdf_files = fieldnames(FI.output);
            nfiles = length(netcdf_files);
            netcdf_fields = zeros(1,nfiles);
            n_netcdf_geovars = zeros(1,nfiles);
            for i = 1:nfiles
                switch netcdf_files{i}
                    case {'gridpoints','reachsegments'}
                        n_netcdf_geovars(i) = 6;
                    case 'observations'
                        n_netcdf_geovars(i) = 4;
                    %case 'waterbalance' % no good data set yet to implement this ...
                    %    n_netcdf_geovars(i) = 2;
                    otherwise
                        % netcdf_data(i) = 0;
                        continue
                end
                n_netcdf_vars = length(FI.output.(netcdf_files{i}).Dataset);
                if FI.output.(netcdf_files{i}).Dimension(1).Length > 0
                    netcdf_fields(i) = max(0, n_netcdf_vars - n_netcdf_geovars(i)) + 1;
                end
            end
        else
            netcdf_fields = [];
        end
        %
        nItems = 6+nCT+hasCxyz+1+nBT+hasLAT+1+nST+1+hasObs+1+nFLD+sum(netcdf_fields);
        %
        Out(2:nItems) = Out(1);
        Out(1).Name = 'network';
        Out(1).Geom = 'POLYL';
        Out(1).Coords = 'xy';
        Out(1).DimFlag(M_) = 1;
        %
        Out(2).Name = 'nodes';
        Out(2).Geom = 'PNT';
        Out(2).Coords = 'xy';
        Out(2).NVal = 4;
        Out(2).DimFlag(M_) = 1;
        %
        modelGrid = 3;
        Out(3).Name = 'grid';
        Out(3).Geom = 'UGRID1D_NETWORK-NODE';
        Out(3).Coords = 'xy';
        Out(3).NVal = 0;
        Out(3).DimFlag(M_) = 1;
        Out(3).UseGrid = modelGrid;
        %
        Out(4).Name = 'grid points';
        Out(4).Geom = 'UGRID1D-NODE';
        Out(4).Coords = 'xy';
        Out(4).NVal = 4;
        Out(4).DimFlag(M_) = 1;
        Out(4).UseGrid = modelGrid;
        nFLD = 4;
        %
        nFLD = nFLD+1; % skip one for separator
        for i = 1:nCT
            Out(nFLD+i).Name = [uCT{i} ' cross section points'];
            Out(nFLD+i).Geom = 'PNT';
            Out(nFLD+i).Coords = 'xy';
            Out(nFLD+i).NVal = 4;
            Out(nFLD+i).DimFlag(M_) = 1;
            Out(nFLD+i).varid = uCT{i};
        end
        nFLD = nFLD+nCT;
        if hasCxyz
            nFLD = nFLD+1;
            Out(nFLD).Name = 'xyz cross section lines';
            Out(nFLD).Geom = 'POLYL';
            Out(nFLD).Coords = 'xy';
            Out(nFLD).DimFlag(M_) = 1;
            Out(nFLD).varid = uCT{i};
        end
        %
        nFLD = nFLD+1; % skip one for separator
        for i = 1:nBT
            switch uBT(i)
                case 1
                    Name = 'water level boundary points';
                case 2
                    Name = 'discharge boundary points';
                otherwise
                    Name = sprintf('boundary points - type %i',uBT(i));
            end
            Out(nFLD+i).Name = Name;
            Out(nFLD+i).Geom = 'PNT';
            Out(nFLD+i).Coords = 'xy';
            Out(nFLD+i).NVal = 4;
            Out(nFLD+i).DimFlag(M_) = 1;
            Out(nFLD+i).varid = uBT(i);
        end
        nFLD = nFLD+nBT;
        if hasLAT
            Out(nFLD+1).Name = 'lateral discharges';
            Out(nFLD+1).Geom = 'PNT';
            Out(nFLD+1).Coords = 'xy';
            Out(nFLD+1).NVal = 4;
            Out(nFLD+1).DimFlag(M_) = 1;
            nFLD = nFLD+1;
        end
        %
        nFLD = nFLD+1; % skip one for separator
        for i = 1:nST
            switch uST{i}
                case 'universalWeir'
                    Name = 'universal weir';
                otherwise
                    Name = uST{i};
            end
            Out(nFLD+i).Name = ['structure points - ' Name];
            Out(nFLD+i).Geom = 'PNT';
            Out(nFLD+i).Coords = 'xy';
            Out(nFLD+i).NVal = 4;
            Out(nFLD+i).DimFlag(M_) = 1;
            Out(nFLD+i).varid = uST{i};
        end
        nFLD = nFLD+nST;
        %
        nFLD = nFLD+1; % skip one for separator
        if hasObs
            nFLD = nFLD+1;
            Out(nFLD).Name = 'observation points';
            Out(nFLD).Geom = 'PNT';
            Out(nFLD).Coords = 'xy';
            Out(nFLD).NVal = 4;
            Out(nFLD).DimFlag(M_) = 1;
        end
        %
        for i = 1:length(flds)
            if isfield(FI,flds{i})
                FI_fld = FI.(flds{i});
                nFLD = nFLD+1; % skip one for separator
                for j = 1:length(FI_fld.SubsName)
                    nFLD = nFLD+1;
                    Name  = FI_fld.SubsName{j};
                    if Name(end)==')'
                        b = strfind(Name,'(');
                        if isempty(b)
                            Units = '';
                        else
                            Units = Name(b(end)+1:end-1);
                            Name  = deblank(Name(1:b(end)-1));
                        end
                    else
                        Units = '';
                    end
                    Out(nFLD).Name  = Name;
                    Out(nFLD).Units = Units;
                    Out(nFLD).Geom  = 'PNT';
                    Out(nFLD).Coords = 'xy';
                    Out(nFLD).NVal  = 1;
                    if strcmp(flds{i},'qwb')
                        Out(nFLD).SubFld = FI_fld.SegmentName;
                        Out(nFLD).DimFlag(T_) = 1;
                    else
                        Out(nFLD).DimFlag([T_ M_]) = 1;
                    end
                    Out(nFLD).varid = {flds{i} j};
                end
            end
        end
        %
        if ~isempty(netcdf_fields)
            for i = 1:length(netcdf_files)
                if netcdf_fields(i) == 0
                    continue
                end
                nc = FI.output.(netcdf_files{i});
                
                nFLD = nFLD+1; % skip one for separator
                for j = (n_netcdf_geovars(i)+1):length(nc.Dataset)
                    nFLD = nFLD+1;
                    Atts = {nc.Dataset(j).Attribute.Name};
                    %
                    is_long_name = strcmp('long_name',Atts);
                    if any(is_long_name)
                        Out(nFLD).Name = nc.Dataset(j).Attribute(is_long_name).Value;
                    else
                        Out(nFLD).Name = nc.Dataset(j).Name;
                    end
                    %
                    is_units = strcmp('units',Atts);
                    if any(is_units)
                        Out(nFLD).Units = nc.Dataset(j).Attribute(is_units).Value;
                    else
                        Out(nFLD).Units = '';
                    end
                    %
                    Out(nFLD).DimFlag([T_ M_]) = 1;
                    switch netcdf_files{i}
                        case 'gridpoints'
                            Out(nFLD).Geom  = 'UGRID1D_NETWORK-NODE';
                            Out(nFLD).UseGrid = modelGrid;
                        case 'reachsegments'
                            Out(nFLD).Geom  = 'UGRID1D_NETWORK-EDGE';
                            Out(nFLD).UseGrid = modelGrid;
                        case 'observations'
                            if isempty(strfind(lower(Out(nFLD).Name),'observ'))
                                % introduced to separater "water level" at
                                % observation points from "water level" at
                                % grid points, but also encountered
                                % Observedwaterlevel (yes, ugly one word)
                                % but that doesn't need the addition at
                                % observation point.
                                Out(nFLD).Name = [Out(nFLD).Name ' at observation point'];
                            end
                            Out(nFLD).Geom  = 'PNT';
                            Out(nFLD).DimFlag(M_) = 0;
                            Out(nFLD).DimFlag(ST_) = 3;
                            Out(nFLD).UseGrid = 0;
                        case 'waterbalance'
                            Out(nFLD).Geom  = ''; % still unknown to me ...
                            Out(nFLD).UseGrid = 0;
                    end
                    Out(nFLD).Coords = 'xy';
                    Out(nFLD).NVal  = 1;
                    Out(nFLD).varid = {'output', netcdf_files{i}, j};
                end
            end
        end
    case 'Delft3D D-Flow2D3D'
        flds = {'grd','-','dep','thd','dry','-','bnd','bct','-','sta','crs'};
        %
        nfld = 0;
        for i = 1:length(flds)
            if isequal(flds{i},'-')
                nfld = nfld+1;
            elseif isequal(flds{i},'dep')
                % include bed levels always if there is a grid
                if isfield(FI,'grd')
                    nfld = nfld+1;
                end
            elseif isfield(FI,flds{i})
                switch flds{i}
                    case 'bnd'
                        nfld = nfld+length(unique(FI.bnd.BndType));
                    case 'bct'
                        nfld = nfld+length(FI.bct.Table);
                    otherwise
                        nfld = nfld+1;
                end
            end
        end
        %
        Out(1:nfld) = Out(1);
        %
        ifld = 0;
        for i = 1:length(flds)
            if isequal(flds{i},'-')
                ifld = ifld+1;
            elseif isfield(FI,flds{i}) || (isequal(flds{i},'dep') && isfield(FI,'grd'))
                ifld = ifld+1;
                switch flds{i}
                    case 'grd'
                        Out(ifld).Name = 'grid';
                        Out(ifld).Geom = 'sQUAD';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag([M_ N_]) = 1;
                    case 'dep'
                        Out(ifld).Name = 'bed levels';
                        Out(ifld).Units = 'm';
                        Out(ifld).Geom = 'sQUAD';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag([M_ N_]) = 1;
                        Out(ifld).NVal = 1;
                        dpsopt = rmhash(inifile('hgeti',FI.mdf,'*','Dpsopt','#MEAN#'));
                        if strcmpi(dpsopt,'DP')
                            Out(ifld).DataInCell = 2;
                        else
                            Out(ifld).DataInCell = 1;
                        end
                    case 'thd'
                        Out(ifld).Name = 'thin dams';
                        Out(ifld).Geom = 'sQUAD';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag([M_ N_]) = 1;
                    case 'dry'
                        Out(ifld).Name = 'dry points';
                        Out(ifld).Geom = 'sQUAD';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag([M_ N_]) = 1;
                        Out(ifld).DataInCell = 2;
                        Out(ifld).NVal = 5;
                    case 'TOBEIMPLEMENTED_bnd'
                        ifld = ifld-1;
                        %
                        bTypes = unique(FI.bnd.BndType);
                        for ib = 1:length(bTypes)
                            ifld = ifld+1;
                            bType = bTypes(ib);
                            switch bType
                                case 'Z'
                                    bType = 'water level';
                                    bTyp2 = 'water elevation (z)';
                                case 'C'
                                    bType = 'current';
                                    bTyp2 = 'current         (c)';
                                case 'Q'
                                    bType = 'discharge';
                                    bTyp2 = 'flux/discharge  (q)';
                                case 'R'
                                    bType = 'Riemann';
                                    bTyp2 = 'riemann         (r)';
                                case 'T'
                                    bType = 'total discharge';
                                    bTyp2 = 'total discharge (t)';
                                case 'N'
                                    bType = 'Neumann';
                                    bTyp2 = 'neumann         (n)';
                            end
                            Out(ifld).Name = [bType ' open boundaries'];
                            Out(ifld).Geom = 'POLYL';
                            Out(ifld).Coords = 'xy';
                            Out(ifld).DimFlag(ST_) = 1;
                            Out(ifld).NVal = 4;
                            %
                            if strcmp(FI.bnd.Forcing(ib),'T')
                                for ib2 = 1:length(FI.bct.Table)
                                    bType2 = FI.bct.Table(ib2).Parameter(2).Name(1:19);
                                    if ~strcmp(bType2,bTyp2)
                                        continue
                                    end
                                    %
                                    ifld = ifld+1;
                                    Out(ifld).Name = [bType ' time series at ' FI.bct.Table(ib2).Location];
                                    Out(ifld).Geom = 'PNT';
                                    Out(ifld).DimFlag([ST_ T_]) = 1;
                                    Out(ifld).NVal = 1;
                                end
                            end
                        end
                    case 'bct'
                        % skip treated above
                    case 'sta'
                        Out(ifld).Name = 'observation points';
                        Out(ifld).Geom = 'PNT';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag(M_) = 1;
                        Out(ifld).NVal = 4;
                    case 'TOBEIMPLEMENTED_crs'
                        Out(ifld).Name = 'observation cross sections';
                        Out(ifld).Geom = 'POLYL';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag(M_) = 1;
                        Out(ifld).NVal = 4;
                end
            end
        end
    case 'Delft3D D-Flow FM'
        flds = {'mesh','-','WaterLevIni','BedLevel','-','FixedWeir','Structure','-','ExtForce','-','ExtForceNew','-','Obs','Crs'};
        nfld = 0;
        for i = 1:length(flds)
            if isequal(flds{i},'-')
                nfld = nfld+1;
            elseif isfield(FI,flds{i})
                switch flds{i}
                    case 'mesh'
                        nfld = nfld + length(FI.mesh.quant);
                    case 'BedLevel'
                        if isscalar(FI.BedLevel)
                            nfld = nfld + length(FI.mesh.meshes);
                        elseif ~isempty(FI.BedLevel)
                            nfld = nfld + 1;
                        end
                    case 'Structure'
                        types = inifile('hcgetstringi',FI.Structure,'structure','type');
                        utypes = unique(types);
                        nfld = nfld + length(utypes);
                    case 'ExtForce'
                        nfld = nfld + length(FI.ExtForce);
                    case 'ExtForceNew'
                        nfld = nfld + length(FI.ExtForceNew.Bnd.Types);
                        for iBT = 1:length(FI.ExtForceNew.Bnd.Types) % return 0 for non-time series
                            nfld = nfld + sum(cellfun(@(f)lentim(f),FI.ExtForceNew.Bnd.Forcing{iBT}));
                        end
                    otherwise
                        nfld = nfld+1;
                end
            end
        end
        %
        Out(1:nfld) = Out(1);
        %
        ifld = 0;
        for i = 1:length(flds)
            if isequal(flds{i},'-')
                ifld = ifld+1;
            elseif isfield(FI,flds{i})
                switch flds{i}
                    case 'mesh'
                        for j = 1:length(FI.mesh.quant)
                            ifld = ifld+1;
                            Out(ifld).Name = FI.mesh.quant(j).Name;
                            Out(ifld).Geom = FI.mesh.quant(j).Geom;
                            Out(ifld).NVal = FI.mesh.quant(j).NVal;
                            Out(ifld).Coords = 'xy';
                            Out(ifld).DimFlag(M_) = 6;
                            Out(ifld).varid = j;
                        end
                    case 'WaterLevIni'
                        ifld = ifld+1;
                        Out(ifld).Name = 'initial water level samples';
                        Out(ifld).Units = 'm';
                        Out(ifld).Geom = 'PNT';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag(M_) = 1;
                        Out(ifld).NVal = 1;
                    case 'BedLevel'
                        ifld = ifld+1;
                        BL = FI.(flds{i});
                        if isstruct(BL) % quantity on mesh
                            if isempty(BL)
                                ifld = ifld-1;
                            elseif FI.BedLevelType == 1
                                Out(ifld).Name = 'bed levels';
                                Out(ifld).Units = 'm';
                                Out(ifld).Geom = 'UGRID2D-FACE';
                                Out(ifld).Coords = 'xy';
                                Out(ifld).DimFlag(M_) = 6;
                                Out(ifld).NVal = 1;
                            else
                                Out(ifld).Name = 'bed levels';
                                Out(ifld).Units = 'm';
                                Out(ifld).Geom = 'UGRID2D-NODE';
                                Out(ifld).Coords = 'xy';
                                Out(ifld).DimFlag(M_) = 6;
                                Out(ifld).NVal = 1;
                            end
                        elseif isscalar(BL)
                            for j = FI.mesh.meshes(:)'
                                Out(ifld).Name = 'bed level';
                                if length(FI.mesh.meshes) > 1
                                    if ~isempty(strfind(FI.mesh.quant(j).Name,'1D'))
                                        Out(ifld).Name = [Out(ifld).Name ' on 1D mesh'];
                                    else
                                        Out(ifld).Name = [Out(ifld).Name ' on 2D mesh'];
                                    end
                                end
                                Out(ifld).Units = 'm';
                                Out(ifld).Geom = 'UGRID1D-NODE';
                                Out(ifld).Coords = 'xy';
                                Out(ifld).DimFlag(M_) = 6;
                                Out(ifld).NVal = 1;
                                Out(ifld).varid = j;
                            end
                        else
                            Out(ifld).Name = 'bed level samples';
                            Out(ifld).Units = 'm';
                            Out(ifld).Geom = 'PNT';
                            Out(ifld).Coords = 'xy';
                            Out(ifld).DimFlag(M_) = 1;
                            Out(ifld).NVal = 1;
                        end
                    case 'FixedWeir'
                        ifld = ifld+1;
                        Out(ifld).Name = 'fixed weirs';
                        Out(ifld).Units = 'm';
                        Out(ifld).Geom = 'POLYL';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag(M_) = 1;
                        Out(ifld).NVal = 1;
                    case 'Structure'
                        for j = 1:length(utypes)
                            ifld = ifld+1;
                            Out(ifld).Name = [utypes{j} ' structures'];
                            Out(ifld).Geom = 'POLYL';
                            Out(ifld).Coords = 'xy';
                            Out(ifld).DimFlag(ST_) = 3;
                            Out(ifld).NVal = 4;
                        end
                        if ismember('dambreak',utypes)
                            ifld = ifld+1;
                            Out(ifld).Name = 'dambreak start points';
                            Out(ifld).Geom = 'PNT';
                            Out(ifld).Coords = 'xy';
                            Out(ifld).DimFlag(ST_) = 3;
                            Out(ifld).NVal = 0;
                        end
                    case 'TOBEIMPLEMENTED_ExtForce'
                        fNames = {FI.ExtForce.Quantity};
                        forces = unique(fNames);
                        translate = {'lowergatelevel'               'lower gate level'                          'm'
                            'damlevel'                              'dam level'                                 'm'
                            'pump'                                  'pump?'                                     '' % discharge?
                            'horizontaleddyviscositycoefficient'    'horizontal eddy viscosity coefficient'     ''
                            'horizontaleddydiffusivitycoefficient'  'horizontal eddy diffusivity coefficient'   ''
                            'bedlevel'                              'bed levels'                                'm'
                            'initialwaterlevel'                     'initial water level'                       'm'
                            'initialsalinity'                       'initial salinity'                          'ppt'
                            'initialsalinitytop'                    'initial salinity near water surface'       'ppt'
                            'initialverticaltemperatureprofile'     'initial vertical temperature profile'      'degC'
                            'initialverticalsalinityprofile'        'initial vertical salinity profile'         'ppt'
                            'windstresscoefficient'                 'wind stress coefficient'                   ''
                            'stemdiameter'                          'stem diameter'                             'm'
                            'stemdensity'                           'stem density'                              ''
                            'stemheight'                            'stem height'                               'm'
                            'interceptionlayerthickness'            'interception layer thickness'              'm'
                            'windx'                                 'wind velocity, x-component'                'm/s'
                            'windy'                                 'wind velocity, y-component'                'm/s'
                            'rainfall'                              'rainfall'                                  ''
                            'airpressure'                           'air pressure'                              ''
                            'atmosphericpressure'                   'air pressure'                              ''};
                        for iuforce = 1:length(forces)
                            fName = forces{iuforce};
                            iforces = find(strcmp(fName,fNames));
                            for iforce = iforces
                                ifld = ifld+1;
                                switch fName
                                    case 'frictioncoefficient'
                                        frctyp = inifile('hgeti',FI.mdu,'physics','UnifFrictType',1);
                                        switch frctyp
                                            case 0
                                                ForceName  = 'Chezy C';
                                                ForceUnits = 'm^{1/2}/s';
                                            case 1
                                                ForceName  = 'Manning n';
                                                ForceUnits = 's/m^{1/3}';
                                            case 2
                                                ForceName  = 'White-Colebrook/Nikuradse k';
                                                ForceUnits = 'm';
                                            case 3
                                                ForceName  = 'White-Colebrook/Nikuradse k (WAQUA implementation)';
                                                ForceUnits = 'm';
                                        end
                                    case translate(:,1)
                                        ifrc = find(strcmp(fName,translate(:,1)));
                                        ForceName  = translate{ifrc,2};
                                        ForceUnits = translate{ifrc,3};
                                    otherwise
                                        ForceName  = fName;
                                        ForceUnits = '';
                                end
                                Out(ifld).Name  = ForceName;
                                Out(ifld).Units = ForceUnits;
                                Out(ifld).varid = {'ExtForce',iforce};
                                switch FI.ExtForce(iforce).FileType
                                    case 'uniform'
                                        Out(ifld).Geom = 'PNT';
                                        Out(ifld).DimFlag(T_) = 1;
                                        Out(ifld).NVal = 1;
                                    case 'unimagdir'
                                        Out(ifld).Geom = 'PNT';
                                        Out(ifld).DimFlag(T_) = 1;
                                        Out(ifld).NVal = 2;
                                    case {'curvi'}
                                        Out(ifld).Geom = 'sQUAD';
                                        Out(ifld).Coords = 'xy';
                                        Out(ifld).DimFlag([T_,M_,N_]) = 1;
                                        Out(ifld).NVal = 1;
                                    case 'triangulation'
                                        Out(ifld).Geom = 'PNT';
                                        Out(ifld).Coords = 'xy';
                                        Out(ifld).DimFlag(M_) = 1;
                                        Out(ifld).NVal = 1;
                                    case 'triangulation_magdir'
                                        Out(ifld).Geom = 'PNT';
                                        Out(ifld).Coords = 'xy';
                                        Out(ifld).DimFlag(M_) = 1;
                                        Out(ifld).NVal = 2;
                                    case 'polyline'
                                        Out(ifld).Geom = 'POLYL';
                                        Out(ifld).Coords = 'xy';
                                        Out(ifld).DimFlag(M_) = 1;
                                        Out(ifld).NVal = 1;
                                    otherwise
                                        error('Not yet supported file type %s for %s', FI.ExtForce(iforce).FileType, FI.ExtForce(iforce).Quantity)
                                end
                                Out(ifld).Name = [Out(ifld).Name ' - ' abbrevfn(FI.ExtForce(iforce).FileName) ' - ' FI.ExtForce(iforce).Operand];
                            end
                        end
                    case 'TOBEIMPLEMENTED_ExtForceNew'
                        for itype = 1:length(FI.ExtForceNew.Bnd.Types)
                            ifld = ifld+1;
                            Out(ifld).Name = [FI.ExtForceNew.Bnd.Types{itype} ' open boundaries'];
                            Out(ifld).Geom = 'POLYL';
                            Out(ifld).Coords = 'xy';
                            Out(ifld).DimFlag(M_) = 1;
                            Out(ifld).NVal = 4;
                            %
                            for iloc = 1:length(FI.ExtForceNew.Bnd.Locs{itype})
                                Force = FI.ExtForceNew.Bnd.Forcing{itype}{iloc};
                                if ~isfield(Force,'Data')
                                    continue
                                end
                                for ipnt = 1:size(Force.Data,1)
                                    if ~strcmp(inifile('hgeti',Force,ipnt,'Function'),'timeseries')
                                        continue
                                    end
                                    ifld = ifld+1;
                                    Loc = inifile('hgeti',Force,ipnt,'Name');
                                    Out(ifld).Name = [FI.ExtForceNew.Bnd.Types{itype} ' time series at ' Loc];
                                    Out(ifld).Geom = 'PNT';
                                    Out(ifld).Coords = 'xy';
                                    Out(ifld).DimFlag(M_) = 1;
                                    Out(ifld).NVal = 1;
                                end
                            end
                        end
                    case 'Obs'
                        ifld = ifld+1;
                        Out(ifld).Name = 'observation points';
                        Out(ifld).Geom = 'PNT';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag(M_) = 1;
                        Out(ifld).NVal = 4;
                    case 'Crs'
                        ifld = ifld+1;
                        Out(ifld).Name = 'observation cross sections';
                        Out(ifld).Geom = 'POLYL';
                        Out(ifld).Coords = 'xy';
                        Out(ifld).DimFlag(M_) = 1;
                        Out(ifld).NVal = 4;
                end
            end
        end
    case 'Delft3D D-Wave'
        Out(1).Name = 'grid';
        Out(1).Geom = 'sQUAD';
        Out(1).Coords = 'xy';
        Out(1).DimFlag([M_ N_]) = 1;
    otherwise
        Out(:,1) = [];
end
% -----------------------------------------------------------------------------

function l = lentim(f)
if isfield(f,'Data')
    l = size(f.Data,1);
else
    l = 0;
end

% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
if iscell(Props.SubFld)
    subf=Props.SubFld;
    if nargin>2 && f~=0
        subf=subf(f);
    end
else
    subf={};
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function sz=getsize(FI,idom,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
ndims = length(Props.DimFlag);
sz = zeros(1,ndims);
switch FI.FileType
    case 'Delft3D 1D2D mapping'
        F=inifile('chaptersi',FI.mapping);
        sz(M_) = sum(strcmpi(F,'1d2dLink'));
    case 'Delft3D D-Flow1D'
        switch Props.Name
            case 'network'
                F=inifile('chaptersi',FI.ntw);
                sz(M_) = sum(strcmpi(F,'Branch'));
            case 'nodes'
                F=inifile('chaptersi',FI.ntw);
                sz(M_) = sum(strcmpi(F,'Node'));
            case {'grid','grid points'}
                F=inifile('hcgeti',FI.ntw,'Branch','gridPointsCount');
                sz(M_) = sum([F{:}]);
            case 'lateral discharges';
                F=inifile('hcgetstringi',FI.latLoc,'LateralDischarge','id');
                sz(M_) = length(F);
            case 'observation points'
                F=inifile('hcgetstringi',FI.obs,'ObservationPoint','id');
                sz(M_) = length(F);
            otherwise
                if strcmp(Props.Name,'xyz cross section lines') || ...
                        ~isempty(strfind(Props.Name,'cross section points'))
                    CT=inifile('hcgeti',FI.crsLoc,'CrossSection','type');
                    sz(M_)=sum(strcmp(Props.varid,CT));
                elseif ~isempty(strfind(Props.Name,'boundary points'))
                    F=inifile('hcgeti',FI.bndLoc,'Boundary','type');
                    BT = [F{:}];
                    sz(M_) = sum(BT==Props.varid);
                elseif strncmp(Props.Name,'structure points',16)
                    ST=inifile('hcgeti',FI.strucLoc,'Structure','type');
                    sz(M_)=sum(strcmp(Props.varid,ST));
                elseif ~iscell(Props.varid)
                    % for development purposes only ...
                elseif strcmp(Props.varid{1},'qwb')
                    sz(T_) = FI.(Props.varid{1}).NTimes;
                elseif strcmp(Props.varid{1},'output')
                    nc = FI.output.(Props.varid{2});
                    sz(T_) = nc.Dimension(1).Length; % time
                    if Props.DimFlag(ST_)
                        sz(ST_) = nc.Dimension(2).Length; % id
                    else
                        sz(M_) = nc.Dimension(2).Length; % id
                    end
                else
                    sz(T_) = FI.(Props.varid{1}).NTimes;
                    sz(M_) = FI.(Props.varid{1}).NumSegm;
                end
        end
    case 'Delft3D D-Flow2D3D'
        switch Props.Name
            case 'observation points'
                sz(M_) = size(FI.sta.MN,1);
            otherwise
                MNK = inifile('hgeti',FI.mdf,'*','MNKmax');
                sz([M_ N_]) = MNK(1:2);
        end
    case 'Delft3D D-Flow FM'
        switch Props.Name
            case {FI.mesh.quant.Name}
                j = Props.varid;
                grdSz = netcdffil(FI.mesh.nc_file,idom,FI.mesh.quant(j),'size');
                sz(M_) = grdSz(M_);
            case 'initial water level samples'
                sz(M_) = size(FI.WaterLevIni,1);
            case {'bed level','bed level on 1D mesh','bed level on 2D mesh'}
                j = Props.varid;
                grdSz = netcdffil(FI.mesh.nc_file,idom,FI.mesh.quant(j),'size');
                sz(M_) = grdSz(M_);
            case 'bed levels'
                grdSz = netcdffil(FI.mesh.nc_file,idom,FI.BedLevel,'size');
                sz(M_) = grdSz(M_);
            case 'bed level samples'
                sz(M_) = size(FI.BedLevel,1);
            case 'fixed weirs'
                sz(M_) = 1;
            case 'observation points'
                szM = 0;
                for i = 1:length(FI.Obs)
                    szM = szM + length(FI.Obs{i}.Name);
                end
                sz(M_) = szM;
            case 'observation cross sections'
                szM = 0;
                for i = 1:length(FI.Crs)
                    szM = szM + length(FI.Crs{i}.Name);
                end
                sz(M_) = szM;
            case 'dambreak start points'
                types = inifile('hcgetstringi',FI.Structure,'structure','type');
                sz(ST_) = sum(strcmpi(types,'dambreak'));
            otherwise
                if iscell(Props.varid)
                    switch Props.varid{1}
                        case 'ExtForce'
                            iforce = Props.varid{2};
                            switch FI.ExtForce(iforce).FileType
                                case 'curvi'
                                    szX = size(FI.ExtForce(iforce).File.Header.grid_file.X);
                                    sz(M_) = szX(1);
                                    sz(N_) = szX(2);
                                    sz(T_) = length(FI.ExtForce(iforce).File.Data);
                            end
                    end
                elseif ~isempty(strfind(Props.Name,'open boundaries'))
                    ibtp = strcmp(FI.ExtForceNew.Bnd.Types,strtok(Props.Name));
                    sz(ST_) = length(FI.ExtForceNew.Bnd.Locs{ibtp});
                elseif length(Props.Name)>11 && strcmp(Props.Name(end-10:end),' structures')
                    types = inifile('hcgetstringi',FI.Structure,'structure','type');
                    sz(ST_) = sum(strcmpi(types,Props.Name(1:end-11)));
                end
        end
    case 'Delft3D D-Wave'
        grdSz = size(FI.domain(idom).grd.X);
        sz([M_ N_]) = grdSz;
    otherwise
        % no generic default dimension code
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T = readtim(FI, idom, Props, t)
if nargin <4 || isequal(t,0)
    t = ':';
end
switch FI.FileType
    case 'Delft3D D-Flow1D'
        if iscell(Props.varid) && strcmp(Props.varid{1},'output')
            nc = FI.output.(Props.varid{2});
            timevar = nc.Dataset(1);
            T = qp_netcdf_gettime(nc, timevar.Name, t);
        end
    case 'Delft3D D-Flow FM'
        switch Props.Name
            otherwise
                if iscell(Props.varid)
                    switch Props.varid{1}
                        case 'ExtForce'
                            iforce = Props.varid{2};
                            switch FI.ExtForce(iforce).FileType
                                case 'curvi'
                                    T = [FI.ExtForce(iforce).File.Data(t).time];
                            end
                    end
                end
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
S={};
switch FI.FileType
    case 'Delft3D D-Flow1D'
        nc = FI.output.observations;
        var = nc.Dataset(2);
        Labels = qp_netcdf_get(nc,var.Name);
        S = cellstr(Labels);
    case 'Delft3D D-Flow FM'
        switch Props.Name
            case 'dambreak start points'
                ids = inifile('hcgetstringi',FI.Structure,'structure','id');
                types = inifile('hcgetstringi',FI.Structure,'structure','type');
                S = ids(strcmpi(types,'dambreak'));
            otherwise
                if length(Props.Name)>11 && strcmp(Props.Name(end-10:end),' structures')
                    ids = inifile('hcgetstringi',FI.Structure,'structure','id');
                    types = inifile('hcgetstringi',FI.Structure,'structure','type');
                    S = ids(strcmpi(types,Props.Name(1:end-11)));
                end
        end
end
if ~isempty(S) && ~isequal(t,0)
    S = S(t);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function xy = branch_idchain2xy(NTWini,bId,bCh)
nPnt = length(bId);
xy = NaN(nPnt,2);
%
[uBId,ia,ic] = unique(bId);
G = inifile('hcgeti',NTWini,'Branch','geometry');
GId = inifile('hcgetstringi',NTWini,'Branch','id');
GgpO = inifile('hcgeti',NTWini,'Branch','gridPointOffsets');
for i = 1:length(uBId)
    Branch = uBId(i);
    iBranch = ustrcmpi(Branch,GId);
    if iBranch>0
        XY   = geom2xy(G{iBranch});
        d    = pathdistance(XY(:,1),XY(:,2));
        db   = diff(d)==0;
        d(db)    = [];
        XY(db,:) = [];
        iOut = ic==i;
        cCS  = [bCh{iOut}];
        %
        bLen = GgpO{iBranch}(end);
        d = d*bLen/d(end);
        %
        xyCS = interp1(d,XY,cCS);
        xy(iOut,:) = xyCS;
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function xy = branch_idoffset2xy(Network, bId, bCh)
nPnt = length(bId);
xy = NaN(nPnt,2);
%
[uBId,~,ic] = unique(bId);
for i = 1:length(uBId)
    Branch = uBId(i);
    iBranch = ustrcmpi(Branch,Network.Val);
    if iBranch>0
        X = Network.EdgeGeometry.X{iBranch};
        Y = Network.EdgeGeometry.Y{iBranch};
        d     = pathdistance(X, Y);
        db    = diff(d)==0;
        d(db) = [];
        X(db) = [];
        Y(db) = [];
        iOut  = ic==i;
        cCS   = bCh(iOut);
        %
        bLen = d(end); % Need access to the branch length!
        d = d*bLen/d(end);
        %
        xCS = interp1(d, X, cCS);
        yCS = interp1(d, Y, cCS);
        xy(iOut,:) = [xCS yCS];
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function FI = check_gpXY(FI)
if ~isfield(FI,'gpXY')
    gpCnt = inifile('hcgeti',FI.ntw,'Branch','gridPointsCount');
    gpX = inifile('hcgeti',FI.ntw,'Branch','gridPointX');
    gpY = inifile('hcgeti',FI.ntw,'Branch','gridPointY');
    gpI = inifile('hcgetstringi',FI.ntw,'Branch','gridPointIds');
    gpCnt = [gpCnt{:}];
    nGP   = sum(gpCnt);
    nEdges = sum(gpCnt-1);
    FI.gpXY       = zeros(nGP,2);
    FI.gpInternal = true(nGP,1);
    FI.gpId       = cell(nGP,1);
    FI.gpConnect  = zeros(nEdges,2);
    oM = 0;
    oE = 0;
    for i = 1:length(gpCnt)
        nPnt = gpCnt(i);
        iM   = oM + (1:nPnt);
        iE   = oE + (1:nPnt-1);
        oM   = oM + nPnt;
        oE   = oE + nPnt-1;
        %
        FI.gpInternal(iM(1))   = false;
        FI.gpInternal(iM(end)) = false;
        FI.gpXY(iM,:) = [gpX{i}(:) gpY{i}(:)];
        FI.gpId(iM)   = multiline(gpI{i},';','cell');
        FI.gpConnect(iE,:) = [iM(1:end-1); iM(2:end)]';
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function FI = check_reachXY(FI)
if ~isfield(FI,'reachXY')
    reachCnt = inifile('hcgeti',FI.ntw,'Branch','gridPointsCount');
    brId = inifile('hcgetstringi',FI.ntw,'Branch','id');
    gpO = inifile('hcgeti',FI.ntw,'Branch','gridPointOffsets');
    reachCnt = [reachCnt{:}]-1;
    nReach   = sum(reachCnt);
    FI.reachXY = zeros(nReach,2);
    oM = 0;
    for i = 1:length(reachCnt)
        nSeg = reachCnt(i);
        iM   = oM + (1:nSeg);
        oM   = oM + nSeg;
        %
        segO = (gpO{i}(1:end-1)+gpO{i}(2:end))/2;
        FI.reachXY(iM,:) = branch_idchain2xy(FI.ntw,repmat(brId(i),1,nSeg),num2cell(segO));
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function FI = check_latXY(FI)
if ~isfield(FI,'latXY')
    bId = inifile('hcgetstringi',FI.latLoc,'LateralDischarge','branchid');
    bCh = inifile('hcgeti',FI.latLoc,'LateralDischarge','chainage');
    FI.latXY = branch_idchain2xy(FI.ntw,bId,bCh);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function FI = check_obsXY(FI)
if ~isfield(FI,'obsXY')
    bId = inifile('hcgetstringi',FI.obs,'ObservationPoint','branchid');
    bCh = inifile('hcgeti',FI.obs,'ObservationPoint','chainage');
    FI.obsXY = branch_idchain2xy(FI.ntw,bId,bCh);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function FI = check_crsXY(FI)
if ~isfield(FI,'crsXY')
    bId = inifile('hcgetstringi',FI.crsLoc,'CrossSection','branchid');
    bCh = inifile('hcgeti',FI.crsLoc,'CrossSection','chainage');
    FI.crsXY = branch_idchain2xy(FI.ntw,bId,bCh);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function FI = check_strucXY(FI)
if ~isfield(FI,'strucXY')
    bId = inifile('hcgetstringi',FI.strucLoc,'Structure','branchid');
    bCh = inifile('hcgeti',FI.strucLoc,'Structure','chainage');
    FI.strucXY = branch_idchain2xy(FI.ntw,bId,bCh);
end
% -----------------------------------------------------------------------------

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

% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};

switch cmd
    case 'initialize'
        switch FI.FileType
            case 'Delft3D D-Flow2D3D'
                OK = optfig_2D3D(mfig);
        end
    case {'save0', 'save90', 'save180', 'save270'}
        angle = str2double(cmd(5:end));
        if nargin>4
            pf = varargin{2};
            [p,f] = fileparts(pf);
        else
            cwd = pwd;
            p = fileparts(FI.mdf.FileName);
            cd(p)
            if matlabversionnumber<6
                filterspec='*.mdf';
            else
                filterspec = {'*.mdf' 'Delft3D-FLOW Master Definition File(*.mdf)'};
            end
            title = sprintf('Specify file name (clockwise rotated by %i degrees)',angle);
            [f,p]=uiputfile(filterspec,title);
            cd(cwd)
        end
        if ischar(f)
            [~,f] = fileparts(f); % get rid of any remaining .mdf extension
            FIR = mdf('rotate',FI,angle);
            mdf('write',FIR,f,p)
            cmdargs={cmd fullfile(p,[f '.mdf'])};
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function OK = optfig_2D3D(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 130 20], ...
    'String','Save Rotated Input', ...
    'HorizontalAlignment','left', ...
    'Style','text');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions save0', ...
    'Position',[141 voffset 20 20], ...
    'String','0', ...
    'HorizontalAlignment','right', ...
    'Tooltip','Non-rotated. Just model under a different name.');
qp_tooltip(h1,{'Model is not rotated.','Just save model under a different name.'});
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions save90', ...
    'Position',[171 voffset 47 20], ...
    'String','90', ...
    'HorizontalAlignment','right');
qp_tooltip(h1,{'Model rotated by 90 degrees clockwise.','Positive M direction becomes negative N direction.'});
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions save180', ...
    'Position',[228 voffset 47 20], ...
    'String','180', ...
    'HorizontalAlignment','right');
qp_tooltip(h1,{'Model rotated by 180 degrees clockwise.','Positive M direction becomes negative M direction.'});
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions save270', ...
    'Position',[284 voffset 47 20], ...
    'String','270', ...
    'HorizontalAlignment','right');
qp_tooltip(h1,{'Model rotated by 270 degrees clockwise, i.e. 90 degrees counterclockwise.','Positive M direction becomes positive N direction.'});

OK=1;

function pf = relpath(path,file)
if length(file)>1 && file(2)==':'
    pf = file;
else
    pf = fullfile(path,file);
end