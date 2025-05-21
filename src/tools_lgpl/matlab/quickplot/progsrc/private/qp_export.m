function cmdargs = qp_export(ExpType,DataState,cmdargs)
%QP_EXPORT Export data set from a QuickPlot support data source.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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

persistent savedir
if ~ischar(savedir)
    savedir='';
elseif ~isempty(savedir)
    if savedir(end)~=filesep
        savedir(end+1)=filesep;
    end
end
T_=1; ST_=2; M_=3; N_=4; K_=5;
scalar=1;

FileInfo=DataState.FI;
Props=DataState.Props;
Domain=DataState.Domain;
Subfield=DataState.SubField;
Selected=DataState.Selected;
Ops=DataState.Ops;
Ops=qp_state_version(Ops);

componentstrings = {'x','y','z'};
if isfield(Props,'MNK') && Props.MNK
    Props.MNK = xyz_or_mnk(Ops,Selected,Props.MNK);
    if Props.MNK>1
        componentstrings = {'m','n','z'};
    end
end

sign=1;
if isequal(ExpType(1),'-'),
    sign=-1;
    ExpType=ExpType(2:end);
end

expType=lower(ExpType);
retrieve='griddata';
AllTAtOnce=0;
MATfile=0;
switch expType
    case {'csv file'}
        AllTAtOnce=1; % code also supports AllTAtOnce=0
        ext='csv';
    case {'csv file (time series)'}
        % assumptions: currently only time series
        AllTAtOnce=1;
        ext='csv';
    case {'grid file','grid file (old format)'}
        % assumptions: 2D, one timestep
        ext='grd';
    case {'netcdf3 file'}
        ext={'*.nc' 'Generic netCDF file (*.nc)'
            '*_net.nc' 'D-Flow FM grid file (*_net.nc)'};
    case {'netcdf4 file'}
        ext={'*.nc' 'Generic netCDF file (*.nc)'
            '*_net.nc' 'D-Flow FM grid file (*_net.nc)'};
    case 'gmsh file'
        ext={'*.msh' 'Gmsh 4.1 ascii file (*.msh)'
            '*.msh' 'Gmsh 4.1 binary file (*.msh)'
            '*.msh' 'Gmsh 2.2 ascii file (*.msh)'
            '*.msh' 'Gmsh 2.2 binary file (*.msh)'};
    case {'quickin file','morsys field file','delft3d-mor field file','box file','simona box file'}
        % assumptions: 2D, one timestep
        % morsys field file: NVal=1
        ext='dep';
    case {'waqview xyz file'}
        ext='xyz';
    case 'polygon file'
        ext='pol';
    case 'tekal file'
        ext='tek';
    case 'tekal file (time series)'
        AllTAtOnce=1;
        ext='tek';
    case 'spline'
        ext='spl';
    case 'landboundary file'
        ext='ldb';
    case 'tecplot file'
        ext={'*.plt' 'Binary Tecplot File (*.plt)'
            '*.dat' 'ASCII Tecplot File (*.dat)'};
    case {'arcview shape', 'geojson file'}
        % assumptions: 2D, one timestep
        switch expType
            case 'arcview shape'
                ext='shp';
            case 'geojson file'
                ext='json';
        end
        if strcmp(Ops.presentationtype,'')
            if Props.NVal==0
                Ops.presentationtype='grid';
            elseif Props.NVal==0.5
                Ops.presentationtype='thin dams';
            end
        end
        switch Ops.presentationtype
            case {'patches','patches with lines','grid','polylines','polygons','edges',''}
                retrieve='gridcelldata';
            case {'markers','values'}
                retrieve='griddata';
            otherwise
                retrieve='';
        end
    case 'sample file'
        % assumptions: one timestep
        ext='xyz';
    case {'stl stereolithography file (ascii)','stl stereolithography file (binary)'}
        % assumptions: one timestep
        ext='stl';
    case {'mat file (v6)','mat file (v7)','mat file (v7.3/hdf5)'}
        % assumptions: one timestep
        MATfile=1;
        AllTAtOnce=1;
        ext='mat';
        if ~isfield(Ops,'presentationtype')
            Ops.presentationtype='dummy';
        end
        switch Ops.presentationtype
            case {'patches','patches with lines'}
                retrieve='gridcelldata';
        end
    otherwise
        ui_message('warning','Export type %s not implemented.',ExpType);
        cmdargs={};
        return
end

if isempty(cmdargs)
    filename = '';
else
    filename = cmdargs{1};
end
if isempty(filename)
    BaseName = Props.Name;
    BaseName = str2file(BaseName);
    if iscell(ext)
        filename = [savedir BaseName];
        while 1
            [f,p,i] = uiputfile(ext, 'Save As', filename);
            if ~ischar(f)
                return
            end
            filename = [p,f];
            if ~wildstrmatch(ext{i,1},f)
                question = sprintf('The specified file name doesn''t match the typical file name filter (%s). Are you sure to use the name: %s?', ext{i,1}, f);
                answer = questdlg(question,'Please confirm ...','Yes','No','Yes');
                if strcmp(answer, 'Yes')
                    break
                end
            else
                break
            end
        end
        cmdargs{2} = ext{i,2};
    else
        [f,p] = uiputfile([savedir BaseName '.' ext], 'Save As');
    end
    if ~ischar(f)
        cmdargs={};
        return
    end
    filename=[p,f];
end
[p,f,e]=fileparts(filename);
savedir=p;
if isempty(e) && ischar(ext)
    filename=cat(2,filename,'.',ext);
end
cmdargs{1} = filename;

SelTim='*';
HasTime = Props.DimFlag(T_);
if HasTime
    if ~AllTAtOnce
        SelTim=Selected{T_};
        if isequal(SelTim,0)
            [Chk,sz]=qp_getdata(FileInfo,Domain,Props,'size');
            if Chk && sz(T_)>0
                SelTim=1:sz(T_);
            end
        end
    end
else % not HasTime
    SelTim = 1;
end

ntim=length(SelTim);
for f=1:ntim
    lastfield = f==ntim;
    if HasTime && ~AllTAtOnce
        Selected{T_}=SelTim(f);
    end
    LocSelected = Selected;
    LocSelected(~Props.DimFlag)=[];

    switch retrieve
        case 'griddata'
            [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'griddata',Subfield{:},LocSelected{:});
        case 'gridcelldata'
            [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'gridcelldata',Subfield{:},LocSelected{:});
        otherwise
            Chk=1;
            data=[];
    end
    data = qp_clipvalues(data, Ops);

    if length(data) == 1 && ...
            (strcmp(Ops.axestype,'Time-Z') || ...
            strcmp(Ops.axestype,'Val-Z'))
        if isfield(data,'ValLocation')
            switch data.ValLocation
                case 'FACE'
                    FNC = data.FaceNodeConnect;
                    missing = isnan(FNC);
                    nNodes = size(missing,2)-sum(missing,2);
                    FNC(missing) = 1;
                    data.X = reshape(data.X(FNC),size(FNC));
                    data.X(missing) = 0;
                    data.X = sum(data.X,2)./nNodes;
                    if isfield(data,'Y')
                        data.Y = reshape(data.Y(FNC),size(FNC));
                        data.Y(missing) = 0;
                        data.Y = sum(data.Y,2)./nNodes;
                    end
                case 'EDGE'
                    ENC = data.EdgeNodeConnect;
                    data.X = reshape(data.X(ENC),size(ENC));
                    data.X = sum(data.X,2)/2;
                    if isfield(data,'Y')
                        data.Y = reshape(data.Y(ENC),size(ENC));
                        data.Y = sum(data.Y,2)/2;
                    end
            end
            data.Geom = 'sSEG';
            for c = {'FaceNodeConnect','EdgeNodeConnect','ValLocation','ZLocation','SEG','XY','XYZ','TRI','EdgeGeometry'}
                s = c{1};
                if isfield(data,s)
                    data = rmfield(data,s);
                end
            end
        end
    end

    if strcmp(Ops.presentationtype,'vector') || ...
            strcmp(Ops.presentationtype,'markers') || ...
            strcmp(Ops.presentationtype,'values') || ...
            strcmp(expType, 'sample file')
        % data = geom2pnt(data);
        for i = 1:length(data)
            if isfield(data(i),'ValLocation')
                if strcmp(data(i).ValLocation,'EDGE')
                    if isfield(data(i),'Geom') && strcmp(data(i).Geom,'sQUAD')
                        data(i).EdgeNodeConnect = [1:length(data(i).X)-1;2:length(data(i).X)]';
                    end
                    data(i).X = mean(data(i).X(data(i).EdgeNodeConnect),2);
                    data(i).Y = mean(data(i).Y(data(i).EdgeNodeConnect),2);
                elseif strcmp(data(i).ValLocation,'FACE')
                    missing = isnan(data(i).FaceNodeConnect);
                    nNodes = size(missing,2)-sum(missing,2);
                    data(i).FaceNodeConnect(missing) = 1;
                    data(i).X = data(i).X(data(i).FaceNodeConnect);
                    data(i).X(missing) = 0;
                    data(i).X = sum(data(i).X,2)./nNodes;
                    data(i).Y = data(i).Y(data(i).FaceNodeConnect);
                    data(i).Y(missing) = 0;
                    data(i).Y = sum(data(i).Y,2)./nNodes;
                end
                data(i).Geom = 'sSEG';
            end
        end
        if length(data)>1
            data(1).X = cat(1,data.X);
            data(1).Y = cat(1,data.Y);
            for cfld = {'Val','CompX','CompY','CompZ'}
                fld = cfld{1};
                if isfield(data,fld)
                    data(1).(fld) = cat(1,data.(fld));
                end
            end
            data(2:end) = [];
        end
        for c = {'FaceNodeConnect','EdgeNodeConnect','EdgeFaceConnect','ValLocation'}
            s = c{1};
            if isfield(data,s)
                data = rmfield(data,s);
            end
        end
    end
    
    if ~Chk
        cmdargs={};
        return
    end
    componentof='';
    if ~isempty(data)
        if ~(isfield(data,'XYZ') && ~MATfile) || (isfield(Ops,'thinningmode') && ~strcmp(Ops.thinningmode,'none'))
            data = qp_thinning(data,Ops);
        end
        %
        VecUnits=data(1).Units;
        if isfield(data,'XComp')
            [data,scalar,component]=computecomponent(data,Ops);
            componentof=[component ' of '];
            if scalar
                Props.NVal=1;
            end
        end
        %
        ValUnits=data(1).Units;
        if isfield(Ops,'units') && strcmp(Ops.units,'**Hide**')
            ValUnits='';
            VecUnits='';
        elseif isfield(data,'XComp')
            % data conversion of vector component already done in computecomponent
        elseif isfield(Ops,'units') && ~isempty(Ops.units) && ~isempty(ValUnits)
            dataX=qp_unitconversion(ValUnits,Ops.units,data);
            if ~ischar(dataX)
                data=dataX;
                dataX=[];
                ValUnits=data(1).Units;
            end
        end
    end
    if isfield(Ops,'units') && strcmp(Ops.units,'**Hide**')
        VecUnits = '';
        ValUnits = '';
    end

    if f==1
        crds={};
        vars={};
        flds={};
        numericValues = false;
        vars{1,end+1}='x coordinate';
        crds{1}='X';
        ypres=isfield(data,'Y') || isfield(data,'XY');
        if ypres
            crds{1,end+1}='Y';
            vars{1,end+1}='y coordinate';
        end
        zpres=isfield(data,'Z');
        if zpres
            crds{1,end+1}='Z';
            vars{1,end+1}='z coordinate';
        end
        nCrd=length(vars);
        if isfield(data,'XComp') && Props.NVal>1
            flds{1,end+1}='XComp';
            vars{1,end+1}=sprintf('%s component of %s',componentstrings{1},Props.Name);
            if ~isempty(VecUnits)
                vars{1,end}=cat(2,vars{1,end},' (',VecUnits,')');
            end
            numericValues = true;
        end
        if isfield(data,'YComp') && Props.NVal>1
            flds{1,end+1}='YComp';
            vars{1,end+1}=sprintf('%s component of %s',componentstrings{2},Props.Name);
            if ~isempty(VecUnits)
                vars{1,end}=cat(2,vars{1,end},' (',VecUnits,')');
            end
            numericValues = true;
        end
        if isfield(data,'ZComp') && Props.NVal>1
            flds{1,end+1}='ZComp';
            vars{1,end+1}=sprintf('%s component of %s',componentstrings{3},Props.Name);
            if ~isempty(VecUnits)
                vars{1,end}=cat(2,vars{1,end},' (',VecUnits,')');
            end
            numericValues = true;
        end
        if isfield(data,'Val')
            flds{1,end+1}='Val';
            vars{1,end+1}=[componentof Props.Name];
            if ~isempty(ValUnits)
                vars{1,end}=cat(2,vars{1,end},' (',ValUnits,')');
            end
            if isnumeric(data(1).Val)
                numericValues = true;
            end
        end
        nVar=length(vars);
        nVal=nVar-nCrd;
    end

    switch expType
        case {'csv file'}
            TIMEFORMAT = '%04d-%02d-%02d %02d:%02d:%02d';
            if f==1
                fid=fopen(filename,'w');
                if fid<0
                    error(['Could not create or open: ',filename])
                end
            end
            %
            if isfield(data,'Time') && ~isempty(data.Time)
                nTim = length(data.Time);
                if nTim==1
                    fprintf(fid,['time,' TIMEFORMAT '\n'],datevec(data.Time));
                end
            else
                nTim = 1;
            end
            Format = [repmat('%s,',1,nCrd+nVal*nTim-1) '%s\n'];
            %
            Vars = [vars(1:nCrd) repmat(vars(nCrd+1:end),1,nTim)];
            fprintf(fid,Format,Vars{:});
            %
            if nTim>1
                Format = [repmat(',',1,nCrd-1) repmat([',' TIMEFORMAT],1,nVal*nTim) '\n'];
                fprintf(fid,Format,datevec(data.Time)');
            end
            %
            if nVal==1 && isequal(flds{1},'Val') && iscellstr(data.Val)
                charOutput = true;
                Format = [repmat('%.15g,',1,nCrd) repmat('%s,',1,nTim-1) '%s\n'];
            else
                charOutput = false;
                Format = [repmat('%.15g,',1,nCrd) repmat('%.15g,',1,nVal*nTim-1) '%.15g\n'];
            end
            Val = zeros(nCrd + nVal*nTim,numel(data.(flds{1}))/nTim);
            for i = 1:nCrd
                crd = data.(crds{i})(:)';
                if numel(crd) == size(Val,2)+1
                    Val(i,:) = (crd(1:end-1) + crd(2:end))/2;
                else
                    Val(i,:) = crd;
                end
                    
            end
            if charOutput
                Val = num2cell(Val);
                if nTim>1
                    for i = 1:nVal
                        Val(nCrd+i:nVal:end,:) = data.(flds{i})(:,:);
                    end
                else
                    for i = 1:nVal
                        Val(nCrd+i,:) = data.(flds{i})(:)';
                    end
                end
                fprintf(fid,Format,Val{:});
            else
                if nTim>1
                    for i = 1:nVal
                        Val(nCrd+i:nVal:end,:) = data.(flds{i})(:,:);
                    end
                else
                    for i = 1:nVal
                        Val(nCrd+i,:) = data.(flds{i})(:)';
                    end
                end
                fprintf(fid,Format,Val);
            end
            %
            if f==ntim
                fclose(fid);
            end
        case {'csv file (time series)','tekal file (time series)'}
            NTim=max(1,length(data.Time));
            switch Props.NVal
                case 1
                    NLoc=prod(size(data.Val))/NTim;
                otherwise
                    NLoc=prod(size(data.XComp))/NTim;
            end
            expdata=zeros(6+nVal*NLoc,NTim);
            sz=[NLoc NTim];
            if isempty(data.Time)
                expdata = zeros(6,1);
            else
                expdata(1:6,:)=transpose(datevec(data.Time*(1+eps)));
                expdata(6,:)=floor(expdata(6,:));
            end
            for fld=1:length(flds)
                FldData=getfield(data,flds{fld});
                expdata(6+fld+(0:NLoc-1)*nVal,:)=reshape(FldData(:,:)',sz);
            end
            %
            for i = 2:NLoc
                len = length(vars);
                vars(end+(1:nVal)) = vars(nCrd+1:nVar);
            end
            %
            [Chk,szvar]=qp_getdata(FileInfo,Domain,Props,'size');
            if isequal(Selected{K_},0)
                Selected{K_} = 1:szvar(K_);
            end
            if isempty(Selected{ST_})
                if ~isempty(Selected{N_})
                    stations = sprintf('m=%i, n=%i',Selected{M_},Selected{N_});
                elseif ~isempty(Selected{M_})
                    stations = sprintf('m=%i',Selected{M_});
                else
                    stations = 'location unknown';
                end
                stations = {stations};
            else
                [Chk,stations]=qp_getdata(FileInfo,Domain,Props,'stations');
                if ~Chk
                    stations = cell(1,szvar(ST_));
                    for i = 1:szvar(ST_)
                        stations{i} = sprintf('station %i',i);
                    end
                end
                if ~isequal(Selected{ST_},0)
                    stations = stations(Selected{ST_});
                end
            end
            %
            i = 0;
            for k = 1:length(Selected{K_})
                for s = 1:length(stations)
                    i = i+1;
                    Loc = sprintf('%s - layer %i',stations{s},Selected{K_}(k));
                    for j = 1:nVal
                        vars{nCrd+(i-1)*nVal+j} = [vars{nCrd+(i-1)*nVal+j} ' - ' Loc];
                    end
                end
            end
            %
            nVar = nCrd+NLoc*nVal;
            switch expType
                case 'csv file (time series)'
                    fid=fopen(filename,'w');
                    if fid<0
                        error(['Could not create or open: ',filename])
                    end
                    Format=cat(2,'%04d-%02d-%02d %02d:%02d:%02d',repmat(', %14.6g',1,size(expdata,1)-6),'\n');
                    %Format=cat(2,'%6.1f',repmat(' %14.6g',1,size(expdata,1)-6),' 999.999\n');
                    if size(expdata,1)-5>256
                        ui_message('error','Number of columns exceeds 256. Too many data columns for Excel!')
                    end
                    fprintf(fid,cat(2,'date and time',repmat(',%s',1,nVar-nCrd),'\n'),vars{nCrd+1:nVar});
                    %dt = datenum(expdata(1,:),expdata(2,:),expdata(3,:),expdata(4,:),expdata(5,:),expdata(6,:));
                    %dt = (dt-datenum(2011,11,4,0,0,0))*24*60;
                    %expdata = cat(1,dt,expdata(7:end,:));
                    Str=sprintf(Format,expdata);
                    Str=strrep(Str,'NaN','');
                    fprintf(fid,'%s',Str);
                    fclose(fid);
                case 'tekal file (time series)'
                    cmnt={};
                    cmnt{1}='column 1 = Date';
                    cmnt{2}='column 2 = Time';
                    for i=nCrd+1:nVar
                        ic=i-nCrd+2;
                        cmnt{ic}=sprintf('column %i = %s',ic,vars{i});
                    end
                    FI.Field(1).Comments=cmnt;
                    FI.Field(1).Name=data.Name;
                    times=expdata(1:6,:)';
                    expdata=expdata([1 4  7:end],:)';
                    expdata(:,1)=times(:,1)*10000+times(:,2)*100+times(:,3);
                    expdata(:,2)=times(:,4)*10000+times(:,5)*100+times(:,6);
                    FI.Field(1).Data=expdata;
                    tekal('write',filename,FI);
            end
        case {'grid file','grid file (old format)'}
            if all(isnan(data.X(end,:)) | data.X(end,:) == data.X(end-1,:)) && ...
                    all(isnan(data.X(:,end)) | data.X(:,end) == data.X(:,end-1))
                G.X = data.X(1:end-1, 1:end-1);
                G.Y = data.Y(1:end-1, 1:end-1);
            else
                G.X = data.X;
                G.Y = data.Y;
            end
            G.CoordSys = 'Cartesian';
            if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                G.CoordSys = 'Spherical';
            end
            switch expType
                case 'grid file'
                    wlgrid('write',filename,G,'anticlockwise');
                case 'grid file (old format)'
                    wlgrid('writeold',filename,G,'anticlockwise');
            end
        case {'netcdf3 file','netcdf4 file'}
            export_netcdf(expType,data,cmdargs{:})
        case {'gmsh file'}
            switch cmdargs{2}
                case 'Gmsh 4.1 ascii file (*.msh)'
                    options = {'version',4.1};
                case 'Gmsh 4.1 binary file (*.msh)'
                    options = {'version',4.1,'binary','l'};
                case 'Gmsh 2.2 ascii file (*.msh)'
                    options = {'version',2.2};
                case 'Gmsh 2.2 binary file (*.msh)'
                    options = {'version',2.2,'binary','l'};
            end
            gmsh('write',cmdargs{1},data,options{:})
        case {'quickin file','morsys field file','delft3d-mor field file','box file','simona box file'}
            for fld=1:length(flds)
                Temp=getfield(data,flds{fld});
                Temp(isnan(Temp))=-999;
                if sign<0
                    expdata(fld).Data=-Temp;
                else
                    expdata(fld).Data=Temp;
                end
            end
            switch expType
                case 'quickin file'
                    wldep('write',filename,'format',Ops.expformat,expdata);
                case 'delft3d-mor field file'
                    wlfdep('write',filename,expdata.Data);
                case 'simona box file'
                    boxfile('write',filename,expdata.Data);
            end
        case {'waqview xyz file'}
            mmax = size(data.Val, 1);
            nmax = size(data.Val, 2);
            [M, N] = ndgrid(1:mmax, 1:nmax);
            id = reshape(1:(mmax*nmax), [mmax, nmax]);
            xyz = [data.X(:), data.Y(:), data.Val(:), M(:), N(:), id(:)]';
            xyz = xyz(:, none(isnan(xyz), 1));
            samples('write', filename, 'format', '%f, %f, %f, %i, %i, %i', 'header', 'x,y,z,m,n,id', xyz)
        case {'tekal file','spline','landboundary file'}
            cmnt = cell(nVar,1);
            for i = 1:nVar
                cmnt{i} = sprintf('column %i = %s',i,vars{i});
            end
            if isfield(data,'XY')
                for i = 1:length(data.XY)
                    xx.Field(i).Comments=cmnt;
                    xx.Field(i).Name=sprintf('F%3.3i',i);
                    xx.Field(i).Data=data.XY{i};
                end
            else
                if isfield(data,'XDam')
                    [x,y] = thindam(data.X,data.Y,data.XDam,data.YDam);
                    expdata = [x, y];
                else
                    expdata=zeros([size(data.X), nVar]);
                    dims(1:ndims(data.X))={':'};
                    locflds=cat(2,crds,flds);
                    for fld=1:length(locflds)
                        expdata(dims{:},fld)=getfield(data,locflds{fld});
                    end
                    switch expType
                        case 'spline'
                            expdata=squeeze(expdata);
                            expdata=expdata(:,1:2); % don't write data or z coordinates ro spline file
                            %
                            % the following line initially made sense when skipping
                            % over small gaps in grid lines, but it doesn't work in
                            % the cases of (a) big gaps in grid lines and (b) lines
                            % from shape files.
                            %
                            %expdata(any(isnan(expdata),2),:)=[];
                        case 'landboundary file'
                            expdata=squeeze(expdata);
                            expdata(any(isnan(expdata(:,1:2)),2),:)=999.999;
                        otherwise
                            expdata(isnan(expdata))=-999;
                    end
                end
                if isfield(data,'Time') && ~isempty(data.Time) && ~isnan(data.Time)
                    cmnt={sprintf('time     = %s',datestr(data.Time,0)),cmnt{:}};
                end
                xx.Field(f).Comments=cmnt;
                xx.Field(f).Name=sprintf('F%3.3i',f);
                xx.Field(f).Data=expdata;
            end
            if lastfield
                if strcmp(expType,'spline')
                    landboundary('write',filename,{xx.Field.Data},'dosplit','-1','format','S%3.3i');
                else
                    tekal('write',filename,xx);
                end
            end
        case 'tecplot file'
            if f==1
                vars{1,end+1}='Active';
                xx.Variables=vars;
                nVar=length(vars);
            end
            expdata=zeros([size(data.X) nVar]);
            dims(1:ndims(data.X))={':'};
            %
            Act=1;
            i=0;
            Flds=cat(2,crds,flds);
            for fldi=1:length(Flds)
                if isfield(data,Flds{fldi})
                    tmp=getfield(data,Flds{fldi});
                    NaNs=isnan(tmp);
                    Act=Act & ~NaNs;
                    tmp(NaNs)=min(tmp(:));
                    i=i+1;
                    expdata(dims{:},i)=tmp;
                end
            end
            i=i+1;
            expdata(dims{:},i)=Act;
            expdata(isnan(expdata))=-999;
            %
            xx.Zone(f).Title=datestr(data.Time,0);
            xx.Zone(f).Data=expdata;
            if lastfield
                tecplot('write',filename,xx);
            end
        case {'arcview shape','polygon file','geojson file'}
            if isfield(data,'XDam')
                Ops.presentationtype = 'thin dams';
            end
            switch Ops.presentationtype
                case {'patches','patches with lines','markers','values','grid','polylines','polygons','edges',''}
                    xy=[];
                    if isfield(Props,'Geom') && (strcmp(Props.Geom,'POLYL') || strcmp(Props.Geom,'POLYG'))
                        % for now only single partitions supported ...
                        vals={};
                        if isfield(data,'XY')
                            xy = data.XY;
                            if isfield(data,'Val')
                                vals={data.Val};
                            end
                        else
                            vNaN=isnan(data.X);
                            if any(vNaN)
                                bs=findseries(~vNaN);
                            else
                                bs=[1 length(vNaN)];
                            end
                            xy={};
                            for i=size(bs,1):-1:1
                                xy{i}=[data.X(bs(i,1):bs(i,2)) data.Y(bs(i,1):bs(i,2))];
                            end
                            if isfield(data,'Val')
                                vals={data.Val(bs(:,1))};
                            end
                        end
                        if strcmp(Ops.facecolour,'none')
                           shp_type = 'polyline';
                        else
                           shp_type = 'polygon';
                        end
                        featureLabels = {};
                        for i = length(xy):-1:1
                            featureLabels{i} = sprintf('polygon %i',i);
                        end
                        switch expType
                            case 'arcview shape'
                                shapewrite(filename,shp_type,xy,vals{:})
                            case 'geojson file'
                                geojson('write', filename, 'polygon', featureLabels, xy)
                            case 'polygon file'
                                DATA = [];
                                for i = length(xy):-1:1
                                    DATA.Field(i).Name = featureLabels{i};
                                    DATA.Field(i).Data = xy{i};
                                end
                                tekal('write',filename,DATA);
                        end
                    else
                        CV = [];
                        FV = [];
                        XV = [];
                        XY = [];
                        RM = logical([]);
                        for d = 1:length(data)
                            cv = [];
                            fv = [];
                            xv = [];
                            xy = [];
                            rm = [];
                            shp_type = 'polygon';
                            if isfield(Props,'Geom') && strncmp(Props.Geom,'UGRID',5)
                                if Props.NVal==0 && isfield(data,'FaceNodeConnect')
                                    Props.Geom='UGRID2D-FACE';
                                    data.ValLocation='FACE';
                                end
                                switch Ops.presentationtype
                                    case {'markers','values'}
                                        retrieve='griddata';
                                        xy=[data(d).X data(d).Y];
                                        rm=[];
                                    otherwise
                                        switch Props.Geom(max(strfind(Props.Geom,'-'))+1:end)
                                            case 'NODE'
                                                retrieve='griddata';
                                                xy=[data(d).X data(d).Y];
                                                rm=[];
                                            case 'EDGE'
                                                xv=[data(d).X data(d).Y];
                                                fv=data(d).EdgeNodeConnect;
                                                shp_type = 'polyline';
                                                rm=[];
                                            case 'FACE'
                                                xv=[data(d).X data(d).Y];
                                                fv=data(d).FaceNodeConnect;
                                                rm=[];
                                            otherwise
                                                error('Unsupported geometry ''%s''.',Props.Geom);
                                        end
                                end
                            elseif isfield(Props,'Tri') && Props.Tri
                                if strcmp(retrieve,'gridcelldata')
                                    xv=data(d).XYZ(1,:,1,1:2);
                                    xv=reshape(xv,[size(xv,2) 2]);
                                    fv=data(d).TRI;
                                    rm=[];
                                else
                                    xy=[data(d).X(:) data(d).Y(:)];
                                    rm=any(isnan(xy),2);
                                    xy(rm,:)=[];
                                end
                            else
                                data(d).X=data(d).X(:,:,1); % remove 3rd dimension when appropriate
                                data(d).Y=data(d).Y(:,:,1); % remove 3rd dimension when appropriate
                                if strcmp(retrieve,'gridcelldata')
                                    faces=reshape(1:prod(size(data(d).X)),size(data(d).X));
                                    faces=faces(1:end-1,1:end-1);
                                    xv=[data(d).X(:) data(d).Y(:)];
                                    fv=[faces(:) faces(:)+1 faces(:)+size(data(d).X,1)+1 faces(:)+size(data(d).X,1)];
                                    xx=data(d).X(fv);
                                    yy=data(d).Y(fv);
                                    rm=any(isnan(xx),2)|any(isnan(yy),2);
                                    fv(rm,:)=[];
                                else
                                    xy=[data(d).X(:) data(d).Y(:)];
                                    rm=any(isnan(xy),2);
                                    xy(rm,:)=[];
                                end
                            end
                            cv=[];
                            cLabels={};
                            if isfield(data,'XComp')
                                cv=[data(d).XComp(:)];
                                cLabels{end+1}='X comp.';
                            end
                            if isfield(data,'YComp')
                                cv=[cv data(d).YComp(:)];
                                cLabels{end+1}='Y comp.';
                            end
                            if isfield(data,'ZComp')
                                cv=[cv data(d).ZComp(:)];
                                cLabels{end+1}='Z comp.';
                            end
                            if isfield(data,'Val')
                                cv=[cv data(d).Val(:)];
                                cLabels{end+1}=componentof;
                            end
                            %
                            if ~isempty(xv)
                                CV = cat(1,CV,cv);
                                FV = cat(1,FV,fv + size(XV,1));
                                XV = cat(1,XV,xv);
                            else
                                XY = cat(1,XY,xy);
                            end
                            RM = cat(1,RM,rm);
                        end
                        if isempty(CV)
                            %
                            % grid only ... export M, N coordinate?
                            %
                            CV = {};
                        else
                            cLabels = strrep(cLabels,' ','_');
                            %
                            CV(RM,:) = [];
                            RM = any(isnan(CV),2);
                            CV(RM,:) = [];
                            if isempty(XY)
                                FV(RM,:) = [];
                            else
                                XY(RM,:) = [];
                            end
                            CV = {cLabels,CV};
                        end
                        if strcmp(retrieve,'gridcelldata')
                            %
                            % make sure that polygons are stored clockwise ...
                            % just checking the first one
                            %
                            if strcmp(shp_type,'polygon') && ~any(isnan(FV(1,:))) && clockwise(XV(FV(1,:),1),XV(FV(1,:),2))<0
                                % a simple fv=fliplr(fv) only works if all
                                % patches have the same number of corner
                                % nodes, so no fill NaNs. To be generic we
                                % have to loop:
                                nv = size(FV,2) - sum(isnan(FV),2);
                                for i = 1:size(FV,1)
                                    % first nv indices should not be NaN
                                    FV(i,1:nv(i)) = FV(i,nv(i):-1:1);
                                end
                            end
                            shapewrite(filename,shp_type,XV,FV,CV{:})
                        else
                            shapewrite(filename,'point',XY,CV{:})
                        end
                    end
                case {'vector','vector (split x,y)','vector (split m,n)','contour lines','coloured contour lines','contour patches','contour patches with lines','thin dams'}
                    if isequal(Ops.presentationtype,'contour lines')
                        Ops.presentationtype='coloured contour lines';
                    end
                    
                    TempFg=figure('visible','off');
                    TempAx=axes('parent',TempFg);
                    PS=DataState;
                    PS.Ops=Ops;
                    PS.Parent=TempAx;
                    PS.Handles=[];
                    PS.Stations={};
                    [hNew,Error,Info]=qp_plot(PS);
                    %
                    % process handles in reverse order for correct overlay effect in case of patches
                    %
                    if iscell(hNew)
                        hNew=cat(1,hNew{:});
                    end
                    hNew0=hNew(1);
                    hNew=hNew(end:-1:1);
                    switch Ops.presentationtype
                        case 'coloured contour lines'
                            if length(hNew) == 1
                                xy = {get(hNew,'vertices')};
                                cv = {get(hNew,'facevertexcdata')};
                            else
                                xy = get(hNew,'vertices');
                                cv = get(hNew,'facevertexcdata');
                            end
                            for i = 1:length(xy)
                                xy{i} = xy{i}(:,1:2);
                            end
                            UD=get(hNew0,'userdata');
                            Thresholds=UD.XInfo.Thresholds;
                            for i=length(xy):-1:1
                                if isempty(xy{i})
                                    xy(i)=[];
                                    cv(i)=[];
                                    continue
                                elseif isnan(xy{i}(end,1))
                                    xy{i}=xy{i}(1:end-1,:);
                                end
                                cv{i}=Thresholds(cv{i}(1));
                            end
                            %
                            [xy,cv] = stitch_segments(xy,cv);
                            %
                            cv=cat(1,cv{:});
                            xy(cellfun('isempty',xy)) = [];
                            cLabels={'Value'};
                            shapewrite(filename,'polyline',xy,cLabels,cv)
                        case 'thin dams'
                            x=get(hNew,'xdata');
                            y=get(hNew,'ydata');
                            switch get(hNew,'type')
                                case 'line'
                                    xy=cell(1,size(x,2)/3);
                                    for i=1:length(xy)
                                        xy{i}=[x((i-1)*3+(1:2));y((i-1)*3+(1:2))]';
                                    end
                                    shapewrite(filename,'polyline',xy)
                                case 'surface'
                                    xy=cell(1,size(x,2)/3);
                                    for i=1:length(xy)
                                        xy{i}=[x(:,(i-1)*3+1) y(:,(i-1)*3+1)];
                                    end
                                    cv=get(hNew,'cdata');
                                    cv=cv(1,1:3:end)';
                                    shapewrite(filename,'polyline',xy,{'Value'},cv)
                            end
                        case {'contour patches','contour patches with lines'}
                            hNew = hNew(strcmp(get(hNew,'type'),'patch'));
                            xy=get(hNew,'vertices');
                            fc=get(hNew,'faces');
                            cv=get(hNew,'facevertexcdata');
                            minmax = zeros(length(hNew),2);
                            for i = 1:length(hNew)
                                minmax(i,:) = [getappdata(hNew(i),'MinThreshold') getappdata(hNew(i),'MaxThreshold')];
                            end
                            %
                            % for tricontourf data: stitch triangles
                            % for contourf data: clip polygons that overlap
                            [xy,cLabels,cv] = process_polygons(xy,fc,cv,minmax);
                            %
                            featureLabels = {};
                            remove = false(size(xy));
                            for i = length(xy):-1:1
                                if isnan(cv(i,1))
                                    if isnan(cv(i,2))
                                        remove(i) = true;
                                    else
                                        featureLabels{i} = sprintf('values smaller than %g',cv(i,2));
                                    end
                                elseif isnan(cv(i,2))
                                    featureLabels{i} = sprintf('values larger than %g',cv(i,1));
                                else
                                    featureLabels{i} = sprintf('values between %g and %g',cv(i,:));
                                end
                            end
                            if any(remove)
                                xy(remove) = [];
                                cv(remove,:) = [];
                                featureLabels(remove) = [];
                            end
                            switch expType
                                case 'arcview shape'
                                    shapewrite(filename,xy,cLabels,cv)
                                case 'geojson file'
                                    properties = [];
                                    for p = 1:length(cLabels)
                                        properties.(cLabels{p}) = cv(:,p);
                                    end
                                    geojson('write', filename, 'polygon', featureLabels, xy, properties)
                                case 'polygon file'
                                    DATA = [];
                                    for i = length(xy):-1:1
                                        DATA.Field(i).Name = featureLabels{i};
                                        DATA.Field(i).Data = xy{i};
                                    end
                                    tekal('write',filename,DATA);
                            end
                        case {'vector','vector (split x,y)','vector (split m,n)'}
                            hascolor = strcmp(get(hNew(end),'type'),'patch');
                            if hascolor
                                vIndex = find(strcmp('patch',get(hNew,'type')))';
                            else
                                vIndex = 1:length(hNew);
                                vIndex(1:3:end) = [];
                            end
                            %
                            N1=0;
                            for v = vIndex
                                if hascolor
                                    N1 = N1 + size(get(hNew(v),'xdata'),1);
                                else
                                    N1 = N1 + length(get(hNew(v),'xdata'));
                                end
                            end
                            %
                            xy=zeros(N1,2);
                            if hascolor
                                cv = zeros(N1,1);
                            end
                            N1=0;
                            for v = vIndex
                                x01 = get(hNew(v),'xdata');
                                y01 = get(hNew(v),'ydata');
                                if hascolor
                                    n1 = size(x01,1);
                                    xy(N1+(1:n1),1)=x01(:,1);
                                    xy(N1+(1:n1),2)=y01(:,1);
                                    %
                                    v1=get(hNew(v),'cdata');
                                    cv(N1+(1:n1),1)=v1(:,1);
                                else
                                    n1 = length(x01);
                                    xy(N1+(1:n1),1)=x01;
                                    xy(N1+(1:n1),2)=y01;
                                end
                                N1 = N1+n1;
                            end
                            seps=[0;find(isnan(xy(:,1)))];
                            %
                            n = seps(2:end)-seps(1:end-1)-1;
                            xy_cell=cell(sum(n>1),1);
                            if hascolor
                                cvr=zeros(sum(n>1),1);
                            end
                            for i=1:length(n)
                                if n(i)>1
                                    xy_cell{i}=xy(seps(i)+(1:n(i)),:);
                                    if hascolor
                                        cvr(i)=cv(seps(i)+1);
                                    end
                                end
                            end
                            %
                            iEmpty = cellfun('isempty',xy_cell);
                            xy_cell(iEmpty)=[];
                            cvr(iEmpty)=[];
                            if hascolor
                                UD=get(hNew(end),'userdata');
                                values={{UD.PlotState.Ops.vectorcolour} cvr};
                            else
                                values={};
                            end
                            shapewrite(filename,'polyline',xy_cell,values{:});
                    end
                    delete(TempFg);
                otherwise
                    error('Unknown presentationtype "%s" during export of ArcView Shape',Ops.presentationtype)
            end
        case 'sample file'
            writeHeader = true;
            x=0; y=0; z=0; sz=[];
            if isfield(data,'X')
                x=1;
                sz=size(data.X);
            end
            if isfield(data,'Y')
                y=1;
                sz=size(data.Y);
            end
            if isfield(data,'Z')
                z=1;
                sz=size(data.Z);
            end
            if isempty(sz)
                if Props.NVal==1
                    sz=size(data.Val);
                elseif Props.NVal>1
                    sz=size(data.XComp);
                end
            end
            xyz=x+y+z;
            if numericValues
                nVarNum = nVar;
            else
                nVarNum = nVar - nVal;
            end
            expdata=zeros([nVarNum prod(sz)]);
            if x
                expdata(1,:)=data.X(:)';
            end
            if y
                expdata(x+1,:)=data.Y(:)';
            end
            if z
                expdata(x+y+1,:)=data.Z(:)';
            end
            if isfield(data,'XComp') && Props.NVal>1
                xyz=xyz+1;
                expdata(xyz,:)=data.XComp(:)';
            end
            if isfield(data,'YComp') && Props.NVal>1
                xyz=xyz+1;
                expdata(xyz,:)=data.YComp(:)';
            end
            if isfield(data,'ZComp') && Props.NVal>1
                xyz=xyz+1;
                expdata(xyz,:)=data.ZComp(:)';
            end
            if isfield(data,'Val') && numericValues
                xyz=xyz+1;
                expdata(xyz,:)=data.Val(:)';
            end
            excludePoints = any(isnan(expdata),1);
            expdata(:,excludePoints) = [];
            fid=fopen(filename,'w');
            if fid<0
                error(['Could not create or open: ',filename])
            end
            if writeHeader
                fprintf(fid,'"%s" ',vars{:});
            end
            fprintf(fid,'\n');
            if numericValues
                Format=repmat(' %14.6f',1,size(expdata,1));
                Format=[Format(2:end) '\n'];
                fprintf(fid,Format,expdata);
            else
                Format=repmat(' %14.6f',1,size(expdata,1));
                Format=[Format(2:end) ' ''%s'' \n'];
                expcell = num2cell(expdata);
                expcell(end+1,:) = data.Val(~excludePoints);
                expcell(end,:) = strrep(expcell(end,:),'''','''''');
                fprintf(fid,Format,expcell{:});
            end
            fclose(fid);
        case {'stl stereolithography file (ascii)','stl stereolithography file (binary)'}
            writeAscii = strcmp(expType(strfind(expType,'('):end), '(ascii)');
            xyz = squeeze(data.XYZ);
            if size(xyz,2)==2
                xyz(:,3) = data.Val(:);
            end
            if writeAscii
                stl('write_ascii',filename,data.Name,data.TRI,xyz)
            else
                stl('write',filename,data.Name,data.TRI,xyz)
            end
        case {'mat file','mat file (v6)','mat file (v7)','mat file (v7.3/hdf5)'}
            saveops={};
            switch expType
                case 'mat file (v6)'
                    if matlabversionnumber>=7
                        saveops={'-v6'};
                    end
                case 'mat file (v7)'
                    if matlabversionnumber>=7.3
                        saveops={'-v7'};
                    end
                case 'mat file (v7.3/hdf5)'
                    saveops={'-v7.3'};
            end
            if scalar && isfield(data,'XComp')
                data.Name = [data.Name ', ' Ops.vectorcomponent];
                if isfield(data,'XComp')
                    data = rmfield(data,'XComp');
                end
                if isfield(data,'YComp')
                    data = rmfield(data,'YComp');
                end
                if isfield(data,'ZComp')
                    data = rmfield(data,'ZComp');
                end
            end
            save(filename,'data',saveops{:});
    end
end

function export_netcdf(expType,data,filename,varargin)
if nargin>3 && strcmp(varargin{1},'D-Flow FM grid file (*_net.nc)')
    writeNetFile = true;
else
    writeNetFile = false;
end
mode = netcdf.getConstant('CLOBBER');
switch expType
    case 'netcdf4 file'
        mode = bitor(mode,netcdf.getConstant('NETCDF4'));
        mode = bitor(mode,netcdf.getConstant('CLASSIC_MODEL'));
end
ncid = netcdf.create(filename,mode);
ui_message('warning','The netCDF export option is still under development.')
try
    for g = 1:length(data)
        if length(data)>1
            prefix = sprintf('DATA%i_',g);
        else
            prefix = '';
        end
        DATA = data(g);
        %
        if isfield(DATA,'Val') && isfield(DATA, 'ValLocation')
            switch DATA.ValLocation
                case 'FACE'
                    % remove any faces with NaN values
                    if isfield(DATA,'FaceNodeConnect')
                        fMask = isnan(DATA.Val);
                        DATA.FaceNodeConnect(fMask,:) = [];
                        DATA.Val(fMask) = [];
                    end
                    % if there is an EdgeNodeConnect, we have to filter out
                    % the edges that only belonged to the faces removed.
                    % Quick solution: remove the EdgeNodeConnect and
                    % regenerate.
                    if isfield(DATA,'EdgeNodeConnect')
                        DATA = rmfield(DATA,'EdgeNodeConnect');
                    end
                case 'EDGE'
                    if isfield(DATA,'FaceNodeConnect')
                        % to remove edges, we have to identify which faces
                        % are bounded by those edges.
                    elseif isfield(DATA,'EdgeNodeConnect')
                        % just remove the edges.
                        eMask = any(ismember(DATA.FaceNodeConnect,nMask),2);
                        DATA.EdgeNodeConnect(eMask,:) = [];
                    end
                case 'NODE'
                    % remove all faces/edges connected to nodes with NaN
                    % values
                    nMask = find(isnan(DATA.Val));
                    if isfield(DATA,'FaceNodeConnect')
                        fMask = any(ismember(DATA.FaceNodeConnect,nMask),2);
                        DATA.FaceNodeConnect(fMask,:) = [];
                    end
                    if isfield(DATA,'EdgeNodeConnect')
                        eMask = any(ismember(DATA.FaceNodeConnect,nMask),2);
                        DATA.EdgeNodeConnect(eMask,:) = [];
                    end
            end
        end
        if isfield(DATA,'FaceNodeConnect')
            allNodes = 1:length(DATA.X);
            keepNodes = ismember(allNodes,DATA.FaceNodeConnect);
            if ~all(keepNodes)
                DATA.X = DATA.X(keepNodes);
                DATA.Y = DATA.Y(keepNodes);
                numNodes = length(DATA.X);
                %
                renum = zeros(1,numNodes);
                renum(keepNodes) = 1:numNodes;
                Mask = isnan(DATA.FaceNodeConnect);
                DATA.FaceNodeConnect(Mask) = 1;
                DATA.FaceNodeConnect = renum(DATA.FaceNodeConnect);
                DATA.FaceNodeConnect(Mask) = NaN;
                %
                if isfield(DATA,'EdgeNodeConnect')
                    DATA.EdgeNodeConnect = renum(DATA.EdgeNodeConnect);
                end
                %
                if isfield(DATA,'Val') && isfield(DATA, 'ValLocation')
                    DATA.Val = DATA.Val(keepNodes);
                end
            end
            % save as UGRID
            % EdgeNodeConnect is required for a D-Flow FM mesh or network file
            if writeNetFile && ~isfield(DATA,'EdgeNodeConnect')
                DATA.EdgeNodeConnect = fnc2enc(DATA.FaceNodeConnect);
            end
            nNodes = [prefix 'nNodes'];
            nEdges = [prefix 'nEdges'];
            nFaces = [prefix 'nFaces'];
            maxNodesPerFace = [prefix 'maxNodesPerFace'];
            dim_nNodes = netcdf.defDim(ncid,nNodes,length(DATA.X));
            dim_nFaces = netcdf.defDim(ncid,nFaces,size(DATA.FaceNodeConnect,1));
            dim_maxNodesPerFace = netcdf.defDim(ncid,maxNodesPerFace,size(DATA.FaceNodeConnect,2));
            if isfield(DATA,'EdgeNodeConnect')
                dim_nEdges = netcdf.defDim(ncid,nEdges,size(DATA.EdgeNodeConnect,1));
                dim_2 = netcdf.defDim(ncid,'TWO',2);
            end
            %
            X = [prefix 'NodeX'];
            var_X = netcdf.defVar(ncid,X,'double',dim_nNodes);
            if isfield(DATA,'XUnits') && strcmp(DATA.XUnits,'deg')
                netcdf.putAtt(ncid,var_X,'standard_name','longitude')
                netcdf.putAtt(ncid,var_X,'units','degrees_east')
            else
                netcdf.putAtt(ncid,var_X,'standard_name','projection_x_coordinate')
                if isfield(DATA,'XUnits') && ~isempty(DATA.XUnits)
                    netcdf.putAtt(ncid,var_X,'units',DATA.XUnits)
                end
            end
            %
            Y = [prefix 'NodeY'];
            var_Y = netcdf.defVar(ncid,Y,'double',dim_nNodes);
            if isfield(DATA,'YUnits') && strcmp(DATA.YUnits,'deg')
                netcdf.putAtt(ncid,var_Y,'standard_name','latitude');
                netcdf.putAtt(ncid,var_Y,'units','degrees_north')
            else
                netcdf.putAtt(ncid,var_Y,'standard_name','projection_y_coordinate')
                if isfield(DATA,'YUnits') && ~isempty(DATA.YUnits)
                    netcdf.putAtt(ncid,var_Y,'units',DATA.YUnits)
                end
            end
            %
            FaceNodeConnect = [prefix 'FaceNodeConnect'];
            var_FaceNodeConnect = netcdf.defVar(ncid,FaceNodeConnect,'int',[dim_maxNodesPerFace dim_nFaces]);
            %netcdf.defVarFill(ncid,var_FaceNodeConnect,false,-999);
            netcdf.putAtt(ncid,var_FaceNodeConnect,'cf_role','face_node_connectivity')
            netcdf.putAtt(ncid,var_FaceNodeConnect,'start_index',1)
            %
            if isfield(DATA,'EdgeNodeConnect')
                EdgeNodeConnect = [prefix 'EdgeNodeConnect'];
                var_EdgeNodeConnect = netcdf.defVar(ncid,EdgeNodeConnect,'int',[dim_2 dim_nEdges]);
                %netcdf.defVarFill(ncid,var_EdgeNodeConnect,false,-999);
                netcdf.putAtt(ncid,var_EdgeNodeConnect,'cf_role','edge_node_connectivity')
                netcdf.putAtt(ncid,var_EdgeNodeConnect,'start_index',1)
            end
            %
            Mesh = [prefix 'Mesh'];
            var_Mesh = netcdf.defVar(ncid,Mesh,'int',[]);
            netcdf.putAtt(ncid,var_Mesh,'cf_role','mesh_topology')
            netcdf.putAtt(ncid,var_Mesh,'topology_dimension',2)
            netcdf.putAtt(ncid,var_Mesh,'node_coordinates',[X ' ' Y])
            netcdf.putAtt(ncid,var_Mesh,'face_node_connectivity',FaceNodeConnect)
            if isfield(DATA,'EdgeNodeConnect')
                netcdf.putAtt(ncid,var_Mesh,'edge_node_connectivity',EdgeNodeConnect)
            end
            netcdf.putAtt(ncid,var_Mesh,'node_dimension',nNodes)
            if isfield(DATA,'EdgeNodeConnect')
                netcdf.putAtt(ncid,var_Mesh,'edge_dimension',nEdges)
            end
            netcdf.putAtt(ncid,var_Mesh,'face_dimension',nFaces)
            %
            if isfield(DATA,'Val') && isfield(DATA, 'ValLocation')
                switch DATA.ValLocation
                    case 'NODE'
                        Val = [prefix 'NodeVal'];
                        dimVal = dim_nNodes;
                    case 'EDGE'
                        Val = [prefix 'EdgeVal'];
                        dimVal = dim_nEdges;
                    case 'FACE'
                        Val = [prefix 'FaceVal'];
                        dimVal = dim_nFaces;
                end
                var_Val = netcdf.defVar(ncid,Val,'double',dimVal);
                netcdf.putAtt(ncid,var_Val,'long_name',DATA.Name);
                if isfield(DATA,'Units') && ~isempty(DATA.Units)
                    netcdf.putAtt(ncid,var_Val,'units',DATA.Units);
                end
            end
            %
            globalId = netcdf.getConstant('GLOBAL');
            netcdf.putAtt(ncid,globalId,'Conventions','UGRID-1.0');
            %
            netcdf.endDef(ncid)
            %
            netcdf.putVar(ncid,var_X,DATA.X)
            netcdf.putVar(ncid,var_Y,DATA.Y)
            if isfield(DATA,'EdgeNodeConnect')
                netcdf.putVar(ncid,var_EdgeNodeConnect,DATA.EdgeNodeConnect')
            end
            netcdf.putVar(ncid,var_FaceNodeConnect,DATA.FaceNodeConnect')
            if isfield(DATA,'Val') && isfield(DATA, 'ValLocation')
                netcdf.putVar(ncid,var_Val,DATA.Val);
            end
        else
            error('Exporting this data set to netCDF not yet supported')
        end
    end
    netcdf.close(ncid)
catch Exception1
    try
        netcdf.close(ncid)
    catch
        % ignore or append
    end
    rethrow(Exception1)
end
