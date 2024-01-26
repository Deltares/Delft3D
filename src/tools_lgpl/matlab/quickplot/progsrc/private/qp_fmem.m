function [FI,FileName,Tp,Otherargs]=qp_fmem(cmd,varargin)
%QP_FMEM Routine for opening data files.
%   [FileInfo,FileName,FileType,Otherargs]=QP_FMEM('open',FilterSpec)

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

lasttp=qp_settings('LastFileType','');

FI=[];
FileName='';
Tp='';
FileFromCall=0;
Otherargs={};
switch cmd
    case {'open','openldb','opennew','openurl'}
        if strcmp(cmd,'opennew') %|| strcmp(cmd,'openldb')
            targetdir=varargin{1};
            filterspec='';
        elseif nargin>1
            filterspec=varargin{1};
            if exist(filterspec,'dir')
                targetdir = filterspec;
                filterspec = '';
            else
                targetdir=fileparts(filterspec);
            end
        else
            filterspec='';
            targetdir=pwd;
        end
        DoDS=0;
        if strcmp(cmd,'openurl')
            DoDS=1;
        elseif length(filterspec)>8 && (isequal(lower(filterspec(1:7)),'http://') || isequal(lower(filterspec(1:8)),'https://'))
            DoDS=1;
        elseif isempty(targetdir) || exist(targetdir)~=7
            targetdir=pwd;
        end
        filtertbl={};
        if DoDS
            if isempty(filterspec)
                % ask user for link
                FileName=uigeturl;
                if ~ischar(FileName)
                    return
                end
            else
                FileName=filterspec;
            end
            FileFromCall=1;
        elseif ~isempty(filterspec) && exist(filterspec)==2
            FileName=filterspec;
            FileFromCall=1;
        else
            if ~isempty(filterspec)
                [dummypath,tmpfn,tmpext]=fileparts(filterspec);
                filterspec=[tmpfn,tmpext];
            else
                if strcmp(cmd,'openldb')
                    filtertbl = qp_filefilters('files-with-lines');
                else
                    filtertbl = qp_filefilters('selected+');
                    [dum,Reorder] = sort(filtertbl(:,2));
                    filtertbl=cat(1,filtertbl(Reorder,:),{'*.*','All Files' 'anyfile'});
                end
                if matlabversionnumber<6
                    filterspec='*.dat';
                else
                    if ~isempty(lasttp)
                        for ifilter = size(filtertbl,1):-1:1
                            if iscell(filtertbl{ifilter,3})
                                if any(strcmp(lasttp,filtertbl{ifilter,3}))
                                    break
                                end
                            elseif strcmp(lasttp,filtertbl{ifilter,3})
                                break
                            end
                        end
                        if ifilter>1
                            filtertbl=filtertbl([ifilter 1:end],:);
                        end
                    else
                    end
                    filterspec=filtertbl(:,1:2);
                end
            end
            currentdir=pwd;
            cd(targetdir);
            [fn,pn]=uigetfile(filterspec,'Select data file to open ...');
            cd(currentdir);
            if ~ischar(fn)
                return
            end
            FileName=[pn fn];
        end
        %autodetect intelligence ...
        FI=[];
        try_next='nefis';
        [pn,fn,en]=fileparts(FileName);
        
        fn_ = lower(fn);
        fen_ = lower([fn en]);
        if DoDS
            try_next='NetCDF';
        elseif strncmp('sds-',fn_,4)
            try_next='waquasds';
        elseif strncmp('morf',fn_,4)
            try_next='morf';
        elseif strncmp('bagdpt',fn_,6)
            try_next='bagdpt';
        elseif strncmp('fourier',fn_,7)
            try_next='tekal';
        elseif strcmp('gcmplt',fn_) || strcmp('gcmtsr',fn_)
            try_next='ecomsed-binary';
        elseif strcmp('network.ntw',fen_) || strcmp('deptop.1',fen_)
            try_next='sobek1d';
        else
            switch lower(en)
                case {'.mdf',',mdw','.md1d','.mdu'}
                    try_next='md*-file';
                case {'.bln'}
                    try_next='surfer';
                case {'.grd','.rgf'}
                    try_next='wlgrid';
                case {'.n','.e','.node','.ele'}
                    try_next='nodelemesh';
                case {'.14','.gr3'}
                    try_next='adcircmesh';
                case {'.2dm'}
                    try_next='smsmesh';
                case {'.mesh'}
                    try_next='mikemesh';
                case {'.shy'}
                    try_next='SHYFEM mesh';
                case {'.gem'}
                    try_next='geomesh';
                case {'.msh'}
                    try_next='gmsh';
                case {'.mat','.fig'}
                    try_next='matlab';
                case {'.map'}
                    try_next='pcraster';
                case {'.his','.plo','.psf','.bal'}
                    try_next='delwaqbin';
                case {'.arc','.amc','.amd','.amh','.amp','.amt','.amu','.amv'}
                    try_next='arcgrid';
                case {'.spw','.wnd'}
                    try_next='asciiwind';
                case {'.inc','.crs','.bin'}
                    try_next='fls';
                case {'.grb','.grib','.grib1','.grib2'}
                    try_next='grib';
                case {'.tek','.ann','.ldb','.pol','.spl','.tka','.tkp','.tkf','.pli','.pliz'}
                    try_next='tekal';
                case {'.dxf'}
                    try_next='AutoCAD DXF';
                case {'.geojson'}
                    try_next='GeoJSON';
                case {'.xyz','.csv'}
                    try_next='samples';
                case {'.seq'}
                    try_next='aukepc';
                case {'.bct'}
                    try_next='bct';
                case {'.dmp'}
                    try_next='CFX dmp';
                case {'.pst','.stu'}
                    try_next='JSPost';
                case {'.jpg','.jpeg','.bmp','.tif','.tiff','.png','.pcx','.xwd'}
                    try_next='bitmap';
                case {'.avi','.mp4'}
                    try_next='video';
                case {'.slf','.out','.res'}
                    try_next='telemac';
                case '.bna'
                    try_next='BNA File';
                case '.gen'
                    try_next='ArcInfoUngenerate';
                case '.nc'
                    try_next='NetCDF';
                case {'.hdf','.hdf5','.xmdf'}
                    try_next='HDF5';
                case {'.fun','.daf'}
                    try_next='unibest';
                case {'.shx','.shp'}
                    try_next='shape';
                case {'.sma'}
                    try_next='shipma';
                case {'.sp1','.sp2'}
                    try_next='SWAN spectral';
                case {'.mdm'}
                    try_next='morf';
                case {'.tim'}
                    try_next='DelwaqTimFile';
                case {'.bil','.hdr'}
                    try_next='bil/hdr';
                case {'.noos'}
                    try_next='NOOS time series';
                case {'.wml'}
                    try_next='WaterML2';
                otherwise
                    if strncmp('hot',fn_,3)
                        try_next='SWAN spectral';
                    else
                        try_next='nefis';
                    end
            end
        end
        FileName = absfullfile(FileName);
        
        %collect the IDs of all file types for the following switch block
        if ~DoDS
            types_to_check = qp_filefilters('all'); %filtertable?
            types_to_check = types_to_check(:,3);
            for i = 1:length(types_to_check)
                if types_to_check{i}(1)=='>'
                    types_to_check{i} = types_to_check{i}(2:end);
                end
            end
            % previously setdiff was used here, it's simpler but sorts the
            % types_to_check array. Unfortunately, grib files should be
            % checked before the NetCDF file, but capital N is sorted
            % before lowercase g.
            types_to_check(ismember(types_to_check,try_next))=[];
        else
            types_to_check = {};
        end

        %try opening the file ...
        userasked=0;
        usertrytp='';
        if DoDS
            isASCII = false;
            REASON  = 'a URL was provided.';
        else
            [isASCII,REASON] = verifyascii(FileName);
        end
        while isempty(FI)
            %ui_message('','Trying %s ...\n',trytp);
            %pause
            try
                switch try_next
                    case 'md*-file'
                        asciicheck(isASCII,REASON)
                        FI=mdf('read',FileName);
                        Tp=FI.FileType;
                        switch Tp
                            case 'Delft3D D-Flow1D'
                                [p,f,e]=fileparts(FI.md1d.FileName);
                                % by default the output is located in a subdirectory "output"
                                % relative to the folder of the input files
                                po = absfullfile(p,'output');
                                d = dir(fullfile(po,'*.his'));
                                if isempty(d)
                                    % if the files are not there assume that they have been moved
                                    % to another folder by DeltaShell. If the md1d-file is located
                                    % in ...\FileWriters then the associated model output files are
                                    % located in ...\work, so relative to the md1d file in: ..\work.
                                    po = absfullfile(p,'..','work');
                                    d = dir(fullfile(po,'*.his'));
                                end
                                for i = 1:length(d)
                                    [fp,f,e] = fileparts(d(i).name);
                                    f = strrep(f,'-','_');
                                    FI.(f) = delwaq('open',fullfile(po,d(i).name));
                                end
                            case 'Delft3D D-Flow2D3D'
                                FI.Options=1;
                        end
                    case 'qpsession'
                        asciicheck(isASCII,REASON)
                        PAR.X=[];
                        PAR = rmfield(PAR,'X');
                        qp_session('rebuild',FileName,PAR);
                        break
                    case 'nefis'
                        FI=vs_use(FileName,'quiet');
                        if ~isempty(FI)
                            switch lower(FI.SubType)
                                case {'delft3d-waq-map','delft3d-par-map'}
                                    delwaq_results = cat(2,'DEL',upper(FI.SubType(9:11)),'_RESULTS');
                                    if isstruct(vs_disp(FI,delwaq_results,[]))
                                        NfsSeg=vs_disp(FI,delwaq_results,'SUBST_001');
                                    else
                                        NfsSeg=vs_disp(FI,lower(delwaq_results),lower('SUBST_001'));
                                    end
                                    NfsSeg=NfsSeg.SizeDim;
                                    filterspec='';
                                    maybeusegrid=1;
                                    if FileFromCall
                                        if nargin>2
                                            filterspec=varargin{2}; % no absfullfile here since filterspec may be concat of multiple file names using ;
                                        else
                                            maybeusegrid=0;
                                            G=[];
                                        end
                                    end
                                    if maybeusegrid
                                        [G,GridFileName]=get_matching_grid(NfsSeg,pn,filterspec);
                                    end
                                    if ~isstruct(G) % cancel for grid -> use indices
                                        Statw=ceil(log10(NfsSeg+1));
                                        FI.SegmentName=num2cell(reshape(sprintf(strcat('%-',num2str(Statw),'i'),1:NfsSeg),Statw,NfsSeg)',2);
                                        FI.SubType=[FI.SubType(1:end-3) 'his'];
                                    else
                                        Otherargs{1}=GridFileName;
                                        F.Nfs=FI;
                                        F.FileType=FI.FileType;
                                        F.SubType=FI.SubType;
                                        F.Grid=G;
                                        FI=F;
                                    end
                                case {'delft3d-trim'}
                                    FI = qp_option(FI,'morfac',1);
                                    FI = qp_option(FI,'morstt',0);
                                    FI = qp_option(FI,'dps','');
                                    FI = qp_option(FI,'displaytime','hydrodynamic time');
                                    FI = detect_partitions(FI);
                                case {'delft3d-trih'}
                                    FI = qp_option(FI,'displaytime','hydrodynamic time');
                                case {'delft3d-com'}
                                    FI = detect_partitions(FI);
                            end
                            if isfield(FI,'SubType')
                                Tp=FI.SubType;
                            else
                                Tp=vs_type(FI);
                            end
                        end
                        
                        if isfield(FI,'SubType')
                            switch lower(FI.SubType)
                                case {'delft3d-trim','delft3d-com','delft3d-trih','delft3d-waq-map','delft3d-waq-his','delft3d-waq-history','delft3d-par-map'}
                                    FI.Options=1;
                            end
                        end
                    case 'matlab'
                        if isstandalone
                            FI=load(FileName);
                        else
                            FI=load('-mat',FileName);
                        end
                        if strcmpi(en,'.fig')
                            d3d_qp('openfigure',FileName);
                            FI=[];
                            break
                        end
                        if isstruct(FI)
                            f=fieldnames(FI);
                            if numel(f)==1 && strcmpi(f{1},'data')
                                FI = FI.(f{1});
                                FI.FileName=FileName;
                                Tp=try_next;
                            else
                                FI=[];
                            end
                        else
                            FI=[];
                        end
                    case 'WaterML2'
                        asciicheck(isASCII,REASON)
                        FI=waterml2('open',FileName);
                        Tp=FI.FileType;
                    case 'ecomsed-binary'
                        FI=ecomsed('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                                if strcmp(FI.SubType,'GCMPLT')
                                    NSeg = [FI.IM FI.JM];
                                    filterspec='corners*';
                                    askforgrid=1;
                                    if FileFromCall
                                        if nargin>2
                                            filterspec=varargin{2}; % no absfullfile here since filterspec may be concat of multiple file names using ;
                                        else
                                            askforgrid=0;
                                            G=[];
                                        end
                                    end
                                    if askforgrid
                                        [G,GridFileName]=get_matching_grid(NSeg,pn,filterspec);
                                    end
                                    if isstruct(G) % cancel for grid -> use indices
                                        Otherargs{1}=GridFileName;
                                        FI.Grid=G;
                                    else
                                        error('Grid required for processing ECOMSED-GCMPLT file.')
                                    end
                                elseif strcmp(FI.SubType,'MODEL_GRID')
                                    Tp = 'wlgrid';
                                    ui_message('error', ...
                                        {'An attempt was made to reconstruct the grid coordinates', ...
                                        'from the distances in the MODEL_GRID file. Significant', ...
                                        'errors may arise.'})
                                end
                            end
                        end
                    case 'NetCDF'
                        Opt = get_matching_names(FileName,'_',-2);
                        if ~isempty(Opt)
                            if Opt{4}~=0 || Opt{3}~=4
                                Opt = {};
                            end
                        end
                        init_netcdf_settings
                        FI = nc_interpret(FileName,Opt{:});
                        %nc_dump(FileName)
                        %
                        if ~isempty(FI.Attribute)
                            ihistory = ustrcmpi('history',{FI.Attribute.Name});
                            if ihistory > 0
                                if ~isempty(strfind(FI.Attribute(ihistory).Value, 'PHAROS'))
                                    try_next = 'NetCDF-PHAROS';
                                    nDir = ustrcmpi('nDir',{FI.Dimension.Name});
                                    if FI.Dimension(nDir).Length > 1
                                        % directional spreading or spectral
                                        if ustrcmpi('SPEC_PHI_R',{FI.Dataset.Name}) > 0
                                            FI.simtype = 'spectral';
                                        else
                                            FI.simtype = 'dir_spread';
                                        end
                                    else
                                        % long crested or seiching
                                        if ustrcmpi('SEICHDEF_IRL', {FI.Dataset.Name}) > 0
                                            FI.simtype = 'seiching';
                                        else
                                            FI.simtype = 'long_crest';
                                        end
                                    end
                                end
                            end
                        end
                        FI.Options=1;
                        FI.FileName = FI.Filename;
                        Tp = try_next;
                    case 'HDF5'
                        FI = hdf5info(FileName);
                        FI.FileName = FI.GroupHierarchy.Filename;
                        Tp = try_next;
                        if isstruct(FI.GroupHierarchy.Datasets)
                            datasets = {FI.GroupHierarchy.Datasets.Name};
                            if ismember('/File Type',datasets)
                                record = hdf5read(FI.FileName,'//File Type');
                                if strcmpi(record.Data,'XMDF')
                                    Tp = 'XMDF';
                                end
                                record = hdf5read(FI.FileName,'//File Version');
                                FI.FileVersion = record.Data;
                            end
                        end
                        FI.FileType = Tp;
                    case 'sobek1d'
                        FI=sobek('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                            end
                            if ~isempty(FI)
                                FI.Data={};
                                p=fileparts(FileName);
                                files=dir(p);
                                fl={'flowmap.his','minmax.his','gsedmap.his','kafhmap.his', ...
                                    'kafpmap.his','kafrmap.his','kaphmap.his','kappmap.his', ...
                                    'saltmap.his','sedtmap.his','morpmap.his','sobekwq.map', ...
                                    'calcpnt.his','reachseg.his','reachvol.his','delwaq.map'};
                                %'calcdim.his','delwaq.his','flowanal.his', ...
                                %'nodesvol.his','nodes_cr.his','qlat.his','qwb.his', ...
                                %'reachdim.his','reachflw.his','reachvol.his','reach_cr.his', ...
                                %'struc.his','strucdim.his','wqbou20.his'};
                                for i=1:length(files)
                                    if ~isempty(strmatch(lower(files(i).name),fl,'exact'))
                                        try
                                            FIH=delwaq('open',fullfile(p,files(i).name));
                                        catch
                                            FIH=[];
                                        end
                                        if ~isempty(FIH)
                                            FI.Data{end+1}=FIH;
                                        end
                                    end
                                end
                            end
                        end
                    case 'pcraster'
                        FI=pcraster('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                            end
                        end
                    case 'delwaqbin'
                        FI=delwaq('open',FileName);
                        if ~isempty(FI)
                            Tp=FI.FileType;
                            switch lower(Tp)
                                case 'delwaqmap'
                                    filterspec='';
                                    maybeusegrid=1;
                                    if FileFromCall
                                        if nargin>2
                                            filterspec=varargin{2}; % no absfullfile here since filterspec may be concat of multiple file names using ;
                                        else
                                            maybeusegrid=0;
                                            G=[];
                                        end
                                    end
                                    if maybeusegrid
                                        [G,GridFileName]=get_matching_grid(FI.NumSegm,pn,filterspec);
                                    end
                                    if ~isstruct(G) % cancel for grid -> use indices
                                        Statw=ceil(log10(FI.NumSegm+1));
                                        FI.SegmentName=num2cell(reshape(sprintf(strcat('%-',num2str(Statw),'i'),1:FI.NumSegm),Statw,FI.NumSegm)',2);
                                        FI.FileType='DelwaqHIS';
                                    else
                                        Otherargs{1}=GridFileName;
                                        F.DwqBin=FI;
                                        F.FileType=FI.FileType;
                                        F.Grid=G;
                                        FI=F;
                                    end
                            end
                            if strcmp(FI.FileType,'DelwaqHIS')
                                [pn,fn,fne]=fileparts(FI.FileName);
                                if isequal(lower(fne),'.bal')
                                    balfil=1;
                                elseif length(FI.FileName)>7 && ...
                                        isequal(lower(FI.FileName(end-6:end)),'bal.his')
                                    balfil=1;
                                else
                                    balfil=0;
                                end
                            else
                                balfil=0;
                            end
                            FI = qp_option(FI,'balancefile',balfil);
                            FI.Options=1;
                        end
                    case 'fls'
                        FI=fls('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                            end
                        end
                    case 'grib'
                        FI=grib('open',FileName);
                        if ~isfield(FI,'OK')
                            FI=[];
                        elseif FI.OK~=1
                            err = FI.Error;
                            FI = [];
                            error(err)
                        end
                        if ~isempty(FI)
                            Tp=FI.FileType;
                            for b = 1:length(FI.Block)
                                if FI.Block(b).Edition>1
                                    ui_message('warning','Skipping GRIB edition %i block',FI.Block(b).Edition)
                                end
                            end
                        end
                    case 'arcgrid'
                        asciicheck(isASCII,REASON)
                        FI=arcgrid('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                                FI.Options=1;
                            end
                        end
                    case 'surfer'
                        FI=surfer('open',FileName);
                        Tp=FI.FileType;
                    case 'asciiwind'
                        asciicheck(isASCII,REASON)
                        FI=asciiwind('open',FileName);
                        if ~isfield(FI,'Check')
                            FI=[];
                        elseif strcmp(FI.Check,'NotOK')
                            FI=[];
                        else
                            switch FI.Header.filetype
                                case {'meteo_on_computational_grid','field_on_computational_grid'}
                                    if FileFromCall
                                        if nargin>2
                                            gridspec=absfullfile(varargin{2});
                                        else
                                            gridspec='';
                                        end
                                    else
                                        mpn=fileparts(FI.FileName);
                                        [gfn,gpn]=uigetfile([mpn filesep '*.grd'],'Select matching grid file ...');
                                        gridspec = [gpn gfn];
                                    end
                                    if ~ischar(gridspec) || isempty(gridspec)
                                        error('ASCIIWIND of type ''field_on_computational_grid'' requires grid specification.')
                                    end
                                    FI.Header.grid_file=wlgrid('open',gridspec);
                                    if prod(size(FI.Header.grid_file.X)+1)~=FI.NVal
                                        error('Number of data values in grid file does not match number of grid points.')
                                    end
                                    Otherargs{1}=gridspec;
                            end
                            Tp=FI.FileType;
                        end
                        if FI.Header.n_quantity==1 && ...
                                strcmp(FI.Header.quantity{1}(2:end),'_wind') && ...
                                ~isfield(FI,'Vector')
                            if FileFromCall
                                if nargin>2
                                    veccomp2=absfullfile(varargin{2});
                                else
                                    veccomp2='';
                                end
                            else
                                switch lower(FI.Header.quantity{1}(1))
                                    case 'x'
                                        Y='y';
                                    case 'y'
                                        Y='x';
                                    otherwise
                                        Y='';
                                end
                                if ~isempty(Y)
                                    ft = qp_filefilters('asciiwind');
                                    ft(2,:) = {'*.*' 'All Files' 'anyfile'};
                                    targetdir=fileparts(FI.FileName);
                                    %
                                    currentdir=pwd;
                                    cd(targetdir);
                                    [gfn,gpn]=uigetfile(ft(:,1:2),sprintf('Select matching %s-component wind file ...',Y));
                                    cd(currentdir);
                                    %
                                    veccomp2 = [gpn gfn];
                                else
                                    veccomp2 = '';
                                end
                            end
                            if ~isempty(veccomp2)
                                FI2=asciiwind('open',veccomp2);
                                FI=asciiwind('merge',FI,FI2);
                                if isfield(FI,'Vector')
                                    Otherargs{1}=veccomp2;
                                end
                            end
                        end
                    case 'waquasds'
                        FI=waqua('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                            end
                        end
                    case 'boxfile'
                        DataFI=boxfile('read',FileName);
                        FI = wlgrid('create',DataFI);
                        FI.FileName = FileName;
                        %
                        FI.FileType = 'wlgrid';
                        FI.Options = 0;
                        FI.QP_Options = [];
                        %
                        FI = gridfil(FI,[],'options',[],'openfile',FileName);
                        Tp = FI.FileType;
                    case 'mike0'
                        FI=mike('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                            end
                            FI.Options=1;
                        end
                    case 'wlgrid'
                        FI=wlgrid('open',FileName);
                        if ~isempty(FI)
                            if isequal(FI.Type,'RGF')
                                Tmp=repmat(NaN,size(FI.X)+1);
                                Tmp(1:end-1,1:end-1)=FI.X;
                                FI.X=Tmp;
                                Tmp(1:end-1,1:end-1)=FI.Y;
                                FI.Y=Tmp;
                            end
                            if isfield(FI,'Orient') && strcmp(FI.Orient,'clockwise')
                                ui_message('warning',{FI.FileName, ...
                                    'The orientation of this grid is "clockwise".', ...
                                    'Delft3D requires a counter-clockwise (CCW) grid.', ...
                                    'Save CCW grid using the File Dependent Options dialog.'})
                            end
                            FI.FileType='wlgrid';
                            FI.Options=1;
                            Tp=FI.FileType;
                        end
                    case 'nodelemesh'
                        FI=nodelemesh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            Tp=FI.FileType;
                        end
                    case 'adcircmesh'
                        asciicheck(isASCII,REASON)
                        FI=adcircmesh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            Tp=FI.FileType;
                        end
                    case 'smsmesh'
                        asciicheck(isASCII,REASON)
                        FI=smsmesh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            Tp=FI.FileType;
                        end
                    case 'mikemesh'
                        asciicheck(isASCII,REASON)
                        FI=mikemesh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            Tp=FI.FileType;
                        end
                    case 'SHYFEM mesh'
                        asciicheck(isASCII,REASON)
                        FI=shyfemmesh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            Tp=FI.FileType;
                        end
                    case 'geomesh'
                        FI=geomesh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            FI.DomainName = 'Layer';
                            Tp=FI.FileType;
                        end
                    case 'gmsh'
                        FI=gmsh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            Tp=FI.FileType;
                        end
                    case 'tuflowmesh'
                        asciicheck(isASCII,REASON)
                        FI=tuflowmesh('open',FileName);
                        if ~isempty(FI)
                            FI.Options=0;
                            Tp=FI.FileType;
                        end
                    case 'tekal'
                        asciicheck(isASCII,REASON)
                        FI=tekal('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            elseif isempty(FI.Field) % only accept non-empty files
                                FI=[];
                            else
                                Tp='Tekal';
                                [pn,fn,ex]=fileparts(FI.FileName);
                                FI.can_be_ldb=1;
                                FI.combinelines=0;
                                ncol = [2 3];
                                for i=1:length(FI.Field)
                                    if length(FI.Field(i).Size)~=2
                                        FI.can_be_ldb=0;
                                    elseif ~ismember(FI.Field(i).Size(2),ncol)
                                        FI.can_be_ldb=0;
                                    elseif ~strcmp(FI.Field(i).DataTp,'numeric')
                                        FI.can_be_ldb=0;
                                    else
                                        ncol = FI.Field(i).Size(2);
                                    end
                                    if ~FI.can_be_ldb
                                        break
                                    end
                                end
                                can_be_kub=0;
                                switch lower(ex)
                                    case {'.ldb','.pol','.pli','.pliz'}
                                        if FI.can_be_ldb
                                            FI.combinelines=1;
                                        end
                                    case '.kub'
                                        can_be_kub=1;
                                        if length(FI.Field)>1
                                            can_be_kub=0;
                                        elseif length(FI.Field(1).Size)~=2
                                            can_be_kub=0;
                                        elseif ~strcmp(FI.Field(1).DataTp,'numeric')
                                            can_be_kub=0;
                                        end
                                end
                                if can_be_kub
                                    ppn = '';
                                    if nargin>2
                                        pfn=absfullfile(varargin{2});
                                    else
                                        pfn='';
                                    end
                                    while 1
                                        if ~exist([ppn pfn])
                                            cp=pwd;
                                            cd(pn);
                                            [pfn,ppn]=uigetfile('*.ldb;*.pol','Select matching polygon file ...');
                                            cd(cp);
                                        end
                                        if ~ischar(pfn)
                                            break
                                        end
                                        try
                                            pfile = tekal('open',[ppn pfn]);
                                            if length(pfile.Field)~=FI.Field(1).Size(1)
                                                ui_message('error','Number of values in KUBINT file (%i) does not\nmatch the number of polygons (%i)',FI.Field(1).Size(1),length(pfile.Field))
                                                ppn='';
                                                pfn='';
                                            else
                                                Otherargs{1} = [ppn pfn];
                                                break
                                            end
                                        end
                                    end
                                    if ischar(pfn)
                                        FI.plotonpoly=pfile;
                                    end
                                end
                                FI.Options=FI.can_be_ldb;
                            end
                        end
                    case 'AutoCAD DXF'
                        Data=dxf('read',FileName);
                        tp = zeros(1,length(Data));
                        for i = 1:length(Data)
                            if size(Data{i},1)==3
                                if all(isnan(Data{i}(1,1:2:end))) % points
                                    tp(i)=1; %point
                                else
                                    tp(i)=2; %poly
                                    if all(Data{i}(:,1)==0)
                                        Data{i} = Data{i}(:,2:end);
                                    end
                                end
                            else
                                tp(i)=3; %patch
                            end
                        end
                        if ~isempty(Data)
                            FI.FileType = try_next;
                            FI.FileName = FileName;
                            FI.Points = cat(2,Data{tp==1});
                            L = Data(tp==2);
                            L(2,:) = {[NaN;NaN;NaN]};
                            FI.Lines  = cat(2,L{:});
                            FI.Patch  = Data(tp==3);
                            Tp=try_next;
                        end
                    case 'GeoJSON'
                        asciicheck(isASCII,REASON)
                        FI=geojson('open',FileName);
                        if ~isempty(FI)
                            Tp=try_next;
                        end
                    case 'shape'
                        FI=shape('open',FileName);
                        if ~isempty(FI)
                            Tp=FI.FileType;
                        end
                    case 'SWAN spectral'
                        asciicheck(isASCII,REASON)
                        FI=readswan(FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=FI.FileType;
                            end
                        end
                    case 'DelwaqTimFile'
                        asciicheck(isASCII,REASON)
                        FI=delwaqtimfile(FileName);
                        if ~isempty(FI)
                            Tp=FI.FileType;
                        end
                    case 'morf'
                        asciicheck(isASCII,REASON)
                        FI=morf('read',FileName);
                        if ~isempty(FI)
                            Tp='MorfTree';
                        end
                    case 'aukepc'
                        FI=aukepc('open',FileName);
                        if ~isempty(FI)
                            Tp='AukePC';
                        end
                    case 'bct'
                        asciicheck(isASCII,REASON)
                        FI=bct_io('read',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check') || strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp='Bct';
                            end
                        end
                    case 'CFX dmp'
                        FI=cfx('open',FileName);
                        if ~isempty(FI)
                            Tp=try_next;
                            Tmp=FI;
                            FI=[];
                            FI.Encaps=Tmp;
                            try
                                B1=cfx1block(Tmp);
                            catch
                                B1=[];
                            end
                            FI.B1=B1;
                        end
                    case 'JSPost'
                        FI=jspost('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=try_next;
                            end
                        end
                    case 'bagdpt'
                        FI=bagdpt('read',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=try_next;
                            end
                        end
                    case 'bitmap'
                        FI=imfinfo(FileName);
                        if ~isempty(FI) && isstruct(FI)
                            FI_temp.FileInfo=FI;
                            FI=FI_temp;
                            FI_temp=[];
                            %
                            % is it a series of bitmaps?
                            %
                            i=find(ismember(FileName,'1234567890'));
                            slash=max(find(FileName==filesep));
                            if ~isempty(i) && slash<max(i)
                                lasti = max(i);
                                firsti = lasti;
                                while ismember(firsti-1,i)
                                    firsti = firsti-1;
                                end
                                FN_len = length(FileName);
                                files=dir([FileName(1:firsti-1) '*' FileName(lasti+1:end)]);
                                files={files.name};
                                times=repmat(NaN,1,length(files));
                                for i=1:length(files)
                                    time=str2num(files{i}(firsti-slash:end-FN_len+lasti));
                                    if ~isempty(time)
                                        times(i)=time;
                                    end
                                end
                                times=sort(times(~isnan(times)));
                                if length(times)>1
                                    format='%i';
                                    %
                                    FN_len = cellfun('length',files);
                                    if length(unique(FN_len))==1
                                        i=num2str(lasti-firsti+1);
                                        format=['%0' num2str(i) 'i'];
                                    end
                                    %
                                    FI.FileInfo.times=times;
                                    FI.FileInfo.format=format;
                                    FI.FileInfo.prefix=FileName(1:firsti-1);
                                    FI.FileInfo.postfix=FileName(lasti+1:end);
                                end
                            end
                            %
                            FI.FileType=try_next;
                            FI.FileName=FileName;
                            FI.Options=1;
                            FI.Loc=[0 0 FI.FileInfo.Width FI.FileInfo.Height];
                            [ImP,ImF,ImE]=fileparts(FI.FileName);
                            switch lower(ImE)
                                case {'.tif','.jpg','.png','.bmp'}
                                    ImE=ImE([1 2 4]);
                                    if ImE(3)==lower(ImE(3))
                                        ImE = [ImE 'w'];
                                    else
                                        ImE = [ImE 'W'];
                                    end
                            end
                            fid = fopen(fullfile(ImP,[ImF ImE]),'r','n','US-ASCII');
                            if fid>0
                                Coords = fscanf(fid,'%f',6);
                                fclose(fid);
                                if length(Coords)==6
                                    FI.Loc = [Coords(5)-Coords(1)/2 Coords(6)-Coords(4)/2+Coords(4)*FI.FileInfo.Height Coords(1)*FI.FileInfo.Width -Coords(4)*FI.FileInfo.Height];
                                    if Coords(2)~=0 || Coords(3)~=0
                                        ui_message('warning',{'Bitmap distortion not yet supported.','Distortion factors reset to 0.'})
                                    end
                                end
                            end
                            Tp=try_next;
                        end
                    case 'video'
                        FI.vidObj = VideoReader(FileName);
                        FI.FileType=try_next;
                        FI.FileName=FileName;
                        FI.Options=1;
                        FI.Loc=[0 0 FI.vidObj.Width FI.vidObj.Height];
                        Tp=try_next;
                    case 'telemac'
                        FI=telemac('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=try_next;
                            end
                        end
                    case 'samples'
                        asciicheck(isASCII,REASON)
                        XYZ=samples('read',FileName,'struct');
                        if isempty(XYZ)
                            FI=[];
                        elseif isfield(XYZ,'FileType')
                            FI=XYZ;
                            Tp=FI.FileType;
                        else
                            FI=[];
                        end
                    case 'BNA File'
                        asciicheck(isASCII,REASON)
                        FI=bna('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=try_next;
                            end
                        end
                    case 'ArcInfoUngenerate'
                        asciicheck(isASCII,REASON)
                        FI=ai_ungen('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=try_next;
                            end
                        end
                    case 'bil/hdr'
                        FI=bil('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=try_next;
                            end
                        end
                    case 'NOOS time series'
                        asciicheck(isASCII,REASON)
                        FI=noosfile('open',FileName);
                        Tp=try_next;
                    case 'shipma'
                        FI=shipma('open',FileName);
                        Tp=try_next;
                        FI.DomainName = 'Proj/Case';
                        FI.Options=1;
                    case 'unibest'
                        FI=unibest('open',FileName);
                        if ~isempty(FI)
                            if ~isfield(FI,'Check')
                                FI=[];
                            elseif strcmp(FI.Check,'NotOK')
                                FI=[];
                            else
                                Tp=try_next;
                            end
                        end
                    otherwise
                        errstr = sprintf('Please check program code:\nNo code found for opening file of type ''%s''!',try_next);
                        ui_message('error',errstr)
                        return
                end
                lasttp = try_next;
            catch err
                FI=[];
            end
            if isempty(FI)
                % opening of file was not successful
                if ~isempty(types_to_check)
                    % other file type to check
                    try_next = types_to_check{1};
                    types_to_check = types_to_check(2:end);
                else
                    % I have tried all file types
                    if userasked
                        % and I have asked the user
                        qp_error(sprintf('Error while opening\n%s\nas %s:',FileName,usertrytp),err)
                        break
                    else
                        if isempty(filtertbl)
                            if DoDS
                                Message=sprintf('Error while opening\n%s\nUnable to make connection.',FileName);
                            else
                                Message=sprintf('Error while opening\n%s\nUnable to identify file format.',FileName);
                            end
                            ui_message('error',Message)
                            break
                        else
                            if strcmp(cmd,'openldb')
                                filtertbl = qp_filefilters('files-with-lines');
                            else
                                filtertbl = qp_filefilters('all');
                            end
                            [usertrytp,try_i]=ui_type(filtertbl(:,2),'windowtitle','Specify file format');
                        end
                        if isempty(usertrytp)
                            break
                        end
                        try_next=filtertbl{try_i,3};
                        if try_next(1)=='>'
                            try_next=try_next(2:end);
                        end
                        userasked=1;
                        err=[];
                        err.message='No specific error message generated';
                        err.stack=[];
                    end
                end
            end
        end
end
if isempty(FI)
    lasttp=[];
end
qp_settings('LastFileType',lasttp)


function [isASCII,REASON] = verifyascii(arg)
fid = fopen(arg,'r');
S = fread(fid,[1 1024],'uint8');
fclose(fid);
if isempty(S)
    isASCII = false;
    REASON  = 'the file is empty';
else
    if S(end) == 26 % last chracter can be an EOF marker
        S = S(1:end-1);
    end
    invalid_chars = S(S~=9 & S~=10 & S~=13 & S<32); % TAB,LF,CR allowed
    if ~isempty(invalid_chars)
        isASCII = false;
        REASON  = sprintf('the file contains character %i.',invalid_chars(1));
    else
        isASCII = true;
        REASON  = '';
    end
end


function asciicheck(ASCII,REASON)
if ~ASCII
    error('This should be an ASCII file, however %s',REASON)
end


function Opt = get_matching_names(FileName,sep,nr)
[p,n,e]=fileparts(FileName);
Opt = {};
locsep = find(n==sep);
if nr==-2 && length(locsep)>=2
    iOffset = locsep(end-1);
    nDigits = locsep(end)-iOffset-1;
elseif nr==-1 && length(locsep)>=1
    iOffset = locsep(end);
    nDigits = length(n)-iOffset;
else
    nDigits = 0;
end
if nDigits>0 && all(ismember(n(iOffset+(1:nDigits)),'0123456789'))
    iPOffset  = length(p)+1+iOffset;
    if length(p)>=1 && p(end)==filesep
        iPOffset = iPOffset-1;
    end
    FileName1 = FileName(1:iPOffset);
    FileName2 = FileName(iPOffset+nDigits+1:end);
    %
    % find all files with same structure
    d = dir([FileName1 '*' FileName2]);
    Files = {d.name}';
    lFiles = cellfun('length',Files);
    Files(lFiles~=length([n e])) = [];
    %
    Files = char(Files);
    N = Files(:,iOffset+(1:nDigits));
    N(~all(ismember(N,'0':'9'),2),:) = [];
    N = sort(str2num(N))';
    %
    if isequal(N,0:length(N)-1) || isequal(N,1:length(N))
        Opt = {length(N) iPOffset nDigits N(1)};
    end
end


function FI = detect_partitions(FI)
FileName = [FI.FileName FI.DatExt];
Opt = get_matching_names(FileName,'-',-1);
if ~isempty(Opt)
    FI.Partitions = Opt;
    nParts = Opt{1};
    for d = nParts:-1:1
        FN = FileName;
        FN(Opt{2}+(1:Opt{3})) = sprintf('%3.3i',d);
        FI.NEFIS(d) = vs_use(FN,'quiet');
    end
    FI.Merge.Length = 0;
    FI.Merge.Indices = cell(1,nParts);
    for d = 1:nParts
        switch FI.SubType
            case 'Delft3D-trim'
                KCS = vs_get(FI.NEFIS(d),'map-const','KCS','quiet');
            case 'Delft3D-com'
                KCS = vs_get(FI.NEFIS(d),'KENMCNST','KCS','quiet');
        end
        if d == 1
            if KCS(end,2) == -1
                FI.Merge.Dimension = 'N';
            else
                FI.Merge.Dimension = 'M';
            end
        end
        switch FI.Merge.Dimension
            case 'M'
                IPart = sum(not(any(KCS==-1,1)));
            case 'N'
                IPart = sum(not(any(KCS==-1,2)));
        end
        FI.Merge.Indices{d} = FI.Merge.Length + (1:IPart);
        FI.Merge.Length = FI.Merge.Length + IPart;
    end
end
