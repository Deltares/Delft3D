function delwaq2raster(ini_file)
%DELWAQ2RASTER rasterize delwaq MAP results
%   DELWAQ2RASTER(INIFILE) reads the INIFILE and converts selected data
%   from the delwaq MAP results file referenced therein to raster grid
%   files for use with for instance the HABITAT module. The INIFILE
%   should contain the following elements:
%
%   A block [general] containing keywords:
%     raster :  name of reference raster file (determining raster
%               dimensions and raster file format). Currently, supports
%               bil/hdr pairs and asc files. Output files will use the same
%               settings.
%     data   :  name of delwaq MAP file
%     grid   :  reference to Delft3D curvilinear cco/lga files or flexible
%               mesh netCDF file.
%     method :  data mapping method, one of:
%               center   : raster value determined by simulation cell in
%                          which the raster cell center is located
%               maxarea  : raster value determined by biggest overlapping
%                          (aggregated) simulation cell
%               weighted : raster value determined by area weighted average
%                          of overlapping simulationn cells; the total
%                          overlapping area fraction must be at least equal
%                          to minarea.
%     minarea:  indicates for method 'weighted' the minimum overlap area to
%               determine the average (in case of smaller overlap, the
%               raster cell is assigned the nodata value). Default: 0.
%     outdir :  output directory for the raster files.
%     tstart :  start index of time steps to include in processing (by
%               default 1, that is the first time step).
%     tstop  :  stop index of time steps to include in processing (by
%               default the last time step in the file).
%     ntstep :  length of period (expressed in number of time steps on the
%               file) to include in each operation (not used for operation
%               ident). See next block for details.
%     skipstep: number of time steps skipped between the time steps
%               actually included in the operation (not used for operation
%               ident). See next block for details.
%
%   One or more [action] blocks containing the following keywords:
%     time_op:  name of operation in time. The tool supports the following
%               operations:
%               max, mean, min, std, percentile and ident (default)
%               The default time operator "ident" will convert every time
%               step in the specified period (from tstart until tstop). In
%               the other cases, it will take the max, mean, min, std or
%               specified percentile of all values in the selected time
%               period.
%     percentile: percentile to be taken from the data of the selected
%               time period (0-100%, required when time_op is percentile).
%     tstart :  start index of time steps to include in processing (default
%               value taken from [general] block).
%     tstop  :  stop index of time steps to include in processing (default
%               value taken from [general] block).
%     ntstep :  length of period (expressed in number of time steps on the
%               file) to include in each operation (not used for operation
%               ident). By default all time steps from tstart to tstop are
%               included. The number of time steps actually included in the
%               operation may be less (see skipstep).
%     tshift :  time steps shift between periods to be processed. The
%               periods are tstart+N*tshift until tstart+N*tshift+ntstep-1
%               for N = 0,1,... The default tshift is equal to ntstep such
%               that the processed periods are sequential.
%     skipstep: number of time steps skipped between the time steps
%               actually included in the operation (not used for operation
%               ident). By default no time steps are skipped (skipstep=0).
%               Note that if you want to use every second value, you skip
%               one value, and hence you need to specify skipstep=1. The
%               time steps included in the operation are
%                     tstart+N*tshift+(skipstep+1)*i
%               for i=0,1,..., floor((ntstep-1)/(skipstep+1)).
%     include:  name of a variable to be converted to raster (the include
%               keyword may be repeated to convert multiple variables using
%               the same settings). The names specified should either match
%               the short DELWAQ name, or the expanded names as used by
%               Delft3D-QUICKPLOT.
%     layer:    number of the layer for which the data needs to be
%               extracted from a 3D delwaq MAP file, for example
%                 layer = 6
%               to retrieve data for layer 6. Specify
%                 layer = 'depth average'
%               to compute the depth averaged values (this is the default).
%               Use the setting
%                 layer = 'bottom layer'
%               to extract the values in the bottom most layer (for sigma-
%               layer models it's more efficient to specify this layer by
%               number). The latter two options require that the LocalDepth
%               variable is available on the delwaq MAP file.
%     localdepth: specify the name of the variable to be used for the
%               layer depth, by default this is: LocalDepth.
%     outputgrid: user specified name for the output grid. By default a 
%               name will be constructed from include, time_op, optionally
%               percentile and timings.
%
%   One or more [<VarName>] blocks in which <VarName> matches the name of a
%   variable specified in the "include" item of an [action] block. The
%   following keywords are supported in such blocks:
%     lowerbound: minimum value to be used for the variable; the maximum of
%               the value and the lower bound will be used in the analysis.
%               The default lowerbound is -Inf, i.e. no limit applied.
%     upperbound: minimum value to be used for the variable; the minimum of
%               the value and the upper bound will be used in the analysis.
%               The default upperbound is Inf, i.e. no limit applied.
%
%   See also: DELWAQ, ARCGRID.

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

LAYER_DEPTH_AVERAGE = -1001;
LAYER_BOTTOM_MOST   = -1002;

if isstandalone
    fprintf(1,'--------------------------------------------------------------------------------\n');
    fprintf(1,'Delft3D-DELWAQ2RASTER conversion tool.\n');
    fprintf(1,'Version <VERSION> (<CREATIONDATE>)\n');
    fprintf(1,'Repository <GITREPO>\n');
    fprintf(1,'Source hash <GITHASH>\n');
    fprintf(1,'--------------------------------------------------------------------------------\n');
end

if nargin==0
    fprintf(1,'Syntax: delwaq2raster <configuration.ini>\n');
    error('No configuration.ini file specified')
end
%
fprintf(1,'Parsing input file ...\n');
ini_info       = inifile('open',ini_file);
%
raster_reffile = inifile('get',ini_info,'general','raster');
gridfile       = inifile('get',ini_info,'general','grid');
waqfile        = inifile('get',ini_info,'general','data');
flwfile        = inifile('get',ini_info,'general','flow',gridfile);
method         = inifile('get',ini_info,'general','method');
outdir         = inifile('get',ini_info,'general','outdir');
%
fprintf(1,'Reading template raster file(s) ...\n');
raster_info    = qpfopen(raster_reffile);
for f = {'qp_filetype','QP_FileType'}
    fs = f{1};
    if isfield(raster_info,fs)
        raster_info    = rmfield(raster_info,fs);
    end
end
%
switch raster_info.FileType
    case 'HDR Raster File - BIP/BIL/BSQ'
        raster_xco     = bil('read',raster_info,'xco');
        raster_xcc     = bil('read',raster_info,'xcc');
        raster_yco     = bil('read',raster_info,'yco');
        raster_ycc     = bil('read',raster_info,'ycc');
        raster_sz      = [raster_info.nRows raster_info.nCols];
        cellarea       = raster_info.Xdim*raster_info.Ydim;
    case 'arcgrid'
        tmp = qpread(raster_info,'grid','grid');
        raster_xco     = tmp.X(:,1)';
        raster_xcc     = raster_xco(1:end-1)+raster_info.CellSize(1)/2;
        raster_yco     = tmp.Y(1,:);
        raster_ycc     = raster_yco(1:end-1)-raster_info.CellSize(2)/2;
        raster_sz      = [raster_info.NRows raster_info.NCols];
        cellarea       = prod(raster_info.CellSize);
end
%
yflip          = raster_yco(2)<raster_yco(1);
if yflip
    raster_yco = raster_yco(end:-1:1);
end
%
fprintf(1,'Reading WAQ data file(s) ...\n');
waq_info       = qpfopen(waqfile,gridfile);
nLayers_WAQ    = waq_info.Grid.MNK(3);
nSeg2D         = waq_info.DwqBin.NumSegm/nLayers_WAQ;
dwq            = waq_info.DwqBin;
waq_qnt        = qpread(waq_info);
waq_qnt(strcmp('-------',{waq_qnt.Name}'))=[];
%
fprintf(1,'Reading FLOW data file(s) ...\n');
[p,f,e] = fileparts(flwfile);
flw_info1      = qpfopen(flwfile);
if ~isfield(flw_info1,'NumDomains') || flw_info1.NumDomains==1
    % regular simulation
    flw_info = flw_info1;
else
    % MPI parallel simulation
    flw_info = cell(flw_info1.NumDomains,3);
    for i = 1:flw_info1.NumDomains
        INFO = flw_info1.Partitions{i};
        INFO.FileName = INFO.Filename; % for qpread
        INFO.QP_FileType = 'NetCDF';   % for qpread
        %
        FEGN = nc_varget(INFO.FileName,'FlowElemGlobalNr'); % nFlowElem
        FED  = nc_varget(INFO.FileName,'FlowElemDomain');   % nFlowElem
        %
        flw_info{i,1} = INFO;
        flw_info{i,2} = FEGN;
        flw_info{i,3} = FED;
    end
    %
    if max(cat(1,flw_info{:,2})) ~= waq_info.Grid.MNK(1)
        error('Total number of cells in flow partitions (%i) should match 2D number of WAQ segments (%i).',max(cat(1,flw_info{:,2})), waq_info.Grid.MNK(1))
    end
    %
    flw_info1 = flw_info{1,1};
end
%
flw_qnt        = qpread(flw_info1);
flw_is_structured = false;
flw_use_mask = false;
if isfield(flw_qnt,'Name')
    for i = length(flw_qnt):-1:1
        idm = flw_qnt(i).Name;
        if isempty(idm) || iscell(idm) %|| ~ismember('nFlowElem',flw_info1.Dataset(idm(1)+1).Dimension)
            flw_qnt(i) = [];
        end
    end
    if strcmp(flw_info1.QP_FileType, 'Delft3D-trim')
        flw_is_structured = true;
        maskid = filter_qnt('1/-1 Active/Non-active water level point (w.r.t. coordinates )',waq_qnt,flw_qnt);
        if maskid < 0
            flw_data = qpread(flw_info,flw_qnt(-maskid),'data',1,0);
            flw_mask = reshape(flw_data.Val',[],1)';
            flw_use_mask = true;
        end
    end
else
    flw_qnt(:) = [];
end
%
fprintf(1,'Collecting data from [action] blocks ...\n');
tstart         = inifile('get',ini_info,'general','tstart',1);
tstop          = inifile('get',ini_info,'general','tstop' ,inf);
ntstep         = inifile('get',ini_info,'general','ntstep',inf);
ntskip         = inifile('get',ini_info,'general','skipstep',0);
layer          = inifile('get',ini_info,'general','layer','depth average');
%
if exist(outdir,'dir')~=7
    error('Invalid output directory: %s',outdir)
end
%
chp = inifile('chapters',ini_info);
nchp = length(chp);
blockprocessed = false(1,nchp);
for ifld = 1:nchp
    switch chp{ifld,1}
        case 'general'
            blockprocessed(ifld) = true;
        case 'action'
            blockprocessed(ifld) = true;
            chp{ifld,1} = inifile('getstring',ini_info,ifld,'time_op','ident');
            [chp{ifld,2},ifldout] = inifile('cgetstring',ini_info,ifld,'include'); % ifldout needed to avoid error when no 'include' statement is found
            [chp{ifld,3},chp{ifld,2}] = filter_qnt(chp{ifld,2},waq_qnt,flw_qnt);
            info = [];
            info.prctl  = inifile('get',ini_info,ifld,'percentile',-999);
            info.tstart = inifile('get',ini_info,ifld,'tstart',tstart);
            info.tstop  = inifile('get',ini_info,ifld,'tstop' ,tstop);
            info.ntstep = inifile('get',ini_info,ifld,'ntstep',ntstep);
            info.tshift = inifile('get',ini_info,ifld,'tshift',NaN);
            info.ntskip = inifile('get',ini_info,ifld,'skipstep',ntskip);
            info.layer  = inifile('get',ini_info,ifld,'layer',layer);
            info.ldepth = inifile('getstring',ini_info,ifld,'localdepth','LocalDepth');
            info.ogrid  = inifile('getstring',ini_info,ifld,'outputgrid','');
            if ischar(info.layer)
                switch info.layer
                    case 'depth average'
                        info.layer = LAYER_DEPTH_AVERAGE;
                    case 'bottom layer'
                        info.layer = LAYER_BOTTOM_MOST;
                    otherwise
                        error('Unknown layer "%s"',info.layer);
                end
            end
            chp{ifld,4} = info;
        otherwise
            %fprintf(1,'  Skipping [%s] block\n',chp{ifld,1});
    end
end
%
fprintf(1,'Identifying quantities to be processed ...\n');
qnts = cat(1,chp{:,2});
iqnt = cat(1,chp{:,3});
if any(iqnt==0)
    qnts = unique(qnts(iqnt==0));
    if length(qnts)==1
        err = 'Unable to locate the following quantity in the data file(s)';
    else
        err = 'Unable to locate the following quantities in the data file(s)';
    end
    error('%s\n',err,qnts{:})
end
[uqnts,idum,iuqnt] = unique(qnts);
offset = 0;
for ifld = 1:nchp
    nq = length(chp{ifld,2});
    chp{ifld,2} = iuqnt(offset+(1:nq));
    offset = offset + nq;
end
%
fprintf(1,'Processing [<quantity name>] blocks ...\n');
nqnts = length(uqnts);
[qntprp(1:nqnts).lowerbound] = deal(-inf);
[qntprp(:).upperbound] = deal(inf);
for iq = 1:nqnts
    ifld = find(strcmp(uqnts{iq},chp(:,1)));
    if ~isempty(ifld) % assuming one block
        blockprocessed(ifld) = true;
        qntprp(iq).lowerbound = inifile('get',ini_info,ifld,'lowerbound',-inf);
        qntprp(iq).upperbound = inifile('get',ini_info,ifld,'upperbound',inf);
        if qntprp(iq).lowerbound~=-inf && qntprp(iq).upperbound~=inf
            fprintf(1,'  %s clipped to range [%g,%g]\n',uqnts{iq},qntprp(iq).lowerbound,qntprp(iq).upperbound);
        elseif qntprp(iq).lowerbound~=-inf
            fprintf(1,'  %s clipped to %g or larger\n',uqnts{iq},qntprp(iq).lowerbound);
        elseif qntprp(iq).upperbound~=inf
            fprintf(1,'  %s clipped to %g or smaller\n',uqnts{iq},qntprp(iq).upperbound);
        end
    end
end
%
if any(~blockprocessed)
    fprintf(1,'The following blocks in have been skipped ....\n');
    for ifld = 1:nchp
        if ~blockprocessed(ifld)
            fprintf(1,'  Block %i: [%s]\n',ifld,chp{ifld,1});
        end
    end
end
%
fprintf(1,'Checking time information ...\n');
ndatasets = 0;
for ifld = 1:nchp
    if ~isempty(chp{ifld,3})
        nq = length(chp{ifld,3});
        info = chp{ifld,4};
        %
        AllTimes  = cell(2,0);
        for i = 1:nq
            if chp{ifld,3}(i)>0
                times = delwaq('read',dwq,[],[]);
            else
                times = qpread(flw_info1,flw_qnt(-chp{ifld,3}(i)),'times');
            end
            match=0;
            for it = 1:size(AllTimes,2)
                if isequal(AllTimes{1,it},times)
                    % add elem
                    AllTimes{2,it}(end+1,1) = uqnts(chp{ifld,2}(i));
                    match=1;
                end
            end
            if ~match
                AllTimes{1,end+1} = times;
                AllTimes{2,end} = uqnts(chp{ifld,2}(i));
            end
        end
        if size(AllTimes,2)>1
            if nq>2
                all_ = 'all ';
            else
                all_ = '';
            end
            err = {sprintf('The output times for the selected variables in block %i don''t %smatch.',ifld,all_)};
            for it = 1:size(AllTimes,2)
                if length(AllTimes{2,it})==1
                    err{end+1,1} = sprintf('%s: %i time step(s)',AllTimes{2,it}{1},length(AllTimes{1,it}));
                else
                    err{end+1,1} = sprintf('%i shared time step(s) for',length(AllTimes{1,it}));
                    for i = 1:length(AllTimes{2,it})
                        err{end+1,1} = sprintf('  %s',AllTimes{2,it}{i});
                    end
                end
            end
            error('%s\n',err{:})
        end
        if ~isfinite(info.tstop)
            info.tstop = length(AllTimes{1});
        elseif info.tstart>=info.tstop
            error('Invalid time range specified: tstart (now %i) should be less than tstop (now %i)',info.tstart,info.tstop)
        elseif info.tstop>length(AllTimes{1})
            error('Time range selected (%i:%i) does not fall inside the time range available (1:%i)',info.tstart,info.tstop,length(AllTimes{1}))
        end
        %
        ntotstep = info.tstop - info.tstart + 1;
        if strcmp(chp{ifld,1},'ident')
            info.ntstep = 1;
        elseif info.ntstep <= 0
            info.ntstep = ntotstep;
        elseif info.ntstep > ntotstep
            info.ntstep = ntotstep;
        end
        %
        if isnan(info.tshift)
            info.tshift = info.ntstep;
        else
            info.tshift = max(1,info.tshift);
        end
        %
        ndatasets = ndatasets + length(chp{ifld,2})*(floor((ntotstep-info.ntstep)/info.tshift)+1);
        %
        chp{ifld,4} = info;
    end
end
%
fprintf(1,'Checking layer information ...\n');
for ifld = 1:nchp
    iqnt = chp{ifld,3};
    if isempty(iqnt)
        continue
    end
    info = chp{ifld,4};
    if any(iqnt)>0
        % delwaq quantities
        if info.layer>nLayers_WAQ
            error('In block %i you request non-existing layer %i of a %i layer model; please correct.',ifld,info.layer,nLayers_WAQ)
        end
        if nLayers_WAQ==1
            info.layer = 1;
        end
        if info.layer == LAYER_DEPTH_AVERAGE || info.layer == LAYER_BOTTOM_MOST
            ildp = filter_qnt(info.ldepth,waq_qnt,[]);
            if ildp==0
                error('Missing depth information (%s) on WAQ map file to compute depth average or near bed quantities.',info.ldepth);
            end
            info.ildp = waq_qnt(ildp).Val1;
        end
    end
    chp{ifld,4} = info;
end
%
fprintf(1,'Checking percentile information ...\n');
for ifld = 1:nchp
    info = chp{ifld,4};
    if strcmp(chp{ifld,1},'percentile')
        if any(info.prctl < 0 | info.prctl > 100) || ~isreal(info.prctl)
            error('Invalid or missing percentile specification in block %i', ifld)
        end
    end
end
%
fprintf(1,'Segment number mapping ...\n');
G = qpread(waq_info, 'segment number', 'gridcell');
if size(G.X,2)>1
else
    idx = find(~isnan(G.X));
    id2 = find(diff([-100;idx;-100])~=1);
    %
    ncell = length(id2)-1;
    %
    G.CellX = cell(ncell,1);
    G.CellY = cell(ncell,1);
    G.CellIdx = zeros(ncell,1);
    for m = 1:ncell
        m1 = idx(id2(m));
        m2 = idx(id2(m+1)-1);
        %
        G.CellX{m} = G.X(m1:m2-1)';
        G.CellY{m} = G.Y(m1:m2-1)';
        G.CellIdx(m) = m1;
    end
    G.CellVal = G.Val(G.CellIdx);
end
%
fprintf(1,'Determining spatial mapping ...\n');
switch method
    case 'center'
        min_area = 0;
        yflip = 0;
        %
        % create mapping
        %
        h = lcwaitbar(0,'Mapping centers ...');
        if size(G.X,2)>1
            wght = cell(size(G.X)-1);
            lidx = cell(size(G.X)-1);
            for m = 1:size(G.X,1)-1
                for n = 1:size(G.X,2)-1
                    x = [G.X(m,n) G.X(m,n+1) G.X(m+1,n+1) G.X(m+1,n)];
                    y = [G.Y(m,n) G.Y(m,n+1) G.Y(m+1,n+1) G.Y(m+1,n)];
                    %
                    if ~any(isnan(x) | isnan(y)) && ~isnan(G.Val(m,n))
                        [wght{m,n},lidx{m,n}] = inside_raster_poly(x,y,raster_xcc,raster_ycc);
                    end
                end
                lcwaitbar(m/(size(G.X,1)-1),h)
            end
        else
            wght = cell(ncell,1);
            lidx = cell(ncell,1);
            for m = 1:ncell
                x = G.CellX{m};
                y = G.CellY{m};
                if ~any(isnan(x) | isnan(y)) && ~isnan(G.CellVal(m))
                    [wght{m},lidx{m}] = inside_raster_poly(x,y,raster_xcc,raster_ycc);
                end
                lcwaitbar(m/ncell,h)
            end
        end
        delete(h)
    case {'weighted','maxarea'}
        min_area = inifile('get',ini_info,'general','minarea',0);
        if ~isnumeric(min_area) || min_area<0 || min_area>1
            error('Invalid value for minarea')
        else
            min_area = min_area*cellarea;
        end
        %
        % create mapping
        %
        h = lcwaitbar(0,'Determining weights ...');
        if size(G.X,2)>1
            wght = cell(size(G.X)-1);
            lidx = cell(size(G.X)-1);
            for m = 1:size(G.X,1)-1
                for n = 1:size(G.X,2)-1
                    x = [G.X(m,n) G.X(m,n+1) G.X(m+1,n+1) G.X(m+1,n)];
                    y = [G.Y(m,n) G.Y(m,n+1) G.Y(m+1,n+1) G.Y(m+1,n)];
                    %
                    if ~any(isnan(x) | isnan(y)) && ~isnan(G.Val(m,n))
                        [wght{m,n},lidx{m,n}] = intersect_raster_poly(x,y,raster_xco,raster_yco);
                    end
                end
                lcwaitbar(m/(size(G.X,1)-1),h)
            end
        else
            wght = cell(ncell,1);
            lidx = cell(ncell,1);
            for m = 1:ncell
                x = G.CellX{m};
                y = G.CellY{m};
                if ~any(isnan(x) | isnan(y)) && ~isnan(G.CellVal(m))
                    [wght{m},lidx{m}] = intersect_raster_poly(x,y,raster_xco,raster_yco);
                end
                lcwaitbar(m/ncell,h)
            end
        end
        delete(h)
        drawnow
    otherwise
        error('Unsupported method: %s',method)
end
%
nwght=cellfun('length',wght);
Wght = zeros(sum(nwght(:)),3);
offset = 0;
if size(G.X,2)>1
    for m = 1:size(G.X,1)-1
        for n = 1:size(G.X,2)-1
            i = offset+(1:nwght(m,n));
            if ~isempty(i)
                Wght(i,1) = wght{m,n};
                Wght(i,2) = lidx{m,n};
                Wght(i,3) = G.Val(m,n);
                offset = i(end);
            end
        end
    end
else
    for m = 1:ncell
        i = offset+(1:nwght(m));
        if ~isempty(i)
            Wght(i,1) = wght{m};
            Wght(i,2) = lidx{m};
            Wght(i,3) = G.CellVal(m);
            offset = i(end);
        end
    end
end
%
switch method
    case 'maxarea'
        % choose the maximum area
        [Ws,I]=sort(Wght(:,2));
        Wght=Wght(I,:);
        dW=find(diff([0;Wght(:,2);0]));
        num_dW=length(dW)-1;
        h = lcwaitbar(0,'Locating maximum weights ...');
        for ix=1:num_dW
            W=Wght(dW(ix):dW(ix+1)-1,:);
            NoData=cellarea-sum(W(:,1));
            %
            if size(W,1)>1
                [c,ia,ic]=unique(W(:,3));
                szc=size(c);
                W=[accumarray(ic,W(:,1),size(c)) repmat(W(1,2),szc) c];
                [mx,im]=max(W(:,1));
                W=W(im,:);
            end
            %
            Wght(dW(ix):dW(ix+1)-1,:)=NaN;
            if W(1)>NoData
                Wght(dW(ix),:)=W;
            end
            lcwaitbar(ix/num_dW,h)
        end
        delete(h)
        drawnow
        Wght(isnan(Wght(:,1)),:)=[];
    case 'center'
        % check if only single value is used
        if any(diff(sort(Wght(:,2)))==0)
            warning('Raster cell centre coincides with simulation cell edge: using average value');
        end
end
% check if there is any overlap between input grid and output raster
if sum(Wght) == 0
    error('No overlap between the input grid and output raster found!')
end
%
% process data sets
%
idataset  = 0;
fprintf(1,'Processing data sets ...\n');
h = lcwaitbar(0,'Processing data sets ...');
for ifld = 1:nchp
    %
    % get data
    %
    iuq = chp{ifld,2};
    if isempty(iuq)
        continue
    end
    qstr = uqnts(iuq);
    tmop = chp{ifld,1}; % time_operator
    iqnt = chp{ifld,3};
    info = chp{ifld,4};
    %
    nq = length(iqnt);
    %
    iwaq = iqnt>0;
    iflw = find(~iwaq);
    iqnt_waq = iqnt(iwaq);
    idwq = [waq_qnt(iqnt_waq).Val1];
    iqnt_flw = -iqnt(~iwaq);
    %
    ntotstep = info.tstop - info.tstart + 1;
    np = floor((ntotstep-info.ntstep)/info.tshift)+1;
    tPeriods = cell(np,1);
    for ip = 1:np
        tPeriods{ip} = info.tstart + (ip-1)*info.tshift + [0 info.ntstep-1];
    end
    %
    switch tmop
        case {'ident','max','min','percentile'}
            missing_value = NaN;
        otherwise
            missing_value = 0;
    end
    %
    for iper = 1:length(tPeriods)
        itstart = tPeriods{iper}(1);
        itstop  = tPeriods{iper}(2);
        itstep  = info.ntskip+1;
        ntim    = 0;
        %
        if strcmp(tmop,'percentile')
            tmopstr = sprintf('%s_%i',tmop,info.prctl);
        else
            tmopstr = tmop;
        end
        if itstep==1
            tmopstr = sprintf('%s_%i-%i',tmopstr,itstart,itstop);
        else
            itstop = itstart + itstep*floor((itstop-itstart)/itstep);
            if itstop>itstart
                tmopstr = sprintf('%s_%i-%i-step%i',tmopstr,itstart,itstop,itstep);
            else
                tmopstr = sprintf('%s_%i',tmopstr,itstart);
            end
        end
        switch tmop
            case {'max','min'}
                DATA = NaN;
            case 'percentile'
                DATA  = [];
            otherwise
                DATA  = 0;
                DATA2 = 0;
        end
        %
        fprintf(1,'  time selection "%s":\n',tmopstr);
        for i = 1:nq
            fprintf(1,'    quantity "%s"\n',qstr{i});
        end
        %
        for it = itstart:itstep:itstop
            if strcmp(tmop,'ident')
                tmopstr = sprintf('time_step_%i',it);
            end
            if any(iwaq)
                [t,DATA_t] = delwaq('read',dwq,idwq,0,it);
                iMissing = DATA_t==-999;
                if nLayers_WAQ>1
                    % 3D quantity
                    if info.layer == LAYER_BOTTOM_MOST
                        nqwq = size(DATA_t,1);
                        %
                        % Determine segment thickness
                        %
                        [t,LDP_t] = delwaq('read',dwq,info.ildp,0,it);
                        LDP_t(LDP_t==-999) = 0;
                        LDP_t = reshape(LDP_t,[nSeg2D nLayers_WAQ]);
                        LDP_t(:,2:end) = diff(LDP_t,1,2);
                        %
                        % Mark cells with zero total depth as missing.
                        %
                        TDP_t = sum(LDP_t,2);
                        zeroDepth = TDP_t==0;
                        iMissing2D = false(nqwq,nSeg2D);
                        iMissing2D(:,zeroDepth) = true;
                        %
                        DATA_t = reshape(DATA_t,[nqwq nSeg2D nLayers_WAQ]);
                        DATA2D_t = repmat(missing_value,[nqwq nSeg2D]);
                        for lyr = 1:nLayers_WAQ % from top to bottom
                            activeCells = LDP_t(:,lyr)>0;
                            DATA2D_t(:,activeCells) = DATA_t(:,activeCells,lyr);
                        end
                        %
                        DATA_t   = DATA2D_t;
                        iMissing = iMissing2D;
                    elseif info.layer == LAYER_DEPTH_AVERAGE
                        %
                        % Determine segment thickness
                        %
                        [t,LDP_t] = delwaq('read',dwq,info.ildp,0,it);
                        LDP_t(LDP_t==-999) = 0;
                        LDP_t = reshape(LDP_t,[nSeg2D nLayers_WAQ]);
                        LDP_t(:,2:end) = diff(LDP_t,1,2);
                        %
                        % Determine total waterdepth (set to 1 if 0 to avoid
                        % division by zero).
                        %
                        TDP_t = sum(LDP_t,2); % more efficient to store LDP_t(:,end) before doing diff ...
                        zeroDepth = TDP_t==0;
                        TDP_t(zeroDepth) = 1;
                        %
                        % Assuming that missing data value occurs in all layers
                        % such that depth average is equal to missing data
                        % value again. Will it be so exactly?
                        %
                        for iq = size(DATA_t,1):-1:1
                            DATA2D_t(iq,:)   = (sum(reshape(DATA_t(iq,:)  .*LDP_t(:)',size(LDP_t)),2)./TDP_t).';
                            iMissing2D(iq,:) = (sum(reshape(iMissing(iq,:).*LDP_t(:)',size(LDP_t)),2)./TDP_t).'>0;
                            iMissing2D(iq,zeroDepth) = true;
                        end
                        %
                        DATA_t   = DATA2D_t;
                        iMissing = iMissing2D;
                    else
                        slice    = (info.layer-1)*nSeg2D + (1:nSeg2D);
                        DATA_t   = DATA_t(:,slice);
                        iMissing = iMissing(:,slice);
                    end
                else
                    % 2D quantity
                end
                DATA_t(iMissing) = missing_value;
            end
            if any(~iwaq)
                if any(iwaq)
                    % expand DATA_t for flow fields
                    DATA_t(iwaq,:) = DATA_t;
                    iMissing(iwaq,:) = iMissing;
                end
                for i = length(iflw):-1:1
                    if iscell(flw_info)
                        val = NaN(1,ncell);
                        for ipar = 1:length(flw_info)
                            X      = qpread(flw_info{ipar,1},flw_qnt(iqnt_flw(i)),'data',it,0);
                            GlobNr = flw_info{ipar,2};
                            PartNr = flw_info{ipar,3};
                            val(GlobNr(PartNr==ipar-1)) = X.Val(PartNr==ipar-1);
                        end
                    else
                        flw_data = qpread(flw_info,flw_qnt(iqnt_flw(i)),'data',it,0);
                        if isfield(flw_data,'XComp') || isfield(flw_data,'YComp')
                            val = sqrt(power(flw_data.XComp, 2) + power(flw_data.XComp, 2));
                        elseif isfield(flw_data,'Val')
                            val = flw_data.Val;
                        else
                            error('Error reading Flow data')
                        end
                        % check if it is 3D data
                        % no 3D depth/active/inactive info, s use a simple approach
                        flw_ndims = ndims(val);
                        if flw_is_structured && flw_ndims == 3
                            if info.layer == LAYER_BOTTOM_MOST
                                val = val(:,:,1);
                            elseif info.layer == LAYER_DEPTH_AVERAGE
                                % just a non weighted average
                                val = mean(val,3);
                            else
                                val = val(:,:,info.layer);
                            end
                        end
                        % reshape data to N columns, 1 row
                        val = reshape(val',[],1)';
                        % apply active cells mask in case it was found (structured only)
                        if (flw_use_mask)
                            val = val(~isnan(flw_mask));
                        end
                    end
                    %
                    iMissing(iflw(i),:) = isnan(val);
                    val(iMissing) = missing_value;
                    %
                    DATA_t(iflw(i),:) = val;
                end
            end
            %
            for i = 1:nq
                mask = isnan(DATA_t(i,:));
                DATA_t(i,:) = min(max(qntprp(iuq(i)).lowerbound,DATA_t(i,:)),qntprp(iuq(i)).upperbound);
                DATA_t(i,mask) = NaN;
            end
            %
            switch tmop
                case 'ident'
                    DATA   = DATA_t;
                case 'mean'
                    DATA = DATA + DATA_t;
                    ntim = ntim + ~iMissing;
                    if it==itstop
                        DATA = DATA./max(1,ntim);
                        DATA(ntim==0) = NaN;
                    end
                case 'max'
                    DATA = max(DATA,DATA_t);
                    ntim = ntim + ~iMissing;
                    if it==itstop
                        DATA(ntim==0) = NaN;
                    end
                case 'min'
                    DATA = min(DATA,DATA_t);
                    ntim = ntim + ~iMissing;
                    if it==itstop
                        DATA(ntim==0) = NaN;
                    end
                case 'std'
                    DATA  = DATA  + DATA_t;
                    DATA2 = DATA2 + DATA_t.^2;
                    ntim  = ntim  + iMissing;
                    if it==itstop
                        DATA = sqrt(DATA2 - (DATA.^2)./max(1,ntim))./max(1,ntim-1);
                        DATA(ntim==0) = NaN;
                    end
                case 'percentile'
                    DATA  = [DATA;DATA_t];
                    ntim  = ntim  + ~iMissing;
                    if it==itstop
                        DATA = compute_percentile(DATA, info.prctl);
                        DATA(ntim==0) = NaN;
                    end
            end
            %
            if it==itstop || strcmp(tmop,'ident')
                for i = 1:nq
                    data = accumarray(Wght(:,2),Wght(:,1).*DATA(i,Wght(:,3))',[prod(raster_sz) 1]);
                    wght = accumarray(Wght(:,2),Wght(:,1),[prod(raster_sz) 1]);
                    if min_area>0
                        data(wght<min_area)=0;
                        wght(wght<min_area)=0;
                    end
                    data = reshape(data./wght,raster_sz);
                    %
                    if yflip
                        data = flipud(data);
                    end
                    %
                    if strcmp(info.ogrid,'')
                        filename = fullfile(outdir,sprintf('%s.%s',qstr{i},tmopstr));
                    else
                        filename = info.ogrid;
                    end
                    switch raster_info.FileType
                        case 'HDR Raster File - BIP/BIL/BSQ'
                            bil('write',filename,raster_info,data);
                        case 'arcgrid'
                            FD = raster_info;
                            FD.Data = data';
                            FD.Comment = [];
                            arcgrid('write',FD,[filename '.asc']);
                    end
                    idataset = idataset+1;
                    lcwaitbar(idataset/ndatasets,h)
                    drawnow
                end
            end
        end
    end
    %
end
delete(h)
fprintf(1,'Finished.\n');


function x=lcwaitbar(varargin)
persistent alpha
if nargout==1
    x = waitbar(varargin{:});
    alpha = 0.01;
else
    a = varargin{1};
    if a==1 || a>=alpha
        alpha = alpha+0.01;
        waitbar(a,varargin{2:end})
    end
end

function [iselqnt,selqnt] = filter_qnt(selqnt,waq_qnt,flw_qnt)
if ~iscell(selqnt)
    selqnt = {selqnt};
end
selqnt = unique(selqnt);
lselqnt = lower(selqnt);
%
% Find WAQ quantity case insenstive
%
[mem,idx1]=ismember(lselqnt,lower({waq_qnt.Name})');
[mem,idx2]=ismember(lselqnt,lower({waq_qnt.ShortName})');
iselqnt=max(idx1,idx2);
%
if ~isempty(flw_qnt)
    %
    % Find FLOW quantity case insensitive
    %
    [mem,idx3]=ismember(lselqnt,lower({flw_qnt.Name})');
    iselqnt(idx3>0)=-idx3(idx3>0);
end

function [area,lidx] = inside_raster_poly(x,y,raster_xcc,raster_ycc)
col = find(min(x)<raster_xcc & max(x)>raster_xcc);
row = find(min(y)<raster_ycc & max(y)>raster_ycc);
nrowt = length(raster_ycc);
%
[xcc,ycc]=ndgrid(raster_xcc(col),raster_ycc(row));
[col,row]=ndgrid(col,row);
area = inpolygon(xcc,ycc,x,y);
lidx = row + (col-1)*nrowt;
chk = area>0;
area = area(chk);
lidx = lidx(chk);

function [area,lidx] = intersect_raster_poly(x,y,raster_xco,raster_yco)
col = find(min(x)<raster_xco(2:end) & max(x)>raster_xco(1:end-1));
ncol = length(col);
row = find(min(y)<raster_yco(2:end) & max(y)>raster_yco(1:end-1));
nrow = length(row);
%
if ncol==0 || nrow==0
    area = [];
    lidx = [];
    return
end
%
col = col(1):col(end)+1;
row = row(1):row(end)+1;
nrowt = length(raster_yco)-1;
%
% figure
% plot(x([1:end 1]),y([1:end 1]))
% set(gca,'ytick',raster_yco(row))
% set(gca,'xtick',raster_xco(col))
% grid on
% hold on
%
area = zeros(nrow,ncol);
lidx = zeros(nrow,ncol);
dx = diff(raster_xco);
dy = diff(raster_yco);
for n = row(1:end-1)
    for m = col(1:end-1)
        in = n-row(1)+1;
        im = m-col(1)+1;
        area(in,im) = intersect_cell_poly((x-raster_xco(m))/dx(m),(y-raster_yco(n))/dy(n))*dx(m)*dy(n);
        lidx(in,im) = n + (m-1)*nrowt;
    end
end
chk  = area>1e-3;
area = area(chk);
lidx = lidx(chk);

function area = intersect_cell_poly(x,y)
area = zeros(1,length(x));
for i = 1:length(x)
    x0 = x(i);
    y0 = y(i);
    if i<length(x)
        x1 = x(i+1);
        y1 = y(i+1);
    else
        x1 = x(1);
        y1 = y(1);
    end
    area(i) = intersect_cell_tri(x0,y0,x1,y1);
end
area = abs(sum(area));

function area = intersect_cell_tri(x0,y0,x1,y1)
%   1 ____
%    |    |       tri (0,0)-(x0,y0)-(x1,y1)
%    |____|
%   0      1
%
if x0<=0 && x1<=0
    % whole triangle to the left of the cell
    area = 0;
elseif y0<=0 && y1<=0
    % whole triangle beneath cell
    area = 0;
else
    % triangle may overlap with cell
    % intersect (0,0)-(x0,y0) with (1,0)-(1,1) ==> (1,y2)
    y2 = y0/x0; % 0 + (y0-0)*(1-0)/(x0-0);
    % intersect (0,0)-(x0,y0) with (0,1)-(1,1) ==> (x2,1)
    x2 = x0/y0; % 0 + (x0-0)*(1-0)/(y0-0);
    %
    % intersect (x0,y0)-(x1,y1) with (0,1)-(1,1) ==> (x3,1)
    x3 = x0 + (x1-x0)*(1-y0)/(y1-y0);
    % intersect (x0,y0)-(x1,y1) with (1,0)-(1,1) ==> (1,y3)
    y3 = y0 + (y1-y0)*(1-x0)/(x1-x0);
    % intersect (x0,y0)-(x1,y1) with (0,0)-(1,0) ==> (x3z,0)
    x3z = x0 + (x1-x0)*(0-y0)/(y1-y0);
    % intersect (x0,y0)-(x1,y1) with (0,0)-(0,1) ==> (0,y3z)
    y3z = y0 + (y1-y0)*(0-x0)/(x1-x0);
    %
    % intersect (0,0)-(x1,y1) with (1,0)-(1,1) ==> (1,y4)
    y4 = y1/x1; %0 + (y1-0)*(1-0)/(x1-0);
    % intersect (0,0)-(x1,y1) with (0,1)-(1,1) ==> (x4,1)
    x4 = x1/y1; %0 + (x1-0)*(1-0)/(y1-0);
    %
    xa = [0 NaN 1 NaN 1 NaN];
    ya = [0 NaN 1 NaN 1 NaN];
    %     idx = idx+1;
    %     if ismember(idx,39)
    %         figure; plot([0 0 1 1 0],[0 1 1 0 0],'b:',[0 x0 x1 0],[0 y0 y1 0],'b',x0,y0,'b*');
    %     end
    %
    if (x0<0 && y3z<0) || (y0<0 && x3z>=0)
        if x3z>1 && x0>=1 && x1<1
            xa(2)=1;
            ya(2)=0;
            ya(3)=max(0,min(y3,1));
        else
            xa(3)=max(0,min(x3z,1));
            ya(3)=0;
        end
    elseif x0<0 || y0<0
        if y3z>1 && y0>=1 && y1<1
            xa(2)=0;
            ya(2)=1;
            xa(3)=max(0,min(x3,1));
        else
            xa(3)=0;
            ya(3)=max(0,min(y3z,1));
        end
    elseif x2>=1 && x0>1
        xa(2)=1;
        ya(2)=y2;
        if x1<1
            if y3>1 && x3>0 && x3<1
                xa(4)=x3;
                ya(4)=1;
            else
                ya(3)=max(0,min(y3,1));
            end
        end
    elseif y2>=1 && y0>1
        xa(2)=x2;
        ya(2)=1;
        if y1<1
            if x3>1 && y3>0 && y3<1
                xa(4)=1;
                ya(4)=y3;
            else
                xa(3)=max(0,min(x3,1));
            end
        end
    else
        xa(3)=x0;
        ya(3)=y0;
    end
    %
    if (x1<0 && y3z<0) || (y1<0 && x3z>=0)
        if x3z>1 && x1>=1 && x0<1
            xa(6)=1;
            ya(6)=0;
            ya(5)=max(0,min(y3,1));
        else
            xa(5)=max(0,min(x3z,1));
            ya(5)=0;
        end
    elseif x1<0 || y1<0
        if y3z>1 && y1>=1 && y0<1
            xa(6)=0;
            ya(6)=1;
            xa(5)=max(0,min(x3,1));
        else
            xa(5)=0;
            ya(5)=max(0,min(y3z,1));
        end
    elseif x4>=1 && x1>1
        xa(6)=1;
        ya(6)=y4;
        if x0<1
            if y3>1 && x3>0 && x3<1
                xa(4)=x3;
                ya(4)=1;
            else
                ya(5)=max(0,min(y3,1));
            end
        end
    elseif y4>=1 && y1>1
        xa(6)=x4;
        ya(6)=1;
        if y0<1
            if x3>1 && y3>0 && y3<1
                xa(4)=1;
                ya(4)=y3;
            else
                xa(5)=max(0,min(x3,1));
            end
        end
    else
        xa(5)=x1;
        ya(5)=y1;
    end
    %
    eq=0;
    ip=1;
    for i=2:5
        if isnan(xa(i))
            continue
        elseif eq==1
            if xa(i)==xa(ip)
                xa(ip)=NaN;
                ya(ip)=NaN;
                ip=i;
                continue
            end
        elseif eq==2
            if ya(i)==ya(ip)
                xa(ip)=NaN;
                ya(ip)=NaN;
                ip=i;
                continue
            end
        end
        %
        if xa(i)==xa(ip) && ya(i)==ya(ip)
            xa(ip)=NaN;
            ya(ip)=NaN;
        elseif xa(i)==xa(ip)
            eq = 1;
        elseif ya(i)==ya(ip)
            eq = 2;
        else
            eq = 0;
        end
        ip = i;
    end
    xa(isnan(xa))=[];
    ya(isnan(ya))=[];
    %area=polyarea(xa,ya)*clockwise([0 x0 x1],[0 y0 y1]);
    area=-sum(xa(2:end-1).*(ya(3:end)-ya(2:end-1))-ya(2:end-1).*(xa(3:end)-xa(2:end-1)))/2;
    %
    %     if ismember(idx,39)
    %         figure; plot([0 0 1 1 0],[0 1 1 0 0],'b:',[0 x0 x1 0],[0 y0 y1 0],'b',[xa xa(1)],[ya ya(1)],'r',x0,y0,'b*');
    %         title(sprintf('%g',area))
    %     end
end
