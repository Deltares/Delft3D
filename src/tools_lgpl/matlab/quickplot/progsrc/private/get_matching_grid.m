function [G,GridFileName]=get_matching_grid(MapSeg,pn,filterspec)
%GET_MATCHING_GRID Get grid file that matches size of current dataset.
%   [GRIDINFO,GRIDFILENAME] = GET_MATCHING_GRID(MAPSEG) opens a dialog to
%   select a file with MAPSEG elements in the (aggregated) grid.
%
%   [GRIDINFO,GRIDFILENAME] = GET_MATCHING_GRID(GRIDSIZE) opens a dialog to
%   select a grid specified GRIDSIZE.

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

GridSeg=-1;
PerLayer=0;
filters = {'*.cco;*.lga;*.m2b;*.geo;geo*;*.slf;T2DD12;*.dwq;*.nc;*.shp' 'Any Supported Grid Files'
    '*.cco;*.lga'      'Delft3D Grid (Aggregation) Files'
    '*.m2b'            'SOBEK Grid Aggregation Files'
    '*.geo;geo*;*.slf' 'Telemac Grid Files'
    'T2DD12;*.dwq'     'DIDO Aggregation File (for Telemac)'
    '*.nc'             'UGRID netCDF Files (D-Flow FM, Untrim)'
    '*.shp'            'Shape File'
    '*.*'              'All Files'};
telemacfilter = filters(4,:);
if nargin<3
    filterspec = '';
end
first = 1;
semicol = findstr(filterspec,';');
if ~isempty(semicol)
    allfilterspec = splitcellstr(filterspec,';');
    filterspec = absfullfile(allfilterspec{1});
else
    allfilterspec = {};
end
while 1
    if ~first
        allfilterspec={};
    end
    if exist(filterspec)==2
        if first
            GridFileName=filterspec;
            first = 0;
        else
            error('Specified grid file can''t be read or doesn''t match data file.\n%s',filterspec)
        end
    else
        cp=pwd;
        cd(pn);
        [gfn,gpn]=uigetfile(filters,'Select matching grid file ...');
        cd(cp);
        if ~ischar(gfn)
            G=[];
            GridFileName='';
            break
        end
        GridFileName=[gpn gfn];
    end
    [pn,fn,ex]=fileparts(GridFileName);
    trytp = lower(ex);
    switch trytp
        case {'.lga','.cco'}
            trytp = '.lga';
        case {'.m2b','.arc'}
            trytp = '.m2b';
        case {'.slf','.geo'}
            trytp = '.geo';
        case {'.grd','.geo','.nc','.dwq'} % single extension cases
            %trytp = trytp;
        otherwise
            trytp = '.lga';
    end
    trytp0 = trytp;
    CouldReadGridData = 0;
    GetError = 0;
    try
    while 1
        GridsChecked = {};
        GridSize=[NaN NaN];
        GridSeg=NaN;
        switch trytp
            case '.lga'
                try
                    G=delwaq('openlga',GridFileName);
                    GridSeg=G.NoSeg;
                    PerLayer=G.NoSegPerLayer;
                    CouldReadGridData = 1;
                catch Ex
                    if GetError
                        rethrow(Ex)
                    end
                    trytp = '.shp';
                end
            case '.shp'
                try
                    G=shape('open',GridFileName);
                    G.AggregationFld=ustrcmpi('DWAQ_AGG',{G.dBase.Fld.Name});
                    if G.AggregationFld>0
                        Agg=dbase('read',G.dBase,0,G.AggregationFld);
                        G.Index=Agg{1};
                        GridSeg=max(G.Index);
                    else
                        GridSeg=G.NShapes;
                        G.Index=(1:GridSeg)';
                    end
                    G.MNK=[G.NShapes 1];
                    PerLayer=GridSeg;
                    CouldReadGridData = 1;
                catch Ex
                    if GetError
                        rethrow(Ex)
                    end
                    trytp = '.grd';
                end
            case '.grd'
                try
                    G=wlgrid('open',GridFileName);
                    GridSize=size(G.X);
                    CouldReadGridData = 1;
                catch Ex
                    if GetError
                        rethrow(Ex)
                    end
                    trytp = '.m2b';
                end
            case '.m2b'
                try
                    G=arcgrid('read',GridFileName);
                    GridSeg=max(G.Data(:));
                    PerLayer=GridSeg;
                    G.MNK=[size(G.Data) 1];
                    G.Index=G.Data;
                    G.Index(isnan(G.Data))=0;
                    CouldReadGridData = 1;
                catch Ex
                    if GetError
                        rethrow(Ex)
                    end
                    trytp = '.nc';
                end
            case '.nc'
                try
                    G = nc_info(GridFileName);
                    G = nc_interpret(G);
                    %
                    matchingAggrTable=false(length(G.Dataset),1);
                    for i=1:length(G.Dataset)
                        for j=1:length(G.Dataset(i).Attribute)
                            if strcmp(G.Dataset(i).Attribute(j).Name,'delwaq_role') && ...
                                    strcmp(G.Dataset(i).Attribute(j).Value,'segment_aggregation_table')
                                table = nc_varget(G.Filename,G.Dataset(i).Name);
                                numSeg = max(table(:));
                                GridsChecked{end+1} = sprintf('Aggregation table "%s", highest segment number = %i',G.Dataset(i).Name,numSeg);
                                if numSeg==MapSeg
                                    matchingAggrTable(i) = true;
                                end
                            end
                        end
                    end
                    %
                    x = 0;
                    y = 0;
                    xbounds = 0;
                    ybounds = 0;
                    %
                    if any(matchingAggrTable)
                        % that this aggregation table matches was already checked above
                        C = [];
                        iAggr = find(matchingAggrTable);
                        iAggr = iAggr(1); % select first aggregation table
                        table = nc_varget(G.Filename,G.Dataset(iAggr).Name);
                        if numel(table)==length(table)
                            GridSeg = MapSeg;
                        else
                            GridSeg = min(max(table,[],2)); % should be recoded, because it could be any dimension
                        end
                        if ~isempty(G.Dataset(iAggr).Mesh)
                            m = G.Dataset(iAggr).Mesh;
                            mInfo = G.Dataset(m{2});
                            mAttribs = mInfo.Attribute;
                            mAttribNames = {mAttribs.Name};
                            %
                            j = strmatch('long_name',mAttribNames,'exact');
                            if ~isempty(j) && ~isempty(mAttribs(j).Value)
                                Name = mAttribs(j).Value;
                            elseif ~isempty(standard_name)
                                Name = standard_name;
                            else
                                Name = mInfo.Name;
                            end
                            %
                            G.Mesh = [m {Name}];
                            switch G.Mesh{1}
                                case 'ugrid'
                                    x = G.Dataset(G.Mesh{2}).X;
                                    y = G.Dataset(G.Mesh{2}).Y;
                                    XDimensions = G.Dataset(G.Mesh{2}).Mesh(4+G.Mesh{3});
                            end
                        else
                            xbounds = G.Dataset(iAggr).XBounds;
                            ybounds = G.Dataset(iAggr).YBounds;
                            x = G.Dataset(iAggr).X;
                            y = G.Dataset(iAggr).Y;
                            XDimensions = G.Dataset(x).Dimension;
                        end
                        G.Aggregation = G.Dataset(iAggr).Name;
                        G.AggregationDims = [XDimensions setdiff(G.Dataset(iAggr).Dimension,XDimensions)];
                        G.MNK=[GridSeg 1 MapSeg/GridSeg];
                        G.Index=(1:GridSeg)';
                    else
                        % check for UGRID
                        C = cell(0,4);
                        for i=1:length(G.Dataset)
                            if strcmp(G.Dataset(i).Type,'ugrid_mesh')
                                    nNodeDim = G.Dataset(i).Mesh{5};
                                nNodes = G.Dimension(ustrcmpi(nNodeDim,{G.Dimension.Name})).Length;
                                GridsChecked{end+1} = sprintf('UGRID mesh "%s", number of nodes = %i',G.Dataset(i).Name,nNodes);
                                if G.Dataset(i).Mesh{2}==2 %2D
                                    nFaceDim = G.Dataset(i).Mesh{7};
                                    nFaces = G.Dimension(ustrcmpi(nFaceDim,{G.Dimension.Name})).Length;
                                    GridsChecked{end+1} = sprintf('UGRID mesh "%s", number of faces = %i',G.Dataset(i).Name,nFaces);
                                else
                                    nFaces = 0;
                                end
                                % check if nNodes or nFaces matches
                                if round(MapSeg/nNodes)==MapSeg/nNodes
                                    % possibly data at nodes
                                    C{end+1,1} = i;
                                    C{end,2} = 0; % nodes
                                    C{end,3} = nNodeDim;
                                    C{end,4} = nNodes;
                                end
                                if nFaces>0 && round(MapSeg/nFaces)==MapSeg/nFaces
                                   % possibly data at faces
                                    C{end+1,1} = i;
                                    C{end,2} = 2; % faces
                                    C{end,3} = nFaceDim;
                                    C{end,4} = nFaces;
                                end
                            end
                        end
                        %
                        if ~isempty(C)
                            % data seems to match one or more UGRID meshes
                            if size(C,1)>1
                                % just select the first one ...
                                C = C(1,:);
                            end
                            GridSeg = C{1,4};
                            %
                            Info = G.Dataset(C{1,1});
                            Attribs = {Info.Attribute.Name};
                            j = strmatch('standard_name',Attribs,'exact');
                            if ~isempty(j)
                                standard_name = Info.Attribute(j).Value;
                            else
                                standard_name = '';
                            end
                            j = strmatch('long_name',Attribs,'exact');
                            if ~isempty(j) && ~isempty(Info.Attribute(j).Value)
                                Name = Info.Attribute(j).Value;
                            elseif ~isempty(standard_name)
                                Name = standard_name;
                            else
                                Name = Info.Name;
                            end
                            switch C{1,2}
                                case 0
                                    Name = [Name ' - node indices'];
                                case 1
                                    Name = [Name ' - edge indices'];
                                case 2
                                    Name = [Name ' - face indices'];
                            end
                            %
                            G.Mesh=[{'ugrid'} C(1,1:2) {Name}];
                            G.MNK=[GridSeg 1 MapSeg/GridSeg];
                            G.Index=(1:GridSeg)';
                        else
                            C = cell(0,4);
                            for i=1:length(G.Dataset)
                                for j=1:length(G.Dataset(i).Attribute)
                                    if strcmp(G.Dataset(i).Attribute(j).Name,'bounds')
                                        C{end+1,1} = G.Dataset(i).Attribute(j).Value;
                                        C{end,2} = G.Dataset(i).Name;
                                        C{end,3} = G.Dataset(i).Dimension;
                                        C{end,4} = G.Dataset(i).Size;
                                        GridsChecked{end+1} = sprintf('Variable "%s", size = %s',G.Dataset(i).Name,var2str(G.Dataset(i).Size));
                                    end
                                end
                            end
                            %
                            if ~isempty(C)
                                matching = 0;
                                for i=1:size(C,1)
                                    if isequal(C{i,4},MapSeg)
                                        matching = i;
                                        break
                                    end
                                end
                                %
                                if ~matching
                                    matching = 0;
                                    for i=1:size(C,1)
                                        K = MapSeg/C{i,4};
                                        if isequal(K,round(K))
                                            matching = i;
                                            break
                                        end
                                    end
                                end
                                %
                                if ~matching
                                    GridsChecked = sprintf('  %s\n',GridsChecked{:});
                                    GridsChecked(end) = [];
                                    error('Unable to identify coordinate bounds variables for %i segments.\nVariables checked:\n%s',MapSeg,GridsChecked)
                                else
                                    segdim = C{matching,3};
                                    GridSeg = C{matching,4};
                                    %
                                    matching = false(size(C,1),1);
                                    for i=1:size(C,1)
                                        matching(i) = isequal(C{i,3},segdim);
                                    end
                                    C = C(matching,:);
                                    %
                                    % locate x/longitude coordinate
                                    %
                                    ytype='';
                                    for i=1:size(C,1)
                                        j=strmatch(C{i,2},{G.Dataset.Name},'exact');
                                        switch G.Dataset(j).Type
                                            case 'longitude'
                                                ytype = 'latitude';
                                                break
                                            case 'x-coordinate'
                                                ytype = 'y-coordinate';
                                                break
                                        end
                                    end
                                    if isempty(ytype)
                                        error('Unable to identify x-coordinate/longitude for segments')
                                    else
                                        x = j;
                                        xbounds = strmatch(C{i,1},{G.Dataset.Name},'exact');
                                    end
                                    %
                                    % locate y/latitude coordinate
                                    %
                                    y=[];
                                    for i=1:size(C,1)
                                        j=strmatch(C{i,2},{G.Dataset.Name},'exact');
                                        switch G.Dataset(j).Type
                                            case ytype
                                                y = j;
                                                ybounds = strmatch(C{i,1},{G.Dataset.Name},'exact');
                                        end
                                    end
                                    if isempty(y)
                                        error('Unable to identify %s for segments',ytype)
                                    end
                                end
                            else
                                % no matching ... attribute table, UGRID, bounds variable
                                if isempty(GridsChecked)
                                    GridsChecked = sprintf('  %s\n',GridsChecked{:});
                                    GridsChecked(end) = [];
                                else
                                    GridsChecked = '  no aggregation table, ugrid mesh, or coordinate variable with bounds found.';
                                end
                                error('Unable to identify coordinate bounds variables for %i segments.\nVariables checked:\n%s',MapSeg,GridsChecked)
                            end
                            G.MNK=[GridSeg 1 1];
                            G.Index=(1:GridSeg)';
                        end
                    end
                    if xbounds~=0
                        G.BCoordinates = {G.Dataset(xbounds).Name G.Dataset(ybounds).Name};
                        XBoundsDimensions = G.Dataset(xbounds).Dimension;
                    else
                        XBoundsDimensions = {};
                    end
                    if x~=0
                        G.CCoordinates = {G.Dataset(x).Name G.Dataset(y).Name};
                        XDimensions = G.Dataset(x).Dimension;
                    elseif ~isempty(C)
                        XDimensions = C(1,3);
                        x = G.Dataset(C{1,1}).X;
                    end
                    G.CoordDims = [intersect(XDimensions,XBoundsDimensions) setdiff(XBoundsDimensions,XDimensions)];
                    G.FileType = 'netCDF';
                    %
                    for j = 1:length(G.Dataset(x).Attribute)
                        if isequal(G.Dataset(x).Attribute(j).Name,'units')
                            switch G.Dataset(x).Attribute(j).Value
                                case {'degrees_east','degree_east','degreesE','degreeE', ...
                                        'degrees_north','degree_north','degreesN','degreeN'}
                                    G.Unit = 'deg';
                                otherwise
                                    G.Unit = G.Dataset(x).Attribute(j).Value;
                            end
                        end
                    end
                    %
                    try
                        if strcmp(G.Filename(end-10:end),'_waqgeom.nc')
                            dpsfil = [G.Filename(1:end-11) '.dps'];
                            dps = waqfil('open',dpsfil);
                            G.DPS = dps.Srf;
                        end
                    catch
                    end
                    CouldReadGridData = 1;
                catch Ex
                    if GetError
                        rethrow(Ex)
                    end
                    trytp = '.geo';
                end
            case '.geo'
                try
                    G=telemac('open',GridFileName);
                    GridSeg=G.Discr.NPnts;
                    PerLayer=GridSeg;
                    G.MNK=[GridSeg 1 1];
                    G.Index=(1:GridSeg)';
                    CouldReadGridData = 1;
                catch Ex
                    if GetError
                        rethrow(Ex)
                    end
                    trytp = '.dwq';
                end
            case '.dwq'
                try
                    DWQ=open_dwq(GridFileName);
                    NumNode = length(DWQ.AggrTable);
                    GridSeg = length(unique(DWQ.AggrTable));
                    PerLayer = GridSeg;
                    %
                    if MapSeg==GridSeg
                        if length(allfilterspec)>=2
                            gridfil2 = absfullfile(allfilterspec{2});
                        else
                            cp=pwd;
                            cd(pn);
                            [gfn,gpn]=uigetfile(telemacfilter,'Select Telemac grid file ...');
                            cd(cp);
                            if ~ischar(gfn)
                                G=[];
                                break
                            end
                            gridfil2=[gpn gfn];
                        end
                        G = telemac('open',gridfil2);
                        if NumNode==G.Discr.NPnts
                            G.MNK=[NumNode 1 1];
                            GridFileName = [GridFileName ';' G.FileName];
                            %
                            % Renumber boundary neighbours
                            %
                            BND = find(DWQ.AggrTable<0);
                            DWQ.AggrTable(BND) = max(DWQ.AggrTable)-DWQ.AggrTable(BND);
                            %
                            G.Index=DWQ.AggrTable;
                            G.AggrFile=DWQ.FileName;
                            CouldReadGridData = 1;
                        end
                    end
                catch Ex
                    if GetError
                        rethrow(Ex)
                    end
                    trytp = '.lga';
                end
        end
        if CouldReadGridData || GetError
            break
        end
        if isequal(trytp,trytp0) % gone full circle
            GetError = 1;
        end
    end
    catch Ex
        qp_error('Catch in get_matching_grid:',Ex,'d3d_qp_core')
    end
    if CouldReadGridData
        if length(MapSeg)==1
            %
            % Number of segments given.
            %
            if MapSeg==GridSeg || ... % exact match for ordinary map files
                    MapSeg==GridSeg+PerLayer % one shift for PART map files with sediment layer
                break
            elseif round(MapSeg/GridSeg)==MapSeg/GridSeg && GridSeg>0 % data amount matches multiple layers
                G.MNK(3) = MapSeg/GridSeg;
                G.Index = repmat(G.Index,[1 1 G.MNK(3)]);
                for k=2:G.MNK(3)
                    G.Index(:,:,k)=G.Index(:,:,k)+(k-1)*GridSeg;
                end
                break
            else
                ui_message('error','Number of segments in map file (%i) does not\nmatch the number of segments in the\ngrid file (%i)',MapSeg,GridSeg);
            end
        else
            %
            % Grid dimensions given.
            %
            if isequal(GridSize,MapSeg)
                break
            else
                ui_message('error','Grid size (%ix%i) does not match data size (%ix%i)',GridSize,MapSeg);
            end
        end
    end
end

function DWQ = open_dwq(filename)
DWQ = [];
fid = fopen(filename,'r','n','US-ASCII');
L = fgetl(fid);
[A,cnt,err,idx]=sscanf(L,'%i',inf);
if ~strcmp(err,'')
    fclose(fid);
    error('Unexpected character in first line of dwq file.\n%s\n%s^',L,repmat(' ',1,idx-1))
elseif cnt~=5
    fclose(fid);
    error('First line of dwq file should contain 5 integers.')
end
DWQ.FileName = filename;
[DWQ.AggrTable,cnt] = fscanf(fid,'%i',A(1));
if cnt<A(1)
    fclose(fid);
    error('Error reading segment numbers in dwq file:\%s',err)
else
    [Dum,cnt] = fscanf(fid,'%i',1);
    if cnt>0 || ~feof(fid)
        fclose(fid);
        error('Too many segment numbers in dwq file.')
    end
end
fclose(fid);