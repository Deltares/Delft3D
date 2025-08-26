function outfile = sim2ugrid(varargin)
%SIM2UGRID transfers simulation results to netCDF UGRID file.
%   NCFILENAME = SIM2UGRID(FILENAME, N)
%   transfers the results of the last N time steps of the specified file to
%   a netCDF UGRID file for use by D-FAST MI and/or D-FAST BE. Three file
%   types are currently supported:
%     * Delft3D-FLOW trim-files
%     * WAQUA SDS-files
%     * D-Flow FM map-files
%   If N is not specified, only the last time step will be transferred. Use
%   ':' to transfer all time steps. The name of the netCDF file is
%   constructed by appending _map.nc to the original file name. The
%   optional return argument NCFILENAME contains the name of the file
%   created. Note that the D-Flow FM map-file is a netCDF UGRID file and
%   can be used directly by the D-FAST tools; the main benefit of this tool
%   is to reduce the file size by only including the quantities needed.
%
%   NCFILENAME = SIM2UGRID(FILENAME01, FILENAME02, ..., FILENAMEnn, N)
%   transfers the results of the last N time steps of all specified files
%   to one merged netCDF UGRID file. The name of the output file is based
%   on the name of the first file.
%
%   The function copies only the data relevant for D-FAST Morphological
%   Impact and D-FAST Bank Erosion, being water levels, bed levels, water
%   depth, flow velocity vector, and Chézy value at the cell centres.
%
%   See also VS_USE, WAQUA, NETCDF.

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2025 Stichting Deltares.
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

if isstandalone
    fprintf(1, '--------------------------------------------------------------------------------\n');
    fprintf(1, 'SIM2UGRID conversion tool.\n');
    fprintf(1, 'Version <VERSION> (<CREATIONDATE>)\n');
    fprintf(1, 'Repository <GITREPO>\n');
    fprintf(1, 'Source hash <GITHASH>\n');
    fprintf(1, '--------------------------------------------------------------------------------\n');
    fprintf(1, '');
end

filenames = varargin;
ntimesReq = 1;
for i = length(filenames):-1:1
    if isnumeric(filenames{i})
        ntimesReq = filenames{i};
        filenames(i) = [];
    elseif strcmp(filenames{i}, ':')
        ntimesReq = filenames{i};
        filenames(i) = [];
    elseif ~isnan(str2double(filenames{i}))
        ntimesReq = str2double(filenames{i});
        filenames(i) = [];
    end
end

if isempty(filenames)
    fprintf(1, 'Usage:\n');
    fprintf(1, '   SIM2UGRID <inputFile> {<inputFile>} {N}\n');
    fprintf(1, '   Transfers the data from the input files to a new netCDF file with the name\n');
    fprintf(1, '   <basename>_map.nc where <basename>.<ext> is the name of the first input file.\n');
    fprintf(1, '   The data of the last N time steps is transferred (N=1 by default).\n');
    return
elseif ischar(ntimesReq)
    ntimesReq = sscanf(ntimesReq, '%i');
end

%% Opening the file
nFiles = length(filenames);
simOrg = cell(1, nFiles);
for i = 1:nFiles
    fprintf(1, 'Opening %s ...\n', filenames{i});
    simOrg{i} = qpfopen('no_merge', filenames{i});
end

%% Processing the time-independent data
simData = cell(1, nFiles);
fprintf(1, 'Processing the time-independent data ...\n');
for i = 1:nFiles
    simData{i} = get_const(simOrg{i});
end

has_chezy = all(cellfun(@(x)x.has_chezy, simData));
if ~has_chezy
    warning('No Chezy information found. Converted file not suitable for D-FAST Bank Erosion.')
end
zb_locs = unique(cellfun(@(x)x.zb_loc, simData, 'uniformoutput', false));
if length(zb_locs)>1
    list = '';
    for i = 1:nFiles
        list = cat(2, list, filenames{i}, ' ', simData{i}.zb_loc, newline);
    end
    error(['Inconsistent bed level locations across files:', newline, list])
end
zb_loc = zb_locs{1};
if ~strcmp(zb_loc, 'node')
    warning('Bed levels defined at faces. Converted file not suitable for D-FAST Bank Erosion.')
end

%% Checking time steps ...
fprintf(1, 'Checking available time steps ...\n');
ntimes = unique(cellfun(@(x)x.ntimes, simData));
if length(ntimes)>1
    list = '';
    for i = 1:nFiles
        list = cat(2, list, filenames{i}, sprintf(' %i\n', simData{i}.ntimes));
    end
    error(['Inconsistent number of times across files:', newline, list])
end
if ischar(ntimesReq) && strcmp(ntimesReq, ':')
    time_steps = 1:ntimes;
elseif ntimes < ntimesReq
    error('The file contains only %i time steps while %i are requested.', ntimes, ntimesReq)
else
    time_steps = (ntimes-ntimesReq+1):ntimes;
end
tunits = unique(cellfun(@(x)x.tunits, simData, 'uniformoutput', false));
if length(tunits)>1
    list = '';
    for i = 1:nFiles
        list = cat(2, list, filenames{i}, ' ', simData{i}.tunits, newline);
    end
    error(['Inconsistent time units across files:', newline, list])
else
    tunits = tunits{1};
end

%% Counting faces and nodes
nfaces = cellfun(@(x)size(x.faces, 1), simData);
nnodes = cellfun(@(x)length(x.xnode), simData);
total_nfaces = sum(nfaces);
total_nnodes = sum(nnodes);

%% Write netCDF file ...
ncfile = simData{1}.ncfile;
fprintf(1, 'Creating %s ...\n', ncfile);
ncid = netcdf.create(ncfile, 'NETCDF4');

% define dimensions, variables and attributes
Err = [];
try
    inodes = netcdf.defDim(ncid, 'mesh2d_nnodes', total_nnodes);
    ifaces = netcdf.defDim(ncid, 'mesh2d_nfaces', total_nfaces);
    imaxfn = netcdf.defDim(ncid, 'mesh2d_nmax_face_nodes', 4);
    itimes = netcdf.defDim(ncid, 'time', netcdf.getConstant('UNLIMITED'));
    
    mesh = netcdf.defVar(ncid, 'mesh2d', 'NC_DOUBLE', []);
    netcdf.putAtt(ncid, mesh, 'cf_role', 'mesh_topology')
    netcdf.putAtt(ncid, mesh, 'topology_dimension', int32(2))
    netcdf.putAtt(ncid, mesh, 'node_coordinates', 'mesh2d_node_x mesh2d_node_y')
    netcdf.putAtt(ncid, mesh, 'face_node_connectivity', 'mesh2d_face_nodes')
    
    ix = netcdf.defVar(ncid, 'mesh2d_node_x', 'NC_DOUBLE', inodes);
    netcdf.putAtt(ncid, ix, 'standard_name', 'projection_x_coordinate')
    netcdf.putAtt(ncid, ix, 'units', 'm')
    
    iy = netcdf.defVar(ncid, 'mesh2d_node_y', 'NC_DOUBLE', inodes);
    netcdf.putAtt(ncid, iy, 'standard_name', 'projection_y_coordinate')
    netcdf.putAtt(ncid, iy, 'units', 'm')
    
    ifnc = netcdf.defVar(ncid, 'mesh2d_face_nodes', 'NC_INT', [imaxfn, ifaces]);
    netcdf.putAtt(ncid, ifnc, 'cf_role', 'face_node_connectivity')
    netcdf.putAtt(ncid, ifnc, 'start_index', int32(1))
    
    if strcmp(zb_loc, 'node')
        idim = inodes;
    else
        idim = ifaces;
    end
    izb = netcdf.defVar(ncid, 'mesh2d_zb', 'NC_DOUBLE', idim);
    netcdf.putAtt(ncid, izb, 'standard_name', 'altitude')
    netcdf.putAtt(ncid, izb, 'long_name', 'Bed level')
    netcdf.putAtt(ncid, izb, 'units', 'm')
    netcdf.putAtt(ncid, izb, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, izb, 'location', zb_loc)
    
    it = netcdf.defVar(ncid, 'time', 'NC_DOUBLE', itimes);
    netcdf.putAtt(ncid, it, 'standard_name', 'time')
    netcdf.putAtt(ncid, it, 'units', tunits)
    
    izw = netcdf.defVar(ncid, 'mesh2d_zw', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, izw, 'standard_name', 'sea_surface_elevation')
    netcdf.putAtt(ncid, izw, 'long_name', 'Water level')
    netcdf.putAtt(ncid, izw, 'units', 'm')
    netcdf.putAtt(ncid, izw, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, izw, 'location', 'face')
    
    ih = netcdf.defVar(ncid, 'mesh2d_h1', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, ih, 'standard_name', 'sea_floor_depth_below_sea_surface')
    netcdf.putAtt(ncid, ih, 'long_name', 'Water depth')
    netcdf.putAtt(ncid, ih, 'units', 'm')
    netcdf.putAtt(ncid, ih, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, ih, 'location', 'face')
    
    iucx = netcdf.defVar(ncid, 'mesh2d_ucx', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, iucx, 'standard_name', 'sea_water_x_velocity')
    netcdf.putAtt(ncid, iucx, 'units', 'm s-1')
    netcdf.putAtt(ncid, iucx, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, iucx, 'location', 'face')
    
    iucy = netcdf.defVar(ncid, 'mesh2d_ucy', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, iucy, 'standard_name', 'sea_water_y_velocity')
    netcdf.putAtt(ncid, iucy, 'units', 'm s-1')
    netcdf.putAtt(ncid, iucy, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, iucy, 'location', 'face')
    
    if has_chezy
        iczs = netcdf.defVar(ncid, 'mesh2d_czs', 'NC_DOUBLE', [ifaces, itimes]);
        netcdf.putAtt(ncid, iczs, 'long_name', 'Chezy roughness')
        netcdf.putAtt(ncid, iczs, 'units', 'm0.5s-1')
        netcdf.putAtt(ncid, iczs, 'mesh', 'mesh2d')
        netcdf.putAtt(ncid, iczs, 'location', 'face')
    end
    
    iglobal = netcdf.getConstant('GLOBAL');
    pfilenames = protect(filenames);
    files = sprintf('"%s" ', pfilenames{:});
    history = [sprintf('%s: sim2ugrid.m %s\n', datestr(now, sim2ugrid_dateformat), files(1:end-1)), simData{1}.prehistory];
    netcdf.putAtt(ncid, iglobal, 'history', history)
    netcdf.putAtt(ncid, iglobal, 'converted_from', simData{1}.modelname)
    netcdf.putAtt(ncid, iglobal, 'Conventions', 'CF-1.8 UGRID-1.0 Deltares-0.10')
catch Err
    % failure --> throw Err again after closing the netCDF file
end

%% write time-independent data
fprintf(1, 'Writing time-independent data ...\n');
if isempty(Err)
    try
        % get arrays
        xnode = cellfun(@(x)x.xnode, simData, 'uniformoutput', false);
        ynode = cellfun(@(x)x.ynode, simData, 'uniformoutput', false);
        faces = cellfun(@(x)x.faces, simData, 'uniformoutput', false);
        zb = cellfun(@(x)x.zb, simData, 'uniformoutput', false);
        % correct indices
        nodeOffsets = cumsum(nnodes);
        for i = 2:nFiles
            faces{i} = faces{i} + nodeOffsets(i-1);
        end
        % concatenate arrays
        xnode = cat(1, xnode{:});
        ynode = cat(1, ynode{:});
        faces = cat(1, faces{:});
        zb = cat(1, zb{:});
        % write arrays
        netcdf.putVar(ncid, ix, xnode)
        netcdf.putVar(ncid, iy, ynode)
        netcdf.putVar(ncid, ifnc, faces')
        netcdf.putVar(ncid, izb, zb)
    catch Err
        % failure --> throw Err again after closing the netCDF file
    end
end

%% transfer time-dependent data
if isempty(Err)
    iTime = 0;
    for iOrg = time_steps
        fprintf(1, 'Transferring data for time step %i ...\n', iOrg);
        % get time dependent data
        timData = cell(1, nFiles);
        for i = 1:nFiles
            timData{i} = get_time_dependent(simOrg{i}, iOrg, simData{i}.face_active, has_chezy);
        end
        % collect and check data
        t = unique(cellfun(@(x)x.t, timData));
        if length(t) > 1
            error('Inconsistent output times across files.')
        end
        zw = cellfun(@(x)x.zw, timData, 'uniformoutput', false);
        h = cellfun(@(x)x.h, timData, 'uniformoutput', false);
        ucx = cellfun(@(x)x.ucx, timData, 'uniformoutput', false);
        ucy = cellfun(@(x)x.ucy, timData, 'uniformoutput', false);
        czs = cellfun(@(x)x.czs, timData, 'uniformoutput', false);
        % concatenate data
        zw = cat(1, zw{:});
        h = cat(1, h{:});
        ucx = cat(1, ucx{:});
        ucy = cat(1, ucy{:});
        czs = cat(1, czs{:});
        % write time dependent data
        start = [0, iTime];
        count = [total_nfaces, 1];
        try
            netcdf.putVar(ncid, it, iTime, 1, t)
            netcdf.putVar(ncid, izw, start, count, zw)
            netcdf.putVar(ncid, ih, start, count, h)
            netcdf.putVar(ncid, iucx, start, count, ucx)
            netcdf.putVar(ncid, iucy, start, count, ucy)
            if has_chezy
                netcdf.putVar(ncid, iczs, start, count, czs)
            end
        catch Err
            % failure --> throw Err again after closing the netCDF file
            break
        end
        iTime = iTime + 1;
    end
end

%% Close
netcdf.close(ncid)
if ~isempty(Err)
    fprintf(1, 'Error encountered during data transfer.\n');
    rethrow(Err)
else
    fprintf(1, 'Data transfer completed successfully.\n');
end

if nargout>0
    outfile = ncfile;
end


function str = protect(str)
if iscellstr(str)
    for i = 1:numel(str)
        str{i} = protect(str{i});
    end
else
    str = strrep(str, '\', '\\');
end


function data = get_const(simOrg)
data = [];
switch simOrg.QP_FileType
    case 'samples'
        % Reading from ASCII file format as exported using WAQPAN from SIMONA SDS-file
        if ~isequal(simOrg.Params, {'x', 'y', 'z', 'm', 'n', 'id'})
            error('Only sample files supported with columns x, y, z, m, n, id.')
        end
        filename = simOrg.FileName;
        [p, f] = fileparts(filename);
        quantity = sscanf(f, 'xyz_%*[^-]-zeta.%i.Q%i');
        if quantity(1)==1
            type = 'reference';
        else % 2
            type = 'measure';
        end
        level = quantity(2);
        data.ncfile = fullfile(p, sprintf('%s-Q%i_map.nc', type, level));
        data.zb = zeros(size(simOrg.XYZ(:, 3)));
        data.zb_loc = 'face';
        
        data.ntimes = 1;
        
        szMN = max(simOrg.XYZ(:, [4 5]))+1;
        face_active = accumarray(simOrg.XYZ(:, [4 5])+1, 1, szMN) > 0;
        
        % These files only contain grid cell centre coordinates.
        % Unfortunately, that's not enough to easily reconstruct the actual grid coordinates.
        % So, let's use a Cartisian mesh matching grid indices instead.
        [xnode, ynode] = ndgrid((1:szMN(1))-0.5, (1:szMN(2))-0.5);
        
        node_active = face_active | face_active([2:end end], :) | face_active(:, [2:end end]) | face_active([2:end end], [2:end end]);
        nodes = nan(szMN);
        nodes(node_active) = 1:sum(node_active(:));
        
        data.xnode = xnode(node_active);
        data.ynode = ynode(node_active);
        data.faces = face_node_connectivity(nodes);
        data.face_active = face_active;
        data.faces = reshape(data.faces, prod(szMN), 4);
        data.faces = data.faces(data.face_active, :);
        
        data.has_chezy = false;
        
        data.tunits = 'seconds';
        
        data.modelname = 'UNKNOWN';
        data.prehistory = 'Converted from WAQMORF samples.';

    case 'NetCDF'
        filename = simOrg.FileName;
        [p,f] = fileparts(filename);
        data.ncfile = [p, filesep, f, '.dfast.nc'];
        
        dimensions = {simOrg.Dimension.Name}';
        itime = ustrcmpi('time', dimensions);
        data.ntimes = simOrg.Dimension(itime).Length;

        dataFields = qpread(simOrg);
        fieldNames = {dataFields.Name}';

        grid = qpread(simOrg, dataFields(1), 'grid'); % Topology data of 2D mesh
        data.xnode = grid.X;
        data.ynode = grid.Y;
        data.faces = grid.FaceNodeConnect;
        data.face_active = true([size(grid.FaceNodeConnect, 1), 1]);

        BEDLEVEL_AT_NODES = 'z-coordinate of mesh nodes';
        BEDLEVEL_AT_FACES = 'flow element center bedlevel (bl) - mesh2d_nFaces: mean';
        if ismember(BEDLEVEL_AT_NODES, fieldNames)
            data.zb_loc = 'node';
            zb = qpread(simOrg, BEDLEVEL_AT_NODES, 'data');
            data.zb = zb.Val;
        elseif ismember(BEDLEVEL_AT_FACES, fieldNames)
            data.zb_loc = 'face';
            zb = qpread(simOrg, BEDLEVEL_AT_FACES, 'data');
            data.zb = zb.Val;
        else
            error('Unable to locate bed level information in the netCDF file.')
        end

        data.has_chezy = ismember('Chezy roughness in flow element center - mesh2d_nFaces: mean', fieldNames);

        data.tunits = 'seconds';
        
        isource = ustrcmpi('source', {simOrg.Attribute.Name});
        if isempty(isource)
            error('Expecting a global "source" attribute in D-Flow FM netCDF file: %s', filename)
        else
            source = simOrg.Attribute(isource).Value;
            if ~strncmp(source, 'D-Flow FM', 9)
                error('Only netCDF files coming from D-Flow FM are currently supported. Found source: %s', source)
            end
        end

        data.modelname = 'D-Flow FM';
        ihistory = find(strcmp('history', {simOrg.Attribute.Name}));
        data.prehistory = simOrg.Attribute(ihistory).Value;

    case 'SIMONA SDS FILE'
        % Reading directly from SIMONA SDS-file
        filename = simOrg.FileName;
        data.ncfile = [filename, '_map.nc'];
        [xd, yd] = waquaio(simOrg, '', 'dgrid');
        data.zb = waquaio(simOrg, '', 'height');
        data.zb_loc = 'node';
        
        Info = waqua('read', simOrg, '', 'SOLUTION_FLOW_SEP', []);
        data.ntimes = length(Info.SimTime);
        last_time = data.ntimes;
        zw = waquaio(simOrg, '', 'wlvl', last_time);
        
        node_active = ~isnan(xd(:));
        data.xnode = xd(node_active);
        data.ynode = yd(node_active);
        data.zb = data.zb(node_active);
        nnodes = length(data.xnode);
        
        nodes = zeros(size(xd)); % shouldn't this be NaN ?
        nodes(node_active) = 1:nnodes;
        data.faces = face_node_connectivity(nodes);
        data.face_active = all(~isnan(data.faces), 3) & ~isnan(zw);
        data.faces = reshape(data.faces, numel(xd), 4);
        data.faces = data.faces(data.face_active, :);
        
        data.has_chezy = true;
        
        refdate = waquaio(simOrg, '', 'refdate');
        [Y, M, D, h, m, s] = datevec(refdate);
        dimen = waqua('readsds', simOrg, '', 'MESH01_GENERAL_DIMENSIONS');
        tzone = dimen(13);
        data.tunits = 'days';
        data.tunits = sprintf('%s since %4i-%2.2i-%2.2i %2i:%2.2i:%2.2i %+i:00', data.tunits, Y, M, D, h, m, s, tzone);
        
        data.modelname = 'SIMONA';
        nmodifiers = length(simOrg.WriteProg);
        cprehistory = cell(1, nmodifiers);
        for i = 1:nmodifiers
            cprehistory{nmodifiers - i + 1} = [datestr(simOrg.WriteProg(i).Date, sim2ugrid_dateformat), ': ', simOrg.WriteProg(i).Name];
        end
        data.prehistory = [sprintf('%s-', cprehistory{1:end-1}) cprehistory{end}];

    case 'NEFIS'
        switch simOrg.SubType
            case 'Delft3D-trim'
                % Reading directly from Delft3D-FLOW trim-file
                filename = simOrg.DatExt;
                [p, f] = fileparts(filename);
                data.ncfile = [p, filesep, f, '_map.nc'];
                xd = vs_get(simOrg, 'map-const', 'XCOR', 'quiet');
                yd = vs_get(simOrg, 'map-const', 'YCOR', 'quiet');
                data.face_active = vs_get(simOrg, 'map-const', 'KCS', 'quiet') == 1;
                node_active = data.face_active | data.face_active([2:end end], :) | data.face_active(:, [2:end end]) | data.face_active([2:end end], [2:end end]);
                
                Info = vs_disp(simOrg, 'map-series', []);
                data.ntimes = Info.SizeDim;
                last_time = data.ntimes;
                ITDATE = vs_get(simOrg, 'map-const', 'ITDATE', 'quiet');
                [Y, M, D, h, m, s] = tdelft3d(ITDATE);
                tzone = vs_get(simOrg, 'map-const', 'TZONE', 'quiet');
                data.tunits = vs_get(simOrg, 'map-const', 'TUNIT', 'quiet');
                switch data.tunits
                    case 1 % seconds
                        data.tunits = 'seconds';
                    case 60 % minutes
                        data.tunits = 'minutes';
                    case 3600 % hours
                        data.tunits = 'hours';
                end
                data.tunits = sprintf('%s since %4i-%2.2i-%2.2i %2i:%2.2i:%2.2i %+i:00', data.tunits, Y, M, D, h, m, s, tzone);
                
                [dps, success] = vs_get(simOrg, 'map-sed-series', {last_time}, 'DPS', 'quiet');
                if success
                    data.zb = -dps;
                    data.zb_loc = 'face';
                else
                    dp = vs_get(simOrg, 'map-const', 'DP0', 'quiet');
                    data.zb = -dp;
                    dpsopt = vs_get(simOrg, 'map-const', 'DRYFLP', 'quiet');
                    if isequal(lower(deblank(dpsopt)), 'dp')
                        data.zb_loc = 'face';
                    else
                        data.zb_loc = 'node';
                    end
                end
                
                info = vs_disp('map-series', 'CFUROU');
                data.has_chezy = isstruct(info);
                
                data.xnode = xd(node_active);
                data.ynode = yd(node_active);
                nnodes = length(data.xnode);
                
                nodes = zeros(size(xd));
                nodes(node_active) = 1:nnodes;
                data.faces = face_node_connectivity(nodes);
                data.faces   = reshape(data.faces, numel(xd), 4);
                data.faces   = data.faces(data.face_active, :);
                
                switch data.zb_loc
                    case 'face'
                        data.zb = data.zb(data.face_active);
                    case 'node'
                        data.zb = data.zb(node_active);
                end
                
                data.modelname = 'Delft3D';
                simdat = vs_get(simOrg, 'map-version', 'FLOW-SIMDAT', 'quiet');
                simdat = datenum(sscanf(simdat, '%4d%2d%2d %2d%2d%2d', [1 6]));
                data.prehistory = [datestr(simdat, sim2ugrid_dateformat), ': Delft3D-FLOW'];
                
            otherwise
                error('NEFIS %s files are not (yet) supported by SIM2UGRID.', simOrg.SubType)
                
        end
    otherwise
        error('%s files are not (yet) supported by SIM2UGRID.', simOrg.QP_FileType)
end


function faces = face_node_connectivity(nodes)
faces = cat(3, ...
    nodes([1 1:end-1], [1 1:end-1]), ...
    nodes(:, [1 1:end-1]), ...
    nodes, ...
    nodes([1 1:end-1], :));


function data = get_time_dependent(simOrg, it, face_active, has_chezy)
zw = [];
switch simOrg.QP_FileType
    case 'samples'
        % Reading from ASCII file format as exported using WAQPAN from SIMONA SDS-file
        filename = simOrg.FileName;
        [p, f, e] = fileparts(filename);
        quantity = sscanf(f, 'xyz_%[^-]');
        quantity = char(quantity);
        
        switch quantity
            case 'velocity'
                ucx = simOrg.XYZ(:, 3);
                f2 = strrep(f, 'velocity', 'waterdepth');
                sim2 = qpfopen(fullfile(p, [f2, e]));
                h = sim2.XYZ(:, 3);
            case 'waterdepth'
                h = simOrg.XYZ(:, 3);
                f2 = strrep(f, 'waterdepth', 'velocity');
                sim2 = qpfopen(fullfile(p, [f2, e]));
                ucx = sim2.XYZ(:, 3);
        end
        ucy = zeros(size(ucx));
        zw = ucy;
        czs = [];
        t = 0;

    case 'NetCDF'
        WATERLEVEL = 'Water level - mesh2d_nFaces: mean';
        WATERDEPTH = 'Water depth at pressure points - mesh2d_nFaces: mean';
        VELOCITY = 'Flow element center velocity vector';
        CHEZY = 'Chezy roughness in flow element center - mesh2d_nFaces: mean';

        data = qpread(simOrg, WATERLEVEL, 'data', it);
        zw = data.Val;
        data = qpread(simOrg, WATERDEPTH, 'data', it);
        h = data.Val;
        data = qpread(simOrg, VELOCITY, 'data', it);
        ucx = data.XComp;
        ucy = data.YComp;
        data = qpread(simOrg, CHEZY, 'data', it);
        czs = data.Val;
        t = data.Time;
        
    case 'SIMONA SDS FILE'
        % Reading directly from SIMONA SDS-file
        [zw, t_abs] = waquaio(simOrg, '', 'wlvl', it);
        h = waquaio(simOrg, '', 'wdepth', it);
        [ucx, ucy] = waquaio(simOrg, '', 'xyveloc', it);
        [chu, chv] = waquaio(simOrg, '', 'chezy'); % can this be time varying?
        refdate = waquaio(simOrg, '', 'refdate');
        czs = sqrt(4./(1./chu(:, [1 1:end-1]).^2 + 1./chu.^2 + 1./chv([1 1:end-1], :).^2 + 1./chv.^2));
        
        zw = zw(face_active);
        h = h(face_active);
        ucx = ucx(face_active);
        ucy = ucy(face_active);
        czs = czs(face_active);
        t = t_abs - refdate;

    case 'NEFIS'
        switch simOrg.SubType
            case 'Delft3D-trim'
                % Reading directly from Delft3D-FLOW trim-file
                zw = vs_get(simOrg, 'map-series', {it}, 'S1', 'quiet');
                h = qpread(simOrg, 'water depth', 'data', it);
                h = h.Val';
                u = qpread(simOrg, 'depth averaged velocity', 'data', it);
                ucx = u.XComp';
                ucy = u.YComp';
                itmapc = vs_get(simOrg, 'map-info-series', {it}, 'ITMAPC', 'quiet');
                dt = vs_get(simOrg, 'map-const', 'DT', 'quiet');
                t = itmapc * dt;
                
                zw = zw(face_active);
                h = h(face_active);
                ucx = ucx(face_active);
                ucy = ucy(face_active);
                
                if has_chezy
                    chu = vs_get(simOrg, 'map-series', {it}, 'CFUROU', 'quiet');
                    chv = vs_get(simOrg, 'map-series', {it}, 'CFVROU', 'quiet');
                    czs = sqrt(4./(1./chu(:, [1 1:end-1]).^2 + 1./chu.^2 + 1./chv([1 1:end-1], :).^2 + 1./chv.^2));
                    czs = czs(face_active);
                else
                    czs = [];
                end
        end
end
if isempty(zw)
    error('%s files are not (yet) supported in the time dependent part of SIM2UGRID.', simOrg.FileType)
end
data.t = t;
data.zw = zw;
data.h = h;
data.ucx = ucx;
data.ucy = ucy;
data.czs = czs;


function format = sim2ugrid_dateformat
format = 'yyyy-mm-ddTHH:MM:SS';