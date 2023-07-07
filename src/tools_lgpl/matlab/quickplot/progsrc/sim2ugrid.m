function outfile = sim2ugrid(filename,ntimesReq)
%SIM2UGRID transfers simulation results to netCDF UGRID file.
%   NCFILENAME = SIM2UGRID(FILENAME, N)
%   transfers the results of the last N time steps of the specified file to
%   a netCDF UGRID file. Two file types are currently supported:
%     * Delft3D-FLOW trim-files
%     * WAQUA SDS-files
%   If N is not specified, only the last time step will be transferred.
%   The name of the netCDF file is constructed by appending _map.nc to the
%   original file name. The optional return argument NCFILENAME contains
%   the name of the file created.
%
%   The function copies only the data relevant for D-FAST Morphological
%   Impact and D-FAST Bank Erosion, being water levels, bed levels, water
%   depth, flow velocity vector, and Chézy value at the cell centres.
%
%   See also VS_USE, WAQUA, NETCDF.

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

if isstandalone
    fprintf(1, '--------------------------------------------------------------------------------\n');
    fprintf(1, 'SIM2UGRID conversion tool.\n');
    fprintf(1, 'Version <VERSION> (<CREATIONDATE>)\n');
    fprintf(1, 'Repository <GITREPO>\n');
    fprintf(1, 'Source hash <GITHASH>\n');
    fprintf(1, '--------------------------------------------------------------------------------\n');
end

if nargin == 0
    fprintf(1, '');
elseif nargin < 2
    ntimesReq = 1;
elseif ischar(ntimesReq)
    ntimesReq = sscanf(ntimesReq,'%i');
end

%% Opening the file
fprintf(1, 'Opening %s ...\n', filename);
simOrg = qpfopen(filename);

%% Processing the time-independent data
fprintf(1, 'Processing the time-independent data ...\n');
[ncfile,xnode,ynode,faces,zb,zb_loc,face_active,ntimes,modelname,prehistory,has_chezy,tunits] = get_const(simOrg);
nfaces = size(faces, 1);
nnodes = length(xnode);
%
if ~has_chezy
    warning('No Chezy information found. Converted file not suitable for D-FAST Bank Erosion.')
end
if ~strcmp(zb_loc,'node')
    warning('Bed levels defined at faces. Converted file not suitable for D-FAST Bank Erosion.')
end

%% Checking time steps ...
fprintf(1, 'Checking available time steps ...\n');
if ischar(ntimesReq) && strcmp(ntimesReq, ':')
    time_steps = 1:ntimes;
elseif ntimes < ntimesReq
    error('The file contains only %i time steps while %i are requested.', ntimes, ntimesReq)
else
    time_steps = (ntimes-ntimesReq+1):ntimes;
end

%% Write netCDF file ...
fprintf(1, 'Creating %s ...\n', ncfile);
ncid = netcdf.create(ncfile, 'NETCDF4');
Err = [];

% define dimensions, variables and attributes
success = true;
try
    inodes = netcdf.defDim(ncid, 'mesh2d_nnodes', nnodes);
    ifaces = netcdf.defDim(ncid, 'mesh2d_nfaces', nfaces);
    imaxfn = netcdf.defDim(ncid, 'mesh2d_nmax_face_nodes', 4);
    itimes = netcdf.defDim(ncid, 'time', netcdf.getConstant('UNLIMITED'));
    %
    mesh = netcdf.defVar(ncid, 'mesh2d', 'NC_DOUBLE', []);
    netcdf.putAtt(ncid, mesh, 'cf_role', 'mesh_topology')
    netcdf.putAtt(ncid, mesh, 'topology_dimension', int32(2))
    netcdf.putAtt(ncid, mesh, 'node_coordinates', 'mesh2d_node_x mesh2d_node_y')
    netcdf.putAtt(ncid, mesh, 'face_node_connectivity', 'mesh2d_face_nodes')
    %
    ix = netcdf.defVar(ncid, 'mesh2d_node_x', 'NC_DOUBLE', inodes);
    netcdf.putAtt(ncid, ix, 'standard_name', 'projection_x_coordinate')
    netcdf.putAtt(ncid, ix, 'units', 'm')
    %
    iy = netcdf.defVar(ncid, 'mesh2d_node_y', 'NC_DOUBLE', inodes);
    netcdf.putAtt(ncid, iy, 'standard_name', 'projection_y_coordinate')
    netcdf.putAtt(ncid, iy, 'units', 'm')
    %
    ifnc = netcdf.defVar(ncid, 'mesh2d_face_nodes', 'NC_INT', [imaxfn, ifaces]);
    netcdf.putAtt(ncid, ifnc, 'cf_role', 'face_node_connectivity')
    netcdf.putAtt(ncid, ifnc, 'start_index', int32(1))
    %
    if strcmp(zb_loc,'node')
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
    %
    it = netcdf.defVar(ncid, 'time', 'NC_DOUBLE', itimes);
    netcdf.putAtt(ncid, it, 'standard_name', 'time')
    netcdf.putAtt(ncid, it, 'units', tunits)
    %
    izw = netcdf.defVar(ncid, 'mesh2d_zw', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, izw, 'standard_name', 'sea_surface_elevation')
    netcdf.putAtt(ncid, izw, 'long_name', 'Water level')
    netcdf.putAtt(ncid, izw, 'units', 'm')
    netcdf.putAtt(ncid, izw, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, izw, 'location', 'face')
    %
    ih = netcdf.defVar(ncid, 'mesh2d_h1', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, ih, 'standard_name', 'sea_floor_depth_below_sea_surface')
    netcdf.putAtt(ncid, ih, 'long_name', 'Water depth')
    netcdf.putAtt(ncid, ih, 'units', 'm')
    netcdf.putAtt(ncid, ih, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, ih, 'location', 'face')
    %
    iucx = netcdf.defVar(ncid, 'mesh2d_ucx', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, iucx, 'standard_name', 'sea_water_x_velocity')
    netcdf.putAtt(ncid, iucx, 'units', 'm s-1')
    netcdf.putAtt(ncid, iucx, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, iucx, 'location', 'face')
    %
    iucy = netcdf.defVar(ncid, 'mesh2d_ucy', 'NC_DOUBLE', [ifaces, itimes]);
    netcdf.putAtt(ncid, iucy, 'standard_name', 'sea_water_y_velocity')
    netcdf.putAtt(ncid, iucy, 'units', 'm s-1')
    netcdf.putAtt(ncid, iucy, 'mesh', 'mesh2d')
    netcdf.putAtt(ncid, iucy, 'location', 'face')
    %
    if has_chezy
        iczs = netcdf.defVar(ncid, 'mesh2d_czs', 'NC_DOUBLE', [ifaces, itimes]);
        netcdf.putAtt(ncid, iczs, 'long_name', 'Chezy roughness')
        netcdf.putAtt(ncid, iczs, 'units', 'm0.5s-1')
        netcdf.putAtt(ncid, iczs, 'mesh', 'mesh2d')
        netcdf.putAtt(ncid, iczs, 'location', 'face')
    end
    %
    iglobal = netcdf.getConstant('GLOBAL');
    history = [sprintf('%s: sim2ugrid.m "%s"\n', datestr(now, sim2ugrid_dateformat), protect(filename)), prehistory];
    netcdf.putAtt(ncid, iglobal, 'history', history)
    netcdf.putAtt(ncid, iglobal, 'converted_from', modelname)
    netcdf.putAtt(ncid, iglobal, 'Conventions', 'CF-1.8 UGRID-1.0 Deltares-0.10')
catch Err
    success = false;
end

%% write time-independent data
fprintf(1, 'Writing time-independent data ...\n');
if success
    try
        netcdf.putVar(ncid, ix, xnode)
        netcdf.putVar(ncid, iy, ynode)
        netcdf.putVar(ncid, ifnc, faces')
        netcdf.putVar(ncid, izb, zb)
    catch Err
        success = false;
    end
end

%% transfer time-dependent data
if success
    i = 0;
    for iOrg = time_steps
        fprintf(1, 'Transferring data for time step %i ...\n', iOrg);
        % get time dependent data
        [t,zw,h,ucx,ucy,czs] = get_time_dependent(simOrg,iOrg,face_active,has_chezy);
        % write time dependent data
        start = [0, i];
        count = [nfaces, 1];
        try
            netcdf.putVar(ncid, it, i, 1, t)
            netcdf.putVar(ncid, izw, start, count, zw)
            netcdf.putVar(ncid, ih, start, count, h)
            netcdf.putVar(ncid, iucx, start, count, ucx)
            netcdf.putVar(ncid, iucy, start, count, ucy)
            if has_chezy
                netcdf.putVar(ncid, iczs, start, count, czs)
            end
        catch Err
            %success = false;
            break
        end
        i = i + 1;
    end
end

%% Close
netcdf.close(ncid)
if ~isempty(Err)
    fprintf(1, 'Error encounterdd during data transfer.\n');
    rethrow(Err)
else
    fprintf(1, 'Data transfer completed successfully.\n');
end

if nargout>0
    outfile = ncfile;
end


function str = protect(str)
str = strrep(str, '\', '\\');


function [ncfile,xnode,ynode,faces,zb,zb_loc,face_active,ntimes,modelname,prehistory,has_chezy,tunits] = get_const(simOrg)
switch simOrg.FileType
    case 'SIMONA SDS FILE'
        filename = simOrg.FileName;
        ncfile = [filename, '_map.nc'];
        [xd, yd] = waquaio(simOrg, '', 'dgrid');
        zb = waquaio(simOrg, '', 'height');
        zb_loc = 'node';
        %
        Info = waqua('read', simOrg, '', 'SOLUTION_FLOW_SEP', []);
        ntimes = length(Info.SimTime);
        last_time = ntimes;
        zw = waquaio(simOrg, '', 'wlvl', last_time);
        %
        node_active = ~isnan(xd(:));
        xnode = xd(node_active);
        ynode = yd(node_active);
        zb = zb(node_active);
        nnodes = length(xnode);
        %
        nodes = xd;
        nodes(node_active) = 1:nnodes;
        faces = cat(3, nodes([1 1:end-1], [1 1:end-1]), ...
                       nodes(:, [1 1:end-1]), ...
                       nodes, ...
                       nodes([1 1:end-1], :));
        face_active = all(~isnan(faces), 3) & ~isnan(zw);
        %
        faces = reshape(faces, numel(xd), 4);
        faces = faces(face_active, :);
        %
        has_chezy = true;
        %
        refdate = waquaio(simOrg, '', 'refdate');
        [Y,M,D,h,m,s] = datevec(refdate);
        dimen = waqua('readsds',simOrg,'','MESH01_GENERAL_DIMENSIONS');
        tzone = dimen(13);
        tunits = 'days';
        tunits = sprintf('%s since %4i-%2.2i-%2.2i %2i:%2.2i:%2.2i %+i:00',tunits,Y,M,D,h,m,s,tzone);
        %
        modelname = 'SIMONA';
        nmodifiers = length(simOrg.WriteProg);
        cprehistory = cell(1,nmodifiers);
        for i = 1:nmodifiers
            cprehistory{nmodifiers - i + 1} = [datestr(simOrg.WriteProg(i).Date, sim2ugrid_dateformat), ': ', simOrg.WriteProg(i).Name];
        end
        prehistory = [sprintf('%s-',cprehistory{1:end-1}) cprehistory{end}];
    case 'NEFIS'
        switch simOrg.SubType
            case 'Delft3D-trim'
                filename = simOrg.DatExt;
                [p,f] = fileparts(filename);
                ncfile = [p, filesep, f, '_map.nc'];
                xd = vs_get(simOrg, 'map-const', 'XCOR', 'quiet');
                yd = vs_get(simOrg, 'map-const', 'YCOR', 'quiet');
                face_active = vs_get(simOrg, 'map-const', 'KCS', 'quiet') == 1;
                node_active = face_active | face_active([2:end end],:) | face_active(:,[2:end end]) | face_active([2:end end],[2:end end]);
                %
                Info = vs_disp(simOrg, 'map-series', []);
                ntimes = Info.SizeDim;
                last_time = ntimes;
                ITDATE = vs_get(simOrg, 'map-const', 'ITDATE', 'quiet');
                [Y,M,D,h,m,s] = tdelft3d(ITDATE);
                tzone = vs_get(simOrg, 'map-const', 'TZONE', 'quiet');
                tunits = vs_get(simOrg, 'map-const', 'TUNIT', 'quiet');
                switch tunits
                    case 1 % seconds
                        tunits = 'seconds';
                    case 60 % minutes
                        tunits = 'minutes';
                    case 3600 % hours
                        tunits = 'hours';
                end
                tunits = sprintf('%s since %4i-%2.2i-%2.2i %2i:%2.2i:%2.2i %+i:00',tunits,Y,M,D,h,m,s,tzone);
                %
                [dps, success] = vs_get(simOrg, 'map-sed-series', {last_time}, 'DPS', 'quiet');
                if success
                    zb = -dps;
                    zb_loc = 'face';
                else
                    dp = vs_get(simOrg, 'map-const', 'DP0', 'quiet');
                    zb = -dp;
                    dpsopt = vs_get(simOrg, 'map-const', 'DRYFLP', 'quiet');
                    if isequal(lower(deblank(dpsopt)), 'dp')
                        zb_loc = 'face';
                    else
                        zb_loc = 'node';
                    end
                end
                %
                zw = vs_get(simOrg, 'map-series', {last_time}, 'S1', 'quiet');
                %
                info = vs_disp('map-series','CFUROU');
                has_chezy = isstruct(info);
                %
                xnode = xd(node_active);
                ynode = yd(node_active);
                nnodes = length(xnode);
                %
                nodes = xd;
                nodes(node_active) = 1:nnodes;
                faces = cat(3, nodes([1 1:end-1], [1 1:end-1]), ...
                    nodes(:, [1 1:end-1]), ...
                    nodes, ...
                    nodes([1 1:end-1], :));
                %
                switch zb_loc
                    case 'face'
                        zb = zb(face_active);
                    case 'node'
                        zb = zb(node_active);
                end
                %
                faces   = reshape(faces, numel(xd), 4);
                faces   = faces(face_active, :);
                %
                modelname = 'Delft3D';
                simdat = vs_get(simOrg, 'map-version', 'FLOW-SIMDAT', 'quiet');
                simdat = datenum(sscanf(simdat,'%4d%2d%2d %2d%2d%2d',[1 6]));
                prehistory = [datestr(simdat, sim2ugrid_dateformat), ': Delft3D-FLOW'];
            otherwise
                error('NEFIS %s files are not (yet) supported by SIM2UGRID.', simOrg.SubType)
        end
    otherwise
        error('%s files are not (yet) supported by SIM2UGRID.', simOrg.FileType)
end


function [t,zw,h,ucx,ucy,czs] = get_time_dependent(simOrg,it,face_active,has_chezy)
zw = [];
switch simOrg.FileType
    case 'SIMONA SDS FILE'
        [zw, t_abs] = waquaio(simOrg, '', 'wlvl', it);
        h = waquaio(simOrg, '', 'wdepth', it);
        [ucx, ucy] = waquaio(simOrg, '', 'xyveloc', it);
        [chu, chv] = waquaio(simOrg, '', 'chezy'); % can this be time varying?
        refdate = waquaio(simOrg, '', 'refdate');
        czs = sqrt(4./(1./chu(:,[1 1:end-1]).^2 + 1./chu.^2 + 1./chv([1 1:end-1],:).^2 + 1./chv.^2));
        %
        zw = zw(face_active);
        h = h(face_active);
        ucx = ucx(face_active);
        ucy = ucy(face_active);
        czs = czs(face_active);
        t = t_abs - refdate;
    case 'NEFIS'
        switch simOrg.SubType
            case 'Delft3D-trim'
                zw = vs_get(simOrg, 'map-series', {it}, 'S1', 'quiet');
                h = qpread(simOrg, 'water depth', 'data', it);
                h = h.Val';
                u = qpread(simOrg, 'depth averaged velocity', 'data', it);
                ucx = u.XComp';
                ucy = u.YComp';
                itmapc = vs_get(simOrg, 'map-info-series', {it}, 'ITMAPC', 'quiet');
                dt = vs_get(simOrg, 'map-const', 'DT', 'quiet');
                t = itmapc * dt;
                %
                zw = zw(face_active);
                h = h(face_active);
                ucx = ucx(face_active);
                ucy = ucy(face_active);
                %
                if has_chezy
                    chu = vs_get(simOrg, 'map-series', {it}, 'CFUROU', 'quiet');
                    chv = vs_get(simOrg, 'map-series', {it}, 'CFVROU', 'quiet');
                    czs = sqrt(4./(1./chu(:,[1 1:end-1]).^2 + 1./chu.^2 + 1./chv([1 1:end-1],:).^2 + 1./chv.^2));
                    czs = czs(face_active);
                else
                    czs = [];
                end
        end
end
if isempty(zw)
    error('%s files are not (yet) supported in the time dependent part of SIM2UGRID.', simOrg.FileType)
end


function format = sim2ugrid_dateformat
format = 'yyyy-mm-ddTHH:MM:SS';