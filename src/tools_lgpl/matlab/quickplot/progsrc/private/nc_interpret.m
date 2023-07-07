function nc = nc_interpret(nc,NumPartitions,PartNr_StrOffset,nDigits,Part1)
%NC_INTERPRET  Interpret the netCDF data based on conventions.
%    NC_OUT = NC_INTERPRET(NC_IN)
%
% where NC_IN  = nc_info(filename)
%
%    See also netcdf, MEXNC, NC_DUMP, NC_INFO

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

persistent CHARDIM
if isempty(CHARDIM)
    if getpref('SNCTOOLS','PRESERVE_FVD',false)
        % true: the first dimension is the character length
        CHARDIM = 1;
    else
        % false: the last dimension is the character length
        CHARDIM = 2;
    end
end

if ischar(nc)
    nc = nc_info(nc);
end
if nargin>1
    nc1 = rmfield(nc,'Filename');
    nc1.Dimension = rmfield(nc1.Dimension,'Length');
    nc1.Dataset   = rmfield(nc1.Dataset,{'Size','Chunking','Shuffle','Deflate'});
    Atts = {nc1.Attribute.Name};
    i_uuid = ustrcmpi('uuid',Atts);
    if i_uuid > 0
        uuid1 = nc.Attribute(i_uuid).Value;
    else
        uuid1 = NaN;
    end
    nc1 = rmfield(nc1,'Attribute');
    partNrFormat = ['%' num2str(nDigits) '.' num2str(nDigits) 'd'];
    %
    FileName2  = nc.Filename;
    Partitions = cell(1,NumPartitions);
    hPB = progressbar(0, 'title', sprintf('Scanning %i files ...',NumPartitions));
    for i = 1:NumPartitions
        FileName2(PartNr_StrOffset+(1:nDigits)) = sprintf(partNrFormat,i+Part1-1);
        nc2 = nc_info(FileName2);
        progressbar(i/NumPartitions, hPB);
        Partitions{i} = nc_interpret(nc2);
        nc2 = rmfield(nc2,'Filename');
        nc2.Dimension = rmfield(nc2.Dimension,'Length');
        nc2.Dataset   = rmfield(nc2.Dataset,{'Size','Chunking','Shuffle','Deflate'});
        Atts = {nc.Attribute.Name};
        i_uuid = ustrcmpi('uuid',Atts);
        if i_uuid > 0
            uuid2 = nc.Attribute(i_uuid).Value;
        else
            uuid2 = NaN;
        end
        nc2 = rmfield(nc2,'Attribute');
        %
        if isequal(uuid1,uuid2)
            % same uuid, so these files must belong to the same simulation
        elseif vardiff(nc1,nc2)>1
            % A mismatch in the data sets was detected, revert to the
            % default one file processing. Make sure that we are processing
            % the file that was selected instead of the first partition.
            Partitions{1} = nc_interpret(nc);
            NumPartitions = 1;
            break
        end
    end
    delete(hPB)
    %
    nc = Partitions{1}; % this is just a place holder ... the contents may differ
    nc.NumDomains = NumPartitions;
    if NumPartitions>1
        nc.Partitions = Partitions;
        nc.DomainCount.StrLoc = PartNr_StrOffset;
        nc.DomainCount.Digits = nDigits;
        nc.DomainCount.Offset = Part1;
        nc.Filename(PartNr_StrOffset+(1:nDigits)) = sprintf(partNrFormat,Part1);
        %
        hPB = progressbar(0, 'title', 'Merging partitions ...');
        try
            merged_mesh = nc_mapmerge(Partitions, hPB);
        catch Ex
            qp_error(sprintf('Unable to merge the partitions'),Ex,'nc_interpret')
            merged_mesh = [];
        end
        delete(hPB)
        if numel(merged_mesh)>1
            ui_message('warning','Multiple unstructured meshes in file. Not yet supported for merging.')
        elseif ~isempty(merged_mesh)
            nc.MergedPartitions = merged_mesh;
        end
    end
    return
else
    nc.NumDomains = 1;
end

if isfield(nc,'Group') && ~isempty(nc.Group)
    for g = 1:length(nc.Group)
        gname = nc.Group(g).Name(2:end);
        for d = 1:length(nc.Group(g).Dimension)
            nc.Group(g).Dimension(d).Name = ...
                [gname '/' nc.Group(g).Dimension(d).Name];
        end
        for d = 1:length(nc.Group(g).Dataset)
            nc.Group(g).Dataset(d).Name = ...
                [gname '/' nc.Group(g).Dataset(d).Name];
            for i = 1:length(nc.Group(g).Dataset(d).Dimension)
                nc.Group(g).Dataset(d).Dimension{i} = ...
                    [gname '/' nc.Group(g).Dataset(d).Dimension{i}];
            end
        end
    end
    nc.Dimension = cat(1,nc.Dimension,nc.Group.Dimension);
    nc.Dataset = cat(1,nc.Dataset,nc.Group.Dataset);
end

ndims = length(nc.Dimension);
if isfield(nc,'DataSet')
    DataSet = nc.DataSet;
    nc = rmfield(nc,'DataSet');
    nc.Dataset = DataSet;
end
nvars = length(nc.Dataset);
%ngatts = length(nc.Attribute);

[nc.Dimension(1:ndims).Type     ] = deal('unknown');
[nc.Dataset(1:nvars).Info       ] = deal([]);
[nc.Dataset(1:nvars).StdName    ] = deal('');
[nc.Dataset(1:nvars).Type       ] = deal('unknown');
[nc.Dataset(1:nvars).Mesh       ] = deal([]);
[nc.Dataset(1:nvars).Coordinates] = deal({});
[nc.Dataset(1:nvars).AutoCoords ] = deal({});
[nc.Dataset(1:nvars).X          ] = deal([]);
[nc.Dataset(1:nvars).XBounds    ] = deal([]);
[nc.Dataset(1:nvars).Y          ] = deal([]);
[nc.Dataset(1:nvars).YBounds    ] = deal([]);
[nc.Dataset(1:nvars).Z          ] = deal([]);
[nc.Dataset(1:nvars).Time       ] = deal([]);
[nc.Dataset(1:nvars).AuxTime    ] = deal([]);
[nc.Dataset(1:nvars).Station    ] = deal([]);
[nc.Dataset(1:nvars).SubField   ] = deal([]);
[nc.Dataset(1:nvars).SubFieldChr] = deal(zeros(0,2));
[nc.Dataset(1:nvars).TSMNK      ] = deal([NaN NaN NaN NaN NaN]);
[nc.Dataset(1:nvars).SubFieldDim] = deal([]);
[nc.Dataset(1:nvars).CharDim    ] = deal([]);
%[nc.Dataset(1:nvars).Vector] = deal([]);

%
% Determine auxiliary coordinates: loop over all variables and collect the
% variables listed by the coordinates attributes. At the same time, mark
% all coordinate (dimension) variables.
%
if isempty(nc.Dimension)
    dimNames = {};
else
    dimNames = {nc.Dimension.Name};
end
varNames = {nc.Dataset.Name};
%AuxCoordVars = {};
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    if isfield(Info.Attribute,'Name')
        Attribs = {Info.Attribute.Name};
    else
        Attribs = {};
    end
    %
    % 1=BYTE, 2=CHAR, 3=SHORT, 4=INT, 5=FLOAT, 6=DOUBLE, 7=UBYTE, 8=USHORT,
    % 9=UINT, 10=INT64, 11=UINT64, ... 12=STRING?
    if Info.Nctype==2 || Info.Nctype>11
        for cattrib = {'_FillValue','missing_value','valid_min','valid_max','valid_range'}
            attrib = cattrib{1};
            j = strmatch(attrib,Attribs,'exact');
            if ~isempty(j) && Info.Nctype==2 && Info.Attribute(j).Nctype==2 && strcmp(attrib,'_FillValue')
                % fill value on char is allowed if it's a single char.
                if length(Info.Attribute(j).Value)==1
                    j = [];
                else
                    msg = 'Invalid value of attribute "%s" on data variable "%s" of type %s: the value should be a single character. Attribute ignored.';
                end
            else
                msg = 'Invalid use of attribute "%s" on data variable "%s" of type %s. Attribute ignored.';
            end
            if ~isempty(j)
                ui_message('error',msg,attrib,Info.Name,Info.Datatype)
                nc.Dataset(ivar).Attribute(j) = [];
                Info = nc.Dataset(ivar);
                Attribs(j) = [];
            end
        end
    end
    %
    j = strmatch('flag_values',Attribs,'exact');
    if ~isempty(j)
        if ~isequal(Info.Attribute(j).Datatype,Info.Datatype)
            msg = {sprintf('Data type of flag_values attribute (%s) inconsistent with data type of data variable %s (%s).',Info.Attribute(j).Datatype,Info.Name,Info.Datatype)};
            if isequal(Info.Attribute(j).Datatype,'char')
                strValues = Info.Attribute(j).Value;
                try
                    switch Info.Datatype
                        case {'int8','int16','int32','int64','uint8','uint16','uint32','uint64'}
                            Values = str2vec(strValues,'%d');
                        otherwise
                            Values = str2vec(strValues,'%f');
                    end
                    Info.Attribute(j).Value = feval(Info.Datatype,Values);
                    Info.Attribute(j).Datatype = Info.Datatype;
                    nc.Dataset(ivar) = Info;
                    msg{2} = sprintf('Attribute value ''%s'' converted.',strValues);
                catch
                    msg{2} = sprintf('Conversion of attribute value ''%s'' failed.',strValues);
                end
            end
            ui_message('error',msg)
        end
    end
    %
    if ~isfield(Info,'Dimid') || isempty(Info.Dimid)
        nc.Dataset(ivar).Varid = ivar-1;
        nc.Dataset(ivar).Dimid = zeros(size(Info.Dimension));
        nc.Dataset(ivar).Rank = length(Info.Dimension);
        for idim = 1:length(Info.Dimension)
            nc.Dataset(ivar).Dimid(idim) = strmatch(Info.Dimension{idim},dimNames,'exact')-1;
        end
        Info = nc.Dataset(ivar);
    end
    %
    j = strmatch('cf_role',Attribs,'exact');
    if isempty(j) % hack to support my coding error in FM
        j = strmatch('cf_type',Attribs,'exact');
        if ~isempty(j) && strcmp(Info.Attribute(j).Value,'mesh_topology')
            ui_message('error','Incorrect attribute "cf_type" used for specifying the mesh_topology role of variable "%s".',Info.Name)
        end
    end
    if ~isempty(j) 
        if strcmp(Info.Attribute(j).Value,'mesh_topology') || strcmp(Info.Attribute(j).Value,'grid_topology')
            [nc,Info] = parse_ugrid_or_sgrid_mesh(nc,varNames,dimNames,ivar,Info,Attribs);
        elseif strcmp(Info.Attribute(j).Value,'mesh_topology_contact')
            [nc,Info] = parse_ugrid_contact(nc,varNames,dimNames,ivar,Info,Attribs);
        end
    end
    %
    j = strmatch('geometry_type',Attribs,'exact');
    if ~isempty(j)
        [nc,Info] = parse_simple_geometry(nc,varNames,dimNames,ivar,Info,Attribs);
    end
    %
    j = strmatch('standard_name',Attribs,'exact');
    if isempty(j)
        j = strmatch('NAVO_code',Attribs,'exact');
        if ~isempty(j)
            Info.Attribute = navo2stdname(Info.Attribute,Info.Attribute(j).Value);
            Attribs = {Info.Attribute.Name};
        end
        %
        j = strmatch('standard_name',Attribs,'exact');
        if ~isempty(j)
            Info.StdName = Info.Attribute(j).Value;
        end
    else
        Info.StdName = Info.Attribute(j).Value;
    end
    %
    if Info.Nctype==2
        %
        % for character variables the CHARDIM dimension is the string length
        %
        if Info.Rank==2
            if strcmp(Info.Dimension{3-CHARDIM},Info.Name)
                Info.Type = 'coordinate';
            end
        end
    elseif Info.Rank==1
        if strcmp(Info.Dimension{1},Info.Name)
            Info.Type = 'coordinate';
        end
    end
    %
    coords = strmatch('coordinates',Attribs,'exact');
    if ~isempty(coords)
        %
        % read coordinate names
        %
        coordstr = Info.Attribute(coords).Value;
        Info.Coordinates = multiline(coordstr,' ','output','cellrow','empty','skip');
    end
    %
    nc.Dataset(ivar) = Info;
end

%
% Detect bounds variables (We don't want bounds directly listed as
% coordinate variable.
%
boundVars = cell(1,nvars);
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    %
    if isfield(Info.Attribute,'Name')
        Attribs = {Info.Attribute.Name};
        j = strmatch('bounds',Attribs,'exact');
        if ~isempty(j)
            boundVars{ivar} = Info.Attribute(j).Value;
        end
    end
end
boundVars(cellfun('isempty',boundVars)) = [];

%
% Try to (auto) detect type (auxiliary) coordinate variables.
%
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    idim = Info.Dimid+1;
    %
    if strmatch(Info.Name,boundVars,'exact')
        % bounds variables should not be marked as (auxiliary) coordinate
        % variables.
        continue
    end
    %
    if isfield(Info.Attribute,'Name')
        Attribs = {Info.Attribute.Name};
    else
        Attribs = {};
    end
    %
    % character variables are labels
    %
    if Info.Nctype == 2 && length(Info.Dimension)==2 && ~isempty(idim)
        nc = setType(nc,ivar,idim(3-CHARDIM),'label');
        continue
    end
    %
    % check standard_name
    %
    j = strmatch('standard_name',Attribs,'exact');
    if ~isempty(j)
        switch Info.Attribute(j).Value
            case {'atmosphere_sigma_coordinate', ...
                        'atmosphere_hybrid_sigma_pressure_coordinate', ...
                        'atmosphere_hybrid_height_coordinate', ...
                        'atmosphere_sleve_coordinate', ...
                        'ocean_sigma_coordinate', ...
                        'ocean_s_coordinate', ...
                        'ocean_sigma_z_coordinate', ...
                        'ocean_double_sigma_coordinate'}
                %
                % Vertical dimension
                %
                if isempty(idim) && length(Info.Dimid)==1 && strcmp(nc.Dimension(Info.Dimid+1).Type,'unknown')
                    idim = Info.Dimid+1;
                end
                nc = setType(nc,ivar,idim,['z-coordinate: ',Info.Attribute(j).Value]);
                continue
            case 'latitude'
                nc = setType(nc,ivar,idim,'latitude');
                continue
            case 'longitude'
                nc = setType(nc,ivar,idim,'longitude');
                continue
            case {'projection_x_coordinate','grid_longitude'} % not true longitude
                nc = setType(nc,ivar,idim,'x-coordinate');
                continue
            case {'projection_y_coordinate','grid_latitude'} % not true latitude
                nc = setType(nc,ivar,idim,'y-coordinate');
                continue
            case 'time'
                nc = setType(nc,ivar,idim,'time');
                % in case of time, continue to interpret the time unit
                % (reference date and time step)
        end
    end
    %
    % standard_name not recognized (or time); look for known units
    %
    j = strmatch('units',Attribs,'exact');
    if ~isempty(j)
        unit = Info.Attribute(j).Value;
        [unit1,unit2] = strtok(lower(unit));
        if ~ischar(unit1)
            unit1='';
        end
        switch unit1
            case {'degrees_north','degree_north','degrees_N','degree_N','degreesN','degreeN'}
                nc = setType(nc,ivar,idim,'latitude');
                continue
            case {'degrees_east','degree_east','degrees_E','degree_E','degreesE','degreeE'}
                nc = setType(nc,ivar,idim,'longitude');
                continue
            case {'level','layer','sigma_level'}
                %
                % deprecated z-coordinate "units"
                %
                nc = setType(nc,ivar,idim,'z-coordinate');
                continue
            case {'millisecond','milliseconds','millisec','millisecs','msec','ms'}
                dt = 0.001;
            case {'second','seconds','sec','secs','s'}
                dt = 1;
            case {'minute','minutes','min','mins'}
                dt = 60;
            case {'hour','hours','hr','hrs','h'}
                dt = 3600;
            case {'day','days','d'}
                dt = 86400; % 24*3600
            case {'week'}
                dt = 7*86400;
            case {'month','months','mon'}
                dt = 365.242198781*24*3600/12;
            case {'year','years','yr','yrs'}
                dt = 365.242198781*24*3600;
            case {'common_year','common_years'}
                dt = 365          *24*3600;
            case {'leap_year','leap_years'}
                dt = 366          *24*3600;
            case {'Julian_year','Julian_years'}
                dt = 365.25       *24*3600;
            case {'Gregorian_year','Gregorian_years'}
                dt = 365.2425     *24*3600;
            otherwise
                try
                    f = qp_unitconversion(unit1,'Pa');
                catch
                    f = 'fail';
                end
                if ~ischar(f)
                    %
                    % dimension unit is compatible with pressure (Pa)
                    % according CF 1.4 this must be a z-coordinate
                    %
                    nc = setType(nc,ivar,idim,'z-coordinate');
                    continue
                end
                dt = [];
        end
        if ~isempty(dt)
            % quantity with time unit
            %
            % even though there is a space between the time and the time
            % zone, this line supports the case in which a + or - of the
            % time zone is directly attached to the time.
            refdate = sscanf(unit2,' since %d-%d-%d%*1[ T]%d:%d:%f %d:%d',[1 8]);
            if length(refdate)==1
                % possibly basic (condensed) format
                refdate = sscanf(unit2,' since %4d%2d%2d%*1[ T]%2d%2d%f %d:%d',[1 8]);
            end
            if length(refdate)>=6
                if length(refdate)==8
                    % offset HH:MM
                    TZshift = refdate(7) + sign(refdate(7))*refdate(8)/60;
                elseif length(refdate)==7
                    % offset HH or HHMM
                    TZshift = refdate(7);
                    if abs(TZshift)>24
                        TZshift = fix(TZshift/100)+rem(TZshift,100)/60;
                    end
                else
                    TZshift = 0;
                end
                refdate = datenum(refdate(1:6));
            elseif length(refdate)>=3
                refdate(6) = 0;
                refdate = datenum(refdate);
                TZshift = NaN;
            else
                refdate = [];
                TZshift = NaN;
            end
            %
            % time quantity "since" --> time dimension
            %
            if ~isempty(refdate) && ~isequal(nc.Dataset(ivar).Type,'time') && length(nc.Dataset(ivar).Dimension)==1
                if strcmp(nc.Dataset(ivar).Type,'coordinate')
                    nc = setType(nc,ivar,idim,'time');
                else
                    nc = setType(nc,ivar,idim,'aux-time');
                end
            elseif strcmp(nc.Dataset(ivar).Type,'coordinate')
                nc = setType(nc,ivar,idim,'aux-time');
            end
            %
            nc.Dataset(ivar).Info.DT      = dt/86400;
            if ~isempty(unit2) && isempty(refdate)
                nc.Dataset(ivar).Info.RefDate = unit;
            else
                nc.Dataset(ivar).Info.RefDate = refdate;
            end
            % Pass TZshift to QUICKPLOT for optional processing
            nc.Dataset(ivar).Info.TZshift = TZshift;
            continue
        end
    end
    %
    % neither unit nor standard_name, try another approach
    %
    j = strmatch('axis',Attribs,'exact');
    if ~isempty(j)
        switch lower(Info.Attribute(j).Value)
            case 'x'
                %
                % x coordinate (may or may not be longitude)
                %
                nc = setType(nc,ivar,idim,'x-coordinate');
                continue
            case 'y'
                %
                % y coordinate (may or may not be latitude)
                %
                nc = setType(nc,ivar,idim,'y-coordinate');
                continue
            case 'z'
                %
                % Vertical dimension
                %
                nc = setType(nc,ivar,idim,'z-coordinate');
                continue
        end
    end
    %
    j = strmatch('positive',Attribs,'exact');
    if ~isempty(j)
        switch lower(Info.Attribute(j).Value)
            case {'up','down'}
                %
                % Vertical dimension
                %
                nc = setType(nc,ivar,idim,'z-coordinate');
                continue
            otherwise
                % produce error?
        end
    end
    %
    j = strmatch('formula_terms',Attribs,'exact');
    jsn = strmatch('standard_name',Attribs,'exact');
    if ~isempty(j)
        %
        % Vertical dimension
        %
        if isempty(jsn)
            ui_message('error','Variable "%s" does not comply to %s conventions for a mesh.\nIgnoring mesh/location attributes on "%s".',nameMesh,TYPE,Info.Name)
        end
        nc = setType(nc,ivar,idim,'z-coordinate');
        continue
    end
    %
    if ~isempty(jsn) && strcmp(Info.Attribute(jsn).Value,'altitude')
        %
        % Altitude accepted as vertical coordinate under certain conditions
        % ... I want to accept mesh2d_layer_z and mesh2d_interface_z, but
        % not mesh2d_flowelem_bl or mesh2d_node_z. 
        %
        if ~isempty(strfind(Info.Name,'_interface_z')) || ...
                ~isempty(strfind(Info.Name,'_layer_z')) || ...
                ~isempty(strfind(Info.Name,'_zcc')) || ...
                ~isempty(strfind(Info.Name,'_zw'))
            nc = setType(nc,ivar,idim,'z-coordinate');
        end
        continue
    end
end

iCoords = ~strcmp({nc.Dataset.Type},'unknown');
AuxCoordVars = varNames(iCoords);

%
% Mark auxiliary coordinate variables.
% Collect coordinates per variable.
% Collect coordinate dimensions used by auxiliary variables.
%
AuxCoordVar_Dimens = cell(size(AuxCoordVars));
for i = 1:length(AuxCoordVars)
    for ivar = 1:nvars
        Info = nc.Dataset(ivar);
        if strcmp(Info.Name,AuxCoordVars{i})
            if Info.Nctype==2 % character
                if CHARDIM==1
                    AuxCoordVar_Dimens{i} = Info.Dimension(2:end);
                else
                    AuxCoordVar_Dimens{i} = Info.Dimension(1:end-1);
                end
            else
                AuxCoordVar_Dimens{i} = Info.Dimension;
            end
        end
    end
end

%
% Identify (auxiliary) coordinates based on dimensions shared with
% variables marked above as some kind of coordinate, such as latitude and
% longitude variables. If we identify some coordinates not yet listed by
% the coordinates attribute, then add them to the AutoCoords list.
%
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    %
    iSelect = false(size(AuxCoordVars));
    for i = 1:length(AuxCoordVars)
        if iscell(AuxCoordVar_Dimens{i}) && all(ismember(AuxCoordVar_Dimens{i},Info.Dimension))
            iSelect(i) = true;
        end
    end
    AutoCoords = AuxCoordVars(iSelect);
    AutoCoords = setdiff(AutoCoords,Info.Coordinates);
    %
    if ~isempty(AutoCoords)
        nc.Dataset(ivar).AutoCoords = union(AutoCoords,Info.AutoCoords);
    end
end

%
% Try to detect x/y/z/time coordinates.
%
iSGrid = strcmp({nc.Dataset.Type}','sgrid_mesh');
iUGrid = strcmp({nc.Dataset.Type}','ugrid_mesh') | strcmp({nc.Dataset.Type}','ugrid_mesh_contact');
SGrid  = {nc.Dataset(iSGrid).Name};
UGrid  = {nc.Dataset(iUGrid).Name};
iSGrid = find(iSGrid);
iUGrid = find(iUGrid);
gridLoc = {'node','edge','face','volume'};
scalarDimWarnings = {};
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    % fprintf('%i: %s\n',ivar,Info.Name);
    if ~isempty(Info.Attribute)
        Attribs = {Info.Attribute.Name};
        j1 = strmatch('mesh',Attribs,'exact');
        TYPE='UGRID';
        if isempty(j1)
            j1 = strmatch('grid',Attribs,'exact');
            TYPE='SGRID';
        end
        j2 = strmatch('location',Attribs,'exact');
        if ~isempty(j1) && ~isempty(j2)
            nameMesh = Info.Attribute(j1).Value;
            % We hve identified a mesh and location attribute, so it looks
            % like a UGRID reference to a mesh variable and location.
            switch TYPE
                case 'UGRID'
                    j3 = strmatch(nameMesh,UGrid,'exact');
                case 'SGRID'
                    j3 = strmatch(nameMesh,SGrid,'exact');
            end
            if isempty(j3)
                % The mesh variable referred to is not yet in list of known
                % meshes.
                v3 = strmatch(nameMesh,varNames,'exact');
                if ~isempty(v3)
                    nc = parse_ugrid_mesh_or_contact(nc,varNames,dimNames,v3);
                    %
                    iUGrid = strcmp({nc.Dataset.Type}','ugrid_mesh') | strcmp({nc.Dataset.Type}','ugrid_mesh_contact');
                    UGrid  = {nc.Dataset(iUGrid).Name};
                    iUGrid = find(iUGrid);
                    %
                    j3 = strmatch(Info.Attribute(j1).Value,UGrid,'exact');
                end
            end
            if ~isempty(j3)
                switch TYPE
                    case 'UGRID'
                        switch nc.Dataset(iUGrid(j3)).Type
                            case 'ugrid_mesh'
                                j4 = strmatch(Info.Attribute(j2).Value,gridLoc,'exact')-1;
                            case 'ugrid_mesh_contact'
                                j4 = strmatch(Info.Attribute(j2).Value,{'contact'},'exact')-1;
                        end
                    case 'SGRID'
                        loc = Info.Attribute(j2).Value;
                        switch loc(end)
                            case '1'
                                loc = loc(1:end-1);
                            case '2'
                                loc = loc(1:end-1);
                            case '3'
                                loc = loc(1:end-1);
                        end
                        j4 = strmatch(loc,gridLoc,'exact')-1;
                end
            end
            if isempty(j3)
                if any(strcmp(nameMesh,varNames))
                    ui_message('error','Variable "%s" does not comply to %s conventions for a mesh.\nIgnoring mesh/location attributes on "%s".',nameMesh,TYPE,Info.Name)
                else
                    ui_message('error','Cannot find mesh "%s"; ignoring mesh/location attributes on "%s".',nameMesh,Info.Name)
                end
            elseif isempty(j4)
                ui_message('error','Invalid location type "%s"; ignoring mesh/location attributes on "%s".',Info.Attribute(j2).Value,Info.Name)
            elseif strcmp(TYPE,'UGRID')
                if strcmp(nc.Dataset(iUGrid(j3)).Type,'ugrid_mesh')
                    topoDim   = nc.Dataset(iUGrid(j3)).Mesh{j4+5};
                    if isempty(strmatch(topoDim,Info.Dimension,'exact'))
                        dims = sprintf('%s, ',Info.Dimension{:});
                        ui_message('error','Variable "%s" points to UGRID mesh "%s" location "%s"\nbut the variable''s dimensions {%s}\ndon''t include the %s dimension "%s".\nIgnoring the mesh/location attributes.',...
                            Info.Name, Info.Attribute(j1).Value, Info.Attribute(j2).Value, dims(1:end-2), Info.Attribute(j2).Value, topoDim)
                    else
                        ugrid = nc.Dataset(iUGrid(j3)).Mesh(1:2);
                        Info.Mesh = {ugrid{:} iUGrid(j3) j4};
                        Info.TSMNK(3) = strmatch(topoDim,dimNames,'exact')-1;
                    end
                end
            else % SGRID
                % ...
            end
        end
    end
    %
    for ctp = {{'Coordinates' 1},{'AutoCoords' -1}}
        % use negative indices for automatic coordinates such that we can
        % distinguish them later and give preference to coordinates
        % specified in the file.
        crdField  = Info.(ctp{1}{1});
        Factor    = ctp{1}{2};
        %
        [coords,ia,ib]=intersect(crdField,varNames);
        % setdiff(crdField,varNames) missing?
        for icvar1 = 1:length(ib)
            icvar  = ib(icvar1);
            sicvar = Factor*icvar;
            %
            vDims  = Info.Dimension;
            cvDims = nc.Dataset(icvar).Dimension;
            cvSize = nc.Dataset(icvar).Size;
            if nc.Dataset(icvar).Nctype==2
                % in case of a character array, remove string length
                if CHARDIM==1
                    nmDims = setdiff(cvDims(2:end),vDims);
                    cvSize = cvSize(2:end);
                else
                    nmDims = setdiff(cvDims(1:end-1),vDims);
                    cvSize = cvSize(1:end-1);
                end
            else
                nmDims = setdiff(cvDims,vDims);
            end
            if ~isempty(nmDims) && ~strcmp(Info.Type,'ugrid_mesh') && ~strcmp(Info.Type,'simple_geometry')
                vDimsStr = sprintf('%s, ',vDims{:});
                cvDimsStr = sprintf('%s, ',cvDims{:});
                if length(nmDims) == 1 && cvSize == 1
                    % coordinate variable with length 1
                    if ~ismember(nc.Dataset(icvar).Name,scalarDimWarnings)
                        Msg = sprintf(['Auxiliary coordinate %s has a dimension %s\n', ...
                            'which does not match any dimension of data variable %s.\n', ...
                            'The length of %s equals 1, so this dimension will be interpreted as a scalar coordinate.\n', ...
                            'Remove this dimension from %s to create a scalar coordinate according CF conventions, or\n', ...
                            'change to a regular auxiliary coordinate by adding the dimension %s to the data variable.\n'], ...
                            nc.Dataset(icvar).Name, nmDims{1}, ...
                            Info.Name, ...
                            nmDims{1}, ....
                            nc.Dataset(icvar).Name, ...
                            nmDims{1});
                        ui_message('warning',Msg)
                        scalarDimWarnings{end+1} = nc.Dataset(icvar).Name;
                    end
                else
                    Msg = sprintf(['Dimensions of variable and auxiliary coordinate do not match.\n', ...
                        'Variable %s: %s\nCoordinate %s: %s\nSkipping auxiliary coordinate.'], ...
                        Info.Name, vDimsStr(1:end-2), ...
                        nc.Dataset(icvar).Name, cvDimsStr(1:end-2));
                    ui_message('error',Msg)
                end
                continue
            end
            switch nc.Dataset(icvar).Type
                case {'longitude','x-coordinate'}
                    Info.X = [Info.X sicvar];
                case {'latitude','y-coordinate'}
                    Info.Y = [Info.Y sicvar];
                case 'time'
                    Info.Time = [Info.Time sicvar];
                case 'aux-time'
                    Info.AuxTime = [Info.AuxTime sicvar];
                case 'label'
                    AcceptedStationNames = {'cross_section_name','cross_section_id','station_name','station_id','dredge_area_name','dump_area_name'};
                    if sicvar>0 % don't use auto detect label dimensions as station ... this will trigger sediment names to be used as station name for map-files
                        Info.Station = [Info.Station sicvar];
                    elseif ismember(nc.Dataset(-sicvar).Name,AcceptedStationNames)
                        Info.Station = [Info.Station sicvar];
                    else
                        if CHARDIM==1
                            sfdim = nc.Dataset(icvar).Dimid(2);
                        else
                            sfdim = nc.Dataset(icvar).Dimid(1);
                        end
                        Info.SubFieldChr = [Info.SubFieldChr; icvar sfdim];
                    end
                case {'ugrid_mesh'}
                    % never a coordinate
                otherwise
                    if strncmp(nc.Dataset(icvar).Type,'z-coordinate',12)
                        Info.Z = [Info.Z sicvar];
                    else
                        Info.SubField = [Info.SubField sicvar];
                    end
            end
        end
    end
    %
    % For every dimension I may have multiple (auxiliary) coordinates. How
    % to deal with that?
    %
    if isempty(Info.Time) && ~isempty(Info.AuxTime)
        Info.Time = Info.AuxTime;
        Info.AuxTime = [];
    end
    if ~isempty(Info.Time)
        iTime = abs(Info.Time);
        %
        % Assumption: time is always unique and coordinate dimension.
        %
        if length(iTime)>1 || length(nc.Dataset(iTime).Dimid)>1
            ui_message('error','Unsupported case encountered: multiple time coordinates encountered.')
        end
        Info.Time = iTime(1);
        %
        if ~isempty(nc.Dataset(Info.Time).Dimid)
            Info.TSMNK(1) = nc.Dataset(Info.Time).Dimid;
        end
    end
    %
    if ~isempty(Info.Station)
        iStation = Info.Station(Info.Station>0);
        if length(iStation)<1
            % if no coordinate was explicitly specified, then use the first
            % autodetected variable without complaint
            iStation = abs(Info.Station(1));
        elseif length(iStation)>1
            % if multiple explicit coordinates match, then report error
            Names = {nc.Dataset(iStation).Name};
            for is = 1:length(Names)
                isInfo = nc.Dataset(iStation(is));
                dims = sprintf('%s, ',isInfo.Dimension{:});
                Names{is} = [isInfo.Datatype ' :: ' Names{is} ' (' dims(1:end-2) ')'];
            end
            ui_message('error', ...
                [{sprintf('Problem detecting station coordinate for "%s".',Info.Name) ...
                'Any one of the following variables seems to be valid'} ...
                Names ...
                {'Using the first one.'}])
            iStation = iStation(1);
        end
        Info.Station = iStation;
        statdim = intersect(Info.Dimid,nc.Dataset(Info.Station).Dimid(3-CHARDIM));
        %
        if any(statdim==Info.TSMNK)
            % don't assign a station dimension that matches also time/space
            % dimension. This is typically caused by a string
            % representation of time or labels for network nodes.
            Info.Station = [];
        else
            Info.TSMNK(2) = statdim;
        end
    end
    %
    xName = '';
    if strcmp(Info.Type,'simple_geometry')
        Info.TSMNK(3) = nc.Dataset(Info.Mesh{4}).Dimid;
    elseif strcmp(Info.Type,'ugrid_mesh') && iscell(Info.Mesh) && strcmp(Info.Mesh{1},'ugrid1d_network')
        crds = Info.Coordinates;
        for i = 1:length(crds)
            crds{i} = nc.Dataset(find(strcmp(crds{i},varNames)));
        end
        is_branchid = false(size(crds));
        is_offset = false(size(crds));
        for i = 1:length(crds)
            if strcmp(crds{i}.Type,'unknown')
                Att = crds{i}.Attribute;
                if isempty(Att)
                    j = 0;
                else
                    j = strcmp('units',{Att.Name});
                end
                if any(j)
                    is_offset(i) = true;
                else
                    is_branchid(i) = true;
                end
            end
        end
        branchid = find(is_branchid);
        offset   = find(is_offset);
        %
        if isempty(offset) && numel(branchid)>1
            toffset = offset;
            tbranchid = branchid;
            offset   = find(strcmp([Info.Name '_nodes_branch_offset'],Info.Coordinates));
            branchid = find(strcmp([Info.Name '_nodes_branch_id'],Info.Coordinates));
            if numel(offset)==1 && numel(branchid)==1
                ui_message('error','Missing important metadata for %s coordinates.\nBranch id and branch offset coordinate variables identified by D-Flow FM specific names.',Info.Name)
            else
                offset   = toffset;
                branchid = tbranchid;
            end
        end
        ok = false;
        if isempty(offset)
            clist = sprintf('''%s'', ',Info.Coordinates{:});
            ui_message('error','None of %s node coordinates {%s} has a units attribute.\nUnable to identify the branch offset variable, so X and Y coordinates will not be set.', Info.Name, clist(1:end-2))
        elseif isempty(branchid)
            clist = sprintf('''%s'', ',Info.Coordinates{:});
            ui_message('error','All %s node coordinates {%s} have a units attribute.\nUnable to identify the branch id variable, so X and Y coordinates will not be set.', Info.Name, clist(1:end-2))
        elseif numel(offset)>1 || numel(branchid)>1
            clist = sprintf('''%s'', ',Info.Coordinates{:});
            ui_message('error','Too many %s node coordinates {%s} unable to uniquely identify the branch id and offset variables.\nX and Y coordinates will not be set.', Info.Name, clist(1:end-2))
        else
            ok = true;
        end
        %
        if ok
            Info.X = strmatch(Info.Coordinates{branchid},varNames);
            Info.Y = strmatch(Info.Coordinates{offset},varNames);
            nodeDim = nc.Dataset(Info.X).Dimid;
        else
            j = strcmp('node_dimension',{Info.Attribute.Name});
            ndim = Info.Attribute(j).Value;
            nodeDim = ustrcmpi(ndim,{nc.Dimension.Name});
            if nodeDim<0
                ui_message('error','No node_dimension attribute found on %s; unable to identify spatial dimension.', Info.Name)
            end
        end
        if nodeDim>0
            iDims = setdiff(nodeDim,Info.TSMNK);
            iDim = intersect(iDim,iDims);
            if ~isempty(iDim)
                Info.TSMNK(3) = iDim(1);
            elseif ~isempty(iDims)
                Info.TSMNK(3) = iDims(1);
            end
        end
    elseif  ~isempty(Info.X)
        iX = abs(Info.X);
        %
        iDim = {nc.Dataset(iX).Dimid};
        iDim = unique(cat(2,iDim{cellfun('length',iDim)==1}));
        if length(iX)>1
            xType = {nc.Dataset(iX).Type};
            xCoord = {nc.Dataset(iX).Name};
            iLon = ismember(xType,'longitude');
            iAux = ismember(xCoord,AuxCoordVars);
            if any(iAux & iLon)
                Info.X = Info.X(iAux & iLon);
            elseif any(iLon)
                Info.X = Info.X(iLon);
            elseif any(iAux)
                Info.X = Info.X(iAux);
            end
        end
        iX = abs(Info.X);
        %
        if length(iX)>1
            %
            % If there are multiple x/longitude coordinates, then restrict
            % to the variable(s) with the most dimensions. This is based on
            % the assumption that an N dimensional longitude is probably
            % more informative than an M dimensional "average" longitude
            % when N>M.
            %
            ncdims = cellfun('length',{nc.Dataset(iX).Dimid});
            Info.X = Info.X(ncdims==max(ncdims));
        end
        iX = abs(Info.X);
        %
        if length(iX)>1
            % if there are multiple x/longitude coordinates and some
            % have been specified in the file, then keep only those.
            if any(Info.X>0)
                Info.X = Info.X(Info.X>0);
                iX = Info.X;
            end
            % if there are still multiple x/longitude coordinates then
            % select the first one
            if length(iX)>1 && Info.X(1)>0
                % show the error message only if we are struggling with
                % coordinates that have explicitly been indicated in the
                % file
                Names = {nc.Dataset(iX).Name};
                ui_message('error', ...
                    [{sprintf('Problem detecting x/longitude coordinate for "%s".',Info.Name) ...
                    'Any one of the following variables seems to be valid'} ...
                    Names ...
                    {'Using the first one.'}])
            end
        end
        Info.X = iX(1);
        %
        xName = nc.Dataset(Info.X).Name;
        iDims = setdiff(nc.Dataset(Info.X).Dimid,Info.TSMNK);
        iDim = intersect(iDim,iDims);
        if ~isempty(iDim)
            Info.TSMNK(3) = iDim(1);
        elseif ~isempty(iDims)
            if length(iDims)==2
                % if there are two horizontal dimensions
                dims = strvcat(nc.Dimension(iDims+1).Name);
                dims = dims(:,1)';
                if strcmpi(dims,'NM')
                    % and the first one starts with N and the second one
                    % starts with M, then make sure that the one starting
                    % with M is assigned to the M dimension. The one
                    % starting with N will then automatically assigned to
                    % the N dimension.
                    iDims = iDims(2);
                end
            end
            Info.TSMNK(3) = iDims(1);
        end
        %
        % If X coordinates have been defined, check whether bounds have been given.
        %
        if isempty(nc.Dataset(Info.X).Attribute)
            coordAttribs = {};
        else
            coordAttribs = {nc.Dataset(Info.X).Attribute.Name}';
        end
        j = strmatch('bounds',coordAttribs,'exact');
        if ~isempty(j) && ~isempty(nc.Dataset(Info.X).Attribute(j).Value)
            Info.XBounds = strmatch(nc.Dataset(Info.X).Attribute(j).Value,varNames);
            if isempty(Info.XBounds)
                ui_message('error','The bounds attribute of %s points to %s, but that variable does not exist.',nc.Dataset(Info.X).Name,nc.Dataset(Info.X).Attribute(j).Value)
            else
                nc.Dataset(Info.XBounds).Type = nc.Dataset(Info.X).Type;
            end
        end
    end
    if ~isempty(Info.Y) && ~isequal(Info.Type,'simple_geometry')
        iY = abs(Info.Y);
        %
        iDim = {nc.Dataset(iY).Dimid};
        iDim = unique(cat(2,iDim{cellfun('length',iDim)==1}));
        if length(iY)>1
            yType = {nc.Dataset(iY).Type};
            yCoord = {nc.Dataset(iY).Name};
            iLat = ismember(yType,'latitude');
            iAux = ismember(yCoord,AuxCoordVars);
            if any(iAux & iLat)
                Info.Y = Info.Y(iAux & iLat);
            elseif any(iLat)
                Info.Y = Info.Y(iLat);
            elseif any(iAux)
                Info.Y = Info.Y(iAux);
            end
        end
        iY = abs(Info.Y);
        %
        if length(iY)>1
            Names = {nc.Dataset(iY).Name};
            if ~isempty(xName)
                yName = strrep(strrep(xName,'longitude','latitude'),'LONGITUDE','LATITUDE');
                if isequal(xName,yName)
                    yName = strrep(strrep(xName,'lon','lat'),'LON','LAT');
                    if isequal(xName,yName)
                        yName = strrep(strrep(xName,'x','y'),'X','Y');
                    end
                end
            else
                yName = '';
            end
            yName = strcmp(yName,Names);
            if sum(yName)==1
                iY = iY(yName);
            else
                % if there are multiple y/latitude coordinates and some
                % have been specified in the file, then keep only those.
                if any(Info.Y>0)
                    iY = iY(Info.Y>0);
                end
                % if there are still multiple y/latitude coordinates then
                % select the first one
                if length(iY)>1
                    Names = {nc.Dataset(iY).Name};
                    ui_message('error', ...
                        [{sprintf('Problem detecting y/latitude coordinate for "%s".',Info.Name) ...
                        'Any one of the following variables seems to be valid'} ...
                        Names ...
                        {'Using the first one.'}])
                end
            end
        end
        Info.Y = iY(1);
        %
        iDims = setdiff(nc.Dataset(Info.Y).Dimid,Info.TSMNK);
        iDim = intersect(iDim,iDims);
        if ~isempty(iDim)
            Info.TSMNK(4) = iDim(1);
        elseif ~isempty(iDims)
            Info.TSMNK(4) = iDims(1);
        end
        %
        % If Y coordinates have been defined, check whether bounds have been given.
        %
        if isempty(nc.Dataset(Info.Y).Attribute)
            coordAttribs = {};
        else
            coordAttribs = {nc.Dataset(Info.Y).Attribute.Name}';
        end
        j = strmatch('bounds',coordAttribs,'exact');
        if ~isempty(j) && ~isempty(nc.Dataset(Info.Y).Attribute(j).Value)
            Info.YBounds = strmatch(nc.Dataset(Info.Y).Attribute(j).Value,varNames);
            if isempty(Info.YBounds)
                ui_message('error','The bounds attribute of %s points to %s, but that variable does not exist.',nc.Dataset(Info.Y).Name,nc.Dataset(Info.Y).Attribute(j).Value)
            else
                nc.Dataset(Info.YBounds).Type = nc.Dataset(Info.Y).Type;
            end
        end
    end
    %
    % if there are Z variables that match a sofar unmatched dimension then
    % we prefer such vertical coordinates over Z variables that don't.
    %
    if iscell(Info.Mesh) && isequal(Info.Mesh{1},'ugrid')
        ugridDims = nc.Dataset(Info.Mesh{3}).Mesh(5:end);
        nonmatchUgridDims = setdiff(ugridDims,Info.Dimension);
        for z = 1:length(nc.Dataset)
            if strncmp(nc.Dataset(z).Type,'z-coordinate',12)
                nmDim = setdiff(nc.Dataset(z).Dimension,Info.Dimension);
                if isempty(nmDim)
                    % all dimensions match ... should already be in the list
                elseif length(nmDim) == 1 && ismember(nmDim{1}, nonmatchUgridDims)
                    % one dimension doesn't match ... and that's a
                    % horizontal stagger position
                    if length(nc.Dataset(z).Dimension) > 1
                        % at least one other dimension needs to exist and
                        % match ... (that is assumed to be the vertical
                        % dimension ... could be checked more thoroughly)
                        Info.Z(end+1) = -z;
                    end
                else
                    % too many dimension don't match or not a ugrid stagger
                    % position
                end
            end
        end
    end
    zAddsDims = true(size(Info.Z));
    for z = length(Info.Z):-1:1
        if Info.Z(z)<0
            ZDims = nc.Dataset(-Info.Z(z)).Dimid;
            ZDims = setdiff(ZDims,Info.TSMNK);
            if isempty(ZDims)
                zAddsDims(z) = false;
            end
        end
    end
    if any(zAddsDims)
        Info.Z(~zAddsDims) = [];
    end
    % Don't let one-dimensional vertical coordinates self reference unless
    % its dimension is not associated with the vertical dimension (e.g. bed
    % elevation defined on an unstructured mesh).
    % This causes issues for ocean_sigma which needs time/space varying
    % fields as upper/lower limit for converting the vertical coordinate
    % to an elevation.
    if length(Info.Dimension) == 1 && ~ismember(Info.Dimid,Info.TSMNK)
        Info.Z(abs(Info.Z)==ivar) = [];
    end
    %
    if ~isempty(Info.Z)
        iZ = abs(Info.Z);
        %
        if length(iZ)>1
            % if there are multiple vertical coordinates and some have been
            % specified in the file, then keep only those.
            if any(Info.Z>0)
                Info.Z = Info.Z(Info.Z>0);
                iZ = Info.Z;
            end
            % the ocean_sigma_z_coordinate is preferent over regular sigma-
            % or z-coordinates since it refers to them.
            if length(iZ)>1
                ztypes = {nc.Dataset(abs(iZ)).Type};
                sigz = strcmp(ztypes,'z-coordinate: ocean_sigma_z_coordinate');
                if any(sigz)
                    iZ = iZ(sigz);
                end
            end
            % if there are still multiple vertical coordinates then select
            % the first one - warn about it if we are not doing this by
            % means of automatic detection
            if length(iZ)>1 && Info.Z(1)>0
                Names = {nc.Dataset(iZ).Name};
                ui_message('error', ...
                    [{sprintf('Problem detecting vertical coordinate for "%s".',Info.Name) ...
                    'Any one of the following variables seems to be valid'} ...
                    Names ...
                    {'Using the first one.'}])
            end
        end
        Info.Z = iZ(1);
        %
        ZDims = nc.Dataset(Info.Z).Dimid;
        ZDims = setdiff(ZDims,Info.TSMNK);
        if length(ZDims)>1
            % Z coordinate is multidimensional variable; try to determine
            % z dimension by excluding any assigned dimension ...
            ZDims = setdiff(ZDims,Info.TSMNK );
            % ... and dimensions associated to X and Y coordinates
            % (assuming that those ain't have a vertical dimension).
            for ix = 1:length(Info.X)
                ZDims = setdiff(ZDims,nc.Dataset(Info.X(ix)).Dimid);
            end
            for iy = 1:length(Info.Y)
                ZDims = setdiff(ZDims,nc.Dataset(Info.Y(iy)).Dimid);
            end
            % ... and dimensions associated with other UGRID dimensions
            if iscell(Info.Mesh) && isequal(Info.Mesh{1},'ugrid')
                ugridDims = nc.Dataset(Info.Mesh{3}).Mesh(5:end);
                iUDims = find(ismember({nc.Dimension.Name},ugridDims));
                ZDims = setdiff(ZDims,iUDims);
            end
        end
        if length(ZDims)==1
           Info.TSMNK(5) = ZDims;
        end
    end
    %
    nc.Dataset(ivar) = Info;
end
%
% auto detect UGRID dimensions
%
auto_ugrid = cell(4,nvars);
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    if isempty(Info.Mesh) && ~isempty(iUGrid)
        for u = iUGrid'
            if strcmp(nc.Dataset(u).Type,'ugrid_mesh_contact')
                continue
            end
            [udim,ia,ib] = intersect(Info.Dimension,nc.Dataset(u).Mesh(5:end));
            if ~isempty(udim)
                xdim = strmatch(udim,dimNames,'exact')-1;
                if any(Info.TSMNK([1:2 4:end])==xdim)
                    % dimension already matched to dimension not equal to M
                else
                    ugrid = nc.Dataset(u).Mesh(1:2);
                    Info.Mesh = {ugrid{:} u ib-1};
                    Info.TSMNK(3) = xdim;
                    if ~isnan(Info.TSMNK(4))
                        Info.TSMNK(4) = NaN;
                    end
                    auto_ugrid(:,ivar) = {Info.Name, udim{1}, nc.Dataset(u).Name, gridLoc{ib}}';
                end
                %
                break
            end
        end
        %
        nc.Dataset(ivar) = Info;
    end
end
%
% Process remaining dimensions
%
for ivar = 1:nvars
    Info = nc.Dataset(ivar);
    %
    % SubField dimensions should not match any of already assigned time/coordinate dimensions.
    %
    Info.SubFieldDim = setdiff(Info.Dimid,Info.TSMNK);
    %
    % Identify the character (i.e. string length) dimension - exclude it from subfield dimension list
    %
    if strcmp(Info.Datatype,'char') && ~isempty(Info.SubFieldDim)
        if CHARDIM==1
            Info.CharDim = setdiff(Info.Dimid(1),Info.TSMNK);
        else
            Info.CharDim = setdiff(Info.Dimid(end),Info.TSMNK);
        end
        Info.SubFieldDim = setdiff(Info.SubFieldDim,Info.CharDim);
    end
    %
    % reassign non-character subfield dimensions to M, N, K if they are all
    % undefined. Usually that means that the variable is not a spatial data
    % set. By assigning non-spatial dimensions to the M, N and K dimensions
    % we enable 1D and 2D plots against simple axis.
    %
    CharSubFieldDims    = setdiff(Info.SubFieldChr(:,2)',Info.TSMNK);
    nonCharSubFieldDims = setdiff(Info.SubFieldDim,CharSubFieldDims);
    if all(isnan(Info.TSMNK(2:end)))
        for i = 1:min(3,length(nonCharSubFieldDims))
            Info.TSMNK(2+i) = nonCharSubFieldDims(i);
        end
        nonCharSubFieldDims = nonCharSubFieldDims(4:end);
    end
    Info.SubFieldDim = [CharSubFieldDims nonCharSubFieldDims];
    %
    nc.Dataset(ivar) = Info;
end

function nc = setType(nc,ivar,idim,value)
nc.Dataset(ivar).Type = value;
if numel(idim) == 1
    nc.Dimension(idim).Type = value;
end

function attrib = navo2stdname(attrib,navo_code)
persistent NAVO_codes NAVO_index
if isempty(NAVO_codes)
    NAVO_codes=navotable;
    NAVO_index=cat(1,NAVO_codes{:,1});
end
iTable = find(NAVO_index==navo_code);
if ~isempty(iTable)
    stdname = NAVO_codes{iTable,4};
    longname = NAVO_codes{iTable,3};
else
    stdname = '';
    longname = '';
end
if ~isempty(stdname)
    attrib(end+1).Name = 'standard_name';
    attrib(end).Nctype = 2;
    attrib(end).Attnum = length(attrib)-1;
    attrib(end).Value = stdname;
end
if ~isempty(longname)
    Names = {attrib.Name};
    if isempty(strmatch('long_name',Names,'exact'))
        attrib(end+1).Name = 'long_name';
        attrib(end).Nctype = 2;
        attrib(end).Attnum = length(attrib)-1;
        attrib(end).Value = longname;
    end
end

function NAVO_codes=navotable
NAVO_codes={
    1,'lat','Latitude','latitude'
    2,'lon','Longitude','longitude'
    3,'gridx','Grid X',''
    4,'gridy','Grid Y',''
    5,'depth','Depth','depth'
    6,'water_pressure','Water Pressure','sea_water_pressure'
    7,'air_pressure','Air Pressure','air_pressure'
    8,'sigma','Sigma','ocean_sigma_coordinate'
    9,'layer','Layer',''
    10,'grid_point','Grid Point',''
    11,'land_point','Land Point',''
    12,'water_point','Water Point',''
    13,'time','Valid Time','time'
    14,'water_surf_temp','Water Surface Temperature','sea_surface_temperature'
    15,'water_temp','Water Temperature','sea_water_temperature'
    16,'salinity','Salinity','sea_water_salinity'
    17,'water_u','Eastward Water Velocity','eastward_sea_water_velocity'
    18,'water_v','Northward Water Velocity','northward_sea_water_velocity'
    19,'water_w','Vertical Water Velocity','upward_sea_water_velocity'
    20,'water_grid_u','Grid U Water Velocity',''
    21,'water_grid_v','Grid V Water Velocity',''
    22,'water_grid_w','Grid W Water Velocity',''
    23,'water_pres','Pressure','sea_water_pressure'
    24,'air_u','Eastward Air Velocity','eastward_wind'
    25,'air_v','Northward Air Velocity','northward_wind'
    26,'air_w','Vertical Air Velocity','upward_air_velocity'
    27,'air_gridu','Grid U Air Velocity',''
    28,'air_gridv','Grid V Air Velocity',''
    29,'air_gridw','Grid W Air Velocity',''
    30,'layer_thickness','Layer Thickness',''
    31,'mix_lay','Mixed Layer Thickness','ocean_mixed_layer_thickness'
    32,'surf_el','Water Surface Elevation','sea_surface_height_above_geoid'
    33,'sound_speed','Sound Speed','speed_of_sound_in_sea_water'
    34,'shal_sndch_dep','Shallow Sound Channel Axis Depth',''
    35,'deep_sndch_dep','Deep Sound Channel Axis Depth',''
    36,'snd_crit_dep','Acoustic Critical Depth',''
    37,'snd_exc_dep','Acoustic Depth Excess',''
    38,'sonic_lay_dep','Sonic Layer Depth',''
    39,'mixed_lay_dep','Mixed Layer Depth',''
    40,'bt_rms_err','Bathythermograph RMS Error',''
    41,'sig_wav_ht','Significant Wave Height','sea_surface_wave_significant_height'
    42,'sig_wav_dir','Significant Wave Direction','sea_surface_wave_to_direction'
    43,'sig_wav_per','Significant Wave Period','sea_surface_wind_wave_period'
    44,'mean_wav_ht','Mean Wave Height',''
    45,'mean_wav_dir','Mean Wave Direction',''
    46,'mean_wav_per','Mean Wave Period',''
    47,'swell_ht','Swell Height','sea_surface_swell_wave_significant_height'
    48,'swell_dir','Swell Direction','sea_surface_swell_wave_to_direction'
    49,'swell_per','Swell Period','sea_surface_swell_wave_period'
    %50,'cutoff_frequency','Cutoff Frequency',''
    50,'grid_water_dep','Grid Water Depth',''
    51,'land_mask','Land Mask',''
    52,'grid_lon','Grid Longitude','longitude'
    53,'grid_lat','Grid Latitude','latitude'
    54,'grid_orient','Grid Orientation',''
    55,'point_dep','Depth At Point',''
    56,'tau','Tau',''
    57,'surf_wnd_stress','Surface Wind Stress Magnitude','magnitude_of_surface_downward_stress'
    58,'surf_wnd_stress_e','Eastward Surface Wind Stress','surface_downward_eastward_stress'
    59,'surf_wnd_stress_n','Northward Surface Wind Stress','surface_downward_northward_stress'
    60,'surf_wnd_stress_gridx','Grid X Surface Wind Stress',''
    61,'surf_wnd_stress_gridy','Grid Y Surface Wind Stress',''
    62,'surf_atm_pres','Surface Atmospheric Pressure','air_pressure_at_sea_level'
    200,'water_err','Error Water Velocity',''
    201,'adcp_echo','ADCP Returned Echo Intensity',''
    202,'adcp_pcnt_good','ADCP Average Percent Good',''
    203,'botdep','Bottom Depth','sea_floor_depth_below_geoid'
    204,'water_temp_stdev','Water Temperature St. Dev',''
    205,'salinity_stdev','Salinity St. Dev.',''
    206,'lati','Time Dependent Latitude','latitude'
    207,'loni','Time Dependent Longitude','longitude'
    208,'ship_speed','Ship Speed',''};

function [nc,Info] = parse_simple_geometry(nc,varNames,dimNames,ivar,Info,Attribs)
if nargin<5
    Info = nc.Dataset(ivar);
    Attribs = {Info.Attribute.Name};
end
geometry  = 'simple_geometry';
%
gt = strmatch('geometry_type',Attribs,'exact');
if ~isempty(gt)
    type = Info.Attribute(gt).Value;
    switch type
        case 'multiline'
            ui_message('error','The geometry_type "%s" for variable "%s" is invalid. Correcting to "%s".',type,Info.Name,'line')
            type = 'line';
        case 'multipolygon'
            ui_message('error','The geometry_type "%s" for variable "%s" is invalid. Correcting to "%s".',type,Info.Name,'polygon')
            type = 'polygon';
    end
else
    type = 'undefined';
end
%
nca = strmatch('node_count',Attribs,'exact');
if ~isempty(nca)
    node_count = Info.Attribute(nca).Value;
    vnc = strmatch(node_count,{nc.Dataset.Name},'exact');
    if isempty(vnc)
        ui_message('error','Can''t locate the node_count variable "%s" for variable "%s".',node_count,Info.Name)
        return
    end
end
%
cn = strmatch('node_coordinates',Attribs,'exact');
if ~isempty(cn)
    node_coords = multiline(Info.Attribute(cn).Value,' ','cellrow');
else
    node_coords = {};
end
%
Info.Type = geometry;
Info.Coordinates = node_coords;
Info.Mesh = {geometry type ivar vnc};


function [nc,Info] = parse_ugrid_contact(nc,varNames,dimNames,ivar,Info,Attribs)
if nargin<5
    Info = nc.Dataset(ivar);
    Attribs = {Info.Attribute.Name};
end
% ugrid mesh contact
Info.Type = 'ugrid_mesh_contact';
%
j = strmatch('cf_role',Attribs,'exact');
if isempty(j) || ~strcmp(Info.Attribute(j).Value,'mesh_topology_contact')
    ui_message('error','Attribute ''cf_role'' should be set to ''mesh_topology_contact'' for UGRID mesh contact variable "%s".',Info.Name)
end
%
j = strmatch('contact',Attribs,'exact');
if isempty(j)
    ui_message('error','Attribute ''contact'' should be defined for UGRID mesh contact variable "%s".',Info.Name)
else
    meshLoc = reshape(multiline(Info.Attribute(j).Value,' ','cell'),[2 2])';
end
%
%      contacts:cf_role = "mesh_topology_contact" ;
%      contacts:contact = "mesh2d: face mesh1d: node" ;
%      contacts:contact_type = "contacts_contact_type" ;
%      contacts:contact_id = "contacts_contact_id" ;
%      contacts:contact_long_name = "contacts_contact_long_name" ;
%      contacts:start_index = 1 d;


function [nc,Info] = parse_ugrid_mesh_or_contact(nc,varNames,dimNames,ivar)
Info = nc.Dataset(ivar);
Attribs = {Info.Attribute.Name};
%
j = strmatch('cf_role',Attribs,'exact');
if isempty(j) || (~strcmp(Info.Attribute(j).Value,'mesh_topology') && ~strcmp(Info.Attribute(j).Value,'mesh_topology_contact'))
    ui_message('error','Unable to interpret "%s" as UGRID variable the ''cf_role'' attribute should equal ''mesh_topology'' or ''mesh_topology_contact''.',Info.Name)
elseif strcmp(Info.Attribute(j).Value,'mesh_topology')
    [nc,Info] = parse_ugrid_or_sgrid_mesh(nc,varNames,dimNames,ivar);
elseif strcmp(Info.Attribute(j).Value,'mesh_topology_contact')
    [nc,Info] = parse_ugrid_contact(nc,varNames,dimNames,ivar);
end
if nargout<2
    nc.Dataset(ivar) = Info;
end

function [nc,Info] = parse_ugrid_or_sgrid_mesh(nc,varNames,dimNames,ivar,Info,Attribs)
if nargin<5
    Info = nc.Dataset(ivar);
    Attribs = {Info.Attribute.Name};
end
% ugrid mesh
j = strmatch('node_dimensions',Attribs,'exact');
if isempty(j)
    % ugrid mesh
    Info.Type = 'ugrid_mesh';
    TYPE = 'UGRID';
else
    % sgrid mesh
    Info.Type = 'sgrid_mesh';
    TYPE = 'SGRID';
end
%
tpd = strmatch('topology_dimension',Attribs,'exact');
if isempty(tpd)
    ui_message('error','Missing attribute ''topology_dimension'' for %s mesh variable %s.',TYPE,Info.Name)
    tpd = -1;
else
    tpd = Info.Attribute(tpd).Value;
    if ischar(tpd)
        ui_message('error','Invalid value ''%s'' for attribute ''topology_dimension'' for %s mesh variable %s: should be integer 1, 2 or 3.',tpd,TYPE,Info.Name)
        % try to convert string to number
        tpd = sscanf(tpd,'%i',1);
        if isempty(tpd) || tpd<1 || tpd>3
            tpd =-1;
        end 
    end
end
%
cn = strmatch('node_coordinates',Attribs,'exact');
if ~isempty(cn)
    node_coords = multiline(Info.Attribute(cn).Value,' ','cellrow');
else
    node_coords = {};
end
Info.Coordinates = node_coords;
%AuxCoordVars = union(AuxCoordVars,Info.Coordinates);
%
switch TYPE
    case 'UGRID'
        j = strmatch('cf_role',Attribs,'exact');
        if isempty(j) || ~strcmp(Info.Attribute(j).Value,'mesh_topology')
            ui_message('error','Attribute ''cf_role'' should be set to ''mesh_topology'' for %s mesh variable %s.',TYPE,Info.Name)
        end
        %
        nd = strmatch('node_dimension',Attribs,'exact');
        if ~isempty(nd)
            set_node_dim = Info.Attribute(nd).Value;
        else
            set_node_dim = '';
        end
        if ~isempty(node_coords)
            ndc = find(strcmp(node_coords{1},varNames));
            node_dim = nc.Dataset(ndc).Dimension{1};
        else
            node_dim = '';
        end
        if ~isempty(set_node_dim)
            id = strmatch(set_node_dim,dimNames,'exact');
            if isempty(id)
                ERR = 'Attribute node_dimension of UGRID mesh %s points to ''%s''. This is not a NetCDF dimension in the file. ';
                if isempty(node_dim)
                    ui_message('error',[ERR 'No alternative found.'],Info.Name,set_node_dim)
                else
                    ui_message('error',[ERR 'Using ''%s'' instead.'],Info.Name,set_node_dim,node_dim)
                end
            end
        end
        %
        ed  = strmatch('edge_dimension',Attribs,'exact');
        ce  = strmatch('edge_coordinates',Attribs,'exact');
        enc = strmatch('edge_node_connectivity',Attribs,'exact');
        if ~isempty(ed)
            edge_dim = Info.Attribute(ed).Value;
        else
            edge_dim = '';
        end
        if ~isempty(edge_dim)
            id = strcmpi(edge_dim,dimNames);
            if none(id)
                ui_message('error','Attribute edge_dimension of UGRID mesh %s points to ''%s''. This is not a dimension in the file.',Info.Name,edge_dim)
                edge_dim = '';
            end
        end
        if ~isempty(ce)
            edge_coords = multiline(Info.Attribute(ce).Value,' ','cellrow');
            if ~isempty(edge_coords)
                edc = find(strcmp(edge_coords{1},varNames));
                if ~isempty(edc) && ~isempty(nc.Dataset(edc).Dimension)
                    if isempty(edge_dim)
                        edge_dim = nc.Dataset(edc).Dimension{1};
                    elseif ~ismember(edge_dim,nc.Dataset(edc).Dimension)
                        char_edc_dim = sprintf('%s,',nc.Dataset(edc).Dimension{:});
                        ui_message('error','The edge_dimension "%s" is inconsistent with the dimensions of edge coordinate "%s" {%s}.',edge_dim,edge_coords{1},char_edc_dim(1:end-1))
                    end
                elseif isempty(edc)
                    ui_message('error','The edge coordinate "%s" of %s is not available in the file.',edge_coords{1},Info.Name)
                else % isempty(nc.Dataset(edc).Dimension)
                    ui_message('error','The edge coordinate "%s" of %s is dimensionless.',edge_coords{1},Info.Name)
                end
            else
                ui_message('error','The edge_coordinates attribute of %s is empty.',Info.Name)
            end
        end
        if ~isempty(enc)
            encv = find(strcmp(Info.Attribute(enc).Value,varNames));
            if isempty(encv)
                ui_message('error','The edge_node_connectivity "%s" of %s is not available in the file.',Info.Attribute(enc).Value,Info.Name)
            else
                enc_dim = nc.Dataset(encv).Dimension; % 2 dimensional
                if isempty(edge_dim)
                    edge_dim = enc_dim{1};
                elseif ~ismember(edge_dim,enc_dim)
                    char_enc_dim = sprintf('%s,',enc_dim{:});
                    ui_message('error','The edge_dimension "%s" is inconsistent with the edge_node_connectivity dimensions {%s}.',edge_dim,char_enc_dim(1:end-1))
                end
            end
        else
            if tpd==1
                ui_message('error','No edge_node_connectivity specified for mesh topology %s.',Info.Name)
            end
        end
        %
        fd  = strmatch('face_dimension',Attribs,'exact');
        cf  = strmatch('face_coordinates',Attribs,'exact');
        fnc = strmatch('face_node_connectivity',Attribs,'exact');
        if ~isempty(fd)
            face_dim = Info.Attribute(fd).Value;
        else
            face_dim = '';
        end
        if ~isempty(face_dim)
            id = strcmpi(face_dim,dimNames);
            if none(id)
                ui_message('error','Attribute face_dimension of UGRID mesh %s points to ''%s''. This is not a dimension in the file.',Info.Name,face_dim)
                face_dim = '';
            end
        end
        if ~isempty(cf)
            face_coords = multiline(Info.Attribute(cf).Value,' ','cellrow');
            if ~isempty(face_coords)
                fcc = find(strcmp(face_coords{1},varNames));
                if ~isempty(fcc) && ~isempty(nc.Dataset(fcc).Dimension)
                    if isempty(face_dim)
                        face_dim = nc.Dataset(fcc).Dimension{1};
                    elseif ~ismember(face_dim,nc.Dataset(fcc).Dimension)
                        char_fcc_dim = sprintf('%s,',nc.Dataset(fcc).Dimension{:});
                        ui_message('error','The face_dimension "%s" is inconsistent with the dimensions of face coordinate "%s" {%s}.',face_dim,face_coords{1},char_fcc_dim(1:end-1))
                    end
                elseif isempty(fcc)
                    ui_message('error','The face coordinate "%s" of %s is not available in the file.',face_coords{1},Info.Name)
                else % isempty(nc.Dataset(fcc).Dimension)
                    ui_message('error','The face coordinate "%s" of %s is dimensionless.',face_coords{1},Info.Name)
                end
            else
                ui_message('error','The face_coordinates attribute of %s is empty.',Info.Name)
            end
        end
        if ~isempty(fnc)
            fncv = find(strcmp(Info.Attribute(fnc).Value,varNames));
            if isempty(fncv)
                ui_message('error','The face_node_connectivity "%s" of %s is not available in the file.',Info.Attribute(fnc).Value,Info.Name)
            else
                fnc_dim = nc.Dataset(fncv).Dimension; % 2 dimensional
                if isempty(face_dim)
                    face_dim = fnc_dim{1};
                elseif ~ismember(face_dim,fnc_dim)
                    char_fnc_dim = sprintf('%s,',fnc_dim{:});
                    ui_message('error','The face_dimension "%s" is inconsistent with the face_node_connectivity dimensions {%s}.',face_dim,char_fnc_dim(1:end-1))
                end
            end
        else
            if tpd==2
                ui_message('error','No face_node_connectivity specified for mesh topology %s.',Info.Name)
            end
        end
        %
        if tpd<0
            if ~isempty(face_dim)
                tpd = 2;
            elseif ~isempty(edge_dim)
                tpd = 1;
            else
                ui_message('error','Unable to detect dimensionality of mesh %s.',Info.Name)
            end
        end
        %
        coordspace = strmatch('coordinate_space',Attribs,'exact');
        ugrid = 'ugrid';
        if ~isempty(coordspace)
            if tpd==1
                ugrid = 'ugrid1d_network';
            else
                ui_message('error','Attribute ''coordinate_space'' not supported for %i-dimensional UGRID mesh %s',tpd,Info.Name)
            end
        end
        %
        Info.Mesh = {ugrid tpd ivar -1 node_dim edge_dim face_dim}; % vol_dim
        %
        if ~isempty(node_dim)
            id = strmatch(node_dim,dimNames,'exact');
            nc.Dimension(id).Type = 'ugrid_node';
        end
        %
        if ~isempty(edge_dim)
            id = strmatch(edge_dim,dimNames,'exact');
            nc.Dimension(id).Type = 'ugrid_edge';
        end
        %
        if ~isempty(face_dim)
            id = strmatch(face_dim,dimNames,'exact');
            nc.Dimension(id).Type = 'ugrid_face';
        end

    case 'SGRID'
        j = strmatch('cf_role',Attribs,'exact');
        if isempty(j) || ~strcmp(Info.Attribute(j).Value,'grid_topology')
            ui_message('error','Attribute ''cf_role'' should be set to ''grid_topology'' for %s mesh variable %s.',TYPE,Info.Name)
        end
        %
        nd = strmatch('node_dimensions',Attribs,'exact');
        if ~isempty(nd)
            set_node_dim = multiline(Info.Attribute(nd).Value,' ','cellrow');
        else
            set_node_dim = {};
        end
        if ~isempty(node_coords)
            ndc = find(strcmp(node_coords{1},varNames));
            node_dim = nc.Dataset(ndc).Dimension;
        else
            node_dim = {};
        end
        if ~isempty(set_node_dim)
            for j = 1:length(set_node_dim)
                id = strmatch(set_node_dim{j},dimNames,'exact');
                if isempty(id)
                    ERR = 'Attribute node_dimensions of SGRID mesh %s includes ''%s''. This is not a dimension in the file. ';
                    if isempty(node_dim)
                        ui_message('error',[ERR 'No alternative found.'],Info.Name,set_node_dim)
                    else
                        ui_message('error',[ERR 'Using ''%s'' instead.'],Info.Name,set_node_dim{j},node_dim{j})
                        set_node_dim{j} = node_dim{j};
                    end
                end
            end
        end
        %
        fd = strmatch('face_dimensions',Attribs,'exact');
        cf  = strmatch('face_coordinates',Attribs,'exact');
        if ~isempty(nd)
            set_face_dim = parse_facedims(Info.Attribute(fd).Value);
        else
            set_face_dim = {};
        end
        if ~isempty(cf)
            face_coords = multiline(Info.Attribute(cf).Value,' ','cellrow');
        end
        if ~isempty(face_coords)
            fcc = find(strcmp(face_coords{1},varNames));
            face_dim = nc.Dataset(fcc).Dimension;
        else
            face_dim = {};
        end
        if ~isempty(set_face_dim)
            for j = 1:length(set_face_dim)
                id = strmatch(set_face_dim{j}{1},dimNames,'exact');
                if isempty(id)
                    ERR = 'Attribute face_dimensions of SGRID mesh %s includes ''%s''. This is not a dimension in the file. ';
                    if isempty(node_dim)
                        ui_message('error',[ERR 'No alternative found.'],Info.Name,set_face_dim{j}{1})
                    else
                        ui_message('error',[ERR 'Using ''%s'' instead.'],Info.Name,set_face_dim{j}{1},face_dim{j}{1})
                        set_face_dim{j}{1} = face_dim{j}{1};
                    end
                end
            end
        end
        %
        sgrid = 'sgrid';
        Info.Mesh = {sgrid tpd ivar -1 set_node_dim set_face_dim}; % vol_dim
end
if nargout<2
    nc.Dataset(ivar) = Info;
end


function facedims = parse_facedims(facedimstr)
facedims = {[],[],[]};
id = 0;
while ~isempty(facedimstr)
    id = id+1;
    [fd,~,~,i] = sscanf(facedimstr,'%[^:]');
    [nd,~,~,i2] = sscanf(facedimstr(i+1:end),'%[^ (]');
    [pd,~,~,i3] = sscanf(facedimstr(i+1+i2:end),' (padding:%[^)]');
    pd = strtrim(pd);
    facedimstr = strtrim(facedimstr(i+i2+i3+2:end));
    switch lower(pd)
        case 'none'
            pd = 0;
        case 'low'
            pd = -1;
        case 'high'
            pd = 1;
        case 'both'
            pd = 2;
    end
    facedims{id} = {fd nd pd};
end
facedims(id+1:end) = [];


function merged_mesh = nc_mapmerge(Partitions, hPB)
P1 = Partitions{1};
nData = length(P1.Dataset);
nPart = length(Partitions);
ugrids = zeros(nData,1);
for i = 1:nData
    M = P1.Dataset(i).Mesh;
    if iscell(M) && numel(M)>=4 && isequal(M{1},'ugrid') && isequal(M{4},-1)
        ugrids(i) = i;
    end
end
ugrids(ugrids == 0) = [];
dims = {P1.Dimension.Name};
merged_mesh = [];
NumMeshes = numel(ugrids);
for mesh = NumMeshes:-1:1
    i = ugrids(mesh);
    M = P1.Dataset(i);
    %
    nodeDim = M.Mesh{5};
    iNodeDim = strcmp(nodeDim,dims);
    edgeDim = M.Mesh{6};
    iEdgeDim = strcmp(edgeDim,dims);
    faceDim = M.Mesh{7};
    iFaceDim = strcmp(faceDim,dims);
    %
    xNodeVar = P1.Dataset(M.X).Name;
    yNodeVar = P1.Dataset(M.Y).Name;
    %
    MeshAttribs = P1.Dataset(M.Varid+1).Attribute;
    MeshAttNames = {MeshAttribs.Name};
    isFNC = strcmp(MeshAttNames,'face_node_connectivity');
    if any(isFNC)
        fncVar = MeshAttribs(isFNC).Value;
    else
        % should only occur for 1D mesh
        fncVar = '';
    end
    isENC = strcmp(MeshAttNames,'edge_node_connectivity');
    if any(isENC)
        encVar = MeshAttribs(isENC).Value;
    else
        encVar = '';
    end
    isEFC = strcmp(MeshAttNames,'edge_face_connectivity');
    if any(isEFC)
        efcVar = MeshAttribs(isEFC).Value;
    else
        efcVar = '';
    end
    %
    nNodes = zeros(nPart,1);
    nEdges = zeros(nPart,1);
    nFaces = zeros(nPart,1);
    xNodes = cell(nPart,1);
    yNodes = cell(nPart,1);
    iFaces = cell(nPart,1);
    faceMask = cell(nPart,1);
    faceDomain = cell(nPart,1);
    for p = 1:nPart
        nNodes(p) = Partitions{p}.Dimension(iNodeDim).Length;
        nEdges(p) = Partitions{p}.Dimension(iEdgeDim).Length;
        nFaces(p) = Partitions{p}.Dimension(iFaceDim).Length;
        %
        file = Partitions{p}.Filename;
        xNodes{p} = nc_varget(file,xNodeVar);
        yNodes{p} = nc_varget(file,yNodeVar);
        for partinfo = 1:3
            try
                switch partinfo
                    case 1
                        % for map files ...
                        iFaces{p} = nc_varget(file,'mesh2d_flowelem_globalnr');
                        faceDomain{p} = nc_varget(file,'mesh2d_flowelem_domain');
                    case 2
                        % for net files ...
                        iFaces{p} = nc_varget(file,'iglobal_s');
                        faceDomain{p} = nc_varget(file,'idomain');
                    case 3
                        % for com files ...
                        iFaces{p} = nc_varget(file,'FlowElemGlobalNr');
                        faceDomain{p} = nc_varget(file,'FlowElemDomain');
                end
                break
            catch
            end
        end
        faceMask{p} = faceDomain{p} == p-1;
        %
        progressbar((NumMeshes-mesh)/NumMeshes + (p/(2*nPart))/NumMeshes, hPB);
    end
    nGlbFaces = sum(cellfun(@sum,faceMask));
    glbFNC = NaN(nGlbFaces,6);
    %
    xyNodes = [cat(1,xNodes{:}) cat(1,yNodes{:})];
    [xyUNodes,~,RI] = unique(xyNodes,'rows');
    nGlbNodes = size(xyUNodes,1);
    iNodes = cell(nPart,1);
    offset = 0;
    fnc = cell(nPart,1);
    enc = cell(nPart,1);
    efc = cell(nPart,1);
    edgeMask = cell(nPart,1);
    for p = 1:nPart
        iNodes{p} = RI(offset + (1:nNodes(p)));
        offset = offset + nNodes(p);
        file = Partitions{p}.Filename;
        %
        if ~isempty(fncVar)
            FNC = nc_varget_start_at_one(file,P1,fncVar);
            Mask = isnan(FNC);
            FNC(Mask) = 1;
            FNC = iNodes{p}(FNC);
            FNC(Mask) = NaN;
            fnc{p} = FNC;
            %
            glbInternal = iFaces{p}(faceMask{p});
            ncol = size(FNC,2);
            if size(glbFNC,2) >= size(FNC,2)
                glbFNC(glbInternal,1:ncol) = FNC(faceMask{p},1:ncol);
            else
                glbFNC(end+1:ncol) = NaN;
                glbFNC(glbInternal,:) = FNC(faceMask{p},:);
            end
        end
        %
        if ~isempty(encVar)
            ENC = nc_varget_start_at_one(file,P1,encVar);
            ENC = iNodes{p}(ENC);
            enc{p} = ENC;
        end
        %
        if ~isempty(efcVar)
            EFC = nc_varget_start_at_one(file,P1,efcVar);
            Mask = isnan(EFC) | EFC==0;
            EFC(Mask) = 1;
            eDom = faceDomain{p}(EFC);
            EFC = iFaces{p}(EFC);
            EFC(Mask) = NaN;
            efc{p} = EFC;
            %
            eDom(Mask) = NaN;
            edgeMask{p} = all(eDom>=p-1 | isnan(eDom),2) & any(eDom==p-1,2);
        end
        progressbar((NumMeshes-mesh)/NumMeshes + ((nPart + p)/(2*nPart))/NumMeshes, hPB);
    end
    %
    % nodes are assigned to the same domain as the connected face with the
    % lowest domain number
    %
    nodeDomain  = NaN(nGlbNodes,1);
    for p = nPart:-1:1
        masked = faceMask{p};
        %
        inodes = fnc{p}(masked,:);
        inodes(isnan(inodes))=[];
        %
        nodeDomain(inodes) = p-1;
    end
    nodeMask = cell(nPart,1);
    for p = 1:nPart
        nodeMask{p} = nodeDomain(iNodes{p}) == p-1;
    end
    %
    % edges are assigned to the same domain as the connected face with the
    % lowest domain number.
    %
    % first sort the edge connectivities to determine the edges uniquely
    [glbENC,~,RI] = unique(sort(cat(1,enc{:}),2),'rows');
    nGlbEdges = size(glbENC,1);
    iEdges = cell(nPart,1);
    % loop in reverse order to make sure that the update of glbENC contains
    % in the end the result of the lowest domain number.
    for p = nPart:-1:1
        offset = sum(nEdges(1:p-1));
        iEdges{p} = RI(offset + (1:nEdges(p)));
        % due to the sorting of the node indices, the order of the nodes
        % may not correspond to the order of the nodes in the source
        % domain. We need to copy the original order to avoid messing up
        % the normal and tangential directions.
        glbENC(iEdges{p},:) = enc{p};
    end
    %
    merged_mesh(mesh).Name = M.Name;
    merged_mesh(mesh).Index = i;
    %
    merged_mesh(mesh).nNodes = nGlbNodes;
    merged_mesh(mesh).X = xyUNodes(:,1);
    merged_mesh(mesh).Y = xyUNodes(:,2);
    merged_mesh(mesh).nodeDMask = nodeMask;
    merged_mesh(mesh).nodeGIndex = iNodes;
    %
    unit = [];
    coordAttribs = P1.Dataset(M.X).Attribute;
    j = strcmp('units',{coordAttribs.Name});
    if sum(j) == 1
        unit = coordAttribs(j).Value;
        units = {'degrees_east','degree_east','degreesE','degreeE'};
        if ismember(unit,units)
            unit = 'deg';
        end
    end
    merged_mesh(mesh).XYUnits = unit;
    %
    merged_mesh(mesh).nEdges = nGlbEdges;
    merged_mesh(mesh).EdgeNodeConnect = glbENC;
    merged_mesh(mesh).edgeDMask = edgeMask;
    merged_mesh(mesh).edgeGIndex = iEdges;
    %
    merged_mesh(mesh).nFaces = nGlbFaces;
    merged_mesh(mesh).FaceNodeConnect = glbFNC;
    merged_mesh(mesh).faceDMask  = faceMask;
    merged_mesh(mesh).faceGIndex = iFaces;
end


function FNC = nc_varget_start_at_one(file,P1,fncVar)
FNC = nc_varget(file,fncVar);
f = P1.Dataset(strcmp({P1.Dataset.Name},fncVar)).Attribute;
si = strcmp({f.Name},'start_index');
if any(si)
    start_index = f(si).Value;
else
    start_index = 0;
end
FNC = FNC - start_index + 1;
