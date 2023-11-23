function varargout = asciiwind(cmd,varargin)
%ASCIIWIND Read operations for ascii wind files.
%   INFO = ASCIIWIND('open',FILENAME) opens the ascii wind file and
%   determines the wind time series characteristics. Supported file types
%   include: 'meteo_on_equidistant_grid', 'meteo_on_curvilinear_grid',
%   'meteo_on_spiderweb_grid', and 'meteo_on_computational_grid'.
%
%   DATA = ASCIIWIND('read',INFO,QUANT,TIME,ROWS,COLS) read data for
%   quantity QUANT (specified either as index in the array of quantities or
%   as string specifying the name of the requested quantity) at the
%   specified TIME (use : for all times). The optional ROWS and COLS
%   determine the grid indices for which to return data (use : for all
%   indices, which is the default).
%
%   [X,Y,UNIT] = ASCIIWIND('grid',INFO,TIME,ROWS,COLS) return x and y
%   coordinates (as well as string indicating coordinate unit) associated
%   with the data at the specified TIME (use : for all times at which data
%   is provided). The optional ROWS and COLS determine the grid indices for
%   which to return data (use : for all indices, which is the default). The
%   coordinates are only time-dependent for 'meteo_on_spiderweb_grid'; for
%   all other files, the same coordinates are returned for all selected
%   times.
%
%   See also ARCGRID.

%   ASCIIWIND('to_netcdf', INFO, FILENAME) writes the data read from the
%   ascii wind file specified by INFO to a new netCDF file FILENAME.

%   MERGED = ASCIIWIND('merge',INFO1,INFO2) verifies whether INFO1 and
%   INFO2 are compatible data structures and if so, it returns INFO1 as
%   MERGED with an extra field called 'Vector' containing the INFO2.

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

if nargin==0
    if nargout>0
        varargout=cell(1,nargout);
    end
    return
end
switch lower(cmd)
    case 'open'
        varargout{1} = Local_open_file(varargin{:});
    case 'read'
        varargout{1} = Local_read_file(varargin{:});
    case 'merge'
        varargout{1} = merge_files(varargin{:});
    case 'grid'
        [varargout{1:3}] = Local_get_grid(varargin{:});
    case 'to_netcdf'
        Local_write_netcdf(varargin{:});
    otherwise
        error('Unknown command: "%s"',cmd)
end


function Data = Local_read_file(Structure,Quant,t,varargin)
if nargin<2
    if Structure.Header.n_quantity>1
        error('No quantity selected')
    else
        Quant=1;
    end
elseif ischar(Quant)
    if isequal(Quant,':')
        Quant = 1:Structure.Header.n_quantity;
    else
        iQuant = ustrcmpi(Quant,Structure.Header.quantity);
        if iQuant<0
            error('Unknown quantity: %s',Quant)
        end
        Quant = iQuant;
    end
elseif any(Quant<0)
    error('Quantity index %i out of range',min(Quant))
elseif any(Quant>Structure.Header.n_quantity)
    error('Quantity index %i out of range',max(Quant))
end
is_direction = strcmp(Structure.Header.quantity(Quant), 'wind_from_direction');
is_pressure = strcmp(Structure.Header.quantity(Quant), 'p_drop');
nQuant = length(Quant);
%
% if nQuant>1
%     error('Requesting multiple quantities not yet supported.')
% end
%
if nargin<3
    if length(Structure.Data)==1
        t = 1;
    else
        error('No time selected')
    end
elseif strcmp(t,':')
    t = 1:length(Structure.Data);
end
%
if nargin>3
    rows = varargin{1};
else
    rows = ':';
end
if nargin>4
    cols = varargin{2};
else
    cols = ':';
end
fid = fopen(Structure.FileName,'r');
for i = 1:length(t)
    fseek(fid,Structure.Data(t(i)).offset,-1);
    OneTime = fscanf(fid,'%f',[Structure.NVal Structure.Header.n_quantity]);
    OneTime = OneTime(:,Quant);
    switch lower(Structure.Header.filetype)
        case {'meteo_on_equidistant_grid','field_on_equidistant_grid'}
            %TODO: nQuant>1
            OneTime = reshape(OneTime,[Structure.Header.n_cols Structure.Header.n_rows])';
            OneTime = flipud(OneTime);
        case {'meteo_on_curvilinear_grid','field_on_curvilinear_grid'}
            %TODO: nQuant>1
            if ~isfield(Structure.Header,'data_row')
                Structure.Header.data_row = 'grid_row';
            end
            if ~isfield(Structure.Header,'first_data_value')
                %Structure.Header.first_data_value = 'grid_llcorner'; % this is how it was intended: as a concatenation of dep-files
                Structure.Header.first_data_value = 'grid_ulcorner'; % this is how it was interpreted
            end
            switch Structure.Header.data_row
                case 'grid_row' % which corresponds to column here
                    sz = size(Structure.Header.grid_file.X);
                    OneTime = reshape(OneTime,sz);
                case {'grid_col','grid_column'} % which corresponds to row here
                    sz = size(Structure.Header.grid_file.X);
                    sz = fliplr(sz);
                    OneTime = reshape(OneTime,sz)';
            end
            switch Structure.Header.first_data_value
                case 'grid_llcorner' % (1,1) coordinate
                    % default
                case 'grid_ulcorner' % (1,NMAX) coordinate
                    OneTime = fliplr(OneTime);
                case 'grid_lrcorner' % (MMAX,1) coordinate
                    OneTime = flipud(OneTime);
                case 'grid_urcorner' % (MMAX,NMAX) coordinate
                    OneTime = rot90(OneTime,2);
            end
        case {'meteo_on_spiderweb_grid','field_on_spiderweb_grid'}
            OneTime = reshape(OneTime,[Structure.Header.n_cols Structure.Header.n_rows nQuant]);
            OneTime = permute(OneTime,[2 1 3]);
            OneTime = OneTime([1 1:end],[1:end 1],:);
            % fill the eye -- Delft3D approach (FM? also for 4?)
            OneTime(1,:,:) = 0; % set to zero by default
            OneTime(1,:,is_direction) = OneTime(2,:,is_direction); % keep direction constant to the eye
            OneTime(1,:,is_pressure) = Structure.Data(t(i)).p_drop_spw_eye; % use eye pressure for the eye
            % fill the eye -- SFINCS approach
            % OneTime(1,:,:) = 2*OneTime(2,:,:) - OneTime(3,:,:); % linearly extrapolate trend of innermost pair of data points
        case {'meteo_on_computational_grid','field_on_computational_grid'}
            %TODO: nQuant>1
            if isfield(Structure.Header,'grid_file')
                OneTime = reshape(OneTime,size(Structure.Header.grid_file.X)+1);
            end
    end
    if ~isempty(varargin)
        OneTime = OneTime(rows,cols,:);
    end
    if isfield(Structure.Header,'nodata_value')
       OneTime(OneTime==Structure.Header.nodata_value)=NaN;
    end
    if i == 1
        Data = zeros([length(t) size(OneTime)]);
    end
    Data(i,:) = OneTime(:);
end
fclose(fid);


function [X,Y,grid_unit] = Local_get_grid(Structure,t,varargin)
if nargin<2
    if ~strcmpi(Structure.Header.filetype,'meteo_on_spiderweb_grid') || length(Structure.Data)==1
        t = 1;
    else
        error('No time selected')
    end
elseif isequal(t,':') && ischar(t)
    t = 1:length(Structure.Data);
elseif isempty(t)
    t = 1;
end
%
Header = Structure.Header;
if nargin>2
    rows = varargin{1};
else
    rows = ':';
end
if nargin>3
    cols = varargin{2};
else
    cols = ':';
end
grid_unit = '';
for i = 1:length(t)
    switch lower(Header.filetype)
        case {'meteo_on_equidistant_grid','field_on_equidistant_grid'}
            x = Header.x_llcorner + ...
                repmat((cols-1)*Header.dx,length(rows),1);
            y = Header.y_llcorner + ...
                repmat((rows'-1)*Header.dy,1,length(cols));
            if isfield(Header,'grid_unit')
                grid_unit = Header.grid_unit;
            end
            t = 1;
        case {'meteo_on_curvilinear_grid','field_on_curvilinear_grid','meteo_on_computational_grid','field_on_computational_grid'}
            x = Header.grid_file.X(rows,cols);
            y = Header.grid_file.Y(rows,cols);
            if isfield(Header.grid_file,'CoordinateSystem')
                switch lower(Header.grid_file.CoordinateSystem)
                    case 'spherical'
                        grid_unit = 'deg';
                    case 'cartesian'
                        grid_unit = 'm';
                end
            end
            t = 1;
        case {'meteo_on_spiderweb_grid','field_on_spiderweb_grid'}
            if isfield(Header,'grid_unit')
                grid_unit = Header.grid_unit;
            end
            if strcmpi(grid_unit,'degree')
                [x,y] = getSphericalSpiderweb(...
                    Structure.Data(t(i)).x_spw_eye, ...
                    Structure.Data(t(i)).y_spw_eye, ...
                    Header.spw_radius, ...
                    Header.n_rows,rows, ...
                    Header.n_cols,cols);
            else
                r = repmat(Header.spw_radius*(rows-1)'/Header.n_rows,1,length(cols));
                phi = pi/2 - repmat(2*pi*(cols-1)/Header.n_cols,length(rows),1);
                x = Structure.Data(t(i)).x_spw_eye + r.*cos(phi);
                y = Structure.Data(t(i)).y_spw_eye + r.*sin(phi);
            end
    end
    if i == 1
        X = zeros([length(t) size(x)]);
        Y = zeros([length(t) size(y)]);
    end
    X(i,:) = x(:);
    Y(i,:) = y(:);
end


function [lon,lat]=getSphericalSpiderweb(lon_spw_eye,lat_spw_eye,spw_radius,n_rows,rows,n_cols,cols)
lonrad = lon_spw_eye*pi/180;
latrad = lat_spw_eye*pi/180;

earth_radius = 6378137;

% create grid surrounding north pole (lon=0, lat=pi/2)
%
%x = [cos(lonrad)*cos(latrad)
%     sin(lonrad)*cos(latrad)
%     sin(latrad)]

latrad_grid = repmat(asin(cos((rows-1)'*spw_radius/n_rows/earth_radius)),1,length(cols));
lonrad_grid = repmat(pi*(1-2*(cols-1)/n_cols),length(rows),1);

x_grid_np = cos(lonrad_grid).*cos(latrad_grid);
y_grid_np = sin(lonrad_grid).*cos(latrad_grid);
z_grid_np = sin(latrad_grid);

% shift grid to lon=0, lat=0
%
%latrad = -pi/2;
%
%x = [cos(latrad) 0 -sin(latrad)
%     0           1  0
%     sin(latrad) 0  cos(latrad)]*x;

x_grid_eq =  z_grid_np;
y_grid_eq =  y_grid_np;
z_grid_eq = -x_grid_np;

% shift grid to eye location (lonrad,latrad)
%
%x = [cos(lonrad)*cos(latrad) -sin(lonrad) -sin(latrad)*cos(lonrad)
%     sin(lonrad)*cos(latrad)  cos(lonrad) -sin(latrad)*sin(lonrad)
%     sin(latrad)              0            cos(latrad)]*x;

x_grid_eye = x_grid_eq*cos(lonrad)*cos(latrad) -y_grid_eq*sin(lonrad) -z_grid_eq*sin(latrad)*cos(lonrad);
y_grid_eye = x_grid_eq*sin(lonrad)*cos(latrad) +y_grid_eq*cos(lonrad) -z_grid_eq*sin(latrad)*sin(lonrad);
z_grid_eye = x_grid_eq*sin(latrad)                                    +z_grid_eq*cos(latrad);

% convert to lon,lat
%
%x = [cos(lonrad)*cos(latrad)
%     sin(lonrad)*cos(latrad)
%     sin(latrad)]

lat = asin(z_grid_eye)*180/pi;
lon = atan2(y_grid_eye,x_grid_eye)*180/pi;

% move longitude back to value range specified in input file

lon = lon - atan2(y_grid_eye(1),x_grid_eye(1))*180/pi + lon_spw_eye;


function Structure=Local_open_file(filename,vector)
if nargin<2
    vector = 0;
end
Structure.Check='NotOK';
Structure.FileType='asciiwind';
Structure.Header.fileversion = 1.0;

fid=fopen(filename,'r');
Structure.FileName=filename;
if fid<0
    return
end
%
itime = 1;
timeformat = 1;
while ~feof(fid)
    [keyw,value] = fgetl_keyval(fid);
    %
    switch keyw
        case {'nodata_value','n_cols','n_rows','x_llcorner','y_llcorner', ...
                'x_llcenter','y_llcenter','dx','dy','n_quantity','spw_radius', ...
                'spw_merge_frac', 'fileversion'}
            value = sscanf(value,'%f');
        case 'time'
            [Structure.Data(itime).dtime,Structure.Data(itime).timezone,RefDate] = value2time_since(value);
            Structure.Data(itime).time = RefDate + Structure.Data(itime).dtime;
            Structure.Header.RefDate = RefDate;
            break
        case ''
            % might still be a time specification in FM format
            % /* TIME (HRS)      0.0  20000101 0
            [key1,rem1] = strtok(value);
            key2 = strtok(rem1);
            if strcmp(key1,'/*') && strcmpi(key2,'time')
                [Structure.Data(itime).dtime,Structure.Data(itime).timezone,RefDate] = value2time_old(value);
                fgetl_noncomment(fid); % skip the line, fgetl_keyval reset the reading point to before this line since it didn't contain an equal sign
                timeformat = 2;
                break
            else
                fclose(fid);
                error('Error reading line:\n%s\nMissing comment symbol or keyword assignment.',value)
            end
        otherwise
            %value = value;
    end
    %
    % Support version 1.0 keywords
    %
    switch keyw
        case 'curvi_grid_file'
            keyw = 'grid_file';
        case {'meteotype','filetype'}
            keyw = 'filetype';
            switch value
                case 'arcinfo'
                    value = 'meteo_on_equidistant_grid';
                case 'spiderweb'
                    value = 'meteo_on_spiderweb_grid';
                case {'svwp_on_flow_grid','meteo_on_flow_grid'}
                    value = 'meteo_on_computational_grid';
                case 'curvi'
                    value = 'meteo_on_curvilinear_grid';
            end
    end
    %
    if ~isvarname(keyw)
        fclose(fid);
        error('Invalid keyword "%s"',keyw)
    end
    Structure.Header.(keyw) = value;
end
%
% Support version 1.01 keywords
%
if Structure.Header.fileversion<1.02 && strcmp(Structure.Header.filetype,'meteo_on_spiderweb_grid')
    nr = Structure.Header.n_cols;
    Structure.Header.n_cols = Structure.Header.n_rows;
    Structure.Header.n_rows = nr;
end
%
if strcmp(Structure.Header.filetype,'meteo_on_equidistant_grid')
    xllcenter_given = isfield(Structure.Header,'x_llcenter');
    yllcenter_given = isfield(Structure.Header,'y_llcenter');
    xllcorner_given = isfield(Structure.Header,'x_llcorner');
    yllcorner_given = isfield(Structure.Header,'y_llcorner');
    xll_given = xllcenter_given | xllcorner_given;
    yll_given = yllcenter_given | yllcorner_given;
    if ~xll_given
        fclose(fid);
        error('Missing X coordinate data in file "%s"',Structure.FileName)
    elseif ~yll_given
        fclose(fid);
        error('Missing Y coordinate data in file "%s"',Structure.FileName)
    elseif xllcorner_given && xllcenter_given
        fclose(fid);
        error('Multiple X coordinates specified in file "%s"',Structure.FileName)
    elseif yllcorner_given && yllcenter_given
        fclose(fid);
        error('Multiple Y coordinates specified in file "%s"',Structure.FileName)
    else
        % both given and unique. Now it's time to become picky ...
        if (xllcenter_given && ~yllcenter_given) || (xllcorner_given && ~yllcorner_given)
           ui_message('error',{Structure.FileName,'Mixing center and corner coordinate info not allowed for computational engines!'})
        end
        if xllcenter_given
            Structure.Header.x_llcorner = Structure.Header.x_llcenter - Structure.Header.dx/2;
        end
        if yllcenter_given
            Structure.Header.y_llcorner = Structure.Header.y_llcenter - Structure.Header.dy/2;
        end
        % now we always have x_llcorner and y_llcorner.
    end
    %
    if Structure.Header.fileversion>=1.03
        if isfield(Structure.Header,'value_pos')
            ui_message('error',{Structure.FileName,'The value_pos keyword is not allowed anymore!'})
        end
    else
        switch Structure.Header.value_pos
            case 'centre' % x_llcenter and y_llcenter given
                Structure.Header.x_llcorner = Structure.Header.x_llcorner - Structure.Header.dx/2;
                Structure.Header.y_llcorner = Structure.Header.y_llcorner - Structure.Header.dy/2;
            case 'corner' % x_llcorner and y_llcorner given
                % nothing to do
        end
    end
end
%
Q = cell(1,Structure.Header.n_quantity);
U = cell(1,Structure.Header.n_quantity);
for i = 1:Structure.Header.n_quantity
    f = sprintf('quantity%i',i);
    if isfield(Structure.Header,f)
        Q{i} = Structure.Header.(f);
        Structure.Header = rmfield(Structure.Header,f);
        %
        f = sprintf('unit%i',i);
        U{i} = Structure.Header.(f);
        Structure.Header = rmfield(Structure.Header,f);
    else
        Q{i} = f;
        U{i} = '';
    end
end
Structure.Header.quantity = Q;
Structure.Header.unit = U;
%
max_ntimes = 1000;
Structure.Data(max_ntimes).time = [];
%
switch lower(Structure.Header.filetype)
    case {'meteo_on_equidistant_grid','field_on_equidistant_grid'}
        nval = Structure.Header.n_quantity * ...
            Structure.Header.n_cols * ...
            Structure.Header.n_rows;
    case {'meteo_on_curvilinear_grid','field_on_curvilinear_grid'}
        p = fileparts(Structure.Header.grid_file);
        if isempty(p)
            p = fileparts(filename);
            Structure.Header.grid_file = fullfile(p,Structure.Header.grid_file);
        end
        Structure.Header.grid_file = wlgrid('read',Structure.Header.grid_file);
        nval = Structure.Header.n_quantity * ...
            numel(Structure.Header.grid_file.X);
        %
        % do some keyword checks
        %
    case 'first_data_value'
        if ~ismember(Structure.Header.first_data_value,{'grid_llcorner','grid_ulcorner','grid_lrcorner','grid_urcorner'})
            fclose(fid);
            error('Unknown value for "first_data_value" in file: %s',Structure.Header.first_data_value)
        end
        if ~ismember(Structure.Header.data_row,{'grid_row','grid_col','grid_column'})
            fclose(fid);
            error('Unknown value for "data_row" in file: %s',Structure.Header.data_row)
        end
    case {'meteo_on_spiderweb_grid','field_on_spiderweb_grid'}
        nval = Structure.Header.n_quantity * ...
            Structure.Header.n_cols * ...
            Structure.Header.n_rows;
    case {'meteo_on_computational_grid','field_on_computational_grid'}
        nval = inf;
    otherwise
        fclose(fid);
        error('Unknown ASCII wind type: %s',Structure.Header.filetype)
end
%
if Structure.Header.fileversion<1.03 ...
        && strcmp(Structure.Header.filetype,'meteo_on_curvilinear_grid')
    ui_message('error',{Structure.FileName,'This curvilinear grid meteo file is interpreted incorrectly by Delft3D versions 3.28.00 until 3.28.09!'})
end
if Structure.Header.fileversion>1.0 ...
        && strcmp(Structure.Header.filetype,'meteo_on_curvilinear_grid') ...
        && (~strcmp(Structure.Header.first_data_value,'grid_ulcorner') ...
            || ~strcmp(Structure.Header.data_row,'grid_row'))
    msg = {Structure.FileName,'This curvilinear grid meteo file is interpreted incorrectly by D-Flow FM.'};
    if ~strcmp(Structure.Header.first_data_value,'grid_ulcorner')
        msg{end+1} = ['D-Flow FM uses first_data_value = grid_ulcorner instead of ',Structure.Header.first_data_value];
    end
    if ~strcmp(Structure.Header.data_row,'grid_row')
        msg{end+1} = ['D-Flow FM uses data_row = grid_row instead of ',Structure.Header.data_row];
    end
    ui_message('error',msg)
end
%
while 1
    %
    % read parameters for current time
    %
    offset = ftell(fid);
    while ~isempty(keyw)
        offset = ftell(fid);
        [keyw,value] = fgetl_keyval(fid);
        switch keyw
            case {'x_spw_eye','y_spw_eye','p_drop_spw_eye'}
                Structure.Data(itime).(keyw) = sscanf(value,'%f');
            case {'pdrop_spw_eye'}
                if Structure.Header.fileversion>=1.03
                    error('Keyword "pdrop_spw_eye" found instead of "p_drop_spw_eye"!')
                else
                    Structure.Data(itime).p_drop_spw_eye = sscanf(value,'%f');
                end
            case ''
                break
            otherwise
                Structure.Data(itime).(keyw) = value;
        end
    end
    fseek(fid,offset,-1);
    %
    % skip data
    %
    Structure.Data(itime).offset = offset;
    dummy = fscanf(fid,'%f',nval);
    if ~isfinite(nval)
        nval = length(dummy);
    elseif nval > length(dummy)
        fclose(fid);
        error('Not enough data available for data block %i (%s) - expecting %i values, read %i..',itime,datestr(Structure.Data(itime).time,0),nval,length(dummy))
    end
    %
    % read next time
    %
    [keyw,value] = fgetl_keyval(fid);
    if timeformat == 2 && strcmp(strtok(value),'/*')
        fgetl_noncomment(fid); % skip the line, fgetl_keyval reset the reading point to before this line since it didn't contain an equal sign
    elseif isempty(keyw)
        if ~isempty(value)
            fclose(fid);
            error('Unexpected data while reading time block %i (%s).\nExpected new time block or end-of-file, but encountered data: ''%s''.',itime,datestr(Structure.Data(itime).time,0),value)
        end
        break
    end
    %
    itime = itime+1;
    if (itime>max_ntimes)
        max_ntimes = 2*max_ntimes;
        Structure.Data(max_ntimes).time = [];
    end
    %
    if timeformat == 1
        [Structure.Data(itime).dtime,Structure.Data(itime).timezone] = value2time_since(value,RefDate);
    elseif timeformat == 2
        [Structure.Data(itime).dtime,Structure.Data(itime).timezone] = value2time_old(value,RefDate);
    end
    Structure.Data(itime).time = RefDate + Structure.Data(itime).dtime;
end
%
Structure.Data(itime+1:end) = [];
Structure.NVal = nval/Structure.Header.n_quantity;
Structure.Check = 'OK';
fclose(fid);
%
if ~vector && ...
        Structure.Header.n_quantity==1 && ...
        strcmp(Structure.Header.quantity{1}(2:end),'_wind')
    [p,f,e] = fileparts(Structure.FileName);
    switch lower(e)
        case '.amu'
            FileName2 = Structure.FileName;
            FileName2(end) = FileName2(end)+1; %u->v
        case '.amv'
            FileName2 = Structure.FileName;
            FileName2(end) = FileName2(end)-1; %v->u
    end
    vector = 1;
    try
        S2 = Local_open_file(FileName2,vector);
        Structure = merge_files(Structure,S2);
    end
end


function S = merge_files(S,S2)
if isequal(S2.Check,'OK')
    if isequal(S2.Header.quantity{1},S.Header.quantity{1})
        ui_message('','The two wind files should not both contain: %s.',S.Header.quantity{1})
    elseif ~isequal(S2.Header.filetype,S.Header.filetype)
        ui_message('','Grid types of wind files for x and y don''t match.')
    elseif strcmp(S2.Header.filetype,'meteo_on_equidistant_grid')
        if S2.Header.n_cols~=S.Header.n_cols || ...
                S2.Header.n_rows~=S.Header.n_rows
            ui_message('','Grid dimensions in wind files for x and y don''t match.')
        elseif S2.Header.x_llcorner~=S.Header.x_llcorner || ...
                S2.Header.y_llcorner~=S.Header.y_llcorner || ...
                S2.Header.dx~=S.Header.dx || ...
                S2.Header.dy~=S.Header.dy
            ui_message('','Grid locations in wind files for x and y don''t match.')
        elseif (isfield(S2.Header,'value_pos') || isfield(S.Header,'value_pos')) && ~isequal(S2.Header.value_pos,S.Header.value_pos)
            ui_message('','Data locations in wind files for x and y don''t match.')
        else
            % checcking whether the times are the same should not include the
            % offset, i.e. the starting byte location within the file
            T = rmfield(S.Data,'offset');
            T2 = rmfield(S2.Data,'offset');
            if  ~isequal(T2,T)
                ui_message('','Times in wind files for x and y don''t match.')
            else
                S.Vector = S2;
            end
        end
    elseif strcmp(S2.Header.filetype,'meteo_on_curvilinear_grid')
        if ~isequal(S2.Header.grid_file,S.Header.grid_file)
            ui_message('','Grids of wind files for x and y don''t match.')
        else
            T1 = rmfield(S.Data,'offset');
            T2 = rmfield(S2.Data,'offset');
            if ~isequal(T2,T1)
                ui_message('','Times in wind files for x and y don''t match.')
            else
                % the two grids were proven to be equal; however, since the
                % two grids were read independently, the data is stored
                % twice in memory. MATLAB's memory management should
                % keep only one copy in memory if we do the following:
                S2.Header.grid_file = S.Header.grid_file;
                %
                S.Vector = S2;
            end
        end
    else
        S.Vector = S2;
    end
end


function Local_write_netcdf(Structure,filename,varargin)
if ischar(Structure)
    Structure = Local_open_file(Structure);
end

Header = Structure.Header;
switch lower(Header.filetype)
    case {'meteo_on_equidistant_grid','field_on_equidistant_grid','meteo_on_curvilinear_grid','field_on_curvilinear_grid','meteo_on_computational_grid','field_on_computational_grid'}
       error('Converting %s to netCDF not yet implemented.')
    case {'meteo_on_spiderweb_grid','field_on_spiderweb_grid'}
       nc_writer = @Local_write_netcdf_spw;
end
mode = netcdf.getConstant('NETCDF4');
%mode = bitor(mode,netcdf.getConstant('CLASSIC_MODEL'));
ncid = netcdf.create(filename,mode);
try
    nc_writer(Structure,ncid,varargin{:})
    netcdf.close(ncid);
catch e
    netcdf.close(ncid);
    rethrow(e)
end


function Local_write_netcdf_spw(Structure,ncid,wind_opt)
if nargin<3
    wind_opt = 'x+y';
end

Header = Structure.Header;
UNLIMITED = netcdf.getConstant('UNLIMITED');
times = [Structure.Data.dtime]; % time
ref_time = Header.RefDate; %min(times);
%times = times - ref_time;
n_times = length(times);
n_range = Header.n_rows;
ranges = (1:n_range) * Header.spw_radius / n_range;
n_azimuth = Header.n_cols;
azimuths = (0:n_azimuth-1) * 360 / n_azimuth;

time_dim = netcdf.defDim(ncid,'time',UNLIMITED);
range_dim = netcdf.defDim(ncid,'range',n_range);
azimuth_dim = netcdf.defDim(ncid,'azimuth',n_azimuth);

time_var = netcdf.defVar(ncid,'time','double',time_dim);
netcdf.putAtt(ncid,time_var,'standard_name','time');
netcdf.putAtt(ncid,time_var,'calendar','gregorian');
tunits = sprintf('days since %sZ',datestr(ref_time,31));
netcdf.putAtt(ncid,time_var,'units',tunits);

if strcmp(Header.grid_unit,'degree')
    x_var = 'longitude_eye';
    x_sname = 'longitude';
    x_descr = 'longitude of eye';
    x_units = 'degrees_east';
    %
    y_var = 'latitude_eye';
    y_sname = 'latitude';
    y_descr = 'latitude of eye';
    y_units = 'degrees_north';
else
    x_var = 'x_eye';
    x_sname = 'projection_x_coordinate';
    x_descr = 'x-coordinate of eye';
    x_units = 'm';
    %
    y_var = 'y_eye';
    y_sname = 'projection_y_coordinate';
    y_descr = 'y-coordinate of eye';
    y_units = 'm';
end
xy_vars = [y_var ' ' x_var];

range_id = netcdf.defVar(ncid,'range','double',range_dim);
netcdf.putAtt(ncid,range_id,'standard_name','projection_range_coordinate');
netcdf.putAtt(ncid,range_id,'long_name','distance from eye');
netcdf.putAtt(ncid,range_id,'eye_coordinates',xy_vars);
netcdf.putAtt(ncid,range_id,'units','m');
netcdf.putAtt(ncid,range_id,'axis','radial_range_coordinate');

azimuth_id = netcdf.defVar(ncid,'azimuth','double',azimuth_dim);
netcdf.putAtt(ncid,azimuth_id,'standard_name','ray_azimuth_angle');
netcdf.putAtt(ncid,azimuth_id,'long_name','azimuth angle clockwise from North');
netcdf.putAtt(ncid,azimuth_id,'units','degrees');
netcdf.putAtt(ncid,azimuth_id,'eye_coordinates',xy_vars);
netcdf.putAtt(ncid,azimuth_id,'axis','radial_azimuth_coordinate');

x_eye_id = netcdf.defVar(ncid,x_var,'double',time_dim);
netcdf.putAtt(ncid,x_eye_id,'standard_name',x_sname);
netcdf.putAtt(ncid,x_eye_id,'long_name',x_descr);
netcdf.putAtt(ncid,x_eye_id,'units',x_units);

y_eye_id = netcdf.defVar(ncid,y_var,'double',time_dim);
netcdf.putAtt(ncid,y_eye_id,'standard_name',y_sname);
netcdf.putAtt(ncid,y_eye_id,'long_name',y_descr);
netcdf.putAtt(ncid,y_eye_id,'units',y_units);

dim_order = [range_dim,azimuth_dim,time_dim];
reorder = [2 1 3];
dim_order = dim_order(reorder);
switch wind_opt
    case 'mag+from' % mag + from_dir
        wind1_id = netcdf.defVar(ncid,'wind_mag','double',dim_order);
        netcdf.putAtt(ncid,wind1_id,'standard_name','wind_speed');
        netcdf.putAtt(ncid,wind1_id,'units','m/s');
        
        wind2_id = netcdf.defVar(ncid,'wind_dir','double',dim_order);
        netcdf.putAtt(ncid,wind2_id,'standard_name','wind_from_direction');
        netcdf.putAtt(ncid,wind2_id,'units','degrees');
        
    case 'mag+to' % mag + to_dir
        wind1_id = netcdf.defVar(ncid,'wind_mag','double',dim_order);
        netcdf.putAtt(ncid,wind1_id,'standard_name','wind_speed');
        netcdf.putAtt(ncid,wind1_id,'units','m/s');
        
        wind2_id = netcdf.defVar(ncid,'wind_dir','double',dim_order);
        netcdf.putAtt(ncid,wind2_id,'standard_name','wind_to_direction');
        netcdf.putAtt(ncid,wind2_id,'units','degrees');
        
    case 'x+y' % x + y
        wind1_id = netcdf.defVar(ncid,'wind_x','double',dim_order);
        netcdf.putAtt(ncid,wind1_id,'standard_name','eastward_wind');
        netcdf.putAtt(ncid,wind1_id,'units','m/s');
        
        wind2_id = netcdf.defVar(ncid,'wind_y','double',dim_order);
        netcdf.putAtt(ncid,wind2_id,'standard_name','northward_wind');
        netcdf.putAtt(ncid,wind2_id,'units','m/s');
        
    otherwise
        error('Invalid option for storing wind "%s", expecting "mag+from", "mag+to" or "x+y".', var2str(wind_opt))
end

var_id = zeros(1,Header.n_quantity);
iq_wsp = 1;
iq_wfd = 2;
iq_prs = 3;
for iq = 1:Header.n_quantity
    switch Header.quantity{iq}
        case 'wind_speed'
            iq_wsp = iq;
            continue
        case 'wind_from_direction'
            iq_wfd = iq;
            continue
        case 'p_drop'
            iq_prs = iq;
            varname = 'pressure';
            stdname = 'surface_air_pressure';
            longname = 'pressure at the bottom of the atmosphere';
            units = Header.unit{iq};
            p_units = units;
        otherwise
            varname = Header.quantity{iq};
            stdname = '';
            longname = '';
            units = Header.unit{iq};
    end

    var_id(iq) = netcdf.defVar(ncid,varname,'double',dim_order);
    if ~isempty(stdname)
        netcdf.putAtt(ncid,var_id(iq),'standard_name',stdname);
    end
    if ~isempty(longname)
        netcdf.putAtt(ncid,var_id(iq),'long_name',longname);
    end
    netcdf.putAtt(ncid,var_id(iq),'units',units);
end
pressure_id = var_id(iq_prs);

if pressure_id > 0
    netcdf.putAtt(ncid,pressure_id,'eye_value','eye_pressure');
    
    eye_pressure_id = netcdf.defVar(ncid,'eye_pressure','double',time_dim);
    %netcdf.putAtt(ncid,eye_pressure_id,'standard_name','surface_air_pressure');
    netcdf.putAtt(ncid,eye_pressure_id,'long_name','surface air pressure in the eye');
    netcdf.putAtt(ncid,eye_pressure_id,'units',p_units);
end

% check for precipitation?

NC_GLOBAL = netcdf.getConstant('GLOBAL');
netcdf.putAtt(ncid,NC_GLOBAL,'date_modified',datestr(now,31));
if isfield(Structure.Header,'spw_merge_frac')
    spw_merge_frac = Structure.Header.spw_merge_frac;
else
    spw_merge_frac = 0.5;
end
netcdf.putAtt(ncid,NC_GLOBAL,'spw_merge_frac',spw_merge_frac);
netcdf.putAtt(ncid,NC_GLOBAL,'Conventions','CF-1.10 Deltares/Radial-0.1');


netcdf.putVar(ncid,time_var,0,n_times,times);
netcdf.putVar(ncid,range_id,0,n_range,ranges);
netcdf.putVar(ncid,azimuth_id,0,n_azimuth,azimuths);

x_eye = [Structure.Data.x_spw_eye];
y_eye = [Structure.Data.y_spw_eye];
netcdf.putVar(ncid,x_eye_id,x_eye);
netcdf.putVar(ncid,y_eye_id,y_eye);

if pressure_id > 0
    p_eye = [Structure.Data.p_drop_spw_eye];
    netcdf.putVar(ncid,eye_pressure_id,p_eye);
end

store_range = 2:n_range+1;
store_azim = 1:n_azimuth;
for t = 1:n_times
    % use Local_read_file to read the data
    % Note that this adds an index 1 for the eye (range dimension)
    % Note that this adds an index n_azimuth+1 to complete the circle (azimuth dimension)
    DATA = Local_read_file(Structure,':',t);
    % remove the extra data lines mentioned above
    % make sure that the dimension order matches: range, azimuth, time
    DATA = permute(DATA(1,store_range,store_azim,:),[2 3 1 4]);
    switch wind_opt
        case 'mag+from' % mag + from_dir
            u = DATA(:,:,1,iq_wsp);
            v = DATA(:,:,1,iq_wfd);
            
        case 'mag+to' % mag + to_dir
            u = DATA(:,:,1,iq_wsp);
            v = mod(DATA(:,:,1,iq_wfd)+180,360);
            
        case 'x+y' % x + y
            um = DATA(:,:,1,iq_wsp);
            to_dir_cart = 270 - DATA(:,:,1,iq_wfd);
            u = um .* cosd(to_dir_cart);
            v = um .* sind(to_dir_cart);
    end
    start = [0 0 t-1];
    count = [n_range n_azimuth 1];
    start = start(reorder);
    count = count(reorder);
    netcdf.putVar(ncid,wind1_id,start,count,permute(u,reorder))
    netcdf.putVar(ncid,wind2_id,start,count,permute(v,reorder))
    
    for iq = 1:Header.n_quantity
        if var_id(iq) > 0
            netcdf.putVar(ncid,var_id(iq),start,count,permute(DATA(:,:,1,iq),reorder))
        end
    end
end


function [Line,floc] = fgetl_noncomment(fid)
% Skip comment lines starting with #
% first argument the line, second argument the offset of that line in case
% you want to undo the reading
Line = '';
while isempty(Line)
    floc = ftell(fid);
    Line = fgetl(fid);
    if ~ischar(Line) % end-of-file
        Line = '';
        break
    end
    % Remove comments
    Line = deblank(sscanf(Line,'%[^#]'));
end


function [keyw,value] = fgetl_keyval(fid)
[Line,floc] = fgetl_noncomment(fid);
eq = strfind(Line,'=');
if isempty(eq)
    fseek(fid,floc,-1);
    keyw = '';
    value = Line;
else
    keyw = lower(deblank(Line(1:eq-1)));
    value = strtrim(Line(eq+1:end));
end


function [Time,TimeZone,RefDate] = value2time_since(value,RefDate)
X = sscanf(value,'%f %*s since %4d-%2d-%2d %2d:%2d:%2d %c%2d:%2d');
[Time,Remainder] = strtok(value);
tunit = strtok(Remainder);
if nargin > 1
    [Time,TimeZone] = value2time_core(X,tunit,RefDate);
else
    [Time,TimeZone,RefDate] = value2time_core(X,tunit);
end


function [Time,TimeZone,RefDate] = value2time_old(value,RefDate)
[tunit,~,~,i] = sscanf(lower(value),'/* time (%[^)])');
X = sscanf(value(i:end),' %f %4d%2d%2d %f');
if length(X)>4
    % offset time and reference date/time incremented both ... choose either
    % - reference date/time
    % - offset time and first reference date/time
    % implementing the first
    X(1) = 0;
end
X(10) = 0;
if nargin > 1
    [Time,TimeZone] = value2time_core(X,tunit,RefDate);
else
    [Time,TimeZone,RefDate] = value2time_core(X,tunit);
end


function [Time,TimeZone,RefDate] = value2time_core(X,tunit,RefDate)
RefDateNew = datenum(X(2:7)');
if nargin<3
    RefDate = RefDateNew;
end
dRefDate = RefDateNew - RefDate;
TimeZone = (X(9)+X(10)/60)*(44-abs(X(8)));
switch lower(tunit)
    case {'minutes','min','m'}
        Time = dRefDate + X(1)/24/60;
    case {'hours','hrs','h'}
        Time = dRefDate + X(1)/24;
    otherwise
        warning('Unknown time unit.')
        Time = 0;
end
