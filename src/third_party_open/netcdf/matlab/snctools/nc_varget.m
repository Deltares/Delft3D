function data = nc_varget(ncfile,varname,varargin)
%NC_VARGET  Retrieve data from netCDF variable or HDF4 data set.
%   DATA = NC_VARGET(NCFILE,VARNAME) retrieves all the data from the 
%   variable VARNAME in the netCDF file NCFILE.  
%
%   DATA = NC_VARGET(NCFILE,VARNAME,START,COUNT) retrieves the contiguous
%   portion of the variable specified by the index vectors START and 
%   COUNT.  Remember that SNCTOOLS indexing is zero-based, not 
%   one-based.  Specifying Inf in COUNT means to retrieve everything 
%   along that dimension from the START coordinate.
%
%   DATA = NC_VARGET(NCFILE,VARNAME,START,COUNT,STRIDE) retrieves 
%   a non-contiguous portion of the dataset.  The amount of
%   skipping along each dimension is given through the STRIDE vector.
%
%   DATA is returned as double precision when this is reasonable. 
%   Consequently, '_FillValue' and 'missing_value' attributes are honored 
%   by flagging those datums as NaN.  Any 'scale_factor' and 'add_offset' 
%   attributes are honored by applying the linear transformation.
%
%   EXAMPLE:  This example file is shipped with R2008b.
%       data = nc_varget('example.nc','peaks',[0 0], [20 30]);
%
%   EXAMPLE: Retrieve data from a URL.  This requires the netcdf-java 
%   backend.
%       url = 'http://coast-enviro.er.usgs.gov/models/share/balop.nc';
%       data = nc_varget(url,'lat_rho');
%
%   Example:  Retrieve data from the example HDF4 file.
%       data = nc_varget('example.hdf','Example SDS');
% 
%   See also:  nc_vargetr, nc_varput, nc_attget, nc_attput, nc_dump.


[data,info] = nc_vargetr(ncfile,varname,varargin{:});

switch(info.Datatype)
    case {'int8','uint8','int16','uint16','int32','uint32', ...
            'single','double'}
        % We will post-process these datatypes.
    otherwise
        return;
end

data = double(data);
data = post_process_fill_value_missing_value(data,info);
data = post_process_scaling(data,info);

%--------------------------------------------------------------------------
function data = post_process_fill_value_missing_value(data,info)
% If the _FillValue or missing_value attributes are present, replace those
% values with NaN.
%
% If both are present, then the _FillValue trumps.

% Locate any fill value or missing value attributes.
fill_value = struct([]);
missing_value = struct([]);


for j = 1:numel(info.Attribute)
    switch(info.Attribute(j).Name)
        case '_FillValue'
            fill_value = info.Attribute(j);
        case 'missing_value'
            missing_value = info.Attribute(j); 
    end
end


% If neither were found, then we are done here.
if isempty(fill_value) && isempty(missing_value)
    return
elseif isempty(fill_value) && ~isempty(missing_value)
    if ~strcmp(missing_value.Datatype,info.Datatype)
        warning('snctools:nc_varget:missingValueMismatch', ...
            'A bad missing_value attribute datatype was detected and ignored.');
        return
    end
    % missing_value attribute present, but no fill value.
    data(data==double(missing_value.Value)) = NaN;
elseif ~isempty(fill_value) && isempty(missing_value)
    if ~strcmp(fill_value.Datatype,info.Datatype)
        warning('snctools:nc_varget:fillValueMismatch', ...
            'A bad missing_value attribute datatype was detected and ignored.');
        return
    end
    % _FillValue present, but no missing_value
    data(data==double(fill_value.Value)) = NaN;
else
    if ~strcmp(fill_value.Datatype,info.Datatype)
        warning('snctools:nc_varget:fillValueMismatch', ...
            'A bad missing_value attribute datatype was detected and ignored.');
        return
    end
    % both are present.  FillValue trumps.
    data(data==double(fill_value.Value)) = NaN;
end


%--------------------------------------------------------------------------
function data = post_process_scaling(data,info)
% The 'scale_factor' and 'add_offset' attributes trigger a linear scaling
% operation.

% Use these defaults if no scale_factor or add_offsets were found.
scale_factor = 1.0;
add_offset = 0.0;


for j = 1:numel(info.Attribute)
    switch(info.Attribute(j).Name)
        case 'scale_factor'
            scale_factor = info.Attribute(j).Value;
        case 'add_offset'
            add_offset = info.Attribute(j).Value; 
    end
end

data = data*scale_factor + add_offset;
