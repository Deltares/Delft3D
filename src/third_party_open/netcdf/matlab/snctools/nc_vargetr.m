function [data,info] = nc_vargetr(ncfile,varname,varargin)
%NC_VARGETR  Retrieve raw data from netCDF variable.
%   [DATA,INFO] = NC_VARGETR(NCFILE,VARNAME) retrieves all the data from 
%   the variable VARNAME in the netCDF file NCFILE.  The data is retrieved
%   'raw', without any transformations with respect to the '_FillValue', 
%   'missing_value', 'add_offset', or 'scale_factor' attributes.  INFO is 
%   the struct of metadata information as retrieve by NC_GETVARINFO. 
%
%   ...  = NC_VARGETR(NCFILE,VARNAME,START,COUNT) retrieves the contiguous 
%   portion of the variable specified by the index vectors START and COUNT.
%   Remember that SNCTOOLS indexing is zero-based, not one-based.  
%   Specifying Inf in COUNT means to retrieve everything along that 
%   dimension from the START coordinate.
%
%   ... = NC_VARGETR(NCFILE,VARNAME,START,COUNT,STRIDE) retrieves a 
%   non-contiguous portion of the dataset.  The amount of skipping along 
%   each dimension is given through the STRIDE vector.
%
%   EXAMPLE:  This example file is shipped with R2008b.
%       data = nc_vargetr('example.nc','peaks',[0 0], [20 30]);
%
%   EXAMPLE: Retrieve data from a URL.  This requires the netcdf-java 
%   backend.
%       url = 'http://coast-enviro.er.usgs.gov/models/share/balop.nc';
%       data = nc_vargetr(url,'lat_rho');
%
%   Example:  Retrieve data from the example HDF4 file.
%       data = nc_vargetr('example.hdf','Example SDS');
% 
%   See also:  nc_varget, nc_varput, nc_attget, nc_attput, nc_dump.
%


retrieval_method = snc_read_backend(ncfile);

switch(retrieval_method)
    case 'tmw'
   		[data,info] = nc_varget_tmw(ncfile,varname,varargin{:});
    case 'java'
        [data,info] = nc_varget_java(ncfile,varname,varargin{:});        
    case 'mexnc'
        [data,info] = nc_varget_mexnc(ncfile,varname,varargin{:});
    case 'tmw_hdf4'
        [data,info] = nc_varget_hdf4(ncfile,varname,varargin{:});
    case 'tmw_hdf4_2011a'
        [data,info] = nc_varget_hdf4_2011a(ncfile,varname,varargin{:});
end

% Check to see if the number of dimensions of the variable exceeds the
% number of dimensions of the data.  If that has happened, then check if
% the missing dimensions correspond to leading singleton dimensions.  If
% that's the case, we can restore them.
if isfield(info,'Size') && (numel(info.Size) > ndims(data))
    n = numel(info.Size) - ndims(data);
    if info.Size(1:n) == ones(1,n)
        data = reshape(data,[ones(1,n) size(data)]);
    end
end
