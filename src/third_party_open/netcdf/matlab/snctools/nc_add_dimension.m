function nc_add_dimension ( ncfile, dimension_name, dimension_length )
% This function is deprecated, use nc_adddim instead.

% NC_ADD_DIMENSION:  adds a dimension to an existing netcdf file
%
% USAGE:  nc_add_dimension ( ncfile, dimension_name, dimension_size );
%
% PARAMETERS:
% Input:
%     ncfile:  path to netcdf file
%     dimension_name:  name of dimension to be added
%     dimension_size:  length of new dimension.  If zero, it will be an
%         unlimited dimension.
%
% Example:  create a netcdf file with a longitude dimension with length 
% 360, a latitude dimension with length 180, and an unlimited time 
% dimension.
% 
%     nc_create_empty('myfile.nc');
%     nc_add_dimension('myfile.nc','latitude',360);
%     nc_add_dimension('myfile.nc','longitude',180);
%     nc_add_dimension('myfile.nc','time',0);
%     
% See also:  nc_addvar.

nc_adddim(ncfile,dimension_name,dimension_length);
return
