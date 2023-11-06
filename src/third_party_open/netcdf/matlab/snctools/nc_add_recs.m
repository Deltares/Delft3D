function nc_add_recs(ncfile,new_data,unlimited_dimension)
%NC_ADD_RECS  Add records to end of netCDF file.
%   This function is not recommended.  Use nc_addrecs instead.

%NC_ADD_RECS:  add records onto the end of a netcdf file
%
%   nc_add_recs ( ncfile, new_data,unlimited_dimension );
% 
%   INPUT:
%     ncfile:  netcdf file
%     new_data:  Matlab structure.  Each field is a data array
%        to be written to the netcdf file.  Each array had
%        better be the same length.  All arrays are written
%        in the same fashion. 
%
%   AUTHOR: 
%     john.g.evans.ne@gmail.com

nc_addrecs(ncfile,new_data);
return
