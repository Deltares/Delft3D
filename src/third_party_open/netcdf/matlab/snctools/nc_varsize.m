function varsize = nc_varsize(ncfile, varname)
%NC_VARSIZE Return size of netCDF variable.
%
%   VSIZE = NC_VARSIZE(NCFILE,VARNAME) returns the size of a netCDF
%   variable.
%
%   Example:  
%       sz = nc_varsize('example.nc','peaks');
%
%   See also:  NC_INFO.

v = nc_getvarinfo ( ncfile, varname );
varsize = v.Size;

return

