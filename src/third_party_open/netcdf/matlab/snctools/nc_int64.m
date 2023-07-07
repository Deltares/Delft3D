function nctype = nc_int64()
%NC_INT64 return constant corresponding to NC_INT64 enumerated constant
%
%   nc_datatype = nc_int64() returns the constant value corresponding to 
%   the NC_INT64 constant in netcdf.h.  This value is not valid for 
%   creating netCDF variables in the classic format.
%
%   See also NC_ADDVAR.

nctype = 10;
return
