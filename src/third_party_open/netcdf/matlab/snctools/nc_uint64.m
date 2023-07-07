function nctype = nc_uint64()
%NC_UINT64 return constant corresponding to NC_UINT64 enumerated constant
%
%   nc_datatype = nc_uint64() returns the constant value corresponding to 
%   the NC_UINT64 constant in netcdf.h.  This value is not valid for 
%   creating netCDF variables in the classic format.
%
%   See also NC_ADDVAR.

nctype = 11;
return

