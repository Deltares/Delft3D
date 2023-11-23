function dinfo = nc_getdiminfo(ncfile,dimname,field)
%NC_GETDIMINFO:  returns metadata about a specific NetCDF dimension
%
%   INFO = NC_GETDIMINFO(NCFILE,DIMNAME) returns information about the
%   dimension DIMNAME in the netCDF file NCFILE.
%
%   Upon output, DINFO will have the following fields.
%
%      Name:  
%          a string containing the name of the dimension.
%      Length:  
%          a scalar equal to the length of the dimension
%      Unlimited:  
%          A flag, either 1 if the dimension is an unlimited dimension
%          or 0 if not.
%
%   INFO = NC_GETDIMINFO(NCFILE,DIMNAME,<'field'>) returns only one of 
%   the above fields: Length or Unlimited. Handy for use in expressions.
%
%   Example:  requires R2008b or higher.
%       info = nc_getdiminfo('example.nc','x')
%
%   Example:  requires R2008b or higher.
%       dlen = nc_getdiminfo('example.nc','x','Length')
%
%   See also:  NC_DUMP, NC_INFO, NC_GETVARINFO.
%


backend = snc_read_backend(ncfile);
switch(backend)
	case 'tmw'
		dinfo = nc_getdiminfo_tmw(ncfile,dimname);
	case 'java'
		dinfo = nc_getdiminfo_java(ncfile,dimname);
	case 'mexnc'
		dinfo = nc_getdiminfo_mexnc(ncfile,dimname);
    case 'tmw_hdf4'
        dinfo = nc_getdiminfo_hdf4(ncfile,dimname);
    case 'tmw_hdf4_2011a'
        dinfo = nc_getdiminfo_hdf4_2011a(ncfile,dimname);
	otherwise
		error('snctools:unhandledBackend', ...
		      'Unhandled backend ''%s''', backend );
end

if nargin > 2
    dinfo = dinfo.(field);
end
