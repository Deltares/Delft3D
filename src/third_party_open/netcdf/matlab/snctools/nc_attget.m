function values = nc_attget(ncfile, varname, attribute_name )
%NC_ATTGET  Get the values of a NetCDF attribute.
%
%   att_value = nc_attget(ncfile, varname, attribute_name) retrieves the
%   specified attribute from the variable given by varname in the file
%   specified by ncfile.  In order to retrieve a global attribute, either
%   specify -1 for varname or NC_GLOBAL.
%
%   The following examples require R2008b or higher for the example file.
%
%   Example:  retrieve a variable attribute.
%       values = nc_attget('example.nc','peaks','description');
%
%   Example:  retrieve a global attribute.
%       cdate = nc_attget('example.nc',nc_global,'creation_date');
%
%   See also nc_attput, nc_global.


backend = snc_read_backend(ncfile);
switch(backend)
	case 'tmw'
		values = nc_attget_tmw(ncfile,varname,attribute_name);
	case 'java'
		values = nc_attget_java(ncfile,varname,attribute_name);
	case 'mexnc'
		values = nc_attget_mexnc(ncfile,varname,attribute_name);
    case 'tmw_hdf4'
        values = nc_attget_hdf4(ncfile,varname,attribute_name);
    case 'tmw_hdf4_2011a'
        values = nc_attget_hdf4_2011a(ncfile,varname,attribute_name);
	otherwise
		error('snctools:attget:unhandledBackend', ...
		      '%s is not a recognized backend for SNCTOOLS.', ...
			  backend);
end


return





