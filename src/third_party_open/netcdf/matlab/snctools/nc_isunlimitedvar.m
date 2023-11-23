function tf = nc_isunlimitedvar(ncfile,varname)
%NC_ISUNLIMITEDVAR determine if variable has unlimited dimension.
%
%   TF = NC_ISUNLIMITEDVAR(NCFILE,VARNAME) returns true if the netCDF
%   variable VARNAME in the netCDF file NCFILE has an unlimited dimension,
%   and false otherwise.
%
%   Example (requires 2008b or higher):
%       nc_dump('example.nc');
%       tf = nc_isunlimitedvar('example.nc','time_series')
%
%   See also NC_ISCOORDVAR, NC_DUMP.

backend = snc_read_backend(ncfile);
switch(backend)
	case 'tmw'
		tf = nc_isunlimitedvar_tmw(ncfile,varname);
    case {'tmw_hdf4','tmw_hdf4_2011a'}
        tf = nc_isunlimitedvar_hdf4(ncfile,varname);
	case 'java'
		tf = nc_isunlimitedvar_java(ncfile,varname);
	case 'mexnc'
		tf = nc_isunlimitedvar_mexnc(ncfile,varname);
	otherwise
		error('snctools:unlimitedVar:unhandledBackend', ...
		      '%s is not a recognized backend.', backend );
end

