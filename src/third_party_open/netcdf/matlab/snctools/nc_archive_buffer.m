function new_data = nc_archive_buffer ( input_buffer, ncfile, record_variable )
%NC_ARCHIVE_BUFFER Add record data to end of netCDF file.
%   This function is not recommended.  Use NC_ADDRECS instead.
%
%   See also NC_ADDRECS.

warning ( 'SNCTOOLS:NC_ARCHIVE_BUFFER:deprecated', ...
    '%s is deprecated and WILL BE REMOVED in a future version of SNCTOOLS.  Start using NC_ADDNEWRECS instead.',...
    upper(mfilename));

new_data = [];

error(nargchk(2,3,nargin,'struct'));

switch nargin
case 2
	new_data = nc_addnewrecs ( ncfile, input_buffer );
case { 3, 4 }
	new_data = nc_addnewrecs ( ncfile, input_buffer, record_variable );
end


return;



