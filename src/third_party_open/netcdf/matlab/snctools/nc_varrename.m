function nc_varrename ( ncfile, old_variable_name, new_variable_name )
%nc_varrename Rename netCDF variable.
%   nc_varrename(ncfile,oldvarname,newvarname) renames a netCDF variable
%   from OLDVARNAME to NEWVARNAME.
%
%   Example:
%      nc_create_empty('myfile.nc');
%      nc_adddim('myfile.nc','x',10);
%      v.Name = 'y';
%      v.Datatype = 'double';
%      v.Dimension = { 'x' };
%      nc_addvar('myfile.nc',v);
%      nc_dump('myfile.nc');
%      nc_varrename('myfile.nc','y','z');
%      nc_dump('myfile.nc');
%
%   See also nc_addvar.



backend = snc_write_backend(ncfile);
switch(backend)
	case 'tmw'
    	nc_varrename_tmw( ncfile, old_variable_name, new_variable_name )
	case 'mexnc'
    	nc_varrename_mexnc( ncfile, old_variable_name, new_variable_name )
    otherwise
    	error('snctools:varRename:unsupportedBackend', ...
              'NC_VARRENAME not supported with %s backend.', backend);
end

