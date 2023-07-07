function nc_adddim(ncfile,dimname,dimlen)
%nc_adddim:  adds a dimension to an existing netcdf file
%   nc_adddim(FILE,dimname,dimlen) adds a dimension with a particular
%   length to the file.  If dimlen is zero, the dimension will be 
%   unlimited.  
%
%   Example:  create a netcdf file with a longitude dimension with length 
%   360, a latitude dimension with length 180, and an unlimited time 
%   dimension.
%       nc_create_empty('myfile.nc');
%       nc_adddim('myfile.nc','latitude',180);
%       nc_adddim('myfile.nc','longitude',360);
%       nc_adddim('myfile.nc','time',0);
%       nc_dump('myfile.nc');
%
%   See also:  nc_addvar.

if ~ischar(dimname)
    error('snctools:adddim:badDimName', 'Dimension name must be char.');
end
if isnumeric(dimlen) && (dimlen < 0)
    error('snctools:adddim:badDimensionLength', ...
        'Dimension lengths cannot be initialized to be less than zero.');
end
    
backend = snc_write_backend(ncfile);
switch(backend)
    case 'tmw'
    	nc_adddim_tmw(ncfile,dimname,dimlen);
    case 'tmw_hdf4'
    	nc_adddim_hdf4(ncfile,dimname,dimlen);
    case 'tmw_hdf4_2011b'
    	nc_adddim_hdf4_2011b(ncfile,dimname,dimlen);
    case 'mexnc'
    	nc_adddim_mexnc(ncfile,dimname,dimlen);
    otherwise
        error('snctools:adddim:unhandledBackend', ...
            'Encountered an unhandled backend string, ''%s''', backend);
end

return
