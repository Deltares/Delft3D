function nc_create_empty ( ncfile, mode )
%NC_CREATE_EMPTY  creates an empty netCDF file.
%   NC_CREATE_EMPTY(FILENAME,MODE) creates the empty netCDF file FILENAME
%   with the given MODE.  MODE can be one of the following strings:
%   
%       'clobber'         - deletes existing file, creates netcdf-3 file
%       'noclobber'       - creates netcdf-3 file if it does not already
%                           exist.
%       '64bit_offset'    - creates a netcdf-3 file with 64-bit offset
%       'netcdf4-classic' - creates a netcdf-3 file with 64-bit offset
%
%   MODE can also be a numeric value that corresponds either to one of the
%   named netcdf modes or a numeric bitwise-or of them.
%
%   EXAMPLE:  Create an empty classic netCDF file.
%       nc_create_empty('myfile.nc');
%
%   EXAMPLE:  Create an empty netCDF file with the 64-bit offset mode, but
%   do not destroy any existing file with the same name.
%       mode = bitor(nc_noclobber_mode,nc_64bit_offset_mode);
%       nc_create_empty('myfile.nc',mode);
%
%   EXAMPLE:  Create a netCDF-4 file.  This assumes that you have a 
%   netcdf-4 enabled mex-file.
%       nc_create_empty('myfile.nc','netcdf4-classic');  
%
%   SEE ALSO:  nc_adddim, nc_addvar.

% Set the default mode if necessary.
if nargin == 1
    mode = nc_clobber_mode;
end

tmw_lt_r2008b = false;
tmw_lt_r2010b = false;

% We cannot rely on snc_write_backend to determine how to proceed in this 
% case because the file has not yet been created!
switch ( version('-release') )
    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
        tmw_lt_r2008b = true;
        tmw_lt_r2010b = true;
    case { '2008b', '2009a', '2009b', '2010a'}
        tmw_lt_r2010b = true;
end

switch(mode)
    case { 'clobber', 'noclobber', '64bit_offset' }
        % Do nothing, this is ok
    case 'netcdf4-classic'
        mode = nc_netcdf4_classic;
end

if strcmp(mode,'hdf4')
    sd_id = hdfsd('start',ncfile,'create');
    if sd_id == -1
        error('snctools:createEmpty:hdf4:create', ...
              'Could not create HDF4 file %s.\n', ncfile);
    end
    status = hdfsd('end',sd_id);
    if status == -1
        error('snctools:createEmpty:hdf4:end', ...
              'Could not close HDF4 file %s.\n', ncfile);
    end
elseif (isnumeric(mode) && (mode == 4352) && tmw_lt_r2010b)  || tmw_lt_r2008b
    % either the matlab version is lower than R2008b, or 
    % the mode involved NC_CLASSIC_MODE, implying netcdf-4
    [ncid, status] = mexnc ( 'CREATE', ncfile, mode );
    if ( status ~= 0 )
        ncerr = mexnc ( 'STRERROR', status );
        error ( 'snctools:createEmpty:mexnc:create', ncerr );
    end
    mexnc('close',ncid);
else
    ncid = netcdf.create(ncfile, mode );
    netcdf.close(ncid);
end


