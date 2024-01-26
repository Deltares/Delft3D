function nc_padheader ( ncfile, num_bytes )
%NC_PADHEADER  pad header of netCDF-3 file.
%   nc_padheader(NCFILE,NUM_BYTES) pads the header of a netCDF-3 file
%   NCFILE with NUM_BYTES bytes.
% 
%   When a netCDF file gets very large, adding new attributes can become
%   a time-consuming process due to the architecture of netCDF-3 files.  
%   This can be mitigated by padding the netCDF header with additional 
%   bytes.  Subsequent new attributes will not result in long time delays 
%   unless the length of the new attribute exceeds that of the header.
%
%   This routine does not ever need to be used with a netCDF-4 file.
%
%   Example:
%       nc_create_empty('nc3.nc');
%       d = dir('nc3.nc')
%       nc_padheader('nc3.nc',20000);
%       d = dir('nc3.nc')
%
%   See also:  nc_create_empty.

backend = snc_write_backend(ncfile);
switch(backend)
	case 'tmw'
		nc_padheader_tmw(ncfile,num_bytes);
	case 'mexnc'
		nc_padheader_mexnc(ncfile,num_bytes);
end


%--------------------------------------------------------------------------
function nc_padheader_tmw(ncfile,num_bytes)

ncid = netcdf.open(ncfile,'WRITE');

try
    netcdf.reDef(ncid);
    netcdf.endDef(ncid,num_bytes,4,0,4);
catch me
    netcdf.close(ncid);
    rethrow(me);
end

netcdf.close(ncid);



%--------------------------------------------------------------------------
function nc_padheader_mexnc(ncfile,num_bytes)

[ncid,status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	error ( 'snctools:padHeader:mexnc:open', ncerr );
end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 )
	mexnc ( 'close', ncid );
	ncerr = mexnc ( 'strerror', status );
	error ( 'snctools:padHeader:mexnc:redef', ncerr );
end

%
% Sets the padding to be "num_bytes" at the end of the header section.  
% The other values are default values used by "ENDDEF".
status = mexnc ( '_enddef', ncid, num_bytes, 4, 0, 4 );
if ( status ~= 0 )
	mexnc ( 'close', ncid );
	ncerr = mexnc ( 'strerror', status );
	error ( 'snctools:padHeader:mexnc:penddef', ncerr );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	mexnc ( 'close', ncid );
	ncerr = mexnc ( 'strerror', status );
	error ( 'snctools:padHeader:mexnc:close', ncerr );
end
