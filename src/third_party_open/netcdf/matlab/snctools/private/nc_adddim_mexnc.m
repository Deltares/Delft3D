function nc_adddim_mexnc(ncfile,dimname,dimlen)
% MEXNC backend to NC_ADDDIM.

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status
    ncerr = mexnc ( 'strerror', status );
    error_id = 'snctools:nc_adddim:openFailed';
    error ( error_id, ncerr );
end

status = mexnc ( 'redef', ncid );
if status
    mexnc ( 'close', ncid );
    ncerr = mexnc ( 'strerror', status );
    error_id = 'snctools:nc_adddim:redefFailed';
    error ( error_id, ncerr );
end

[dimid, status] = mexnc ('def_dim',ncid,dimname,dimlen); %#ok<ASGLU>
if status
    mexnc ( 'close', ncid );
    ncerr = mexnc ( 'strerror', status );
    error_id = 'snctools:nc_adddim:defdimFailed';
    error ( error_id, ncerr );
end

status = mexnc ( 'enddef', ncid );
if status
    mexnc ( 'close', ncid );
    ncerr = mexnc ( 'strerror', status );
    error_id = 'snctools:nc_adddim:enddefFailed';
    error ( error_id, ncerr );
end

status = mexnc ( 'close', ncid );
if status 
    ncerr = mexnc ( 'strerror', status );
    error_id = 'snctools:nc_adddim:closeFailed';
    error ( error_id, ncerr );
end

return
