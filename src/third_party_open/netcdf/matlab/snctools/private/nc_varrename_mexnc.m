function nc_varrename_mexnc ( ncfile, old_variable_name, new_variable_name )
% MEXNC backend to NC_VARRENAME.

[ncid,status ]=mexnc('OPEN',ncfile,nc_write_mode);
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varrename:mexnc:OPEN', ncerr );
end


status = mexnc('REDEF', ncid);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varrename:mexnc:REDEF', ncerr );
end


[varid, status] = mexnc('INQ_VARID', ncid, old_variable_name);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varrename:mexnc:INQ_VARID', ncerr );
end


status = mexnc('RENAME_VAR', ncid, varid, new_variable_name);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varrename:mexnc:RENAME_VAR', ncerr );
end


status = mexnc('ENDDEF', ncid);
if status ~= 0
    mexnc('close',ncid);
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varrename:mexnc:ENDDEF', ncerr );
end


status = mexnc('close',ncid);
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varrename:mexnc:CLOSE', ncerr );
end


