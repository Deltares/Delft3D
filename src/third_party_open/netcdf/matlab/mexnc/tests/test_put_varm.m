function test_put_varm ( ncfile )

%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, 'write' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''create'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 6 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim y, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 4 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim x, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[varid, status] = mexnc ( 'def_var', ncid, 'rh', 'NC_FLOAT', 2, [ydimid xdimid] );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_var'' failed on var rh, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

status = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''enddef'' failed on file %s, error message '' %s ''\n', mfilename,  ncerr_msg );
	error ( msg );
end


%
% Write the data without transposing first.
data = [1:24]';
data2 = reshape ( data, 6, 4 );
start_coord = [0 0];
count_coord = [6 4];
stride_coord = [1 1];
imap_coord = [1 6];
status = mexnc ( 'put_varm_double', ncid, varid, start_coord, count_coord, stride_coord, imap_coord, data2 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''put_varm_double'' failed on var rh, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

status = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''sync'' failed on file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
% make sure it is the same
[datat,status] = mexnc ( 'get_var_double', ncid, varid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''get_var_double'' failed on file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''close'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

datat = datat';
the_diff = abs ( datat - data2 );
if ( max(the_diff(:)) ~= 0 )
	msg = sprintf ( 'PUT_VARM failed\n' );
end



fprintf ( 1, 'PUT_VARM passed\n' );


return
