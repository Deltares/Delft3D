function test_sync ( ncfile )
% TEST_SYNC
%
% Test 1:  Normal set
% Test 2:  Invalid ncid 
% Test 3:  ncid = []
% Test 4:  non numeric ncid

error_condition = 0;

%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''create'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
% Check that the default mode is nc_fill
fill_mode = nc_fill_mode;
[old_mode, status] = mexnc ( 'set_fill', ncid, fill_mode );
if ( status < 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr_msg );
	error ( msg );
end
if old_mode ~= fill_mode
	msg = sprintf ( '%s:  The default mode was %d instead of %d.\n', mfilename, old_mode, fill_mode );
	error ( msg );
end


%
% Set the mode to nofill
fill_mode = nc_nofill_mode;
fill_mode = nc_fill_mode;
[old_mode, status] = mexnc ( 'set_fill', ncid, fill_mode );
if ( status < 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr_msg );
	error ( msg );
end
if old_mode ~= nc_fill_mode
	msg = sprintf ( '%s:  The old mode was %d instead of %d.\n', mfilename, old_mode, nc_fill_mode );
	error ( msg );
end


% Create the fixed dimension.  
len_x = 4;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim x, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


len_y = 6;
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', len_y );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim y, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[varid, status] = mexnc ( 'def_var', ncid, 'z_double', 'NC_DOUBLE', 2, [ydimid xdimid] );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_var'' failed on var x_short, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
% Set the fill value.
status = mexnc ( 'put_att_double', ncid, varid, '_FillValue', nc_double, 1, -1 );
if ( status < 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr_msg );
	error ( msg );
end


[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''enddef'' failed, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARID failed\n', mfilename );
	error ( msg );
end


%
% Test PUT/GET_VARA
input_data = [1:1:len_y*len_x];
input_data = reshape ( input_data, len_y, len_x );
input_data = input_data(2:4,1:3);

status = mexnc ( 'PUT_VARA_DOUBLE', ncid, varid, [1 0], [3 3], input_data' );
if ( status < 0 )
	msg = sprintf ( '%s:  PUT_VARA_DOUBLE failed\n', mfilename );
	error ( msg );
end


status = mexnc ( 'SYNC', ncid );
if ( status < 0 )
	msg = sprintf ( '%s:  SYNC failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end


[output_data, status] = mexnc ( 'GET_VARA_DOUBLE', ncid, varid, [1 0], [3 3] );
if ( status < 0 )
	msg = sprintf ( '%s:  GET_VARA_DOUBLE failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
if (any(d))
	msg = sprintf ( '%s:  values written by PUT_VARA_DOUBLE do not match what was retrieved by GET_VARA_DOUBLE\n', mfilename  );
	error ( msg );
end


%
% Test 1:  normal sync
testid = 'Test 1';
[status] = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



%
% Test 2:  bad ncid
testid = 'Test 2';
[status] = mexnc ( 'sync', -20000 );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 3:  [] ncid
testid = 'Test 3';
try
	[status] = mexnc ( 'sync', [] );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 4:  non numeric ncid
testid = 'Test 4';
try
	[status] = mexnc ( 'sync', 'ncid' );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% CLOSE
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed' );
end

fprintf ( 1, 'SYNC succeeded\n' );


return






