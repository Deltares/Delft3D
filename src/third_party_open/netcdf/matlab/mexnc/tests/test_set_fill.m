function test_set_fill ( ncfile )
% TEST_SET_FILL
%
% Test 1:  Normal set
% Test 2:  Invalid ncid 
% Test 3:  ncid = []
% Test 4:  non numeric ncid
% Test 5:  Fill mode = []
% Test 6:  Fill mode is non numeric

if nargin < 1
	ncfile = 'foo.nc';
end
error_condition = 0;

%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( ncerr_msg );
end


%
% Check that the default mode is nc_fill
fill_mode = nc_fill_mode;
[old_mode, status] = mexnc ( 'set_fill', ncid, fill_mode );
if ( status < 0 )
	error( mexnc ( 'strerror', status ) );
end
if old_mode ~= fill_mode
	error ( 'The default mode was %d instead of %d.\n',old_mode, fill_mode );
end


fill_mode = nc_fill_mode;
[old_mode, status] = mexnc ( 'set_fill', ncid, fill_mode );
if ( status < 0 )
	error( mexnc ( 'strerror', status ));
end
if old_mode ~= nc_fill_mode
	error ( 'The old mode was %d instead of %d.\n', old_mode, nc_fill_mode );
end


% Create the fixed dimension.  
len_x = 4;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 )
	error( mexnc ( 'strerror', status ));
end


len_y = 6;
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', len_y );
if ( status ~= 0 )
	error( mexnc ( 'strerror', status ) );
end


[varid, status] = mexnc ( 'def_var', ncid, 'z_double', 'NC_DOUBLE', 2, [ydimid xdimid] );
if ( status ~= 0 )
	error( mexnc ( 'strerror', status ));
end


%
% Set the fill value.
status = mexnc ( 'put_att_double', ncid, varid, '_FillValue', nc_double, 1, -1 );
if ( status < 0 )
	error( mexnc ( 'strerror', status ));
end


[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	error(mexnc ( 'strerror', status ));
end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 )
	error ( 'INQ_VARID failed\n');
end


%
% Test PUT/GET_VARA
input_data = [1:1:len_y*len_x];
input_data = reshape ( input_data, len_y, len_x );
input_data = input_data(2:4,1:3);

status = mexnc ( 'PUT_VARA_DOUBLE', ncid, varid, [1 0], [3 3], input_data' );
if ( status < 0 )
	error ( 'PUT_VARA_DOUBLE failed');
end


status = mexnc ( 'SYNC', ncid );
if ( status < 0 )
	error( 'SYNC failed, msg ''%s''', mexnc ( 'strerror', status ) );
end


[output_data, status] = mexnc ( 'GET_VARA_DOUBLE', ncid, varid, [1 0], [3 3] );
if ( status < 0 )
	error ( mexnc ( 'strerror', status ) );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'values written by PUT_VARA_DOUBLE do not match what was retrieved by GET_VARA_DOUBLE' );
end



%
% Test 2:  bad ncid
testid = 'Test 2';
[old_mode, status] = mexnc ( 'set_fill', -20000, fill_mode );
if ( status == 0 )
	error ( 'succeeded when it should have failed' );
end



% Test 3:  [] ncid
testid = 'Test 3';
try
	[old_mode, status] = mexnc ( 'set_fill', [], fill_mode );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'succeeded when it should have failed' );
end


% Test 4:  non numeric ncid
testid = 'Test 4';
try
	[old_mode, status] = mexnc ( 'set_fill', 'ncid', fill_mode );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'succeeded when it should have failed' );
end



% Test 5:  Fill mode = []
testid = 'Test 5';
try
	[old_mode, status] = mexnc ( 'set_fill', ncid, [] );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'succeeded when it should have failed' );
end


% Test 6:  Fill mode is non numeric
testid = 'Test 6';
try
	[old_mode, status] = mexnc ( 'set_fill', ncid, 'fill_mode' );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'succeeded when it should have failed' );
end


%
% CLOSE
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed' );
end

fprintf ( 1, 'SET_FILL succeeded\n' );


return






