function test_varput1 ( ncfile )
% TEST_VARPUT_1
%
% This routine tests VARGET1, VARPUT1
%
% Test 010:  test writing a short datum to a double precision variable.  Bad test, same reason.

if nargin < 1
	ncfile = 'foo.nc';
end
mexnc ( 'setopts', 0 );

create_testfile ( ncfile );
test_varget1_singleton ( ncfile );
test_varget_varput_double ( ncfile );
test_varget1_float ( ncfile );
test_varget1_short ( ncfile );
test_vargput1_varget1_scaling_short ( ncfile );
test_vargput1_varget1_scaling_off_short ( ncfile );
test_2D_varput1 ( ncfile );

test_neg_varput1_varget1_float ( ncfile );
test_neg_varput1_bad_ncid ( ncfile );
test_neg_varget1_bad_ncid ( ncfile );
test_neg_varput1_bad_varid ( ncfile );

fprintf ( 1, 'VARPUT1 succeeded\n' );
fprintf ( 1, 'VARGET1 succeeded\n' );

return




function create_testfile ( ncfile )


%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

% create a singleton
[varid, status] = mexnc('def_var',ncid,'test_singleton',nc_double,0,[]); %#ok<ASGLU>
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

%
% Create the fixed dimension.  
len_x = 100;
len_y = 200;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', len_y );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[varid, status] = mexnc ( 'def_var', ncid, 'z_double', nc_double, 1, [xdimid] ); %#ok<ASGLU>
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[varid, status] = mexnc ( 'def_var', ncid, 'z_float', nc_float, 1, [xdimid] ); %#ok<ASGLU>
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



[z_short_varid, status] = mexnc ( 'def_var', ncid, 'z_short', nc_short, 1, [xdimid] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'def_var', ncid, 'twoD', nc_double, 2, [ydimid xdimid] ); %#ok<ASGLU>
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


eps = 0.01;
status = mexnc ( 'put_att_double', ncid, z_short_varid, 'scale_factor', nc_double, 1, eps );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


status = mexnc ( 'put_att_double', ncid, z_short_varid, 'add_offset', nc_double, 1, 0.00 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


%
% CLOSE
status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return















function test_varget_varput_double ( ncfile );


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 3.14159;
status = mexnc ( 'VARPUT1', ncid, z_double_varid, [0], input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[output_data, status] = mexnc ( 'VARGET1', ncid, z_double_varid, [0] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';

d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'values written by VARGET1 do not match what was retrieved by VARPUT1\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








function test_varget1_singleton ( ncfile )
% Should return a double precision value.

varname = 'test_singleton';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, varname);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 3.14159;
status = mexnc ( 'VARPUT1', ncid, varid, 0, input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'VARGET1', ncid, varid, 0 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'value read by VARGET1 was not correct\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return









function test_varget1_float ( ncfile )
% Should return a double precision value.


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_float');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 3.14159;
status = mexnc ( 'VARPUT1', ncid, varid, [0], input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'VARGET1', ncid, varid, [0] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = double(single(input_data));
d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'values written by VARGET1 do not match what was retrieved by VARPUT1\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return









function test_varget1_short ( ncfile )
% Should return a double precision value.


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_short');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 3;
status = mexnc ( 'VARPUT1', ncid, varid, [0], input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'VARGET1', ncid, varid, [0] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ~strcmp(class(output_data),'double')
	error('output data was not double precision');
end

input_data = double(single(input_data));
d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'values written by VARGET1 do not match what was retrieved by VARPUT1\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return









function test_neg_varput1_varget1_float ( ncfile );
% Should throw an error.  Only double precision or char data is accepted.


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_float');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = single(3.14159);
fail = false;
try %#ok<TRYNC>
	mexnc ( 'VARPUT1', ncid, varid, [0], input_data );
	fail = true;
end
if fail
	error( 'Should have failed.  VARPUT1 should only accepts double or char data.');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return









function test_010 ( ncfile )


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = int16(3.14159);
status = mexnc ( 'VARPUT1', ncid, varid, [0], input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[output_data, status] = mexnc ( 'VARGET1', ncid, varid, [0] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';

d = max(abs(double(output_data)-double(input_data)))';
if (any(d))
	error ( 'values written by VARGET1 do not match what was retrieved by VARPUT1\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return









function test_neg_varput1_bad_ncid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 3.14159;
status = mexnc ( 'VARPUT1', -100, z_double_varid, 0, input_data );
if (status == 0), error ( 'VARPUT1 succeeded with a bad ncid' ); end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







function test_neg_varget1_bad_ncid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'VARGET1', -100, z_double_varid, [0] ); %#ok<ASGLU>
if ( status == 0 ), error ( 'VARGET1 succeeded with a bad ncid' ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








function test_neg_varput1_bad_varid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


status = mexnc ( 'VARPUT1', ncid, -500, [0], 0 );
if status == 0
	error ( 'VARPUT1 succeeded with a bad varid' );
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return



% test VARPUT1/VARGET1 with scale flag set to 1
function test_vargput1_varget1_scaling_short ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_short_varid, status] = mexnc('INQ_VARID', ncid, 'z_short');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[scale_factor, status] = mexnc('GET_ATT_DOUBLE', ncid, z_short_varid, 'scale_factor');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 3.14159;
status = mexnc ( 'VARPUT1', ncid, z_short_varid, [0], input_data,1 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( 'VARPUT1 failed, (%s)\n', ncerr_msg );
end


[output_data, status] = mexnc ( 'VARGET1', ncid, z_short_varid, 0, 1 );
if ( status ~= 0 )
	error ( 'GET_VAR_DOUBLE failed, msg ''%s''\n', mexnc ( 'strerror', status ) );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
ind = find ( d > scale_factor/2 );
if (any(ind))
	error ( 'values written by VARPUT1 do not match what was retrieved by VARGET1\n' );
end




% test VARPUT1/VARGET1 with scale flag set to 0
function test_vargput1_varget1_scaling_off_short ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_short_varid, status] = mexnc('INQ_VARID', ncid, 'z_short');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


input_data = 31;
status = mexnc ( 'VARPUT1', ncid, z_short_varid, 0, input_data,0 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( 'VARPUT1 failed, (%s)\n', ncerr_msg );
end


[output_data, status] = mexnc ( 'VARGET1', ncid, z_short_varid, 0, 0 );
if ( status ~= 0 )
	error('GET_VAR_DOUBLE failed, msg ''%s''\n', mexnc ( 'strerror', status ) );
end

mexnc('close',ncid);

output_data = output_data';

d = max(abs(output_data-input_data))';
ind = find ( d > 0 );
if (any(ind))
	error ( 'values written by VARPUT1 do not match what was retrieved by VARGET1\n');
end






function test_2D_varput1 ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[xdimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[ydimid, status] = mexnc('INQ_DIMID', ncid, 'y');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, xdimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_y, status] = mexnc('INQ_DIMLEN', ncid, ydimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 27;

status = mexnc ( 'VARPUT1', ncid, varid, [50 50], input_data' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUT1 failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGET1', ncid, varid, [50 50] );
if ( status ~= 0 )
	msg = sprintf ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';

if (~strcmp(class(output_data),'double'))
	msg = sprintf ( 'data was not double precision' );
	error ( msg );
end

d = max(abs(output_data-input_data))';
ind = find ( d > 0 );
if (any(ind))
	msg = sprintf ( 'values written by VARPUT do not match what was retrieved by VARGET' );
	error ( msg );
end






