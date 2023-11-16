function test_vars_put_get ( ncfile )
% TEST_VARS_PUT_GET:  

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
% Put in the odd numbers
data = [1:2:24]';
data2 = reshape ( data, 3, 4 );
start_coord = [0 0];
count_coord = [3 4];
stride_coord = [2 1];
status = mexnc ( 'put_vars_double', ncid, varid, start_coord, count_coord, stride_coord, data2' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''put_vars_double'' failed on var rh, first half, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

%
% Put in the even numbers
data = [2:2:24]';
data2 = reshape ( data, 3, 4 );
start_coord = [1 0];
count_coord = [3 4];
stride_coord = [2 1];
status = mexnc ( 'put_vars_double', ncid, varid, start_coord, count_coord, stride_coord, data2' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''put_vars_double'' failed on var rh, first half, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

status = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''sync'' failed on file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end



%
% now check that we can retrieve
% Extract the 1st, 2nd, and 5th, 6th multiples of 3
start_coord = [2 0];
count_coord = [2 2];
stride_coord = [3 2];
[data3, status] = mexnc ( 'get_vars_double', ncid, varid, start_coord, count_coord, stride_coord );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''get_vara_double'' failed on file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end
data3 = data3';

the_diff =  abs ( data3 - [3 15; 6 18] );
if ( max ( the_diff ) ~= 0 )
	msg = sprintf ( 'PUT_VARS failed..\nGET_VARS failed..\n' );
	error ( msg );
end


fprintf ( 1, 'PUT_VARS passed..\n' );
fprintf ( 1, 'GET_VARS passed..\n' );


status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''close'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end



return
