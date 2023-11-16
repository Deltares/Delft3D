function test_vara_put_get ( ncfile )
% TEST_VARA_PUT_GET:  

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


data = [1:24]';
data2 = reshape ( data, 6, 4 );
start_coord = [0 0];
count_coord = [3 4];
status = mexnc ( 'put_vara_double', ncid, varid, start_coord, count_coord, data2(1:3,:)' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''put_var_double'' failed on var rh, first half, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

status = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''sync'' failed on file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
start_coord = [3 0]; 
status = mexnc ( 'put_vara_double', ncid, varid, start_coord, count_coord, data2(4:6,:)' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''put_var_double'' failed on var rh, first half, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
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
% Get the middle block.
start_coord = [1 1];
count_coord = [4 2];
[data3, status] = mexnc ( 'get_vara_double', ncid, varid, start_coord, count_coord );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''get_vara_double'' failed on file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end
data3 = data3';

the_diff =  abs ( data3 - data2(2:5,2:3) );
if ( max ( the_diff ) ~= 0 )
	msg = sprintf ( 'PUT_VARA failed..\nGET_VARA failed..\n' );
	error ( msg );
end


fprintf ( 1, 'PUT_VARA passed..\n' );
fprintf ( 1, 'GET_VARA passed..\n' );


status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''close'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end



return
