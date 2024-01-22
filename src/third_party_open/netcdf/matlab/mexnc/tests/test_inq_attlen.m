function test_inq_attlen ( ncfile )
% TEST_INQ_ATTLEN
%

% Test 1:  test a double precision attribute
% Test 2:  bad ncid
% Test 3:  [] ncid
% Test 4:  non numeric ncid
% Test 5:  bad varid
% Test 6:  [] varid
% Test 7:  non numeric varid
% Test 8:  wrong attribute name
% Test 9:  [] attribute name
% Test 10:  '' attribute name
% Test 11:  non character attribute name

error_condition = 0;

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

input_data = [3.14159 5];
status = mexnc ( 'put_att_double', ncid, varid, 'test_double', nc_double, 2, input_data );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end
[status] = mexnc ( 'enddef', ncid );

status = mexnc ( 'sync', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[attlen, status] = mexnc ( 'inq_attlen', ncid, varid, 'test_double' );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

if attlen ~= 2
	err_msg = sprintf ( '%s:  inq_attlen did not return the expected value.\n', mfilename, ncerr );
	error ( err_msg );
end


%--------------------------------------------------------------------------
% Test 2:  bad ncid
testid = 'Test 2';
[attlen, status] = mexnc ( 'inq_attlen', -2000, varid, 'test_double' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 3:  [] ncid
testid = 'Test 3';
try
	[attlen, status] = mexnc ( 'inq_attlen', [], varid, 'test_double' );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 4:  non numeric ncid
testid = 'Test 4';
try
	[attlen, status] = mexnc ( 'inq_attlen', 'ncid', varid, 'test_double' );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




% Test 5:  bad varid
testid = 'Test 5';
[attlen, status] = mexnc ( 'inq_attlen', ncid, -2000, 'test_double' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 6:  [] varid
testid = 'Test 6';
try
	[attlen, status] = mexnc ( 'inq_attlen', ncid, [], 'test_double' );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid, ncerr );
	error ( err_msg );
end


% Test 7:  non numeric varid
testid = 'Test 7';
try
	[attlen, status] = mexnc ( 'inq_attlen', ncid, 'varid', 'test_double' );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid, ncerr );
	error ( err_msg );
end



% Test 8:  wrong attribute name
testid = 'Test 5';
[attlen, status] = mexnc ( 'inq_attlen', ncid, varid, 'test_bad' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 9:  [] attribute name
testid = 'Test 9';
try
	[attlen, status] = mexnc ( 'inq_attlen', ncid, varid, [] );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end


% Test 10:  '' attribute name
testid = 'Test 10';
try
	[attlen, status] = mexnc ( 'inq_attlen', ncid, varid, '' );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end


% Test 11:  non character attribute name
testid = 'Test 11';
try
	[attlen, status] = mexnc ( 'inq_attlen', ncid, 'varid', -2000 );
	error_condition = 1;
end
if ( error_condition == 1 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid, ncerr );
	error ( err_msg );
end




fprintf ( 1, 'INQ_ATTLEN succeeded.\n' );


status = mexnc ( 'close', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


return
















