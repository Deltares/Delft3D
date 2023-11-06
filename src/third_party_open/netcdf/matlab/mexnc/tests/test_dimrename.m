function test_dimrename ( ncfile )
% TEST_DIMRENAME
%
% Test 1:  Normal rename
% Test 2:  Bad ncid.
% Test 3:  Empty set ncid.
% Test 4:  Bad dimid.
% Test 5:  Empty set dimid.
% Test 5.5:  Dimid is non double and non character
% Test 6:  New dimension name is same as old.
% Test 7:  New dimension name is empty set.
% Test 8:  New dimension name is empty string.


error_condition = 0;

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% DIMDEF
[xdimid, status] = mexnc ( 'dimdef', ncid, 'x', 20 );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[ydimid, status] = mexnc ( 'dimdef', ncid, 'y', 20 );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[zdimid, status] = mexnc ( 'dimdef', ncid, 'z', 20 );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


% Test 1:  Normal rename
testid = 'Test 1';
status = mexnc ( 'DIMRENAME', ncid, xdimid, 'x2' );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end


% Test 2:  Bad ncid.
testid = 'Test 2';
status = mexnc ( 'DIMRENAME', -5, xdimid, 'x3' );
if ( status == 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end


% Test 3:  Empty set ncid.
testid = 'Test 3';
try
	status = mexnc ( 'DIMRENAME', ncid, [], 'x3' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed.\n', mfilename, testid );
	error ( err_msg );
end


% Test 4:  Bad dimid.
testid = 'Test 4';
status = mexnc ( 'DIMRENAME', ncid, -5, 'x3' );
if ( status == 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end


% Test 5:  Empty set dimid.
testid = 'Test 5';
try
	status = mexnc ( 'DIMRENAME', ncid, [], 'x3' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed.\n', mfilename, testid );
	error ( err_msg );
end


% Test 5.5:  Empty set dimid.
testid = 'Test 5.5';
try
	status = mexnc ( 'DIMRENAME', ncid, int32(5), 'x3' );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed.\n', mfilename, testid );
	error ( err_msg );
end


% Test 6:  New dimension name is same as old.
testid = 'Test 6';
status = mexnc ( 'DIMRENAME', ncid, ydimid, 'y' );
if ( status == 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end


% Test 7:  New dimension name is empty set.
testid = 'Test 7';
try
	status = mexnc ( 'DIMRENAME', ncid, zdimid, [] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed.\n', mfilename, testid );
	error ( err_msg );
end


% Test 8:  New dimension name is empty string.
testid = 'Test 8';
try
	status = mexnc ( 'DIMRENAME', ncid, zdimid, '');
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end




%
% ENDEF
[status] = mexnc ( 'enddef', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

fprintf ( 1, 'DIMRENAME succeeded.\n' );


return














