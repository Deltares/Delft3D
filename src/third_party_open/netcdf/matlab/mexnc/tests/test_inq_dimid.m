function test_inq_dimid ( ncfile )
% TEST_INQ_DIMID
%
% Test 1:  Normal retrieval
% Test 2:  Bad ncid.
% Test 3:  Empty set ncid.
% Test 4:  Non numeric ncid
% Test 5:  Bad name.
% Test 6:  Empty set name.
% Test 7:  Non character name

if nargin == 0
	ncfile = 'foo.nc';
end

error_condition = 0;

%
testid = 'Test 1';
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, mexnc ( 'strerror', status ) );
	error ( err_msg );
end


%
% DIMDEF
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, mexnc ( 'strerror', status ) );
	error ( err_msg );
end

[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, mexnc ( 'strerror', status ) );
	error ( err_msg );
end

[dimid, status] = mexnc ( 'inq_dimid', ncid, 'x' );
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, mexnc ( 'strerror', status ) );
	error ( err_msg );
end
if dimid ~= xdimid
	err_msg = sprintf ( '%s:  INQ_DIMID did not return the expected value.\n', mfilename, ncerr );
	error ( err_msg );
end





% Test 2:  Bad ncid.
testid = 'Test 2';
[dimid, status] = mexnc ( 'inq_dimid', -20000, 'x' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 3:  Empty set ncid.
testid = 'Test 3';
try
	[dimid, status] = mexnc ( 'inq_dimid', [], 'x' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 4:  Non numeric ncid
testid = 'Test 4';
try
	[name, length, status] = mexnc('INQ_DIM', 'ncid', 'x');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





% Test 5:  Bad name.
testid = 'Test 5';
[dimid, status] = mexnc ( 'inq_dimid', ncid, 'y' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 6:  Empty set name.
testid = 'Test 6';
try
	[dimid, status] = mexnc ( 'inq_dimid', ncid, [] );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 7:  Non character name
testid = 'Test 7';
try
	[dimid, status] = mexnc ( 'inq_dimid', ncid, 5 );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end








status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


fprintf ( 1, 'INQ_DIMID succeeded.\n' );


return














