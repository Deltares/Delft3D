function test_attrename ( ncfile )
% TEST_ATTRENAME
%
% Test 1:  rename an attribute.
% Test 2:  try to rename an attribute using a bad ncid.
% Test 3:  try to rename an attribute using a bad varid.
% Test 4:  try to rename an attribute using a bad source attribute name.
% Test 5:  try to rename an attribute using a bad destination attribute name.


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

input_data = 3.14159;
status = mexnc ( 'put_att_double', ncid, varid, 'test_double', nc_double, 1, input_data );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

%status = mexnc ( 'sync', ncid );
%if ( status < 0 )
%	ncerr = mexnc ( 'strerror', status );
%	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
%	error ( err_msg );
%end

% Test 1:  rename an attribute.
status = mexnc ( 'attrename', ncid, varid, 'test_double', 'babaganoush' );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[status] = mexnc ( 'enddef', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

status = mexnc ( 'sync', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[datatype, attlen, status] = mexnc ( 'inq_att', ncid, varid, 'babaganoush' );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


status = mexnc ( 'redef', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

% Test 2:  try to rename an attribute using a bad ncid.
testid = 'Test 2';
status = mexnc ( 'attrename', -5, varid, 'babaganoush', 'dont_get_eliminated' );
if ( status >= 0 )
	err_msg = sprintf ( '%s:  %s:  ATTRENAME succeeded with a bad ncid\n', mfilename, testid );
	error ( err_msg );
end


% Test 3:  try to rename an attribute using a bad varid.
testid = 'Test 3';
status = mexnc ( 'attrename', ncid, -5, 'babaganoush', 'dont_get_eliminated' );
if ( status >= 0 )
	err_msg = sprintf ( '%s:  %s:  ATTRENAME succeeded with a bad varid\n', mfilename, testid );
	error ( err_msg );
end



% Test 4:  try to rename an attribute using a bad source attribute name.
testid = 'Test 4';
status = mexnc ( 'attrename', ncid, varid, 'dont_get_eliminated', 'babaganoush' );
if ( status >= 0 )
	err_msg = sprintf ( '%s:  %s:  ATTRENAME succeeded with a bad varid\n', mfilename, testid );
	error ( err_msg );
end



% Test 5:  try to rename an attribute using a bad destination attribute name.
testid = 'Test 5';

status = mexnc ( 'put_att_double', ncid, varid, 'dont_get_eliminated', nc_double, 1, input_data );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

status = mexnc ( 'attrename', ncid, varid, 'babaganoush', 'dont_get_eliminated' );
if ( status >= 0 )
	err_msg = sprintf ( '%s:  %s:  ATTRENAME succeeded with a bad new name\n', mfilename, testid );
	error ( err_msg );
end
[status] = mexnc ( 'enddef', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end



status = mexnc ( 'close', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

fprintf ( 1, 'ATTRENAME succeeded.\n' );

return


















