function test_rename_att ( ncfile )
% TEST_RENAME_ATT
%
% Test 1:  Normal rename
% Test 2:  Bad ncid.
% Test 3:  Empty set ncid.
% Test 4:  Non numeric ncid
% Test 5:  Bad varid.
% Test 6:  Empty set varid.
% Test 7:  Non numeric varid
% Test 8:  non existant attribute
% Test 9:  old attribute name is []
% Test 10:  old attribute name is ''
% Test 11:  old attribute name is non char
% Test 12:  new attribute name already taken
% Test 13:  new attribute name is []
% Test 14:  new attribute name is ''
% Test 15:  new attribute name is non char

error_condition = 0;

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

input_data = 3.14159;
status = mexnc ( 'put_att_double', ncid, varid, 'test_double', nc_double, 1, input_data );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

for j = 2:25
	attname = sprintf ( 'test_%d', j );
	status = mexnc ( 'put_att_double', ncid, varid, attname, nc_double, 1, input_data );
	if ( status ~= 0 )
		ncerr = mexnc ( 'strerror', status );
		err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
		error ( err_msg );
	end
end

%status = mexnc ( 'sync', ncid );
%if ( status < 0 )
%	ncerr = mexnc ( 'strerror', status );
%	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
%	error ( err_msg );
%end

testid = 'Test 1';
status = mexnc ( 'rename_att', ncid, varid, 'test_double', 'babaganoush' );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end





% Test 2:  Bad ncid.
testid = 'Test 2';
status = mexnc ( 'rename_att', -20000, varid, 'test_2', 'babaganoush2' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 3:  Empty set ncid.
testid = 'Test 3';
try
	status = mexnc ( 'rename_att', [], varid, 'test_3', 'babaganoush3' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 4:  Non numeric ncid
testid = 'Test 4';
try
	status = mexnc ( 'rename_att', 'ncid', varid, 'test_4', 'babaganoush4' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





% Test 5:  Bad varid.
testid = 'Test 2';
status = mexnc ( 'rename_att', ncid, -20000, 'test_5', 'babaganoush5' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 6:  Empty set varid.
testid = 'Test 6';
try
	status = mexnc ( 'rename_att', ncid, [], 'test_6', 'babaganoush6' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 7:  Non numeric varid
testid = 'Test 7';
try
	status = mexnc ( 'rename_att', 'ncid', 'varid', 'test_7', 'babaganoush7' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 8:  non existant attribute
testid = 'Test 8';
status = mexnc ( 'rename_att', ncid, varid, 'i_dont_exist', 'babaganoush8' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 9:  old attribute name is []

testid = 'Test 9';
try
	status = mexnc ( 'rename_att', ncid, varid, [], 'babaganoush9' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 10:  old attribute name is ''
testid = 'Test 10';
try
	status = mexnc ( 'rename_att', ncid, varid, '', 'babaganoush9' );
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 11:  old attribute name is non char
testid = 'Test 11';
try
	status = mexnc ( 'rename_att', ncid, varid, 5, 'babaganoush11' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 12:  new attribute name already taken
testid = 'Test 12';
status = mexnc ( 'rename_att', ncid, varid, 'test_12', 'test_11' );
if status == 0
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 13:  new attribute name is []
testid = 'Test 13';
try
	status = mexnc ( 'rename_att', ncid, varid, 'test_13', [] );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 14:  new attribute name is ''
testid = 'Test 14';
try
	status = mexnc ( 'rename_att', ncid, varid, 'test_14', '' );
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 15:  new attribute name is non char
testid = 'Test 15';
try
	status = mexnc ( 'rename_att', ncid, varid, 'test_15', 5 );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end








[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

status = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

fprintf ( 1, 'RENAME_ATT succeeded.\n' );


status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


return

















