function test_rename_var ( ncfile )
% TEST_RENAME_VAR
%
% Just renames a variable.
%
% Test 1:  Normal rename
% Test 2:  Bad ncid.
% Test 3:  Empty set ncid.
% Test 4:  Non numeric ncid
% Test 5:  Bad varid.
% Test 6:  Empty set varid.
% Test 7:  Non numeric varid
% Test 12:  new variable name already taken
% Test 13:  new variable name is []
% Test 14:  new variable name is ''
% Test 15:  new variable name is non char

if nargin < 1
    ncfile = 'foo.nc';
end

error_condition = 0;

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	error ( 'CREATE failed' );
end


%
% DIMDEF
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 20 );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', 'double', 1, xdimid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


[ydvarid, status] = mexnc ( 'def_var', ncid, 'y', 'double', 1, ydimid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


status = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
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


testid = 'Test 1';
status = mexnc ( 'RENAME_VAR', ncid, xdvarid, 'x2' );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end




%--------------------------------------------------------------------------
% Test 2:  Bad ncid.
status = mexnc ( 'RENAME_VAR', -20000, xdvarid, 'x2' );
if ( status == 0 )
	error('Succeeded when it should have failed');
end




%--------------------------------------------------------------------------
% Test 3:  Empty set ncid.
try %#ok<TRYNC>
	mexnc ( 'RENAME_VAR', [], xdvarid, 'x2' );
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





%--------------------------------------------------------------------------
% Test 4:  Non numeric ncid
try %#ok<TRYNC>
	mexnc ( 'RENAME_VAR', 'ncid', xdvarid, 'x2' );
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





% Test 5:  Bad dimid.
testid = 'Test 2';
status = mexnc ( 'RENAME_VAR', ncid, -20000, 'x2' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 6:  Empty set dimid.
testid = 'Test 6';
try
	status = mexnc ( 'RENAME_VAR', ncid, [], 'x6' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 7:  Non numeric dimid
testid = 'Test 7';
try
	status = mexnc ( 'RENAME_VAR', ncid, 'xdvarid', 'x2' );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 12:  new dimension name already taken
testid = 'Test 12';
status = mexnc ( 'RENAME_VAR', ncid, xdvarid, 'y' );
if status == 0
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 13:  new dimension name is []
testid = 'Test 13';
try
	status = mexnc ( 'RENAME_VAR', ncid, xdvarid, [] );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 14:  new attribute name is ''
testid = 'Test 14';
try
	status = mexnc ( 'RENAME_VAR', ncid, xdvarid, '' );
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 15:  new attribute name is non char
testid = 'Test 15';
try
	status = mexnc ( 'RENAME_VAR', ncid, xdvarid, 5 );
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end


fprintf ( 1, 'RENAME_VAR succeeded\n' );


%
% ENDEF
[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end



return












