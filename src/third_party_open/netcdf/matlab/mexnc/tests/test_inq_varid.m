function test_inq_varid ( ncfile )
% TEST_INQ_VARID
%
% Test 1:  Normal retrieval
% Test 2:  Bad ncid.
% Test 3:  Empty set ncid.
% Test 4:  Non numeric ncid
% Test 5:  Bad variable name.
% Test 5a:  variable name is ""
% Test 6:  Empty set variable name.
% Test 7:  Non character variable name
%

if nargin < 1
    ncfile = 'foo.nc';
end

error_condition = 0;

%
% Create a netcdf file with
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% DIMDEF
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 24 );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end
[zdimid, status] = mexnc ( 'def_dim', ncid, 'z', 32 );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% VARDEF
[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', 'double', 1, xdimid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end




% Test 1:  Normal retrieval
testid = 'Test 1';
[varid, status] = mexnc('INQ_VARID', ncid, 'x_double');
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end

if ( varid ~= xdvarid )
	err_msg = sprintf ( '%s:  INQ_VARID did not return the expected value\n', mfilename );
	error ( err_msg );
end



%--------------------------------------------------------------------------
% Test 2:  Bad ncid.
[varid, status] = mexnc('INQ_VARID', -20000, 'x_double'); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end



%--------------------------------------------------------------------------
% Test 3:  Empty set ncid.
try %#ok<TRYNC>
	mexnc('INQ_VARID', [], 'x_double');
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





%--------------------------------------------------------------------------
% Test 4:  Non numeric ncid
try %#ok<TRYNC>
	mexnc('INQ_VARID', 'ncid', 'x_double');
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed\n');
end









% Test 5:  Bad variable name.
testid = 'Test 5';
[varid, status] = mexnc('INQ_VARID', ncid, 'xx_double');
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 5a:  Bad variable name "".
testid = 'Test 5';
try
	[varid, status] = mexnc('INQ_VARID', ncid, '');
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 6:  Empty set variable name.
testid = 'Test 6';
try
	[varid, status] = mexnc('INQ_VARID', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 7:  non character variable name
testid = 'Test 7';
try
	[varid, status] = mexnc('INQ_VARID', ncid, 5);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




fprintf ( 'INQ_VARID succeeded.\n' );





% ENDEF
[status] = mexnc ( 'enddef', ncid );
if ( status < 0 )
	error ( 'ENDEF failed with write' );
end



status = mexnc ( 'close', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  %s\n', mfilename, ncerr );
	error ( msg );
end

return












