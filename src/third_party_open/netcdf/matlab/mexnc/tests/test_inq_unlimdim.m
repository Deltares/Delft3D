function test_inq_unlimdim ( ncfile )
% TEST_INQ_UNLIMDIM:  
%
% Test 1:  Normal retrieval.
% Test 2:  Invalid ncid 
% Test 3:  ncid = []
% Test 4:  non numeric ncid


error_condition = 0;


%
% Create the netcdf file
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
[tdimid, status] = mexnc ( 'def_dim', ncid, 't', 0 );
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

[status] = mexnc ( 'enddef', ncid );

status = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end



% Test 1
testid = 'Test 1';
[unlimdimid, status] = mexnc ( 'inq_unlimdim', ncid );
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, ncerr, mexnc ( 'strerror', status ) );
	error ( err_msg );
end
if unlimdimid ~= tdimid
	error ( 'inq_unlimid failed the basic test.\n' );
end


% Test 2:  Invalid ncid 
testid = 'Test 2';
[unlimdimid, status] = mexnc ( 'inq_unlimdim', -20000 );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, ncerr, mexnc ( 'strerror', status ) );
	error ( err_msg );
end


% Test 3:  ncid = []
testid = 'Test 3';
try
	[value,status] = mexnc ( 'INQ_UNLIMDIM', [] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end



% Test 4:  non numeric ncid
testid = 'Test 4';
try
	[value,status] = mexnc ( 'INQ_UNLIMDIM', 'ncid' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




mexnc ( 'close', ncid );

fprintf ( 'INQ_UNLIMDIM succeeded.\n' );
return
