function test_inq_attid ( ncfile )
% TEST_INQ_ATTID:  Also tests INQ_ATTNAME
%
% Test 1:  Use INQ_ATTID to get the attid from a name, then use 
%          INQ_ATTNAME to get the name from the id we just got.
% Test 2:  INQ_ATTID:  Invalid ncid 
% Test 3:  INQ_ATTID:  Invalid varid 
% Test 4:  INQ_ATTID:  Invalid name. 
% Test 5:  INQ_ATTID:  ncid = []
% Test 6:  INQ_ATTID:  varid = [] 
% Test 7:  INQ_ATTID:  name = []
% Test 8:  INQ_ATTID:  non numeric ncid
% Test 9:  INQ_ATTID:  non numeric varid
% Test 10:  INQ_ATTID:  non character att name
% Test 11:  INQ_ATTNAME:  Invalid ncid 
% Test 12:  INQ_ATTNAME:  Invalid varid 
% Test 13:  INQ_ATTNAME:  Invalid name. 
% Test 14:  INQ_ATTNAME:  ncid = []
% Test 15:  INQ_ATTNAME:  varid = [] 
% Test 16:  INQ_ATTNAME:  name = []
% Test 17:  INQ_ATTNAME:  non numeric ncid
% Test 18:  INQ_ATTNAME:  non numeric varid
% Test 19:  INQ_ATTNAME:  non character att name


if nargin < 1
    ncfile = 'foo.nc';
end

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
[status] = mexnc ( 'enddef', ncid );

status = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


% Test 1
testid = 'Test 1';
[attnum, status] = mexnc ( 'inq_attid', ncid, varid, 'test_double' );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end




[attname, status] = mexnc ( 'inq_attname', ncid, varid, attnum );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

if ( ~strcmp ( attname, 'test_double' ) )
	err_msg = sprintf ( '%s:  %s: attribute name did not match what we put in there\n', mfilename, testid );
	error ( err_msg );
end


% Test 2:  INQ_ATTID:  Invalid ncid 
testid = 'Test 2';
[value,status] = mexnc ( 'INQ_ATTID', -2000, varid, 'test_double' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end

	
% Test 3:  INQ_ATTID:  Invalid varid 
testid = 'Test 3';
[value,status] = mexnc ( 'INQ_ATTID', ncid,  -2000, 'test_double' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end

	
% Test 4:  INQ_ATTID:  Invalid name. 
testid = 'Test 4';
[value,status] = mexnc ( 'INQ_ATTID', ncid,  varid, 'test_double2' );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end

%--------------------------------------------------------------------------	
% Test 5:  INQ_ATTID:  ncid = []
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTID', [], varid, 'test_double' );
	error('succeeded when it should have failed');
end

%--------------------------------------------------------------------------	
% Test 6:  INQ_ATTID:  varid = [] 
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTID', ncid, [], 'test_double' );
	error('succeeded when it should have failed');
end

%--------------------------------------------------------------------------	
% Test 7:  INQ_ATTID:  name = []
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTID', ncid, varid, [] );
	error_condition = 1;
end
if error_condition
	error('succeeded when it should have failed');
end

%--------------------------------------------------------------------------	
% Test 8:  INQ_ATTID:  non numeric ncid
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTID', 'ncid', varid, 'test_double' );
	error_condition = 1;
end
if error_condition
	error('succeeded when it should have failed');
end

%--------------------------------------------------------------------------	
% Test 9:  INQ_ATTID:  non numeric varid
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTID', ncid, 'varid', 'test_double' );
	error_condition = 1;
end
if error_condition
	error('succeeded when it should have failed');
end


% Test 10:  INQ_ATTID:  non character att name
testid = 'Test 10';
try
	[value,status] = mexnc ( 'INQ_ATTID', ncid, varid, 0 );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 11:  INQ_ATTNAME:  Invalid ncid 
testid = 'Test 11';
[value,status] = mexnc ( 'INQ_ATTNAME', -2000, varid, attnum );
if ( status == 0 )
	error('succeeded when it should have failed');
end

%--------------------------------------------------------------------------
% Test 12:  INQ_ATTNAME:  Invalid varid 
[value,status] = mexnc ( 'INQ_ATTNAME', ncid, -2000, attnum ); %#ok<ASGLU>
if ( status == 0 )
	error('succeeded when it should have failed\n');
end

%--------------------------------------------------------------------------	
% Test 13:  INQ_ATTNAME:  Invalid attnum. 
[value,status] = mexnc ( 'INQ_ATTNAME', ncid, varid, -2000 ); %#ok<ASGLU>
if ( status == 0 )
	error('succeeded when it should have failed\n');
end

%--------------------------------------------------------------------------	
% Test 14:  INQ_ATTNAME:  ncid = []
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTNAME', [], varid, attnum );
	error_condition = 1;
end
if error_condition
	error('succeeded when it should have failed\n');
end

%--------------------------------------------------------------------------
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTNAME', ncid, [], attnum );
	error_condition = 1;
end
if error_condition
	error('succeeded when it should have failed\n');
end


%--------------------------------------------------------------------------
% Test 16:  INQ_ATTNAME:  name = []
testid = 'Test 16';
try %#ok<TRYNC>
	mexnc ( 'INQ_ATTNAME', ncid, varid, [] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 17:  INQ_ATTNAME:  non numeric ncid
testid = 'Test 17';
try
	[value,status] = mexnc ( 'INQ_ATTNAME', 'ncid', varid, attnum );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 18:  INQ_ATTNAME:  non numeric varid
testid = 'Test 18';
try
	[value,status] = mexnc ( 'INQ_ATTNAME', ncid, 'varid', attnum );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 19:  INQ_ATTNAME:  non numeric attid
testid = 'Test 19';
try
	[value,status] = mexnc ( 'INQ_ATTNAME', ncid, varid, 'attnum' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


fprintf ( 1, 'INQ_ATTID succeeded.\n' );
fprintf ( 1, 'INQ_ATTNAME succeeded.\n' );


return
















