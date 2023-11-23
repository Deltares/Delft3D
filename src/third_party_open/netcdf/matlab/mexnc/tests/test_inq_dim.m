function test_inq_dim ( ncfile )
% TEST_INQ_DIM
%
% Tests number of dimensions, variables, global attributes, record dimension for
% foo.nc.  Also tests helper routines, "nc_inq_dimlen", "nc_inq_dimname".
%
% Test 1:  INQ_DIM:  Normal retrieval
% Test 2:  INQ_DIM:  Bad ncid.
% Test 3:  INQ_DIM:  Empty set ncid.
% Test 4:  INQ_DIM:  Non numeric ncid
% Test 5:  INQ_DIM:  Bad dimid.
% Test 6:  INQ_DIM:  Empty set dimid.
% Test 7:  INQ_DIM:  Non numeric dimid
% Test 11:  INQ_DIMLEN:  Normal retrieval
% Test 12:  INQ_DIMLEN:  Bad ncid.
% Test 13:  INQ_DIMLEN:  Empty set ncid.
% Test 14:  INQ_DIMLEN:  Non numeric ncid
% Test 15:  INQ_DIMLEN:  Bad dimid.
% Test 16:  INQ_DIMLEN:  Empty set dimid.
% Test 17:  INQ_DIMLEN:  Non numeric dimid
% Test 21:  INQ_DIMNAME:  Normal retrieval
% Test 22:  INQ_DIMNAME:  Bad ncid.
% Test 23:  INQ_DIMNAME:  Empty set ncid.
% Test 24:  INQ_DIMNAME:  Non numeric ncid
% Test 25:  INQ_DIMNAME:  Bad dimid.
% Test 26:  INQ_DIMNAME:  Empty set dimid.
% Test 27:  INQ_DIMNAME:  Non numeric dimid

if nargin == 0
	ncfile = 'foo.nc';
end

error_condition = 0;

%
% Create a netcdf file with
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	error ( 'CREATE failed' );
end


%
% DIMDEF
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 )
	error ( 'DEF_DIM failed on X' );
end
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 24 );
if ( status ~= 0 )
	error ( 'DEF_DIM failed on y' );
end
[zdimid, status] = mexnc ( 'def_dim', ncid, 'z', 32 );
if ( status ~= 0 )
	error ( 'DEF_DIM failed on z' );
end


%
% ENDEF
[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	error ( 'ENDEF failed with write' );
end

[status] = mexnc ( 'sync', ncid );
if ( status ~= 0 )
	msg = sprintf ( 'SYNC failed with write, %s', mexnc ( 'strerror', status ) );
	error ( msg );
end

%
% dimension 0 should have name 'x', length 20
% Test 1:  Normal retrieval
testid = 'Test 1';
[name, length, status] = mexnc('INQ_DIM', ncid, xdimid);
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s, ''%s''\n', mfilename, test_id, ncerr );
	error ( err_msg );
end

if ~strcmp ( name, 'x' )
	msg = sprintf ( 'INQ_DIM returned ''%s'' as a name, but it should have been ''x''', name );
	error ( msg );
end
if ( length ~= 20 )
	msg = sprintf ( 'INQ_DIM returned %d as x''s length, but it should have been 20', length );
	error ( msg );
end




%--------------------------------------------------------------------------
% Bad ncid.
[name, length, status] = mexnc('INQ_DIM', -20000, xdimid);
if ( status == 0 )
	error('Succeeded when it should have failed');
end



%--------------------------------------------------------------------------
% Empty set ncid.
try
	[name, length, status] = mexnc('INQ_DIM', [], xdimid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed.');
end





%--------------------------------------------------------------------------
% Non numeric ncid
try
	[name, length, status] = mexnc('INQ_DIM', 'ncid', xdimid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





% Test 5:  Bad dimid.
testid = 'Test 5';
[name, length, status] = mexnc('INQ_DIM', ncid, -20000);
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 6:  Empty set dimid.
testid = 'Test 6';
try
	[name, length, status] = mexnc('INQ_DIM', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 7:  Non numeric dimid
testid = 'Test 7';
try
	[name, length, status] = mexnc('INQ_DIM', ncid, 'xdimid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






fprintf ( 1, 'INQ_DIM succeeded\n' );




%
% dimension 0 should have name 'x', length 20
% Test 11:  Normal retrieval
testid = 'Test 11';
[dimlen, status] = mexnc('INQ_DIMLEN', ncid, xdimid);
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s, ''%s''\n', mfilename, test_id, ncerr );
	error ( err_msg );
end

if ( dimlen ~= 20 )
	msg = sprintf ( 'INQ_DIMLEN returned %d as x''s length, but it should have been 20', dimlen );
	error ( msg );
end





% Test 12:  Bad ncid.
testid = 'Test 12';
[dimlen, status] = mexnc('INQ_DIMLEN', -20000, xdimid);
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 13:  Empty set ncid.
testid = 'Test 13';
try
	[dimlen, status] = mexnc('INQ_DIMLEN', [], xdimid);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 14:  Non numeric ncid
testid = 'Test 14';
try
	[dimlen, status] = mexnc('INQ_DIMLEN', 'ncid', xdimid);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





% Test 15:  Bad dimid.
testid = 'Test 15';
[dimlen, status] = mexnc('INQ_DIMLEN', ncid, -20000);
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 16:  Empty set dimid.
testid = 'Test 16';
try
	[dimlen, status] = mexnc('INQ_DIMLEN', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 17:  Non numeric dimid
testid = 'Test 17';
try
	[dimlen, status] = mexnc('INQ_DIMLEN', ncid, 'xdimid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






fprintf ( 1, 'INQ_DIMLEN succeeded\n' );



%
% dimension 0 should have name 'x', length 20
% Test 21:  Normal retrieval
testid = 'Test 21';
[name, status] = mexnc('INQ_DIMNAME', ncid, xdimid);
if ( status ~= 0 )
	err_msg = sprintf ( '%s:  %s, ''%s''\n', mfilename, test_id, ncerr );
	error ( err_msg );
end

if ~strcmp ( name, 'x' )
	msg = sprintf ( 'INQ_DIMNAME returned ''%s'' as a name, but it should have been ''x''', name );
	error ( msg );
end





%--------------------------------------------------------------------------
% Test 22:  Bad ncid.
[name, status] = mexnc('INQ_DIMNAME', -20000, xdimid);
if ( status == 0 )
	error('Succeeded when it should have failed');
end




%--------------------------------------------------------------------------
% Test 23:  Empty set ncid.
try
	[name, status] = mexnc('INQ_DIMNAME', [], xdimid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end






%--------------------------------------------------------------------------
% Test 24:  Non numeric ncid
try
	[name, status] = mexnc('INQ_DIMNAME', 'ncid', xdimid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





% Test 25:  Bad dimid.
testid = 'Test 25';
[name, status] = mexnc('INQ_DIMNAME', ncid, -20000);
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 26:  Empty set dimid.
testid = 'Test 26';
try
	[name, status] = mexnc('INQ_DIMNAME', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 27:  Non numeric dimid
testid = 'Test 27';
try
	[name, status] = mexnc('INQ_DIMNAME', ncid, 'xdimid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end







fprintf ( 1, 'INQ_DIMNAME succeeded\n' );







status = mexnc ( 'close', ncid );
if ( status < 0 )
	error ( 'CLOSE failed on nowrite' );
end



return











