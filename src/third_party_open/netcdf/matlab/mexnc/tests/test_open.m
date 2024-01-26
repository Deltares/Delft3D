function test_open ( ncfile )
% Tests run are
%
% Test 1:  test write mode
% Test 2:  test share mode
% Test 3:  bitwise or of write mode and share mode
% Test 4:  only two input arguments given
% Test 5:  filename argument is bad
% Test 6:  filename argument is non character
% Test 7:  mode argument is non character and non double
% Test 8:  mode argument is character, but unknown
% Test 100:  mode is 'write' instead of nc_write_mode

error_condition = 0;

if nargin < 1
    ncfile = 'foo.nc';
end

create_ncfile ( ncfile );

%
% Test 1:   write mode
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 )
	error ( 'OPEN failed with nc_write_mode' );
end
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed with nc_write_mode' );
end

%
% Test 2:  share mode
[ncid, status] = mexnc ( 'open', ncfile, nc_share_mode );
if ( status ~= 0 )
	error ( 'OPEN failed with nc_share_mode' );
end
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed with nc_write_mode' );
end



%
% Test 3:  bitwise or of write mode and share mode
[ncid, status] = mexnc ( 'open', ncfile, bitor ( nc_write_mode, nc_share_mode ) );
if ( status ~= 0 )
	error ( 'OPEN failed with nc_write_mode | nc_share_mode' );
end
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed with nc_write_mode' );
end


%
% Test 4:  only two input arguments given
testid = 'Test 5';
[ncid, status] = mexnc ( 'open', ncfile );
if ( status ~= 0 )
	msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, mexnc ( 'strerror', status ) );
	error ( msg );
end
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( [testid] );
end



% Test 5:  filename argument is bad
testid = 'Test 5';
[ncid, status] = mexnc ( 'open', 'i_do_not_exists', nc_noclobber_mode );
if ( status == 0 )
	msg = sprintf ( '%s:  %s:  succeeded when it should have failed.\n', mfilename, testid );
	error ( msg );
end


% Test 6:  filename argument is non character
testid = 'Test 6';
try
	[ncid, status] = mexnc ( 'open', 20000, nc_noclobber_mode );
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end



% Test 7:  mode argument is non character and non double
testid = 'Test 7';
try
	[ncid, status] = mexnc ( 'open', ncfile, single(5) );
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end


test_100 ( ncfile );

fprintf ( 1, 'OPEN succeeded.\n' );




return







function test_100 ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, 'write' );
if status, error ( mexnc('STRERROR',status) ), end

%
% Try to write to a variable
status = mexnc ( 'put_var1_double', ncid, 0, 0, pi );
if status, error ( mexnc('STRERROR',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('STRERROR',status) ), end

return



function create_ncfile ( ncfile )


	[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
	if status, error ( mexnc('STRERROR',status) ), end
	
	[dimid, status] = mexnc ( 'DEF_DIM', ncid, 'x', 10 );
	if status, error ( mexnc('STRERROR',status) ), end
	
	[varid, status] = mexnc ( 'DEF_VAR', ncid, 'x', nc_double, 1, dimid ); %#ok<ASGLU>
	if status, error ( mexnc('STRERROR',status) ), end
	
	status = mexnc ( 'close', ncid );
	if status, error ( mexnc('STRERROR',status) ), end
	

return
