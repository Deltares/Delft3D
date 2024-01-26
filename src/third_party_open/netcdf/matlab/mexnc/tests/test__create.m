function test__create ( ncfile )
% TEST__CREATE:
%
% Tests run are open with
% Test 1:   nc_clobber_mode.  Check the initial file size.
% Test 2:   nc_noclobber_mode
% Test 3:   clobber and share and 64 bit offset
% Test 4:  share mode.  Should also clobber it.
% Test 5:  share | 64bit_offset
% Test 6:  64 bit offset.  Should also clobber it.
% Test 7:  noclobber mode.  Should not succeed.
% Test 8:  only one input, should not succeed
% Test 9:  Filename is empty
% Test 10:  mode argument not supplied
%
% Basically the tests are the same as those for CREATE except we are
% using _CREATE instead.
%
% The _CREATE routine really isn't necesary anymore in NetCDF-4.  This
% is for backwards compatibility only.

if nargin == 0
	ncfile = 'foo.nc';
end

error_condition = 0;

test_no_clobber_mode(ncfile);
test_no_clobber_shared_mode(ncfile);
test_no_clobber_shared_64bit_offset_mode(ncfile);
test_shared_mode(ncfile);
test_shared_64bit_offset_mode(ncfile);
test_64bit_offset_mode(ncfile);
test_no_clobber_mode(ncfile);
test_only_one_input(ncfile);
test_filename_is_empty('');

fprintf ( '_CREATE succeeded.\n' );

%--------------------------------------------------------------------------
function test_no_clobber_mode(ncfile);
% Test 1:   nc_clobber_mode
[chunksize,ncid, status] = mexnc ( '_create', ncfile, nc_clobber_mode, 5000 );
if status, error(mexnc('strerror',status)), end

d = dir ( ncfile );
if d.bytes ~= 5000
	error('initialsize not honored.');
	error ( msg );
end
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end





%--------------------------------------------------------------------------
function test_no_clobber_shared_mode(ncfile)
% Test 2:   nc_noclobber_mode | nc_share_mode
mode = bitor ( nc_clobber_mode, nc_share_mode );
[chunksize, ncid, status] = mexnc ( '_create', ncfile, mode, 5000 );
if status, error(mexnc('strerror',status)), end
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_no_clobber_shared_64bit_offset_mode(ncfile)
% Test 3:   clobber and share and 64 bit offset
mode = bitor ( nc_clobber_mode, nc_share_mode );
mode = bitor ( mode, nc_64bit_offset_mode );
[chunksize, ncid, status] = mexnc ( '_create', ncfile, mode, 5000 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_shared_mode(ncfile)
% Test 4:  share mode.  Should also clobber it.
[chunksize, ncid, status] = mexnc ( '_create', ncfile, nc_share_mode, 5000 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_shared_64bit_offset_mode(ncfile);
% Test 5:  share | 64bit_offset
testid = 'Test 5';
mode = bitor ( nc_share_mode, nc_64bit_offset_mode );
[chunksize, ncid, status] = mexnc ( '_create', ncfile, mode, 5000 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_64bit_offset_mode(ncfile);
% Test 6:  64 bit offset.  Should also clobber it.
[chunksize, ncid, status] = mexnc ( '_create', ncfile, nc_64bit_offset_mode, 5000 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_noclobber_mode(ncfile);
% Test 7:  noclobber mode.  Should not succeed.
[chunksize, ncid, status] = mexnc ( '_create', ncfile, nc_noclobber_mode, 5000 );
if ( status == 0 )
	error ( '''_create'' succeeded on nc_noclobber_mode, should have failed' );
end


%--------------------------------------------------------------------------
function test_only_one_input(ncfile);
% Test 8:  only one input, should not succeed.  Throws an exception, 
%          because there are way too few arguments.
try
	[chunksize, ncid, status] = mexnc ( '_create' );
	error ( 'succeeded when it should have failed' );
catch	
	;
end





%--------------------------------------------------------------------------
function test_filename_is_empty(ncfile);
try
	[chunksize, ncid, status] = mexnc ( '_create', '', nc_clobber_mode, 5000 );
	error('succeeded when it should have failed' );
end




