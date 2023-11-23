function test__open ( ncfile )
% Tests run are
%
% Test 1:  test write mode
% Test 2:  test share mode
% Test 3:  bitwise or of write mode and share mode
% Test 4:  only two input arguments given
% Test 5:  filename argument is bad
% Test 6:  filename argument is non character
% Test 7:  mode argument is non character and non double

if nargin < 1
    ncfile = 'foo.nc';
end

create_file(ncfile);
test_write_mode(ncfile);
test_share_mode(ncfile);
test_write_share_mode(ncfile);
test_too_few_inputs(ncfile);
test_bad_filename(ncfile);
test_bad_filename2(ncfile);
test_bad_mode(ncfile);
fprintf('_OPEN succeeded.\n' );

%--------------------------------------------------------------------------
function create_file(ncfile)
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_write_mode(ncfile)
% Test 1:   write mode

[chunksizehint, ncid, status] = mexnc ( '_open', ncfile, nc_write_mode, 1024 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_share_mode(ncfile)
% Test 2:  share mode

[chunksizehint, ncid, status] = mexnc ( '_open', ncfile, nc_share_mode, 1024 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_write_share_mode(ncfile)
% Test 3:  bitwise or of write mode and share mode
[chunksizehint, ncid, status] = mexnc ( '_open', ncfile, bitor ( nc_write_mode, nc_share_mode ) , 1024);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_too_few_inputs(ncfile)
% Test 4:  only two input arguments given
try
	[csh, ncid, status] = mexnc ( '_open', ncfile, 1024 );
	mexnc ( 'close', ncid );
	error('succeed when it should have failed');
catch
	;
end



%--------------------------------------------------------------------------
function test_bad_filename(ncfile)
% Test 5:  filename argument is bad
[csh, ncid, status] = mexnc ( '_open', 'i_do_not_exists', nc_noclobber_mode, 1024 );
if ( status == 0 )
	error( 'succeeded when it should have failed.');
end




%--------------------------------------------------------------------------
function test_bad_filename2(ncfile)
% Test 6:  filename argument is non character
try
	[csh, ncid, status] = mexnc ( '_open', 20000, nc_noclobber_mode, 1024 );
	error( 'Succeeded when it should have failed');
catch
	;
end



%--------------------------------------------------------------------------
function test_bad_mode(ncfile)
% Test 7:  mode argument is non character and non double
try
	[csh, ncid, status] = mexnc ( '_open', ncfile, single(5), 1024 );
	error('Succeeded when it should have failed');
catch
	;
end



