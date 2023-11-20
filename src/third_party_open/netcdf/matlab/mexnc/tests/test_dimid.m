function test_dimid ( ncfile )
% TEST_DIMID
%
% Test 1:  Retrieve a dimid.
% Test 2:  Bad ncid.
% Test 3:  Empty set ncid.
% Test 4:  Empty string dim name.
% Test 5:  Empty set dim name.
% Test 6:  Bad dim name.

if nargin < 1
    ncfile = 'foo.nc';
end


create_ncfile(ncfile);

test_normal_dimid(ncfile);
test_bad_ncid(ncfile);
test_empty_set_ncid(ncfile);
test_empty_string_dimname(ncfile);
test_empty_set_dimname(ncfile);
test_bad_dimname(ncfile);

fprintf('DIMID succeeded.\n');
%--------------------------------------------------------------------------
function create_ncfile(ncfile)
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error(mexnc('strerror',status)), end

[status] = mexnc ( 'enddef', ncid );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_normal_dimid(ncfile)
% Retrieve a dimid.

[ncid, status] = mexnc('open', ncfile, nc_nowrite_mode );
if status, error(mexnc('strerror',status)), end

[dimid, status] = mexnc('DIMID', ncid, 'x');
if status, error(mexnc('strerror',status)), end
if ( dimid ~= 0 )
	error('failed');
end

if dimid ~= 0
	error('failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end

%--------------------------------------------------------------------------
function test_bad_ncid(ncfile)

[ncid, status] = mexnc('open', ncfile, nc_nowrite_mode );
if status, error(mexnc('strerror',status)), end

[test_dimid, status] = mexnc ( 'dimid', -2, 'x' );
if ( status >= 0 )
	error('Succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_empty_set_ncid(ncfile)

[ncid, status] = mexnc('open', ncfile, nc_nowrite_mode );
if status, error(mexnc('strerror',status)), end

try
	[test_dimid, status] = mexnc ( 'dimid', [], 'x' );
	error('failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_empty_string_dimname(ncfile)

[ncid, status] = mexnc('open', ncfile, nc_nowrite_mode );
if status, error(mexnc('strerror',status)), end

testid = 'Test 4';
try
	[test_dimid, status] = mexnc ( 'dimid', ncid, '' );
	error('failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_empty_set_dimname(ncfile)

[ncid, status] = mexnc('open', ncfile, nc_nowrite_mode );
if status, error(mexnc('strerror',status)), end

try
	[test_dimid, status] = mexnc ( 'dimid', ncid, [] );
	error('failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end

%--------------------------------------------------------------------------
function test_bad_dimname(ncfile)

[ncid, status] = mexnc('open', ncfile, nc_nowrite_mode );
if status, error(mexnc('strerror',status)), end

[test_dimid, status] = mexnc ( 'dimid', ncid, 'y' );
if ( status >= 0 )
	error('failed'); 
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end

