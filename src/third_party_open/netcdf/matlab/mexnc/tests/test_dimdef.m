function test_dimdef ( ncfile )
%
% Test:  Define a dimension.
% Test:  Bad ncid.
% Test:  Empty string name.
% Test:  Empty set name.
% Test:  Negative dimension length
% Test:  Empty set length.

if nargin == 0
	ncfile = 'foo.nc';
end

mexnc ( 'setopts', 0 );

test_define_dimension(ncfile);
test_define_unlimited_dimension(ncfile);
test_define_unlimited_dimension_with_char(ncfile);

test_bad_ncid(ncfile);
test_zero_length_dimension_name(ncfile);
test_empty_dimension_name(ncfile);
test_negative_dimension_length(ncfile);
test_empty_set_dimension_length(ncfile);

fprintf ( 1, 'DIMDEF succeeded.\n' );



%--------------------------------------------------------------------------
function test_define_dimension(ncfile);

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end;

[xdimid, status] = mexnc ( 'dimdef', ncid, 'x', 20 );
if status, error(mexnc('strerror',status)), end;

[status] = mexnc ( 'enddef', ncid );
if status, error(mexnc('strerror',status)), end;

[dimid, status] = mexnc ( 'inq_dimid', ncid, 'x' );
if status, error(mexnc('strerror',status)), end;

if dimid ~= xdimid
	error ( 'INQ_DIMID did not validate DIMDEF' );
end


status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

%--------------------------------------------------------------------------
function test_define_unlimited_dimension(ncfile);

ncid = mexnc('open',ncfile,'write');
status = mexnc('redef',ncid);
if status, error ( mexnc('strerror',status) ), end

try
	[xdimid, status] = mexnc ( 'dimdef', ncid, 'y', 0 );
	error ( 'succeeded when it should have failed.'  );
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end



%--------------------------------------------------------------------------
function test_define_unlimited_dimension_with_char(ncfile);

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end;

[xdimid, status] = mexnc ( 'dimdef', ncid, 'x', 'NC_UNLIMITED' );
if status, error(mexnc('strerror',status)), end;

[status] = mexnc ( 'enddef', ncid );
if status, error(mexnc('strerror',status)), end;

[dimid, status] = mexnc ( 'inq_dimid', ncid, 'x' );
if status, error(mexnc('strerror',status)), end;

if dimid ~= xdimid
	error ( 'INQ_DIMID did not validate DIMDEF' );
end


status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

%--------------------------------------------------------------------------
function test_bad_ncid(ncfile);

[xdimid, status] = mexnc ( 'dimdef', -3, 'x', 20 );
if ( status == 0 )
	error ( 'succeeded when it should have failed.' );
end




%--------------------------------------------------------------------------
function test_zero_length_dimension_name(ncfile);

ncid = mexnc('open',ncfile,'write');
status = mexnc('redef',ncid);
if status, error ( mexnc('strerror',status) ), end

try
	[xdimid, status] = mexnc ( 'dimdef', ncid, '', 20 );
	error ( 'succeeded when it should have failed.'  );
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end


%--------------------------------------------------------------------------
function test_empty_dimension_name(ncfile);

ncid = mexnc('open',ncfile,'write');
status = mexnc('redef',ncid);
if status, error ( mexnc('strerror',status) ), end

try
	[xdimid, status] = mexnc ( 'dimdef', ncid, [], 20 );
	error ( 'failed to throw an exception.' );
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end


%--------------------------------------------------------------------------
function test_negative_dimension_length(ncfile);

ncid = mexnc('open',ncfile,'write');
status = mexnc('redef',ncid);
if status, error ( mexnc('strerror',status) ), end

[xdimid, status] = mexnc ( 'dimdef', ncid, 'x2', -5 );
if ( status == 0 )
	error ( 'succeeded when it should have failed.' );
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

%--------------------------------------------------------------------------
function test_empty_set_dimension_length(ncfile);

ncid = mexnc('open',ncfile,'write');
status = mexnc('redef',ncid);
if status, error ( mexnc('strerror',status) ), end


% Test 6:  Empty set length.
try
	[xdimid, status] = mexnc ( 'dimdef', ncid, 'x3', [] );
	error ( 'succeeded when it should have failed.' );
end


status = mexnc ( 'enddef', ncid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end


return

