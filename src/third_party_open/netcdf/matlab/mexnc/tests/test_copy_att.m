function test_copy_att ( ncfile1, ncfile2 )

if nargin < 1
	ncfile1 = 'foo1.nc';
	ncfile2 = 'foo2.nc';
end

create_ncfiles(ncfile1,ncfile2);
test_copy(ncfile1,ncfile2);
test_bad_source_ncid(ncfile1,ncfile2);
test_bad_source_varid(ncfile1,ncfile2);
test_bad_destination_ncid(ncfile1,ncfile2);
test_bad_destination_varid(ncfile1,ncfile2);
test_bad_attribute_name(ncfile1,ncfile2);
test_non_numeric_source_ncid(ncfile1,ncfile2);
test_non_numeric_source_varid(ncfile1,ncfile2);
test_non_numeric_destination_ncid(ncfile1,ncfile2);
test_non_numeric_destination_varid(ncfile1,ncfile2);

fprintf('COPY_ATT succeeded.\n');


%-------------------------------------------------------------------------
function create_ncfiles(ncfile1,ncfile2)
[ncid1, status] = mexnc ( 'create', ncfile1, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end

[xdimid1, status] = mexnc ( 'def_dim', ncid1, 'x', 20 );
if status, error(mexnc('strerror',status)), end

[varid1, status] = mexnc ( 'def_var', ncid1, 'x', nc_double, 1, xdimid1 );
if status, error(mexnc('strerror',status)), end

input_data = [3.14159];
status = mexnc ( 'put_att_double', ncid1, varid1, 'test_double', nc_double, 1, input_data );
if status, error(mexnc('strerror',status)), end

[status] = mexnc ( 'enddef', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'sync', ncid1 );
if status, error(mexnc('strerror',status)), end

[ncid2, status] = mexnc ( 'create', ncfile2, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end

[xdimid2, status] = mexnc ( 'def_dim', ncid2, 'x', 20 );
if status, error(mexnc('strerror',status)), end

[varid2, status] = mexnc ( 'def_var', ncid2, 'x', nc_double, 1, xdimid2 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end

%-------------------------------------------------------------------------
function test_copy(ncfile1,ncfile2)
% Test 1:  copy an attribute.

[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'copy_att', ncid1, varid1, 'test_double', ncid2, varid2 );
if status, error(mexnc('strerror',status)), end

[status] = mexnc ( 'enddef', ncid2 );
if status, error(mexnc('strerror',status)), end

[return_value, status] = mexnc ( 'get_att_double', ncid2, varid2, 'test_double' );
if status, error(mexnc('strerror',status)), end

if return_value ~= 3.14159
	err_msg = sprintf ( 'COPY_ATT did not seem to work\n', mfilename  );
	error ( err_msg );
end


[status] = mexnc ( 'redef', ncid2 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end

%-------------------------------------------------------------------------
function test_bad_source_ncid(ncfile1,ncfile2)
% Test 2:  try with a bad source ncid
[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'copy_att', -2, varid1, 'test_double', ncid2, varid2 );
if ( status >= 0 )
	error('COPY_ATT succeeded when it should have failed.' );
end


status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end


%-------------------------------------------------------------------------
function test_bad_source_varid(ncfile1,ncfile2)
% Test 3:  try with a bad source varid
[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'copy_att', ncid1, -2, 'test_double', ncid2, varid2 );
if ( status >= 0 )
	error('COPY_ATT succeeded with a bad varid1 when it should have failed.');
end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end



%-------------------------------------------------------------------------
function test_bad_destination_ncid(ncfile1,ncfile2)
% Test 4:  try with a bad destination ncid

[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'copy_att', ncid1, varid1, 'test_double', -2, varid2 );
if ( status >= 0 )
	error('COPY_ATT succeeded with a bad ncid2 when it should have failed.');
end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end



%-------------------------------------------------------------------------
function test_bad_destination_varid(ncfile1,ncfile2)
% Test 5:  try with a bad destination varid
[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'copy_att', ncid1, varid1, 'test_double', ncid2, -2 );
if ( status >= 0 )
	error('COPY_ATT succeeded with a bad varid2 when it should have failed.');
end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end



%-------------------------------------------------------------------------
function test_bad_attribute_name(ncfile1,ncfile2)
% Test 6:  try with a bad attribute name

[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'copy_att', ncid1, varid1, 'i_dont_exist', ncid2, varid2 );
if ( status >= 0 )
	error('COPY_ATT succeeded with a bad attribute name when it should have failed.');
end


status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end



%-------------------------------------------------------------------------
function test_non_numeric_source_ncid(ncfile1,ncfile2)
% Test 7:  try with non numeric source ncid

[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'copy_att', 'ncid1', varid1, 'test_double', ncid2, varid2 );
	error('succeeded when it should have failed.');
end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end



%-------------------------------------------------------------------------
function test_non_numeric_source_varid(ncfile1,ncfile2)
% Test 8:  try with non numeric source varid

[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'copy_att', ncid1, 'varid1', 'test_double', ncid2, varid2 );
	error('succeeded when it should have failed.');
	error ( err_msg );
end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end



%-------------------------------------------------------------------------
function test_non_numeric_destination_ncid(ncfile1,ncfile2)
% Test 9:  try with non numeric target ncid

[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'copy_att', ncid1, varid1, 'test_double', 'ncid2', varid2 );
	error('succeeded when it should have failed.');
end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end


%-------------------------------------------------------------------------
function test_non_numeric_destination_varid(ncfile1,ncfile2)
% Test 10:  try with non numeric target varid

[ncid1,status] = mexnc('open',ncfile1,nc_nowrite_mode);
if status, error(mexnc('strerror',status)), end

[ncid2,status] = mexnc('open',ncfile2,nc_write_mode);
if status, error(mexnc('strerror',status)), end

[varid1,status] = mexnc('inq_varid',ncid1,'x');
if status, error(mexnc('strerror',status)), end

[varid2,status] = mexnc('inq_varid',ncid2,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc('redef',ncid2');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'copy_att', ncid1, varid1, 'test_double', ncid2, 'varid2' );
	error('succeeded when it should have failed.');
end

status = mexnc ( 'close', ncid1 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid2);
if status, error(mexnc('strerror',status)), end


















