function test_del_att ( ncfile )

if nargin == 0
	ncfile = 'foo.nc';
end

create_ncfile(ncfile);

test_normal_delete(ncfile);
test_bad_ncid(ncfile);
test_bad_varid(ncfile);
test_empty_name(ncfile);
test_bad_name(ncfile);
test_empty_ncid(ncfile);
test_empty_varid(ncfile);
test_empty_attname(ncfile);
test_bad_ncid_datatype(ncfile);
test_bad_varid_datatype(ncfile);
test_bad_attname_datatype(ncfile);

fprintf('DEL_ATT succeeded.\n');

%--------------------------------------------------------------------------
function create_ncfile(ncfile)

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error(mexnc('strerror',status)), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if status, error(mexnc('strerror',status)), end

input_data = 3.14159;
status = mexnc ( 'put_att_double', ncid, varid, 'test_double', nc_double, 1, input_data );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'enddef', ncid  );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end





%--------------------------------------------------------------------------
function test_normal_delete(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'del_att', ncid, varid, 'test_double' );
if status, error(mexnc('strerror',status)), end

[attnum, status] = mexnc ( 'inq_attid', ncid, varid, 'test_double' );
if ( status >= 0 )
	error('attribute was not deleted');
end

input_data = 3.14159;
status = mexnc ( 'put_att_double', ncid, varid, 'to_be_deleted', nc_double, 1, input_data );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_bad_ncid(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'del_att', -2000, varid, 'to_be_deleted' );
if ( status == 0 )
	error('failed');
end


status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end






%--------------------------------------------------------------------------
function test_bad_varid(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'del_att', ncid, -2000, 'to_be_deleted' );
if ( status == 0 )
	error('failed');
end


status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_empty_name(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'del_att', ncid, varid, '' );
	error('failed');
end


status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_bad_name(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'del_att', ncid, varid, 'I know nothing' );
if ( status == 0 )
	error('failed');
end

status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_empty_ncid(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'del_att', [], varid, 'I know less than nothing' );
	error('failed');
catch
	;
end


status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end





%--------------------------------------------------------------------------
function test_empty_varid(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'del_att', ncid, [], 'I know less than nothing' );
	error('failed');
catch
	;
end


status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_empty_attname(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'del_att', ncid, varid, [] );
	error('failed');
catch
	;
end

status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_bad_ncid_datatype(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'del_att', 'ncid', varid, 'to_be_deleted' );
	error('failed');
end

status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end


%--------------------------------------------------------------------------
function test_bad_varid_datatype(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'del_att', ncid, 'varid', 'to_be_deleted' );
	error('failed');
end

status = mexnc ( 'close', ncid  );
if status, error(mexnc('strerror',status)), end

%--------------------------------------------------------------------------
function test_bad_attname_datatype(ncfile)

[ncid,status] = mexnc('open',ncfile,nc_write_mode);
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid  );
if status, error(mexnc('strerror',status)), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error(mexnc('strerror',status)), end

try
	status = mexnc ( 'del_att', ncid, varid, 5 );
	error('failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



