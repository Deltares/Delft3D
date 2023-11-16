function test_attname ( ncfile )



if nargin < 1
    ncfile = 'foo.nc';
end
create_ncfile(ncfile);
test_existance(ncfile);
test_bad_ncid(ncfile);
test_bad_varid(ncfile);
test_bad_attnum(ncfile);

fprintf('ATTNAME succeeded.\n');

%--------------------------------------------------------------------------
function create_ncfile(ncfile)

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end


[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error(mexnc('strerror',status)), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if status, error(mexnc('strerror',status)), end

input_data = [3.14159];
status = mexnc ( 'put_att_double', ncid, varid, 'test_double', nc_double, 1, input_data );
if status, error(mexnc('strerror',status)), end

[status] = mexnc ( 'enddef', ncid );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'sync', ncid );
if status, error(mexnc('strerror',status)), end

[attnum, status] = mexnc ( 'inq_attid', ncid, varid, 'test_double' );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_existance(ncfile)
% Test 1:  Write an attribute then test for existance.

[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

[attnum, status] = mexnc ( 'inq_attid', ncid, varid, 'test_double' );
if status, error(mexnc('strerror',status)), end

[attname, status] = mexnc ( 'attname', ncid, varid, attnum );
if status, error(mexnc('strerror',status)), end

if ( ~strcmp ( attname, 'test_double' ) )
	error('attribute name retrieved by ATTNAME did not match what we put in there');
end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_bad_ncid(ncfile)
% Test 2:  Inquire from a bad source ncid.  Should fail.
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

[attnum, status] = mexnc ( 'inq_attid', ncid, varid, 'test_double' );
if status, error(mexnc('strerror',status)), end

[attname, status] = mexnc ( 'attname', -5, varid, attnum );
if ( status >= 0 )
	error('ATTNAME succeeded with a bad ncid');
end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end




%--------------------------------------------------------------------------
function test_bad_varid(ncfile)
% Test 3:  Inquire from a bad source varid.  Should fail.
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

[attnum, status] = mexnc ( 'inq_attid', ncid, varid, 'test_double' );
if status, error(mexnc('strerror',status)), end

[attname, status] = mexnc ( 'attname', ncid, -5, attnum );
if ( status >= 0 )
	error('ATTNAME succeeded with a bad varid');
end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end



%--------------------------------------------------------------------------
function test_bad_attnum(ncfile)
% Test 4:  Inquire from a bad attnum.  Should fail.
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

[attname, status] = mexnc ( 'attname', ncid, varid, -89877 );
if ( status >= 0 )
	error('ATTNAME succeeded with a bad attnum');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end
