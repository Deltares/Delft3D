function test_attcopy ( ncfile1, ncfile2 )
% TEST_ATTCOPY

if nargin < 2
    ncfile1 = 'foo1.nc';
    ncfile2 = 'foo2.nc';
end

mexnc ( 'setopts', 0 );

create_testfile ( ncfile1 );
create_testfile ( ncfile2 );

test_copy_double_att ( ncfile1, ncfile2 );
test_copy_bad_src_ncid ( ncfile1, ncfile2 );
test_bad_src_varid ( ncfile1, ncfile2 );
test_bad_dest_ncid ( ncfile1, ncfile2 );
test_copy_bad_dest_varid ( ncfile1, ncfile2 );

fprintf ( 1, 'ATTCOPY succeeded.\n' );

return;







%--------------------------------------------------------------------------
function create_testfile ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status ~= 0, error ( mexnc('strerror',status) ), end

return











%--------------------------------------------------------------------------
function test_copy_double_att ( ncfile1, ncfile2 )
% Test 1:  Copy a double precision attribute.

[ncid1, status] = mexnc ( 'open', ncfile1, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid1, status] = mexnc ( 'inq_varid', ncid1, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

[ncid2, status] = mexnc ( 'open', ncfile2, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid2, status] = mexnc ( 'inq_varid', ncid2, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

input_data = [3.14159];
status = mexnc ( 'put_att_double', ncid1, varid1, 'test_double', nc_double, 1, input_data );
if status ~= 0, error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'enddef', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'sync', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end



status = mexnc ( 'attcopy', ncid1, varid1, 'test_double', ncid2, varid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'enddef', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

[return_value, status] = mexnc ( 'get_att_double', ncid2, varid2, 'test_double' );
if status ~= 0, error ( mexnc('strerror',status) ), end

if return_value ~= 3.14159
	error ('ATTCOPY did not seem to work' );
end

status = mexnc ( 'close', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

return










%--------------------------------------------------------------------------
function test_copy_bad_src_ncid ( ncfile1, ncfile2 )
% Test 2:  Copy from a bad source ncid.  Should fail.

[ncid1, status] = mexnc ( 'open', ncfile1, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid1, status] = mexnc ( 'inq_varid', ncid1, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

[ncid2, status] = mexnc ( 'open', ncfile2, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid2, status] = mexnc ( 'inq_varid', ncid2, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

%
% try a bad ncid1
% Test 2:  Copy from a bad source ncid.  Should fail.
status = mexnc ( 'attcopy', -12, varid1, 'test_double', ncid2, varid2 );
if ( status == 0 )
	err_msg = sprintf ( '%s:  ATTCOPY succeeded with a bad 1st ncid\n', mfilename);
	error ( err_msg );
end


status = mexnc ( 'close', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

return















%--------------------------------------------------------------------------
function test_bad_src_varid ( ncfile1, ncfile2 )
% Test 3:  Copy from a bad source varid.  Should fail.

[ncid1, status] = mexnc ( 'open', ncfile1, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid1, status] = mexnc ( 'inq_varid', ncid1, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

[ncid2, status] = mexnc ( 'open', ncfile2, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid2, status] = mexnc ( 'inq_varid', ncid2, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

% Test 3:  Copy from a bad source varid.  Should fail.
status = mexnc ( 'attcopy', ncid1, -7, 'test_double', ncid2, varid2 );
if ( status == 0 )
	err_msg = sprintf ( '%s:  ATTCOPY succeeded with a bad 1st varid\n', mfilename);
	error ( err_msg );
end

status = mexnc ( 'close', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

return
















%--------------------------------------------------------------------------
function test_bad_dest_ncid ( ncfile1, ncfile2 )
% Test 4:  Copy to a bad destination ncid.  Should fail.

[ncid1, status] = mexnc ( 'open', ncfile1, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid1, status] = mexnc ( 'inq_varid', ncid1, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

[ncid2, status] = mexnc ( 'open', ncfile2, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid2, status] = mexnc ( 'inq_varid', ncid2, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end


status = mexnc ( 'redef', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

% Test 4:  Copy to a bad destination ncid.  Should fail.
status = mexnc ( 'attcopy', ncid1, varid1, 'test_double', -12, varid2 );
if ( status == 0 )
	error ( 'ATTCOPY succeeded with a bad 2nd ncid' );
end

status = mexnc ( 'close', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

return














%--------------------------------------------------------------------------
function test_copy_bad_dest_varid ( ncfile1, ncfile2 )
% Test 5:  Copy to a bad destination varid.  Should fail.

[ncid1, status] = mexnc ( 'open', ncfile1, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid1, status] = mexnc ( 'inq_varid', ncid1, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

[ncid2, status] = mexnc ( 'open', ncfile2, nc_write_mode );
if status ~= 0, error ( mexnc('strerror',status) ), end

[varid2, status] = mexnc ( 'inq_varid', ncid2, 'x' );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

% Test 5:  Copy to a bad destination varid.  Should fail.
status = mexnc ( 'attcopy', ncid1, varid1, 'test_double', ncid2, -12 );
if ( status >= 0 )
	error ('ATTCOPY succeeded with a bad 2nd varid');
end

status = mexnc ( 'close', ncid1 );
if status ~= 0, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid2 );
if status ~= 0, error ( mexnc('strerror',status) ), end

return
