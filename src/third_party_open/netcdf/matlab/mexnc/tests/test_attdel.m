function test_attdel ( ncfile )

if nargin < 1 
    ncfile = 'foo.nc'; 
end

create_test_file(ncfile);
test_delete_double(ncfile);
test_bad_ncid(ncfile);
test_bad_varid(ncfile);
test_does_not_exist(ncfile);

fprintf('ATTDEL succeeded.\n' );

%--------------------------------------------------------------------------
function create_test_file(ncfile)

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error( mexnc ( 'strerror', status ) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error( mexnc ( 'strerror', status ) ), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if status, error( mexnc ( 'strerror', status ) ), end

input_data = 3.14159;
status = mexnc ( 'put_att_double', ncid, varid, 'test_double', nc_double, 1, input_data );
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc('close',ncid);
if status, error( mexnc ( 'strerror', status ) ), end


%--------------------------------------------------------------------------
function test_delete_double(ncfile)
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc('redef',ncid);
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

% Test 1:  delete a double precision attribute of a variable
status = mexnc ( 'ATTDEL', ncid, varid, 'test_double' );
if (status < 0), error( mexnc ( 'strerror', status ) ), end

[attnum, status] = mexnc ( 'inq_attid', ncid, varid, 'test_double' );
if ( status >= 0 )
	error  ( 'attribute was not deleted' );
end

status = mexnc('close',ncid);
if status, error( mexnc ( 'strerror', status ) ), end




%--------------------------------------------------------------------------
function test_bad_ncid(ncfile)
% Test 2:  try to delete from a bad ncid

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc('redef',ncid);
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'ATTDEL', -5, varid, 'test_double' );
if ( status ~= -1 )
	err_msg = sprintf ( '%s:  ATTDEL succeeded on a bad ncid.\n', mfilename );
	error ( err_msg );
end

status = mexnc('close',ncid);
if status, error( mexnc ( 'strerror', status ) ), end



%--------------------------------------------------------------------------
function test_bad_varid(ncfile)
% Test 3:  try to delete from a bad varid

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc('redef',ncid);
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'ATTDEL', ncid, -5, 'test_double' );
if ( status ~= -1 )
	error ( 'ATTDEL succeeded on a bad varid.' );
end


status = mexnc('close',ncid);
if status, error( mexnc ( 'strerror', status ) ), end



%--------------------------------------------------------------------------
function test_does_not_exist(ncfile)
% Test 4:  try to delete a non-existant attribute

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc('redef',ncid);
if status, error( mexnc ( 'strerror', status ) ), end

[varid,status] = mexnc('inq_varid',ncid,'x');
if status, error( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'ATTDEL', ncid, varid, 'blah' );
if ( status ~= -1 )
	error ( 'ATTDEL succeeded on a bad attribute name.'  );
end

status = mexnc('close',ncid);
if status, error( mexnc ( 'strerror', status ) ), end



