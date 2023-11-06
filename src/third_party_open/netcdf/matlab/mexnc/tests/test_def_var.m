function test_def_var ( ncfile )
% TEST_DEF_VAR
%
% Test 1:  Create a double var
% Test 2:  Create a float var
% Test 3:  Create an int32 var
% Test 4:  Create an int16 var
% Test 5:  Create a byte var
% Test 6:  Create a char var
% Test 7:  Bad ncid.
% Test 8:  Empty name.
% Test 9:  Bogus datatype.
% Test 10:  Bad number of dimensions.
% Test 11:  Bogus dimid.
% Test 12:  ncid is not numeric
% Test 13:  varname is not character
% Test 14:  datatype is not character or numeric
% Test 15:  ndims is not numeric
% Test 16:  dimids is not numeric
% Test 17:  try to pass too many dimensions

if nargin<1
	ncfile = 'foo.nc';
end

create_testfile(ncfile);
test_double(ncfile);
test_float(ncfile);
test_int32(ncfile);
test_int16(ncfile);
test_byte(ncfile);
test_char(ncfile);
test_bad_ncid(ncfile);
test_empty_name(ncfile);
test_bad_datatype(ncfile);
test_bad_num_dims(ncfile);
test_bad_dimid(ncfile);
test_non_numeric_ncid(ncfile);
test_bad_datatype_datatype(ncfile);
test_non_numeric_ndims(ncfile);
test_non_numeric_dimids(ncfile);
test_too_many_dimensions(ncfile);

fprintf('DEF_VAR succeeded.\n');

%--------------------------------------------------------------------------
function create_testfile(ncfile)

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end;

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error(mexnc('strerror',status)), end;

[status] = mexnc ( 'end_def', ncid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;


%--------------------------------------------------------------------------
function test_double(ncfile)
% Test 1:  Create a double var

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', nc_double, 1, xdimid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
%--------------------------------------------------------------------------
function test_float(ncfile)
% Test 2:  Create a float var
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_float', nc_float, 1, xdimid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
%--------------------------------------------------------------------------
function test_int32(ncfile)
% Test 3:  Create an int32 var
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_int32', nc_int, 1, xdimid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
%--------------------------------------------------------------------------
function test_int16(ncfile)
% Test 4:  Create an int16 var
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_int16', nc_short, 1, xdimid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
%--------------------------------------------------------------------------
function test_byte(ncfile)
% Test 5:  Create a byte var
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_byte', nc_byte, 1, xdimid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
%--------------------------------------------------------------------------
function test_char(ncfile)
% Test 6:  Create a char var
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_char', nc_char, 1, xdimid );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
%--------------------------------------------------------------------------
function test_bad_ncid(ncfile)
% Test 7:  Bad ncid.
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[test_dimid, status] = mexnc ( 'def_var', -2, 'x_double', nc_double, 1, xdimid );
if ( status == 0 )
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

%--------------------------------------------------------------------------
function test_empty_name(ncfile)
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[test_dimid, status] = mexnc ( 'def_var', ncid, '', nc_double, 1, xdimid );
if ( status == 0 )
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

%--------------------------------------------------------------------------
function test_bad_datatype(ncfile)
% Test 9:  Bogus datatype.
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

fprintf ( 2, 'Bogus character datatype.  Need to rethink this at some point.\n' );
%[test_dimid, status] = mexnc ( 'def_var', ncid, 'xxx', 'bad_data_type', 1, xdimid );
%if ( status == 0 )
%	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid );
%	error ( err_msg );
%end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

%--------------------------------------------------------------------------
function test_bad_num_dims(ncfile)
% Test 10:  Bad number of dimensions.
testid = 'Test 10';
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

try
	[test_dimid, status] = mexnc ( 'def_var', ncid, 'xxx', nc_double, 5, xdimid );
	error('succeeded when it should have failed');
end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

%--------------------------------------------------------------------------
function test_bad_dimid(ncfile)
% Test 11:  Bogus dimid.
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

[test_dimid, status] = mexnc ( 'def_var', ncid, 'xxx', nc_double, 1, -5 );
if ( status == 0 )
	error('succeeded when it should have failed');
end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;


%--------------------------------------------------------------------------
function test_non_numeric_ncid(ncfile)
% Test 12:  ncid is not numeric
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

try
	[xdvarid, status] = mexnc ( 'def_var', 'ncid', 'x_double12', nc_double, 1, xdimid );
	error('succeeded when it should have failed');
end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;



%--------------------------------------------------------------------------
function test_non_char_varname(ncfile)
% Test 13:  varname is not character
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

try
	[xdvarid, status] = mexnc ( 'def_var', ncid, 25, nc_double, 1, xdimid );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;


%--------------------------------------------------------------------------
function test_bad_datatype_datatype(ncfile)
% Test 14:  datatype is not character or numeric
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

try
	[xdvarid, status] = mexnc ( 'def_var', ncid, 't14', struct([]), 1, xdimid );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

%--------------------------------------------------------------------------
function test_non_numeric_ndims(ncfile)
% Test 15:  ndims is not numeric
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

try
	[xdvarid, status] = mexnc ( 'def_var', ncid, 't14', nc_double, '1', xdimid );
	error('succeeded when it should have failed' );
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

%--------------------------------------------------------------------------
function test_non_numeric_dimids(ncfile)
% Test 16:  dimids is not numeric
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

try
	[xdvarid, status] = mexnc ( 'def_var', ncid, 't14', nc_double, 1, 'd');
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
%--------------------------------------------------------------------------
function test_too_many_dimensions(ncfile)
% Test 17:  try to pass too many dimensions
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc('redef',ncid );
if status, error(mexnc('strerror',status)), end;

[xdimid,status] = mexnc('inq_dimid',ncid,'x');
if status, error(mexnc('strerror',status)), end;

try
	num_dims = 10000;
	[xdvarid, status] = mexnc ( 'def_var', ncid, 't14', nc_double, 10000, xdimid*ones(num_dims,1) );
	error('succeeded when it should have failed');
end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;


%--------------------------------------------------------------------------

