function test_inquire ( ncfile )
% TEST_INQUIRE
%
% Tests number of dimensions, variables, global attributes, record dimension for
% foo.nc
%
% Test 001:  standard test
% Test 002:  1x5 output vector
% Test 003:  bad ncid

if ( nargin == 0 )
	ncfile = 'foo.nc';
end

create_testfile (ncfile);
test_001 ( ncfile );
test_002 ( ncfile );
test_003 ( ncfile );
return;


function create_testfile(ncfile );

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc('strerror') ), end


[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 ), error ( mexnc('strerror') ), end
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 24 );
if ( status ~= 0 ), error ( mexnc('strerror') ), end
[zdimid, status] = mexnc ( 'def_dim', ncid, 'z', 32 );
if ( status ~= 0 ), error ( mexnc('strerror') ), end


[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', 'double', 1, xdimid );
if ( status ~= 0 ), error ( mexnc('strerror') ), end


attvalue = 'this is a test';
attlen = length(attvalue);
status = mexnc ( 'put_att_text', ncid, xdvarid, 'test_variable_attributes', 'char', attlen, attvalue );
if ( status ~= 0 ), error ( mexnc('strerror') ), end

attvalue = 'this is a global test';
attlen = length(attvalue);
status = mexnc ( 'put_att_text', ncid, -1, 'test_global_attributes', 'char', attlen, attvalue );
if ( status ~= 0 ), error ( mexnc('strerror') ), end


[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 ), error ( mexnc('strerror') ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror') ), end

return




function test_002 ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror') ), end

outargs = mexnc('INQUIRE', ncid);

if numel(outargs) ~= 5
	msg = sprintf ( 'INQUIRE did not return a 5-tuple\n' );
	error ( msg );
end

if outargs(1) ~= 3
	msg = sprintf ( 'INQUIRE returned %d dimensions when there should only have been 1\n', ndims );
	error ( msg );
end

if outargs(2) ~= 1 
	msg = sprintf ( 'INQUIRE returned %d variables when there should have been 18\n', nvars );
	error ( msg );
end

if outargs(3) ~= 1
	msg = sprintf ( 'INQUIRE returned %d attributes when there should have been 1\n', natts );
	error ( msg );
end

if outargs(4) ~= -1
	msg = sprintf ( 'INQUIRE returned an unlimited dimension when there should not have been one\n' );
	error ( msg );
end

if outargs(5) ~= 0
	msg = sprintf ( 'INQUIRE returned a non-zero status\n' );
	error ( msg );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror') ), end

return









function test_001 ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[ndims, nvars, natts, recdim, status] = mexnc('INQUIRE', ncid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ndims ~= 3
	msg = sprintf ( 'INQUIRE returned %d dimensions when there should only have been 1\n', ndims );
	error ( msg );
end

if nvars ~= 1 
	msg = sprintf ( 'INQUIRE returned %d variables when there should have been 18\n', nvars );
	error ( msg );
end

if natts ~= 1
	msg = sprintf ( 'INQUIRE returned %d attributes when there should have been 1\n', natts );
	error ( msg );
end

if recdim ~= -1
	msg = sprintf ( 'INQUIRE returned an unlimited dimension when there should not have been one\n' );
	error ( msg );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return











function test_003 ( ncfile )

mexnc('setopts',0);
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[ndims, nvars, natts, recdim, status] = mexnc('INQUIRE', -1);
if ( status >= 0 )
	error ( 'INQUIRE return status did not signal an error on bogus ncid case' );
end
fprintf ( 1, 'INQUIRE succeeded\n' );


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return

