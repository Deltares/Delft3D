function test_inq_att ( ncfile )
% TEST_INQ_ATT:  
%
% The matlab API is
%
%     [datatype, attlen, status] = mexnc ( 'inq_att', ncid, varid, attname );
%    
%
% Test 1:  Normal retrieval.
% Test 2:  Invalid ncid 
% Test 3:  Invalid varid 
% Test 4:  Invalid name. 
% Test 5:  ncid = []
% Test 6:  varid = [] 
% Test 7:  name = []
% Test 8:  non numeric ncid
% Test 9:  non numeric varid
% Test 10: non character attribute name


create_ncfile ( ncfile );
test_001 ( ncfile );
test_002 ( ncfile );
test_003 ( ncfile );
test_004 ( ncfile );
test_005 ( ncfile );
test_006 ( ncfile );
test_007 ( ncfile );
test_008 ( ncfile );
test_009 ( ncfile );
test_010 ( ncfile );

fprintf ( 1, 'INQ_ATT succeeded.\n' );
return





function create_ncfile ( ncfile )


[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error ( mexnc('STRERROR',status) ), end


[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if status, error ( mexnc('STRERROR',status) ), end

input_data = [3.14159 0];
status = mexnc ( 'put_att_double', ncid, varid, 'test_double', nc_double, 2, input_data );
if status, error ( mexnc('STRERROR',status) ), end

[status] = mexnc ( 'enddef', ncid );
if status, error ( mexnc('STRERROR',status) ), end

mexnc ( 'close', ncid );






function test_001 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

[datatype, att_length, status] = mexnc ( 'inq_att', ncid, varid, 'test_double' );
if status, error ( mexnc('STRERROR',status) ), end

if datatype ~= nc_double
	msg = sprintf ( '%s:  datatype did not match.\n', mfilename );
	error ( msg );
end

if att_length ~= 2
	msg = sprintf ( '%s:  attribute length did not match.\n', mfilename );
	error ( msg );
end

mexnc ( 'close', ncid );






function test_002 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

testid = 'Test 2';
[datatype,attlen,status] = mexnc ( 'INQ_ATT', -20000, varid, 'test_double' );
if ( status == 0 )
	error ( 'Invalid ncid' );
end

	
mexnc ( 'close', ncid );




function test_003 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 3:  Invalid varid 
testid = 'Test 3';
[datatype,value,status] = mexnc ( 'INQ_ATT', ncid,  -2000, 'test_double' );
if ( status == 0 )
	error ( 'invalid varid' );
end
mexnc ( 'close', ncid );


	
function test_004 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 4:  Invalid name. 
testid = 'Test 4';
[datatype,value,status] = mexnc ( 'INQ_ATT', ncid,  varid, 'test_double2' );
if ( status == 0 )
	error ( 'invalid attribute name' );
end
mexnc ( 'close', ncid );


	
function test_005 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 5:  ncid = []
testid = 'Test 5';
try
	[value,status] = mexnc ( 'INQ_ATT', [], varid, 'test_double' );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end
mexnc ( 'close', ncid );



function test_006 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 6:  varid = [] 
testid = 'Test 6';
try
	[value,status] = mexnc ( 'INQ_ATT', ncid, [], 'test_double' );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end
mexnc ( 'close', ncid );



function test_007 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 7:  name = []
testid = 'Test 7';
try
	[value,status] = mexnc ( 'INQ_ATT', ncid, varid, [] );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end
mexnc ( 'close', ncid );



function test_008 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 8:  non numeric ncid
testid = 'Test 8';
try
	[value,status] = mexnc ( 'INQ_ATT', 'ncid', varid, 'test_double' );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end
mexnc ( 'close', ncid );



function test_009 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 9:  non numeric varid
testid = 'Test 9';
try
	[value,status] = mexnc ( 'INQ_ATT', ncid, 'varid', 'test_double' );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end
mexnc ( 'close', ncid );



function test_010 ( ncfile )

[ncid, status] = mexnc ( 'OPEN', ncfile, nc_nowrite_mode );
if status, error ( mexnc('STRERROR',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('STRERROR',status) ), end

% Test 10: non character attribute name
testid = 'Test 10';
try
	[value,status] = mexnc ( 'INQ_ATT', ncid, varid, 0 );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


mexnc ( 'close', ncid );

