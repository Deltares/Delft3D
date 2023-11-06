function test_varid ( ncfile )
% TEST_VARID
% test 1:  simple check for an existing variable
% test 2:  variable does not exist
% test 3:  bad ncid
% test 4:  illegal variable name

create_testfile ( ncfile );
test_001 ( ncfile );
test_002 ( ncfile );
test_003 ( ncfile );
test_003 ( ncfile );
test_004 ( ncfile );

fprintf ( 1, 'VARID succeeded.\n' );
return

function create_testfile ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;


[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 24 );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

[zdimid, status] = mexnc ( 'def_dim', ncid, 'z', 32 );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;


[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', 'double', 1, xdimid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

return;




function test_001 ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

[varid, status] = mexnc('VARID', ncid, 'x_double');
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

return;





function test_002 ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

[varid, status] = mexnc('VARID', ncid, 'y_double');
if ( status ~= -1 ), error ( mexnc ( 'strerror', status ) ), end;

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

return;





function test_003 ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

[varid, status] = mexnc('VARID', -4, 'x_double');
if ( status >= 0 )
	error ( 'Bogus ncid case succeeded for VARID.\n' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

return;



function test_004 ( ncfile )
lasterror('reset');

[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

try
	[varid,status] = mexnc('VARID', ncid, '');
	error ( 'VARID returned non-negative status for empty string variable\n');
catch
	;
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end;

return;



