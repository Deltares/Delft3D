function test_endef ( ncfile )
% TEST_ENDEF
%
% Tests ENDEF by defining a new dimension.  Then tests
% REDEF by defining another dimension.
%
% Test 1:  Usual ENDEF
% Test 2:  File is not in define mode. 
% Test 3:  Bad ncid.


test_001 ( ncfile );
test_002 ( ncfile );
test_003 ( ncfile );

fprintf ( 1, 'ENDEF succeeded.\n' );
return




function test_001 ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'endef', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end



function test_002 ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'endef', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'endef', ncid );
if ( status == 0 )
	status = mexnc ( 'close', ncid );
	error ( 'Failed to flag a bad ENDEF call' ) 
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end




return

function test_003 ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'endef', ncid-1 );
if ( status == 0 )
	status = mexnc ( 'close', ncid );
	error ( 'Failed to flag a bad ENDEF call' ) 
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end




return

