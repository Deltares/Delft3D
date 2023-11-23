function test_vardef ( ncfile )
% TEST_VARDEF

if nargin < 1
    ncfile = 'foo.nc';
end

% Test:  Create a singleton dimension using [] as the list of dimids.
test_empty_set(ncfile);

% Test:  Create a singleton dimension using 0 as number of dimensions
test_zero_dims(ncfile);

% Test:  Test with bad ncid, bad dimension id
test_bad_ncid_dimid(ncfile);

fprintf ( 1, 'VARDEF succeeded.\n' );


%-----------------------------------------------------------------------
function test_bad_ncid_dimid(ncfile)
% Test:  bad ncid, bad dimid
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[xdvarid, status] = mexnc ( 'vardef', ncid, 'x_double', 'double', 1, xdimid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

% Try a bad ncid.
[test_dimid, status] = mexnc ( 'vardef', -2, 'x_double', 'double', 1, xdimid );
if ( status >= 0 ), error ( 'DEF_VAR succeeded on a bad ncid\n' ), end

% Try a bad dimid.
[test_dimid, status] = mexnc ( 'vardef', ncid, 'x_double', 'double', 1, -3 );
if ( status >= 0 ), error ( 'DEF_VAR succeeded on a bad ncid' ), end

[status] = mexnc ( 'enddef', ncid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

status = mexnc ( 'close', ncid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

return





%-----------------------------------------------------------------------
function test_zero_dims(ncfile)
% Test:  Create a singleton dimension using 0 as the number of dimensions, 
% plus a dimid (that's just wrong!)
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[xdvarid, status] = mexnc ( 'vardef', ncid, 'x_double', 'double', 1, xdimid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[singleton, status] = mexnc ( 'vardef', ncid, 'x_empty', 'double', 0, 0 );
if status 
	error('VARDEF failed' );
end

[status] = mexnc ( 'enddef', ncid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[name,dtype,ndims,dimids,natts,status] = mexnc('varinq',ncid,singleton);
if ( status < 0 ), error ( mexnc('strerror', status) ), end

if ( ndims ~= 0)
	error('number of dimensions was not zero for singleton');
end
if ~isempty(dimids)
	error('list of dimensions was not empty set');
end

status = mexnc ( 'close', ncid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

return




%-----------------------------------------------------------------------
function test_empty_set(ncfile)
% Test:  Create a singleton dimension using [] as the list of dimids.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[xdvarid, status] = mexnc ( 'vardef', ncid, 'x_double', 'double', 1, xdimid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

[singleton, status] = mexnc ( 'vardef', ncid, 'x_empty', 'double', 0, [] );
if status 
	error('VARDEF failed' );
end

[status] = mexnc ( 'enddef', ncid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

status = mexnc ( 'close', ncid );
if ( status < 0 ), error ( mexnc('strerror', status) ), end

return













