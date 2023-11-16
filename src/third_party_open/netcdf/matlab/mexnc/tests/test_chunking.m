function test_chunking ( ncfile )

if nargin == 0
	ncfile = 'foo.nc';
end

v = mexnc('inq_libvers');
if v(1) ~= '4'
	fprintf('chunking tests filtered out when the library version is less than 4.0.\n');
	return
end

test_netcdf3(ncfile);                   % #1 
test_netcdf3_64bit(ncfile);             % #2 
test_netcdf4_1d(ncfile);                % #3 
test_netcdf4_2d(ncfile);                % #4 
test_netcdf4_contiguous_with_csize(ncfile);  % #5 
test_use_tmw(ncfile);                        % #6 

fprintf ( 'DEF_VAR_CHUNKING succeeded.\n' );
fprintf ( 'INQ_VAR_CHUNKING succeeded.\n' );

return


%--------------------------------------------------------------------------
function test_netcdf3(ncfile)
% This test should fail because chunking is not allowed on netcdf-3

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdimid,status] = mexnc('def_dim',ncid,'x',100);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

try
	status = mexnc('def_var_chunking',ncid,varid,'contiguous',[]);
catch
	% Good.  We caught the error.  Just close the file and be done
	% with it.
	status = mexnc ( 'close', ncid );
	return
end

error('should not have been able to chunk a netcdf-3 variable');

return



%--------------------------------------------------------------------------
function test_netcdf3_64bit(ncfile)
% This test should fail because chunking is not allowed on netcdf-3 files,
% whether they are 64-bit or not

mode = bitor(nc_clobber_mode,nc_64bit_offset_mode);
[ncid, status] = mexnc ( 'create', ncfile, mode );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdimid,status] = mexnc('def_dim',ncid,'x',100);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

try
	status = mexnc('def_var_chunking',ncid,varid,'contiguous',[]);
catch
	% Good.  We caught the error.  Just close the file and be done
	% with it.
	status = mexnc ( 'close', ncid );
	return
end

error('should not have been able to chunk a netcdf-3 64bit offset variable');

return



%--------------------------------------------------------------------------
function test_netcdf4_1d(ncfile)
% 

delete(ncfile);
[ncid, status] = mexnc ( 'create', ncfile, nc_netcdf4_classic );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdimid,status] = mexnc('def_dim',ncid,'x',100);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

status = mexnc('def_var_chunking',ncid,xdvarid,'contiguous',[]);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

status = mexnc('enddef',ncid);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[storage,chunksize,status] = mexnc('inq_var_chunking',ncid,xdvarid);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

if ~strcmp(storage,'contiguous')
	error('1D contiguous storage failed');
end

if ~isempty(chunksize)
	error('1D chunking failed');
end

status = mexnc('close',ncid);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end


return



%--------------------------------------------------------------------------
function test_netcdf4_2d(ncfile)
% 

delete(ncfile);
[ncid, status] = mexnc ( 'create', ncfile, nc_netcdf4_classic );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdimid,status] = mexnc('def_dim',ncid,'x',100);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[ydimid,status] = mexnc('def_dim',ncid,'y',200);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[varid1, status] = mexnc ( 'def_var', ncid, 'z', nc_double, 2, [xdimid ydimid] );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

status = mexnc('def_var_chunking',ncid,varid1,'chunked',[10 20]);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[varid2, status] = mexnc ( 'def_var', ncid, 'alpha', nc_double, 2, [xdimid ydimid] );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

status = mexnc('def_var_chunking',ncid,varid2,'contiguous',[]);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

status = mexnc('enddef',ncid);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[storage1,chunksize1,status] = mexnc('inq_var_chunking',ncid,varid1);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

if ~strcmp(storage1,'chunked')
	error('2D contiguous storage failed');
end

if isempty(chunksize1)
	error('2D chunking failed');
end

if ((chunksize1(1) ~= 10) || (chunksize1(2) ~= 20))
	error('2D chunking failed');
end

[storage2,chunksize2,status] = mexnc('inq_var_chunking',ncid,varid2);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

if ~strcmp(storage2,'contiguous')
	error('2D contiguous storage failed');
end

if ~isempty(chunksize2)
	error('2D chunking failed');
end


status = mexnc('close',ncid);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end


return



%--------------------------------------------------------------------------
function test_netcdf4_contiguous_with_csize(ncfile)
% 

delete(ncfile);
[ncid, status] = mexnc ( 'create', ncfile, nc_netcdf4_classic );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdimid,status] = mexnc('def_dim',ncid,'x',100);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end


try
	status = mexnc('def_var_chunking',ncid,varid,'contiguous',5);
catch
	% Good.  We caught the error.  Just close the file and be done
	% with it.
	status = mexnc ( 'close', ncid );
	return
end


return


%--------------------------------------------------------------------------
function test_use_tmw(ncfile)

% This test is meaningless unless the preference is set to zero.
use_tmw = getpref('MEXNC','USE_TMW');
if ~use_tmw
	return
end


% chunking isn't supported in TMW yet.
delete(ncfile);
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[xdimid,status] = mexnc('def_dim',ncid,'x',100);
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if ( status ~= 0 ), error(mexnc ( 'strerror', status )), end


try
	status = mexnc('def_var_chunking',ncid,varid,'contiguous',[]);
catch
	% Good.  We caught the error.  Just close the file and be done
	% with it.
	status = mexnc ( 'close', ncid );
	return
end


return


