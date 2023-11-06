function test__enddef ( ncfile, ncfile2 )
% TEST__ENDDEF:

switch(nargin)
    case 0
        ncfile = 'foo.nc';
        ncfile2 = 'foo2.nc';
    case 1
        ncfile2 = 'foo2.nc';
end    
% Increase the header space by 20000 bytes.


[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end


%
% Define a dimension and a variable.
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error(mexnc('strerror',status)), end

[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', nc_double, 1, xdimid );
if status, error(mexnc('strerror',status)), end


%
% End the definitions, but leave space in the header.
% Usually, nc_enddef(ncid) is equivalent to nc__enddef ( ncid, 0, 4, 0, 4 );
[status] = mexnc ( '_enddef', ncid, 20000, 4, 0, 4 );
if status, error(mexnc('strerror',status)), end


status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


d = dir ( ncfile );
if d.bytes < 20000
    msg = sprintf ( '%s:  %s:  __enddef didn''t work.\n', mfilename, testid );
    error ( msg );
end

fprintf('__ENDDEF succeeded.\n' );

return













