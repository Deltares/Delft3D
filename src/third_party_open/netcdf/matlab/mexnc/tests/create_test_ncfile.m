function create_test_ncfile ( ncfile )
% CREATE_TEST_NCFILE:  creates a file to use for several tests.
%

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end



%
% Create the fixed dimension.  
len_x = 4;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if status, error(mexnc('strerror',status)), end


len_y = 6;
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', len_y );
if status, error(mexnc('strerror',status)), end


% Create the double singleton variable
[varid, status] = mexnc ( 'def_var', ncid, 'double_singleton', nc_double, 0, [] );
if status, error(mexnc('strerror',status)), end


% Create the float singleton variable
[varid, status] = mexnc ( 'def_var', ncid, 'single_singleton', nc_float, 0, [] );
if status, error(mexnc('strerror',status)), end


% Create the int singleton variable
[varid, status] = mexnc ( 'def_var', ncid, 'int_singleton', nc_int, 0, [] );
if status, error(mexnc('strerror',status)), end


% Create the short singleton variable
[varid, status] = mexnc ( 'def_var', ncid, 'short_singleton', nc_short, 0, [] );
if status, error(mexnc('strerror',status)), end



% Create the byte singleton variable
[varid, status] = mexnc ( 'def_var', ncid, 'byte_singleton', nc_byte, 0, [] );
if status, error(mexnc('strerror',status)), end


% Create the char singleton variable
[varid, status] = mexnc ( 'def_var', ncid, 'char_singleton', nc_char, 0, [] );
if status, error(mexnc('strerror',status)), end


[varid, status] = mexnc ( 'def_var', ncid, 'z_double', nc_double, 2, [ydimid xdimid] );
if status, error(mexnc('strerror',status)), end

[varid, status] = mexnc ( 'def_var', ncid, 'z_float', nc_float, 2, [ydimid xdimid] );
if status, error(mexnc('strerror',status)), end

[varid, status] = mexnc ( 'def_var', ncid, 'z_int', nc_int, 2, [ydimid xdimid] );
if status, error(mexnc('strerror',status)), end


[varid, status] = mexnc ( 'def_var', ncid, 'z_short', nc_short, 2, [ydimid xdimid] );
if status, error(mexnc('strerror',status)), end


[varid, status] = mexnc ( 'def_var', ncid, 'z_byte', nc_byte, 2, [ydimid xdimid] );
if status, error(mexnc('strerror',status)), end

[varid, status] = mexnc ( 'def_var', ncid, 'z_char', nc_char, 2, [ydimid xdimid] );
if status, error(mexnc('strerror',status)), end


[status] = mexnc ( 'enddef', ncid );
if status, error(mexnc('strerror',status)), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if status, error(mexnc('strerror',status)), end



status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end


return






