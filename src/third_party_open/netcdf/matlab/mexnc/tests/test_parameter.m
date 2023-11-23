function test_parameter (  )
%
% This routine tests the TYPELEN operation

parms = { 'max_nc_name', ...
    'max_nc_dims', ...
    'max_nc_vars', ...
    'max_nc_attrs', ...
    'nc_byte', ...
    'nc_char', ...
    'nc_clobber', ...
    'nc_double', ...
    'nc_fatal', ...
    'nc_fill', ...
    'nc_float', ...
    'nc_global', ...
    'nc_int', ...
    'nc_long', ...
    'nc_lock', ...
    'nc_noclobber', ...
    'nc_nofill', ...
    'nc_nowrite', ...
    'nc_share', ...
    'nc_short', ...
    'nc_unlimited', ...
    'nc_verbose', ...
    'nc_write' };

for j = 1:numel(parms);
	actVal(j) = mexnc('parameter',parms{j});
end

fprintf ( 1, 'PARAMETER succeeded\n' );

return



function test_001 ( )
%
% Tests NC_DOUBLE input.  Result should be 
%     length = 8 bytes, 
%     status = 0

[len, status] = mexnc ( 'TYPELEN', nc_double );
if ( len ~= 8 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 0 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return









function test_002 ( )
%
% Tests NC_FLOAT input.  Result should be 
%     length = 4 bytes, 
%     status = 0

[len, status] = mexnc ( 'TYPELEN', nc_float );
if ( len ~= 4 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 0 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return









function test_003 ( )
%
% Tests NC_INT input.  Result should be 
%     length = 4 bytes, 
%     status = 0

[len, status] = mexnc ( 'TYPELEN', nc_int );
if ( len ~= 4 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 0 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return









function test_004 ( )
%
% Tests NC_SHORT input.  Result should be 
%     length = 2 bytes, 
%     status = 0

[len, status] = mexnc ( 'TYPELEN', nc_short );
if ( len ~= 2 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 0 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return









function test_005 ( )
%
% Tests NC_BYTE input.  Result should be 
%     length = 1 bytes, 
%     status = 0

[len, status] = mexnc ( 'TYPELEN', nc_byte );
if ( len ~= 1 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 0 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return









function test_006 ( )
%
% Tests NC_CHAR input.  Result should be 
%     length = 1 bytes, 
%     status = 0

[len, status] = mexnc ( 'TYPELEN', nc_char );
if ( len ~= 1 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 0 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return









function test_007 ( )
%
% Tests NC_NAT
%     length = -1
%     status = 1

[len, status] = mexnc ( 'TYPELEN', nc_nat );
if ( len ~= -1 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 1 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return









function test_008 ( )
%
% Tests invalid datatype
%     length = -1
%     status = 1

[len, status] = mexnc ( 'TYPELEN', -100 );
if ( len ~= -1 ) 
    error ( 'TYPELEN returned incorrect length' );
end
if ( status ~= 1 ) 
    error ( 'TYPELEN returned incorrect status' );
end

return











