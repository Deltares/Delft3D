function test_typelen (  )
% TEST_TYPELEN
%
% This routine tests the TYPELEN operation
%
% Test 001:  NC_DOUBLE
% Test 002:  NC_FLOAT
% Test 003:  NC_INT
% Test 004:  NC_SHORT
% Test 005:  NC_BYTE
% Test 006:  NC_CHAR
% Test 007:  NC_NAT
% Test 008:  invalid input

test_001;
test_002;
test_003;
test_004;
test_005;
test_006;
test_007;
test_008;

fprintf ( 1, 'TYPELEN succeeded\n' );

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










