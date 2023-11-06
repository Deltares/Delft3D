function test_nc_datatype_string()
% TEST_NC_DATATYPE_STRING:
%
% Bad input argument tests.
% Test 1:  no inputs
% Test 2:  two inputs
% test 3:  input not numeric
% test 4:  input is outside of 0-6
%
% These tests should succeed
% test 5:  input is 0 ==> 'NC_NAT'
% test 6:  input is 1 ==> 'NC_BYTE'
% test 7:  input is 2 ==> 'NC_CHAR'
% test 8:  input is 3 ==> 'NC_SHORT'
% test 9:  input is 4 ==> 'NC_INT'
% test 10:  input is 5 ==> 'NC_FLOAT'
% test 11:  input is 6 ==> 'NC_DOUBLE'

fprintf('\t\tTesting NC_DATATYPE_STRING ...  ');

run_negative_tests;
run_positive_tests;

fprintf('OK\n');

%--------------------------------------------------------------------------
function run_negative_tests()
test_no_inputs;
test_too_many_inputs;
test_input_not_numeric;
test_input_out_of_range;
return

%--------------------------------------------------------------------------
function run_positive_tests()

test_nat;
test_byte;
test_char;
test_short;
test_int;
test_float;
test_double;

return






%--------------------------------------------------------------------------
function test_no_inputs (  )

try
	nc_datatype_string;
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');










%--------------------------------------------------------------------------
function test_too_many_inputs (  )

try
	nc_datatype_string ( 0, 1 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');










%--------------------------------------------------------------------------
function test_input_not_numeric ( )

% test 3:  input not numeric
try
	nc_datatype_string ( 'a' );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');










%--------------------------------------------------------------------------
function test_input_out_of_range (  )


try
	nc_datatype_string ( -1 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');








%--------------------------------------------------------------------------
function test_nat (  )
%
% These tests should succeed
% test 5:  input is 0 ==> 'NC_NAT'
dt = nc_datatype_string ( 0 );
if ~strcmp(dt,'NC_NAT')
	error ( 'failed to convert 0.');
end
return










%--------------------------------------------------------------------------
function test_byte (  )

dt = nc_datatype_string ( 1 );
if ~strcmp(dt,'NC_BYTE')
	error( 'failed to convert 1.\n' );
end
return










%--------------------------------------------------------------------------
function test_char ( )

dt = nc_datatype_string ( 2 );
if ~strcmp(dt,'NC_CHAR')
	error( 'failed to convert 2.' );
end
return











%--------------------------------------------------------------------------
function test_short (  )

dt = nc_datatype_string ( 3 );
if ~strcmp(dt,'NC_SHORT')
	error('failed to convert 3.');
end
return










%--------------------------------------------------------------------------
function test_int ( )

dt = nc_datatype_string ( 4 );
if ~strcmp(dt,'NC_INT')
	error( 'failed to convert 4.' );
end
return










%--------------------------------------------------------------------------
function test_float (  )

dt = nc_datatype_string ( 5 );
if ~strcmp(dt,'NC_FLOAT')
	error( 'failed to convert 5.');
end
return










%--------------------------------------------------------------------------
function test_double (  )

dt = nc_datatype_string ( 6 );
if ~strcmp(dt,'NC_DOUBLE')
	error('failed to convert 6.');
end
return












