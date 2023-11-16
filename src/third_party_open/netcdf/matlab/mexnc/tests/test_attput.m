function test_attput ( ncfile )

if nargin < 1
	ncfile = 'foo.nc';
end

create_testfile ( ncfile );
test_write_double ( ncfile );
test_write_float ( ncfile );
test_write_int32 ( ncfile );
test_write_int16 ( ncfile );
test_write_uchar ( ncfile );
test_write_schar ( ncfile );
test_write_char ( ncfile );
test_read_double ( ncfile );
test_read_float ( ncfile );
test_read_int32 ( ncfile );
test_read_int16 ( ncfile );
test_read_uchar ( ncfile );
test_read_schar ( ncfile );
test_read_char ( ncfile );
test_write_bad_ncid ( ncfile );
test_write_bad_varid ( ncfile );
%test_write_bad_dtype ( ncfile );
test_write_bad_length ( ncfile );
test_read_bad_ncid ( ncfile );
test_read_bad_varid ( ncfile );
test_read_bad_name ( ncfile );
test_zero_length_nan_as_char ( ncfile );
test_zero_length_inf_as_char ( ncfile );
test_gatt_with_char_global_id(ncfile);


fprintf('ATTGET succeeded.\n' );
fprintf('ATTPUT succeeded.\n' );




%===============================================================================
function create_testfile ( ncfile );

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error ( mexnc('strerror',status) ), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'def_var', ncid, 'x', nc_double, 1, xdimid );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

return





%===============================================================================
function test_write_double ( ncfile )
% Test 1:  Write a double attribute.

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

input_data = double_data;
status = mexnc ( 'ATTPUT', ncid, varid, 'test_double', nc_double, 1, input_data );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

return







%===============================================================================
function test_write_float ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 2:  Write a float attribute.
status = mexnc ( 'ATTPUT', ncid, varid, 'test_float', nc_float, 1, double(float_data) );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end





%===============================================================================
function test_write_int32 ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 3:  Write an int attribute.
status = mexnc ( 'ATTPUT', ncid, varid, 'test_int', nc_int, 1, double_data );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end







%===============================================================================
function test_write_int16 ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 4:  Write a short int attribute.
status = mexnc ( 'ATTPUT', ncid, varid, 'test_short_int', nc_short, 1, double_data );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end




%===============================================================================
function test_write_uchar ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 5:  Write a uchar attribute.
status = mexnc ( 'ATTPUT', ncid, varid, 'test_uchar', nc_byte, 1, double_data );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end



%===============================================================================
function test_write_schar ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 6:  Write an schar attribute.
status = mexnc ( 'ATTPUT', ncid, varid, 'test_schar', nc_byte, 1, double_data );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end





%===============================================================================
function test_write_char ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 7:  Write a character attribute.
input_data = char_data;
status = mexnc ( 'ATTPUT', ncid, varid, 'test_char', nc_char, length(input_data), input_data );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end











%===============================================================================
function test_read_double ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 8:  Read said double attribute.
[return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_double' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if ( return_value ~= double_data )
	error('return value did not match input for ATT[GET,PUT]');
end




%===============================================================================
function test_read_float ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 9:  Read said float attribute.
[return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_float' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if ( single(return_value) ~= float_data )
	error('return value did not match input for ATT[GET,PUT]');
end




%===============================================================================
function test_read_int32 ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 10:  Read said int attribute.
[return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_int' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if ( int32(return_value) ~= int_data )
	error ('return value did not match input for ATT[GET,PUT]');
end







%===============================================================================
function test_read_int16 ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 11:  Read said short int attribute.
[return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_short_int' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if ( int16(return_value) ~= short_int_data )
	error('return value did not match input for ATT[GET,PUT]');
end










%===============================================================================
function test_read_uchar ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 12:  Read said uchar attribute.
[return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_uchar' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if ( return_value ~= floor(double_data) )
	error('return value did not match input for ATT[GET,PUT]');
end





%===============================================================================
function test_read_schar ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 13:  Read said schar attribute.
[return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_schar' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if ( return_value ~= floor(double_data) )
	error('return value did not match input for ATT[GET,PUT]');
end



%===============================================================================
function test_read_char ( ncfile )

double_data = 3.14159;
float_data = single(double_data);
int_data = int32(double_data);
short_int_data = int16(double_data);
uchar_data = uint8(double_data);
schar_data = int8(double_data);
char_data = 'It was a dark and stormy night.  Suddenly a shot rang out.';

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 14:  Read said character attribute.
[return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_char' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if ~strcmp(class(return_value),'char')
	error('return value class did not match ''char''');
end
if ( ~strcmp(deblank(return_value),char_data ) )
	error('return value did not match input for ATTGET');
end




%===============================================================================
function test_write_bad_ncid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end


% Test 15:  Write with a bad ncid.
input_data = 5;
status = mexnc ( 'ATTPUT', -2, varid, 'test_double', nc_double, 1, input_data );
if ( status >= 0 )
	error('ATTPUT succeeded with a bad ncid');
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end



%===============================================================================
function test_write_bad_varid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

% Test 16:  Write with a bad varid.
status = mexnc ( 'ATTPUT', ncid, -2000, 'test_double', nc_double, 1, 0 );
if ( status >= 0 )
	error('ATTPUT succeeded with a bad varid');
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end


%===============================================================================
function test_write_bad_dtype ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 17:  Write with a bad type.
status = mexnc ( 'ATTPUT', ncid, varid, 'test_blah17', -2000, 1, 0 );
if ( status >= 0 )
	error('ATTPUT succeeded with a bad nc_type');
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end




%===============================================================================
function test_write_bad_length ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 18:  Write with a bad length.
%    This should actually succeed.  The old code is set to try to 
%    dynamically figure out how long the attribute is in case of
%    a negative length.
status = mexnc ( 'ATTPUT', ncid, varid, 'test_blah18', nc_double, -2, 0 );
if ( status ~= 0 )
	error('ATTPUT failed when it should have succeeded');
end



status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end






%===============================================================================
function test_read_bad_ncid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 19:  Read with a bad ncid.
try
    [return_value, status] = mexnc ( 'ATTGET', -2, varid, 'test_char_19' );
	error('ATTGET succeeded with a bad ncid');
catch
    ;
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end


%===============================================================================
function test_read_bad_varid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 20:  Read with a bad varid.
try
    [return_value, status] = mexnc ( 'ATTGET', ncid, -2, 'test_char_20' );
	error('ATTGET succeeded with a bad varid');
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end


%===============================================================================
function test_read_bad_name ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'redef', ncid );
if status, error ( mexnc('strerror',status) ), end

[varid, status] = mexnc ( 'INQ_VARID', ncid, 'x' );
if status, error ( mexnc('strerror',status) ), end

% Test 21:  Read with a bad name.
try
    [return_value, status] = mexnc ( 'ATTGET', ncid, varid, 'test_blah_21' );
	error('ATTGET succeeded with a bad name');
end




status = mexnc ( 'close', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	error ( ncerr );
end



return















%===============================================================================
function test_zero_length_nan_as_char ( ncfile )
% This sucks.  The old release apparently allowed one to write a "zero-length"
% NaN double attribute that would get interpreted as char.  Yeah that makes a 
% lot of sense.  Fantastic.
switch(version('-release'))
case { '13', '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
	
otherwise	
	warning ( 'Skipping nan/char, Just don''t use NaN for an attribute value, ok?' );
	return
end

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'attput',ncid,nc_global,'testatt',nc_double,0,NaN);
if status, error ( mexnc('strerror',status) ), end

[attval, status] = mexnc ( 'GET_ATT_TEXT', ncid, nc_global, 'testatt' );
if status, error ( mexnc('strerror',status) ), end

if ~strcmp(class(attval),'char')
	error ( 'Did not return class char for the attribute.\n' );
end

if ~isempty(attval)
	error ( 'Did not return [] for the attribute.\n' );
end




status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end



return



%===============================================================================
function test_gatt_with_char_global_id ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'attput',ncid,'global','testatt',nc_char,2,'hi');
if status, error ( mexnc('strerror',status) ), end

[attval, status] = mexnc ( 'attget', ncid, 'global', 'testatt' );
if status, error ( mexnc('strerror',status) ), end

if ~strcmp(class(attval),'char')
	error ( 'Did not return class char for the attribute.\n' );
end

if ~strcmp(attval,'hi')
	error ( 'Did not return correct attribute value.\n' );
end




status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end



return














%===============================================================================
function test_zero_length_inf_as_char ( ncfile )
% This sucks.  The old release apparently allowed one to write a "zero-length"
% Inf double attribute that would get interpreted as char.  Yeah that makes a 
% lot of sense.  Fantastic.

switch(version('-release'))
case { '13', '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
	
otherwise	
	warning ( 'Skipping inf/char test, Just don''t use Inf for an attribute value, ok?' );
	return
end


[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'attput',ncid,nc_global,'testatt',nc_double,0,Inf);
if status, error ( mexnc('strerror',status) ), end

[attval, status] = mexnc ( 'GET_ATT_TEXT', ncid, nc_global, 'testatt' );
if status, error ( mexnc('strerror',status) ), end

if ~strcmp(class(attval),'char')
	error ( 'Did not return class char for the attribute.\n' );
end

if ~isempty(attval)
	error ( 'Did not return [] for the attribute.\n' );
end


status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

return














