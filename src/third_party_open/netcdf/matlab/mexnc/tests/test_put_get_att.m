function test_put_get_att ( ncfile )
% TEST_PUT_GET_ATT:  tests the PUT_ATT and GET_ATT family of calls
%

if nargin < 1
	ncfile = 'foo.nc';
end

create_test_file ( ncfile );

test_writeReadDouble ( ncfile );
test_writeReadFloat ( ncfile );
test_writeReadInt ( ncfile );
test_writeReadShort ( ncfile );
test_writeReadNcByte ( ncfile );
test_readWriteNcChar ( ncfile );


test_writeAsFloatReadAsDouble ( ncfile );
test_writeAsDoubleReadAsFloat ( ncfile );
test_writeAsDoubleReadAsInt ( ncfile );
test_writeAsDoubleReadAsShort ( ncfile );
test_writeAsDoubleReadAsUint8 ( ncfile );
test_writeAsDoubleReadAsInt8 ( ncfile );

test_putAttWhenNotInDefineMode(ncfile);

return;










% Try to write an attribute when the file is in data mode.
function test_putAttWhenNotInDefineMode ( ncfile );

[ncid, status] = mexnc('OPEN', ncfile, nc_write_mode);
if ( status ~= 0 ), error(mexnc('strerror',status)), end

input_data = 'abcdefghijklmnopqrstuvwxyz';

status = mexnc ( 'put_att_text', ncid, nc_global, 'test_attribute_not_in_define_mode', nc_char, numel(input_data), input_data );
if status == 0
	error ( 'writing an attribute succeeded in data mode, should have failed.\n' );
end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end


return







% Write and read back an NC_CHAR attribute using PUT/GET_ATT_TEXT.
function test_readWriteNcChar ( ncfile );

[ncid, status] = mexnc('OPEN', ncfile, nc_write_mode);
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'REDEF', ncid );
if status, error ( mexnc('strerror',status) ), end

input_data = 'abcdefghijklmnopqrstuvwxyz';
status = mexnc('put_att_text',ncid,nc_global,'test_att_text',nc_char,numel(input_data),input_data);
if status, error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_text', ncid, nc_global, 'test_att_text' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if any ( double(input_data(:)) - double(output_data(:)) )
	err_msg = sprintf ( '%s:  attribute values differ.\n', mfilename );
	error ( err_msg );
end

fprintf ( 1, 'PUT_ATT_TEXT succeeded.\n' );
fprintf ( 1, 'GET_ATT_TEXT succeeded.\n' );

return









% Write and read back an NC_BYTE attribute using PUT/GET_ATT_SCHAR.
function test_writeReadNcByte ( ncfile );

[ncid, status] = mexnc('OPEN', ncfile, nc_write_mode);
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'REDEF', ncid );
if status, error ( mexnc('strerror',status) ), end

input_data = int8([-3 6 9]);
status = mexnc ( 'put_att_schar', ncid, nc_global, 'test_int8', nc_byte, 3, input_data );
if status, error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_schar', ncid, nc_global, 'test_int8' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if any ( double(input_data(:)) - double(output_data(:)) )
	err_msg = sprintf ( '%s:  %s:  attribute values differ.\n', mfilename, testid );
	[input_data output_data]
	error ( err_msg );
end


fprintf ( 1, 'PUT_ATT_SCHAR succeeded.\n' );
fprintf ( 1, 'GET_ATT_SCHAR succeeded.\n' );

return




function test_writeReadShort ( ncfile );

[ncid, status] = mexnc('OPEN', ncfile, nc_write_mode);
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'REDEF', ncid );
if status, error ( mexnc('strerror',status) ), end

input_data = int16([3 6 9]);
status = mexnc ( 'put_att_short', ncid, nc_global, 'test_int16', nc_short, 3, input_data );
if status, error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_short', ncid, nc_global, 'test_int16' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if any ( double(input_data(:)) - double(output_data(:)) )
	error ( 'attribute values differ.\n' );
end







function test_writeReadInt ( ncfile );

testid = 'Test 10';
[ncid, status] = mexnc('OPEN', ncfile, nc_write_mode);
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'REDEF', ncid );
if status, error ( mexnc('strerror',status) ), end

input_data = int32([3 6 9]);
status = mexnc ( 'put_att_int', ncid, nc_global, 'test_int32', nc_int, 3, input_data );
if status, error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_int', ncid, nc_global, 'test_int32' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if any ( double(input_data(:)) - double(output_data(:)) )
	error ( 'attribute values differ.\n' );
end

fprintf ( 1, 'PUT_ATT_INT succeeded.\n' );
fprintf ( 1, 'GET_ATT_INT succeeded.\n' );

return






function test_writeReadFloat ( ncfile );

[ncid, status] = mexnc('OPEN', ncfile, nc_write_mode);
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'REDEF', ncid );
if status, error ( mexnc('strerror',status) ), end

input_data = single([3 6 9]);
status = mexnc ( 'put_att_float', ncid, nc_global, 'test_float9', nc_float, 3, input_data );
if status, error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_float', ncid, nc_global, 'test_float9' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );

if any ( double(input_data(:)) - double(output_data(:)) )
	error ( 'attribute values differ.\n');
	error ( err_msg );
end

fprintf ( 1, 'PUT_ATT_FLOAT succeeded.\n' );
fprintf ( 1, 'GET_ATT_FLOAT succeeded.\n' );
return






function test_writeReadDouble ( ncfile );

[ncid, status] = mexnc('OPEN', ncfile, nc_write_mode);
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'REDEF', ncid );
if status, error ( mexnc('strerror',status) ), end

input_data = [3 6 9];
status = mexnc ( 'put_att_double', ncid, nc_global, 'test_double2', nc_double, 3, input_data );
if status, error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_double', ncid, nc_global, 'test_double2' );
if status, error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if status, error ( mexnc('strerror',status) ), end

if any ( double(input_data(:)) - double(output_data(:)) )
	error ( 'attribute values differ.\n' );
end

fprintf ( 1, 'PUT_ATT_DOUBLE succeeded.\n' );
fprintf ( 1, 'GET_ATT_DOUBLE succeeded.\n' );
return







function test_writeAsFloatReadAsDouble ( ncfile )

create_test_file ( ncfile );

%
% Add the float attribute
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'redef', ncid  );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'put_att_float', ncid, nc_global, 'float_to_other', nc_float, 1, single(3.14) );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



%
% read back the attribute as double precision
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_double', ncid, nc_global, 'float_to_other' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ( ~strcmp(class(output_data),'double') )
	error ( 'attribute not converted to desired class' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return






function test_writeAsDoubleReadAsFloat ( ncfile )

create_test_file ( ncfile );

%
% Add the float attribute
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'redef', ncid  );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'put_att_double', ncid, nc_global, 'double_to_other', nc_double, 1, 3.14 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



%
% read back the attribute as double precision
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_float', ncid, nc_global, 'double_to_other' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ( ~strcmp(class(output_data),'single') )
	error ( 'attribute not converted to desired class' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return





function test_writeAsDoubleReadAsInt ( ncfile )

create_test_file ( ncfile );

%
% Add the float attribute
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'redef', ncid  );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'put_att_double', ncid, nc_global, 'double_to_other', nc_double, 1, 3.14 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



%
% read back the attribute as int32 
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_int', ncid, nc_global, 'double_to_other' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ( ~strcmp(class(output_data),'int32') )
	error ( 'attribute not converted to desired class' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return






function test_writeAsDoubleReadAsShort ( ncfile )

create_test_file ( ncfile );

%
% Add the float attribute
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'redef', ncid  );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'put_att_double', ncid, nc_global, 'double_to_other', nc_double, 1, 3.14 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



%
% read back the attribute as int16
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_short', ncid, nc_global, 'double_to_other' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ( ~strcmp(class(output_data),'int16') )
	error ( 'attribute not converted to desired class' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return






function test_writeAsDoubleReadAsUint8 ( ncfile )

create_test_file ( ncfile );

%
% Add the float attribute
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'redef', ncid  );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'put_att_double', ncid, nc_global, 'double_to_other', nc_double, 1, 3.14 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



%
% read back the attribute as int8 
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_schar', ncid, nc_global, 'double_to_other' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ( ~strcmp(class(output_data),'int8') )
	error ( 'attribute not converted to desired class' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return






function test_writeAsDoubleReadAsInt8 ( ncfile )

create_test_file ( ncfile );

%
% Add the float attribute
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'redef', ncid  );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'put_att_double', ncid, nc_global, 'double_to_other', nc_double, 1, 3.14 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



%
% read back the attribute as uint8 
[ncid, status] = mexnc ( 'open', ncfile, nc_nowrite_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'get_att_uchar', ncid, nc_global, 'double_to_other' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ( ~strcmp(class(output_data),'uint8') )
	error ( 'attribute not converted to desired class' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return






function create_test_file ( ncfile )


%
% ok, first create this baby.
mode = nc_clobber_mode;
[ncid, status] = mexnc ( 'create', ncfile, mode );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''create'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end





return





function c = load_text_into_cell ( file )


afid = fopen ( file, 'r' );
count = 0;
while 1
	line = fgetl ( afid );
	if ~ischar(line)
		break;
	end
	count = count + 1;
	c{count,1} = line;
end
fclose ( afid );
return















