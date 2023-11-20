function test_put_var_bad_datatype ( ncfile )
% TEST_PUT_VAR_BAD_DATATYPE:  
%
% Test 1:  PUT_VAR1_DOUBLE:  non numeric ncid
% Test 2:  PUT_VAR1_FLOAT:  non numeric ncid
% Test 3:  PUT_VAR1_INT:  non numeric ncid
% Test 4:  PUT_VAR1_SHORT:  non numeric ncid
% Test 5:  PUT_VAR1_SCHAR:  non numeric ncid
% Test 6:  PUT_VAR1_UCHAR:  non numeric ncid
% Test 7:  PUT_VAR1_TEXT:  non numeric ncid
% Test 8:  PUT_VAR1_DOUBLE:  non numeric varid
% Test 9:  PUT_VAR1_FLOAT:  non numeric varid
% Test 10:  PUT_VAR1_INT:  non numeric varid
% Test 11:  PUT_VAR1_SHORT:  non numeric varid
% Test 12:  PUT_VAR1_SCHAR:  non numeric varid
% Test 13:  PUT_VAR1_UCHAR:  non numeric varid
% Test 14:  PUT_VAR1_TEXT:  non numeric varid
% Test 15:  PUT_VAR1_DOUBLE:  non numeric start index
% Test 16:  PUT_VAR1_FLOAT:  non numeric start index
% Test 17:  PUT_VAR1_INT:  non numeric start index
% Test 18:  PUT_VAR1_SHORT:  non numeric start index
% Test 19:  PUT_VAR1_SCHAR:  non numeric start index
% Test 20:  PUT_VAR1_UCHAR:  non numeric start index
% Test 21:  PUT_VAR1_TEXT:  non numeric start index

% Test 22:  PUT_VARA_DOUBLE:  non numeric ncid
% Test 23:  PUT_VARA_FLOAT:  non numeric ncid
% Test 24:  PUT_VARA_INT:  non numeric ncid
% Test 25:  PUT_VARA_SHORT:  non numeric ncid
% Test 26:  PUT_VARA_SCHAR:  non numeric ncid
% Test 27:  PUT_VARA_UCHAR:  non numeric ncid
% Test 28:  PUT_VARA_TEXT:  non numeric ncid
% Test 29:  PUT_VARA_DOUBLE:  non numeric varid
% Test 30:  PUT_VARA_FLOAT:  non numeric varid
% Test 31:  PUT_VARA_INT:  non numeric varid
% Test 32:  PUT_VARA_SHORT:  non numeric varid
% Test 33:  PUT_VARA_SCHAR:  non numeric varid
% Test 34:  PUT_VARA_UCHAR:  non numeric varid
% Test 35:  PUT_VARA_TEXT:  non numeric varid
% Test 36:  PUT_VARA_DOUBLE:  non numeric start index
% Test 37:  PUT_VARA_FLOAT:  non numeric start index
% Test 38:  PUT_VARA_INT:  non numeric start index
% Test 39:  PUT_VARA_SHORT:  non numeric start index
% Test 40:  PUT_VARA_SCHAR:  non numeric start index
% Test 41:  PUT_VARA_UCHAR:  non numeric start index
% Test 42:  PUT_VARA_TEXT:  non numeric start index
% Test 43:  PUT_VARA_DOUBLE:  non numeric count index
% Test 44:  PUT_VARA_FLOAT:  non numeric count index
% Test 45:  PUT_VARA_INT:  non numeric count index
% Test 46:  PUT_VARA_SHORT:  non numeric count index
% Test 47:  PUT_VARA_SCHAR:  non numeric count index
% Test 48:  PUT_VARA_UCHAR:  non numeric count index
% Test 49:  PUT_VARA_TEXT:  non numeric count index

% Test 201:  PUT_VARS_DOUBLE:  non numeric ncid
% Test 202:  PUT_VARS_FLOAT:  non numeric ncid
% Test 203:  PUT_VARS_INT:  non numeric ncid
% Test 204:  PUT_VARS_SHORT:  non numeric ncid
% Test 205:  PUT_VARS_SCHAR:  non numeric ncid
% Test 206:  PUT_VARS_UCHAR:  non numeric ncid
% Test 207:  PUT_VARS_TEXT:  non numeric ncid
% Test 208:  PUT_VARS_DOUBLE:  non numeric varid
% Test 209:  PUT_VARS_FLOAT:  non numeric varid
% Test 210:  PUT_VARS_INT:  non numeric varid
% Test 211:  PUT_VARS_SHORT:  non numeric varid
% Test 212:  PUT_VARS_SCHAR:  non numeric varid
% Test 213:  PUT_VARS_UCHAR:  non numeric varid
% Test 214:  PUT_VARS_TEXT:  non numeric varid
% Test 215:  PUT_VARS_DOUBLE:  non numeric start index
% Test 216:  PUT_VARS_FLOAT:  non numeric start index
% Test 217:  PUT_VARS_INT:  non numeric start index
% Test 218:  PUT_VARS_SHORT:  non numeric start index
% Test 219:  PUT_VARS_SCHAR:  non numeric start index
% Test 220:  PUT_VARS_UCHAR:  non numeric start index
% Test 221:  PUT_VARS_TEXT:  non numeric start index
% Test 222:  PUT_VARS_DOUBLE:  non numeric count index
% Test 223:  PUT_VARS_FLOAT:  non numeric count index
% Test 224:  PUT_VARS_INT:  non numeric count index
% Test 225:  PUT_VARS_SHORT:  non numeric count index
% Test 226:  PUT_VARS_SCHAR:  non numeric count index
% Test 227:  PUT_VARS_UCHAR:  non numeric count index
% Test 228:  PUT_VARS_TEXT:  non numeric count index
% Test 229:  PUT_VARS_DOUBLE:  non numeric stride index
% Test 230:  PUT_VARS_FLOAT:  non numeric stride index
% Test 231:  PUT_VARS_INT:  non numeric stride index
% Test 232:  PUT_VARS_SHORT:  non numeric stride index
% Test 233:  PUT_VARS_SCHAR:  non numeric stride index
% Test 234:  PUT_VARS_UCHAR:  non numeric stride index
% Test 235:  PUT_VARS_TEXT:  non numeric stride index

% Test 301:  PUT_VARM_DOUBLE:  non numeric ncid
% Test 302:  PUT_VARM_FLOAT:  non numeric ncid
% Test 303:  PUT_VARM_INT:  non numeric ncid
% Test 304:  PUT_VARM_SHORT:  non numeric ncid
% Test 305:  PUT_VARM_SCHAR:  non numeric ncid
% Test 306:  PUT_VARM_UCHAR:  non numeric ncid
% Test 307:  PUT_VARM_TEXT:  non numeric ncid
% Test 308:  PUT_VARM_DOUBLE:  non numeric varid
% Test 309:  PUT_VARM_FLOAT:  non numeric varid
% Test 310:  PUT_VARM_INT:  non numeric varid
% Test 311:  PUT_VARM_SHORT:  non numeric varid
% Test 312:  PUT_VARM_SCHAR:  non numeric varid
% Test 313:  PUT_VARM_UCHAR:  non numeric varid
% Test 314:  PUT_VARM_TEXT:  non numeric varid
% Test 315:  PUT_VARM_DOUBLE:  non numeric start index
% Test 316:  PUT_VARM_FLOAT:  non numeric start index
% Test 317:  PUT_VARM_INT:  non numeric start index
% Test 318:  PUT_VARM_SHORT:  non numeric start index
% Test 319:  PUT_VARM_SCHAR:  non numeric start index
% Test 320:  PUT_VARM_UCHAR:  non numeric start index
% Test 321:  PUT_VARM_TEXT:  non numeric start index
% Test 322:  PUT_VARM_DOUBLE:  non numeric count index
% Test 323:  PUT_VARM_FLOAT:  non numeric count index
% Test 324:  PUT_VARM_INT:  non numeric count index
% Test 325:  PUT_VARM_SHORT:  non numeric count index
% Test 326:  PUT_VARM_SCHAR:  non numeric count index
% Test 327:  PUT_VARM_UCHAR:  non numeric count index
% Test 328:  PUT_VARM_TEXT:  non numeric count index
% Test 329:  PUT_VARM_DOUBLE:  non numeric stride index
% Test 330:  PUT_VARM_FLOAT:  non numeric stride index
% Test 331:  PUT_VARM_INT:  non numeric stride index
% Test 332:  PUT_VARM_SHORT:  non numeric stride index
% Test 333:  PUT_VARM_SCHAR:  non numeric stride index
% Test 334:  PUT_VARM_UCHAR:  non numeric stride index
% Test 335:  PUT_VARM_TEXT:  non numeric stride index
% Test 336:  PUT_VARM_DOUBLE:  non numeric imap index
% Test 337:  PUT_VARM_FLOAT:  non numeric imap index
% Test 338:  PUT_VARM_INT:  non numeric imap index
% Test 339:  PUT_VARM_SHORT:  non numeric imap index
% Test 340:  PUT_VARM_SCHAR:  non numeric imap index
% Test 341:  PUT_VARM_UCHAR:  non numeric imap index
% Test 342:  PUT_VARM_TEXT:  non numeric imap index

% Test 401:  PUT_VAR_DOUBLE:  non numeric ncid
% Test 402:  PUT_VAR_FLOAT:  non numeric ncid
% Test 403:  PUT_VAR_INT:  non numeric ncid
% Test 404:  PUT_VAR_SHORT:  non numeric ncid
% Test 405:  PUT_VAR_SCHAR:  non numeric ncid
% Test 406:  PUT_VAR_UCHAR:  non numeric ncid
% Test 407:  PUT_VAR_TEXT:  non numeric ncid
% Test 408:  PUT_VAR_DOUBLE:  non numeric varid
% Test 409:  PUT_VAR_FLOAT:  non numeric varid
% Test 410:  PUT_VAR_INT:  non numeric varid
% Test 411:  PUT_VAR_SHORT:  non numeric varid
% Test 412:  PUT_VAR_SCHAR:  non numeric varid
% Test 413:  PUT_VAR_UCHAR:  non numeric varid
% Test 414:  PUT_VAR_TEXT:  non numeric varid

error_condition = 0;

%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''create'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end



%
% Create the fixed dimension.  
len_x = 4;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim x, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


len_y = 6;
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', len_y );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim y, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[varid, status] = mexnc ( 'def_var', ncid, 'z_double', 'NC_DOUBLE', 2, [ydimid xdimid] );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_var'' failed on var x_short, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''enddef'' failed, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


input_data = [1:1:len_y*len_x];
input_data = reshape ( input_data, len_y, len_x );

status = mexnc ( 'put_var_double', ncid, varid, input_data' );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end




%
% Test 1:  PUT_VAR1_DOUBLE:  non numeric ncid
testid = 'Test 1';
try
	[status] = mexnc ( 'put_var1_double', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 2:  PUT_VAR1_FLOAT:  non numeric ncid
testid = 'Test 2';
try
	[status] = mexnc ( 'put_var1_float', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 3:  PUT_VAR1_INT:  non numeric ncid
testid = 'Test 3';
try
	[status] = mexnc ( 'put_var1_int', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 4:  PUT_VAR1_SHORT:  non numeric ncid
testid = 'Test 4';
try
	[status] = mexnc ( 'put_var1_short', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 5:  PUT_VAR1_SCHAR:  non numeric ncid
testid = 'Test 5';
try
	[status] = mexnc ( 'put_var1_schar', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 6:  PUT_VAR1_UCHAR:  non numeric ncid
testid = 'Test 6';
try
	[status] = mexnc ( 'put_var1_uchar', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 7:  PUT_VAR1_TEXT:  non numeric ncid
testid = 'Test 7';
try
	[status] = mexnc ( 'put_var1_text', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 8:  PUT_VAR1_DOUBLE:  non numeric varid
testid = 'Test 8';
try
	[status] = mexnc ( 'put_var1_double', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 9:  PUT_VAR1_FLOAT:  non numeric varid
testid = 'Test 9';
try
	[status] = mexnc ( 'put_var1_float', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 10:  PUT_VAR1_INT:  non numeric varid
testid = 'Test 10';
try
	[status] = mexnc ( 'put_var1_int', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 11:  PUT_VAR1_SHORT:  non numeric varid
testid = 'Test 11';
try
	[status] = mexnc ( 'put_var1_short', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 12:  PUT_VAR1_SCHAR:  non numeric varid
testid = 'Test 12';
try
	[status] = mexnc ( 'put_var1_schar', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 13:  PUT_VAR1_UCHAR:  non numeric varid
testid = 'Test 13';
try
	[status] = mexnc ( 'put_var1_uchar', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 14:  PUT_VAR1_TEXT:  non numeric varid
testid = 'Test 14';
try
	[status] = mexnc ( 'put_var1_text', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 15:  PUT_VAR1_DOUBLE:  non numeric start index
testid = 'Test 15';
try
	[status] = mexnc ( 'put_var1_double', ncid, varid, 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 16:  PUT_VAR1_FLOAT:  non numeric start index
testid = 'Test 16';
try
	[status] = mexnc ( 'put_var1_float', ncid, varid, 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 17:  PUT_VAR1_INT:  non numeric start index
testid = 'Test 17';
try
	[status] = mexnc ( 'put_var1_int', ncid, varid, 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 18:  PUT_VAR1_SHORT:  non numeric start index
testid = 'Test 18';
try
	[status] = mexnc ( 'put_var1_short', ncid, varid, 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 19:  PUT_VAR1_SCHAR:  non numeric start index
testid = 'Test 19';
try
	[status] = mexnc ( 'put_var1_schar', ncid, varid, 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 20:  PUT_VAR1_UCHAR:  non numeric start index
testid = 'Test 20';
try
	[status] = mexnc ( 'put_var1_uchar', ncid, varid, 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 21:  PUT_VAR1_TEXT:  non numeric start index
testid = 'Test 21';
try
	[status] = mexnc ( 'put_var1_text', ncid, varid, 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 22:  PUT_VARA_DOUBLE:  non numeric ncid
testid = 'Test 22';
try
	[status] = mexnc ( 'put_vara_double', 'ncid', varid, [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 23:  PUT_VARA_FLOAT:  non numeric ncid
testid = 'Test 23';
try
	[status] = mexnc ( 'put_vara_float', 'ncid', varid, [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 24:  PUT_VARA_INT:  non numeric ncid
testid = 'Test 24';
try
	[status] = mexnc ( 'put_vara_int', 'ncid', varid, [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 25:  PUT_VARA_SHORT:  non numeric ncid
testid = 'Test 25';
try
	[status] = mexnc ( 'put_vara_short', 'ncid', varid, [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 26:  PUT_VARA_SCHAR:  non numeric ncid
testid = 'Test 26';
try
	[status] = mexnc ( 'put_vara_schar', 'ncid', varid, [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 27:  PUT_VARA_UCHAR:  non numeric ncid
testid = 'Test 27';
try
	[status] = mexnc ( 'put_vara_uchar', 'ncid', varid, [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 28:  PUT_VARA_TEXT:  non numeric ncid
testid = 'Test 28';
try
	[status] = mexnc ( 'put_vara_text', 'ncid', varid, [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 29:  PUT_VARA_DOUBLE:  non numeric varid
testid = 'Test 29';
try
	[status] = mexnc ( 'put_vara_double', ncid, 'varid', [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 30:  PUT_VARA_FLOAT:  non numeric varid
testid = 'Test 30';
try
	[status] = mexnc ( 'put_vara_float', ncid, 'varid', [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 31:  PUT_VARA_INT:  non numeric varid
testid = 'Test 31';
try
	[status] = mexnc ( 'put_vara_int', ncid, 'varid', [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 32:  PUT_VARA_SHORT:  non numeric varid
testid = 'Test 32';
try
	[status] = mexnc ( 'put_vara_short', ncid, 'varid', [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 33:  PUT_VARA_SCHAR:  non numeric varid
testid = 'Test 33';
try
	[status] = mexnc ( 'put_vara_schar', ncid, 'varid', [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 34:  PUT_VARA_UCHAR:  non numeric varid
testid = 'Test 34';
try
	[status] = mexnc ( 'put_vara_uchar', ncid, 'varid', [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 35:  PUT_VARA_TEXT:  non numeric varid
testid = 'Test 35';
try
	[status] = mexnc ( 'put_vara_text', ncid, 'varid', [0 0], [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 36:  PUT_VARA_DOUBLE:  non numeric start index
testid = 'Test 36';
try
	[status] = mexnc ( 'put_vara_double', ncid, varid, 'blah', [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 37:  PUT_VARA_FLOAT:  non numeric start index
testid = 'Test 37';
try
	[status] = mexnc ( 'put_vara_float', ncid, varid, 'blah', [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 38:  PUT_VARA_INT:  non numeric start index
testid = 'Test 38';
try
	[status] = mexnc ( 'put_vara_int', ncid, varid, 'blah', [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 39:  PUT_VARA_SHORT:  non numeric start index
testid = 'Test 39';
try
	[status] = mexnc ( 'put_vara_short', ncid, varid, 'blah', [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 40:  PUT_VARA_SCHAR:  non numeric start index
testid = 'Test 40';
try
	[status] = mexnc ( 'put_vara_schar', ncid, varid, 'blah', [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 41:  PUT_VARA_UCHAR:  non numeric start index
testid = 'Test 41';
try
	[status] = mexnc ( 'put_vara_uchar', ncid, varid, 'blah', [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 42:  PUT_VARA_TEXT:  non numeric start index
testid = 'Test 42';
try
	[status] = mexnc ( 'put_vara_text', ncid, varid, 'blah', [4 5], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 43:  PUT_VARA_DOUBLE:  non numeric count index
testid = 'Test 43';
try
	[status] = mexnc ( 'put_vara_double', ncid, varid, [0 0], 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 44:  PUT_VARA_FLOAT:  non numeric count index
testid = 'Test 44';
try
	[status] = mexnc ( 'put_vara_float', ncid, varid, [0 0], 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 45:  PUT_VARA_INT:  non numeric count index
testid = 'Test 45';
try
	[status] = mexnc ( 'put_vara_int', ncid, varid, [0 0], 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 46:  PUT_VARA_SHORT:  non numeric count index
testid = 'Test 46';
try
	[status] = mexnc ( 'put_vara_short', ncid, varid, [0 0], 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 47:  PUT_VARA_SCHAR:  non numeric count index
testid = 'Test 47';
try
	[status] = mexnc ( 'put_vara_schar', ncid, varid, [0 0], 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 48:  PUT_VARA_UCHAR:  non numeric count index
testid = 'Test 48';
try
	[status] = mexnc ( 'put_vara_uchar', ncid, varid, [0 0], 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 49:  PUT_VARA_TEXT:  non numeric count index
testid = 'Test 49';
try
	[status] = mexnc ( 'put_vara_text', ncid, varid, [0 0], 'blah', input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 201:  PUT_VARS_DOUBLE:  non numeric ncid
testid = 'Test 201';
try
	[status] = mexnc ( 'put_vars_double', 'ncid', varid, [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 202:  PUT_VARS_FLOAT:  non numeric ncid
testid = 'Test 202';
try
	[status] = mexnc ( 'put_vars_float', 'ncid', varid, [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 203:  PUT_VARS_INT:  non numeric ncid
testid = 'Test 203';
try
	[status] = mexnc ( 'put_vars_int', 'ncid', varid, [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 204:  PUT_VARS_SHORT:  non numeric ncid
testid = 'Test 204';
try
	[status] = mexnc ( 'put_vars_short', 'ncid', varid, [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 205:  PUT_VARS_SCHAR:  non numeric ncid
testid = 'Test 205';
try
	[status] = mexnc ( 'put_vars_schar', 'ncid', varid, [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 206:  PUT_VARS_UCHAR:  non numeric ncid
testid = 'Test 206';
try
	[status] = mexnc ( 'put_vars_uchar', 'ncid', varid, [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 207:  PUT_VARS_TEXT:  non numeric ncid
testid = 'Test 207';
try
	[status] = mexnc ( 'put_vars_text', 'ncid', varid, [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 208:  PUT_VARS_DOUBLE:  non numeric varid
testid = 'Test 208';
try
	[status] = mexnc ( 'put_vars_double', ncid, 'varid', [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 209:  PUT_VARS_FLOAT:  non numeric varid
testid = 'Test 209';
try
	[status] = mexnc ( 'put_vars_float', ncid, 'varid', [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 210:  PUT_VARS_INT:  non numeric varid
testid = 'Test 210';
try
	[status] = mexnc ( 'put_vars_int', ncid, 'varid', [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 211:  PUT_VARS_SHORT:  non numeric varid
testid = 'Test 211';
try
	[status] = mexnc ( 'put_vars_short', ncid, 'varid', [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 212:  PUT_VARS_SCHAR:  non numeric varid
testid = 'Test 212';
try
	[status] = mexnc ( 'put_vars_schar', ncid, 'varid', [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 213:  PUT_VARS_UCHAR:  non numeric varid
testid = 'Test 213';
try
	[status] = mexnc ( 'put_vars_uchar', ncid, 'varid', [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 214:  PUT_VARS_TEXT:  non numeric varid
testid = 'Test 214';
try
	[status] = mexnc ( 'put_vars_text', ncid, 'varid', [0 0], [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 215:  PUT_VARS_DOUBLE:  non numeric start index
testid = 'Test 215';
try
	[status] = mexnc ( 'put_vars_double', ncid, varid, 'blah', [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 216:  PUT_VARS_FLOAT:  non numeric start index
testid = 'Test 216';
try
	[status] = mexnc ( 'put_vars_float', ncid, varid, 'blah', [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 217:  PUT_VARS_INT:  non numeric start index
testid = 'Test 217';
try
	[status] = mexnc ( 'put_vars_int', ncid, varid, 'blah', [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 218:  PUT_VARS_SHORT:  non numeric start index
testid = 'Test 218';
try
	[status] = mexnc ( 'put_vars_short', ncid, varid, 'blah', [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 219:  PUT_VARS_SCHAR:  non numeric start index
testid = 'Test 219';
try
	[status] = mexnc ( 'put_vars_schar', ncid, varid, 'blah', [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 220:  PUT_VARS_UCHAR:  non numeric start index
testid = 'Test 220';
try
	[status] = mexnc ( 'put_vars_uchar', ncid, varid, 'blah', [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 221:  PUT_VARS_TEXT:  non numeric start index
testid = 'Test 221';
try
	[status] = mexnc ( 'put_vars_text', ncid, varid, 'blah', [4 5], [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


%
% Test 222:  PUT_VARS_DOUBLE:  non numeric count index
testid = 'Test 222';
try
	[status] = mexnc ( 'put_vars_double', ncid, varid, [0 0], 'blah', [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 223:  PUT_VARS_FLOAT:  non numeric count index
testid = 'Test 223';
try
	[status] = mexnc ( 'put_vars_float', ncid, varid, [0 0], 'blah', [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 224:  PUT_VARS_INT:  non numeric count index
testid = 'Test 224';
try
	[status] = mexnc ( 'put_vars_int', ncid, varid, [0 0], 'blah', [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 225:  PUT_VARS_SHORT:  non numeric count index
testid = 'Test 225';
try
	[status] = mexnc ( 'put_vars_short', ncid, varid, [0 0], 'blah', [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 226:  PUT_VARS_SCHAR:  non numeric count index
testid = 'Test 226';
try
	[status] = mexnc ( 'put_vars_schar', ncid, varid, [0 0], 'blah', [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 227:  PUT_VARS_UCHAR:  non numeric count index
testid = 'Test 227';
try
	[status] = mexnc ( 'put_vars_uchar', ncid, varid, [0 0], 'blah', [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 228:  PUT_VARS_TEXT:  non numeric count index
testid = 'Test 228';
try
	[status] = mexnc ( 'put_vars_text', ncid, varid, [0 0], 'blah', [2 2], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 229:  PUT_VARS_DOUBLE:  non numeric stride argument
testid = 'Test 229';
try
	[status] = mexnc ( 'put_vars_double', ncid, varid, [0 0], [2 3], 'blah', input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 230:  PUT_VARS_FLOAT:  non numeric stride argument
testid = 'Test 230';
try
	[status] = mexnc ( 'put_vars_float', ncid, varid, [0 0], [2 3], 'blah', input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 231:  PUT_VARS_INT:  non numeric stride argument
testid = 'Test 231';
try
	[status] = mexnc ( 'put_vars_int', ncid, varid, [0 0], [2 3], 'blah', input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 232:  PUT_VARS_SHORT:  non numeric stride argument
testid = 'Test 232';
try
	[status] = mexnc ( 'put_vars_short', ncid, varid, [0 0], [2 3], 'blah', input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 233:  PUT_VARS_SCHAR:  non numeric stride argument
testid = 'Test 233';
try
	[status] = mexnc ( 'put_vars_schar', ncid, varid, [0 0], [2 3], 'blah', input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 234:  PUT_VARS_UCHAR:  non numeric stride argument
testid = 'Test 234';
try
	[status] = mexnc ( 'put_vars_uchar', ncid, varid, [0 0], [2 3], 'blah', input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 235:  PUT_VARS_TEXT:  non numeric stride argument
testid = 'Test 235';
try
	[status] = mexnc ( 'put_vars_text', ncid, varid, [0 0], [2 3], 'blah', input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end







%
% Test 301:  PUT_VARM_DOUBLE:  non numeric ncid
testid = 'Test 301';
try
	[status] = mexnc ( 'put_varm_double', 'ncid', varid, [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 302:  PUT_VARM_FLOAT:  non numeric ncid
testid = 'Test 302';
try
	[status] = mexnc ( 'put_varm_float', 'ncid', varid, [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 303:  PUT_VARM_INT:  non numeric ncid
testid = 'Test 303';
try
	[status] = mexnc ( 'put_varm_int', 'ncid', varid, [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 304:  PUT_VARM_SHORT:  non numeric ncid
testid = 'Test 304';
try
	[status] = mexnc ( 'put_varm_short', 'ncid', varid, [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 305:  PUT_VARM_SCHAR:  non numeric ncid
testid = 'Test 305';
try
	[status] = mexnc ( 'put_varm_schar', 'ncid', varid, [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 306:  PUT_VARM_UCHAR:  non numeric ncid
testid = 'Test 306';
try
	[status] = mexnc ( 'put_varm_uchar', 'ncid', varid, [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 307:  PUT_VARM_TEXT:  non numeric ncid
testid = 'Test 307';
try
	[status] = mexnc ( 'put_varm_text', 'ncid', varid, [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 308:  PUT_VARM_DOUBLE:  non numeric varid
testid = 'Test 308';
try
	[status] = mexnc ( 'put_varm_double', ncid, 'varid', [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 309:  PUT_VARM_FLOAT:  non numeric varid
testid = 'Test 309';
try
	[status] = mexnc ( 'put_varm_float', ncid, 'varid', [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 310:  PUT_VARM_INT:  non numeric varid
testid = 'Test 310';
try
	[status] = mexnc ( 'put_varm_int', ncid, 'varid', [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 311:  PUT_VARM_SHORT:  non numeric varid
testid = 'Test 311';
try
	[status] = mexnc ( 'put_varm_short', ncid, 'varid', [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 312:  PUT_VARM_SCHAR:  non numeric varid
testid = 'Test 312';
try
	[status] = mexnc ( 'put_varm_schar', ncid, 'varid', [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 313:  PUT_VARM_UCHAR:  non numeric varid
testid = 'Test 313';
try
	[status] = mexnc ( 'put_varm_uchar', ncid, 'varid', [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 314:  PUT_VARM_TEXT:  non numeric varid
testid = 'Test 314';
try
	[status] = mexnc ( 'put_varm_text', ncid, 'varid', [0 0], [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 315:  PUT_VARM_DOUBLE:  non numeric start index
testid = 'Test 315';
try
	[status] = mexnc ( 'put_varm_double', ncid, varid, 'blah', [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 316:  PUT_VARM_FLOAT:  non numeric start index
testid = 'Test 316';
try
	[status] = mexnc ( 'put_varm_float', ncid, varid, 'blah', [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 317:  PUT_VARM_INT:  non numeric start index
testid = 'Test 317';
try
	[status] = mexnc ( 'put_varm_int', ncid, varid, 'blah', [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 318:  PUT_VARM_SHORT:  non numeric start index
testid = 'Test 318';
try
	[status] = mexnc ( 'put_varm_short', ncid, varid, 'blah', [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 319:  PUT_VARM_SCHAR:  non numeric start index
testid = 'Test 319';
try
	[status] = mexnc ( 'put_varm_schar', ncid, varid, 'blah', [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 320:  PUT_VARM_UCHAR:  non numeric start index
testid = 'Test 320';
try
	[status] = mexnc ( 'put_varm_uchar', ncid, varid, 'blah', [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 321:  PUT_VARM_TEXT:  non numeric start index
testid = 'Test 321';
try
	[status] = mexnc ( 'put_varm_text', ncid, varid, 'blah', [6 4], [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


%
% Test 322:  PUT_VARM_DOUBLE:  non numeric count index
testid = 'Test 322';
try
	[status] = mexnc ( 'put_varm_double', ncid, varid, [0 0], 'blah', [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 323:  PUT_VARM_FLOAT:  non numeric count index
testid = 'Test 323';
try
	[status] = mexnc ( 'put_varm_float', ncid, varid, [0 0], 'blah', [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 324:  PUT_VARM_INT:  non numeric count index
testid = 'Test 324';
try
	[status] = mexnc ( 'put_varm_int', ncid, varid, [0 0], 'blah', [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 325:  PUT_VARM_SHORT:  non numeric count index
testid = 'Test 325';
try
	[status] = mexnc ( 'put_varm_short', ncid, varid, [0 0], 'blah', [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 326:  PUT_VARM_SCHAR:  non numeric count index
testid = 'Test 326';
try
	[status] = mexnc ( 'put_varm_schar', ncid, varid, [0 0], 'blah', [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 327:  PUT_VARM_UCHAR:  non numeric count index
testid = 'Test 327';
try
	[status] = mexnc ( 'put_varm_uchar', ncid, varid, [0 0], 'blah', [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 328:  PUT_VARM_TEXT:  non numeric count index
testid = 'Test 328';
try
	[status] = mexnc ( 'put_varm_text', ncid, varid, [0 0], 'blah', [1 1], [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 329:  PUT_VARM_DOUBLE:  non numeric stride argument
testid = 'Test 329';
try
	[status] = mexnc ( 'put_varm_double', ncid, varid, [0 0], [2 3], 'blah' , [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 330:  PUT_VARM_FLOAT:  non numeric stride argument
testid = 'Test 330';
try
	[status] = mexnc ( 'put_varm_float', ncid, varid, [0 0], [2 3], 'blah', [1 6], input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 331:  PUT_VARM_INT:  non numeric stride argument
testid = 'Test 331';
try
	[status] = mexnc ( 'put_varm_int', ncid, varid, [0 0], [2 3], 'blah', [1 6], input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 332:  PUT_VARM_SHORT:  non numeric stride argument
testid = 'Test 332';
try
	[status] = mexnc ( 'put_varm_short', ncid, varid, [0 0], [2 3], 'blah', [1 6], input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 333:  PUT_VARM_SCHAR:  non numeric stride argument
testid = 'Test 333';
try
	[status] = mexnc ( 'put_varm_schar', ncid, varid, [0 0], [2 3], 'blah', [1 6] , input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 334:  PUT_VARM_UCHAR:  non numeric stride argument
testid = 'Test 334';
try
	[status] = mexnc ( 'put_varm_uchar', ncid, varid, [0 0], [2 3], 'blah', [1 6], input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 335:  PUT_VARM_TEXT:  non numeric stride argument
testid = 'Test 335';
try
	[status] = mexnc ( 'put_varm_text', ncid, varid, [0 0], [2 3], 'blah', [1 6], input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 336:  PUT_VARM_DOUBLE:  non numeric imap argument
testid = 'Test 336';
try
	[status] = mexnc ( 'put_varm_double', ncid, varid, [0 0], [2 3], 'blah' , [1 6], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 337:  PUT_VARM_FLOAT:  non numeric imap argument
testid = 'Test 337';
try
	[status] = mexnc ( 'put_varm_float', ncid, varid, [0 0], [2 3], [1 1], 'blah', input_data'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 338:  PUT_VARM_INT:  non numeric imap argument
testid = 'Test 338';
try
	[status] = mexnc ( 'put_varm_int', ncid, varid, [0 0], [2 3], [1 1], 'blah', input_data'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 339:  PUT_VARM_SHORT:  non numeric imap argument
testid = 'Test 339';
try
	[status] = mexnc ( 'put_varm_short', ncid, varid, [0 0], [2 3], [1 1], 'blah' , input_data'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 340:  PUT_VARM_SCHAR:  non numeric imap argument
testid = 'Test 340';
try
	[status] = mexnc ( 'put_varm_schar', ncid, varid, [0 0], [2 3], [1 1], 'blah', input_data'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 341:  PUT_VARM_UCHAR:  non numeric imap argument
testid = 'Test 341';
try
	[status] = mexnc ( 'put_varm_uchar', ncid, varid, [0 0], [2 3], [1 1], 'blah', input_data'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 342:  PUT_VARM_TEXT:  non numeric imap argument
testid = 'Test 342';
try
	[status] = mexnc ( 'put_varm_text', ncid, varid, [0 0], [2 3], [1 1], 'blah', input_data'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 401:  PUT_VAR_DOUBLE:  non numeric ncid
testid = 'Test 401';
try
	[status] = mexnc ( 'put_var_double', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 402:  PUT_VAR_FLOAT:  non numeric ncid
testid = 'Test 402';
try
	[status] = mexnc ( 'put_var_float', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 403:  PUT_VAR_INT:  non numeric ncid
testid = 'Test 403';
try
	[status] = mexnc ( 'put_var_int', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 404:  PUT_VAR_SHORT:  non numeric ncid
testid = 'Test 404';
try
	[status] = mexnc ( 'put_var_short', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 405:  PUT_VAR_SCHAR:  non numeric ncid
testid = 'Test 405';
try
	[status] = mexnc ( 'put_var_schar', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 406:  PUT_VAR_UCHAR:  non numeric ncid
testid = 'Test 406';
try
	[status] = mexnc ( 'put_var_uchar', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 407:  PUT_VAR_TEXT:  non numeric ncid
testid = 'Test 407';
try
	[status] = mexnc ( 'put_var_text', 'ncid', varid, [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 408:  PUT_VAR_DOUBLE:  non numeric varid
testid = 'Test 408';
try
	[status] = mexnc ( 'put_var_double', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 409:  PUT_VAR_FLOAT:  non numeric varid
testid = 'Test 409';
try
	[status] = mexnc ( 'put_var_float', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 410:  PUT_VAR_INT:  non numeric varid
testid = 'Test 410';
try
	[status] = mexnc ( 'put_var_int', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 411:  PUT_VAR_SHORT:  non numeric varid
testid = 'Test 411';
try
	[status] = mexnc ( 'put_var_short', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 412:  PUT_VAR_SCHAR:  non numeric varid
testid = 'Test 412';
try
	[status] = mexnc ( 'put_var_schar', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 413:  PUT_VAR_UCHAR:  non numeric varid
testid = 'Test 413';
try
	[status] = mexnc ( 'put_var_uchar', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 414:  PUT_VAR_TEXT:  non numeric varid
testid = 'Test 414';
try
	[status] = mexnc ( 'put_var_text', ncid, 'varid', [0 0], input_data' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


fprintf ( 1, 'PUT_VAR_BAD_PARAM_DATATYPE succeeded.\n' );


return
















