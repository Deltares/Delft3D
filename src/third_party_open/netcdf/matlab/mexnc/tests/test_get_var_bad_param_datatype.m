function test_get_var_bad_datatype ( ncfile )
% TEST_GET_VAR_BAD_DATATYPE:  
%
% Test 1:  GET_VAR1_DOUBLE:  non numeric ncid
% Test 2:  GET_VAR1_FLOAT:  non numeric ncid
% Test 3:  GET_VAR1_INT:  non numeric ncid
% Test 4:  GET_VAR1_SHORT:  non numeric ncid
% Test 5:  GET_VAR1_SCHAR:  non numeric ncid
% Test 6:  GET_VAR1_UCHAR:  non numeric ncid
% Test 7:  GET_VAR1_TEXT:  non numeric ncid
% Test 8:  GET_VAR1_DOUBLE:  non numeric varid
% Test 9:  GET_VAR1_FLOAT:  non numeric varid
% Test 10:  GET_VAR1_INT:  non numeric varid
% Test 11:  GET_VAR1_SHORT:  non numeric varid
% Test 12:  GET_VAR1_SCHAR:  non numeric varid
% Test 13:  GET_VAR1_UCHAR:  non numeric varid
% Test 14:  GET_VAR1_TEXT:  non numeric varid
% Test 15:  GET_VAR1_DOUBLE:  non numeric start index
% Test 16:  GET_VAR1_FLOAT:  non numeric start index
% Test 17:  GET_VAR1_INT:  non numeric start index
% Test 18:  GET_VAR1_SHORT:  non numeric start index
% Test 19:  GET_VAR1_SCHAR:  non numeric start index
% Test 20:  GET_VAR1_UCHAR:  non numeric start index
% Test 21:  GET_VAR1_TEXT:  non numeric start index

% Test 22:  GET_VARA_DOUBLE:  non numeric ncid
% Test 23:  GET_VARA_FLOAT:  non numeric ncid
% Test 24:  GET_VARA_INT:  non numeric ncid
% Test 25:  GET_VARA_SHORT:  non numeric ncid
% Test 26:  GET_VARA_SCHAR:  non numeric ncid
% Test 27:  GET_VARA_UCHAR:  non numeric ncid
% Test 28:  GET_VARA_TEXT:  non numeric ncid
% Test 29:  GET_VARA_DOUBLE:  non numeric varid
% Test 30:  GET_VARA_FLOAT:  non numeric varid
% Test 31:  GET_VARA_INT:  non numeric varid
% Test 32:  GET_VARA_SHORT:  non numeric varid
% Test 33:  GET_VARA_SCHAR:  non numeric varid
% Test 34:  GET_VARA_UCHAR:  non numeric varid
% Test 35:  GET_VARA_TEXT:  non numeric varid
% Test 36:  GET_VARA_DOUBLE:  non numeric start index
% Test 37:  GET_VARA_FLOAT:  non numeric start index
% Test 38:  GET_VARA_INT:  non numeric start index
% Test 39:  GET_VARA_SHORT:  non numeric start index
% Test 40:  GET_VARA_SCHAR:  non numeric start index
% Test 41:  GET_VARA_UCHAR:  non numeric start index
% Test 42:  GET_VARA_TEXT:  non numeric start index
% Test 43:  GET_VARA_DOUBLE:  non numeric count index
% Test 44:  GET_VARA_FLOAT:  non numeric count index
% Test 45:  GET_VARA_INT:  non numeric count index
% Test 46:  GET_VARA_SHORT:  non numeric count index
% Test 47:  GET_VARA_SCHAR:  non numeric count index
% Test 48:  GET_VARA_UCHAR:  non numeric count index
% Test 49:  GET_VARA_TEXT:  non numeric count index

% Test 201:  GET_VARS_DOUBLE:  non numeric ncid
% Test 202:  GET_VARS_FLOAT:  non numeric ncid
% Test 203:  GET_VARS_INT:  non numeric ncid
% Test 204:  GET_VARS_SHORT:  non numeric ncid
% Test 205:  GET_VARS_SCHAR:  non numeric ncid
% Test 206:  GET_VARS_UCHAR:  non numeric ncid
% Test 207:  GET_VARS_TEXT:  non numeric ncid
% Test 208:  GET_VARS_DOUBLE:  non numeric varid
% Test 209:  GET_VARS_FLOAT:  non numeric varid
% Test 210:  GET_VARS_INT:  non numeric varid
% Test 211:  GET_VARS_SHORT:  non numeric varid
% Test 212:  GET_VARS_SCHAR:  non numeric varid
% Test 213:  GET_VARS_UCHAR:  non numeric varid
% Test 214:  GET_VARS_TEXT:  non numeric varid
% Test 215:  GET_VARS_DOUBLE:  non numeric start index
% Test 216:  GET_VARS_FLOAT:  non numeric start index
% Test 217:  GET_VARS_INT:  non numeric start index
% Test 218:  GET_VARS_SHORT:  non numeric start index
% Test 219:  GET_VARS_SCHAR:  non numeric start index
% Test 220:  GET_VARS_UCHAR:  non numeric start index
% Test 221:  GET_VARS_TEXT:  non numeric start index
% Test 222:  GET_VARS_DOUBLE:  non numeric count index
% Test 223:  GET_VARS_FLOAT:  non numeric count index
% Test 224:  GET_VARS_INT:  non numeric count index
% Test 225:  GET_VARS_SHORT:  non numeric count index
% Test 226:  GET_VARS_SCHAR:  non numeric count index
% Test 227:  GET_VARS_UCHAR:  non numeric count index
% Test 228:  GET_VARS_TEXT:  non numeric count index
% Test 229:  GET_VARS_DOUBLE:  non numeric stride index
% Test 230:  GET_VARS_FLOAT:  non numeric stride index
% Test 231:  GET_VARS_INT:  non numeric stride index
% Test 232:  GET_VARS_SHORT:  non numeric stride index
% Test 233:  GET_VARS_SCHAR:  non numeric stride index
% Test 234:  GET_VARS_UCHAR:  non numeric stride index
% Test 235:  GET_VARS_TEXT:  non numeric stride index

% Test 301:  GET_VARM_DOUBLE:  non numeric ncid
% Test 302:  GET_VARM_FLOAT:  non numeric ncid
% Test 303:  GET_VARM_INT:  non numeric ncid
% Test 304:  GET_VARM_SHORT:  non numeric ncid
% Test 305:  GET_VARM_SCHAR:  non numeric ncid
% Test 306:  GET_VARM_UCHAR:  non numeric ncid
% Test 307:  GET_VARM_TEXT:  non numeric ncid
% Test 308:  GET_VARM_DOUBLE:  non numeric varid
% Test 309:  GET_VARM_FLOAT:  non numeric varid
% Test 310:  GET_VARM_INT:  non numeric varid
% Test 311:  GET_VARM_SHORT:  non numeric varid
% Test 312:  GET_VARM_SCHAR:  non numeric varid
% Test 313:  GET_VARM_UCHAR:  non numeric varid
% Test 314:  GET_VARM_TEXT:  non numeric varid
% Test 315:  GET_VARM_DOUBLE:  non numeric start index
% Test 316:  GET_VARM_FLOAT:  non numeric start index
% Test 317:  GET_VARM_INT:  non numeric start index
% Test 318:  GET_VARM_SHORT:  non numeric start index
% Test 319:  GET_VARM_SCHAR:  non numeric start index
% Test 320:  GET_VARM_UCHAR:  non numeric start index
% Test 321:  GET_VARM_TEXT:  non numeric start index
% Test 322:  GET_VARM_DOUBLE:  non numeric count index
% Test 323:  GET_VARM_FLOAT:  non numeric count index
% Test 324:  GET_VARM_INT:  non numeric count index
% Test 325:  GET_VARM_SHORT:  non numeric count index
% Test 326:  GET_VARM_SCHAR:  non numeric count index
% Test 327:  GET_VARM_UCHAR:  non numeric count index
% Test 328:  GET_VARM_TEXT:  non numeric count index
% Test 329:  GET_VARM_DOUBLE:  non numeric stride index
% Test 330:  GET_VARM_FLOAT:  non numeric stride index
% Test 331:  GET_VARM_INT:  non numeric stride index
% Test 332:  GET_VARM_SHORT:  non numeric stride index
% Test 333:  GET_VARM_SCHAR:  non numeric stride index
% Test 334:  GET_VARM_UCHAR:  non numeric stride index
% Test 335:  GET_VARM_TEXT:  non numeric stride index
% Test 336:  GET_VARM_DOUBLE:  non numeric imap index
% Test 337:  GET_VARM_FLOAT:  non numeric imap index
% Test 338:  GET_VARM_INT:  non numeric imap index
% Test 339:  GET_VARM_SHORT:  non numeric imap index
% Test 340:  GET_VARM_SCHAR:  non numeric imap index
% Test 341:  GET_VARM_UCHAR:  non numeric imap index
% Test 342:  GET_VARM_TEXT:  non numeric imap index

% Test 401:  GET_VAR_DOUBLE:  non numeric ncid
% Test 402:  GET_VAR_FLOAT:  non numeric ncid
% Test 403:  GET_VAR_INT:  non numeric ncid
% Test 404:  GET_VAR_SHORT:  non numeric ncid
% Test 405:  GET_VAR_SCHAR:  non numeric ncid
% Test 406:  GET_VAR_UCHAR:  non numeric ncid
% Test 407:  GET_VAR_TEXT:  non numeric ncid
% Test 408:  GET_VAR_DOUBLE:  non numeric varid
% Test 409:  GET_VAR_FLOAT:  non numeric varid
% Test 410:  GET_VAR_INT:  non numeric varid
% Test 411:  GET_VAR_SHORT:  non numeric varid
% Test 412:  GET_VAR_SCHAR:  non numeric varid
% Test 413:  GET_VAR_UCHAR:  non numeric varid
% Test 414:  GET_VAR_TEXT:  non numeric varid

if ( nargin < 1 )
	ncfile = 'foo.nc';
end

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
% Test 1:  GET_VAR1_DOUBLE:  non numeric ncid
testid = 'Test 1';
try
	[vardata, status] = mexnc ( 'get_var1_double', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 2:  GET_VAR1_FLOAT:  non numeric ncid
testid = 'Test 2';
try
	[vardata, status] = mexnc ( 'get_var1_float', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 3:  GET_VAR1_INT:  non numeric ncid
testid = 'Test 3';
try
	[vardata, status] = mexnc ( 'get_var1_int', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 4:  GET_VAR1_SHORT:  non numeric ncid
testid = 'Test 4';
try
	[vardata, status] = mexnc ( 'get_var1_short', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 5:  GET_VAR1_SCHAR:  non numeric ncid
testid = 'Test 5';
try
	[vardata, status] = mexnc ( 'get_var1_schar', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 6:  GET_VAR1_UCHAR:  non numeric ncid
testid = 'Test 6';
try
	[vardata, status] = mexnc ( 'get_var1_uchar', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 7:  GET_VAR1_TEXT:  non numeric ncid
testid = 'Test 7';
try
	[vardata, status] = mexnc ( 'get_var1_text', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 8:  GET_VAR1_DOUBLE:  non numeric varid
testid = 'Test 8';
try
	[vardata, status] = mexnc ( 'get_var1_double', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 9:  GET_VAR1_FLOAT:  non numeric varid
testid = 'Test 9';
try
	[vardata, status] = mexnc ( 'get_var1_float', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 10:  GET_VAR1_INT:  non numeric varid
testid = 'Test 10';
try
	[vardata, status] = mexnc ( 'get_var1_int', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 11:  GET_VAR1_SHORT:  non numeric varid
testid = 'Test 11';
try
	[vardata, status] = mexnc ( 'get_var1_short', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 12:  GET_VAR1_SCHAR:  non numeric varid
testid = 'Test 12';
try
	[vardata, status] = mexnc ( 'get_var1_schar', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 13:  GET_VAR1_UCHAR:  non numeric varid
testid = 'Test 13';
try
	[vardata, status] = mexnc ( 'get_var1_uchar', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 14:  GET_VAR1_TEXT:  non numeric varid
testid = 'Test 14';
try
	[vardata, status] = mexnc ( 'get_var1_text', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 15:  GET_VAR1_DOUBLE:  non numeric start index
testid = 'Test 15';
try
	[vardata, status] = mexnc ( 'get_var1_double', ncid, varid, 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 16:  GET_VAR1_FLOAT:  non numeric start index
testid = 'Test 16';
try
	[vardata, status] = mexnc ( 'get_var1_float', ncid, varid, 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 17:  GET_VAR1_INT:  non numeric start index
testid = 'Test 17';
try
	[vardata, status] = mexnc ( 'get_var1_int', ncid, varid, 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 18:  GET_VAR1_SHORT:  non numeric start index
testid = 'Test 18';
try
	[vardata, status] = mexnc ( 'get_var1_short', ncid, varid, 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 19:  GET_VAR1_SCHAR:  non numeric start index
testid = 'Test 19';
try
	[vardata, status] = mexnc ( 'get_var1_schar', ncid, varid, 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 20:  GET_VAR1_UCHAR:  non numeric start index
testid = 'Test 20';
try
	[vardata, status] = mexnc ( 'get_var1_uchar', ncid, varid, 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 21:  GET_VAR1_TEXT:  non numeric start index
testid = 'Test 21';
try
	[vardata, status] = mexnc ( 'get_var1_text', ncid, varid, 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 22:  GET_VARA_DOUBLE:  non numeric ncid
testid = 'Test 22';
try
	[vardata, status] = mexnc ( 'get_vara_double', 'ncid', varid, [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 23:  GET_VARA_FLOAT:  non numeric ncid
testid = 'Test 23';
try
	[vardata, status] = mexnc ( 'get_vara_float', 'ncid', varid, [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 24:  GET_VARA_INT:  non numeric ncid
testid = 'Test 24';
try
	[vardata, status] = mexnc ( 'get_vara_int', 'ncid', varid, [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 25:  GET_VARA_SHORT:  non numeric ncid
testid = 'Test 25';
try
	[vardata, status] = mexnc ( 'get_vara_short', 'ncid', varid, [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 26:  GET_VARA_SCHAR:  non numeric ncid
testid = 'Test 26';
try
	[vardata, status] = mexnc ( 'get_vara_schar', 'ncid', varid, [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 27:  GET_VARA_UCHAR:  non numeric ncid
testid = 'Test 27';
try
	[vardata, status] = mexnc ( 'get_vara_uchar', 'ncid', varid, [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 28:  GET_VARA_TEXT:  non numeric ncid
testid = 'Test 28';
try
	[vardata, status] = mexnc ( 'get_vara_text', 'ncid', varid, [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 29:  GET_VARA_DOUBLE:  non numeric varid
testid = 'Test 29';
try
	[vardata, status] = mexnc ( 'get_vara_double', ncid, 'varid', [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 30:  GET_VARA_FLOAT:  non numeric varid
testid = 'Test 30';
try
	[vardata, status] = mexnc ( 'get_vara_float', ncid, 'varid', [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 31:  GET_VARA_INT:  non numeric varid
testid = 'Test 31';
try
	[vardata, status] = mexnc ( 'get_vara_int', ncid, 'varid', [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 32:  GET_VARA_SHORT:  non numeric varid
testid = 'Test 32';
try
	[vardata, status] = mexnc ( 'get_vara_short', ncid, 'varid', [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 33:  GET_VARA_SCHAR:  non numeric varid
testid = 'Test 33';
try
	[vardata, status] = mexnc ( 'get_vara_schar', ncid, 'varid', [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 34:  GET_VARA_UCHAR:  non numeric varid
testid = 'Test 34';
try
	[vardata, status] = mexnc ( 'get_vara_uchar', ncid, 'varid', [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 35:  GET_VARA_TEXT:  non numeric varid
testid = 'Test 35';
try
	[vardata, status] = mexnc ( 'get_vara_text', ncid, 'varid', [0 0], [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 36:  GET_VARA_DOUBLE:  non numeric start index
testid = 'Test 36';
try
	[vardata, status] = mexnc ( 'get_vara_double', ncid, varid, 'blah', [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 37:  GET_VARA_FLOAT:  non numeric start index
testid = 'Test 37';
try
	[vardata, status] = mexnc ( 'get_vara_float', ncid, varid, 'blah', [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 38:  GET_VARA_INT:  non numeric start index
testid = 'Test 38';
try
	[vardata, status] = mexnc ( 'get_vara_int', ncid, varid, 'blah', [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 39:  GET_VARA_SHORT:  non numeric start index
testid = 'Test 39';
try
	[vardata, status] = mexnc ( 'get_vara_short', ncid, varid, 'blah', [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 40:  GET_VARA_SCHAR:  non numeric start index
testid = 'Test 40';
try
	[vardata, status] = mexnc ( 'get_vara_schar', ncid, varid, 'blah', [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 41:  GET_VARA_UCHAR:  non numeric start index
testid = 'Test 41';
try
	[vardata, status] = mexnc ( 'get_vara_uchar', ncid, varid, 'blah', [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 42:  GET_VARA_TEXT:  non numeric start index
testid = 'Test 42';
try
	[vardata, status] = mexnc ( 'get_vara_text', ncid, varid, 'blah', [4 5] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 43:  GET_VARA_DOUBLE:  non numeric count index
testid = 'Test 43';
try
	[vardata, status] = mexnc ( 'get_vara_double', ncid, varid, [0 0], 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 44:  GET_VARA_FLOAT:  non numeric count index
testid = 'Test 44';
try
	[vardata, status] = mexnc ( 'get_vara_float', ncid, varid, [0 0], 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 45:  GET_VARA_INT:  non numeric count index
testid = 'Test 45';
try
	[vardata, status] = mexnc ( 'get_vara_int', ncid, varid, [0 0], 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 46:  GET_VARA_SHORT:  non numeric count index
testid = 'Test 46';
try
	[vardata, status] = mexnc ( 'get_vara_short', ncid, varid, [0 0], 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 47:  GET_VARA_SCHAR:  non numeric count index
testid = 'Test 47';
try
	[vardata, status] = mexnc ( 'get_vara_schar', ncid, varid, [0 0], 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 48:  GET_VARA_UCHAR:  non numeric count index
testid = 'Test 48';
try
	[vardata, status] = mexnc ( 'get_vara_uchar', ncid, varid, [0 0], 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 49:  GET_VARA_TEXT:  non numeric count index
testid = 'Test 49';
try
	[vardata, status] = mexnc ( 'get_vara_text', ncid, varid, [0 0], 'blah' );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 201:  GET_VARS_DOUBLE:  non numeric ncid
testid = 'Test 201';
try
	[vardata, status] = mexnc ( 'get_vars_double', 'ncid', varid, [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 202:  GET_VARS_FLOAT:  non numeric ncid
testid = 'Test 202';
try
	[vardata, status] = mexnc ( 'get_vars_float', 'ncid', varid, [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 203:  GET_VARS_INT:  non numeric ncid
testid = 'Test 203';
try
	[vardata, status] = mexnc ( 'get_vars_int', 'ncid', varid, [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 204:  GET_VARS_SHORT:  non numeric ncid
testid = 'Test 204';
try
	[vardata, status] = mexnc ( 'get_vars_short', 'ncid', varid, [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 205:  GET_VARS_SCHAR:  non numeric ncid
testid = 'Test 205';
try
	[vardata, status] = mexnc ( 'get_vars_schar', 'ncid', varid, [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 206:  GET_VARS_UCHAR:  non numeric ncid
testid = 'Test 206';
try
	[vardata, status] = mexnc ( 'get_vars_uchar', 'ncid', varid, [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 207:  GET_VARS_TEXT:  non numeric ncid
testid = 'Test 207';
try
	[vardata, status] = mexnc ( 'get_vars_text', 'ncid', varid, [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 208:  GET_VARS_DOUBLE:  non numeric varid
testid = 'Test 208';
try
	[vardata, status] = mexnc ( 'get_vars_double', ncid, 'varid', [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 209:  GET_VARS_FLOAT:  non numeric varid
testid = 'Test 209';
try
	[vardata, status] = mexnc ( 'get_vars_float', ncid, 'varid', [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 210:  GET_VARS_INT:  non numeric varid
testid = 'Test 210';
try
	[vardata, status] = mexnc ( 'get_vars_int', ncid, 'varid', [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 211:  GET_VARS_SHORT:  non numeric varid
testid = 'Test 211';
try
	[vardata, status] = mexnc ( 'get_vars_short', ncid, 'varid', [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 212:  GET_VARS_SCHAR:  non numeric varid
testid = 'Test 212';
try
	[vardata, status] = mexnc ( 'get_vars_schar', ncid, 'varid', [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 213:  GET_VARS_UCHAR:  non numeric varid
testid = 'Test 213';
try
	[vardata, status] = mexnc ( 'get_vars_uchar', ncid, 'varid', [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 214:  GET_VARS_TEXT:  non numeric varid
testid = 'Test 214';
try
	[vardata, status] = mexnc ( 'get_vars_text', ncid, 'varid', [0 0], [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 215:  GET_VARS_DOUBLE:  non numeric start index
testid = 'Test 215';
try
	[vardata, status] = mexnc ( 'get_vars_double', ncid, varid, 'blah', [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 216:  GET_VARS_FLOAT:  non numeric start index
testid = 'Test 216';
try
	[vardata, status] = mexnc ( 'get_vars_float', ncid, varid, 'blah', [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 217:  GET_VARS_INT:  non numeric start index
testid = 'Test 217';
try
	[vardata, status] = mexnc ( 'get_vars_int', ncid, varid, 'blah', [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 218:  GET_VARS_SHORT:  non numeric start index
testid = 'Test 218';
try
	[vardata, status] = mexnc ( 'get_vars_short', ncid, varid, 'blah', [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 219:  GET_VARS_SCHAR:  non numeric start index
testid = 'Test 219';
try
	[vardata, status] = mexnc ( 'get_vars_schar', ncid, varid, 'blah', [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 220:  GET_VARS_UCHAR:  non numeric start index
testid = 'Test 220';
try
	[vardata, status] = mexnc ( 'get_vars_uchar', ncid, varid, 'blah', [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 221:  GET_VARS_TEXT:  non numeric start index
testid = 'Test 221';
try
	[vardata, status] = mexnc ( 'get_vars_text', ncid, varid, 'blah', [4 5], [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


%
% Test 222:  GET_VARS_DOUBLE:  non numeric count index
testid = 'Test 222';
try
	[vardata, status] = mexnc ( 'get_vars_double', ncid, varid, [0 0], 'blah', [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 223:  GET_VARS_FLOAT:  non numeric count index
testid = 'Test 223';
try
	[vardata, status] = mexnc ( 'get_vars_float', ncid, varid, [0 0], 'blah', [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 224:  GET_VARS_INT:  non numeric count index
testid = 'Test 224';
try
	[vardata, status] = mexnc ( 'get_vars_int', ncid, varid, [0 0], 'blah', [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 225:  GET_VARS_SHORT:  non numeric count index
testid = 'Test 225';
try
	[vardata, status] = mexnc ( 'get_vars_short', ncid, varid, [0 0], 'blah', [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 226:  GET_VARS_SCHAR:  non numeric count index
testid = 'Test 226';
try
	[vardata, status] = mexnc ( 'get_vars_schar', ncid, varid, [0 0], 'blah', [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 227:  GET_VARS_UCHAR:  non numeric count index
testid = 'Test 227';
try
	[vardata, status] = mexnc ( 'get_vars_uchar', ncid, varid, [0 0], 'blah', [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 228:  GET_VARS_TEXT:  non numeric count index
testid = 'Test 228';
try
	[vardata, status] = mexnc ( 'get_vars_text', ncid, varid, [0 0], 'blah', [2 2] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 229:  GET_VARS_DOUBLE:  non numeric stride argument
testid = 'Test 229';
try
	[vardata, status] = mexnc ( 'get_vars_double', ncid, varid, [0 0], [2 3], 'blah'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 230:  GET_VARS_FLOAT:  non numeric stride argument
testid = 'Test 230';
try
	[vardata, status] = mexnc ( 'get_vars_float', ncid, varid, [0 0], [2 3], 'blah'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 231:  GET_VARS_INT:  non numeric stride argument
testid = 'Test 231';
try
	[vardata, status] = mexnc ( 'get_vars_int', ncid, varid, [0 0], [2 3], 'blah'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 232:  GET_VARS_SHORT:  non numeric stride argument
testid = 'Test 232';
try
	[vardata, status] = mexnc ( 'get_vars_short', ncid, varid, [0 0], [2 3], 'blah'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 233:  GET_VARS_SCHAR:  non numeric stride argument
testid = 'Test 233';
try
	[vardata, status] = mexnc ( 'get_vars_schar', ncid, varid, [0 0], [2 3], 'blah'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 234:  GET_VARS_UCHAR:  non numeric stride argument
testid = 'Test 234';
try
	[vardata, status] = mexnc ( 'get_vars_uchar', ncid, varid, [0 0], [2 3], 'blah'  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 235:  GET_VARS_TEXT:  non numeric stride argument
testid = 'Test 235';
try
	[vardata, status] = mexnc ( 'get_vars_text', ncid, varid, [0 0], [2 3], 'blah'  );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end







%
% Test 301:  GET_VARM_DOUBLE:  non numeric ncid
testid = 'Test 301';
try
	[vardata, status] = mexnc ( 'get_varm_double', 'ncid', varid, [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 302:  GET_VARM_FLOAT:  non numeric ncid
testid = 'Test 302';
try
	[vardata, status] = mexnc ( 'get_varm_float', 'ncid', varid, [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 303:  GET_VARM_INT:  non numeric ncid
testid = 'Test 303';
try
	[vardata, status] = mexnc ( 'get_varm_int', 'ncid', varid, [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 304:  GET_VARM_SHORT:  non numeric ncid
testid = 'Test 304';
try
	[vardata, status] = mexnc ( 'get_varm_short', 'ncid', varid, [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 305:  GET_VARM_SCHAR:  non numeric ncid
testid = 'Test 305';
try
	[vardata, status] = mexnc ( 'get_varm_schar', 'ncid', varid, [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 306:  GET_VARM_UCHAR:  non numeric ncid
testid = 'Test 306';
try
	[vardata, status] = mexnc ( 'get_varm_uchar', 'ncid', varid, [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 307:  GET_VARM_TEXT:  non numeric ncid
testid = 'Test 307';
try
	[vardata, status] = mexnc ( 'get_varm_text', 'ncid', varid, [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 308:  GET_VARM_DOUBLE:  non numeric varid
testid = 'Test 308';
try
	[vardata, status] = mexnc ( 'get_varm_double', ncid, 'varid', [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 309:  GET_VARM_FLOAT:  non numeric varid
testid = 'Test 309';
try
	[vardata, status] = mexnc ( 'get_varm_float', ncid, 'varid', [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 310:  GET_VARM_INT:  non numeric varid
testid = 'Test 310';
try
	[vardata, status] = mexnc ( 'get_varm_int', ncid, 'varid', [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 311:  GET_VARM_SHORT:  non numeric varid
testid = 'Test 311';
try
	[vardata, status] = mexnc ( 'get_varm_short', ncid, 'varid', [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 312:  GET_VARM_SCHAR:  non numeric varid
testid = 'Test 312';
try
	[vardata, status] = mexnc ( 'get_varm_schar', ncid, 'varid', [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 313:  GET_VARM_UCHAR:  non numeric varid
testid = 'Test 313';
try
	[vardata, status] = mexnc ( 'get_varm_uchar', ncid, 'varid', [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 314:  GET_VARM_TEXT:  non numeric varid
testid = 'Test 314';
try
	[vardata, status] = mexnc ( 'get_varm_text', ncid, 'varid', [0 0], [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 315:  GET_VARM_DOUBLE:  non numeric start index
testid = 'Test 315';
try
	[vardata, status] = mexnc ( 'get_varm_double', ncid, varid, 'blah', [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 316:  GET_VARM_FLOAT:  non numeric start index
testid = 'Test 316';
try
	[vardata, status] = mexnc ( 'get_varm_float', ncid, varid, 'blah', [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 317:  GET_VARM_INT:  non numeric start index
testid = 'Test 317';
try
	[vardata, status] = mexnc ( 'get_varm_int', ncid, varid, 'blah', [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 318:  GET_VARM_SHORT:  non numeric start index
testid = 'Test 318';
try
	[vardata, status] = mexnc ( 'get_varm_short', ncid, varid, 'blah', [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 319:  GET_VARM_SCHAR:  non numeric start index
testid = 'Test 319';
try
	[vardata, status] = mexnc ( 'get_varm_schar', ncid, varid, 'blah', [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 320:  GET_VARM_UCHAR:  non numeric start index
testid = 'Test 320';
try
	[vardata, status] = mexnc ( 'get_varm_uchar', ncid, varid, 'blah', [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 321:  GET_VARM_TEXT:  non numeric start index
testid = 'Test 321';
try
	[vardata, status] = mexnc ( 'get_varm_text', ncid, varid, 'blah', [6 4], [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


%
% Test 322:  GET_VARM_DOUBLE:  non numeric count index
testid = 'Test 322';
try
	[vardata, status] = mexnc ( 'get_varm_double', ncid, varid, [0 0], 'blah', [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 323:  GET_VARM_FLOAT:  non numeric count index
testid = 'Test 323';
try
	[vardata, status] = mexnc ( 'get_varm_float', ncid, varid, [0 0], 'blah', [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 324:  GET_VARM_INT:  non numeric count index
testid = 'Test 324';
try
	[vardata, status] = mexnc ( 'get_varm_int', ncid, varid, [0 0], 'blah', [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 325:  GET_VARM_SHORT:  non numeric count index
testid = 'Test 325';
try
	[vardata, status] = mexnc ( 'get_varm_short', ncid, varid, [0 0], 'blah', [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 326:  GET_VARM_SCHAR:  non numeric count index
testid = 'Test 326';
try
	[vardata, status] = mexnc ( 'get_varm_schar', ncid, varid, [0 0], 'blah', [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 327:  GET_VARM_UCHAR:  non numeric count index
testid = 'Test 327';
try
	[vardata, status] = mexnc ( 'get_varm_uchar', ncid, varid, [0 0], 'blah', [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 328:  GET_VARM_TEXT:  non numeric count index
testid = 'Test 328';
try
	[vardata, status] = mexnc ( 'get_varm_text', ncid, varid, [0 0], 'blah', [1 1], [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 329:  GET_VARM_DOUBLE:  non numeric stride argument
testid = 'Test 329';
try
	[vardata, status] = mexnc ( 'get_varm_double', ncid, varid, [0 0], [2 3], 'blah' , [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 330:  GET_VARM_FLOAT:  non numeric stride argument
testid = 'Test 330';
try
	[vardata, status] = mexnc ( 'get_varm_float', ncid, varid, [0 0], [2 3], 'blah', [1 6]  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 331:  GET_VARM_INT:  non numeric stride argument
testid = 'Test 331';
try
	[vardata, status] = mexnc ( 'get_varm_int', ncid, varid, [0 0], [2 3], 'blah', [1 6]  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 332:  GET_VARM_SHORT:  non numeric stride argument
testid = 'Test 332';
try
	[vardata, status] = mexnc ( 'get_varm_short', ncid, varid, [0 0], [2 3], 'blah', [1 6]  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 333:  GET_VARM_SCHAR:  non numeric stride argument
testid = 'Test 333';
try
	[vardata, status] = mexnc ( 'get_varm_schar', ncid, varid, [0 0], [2 3], 'blah', [1 6]  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 334:  GET_VARM_UCHAR:  non numeric stride argument
testid = 'Test 334';
try
	[vardata, status] = mexnc ( 'get_varm_uchar', ncid, varid, [0 0], [2 3], 'blah', [1 6]  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 335:  GET_VARM_TEXT:  non numeric stride argument
testid = 'Test 335';
try
	[vardata, status] = mexnc ( 'get_varm_text', ncid, varid, [0 0], [2 3], 'blah', [1 6]  );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 336:  GET_VARM_DOUBLE:  non numeric imap argument
testid = 'Test 336';
try
	[vardata, status] = mexnc ( 'get_varm_double', ncid, varid, [0 0], [2 3], 'blah' , [1 6] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 337:  GET_VARM_FLOAT:  non numeric imap argument
testid = 'Test 337';
try
	[vardata, status] = mexnc ( 'get_varm_float', ncid, varid, [0 0], [2 3], [1 1], 'blah'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 338:  GET_VARM_INT:  non numeric imap argument
testid = 'Test 338';
try
	[vardata, status] = mexnc ( 'get_varm_int', ncid, varid, [0 0], [2 3], [1 1], 'blah'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 339:  GET_VARM_SHORT:  non numeric imap argument
testid = 'Test 339';
try
	[vardata, status] = mexnc ( 'get_varm_short', ncid, varid, [0 0], [2 3], [1 1], 'blah'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 340:  GET_VARM_SCHAR:  non numeric imap argument
testid = 'Test 340';
try
	[vardata, status] = mexnc ( 'get_varm_schar', ncid, varid, [0 0], [2 3], [1 1], 'blah'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 341:  GET_VARM_UCHAR:  non numeric imap argument
testid = 'Test 341';
try
	[vardata, status] = mexnc ( 'get_varm_uchar', ncid, varid, [0 0], [2 3], [1 1], 'blah'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




%
% Test 342:  GET_VARM_TEXT:  non numeric imap argument
testid = 'Test 342';
try
	[vardata, status] = mexnc ( 'get_varm_text', ncid, varid, [0 0], [2 3], [1 1], 'blah'   );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 401:  GET_VAR_DOUBLE:  non numeric ncid
testid = 'Test 401';
try
	[vardata, status] = mexnc ( 'get_var_double', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 402:  GET_VAR_FLOAT:  non numeric ncid
testid = 'Test 402';
try
	[vardata, status] = mexnc ( 'get_var_float', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 403:  GET_VAR_INT:  non numeric ncid
testid = 'Test 403';
try
	[vardata, status] = mexnc ( 'get_var_int', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 404:  GET_VAR_SHORT:  non numeric ncid
testid = 'Test 404';
try
	[vardata, status] = mexnc ( 'get_var_short', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 405:  GET_VAR_SCHAR:  non numeric ncid
testid = 'Test 405';
try
	[vardata, status] = mexnc ( 'get_var_schar', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 406:  GET_VAR_UCHAR:  non numeric ncid
testid = 'Test 406';
try
	[vardata, status] = mexnc ( 'get_var_uchar', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 407:  GET_VAR_TEXT:  non numeric ncid
testid = 'Test 407';
try
	[vardata, status] = mexnc ( 'get_var_text', 'ncid', varid, [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 408:  GET_VAR_DOUBLE:  non numeric varid
testid = 'Test 408';
try
	[vardata, status] = mexnc ( 'get_var_double', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 409:  GET_VAR_FLOAT:  non numeric varid
testid = 'Test 409';
try
	[vardata, status] = mexnc ( 'get_var_float', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 410:  GET_VAR_INT:  non numeric varid
testid = 'Test 410';
try
	[vardata, status] = mexnc ( 'get_var_int', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 411:  GET_VAR_SHORT:  non numeric varid
testid = 'Test 411';
try
	[vardata, status] = mexnc ( 'get_var_short', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 412:  GET_VAR_SCHAR:  non numeric varid
testid = 'Test 412';
try
	[vardata, status] = mexnc ( 'get_var_schar', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 413:  GET_VAR_UCHAR:  non numeric varid
testid = 'Test 413';
try
	[vardata, status] = mexnc ( 'get_var_uchar', ncid, 'varid', [0 0] );
	error_condition = 1;
end
if error_condition
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end





%
% Test 414:  GET_VAR_TEXT:  non numeric varid
testid = 'Test 414';
try
	[vardata, status] = mexnc ( 'get_var_text', ncid, 'varid', [0 0] );
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


fprintf ( 1, 'GET_VAR_BAD_PARAM_DATATYPE succeeded.\n' );


return
















