function test_get_var1_text ( ncfile )
%TEST_GET_VAR1_TEXT:  tests GET_VAR1_TEXT function
%
% Test 1:  
%     Uses PUT_VAR_TEXT to write out the alphabet, then read back the 
%     letter b.
% Test 2:  Use an invalid ncid.
% Test 3:  Use empty set ncid.
% Test 4:  Use an invalid varid.
% Test 5:  Use empty set varid.
% Test 6:  Try to retrieve from position -1.
% Test 7:  Try to retrieve from position 27.
% Test 8:  Use empty set position.
% Test 9:  Use non-numeric position.
% Test 10:  Use non-numeric ncid.
% Test 11:  Use non-numeric varid.
%

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	error ( 'OPEN failed with write' );
end




[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 26 );
if ( status ~= 0 )
	error ( 'DEF_DIM failed on X' );
end

[x_char_varid, status] = mexnc ( 'def_var', ncid, 'x_char', 'char', 1, xdimid );
if ( status ~= 0 )
	error ( 'DEF_VAR failed on x_char' );
end

[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	error ( 'ENDEF failed with write' );
end


[varid, status] = mexnc('INQ_VARID', ncid, 'x_char' );
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARID failed on %s\n', mfilename, varnames{j} );
	error ( msg );
end
	
status = mexnc ( 'PUT_VAR_TEXT', ncid, varid, 'abcdefghijklmnopqrstuvwxyz' );
if ( status ~= 0 )
	msg = sprintf ( '%s:  PUT_VAR1_TEXT failed\n', mfilename );
	error ( msg );
end
	
status = mexnc ( 'SYNC', ncid );
if ( status ~= 0 )
	ncerr_mesg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  PUT_VAR1_TEXT failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end
	




testid = 'Test 1';
[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, varid, [1] );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  %s:  ''%s''\n', mfilename, testid, ncerr );
	error ( err_msg );
end
	
if ( ~strcmp(value,'b') )
	msg = sprintf ( '%s:  value returned by GET_VAR1_TEXT ''%s'' for x_char wasn''t what it should be, %s\n', mfilename, value, 'b'  );
	error ( msg );
end



% Test 2:  Use an invalid ncid.
testid = 'Test 2';
[value,status] = mexnc ( 'GET_VAR1_TEXT', -2000, varid, [1] );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end

	
% Test 3:  Use empty set ncid.
testid = 'Test 3';
try
	[value,status] = mexnc ( 'GET_VAR1_TEXT', [], varid, [1] );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end



% Test 4:  Use an invalid varid.
testid = 'Test 4';
[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, -2000, [1] );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end



% Test 5:  Use empty set varid.
testid = 'Test 5';
try
	[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, [], [1] );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end



% Test 6:  Try to retrieve from position -1.
testid = 'Test 6';
[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, varid, [-1] );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end
% Test 7:  Try to retrieve from position 27.
testid = 'Test 7';
[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, varid, [27] );
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end




% Test 8:  Use empty set position.
testid = 'Test 8';
try
	[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, varid, [] );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 9:  Use non-numeric position
testid = 'Test 9';
try
	[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, varid, 'badbadbad' );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end


% Test 10:  Use non-numeric position
testid = 'Test 10';
try
	[value,status] = mexnc ( 'GET_VAR1_TEXT', 'bad', varid, [1] );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end



% Test 11:  Use non-numeric varid
testid = 'Test 11';
try
	[value,status] = mexnc ( 'GET_VAR1_TEXT', ncid, 'bad', [1] );
	err_msg = sprintf ( '%s:  %s:  succeeded when it should have failed\n', mfilename, testid);
	error ( err_msg );
end



%
% CLOSE
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed on nowrite' );
end

fprintf ( 1, 'GET_VAR1_TEXT succeeded\n' );

return






