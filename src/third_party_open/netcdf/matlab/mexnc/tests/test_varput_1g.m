function test_varput_1g ( ncfile )
% TEST_VARPUT_1G
%
% This routine tests VARPUT1, VARPUT, and VARPUTG routines with both scaling
% and no scaling.  MEXCDF should never have been written with scaling built in.
%
% Test 1:  VARPUT1/VARGET1
% Test 2:  varput1/varget1 with scaling
% Test 3:  varput/varget
% Test 4:  varput/varget with scaling
% Test 4.5:  varput/varget with scaling, negative integers
% Test 5:  varputg/vargetg 
% Test 6:  varputg/vargetg  with scaling
% Test 7:  VARPUT1 with a bad ncid
% Test 8:  VARGET1 with a bad ncid

mexnc ( 'setopts', 0 );

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
len_x = 100;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim x, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[z_double_varid, status] = mexnc ( 'def_var', ncid, 'z_double', nc_double, 1, [xdimid] );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_var'' failed on var x_short, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end



[z_short_varid, status] = mexnc ( 'def_var', ncid, 'z_short', nc_short, 1, [xdimid] );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_var'' failed on var z_short, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


eps = 0.01;
status = mexnc ( 'put_att_double', ncid, z_short_varid, 'scale_factor', nc_double, 1, eps );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr_msg );
	error ( msg );
end


status = mexnc ( 'put_att_double', ncid, z_short_varid, 'add_offset', nc_double, 1, 0.00 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr_msg );
	error ( msg );
end





[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''enddef'' failed, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
% CLOSE
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed' );
end



[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr_msg );
	error ( msg );
end



[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARID failed\n', mfilename );
	error ( msg );
end


%
% Test 1:  varput1/varget1
testid = 'Test 1';
input_data = 3.14159;
status = mexnc ( 'VARPUT1', ncid, z_double_varid, [0], input_data );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUT1 failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGET1', ncid, z_double_varid, [0] );
if ( status ~= 0 )
	msg = sprintf ( '%s:  GET_VAR_DOUBLE failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
if (any(d))
	msg = sprintf ( '%s:  %s:  values written by PUT_VAR_DOUBLE do not match what was retrieved by GET_VAR_DOUBLE\n', mfilename, testid  );
	error ( msg );
end

fprintf ( 1, 'VARPUT1 succeeded\n' );
fprintf ( 1, 'VARGET1 succeeded\n' );



%
% Test 2:  varput1/varget1 with scaling
testid = 'Test 2';
input_data = 3.14159;
status = mexnc ( 'VARPUT1', ncid, z_short_varid, [0], input_data,1 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUT1 failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGET1', ncid, z_short_varid, [0], 1 );
if ( status ~= 0 )
	msg = sprintf ( '%s:  GET_VAR_DOUBLE failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
ind = find ( d > eps );
if (any(ind))
	msg = sprintf ( '%s:  %s:  values written by VARPUT1 do not match what was retrieved by VARGET1\n', mfilename  , testid );
	error ( msg );
end

fprintf ( 1, 'VARPUT1 with scaling (please don''t do this, it''s bad) succeeded\n' );
fprintf ( 1, 'VARGET1 with scaling (please don''t do this, it''s bad) succeeded\n' );


%
% Test 3:  varput/varget
testid = 'Test 3';
input_data = rand(100,1);
status = mexnc ( 'VARPUT', ncid, z_double_varid, [0], [len_x], input_data' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUT failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, z_double_varid, [0], [len_x] );
if ( status ~= 0 )
	msg = sprintf ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
if (any(d))
	msg = sprintf ( '%s:  %s:  values written by VARPUT do not match what was retrieved by VARGET\n', mfilename, testid  );
	error ( msg );
end

fprintf ( 1, 'VARPUT succeeded\n' );
fprintf ( 1, 'VARGET succeeded\n' );


%
% Test 4:  varput/varget with scaling
testid = 'Test 4';
input_data = rand(100,1);
status = mexnc ( 'VARPUT', ncid, z_short_varid, [0], [len_x], input_data', 1 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUT failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, z_short_varid, [0], [len_x], 1 );
if ( status ~= 0 )
	msg = sprintf ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
ind = find ( d > eps/2 );
if (any(ind))
	msg = sprintf ( '%s:  %s:  values written by VARPUT do not match what was retrieved by VARGET\n', mfilename , testid );
	error ( msg );
end


fprintf ( 1, 'VARPUT with scaling (please don''t do this, it''s bad) succeeded\n' );
fprintf ( 1, 'VARGET with scaling (please don''t do this, it''s bad) succeeded\n' );




%
% Test 4.5:  varput/varget with scaling, negative integers being the target.
testid = 'Test 4.5';
input_data = rand(100,1) - 1;
status = mexnc ( 'VARPUT', ncid, z_short_varid, [0], [len_x], input_data', 1 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUT failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, z_short_varid, [0], [len_x], 1 );
if ( status ~= 0 )
	msg = sprintf ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
ind = find ( d > eps/2 );
if (any(ind))
	msg = sprintf ( '%s:  %s:  values written by VARPUT do not match what was retrieved by VARGET\n', mfilename , testid );
	error ( msg );
end


fprintf ( 1, 'VARPUT with scaling, negative integers\n' );
fprintf ( 1, 'VARGET with scaling, negative integers succeeded\n' );




%
% Test 4.6:  varput/varget with negative integers
testid = 'Test 4.6';
input_data = round(rand(100,1)*100 - 200);
status = mexnc ( 'VARPUT', ncid, z_short_varid, [0], [len_x], input_data', 1 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUT failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, z_short_varid, [0], [len_x], 1 );
if ( status ~= 0 )
	msg = sprintf ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
ind = find ( d > eps/2 );
if (any(ind))
	msg = sprintf ( '%s:  %s:  values written by VARPUT do not match what was retrieved by VARGET\n', mfilename , testid );
	error ( msg );
end


fprintf ( 1, 'VARPUT with negative integers\n' );
fprintf ( 1, 'VARGET with negative integers succeeded\n' );




%
% VARPUTG
% Test 5:  varputg/vargetg 
testid = 'Test 5';
input_data = rand(100,1);
input_data = input_data(1:2:end,1:2:end);
[r,c] = size(input_data);
status = mexnc ( 'VARPUTG', ncid, z_double_varid, [0 0], [r c], [2 2], [], input_data' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUTG failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGETG', ncid, z_double_varid, [0 0], [r c], [2 2], [] );
if ( status ~= 0 )
	msg = sprintf ( '%s:  VARGETG failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
if (any(d))
	msg = sprintf ( '%s:  %s:  values written by VARPUTG do not match what was retrieved by VARGETG\n', mfilename, testid  );
	error ( msg );
end

fprintf ( 1, 'VARPUTG succeeded\n' );
fprintf ( 1, 'VARGETG succeeded\n' );


%
%
% VARPUTG
% Test 6:  varputg/vargetg  with scaling
testid = 'Test 6';
input_data = rand(100,1);
input_data = input_data(1:2:end);
[r,c] = size(input_data);
status = mexnc ( 'VARPUTG', ncid, z_short_varid, [0 0], [r], [2], [], input_data', 1 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  VARPUTG failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGETG', ncid, z_short_varid, [0], [r], [2], [], 1 );
if ( status ~= 0 )
	msg = sprintf ( '%s:  ''%s''\n', mfilename,  mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(output_data-input_data))';
ind = find ( d > eps/2 );
if (any(ind))
	msg = sprintf ( '%s:  %s:  values written by VARPUTG do not match what was retrieved by VARGETG\n', mfilename, testid  );
	error ( msg );
end


fprintf ( 1, 'VARPUTG with scaling (please don''t do this, it''s bad) succeeded\n' );
fprintf ( 1, 'VARGETG with scaling (please don''t do this, it''s bad) succeeded\n' );





%
%
% Test 7:  VARPUT1 with a bad ncid
testid = 'Test 7';
input_data = 3.14159;
status = mexnc ( 'VARPUT1', -100, z_double_varid, [0], input_data );
if ( status >= 0 )
	msg = sprintf ( '%s:  %s:  VARPUT1 succeeded with a bad ncid\n', mfilename, testid );
	error ( msg );
end


%
% Test 8:  VARGET1 with a bad ncid
testid = 'Test 8';
[output_data, status] = mexnc ( 'VARGET1', -100, z_double_varid, [0] );
if ( status == 0 )
	msg = sprintf ( '%s:  %s:  VARGET1 succeeded with a bad ncid\n', mfilename, testid );
	error ( msg );
end



%
% Test 9:  VARPUT1 with a bad varid
testid = 'Test 9';
input_data = 3.14159;
status = mexnc ( 'VARPUT1', ncid, -500, [0], input_data );
if ( status == 0 )
	msg = sprintf ( '%s:  %s:  VARPUT1 succeeded with a bad varid\n', mfilename, testid );
	error ( msg );
end


%
% VARGET1 with a bad varid
[output_data, status] = mexnc ( 'VARGET1', ncid, -500, [0] );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARGET1 succeeded with a bad varid\n', mfilename );
	error ( msg );
end



%
% VARPUT with a bad ncid
%input_data = [1:1:len_y*len_x] + 3.14159;
%input_data = reshape ( input_data, len_y, len_x );
input_data = rand(100,1);
status = mexnc ( 'VARPUT', -500, z_short_varid, [0], size(input_data), input_data' );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARPUT succeeded with a bad varid\n', mfilename );
	error ( msg );
end


%
% VARGET with a bad ncid
[output_data, status] = mexnc ( 'VARGET', -500, z_short_varid, [0], size(input_data) );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARGET succeeded with a bad varid\n', mfilename );
	error ( msg );
end




%
% VARPUT with a bad varid
input_data = rand(100,1);
status = mexnc ( 'VARPUT', ncid, -500, [0], size(input_data), input_data' );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARPUT succeeded with a bad varid\n', mfilename );
	error ( msg );
end


%
% VARGET with a bad varid
[output_data, status] = mexnc ( 'VARGET', ncid, -500, [0], size(input_data) );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARGET succeeded with a bad varid\n', mfilename );
	error ( msg );
end




%
% VARPUTG with a bad ncid
%Tinput_data = input_data(1:2:end,1:2:end);
input_data = rand(100,1);
[r,c] = size(input_data);
status = mexnc ( 'VARPUTG', -5, z_short_varid, [0], [r c], [2 2], [], input_data', 1 );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARPUTG succeeded with a bad ncid\n', mfilename );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGETG', -5, z_short_varid, [0 0], [r c], [2 2], [], 1 );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARGETG succeeded with a bad ncid\n', mfilename );
	error ( msg );
end


status = mexnc ( 'VARPUTG', ncid, -5, [0 0], [r c], [2 2], [], input_data', 1 );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARPUTG succeeded with a bad varid\n', mfilename );
	error ( msg );
end


[output_data, status] = mexnc ( 'VARGETG', ncid, -5, [0 0], [r c], [2 2], [], 1 );
if ( status >= 0 )
	msg = sprintf ( '%s:  VARGETG succeeded with a bad varid\n', mfilename );
	error ( msg );
end




%
% CLOSE
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed' );
end


return







