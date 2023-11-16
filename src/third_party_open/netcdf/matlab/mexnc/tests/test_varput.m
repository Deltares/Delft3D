function test_varput ( ncfile )
% TEST_VARPUT
%

if ( nargin < 1 )
	ncfile = 'foo.nc';
end

mexnc ( 'setopts', 0 );

create_testfile ( ncfile );


test_read_col_inds            ( ncfile );
test_double_precision         ( ncfile );
test_scaling                  ( ncfile );
test_scaling_flag_set_to_zero ( ncfile );
test_2D                       ( ncfile );
test_read_single              ( ncfile );
test_read_short               ( ncfile );
test_2D_nonzero_start         ( ncfile );
test_1D_row                   ( ncfile );
test_1D_vector                ( ncfile );

test_neg_float_input          ( ncfile );
test_neg_short_input          ( ncfile );

test_neg_varput_bad_ncid      ( ncfile );
test_neg_varget_with_bad_ncid ( ncfile );
test_neg_varput_with_bad_varid ( ncfile );

regression_charVarId            (ncfile);
regression_scalingCharWithVarId ( ncfile );

test_minus_one_count ( ncfile );
create_testfile(ncfile);
test_minus_write ( ncfile );

fprintf ( 1, 'VARPUT succeeded\n' );
fprintf ( 1, 'VARGET succeeded\n' );

return




%--------------------------------------------------------------------------
function create_testfile ( ncfile )


%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



%
% Create the fixed dimension.  
len_x = 100;
len_y = 200;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', len_y );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[z_double_varid, status] = mexnc ( 'def_var', ncid, 'z_double', nc_double, 1, xdimid ); %#ok<ASGLU>
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[z_float_varid, status] = mexnc ( 'def_var', ncid, 'z_float', nc_float, 1, xdimid ); %#ok<ASGLU>
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



[z_short_varid, status] = mexnc ( 'def_var', ncid, 'z_short', nc_short, 1, xdimid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[twod_varid, status] = mexnc ( 'def_var', ncid, 'twoD', nc_double, 2, [ydimid xdimid] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end



eps = 0.01;
status = mexnc ( 'put_att_double', ncid, z_short_varid, 'scale_factor', nc_double, 1, eps );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


status = mexnc ( 'put_att_double', ncid, z_short_varid, 'add_offset', nc_double, 1, 0.00 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


status = mexnc('put_var_double',ncid,twod_varid,1:len_x*len_y);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return















%--------------------------------------------------------------------------
function test_minus_one_count ( ncfile )
% Notes sent from Simon Spagnol
%
% theta1 = ncmex('varget',cdfid,'thetau1',[0 0],[-1 -1],0) ;
%
% Now for some reason putting what I assume was some sort of default 
% [-1 -1] causes my matlab implentation to fail with a memory error 
% (only have to one installation so can't test on others)


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

xdimid = mexnc('INQ_DIMID',ncid,'x');
[name,xlen] = mexnc('DIMINQ',ncid,xdimid); %#ok<ASGLU>

ydimid = mexnc('INQ_DIMID',ncid,'y');
[name,ylen] = mexnc('DIMINQ',ncid,ydimid); %#ok<ASGLU>

% 1D case
[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(xlen,1);
status = mexnc ( 'VARPUT', ncid, z_double_varid, 0, xlen, input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, z_double_varid, 1, -1 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data(:);

d = max(abs(output_data-input_data(2:100)))';
if (any(d))
	error ( 'values written by VARGET do not match what was retrieved by VARPUT\n'  );
end


% 2D case
[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(ylen,xlen);
status = mexnc ( 'VARPUT', ncid, varid, [0 0], [ylen xlen], input_data' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, varid, [5 5], [-1 -1] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';
input_data = input_data(6:end,6:end);
d = max(abs(output_data(:) - input_data(:)));
if (any(d))
	error ( 'values written by VARGET do not match what was retrieved by VARPUT\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_minus_write ( ncfile )
% test negative count argument when writing


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

xdimid = mexnc('INQ_DIMID',ncid,'x');
[name,xlen] = mexnc('DIMINQ',ncid,xdimid); %#ok<ASGLU>

ydimid = mexnc('INQ_DIMID',ncid,'y');
[name,ylen] = mexnc('DIMINQ',ncid,ydimid); %#ok<ASGLU>

[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(150,50);
start = [50 50]; count = [-1 -1]; 
status = mexnc('VARPUT',ncid, varid, start, count, input_data' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, varid, start, count );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end
output_data = output_data';

if (size(output_data,1) ~= 150) || (size(output_data,2) ~= 50)
    error('output data size did not match input data size');
end

d = max(abs(output_data(:)-input_data(:)))';
if (any(d))
	error ( 'values written by VARGET do not match what was retrieved by VARPUT\n'  );
end


% 2D case
[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(ylen,xlen);
status = mexnc ( 'VARPUT', ncid, varid, [0 0], [ylen xlen], input_data' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, varid, [5 5], [-1 -1] );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';
input_data = input_data(6:end,6:end);
d = max(abs(output_data(:) - input_data(:)));
if (any(d))
	error ( 'values written by VARGET do not match what was retrieved by VARPUT\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_double_precision ( ncfile )


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(50,1);
status = mexnc ( 'VARPUT', ncid, z_double_varid, 1, 50, input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, z_double_varid, 1, 50 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data(:);

d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'values written by VARGET do not match what was retrieved by VARPUT\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_read_single ( ncfile )
% Make sure that the data is read back as double precision


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_varid, status] = mexnc('INQ_VARID', ncid, 'z_float');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(50,1);
status = mexnc ( 'VARPUT', ncid, z_varid, 1, 50, input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, z_varid, 1, 50 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ~strcmp(class(output_data),'double')
	error ( 'values read back by varget should be double precision');
end
output_data = output_data(:);

d = max(abs(output_data-double(single(input_data))))';
if (any(d))
	error ( 'values written by VARGET are not as expected\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








%--------------------------------------------------------------------------
function test_read_short ( ncfile )
% Make sure that the data is read back as double precision


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_short');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 1:50;
status = mexnc ( 'VARPUT', ncid, varid, 1, 50, input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, varid, 1, 50 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

if ~strcmp(class(output_data),'double')
	error ( 'values read back by varget should be double precision');
end

d = max(abs(output_data(:)-input_data(:)));
if (any(d))
	error ( 'values written by VARGET are not as expected\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_read_col_inds ( ncfile )
% Make sure that we can read data if the indices are columns.


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = [1 2; 3 4];
status = mexnc ( 'VARPUT', ncid, varid, [0 1]', [2 2], input_data' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, varid, [0 1]', [2 2]' );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';

if ~strcmp(class(output_data),'double')
	error ( 'values read back by varget should be double precision');
end

d = max(abs(output_data(:)-input_data(:)));
if (any(d))
	error ( 'values written by VARGET are not as expected'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








%--------------------------------------------------------------------------
function test_neg_float_input ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_float');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = single(rand(50,1));
fail = false;
try %#ok<TRYNC>
	mexnc ( 'VARPUT', ncid, varid, 1, 50, input_data );
	fail = true;
end
if fail
	error ( 'Succeeded when it should have failed.' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return









%--------------------------------------------------------------------------
function test_neg_short_input ( ncfile )


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = int16(rand(50,1)*100);
fail = false;
try %#ok<TRYNC>
	status = mexnc ( 'VARPUT', ncid, varid, 1, 50, input_data ); %#ok<NASGU>
	fail = true;
end
if fail
	error ( 'Succeeded when it should have failed.' );
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return









%--------------------------------------------------------------------------
function test_neg_varput_bad_ncid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(50,1);
status = mexnc ( 'VARPUT', -100, z_double_varid, 1, 50, input_data );
if ( status == 0 )
	error ( 'VARPUT succeeded with a bad ncid' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_neg_varget_with_bad_ncid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[output_data, status] = mexnc ( 'VARGET', -100, z_double_varid, 1, 50 ); %#ok<ASGLU>
if ( status == 0 )
	error ( 'VARGET succeeded with a bad ncid' );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








%--------------------------------------------------------------------------
function test_neg_varput_with_bad_varid ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_double_varid, status] = mexnc('INQ_VARID', ncid, 'z_double'); %#ok<ASGLU>
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(50,1);
status = mexnc ( 'VARPUT', ncid, -500, 1, 50, input_data );
if ( status == 0 )
	error ( 'VARPUT succeeded with a bad varid' );
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return



%--------------------------------------------------------------------------
function test_scaling ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_short_varid, status] = mexnc('INQ_VARID', ncid, 'z_short');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[dimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, dimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[scale_factor, status] = mexnc('GET_ATT_DOUBLE', ncid, z_short_varid, 'scale_factor');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


input_data = rand(len_x,1);
status = mexnc ( 'VARPUT', ncid, z_short_varid, 0, len_x, input_data', 1 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( '%s:  VARPUT failed, (%s)\n', mfilename, ncerr_msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, z_short_varid, 0, len_x, 1 );
if ( status ~= 0 )
	error ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
end

output_data = output_data';

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


if (~strcmp(class(output_data),'double'))
	error ( 'data was not double precision' );
end

d = max(abs(output_data(:)-input_data(:)))';
ind = find ( d > abs(scale_factor)/2 );
if (any(ind))
	error ( 'values written by VARPUT do not match what was retrieved by VARGET' );
end






%--------------------------------------------------------------------------
function regression_scalingCharWithVarId ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[dimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, dimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[scale_factor, status] = mexnc('ATTGET', ncid, 'z_short', 'scale_factor');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


input_data = rand(len_x,1);
status = mexnc ( 'VARPUT', ncid, 'z_short', 0, len_x, input_data', 1 );
if ( status ~= 0 )
	ncerr_msg = mexnc('strerror',status);
	error( ncerr_msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, 'z_short', 0, len_x, 1 );
if ( status ~= 0 )
	error ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
end

output_data = output_data';

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


if (~strcmp(class(output_data),'double'))
	error ( 'data was not double precision' );
end

d = max(abs(output_data(:)-input_data(:)))';
ind = find ( d > abs(scale_factor)/2 );
if (any(ind))
	error ( 'values written by VARPUT do not match what was retrieved by VARGET' );
end







%--------------------------------------------------------------------------
function test_scaling_flag_set_to_zero ( ncfile )
% test with scaling flag set to zero.

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[z_short_varid, status] = mexnc('INQ_VARID', ncid, 'z_short');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[dimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, dimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = int16(rand(len_x,1)*100);
input_data = double(input_data);

status = mexnc ( 'VARPUT', ncid, z_short_varid, 0, len_x, input_data', 0 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( '%s:  VARPUT failed, (%s)\n', mfilename, ncerr_msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, z_short_varid, 0, len_x, 0 );
if ( status ~= 0 )
	error ( '%s:  VARGET failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
end

output_data = output_data';

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


if (~strcmp(class(output_data),'double'))
	error ( 'data was not double precision' );
end

d = max(abs(output_data(:)-input_data(:)))';
ind = find ( d > 0 );
if (any(ind))
	error ( 'values written by VARPUT do not match what was retrieved by VARGET' );
end



%--------------------------------------------------------------------------
function test_2D ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[xdimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[ydimid, status] = mexnc('INQ_DIMID', ncid, 'y');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, xdimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_y, status] = mexnc('INQ_DIMLEN', ncid, ydimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 1:len_y*len_x;
input_data = reshape(input_data,[len_y len_x]);

status = mexnc ( 'VARPUT', ncid, varid, [0 0], [len_y len_x], input_data' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error( ncerr_msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, varid, [0 0], [len_y len_x] );
if ( status ~= 0 )
	error ( mexnc ( 'strerror', status ) );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';

if (~strcmp(class(output_data),'double'))
	error ( 'data was not double precision' );
end

d = max(abs(output_data-input_data))';
ind = find ( d > 0 );
if (any(ind))
    error ( 'values written by VARPUT do not match what was retrieved by VARGET' );
end



%--------------------------------------------------------------------------
function test_1D_vector ( ncfile )
% Make sure that reading a 1D dataset works the same as it did in R2008a

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_double');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


[xdimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, xdimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

start = 0;
count = len_x;

[output_data,status] = mexnc ( 'VARGET', ncid, varid, start, count );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( 'VARGET failed, (%s)\n', ncerr_msg );
end

% We do the transpose as usual here.  
output_data = output_data';

if size(output_data,2) ~= 1
	error('output data was not a column');
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end




%--------------------------------------------------------------------------
function test_1D_row ( ncfile )
% Make sure that reading a row or a column acts as it did in 2008a

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[xdimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[ydimid, status] = mexnc('INQ_DIMID', ncid, 'y');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, xdimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_y, status] = mexnc('INQ_DIMLEN', ncid, ydimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

start = [0 0];
count = [1 len_x];

[output_data,status] = mexnc ( 'VARGET', ncid, varid, start, count );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( 'VARPUT failed, (%s)\n', ncerr_msg );
end

% We do the transpose as usual here.  
output_data = output_data';

if size(output_data,1) ~= 1
	error('output data was not a row');
end



% Now try the same reading a column
start = [0 0];
count = [len_y 1];

[output_data,status] = mexnc ( 'VARGET', ncid, varid, start, count );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( 'VARPUT failed, (%s)\n', ncerr_msg );
end

% We do the transpose as usual here.  But VARGET makes it a row!!!
output_data = output_data';

if size(output_data,2) ~= 1
	error('output data was not a column');
end




status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end




%--------------------------------------------------------------------------
function test_2D_nonzero_start ( ncfile )

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[varid, status] = mexnc('INQ_VARID', ncid, 'twoD');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[xdimid, status] = mexnc('INQ_DIMID', ncid, 'x');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[ydimid, status] = mexnc('INQ_DIMID', ncid, 'y');
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_x, status] = mexnc('INQ_DIMLEN', ncid, xdimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

[len_y, status] = mexnc('INQ_DIMLEN', ncid, ydimid);
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = 1:len_y*len_x;
input_data = reshape(input_data,[len_y len_x]);
input_data = input_data(end-7:end,end-5:end);
start = [len_y-8 (len_x-6)];
count = [8 6];

status = mexnc ( 'VARPUT', ncid, varid, start, count, input_data' );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	error ( 'VARPUT failed, (%s)\n', ncerr_msg );
end


[output_data, status] = mexnc ( 'VARGET', ncid, varid, start, count );
if ( status ~= 0 )
	error( mexnc ( 'strerror', status ) );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data';

if (~strcmp(class(output_data),'double'))
	error ( 'data was not double precision' );
end

d = max(abs(output_data-input_data))';
ind = find ( d > 0 );
if (any(ind))
	error ( 'values written by VARPUT do not match what was retrieved by VARGET' );
end



%--------------------------------------------------------------------------
function regression_charVarId ( ncfile )


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

input_data = rand(50,1);
status = mexnc ( 'VARPUT', ncid, 'z_double', 1, 50, input_data );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

mexnc('sync',ncid);

[output_data, status] = mexnc ( 'VARGET', ncid, 'z_double', 1, 50 );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

output_data = output_data(:);

d = max(abs(output_data-input_data))';
if (any(d))
	error ( 'values written by VARGET do not match what was retrieved by VARPUT\n'  );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return






