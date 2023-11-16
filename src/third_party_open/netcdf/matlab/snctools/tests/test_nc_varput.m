function test_nc_varput(mode)

if nargin < 1
	mode = nc_clobber_mode;
end

fprintf('\t\tTesting NC_VARPUT...  ' );
testroot = fileparts(mfilename('fullpath'));

switch(mode)
	case nc_clobber_mode
		ncfile = fullfile(testroot,'testdata/varput.nc');
		run_local_tests(ncfile,mode);

		% This doesn't work for nc4 or hdf4
		test_bad_fill_value;
		run_negative_tests;


	case 'hdf4'
		run_hdf4_tests;

	case 'netcdf4-classic'
		ncfile = fullfile(testroot,'testdata/varput4.nc');
		run_local_tests(ncfile,mode);


end


fprintf('OK\n');
return





%--------------------------------------------------------------------------
function run_local_tests(ncfile,mode)
run_generic_tests(ncfile);
run_singleton_tests(ncfile);
run_scaling_tests(mode);

%--------------------------------------------------------------------------
function run_singleton_tests(input_ncfile)

ncfile = 'foo.nc';
copyfile(input_ncfile,ncfile);

test_write_singleton ( ncfile );

return

%--------------------------------------------------------------------------
function run_generic_tests(input_ncfile)

ncfile = 'foo.nc';
copyfile(input_ncfile,ncfile);

test_put_vars ( ncfile );


test_1D_all();
test_write_1D_good_count ( ncfile );
test_write_1D_good_stride ( ncfile );

test_1D_strided ( ncfile );
test_write_1D_one_element ( ncfile );

test_write_2D_all ( ncfile );
test_write_2D_contiguous_chunk ( ncfile );
test_write_2D_contiguous_chunk_offset ( ncfile );
test_write_2D_strided ( ncfile );

test_write_2D_too_little_with_putvar ( ncfile );



return



%--------------------------------------------------------------------------
function run_scaling_tests(mode)
test_read_scale_offset(mode);
test_write_scale_offset(mode);
test_read_scale_no_offset(mode);
test_read_missing_value(mode);
test_read_floating_point_scale_factor(mode);
test_missing_value_and_fill_value(mode);



















%--------------------------------------------------------------------------
function test_1D_strided(ncfile )

info = nc_info(ncfile);

% This function may segfault on R2008b and R2009a unless the bug fix at
% http://www.mathworks.com/support/bugreports/522794 is applied.  Do not
% run the test unless the user is sure to do so.
v = version('-release');
switch(v)
    case '2008b'
		if ~getpref('SNCTOOLS','FORCE_R2008B_TESTS',false) && strcmp(info.Format,'NetCDF')
		    fprintf('\n\t\t\tFiltering out test_1D_strided on R2008b, please consult the \n');
		    fprintf('\t\t\tsection ''Bug Reports You Should Know About'' in the README for \n');
		    fprintf('\t\t\tbug #522794, or go to http://www.mathworks.com/support/bugreports/522794.\n');
            return
		end
        
    case '2009a'
		if ~getpref('SNCTOOLS','FORCE_R2009A_TESTS',false) && strcmp(info.Format,'NetCDF')
		    fprintf('\n\t\t\tFiltering out test_1D_strided on R2009a, please consult the \n');
		    fprintf('\t\t\tsection ''Bug Reports You Should Know About'' in the README for \n');
		    fprintf('\t\t\tbug #522794, or go to http://www.mathworks.com/support/bugreports/522794.\n');
            return
		end
        
end

input_data = [3.14159; 2];
nc_varput ( ncfile, 'test_1D', input_data, 0, 2, 2 );
output_data = nc_varget ( ncfile, 'test_1D', 0, 2, 2 );

ddiff = abs(input_data - output_data);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.' );
end

return










%--------------------------------------------------------------------------
function test_put_vars ( ncfile )

indata = rand(2,2);
nc_varput ( ncfile, 'test_2D', indata, [0 0], [2 2], [2 2] );
outdata = nc_varget(ncfile,'test_2D',[0 0], [2 2], [2 2]);
if any((abs(indata(:) - outdata(:))) > 1e-10)
    error('failed');
end
return









%--------------------------------------------------------------------------
function test_write_singleton ( ncfile )


input_data = 3.14159;
nc_varput ( ncfile, 'test_singleton', input_data );
output_data = nc_varget ( ncfile, 'test_singleton' );

ddiff = abs(input_data - output_data);
if any( find(ddiff > eps) )
    error( 'input data ~= output data.');
end

return




%--------------------------------------------------------------------------
function test_write_1D_one_element ( ncfile )

input_data = 3.14159;
nc_varput(ncfile,'test_1D',input_data,4);



%--------------------------------------------------------------------------
function test_write_1D_good_count ( ncfile )
% Test that a one-dimensional contiguous write operation works.

input_data = 3.14159;
nc_varput ( ncfile, 'test_1D', input_data, 0, 1 );
output_data = nc_varget ( ncfile, 'test_1D', 0, 1 );

ddiff = abs(input_data - output_data);
if any( find(ddiff > eps) )
    error('input data ~= output data.' );
end

return




%--------------------------------------------------------------------------
function test_write_1D_good_stride ( ncfile )
% Test that one-dimensional strides work.

input_data = [3.14159 2];
nc_varput(ncfile,'test_1D',input_data,0,2,2);   
output_data = nc_varget(ncfile,'test_1D',0,2,2);


ddiff = abs(input_data(:) - output_data(:));
if any( find(ddiff > eps) )
    error('failed' );
end






%--------------------------------------------------------------------------
function test_write_2D_all(ncfile)
% Test that a full write operation works.

input_data = 1:24;

count = nc_varsize(ncfile,'test_2D');
input_data = reshape(input_data,count);
nc_varput(ncfile,'test_2D',input_data);
output_data = nc_varget(ncfile,'test_2D');

ddiff = abs(input_data - output_data);
if any( find(ddiff > eps) )
    error('input data ~= output data');
end

return







%--------------------------------------------------------------------------
function test_write_2D_contiguous_chunk ( ncfile )
% Test that contiguous writes work.

sz = nc_varsize(ncfile,'test_2D');
start = [0 0];
count = sz-1;

input_data = 1:prod(count);

input_data = reshape(input_data,count);
nc_varput ( ncfile, 'test_2D', input_data, start, count );
output_data = nc_varget ( ncfile, 'test_2D', start, count );

ddiff = abs(input_data - output_data);
if any( find(ddiff > eps) )
    error('input data ~= output data' );
end

return





%--------------------------------------------------------------------------
function test_write_2D_contiguous_chunk_offset ( ncfile )
% Test that contiguous writes with a non-zero start work.

sz = nc_varsize(ncfile,'test_2D');
start = [1 1];
count = sz-1;

input_data = (1:prod(count)) - 5;
input_data = reshape(input_data,count);

nc_varput ( ncfile, 'test_2D', input_data, start, count );
output_data = nc_varget ( ncfile, 'test_2D', start, count );

ddiff = abs(input_data - output_data);
if any( find(ddiff > eps) )
    error('failed');
end


return












%--------------------------------------------------------------------------
function test_write_2D_strided ( ncfile )

% Test that strided writes work.

sz = nc_varsize(ncfile,'test_2D');
start = [0 0];
count = sz/2;
stride = [2 2];

input_data = 1:prod(count);

input_data = reshape(input_data,count);
nc_varput ( ncfile, 'test_2D', input_data, start, count, stride );
output_data = nc_varget ( ncfile, 'test_2D', start, count, stride );

ddiff = abs(input_data - output_data);
if any( find(ddiff > eps) )
    error('failed');
end

return








%--------------------------------------------------------------------------
function test_write_2D_too_little_with_putvar ( ncfile )
return
% This isn't a failure.  It assumes [0 0] and [count]
sz = nc_varsize(ncfile,'test_2D');
count = sz-1;

input_data = 1:prod(count);
input_data = reshape(input_data,count);
nc_varput ( ncfile, 'test_2D',input_data);








%--------------------------------------------------------------------------
function test_read_scale_offset ( mode )

ncfile = 'foo.nc';
create_test_file(ncfile,mode);

%
% Write some data, then put a scale factor of 2 and add offset of 1.  The
% data read back should be twice as large plus 1.
%create_test_file ( ncfile );

sz = nc_varsize(ncfile,'test_2D');
count = sz;
input_data = 1:prod(count);
input_data = reshape(input_data,count);

nc_varput ( ncfile, 'test_2D', input_data );
nc_attput ( ncfile, 'test_2D', 'scale_factor', 2.0 );
nc_attput ( ncfile, 'test_2D', 'add_offset', 1.0 );
output_data = nc_varget ( ncfile, 'test_2D' );

ddiff = abs(input_data - (output_data-1)/2);
if any( find(ddiff > eps) )
    error('failed');
end







%--------------------------------------------------------------------------
function test_write_scale_offset ( mode )
%
% Put a scale factor of 2 and add offset of 1.
% Write some data, 
% Put a scale factor of 4 and add offset of 2.
% data read back should be twice as large 

ncfile = 'foo.nc';
create_test_file(ncfile,mode);


sz = nc_varsize(ncfile,'test_2D');
count = sz;
input_data = 1:prod(count);
input_data = reshape(input_data,count);


nc_attput ( ncfile, 'test_2D', 'scale_factor', 2.0 );
nc_attput ( ncfile, 'test_2D', 'add_offset', 1.0 );
nc_varput ( ncfile, 'test_2D', input_data );
nc_attput ( ncfile, 'test_2D', 'scale_factor', 4.0 );
nc_attput ( ncfile, 'test_2D', 'add_offset', 2.0 );
output_data = nc_varget ( ncfile, 'test_2D' );
ddiff = abs(input_data - (output_data)/2);
if any( find(ddiff > eps) )
    error('failed');
end
return









%--------------------------------------------------------------------------
function test_read_scale_no_offset( mode )
%
% Put a scale factor of 2 and no add offset.
% Write some data.  
ncfile = 'foo.nc';
create_test_file(ncfile,mode);



sz = nc_varsize(ncfile,'test_2D');
count = sz;
input_data = 1:prod(count);
input_data = reshape(input_data,count);


nc_attput ( ncfile, 'test_2D', 'scale_factor', 2.0 );
nc_varput ( ncfile, 'test_2D', input_data );

%
% Now change the scale_factor, doubling it.
nc_attput ( ncfile, 'test_2D', 'scale_factor', 4.0 );
output_data = nc_varget ( ncfile, 'test_2D' );

if output_data(1) ~= 2
    error('failed');
end
return






















%--------------------------------------------------------------------------
function test_read_missing_value ( mode )

ncfile = 'foo.nc';
create_test_file(ncfile,mode);


sz = nc_varsize(ncfile,'test_2D');
count = sz;
input_data = 1:prod(count);
input_data = reshape(input_data,count);


input_data(1,1) = NaN;

nc_attput ( ncfile, 'test_2D', 'missing_value', -1 );
nc_varput ( ncfile, 'test_2D', input_data );

% Now read the data back.  Should have a NaN in position (1,1).
output_data = nc_varget ( ncfile, 'test_2D' );

if ~isnan(output_data(1,1))
    error('failed');
end
return






%--------------------------------------------------------------------------
% Read from a single precision dataset with a single precision scale factor.
% Should still produce single precision.
function test_read_floating_point_scale_factor ( mode )

if ischar(mode) && strcmp(mode,'hdf4')
    fprintf('\tFiltering out floating point scale factor test on HDF4.  ');
    return
end
ncfile = 'foo.nc';
create_test_file(ncfile,mode);


%
% Write some data, then put a scale factor of 2 and add offset of 1.  The
% data read back should be twice as large plus 1.


sz = nc_varsize(ncfile,'test_2D');
count = sz;
input_data = rand(1,prod(count));
input_data = reshape(input_data,count);


scale_factor = single(0.5);
add_offset = single(1.0);
nc_attput ( ncfile, 'test_2D_float', 'scale_factor', scale_factor );
nc_attput ( ncfile, 'test_2D_float', 'add_offset', add_offset );
nc_varput ( ncfile, 'test_2D_float', input_data );
output_data = nc_varget ( ncfile, 'test_2D_float' );

ddiff = abs(input_data - output_data);
if any( find(ddiff > 1e-6) )
    error('failed');
end

return



%--------------------------------------------------------------------------
% Test a fill value / missing value conflict.  The fill value should take 
% precedence.
function test_missing_value_and_fill_value ( mode)

if strcmp(mode,'netcdf4-classic')
    % This test should not be done on netcdf-4 files.
    return
end

ncfile = 'foo.nc';
create_test_file(ncfile,mode);


sz = nc_varsize(ncfile,'test_2D');
count = sz;
input_data = 1:prod(count);
input_data = reshape(input_data,count);


input_data(1,1) = NaN;

nc_attput ( ncfile, 'test_2D', '_FillValue', -1 );
nc_attput ( ncfile, 'test_2D', 'missing_value', -2 );
nc_varput ( ncfile, 'test_2D', input_data );


%
% Now read the data back.  Should have a NaN in position (1,1).
output_data = nc_varget ( ncfile, 'test_2D' );

if ~isnan(output_data(1,1))
    error('failed');
end
return

%--------------------------------------------------------------------------
function test_1D_all()


create_test_file('foo.nc',nc_clobber_mode);
nc_varput('foo.nc','test_1D',zeros(6,1));


%--------------------------------------------------------------------------
function test_bad_fill_value()

% The fill value really should match the datatype of the variable.  This
% used to error out.

warning('off','SNCTOOLS:nc_varput:badFillValueType');
warning('off','SNCTOOLS:nc_varget:mexnc:missingValueMismatch');
create_test_file('foo.nc',nc_clobber_mode);
nc_attput('foo.nc','test_1D','_FillValue','1');
nc_varput('foo.nc','test_1D',zeros(6,1));
warning('on','SNCTOOLS:nc_varput:badFillValueType');
warning('on','SNCTOOLS:nc_varget:mexnc:missingValueMismatch');


%--------------------------------------------------------------------------
function create_test_file ( ncfile, mode )


nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x', 4 );
nc_adddim(ncfile,'y', 6 );

% Add a singleton
varstruct.Name = 'test_singleton';
varstruct.Datatype = 'double';
varstruct.Dimension = [];

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'test_1D';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y' };

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'test_2D';
varstruct.Datatype = 'double';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'x', 'y' };
else
    varstruct.Dimension = { 'y', 'x' };
end

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'test_2D_float';
varstruct.Nctype = 'float';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'x', 'y' };
else
    varstruct.Dimension = { 'y', 'x' };
end

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'test_var3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'x' };

nc_addvar ( ncfile, varstruct );
return











%--------------------------------------------------------------------------
function run_hdf4_tests()
testroot = fileparts(mfilename('fullpath'));

ncfile = fullfile(testroot,'testdata/varput.hdf');
run_generic_tests(ncfile);

run_scaling_tests('hdf4');
return





%--------------------------------------------------------------------------
function run_negative_tests()
v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a'}
        fprintf('No negative tests run on %s...\n',v);
        return
end












