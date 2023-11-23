function test_nc_getlast(mode)

fprintf('\t\tTesting NC_GETLAST...  ');

% This first set of tests should all fail.
% Test:  No inputs.
% Test:  Too few inputs (one).
% Test:  Too many inputs (4).
% Test:  1st input is not character.
% Test:  2nd input is not character.
% Test:  3rd input is not numeric.
% Test:  1st input is not a netcdf file.
% Test:  2nd input is not a netcdf variable.
% Test:  2nd input is a netcdf variable, but not unlimited.
% Test:  Non-positive "num_records"
% Test:  Time series variables have data, but fewer than what was 
%           requested.
%
% This second set of tests should all succeed.
% Test:  Two inputs, should return the last record.
% Test:  Three valid inputs.
% Test:  Get everything

if nargin < 1
	mode = 'netcdf-3';
end

testroot = fileparts(mfilename('fullpath'));
switch(mode)
	case 'netcdf-3'
		emptyfile = fullfile(testroot,'testdata/empty.nc');
		regfile = fullfile(testroot,'testdata/getlast.nc');
		run_all_tests(emptyfile,regfile);
	case 'netcdf4-classic'
		emptyfile = fullfile(testroot,'testdata/empty-4.nc');
		regfile = fullfile(testroot,'testdata/getlast-4.nc');
		run_all_tests(emptyfile,regfile);
end



%--------------------------------------------------------------------------
function run_all_tests(emptyfile,regfile)
test_no_inputs;
test_too_few_inputs(emptyfile);
test_too_many_inputs(emptyfile);
test_first_input_not_char;
test_2nd_input_not_char(emptyfile);
test_3rd_input_not_numeric(emptyfile);
test_1st_input_not_netcdf;
test_2nd_input_not_netcdf_variable(emptyfile);
test_var_not_unlimited(regfile);
test_nonpositive_records(regfile);
test_too_few_records(regfile);

test_last_record(regfile);
test_last_few_records(regfile);
test_get_everything(regfile);

fprintf('OK\n');
return




%--------------------------------------------------------------------------
function test_no_inputs (  )

try
	nc_getlast;
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');








%--------------------------------------------------------------------------
function test_too_few_inputs ( ncfile )

try
	nc_getlast ( ncfile );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');





%--------------------------------------------------------------------------
function test_too_many_inputs ( ncfile )

try
	nc_getlast ( ncfile, 't1', 3, 4 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');






%--------------------------------------------------------------------------
function test_first_input_not_char (  )

try
	nc_getlast ( 0, 't1' );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.\n');





%--------------------------------------------------------------------------
function test_2nd_input_not_char ( ncfile )

try
	nc_getlast ( ncfile, 0 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');





%--------------------------------------------------------------------------
function test_3rd_input_not_numeric ( ncfile )

try
	nc_getlast ( ncfile, 't1', 'a' );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');





%--------------------------------------------------------------------------
function test_1st_input_not_netcdf ( )

try
	nc_getlast ( 'test_nc_getlast.m', 't1', 1 );
catch %#ok<CTCH>
    return
end

error('succeeded when it should have failed.');





%--------------------------------------------------------------------------
function test_2nd_input_not_netcdf_variable ( ncfile )

try
	nc_getlast ( ncfile, 't4', 1 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');





%--------------------------------------------------------------------------
function test_var_not_unlimited ( ncfile )

try
	nc_getlast ( ncfile, 'x', 1 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');






%--------------------------------------------------------------------------
function test_nonpositive_records( ncfile )

try
	nc_getlast ( ncfile, 't1', 0 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');






%--------------------------------------------------------------------------
function test_too_few_records ( ncfile )


try
	nc_getlast ( ncfile, 't1', 12 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');








%--------------------------------------------------------------------------
function test_last_record ( ncfile )

v = nc_getlast ( ncfile, 't1' );
if ( length(v) ~= 1 )
	error ( 'return value length was wrong' );
end
return




%--------------------------------------------------------------------------
function test_last_few_records ( ncfile )
v = nc_getlast ( ncfile, 't1', 7 );
if ( length(v) ~= 7 )
	error('return value length was wrong.');
end
return



%--------------------------------------------------------------------------
function test_get_everything ( ncfile )

v = nc_getlast ( ncfile, 't1', 10 );
if ( length(v) ~= 10 )
	error('return value length was wrong.');
end
return


