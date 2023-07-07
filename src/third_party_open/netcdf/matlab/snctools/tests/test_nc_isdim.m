function test_nc_isdim(mode)

if nargin < 1
	mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_ISDIM ...' );

testroot = fileparts(mfilename('fullpath'));

switch(mode)
	case 'netcdf-3'
		ncfile = fullfile(testroot,'testdata/full.nc');
		run_all_tests(ncfile);

	case 'netcdf4-classic'
		ncfile = fullfile(testroot,'testdata/full-4.nc');
		run_all_tests(ncfile);


end
fprintf('OK\n');




%--------------------------------------------------------------------------
function run_all_tests(ncfile)

test_true(ncfile);
test_false(ncfile);









%--------------------------------------------------------------------------
function test_true ( ncfile )



b = nc_isdim ( ncfile, 's' );
if ( b ~= 1 )
	error('failed');
end
return

%--------------------------------------------------------------------------
function test_false ( ncfile )



b = nc_isdim ( ncfile, 'z' );
if ( b ~= 0 )
	error('failed');
end
return

