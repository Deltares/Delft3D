function test_nc_isunlimitedvar(mode)

if nargin < 1
	mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_ISUNLIMITEDVAR ...' );

testroot = fileparts(mfilename('fullpath'));

switch(mode)
	case 'netcdf-3'
		ncfile = fullfile(testroot, 'testdata/full.nc');
		run_all_tests(ncfile);
	case 'hdf4'
		ncfile = fullfile(testroot, 'testdata/full.hdf');
		run_all_tests(ncfile);
	case 'netcdf4-classic'
		ncfile = fullfile(testroot, 'testdata/full-4.nc');
        run_all_tests(ncfile);
end

fprintf('OK\n');

return










%--------------------------------------------------------------------------
function run_all_tests ( ncfile )
test_not_unlimited (ncfile);
test_1D_unlimited (ncfile);
test_2D_unlimited (ncfile);
test_no_such_var(ncfile);







%--------------------------------------------------------------------------
function test_2D_unlimited ( ncfile )

b = nc_isunlimitedvar ( ncfile, 't3' );
if ( ~b  )
	error('incorrect result.');
end

return














%--------------------------------------------------------------------------
function test_not_unlimited ( ncfile )

b = nc_isunlimitedvar ( ncfile, 's' );
if b
	error( 'incorrect result.');
end
return







%--------------------------------------------------------------------------
function test_1D_unlimited ( ncfile )

b = nc_isunlimitedvar ( ncfile, 't2' );
if ~b
	error( 'incorrect result.');
end
return





%--------------------------------------------------------------------------
function test_no_such_var(ncfile)

b = nc_isunlimitedvar ( ncfile, 'tt' );
if b 
    error('failed');
end
return










