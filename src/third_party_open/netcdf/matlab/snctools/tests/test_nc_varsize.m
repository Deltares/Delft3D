function test_nc_varsize(mode)

if nargin < 1
	mode = 'nc-3';
end

fprintf('\t\tTesting NC_VARSIZE ...  ');

testroot = fileparts(mfilename('fullpath'));

switch(mode)
	case 'nc-3'
		ncfile = fullfile(testroot,'testdata/full.nc');
		run_local_tests(ncfile);
	case 'netcdf4-classic'
		ncfile = fullfile(testroot,'testdata/full-4.nc');
		run_local_tests(ncfile);
end

run_negative_tests;

fprintf('OK\n');
return



%--------------------------------------------------------------------------
function run_local_tests(ncfile)
test_singleton (ncfile);
test_1D (ncfile);
test_1D_unlimited_empty (ncfile);
test_2D (ncfile);



%--------------------------------------------------------------------------
function run_negative_tests()

v = version('-release');
switch(v)
	case{'14','2006a','2006b','2007a','2007b'}
	    fprintf('Some negative tests filtered out on version %s... ', v);
    otherwise
		test_nc_varsize_neg;
end


%--------------------------------------------------------------------------
function test_singleton ( ncfile )

varsize = nc_varsize ( ncfile, 's' );
if ( varsize ~= 1 )
	error ( 'varsize was not right.');
end
return









%--------------------------------------------------------------------------
function test_1D ( ncfile )

varsize = nc_varsize ( ncfile, 's' );
if ( varsize ~= 1 )
	error ( 'varsize was not right.');
end
return











%--------------------------------------------------------------------------
function test_1D_unlimited_empty ( ncfile )

varsize = nc_varsize ( ncfile, 't3' );
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    if ( varsize(1) ~= 1 ) && ( varsize(2) ~= 0 )
        error ( '%s:  varsize was not right.\n', mfilename );
    end
else
    if ( varsize(1) ~= 0 ) && ( varsize(2) ~= 1 )
        error ( '%s:  varsize was not right.\n', mfilename );
    end
end
return










%--------------------------------------------------------------------------
function test_2D ( ncfile )


varsize = nc_varsize ( ncfile, 'v' );
if ( varsize(1) ~= 1 ) && ( varsize(2) ~= 1 )
	error ( '%s:  varsize was not right.\n', mfilename );
end
return










