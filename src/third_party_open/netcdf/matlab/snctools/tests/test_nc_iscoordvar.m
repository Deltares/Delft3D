function test_nc_iscoordvar(mode)

if nargin < 1
    mode = 'nc-3';
end

fprintf('\t\tTesting NC_ISCOORDVAR...  ');

switch(mode)
	case 'nc-3'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/iscoordvar.nc');
		run_local_tests(ncfile);

	case 'hdf'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/iscoordvar.hdf');
		run_local_tests(ncfile);

	case 'netcdf4-classic'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/iscoordvar-4.nc');
		run_local_tests(ncfile);

	case 'http'
		test_coordvar_http;

end

run_backend_neutral_negative_tests;

fprintf('OK\n');



%--------------------------------------------------------------------------
function run_local_tests(ncfile)
test_coordvar(ncfile);
test_variable_not_present (ncfile);
test_not_a_coordvar (ncfile);
test_var_has_2_dims (ncfile);
test_singleton_variable (ncfile);

%--------------------------------------------------------------------------
function run_backend_neutral_negative_tests()

testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/empty.nc');

test_no_inputs;
test_only_one_input (ncfile);
test_not_netcdf_file;
test_empty_ncfile (ncfile);











%--------------------------------------------------------------------------
function test_no_inputs()
% Should error if no inputs.
try
	nc_iscoordvar;
catch %#ok<CTCH>
    return
end
error('failed');





%--------------------------------------------------------------------------
function test_only_one_input ( ncfile )
% Need at least two inputs.

try
	nc_iscoordvar ( ncfile );
catch %#ok<CTCH>
    return
end
error('failed');













%--------------------------------------------------------------------------
function test_not_netcdf_file (  )
% Must have a netCDF/HDF4 file, obviously.

try
	nc_iscoordvar ( 'test_iscoordvar.m', 't' );
catch %#ok<CTCH>
    return
end
error('failed');








%--------------------------------------------------------------------------
function test_empty_ncfile ( ncfile )
% Should error if the variable doesn't exist.
try
	nc_iscoordvar ( ncfile, 't' );
catch %#ok<CTCH>
    return
end
error('failed');












%--------------------------------------------------------------------------
function test_variable_not_present( ncfile )
% Should error if the variable doesn't exist.

try
	nc_iscoordvar ( ncfile, 'y' );
catch %#ok<CTCH>
    return
end
error('failed');










%--------------------------------------------------------------------------
function test_not_a_coordvar ( ncfile )
% Should return false if the variable's dimension doesn't have the same
% name.

b = nc_iscoordvar ( ncfile, 'u' );
if ( b ~= 0 )
	error('incorrect result.');
end
return






%--------------------------------------------------------------------------
function test_var_has_2_dims ( ncfile )
% By definition, a coordinate variable has just one dimension by the same
% name.

b = nc_iscoordvar ( ncfile, 's' );
if ( ~b )
	error ( 'incorrect result.\n' );
end
return







%--------------------------------------------------------------------------
function test_singleton_variable ( ncfile )
% By definition, a coordinate variable has one dimension by the same
% name.

yn = nc_iscoordvar ( ncfile, 'z' );
if ( yn )
	error ( 'incorrect result.\n'  );
end

return






%--------------------------------------------------------------------------
function test_coordvar ( ncfile )
% Positive test.

b = nc_iscoordvar ( ncfile, 's' );
if ~b
	error ( 'incorrect result.\n'  );
end

return









%--------------------------------------------------------------------------
function test_coordvar_http ()
% Positive test.

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';

bool = nc_iscoordvar(url,'xpos');
if ~bool
	error ( 'failed' );
end
return


