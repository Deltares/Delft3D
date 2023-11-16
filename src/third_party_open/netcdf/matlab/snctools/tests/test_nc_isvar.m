function test_nc_isvar(mode)

if nargin < 1
	mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_ISVAR ...' );

testroot = fileparts(mfilename('fullpath'));

switch(mode)
	case 'netcdf-3'
		ncfile1 = fullfile(testroot,'testdata/empty.nc');
		ncfile2 = fullfile(testroot,'testdata/full.nc');
		run_all_tests(ncfile1,ncfile2);

	case 'netcdf4-classic'
		ncfile1 = fullfile(testroot,'testdata/empty-4.nc');
		ncfile2 = fullfile(testroot,'testdata/full-4.nc');
		run_all_tests(ncfile1,ncfile2);

	case 'http'
		run_http_tests;

end
fprintf('OK\n');




%--------------------------------------------------------------------------
function run_all_tests(emptyfile,regfile);
ncfile = emptyfile;
test_noArgs;
test_oneArg             ( ncfile );
test_tooManyArgs        ( ncfile );
test_varnameNotChar ;
test_notNetcdfFile;
test_emptyFile          ( ncfile );
test_dimsButNoVars      ( ncfile );

ncfile = regfile;
test_variableNotPresent ( ncfile );
test_variablePresent    ( ncfile );

return



%--------------------------------------------------------------------------
function test_noArgs()

try
	nc = nc_isvar; %#ok<NASGU>
	error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end










%--------------------------------------------------------------------------
function test_oneArg ( ncfile )

try
	nc = nc_isvar ( ncfile ); %#ok<NASGU>
	error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end










%--------------------------------------------------------------------------
function test_tooManyArgs ( ncfile )

try
	nc = nc_isvar ( ncfile, 'blah', 'blah2' ); %#ok<NASGU>
	error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end










%--------------------------------------------------------------------------
function test_varnameNotChar( )

ncfile = 'testdata/empty.nc';
try
	nc = nc_isvar ( ncfile, 5 ); %#ok<NASGU>
	error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end














%--------------------------------------------------------------------------
function test_notNetcdfFile ()

% test 5:  not a netcdf file
try
	nc = nc_isvar ( mfilename, 't' ); %#ok<NASGU>
    error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end











%--------------------------------------------------------------------------
function test_emptyFile ( ncfile )

yn = nc_isvar ( ncfile, 't' );
if ( yn == 1 )
	error('incorrectly classified.');
end
return











%--------------------------------------------------------------------------
function test_dimsButNoVars ( ncfile )

yn = nc_isvar ( ncfile, 't' );
if ( yn == 1 )
	error('incorrectly classified.');
end
return













%--------------------------------------------------------------------------
function test_variableNotPresent ( ncfile )


b = nc_isvar ( ncfile, 'y' );
if ( b ~= 0 )
	error('incorrect result.');
end
return











%--------------------------------------------------------------------------
function test_variablePresent ( ncfile )



b = nc_isvar ( ncfile, 's' );
if ( b ~= 1 )
	error('incorrect result.');
end
return



%--------------------------------------------------------------------------
function run_http_tests(testroot) %#ok<INUSD>

test_javaNcidHttp ;
return


%--------------------------------------------------------------------------
function test_javaNcidHttp ( )
% Ensure that NC_ISVAR works on an opened java file.

import ucar.nc2.dods.*     
import ucar.nc2.*          
                           
url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';
jncid = NetcdfFile.open(url);

b = nc_isvar ( jncid, 'time' );
if ( b ~= 1 )
	error('incorrect result.');
end
close(jncid);
return
