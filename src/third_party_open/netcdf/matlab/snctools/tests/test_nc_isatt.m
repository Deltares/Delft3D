function test_nc_isatt ( )

global ignore_eids;
ignore_eids = getpref('SNCTOOLS','IGNOREEIDS',true);

fprintf ('Testing NC_ISATT...\n' );

test_mexnc_backend;
test_tmw_backend;
test_java_backend;


%--------------------------------------------------------------------------
function test_java_backend()

fprintf('\tTesting java backend ...\n');

run_http_tests;
run_grib2_tests;

v = version('-release');
switch(v)
    case '14'
        run_nc3_tests;
        
    case { '2006a','2006b','2007a','2007b','2008a'}
        run_nc3_tests;
        run_nc4_tests;
        
    case { '2008b', '2009a', '2009b', '2010a' }
        run_nc4_tests;
        
end


%--------------------------------------------------------------------------
function test_mexnc_backend()

fprintf('\tTesting mexnc backend ...\n');
v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a','2007b','2008a'}
		try
		    mexnc('inq_libvers');
		catch
		    fprintf('\t\tmexnc testing filtered out where mexnc mex-file not available.\n');
		    return
		end
        run_nc3_tests;
        
    otherwise
        fprintf('\t\tmexnc testing filtered out on release %s.\n', v);
        return
end


return
%--------------------------------------------------------------------------
function test_tmw_backend()

fprintf('\tTesting tmw backend ...\n');

run_hdf4_tests;

v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a','2007b','2008a'}
        fprintf('\t\ttmw testing filtered out on release %s... ', v);
        return;
        
    case { '2008b','2009a','2009b','2010a'}
        run_nc3_tests;
        
    otherwise
        run_nc3_tests;
        run_nc4_tests;
end



return










%--------------------------------------------------------------------------
function run_grib2_tests()

fprintf('\t\tRunning grib2 tests...  ');

testroot = fileparts(mfilename('fullpath'));
gribfile = fullfile(testroot,'testdata',...
    'ecmf_20070122_pf_regular_ll_pt_320_pv_grid_simple.grib2');
test_grib2_char(gribfile);
fprintf('OK\n');

return

%--------------------------------------------------------------------------
function test_grib2_char(gribfile)
if ~getpref('SNCTOOLS','TEST_GRIB2',false)
    fprintf('GRIB2 testing filtered out where SNCTOOLS preference ');
    fprintf('TEST_GRIB2 is set to false.\n');
    return
end
act_data = nc_isatt(gribfile,-1,'creator_name');
if ~act_data
    error('failed'); 
end
return

%--------------------------------------------------------------------------
function run_nc3_tests()

fprintf('\t\tRunning local netcdf-3 tests...');
testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/attget.nc');
run_local_tests(ncfile);
fprintf('OK\n');

return

%--------------------------------------------------------------------------
function run_hdf4_tests()

fprintf('\t\tRunning local HDF4 tests...');

testroot = fileparts(mfilename('fullpath'));
hfile = fullfile(testroot,'testdata/attget.hdf');
run_local_tests(hfile);    
fprintf('OK\n');

return


%--------------------------------------------------------------------------
function run_nc4_tests()

fprintf('\t\tRunning local netcdf-4 tests...');    
testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/attget-4.nc');
run_local_tests(ncfile);
fprintf('OK\n');

return






%--------------------------------------------------------------------------
function run_local_tests(ncfile)

test_present ( ncfile );
test_not_present ( ncfile );
test_global_att(ncfile);
return;


%--------------------------------------------------------------------------
function test_global_att(ncfile)
% Check for existence of a global attribute.

if ~nc_isatt(ncfile,nc_global,'test_double_att')
    error('failed');
end

%--------------------------------------------------------------------------
function run_http_tests()
% These tests are regular URLs, not OPeNDAP URLs.

if ~ ( getpref ( 'SNCTOOLS', 'TEST_REMOTE', false ) )
	fprintf('\tjava http backend testing filtered out when SNCTOOLS ');
    fprintf('''TEST_REMOTE'' preference is false.\n');
	return
end

fprintf('\t\tRunning http tests...');
test_present_attr_HTTP;
test_not_present_attr_HTTP;
fprintf('OK\n');

return







%--------------------------------------------------------------------------
function test_present_attr_HTTP ()

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';

bool = nc_isatt ( url, 'w', 'valid_range' );
if ~bool
	error ( 'failed' );
end
return


%--------------------------------------------------------------------------
function test_not_present_attr_HTTP ()

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';

bool = nc_isatt ( url, 'w', 'valid_range_49' );
if bool
	error ( 'failed' );
end
return


%--------------------------------------------------------------------------
function test_present ( ncfile )

bool = nc_isatt ( ncfile, 'x_db', 'test_double_att' );
if ~bool
	error('failed');
end

return


%--------------------------------------------------------------------------
function test_not_present ( ncfile )

bool = nc_isatt ( ncfile, 'x_db', 'test_double_att_49' );
if bool
	error('failed');
end

return

