function run_snctools_tests()

pre_testing;
run_all_tests;
restore_state;


fprintf ('\nAll  possible tests for your configuration have been ');
fprintf ('run.\n\n' );

fprintf('If this is the first time you have run SNCTOOLS, then you should\n');
fprintf('know that several preferences have been set.\n');

getpref('SNCTOOLS')

fprintf('Only the ''PRESERVE_FVD'' preference is important for daily\n');
fprintf('use of SNCTOOLS.  Check the top-level README for details.  \n');
fprintf('Bye-bye.\n');

clear mex;

return




%--------------------------------------------------------------------------
function restore_state()

% restore all the warning states.
warning('on', 'SNCTOOLS:nc_archive_buffer:deprecated' );
warning('on', 'SNCTOOLS:nc_datatype_string:deprecated' );
warning('on', 'SNCTOOLS:nc_diff:deprecated' );
warning('on', 'SNCTOOLS:nc_getall:dangerous' );

        


%--------------------------------------------------------------------------
function run_all_tests()
% We test the mathworks backend, mexnc backend, and java backend.

test_tmw_backend;
test_mexnc_backend;
test_java_backend;



%--------------------------------------------------------------------------
function pre_testing()

% can we even run?
if ~exist('nc_attget','file')
	error('Cannot find NC_ATTGET.  Check the SNCTOOLS installation instructions again on how to set up your path.');
end
if ~exist('mexnc','file')
	error('Cannot find MEXNC.  Check the SNCTOOLS installation instructions again on how to set up your path.');
end

% clear the error state
lasterr(''); %#ok<LERR>

% make sure we can even run the tests.
mver = version('-release');
switch mver
    case {'11', '12'}
        error ('This version of MATLAB is too old, SNCTOOLS will not run.');
    case {'13'}
        error ('R13 is not supported in this release of SNCTOOLS');
end


% Disable these warning for the duration of the tests.
warning('off', 'SNCTOOLS:nc_archive_buffer:deprecated' );
warning('off', 'SNCTOOLS:nc_datatype_string:deprecated' );
warning('off', 'SNCTOOLS:nc_diff:deprecated' );
warning('off', 'SNCTOOLS:nc_getall:dangerous' );



%--------------------------------------------------------------------------
function test_java_backend()
fprintf('Testing java backend ...\n');

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a'}
		try
		    mexnc('inq_libvers');
			%  mexnc is available, so do not use java
            fprintf('\tjava netcdf-3 testing filtered out on ');
            fprintf('configurations where mexnc is available.\n ');
	    catch
            run_nc3_read_tests;
        end
                
    otherwise
        fprintf('\tnetcdf-3 java backend testing with local files filtered out on release %s\n', v);
end

v = version('-release');
switch(v)
    case {'2006a','2006b','2007a','2007b','2008a','2008b','2009a','2009b','2010a'}
        if netcdf4_capable
            run_nc4_read_tests;
            run_nc4_enhanced_read_tests;
        else
            fprintf('\tnetcdf-4 testing filtered out because  ');
            fprintf('netcdf-java is not available.\n');   
        end
                
    otherwise
        fprintf('\tnetcdf-4 java backend testing with local files filtered out on release %s\n', v);
end


run_http_tests;
run_grib_tests;
run_thredds_tests;

% Don't use java for opendap tests on 2012a
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a','2008b','2009a','2009b', ...
            '2010a','2010b','2011a','2011b'}
        run_opendap_tests;
    otherwise
        if getpref('SNCTOOLS','USE_NETCDF_JAVA',false)
            run_opendap_tests;
        else
            fprintf('\tOPeNDAP tests not run via java on %s when USE_NETCDF_JAVA preference is false.\n');
        end
end

%--------------------------------------------------------------------------
function test_mexnc_backend()

fprintf('Testing mexnc backend ...\n');
v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a','2007b','2008a'}
        if strcmp(computer,'PCWIN64')
            fprintf('\tmexnc not supported on PCWIN64, release %s.\n', v);
            return;
        end

		try
		    v = mexnc('inq_libvers');
		catch
		    fprintf('\tmexnc testing filtered out where mex-file is not available.\n');
		    return
		end
        run_nc3_read_tests;
        run_nc3_write_tests;
        if v(1) == '4'
            run_nc4_read_tests;
            run_nc4_write_tests;
        end
        
    otherwise
        fprintf('\tmexnc testing filtered out on release %s.\n', v);
        return
end


return


%--------------------------------------------------------------------------
function test_tmw_backend()

fprintf('Testing tmw backend ...\n');

v = version('-release');
if ~strcmp(v,'14')
    % HDF4 not supported on R14.
    run_hdf4_read_tests;

    % We don't bother with HDF4 writing anymore.
end

switch(v)
    case { '14','2006a','2006b','2007a','2007b','2008a'}
        fprintf('\ttmw testing filtered out on release %s...\n', v);
        return;
        
    case { '2008b','2009a','2009b','2010a'}
        run_nc3_read_tests;
        run_nc3_write_tests;
        
    otherwise
        run_nc3_read_tests;
        run_nc3_write_tests;
        run_nc4_read_tests;
        run_nc4_write_tests;
        run_nc4_enhanced_read_tests;
end

% Run opendap tests on 2012a
switch(v)
    case {'2006a','2006b','2007a','2007b','2008a','2008b','2009a','2009b', ...
            '2010a','2010b','2011a','2011b'}
        fprintf('\tOPeNDAP tests not run via TMW backend on %s.\n', v);
    otherwise
        run_opendap_tests;
end

return
%--------------------------------------------------------------------------
function run_nc3_write_tests()

test_nc_adddim;
test_nc_addvar(nc_clobber_mode);
test_nc_attput;
test_nc_varput;
test_nc_addhist;
test_nc_addnewrecs;
test_nc_create_empty;
test_nc_varrename;
test_nc_addrecs(nc_clobber_mode);
test_nc_cat;

%--------------------------------------------------------------------------
function run_nc4_write_tests()

mode = 'netcdf4-classic';
test_nc_adddim(mode);
test_nc_addvar(mode);
test_nc_attput(mode);
test_nc_varput(mode);
test_nc_addhist(mode);
test_nc_addnewrecs(mode);
test_nc_create_empty(mode);
test_nc_varrename(mode);
test_nc_addrecs(mode);
test_nc_cat(mode);

%--------------------------------------------------------------------------
function run_nc3_read_tests()

fprintf('\tTesting netcdf-3...\n');

test_nc_attget;
test_nc_vargetr;
test_nc_varget;
test_nc_datatype_string;
test_nc_iscoordvar;
test_nc_isunlimitedvar;
test_nc_getlast;
test_nc_isvar;
test_nc_varsize;
test_nc_getvarinfo;
test_nc_getbuffer;
test_nc_info;
test_nc_getdiminfo;
test_nc_isdim;
test_nc_dump;

%--------------------------------------------------------------------------
function run_nc4_read_tests()


mode = 'netcdf4-classic';
fprintf('\tTesting %s..\n',mode);

test_nc_attget(mode);
test_nc_vargetr(mode);
test_nc_varget(mode);
test_nc_datatype_string;
test_nc_iscoordvar(mode);
test_nc_isunlimitedvar(mode);

test_nc_getlast(mode);
test_nc_isvar(mode);
test_nc_varsize(mode);
test_nc_getvarinfo(mode);
test_nc_getbuffer(mode);
test_nc_info(mode);
test_nc_getdiminfo(mode);
test_nc_isdim(mode);

test_nc_dump('nc-4');

%--------------------------------------------------------------------------
function run_nc4_enhanced_read_tests()

v = version('-release');
switch(v)
    case {'2006a','2006b','2007a','2007b','2008a','2008b','2009a', ...
            '2009b','2010a','2010b','2011a'}
        fprintf('\tfiltering out enhanced-model tests on %s.\n', v);
        return;
end


mode = 'netcdf4-enhanced';
fprintf('\tTesting %s... \n',mode);

v = nc_info('example.nc');
if strcmp(v.Format,'netcdf-java')
    fprintf('\t\tFiltering out enhanced-model tests when netcdf-java is the read backend.\n');
    return;
end

test_nc_varget(mode);
test_nc_info(mode);
test_nc_attget(mode);


%--------------------------------------------------------------------------
function run_hdf4_read_tests()


fprintf('\tTesting hdf4...\n');

mode = 'hdf4';
test_nc_attget(mode);
test_nc_dump(mode);
test_nc_iscoordvar(mode);
test_nc_isunlimitedvar(mode);
test_nc_vargetr(mode);
test_nc_varget(mode);

return
%--------------------------------------------------------------------------
function run_hdf4_write_tests()


v = version('-release');
if strcmp(v,'14')
    fprintf('\thdf testing filtered out when the version is 14.');
    fprintf('There''s a known issue with no workaround yet.');
    return
end

fprintf('\tTesting hdf4...\n');

mode = 'hdf4';

test_nc_adddim(mode);
test_nc_addvar(mode);
test_nc_addrecs(mode);
test_nc_varput(mode);
test_nc_attput(mode);
test_nc_cat(mode);
test_nc_varrename(mode);
return
%--------------------------------------------------------------------------
function run_http_tests()
fprintf('\tTesting java/http...\n');

if getpref('SNCTOOLS','TEST_REMOTE',false) && getpref('SNCTOOLS','TEST_HTTP',false) && getpref('SNCTOOLS','USE_NETCDF_JAVA',false)
    test_nc_attget('http');
    test_nc_iscoordvar('http');
    test_nc_isvar('http');
    test_nc_info('http');
    test_nc_varget('http');
    test_nc_getvarinfo('http');
    return
end


fprintf('\t\tjava http testing filtered out when either of SNCTOOLS preferences ');
fprintf('\n\t\t''TEST_REMOTE'' or ''TEST_HTTP'' or ''USE_NETCDF_JAVA'' is false.\n');

return

%--------------------------------------------------------------------------
function run_thredds_tests()
fprintf('\tSkipping THREDDS tests...\n')
return
fprintf('\tTesting THREDDS... ');

v = version('-release');
switch(v)
	case '14'
        fprintf('\n\t\tTHREDDS testing filtered out on R14.\n');
        return
end

import ucar.nc2.*
if ~exist('NetcdfFile','class')
    fprintf('\n\t\tTHREDDS testing filtered out when netcdf-java is not available.\n');
    return
end


if getpref('SNCTOOLS','TEST_REMOTE',false)
    test_thredds_info;
    return
end

fprintf('\n');
fprintf('\t\tTHREDDS testing filtered out when SNCTOOLS preference ');
fprintf('\n\t\t''TEST_REMOTE'' is false.\n');

%--------------------------------------------------------------------------
function run_grib_tests()
fprintf('\tTesting GRIB... ');

v = version('-release');
switch(v)
	case '14'
        fprintf('\n\t\tGRIB2 testing filtered out on R14.\n');
        return

	case {'2006a','2006b','2007a', '2007b', '2008a'}
        fprintf('\n\t\tGRIB2 testing filtered out on %s.  Please read the README.\n', v);
        return

end

import ucar.nc2.*
if ~exist('NetcdfFile','class')
    fprintf('\n\t\tGRIB2 testing filtered out when netcdf-java is not available.\n');
    return
end


if ~getpref('SNCTOOLS','TEST_GRIB2',false)
	fprintf('\n\t\tjava GRIB testing filtered out when SNCTOOLS preferences ');
	fprintf('\n\t\t''TEST_GRIB2'' is false.\n');
	return
end
fprintf('\n');
test_nc_attget('grib');
test_nc_info('grib');
test_nc_dump('grib');
test_nc_varget('grib');


%--------------------------------------------------------------------------
function run_opendap_tests()

fprintf('\tTesting OPeNDAP...\n');

if ~(getpref('SNCTOOLS','TEST_REMOTE',false) && getpref('SNCTOOLS','TEST_OPENDAP',false))
    fprintf('\t\tjava opendap testing filtered out when SNCTOOLS ');
    fprintf('''TEST_REMOTE'' or ''TEST_OPENDAP'' preferences are false.\n');
    return
end

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a','2008b', ...
        '2009a','2009b','2010a','2010b','2011a','2011b' }
        import ucar.nc2.*
        if ~exist('NetcdfFile','class')
            fprintf('\t\tOPeNDAP testing filtered out when netcdf-java is not available.\n');
            return
        end
    
    otherwise
        % 
end

test_nc_info('opendap');
test_nc_varget('opendap');

% If we are using mexnc or if the release is 8b or higher, run this
% system-level test.
switch(v)
	case { '14','2006a','2006b','2007a','2007b','2008a'}
		;
	otherwise
        test_opendap_local_system;
end



return

