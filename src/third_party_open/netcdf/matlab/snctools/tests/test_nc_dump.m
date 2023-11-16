function test_nc_dump(mode)

if nargin < 1
	mode = 'nc-3';
end

fprintf('\t\tTesting NC_DUMP ...  ' );


switch(mode)
	case 'hdf'
		run_hdf4_tests;

	case 'nc-3'
		run_nc3_tests;

	case 'nc-4'
		run_nc4_tests;

	case 'grib'
		run_grib_tests;

end


fprintf('OK\n');




%--------------------------------------------------------------------------
function run_opendap_tests()

test_opendap_url;

return



%--------------------------------------------------------------------------
function test_opendap_url (  )
if getpref('SNCTOOLS','TEST_REMOTE',false) && ...
        getpref ( 'SNCTOOLS', 'TEST_OPENDAP', false ) 
    
    load('testdata/nc_dump.mat');
    % use data of today as the server has a clean up policy
    today = datestr(floor(now),'yyyymmdd');
    url = ['http://thredds.ucar.edu/thredds/dodsC/satellite/CTP/SUPER-NATIONAL_1km/current/SUPER-NATIONAL_1km_CTP_',today,'_0000.gini'];
	fprintf('\t\tTesting remote DODS access %s...  ', url );
    
    cmd = sprintf('nc_dump(''%s'')',url);
    act_data = evalc(cmd);
    
    if ~strcmp(act_data,d.opendap.unidata_motherlode)
        error('failed');
    end
    fprintf('OK\n');
else
	fprintf('Not testing NC_DUMP on OPeNDAP URLs.  Read the README for details.\n');	
end
return




%--------------------------------------------------------------------------
function outdata = post_process_dump(indata)
% R2010b allows us to collect more information about the netcdf file format
% which is reflected in the 1st line of the nc_dump output.  We need to
% remove this from the output in order to properly compare on 10a and
% below.

[t,r] = strtok(indata);
if strcmp(t,'NetCDF') || strcmp(t,'NetCDF-4') || strcmp(t,'HDF4') || strcmp(t,'NetCDF-3')
    indata = r;
end
[t,r] = strtok(indata);
if strcmp(t,'Classic')
    indata = r;
end
outdata = indata;

%--------------------------------------------------------------------------
function run_nc4_tests()

test_nc4file();
test_nc4_compressed;
run_common_files('nc_netcdf4_classic');


%--------------------------------------------------------------------------
function test_nc4_compressed()

testroot = fileparts(mfilename('fullpath'));

ncfile = 'deflate9.nc';
load(fullfile(testroot,'testdata','nc_dump.mat'));

copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
return
if ~strcmp(act_data,d.netcdf.nc4_compressed)
    error('failed');
end


return



%--------------------------------------------------------------------------
function test_nc4file() 


testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'tst_pres_temp_4D_netcdf4.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);

cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
return;

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.nc4);

if ~strcmp(act_data,exp_data)
    error('failed');
end

return




%--------------------------------------------------------------------------
function run_hdf4_tests()
dump_hdf4_tp;
run_common_files('hdf4');
dump_hdf4_example;




%--------------------------------------------------------------------------
function dump_hdf4_example()
% Dump the example HDF4 file that ships with MATLAB.
evalc('nc_dump(''example.hdf'');');

%--------------------------------------------------------------------------
function dump_hdf4_tp()
% dumps my temperature pressure file

if getpref('SNCTOOLS','PRESERVE_FVD',false);
	% don't bother.  The header dimension order is switched.
	return
end

testroot = fileparts(mfilename('fullpath'));

matfile = fullfile(testroot,'testdata','nc_dump.mat');
load(matfile);

hdffile = 'temppres.hdf'; %#ok<NASGU>
copyfile(fulltile(testroot,'testdata',hdffile),pwd);

act_data = evalc('nc_dump(hdffile);');

act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.hdf4.temppres);
if ~strcmp(act_data,exp_data)
    error('failed');
end











%--------------------------------------------------------------------------
function run_grib_tests ( )

test_grib2;

return



%--------------------------------------------------------------------------
function test_grib2()

% Test a GRIB2 file.  Requires java as far as I know.
testroot = fileparts(mfilename('fullpath'));
matfile = fullfile(testroot,'testdata','nc_dump.mat');
load(matfile);
origfile = fullfile(testroot,'testdata',...
    'ecmf_20070122_pf_regular_ll_pt_320_pv_grid_simple.grib2'); %#ok<NASGU>
grib_file = tempname;
copyfile(origfile,grib_file);
act_data = evalc('nc_dump(grib_file);'); %#ok<NASGU>

% So long as it didn't error out, I'm cool with that.

return






%--------------------------------------------------------------------------
function run_common_files(mode) 
% Just make sure that we don't error out.

testroot = fileparts(mfilename('fullpath'));

load(fullfile(testroot,'testdata','nc_dump.mat'));

switch(mode)
	case 'hdf4'
		ncfile = 'empty.hdf'; 
		copyfile(fullfile(testroot,'testdata',ncfile),pwd);
		cmd = sprintf('nc_dump(''%s'')',ncfile);
		evalc(cmd);

	case nc_clobber_mode
		ncfile = 'empty.nc'; 
		copyfile(fullfile(testroot,'testdata',ncfile),pwd);
		cmd = sprintf('nc_dump(''%s'')',ncfile);
		evalc(cmd);

	case 'nc_netcdf4_classic'
		ncfile = 'empty-4.nc'; 
		copyfile(fullfile(testroot,'testdata',ncfile),pwd);
		cmd = sprintf('nc_dump(''%s'')',ncfile);
		evalc(cmd);

end



return




%--------------------------------------------------------------------------
function run_nc3_tests() 
test_nc3_file_with_one_dimension;
test_nc3_empty;
test_nc3_singleton;
test_nc3_unlimited_variable;
test_nc3_variable_attributes;
test_nc3_one_fixed_size_variable;

run_common_files(nc_clobber_mode);

test_nc3_canonical();


%--------------------------------------------------------------------------
function test_nc3_canonical() 

pvd = getpref('SNCTOOLS','PRESERVE_FVD',true);
if pvd
    majority = 'f';
else
    majority = 'c';
end

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a','2008b', ...
            '2009a','2009b','2010a'}
        rel = 'R14';
    otherwise
        rel = 'R2010b';
end


testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'tst_pres_temp_4D_netcdf.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
exp_data = d.netcdf.classic3.(majority).(rel);

% Remove backend dependency
p = strfind(act_data,ncfile);
act_data = act_data(p:end);
p = strfind(exp_data,ncfile);
exp_data = exp_data(p:end);

if ~strcmp(act_data,exp_data)
    error('failed');
end

return
%--------------------------------------------------------------------------
function test_nc3_empty() 

testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'empty.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
return
act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.empty_file);
if ~strcmp(act_data,exp_data)
    error('failed');
end

return








%--------------------------------------------------------------------------
function test_nc3_file_with_one_dimension()


testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'just_one_dimension.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);
act_data = evalc(cmd);
return
act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.one_dimension);

if ~strcmp(act_data,exp_data)
    error('failed');
end
return



%--------------------------------------------------------------------------
function test_nc3_singleton()

testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'full.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
return
act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.singleton_variable);

if ~strcmp(act_data,exp_data)
    error('failed');
end
return





%--------------------------------------------------------------------------
function test_nc3_unlimited_variable()

testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'full.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
return
act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.unlimited_variable);

if ~strcmp(act_data,exp_data)
    error('failed');
end
return




%--------------------------------------------------------------------------
function test_nc3_variable_attributes()

testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'full.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
return
act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.variable_attributes);

if ~strcmp(act_data,exp_data)
    error('failed');
end
return







%--------------------------------------------------------------------------
function test_nc3_one_fixed_size_variable()

testroot = fileparts(mfilename('fullpath'));
load(fullfile(testroot,'testdata','nc_dump.mat'));

ncfile = 'just_one_fixed_size_variable.nc'; 
copyfile(fullfile(testroot,'testdata',ncfile),pwd);
cmd = sprintf('nc_dump(''%s'')',ncfile);

act_data = evalc(cmd);
return
act_data = post_process_dump(act_data);
exp_data = post_process_dump(d.netcdf.one_fixed_size_variable);

if ~strcmp(act_data,exp_data)
    error('failed');
end
return



