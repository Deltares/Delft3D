function test_nc_vargetr(mode)

if nargin < 1
    mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_VARGETR ...  ' );

testroot = fileparts(mfilename('fullpath'));
switch(mode)
    case 'hdf4';
        ncfile = fullfile(testroot,'testdata/fillvalue_scaling.hdf');
        run_local_tests(ncfile);

    case 'netcdf-3'
        ncfile = fullfile(testroot,'testdata/fillvalue_scaling.classic.nc');
        run_local_tests(ncfile);

        ncfile = fullfile(testroot,'testdata/varget.nc');
		test_singleton_dimension(ncfile);

    case 'netcdf4-classic'
        ncfile = fullfile(testroot,'testdata/fillvalue_scaling.classic.nc4');
        run_local_tests(ncfile);

end

fprintf('OK\n');


%--------------------------------------------------------------------------
function run_local_tests(ncfile)

test_double(ncfile);
test_single(ncfile);
test_int(ncfile);
test_short(ncfile);
test_byte(ncfile);

%--------------------------------------------------------------------------
function test_singleton_dimension(ncfile)
% Verify the size of data being read from a classic file when the variable
% has an unlimited dimension with extent of 1.

% Write a single timestep into the variable.
copyfile(ncfile,'foo.nc');
pv = getpref('SNCTOOLS','PRESERVE_FVD',false);
if pv
	exp_data = reshape(1:24,[6 4]);
    nc_varput('foo.nc','d',exp_data, [0 0 0], [6 4 1])
else
	exp_data = reshape(1:24,[1 4 6]);
    nc_varput('foo.nc','d',exp_data);
end


act_data = nc_varget('foo.nc','d');
if ~isequal(act_data,exp_data)
	error('failed');
end

act_data = nc_vargetr('foo.nc','d');
if ~isequal(act_data,exp_data)
	error('failed');
end

%--------------------------------------------------------------------------
function test_double(ncfile)

varname = 'test_double';
data = nc_vargetr(ncfile,varname);

info = nc_getvarinfo(ncfile,varname);
sz = info.Size;

exp_data = 1:24;

pvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
if ~pvd
    exp_data = reshape(exp_data,fliplr(sz))';
end
exp_data = reshape(exp_data,sz);

exp_data(1) = -1;  % This would be NaN via NC_VARGET

if ~isequal(data,exp_data)
    error('failed');
end



%--------------------------------------------------------------------------
function test_single(ncfile)

varname = 'test_float';
data = nc_vargetr(ncfile,varname);

info = nc_getvarinfo(ncfile,varname);
sz = info.Size;

exp_data = single(1:24);

pvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
if pvd
    exp_data = reshape(exp_data,sz);
else
    exp_data = reshape(exp_data,fliplr(sz));
    exp_data = exp_data';
end
exp_data(1) = -1;  % This would be NaN via NC_VARGET

if ~isequal(data,exp_data)
    error('failed');
end



%--------------------------------------------------------------------------
function test_int(ncfile)

varname = 'test_int';
data = nc_vargetr(ncfile,varname);

info = nc_getvarinfo(ncfile,varname);
sz = info.Size;

exp_data = int32(1:24);

pvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
if pvd
    exp_data = reshape(exp_data,sz);
else
    exp_data = reshape(exp_data,fliplr(sz));
    exp_data = exp_data';
end
exp_data(1) = -1;  % This would be NaN via NC_VARGET

if ~isequal(data,exp_data)
    error('failed');
end




%--------------------------------------------------------------------------
function test_short(ncfile)

varname = 'test_short';
data = nc_vargetr(ncfile,varname);

info = nc_getvarinfo(ncfile,varname);
sz = info.Size;

exp_data = int16(1:24);

pvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
if pvd
    exp_data = reshape(exp_data,sz);
else
    exp_data = reshape(exp_data,fliplr(sz));
    exp_data = exp_data';
end
exp_data(1) = -1;  % This would be NaN via NC_VARGET

if ~isequal(data,exp_data)
    error('failed');
end







%--------------------------------------------------------------------------
function test_byte(ncfile)

varname = 'test_byte';
data = nc_vargetr(ncfile,varname);

info = nc_getvarinfo(ncfile,varname);
sz = info.Size;

exp_data = int8(1:24);

pvd = getpref('SNCTOOLS','PRESERVE_FVD',false);
if pvd
    exp_data = reshape(exp_data,sz);
else
    exp_data = reshape(exp_data,fliplr(sz));
    exp_data = exp_data';
end
exp_data(1) = -1;  % This would be NaN via NC_VARGET

if ~isequal(data,exp_data)
    error('failed');
end







