function test_nc_varget(mode)

if nargin < 1
    mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_VARGET ...  ' );

testroot = fileparts(mfilename('fullpath'));
switch(mode)
    case 'hdf4';
        run_hdf_tests;

    case 'grib'
        run_grib2_tests;

    case 'netcdf-3'
        ncfile = fullfile(testroot,'testdata/varget.nc');
        run_local_tests(ncfile,testroot);

		test_singleton_dimension(ncfile);


    case 'netcdf4-classic'
        ncfile = fullfile(testroot,'testdata/varget4.nc');
        run_local_tests(ncfile,testroot);

    case 'netcdf4-enhanced'
        run_nc4_enhanced;
        
    case 'opendap'
        run_opendap_tests;

end

fprintf('OK\n');


%--------------------------------------------------------------------------
function run_nc4_enhanced()
testroot = fileparts(mfilename('fullpath'));

ncfile = fullfile(testroot,'testdata/enhanced.nc');
test_enhanced_group_and_var_have_same_name(ncfile);

% Atomic datatypes.
ncfile = fullfile(testroot,'testdata/netcdf4_atomic.nc');
test_enhanced_atomic_datatypes(ncfile);

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a','2008b','2009a',...
            '2009b','2010a','2010b'};
        fprintf('\tfiltering out enhanced-model datatype tests on %s.\n', v);
        return;

end


% Strings
ncfile = fullfile(testroot,'testdata/moons.nc');
test_enhanced_vara_strings(ncfile);

% Enums
ncfile = fullfile(testroot,'testdata/tst_enum_data.nc');
test_1D_enum_var(ncfile);
test_1D_enum_vara(ncfile);
test_1D_enum_vars(ncfile);

% VLens
ncfile = fullfile(testroot,'testdata/tst_vlen_data.nc');
test_1D_vlen_var(ncfile);
test_1D_vlen_vara(ncfile);
test_1D_vlen_vars(ncfile);

% Opaques
ncfile = fullfile(testroot,'testdata/tst_opaque_data.nc');
test_1D_opaque_var(ncfile);
test_1D_opaque_vara(ncfile);
test_1D_opaque_vars(ncfile);

% Compounds
ncfile = fullfile(testroot,'testdata/tst_comp.nc');
test_1D_cmpd_var(ncfile);
test_1D_cmpd_vara(ncfile);
test_1D_cmpd_vars(ncfile);

%--------------------------------------------------------------------------
function test_enhanced_atomic_datatypes(ncfile)

% int64 datatypes should NOT be scaled into double precision
act_data = nc_varget(ncfile,'y');

exp_data = 0:9;
exp_data = int64(exp_data');

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_cmpd_vara(ncfile)

act_data = nc_varget(ncfile,'obs',1,2);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

exp_data = struct(...
    'day', int8([-99 20]'), ...
    'elev', int16([-99 6]'), ...
    'count', int32([  -99 3]'), ...
    'relhum', single([ -99 0.75]'), ...
    'time', [ -99 5000.01]', ...
    'category', uint8([ 255 200]'), ...
    'id', uint16([65535 64000]'), ...
    'particularity', uint32([ 4294967295 4220002000]'), ...
    'attention_span', int64([ (intmin('int64')) + 2 9000000000000000000]'));
  
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_cmpd_vars(ncfile)

act_data = nc_varget(ncfile,'obs',0,2,2);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

exp_data = struct(...
    'day', int8([15  20]'), ...
    'elev', int16([2  6]'), ...
    'count', int32([ 1  3]'), ...
    'relhum', single([0.5  0.75]'), ...
    'time', [3600.01  5000.01]', ...
    'category', uint8([0 200]'), ...
    'id', uint16([0 64000]'), ...
    'particularity', uint32([0 4220002000]'), ...
    'attention_span', int64([0 9000000000000000000]'));
  
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end


%--------------------------------------------------------------------------
function test_1D_cmpd_var(ncfile)

act_data = nc_varget(ncfile,'obs');

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

exp_data = struct(...
    'day', int8([15 -99 20]'), ...
    'elev', int16([2 -99 6]'), ...
    'count', int32([ 1 -99 3]'), ...
    'relhum', single([0.5 -99 0.75]'), ...
    'time', [3600.01 -99 5000.01]', ...
    'category', uint8([0 255 200]'), ...
    'id', uint16([0 65535 64000]'), ...
    'particularity', uint32([0 4294967295 4220002000]'), ...
    'attention_span', int64([0 (intmin('int64')) + 2 9000000000000000000]'));
  
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end


%--------------------------------------------------------------------------
function test_1D_opaque_var(ncfile)

act_data = nc_varget(ncfile,'raw_obs');

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

v = (1:11)';                                        exp_data{1} = uint8(v);
v = [170 187 204 221 238 255 238 221 204 187 170]'; exp_data{2} = uint8(v);
v = 255*ones(11,1);                                 exp_data{3} = uint8(v);
v = [202 254 186 190 202 254 186 190 202 254 186]'; exp_data{4} = uint8(v);
v = [207 13 239 172 237 12 175 224 250 202 222]';   exp_data{5} = uint8(v);

if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_opaque_vara(ncfile)

act_data = nc_varget(ncfile,'raw_obs',1,3);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

v = (1:11)';                                        exp_data{1} = uint8(v);
v = [170 187 204 221 238 255 238 221 204 187 170]'; exp_data{2} = uint8(v);
v = 255*ones(11,1);                                 exp_data{3} = uint8(v);
v = [202 254 186 190 202 254 186 190 202 254 186]'; exp_data{4} = uint8(v);
v = [207 13 239 172 237 12 175 224 250 202 222]';   exp_data{5} = uint8(v);

exp_data = exp_data(2:4);
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_opaque_vars(ncfile)

act_data = nc_varget(ncfile,'raw_obs',0,3,2);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

v = (1:11)';                                        exp_data{1} = uint8(v);
v = [170 187 204 221 238 255 238 221 204 187 170]'; exp_data{2} = uint8(v);
v = 255*ones(11,1);                                 exp_data{3} = uint8(v);
v = [202 254 186 190 202 254 186 190 202 254 186]'; exp_data{4} = uint8(v);
v = [207 13 239 172 237 12 175 224 250 202 222]';   exp_data{5} = uint8(v);

exp_data = exp_data(1:2:5);
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_vlen_vara(ncfile)

act_data = nc_varget(ncfile,'ragged_array',1,3);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

exp_data = { single(20:23)' single(30:32)' single(40:41)' };
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end
%--------------------------------------------------------------------------
function test_1D_vlen_var(ncfile)

act_data = nc_varget(ncfile,'ragged_array');

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

exp_data = { single(10:14)' single(20:23)' single(30:32)' single(40:41)' single(-999)};
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_vlen_vars(ncfile)

act_data = nc_varget(ncfile,'ragged_array',0,3,2);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

exp_data = { single(10:14)' single(30:32)' single(-999)};
if pfd
    exp_data =exp_data';
end

if ~isequal(act_data,exp_data)
    error('failed');
end
%--------------------------------------------------------------------------
function test_1D_enum_vars(ncfile)

act_data = nc_varget(ncfile,'primary_cloud',0,3,2);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');
if pfd
    exp_data = { 'Clear', 'Clear', 'Missing' }';
else
    exp_data = { 'Clear', 'Clear', 'Missing' };
end

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_enum_var(ncfile)

act_data = nc_varget(ncfile,'primary_cloud');
pfd = getpref('SNCTOOLS','PRESERVE_FVD');
if pfd
    exp_data = { 'Clear', 'Stratus', 'Clear', 'Cumulonimbus', 'Missing' }';
else
    exp_data = { 'Clear', 'Stratus', 'Clear', 'Cumulonimbus', 'Missing' };
end
if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_1D_enum_vara(ncfile)

act_data = nc_varget(ncfile,'primary_cloud',1,3);

pfd = getpref('SNCTOOLS','PRESERVE_FVD');
if pfd
    exp_data = { 'Stratus', 'Clear', 'Cumulonimbus' }';
else
    exp_data = { 'Stratus', 'Clear', 'Cumulonimbus' };
end
if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_enhanced_vara_strings(ncfile)

varname = 'ourano';
exp_data = {'Puck'};
act_data = nc_varget(ncfile,varname,[0 0],[1 1]);
if ~isequal(act_data,exp_data)
    error('failed');
end

act_data = nc_varget(ncfile,varname,[0 0],[1 2]);
pfd = getpref('SNCTOOLS','PRESERVE_FVD');
if pfd
    exp_data = {'Puck','Umbriel'};
else
    exp_data = {'Puck','Miranda'};
end
if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_enhanced_group_and_var_have_same_name(ncfile)

expData = (1:10)';
actData = nc_varget(ncfile,'/grp1/grp1');
ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.' );
end

%--------------------------------------------------------------------------
function test_bad_missing_value(testroot)


warning('off','SNCTOOLS:nc_varget:missingValueMismatch');
nc_varget([testroot filesep 'testdata' filesep 'badfillvalue.nc'],'z');
warning('on','SNCTOOLS:nc_varget:missingValueMismatch');


%--------------------------------------------------------------------------
function test_bad_fill_value(testroot)

warning('off','SNCTOOLS:nc_varget:fillValueMismatch');
nc_varget([testroot filesep 'testdata' filesep 'badfillvalue.nc'],'y');
warning('on','SNCTOOLS:nc_varget:fillValueMismatch');














%--------------------------------------------------------------------------
function test_1D_variable ( ncfile )
% Verify that a 1D variable read returns a column.

actData = nc_varget ( ncfile, 'test_1D' );

sz = size(actData);
if sz(1) ~= 6 && sz(2) ~= 1
    error('failed');
end




%--------------------------------------------------------------------------
function test_read_single_value_from_1d_variable(ncfile)

expData = 1.2;
actData = nc_varget ( ncfile, 'test_1D', 1, 1 );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.' );
end

return








%--------------------------------------------------------------------------
function test_read_single_value_from_2d_variable(ncfile)

expData = 1.5;
actData = nc_varget ( ncfile, 'test_2D', [2 2], [1 1] );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error('input data ~= output data.');
end

return




%--------------------------------------------------------------------------
function test_read3x2hyperslabFrom2dVariable ( ncfile )

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = [0.8 0.9; 1.4 1.5; 2.0 2.1];
else
    expData = [0.8 1.4; 0.9 1.5; 1.0 1.6];
end
actData = nc_varget ( ncfile, 'test_2D', [1 1], [3 2] );

if ndims(actData) ~= 2
    error ( 'rank of output data was not correct' );
end
if numel(actData) ~= 6
    error ( 'size of output data was not correct' );
end
ddiff = abs(expData(:) - actData(:));
if any( find(ddiff > eps) )
    error ( 'input data ~= output data ' );
end

return






%--------------------------------------------------------------------------
function test_stride_with_negative_count ( ncfile )

expData = [0.1 1.3; 0.3 1.5; 0.5 1.7];

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
actData = nc_varget(ncfile,'test_2D',[0 0],[-1 -1],[2 2] );

if ndims(actData) ~= 2
    error ( 'rank of output data was not correct' );
end
if numel(actData) ~= 6
    error ( 'count of output data was not correct' );
end
ddiff = abs(expData(:) - actData(:));
if any( find(ddiff > eps) )
    error ( 'input data ~= output data ' );
end

return







%--------------------------------------------------------------------------
function test_zero_size ( ncfile )
% If the variable is empty, then we shouldn't necessarily error out.

expSize = [0 4];

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expSize = fliplr(expSize);
end
data = nc_varget(ncfile,'c');
actSize = size(data);
if ~isequal(actSize,expSize)
    error('Zero size mismatch.');
end






%--------------------------------------------------------------------------
function test_inf_count ( ncfile )
% If the count has Inf anywhere, treat that as meaning to "retrieve unto
% the end of the file.

expData = [0.1 1.3; 0.3 1.5; 0.5 1.7];

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
actData = nc_varget(ncfile,'test_2D',[0 0],[Inf Inf],[2 2] );

if ndims(actData) ~= 2
    error ( 'rank of output data was not correct' );
end
if numel(actData) ~= 6
    error ( 'count of output data was not correct' );
end
ddiff = abs(expData(:) - actData(:));
if any( find(ddiff > eps) )
    error ( 'input data ~= output data ' );
end

return







%--------------------------------------------------------------------
function test_read_singleton_variable(ncfile)


expData = 3.14159;
actData = nc_varget(ncfile,'test_singleton');

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return



%--------------------------------------------------------------------------
function test_readFullDoublePrecisionVariable ( ncfile )


expData = 1:24;
expData = reshape(expData,6,4) / 10;

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end

actData = nc_varget ( ncfile, 'test_2D' );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return




%--------------------------------------------------------------------------
function test_readStridedVariable ( ncfile )

expData = 1:24;
expData = reshape(expData,6,4) / 10;
expData = expData(1:2:3,1:2:3);
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end

actData = nc_varget ( ncfile, 'test_2D', [0 0], [2 2], [2 2] );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return





%--------------------------------------------------------------------------
function regression_NegSize ( ncfile )
% A negative size means to retrieve to the end along the given dimension.
expData = 1:24;
expData = reshape(expData,6,4) / 10;
sz = size(expData);
sz(2) = -1;
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
    sz = fliplr(sz);
end

actData = nc_varget ( ncfile, 'test_2D', [0 0], sz );

ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return


%--------------------------------------------------------------------------
function test_missing_value(ncfile)
% The last value should be nan.

actData = nc_varget ( ncfile, 'sst_mv' );


if ~isnan( actData(end) )
    nc_dump(ncfile, 'sst_mv' ) % show nc_dump to illustrate error
    error ('failed');
end

return

%--------------------------------------------------------------------------
function test_missing_value_nan(ncfile)
% Special case where the missing value is NaN itself.

actData = nc_varget ( ncfile, 'a' );


if ~isnan( actData(end) )
    error ('failed');
end

return

%--------------------------------------------------------------------------
function test_fill_value_nan_extend(ncfile)
% Special case where the fill value is NaN itself (on time series).

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a','2008b','2009a','2009b','2010a'}
        % cannot run on these releases without further modification.
        return
    otherwise
        % go ahead
end

copyfile(ncfile,'foo.nc');
ncfile = 'foo.nc';

info = nc_info(ncfile);

clear v;
if ~strcmp(info.Format,'HDF4')
    v.Name = 'time';
    v.Datatype = 'double';
    v.Dimension = { 'time' };
    nc_addvar ( ncfile, v );
end

clear v;

v.Name = 'time2';
v.Datatype = 'double';
v.Dimension = { 'time' };
v.Attribute.Name = '_FillValue';
v.Attribute.Value = NaN;
nc_addvar ( ncfile, v );

% Now extend the time variable
nc_varput(ncfile,'time',0);

% Now retrieve 'time2'.  The only value should be NaN
data = nc_varget(ncfile,'time2');
if ~isnan(data)
    error ( 'extended data not set with proper fill value');
end


%--------------------------------------------------------------------------
function test_fill_value_nan(ncfile)
% Special case where the fill value is NaN itself.

actData = nc_varget ( ncfile, 'b' );

if ~isnan( actData(end) )
    error('failed');
end

return

%--------------------------------------------------------------------------
function test_scaling ( ncfile )

expData = [32 32 32 32; 50 50 50 50; 68 68 68 68; ...
           86 86 86 86; 104 104 104 104; 122 122 122 122]';

if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
    
actData = nc_varget(ncfile,'temp');

if ~isa(actData,'double')
    error ( 'short data was not converted to double');
end
ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error ( 'input data ~= output data.\n'  );
end

return

%--------------------------------------------------------------------------
function test_datatype ( ncfile )

expData = -32767 * ones(4,6);


if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    expData = expData';
end
    
actData = nc_varget(ncfile,'test_2D_short');

if ~isa(actData,'double')
    error('failed');
end
ddiff = abs(expData - actData);
if any( find(ddiff > eps) )
    error('failed');
end

actData = nc_varget(ncfile,'test_2D_int',[0 0],[1 1]);
if ~isnan(actData)
    error('failed');
end
return




%--------------------------------------------------------------------------
function run_grib2_tests()


testroot = fileparts(mfilename('fullpath'));
origfile = fullfile(testroot,'testdata',...
    'ecmf_20070122_pf_regular_ll_pt_320_pv_grid_simple.grib2');
grib_file = tempname;
copyfile(origfile,grib_file);
test_read_grib_full_var_double_precision(grib_file);
test_read_grib_unity_var(grib_file);
test_read_grib_singleton_var(grib_file);
test_read_grib_single_value(grib_file);
test_read_grib_contiguous(grib_file);
test_read_grib_strided_var(grib_file);

return
%--------------------------------------------------------------------------
function test_read_grib_strided_var(gribfile)

start = [1 2 0 0 0]; count = [2 3 1 1 1]; stride = [2 2 1 1 1];

% Close enough :-)
expData = [7199 6645 237; 6075 5513 112];

if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    start = fliplr(start); count = fliplr(count); stride = fliplr(stride);
    expData = expData';
    expData = reshape(expData, [1 1 1 3 2]);
end

actData = nc_varget(gribfile,'Potential_vorticity',start,count,stride);
actData = round(actData*1e9);

if ~isequal(actData,expData)
    error('failed');
end
return
%--------------------------------------------------------------------------
function test_read_grib_contiguous(gribfile)

start = [1 2 0 0 0]; count = [2 3 1 1 1];

% Close enough :-)
expData = [7199 4388 6645; 3625 2257 6847];

if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    start = fliplr(start); count = fliplr(count);
    expData = expData';
    expData = reshape(expData,[1 1 1 3 2]);
end

actData = nc_varget(gribfile,'Potential_vorticity',start,count);
actData = round(actData*1e9);

if ~isequal(actData,expData)
    error('failed');
end
return
%--------------------------------------------------------------------------
function test_read_grib_single_value(gribfile)

actData = nc_varget(gribfile,'lat',1,1);
expData = 80;
if actData ~= expData
    error('failed');
end
return
%--------------------------------------------------------------------------
function test_read_grib_singleton_var(gribfile)
% 'latLonCoordSys' has no dimensions
actData = nc_varget(gribfile,'latLonCoordSys');
expData = '0';
if actData ~= expData
    error('failed');
end
return
%--------------------------------------------------------------------------
function test_read_grib_unity_var(gribfile)
% 'isentrope' has just a single value
actData = nc_varget(gribfile,'isentrope');
expData = 320;
if actData ~= expData
    error('failed');
end
return
%--------------------------------------------------------------------------
function test_read_grib_full_var_double_precision(gribfile)

actData = nc_varget(gribfile,'lon');
expData = 10*(0:35)';
if actData ~= expData
    error('failed');
end
return










%--------------------------------------------------------------------------
function run_hdf_tests()

test_hdf4_example;
test_hdf4_scaling;

%--------------------------------------------------------------------------
function test_hdf4_example()
% test the example file that ships with matlab
exp_data = hdfread('example.hdf','Example SDS');
act_data = nc_varget('example.hdf','Example SDS');

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    act_data = act_data';
end

if exp_data ~= act_data
    error('failed');
end


%--------------------------------------------------------------------------
function test_hdf4_scaling()
testroot = fileparts(mfilename('fullpath'));

hdffile = fullfile(testroot,'testdata','temppres.hdf');

act_data = nc_varget(hdffile,'temp',[0 0],[2 2]);
exp_data = 1.8*[32 32; 33 33] + 32;

if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
    act_data = act_data';
end

if exp_data ~= act_data
    error('failed');
end



%--------------------------------------------------------------------------
function run_local_tests(ncfile,testroot)

test_1D_variable ( ncfile );
test_read_single_value_from_1d_variable(ncfile);
test_read_single_value_from_2d_variable(ncfile);
test_read3x2hyperslabFrom2dVariable ( ncfile );
test_stride_with_negative_count ( ncfile );
test_inf_count ( ncfile );
test_zero_size ( ncfile );

test_read_singleton_variable ( ncfile );
test_readFullDoublePrecisionVariable ( ncfile );

test_readStridedVariable ( ncfile );
test_scaling(ncfile);
test_missing_value(ncfile);
test_missing_value_nan(ncfile);
test_fill_value_nan(ncfile);
test_fill_value_nan_extend(ncfile);
test_datatype(ncfile);

test_indices_are_cols(ncfile);
regression_NegSize(ncfile);

test_bad_fill_value(testroot);
test_bad_missing_value(testroot);
test_not_full_path;


v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b'}
        %
    otherwise
        test_nc_varget_neg(ncfile);
end
            

return






%--------------------------------------------------------------------------
function test_not_full_path()
% verify that we can read from a file that is on the matlab path.
v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b','2008a'}
        % no example.nc yet
        return;
    otherwise
        % It's enough that we do not error out.
        nc_varget('example.nc','temperature');
end
%--------------------------------------------------------------------------
function test_indices_are_cols(ncfile)

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    exp_data = [32 50; 32 50];
else
    exp_data = [32 32; 50 50];
end
act_data = nc_varget(ncfile,'temp',[0 0],[2 2]');
if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function run_opendap_tests()

test_readOpendapVariable;
test_1D_char_opendap_variable;
test_2D_char_opendap_variable;

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a'}
        fprintf('negative tests filtered out on release %s.', v);
    otherwise
        test_nc_varget_neg_opendap;
end
return


%--------------------------------------------------------------------------
function test_1D_char_opendap_variable ()

if ~getpref('SNCTOOLS','USE_NETCDF_JAVA', false)
    fprintf('\n\t\t\tOPeNDAP char var tests filtered out if USE_NETCDF_JAVA\n');
    fprintf('\t\t\tpreference not set.  Check the README.\n');
    return
end

% Should be 65 chars long.
url = 'http://dtvirt5.deltares.nl:8080/thredds/dodsC/opendap/test/matlab-ncread-error-classic-65.nc';
data = nc_varget(url,'str');
if numel(data) ~= 65
    error('failed');
end
return


%--------------------------------------------------------------------------
function test_2D_char_opendap_variable ()
% 2D strings seem to be treated differently.

% URL is no longer available.
% 10/31/2013
return

if ~getpref('SNCTOOLS','USE_NETCDF_JAVA', false)
    fprintf('\n\t\t\tOPeNDAP char var tests filtered out if USE_NETCDF_JAVA\n');
    fprintf('\t\t\tpreference not set.  Check the README.\n');
    return
end

pfd = getpref('SNCTOOLS','PRESERVE_FVD');

url = 'http://www.marine.csiro.au/dods/nph-dods/dods-data/test_data/test_1.nc';
info = nc_info(url);
actData = nc_varget(url,'uchar2');

expData = ['defghijklmn';
           'fghijklmnop';
           'hijklmnopqr';
           'jklmnopqrst';
           'lmnopqrstuv';
           'nopqrstuvwx';
           'pqrstuvwxyz';
           'rstuvwxyzab';
           'ccccccccccc';
           'ccccccccccc';
           'ccccccccccc';
           'ccccccccccc' ];
expData = num2cell(expData);

if pfd
    expData = expData';
end

if strcmp(info.Format,'netcdf-java')
    if ~isequal(actData,expData)
        error('failed');
    end
end

return


%--------------------------------------------------------------------------
function test_readOpendapVariable ()
    % use data of today as the server has a clean up policy
    today = datestr(floor(now),'yyyymmdd');
    url = ['http://thredds.ucar.edu/thredds/dodsC/satellite/CTP/SUPER-NATIONAL_1km/current/SUPER-NATIONAL_1km_CTP_',today,'_0000.gini'];
    
    % I have no control over what this value is, so we'll just assume it
    % is correct.
    nc_varget(url,'y',0,1);
return



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

