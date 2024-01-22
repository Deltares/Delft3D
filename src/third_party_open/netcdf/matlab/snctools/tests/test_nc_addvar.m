function test_nc_addvar(create_mode)

fprintf('\t\tTesting NC_ADDVAR ...  ');

switch(create_mode)
    case 'hdf4'
        run_local_tests(create_mode);
        
    case nc_clobber_mode
        run_local_tests(create_mode);
        
    case 'netcdf4-classic'
        run_local_tests(create_mode);
        run_nc4_specific_tests(create_mode);
end

fprintf('OK\n');

%--------------------------------------------------------------------------
function run_local_tests(create_mode)

ncfile = 'foo.nc';

test_no_inputs;
test_2nd_input_not_struct ( ncfile, create_mode );
test_empty_struct ( ncfile, create_mode );
test_singletons ( ncfile, create_mode );
test_fixed_dimension ( ncfile, create_mode );
test_unlimited ( ncfile, create_mode );
test_unlimited_plus_fixed ( ncfile, create_mode );
test_with_attributes ( ncfile, create_mode );
test_numeric_nctype ( ncfile, create_mode );
test_var_already_there ( ncfile, create_mode );
test_illegal_field_name ( ncfile, create_mode );

return







%--------------------------------------------------------------------------
function run_nc4_specific_tests(create_mode)

ncfile = 'foo4.nc';

%test_illegal_type(ncfile,'uint8',create_mode);
test_illegal_type(ncfile,'uint16',create_mode);
test_illegal_type(ncfile,'uint32',create_mode);
test_illegal_type(ncfile,'int64',create_mode);
test_illegal_type(ncfile,'uint64',create_mode);


test_1d_no_chunking(ncfile);
test_1d_chunking(ncfile);
test_2d_chunking(ncfile);
test_2d_bad_chunking(ncfile);
test_2d_shuffle(ncfile);
test_2d_deflate(ncfile);
test_2d_chunking_shuffle_deflate(ncfile);
test_nc4_fill_value(ncfile);

return



%--------------------------------------------------------------------------
function test_1d_no_chunking (ncfile)
% Create a 1D variable without chunking. 


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );

clear varstruct;
varstruct.Name = 'x';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x' };
varstruct.Storage = 'contiguous';
varstruct.Chunking = [];

nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo(ncfile,'x');


if ~isempty(v.Chunking) 
	error('1D contiguous size test failed');
end













%--------------------------------------------------------------------------
function test_1d_chunking (ncfile)
% Create a 1D variable with chunking. 


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );

clear varstruct;
varstruct.Name = 'x';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x' };
varstruct.Chunking = 10;

nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo(ncfile,'x');

if v.Chunking ~= 10
	error('1D chunking size test failed');
end











%--------------------------------------------------------------------------
function test_2d_chunking (ncfile)
% Create a 2D variable with chunking.


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );
nc_adddim ( ncfile, 'y', 100 );

clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y', 'x' };
varstruct.Chunking = [10 50];

nc_addvar ( ncfile, varstruct );

info = nc_info(ncfile);
if strcmp(info.Format,'netcdf-java')
    % netcdf-java can't return information about chunk size.
    return
end
v = nc_getvarinfo(ncfile,'z');

if (v.Chunking(1) ~= 10) || (v.Chunking(2) ~= 50)
	error('2D chunking size test failed');
end




%--------------------------------------------------------------------------
function test_2d_bad_chunking (ncfile)
% Negative test:  bad chunk size.


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );
nc_adddim ( ncfile, 'y', 100 );

clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y', 'x' };
varstruct.Chunking = [40 50 10];

try
	nc_addvar ( ncfile, varstruct );
catch %#ok<CTCH>
	return
end
error('test succeeded when it should have failed');




%--------------------------------------------------------------------------
function test_2d_shuffle (ncfile)
% Create a 2D variable with shuffle filter turned on.


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );
nc_adddim ( ncfile, 'y', 100 );

clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y', 'x' };
varstruct.Shuffle = 1;

nc_addvar ( ncfile, varstruct );

info = nc_info(ncfile);
if strcmp(info.Format,'netcdf-java')
    % netcdf-java can't return information about compression
    return
end

v = nc_getvarinfo(ncfile,'z');

if v.Shuffle ~= 1
	error('2D shuffle test failed');
end

if v.Deflate ~= 0
	error('2D deflate test failed');
end















%--------------------------------------------------------------------------
function test_2d_deflate (ncfile)
% Create a 2D variable with deflate filter turned on.


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );
nc_adddim ( ncfile, 'y', 100 );

clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y', 'x' };
varstruct.Deflate = 1;

nc_addvar ( ncfile, varstruct );
info = nc_info(ncfile);
if strcmp(info.Format,'netcdf-java')
    % netcdf-java can't return information about chunk size.
    return
end
v = nc_getvarinfo(ncfile,'z');

if v.Shuffle ~= 0
	error('2D shuffle test failed');
end

if v.Deflate ~= 1
	error('2D deflate test failed');
end




clear varstruct;
varstruct.Name = 'z2';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y', 'x' };
varstruct.Deflate = 5;

nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo(ncfile,'z2');

if v.Shuffle ~= 0
	error('2D shuffle test failed');
end

if v.Deflate ~= 5
	error('2D deflate test failed');
end










%--------------------------------------------------------------------------
function test_2d_chunking_shuffle_deflate (ncfile)
% Create a 2D variable with shuffle and deflate filters turned on.


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );
nc_adddim ( ncfile, 'y', 100 );

clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y', 'x' };
varstruct.Chunking = [10 50];
varstruct.Shuffle = 1;
varstruct.Deflate = 9;

nc_addvar ( ncfile, varstruct );

info = nc_info(ncfile);
if strcmp(info.Format,'netcdf-java')
    % netcdf-java can't return information about chunk size.
    return
end

v = nc_getvarinfo(ncfile,'z');

if (v.Chunking(1) ~= 10) || (v.Chunking(2) ~= 50)
	error('2D chunking size test failed');
end


if v.Shuffle ~= 1
	error('2D shuffle test failed');
end

if v.Deflate ~= 9
	error('2D deflate test failed');
end






%--------------------------------------------------------------------------
function test_nc4_fill_value (ncfile)
% Create a 2D variable with a fill value.


nc_create_empty (ncfile,nc_netcdf4_classic);
nc_adddim ( ncfile, 'x', 500 );
nc_adddim ( ncfile, 'y', 100 );

clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'y', 'x' };
varstruct.Attribute.Name = '_FillValue';
varstruct.Attribute.Value = -999;
nc_addvar ( ncfile, varstruct );
fv = nc_attget(ncfile,'z','_FillValue');
if (fv ~= -999)
    error('failed');
end

















%--------------------------------------------------------------------------
function test_no_inputs()
% Negative test, should error if no inputs.

try
    nc_addvar;
catch %#ok<CTCH>
	return
end
error ( '%s:  succeeded when it should have failed.\n', mfilename );















%--------------------------------------------------------------------------
function test_2nd_input_not_struct (ncfile,mode)
% Negative test, the 2nd parameter must be a struct.
nc_create_empty (ncfile,mode);

try
    nc_addvar ( ncfile, 5 );
catch %#ok<CTCH>
	return
end
error ( 'succeeded when it should have failed.');













%--------------------------------------------------------------------------
function test_empty_struct (ncfile,mode)
% Negative test, the struct has no Name field.
nc_create_empty (ncfile,mode);
clear varstruct;
varstruct = struct([]);
try
    nc_addvar ( ncfile, varstruct );
catch %#ok<CTCH>
	return
end
error ( 'succeeded when it should have failed.' );










%--------------------------------------------------------------------------
function test_singletons (ncfile,mode)
% Create singletons of different datatypes.

if exist(ncfile,'file')
    delete(ncfile);
end
nc_create_empty (ncfile,mode);
clear varstruct;
varstruct.Name = 'x';
varstruct.Datatype = 'double';
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'x' );
if ~strcmp(v.Datatype,'double')
    error ( '%s:  data type was wrong.\n', mfilename );
end
if ( v.Size ~= 1 ) 
    error ( '%s:  data size was wrong.\n', mfilename );
end
if ( ~isempty(v.Dimension) ) 
    error ( '%s:  dimensions were wrong.\n', mfilename );
end


clear varstruct;
varstruct.Name = 'x2';
varstruct.Datatype = 'float';
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'x2' );
if ~strcmp(v.Datatype,'single')
    error ( 'failed' );
end

varstruct.Name = 'x3';
varstruct.Datatype = 'int';
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'x3' );
if ~strcmp(v.Datatype,'int32')
    error ( '%s:  data type was wrong.\n', mfilename );
end

clear varstruct;
varstruct.Name = 'x4';
varstruct.Datatype = 'short';
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'x4' );
if ~strcmp(v.Datatype,'int16')
    error ( '%s:  data type was wrong.\n', mfilename );
end

clear varstruct;
varstruct.Name = 'x5';
varstruct.Datatype = 'byte';
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'x5' );
if ~strcmp(v.Datatype,'int8')
    error ( '%s:  data type was wrong.\n', mfilename );
end

clear varstruct;
varstruct.Name = 'x6';
varstruct.Datatype = 'char';
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'x6' );
if ~strcmp(v.Datatype,'char')
    error ( '%s:  data type was wrong.\n', mfilename );
end

return







%--------------------------------------------------------------------------
function test_fixed_dimension (ncfile,mode)
% Create a variable with a fixed length dimension.

nc_create_empty (ncfile,mode);
nc_adddim(ncfile,'x',5);

clear varstruct;
varstruct.Name = 'y';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x' };
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'y' );
if ~strcmp(v.Datatype,'double')
    error ( '%s:  data type was wrong.\n', mfilename );
end
if any(v.Size - 5)
    error ( '%s:  variable size was wrong.\n', mfilename );
end
if ( length(v.Dimension) ~= 1 ) 
    error ( '%s:  dimensions were wrong.\n', mfilename );
end
if ( ~strcmp(v.Dimension{1}, 'x' ) ) 
    error ( '%s:  dimensions were wrong.\n', mfilename );
end

return










%--------------------------------------------------------------------------
function test_unlimited(ncfile,mode)
% Create an unlimited variable.

nc_create_empty (ncfile,mode);
nc_adddim ( ncfile, 'x', 0 );
clear varstruct;
varstruct.Name = 'y';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x' };
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'y' );
if ~strcmp(v.Datatype,'double')
    error ( 'failed' );
end
if (v.Size ~= 0)
    error ( 'failed' );
end
if ( ~v.Unlimited)
    error ( 'failed' );
end

return












%--------------------------------------------------------------------------
function test_unlimited_plus_fixed (ncfile,mode)
% Create a variable with both an unlimited and limited dimension.
nc_create_empty (ncfile,mode);
nc_adddim ( ncfile, 'x', 0 );
nc_adddim(ncfile,'y',5);

clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'y', 'x' };
else
    varstruct.Dimension = { 'x', 'y' };
end

nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'z' );
if ~strcmp(v.Datatype,'double')
    error('failed')
end

if getpref('SNCTOOLS','PRESERVE_FVD',false)
    if (v.Size(2) ~= 0) && (v.Size(1) ~= 5 )
        error ( '%s:  %s:  variable size was wrong.\n', mfilename, testid );
    end
else
    if (v.Size(1) ~= 0) && (v.Size(2) ~= 5 )
        error ( '%s:  %s:  variable size was wrong.\n', mfilename, testid );
    end
end

if ( ~v.Unlimited )
    error ( '%s:  %s:  unlimited classifaction was wrong.\n', mfilename, testid );
end

return









%--------------------------------------------------------------------------
function test_with_attributes (ncfile,mode)
% Create a variable and attributes.
nc_create_empty (ncfile,mode);
nc_adddim ( ncfile, 'x', 0 );
nc_adddim(ncfile,'y',5);
clear varstruct;
varstruct.Name = 'z';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x' };
varstruct.Attribute(1).Name = 'test';
varstruct.Attribute(1).Value = 'blah';
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'z' );
if ~strcmp(v.Datatype,'double')
    error ( 'failed');
end
if (v.Size(1) ~= 0) && (v.Size(2) ~= 5 )
    error ( '%s:  variable size was wrong.\n', mfilename );
end
if ( ~v.Unlimited)
    error ( '%s:  unlimited classifaction was wrong.\n', mfilename );
end
if ( length(v.Attribute) ~= 1)
    error ( '%s:  number of attributes was wrong.\n', mfilename );
end

return









%--------------------------------------------------------------------------
function test_numeric_nctype (ncfile,mode)
% Create a variable where the datatype is given as numeric.

nc_create_empty (ncfile,mode);
nc_adddim ( ncfile, 'x', 5 );
clear varstruct;
varstruct.Name = 'y';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x' };
nc_addvar ( ncfile, varstruct );

v = nc_getvarinfo ( ncfile, 'y' );
if ~strcmp(v.Datatype,'double')
    error ( 'failed')
end
if any(v.Size - 5)
    error ( '%s:  variable size was wrong.\n', mfilename );
end
if ( length(v.Dimension) ~= 1 ) 
    error ( '%s:  dimensions were wrong.\n', mfilename );
end
if ( ~strcmp(v.Dimension{1}, 'x' ) ) 
    error ( '%s:  dimensions were wrong.\n', mfilename );
end




return











%--------------------------------------------------------------------------
function test_var_already_there (ncfile,mode)
% Negative test, the specified variable already exists.


nc_create_empty (ncfile,mode);
nc_adddim ( ncfile, 'x', 5 );

clear varstruct;
varstruct.Name = 'y';
varstruct.Datatype = 'double';
varstruct.Dimension = { 'x' };
nc_addvar ( ncfile, varstruct );
try
    nc_addvar ( ncfile, varstruct );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed');



%--------------------------------------------------------------------------
function test_illegal_type(ncfile,dtype,mode)
% Negative test, the datatype is not legal.

nc_create_empty (ncfile,mode);
nc_adddim ( ncfile, 'x', 5 );

clear varstruct;
varstruct.Name = 'x';
varstruct.Datatype = dtype;
varstruct.Dimension = { 'x' };

try
    nc_addvar ( ncfile, varstruct );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed');




%--------------------------------------------------------------------------
function test_illegal_field_name (ncfile,mode)
% Should produce a warning, which we want to suppress.  The varstruct
% has an unrecognized field.

warning('off','snctools:addvar:unrecognizedFieldName');
nc_create_empty (ncfile,mode);
nc_adddim ( ncfile, 'x', 5 );

clear varstruct;
varstruct.Name = 'y';
varstruct.nnccttyyppee = { 'x' };
varstruct.Dimension = { 'x' };
nc_addvar ( ncfile, varstruct );

warning('on','snctools:addvar:unrecognizedFieldName');



