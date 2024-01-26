function test_nc_addnewrecs(mode)
% TEST_NC_ADDNEWRECS
%
% Relies on nc_addvar, nc_getvarinfo
%
% Test run include
%    No inputs, should fail.
%    One inputs, should fail.
%    3.  Two inputs, 2nd is not a structure, should fail.
%    4.  Two inputs, 2nd is an empty structure, should fail.
%    5.  Two inputs, 2nd is a structure with bad variable names, should fail.
%    6.  Three inputs, 3rd is non existant unlimited dimension.
%    7.  Two inputs, write to two variables, should succeed.
%    8.  Two inputs, write to two variables, one of them not unlimited, should fail.
%    9.  Try to write to a file with no unlimited dimension.
%   10.  Do two successive writes.  Should succeed.
%   11.  Do two successive writes, but on the 2nd write let the coordinate
%        variable overlap with the previous write.  Should still succeed,
%        but fewer datums will be written out.
%   12.  Do two successive writes, but with the same data.  Should 
%        return an empty buffer, but not fail
% Test 13:  Add a single record.  This is a corner case.
% Test 14:  Add a single record, trailing singleton dimensions.

fprintf('\t\tTesting NC_ADDNEWRECS ...  ' );
if nargin < 1
    ncfile = 'foo.nc';
    mode = nc_clobber_mode;
else
    ncfile = 'foo4.nc';
end

run_all_tests(ncfile,mode);
fprintf('OK\n');
return



%--------------------------------------------------------------------------
function run_all_tests(ncfile,mode)


create_ncfile(ncfile,mode);

test_only_one_input ( ncfile );
test_003 ( ncfile );
test_004 ( ncfile );
test_005 ( ncfile );
test_006 ( ncfile );
test_two_inputs ( ncfile );
test_008 ( ncfile );

% test_009 makes a new file

test_009(ncfile,mode);
test_010(ncfile,mode);
test_011(ncfile,mode);

test_012(ncfile,mode);
test_013(ncfile,mode)
test_014(ncfile,mode);

run_negative_tests(ncfile,mode);

return








%-------------------------------------------------------------------------------
function run_negative_tests(ncfile,mode)

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b'}
        fprintf('\tNo negative tests on %s.  ' , v);
        return
    otherwise
        test_nc_addnewrecs_neg(ncfile,mode);
end

        
%-------------------------------------------------------------------------------
function create_ncfile ( ncfile, mode )

if exist(ncfile,'file')
    delete(ncfile);
end

nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',4);
nc_adddim(ncfile,'y',1);
nc_adddim(ncfile,'z',1);
nc_adddim(ncfile,'time',0);


% Add a variable along the time dimension
varstruct.Name = 'test_var';
varstruct.Nctype = 'float';
varstruct.Dimension = { 'time' };
varstruct.Attribute(1).Name = 'long_name';
varstruct.Attribute(1).Value = 'This is a test';
varstruct.Attribute(2).Name = 'short_val';
varstruct.Attribute(2).Value = int16(5);

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'test_var2';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'time' };

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'trailing_singleton';
varstruct.Nctype = 'double';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'y', 'z', 'time' };
else
    varstruct.Dimension = { 'time', 'z', 'y' };
end

nc_addvar ( ncfile, varstruct );


% Don't do this if HDF4.  We already have the coordinate variable there.
if ~((nargin == 2) && ischar(mode) && strcmp(mode,'hdf4'))
    clear varstruct;
    varstruct.Name = 'time';
    varstruct.Nctype = 'double';
    varstruct.Dimension = { 'time' };
    
    nc_addvar ( ncfile, varstruct );
end


clear varstruct;
varstruct.Name = 'test_var3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'x' };

nc_addvar ( ncfile, varstruct );

return







%---------------------------------------------------------------------------
function test_only_one_input ( ncfile )

%
% Try one inputs
try
    nc_addnewrecs ( ncfile );
catch %#ok<CTCH>
	return
end

error('test failure');











%---------------------------------------------------------------------------
function test_003 ( ncfile )

% Try with 2nd input that isn't a structure.
try
    nc_addnewrecs ( ncfile, [] );
catch
    return
end
error('test failure');












%---------------------------------------------------------------------------
function test_004 ( ncfile )


% Try with 2nd input that is an empty structure.
try
    nc_addnewrecs ( ncfile, struct([]) );
catch
    return
end
error('test failure');











%---------------------------------------------------------------------------
function test_005 ( ncfile )

%
% Try a structure with bad names
input_data.a = [3 4];
input_data.b = [5 6];
try
    nc_addnewrecs ( ncfile, input_data );
catch %#ok<CTCH>
	return
end

error('test failure');











%---------------------------------------------------------------------------
function test_006 ( ncfile )

%
% Try good data with a bad record variable name
input_data.test_var = [3 4]';
input_data.test_var2 = [5 6]';
try
    nc_addnewrecs ( ncfile, input_data, 'bad_time' );
catch %#ok<CTCH>
    return
end
error('nc_addnewrecs succeeded with a badly named record variable, should have failed');










%---------------------------------------------------------------------------
function test_two_inputs ( ncfile )

%
% Try a good test.
before = nc_getvarinfo ( ncfile, 'test_var2' );


clear input_buffer;
input_buffer.test_var = single([3 4 5]');
input_buffer.test_var2 = [3 4 5]';
input_buffer.time = [1 2 3]';

nc_addnewrecs ( ncfile, input_buffer );

after = nc_getvarinfo ( ncfile, 'test_var2' );
if ( (after.Size - before.Size) ~= 3 )
    error ( '%s:  nc_addnewrecs failed to add the right number of records.', mfilename );
end
return











%---------------------------------------------------------------------------
function test_008 ( ncfile )

%
% Try writing to a fixed size variable


input_buffer.test_var = single([3 4 5]');
input_buffer.test_var2 = [3 4 5]';
input_buffer.test_var3 = [3 4 5]';

try
    nc_addnewrecs ( ncfile, input_buffer );
catch %#ok<CTCH>
    return
end
error('nc_addnewrecs succeeded on writing to a fixed size variable, should have failed.');













%---------------------------------------------------------------------------
function test_009(ncfile,mode)


nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',4);

clear varstruct;
varstruct.Name = 'test_var3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'x' };

nc_addvar ( ncfile, varstruct );


input_buffer.time = [1 2 3]';
try
    nc_addnewrecs ( ncfile, input_buffer );
catch %#ok<CTCH>
    return
end

error('nc_addnewrecs passed when writing to a file with no unlimited dimension');












%---------------------------------------------------------------------------
function test_010(ncfile,mode)

nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',4);
nc_adddim(ncfile,'time',0);
switch(mode)
    case 'hdf4'
        %
    otherwise
        
        clear varstruct;
        varstruct.Name = 'time';
        varstruct.Datatype = 'double';
        varstruct.Dimension = { 'time' };
        nc_addvar(ncfile,varstruct);
end



before = nc_getvarinfo ( ncfile, 'time' );

clear input_buffer;
input_buffer.time = [1 2 3]';


nc_addnewrecs ( ncfile, input_buffer );
input_buffer.time = [4 5 6]';
nc_addnewrecs ( ncfile, input_buffer );

after = nc_getvarinfo ( ncfile, 'time' );
if ( (after.Size - before.Size) ~= 6 )
    error ( '%s:  nc_addnewrecs failed to add the right number of records.', mfilename );
end

return








%---------------------------------------------------------------------------
function test_011(ncfile,mode)

nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',4);
nc_adddim(ncfile,'time',0);

clear varstruct;
varstruct.Name = 'time';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'time' };
nc_addvar ( ncfile, varstruct );




before = nc_getvarinfo ( ncfile, 'time' );

clear input_buffer;
input_buffer.time = [1 2 3]';


nc_addnewrecs ( ncfile, input_buffer );
input_buffer.time = [3 4 5]';
nc_addnewrecs ( ncfile, input_buffer );

after = nc_getvarinfo ( ncfile, 'time' );
if ( (after.Size - before.Size) ~= 5 )
    error ( '%s:  nc_addnewrecs failed to add the right number of records.', mfilename );
end
return














%---------------------------------------------------------------------------
function create_test012_file(ncfile,mode)

nc_create_empty(ncfile,mode);

% baseline case
nc_add_dimension ( ncfile, 'ocean_time', 0 );

if ischar(mode) && strcmp(mode,'hdf4')
    % We already have the ocean_time coordinate variable in hdf4 case.
else
    
    clear varstruct;
    varstruct.Name = 'ocean_time';
    varstruct.Nctype = 'double';
    varstruct.Dimension = { 'ocean_time' };
    nc_addvar ( ncfile, varstruct );
end

clear varstruct;
varstruct.Name = 't1';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'ocean_time' };
nc_addvar ( ncfile, varstruct );

clear varstruct;
varstruct.Name = 't2';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'ocean_time' };
nc_addvar ( ncfile, varstruct );

clear varstruct;
varstruct.Name = 't3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'ocean_time' };
nc_addvar ( ncfile, varstruct );

%---------------------------------------------------------------------------
function test_012(ncfile,mode)

create_test012_file(ncfile,mode);
%
% write ten records
x = (0:9)';
b.ocean_time = x;
b.t1 = x;
b.t2 = 1./(1+x);
b.t3 = x.^2;
nc_addnewrecs ( ncfile, b, 'ocean_time' );
nc_addnewrecs ( ncfile, b, 'ocean_time' );
v = nc_getvarinfo ( ncfile, 't1' );
if ( v.Size ~= 10 )
    error ( '%s:  expected var length was not 10.\n', mfilename );
end

return








%--------------------------------------------------------------------------
function create_013_testfile(ncfile,mode)
nc_create_empty(ncfile,mode);

nc_add_dimension ( ncfile, 'time', 0 );
nc_add_dimension ( ncfile, 'x', 10 );
nc_add_dimension ( ncfile, 'y', 10 );
nc_add_dimension ( ncfile, 'z', 10 );

if ischar(mode) && strcmp(mode,'hdf4')
    % no need to add time var here
else
    clear varstruct;
    varstruct.Name = 'time';
    varstruct.Nctype = 'double';
    varstruct.Dimension = { 'time' };
    nc_addvar ( ncfile, varstruct );
end

clear varstruct;
varstruct.Name = 't1';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'time' };
nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 't2';
varstruct.Nctype = 'double';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'x','y', 'time' };
else
    varstruct.Dimension = { 'time', 'y', 'x' };
end
nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 't3';
varstruct.Nctype = 'double';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'y', 'time' };
else
    varstruct.Dimension = { 'time', 'y'};
end
nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 't4';
varstruct.Nctype = 'double';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'x', 'y', 'z', 'time' };
else
    varstruct.Dimension = { 'time', 'z', 'y', 'x'};
end
nc_addvar ( ncfile, varstruct );


%---------------------------------------------------------------------------
function test_013(ncfile,mode)

create_013_testfile(ncfile,mode);


if getpref('SNCTOOLS','PRESERVE_FVD',false)
    b.time = 0;
    b.t1 = 0;
    b.t2 = zeros(10,10);
    b.t3 = zeros(10,1);
    b.t4 = zeros(10,10,10);
else
    b.time = 0;
    b.t1 = 0;
    b.t2 = zeros(10,10);
    b.t3 = zeros(1,10);
    b.t4 = zeros(10,10,10);
end

nc_addnewrecs ( ncfile, b, 'time' );

clear b
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    b.time = 1;
    b.t1 = 1;
    b.t2 = ones(10,10);
    b.t3 = ones(10,1);
    b.t4 = ones(10,10,10);
else
    b.time = 1;
    b.t1 = 1;
    b.t2 = ones(10,10);
    b.t3 = ones(1,10);
    b.t4 = ones(10,10,10);
end
nc_addnewrecs ( ncfile, b, 'time' );


%
% Now read them back.  
b = nc_getbuffer ( ncfile, 0, 2 );
if length(b.time) ~= 2
    error('length of time variable was %d and not the expected 2\n', length(b.time) );
end
if (b.time(1) ~= 0) && (b.time(2) ~= 1)
    error('values of time variable are wrong');
end


return









%--------------------------------------------------------------------------
function test_014(ncfile,mode)
% Add a single record, trailing singleton dimensions.
create_014_testfile(ncfile,mode);

b.time = 0;
b.t1 = 0;

nc_addnewrecs ( ncfile, b, 'time' );

clear b
b.time = 1;
b.t1 = 1;
nc_addnewrecs ( ncfile, b, 'time' );


%
% Now read them back.  
t1 = nc_varget ( ncfile, 't1' );
if (t1(1) ~= 0) && (t1(2) ~= 1)
    error('values are wrong');
end


return

%--------------------------------------------------------------------------
function create_014_testfile(ncfile,mode)

nc_create_empty(ncfile,mode);

nc_add_dimension ( ncfile, 'time', 0 );
nc_add_dimension ( ncfile, 'x', 1 );
nc_add_dimension ( ncfile, 'y', 1 );

if ischar(mode) && strcmp(mode,'hdf4')
    % no need to add time var here
else
    
    clear varstruct;
    varstruct.Name = 'time';
    varstruct.Nctype = 'double';
    varstruct.Dimension = { 'time' };
    nc_addvar ( ncfile, varstruct );
end

clear varstruct;
varstruct.Name = 't1';
varstruct.Nctype = 'double';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    varstruct.Dimension = { 'x', 'y', 'time' };
else
    varstruct.Dimension = { 'time', 'y', 'x' };
end
nc_addvar ( ncfile, varstruct );
