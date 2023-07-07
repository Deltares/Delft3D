function test_nc_addrecs(mode)


% netcdf foo {
% dimensions:
%     x = 4 ;
%     time = UNLIMITED ; // (0 currently)
% variables:
%     float test_var(time) ;
%         test_var:long_name = "This is a test" ;
%         test_var:short_val = 5s ;
%     double test_var2(time) ;
%     double test_var3(x) ;
% }

fprintf ('\t\tTesting NC_ADDRECS...  ' );
if nargin < 1
	mode = nc_clobber_mode;
end

ncfile = 'foo.nc';
create_ncfile(ncfile,mode);
test_2_inputs_2_vars(ncfile);
test_2_successive_writes(ncfile,mode);
run_negative_tests(mode);
fprintf('OK\n');






%--------------------------------------------------------------------------
function test_2_inputs_2_vars ( ncfile )
% Add records to two variables.

before = nc_getvarinfo ( ncfile, 'test_var2' );

input_buffer.test_var = single([3 4 5]');
input_buffer.test_var2 = [3 4 5]';

nc_addrecs ( ncfile, input_buffer );

after = nc_getvarinfo ( ncfile, 'test_var2' );
if ( (after.Size - before.Size) ~= 3 )
    error ( 'nc_addrecs failed to add the right number of records.');
end


return







%--------------------------------------------------------------------------
function test_2_successive_writes(ncfile,mode)
% Run it twice.

nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',4);
nc_adddim(ncfile,'time',0);
if ~(ischar(mode) && strcmp(mode,'hdf4'))
	v.Name = 'time';
	v.Dimension = {'time'};
	nc_addvar(ncfile,v);
end

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
varstruct.Name = 'test_var3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'x' };

nc_addvar ( ncfile, varstruct );


before = nc_getvarinfo ( ncfile, 'test_var2' );
clear input_buffer;
input_buffer.time = [1 2 3];
input_buffer.test_var = single([3 4 5]');
input_buffer.test_var2 = [3 4 5]';
nc_addrecs ( ncfile, input_buffer );

input_buffer.time = [4 5 6];
nc_addrecs ( ncfile, input_buffer );

after = nc_getvarinfo ( ncfile, 'test_var2' );
if ( (after.Size - before.Size) ~= 6 )
    error ( '%s:  nc_addrecs failed to add the right number of records.', mfilename );
end
return











%--------------------------------------------------------------------------
function create_ncfile(ncfile,mode)

nc_create_empty(ncfile,mode)
nc_adddim(ncfile,'x',4);
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
varstruct.Name = 'test_var3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'x' };

nc_addvar ( ncfile, varstruct );

return





%--------------------------------------------------------------------------
function run_negative_tests(mode)
% Don't bother on 2006b or below
v = version('-release');
switch(v)
    case {'14','2006a','2006b'}
        return
end

ncfile = 'foo.nc';
create_ncfile(ncfile,mode)
		
test_no_inputs;
test_only_one_input ( ncfile );
test_2nd_input_not_structure ( ncfile );
test_2nd_input_is_empty_structure ( ncfile );
test_2nd_input_has_bad_fieldnames ( ncfile );
test_one_field_not_unlimited ( ncfile );
test_no_unlimited_dimension(ncfile,mode);

return






%--------------------------------------------------------------------------
function test_no_inputs (  )

% Try no inputs
try
    nc_addrecs;
catch %#ok<CTCH>
    return
end
error ( 'succeeded on no inputs, should have failed' );








%--------------------------------------------------------------------------
function test_only_one_input ( ncfile )
%
% Try one input, should fail
try
    nc_addrecs ( ncfile );
catch %#ok<CTCH>
    return
end
error ( 'nc_addrecs succeeded on one input, should have failed');









%--------------------------------------------------------------------------
function test_2nd_input_not_structure ( ncfile )


% Try with 2nd input that isn't a structure.
try
    nc_addrecs ( ncfile, [] );
catch %#ok<CTCH>
    return
end
error ( 'nc_addrecs succeeded on one input, should have failed');












%--------------------------------------------------------------------------
function test_2nd_input_is_empty_structure ( ncfile )

%
% Try with 2nd input that is an empty structure.
try
    nc_addrecs ( ncfile, struct([]) );
catch %#ok<CTCH>
    return
end
error ( 'nc_addrecs succeeded on empty structure, should have failed');










%--------------------------------------------------------------------------
function test_2nd_input_has_bad_fieldnames ( ncfile )

%
% Try a structure with bad names
input_data.a = [3 4];
input_data.b = [5 6];
try
    nc_addrecs ( ncfile, input_data );
catch %#ok<CTCH>
    return
end
error ( 'nc_addrecs succeeded on a structure with bad names, should have failed');




%--------------------------------------------------------------------------
function test_one_field_not_unlimited ( ncfile )

% Try writing to a fixed size variable

input_buffer.test_var = single([3 4 5]');
input_buffer.test_var2 = [3 4 5]';
input_buffer.test_var3 = [3 4 5]';

try
    nc_addrecs ( ncfile, input_buffer );
catch %#ok<CTCH>
    return
end
error ( 'nc_addrecs succeeded on writing to a fixed size variable, should have failed.');






%--------------------------------------------------------------------------
function test_no_unlimited_dimension(ncfile,mode)


nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',4);

clear varstruct;
varstruct.Name = 'test_var3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'x' };

nc_addvar ( ncfile, varstruct );


input_buffer.time = [1 2 3]';
try
    nc_addrecs ( ncfile, input_buffer );
catch %#ok<CTCH>
    return
end
error ( 'nc_addrecs passed when writing to a file with no unlimited dimension');




