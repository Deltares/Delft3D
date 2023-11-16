function test_nc_diff()
% TEST_NC_DIFF
%
% Relies upon nc_addnewrecs, nc_addvar
%
% Test run include
% 1.  Create two empty identical files.
% 2.  Add different data to the test_var variable in both files. 
% 3.  Test files with unequal dimensions.
% 4.  Test files with variables with unequal rank.
% 5.  Test files and attributes.
% 6.  Test files and attributes with a different number of attributes.  
%     Should fail.
% 7.  Test files and attributes with a different number of attributes.  
%     Reverse order.  Should fail.
% 8.  Test files and attributes, one attribute has different class.
% 9.  Test files and attributes, one attribute has different length
% 10.  Test files and attributes, one attribute has different value
% 11.  Test files and global attributes, one attribute has different value

ncfile_1 = 'foo1.nc';
ncfile_2 = 'foo2.nc';


v = version('-release');
switch(v)
	case { '14', '2006a', '2006b', '2007a', '2007b', '2008a'}
		try
		    mexnc('inq_libvers');
		catch
			fprintf('\tNo testing yet on where mexnc mex-file not available.\n');
			return
		end
end


test_empty_files                      ( ncfile_1, ncfile_2 );
test_different_unlimited_coord_var    ( ncfile_1, ncfile_2 );
test_unlimited_var_has_different_size ( ncfile_1, ncfile_2 );
test_different_rank                   ( ncfile_1, ncfile_2 );
test_attributes                       ( ncfile_1, ncfile_2 );
test_different_number_of_attributes   ( ncfile_1, ncfile_2 );
test_different_num_atts_2nd_file      ( ncfile_1, ncfile_2 );
test_one_attr_with_different_class    ( ncfile_1, ncfile_2 );
test_one_attr_with_different_length   ( ncfile_1, ncfile_2 );
test_one_attr_with_different_value    ( ncfile_1, ncfile_2 );
test_one_gattr_with_different_value   ( ncfile_1, ncfile_2 );

fprintf('OK\n');
return





%--------------------------------------------------------------------------
% 
% Test the empty files
function test_empty_files ( ncfile_1, ncfile_2 )

create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );

status = nc_diff ( ncfile_1, ncfile_2 );
if  status < 0
	error('did not produce the expected result.');
end
return






%--------------------------------------------------------------------------
function test_different_unlimited_coord_var ( ncfile_1, ncfile_2 )

create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );

write_buffer.time = (1:5)';
write_buffer.test_var = rand(5,1);
nc_addnewrecs ( ncfile_1, write_buffer, 'time' );

write_buffer.test_var = rand(5,1);
nc_addnewrecs ( ncfile_2, write_buffer, 'time' );

status = nc_diff ( ncfile_1, ncfile_2 );
if  status >= 0
	error('did not produce the expected result.');
end
return




%--------------------------------------------------------------------------
function test_unlimited_var_has_different_size ( ncfile_1, ncfile_2 )

create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );

write_buffer.time = (1:5)';
write_buffer.test_var = rand(5,1);
nc_addnewrecs ( ncfile_1, write_buffer, 'time' );

write_buffer.test_var = rand(5,1);
nc_addnewrecs ( ncfile_2, write_buffer, 'time' );

%
% add to the unlimited dimension in just one file
write_buffer.time = (6:7)';
write_buffer.test_var = rand(2,1);
nc_addnewrecs ( ncfile_1, write_buffer, 'time' );

status = nc_diff ( ncfile_1, ncfile_2 );
if  status >= 0
	error('did not produce the expected result.');
end
return










%--------------------------------------------------------------------------
function test_different_rank ( ncfile_1, ncfile_2 )

create_test_file ( ncfile_1 );
write_buffer.time = (1:5)';
write_buffer.test_var = rand(5,1);
nc_addnewrecs ( ncfile_1, write_buffer, 'time' );

%
% add to the unlimited dimension in just one file
write_buffer.time = (6:7)';
write_buffer.test_var = rand(2,1);
nc_addnewrecs ( ncfile_1, write_buffer, 'time' );


% Create a netcdf file with a variable with different rank
create_test_file ( ncfile_2 );
status = nc_diff ( ncfile_1, ncfile_2 );
if  status >= 0
	error('did not produce the expected result.');
end

return










%--------------------------------------------------------------------------
function test_attributes ( ncfile_1, ncfile_2 )

% 
% Test the empty files and attributes.
create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );
status = nc_diff ( ncfile_1, ncfile_2, '-attributes' );
if  status < 0
	error('did not produce the expected result.');
end
return










%--------------------------------------------------------------------------
function test_different_number_of_attributes ( ncfile_1, ncfile_2 )

% 
% Test the empty files and different number of variable attributes.
create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );
nc_attput ( ncfile_1, 'test_var', 'test_attribute', 0 );
status = nc_diff ( ncfile_1, ncfile_2, '-attribute' );
if  status >= 0
	error('did not produce the expected result.');
end
return









%--------------------------------------------------------------------------
function test_different_num_atts_2nd_file ( ncfile_1, ncfile_2 )

% 
% Test the empty files and different number of variable attributes.
create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );
nc_attput ( ncfile_2, 'test_var', 'test_attribute', 0 );
status = nc_diff ( ncfile_1, ncfile_2, '-attribute' );
if  status >= 0
	error('did not produce the expected result.');
end
return















%--------------------------------------------------------------------------
function test_one_attr_with_different_class ( ncfile_1, ncfile_2 )

% 
% Test the empty files and one attribute with different class.
create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );
nc_attput ( ncfile_1, 'test_var', 'short_val', 0 );
status = nc_diff ( ncfile_1, ncfile_2, '-attribute' );
if  status >= 0
	error('did not produce the expected result.');
end
return






%--------------------------------------------------------------------------
function test_one_attr_with_different_length ( ncfile_1, ncfile_2 )

% 
% Test the empty files and one attribute with different class.
create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );
nc_attput ( ncfile_1, 'test_var', 'short_val', int16([5 3]) );
status = nc_diff ( ncfile_1, ncfile_2, '-attribute' );
if  status >= 0
	error('did not produce the expected result.');
end
return








%--------------------------------------------------------------------------
function test_one_attr_with_different_value ( ncfile_1, ncfile_2 )

% 
% Test the empty files and one attribute with different value.
create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );
nc_attput ( ncfile_1, 'test_var', 'short_val', int16(27) );
status = nc_diff ( ncfile_1, ncfile_2, '-attribute' );
if  status >= 0
	error('did not produce the expected result.');
end
return






%--------------------------------------------------------------------------
function test_one_gattr_with_different_value ( ncfile_1, ncfile_2 )

% 
% Test the empty files and one global attribute with different value.
create_test_file ( ncfile_1 );
create_test_file ( ncfile_2 );
nc_attput ( ncfile_1, nc_global, 'short_val', int16(27) );
status = nc_diff ( ncfile_1, ncfile_2, '-attribute' );
if  status >= 0
	error('did not produce the expected result.');
end

return








%--------------------------------------------------------------------------
function create_test_file ( ncfile)

nc_create_empty(ncfile,nc_clobber_mode);
nc_adddim(ncfile,'x',4);
nc_adddim(ncfile,'y',5);
nc_adddim(ncfile,'time',0);

% Add a variable along the time dimension
varstruct.Name = 'test_var';
varstruct.Nctype = 'float';

if getpref('SNCTOOLS','PRESERVE_FVD',false)
	if nargin > 1
		varstruct.Dimension = { 'y', 'time' };
	else
		varstruct.Dimension = { 'time' };
	end
else
	if nargin > 1
		varstruct.Dimension = { 'time', 'y' };
	else
		varstruct.Dimension = { 'time' };
	end
end

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
varstruct.Name = 'time';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'time' };

nc_addvar ( ncfile, varstruct );


clear varstruct;
varstruct.Name = 'test_var3';
varstruct.Nctype = 'double';
varstruct.Dimension = { 'x' };

nc_addvar ( ncfile, varstruct );
return


