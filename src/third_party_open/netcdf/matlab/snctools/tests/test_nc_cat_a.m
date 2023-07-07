function test_nc_cat_a ( )
% TEST_NC_CAT_A:  tests the m-file nc_cat_a
%
% Test 01:  wrong number of input arguments.
% Test 02:  concatenating empty files
% Test 03:  First file has 5, 2nd has 10, 3rd has 15 time values.  They
%     do not overlap.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Id$
% $LastChangedDate$
% $LastChangedRevision$
% $LastChangedBy$
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf('Testing NC_CAT_A...  ' );

test_cat_empty_files;
test_wrong_number_of_inputs;
test_simple;

fprintf('OK\n');

return;









%--------------------------------------------------------------------------
%
% Create a netCDF file .  The general format is
%
% netcdf test_01 {
% dimensions:
%     time = UNLIMITED ; // (0 currently)
%     x = 10 ;
%     y = 20 ;
% variables:
%     double time(time) ;
%     double var1(time, y) ;
%     double var2(time, y, x) ;
% }
%
function create_default_test_file ( ncfile )
nc_create_empty ( ncfile );

nc_add_dimension ( ncfile, 'time', 0 );
nc_add_dimension ( ncfile, 'x', 10 );
nc_add_dimension ( ncfile, 'y', 20 );

v.Name = 'time';
v.Dimension = { 'time' };
nc_addvar ( ncfile, v );

v.Name = 'var1';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    v.Dimension = { 'y', 'time' };
    nc_addvar ( ncfile, v );

    v.Name = 'var2';
    v.Dimension = { 'x', 'y', 'time' };
    nc_addvar ( ncfile, v );
else
    v.Dimension = { 'time', 'y' };
    nc_addvar ( ncfile, v );

    v.Name = 'var2';
    v.Dimension = { 'time', 'y', 'x' };
    nc_addvar ( ncfile, v );
end

return








%--------------------------------------------------------------------------
function test_wrong_number_of_inputs ( )

ncfile1 = 'test_01.nc';
ncfile2 = 'test_02.nc';
ncfile3 = 'test_03.nc';


create_default_test_file ( ncfile1 );
create_default_test_file ( ncfile2 );
create_default_test_file ( ncfile3 );

try
    nc_cat_a;
catch %#ok<CTCH>
    return
end
error ( 'Succeeded when it should have failed.' );











%--------------------------------------------------------------------------
function test_cat_empty_files (  )
% concatentate some empty files

ncfile1 = 'test_01.nc';
ncfile2 = 'test_02.nc';
ncfile3 = 'test_03.nc';
abscissa_var = 'time';

create_default_test_file ( ncfile1 );
create_default_test_file ( ncfile2 );
create_default_test_file ( ncfile3 );

ncfiles{1} = 'test_01.nc';
ncfiles{2} = 'test_02.nc';
ncfiles{3} = 'test_03.nc';
output_ncfile = 'test_out.nc';
nc_cat_a ( ncfiles, output_ncfile, abscissa_var );

info = nc_getvarinfo(output_ncfile,'time');
if ( info.Size ~= 0 )
    error('size should have been zero.');
end
return










%--------------------------------------------------------------------------
function test_simple (  )
% First file has 5, 2nd has 10, 3rd has 15 time values.  They
%     do not overlap.  So the total number of records would be 30.

ncfile1 = 'test_01.nc';
ncfile2 = 'test_02.nc';
ncfile3 = 'test_03.nc';
abscissa_var = 'time';

create_default_test_file ( ncfile1 );
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    vardata.time = (1:5)';
    vardata.var1 = ones(20,5);
    vardata.var2 = 2*ones(10,20,5);
    nc_addnewrecs ( ncfile1, vardata, abscissa_var );
else
    vardata.time = (1:5)';
    vardata.var1 = ones(5,20);
    vardata.var2 = 2*ones(5,20,10);
    nc_addnewrecs ( ncfile1, vardata, abscissa_var );
end



create_default_test_file ( ncfile2 );
vardata.time = (1:10)' + 10;
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    vardata.var1 = ones(20,10);
    vardata.var2 = 2*ones(10,20,10);
else
    vardata.var1 = ones(10,20);
    vardata.var2 = 2*ones(10,20,10);
end
nc_addnewrecs ( ncfile2, vardata, abscissa_var );



create_default_test_file ( ncfile3 );
vardata.time = (1:15)' + 20;
if getpref('SNCTOOLS','PRESERVE_FVD',false)
    vardata.var1 = ones(20,15);
    vardata.var2 = 2*ones(10,20,15);
else
    vardata.var1 = ones(15,20);
    vardata.var2 = 2*ones(15,20,10);
end
nc_addnewrecs ( ncfile3, vardata, abscissa_var );


ncfiles = { ncfile1, ncfile2, ncfile3 };
output_ncfile = 'test_out.nc';
nc_cat_a ( ncfiles, output_ncfile, abscissa_var );

%
% There should be 30 records.
nt = nc_varsize ( output_ncfile, abscissa_var );
if nt ~= 30
    error ( 'wrong number of final records.');
end
return










