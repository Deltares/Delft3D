function test_mexnc()
% TEST_MEXNC:  Wrapper routine that invokes all tests for MEXNC
%
% USAGE:  test_mexnc;

p = which ( 'mexnc', '-all' );
if isempty(p)
	fprintf ( 1, 'Could not find mexnc on the matlab path.  Read the README!!\n' );
	fprintf ( 1, 'Bye\n' );
	return
end

fprintf ( 1, 'Your path for mexnc is listed below.  Before continuing on, make sure\n' );
fprintf ( 1, 'that any old versions of mexnc are either NOT present or are shadowed.\n' );
disp ( p );
answer = input ( 'Does the above path for mexnc look right? [y/n]\n', 's' );
if strcmp ( lower(answer), 'n' )
	fprintf ( 1, 'Bye\n' );
	return
end

fprintf ( 1, '\n\n\nDo you wish to remove all test NetCDF files in this directory prior to running the tests?\n' );
fprintf ( 1,  'It''s a good idea to say yes if you are in the test suite directory.\n' );
fprintf ( 1, 'It''s a bad idea if you are running the test from somewhere else, \n' );
fprintf ( 1, 'e.g. your PhD thesis results directory.\n' );
answer = input ( '[y | n]\n', 's' );
if strcmp ( lower(answer(1)), 'y' )
	delete ( '*.nc' );
end

ncver = mexnc ( 'inq_libvers' );
if ncver(1) == '4'
    %test_def_grp ( 'foo.nc' );
end


test_copy_att ( 'foo_copy_att1.nc', 'foo_copy_att2.nc' );
test__create ( 'foo__create.nc' );
test_create ( 'foo_create.nc' );
test_def_dim ( 'foo_def_dim.nc' );
test_def_var ( 'foo_def_var.nc' );
test_del_att ( 'foo_del_att.nc' );
test__enddef ( 'foo__enddef.nc' );
test_inq ( 'foo_inq.nc' );
test_inq_dim ( 'foo_inq_dim.nc' );
test_inq_dimid ( 'foo_inq_dimid.nc' );
test_inq_libvers;
test_inq_var ( 'foo_inq_var.nc' );
test_inq_varid ( 'foo_inq_varid.nc' );
test__open ( 'foo__open.nc' );
test_open ( 'foo_open.nc' );
%test_redef_def_dim ( 'foo_redef.nc' );
test_rename_dim ( 'foo_rename_dim.nc' );
test_rename_var ( 'foo_rename.nc' );

test_inq_att ( 'foo_inq_att.nc' );
test_inq_attid ( 'foo_inq_attid.nc' );
test_inq_atttype ( 'foo_inq_atttype.nc' );
test_inq_attlen ( 'foo_inq_attlen.nc' );
test_inq_unlimdim ( 'foo_unlimdic.nc' );

test_put_get_att ( 'foo_put_get_att.nc' );
test_get_var_bad_param_datatype ( 'foo_get_var_bad_param.nc' );
test_put_get_var_double ( 'foo_put_get_var_double.nc' );
test_put_get_var_float ( 'foo_put_get_var_float.nc' );
test_put_get_var_int ( 'foo_put_get_var_int.nc' );
test_put_get_var_short ( 'foo_put_get_var_short.nc' );
test_put_get_var_schar ( 'foo_put_get_var_schar.nc' );
test_put_get_var_uchar ( 'foo_put_get_var_uchar.nc' );
test_put_get_var_text ( 'foo_put_get_var_text.nc' );

test_put_var_bad_param_datatype ( 'foo_put_var_bad_param.nc' );
test_rename_att ( 'foo_rename_att.nc' );

test_set_fill ( 'foo_fill.nc' );
test_strerror;
test_sync ( 'foo_sync.nc' );

test_lfs ( 'foo_lfs_64.nc' );

test_chunking('foo_chunking.nc');
test_deflate('foo_deflate.nc');



% Deprecated functions
fprintf ( 1, '\n' );
fprintf ( 1, 'Testing NetCDF-2 functions.\n' );
fprintf ( 1, '\n' );
mexnc ( 'setopts', 0 );
test_attcopy ( 'foo_attcopy.nc', 'foo_attcopy2.nc' );
test_attdel ( 'foo_attdel.nc' );
test_attinq ( 'foo_attinq.nc' );
test_attname ( 'foo_attname.nc' );
test_attput ( 'foo_attput.nc' );
test_attrename ( 'foo_attrename.nc' );
test_dimdef ( 'foo_dimdef.nc' );
test_dimid ( 'foo_dimid.nc' );
test_diminq ( 'foo_diminq.nc' );
test_dimrename ( 'foo_dimrename.nc' );
test_endef ( 'foo_endef.nc' );
test_inquire ( 'foo_inquire.nc' );
test_typelen;
test_vardef ( 'foo_vardef.nc' );
test_varid ( 'foo_varid.nc' );
test_varinq ( 'foo_varinq.nc' );
test_varrename ( 'foo_varrename.nc' );
test_varput ( 'foo_varput.nc' );
test_varput1 ( 'foo_varput1.nc' );
test_varputg ( 'foo_varputg.nc' );
test_parameter;



fprintf ( 1, 'All tests succeeded.\n' );
fprintf ( 1, '\n' );
answer = input ( 'Do you wish to remove all test NetCDF files that were created? [y/n]\n', 's' );
if strcmp ( lower(answer), 'y' )
	delete ( '*.nc' );
end
fprintf ( 1, 'We''re done.\n' );



return



function test_inq_libvers ()
lib_version = mexnc ( 'inq_libvers' );

fprintf ( 1, 'MEXNC says it was built with version %s.\n', lib_version );
fprintf ( 1, 'INQ_LIBVERS succeeded\n' );
return
















