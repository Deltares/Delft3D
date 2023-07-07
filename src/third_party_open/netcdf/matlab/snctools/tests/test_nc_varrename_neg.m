function test_nc_varrename_neg(mode)

switch(mode)
	case nc_clobber_mode
		run_nc3_tests;

	case 'netcdf4-classic' 
		run_nc4_tests;

end



%--------------------------------------------------------------------------
function run_nc3_tests()

ncfile = 'foo.nc';
mode = nc_clobber_mode;
test_inputs_not_all_char ( ncfile, mode );
test_empty_file ( ncfile,mode );
test_variable_not_present ( ncfile,mode );
test_variable_with_same_name (ncfile,mode);
return


%--------------------------------------------------------------------------
function run_nc4_tests()

ncfile = 'foo4.nc';
mode = bitor(nc_clobber_mode,nc_netcdf4_classic);
test_inputs_not_all_char ( ncfile, mode );
test_empty_file ( ncfile,mode );
test_variable_not_present ( ncfile,mode );
test_variable_with_same_name (ncfile,mode);

return











%--------------------------------------------------------------------------
function test_variable_with_same_name ( ncfile,mode )


nc_create_empty ( ncfile,mode );
nc_add_dimension ( ncfile, 't', 0 );
clear varstruct;
varstruct.Name = 't';
varstruct.Nctype = 'double';
varstruct.Dimension = { 't' };
nc_addvar ( ncfile, varstruct );
varstruct.Name = 't2';
varstruct.Nctype = 'double';
varstruct.Dimension = { 't' };
nc_addvar ( ncfile, varstruct );

try
	nc_varrename ( ncfile, 't', 't2' );
catch me
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ... % 2011b
                'MATLAB:netcdf:renameVar:enameinuse:nameIsAlreadyInUse', ... % 2011a
                'MATLAB:netcdf:renameVar:nameIsAlreadyInUse', ... % 2009b
                'snctools:varrename:mexnc:RENAME_VAR' } % 2008a
            return
        otherwise
            rethrow(me)
    end
end

error('failed')




%--------------------------------------------------------------------------
function test_empty_file ( ncfile,mode )


nc_create_empty ( ncfile,mode );
try
	nc_varrename ( ncfile, 'x', 'y' );
catch me
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ... % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ... % 2009b
                'snctools:varrename:mexnc:INQ_VARID' } % 2008a
            return
        otherwise
            rethrow(me)
    end
end
error('succeeded when it should have failed');








%--------------------------------------------------------------------------
function test_variable_not_present ( ncfile,mode )


nc_create_empty ( ncfile,mode );
nc_add_dimension ( ncfile, 't', 0 );
clear varstruct;
varstruct.Name = 't';
varstruct.Nctype = 'double';
varstruct.Dimension = { 't' };
nc_addvar ( ncfile, varstruct );

try
    nc_varrename ( ncfile, 't2', 't3' );
catch me
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ... % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ... % 2009b
                'snctools:varrename:mexnc:INQ_VARID' } % 2008a
            return
        otherwise
            rethrow(me)
    end
end
error('succeeded when it should have failed.');










%--------------------------------------------------------------------------
function test_inputs_not_all_char(ncfile,mode)


% Ok, now we'll create the test file
nc_create_empty ( ncfile,mode );
nc_add_dimension ( ncfile, 't', 0 );
clear varstruct;
varstruct.Name = 't';
varstruct.Nctype = 'double';
varstruct.Dimension = {'t'};
nc_addvar ( ncfile, varstruct );


try
	nc_varrename ( ncfile, 'x', 1 );
catch me
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ... % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ... % 2009b
                'snctools:varrename:mexnc:INQ_VARID' } % 2008a
            return
        otherwise
            rethrow(me)
    end
end
error('succeeded when it should have failed');
















