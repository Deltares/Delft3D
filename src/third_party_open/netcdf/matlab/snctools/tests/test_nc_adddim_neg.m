function test_nc_adddim_neg(mode)
% Negative tests for NC_ADDDIM.
     
ncfile = 'foo4.nc';
test_no_inputs ();                               
test_too_many_inputs ( ncfile,mode );                 
test_not_netcdf_file;                 
test_2nd_input_not_char(ncfile,mode);              
test_3rd_input_not_numeric(ncfile,mode);           
test_3rd_input_negative(ncfile,mode);  

test_dimension_already_exists(ncfile,mode);











%--------------------------------------------------------------------------
function test_dimension_already_exists(ncfile,mode)
% Negative test:  try to add a dimension that is already there.  Should 
% error out.

nc_create_empty(ncfile,mode);
nc_adddim ( ncfile, 't', 0 );
try
	nc_adddim ( ncfile, 't', 0 );
catch me
    switch me.identifier
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...  % 2011b
                'MATLAB:netcdf:defDim:eunlimit:onlyOneUnlimitedDimensionAllowed' ... % 2010b
                'MATLAB:netcdf:defDim:onlyOneUnlimitedDimensionAllowed', ...         % 2009b
                'snctools:nc_adddim:defdimFailed' }                                    % 2008a
            return
        otherwise
            rethrow(me);
    end
end
error('failed');






            


%--------------------------------------------------------------------------
function test_no_inputs ()
% NC_ADDDIM needs 3 arguments. 
try
	nc_adddim;
catch me
    switch(me.identifier)
        case {'MATLAB:minrhs', ...            % 2011b
                'MATLAB:inputArgUndefined' }  % 2010b
            return
        otherwise
            rethrow(me);
    end

end
error('failed');






%--------------------------------------------------------------------------
function test_too_many_inputs(ncfile,mode)
% NC_ADDDIM needs only 3 arguments. 


nc_create_empty ( ncfile, mode );
try
	nc_adddim ( ncfile, 'x', 10, 12 );
catch me
    if ~strcmp(me.identifier,'MATLAB:TooManyInputs')
        rethrow(me);
    end
	return
end
error('failed');










%--------------------------------------------------------------------------
function test_not_netcdf_file()
% The file must be a netcdf file


try
	nc_adddim('example.cdf','x',3);
catch me
    if ~strcmp(me.identifier,'snctools:writeBackend:unknown')
        rethrow(me);
    end
	return
end
error('failed');












%--------------------------------------------------------------------------
function test_2nd_input_not_char(ncfile,mode)
% dimension name must be char.
nc_create_empty(ncfile,mode);
try
	nc_adddim(ncfile,3,3);
catch me
    switch(me.identifier)
        case 'snctools:adddim:badDimName' 
            return
        otherwise
            rethrow(me);
    end
end
error('failed');












%--------------------------------------------------------------------------
function test_3rd_input_not_numeric(ncfile,mode)
% The dimension length must be numeric

% test 5:  3rd input not numeric
nc_create_empty(ncfile,mode);
try
	nc_adddim ( ncfile, 't', 't' );
catch me
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:badScalarArgument', ...       % 2012a
                'MATLAB:imagesci:netcdf:badSizeArgumentDatatype', ... % 2011b
                'MATLAB:netcdf:badSizeArgumentDatatype', ...        % 2010b
                'MEXNC:checkNumericArgumentType:wasChar' }          % 2008a
            return
        otherwise
            rethrow(me);
    end
end
error('failed');










%--------------------------------------------------------------------------
function test_3rd_input_negative(ncfile,mode)
% The dimension length must be non-negative.

nc_create_empty(ncfile,mode);
try
	nc_adddim ( ncfile, 't', -1 );
catch me
    if strcmp(me.identifier,'snctools:adddim:badDimensionLength')
        return
    end
    rethrow(me);
end
error('failed');






