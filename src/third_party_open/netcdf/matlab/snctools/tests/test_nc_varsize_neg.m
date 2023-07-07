function test_nc_varsize_neg()
% Negative testing for NC_VARSIZE

% test 2:  1 input
% test 3:  too many inputs
% test 4:  inputs are not all character
% test 5:  not a netcdf file
% test 6:  empty netcdf file
% test 7:  given variable is not present
testroot = fileparts(mfilename('fullpath'));

ncfile = fullfile(testroot, 'testdata/empty.nc' );

test_varname_not_char (ncfile);
test_not_netcdf;
test_empty (ncfile);

ncfile = fullfile(testroot, 'testdata/full.nc' );
test_var_not_present (ncfile);
return












%--------------------------------------------------------------------------
function test_varname_not_char ( ncfile )

try
	nc_varsize ( ncfile, 1 );
catch me
    switch(me.identifier)
        case {'snctools:getvarinfo:tmw:badTypes', ... % R2011b
                'snctools:nc_getvarinfo:mexnc:badTypes', ... % 2008a
                'MATLAB:UndefinedFunction' } % 2008a win64
            return
    end
    rethrow(me);        
end


error('failed')









%--------------------------------------------------------------------------
function test_not_netcdf ( )

% test 5:  not a netcdf file
try
	nc_varsize('example.cdf','t');
catch me
    switch(me.identifier)
        case {'snctools:unknownWriteBackendSituation', ... % 2011b
                'MATLAB:Java:GenericException', ...        % 2009b - java
                'snctools:noNetcdfJava' }           % 2009b - no java
            return
        otherwise
            rethrow(me);
    end
end

error('failed')















%--------------------------------------------------------------------------
function test_empty ( ncfile )

% no such variable
try
	nc_varsize(ncfile,'t');
catch me
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...             % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2010b
                'MATLAB:netcdf:inqVarID:variableNotFound', ...         % 2009b
                'snctools:getVarInfo:mexnc:inqVarID', ...              % 2008a
                'snctools:getVarInfo:badVariableName' }                % 2008a win64
            return
        otherwise
            rethrow(me);
    end
end

error('failed')












%--------------------------------------------------------------------------
function test_var_not_present ( ncfile )

try	
    nc_varsize ( ncfile, 'xyz' );
catch me
    switch(me.identifier)
        case { 'MATLAB:imagesci:netcdf:libraryFailure', ... % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ...  % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ... % 2009b
                'snctools:getVarInfo:mexnc:inqVarID', ...      % 2008a
                'snctools:getVarInfo:badVariableName' }        % 2008a win64
            return
        otherwise
            rethrow(me)
    end
end
error('failed');






