function test_nc_varget_neg(ncfile)

test_no_such_file;
test_no_such_variable(ncfile);

test_1D_start_too_large(ncfile);
test_1D_count_too_large(ncfile);
test_1D_stride_too_large(ncfile);

test_1D_start_rank_mismatch(ncfile);
test_1D_count_rank_mismatch(ncfile);
test_1D_stride_rank_mismatch(ncfile);

test_1D_bad_start_datatype(ncfile);
test_1D_bad_count_datatype(ncfile);

test_zero_count(ncfile);

%----------------------------------------------------------------------
function test_1D_bad_count_datatype(ncfile)
try
    nc_varget(ncfile,'test_1D',0,'1');
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:argumentHasBadDatatype', ... % 2011b
                'MATLAB:netcdf:countArgumentHasBadDatatype',   ... % 2011a
                'MATLAB:Java:GenericException',                ... % 2009b
                'MATLAB:invalidType' }                             % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end
end
error('failed');

%----------------------------------------------------------------------
function test_1D_bad_start_datatype(ncfile)
try
    nc_varget(ncfile,'test_1D','0',1);
catch me

    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:argumentHasBadDatatype', ... % 2011b
                'MATLAB:netcdf:startArgumentHasBadDatatype'    ... % 2011a
                'MATLAB:Java:GenericException',                ... % 2009b 
                'MATLAB:invalidType' }                             % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end

end
error('failed')


%----------------------------------------------------------------------
function test_no_such_file()
try
    nc_varget('doesnt_exist','test_5D');
catch me
    %me.identifier
    %me.message
    
    if strcmp(me.identifier,'snctools:format:cannotOpenFile')
        return
    end
    
    rethrow(me);
end

error('failed');

%----------------------------------------------------------------------
function test_no_such_variable(ncfile)
try
    nc_varget(ncfile,'test_5D');
catch me
    %me.message
    %me.identifier
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...            % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound' ... % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ...        % 2009b tmw
                'snctools:varget:java:noSuchVariable', ...            % 2009b java
                'snctools:varget:mexnc:inqVarID' }                    % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end
                
end

error('failed')

%----------------------------------------------------------------------
function test_1D_stride_rank_mismatch(ncfile)
try
    nc_varget(ncfile,'test_1D',2,2,[2 2]);
catch me
    %me.message
    %me.identifier
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:getVarsStrideVectorLengthMismatch', ... %2011b
                'MATLAB:netcdf:getVars:strideVectorLengthMismatch', ... %2011a
                'snctools:indexing:badIndexLength'}
            return
        otherwise
            rethrow(me);
    end
end

error('failed')

%----------------------------------------------------------------------
function test_1D_count_rank_mismatch(ncfile)
try
    nc_varget(ncfile,'test_1D',2,[2 2]);
catch me
    %me.message
    %me.identifier
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:getVarsCountVectorLengthMismatch', ... %2011b
                'MATLAB:netcdf:getVars:countVectorLengthMismatch', ... %2011a
                'snctools:indexing:badIndexLength'}
            return
        otherwise
            rethrow(me);
    end
end

error('failed')



%----------------------------------------------------------------------
function test_1D_start_rank_mismatch(ncfile)
% The length of the start argument should have been 1.
try
    nc_varget(ncfile,'test_1D',[2 2],2);
catch me
    %me.message
    %me.identifier
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:getVarsStartVectorLengthMismatch', ... %2011b
                'MATLAB:netcdf:getVars:startVectorLengthMismatch', ...
                'MATLAB:badsubscript', ... %2009b java
                'snctools:indexing:badIndexLength' } % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end
                
end

error('failed')

%----------------------------------------------------------------------
function test_1D_stride_too_large(ncfile)
try
    nc_varget(ncfile,'test_1D',4,2,2);
catch me
    %me.message
    %me.identifier
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure',                          ... % 2011b
                'MATLAB:netcdf:getVars:einvalcoords:indexExceedsDimensionBound' ... % 2011a
                'MATLAB:netcdf:getVars:indexExceedsDimensionBound',             ... % 2009b tmw
                'MATLAB:Java:GenericException'                                  ... % 2009b java
                'snctools:varget:mexnc:getVarFuncstrFailure' }                      % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end
    
end

error('failed')

%----------------------------------------------------------------------
function test_1D_count_too_large(ncfile)
try
    nc_varget(ncfile,'test_1D',4,4);
catch me
    %me.message
    %me.identifier
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure',                              ... % 2011b
                'MATLAB:netcdf:getVars:eedge:startPlusCountExceedsDimensionBound' , ... % 2011a
                'MATLAB:netcdf:getVars:startPlusCountExceedsDimensionBound',        ... % 2009b tmw
                'MATLAB:Java:GenericException',                                     ... % 2009b java
                'snctools:varget:mexnc:getVarFuncstrFailure' }                          % 2008b mexnc
                return
        otherwise
            rethrow(me);
    end
end

error('failed')

%----------------------------------------------------------------------
function test_1D_start_too_large(ncfile)
try
    nc_varget(ncfile,'test_1D',8,2);
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure',                          ... % 2011b
                'MATLAB:netcdf:getVars:einvalcoords:indexExceedsDimensionBound' ... % 2011a
                'MATLAB:netcdf:getVars:indexExceedsDimensionBound',             ... % 2009b tmw 
                'MATLAB:Java:GenericException',                                 ... % 2009b java
                'snctools:varget:mexnc:getVarFuncstrFailure' }
            return
        otherwise
            rethrow(me);
    end
                
end

error('failed')

%--------------------------------------------------------------------------
function test_zero_count(ncfile)
% The count cannot have any zeros.

try
    nc_varget(ncfile,'test_1D',0,0);
catch me
    switch(me.identifier)
        case { 'MATLAB:expectedPositive' }
            return
        otherwise
            rethrow(me);
    end
end

error('failed')