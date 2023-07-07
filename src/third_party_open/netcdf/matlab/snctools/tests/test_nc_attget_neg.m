function test_nc_attget_neg(ncfile)

v = version('-release');

testroot = fileparts(mfilename('fullpath'));
test_file_not_there;
test_var_att_not_there_classic(ncfile);
test_var_not_there(ncfile);
test_global_att_not_there_classic(ncfile);


switch(v)
    case {'2008b','2009a','2009b'}
        % problems exist in java interface
    otherwise      
        test_group_att_not_there([testroot filesep 'testdata/enhanced.nc']);
        test_group_not_there    ([testroot filesep 'testdata/enhanced.nc']);
        test_get_att_not_there_enhanced;
end











%--------------------------------------------------------------------------
function test_get_att_not_there_enhanced()

v = version('-release');
if strcmp(v,'2008a')
    return
end

ncfile = 'example.nc';

try
	nc_attget(ncfile,'z_double', 'test_double_att' );
catch me
    switch(me.identifier)
        case { 'MATLAB:imagesci:netcdf:libraryFailure', ... % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ...      % 2010a
                'snctools:attget:java:variableNotFound' }
            return
        otherwise
            rethrow(me);
    end

end

error('failed');







%--------------------------------------------------------------------------
function test_file_not_there()

try
	nc_attget('idonotexist.nc','z_double','test_double_att');
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case 'snctools:format:cannotOpenFile'
            return
        otherwise
            rethrow(me);
    end
end
error('failed')



%--------------------------------------------------------------------------
function test_var_att_not_there_classic ( ncfile )

try
	nc_attget(ncfile,'z_double','test_double_att');
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...            % 2011b
                'MATLAB:netcdf:inqAtt:enotatt:attributeNotFound', ... % 2011a
                'MATLAB:netcdf:inqAtt:attributeNotFound', ...         % 2009b tmw
                'snctools:attget:java:attributeNotFound', ...         % 2009b java
                'snctools:attget:mexnc:inqAttType' }                  % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end
end
error('failed');



%--------------------------------------------------------------------------
function test_var_not_there(ncfile)

try
	nc_attget(ncfile,'blah','test_double_att');
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...             % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ...         % 2009b tmw
                'snctools:attget:java:variableNotFound', ...           % 2009b java
                'snctools:attget:mexnc:inqVarID'}                      % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end
end
error('failed');



%--------------------------------------------------------------------------
function test_group_not_there(ncfile)

try
	nc_attget(ncfile,'/grp1/grp4','blah');
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...             % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2011a
                'snctools:noNetcdfJava', ...                           % 2010a java
                'snctools:attget:java:variableNotFound' }              % 2009b java
            return
        otherwise
            rethrow(me);
    end
end
error('failed');




%--------------------------------------------------------------------------
function test_group_att_not_there(ncfile)

try
	nc_attget(ncfile,'/grp1','blah');
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure',            ... % 2011b
                'MATLAB:netcdf:inqAtt:enotatt:attributeNotFound', ... % 2011a
                'snctools:noNetcdfJava',                          ... % 2010a java
                'snctools:attget:java:attributeNotFound' }            % 2009b java
            return
        otherwise
            rethrow(me);
    end
end
return








%--------------------------------------------------------------------------
function test_global_att_not_there_classic ( ncfile )

try
	nc_attget(ncfile,-1,'blah');
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...            % 2011b
                'MATLAB:netcdf:inqAtt:enotatt:attributeNotFound', ... % 2011a
                'MATLAB:netcdf:inqAtt:attributeNotFound', ...         % 2009b tmw
                'snctools:attget:java:attributeNotFound', ...         % 2009b java
                'snctools:attget:mexnc:inqAttType' }                  % 2008a mexnc
            return
        otherwise
            rethrow(me);
    end
end
return









