function test_nc_attput_neg(ncfile,mode)

if strcmp(mode,'hdf4')
    return
end


test_bad_datatype(ncfile,mode);
test_write_var_not_there(ncfile,mode);
test_write_fill_value(ncfile,mode);






%--------------------------------------------------------------------------
function test_bad_datatype(ncfile,mode)

nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',5);
v.Name = 'y';
v.Dimension = {'x'};
nc_addvar(ncfile,v);

try
	nc_attput(ncfile,-1,'test_double_att',int64(0));
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure' ...                % R2011b 
                'snctools:attput:badDatatype', ...                       % netcdf4-classic
                'MATLAB:netcdf:putAttInt64:ebadtype:invalidDataType' ... % R2011a
                'MATLAB:netcdf:putAtt:invalidDatatype', ...              % R2009b
                'snctools:attput:mexnc:unhandledDatatype' }              % R2008a
            return
        otherwise
            rethrow(me);
    end
end
error('failed');

%--------------------------------------------------------------------------
function test_write_var_not_there(ncfile,mode)


nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',5);
v.Name = 'y';
v.Dimension = {'x'};
nc_addvar(ncfile,v);

try
	nc_attput(ncfile,'z_double','test_double_att',0);
catch me
    %me.identifier
    %me.message
    switch(me.identifier)
        case {'MATLAB:imagesci:netcdf:libraryFailure', ...             % 2011b
                'MATLAB:netcdf:inqVarID:enotvar:variableNotFound', ... % 2011a
                'MATLAB:netcdf:inqVarID:variableNotFound', ...         % 2009b
                'snctools:attput:mexnc:inq_varid' }
            return
        otherwise
            rethrow(me);
    end
end
error('failed');

%--------------------------------------------------------------------------
function test_write_fill_value(ncfile,mode)
% Fill values for netcdf-4 files should only be set before the nc_enddef 
% has been called.  It's fine for netcdf-3 files, though.

nc_create_empty(ncfile,mode);
nc_adddim(ncfile,'x',5);
v.Name = 'y';
v.Dimension = {'x'};
nc_addvar(ncfile,v);

% If we try to write '_FillValue' as an attribute and if the file is netcdf-4,
% we should error out.
info = nc_info(ncfile);
if strcmp(info.Format,'netcdf-java')
    
    fprintf('\tSkipping test_write_fill_value test... ');
    
elseif strcmp(info.Format,'NetCDF-4 Classic')

	% This should issue an error.
	try
		nc_attput(ncfile,'y','_FillValue',-99);
	catch me
        switch(me.identifier)
            case {'snctools:attput:netcdf4ClassicFillValue'}
                return;
            otherwise
                rethrow(me);
        end
	end

	% If we get this far, then we failed to detect the condition.
   	error('failed');

else
	% OK it's not netcdf-4, so rewriting the fill value is fine.
	nc_attput(ncfile,'y','_FillValue',-99);
	fv = nc_attget(ncfile,'y','_FillValue');
	if fv ~= -99
    	error('failed');
	end

end
