function nc_varput_tmw(ncfile,varname,data,varargin)

preserve_fvd = nc_getpref('PRESERVE_FVD');

ncid = netcdf.open(ncfile,'WRITE');
try
    varid = netcdf.inqVarID(ncid,varname);
    [dud,xtype,dims]=netcdf.inqVar(ncid,varid); %#ok<ASGLU>
    ndims = numel(dims);
    
    info = nc_getvarinfo_tmw(ncid,varid);
    [start,count,stride] = snc_get_varput_indexing(ndims,info.Size,size(data),varargin{:});
    if ~preserve_fvd
        start = fliplr(start); 
        count = fliplr(count); 
        stride = fliplr(stride);
    end
    data = pre_process(ncid,varid,xtype,preserve_fvd,data);

    if ndims == 0
        netcdf.putVar(ncid,varid,data);
    else
        netcdf.putVar(ncid,varid,start,count,stride,data);
    end

catch myException
    netcdf.close(ncid);
    rethrow(myException);
end

netcdf.close(ncid);
return


%--------------------------------------------------------------------------
function data = pre_process(ncid,varid,xtype,preserve_fvd,data)

if ~preserve_fvd
    data = permute(data,fliplr(1:ndims(data)));
end

data = handle_scaling(ncid,varid,data);
data = handle_fill_value(ncid,varid,data);
if ( xtype == nc_char ) && (~ischar(data))
    data = char(data);
end
%--------------------------------------------------------------------------
function data = handle_scaling(ncid,varid,data)
% If there is a scale factor and/or  add_offset attribute, convert the data
% to double precision and apply the scaling.

have_scale_factor = 0;
have_add_offset = 0;


varname = netcdf.inqVar(ncid,varid);
try
    att_type = netcdf.inqAtt(ncid, varid, 'scale_factor' );
    if att_type == netcdf.getConstant('NC_CHAR')
        warning('snctools:varput:scaleFactorShouldNotBeChar', ...
            'The scale_factor attribute for %s should not be char, it will be ignored.', ...
            varname);
    else
        have_scale_factor = 1;
        scale_factor = netcdf.getAtt(ncid, varid, 'scale_factor','double');
    end
catch %#ok<CTCH>
    scale_factor = 1.0;
end

try
    att_type = netcdf.inqAtt(ncid, varid, 'add_offset' );
    if att_type == netcdf.getConstant('NC_CHAR')
        warning('snctools:varput:addOffsetShouldNotBeChar', ...
            'The add_offset attribute for %s should not be char, it will be ignored.', ...
            varname);
    else
        have_add_offset = 1;
        add_offset = netcdf.getAtt(ncid, varid, 'add_offset','double');
    end
catch %#ok<CTCH>
    add_offset = 0.0;
end

%
% Return early if we don't have either one.
if ~(have_scale_factor || have_add_offset)
    return;
end


data = (double(data) - add_offset) / scale_factor;

%
% When scaling to an integer, we should add 0.5 to the data.  Otherwise
% there is a tiny loss in precision, e.g. 82.7 should round to 83, not
% 82.
[varname,xtype] = netcdf.inqVar(ncid,varid);  %#ok<ASGLU>
switch xtype
    case { nc_int, nc_short, nc_byte, nc_char }
        data = round(data);
end


return









%--------------------------------------------------------------------------
function data = handle_fill_value(ncid,varid,data)
% Handle the fill value.  We do this by changing any NaNs into
% the _FillValue.  That way the netcdf library will recognize it.
try
    
    [varname,xtype] = netcdf.inqVar(ncid,varid);
    att_type = netcdf.inqAtt(ncid,varid,'_FillValue');
    if att_type ~= xtype
        warning('snctools:varput:badFillValueType', ...
            ['The datatype for the "_FillValue" attribute does not match ' ...
            'the datatype of the "%s" variable.  It will be ignored.'], ...
            varname);
        return
    end
    
    switch ( class(data) )
        case 'double'
            myClass = 'double';
        case 'single'
            myClass = 'float';
        case 'int32'
            myClass = 'int';
        case 'int16'
            myClass = 'short';
        case 'int8'
            myClass = 'schar';
        case 'uint8'
            myClass = 'uchar';
        case 'char'
            myClass = 'text';
        otherwise
            error ( 'snctools:varput:unhandledDatatype', ...
                'Unhandled datatype for fill value, ''%s''.', ...
                class(data) );
    end

    fill_value  = netcdf.getAtt(ncid,varid,'_FillValue',myClass);

    data(isnan(data)) = fill_value;

catch myException %#ok<NASGU>
    return
end
