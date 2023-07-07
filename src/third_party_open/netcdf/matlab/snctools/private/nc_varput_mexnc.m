function nc_varput_mexnc(ncfile,varname,data,varargin)
% Handler for MEXNC backend for NC_VARPUT.

preserve_fvd = nc_getpref('PRESERVE_FVD');

[ncid, status] = mexnc('open',ncfile,nc_write_mode);
if (status ~= 0)
    ncerr = mexnc('strerror',status);
    error('snctools:varput:mexnc:OPEN',ncerr);
end

try
    % check to see if the variable already exists.  
    [varid, status] = mexnc('INQ_VARID',ncid,varname);
    if ( status ~= 0 )
        ncerr = mexnc('strerror',status);
        error('snctools:varput:mexnc:INQ_VARID',ncerr);
    end
    
    v = nc_getvarinfo_mexnc(ncid,varid);
    nvdims = numel(v.Dimension);
    [start,count,stride] = snc_get_varput_indexing(nvdims,v.Size,size(data),varargin{:});

    nc_count = v.Size;
    
    % check that the length of the start argument matches the rank of the variable.
    if length(start) ~= length(nc_count)
        error('snctools:varput:badIndexing', ...
                ['Length of START index (%d) does not make sense with a ' ...
                 'variable rank of %d.'], ...
                length(start), length(nc_count) );
    end
    
    
    % Figure out which write routine we will use. 
    if isempty(start) || (nvdims == 0)
        write_op = 'put_var';
        if (numel(data) ~= prod(v.Size))
            error('snctools:varput:mexnc:varput:dataSizeMismatch', ...
    	        'Attempted to write wrong amount of data to %s.', v.Name );
        end
    elseif isempty(count)
        write_op = 'put_var1';
        if ( numel(data) ~= 1 )
            error('snctools:varput:mexnc:putVara:dataSizeMismatch', ...
    	        'Amount of data to be written to %s does not match up with count argument.', ...
                v.Name );        
        end
    elseif isempty(stride)
        write_op = 'put_vara';
        if (numel(data) ~= prod(count))
            error('snctools:varput:mexnc:putVara:dataSizeMismatch', ...
    	        'Amount of data to be written to %s does not match up with count argument.', ...
                v.Name );
        end
    else
        write_op = 'put_vars';
        if (numel(data) ~= prod(count))
            error('snctools:varput:mexnc:putVars:dataSizeMismatch', ...
    	        'Amount of data to be written to %s does not match up with count argument.', ...
                v.Name );
        end    
    end
    
    data = handle_scaling(ncid,varid,data);
    data = handle_fill_value(ncid,varid,data);
    
    if preserve_fvd
        start = fliplr(start);
        count = fliplr(count);
        stride = fliplr(stride);
    else
        data = permute(data,fliplr(1:ndims(data)));
    end
    
    write_the_data(ncid,varid,start,count,stride,write_op,data);
    
catch %#ok<CTCH>
	mexnc('close',ncid);
	rethrow(lasterror); %#ok<LERR>
end

status = mexnc('close',ncid);
if ( status ~= 0 )
    error('snctools:nc_varput:close',mexnc('STRERROR',status));
end


return



%--------------------------------------------------------------------------
function data = handle_scaling(ncid,varid,data)
% If there is a scale factor and/or  add_offset attribute, convert the data
% to double precision and apply the scaling.

[dud, dud, status] = mexnc('INQ_ATT',ncid,varid,'scale_factor'); %#ok<ASGLU>
if ( status == 0 )
    have_scale_factor = 1;
else
    have_scale_factor = 0;
end
[dud, dud, status] = mexnc('INQ_ATT',ncid,varid,'add_offset'); %#ok<ASGLU>
if ( status == 0 )
    have_add_offset = 1;
else
    have_add_offset = 0;
end

%
% Return early if we don't have either one.
if ~(have_scale_factor || have_add_offset)
    return;
end

scale_factor = 1.0;
add_offset = 0.0;

if have_scale_factor
    [scale_factor, status] = mexnc('get_att_double',ncid,varid,'scale_factor');
    if ( status ~= 0 )
        ncerr = mexnc('strerror', status);
        error ( 'snctools:varput:mexnc:GET_ATT_DOUBLE', ncerr );
    end
end

if have_add_offset
    [add_offset, status] = mexnc('get_att_double',ncid,varid,'add_offset');
    if ( status ~= 0 )
        ncerr = mexnc('strerror', status);
        error ( 'snctools:varput:mexnc:GET_ATT_DOUBLE', ncerr );
    end
end

[var_type,status]=mexnc('INQ_VARTYPE',ncid,varid);
if status ~= 0 
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varput:mexnc:INQ_VARTYPE', ncerr );
end

data = (double(data) - add_offset) / scale_factor;

%
% When scaling to an integer, we should add 0.5 to the data.  Otherwise
% there is a tiny loss in precision, e.g. 82.7 should round to 83, not 
% .
switch var_type
    case { nc_int, nc_short, nc_byte, nc_char }
        data = round(data);
end


return









%--------------------------------------------------------------------------
function data = handle_fill_value(ncid,varid,data)

[vartype, status] = mexnc('INQ_VARTYPE', ncid, varid);
if status ~= 0
    ncerr = mexnc('strerror', status);
    error('snctools:nc_varput:mexnc:inqVarTypeFailed', ncerr );
end

% Handle the fill value.  We do this by changing any NaNs into
% the _FillValue.  That way the netcdf library will recognize it.
[att_type, dud, status] = mexnc('INQ_ATT',ncid,varid,'_FillValue'); %#ok<ASGLU>
if ( status == 0 )

    if att_type ~= vartype
        warning('snctools:nc_varget:mexnc:missingValueMismatch', ...
            'The _FillValue datatype is wrong and will not be honored.');
        return
    end

    switch ( class(data) )
        case 'double'
            funcstr = 'get_att_double';
        case 'single'
            funcstr = 'get_att_float';
        case 'int32'
            funcstr = 'get_att_int';
        case 'int16'
            funcstr = 'get_att_short';
        case 'int8'
            funcstr = 'get_att_schar';
        case 'uint8'
            funcstr = 'get_att_uchar';
        case 'char'
            funcstr = 'get_att_text';
        otherwise
            error ( 'snctools:varput:unhandledDatatype', ...
                'Unhandled datatype for fill value, ''%s''.', ...
                class(data) );
    end

    [fill_value, status] = mexnc(funcstr,ncid,varid,'_FillValue');
    if ( status ~= 0 )
        ncerr = mexnc('strerror', status);
        err_id = [ 'snctools:varput:mexnc:' funcstr ];
        error ( err_id, ncerr );
    end

    data(isnan(data)) = fill_value;

end

    







%--------------------------------------------------------------------------
function write_the_data(ncid,varid,start,count,stride,write_op,pdata)

% write the data
switch ( write_op )
    
    case 'put_var1'
        switch ( class(pdata) )
            case 'double'
                funcstr = 'put_var1_double';
            case 'single'
                funcstr = 'put_var1_float';
            case 'int32'
                funcstr = 'put_var1_int';
            case 'int16'
                funcstr = 'put_var1_short';
            case 'int8'
                funcstr = 'put_var1_schar';
            case 'uint8'
                funcstr = 'put_var1_uchar';
            case 'char'
                funcstr = 'put_var1_text';
            otherwise
                error ( 'snctools:varput:unhandledMatlabType', ...
                    'unhandled data class %s\n', ...
                    class(pdata));
        end
        status = mexnc (funcstr, ncid, varid, start, pdata );
        
    case 'put_var'
        switch ( class(pdata) )
            case 'double'
                funcstr = 'put_var_double';
            case 'single'
                funcstr = 'put_var_float';
            case 'int32'
                funcstr = 'put_var_int';
            case 'int16'
                funcstr = 'put_var_short';
            case 'int8'
                funcstr = 'put_var_schar';
            case 'uint8'
                funcstr = 'put_var_uchar';
            case 'char'
                funcstr = 'put_var_text';
            otherwise
                error ( 'snctools:varput:unhandledMatlabType', ...
                    'unhandled data class %s\n', class(pdata)  );
        end
        status = mexnc (funcstr, ncid, varid, pdata );
        
    case 'put_vara'
        switch ( class(pdata) )
            case 'double'
                funcstr = 'put_vara_double';
            case 'single'
                funcstr = 'put_vara_float';
            case 'int32'
                funcstr = 'put_vara_int';
            case 'int16'
                funcstr = 'put_vara_short';
            case 'int8'
                funcstr = 'put_vara_schar';
            case 'uint8'
                funcstr = 'put_vara_uchar';
            case 'char'
                funcstr = 'put_vara_text';
            otherwise
                error ( 'snctools:varput:unhandledMatlabType',...
                    'unhandled data class %s\n', class(pdata) );
        end
        status = mexnc (funcstr, ncid, varid, start, count, pdata );
        
    case 'put_vars'
        switch ( class(pdata) )
            case 'double'
                funcstr = 'put_vars_double';
            case 'single'
                funcstr = 'put_vars_float';
            case 'int32'
                funcstr = 'put_vars_int';
            case 'int16'
                funcstr = 'put_vars_short';
            case 'int8'
                funcstr = 'put_vars_schar';
            case 'uint8'
                funcstr = 'put_vars_uchar';
            case 'char'
                funcstr = 'put_vars_text';
            otherwise
                error ( 'snctools:varput:unhandledMatlabType', ...
                    'unhandled data class %s\n', class(pdata) );
        end
        status = mexnc(funcstr,ncid,varid,start,count,stride,pdata);
        
    otherwise
        error ( 'snctools:varput:unhandledWriteOp', ...
            'unknown write operation''%s''.\n', write_op );
            
end

if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    error ( 'snctools:varput:writeOperationFailed', ...
        'write operation ''%s'' failed with error ''%s''.', ...
        write_op, ncerr);
end

return
