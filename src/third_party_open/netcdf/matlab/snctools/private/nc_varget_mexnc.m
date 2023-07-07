function [values,info] = nc_varget_mexnc(ncfile,varname,varargin)
% Handler for NC_VARGET it case where the old community mex-file mexnc must
% be used. 

preserve_fvd = nc_getpref('PRESERVE_FVD');

[ncid,status]=mexnc('open',ncfile,'NOWRITE');
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'snctools:varget:mexnc:open', ncerr );
end


try
    [varid, status]=mexnc('inq_varid',ncid,varname);
    if status ~= 0
        ncerr = mexnc('strerror', status);
        error ( 'snctools:varget:mexnc:inqVarID', ncerr );
    end

	info = nc_getvarinfo_mexnc(ncid,varid);

    [dud,var_type,nvdims,dimids,dud,status]=mexnc('inq_var',ncid,varid); %#ok<ASGLU>
    if status ~= 0
        error ( 'snctools:varget:mexnc:inqVar', mexnc('strerror',status) );
    end

    var_size = determine_varsize_mex(ncid,dimids,nvdims,preserve_fvd);
    if any(var_size==0)
        values = zeros(var_size); % values = [];
        return
    end

    [start,count,stride] = snc_get_indexing(nvdims,var_size,varargin{:});
    

    % mexnc does not preserve the fastest varying dimension.  If we want this,
    % then we flip the indices.
    if preserve_fvd
        start = fliplr(start);
        count = fliplr(count);
        stride = fliplr(stride);
    end

    
    % What mexnc operation will we use?
    [funcstr_family, funcstr] = determine_funcstr(var_type,nvdims,start,count,stride);


    % At long last, retrieve the data.
    switch funcstr_family
        case 'get_var'
            [values, status] = mexnc(funcstr,ncid,varid);

        case 'get_var1'
            [values, status] = mexnc(funcstr,ncid,varid,0);

        case 'get_vara'
            [values, status] = mexnc(funcstr,ncid,varid,start,count);


        case 'get_vars'
            [values, status] = mexnc(funcstr,ncid,varid,start,count,stride);

        otherwise
            error ( 'snctools:varget:mexnc:unhandledType', ...
                'Unhandled function string type ''%s''\n', funcstr_family);
    end

    if ( status ~= 0 )
        error('snctools:varget:mexnc:getVarFuncstrFailure', ...
            mexnc('strerror',status) );
    end


    % If it's a 1D vector, make it a column vector.
    % Otherwise permute the data
    % to make up for the row-major-order-vs-column-major-order issue.
    if length(var_size) == 1
        values = values(:);
    else
        % Ok it's not a 1D vector.  If we are not preserving the fastest
        % varying dimension, we should permute the data.
        if ~preserve_fvd
            pv = fliplr ( 1:length(var_size) );
            values = permute(values,pv);
        end
    end


catch %#ok<CTCH>
    mexnc('close',ncid);
    rethrow(lasterror);
end

mexnc('close',ncid);


return






%--------------------------------------------------------------------------
function [prefix,funcstr] = determine_funcstr(var_type,nvdims,start,count,stride)
% DETERMINE_FUNCSTR
%     Determines if we are to use, say, 'get_var1_text', or 'get_vars_double',
%     or whatever.

% Determine if we are retriving a single value, the whole variable, a 
% contiguous portion, or a strided portion.
if nvdims == 0
    % It is a singleton variable.
    prefix = 'get_var1';
else
    prefix = 'get_vars';
end



switch ( var_type )
    case nc_char
        funcstr = [prefix '_text'];

    case nc_double
        funcstr = [prefix '_double'];

    case nc_float
        funcstr = [prefix '_float'];

    case nc_int
        funcstr = [prefix '_int'];

    case nc_short
        funcstr = [prefix '_short'];

    case nc_byte
        funcstr = [prefix '_schar'];

    otherwise
        error ( 'snctools:nc_varget:mexnc:badDatatype', ...
                'Unhandled datatype %d.', var_type );

end
return





%-----------------------------------------------------------------------
function the_var_size = determine_varsize_mex(ncid,dimids,nvdims,preserve_fvd)
% DETERMINE_VARSIZE_MEX: Need to figure out just how big the variable is.
%
% VAR_SIZE = DETERMINE_VARSIZE_MEX(NCID,DIMIDS,NVDIMS);

%
% If not a singleton, we need to figure out how big the variable is.
if nvdims == 0
    the_var_size = 1;
else
    the_var_size = zeros(1,nvdims);
    for j=1:nvdims,
        dimid = dimids(j);
        [dim_size,status]=mexnc('inq_dimlen', ncid, dimid);
        if ( status ~= 0 )
            ncerr = mexnc ( 'strerror', status );
            error ( 'snctools:nc_varget:mexnc:inq_dimlen', ncerr );
        end
        the_var_size(j)=dim_size;
    end
end

if preserve_fvd
    the_var_size = fliplr(the_var_size);
end

return





