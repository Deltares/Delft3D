function nc_addvar_mexnc(ncfile,varstruct,preserve_fvd)


[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    error ( 'snctools:addvar:mexnc:OPEN', ...
        'OPEN failed on %s, ''%s''', ncfile, ncerr);
end

% determine the dimids of the named dimensions
num_dims = length(varstruct.Dimension);
dimids = zeros(num_dims,1);
for j = 1:num_dims
    [dimids(j), status] = mexnc ( 'dimid', ncid, varstruct.Dimension{j} );
    if ( status ~= 0 )
        mexnc ( 'close', ncid );
        ncerr = mexnc ( 'strerror', status );
        error ( 'snctools:addvar:mexnc:DIMID', ncerr );
    end
end

% If preserving the fastest varying dimension in mexnc, we have to 
% reverse their order.
if preserve_fvd
    dimids = flipud(dimids);
end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    mexnc ( 'close', ncid );
    error ( 'snctools:addvar:mexnc:REDEF', ncerr );
end

% We prefer to use 'Datatype' instead of 'Nctype', but we'll try to be 
% backwards compatible.
if isfield(varstruct,'Datatype')
    [varid, status] = mexnc ( 'DEF_VAR', ncid, varstruct.Name, ...
        varstruct.Datatype, num_dims, dimids );
else
    [varid, status] = mexnc ( 'DEF_VAR', ncid, varstruct.Name, ...
        varstruct.Nctype, num_dims, dimids );
end
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    mexnc ( 'endef', ncid );
    mexnc ( 'close', ncid );
    error ( 'snctools:addvar:mexnc:DEF_VAR', ncerr );
end


if ~isempty(varstruct.Chunking)

    if preserve_fvd
        chunking = fliplr(varstruct.Chunking);
    else
        chunking = varstruct.Chunking;
    end
    
    if ( numel(chunking) ~= num_dims) 
        mexnc ( 'endef', ncid );
        mexnc ( 'close', ncid );
        error ( 'snctools:addvar:mexnc:defVarChunking', ...
           'Chunking size does not jive with number of dimensions.');
    end

    status = mexnc('DEF_VAR_CHUNKING',ncid,varid,'chunked',chunking);
    if ( status ~= 0 )
        ncerr = mexnc ( 'strerror', status );
        mexnc ( 'endef', ncid );
        mexnc ( 'close', ncid );
        error ( 'snctools:addvar:mexnc:DEF_VAR_CHUNKING', ncerr );
    end
end

if (varstruct.Shuffle || varstruct.Deflate)

    status = mexnc('DEF_VAR_DEFLATE',ncid,varid, varstruct.Shuffle,varstruct.Deflate,varstruct.Deflate);
    if ( status ~= 0 )
        ncerr = mexnc ( 'strerror', status );
        mexnc ( 'endef', ncid );
        mexnc ( 'close', ncid );
        error ( 'snctools:addvar:mexnc:DEF_VAR_DEFLATE', ncerr );
    end
end

handle_attributes(ncid,varid,varstruct);

status = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    mexnc ( 'close', ncid );
    error ( 'snctools:addvar:mexnc:ENDDEF', ncerr );
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 )
    ncerr = mexnc ( 'strerror', status );
    error ( 'snctools:addvar:mexnc:CLOSE', ncerr );
end




%--------------------------------------------------------------------------
function handle_attributes(ncid,varid,varstruct)

if isempty(varstruct.Attribute)
    return
end

% Check for _FillValue.  Netcdf-4 is a special case.
v = mexnc('inq_libvers');
idx = find(strcmp({varstruct.Attribute.Name},'_FillValue'));
if ~isempty(idx) && str2double(v(1)) > 3
    fmt = mexnc('INQ_FORMAT',ncid);
    if strcmp(fmt,'FORMAT_NETCDF4') || strcmp(fmt,'FORMAT_NETCDF4_CLASSIC')
        
        attval = varstruct.Attribute(idx).Value;
        
        [xtype,status] = mexnc('INQ_VARTYPE',ncid,varid);
        if ( status ~= 0 )
            ncerr = mexnc ( 'strerror', status );
            mexnc ( 'close', ncid );
            error ( 'snctools:addvar:mexnc:inqVarType', ncerr );
        end

        switch(xtype)
            case nc_double
                attval = double(attval);
            case nc_float
                attval = single(attval);
            case nc_int
                attval = int32(attval);
            case nc_short
                attval = int16(attval);
            case nc_byte
                attval = int8(attval);
            case nc_char
                attval = char(attval);
        end
        status = mexnc('def_var_fill',ncid,varid,false,attval);
        if ( status ~= 0 )
            ncerr = mexnc ( 'strerror', status );
            mexnc ( 'close', ncid );
            error ( 'snctools:addvar:mexnc:defVarFill', ncerr );
        end
        
        % Remove the attribute name/pair value now.
        varstruct.Attribute(idx) = [];
    end
end


% Now just use nc_attput to put in the attributes
for j = 1:numel(varstruct.Attribute)
    attname = varstruct.Attribute(j).Name;
    attval = varstruct.Attribute(j).Value;
    nc_attput_mex(ncid,varid,attname,attval);
end
