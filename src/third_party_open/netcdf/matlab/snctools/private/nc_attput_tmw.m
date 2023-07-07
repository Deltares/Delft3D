function nc_attput_tmw(ncfile,varname,attribute_name,attval)
% TMW backend for NC_ATTPUT.

if isnumeric(ncfile) && isnumeric(varname)
    % the variable and file are already open, write the attribute now.
    attput(ncfile,varname,attribute_name,attval);
    return
end

ncid = netcdf.open(ncfile,'write');
netcdf.reDef(ncid);
try
    attput(ncid,varname,attribute_name,attval);
catch e
    netcdf.close(ncid);
    handle_error(e);
end
netcdf.endDef(ncid);
netcdf.close(ncid);

return;


%---------------------------------------------------------------------------
function attput(ncid,varname,attribute_name,attval)

library_version = netcdf.inqLibVers();
library_version = str2double(library_version(1));


% If netcdf-4, make sure that the user did not try to set the 
% fill value with NC_ATTPUT.
if strcmp(attribute_name,'_FillValue') && (library_version >= 4)
    fmt = netcdf.inqFormat(ncid);
    switch(fmt)
        case {'FORMAT_CLASSIC','FORMAT_64BIT'}
            % this is ok
        case {'FORMAT_NETCDF4','FORMAT_NETCDF4_CLASSIC'}
            error('snctools:attput:netcdf4ClassicFillValue', ...
                ['FillValues for netcdf-4 files should be set with ' ...
                 'NC_ADDVAR instead of NC_ATTPUT.']);
    end
end


% If netcdf-4, then a few additional checks on the datatype are necessary.
if library_version >= 4
    fmt = netcdf.inqFormat(ncid);
    switch(class(attval))
        case 'uint8'
            % Must convert it to int8
            switch(fmt)
                case {'FORMAT_CLASSIC','FORMAT_64BIT'}
                    % this is ok
                case {'FORMAT_NETCDF4','FORMAT_NETCDF4_CLASSIC'}
                    attval = typecast(attval,'int8');
            end
            
        case {'uint16','uint32','uint64','int64'}
            if strcmp(fmt,'FORMAT_NETCDF4_CLASSIC')
                error('snctools:attput:badDatatype', ...
                    ['%s is not an allowed datatype under the ' ...
                     'classic model.'], ...
                     class(attval));
			end
            
    end
end

if isnumeric(varname)
    varid = varname;
else
    varid = netcdf.inqVarID(ncid,varname);
end


% If we are dealing with the fill value, then force the type to be
% correct.
if strcmp(attribute_name,'_FillValue')
    [name,xtype] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
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
    
    % If this is a netcdf-4 file, then don't treat _FillValue as an
    % attribute.  Let the library take care of it.
    if library_version > 3
        fmt = netcdf.inqFormat(ncid);
        if strcmp(fmt,'FORMAT_NETCDF4') ...
            || strcmp(fmt,'FORMAT_NETCDF4_CLASSIC')
            netcdf.defVarFill(ncid,varid,false,attval);
            return
        end
    end
end

try
    if iscellstr(attval) && (numel(attval) == 1)
        netcdf.putAtt(ncid,varid,attribute_name,attval{1});
    else
        netcdf.putAtt(ncid,varid,attribute_name,attval);
    end
catch me
    switch(me.identifier)
        case 'MATLAB:netcdf_common:emptySetArgument'
            % Bug #609383
            % Please consult the README.
            %
            % If char, change attval to ' '
            warning('snctools:NCATTPUT:emptyAttributeBug', ...
                ['Changing attribute from empty to single space, ' ...
                'please consult the README regarding Bug #609383.']);
            netcdf.putAtt(ncid,varid,attribute_name,' ');
        otherwise
            rethrow(me);
    end
    
end
    

%--------------------------------------------------------------------------
function handle_error(e)
v = version('-release');
switch(e.identifier)
    
    case 'MATLAB:imagesci:netcdf:libraryFailure'   
        
        switch(v)
            case '2011b'
                if strfind(e.message,'enotvar:variableNotFound')
                    error(e.identifier, ...
                        'Variable not found.');
                else
                    rethrow(e);
                end
            otherwise
                rethrow(e);
        end
     
    otherwise
        rethrow(e);
        
end
