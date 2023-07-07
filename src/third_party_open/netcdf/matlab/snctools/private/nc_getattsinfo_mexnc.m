function attribute = nc_getattsinfo_mexnc(cdfid,varid,attnum)
% NC_GETATTSINFO:  Returns a NetCDF attributes as a struct array
%
% You don't want to be calling this routine directly.  Just don't use 
% it.  Use nc_attget instead.  Go away.  Nothing to see here, folks.  
% Move along, move along.
%
% USAGE:  attstruct = nc_getattsinfo ( cdfid, varid, attnum );
%
% PARAMETERS:
% Input:
%     cdfid:  NetCDF file id
%     varid:  NetCDF variable id
%     attnum:  number of attribute
% Output:
%     attstruct:  structure with "Name", "Nctype", "Attnum", and "Value" 
%                 fields
%
% In case of an error, an exception is thrown.

% Fill the attribute struct with default values
attribute = struct('Name','','Nctype',NaN,'Datatype','','Value',NaN);


[attname, status] = mexnc('INQ_ATTNAME', cdfid, varid, attnum);
if status < 0 
    ncerr = mexnc('strerror',status);
	error ( 'snctools:nc_getattsinfo:inq_attname', ncerr);
end
attribute.Name = attname;

[att_datatype, status] = mexnc('INQ_ATTTYPE', cdfid, varid, attname);
if status < 0 
    ncerr = mexnc('strerror',status);
	error ( 'snctools:nc_getattsinfo:inq_atttype', ncerr);
end

attribute.Nctype = att_datatype;
switch(att_datatype)
    case nc_nat
        attribute.Datatype = '';
    case nc_byte
        attribute.Datatype = 'int8';
    case nc_char
        attribute.Datatype = 'char';
    case nc_short
        attribute.Datatype = 'int16';
    case nc_int
        attribute.Datatype = 'int32';
    case nc_float
        attribute.Datatype = 'single';
    case nc_double
        attribute.Datatype = 'double';
end

switch att_datatype
    case 0
        attval = NaN;
    case nc_char
        [attval, status]=mexnc('get_att_text',cdfid,varid,attname);
    case { nc_double, nc_float, nc_int, nc_short, nc_byte }
        [attval, status]=mexnc('get_att_double',cdfid,varid,attname);
    otherwise
        error ( 'snctools:nc_getattsinfo:unhandledAttributeType', ...
            'Unhandled attribute type %d.', att_datatype );
end
if status < 0 
    ncerr = mexnc('strerror',status);
	error ( 'snctools:nc_getattsinfo:get_att', ncerr);
end

% this puts the attribute into the variable structure
attribute.Value = attval;


return


