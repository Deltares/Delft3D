function str = nc_datatype_string ( datatype_id )
%NC_DATATYPE_STRING Construct string representation of netcdf type id.
%   This function is not recommended.
%
%   DSTRING = NC_DATATYPE_STRING(TYPE_ID) takes a numeric type TYPE_ID and 
%   returns a string equivalent, DSTRING.  
%
%
%   The type conversions are as follows:
%
%          0 ==> 'NC_NAT'.
%          1 ==> 'NC_BYTE'.
%          2 ==> 'NC_CHAR'.
%          3 ==> 'NC_SHORT'.
%          4 ==> 'NC_INT'.
%          5 ==> 'NC_FLOAT'.
%          6 ==> 'NC_DOUBLE'.
%



warning ( 'SNCTOOLS:nc_datatype_string:deprecated', ...
    '%s is deprecated and may be removed in a future version of SNCTOOLS.', ...
    upper(mfilename)  );


if ~isnumeric ( datatype_id )
	error ( 'SNCTOOLS:NC_DATATYPE_STRING:badInput', 'datatype ID must be numeric' );
end

switch ( datatype_id )
    case nc_nat 
        str = 'NC_NAT';
    case nc_byte
        str = 'NC_BYTE';
    case nc_char
        str = 'NC_CHAR';
    case nc_short
        str = 'NC_SHORT';
    case nc_int
        str = 'NC_INT';
    case nc_float
        str = 'NC_FLOAT';
    case nc_double
        str = 'NC_DOUBLE';
    otherwise
        error ( 'SNCTOOLS:NC_DATATYPE_STRING:badInput', ...
            'unhandled type ID %d', datatype_id );
end

return
