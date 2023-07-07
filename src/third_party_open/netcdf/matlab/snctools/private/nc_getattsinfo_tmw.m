function attribute = nc_getattsinfo_tmw(ncid,varid,attnum)
% NC_GET_ATTRIBUTE_STRUCT_TMW:  Returns a NetCDF attribute as a structure
%
% You don't want to be calling this routine directly.  Just don't use 
% it.  Use nc_attget instead.  Go away.  Nothing to see here, folks.  
% Move along, move along.


attribute = struct('Name','','Nctype','','Datatype','','Value',NaN);


attname = netcdf.inqAttName(ncid, varid, attnum);
attribute.Name = attname;

[att_datatype] = netcdf.inqAtt(ncid, varid, attname);
attribute.Nctype = att_datatype;
switch(att_datatype)
    case nc_nat
        attribute.Datatype = '';
    case nc_byte
        attribute.Datatype = 'int8';
    case nc_ubyte
        attribute.Datatype = 'uint8';
    case nc_char
        attribute.Datatype = 'char';
    case nc_short
        attribute.Datatype = 'int16';
    case nc_ushort
        attribute.Datatype = 'uint16';
    case nc_int
        attribute.Datatype = 'int32';
    case nc_uint
        attribute.Datatype = 'uint32';
    case nc_int64
        attribute.Datatype = 'int64';
    case nc_uint64
        attribute.Datatype = 'uint64';
    case nc_float
        attribute.Datatype = 'single';
    case nc_double
        attribute.Datatype = 'double';
    otherwise
        attribute.Datatype = '';
end

switch att_datatype
    case 0
        attribute.Value = NaN;
    case { nc_char, nc_int64, nc_uint64 }
        attribute.Value=netcdf.getAtt(ncid,varid,attname);
    case { nc_double, nc_float, nc_int, nc_short, nc_byte, nc_ubyte, nc_ushort, nc_uint }
        attribute.Value=netcdf.getAtt(ncid,varid,attname,'double');
    otherwise
        attribute.Value = [];
end


return


