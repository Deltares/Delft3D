function nc_adddim_tmw ( ncfile, dimension_name, dimension_length )
% TMW backend for NC_ADDDIM.

ncid = netcdf.open(ncfile, nc_write_mode );

try
    netcdf.reDef(ncid );
    netcdf.defDim(ncid, dimension_name, dimension_length);
    netcdf.endDef(ncid );
catch myException
    netcdf.close(ncid);
    rethrow(myException);
end

netcdf.close(ncid);

return







