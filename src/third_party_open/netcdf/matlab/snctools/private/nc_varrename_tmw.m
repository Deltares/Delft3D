function nc_varrename_tmw ( ncfile, old_variable_name, new_variable_name )
ncid=netcdf.open(ncfile,nc_write_mode);
try
    netcdf.reDef(ncid);
    varid = netcdf.inqVarID(ncid, old_variable_name);
    netcdf.renameVar(ncid, varid, new_variable_name);
    netcdf.endDef(ncid);
catch myException
    netcdf.close(ncid);
    rethrow(myException);
end
netcdf.close(ncid);