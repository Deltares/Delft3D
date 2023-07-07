function value = nc_getVarByName(nc_id, varname)
%nc_getVarByName Return data from netCDF variable by name.
%   value = nc_getVarByName(nc_id, varname) looks in NetCDF source with ID
%   nc_id. Returns empty matrix when varname is nonexistent.
%   When variable contains NetCDF-fill values, these are replaced by NaNs.

% $Id$

try
    id_var = netcdf.inqVarID(nc_id, varname);
    value = netcdf.getVar(nc_id, id_var);
    
    % Try to find fill value at 1: variable level, 2: global file level,
    % 3: netcdf default level.
    try
        fill_value = netcdf.getAtt(nc_id, id_var, '_FillValue');
    catch
        try
            fill_value = netcdf.getAtt(nc_id, netcdf.getConstant('NC_GLOBAL'), '_FillValue');
        catch
            fill_value = netcdf.getConstant('NC_FILL_FLOAT');
        end
    end

    [name,xtype] = netcdf.inqVar(nc_id,id_var);
    if ( xtype~=netcdf.getConstant('NC_CHAR') )
        value(value==fill_value) = NaN;
    end
catch
    warning('Variable ''%s'' not found, returning empty value.', varname);
    value = [];
end
end