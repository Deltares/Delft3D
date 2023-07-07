function data = nc_attget_hdf4(hfile,varname,attname)
% HDF4 handler for NC_ATTGET.

is_var = true;

sd_id = hdfsd('start',hfile,'read');
if sd_id < 0
    error('snctools:attget:hdf4:start', 'START failed on %s.', hfile);
end

if isnumeric(varname)
    is_var = false;
    obj_id = sd_id;
elseif ischar(varname) && strcmp(varname,'GLOBAL')
    
    % Is it a variable or global?
    idx = hdfsd('nametoindex',sd_id,varname);   
    if idx < 0
        is_var = false;
        obj_id = sd_id;
    else
        obj_id = hdfsd('select',sd_id,idx);
    end
        
else
    
    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        hdfsd('end',sd_id);
        error('snctools:attget:hdf4:nametoindex', ...
            'NAMETOINDEX failed on %s, %s.', varname, hfile);
    end
    
    sds_id = hdfsd('select',sd_id,idx);
    if sds_id < 0
        hdfsd('end',sd_id);
        error('snctools:attget:hdf4:select', ...
            'SELECT failed on %s, %s.', varname, hfile);
    end
    
    obj_id = sds_id;
end

attr_idx = hdfsd('findattr',obj_id,attname);
if attr_idx < 0
    if is_var
        hdfsd('endaccess',obj_id);
    end
    hdfsd('end',sd_id);
    error('snctools:attget:hdf4:findattr', ...
        'Attribute "%s" does not exist.', attname);
end

[data,status] = hdfsd('readattr',obj_id,attr_idx);
if status < 0
    if is_var
        hdfsd('endaccess',obj_id);
    end
    hdfsd('end',sd_id);
    error('snctools:attget:hdf4:readattr', ...
        'READATTR failed on %s, %s, %s.', hfile, varname, attname);
end


if is_var
    status = hdfsd('endaccess',obj_id);
    if status < 0
        error('snctools:attput:hdf4:endaccess', ...
            'ENDACCESS failed on %s.', varname);
    end
end

status = hdfsd('end', sd_id);
if status < 0
    error('snctools:attget:hdf4:end', 'END failed on %s.', hfile);
end


