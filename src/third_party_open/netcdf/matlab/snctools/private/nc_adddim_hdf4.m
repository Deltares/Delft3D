function nc_adddim_hdf4(hfile,dimname,dimlen)
% HDF4 backend to NC_ADDDIM.

if ~exist(hfile,'file')
    sd_id = hdfsd('start',hfile,'create');
else
    sd_id = hdfsd('start',hfile,'write');
end

if sd_id < 0
    error('snctools:addDim:hdf4:startFailed', ...
        'START failed on %s.', hfile);
end

try
    % Is there already a dataset with this name?
    idx = hdfsd('nametoindex',sd_id,dimname);
    if idx >=0
        error('snctools:addDim:hdf4:badName', ...
            'There is already a dataset with this name, "%s".', dimname);
    end
    
    % is it unlimited?  Netcdf conventions make this -1.
    if (dimlen == -1) || (dimlen == 0) || isinf(dimlen)
        create_arg = inf;
    else
        create_arg = dimlen;
    end
    sds_id = hdfsd('create',sd_id,dimname,class(dimlen),1,create_arg);
    if sds_id < 0
        error('snctools:addVar:hdf4:startFailed', ...
            'CREATE failed on %s.', hfile);
    end
    
    % ok, now make it a dimension as well
    dimid = hdfsd('getdimid',sds_id,0);
    if dimid < 0
        error('snctools:addDim:hdf4:getdimidFailed', ...
            'GETDIMID failed on %s, %s.', dimname, hfile);
    end
    
    status = hdfsd('setdimname',dimid,dimname);
    if status < 0
        error('snctools:addDim:hdf4:setdimnameFailed', ...
            'SETDIMNAME failed on %s.', hfile);
    end

catch
    if exist('sds_id','var')
        hdfsd('endaccess',sds_id);
    end
    hdfsd('end',sd_id);
    e = lasterror;
    error(e.identifier,e.message);
end

status = hdfsd('endaccess',sds_id);
if status < 0
    hdfsd('end',sd_id);
    error('snctools:addDim:hdf4:endaccessFailed', ...
        'ENDACCESS failed on %s.', hfile);
end

status = hdfsd('end',sd_id);
if status < 0
    error('snctools:addDim:hdf4:endFailed', ...
        'END failed on %s, \"%s\".', hfile);
end
return

