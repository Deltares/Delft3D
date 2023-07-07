function nc_adddim_hdf4_2011b(hfile,dimname,dimlen)
% HDF4 backend to NC_ADDDIM.

import matlab.io.hdf4.*

if ~exist(hfile,'file')
    sd_id = sd.start(hfile,'create');
else
    sd_id = sd.start(hfile,'write');
end

try
    % Is there already a dataset with this name?
    idx = sd.nameToIndex(sd_id,dimname);
catch me
    idx = -1;
end
if idx >=0
    sd.close(sd_id);
    error('snctools:addDim:hdf4:badName', ...
        'There is already a dataset with this name, "%s".', dimname);
end
    
try
    % is it unlimited?  Netcdf conventions make this -1.
    if (dimlen == -1) || (dimlen == 0) || isinf(dimlen)
        create_arg = inf;
    else
        create_arg = dimlen;
    end
    sds_id = sd.create(sd_id,dimname,class(dimlen),create_arg);
    
    % ok, now make it a dimension as well
    dimid = sd.getDimID(sds_id,0);
    
    sd.setDimName(dimid,dimname);
    sd.endAccess(sds_id);

catch me
    if exist('sds_id','var')
        sd.endAccess(sds_id);
    end
    sd.close(sd_id);
    rethrow(me);
end

sd.close(sd_id);

return

