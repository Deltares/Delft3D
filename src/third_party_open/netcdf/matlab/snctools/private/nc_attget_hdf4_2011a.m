function data = nc_attget_hdf4_2011a(hfile,varname,attname)
% HDF4 handler for NC_ATTGET, 2011a and later.

import matlab.io.hdf4.*

sd_id = sd.start(hfile);

try
    if isnumeric(varname)
        obj_id = sd_id;
    elseif ischar(varname) && strcmp(varname,'GLOBAL')
        
        % Is it a variable or global?
        try
            idx = sd.nameToIndex(sd_id,varname);
            sds_id = sd.select(sd_id,idx);
            obj_id = sds_id;
        catch me %#ok<NASGU>
            obj_id = sd_id;
        end
        
    else
        
        idx = sd.nameToIndex(sd_id,varname);
        sds_id = sd.select(sd_id,idx);
        obj_id = sds_id;
        
    end
catch me
    if exist('sds_id','var')
        sd.endAcess(sds_id);
    end
    rethrow(me);
end
	
attr_idx = sd.findAttr(obj_id,attname);
data = sd.readAttr(obj_id,attr_idx);

if exist('sds_id','var')
    sd.endAccess(sds_id);
end
sd.close(sd_id);

