function diminfo = nc_getdiminfo_hdf4_2011a(arg1,arg2)
% info = nc_getdiminfo(sds_id);

import matlab.io.hdf4.*

diminfo = struct('Name','','Length',0,'Unlimited',0);

if nargin == 1
    sds_id = arg1;
else
    sd_id = sd.start(arg1,'read');
    idx = sd.nameToIndex(sd_id,arg2);
    sds_id = sd.select(sd_id,idx);
end

% Just one dimension to look up here.
dimid = sd.getDimID(sds_id,0);
[name,count] = sd.dimInfo(dimid);

diminfo.Name = name;
if isinf(count)
    diminfo.Unlimited = 1;
    diminfo.Length = 0;
else
    diminfo.Unlimited = 0;
    diminfo.Length = count;
end

if exist('sds_id','var')
	sd.endAccess(sds_id);
end
if exist('sd_id','var')
	sd.close(sd_id);
end
