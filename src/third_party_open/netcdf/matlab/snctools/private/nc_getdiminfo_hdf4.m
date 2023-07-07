function diminfo = nc_getdiminfo_hdf4(arg1,arg2)
% info = nc_getdiminfo(sds_id);
% info = nc_getdiminfo(hdffile,dimscale_name);

diminfo = struct('Name','','Length',0,'Unlimited',0);

if nargin == 1
    sds_id = arg1;
else
    sd_id = hdfsd('start',arg1,'read');
    if sd_id < 0
        error('snctools:getdiminfo:hdf4:startFailed', ...
            'START failed on %s.', arg1);
    end
    idx = hdfsd('nametoindex',sd_id,arg2);
    if idx < 0
        hdfsd('end',sd_id);
        error('snctools:getdiminfo:hdf4:nametoindexFailed', ...
            'NAMETOINDEX failed on %s.', arg2);
    end   
    sds_id = hdfsd('select',sd_id,idx);
    if sds_id < 0
        hdfsd('end',sd_id);
        error('snctools:getdiminfo:hdf4:selectFailed', ...
            'SELECT failed on %s.', arg2);
    end
    
end

% Just one dimension to look up here.
dimid = hdfsd('getdimid',sds_id,0);
if dimid < 0
    error('snctools:getdiminfo:hdf4:getdimidFailed', 'getdimid failed.' );
end

[name,count,dud,dud,status] = hdfsd('diminfo',dimid);
if status < 0
    error('snctools:getdiminfo:hdf4:getdimidFailed', 'diminfo failed.' );
end

diminfo.Name = name;
if isinf(count)
    diminfo.Unlimited = 1;
    diminfo.Length = 0;
else
    diminfo.Unlimited = 0;
    diminfo.Length = count;
end


if nargin > 1
    hdfsd('endaccess',sds_id);
    hdfsd('end',sd_id);
end
return
