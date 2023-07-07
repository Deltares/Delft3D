function fileinfo = nc_info_hdf4(hdf4file)
% HDF4 backend for NC_INFO

fileinfo.Name = '/';

% Get the full path name.
fid = fopen(hdf4file,'r');
fullfile = fopen(fid);
fclose(fid);

fileinfo.Filename = hdf4file; % [name ext];
fileinfo.Datatype = [];       % Never used for HDF4.

sd_id = hdfsd('start',fullfile,'read');
if sd_id < 0
    error('snctools:nc_info:hdf4:startFailed', ...
        'start failed on %s.', hdf4file);
end

[ndatasets,nglobal_attr,status] = hdfsd('fileinfo',sd_id);
if status < 0
    hdfsd('end',sd_id);
    error('snctools:nc_info:hdf4:fileinfoFailed', ...
        'fileinfo failed on %s.', hdf4file);
end

dim_count = 0;
dataset_count = 0;

for idx = 0:ndatasets-1
    
    sds_id = hdfsd('select',sd_id,idx);
    if sds_id < 0
        hdfsd('end',sd_id);
        error('snctools:nc_info:hdf4:selectFailed', ...
            'Select failed on dataset with index %d.', idx);
    end

	[sds_name,sds_rank,sds_dimsizes,dtype_wr,nattrs,status] = hdfsd('getinfo',sds_id); %#ok<ASGLU>
    if status < 0
        hdfsd('endaccess',sds_id);
        hdfsd('end',sd_id);
        error('snctools:nc_info:hdf4:getinfoFailed', ...
            'getinfo failed on dataset with index %d.', idx);
    end

	% Look at each dimension
	for dimidx = 0:sds_rank-1

        dimid = hdfsd('getdimid',sds_id,dimidx);
        if dimid < 0
            hdfsd('endaccess',sds_id);
            hdfsd('end',sd_id);
            error('snctools:varput:hdf4:getdimidFailed', 'GETDIMID failed on %s.', varname);
        end
        [dname,dcount,ddatatype,dattrs,status] = hdfsd('diminfo',dimid); %#ok<ASGLU>
        if status < 0
            hdfsd('endaccess',sds_id);
            hdfsd('end',sd_id);
            error('snctools:varput:hdf4:diminfoFailed', 'DIMINFO failed');
        end

		% Do we already have it?
		if (dim_count > 0) && any(strcmp(dname,{fileinfo.Dimension.Name}))
            % we already have it.
			continue;
        else
			dim_count = dim_count + 1;
			fileinfo.Dimension(dim_count).Name = dname;
			if isinf(dcount)
				fileinfo.Dimension(dim_count).Unlimited = true;
                %if isinf(sds_dimsizes(dimidx+1))
                %   fileinfo.Dimension(dim_count).Length = 0;
                %else
                    fileinfo.Dimension(dim_count).Length = sds_dimsizes(dimidx+1);
                %end
			else
				fileinfo.Dimension(dim_count).Unlimited = false;
				fileinfo.Dimension(dim_count).Length = dcount;
			end
		end

	end
    
	dataset_count = dataset_count + 1;
    fileinfo.Dataset(dataset_count) = nc_getvarinfo_hdf4(hdf4file,sds_name);
    
    status = hdfsd('endaccess',sds_id);
    if status < 0
        hdfsd('end',sd_id);
        error('snctools:nc_info:hdf4:endaccessFailed', ...
            'endaccess failed on dataset with index %d.', idx);
    end
end

if ndatasets == 0
    fileinfo.Dataset = [];
end


Attribute = [];
if nglobal_attr > 0
    
    Attribute = repmat(struct('Name','','Datatype','','Value',0),nattrs,1);
    for j = 0:nglobal_attr-1
        [name,atype,acount,status] = hdfsd('attrinfo',sd_id,j); %#ok<ASGLU>
        if status < 0
            error('snctools:nc_info:hdf4:attrinfoFailed', ...
                'Could not read attribute %d.', j );
        end
        Attribute(j+1).Name = name;
        
        [Attribute(j+1).Value, status] = hdfsd('readattr',sd_id,j);
        if status < 0
            error('snctools:nc_info:hdf4:readattrFailed', ...
                'Could not read attribute %d.',j );
        end
        Attribute(j+1).Datatype = class(Attribute(j+1).Value);
    end
end
fileinfo.Attribute = Attribute;


status = hdfsd('end',sd_id);
if status < 0
    error('snctools:nc_info:hdf4:endFailed', ...
        'end failed on %s.', hdf4file);
end

if isempty(fileinfo.Dataset) 
    return
end

return;

% Post process it.  For some reason, we cannot retrieve the length of an
% unlimited coordinate variable via the low-level interface.  Have to fudge
% it for the time being by getting the length from another variable that
% depends upon the unlimited dimension.
idx_of_unlimiteds = find(cell2mat({fileinfo.Dataset.Unlimited}));
if isempty(idx_of_unlimiteds)
    % no unlimited datasets, nothing to post process
    return
end

% Are any of the unlimited variables also coordinate variables?
coord_idx = [];
for j = idx_of_unlimiteds
    if nc_iscoordvar(hdf4file,fileinfo.Dataset(j).Name)
        coord_idx = j;
    end
end


unlim_varname = fileinfo.Dataset(coord_idx).Name;
idx = setdiff(idx_of_unlimiteds,coord_idx);
idx = idx(1); % just need the first one.

dims = fileinfo.Dataset(idx).Dimension;
for j =1:numel(dims)
    if strcmp(dims{j},unlim_varname)
        extent = fileinfo.Dataset(idx).Size(j);
    end
end

% Update the metadata.
fileinfo.Dataset(coord_idx).Size = extent;

% Update the length of the unlimited dimension.
idx_of_unlimited_dim = find(cell2mat({fileinfo.Dimension.Unlimited}));
fileinfo.Dimension(idx_of_unlimited_dim).Length = extent; %#ok<FNDSB>
