function fileinfo = nc_info_hdf4_sd(hdf4file)
% HDF4 SD package backend for NC_INFO
import matlab.io.hdf4.*

fileinfo.Name = '/';

% Get the full path name.
fid = fopen(hdf4file,'r');
fullfile = fopen(fid);
fclose(fid);

fileinfo.Filename = hdf4file; % [name ext];
fileinfo.Datatype = [];       % Never used for HDF4.

sd_id = sd.start(fullfile,'read');

try
	[ndatasets,nglobal_attr] = sd.fileInfo(sd_id);
	
	dim_count = 0;
	dataset_count = 0;
	
	for idx = 0:ndatasets-1
	    
	    sds_id = sd.select(sd_id,idx);
	
		[sds_name,sds_dimsizes,dtype_wr,nattrs] = sd.getInfo(sds_id); %#ok<ASGLU>
		sds_rank = numel(sds_dimsizes);
	
		% Look at each dimension
		for dimidx = 0:sds_rank-1
	
	        dimid = sd.getDimID(sds_id,dimidx);
	        [dname,dcount] = sd.dimInfo(dimid);
            
            % Is this the unlimited dimension?  Fudge this.
            if (sds_rank == 1) && isinf(dcount) && isinf(sds_dimsizes) && strcmp(sds_name,dname)
                sds_dimsizes = 0;
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
	                fileinfo.Dimension(dim_count).Length = sds_dimsizes(dimidx+1);
				else
					fileinfo.Dimension(dim_count).Unlimited = false;
					fileinfo.Dimension(dim_count).Length = dcount;
				end
			end
	
		end
	    
		dataset_count = dataset_count + 1;
	    fileinfo.Dataset(dataset_count) = nc_getvarinfo_hdf4(hdf4file,sds_name);
	    
	    sd.endAccess(sds_id);
	end
	
	if ndatasets == 0
	    fileinfo.Dataset = [];
	end
	
	
	Attribute = [];
	if nglobal_attr > 0
	    
	    Attribute = repmat(struct('Name','','Datatype','','Value',0),nattrs,1);
	    for j = 0:nglobal_attr-1
	        name = sd.attrInfo(sd_id,j);
	        Attribute(j+1).Name = name;
	        
	        Attribute(j+1).Value = sd.readAttr(sd_id,j);
	        Attribute(j+1).Datatype = class(Attribute(j+1).Value);
	    end
	end
	fileinfo.Attribute = Attribute;

catch me
	if exist('sds_id','var')
		sd.endAccess(sds_id);
	end
	sd.close(sd_id);
    rethrow(me);
end


if exist('sds_id','var')
	sd.endAccess(sds_id);
end
sd.close(sd_id);


if isempty(fileinfo.Dataset) 
    return
end

return;

