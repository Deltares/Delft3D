function nc_addvar_hdf4_2011b(hfile,varstruct,preserve_fvd)

import matlab.io.hdf4.*
sd_id = sd.start(hfile,'write');

% Is the variable already present?
try 
    idx = hdfsd('nametoindex',sd_id,varstruct.Name);
catch
    idx < 0;
end
if idx >= 0
    sd.close(sd_id);
    error('snctools:nc_addvar:hdf4:variableAlreadyPresent', ...
        '%s is already present.', varstruct.Name);
end
    
try
    % determine the lengths of the named dimensions
    num_dims = length(varstruct.Dimension);
    dim_sizes = zeros(1,num_dims);
    dimids = zeros(1,num_dims);
    dim_names = varstruct.Dimension;
    
    % have to reverse the order of the dimensions if we want to preserve the
    % fastest varying dimension and keep it consistent.
    if ~preserve_fvd
        dim_names = fliplr(dim_names);
    end
    
    for j = 1:num_dims
        
        idx = sd.nameToIndex(sd_id,dim_names{j});
    
        dim_sds_id = sd.select(sd_id,idx);
    
        dimids(j) = j-1;
        [name,dim_sizes(j),dtype,nattrs] = sd.getInfo(dim_sds_id); %#ok<ASGLU>
    
        sd.endAccess(dim_sds_id);
    
    end
    
    switch(varstruct.Datatype)
        case 'byte'
            dtype = 'int8';
        case 'short'
            dtype = 'int16';
        case 'int'
            dtype = 'int32';
        otherwise
            dtype = varstruct.Datatype;
    end
    sd.create(sd_id,varstruct.Name,dtype,dim_sizes);
    
    % Attach the named dimensions to the dataset.
    for j = 1:num_dims
        dimid = sd.getDimID(sds_id,dimids(j));
        sd.setDimName(dimid,dim_names{j});
    end
    
    sd.endAccess(sds_id);

catch me
    sd.close(sd_id);
    rethrow(me);
end

sd.close(sd_id);


% Now just use nc_attput to put in the attributes
for j = 1:length(varstruct.Attribute)
    attname = varstruct.Attribute(j).Name;
    attval = varstruct.Attribute(j).Value;
    nc_attput(hfile,varstruct.Name,attname,attval);
end


