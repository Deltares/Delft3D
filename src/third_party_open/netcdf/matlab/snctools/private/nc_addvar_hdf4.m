function nc_addvar_hdf4(hfile,varstruct,preserve_fvd)

sd_id = hdfsd('start',hfile,'write');
if sd_id < 0
    error('snctools:addVar:hdf4:startFailed', ...
        'START failed on %s.', hfile);
end

% Is the variable already present?
idx = hdfsd('nametoindex',sd_id,varstruct.Name);
if idx >= 0
    hdfsd('end',sd_id);
    error('snctools:nc_addvar:hdf4:variableAlreadyPresent', ...
        '%s is already present.', varstruct.Name);
end
    
% determine the lengths of the named dimensions
num_dims = length(varstruct.Dimension);
dim_sizes = zeros(1,num_dims);
dimids = zeros(1,num_dims);
dim_names = varstruct.Dimension;

% have to reverse the order of the dimensions if we want to preserve the
% fastest varying dimension and keep it consistent.
if preserve_fvd
    dim_names = fliplr(dim_names);
end

for j = 1:num_dims
    
    idx = hdfsd('nametoindex',sd_id,dim_names{j});
    if idx < 0
        hdfsd('end',sd_id);
        error('snctools:addVar:hdf4:nametoindexFailed', ...
            'NAMETOINDEX failed on %s, \"%s\".', dim_names{j}, hfile);
    end

    dim_sds_id = hdfsd('select',sd_id,idx);
    if dim_sds_id < 0
        hdfsd('end',sd_id);
        error('snctools:addVar:hdf4:selectFailed', ...
            'SELECT failed on %s, \"%s\".', dim_names{j}, hfile);
    end

    dimids(j) = j-1;
    [name,rank,dim_sizes(j),dtype,nattrs,status] = hdfsd('getinfo',dim_sds_id); %#ok<ASGLU>
    if status < 0
        hdfsd('endaccess',dim_sds_id);
        hdfsd('end',sd_id);
        error('snctools:addVar:hdf4:getinfoFailed', ...
            'GETINFO failed on %s, \"%s\".', dim_names{j}, hfile);
    end

    status = hdfsd('endaccess',dim_sds_id);
    if status < 0
        error('snctools:addVar:hdf4:endaccessFailed', ...
            'ENDACCESS failed on %s, \"%s\".', dim_names{j}, hfile);
    end

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
sds_id = hdfsd('create',sd_id,varstruct.Name,dtype,num_dims,dim_sizes);
if sds_id < 0
    hdfsd('end',sd_id);
    error('snctools:addVar:hdf4:createFailed', ...
        'CREATE failed on %s, \"%s\".', varstruct.Name, hfile);
end

% Attach the named dimensions to the dataset.
for j = 1:num_dims
    dimid = hdfsd('getdimid',sds_id,dimids(j));
    if dimid < 0
        hdfsd('endaccess',sds_id);
        hdfsd('end',sd_id);
        error('snctools:addVar:getdimidFailed', ...
            'GETDIMID failed.');
            
    end
    
    status = hdfsd('setdimname',dimid,dim_names{j});
    if status < 0
        hdfsd('endaccess',sds_id);
        hdfsd('end',sd_id);
        error('snctools:addVar:hdf4:setdimFailed', ...
            'SETDIM failed on %s, \"%s\".', varstruct.Name, hfile);
    end
end

status = hdfsd('endaccess',sds_id);
if status < 0
    error('snctools:addVar:hdf4:endaccessFailed', ...
        'ENDACCESS failed on %s, \"%s\".', varstruct.Name, hfile);
end

status = hdfsd('end',sd_id);
if status < 0
    error('snctools:addVar:hdf4:endFailed', ...
        'END failed on %s, \"%s\".', hfile);
end


% Now just use nc_attput to put in the attributes
for j = 1:length(varstruct.Attribute)
    attname = varstruct.Attribute(j).Name;
    attval = varstruct.Attribute(j).Value;
    nc_attput(hfile,varstruct.Name,attname,attval);
end


