function vinfo = nc_getvarinfo_hdf4(hfile,varname)

preserve_fvd = nc_getpref('PRESERVE_FVD');



% HDF4 backend for nc_getvarinfo.
if ~ischar(varname)
    error('snctools:nc_getvarinfo:hdf4:variableNotChar', ...
        'The variable name must be a character string.');
end

fid = fopen(hfile,'r');
filename = fopen(fid);
fclose(fid);
sd_id = hdfsd('start',filename,'read');
if sd_id < 0
    error('snctools:nc_info:hdf4:startFailed', ...
        'Unable to gain access to %s.', filename);
end

try
    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        error('snctools:nc_info:hdf4:nametoindexFailed', ...
            '''nametoindex'' failed on ''%s''.', varname);
    end
    sds_id = hdfsd('select',sd_id,idx);
    if  sds_id < 0
        error('snctools:nc_info:hdf4:selectFailed', ...
            'Unable to select %s.', varname);
    end
    
    
    [name,rank,dim_sizes,data_type,nattrs,status] = hdfsd('getinfo',sds_id);
    if status < 0
        error('snctools:nc_info:hdf4:getinfoFailed', ...
            'Unable to get information about scientific dataset.');
    end
    
    vinfo.Name = name;
    switch(data_type)
        case 'float'
            vinfo.Datatype = 'single';
        case 'char8'
            vinfo.Datatype = 'char';
        otherwise
            vinfo.Datatype = data_type;
    end
    
    
    if hdfsd('isrecord',sds_id)
        vinfo.Unlimited = true;
    else
        vinfo.Unlimited = false;
    end
    
    if (rank == 0)
        vinfo.Dimension = {};
        vinfo.Size = 1;
    elseif (rank == 1)
        
        % 1D variable sizes, particularly coordinate record variables, are
        % finitely reported by hdfsd.  Not so for variables with higher
        % rank.
        dim_id = hdfsd('getdimid',sds_id,0);
        [dname,dcount,ddatatype,dnattrs,status] = hdfsd('diminfo',dim_id);
        if status < 0
            error('snctools:nc_info:hdf4:diminfoFailed', ...
                'Unable to get information about dimension 0 for %s.', vinfo.Name);
        end
        vinfo.Dimension = {dname};
        if isinf(dim_sizes) && isinf(dcount)
            vinfo.Size = 0;
        else
            vinfo.Size = dim_sizes;
        end
    else
        for j = 0:rank-1
            dim_id = hdfsd('getdimid',sds_id,j);
            if dim_id < 0
                error('snctools:nc_info:hdf4:getdimidFailed', ...
                    'Unable to get a dimension scale identifier for dimension %d for %s.',j, vinfo.Name);
            end
            [dname,dcount,ddatatype,dnattrs,status] = hdfsd('diminfo',dim_id); %#ok<ASGLU>
            if status < 0
                error('snctools:nc_info:hdf4:diminfoFailed', ...
                    'Unable to get information about dimension scale %d for %s.',j, vinfo.Name);
            end
            vinfo.Dimension{j+1} = dname;
            
            % inf means unlimited dimension.
            if isinf(dcount)
                if isinf(dim_sizes(j+1))
                    % Try to resolve by getting the length of the
                    % "coordinate variable".
                    cvar = nc_getvarinfo(hfile,dname);
                    vinfo.Size(j+1) = cvar.Size;
                else
                    vinfo.Size(j+1) = dim_sizes(j+1);
                end
            else
                vinfo.Size(j+1) = dcount;
            end
        end
    end
    
    if preserve_fvd
        vinfo.Dimension = fliplr(vinfo.Dimension);
        vinfo.Size = fliplr(vinfo.Size);
    end
    
    
    Attribute = [];
    if nattrs > 0
    
        Attribute = repmat(struct('Name','','Datatype','','Value',0),nattrs,1);
        for j = 0:nattrs-1
            [name,atype,acount,status] = hdfsd('attrinfo',sds_id,j); %#ok<ASGLU>
            if status < 0
                error('snctools:getvarinfo:hdf4:attrinfoFailed', ...
                    'Could not read attribute %d.', j );
            end
            Attribute(j+1).Name = name;
            
            [Attribute(j+1).Value, status] = hdfsd('readattr',sds_id,j);
            if status < 0
                error('snctools:getvarinfo:hdf4:readattrFailed', ...
                    'Could not read attribute %d.',j );
            end
            Attribute(j+1).Datatype = class(Attribute(j+1).Value);
        end
    
    end
    vinfo.Attribute = Attribute;

catch
    if exist('sds_id','var')
        hdfsd('endaccess',sds_id);
    end
    hdfsd('end',sd_id);
    error(lasterror);
end

status = hdfsd('endaccess',sds_id);
if status < 0
    hdfsd('end',sd_id);
    error('snctools:nc_info:hdf4:endaccessFailed', ...
        'Unable to end access to %s.', varname);
end
status = hdfsd('end',sd_id);
if status < 0
    error('snctools:nc_info:hdf4:endaccessFailed', ...
        'Unable to end access to %s.', filename);
end
    

