function vinfo = nc_getvarinfo_hdf4_2011a(hfile,varname)

import matlab.io.hdf4.*

preserve_fvd = nc_getpref('PRESERVE_FVD');

% HDF4 backend for nc_getvarinfo.
if ~ischar(varname)
    error('snctools:nc_getvarinfo:hdf4:variableNotChar', ...
        'The variable name must be a character string.');
end

fid = fopen(hfile,'r');
filename = fopen(fid);
fclose(fid);

sd_id = sd.start(filename,'read');

try
    idx = sd.nameToIndex(sd_id,varname);
    sds_id = sd.select(sd_id,idx);
    [name,dim_sizes,data_type,nattrs] = sd.getInfo(sds_id);
    
    vinfo.Name = name;
    switch(data_type)
    	case 'float'
    		vinfo.Datatype = 'single';
        case 'char8'
            vinfo.Datatype = 'char';
    	otherwise
    		vinfo.Datatype = data_type;
    end
    
    
    if sd.isRecord(sds_id)
        vinfo.Unlimited = true;
    else
        vinfo.Unlimited = false;
    end
    
    if isempty(dim_sizes)
        vinfo.Dimension = {};
        vinfo.Size = 1;
    elseif (numel(dim_sizes) == 1)
        
        % 1D variable sizes, particularly coordinate record variables, are
        % finitely reported by hdfsd.  Not so for variables with higher
        % rank.
        dim_id = sd.getDimID(sds_id,0);
        [dname,dcount] = sd.dimInfo(dim_id);
        vinfo.Dimension = {dname};
        if isinf(dim_sizes) && isinf(dcount)
            vinfo.Size = 0;
        else
            vinfo.Size = dim_sizes;
        end
    else
        for j = 0:numel(dim_sizes)-1
            dim_id = sd.getDimID(sds_id,j);
            [dname,dcount] = sd.dimInfo(dim_id); 
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
    
    if ~preserve_fvd
        vinfo.Dimension = fliplr(vinfo.Dimension);
        vinfo.Size = fliplr(vinfo.Size);
    end
    
    
    Attribute = [];
    if nattrs > 0
    
        Attribute = repmat(struct('Name','','Datatype','','Value',0),nattrs,1);
        for j = 0:nattrs-1
            name = sd.attrInfo(sds_id,j); 
            Attribute(j+1).Name = name;
            
            Attribute(j+1).Value = sd.readAttr(sds_id,j);
            Attribute(j+1).Datatype = class(Attribute(j+1).Value);
        end
    
    end
    vinfo.Attribute = Attribute;

catch me
	if exist('sds_id','var')
		sd.endAccess(sds_id);
	end
	sd.close(sd_id);
    rethrow(me);
end

sd.endAccess(sds_id);
sd.close(sd_id);
    

