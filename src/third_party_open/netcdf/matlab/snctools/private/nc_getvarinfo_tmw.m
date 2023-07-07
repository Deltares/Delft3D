function Dataset = nc_getvarinfo_tmw(arg1,arg2)
% TMW backend for NC_GETVARINFO

if ischar(arg1) && ischar(arg2)
    % We were given a char filename and a char varname.

    ncfile = arg1;
    varname = arg2;

    ncid=netcdf.open(ncfile,'NOWRITE');
    try
        varid = netcdf.inqVarID(ncid,varname);
        Dataset = get_varinfo(ncid,varid);
    catch me
        netcdf.close(ncid);
        rethrow(me);
    end
    
    netcdf.close(ncid);

elseif isnumeric ( arg1 ) && isnumeric ( arg2 )

    % We were given a numeric file handle and a numeric id.
	Dataset = get_varinfo(arg1,arg2);

else
    error ( 'snctools:getvarinfo:tmw:badTypes', ...
            'Must have either both character inputs, or both numeric.' );
end


return



%-------------------------------------------------------------------------
function Dataset = get_varinfo(ncid,varid)

preserve_fvd = nc_getpref('PRESERVE_FVD');

Attribute = struct('Name','','Nctype','','Datatype','','Value',NaN);
Dataset = struct('Name','','Nctype','','Datatype','','Unlimited',false,'Dimension',{''},'Size',[],'Attribute',Attribute,'Chunking',[],'Shuffle',0,'Deflate',0);

[ndims,nvars,ngatts,record_dimension] = netcdf.inq(ncid); %#ok<ASGLU>
[varname,datatype,dims,natts] = netcdf.inqVar(ncid, varid);
ndims = numel(dims);

Dataset.Name = varname;
Dataset.Nctype = datatype;
switch(datatype)
    case nc_nat
        Dataset.Datatype = '';
    case nc_byte
        Dataset.Datatype = 'int8';
    case nc_ubyte
        Dataset.Datatype = 'uint8';
    case nc_char
        Dataset.Datatype = 'char';
    case nc_short
        Dataset.Datatype = 'int16';
    case nc_ushort
        Dataset.Datatype = 'uint16';
    case nc_int
        Dataset.Datatype = 'int32';
    case nc_uint
        Dataset.Datatype = 'uint32';
    case nc_float
        Dataset.Datatype = 'single';
    case nc_double
        Dataset.Datatype = 'double';
    case nc_int64
        Dataset.Datatype = 'int64';
    case nc_uint64
        Dataset.Datatype = 'uint64';
    case 12
        Dataset.Datatype = '';
    otherwise
         Dataset.Datatype = '';

end

% Assume the current variable does not have an unlimited dimension until
% we know that it does.
Dataset.Unlimited = false;

if ndims == 0
	Dataset.Dimension = {};
	Dataset.Size = 1;
else

    for j = 1:ndims
        [dimname, dimlength] = netcdf.inqDim(ncid, dims(j));
        
        Dataset.Dimension{j} = dimname;
        Dataset.Size(j) = dimlength;
        
        if dims(j) == record_dimension
            Dataset.Unlimited = true;
        end
    end
    
end

% get all the attributes
if natts == 0
	Dataset.Attribute = [];
else
	for attnum = 0:natts-1
		Dataset.Attribute(attnum+1) = nc_getattsinfo_tmw(ncid,varid,attnum);
	end
end

v = netcdf.inqLibVers;
if v(1) == '4' 
	fmt = netcdf.inqFormat(ncid);
	if (strcmp(fmt,'FORMAT_NETCDF4') || strcmp(fmt,'FORMAT_NETCDF4_CLASSIC'))
		% Get the chunksize
		[storage,chunking] = netcdf.inqVarChunking(ncid,varid); %#ok<ASGLU>
		Dataset.Chunking = chunking;
		
		% Get the compression parameters
		[shuffle,deflate,deflate_level] = netcdf.inqVarDeflate(ncid,varid);  %#ok<ASGLU>
		Dataset.Shuffle = shuffle;
		Dataset.Deflate = deflate_level;
	end
end

if ~preserve_fvd
	Dataset.Dimension = fliplr(Dataset.Dimension);
	Dataset.Size = fliplr(Dataset.Size);
	if isfield(Dataset,'Chunking')
		Dataset.Chunking = fliplr(Dataset.Chunking);
	end
end

return










