function Dataset = nc_getvarinfo_mexnc(arg1,arg2)
% MEXNC backend for NC_GETVARINFO

if ischar(arg1) && ischar(arg2)

    % Open the file, access the variable, then get the information.
    ncfile = arg1;
    varname = arg2;

    [ncid,status ]=mexnc('open',ncfile,nc_nowrite_mode);
    if status ~= 0
        ncerr = mexnc('strerror', status);
        error('snctools:getVarInfo:mexnc:open', ncerr);
    end

    [varid, status] = mexnc('INQ_VARID', ncid, varname);
    if ( status ~= 0 )
        ncerr = mexnc('strerror', status);
        mexnc('close',ncid);
        error('snctools:getVarInfo:mexnc:inqVarID',ncerr);
    end
   
    Dataset = get_varinfo(ncid,varid);

    % close whether or not we were successful.
    mexnc('close',ncid);

elseif isnumeric(arg1) && isnumeric(arg2)

    % The file is already open.
    ncid = arg1;
    varid = arg2;

    Dataset = get_varinfo(ncid,varid);

else
    error ( 'snctools:nc_getvarinfo:mexnc:badTypes', ...
            'Must have either both character inputs, or both numeric.' );
end

return


%-------------------------------------------------------------------------
function Dataset = get_varinfo ( ncid, varid )


preserve_fvd = nc_getpref('PRESERVE_FVD');

[record_dimension, status] = mexnc ( 'INQ_UNLIMDIM', ncid );
if status ~= 0
   	ncerr = mexnc('strerror', status);
    mexnc('close',ncid);
    error ( 'snctools:NC_VARGET:MEXNC:INQ_UNLIMDIM', ncerr );
end



[varname,datatype,ndims,dims,natts,status] = mexnc('INQ_VAR',ncid,varid);
if status ~= 0 
   	ncerr = mexnc('strerror', status);
    mexnc('close',ncid);
    error ( 'snctools:NC_VARGET:MEXNC:INQ_VAR', ncerr );
end



Dataset.Name = varname;
Dataset.Nctype = datatype;
switch(datatype)
    case nc_nat
        Dataset.Datatype = '';
    case nc_byte
        Dataset.Datatype = 'int8';
    case nc_char
        Dataset.Datatype = 'char';
    case nc_short
        Dataset.Datatype = 'int16';
    case nc_int
        Dataset.Datatype = 'int32';
    case nc_float
        Dataset.Datatype = 'single';
    case nc_double
        Dataset.Datatype = 'double';
end


% Assume the current variable does not have an unlimited dimension until
% we know that it does.
Dataset.Unlimited = false;

if ndims == 0
	Dataset.Dimension = {};
	Dataset.Size = 1;
else

	for j = 1:ndims
	
		[dimname, dimlength, status] = mexnc('INQ_DIM', ncid, dims(j));
		if ( status ~= 0 )
   			ncerr = mexnc('strerror', status);
		    mexnc('close',ncid);
		    error ( 'snctools:NC_VARGET:MEXNC:INQ_DIM', ncerr );
		end
	
		Dataset.Dimension{j} = dimname; 
		Dataset.Size(j) = dimlength;
	
		if dims(j) == record_dimension
			Dataset.Unlimited = true;
		end
	end
end

%
% get all the attributes
if natts == 0
	Dataset.Attribute = [];
else
	for attnum = 0:natts-1
		Dataset.Attribute(attnum+1) = nc_getattsinfo_mexnc(ncid,varid,attnum);
	end
end

v = mexnc('INQ_LIBVERS');
if v(1) == '4'
	% Get the chunksize
	[storage,chunking,status] = mexnc('INQ_VAR_CHUNKING',ncid,varid); %#ok<ASGLU>
	if ( status ~= 0 )
	   	ncerr = mexnc('strerror', status);
		mexnc('close',ncid);
		error ( 'snctools:NC_VARGET:MEXNC:INQ_VAR_DEFLATE', ncerr );
	end
	Dataset.Chunking = chunking;
	
	% Get the compression parameters
	[shuffle,deflate,deflate_level,status] = mexnc('INQ_VAR_DEFLATE',ncid,varid); %#ok<ASGLU>
	if ( status ~= 0 )
	   	ncerr = mexnc('strerror', status);
		mexnc('close',ncid);
		error ( 'snctools:NC_VARGET:MEXNC:INQ_VAR_DEFLATE', ncerr );
	end
	Dataset.Shuffle = shuffle;
	Dataset.Deflate = deflate_level;
end



if preserve_fvd
	Dataset.Dimension = fliplr(Dataset.Dimension);
	Dataset.Size = fliplr(Dataset.Size);
	if isfield(Dataset,'Chunking')
		Dataset.Chunking = fliplr(Dataset.Chunking);
	end
end





return







