function nc_varput_hdf4(hfile,varname,data,varargin)
% HDF4 handler for NC_VARPUT.

preserve_fvd = nc_getpref('PRESERVE_FVD');
use_std_hdf4_scaling = getpref('SNCTOOLS','USE_STD_HDF4_SCALING',false);

sd_id = hdfsd('start',hfile,'write');
if sd_id < 0
    error('snctools:varput:hdf4:startFailed', ...
        'START failed on %s.', hfile);
end

try
    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        error('snctools:varput:hdf4:nametoindexFailed', ...
            'Unable to index %s.', varname);
    end
    
    sds_id = hdfsd('select',sd_id, idx);
    if sds_id < 0
        error('snctools:varput:hdf4:selectFailed', ...
            'SELECT failed on %s.', varname);
    end
    
    [sds_name,sds_rank,sds_dimsizes,dtype_wr,nattrs,status] ...
        = hdfsd('getinfo',sds_id); %#ok<ASGLU>
    if status < 0
        error('snctools:varput:hdf4:getinfoFailed', ...
            'GETINFO failed on %s.', varname);
    end
    
    [start,edges,stride] = snc_get_varput_indexing(sds_rank,sds_dimsizes,size(data),varargin{:});
    
    % calibrate the data so as not to lose precision
    [cal,cal_err,offset,offset_err,data_type,status] = hdfsd('getcal',sds_id); %#ok<ASGLU>
    if ( status ~= -1 )
        if use_std_hdf4_scaling
            data = double(data)/cal + offset; 
        else
            % Use standard CF convention scaling.
            data = (double(data) - offset)/cal;
        end
    end	
    
    % Locate any NaNs.  If any exist, set them to the FillValue
    nan_inds = find ( isnan(data) );
    if ( ~isempty(nan_inds) )
        [fill_value,status] = hdfsd('getfillvalue',sds_id);
        if status < 0
            
            % Is missing value defined?
            attr_idx = hdfsd('findattr',sds_id,'missing_value');
            if ( attr_idx < 0 )
                % No it is not.  Something is wrong.  Abort!  Abort!
        	    error('snctools:varput:hdf4:getfillvalueFailed', ...  
    		    	'The data has NaN values, but neither _FillValue nor missing_value is defined.');
            else
                % Yes it is defined.  Go ahead and retrieve it.
                [fill_value,status] = hdfsd('readattr',sds_id,attr_idx);
                if (status < 0)
                    error('snctools:varput:hdf4:readattrFailed', ...
                        'READATTR failed on missing_value.');
                end
            end
    
        end
        data(nan_inds) = double(fill_value) * ones(size(nan_inds));
    end
    
    
    % convert to the proper data type
    switch ( dtype_wr )
        case 'uint8',
            data_wr = uint8(data);
    
        case 'int8',
            data_wr = int8(data);
    
        case 'uint16',
            data_wr = uint16(data);
    
        case 'int16',
            data_wr = int16(data);
    
        case 'uint32',
            data_wr = uint32(data);
    
        case 'int32',
            data_wr = int32(data);
    
        case 'float',
            data_wr = single(data);
    
        case 'float32',
            data_wr = single(data);
    
        case 'single',
            data_wr = single(data);
    
        case 'float64',
            data_wr = double(data);
    
        case 'double',
            data_wr = double(data);
    
        otherwise,
        	error('snctools:varput:hdf4:unhandledDatatype',...
                'Unhandled datatype.');
    
    end
    
    
    % If we have an empty START argument, then we need to construct both
    % EDGES and STRIDE.
    if isempty(start)
        start = zeros(1,sds_rank);
        edges = ones(1,sds_rank);
        for j = 1:sds_rank
            edges(j) = size(data_wr,j);
        end
        stride = ones(1,sds_rank);
    end
    
    % Do we transpose the data?
    if preserve_fvd
        start = fliplr(start);
        edges = fliplr(edges);
        stride = fliplr(stride);
    else
        % Need to permute it first.
        data_wr = permute ( data_wr, fliplr( 1:length(size(data_wr)) ) );
    end
    
    % attempt to write the data set.  
   	status = hdfsd('writedata',sds_id,start,stride,edges,data_wr);
   	if status < 0
   	    error('snctools:varput:hdf4:writedataFailed', ...
   	        'WRITEDATA failed on %s.', varname);
   	end

catch
	if exist('sds_id','var')
		hdfsd('endaccess',sds_id);
	end
	hdfsd('end',sd_id);
	e = lasterror;
	error(e.identifier,e.message);
end

status = hdfsd('endaccess',sds_id);
if status < 0
    hdfsd('end',sd_id);
    error('snctools:varput:hdf4:endaccessFailed', ...
    'ENDACCESS failed on %s.', hfile);
end
status = hdfsd('end',sd_id);
if status < 0
    error('snctools:attput:hdf4:endFailed', ...
        'END failed on %s, "%s".', hfile, hdf4_error_msg);
end
return





