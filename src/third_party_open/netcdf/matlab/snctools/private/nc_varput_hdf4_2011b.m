function nc_varput_hdf4_2011b(hfile,varname,data,varargin)
% HDF4 handler for NC_VARPUT.

import matlab.io.hdf4.*

preserve_fvd = nc_getpref('PRESERVE_FVD');
use_std_hdf4_scaling = getpref('SNCTOOLS','USE_STD_HDF4_SCALING',false);

sd_id = sd.start(hfile,'write');

try
    idx = sd.nameToIndex(sd_id,varname);
    
    sds_id = sd.select(sd_id, idx);
    
    [~,sds_dimsizes,dtype_wr] = sd.getInfo(sds_id); 
    sds_rank = numel(sds_dimsizes);
    [start,~,stride] = snc_get_varput_indexing(sds_rank,sds_dimsizes,size(data),varargin{:});

    
    
    % calibrate the data so as not to lose precision
    try
        [cal,cal_err,offset] = sd.getCal(sds_id); %#ok<ASGLU>
    catch me
        cal = 1.0;
        offset = 0.0;
    end
    if use_std_hdf4_scaling
        data = double(data)/cal + offset; 
    else
        % Use standard CF convention scaling.
        data = (double(data) - offset)/cal;
    end
    
    % Locate any NaNs.  If any exist, set them to the FillValue
    nan_inds = find ( isnan(data) );
    if ( ~isempty(nan_inds) )
        try
            fill_value = sd.getFillValue(sds_id);
        catch me
            
            % Is missing value defined?
            try 
                attr_idx = sd.findAttr(sds_id,'missing_value');
            catch me
                % No it is not.  Something is wrong.  Abort!  Abort!
        	    error('snctools:varput:hdf4:getfillvalueFailed', ...  
    		    	'The data has NaN values, but neither _FillValue nor missing_value is defined.');
            end

            % Yes it is defined.  Go ahead and retrieve it.
            fill_value = sd.readAttr(sds_id,attr_idx);
    
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
    
    
    % If we have an empty START argument, then we need to construct STRIDE.
    if isempty(start)
        start = zeros(1,sds_rank);
        stride = ones(1,sds_rank);
    end
    
    if isempty(stride)
        stride = ones(1,sds_rank);
    end
    
    % Do we transpose the data?
    if ~preserve_fvd
        start = fliplr(start);
        stride = fliplr(stride);
        % Need to permute it first.
        data_wr = permute ( data_wr, fliplr( 1:length(size(data_wr)) ) );
    end
    
    % attempt to write the data set.  
   	sd.writeData(sds_id,start,stride,data_wr);

catch me
	if exist('sds_id','var')
        sd.endAccess(sds_id);
	end
	sd.close(sd_id);
    rethrow(me);
end

sd.endAccess(sds_id);
sd.close(sd_id);
return





