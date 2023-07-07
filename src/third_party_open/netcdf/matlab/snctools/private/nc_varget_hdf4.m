function [data,info] = nc_varget_hdf4(hfile,varname,varargin)
% HDF4 backend for NC_VARGET

info = nc_getvarinfo(hfile,varname);

preserve_fvd = nc_getpref('PRESERVE_FVD');


fid = fopen(hfile,'r');
fullfile = fopen(fid);
fclose(fid);

v = nc_getvarinfo(fullfile,varname);
switch(numel(varargin))
    
    case 0
        % retrieve everything.
        start = zeros(1,numel(v.Size));
        edge = v.Size;
        stride = ones(1,numel(v.Size));
        
    case 1
        % if only start was provided, then the count is implied to be one.
        start = varargin{1};
        edge = ones(1,numel(v.Size));
        stride = ones(1,numel(v.Size));
        
    case 2        
        % just a contiguous hyperslab.
        start = varargin{1};
        edge = varargin{2};
        stride = ones(1,numel(v.Size));
        
    case 3
        start = varargin{1};
        edge = varargin{2};
        stride = varargin{3};
end


sd_id = hdfsd('start',fullfile,'read');
if sd_id < 0
    error('snctools:varget:hdf4:startFailed', ...
        'START failed on %s.', hfile);
end

try
    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        error('snctools:varget:hdf4:nametoindexFailed', ...
            'NAMETOINDEX failed on %s, %s.', varname, hfile);
    end
    
    sds_id = hdfsd('select',sd_id,idx);
    if sds_id < 0
        error('snctools:varget:hdf4:selectFailed', ...
            'SELECT failed on %s, %s.', varname, hfile);
    end
    
    
    if isempty(start) && isempty(edge) && isempty(stride)
        
        % retrieve everything.
        start = zeros(1,numel(v.Size));
        edge = v.Size;
        stride = ones(1,numel(v.Size));
        
    elseif isempty(edge) && isempty(stride)
        % if only start was provided, then the count is implied to be one.
        edge = ones(1,numel(v.Size));
        stride = ones(1,numel(v.Size));
    elseif isempty(stride)
        % just a contiguous hyperslab.
        stride = ones(1,numel(v.Size));
    end
    
    negs = find((edge<0) | isinf(edge));
    if isempty(stride)
        edge(negs) =        v.Size(negs) - start(negs);
    else
        edge(negs) = floor((v.Size(negs) - start(negs))./stride(negs));
    end
    
    if preserve_fvd
        start = fliplr(start);
        edge = fliplr(edge);
        stride = fliplr(stride);
    end
    
        
    
    [data,status] = hdfsd('readdata',sds_id,start,stride,edge);
    if status < 0
        error('snctools:varget:hdf4:readdataFailed', ...
            'READDATA failed on %s, %s.', varname, hfile);
    end
    
    

catch %#ok<CTCH>
    if exist('sds_id','var')
        hdfsd('endaccess',sds_id);
    end
    hdfsd('end',sd_id);
    e = lasterror; %#ok<LERR>
    error(e.identifier,e.message);
end

status = hdfsd('endaccess',sds_id);
if status < 0
    hdfsd('end',sd_id);
    error('snctools:varget:hdf4:endaccessFailed', ...
        'ENDACCESS failed on %s, %s.', varname, hfile);
end

status = hdfsd('end',sd_id);
if status < 0
    error('snctools:varget:hdf4:endFailed', ...
        'END failed on %s, %s.', varname, hfile);
end

if ~preserve_fvd
    data = permute(data,ndims(data):-1:1);
end

% If 1D vector, force to be a column.
if numel(start) == 1
    data = data(:);
end

return
