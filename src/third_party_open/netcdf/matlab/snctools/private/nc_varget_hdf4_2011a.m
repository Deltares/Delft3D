function [data,info] = nc_varget_hdf4_2011a(hfile,varname,varargin)
% HDF4 package backend for NC_VARGET

import matlab.io.hdf4.*

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

% Force to be rows.
start = start(:)';
edge = edge(:)';
stride = stride(:)';


negs = find((edge<0) | isinf(edge));
if isempty(stride)
    edge(negs) =        v.Size(negs) - start(negs);
else
    edge(negs) = floor((v.Size(negs) - start(negs))./stride(negs));
end

if ~preserve_fvd
    start = fliplr(start);
    edge = fliplr(edge);
    stride = fliplr(stride);
end
    
sd_id = sd.start(fullfile,'read');

try
    idx = sd.nameToIndex(sd_id,varname);
    sds_id = sd.select(sd_id,idx);
    data = sd.readData(sds_id,start,edge,stride);
    
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

if ~preserve_fvd
    data = permute(data,ndims(data):-1:1);
end

% If 1D vector, force to be a column.
if numel(start) == 1
    data = data(:);
end

return
