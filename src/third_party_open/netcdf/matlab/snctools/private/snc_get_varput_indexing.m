function [start,count,stride] = snc_get_varput_indexing(ndims,varsize,datasize,varargin)


switch numel(varargin)
    case 0
        if ndims == 0
            start = 0;
            count = 1;
            stride = 1;
        elseif ndims == 1
            start = 0;
            count = prod(datasize);
            stride = 1;
        else
            start = zeros(1,ndims);
            count = datasize;
            stride = ones(1,ndims);
        end
    case 1
        start = varargin{1};
        count = one_d_datasize(ndims,datasize);
        stride = ones(1,ndims);
    case 2
        start = varargin{1};
        count = varargin{2};
        stride = ones(1,ndims);
    case 3
        start = varargin{1};
        count = varargin{2};
        stride = varargin{3};
end


v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a'}
        if ~isnumeric(start) || ~isnumeric(count) || ~isnumeric(stride)
            error ('snctools:indexing:badInput', ...
                'The indexing arguments must be numeric.');
        end
        
    otherwise
        validateattributes(start,{'numeric'},{'nonempty','integer','nonnegative'},'','START');
        validateattributes(count,{'numeric'},{'nonempty','integer','nonnegative'},'','COUNT');
        validateattributes(stride,{'numeric'},{'nonempty','integer','nonnegative'},'','STRIDE');
end
return

%--------------------------------------------------------------------------
function count = one_d_datasize(ndims,datasize)
if ndims == 0
    count = 1;
elseif ndims == 1
    count = prod(datasize);
else
    count = datasize;
end
