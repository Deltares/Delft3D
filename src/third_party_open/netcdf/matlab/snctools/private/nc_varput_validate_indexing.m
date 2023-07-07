function [start, count] = nc_varput_validate_indexing(nvdims,data,start,count,stride)
% Check that any given start, count, and stride arguments actually make sense
% for this variable.  

% Singletons are a special case.  We need to set the start and count 
% carefully.
if nvdims == 0

    if (isempty(start) && isempty(count) && isempty(stride))

        % This is the case of "nc_varput(file,var,single_datum);"
        start = 0;
        count = 1;
        
    elseif ((start ==0) && (count == 1))
        
        return

    else     
        error('snctools:nc_varput:badIndexing',...
            'Do not use indexing for singleton variables.');
    end

    return;

end

% If START and COUNT not given, and if not a singleton variable, then START 
% is [0,..] and COUNT is the size of the data.  
if isempty(start) && isempty(count) && ( nvdims > 0 )
    start = zeros(1,nvdims);
    count = zeros(1,nvdims);
    for j = 1:nvdims
        count(j) = size(data,j);
    end
end

% Check that the start, count, and stride arguments have the same length.
if (numel(start) ~= numel(count)) || (~isempty(stride) && numel(start) ~= numel(stride))
    error('snctools:nc_varput:badIndexLength', ...
          'The START, COUNT, and STRIDE arguments must have the same length.');
end
