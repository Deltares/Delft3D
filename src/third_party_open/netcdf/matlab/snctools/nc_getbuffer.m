function theBuffer = nc_getbuffer ( ncfile, varargin )
%NC_GETBUFFER  Read unlimited variables into structure.
%
%   theBuffer = NC_GETBUFFER(NCFILE) reads all unlimited variables into a
%   structure.
%
%   theBuffer = NC_GETBUFFER(NCFILE,VARLIST) reads variables listed in the
%   cell array VARLIST into a structure.
%
%   theBuffer = NC_GETBUFFER(NCFILE,START,COUNT) reads all unlimited
%   variables into a structure.  I/O starts at zero-indexed record number
%   START and encompasses COUNT records.  IF COUNT is negative, then all
%   records starting at COUNT to the end of the file are retrieved.
%
%   theBuffer = NC_GETBUFFER(NCFILE,VARLIST,START,COUNT) reads variables
%   listed in the cell array VARLIST into a structure.  I/O starts at
%   zero-indexed record number START and encompasses COUNT records.
%
%   See also NC_VARGET.

preserve_fvd = nc_getpref('PRESERVE_FVD');

% assume failure until success is known
theBuffer = [];

error(nargchk(1,4,nargin,'struct'));
error(nargoutchk(1,1,nargout,'struct'));


%
% check that the first argument is a char
if ~ischar ( ncfile )
       error (  'snctools:getbuffer:badInput', 'filename argument must be character.' );
end


[varlist,start,count] = parse_inputs ( varargin{:} );

metadata = nc_info ( ncfile );

num_datasets = length(metadata.Dataset);

skip_this_variable = construct_skip_list(varlist,metadata);

%
% Find the unlimited dimension and it's length
record_length = -1;
num_dims = length(metadata.Dimension);
for j = 1:num_dims
    if metadata.Dimension(j).Unlimited
        record_length = metadata.Dimension(j).Length;
    end
end
if record_length < 0
       error (  'snctools:getbuffer:noUnlimitedDimension', ...
             'An unlimited dimension is required.');
end

%
% figure out what the start and count really are.
if ~isempty(start) && ~isempty(count)
    if start < 0
        start = record_length - count;
    end
    if count < 0
        count = record_length - start;
    end
    if (start < 0) && (count < 0)
           error (  'snctools:getbuffer:badIndexing', ...
                 'both start and count cannot be less than zero.');
    end
end




for j = 1:num_datasets
    
    %
    % Did we restrict retrieval to a few variables?
    if ~isempty(varlist) && skip_this_variable(j)
        continue
    end

    %
    % If it is not an unlimited variable, we don't want it.
    if ~metadata.Dataset(j).Unlimited
        continue
    end

    if ~isempty(start) && ~isempty(count) 

        varstart = zeros(size(metadata.Dataset(j).Size));
        varcount = metadata.Dataset(j).Size;

        if preserve_fvd
            varstart(end) = start;
            varcount(end) = count;
        else
            varstart(1) = start;
            varcount(1) = count;
        end

        vardata = nc_varget ( ncfile, metadata.Dataset(j).Name, varstart, varcount );

    else
        vardata = nc_varget ( ncfile, metadata.Dataset(j).Name );
    end


    theBuffer.(metadata.Dataset(j).Name) = vardata;

end


return




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [varlist, start, count] = parse_inputs ( varargin )

varlist = {};
start = [];
count = [];

%
% figure out what the inputs actually were
switch nargin
case 1
    if iscell(varargin{1})
        varlist = varargin{1};
    else
        error ( 'snctools:getbuffer:badInput', '2nd of two input arguments must be a cell array.' );
    end
case 2
    if isnumeric(varargin{1}) && isnumeric(varargin{2})
        start = varargin{1};
        count = varargin{2};
    else
        error ( 'snctools:getbuffer:badInput', '2nd and 3rd of three input arguments must be numeric.' );
    end
case 3
    if iscell(varargin{1})
        varlist = varargin{1};
    else
        error ( 'snctools:getbuffer:badInput', '2nd of four input arguments must be a cell array.' );
    end
    if isnumeric(varargin{2}) && isnumeric(varargin{3})
        start = varargin{2};
        count = varargin{3};
    else
        error ( 'snctools:getbuffer:badInput', '3rd and 4th of four input arguments must be numeric.' );
    end
end






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function skip_it = construct_skip_list(varlist,metadata)

num_datasets = length(metadata.Dataset);
if ~isempty(varlist) 

    skip_it = ones(num_datasets,1);

    %
    % Go thru and quickly set up a flag for each Dataset
    for j = 1:num_datasets
        for k = 1:length(varlist)
            if strcmp(varlist{k}, metadata.Dataset(j).Name)
                skip_it(j) = 0;
            end
        end
    end

else
    skip_it = zeros(num_datasets,1);
end


retrievable_datasets = find(1 - skip_it);
if ~any(retrievable_datasets)
    error ( 'No datasets found.\n' );
end


