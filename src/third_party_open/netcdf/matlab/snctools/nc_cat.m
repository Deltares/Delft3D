function nc_cat(file1,file2,recsize)
%nc_cat Concatenate netCDF files along unlimited dimension.
%   nc_cat(file1,file2) concatenates files along the unlimited dimension.
%   The records of file2 are added to file1.  Variable not defined along
%   the unlimited dimension are left untouched.
%
%   nc_cat(file1,file2,N) concatenates the files N records at a time.  This 
%   may be preferable if the files are large and all the records cannot fit 
%   into memory.
%
%   Example:
%       nc_create_empty('f1.nc');
%       nc_adddim('f1.nc','time',0);
%       v.Name = 'time';
%       v.Dimension = {'time'};
%       nc_addvar('f1.nc',v);
%       v.Name = 'money';
%       v.Dimension = {'time'};
%       nc_addvar('f1.nc',v);
%       copyfile('f1.nc','f2.nc');
%
%       % Populate the first file.
%       buf = struct('Name','Data');
%       buf(1).Name = 'time';  buf(1).Data = [0 1 2];
%       buf(2).Name = 'money';  buf(2).Data = [0 1000 2000];
%       nc_addnewrecs('f1.nc',buf);
%
%       % Now populate the 2nd file.
%       buf(1).Data = [3 4 5 6];
%       buf(2).Data = [3000 4000 5000 6000];
%       nc_addnewrecs('f2.nc',buf);
%
%       % Now concatenate them.
%       nc_cat('f1.nc','f2.nc');
%       data = nc_varget('f1.nc','money');
%
%
%   See also nc_addnewrecs.

record_variable = snc_find_record_variable(file1);


% Verify that the record variable exists in file2.
if ~nc_isvar(file2,record_variable)
    error('snctools:cat:recordVariableNotThere', ...
        'The record variables must have the same name.  Could not find "%s" in %s.', ...
        record_variable, file2);
end

% Verify that all the unlimited variables in the 2nd file exist in the first file
info2 = nc_info(file2);
for j = 1:numel(info2.Dataset)
    if info2.Dataset(j).Unlimited
        if ~nc_isvar(file1,info2.Dataset(j).Name)
            error('snctools:cat:recordVariableMissing', ...
                'Could not find unlimited variable %s in %s.', ...
                info2.Dataset(j).Name,file1);       
        end
    end
end



if nargin < 3
	b = snc_getbuffer(file2);
	nc_addrecs(file1,b);
else
	vinfo = nc_getvarinfo(file2,record_variable);
	num_ops = ceil(vinfo.Size / recsize);
	for j = 1:num_ops
		start = (j-1)*recsize;
		count = min(recsize, vinfo.Size-start);
		b = snc_getbuffer(file2,start,count);
		nc_addrecs(file1,b);
	end
end



%--------------------------------------------------------------------------
function theBuffer = snc_getbuffer ( ncfile, varargin )
%SNC_GETBUFFER  Read unlimited variables into Name-Data structure
%
%   theBuffer = SNC_GETBUFFER(NCFILE) reads all unlimited variables into a
%   structure.
%
%   theBuffer = SNC_GETBUFFER(NCFILE,VARLIST) reads variables listed in the
%   cell array VARLIST into a structure.
%
%   theBuffer = SNC_GETBUFFER(NCFILE,START,COUNT) reads all unlimited
%   variables into a structure.  I/O starts at zero-indexed record number
%   START and encompasses COUNT records.  IF COUNT is negative, then all
%   records starting at COUNT to the end of the file are retrieved.
%
%   theBuffer = SNC_GETBUFFER(NCFILE,VARLIST,START,COUNT) reads variables
%   listed in the cell array VARLIST into a structure.  I/O starts at
%   zero-indexed record number START and encompasses COUNT records.
%
%   See also NC_VARGET.


preserve_fvd = nc_getpref('PRESERVE_FVD');

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
       error('snctools:cat:noUnlimitedDimension', ...
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
           error ('snctools:cat:badIndexing', ...
                 'both start and count cannot be less than zero.');
    end
end



keepers = [];
theBuffer = repmat(struct('Name','','Data',[]),num_datasets,1);
for j = 1:num_datasets
    
    % Did we restrict retrieval to a few variables?
    if ~isempty(varlist) && skip_this_variable(j)
        continue
    end

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

    keepers(end+1) = j;
    theBuffer(j).Name = metadata.Dataset(j).Name;
    theBuffer(j).Data = vardata;

end

theBuffer = theBuffer(keepers);
return




%--------------------------------------------------------------------------
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
        error ( 'snctools:cat:badInput', '2nd of two input arguments must be a cell array.' );
    end
case 2
    if isnumeric(varargin{1}) && isnumeric(varargin{2})
        start = varargin{1};
        count = varargin{2};
    else
        error ( 'snctools:cat:badInput', '2nd and 3rd of three input arguments must be numeric.' );
    end
case 3
    if iscell(varargin{1})
        varlist = varargin{1};
    else
        error ( 'snctools:cat:badInput', '2nd of four input arguments must be a cell array.' );
    end
    if isnumeric(varargin{2}) && isnumeric(varargin{3})
        start = varargin{2};
        count = varargin{3};
    else
        error ( 'snctools:cat:badInput', '3rd and 4th of four input arguments must be numeric.' );
    end
end






%--------------------------------------------------------------------------
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


