function nc_addnewrecs(ncfile,input_buffer,record_variable) %#ok<INUSD>
%NC_ADDNEWRECS:  Append records to netcdf file.
%   new_data = nc_addnewrecs(ncfile,record_struct) appends records in
%   record_struct to the end of a netcdf file.  The data for the record
%   variable itself must be monotonically increasing.   Any records
%   that are "older" than the last record are ignored.
%   
%   The difference between this m-file and nc_add_recs is that this 
%   routine assumes that the unlimited dimension has a monotonically
%   increasing coordinate variable, e.g. time series. 
%  
%   Example:
%       nc_create_empty('myfile.nc');
%       nc_adddim('myfile.nc','time',0);
%       v(1).Name = 'time';
%       v(1).Dimension = {'time'};
%       nc_addvar('myfile.nc',v);
%       v.Name = 'money';
%       v.Dimension = {'time'};
%       nc_addvar('myfile.nc',v);
%
%       % Add some records.
%       buf.time = [0 1 2];
%       buf.money = [0 1000 2000];
%       nc_addnewrecs('myfile.nc',buf);
%
%       % Add another record.
%       buf.time = 3;
%       buf.money = 3000;
%       nc_addnewrecs('myfile.nc',buf);
%       nc_dump('myfile.nc');   
%  
%   See also nc_varput, nc_cat.

preserve_fvd = nc_getpref('PRESERVE_FVD');

% If the input structure is old-style, convert it to the name-value format.
if ~isfield(input_buffer,'Name') || ~isfield(input_buffer,'Data')
    input_buffer = convert_buffer(input_buffer);
end

record_variable = snc_find_record_variable(ncfile);

% Check that the record variable is present in the input buffer.
if ~any(strcmp(record_variable,{input_buffer.Name}));
    error ( 'snctools:addnewrecs:missingRecordVariable', ...
            'input structure is missing the record variable ''%s''.', ...
            record_variable );
end


% Remove any fields that aren't actually in the file.
[input_buffer, vsize] = restrict_to_those_in_file(input_buffer,ncfile);


%
% If the length of the record variable data to be added is just one,
% then we may have a special corner case.  The leading dimension might
% have been squeezed out of the other variables.  MEXNC wants the rank
% of the incoming data to match that of the infile variable.  We address 
% this by forcing the leading dimension in these cases to be 1.
input_buffer = force_rank_match(ncfile,input_buffer,record_variable);

% Retrieve the dimension id of the unlimited dimension upon which
% all depends.  
varinfo = nc_getvarinfo(ncfile,record_variable);

% Get the last time value.   If the record variable is empty, then
% only take datums that are more recent than the latest old datum
rec_idx = find(strcmp(record_variable,{input_buffer.Name}));
input_buffer_time_values = input_buffer(rec_idx).Data;
if varinfo.Size > 0
    last_time = nc_getlast ( ncfile, record_variable, 1 );
    recent_inds = find( input_buffer_time_values > last_time );
else
    recent_inds = 1:length(input_buffer_time_values);
end

% if no data is new enough, just return.  There's nothing to do.
if isempty(recent_inds)
    return
end

% Go thru each variable.  Restrict to what's new.
for j = 1:numel(input_buffer)
    data = input_buffer(j).Data;

    if preserve_fvd
        %&& (ndims(data) > 1) && (size(data,ndims(data)) > 1)
        if numel(vsize{j}) == 1
            % netCDF variable is 1D
            restricted_data = data(recent_inds);
        elseif (numel(vsize{j}) == 2) 
            % netCDF variable is 2D
            restricted_data = data(:,recent_inds);
        elseif (ndims(data) < numel(vsize{j})) && (numel(recent_inds) == 1)
            % netCDF variable is more than 2D, but we are given just one record.
            restricted_data = data;
        else
            cmdstring = repmat(':,',1,ndims(data)-1);
            cmdstring = sprintf ( 'restricted_data = data(%srecent_inds);', cmdstring );
            eval(cmdstring);
        end
    else
        if numel(vsize{j}) == 1
            % netCDF variable is 1D
            restricted_data = data(recent_inds);
        elseif (numel(vsize{j}) == 2) 
            % netCDF variable is 2D
            restricted_data = data(recent_inds,:);
        elseif (ndims(data) < numel(vsize{j})) && (numel(recent_inds) == 1)
            % netCDF variable is more than 2D, but we are given just one record.
            restricted_data = data;
        else
            cmdstring = repmat(',:',1,ndims(data)-1);
            cmdstring = sprintf ( 'restricted_data = data(recent_inds%s);', cmdstring );
            eval(cmdstring);
        end
    end


    input_buffer(j).Data = restricted_data;
end

% Write the records out to file.
nc_addrecs(ncfile,input_buffer);

return;




%--------------------------------------------------------------------------
function newbuffer = convert_buffer(input_buffer)
% Convert the input structure from "field_name": field_value to
%
%     struct(1).Name
%     struct(1).Value
%     struct(2).Name
%     struct(2).Value  etc.

fields = fieldnames(input_buffer);
newbuffer = struct('Name','','Data',[]);
newbuffer = repmat(newbuffer,numel(fields),1);
for j = 1:numel(fields)
    newbuffer(j).Name = fields{j};
    newbuffer(j).Data = input_buffer.(fields{j});
end


%--------------------------------------------------------------------------
function input_buffer = force_rank_match(ncfile,input_buffer,record_variable)
% If the length of the record variable data to be added is just one,
% then we may have a special corner case.  The leading dimension might
% have been squeezed out of the other variables.  MEXNC wants the rank
% of the incoming data to match that of the infile variable.  We address 
% this by forcing the leading dimension in these cases to be 1.

preserve_fvd = nc_getpref('PRESERVE_FVD');

rec_idx = strcmp(record_variable,{input_buffer.Name});

if numel(input_buffer(rec_idx).Data) == 1 
    for j = 1:numel(input_buffer)

        % Skip the record variable, it's irrelevant at this stage.
        if strcmp ( input_buffer(j).Name, record_variable )
            continue;
        end

        infile_vsize = nc_varsize(ncfile, input_buffer(j).Name );

        % Disregard any trailing singleton dimensions.
        %effective_nc_rank = calculate_effective_nc_rank(infile_vsize);

        if (numel(infile_vsize) > 2) && (ndims(input_buffer(j).Data) ~= numel(infile_vsize))
            
            % Ok we have a mismatch.
            if preserve_fvd
                rsz = [infile_vsize(1:end-1) numel(input_buffer(rec_idx))]; 
            else
                %rsz = [numel(input_buffer(rec_idx)) infile_vsize(2:end) ];
                rsz = [numel(input_buffer(rec_idx)) infile_vsize(2:end) ]; 
            end
            input_buffer(j).Data = reshape( input_buffer(j).Data, rsz );
        end


    end
end



%------------------------------------------------------------------------
function [input_buffer, vsize] = restrict_to_those_in_file(input_buffer,ncfile)

% check to see that all fields are actually there.
nc = nc_info ( ncfile );
num_nc_vars = length(nc.Dataset);

vsize = [];
idx = [];
count = 0;
for j = 1:numel(input_buffer)
    for k = 1:num_nc_vars
        if strcmp(input_buffer(j).Name, nc.Dataset(k).Name)
            count = count+1;
            idx(count) = j;
            vsize{count} = nc.Dataset(k).Size;
        end
    end
end

input_buffer = input_buffer(idx);


