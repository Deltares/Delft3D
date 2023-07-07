function nc_addrecs(ncfile,new_data)
%NC_ADDRECS  Add records onto the end of netcdf file.
%   nc_addrecs(NCFILE,RECS) appends records along the unlimited 
%   dimension.  RECS is a structure containing all the variables that
%   are defined along the unlimited dimension.
%
%   This function differs from NC_ADDNEWRECS in that it does not require
%   the unlimited coordinate variable to be monotonically increasing.
%
%   See also nc_addnewrecs, nc_cat.

ncinfo = nc_info(ncfile);

preserve_fvd = nc_getpref('PRESERVE_FVD');

% If the input structure is old-style, convert it to the name-data format.
if ~isfield(new_data,'Name') || ~isfield(new_data,'Data')
    new_data = convert_buffer(new_data);
end

% Check that we were given good inputs.
if ~isstruct ( new_data )
    err_id = 'snctools:addrecs:badStruct';
    error ( err_id, '2nd input argument must be a structure .\n' );
end

%
% Check that each field of the structure has the same length.
if isempty(new_data)
    err_id = 'snctools:addrecs:badRecord';
    error ( err_id, 'data record cannot be empty' );
end

field_length = zeros(numel(new_data),1);
for j = 1:numel(new_data)

    v = nc_getvarinfo(ncfile,new_data(j).Name);

    if ~v.Unlimited
        error('snctools:addRecs:notUnlimited', ...
            'All variables must have an unlimited dimension.');
    end
    
    if preserve_fvd

        if numel(v.Size) == 1
            % netCDF variable is 1D
            field_length(j) = numel(new_data(j).Data);
        elseif (numel(v.Size) == 2) 
            % netCDF variable is 2D
            field_length(j) = size(new_data(j).Data,2);
        elseif (numel(v.Size) > 2) && (numel(v.Size) == (ndims(new_data(j).Data)) + 1)
            % netCDF variable is more than 2D, but we're given just one record.
            field_length(j) = 1;
        else
            % netCDF variable is n-D
            n = ndims(new_data(j).Data);
            field_length(j) = size(new_data(j).Data,n);
        end

    else
        if numel(v.Size) == 1
            % netCDF variable is 1D
            field_length(j) = numel(new_data(j).Data);
        elseif (numel(v.Size) == 2) 
            % netCDF variable is 2D
            field_length(j) = size(new_data(j).Data,1);
        elseif (numel(v.Size) > 2) && (numel(v.Size) == (ndims(new_data(j).Data) + 1)) && v.Size(end) ~= 1
            % netCDF variable is more than 2D, but we're given just one record.
            field_length(j) = 1;
        else
            % netCDF variable is n-D
            field_length(j) = size(new_data(j).Data,1);
        end

    end
end
if any(diff(field_length))
    err_id = 'snctools:addrecs:badFieldLengths';
    error ( err_id, 'Some of the fields do not have the same length.\n' );
end


% So we have this many records to write.
record_count = field_length(1);

% Ok, get the unlimited dimension name and current length
unlim_idx = 0;
for j = 1:numel(ncinfo.Dimension)
    if ncinfo.Dimension(j).Unlimited
        unlim_idx = j;
    end
end



% Need to retrieve the variable sizes NOW, before the first write.
all_names = {ncinfo.Dataset.Name};
data_names = {new_data.Name};
num_vars = numel(data_names);
for j = 1:num_vars
    tf = strcmp(data_names{j},all_names);
    idx = find(tf);
    varsize{j} = ncinfo.Dataset(idx).Size;
end


% So we start writing here.
record_corner = ncinfo.Dimension(unlim_idx).Length;



% write out each data field
for j = 1:numel(new_data)

    current_var_data = new_data(j).Data;

    netcdf_var_size = varsize{j};

    corner = zeros( 1, numel(netcdf_var_size) );
    count = netcdf_var_size;

    if preserve_fvd
        % record dimension is last.
        corner(end) = record_corner;
        count(end) = record_count;
    else
        % Old school
        corner(1) = record_corner;
        count(1) = record_count;
    end

    % Ok, we are finally ready to write some data.
    nc_varput ( ncfile, new_data(j).Name, current_var_data, corner, count );

end


return












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








    


