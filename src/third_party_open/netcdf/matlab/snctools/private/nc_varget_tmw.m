function [data,info] = nc_varget_tmw(ncfile,varname,varargin)
% TMW backend for NC_VARGET.

try
    [data,info] = varget_nc(ncfile,varname,varargin{:});
catch me
    switch(me.identifier)
        case 'snctools:switchBackendsToHDF5'
            data = varget_hdf5(ncfile,varname,varargin{:});
            
            % Revisit this at some point.
            info.Datatype = 'enhanced';          
        otherwise
            rethrow(me);
    end
end

%----------------------------------------------------------------------
function [data,info] = varget_nc(ncfile,varname,varargin)

ncid=netcdf.open(ncfile,'NOWRITE');

try
    [data,info] = varget(ncid,varname,varargin{:});
catch me
    netcdf.close(ncid);
    handle_error(me);
end

netcdf.close(ncid);



%--------------------------------------------------------------------------
function [values,info] = varget(ncid,varname,varargin)

% Assume that we retrieve the variable in the root group until we know
% otherwise.  Assume that the variable name is given.
gid = ncid;

% If the library is > 4 and the format is unrestricted netcdf-4, then we
% may need to drill down thru the groups.
lv = netcdf.inqLibVers;
if lv(1) == '4'
    fmt = netcdf.inqFormat(ncid);
    if strcmp(fmt,'FORMAT_NETCDF4') && (numel(strfind(varname,'/')) > 1)
        varpath = regexp(varname,'/','split');
        ngroups = numel(varpath)-1;
        for k = 2:ngroups
            gid = netcdf.inqNcid(gid,varpath{k});
        end
        varname = varpath{ngroups+1};
    end
end


% The assumption is that ncid is ID for the group (possibly the root group)
% containing the named variable, which had better not have a slash in the
% name.  
preserve_fvd = nc_getpref('PRESERVE_FVD');

varid=netcdf.inqVarID(gid,varname);
info = nc_getvarinfo_tmw(gid,varid);

% Check the datatype.  We can't handle enhanced datatypes here.
[dud,xtype,dimids]=netcdf.inqVar(gid,varid); %#ok<ASGLU>
switch(xtype)
    case { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}
        % Do nothing.
    otherwise
        % Have to let the HDF5 backend handle it.
        error('snctools:switchBackendsToHDF5', 'Unhandled by TMW backend');
end

nvdims = numel(dimids);
var_size = get_varsize(gid,varid,preserve_fvd);
if any(var_size==0)
    values = zeros(var_size); % values = [];
    return
end

[start,count,stride] = snc_get_indexing(nvdims,var_size,varargin{:});

% R2008b expects to preserve the fastest varying dimension, so if the
% user didn't want that, we have to reverse the indices.
if ~preserve_fvd
    start = fliplr(start);
    count = fliplr(count);
    stride = fliplr(stride);
end


% Pack up the input parameters to the netcdf package function.
ncargs{1} = gid;
ncargs{2} = varid;
if nvdims ~= 0
    if ~isempty(start)
        ncargs{3} = start;
    end
    if ~isempty(count)
        ncargs{4} = count;
    end
    if ~isempty(stride)
        ncargs{5} = stride;
    end
end


values = netcdf.getVar(ncargs{:});


% If it's a 1D vector, make it a column vector.  Otherwise permute the
% data to make up for the row-major-order-vs-column-major-order issue.
if isvector(values)
    if (size(values,2) > 1)
        % same as 'ISROW' (2010b).  If it's already a column, then we don't
        % need to do anything.
        values = values(:);
    end
elseif ~preserve_fvd
    % In other words, if we generally need to permute AND if we have a
    % real 2D or higher matrix, then go ahead and permute.
    pv = fliplr ( 1:ndims(values) );
    values = permute(values,pv);
end




%-----------------------------------------------------------------------
function var_size = get_varsize(ncid,varid,preserve_fvd)
% GET_VARSIZE: Need to figure out just how big the variable is.

[dud,xtype,dimids]=netcdf.inqVar(ncid,varid); %#ok<ASGLU>
nvdims = numel(dimids);
% If not a singleton, we need to figure out how big the variable is.
if nvdims == 0
    var_size = [];
else
    var_size = zeros(1,nvdims);
    for j=1:nvdims,
        dimid = dimids(j);
        [dim_name,dim_size]=netcdf.inqDim(ncid, dimid); %#ok<ASGLU>
        var_size(j)=dim_size;
    end
end

% Reverse the dimensionsions?
if ~preserve_fvd
    var_size = fliplr(var_size);
end

return


%--------------------------------------------------------------------------
function values = varget_hdf5(ncfile,varname,varargin)


preserve_fvd = nc_getpref('PRESERVE_FVD');
if numel(varargin) > 0
    % H5READ is one-based.
    varargin{1} = varargin{1} + 1;
end
for j = 1:numel(varargin)
    if ~preserve_fvd
        varargin{j} = fliplr(varargin{j});
    end
end
values = h5read(ncfile,['/' varname],varargin{:});
            
if ~preserve_fvd
    values = permute(values, ndims(values):-1:1 );
end
            

%--------------------------------------------------------------------------
function handle_error(e)

v = version('-release');

 
switch(e.identifier)

    case 'MATLAB:imagesci:netcdf:libraryFailure'
        
        switch(v)
            case'2011b'
                % 2011b error messages are unfortunate.
                if strfind(e.message,'einvalcoords:indexExceedsDimensionBound')
                    % Bad start.
                    error(e.identifier, ...
                        'Index exceeds dimension bound.');
                elseif strfind(e.message,'eedge:startPlusCountExceedsDimensionBound')
                    
                    % Bad count
                    error(e.identifier, ...
                        'Start+count exceeds dimension bound.');
                    
                elseif strfind(e.message,'enotvar:variableNotFound')
                    error(e.identifier, 'Variable not found.');
                    
                else
                    rethrow(e);
                end
            otherwise
                rethrow(e);
        end
        
        
    otherwise
        rethrow(e);
end
