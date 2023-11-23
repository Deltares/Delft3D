function nc_addvar(ncfile,varstruct)
%NC_ADDVAR  Add variable to NetCDF file.
%
%   nc_addvar(FILE,VARSTRUCT) adds a variable described by varstruct to a 
%   netcdf file.  VARSTRUCT is a structure with the following fields:
%
%      Name       - name of netcdf variable.
%      Datatype   - datatype of the variable.  This should be one of
%                   'double', 'float', 'int', 'short', or 'byte', or
%                   'char'. If omitted, this defaults to 'double'.
%      Dimension  - a cell array of dimension names.
%      Attribute  - a structure array.  Each element has two fields, 'Name'
%                   and 'Value'.   
%      Chunking   - defines the chunk size.  This can only be used with 
%                   netcdf-4 files.  The default value is [], which
%                   specifies no chunking.
%      Shuffle    - if non-zero, the shuffle filter is turned on.  The
%                   default value is off.  This can only be used with
%                   netcdf-4 files.
%      Deflate    - specifies the deflate level and should be between 0 and
%                   9.  Defaults to 0, which turns the deflate filter off.
%                   This can only be used with netcdf-4 files.
%
%   Example:  create a variable called 'earth' that depends upon two 
%   dimensions, 'lat' and 'lon'.
%      nc_create_empty('myfile.nc');
%      nc_adddim('myfile.nc','lon',361);
%      nc_adddim('myfile.nc','lat',181);
%      v1.Name = 'earth';
%      v1.Datatype = 'double';
%      v1.Dimension = { 'lat','lon' };
%      nc_addvar('myfile.nc',v1);
%      nc_dump('myfile.nc');
%
%   Example:  create a variable called 'mars' in a netCDF-4 classic file
%   that has two dimensions, 'lat' and 'lon'.  Use 10x10 chunking scheme
%   and turn on full deflate compression.  Please note that this is the 
%   ONLY recommended way to set a fill value in netCDF-4 files!
%      nc_create_empty('myfile.nc', nc_netcdf4_classic);
%      nc_adddim('myfile.nc','lon',361);
%      nc_adddim('myfile.nc','lat',181);
%      v2.Name = 'mars';
%      v2.Datatype = 'double';
%      v2.Attribute.Name = '_FillValue';
%      v2.Attribute.Value = -999;
%      v2.Dimension = { 'lat','lon' };
%      v2.Chunking = [10 10];
%      v2.Deflate = 9;
%      nc_addvar('myfile.nc',v2);
%      nc_dump('myfile.nc');
%
%   See also nc_adddim.


preserve_fvd = nc_getpref('PRESERVE_FVD');

if  ~ischar(ncfile) 
    error ( 'snctools:addvar:badInput', 'file argument must be character' );
end

if ( ~isstruct(varstruct) )
    error ( 'snctools:addvar:badInput', '2nd argument must be a structure' );
end

varstruct = validate_varstruct ( varstruct );

backend = snc_write_backend(ncfile);
switch ( backend )
    case 'mexnc'
        nc_addvar_mexnc(ncfile,varstruct,preserve_fvd);
    case 'tmw_hdf4'
        nc_addvar_hdf4(ncfile,varstruct,preserve_fvd);
    case 'tmw_hdf4_2011b'
        nc_addvar_hdf4(ncfile,varstruct,preserve_fvd);
    otherwise
        nc_addvar_tmw(ncfile,varstruct,preserve_fvd);
end



%--------------------------------------------------------------------------
function varstruct = validate_varstruct ( varstruct )

% Check that required fields are there.
% Must at least have a name.
if ~isfield ( varstruct, 'Name' )
    error ( 'snctools:addvar:badInput', ...
            'structure argument must have at least the ''Name'' field.' );
end

% Check that required fields are there.
% Default datatype is double
if ~isfield(varstruct,'Datatype')
    if ~isfield ( varstruct, 'Nctype' )
        varstruct.Datatype = 'double';
    else
        varstruct.Datatype = varstruct.Nctype;
    end

end

% Are there any unrecognized fields?
fnames = fieldnames ( varstruct );
for j = 1:length(fnames)
    fname = fnames{j};
    switch ( fname )

    case { 'Datatype', 'Nctype', 'Name', 'Dimension', 'Attribute', ...
            'FillValue', 'Storage', 'Chunking', 'Shuffle', 'Deflate', 'DeflateLevel' }

        % These are used to create the variable.  They are ok.
        
    case { 'Unlimited', 'Size', 'Rank' }
       
        % These come from the output of nc_getvarinfo.  We don't 
        % use them, but let's not give the user a warning about
        % them either.

    otherwise
        warning('snctools:addvar:unrecognizedFieldName', ...
            '%s:  unrecognized field name ''%s''.  Ignoring it...\n', mfilename, fname );
    end
end


% If the datatype is not a string.
% Change suggested by Brian Powell
if ( isa(varstruct.Datatype, 'double') && varstruct.Datatype < 7 )
    types={ 'byte' 'char' 'short' 'int' 'float' 'double'};
    varstruct.Datatype = char(types(varstruct.Datatype));
end


% Check that the datatype is known.
switch ( varstruct.Datatype )
    case { 'NC_DOUBLE', 'double', ...
            'NC_FLOAT', 'float', ...
            'NC_INT', 'int',  ...
            'NC_SHORT', 'short', ...
            'NC_BYTE', 'byte', ...
            'NC_CHAR', 'char'  }
        % Do nothing
    case 'single'
        varstruct.Datatype = 'float';
    case 'int32'
        varstruct.Datatype = 'int';
    case 'int16'
        varstruct.Datatype = 'short';
    case { 'int8','uint8' }
        varstruct.Datatype = 'byte';
    
    case { 'uint16', 'uint32', 'int64', 'uint64' }
        error('snctools:addvar:notClassicDatatype', ...
            'Datatype ''%s'' is not a classic model datatype.', ...
            varstruct.Datatype);
        
    otherwise
        error ( 'snctools:addvar:unknownDatatype', 'unknown type ''%s''\n', mfilename, varstruct.Datatype );
end

% Default Dimension is none.  Singleton scalar.
if ~isfield ( varstruct, 'Dimension' )
    varstruct.Dimension = [];
end

% Default Attributes are none
if ~isfield ( varstruct, 'Attribute' )
    varstruct.Attribute = [];
end

if ~isfield(varstruct,'Storage')
    varstruct.Storage = 'contiguous';
end

if ~isfield(varstruct,'Chunking')
    varstruct.Chunking = [];
end

if ~isfield(varstruct,'Shuffle')
    varstruct.Shuffle = 0;
end

if ~isfield(varstruct,'Deflate')
    varstruct.Deflate = 0;
end

if ~isfield(varstruct,'DeflateLevel')
    varstruct.DeflateLevel = 0;
end

