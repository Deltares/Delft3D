function nc_dump(file_name, varargin)
%nc_dump  Print netCDF metadata.
%   NC_DUMP(NCFILE) displays metadata about the netCDF file NCFILE.  NC_DUMP
%   is a counterpart to the 'ncdump' utility that comes with the netCDF
%   library.
%
%   NC_DUMP(NCFILE,LOCATION) displays metadata for LOCATION, which may be
%   either a variable in the root group or a netcdf-4 group.
%
%   NC_DUMP(NCFILE,<LOCATION>,fid) prints output to file opened 
%   with fid = fopen(...) instead of to screen (default fid=1: screen).
%   NC_DUMP(NCFILE,LOCATION,<'fname'>) prints output to new file 'fname'.
%
%   NC_DUMP(NCFILE,LOCATION,fid,<keyword,value>) passes keyword-value
%   pairs, LOCATION and fid must be supplied, use [] for defaults.
%   NC_DUMP(NCFILE,LOCATION,fid,'h',false) writes data too (not human readable)
%
%   If netcdf-java is on the java classpath, NC_DUMP can also display 
%   metadata for GRIB2 files as if they were netCDF files.
%
%   NC_DUMP will display OPeNDAP metadata as if they were netCDF files.
%   This is native to MATLAB in R2012a, but requires netcdf-java on earler
%   releases.
%
%   Setting the preference 'PRESERVE_FVD' to true will compel MATLAB to 
%   display the dimensions in the opposite order from what the C utility 
%   ncdump displays.  
% 
%   Example:  This example file is shipped with R2008b.
%       nc_dump('example.nc');
%  
%   Example:  Display metadata for an OPeNDAP URL.  On releases prior to
%   2012b, this requires the netcdf-java backend.  Because URLs can often
%   be so long, it is broken across multiple lines here for formatting 
%   purposes.
%       today = datestr(floor(now),'yyyymmdd');
%       server = 'http://motherlode.ucar.edu:8080';
%       dir = '/thredds/dodsC/satellite/CTP/SUPER-NATIONAL_1km/current';
%       url = sprintf('%s%s/SUPER-NATIONAL_1km_CTP_%s_0000.gini',server,dir,today);
%       nc_dump(url);
%
%   See also nc_info.
location = ''; 
fid      = 1;

% ncdump.exe options
%ncdump [-c|-h] [-v ...] [[-b|-f] [c|f]] [-l len] [-n name] [-p n[,n]] file
%  [-c]             Coordinate variable data and header information
%  [-h]             Header information only, no data
%  [-v var1[,...]]  Data for variable(s) <var1>,... only
%  [-b [c|f]]       Brief annotations for C or Fortran indices in data
%  [-f [c|f]]       Full annotations for C or Fortran indices in data
%  [-l len]         Line length maximum in data section (default 80)
%  [-n name]        Name for netCDF (default derived from file name)
%  [-p n[,n]]       Display floating-point values with less precision
%  file             File name of input netCDF file

OPT.h      = true; % -h
OPT.hwidth = Inf;  % max no values per line

switch(nargin)
    case 0
        help nc_dump
        return
    case 1
    case 2
        if ischar(varargin{1})
            location = varargin{1};
        else
            fid = varargin{1};
        end
        varargin{1}   = [];
        
    case 3
        location = varargin{1};
        fid      = varargin{2};
        varargin(1:2) = [];
        
    otherwise
       if ~isempty(varargin{1})
          location = varargin{1};
       end
       if ~isempty(varargin{2})
          fid = varargin{2};
       end
       varargin(1:2) = [];
end

keys = varargin(1:2:end);
vals = varargin(2:2:end);

ind = find(strcmp(keys,'h'));if any(ind);OPT.h = vals{ind};end;

if ischar(fid)
    close_fid = 1;    
    fid = fopen(fid,'w');
else
    close_fid = 0;    
end

info = nc_info(file_name);

fprintf (fid,'%s %s {\n',info.Format,info.Filename);

dump_group(info,location,fid);

if ~(OPT.h)
   dump_group_data(info,location,fid,file_name,OPT);
end

fprintf(fid,'}\n');

if close_fid
    fclose(fid);
end

return;

%--------------------------------------------------------------------------
function dump_group(group,restricted_variable,fid)

if strcmp(group.Name,'/') && isfield(group,'Format') && strcmp(group.Format,'NetCDF-4')
    fprintf(fid,'\nGroup %s {\n', group.Name);
elseif ~strcmp(group.Name,'/')
    fprintf(fid,'\nGroup %s {\n', group.Name);
end

dump_dimension_metadata(group, fid );
dump_datatype_metadata(group,fid);

not_found = dump_variables(group.Dataset,restricted_variable,fid);

if isempty(restricted_variable)
    if isfield(group,'Name') && ~strcmp(group.Name,'/')
        dump_group_attributes(group,fid,false);
    else
        dump_group_attributes(group,fid,true);
    end
end


if not_found || isempty(restricted_variable)
    
    if isfield(group,'Group') && numel(group.Group) > 0
        for j = 1:numel(group.Group)
            dump_group(group.Group(j),restricted_variable,fid);
        end
    end
end

if strcmp(group.Name,'/') && isfield(group,'Format') && strcmp(group.Format,'NetCDF-4')
    fprintf(fid,'} End Group %s\n\n', group.Name);
elseif ~strcmp(group.Name,'/')
    fprintf(fid,'} End Group %s\n\n', group.Name);   
end
fprintf(fid,'\n');

return

%--------------------------------------------------------------------------
function dump_group_data(group,restricted_variable,fid,file_name,OPT)

not_found = dump_variables_data(group.Dataset,restricted_variable,fid,file_name,OPT);

return

%--------------------------------------------------------------------------
function dump_datatype_metadata(info,fid)

if isempty(info.Datatype)
    return
end

ndatatypes = numel(info.Datatype);

fprintf(fid,'  datatypes:\n');
for j = 1:ndatatypes
    switch(info.Datatype(j).Class)
        case 'enum'
            dump_enum_datatype_metadata(info.Datatype(j),fid);
        case 'opaque'
            dump_opaque_datatype_metadata(info.Datatype(j),fid);
        case 'vlen'
            dump_vlen_datatype_metadata(info.Datatype(j),fid);
        case 'compound'
            dump_compound_datatype_metadata(info.Datatype(j), fid);
        otherwise
            warning('snctools:unhandledHDF5class', ...
                    'Unhandled HDF5 class %s.\n', info.Datatype(j).Class);
    end
end
fprintf(fid,'\n');

return


%--------------------------------------------------------------------------
function dump_opaque_datatype_metadata(info,fid)

fprintf(fid,'    opaque(%d) ''%s''\n', info.Size, info.Name);

return

%--------------------------------------------------------------------------
function dump_vlen_datatype_metadata(info,fid)

fprintf(fid,'    %s vlen ''%s''\n', info.Type.Type, info.Name);


return

%--------------------------------------------------------------------------
function dump_compound_datatype_metadata(info,fid)


fprintf(fid,'    compound ''%s''\n', info.Name);
for j = 1:numel(info.Type.Member)
    fprintf(fid,'      %s %s\n', info.Type.Member(j).Datatype.Type, info.Type.Member(j).Name);
end

return

%--------------------------------------------------------------------------
function dump_enum_datatype_metadata(info,fid)


fprintf(fid,'    %s enum ''%s''\n', info.Type.Type, info.Name);
for j = 1:numel(info.Type.Member)
    fprintf(fid,'      %s = %d\n', info.Type.Member(j).Name, info.Type.Member(j).Value);
end

return

%--------------------------------------------------------------------------
function dump_dimension_metadata(info,fid)

if isfield(info,'Dimension' )
    num_dims = numel(info.Dimension);
else
    num_dims = 0;
end

fprintf(fid,'dimensions:\n');
for j = 1:num_dims
    if info.Dimension(j).Unlimited
        fprintf(fid,'	%s = UNLIMITED ; (%i currently)\n', ...
                 deblank(info.Dimension(j).Name), info.Dimension(j).Length );
    else
        fprintf(fid, '	%s = %i ;\n', info.Dimension(j).Name,info.Dimension(j).Length );
    end
end
fprintf(fid,'\n');

return


%--------------------------------------------------------------------------
function not_found = dump_variables(Dataset,restricted_variable,fid)

% Is it here?
not_found = true;

for j = 1:numel(Dataset)
    if ~isempty(restricted_variable)
        if strcmp(restricted_variable,Dataset(j).Name)
            not_found = false;
        end
    end
end

if not_found && ~isempty(restricted_variable)
    return
end

pfvd = nc_getpref('PRESERVE_FVD');

fprintf (fid,'variables:\n' );

if pfvd == 0;
   fprintf (fid,'	// Preference ''PRESERVE_FVD'':  false,\n' );
   fprintf (fid,'	// dimensions consistent with ncBrowse, not with native MATLAB netcdf package.\n' );
else
   fprintf (fid,'	// Preference ''PRESERVE_FVD'':  true,\n' );
   fprintf (fid,'	// dimensions consistent with native MATLAB netcdf package, not with ncBrowse.\n' );
end


for j = 1:numel(Dataset)

    if ~isempty(restricted_variable)
        if ~strcmp(restricted_variable,Dataset(j).Name)
            continue
        end
    end

    dump_single_variable(Dataset(j),fid);

end

fprintf (fid,'\n' );

%--------------------------------------------------------------------------
function not_found = dump_variables_data(Dataset,restricted_variable,fid,file_name,OPT)

% Is it here?
not_found = true;

for j = 1:numel(Dataset)
    if ~isempty(restricted_variable)
        if strcmp(restricted_variable,Dataset(j).Name)
            not_found = false;
        end
    end
end
if not_found && ~isempty(restricted_variable)
    return
end
pfvd = nc_getpref('PRESERVE_FVD');
fprintf(fid,'data:\n');
for j = 1:numel(Dataset)
    if ~isempty(restricted_variable)
        if ~strcmp(restricted_variable,Dataset(j).Name)
            continue
        end
    end
    fprintf(fid,'\n');
    array = nc_vargetr(file_name,Dataset(j).Name);
    sz = size(array);
    
    if ischar(array)
        fmt = '"%s"';
    else
        fmt = '%g';
    end
    
    if prod(sz)==1
        fprintf(fid,['%s = ',fmt,';\n'], Dataset(j).Name,array);
    else
        fprintf(fid,['%s =\n'], Dataset(j).Name);
        array = permute(array,length(sz):-1:1);
        n  = length(array(:));
        dn = min(sz(end),OPT.hwidth);
        for i=[1:dn:n-dn-1] % always skip last line as it needs special eol treatment
           fprintf(fid,[' ',fmt,','], array(i:i+dn-1));
           fprintf(fid,'\n');
        end
        fprintf(fid,[' ',fmt,','],  array(i+dn:n-1));
        fprintf(fid,[' ',fmt,' ;'], array(n));
        fprintf(fid,'\n');
    end
    
% TO DO replace fillvalue with  "_"
% TO DO replace nan with "1.#QNAN"
    
end
fprintf (fid,'\n' );

%--------------------------------------------------------------------------
function dump_single_variable ( var_metadata , fid )

if isempty(var_metadata.Datatype)
    var_metadata.Datatype = 'ENHANCED MODEL DATATYPE';
end
fprintf(fid,'	%s ', var_metadata.Datatype);

fprintf(fid,'%s', var_metadata.Name );

if isempty(var_metadata.Dimension) 
    fprintf (fid, '([]), ' );
else
    fprintf (fid, '(%s', var_metadata.Dimension{1} );
    for k = 2:length(var_metadata.Size)
        fprintf (fid, ',%s', var_metadata.Dimension{k} );
    end
    fprintf (fid, '), ');
end


if isempty(var_metadata.Dimension)
    fprintf (fid, 'shape = [1]' );
else
    fprintf (fid, 'shape = [%d', var_metadata.Size(1)  );
    for k = 2:length(var_metadata.Size)
        fprintf (fid, ' %d', var_metadata.Size(k)  );
    end
    fprintf (fid, ']');
end


if isfield(var_metadata, 'Chunking')
    if ~isempty(var_metadata.Chunking)
        fprintf(fid,', Chunking = [' );
        fprintf(fid,'%d', var_metadata.Chunking(1));
        for j = 2:numel(var_metadata.Chunking)
            fprintf(fid,' %d', var_metadata.Chunking(j));
        end
        fprintf(fid,']');
    end
end

if isfield(var_metadata,'Deflate') && ~isempty(var_metadata.Deflate) ...
        && isfield(var_metadata, 'Chunking') && ~isempty(var_metadata.Chunking)
    fprintf(fid,', Deflate = %d', var_metadata.Deflate);
end

fprintf(fid,'\n');

% Now do all attributes for each variable.
num_atts = length(var_metadata.Attribute);
for k = 1:num_atts
    dump_single_attribute(var_metadata.Attribute(k),var_metadata.Name,fid);
end

return

%--------------------------------------------------------------------------
function dump_single_attribute ( attribute, varname , fid )

if isnumeric(varname)
   fid = varname;
   varname = ''
end

switch ( attribute.Datatype )
    case ''
        att_val = '';
        att_type = 'NC_NAT';
    case 'int8'
        att_val = sprintf ('%d ', fix(attribute.Value) );
        att_type = 'b';
    case 'uint8'
        att_val = sprintf ('%d ', fix(attribute.Value) );
        att_type = 'ub';        
    case 'char'
        att_val = sprintf ('"%s" ', attribute.Value );
        att_type = '';
    case 'int16'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 's';
     case 'uint16'
        att_val = sprintf ('%d ', attribute.Value );
        att_type = 'us';       
    case 'int32'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'd';
    case 'uint32'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'ud'; 
     case 'int64'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'L';
    case 'uint64'
        att_val = sprintf ('%i ', attribute.Value );
        att_type = 'UL';        
    case 'single'
        att_val = sprintf ('%f ', attribute.Value );
        att_type = 'f';
    case 'double'
        att_val = sprintf ('%g ', attribute.Value );
        att_type = '';
    case 'string'
        att_type = '';
        % If it's a char value, then treat it like NC_CHAR.
        if ischar(attribute.Value)
            att_val = sprintf ('"%s" ', attribute.Value);
        elseif isempty(attribute.Value)
            att_val = '{}';
        else
            att_val = cellstr2str(attribute.Value);
        end
    otherwise
        if strncmp(attribute.Datatype,'enum',4)
            att_val = cellstr2str(attribute.Value);
            att_type = '';
        elseif strncmp(attribute.Datatype,'vlen',4)
            att_val = vlenattr2str(attribute);
            att_type = '';
        elseif strncmp(attribute.Datatype,'opaque',6)
            att_val = vlenattr2str(attribute);
            att_type = '';
        elseif strncmp(attribute.Datatype,'compound',8)
            att_val = '';
            att_type = [attribute.Datatype ' (not displayed)'];
        else
            error('unhandled datatype "%s"', attribute.Datatype);
        end
end

if ~exist('varname','var')
    fprintf(fid, '		%s:%s = %s%s;\n', ...
         varname,attribute.Name, att_val, att_type);
else
    fprintf(fid, '		%s:%s = %s%s;\n', ...
         varname,attribute.Name, att_val, att_type);
end

return
   

%--------------------------------------------------------------------------
function strval = compoundattr2str(attr)
strval = '{';
fields = fieldnames(attr.Value);
n = numel(fields);
for j = 1:n
    strval = [strval ' ' compound_field_val2str(attr.Value.(fields{j}))];
end
strval = [strval '}'];

%--------------------------------------------------------------------------
function strval = compound_field_val2str(field_value)

if isnumeric(field_value)
    strval = num2str(field_value);
else
    strval = field_value;
end

%--------------------------------------------------------------------------
function strval = vlenattr2str(attr)
strval = '{';
for j = 1:numel(attr.Value)
    strval = [strval vlenattval2str(attr.Value{j})];
end
strval = [strval '}'];


%--------------------------------------------------------------------------
function strval = vlenattval2str(vlen_val)
if isnumeric(vlen_val)
    strval = sprintf('{ %s }', num2str(vlen_val'));
else
    strval = '{ UNHANDLED }';
end
    
%--------------------------------------------------------------------------
function strval = cellstr2str(attval)

strval = ['{''' attval{1} '''' ];
for j = 2:numel(attval)
    strval = sprintf('%s, ''%s''', strval, attval{j});
end
strval = [strval '}'];
            
%--------------------------------------------------------------------------
function dump_group_attributes(group,fid,is_global)


if isfield(group,'Attribute')
    num_atts = numel(group.Attribute);
else
    num_atts = 0;
end

if num_atts > 0
    if is_global
        fprintf (fid, '//global attributes:\n' );
    else 
        fprintf(fid,'//group attributes:\n');
    end
end

for k = 1:num_atts
   dump_single_attribute(group.Attribute(k),'',fid);
end


fprintf (fid, '\n' );

return
