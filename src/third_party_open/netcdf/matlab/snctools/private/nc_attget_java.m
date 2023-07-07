function attval = nc_attget_java(ncfile, varname, attribute_name )
% attget_JAVA:  This function retrieves an attribute using the java API
       
[jncid,close_it] = open_data_source(ncfile);
v = version('-release');
switch(v)
case {'14','2006a','2006b','2007a'}
	attval = attget_2007a(jncid,varname,attribute_name,close_it);
otherwise
	attval = attget_2007b(jncid,varname,attribute_name,close_it);
end

%--------------------------------------------------------------------------
function attval = attget_2007a(jncid,varname,attribute_name,close_it)

attval = attget ( jncid, varname, attribute_name );

if close_it
    close(jncid);
end

%--------------------------------------------------------------------------
function attval = attget_2007b(jncid,varname,attribute_name,close_it)
% 2007b and later allow us to catch errors.

try
    attval = attget ( jncid, varname, attribute_name );
catch 
    if close_it
        close(jncid);
    end
    rethrow(lasterror)
end

if close_it
    close(jncid);
end


%--------------------------------------------------------------------------
function [jncid, close_it] = open_data_source(ncfile)

import ucar.nc2.dods.*    
import ucar.nc2.* 

close_it = true;

% Try it as a local file.  If not a local file, try as
% via HTTP, then as dods
if isa(ncfile,'ucar.nc2.NetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif exist(ncfile,'file')
    fid = fopen(ncfile);
    ncfile = fopen(fid);
    fclose(fid);
	jncid = NetcdfFile.open(ncfile);
else
	try 
		jncid = NetcdfFile.open ( ncfile );
	catch %#ok<CTCH>
		try
            jncid = snc_opendap_open(ncfile);
		catch %#ok<CTCH>
			error ( 'snctools:nc_varget_java:fileOpenFailure', ...
                'Could not open ''%s'' as either a local file, a regular URL, or as a DODS URL.', ...
                ncfile);
		end
	end
end


%--------------------------------------------------------------------------
function values = attget(jncid,varname,attribute_name)

root_group = jncid.getRootGroup();

if ischar ( varname ) && (isempty(varname))

	% The user passed in ''.  That means NC_GLOBAL.
	warning ( 'snctools:nc_attget:java:doNotUseGlobalString', ...
	          'Please consider using the m-file NC_GLOBAL.M instead of the empty string.' );
    jatt = root_group.findGlobalAttribute(attribute_name);

elseif ischar(varname) && (strcmpi(varname,'global'))

	% The user passed in 'global'.   Is there a variable named 'global'?
    jvarid = root_group.findVariable(varname);
	if isempty(jvarid)
		% No, it's a global attribute.
		warning ( 'snctools:attget:java:doNotUseGlobalString', ...
			'Please consider using ''NC_GLOBAL'' or -1 instead of the empty string.' );
    	jatt = root_group.findAttribute(attribute_name);
	else
    	jatt = root_group.findAttribute(attribute_name);
	end

elseif ischar ( varname )

    % Just a regular variable name.  Might be embedded in a group, though.
    jgid = root_group;
    if varname(1) == '/'
        if strcmp(version('-release'),'2006a')
            error('snctools:attget:java:groupsNotSupported', ...
                'Groups are not supported on R2006a.''%s''.', varname);
        end
        % Drill down thru any groups.
        group_parts = regexp(varname,'/','split');
        for j = 2:numel(group_parts)-1
            jgid = jgid.findGroup(group_parts{j});
        end
        varname = group_parts{end};
        
    end

    % Do we have a variable attribute or a group attribute?
    last_gid = jgid.findGroup(varname);
    if ~isempty(last_gid)
        jatt = last_gid.findAttribute(attribute_name);
    else
        jvarid = jgid.findVariable(varname);
        if isempty(jvarid)
            error('snctools:attget:java:variableNotFound', ...
                'Could not find variable ''%s''.', varname);
        end
        jatt = jvarid.findAttribute ( attribute_name );
    end

else

    % The user passed a numeric identifier for the variable.  
    % Assume that this means a global attribute.
    jatt = root_group.findAttribute(attribute_name);
end

if isempty(jatt)
    error ( 'snctools:attget:java:attributeNotFound', ...
		'Could not locate attribute ''%s''.', attribute_name );
end


% Retrieve the values.  Convert it to the appropriate matlab datatype.
if ( jatt.isString() ) 
    values = jatt.getStringValue();
    values = char ( values );
	return
end

% Ok, so it's numeric data.
% convert it to a numeric array.
if jatt.getLength == 0
    values = [];
    return;
end

j_array = jatt.getValues();
values = j_array.copyTo1DJavaArray();
values = values';

theDataTypeString = char(jatt.getDataType.toString()) ;
switch (theDataTypeString)
    case 'double'
        values = double(values);
    case 'float'
        values = single(values);
    case 'int'
        values = int32(values);
    case 'short'
        values = int16(values);
    case 'byte'
        values = int8(values);
    otherwise
        error ( 'snctools:attget:badDatatype', ...
            'Unhandled attribute type ''%s'' for attribute ''%s''', ...
            theDataTypeString, attribute_name );
end


