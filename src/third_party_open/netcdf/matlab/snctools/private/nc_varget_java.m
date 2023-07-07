function [values,varinfo] = nc_varget_java(ncfile,varname,varargin)
% NC_VARGET_JAVA:  Java backend for nc_varget.

snc_turnoff_log4j;
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
    % non-opendap HTTP?
    fid = fopen(ncfile);
    ncfile = fopen(fid);
    fclose(fid);
    jncid = NetcdfFile.open(ncfile);
else
    try 
        jncid = NetcdfFile.open(ncfile);
    catch %#ok<CTCH>
        try
            jncid = snc_opendap_open(ncfile);
        catch %#ok<CTCH>
            error ( 'snctools:nc_varget_java:fileOpenFailure', ...
                'Could not open ''%s'' with java backend.' , ncfile);
        end
    end
end

% Get the variable object
jvarid = jncid.findVariable(varname);
if isempty ( jvarid )
    error('snctools:varget:java:noSuchVariable', ...
        'findVariable failed on variable ''%s'', file ''%s''.',...
        varname,ncfile);
end


varinfo = nc_getvarinfo_java ( jncid, jvarid );
var_size = varinfo.Size;
if any(var_size==0)
    values = zeros(var_size); % values = [];
    return
end

theDataType = jvarid.getDataType();
theDataTypeString = char ( theDataType.toString() ) ;

[start,count,stride] = get_indexing(jvarid,varinfo,varargin{:});


try
    
    if isempty(varinfo.Dimension)
        values = read_singleton_var(jvarid,theDataTypeString);
    else
        values = read_var(jvarid,theDataTypeString,start,count,stride);
    end
    
catch %#ok<CTCH>
    
    if close_it
        close(jncid);
    end
    rethrow(lasterror);
    
end


% Permute?
if (numel(var_size) == 1) ...
        && (strcmp(theDataType,'String') || strcmp(theDataType,'char'))
    values = values';
elseif length(var_size) == 1
    values = values(:);
else
    preserve_fvd = nc_getpref('PRESERVE_FVD');
    if preserve_fvd
        values = permute(values,numel(var_size):-1:1);
    else
        rdims = var_size;
        values = reshape(values,count);
    end


end                                                                                   



% If we were passed an open java file id, don't close it upon exit.
if close_it
    close ( jncid );
end


return








%--------------------------------------------------------------------------
function [start,count,stride] = get_indexing(jvarid,varinfo,varargin)

import ucar.nc2.dods.*     
import ucar.nc2.* 

theDimensions = jvarid.getDimensions();
nDims = theDimensions.size();

if isempty(varinfo.Dimension)
    % Singleton variable.
    start = 0;
    count = 1;
    stride = 1;
    return;
end
[start,count,stride] = snc_get_indexing(nDims,varinfo.Size,varargin{:});


% Java expects in C-style order.
preserve_fvd = nc_getpref('PRESERVE_FVD');
if preserve_fvd
    start = fliplr(start);
    count = fliplr(count);
    stride = fliplr(stride);
end




%--------------------------------------------------------------------------
function value = read_singleton_var(jvarid,theDataTypeString)
% reads a singleton

switch ( theDataTypeString )

    case 'char'
        value = jvarid.read();
        value = char ( value.toString() );

    case 'String'
        jdata = jvarid.read();
        value = snc_pp_strings(jvarid,jdata,1);
        
    case 'double'
        value = jvarid.readScalarDouble();

    case 'float'
        value = jvarid.readScalarFloat();
        value = single(value);

    case 'int'
        value = jvarid.readScalarInt();
        value = int32(value);

    case 'short'
        value = jvarid.readScalarShort();
        value = int16(value);

    case 'byte'
        value = jvarid.readScalarByte();
        value = int8(value);

    otherwise
        error ('snctools:nc_varget:var1:java:unhandledDatatype', ...
            'unhandled datatype ''%s''', theDataTypeString );
    
end
    
    
return






%--------------------------------------------------------------------------
function values = read_var(jvarid,theDataTypeString,start,count,stride)
% read netcdf-java variable with normal dimensions.

% Have to use the method with the section selector.
% "1:2,10,:,1:100:10"
extent = start + count.*stride-1;
section_selector = '';
for j = 1:length(start)
    section_selector = sprintf ( '%s,%d:%d:%d', ...
        section_selector, start(j), extent(j), stride(j) );
end

% Get rid of the first comma.
section_selector(1) = [];

values = jvarid.read(section_selector);
switch ( theDataTypeString )
    case 'char'
        values = copyToNDJavaArray(values);
    case 'String'
        values = snc_pp_strings(jvarid,values,count);
        
    case { 'double', 'float', 'int', 'long', 'short', 'byte' }
        values = copyToNDJavaArray(values);

    otherwise
        error ( 'snctools:nc_varget:vars:java', ...
            'unhandled datatype ''%s''', theDataTypeString );
    
end
    
    
return


