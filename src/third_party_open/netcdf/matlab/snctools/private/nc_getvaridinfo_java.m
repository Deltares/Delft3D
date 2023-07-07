function Dataset = nc_getvaridinfo_java(jvarid)
% NC_GETVARIDINFO_JAVA:  returns metadata structure for a netcdf variable
%
% This function is private to snctools.  It is called by nc_info and
% nc_getvarinfo, and uses the java API.
%
% USAGE:   Dataset = nc_getvaridinfo_java(jvarid);
% 
% PARAMETERS:
% Input:
%     jvarid:  
%         of type ucar.nc2.dods.DODSVariable
% Output:
%     Dataset:
%         struct of variable metadata

Attribute = struct('Name','','Nctype',0,'Datatype','','Value',NaN);
Dataset = struct('Name','','Nctype','','Datatype','','Unlimited',false,...
    'Dimension',{''},'Size',[],'Attribute',Attribute,'Chunking',[],...
    'Shuffle',0,'Deflate',0);

Dataset.Name = char ( jvarid.getName() );

% Get the datatype, store as an integer
datatype = char(jvarid.getDataType().toString());
switch ( datatype )
    case 'double'
        Dataset.Nctype = nc_double;
        Dataset.Datatype = datatype;
    case 'float'
        Dataset.Nctype = nc_float;
        Dataset.Datatype = 'single';
    case {'int','long'}
        Dataset.Nctype = nc_int;
        Dataset.Datatype = 'int32';
    case 'short'
        Dataset.Nctype = nc_short;
        Dataset.Datatype = 'int16';
        
    % So apparently, DODSNetcdfFile returns 'String', while
    % NetcdfFile returns 'char'???
    case 'String'
        Dataset.Nctype = 12;
        Dataset.Datatype = 'string';
    case 'char'
        Dataset.Nctype = nc_char;
        Dataset.Datatype = 'char';
    case 'byte'
        Dataset.Nctype = nc_byte;
        Dataset.Datatype = 'int8';
    otherwise
        error ( 'snctools:varinfo:unhandledDatatype', ...
            '%s:  unhandled datatype ''%s''\n', datatype );
end

% determine if it is unlimited or not
Dataset.Unlimited = double ( jvarid.isUnlimited() );

% Retrieve the dimensions
dims = jvarid.getDimensions();
nvdims = dims.size();
Dimension = cell(1,nvdims);
for j = 1:nvdims
	theDim = jvarid.getDimension(j-1);
	Dimension{j} = char ( theDim.getName() );
end
Dataset.Dimension = Dimension;

% Get the size of the variable
if nvdims == 0
	Dataset.Size = 1;
else
	Size = double ( jvarid.getShape() );
	Dataset.Size = Size';
end

if nc_getpref('PRESERVE_FVD')
	Dataset.Dimension = fliplr(Dataset.Dimension);
	Dataset.Size = fliplr(Dataset.Size);
end

% Get the list of attributes.
j_att_list = jvarid.getAttributes();
Dataset.Attribute = nc_getattsinfo_java(j_att_list);

return

