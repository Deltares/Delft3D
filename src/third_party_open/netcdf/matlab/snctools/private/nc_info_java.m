function fileinfo = nc_info_java(ncfile)
%NC_INFO_JAVA java backend for nc_info
%
% This function returns the same metadata structure using the java 
% API.  

import ucar.nc2.dods.*    
import ucar.nc2.*         

close_it = true;

% Try it as a local file.  If not a local file, try as
% via HTTP, then as dods
if isa(ncfile,'ucar.nc2.NetcdfFile')
	jncid = ncfile;
	ncfile = char(jncid.getLocation());
	close_it = false;
elseif isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile')
	jncid = ncfile;
	ncfile = char(jncid.getLocation());
	close_it = false;
elseif exist(ncfile,'file')
    fid = fopen(ncfile);
    ncfile = fopen(fid);
    fclose(fid);
	jncid = NetcdfFile.open(ncfile);
else
	try 
		jncid = NetcdfFile.open ( ncfile );
    catch  %#ok<CTCH>
		try
            jncid = snc_opendap_open(ncfile);
        catch  %#ok<CTCH>
			error ( 'snctools:nc_varget_java:fileOpenFailure', ...
                'Could not open ''%s'' as either a local file, a regular URL, or as a DODS URL.', ...
                ncfile );
		end
	end
end


root_group = jncid.getRootGroup();
fileinfo = nc_group_info_java(root_group);
fileinfo.Name = '/';
fileinfo.Datatype = [];
fileinfo.Filename = ncfile; %[name ext];

if close_it
	close ( jncid );
end


%--------------------------------------------------------------------------
function info = nc_group_info_java(parent_group)

info_template = struct('Name','','Dimension',[],'Dataset',[],'Attribute',[],'Group',[]);

info = info_template;
info.Dimension = get_diminfo_java(parent_group);
info.Dataset = get_varinfo_java(parent_group);
info.Name = ['/' char(parent_group.getName())];

% Get the global attributes and variable attributes
j_att_list = parent_group.getAttributes();
info.Attribute = nc_getattsinfo_java(j_att_list);


% Any sub groups?
child_groups = parent_group.getGroups();
ngroups = child_groups.size();
if ngroups == 0
	return	
end

info.Group = repmat(info_template,ngroups,1);

for j = 1:ngroups
	info.Group(j) = nc_group_info_java(child_groups.get(j-1));
end


return





%--------------------------------------------------------------------------
function Dimension = get_diminfo_java ( parent_group )
% GET_DIMENSIONS_J:  Get the dimensions using the java backend.

dim_count = 0;
dims = parent_group.getDimensions();

% Set up an empty list first, in order to pre-allocate.
Dimension.Name = '';
Dimension.Length = 0;
Dimension.Unlimited = 0;

Dimension = repmat ( Dimension, dims.size(), 1 );

dims_iterator = dims.listIterator();
while 1

    try
        % This throws an exception when we've reached the end of the list.
        jDim = dims_iterator.next();
    catch %#ok<CTCH>
        % Break out of the while loop, there are no more dimensions to 
        % process.
        break;
    end

    dim_count = dim_count + 1;

    mdim.Name = char ( jDim.getName() );
    mdim.Length = jDim.getLength();
    mdim.Unlimited = jDim.isUnlimited();

    Dimension(dim_count,1) = mdim;

end

if dim_count == 0

    % Singleton variable case.
    Dimension = [];

end





%--------------------------------------------------------------------------
function Dataset = get_varinfo_java(parent_group)

% Get information on the variables themselves.
var_count = 0;
var_list = parent_group.getVariables();
var_iterator = var_list.listIterator();

Attribute = struct('Name','','Nctype','','Datatype','','Value',NaN);
Dataset = struct('Name','','Nctype',0,'Datatype','','Unlimited',false,...
    'Dimension',{''},'Size',[],'Attribute',Attribute,'Chunking',[],...
    'Shuffle',0,'Deflate',0);


Dataset = repmat ( Dataset, var_list.size(), 1 );

while 1

    try

        % This throws an exception when we've reached the end of the list.
        jvarid = var_iterator.next();

    catch  %#ok<CTCH>
        
        % No more variables left to process.
        break;

    end

    var_count = var_count + 1;

	mDataset = nc_getvaridinfo_java(jvarid);
    
    % adjust the name, strip off the leading group name
    [pp,nn] = fileparts(mDataset.Name);
    mDataset.Name = nn;

    Dataset(var_count,1) = mDataset;

end

if var_count == 0
    Dataset = [];
end
