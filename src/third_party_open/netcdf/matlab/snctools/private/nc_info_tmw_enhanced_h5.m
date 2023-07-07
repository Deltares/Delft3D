function fileinfo = nc_info_tmw_enhanced_h5 ( ncfile )
% NC_INFO Enhanced model backend for Mathworks package.

hinfo = h5info(ncfile);
ncid=netcdf.open(ncfile, nc_nowrite_mode );

fileinfo = nc_group_info(ncfile,ncid,hinfo);
fileinfo.Format = 'NetCDF-4';

netcdf.close(ncid);

fileinfo.Filename = ncfile;

%--------------------------------------------------------------------------
function info = nc_group_info(ncfile,ncid,hinfo)

[ndims, nvars, ngatts] = netcdf.inq(ncid);

dimids = netcdf.inqDimIDs(ncid);
info.Name = netcdf.inqGrpNameFull(ncid);

% Get the dimensions
if ndims == 0
	Dimension = struct ( [] );
else
    Dimension = struct('Name','','Length',[],'Unlimited',false);
    Dimension = repmat(Dimension,ndims,1);
	for j = 1:ndims
		Dimension(j)=nc_getdiminfo_tmw(ncid,dimids(j));
	end
end



% Get the global attributes.
if ngatts == 0
	info.Attribute = struct([]);
else
    Attribute = struct('Name','','Nctype','','Datatype','','Value',NaN);
	for attnum = 0:ngatts-1
		Attribute(attnum+1) = nc_getattsinfo_tmw(ncfile,ncid,nc_global,attnum);
	end
	info.Attribute = Attribute;
end


% Get the variable information.
if nvars == 0
	Dataset = struct([]);
else
    Attribute = struct('Name','','Nctype','','Datatype','','Value',NaN);
    Dataset = struct('Name','','Nctype','','Datatype','','Unlimited',false,'Dimension',{''},'Size',[],'Attribute',Attribute,'Chunking',[],'Shuffle',[],'Deflate',[]);
	Dataset = repmat ( Dataset, nvars, 1 );
	for varid=0:nvars-1
		Dataset(varid+1) = nc_getvarinfo_tmw_enhanced_h5(ncid,varid,hinfo);
	end
end

info.Dimension = Dimension;
info.Dataset = Dataset;

Group = [];
childGroups = netcdf.inqGrps(ncid);
if numel(childGroups) > 0
    Group = nc_group_info_tmw(ncfile,childGroups(1));
    Group = repmat(Group, numel(childGroups),1);
    for j = 2:numel(childGroups)
        Group(j) = nc_group_info_tmw(ncfile,childGroups(j));
    end
end
info.Group = Group;


return
