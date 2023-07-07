function dinfo = nc_getdiminfo_java ( arg1, arg2 )
% java backend for NC_GETDIMINFO

import ucar.nc2.dods.*  ;
import ucar.nc2.*       ;

if isa(arg1,'char') && isa(arg2,'char')
	if exist(arg1,'file')
		jncid = NetcdfFile.open(arg1);
	else
        jncid = snc_opendap_open(arg1);
	end
	dim = jncid.findDimension(arg2);
elseif isa(arg1,'ucar.nc2.NetcdfFile') ...
        && isa(arg2,'ucar.nc2.Dimension')
	jncid = arg1;
	dim = arg2;
elseif isa(arg1,'ucar.nc2.dods.DODSNetcdfFile') ...
        && isa(arg2,'ucar.nc2.Dimension')
	jncid = arg1;
	dim = arg2;
else
	error ( 'snctools:nc_getdiminfo:java:badDatatypes', ...
        ['For a java retrieval, datatypes must be either both char, ' ...
         'or one must be a file ID and the other a dimension ID.' ]);
end

dinfo.Name = char(dim.getName());
dinfo.Length = dim.getLength();
dinfo.Unlimited = dim.isUnlimited();

if isa(arg1,'char') && isa(arg2,'char')
	jncid.close();	
end

return

