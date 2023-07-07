function bool = nc_isvar_tmw(ncfile,varname)
% TMW backend for NC_ISVAR.

ncid = netcdf.open(ncfile,'NOWRITE');
try
	netcdf.inqVarID(ncid,varname);
	bool = true;
catch myException %#ok<NASGU>
	bool = false;
end

netcdf.close(ncid);
return
