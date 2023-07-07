function verify_netcdf4(ncfile)
fid = fopen(ncfile,'r');
x = fread(fid,4,'uint8=>char');
fclose(fid);

if ~strcmp(x(2:4)','HDF')
	error('%s is not a netcdf-4 file');
end
