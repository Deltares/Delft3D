function test_nc_create_empty(mode)

fprintf('\t\tTesting NC_CREATE_EMPTY... ' );

if nargin < 1
    mode = nc_clobber_mode;
end

ncfile = 'foo.nc';
test_no_mode_given(ncfile);
test_64bit_mode(ncfile);

if ischar(mode) && strcmp(mode,'netcdf4-classic')
    test_netcdf4_classic(ncfile);
end

fprintf('OK\n');






%--------------------------------------------------------------------------
function test_netcdf4_classic ( ncfile )
% Create a netcdf-4 file.

delete(ncfile);
nc_create_empty(ncfile,nc_netcdf4_classic);
fid = fopen(ncfile,'r');
x = fread(fid,4,'uint8=>char');
fclose(fid);

if ~strcmp(x(2:4)','HDF')
	error('Did not create a netcdf-4 file');
end
return




%--------------------------------------------------------------------------
function test_no_mode_given ( ncfile )
% Should create a classic netcdf-3 file if no mode argument given.
nc_create_empty ( ncfile );
md = nc_info ( ncfile );

if ~isempty(md.Dataset)
	error('number of variables was not zero');
end

if ~isempty(md.Attribute)
	error('number of global attributes was not zero');
end

if ~isempty(md.Dimension)
	error('number of dimensions was not zero');
end

fid = fopen(ncfile,'r');
x = fread(fid,4,'uint8=>uint8'); 
fclose(fid);

if ~strcmp(char(x(1:3)'),'CDF') && x(4) ~= 1
	error('Did not create a netcdf-3 classic file');
end

return






%--------------------------------------------------------------------------
function test_64bit_mode ( ncfile )
% should create a 64-bit offset netcdf-3 file.

mode = bitor ( nc_clobber_mode, nc_64bit_offset_mode );

nc_create_empty ( ncfile, mode );
md = nc_info ( ncfile );

if ~isempty(md.Dataset)
	error('number of variables was not zero');
end

if ~isempty(md.Attribute)
	error('number of global attributes was not zero');
end

if ~isempty(md.Dimension)
	error('number of dimensions was not zero');
end

fid = fopen(ncfile,'r');
x = fread(fid,4,'uint8=>uint8'); 
fclose(fid);

if ~strcmp(char(x(1:3)'),'CDF') && x(4) ~= 2
	error('Did not create a netcdf-3 64-bit offset file');
end
return





