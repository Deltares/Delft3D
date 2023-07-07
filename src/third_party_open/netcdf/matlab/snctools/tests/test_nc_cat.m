function test_nc_cat(mode)

if nargin < 1
	mode = nc_clobber_mode;
end


fprintf('\t\tTesting NC_CAT...  ');
run_all_tests(mode);
fprintf('OK\n');
return




%--------------------------------------------------------------------------
function run_all_tests(mode)


test_normal_usage(mode);
test_dash(mode);
test_recvar_not_time(mode);







%--------------------------------------------------------------------------
function test_normal_usage(mode)


ncfile1 = 'ts1.nc';
ncfile2 = 'ts2.nc';
create_test_file(ncfile1,mode);
create_test_file(ncfile2,mode);
populate(ncfile1,ncfile2);

nc_cat(ncfile1,ncfile2);
expdata = [1 2 3 4 5 6]';
t = nc_varget(ncfile1,'time');
ddiff = abs(expdata - t);
if any(ddiff)
    error('failed');
end


return

%--------------------------------------------------------------------------
function test_dash(mode)
% test with a variable with a dash in the name.

ncfile1 = 'ts1.nc';
ncfile2 = 'ts2.nc';
create_dash_file(ncfile1,mode);
create_dash_file(ncfile2,mode);
populate_dash(ncfile1,ncfile2);

nc_cat(ncfile1,ncfile2);
expdata = [1 2 3 4 5 6]';
t = nc_varget(ncfile1,'time');
ddiff = abs(expdata - t);
if any(ddiff)
    error('failed');
end


return

%--------------------------------------------------------------------------
function test_recvar_not_time(mode)


ncfile1 = 'ts1.nc';
ncfile2 = 'ts2.nc';
create_test_file_not_time(ncfile1,mode);
create_test_file_not_time(ncfile2,mode);
populate_not_time(ncfile1,ncfile2);

nc_cat(ncfile1,ncfile2);
expdata = [1 2 3 4 5 6]';
t = nc_varget(ncfile1,'time2');
ddiff = abs(expdata - t);
if any(ddiff)
    error('failed');
end


return
%--------------------------------------------------------------------------
function populate(file1,file2)
v.time = ones(3,1);
v.heat = ones(180,360,3);
for j = 1:3
    v.time(j) = j;
    v.heat(:,:,j) = v.heat(:,:,j) * j;
end
if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
	v.heat = permute(v.heat,[3 2 1]);
end

nc_addnewrecs(file1,v);

clear v;
v.time = ones(3,1);
v.heat = ones(180,360,3);
for j = 1:3
    v.time(j) = j + 3;
    v.heat(:,:,j) = v.heat(:,:,j) * j + 3;
end
if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
	v.heat = permute(v.heat,[3 2 1]);
end

nc_addnewrecs(file2,v);

%--------------------------------------------------------------------------
function populate_dash(file1,file2)

v(1).Name = 'time';
v(1).Data = [1 2 3];
v(2).Name = 'wind-x';
v(2).Data = [4 5 6];

nc_addnewrecs(file1,v);

clear v;
v(1).Name = 'time';
v(1).Data = [4 5 6];
v(2).Name = 'wind-x';
v(2).Data = [7 8 9];


nc_addnewrecs(file2,v);

%--------------------------------------------------------------------------
function populate_not_time(file1,file2)
v.time2 = ones(3,1);
v.heat = ones(180,360,3);
for j = 1:3
    v.time2(j) = j;
    v.heat(:,:,j) = v.heat(:,:,j) * j;
end
if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
	v.heat = permute(v.heat,[3 2 1]);
end

nc_addnewrecs(file1,v);

clear v;
v.time2 = ones(3,1);
v.heat = ones(180,360,3);
for j = 1:3
    v.time2(j) = j + 3;
    v.heat(:,:,j) = v.heat(:,:,j) * j + 3;
end
if ~getpref('SNCTOOLS','PRESERVE_FVD',false)
	v.heat = permute(v.heat,[3 2 1]);
end

nc_addnewrecs(file2,v);


%--------------------------------------------------------------------------
function create_test_file_not_time(filename,mode)

nc_create_empty(filename,mode);
nc_adddim(filename,'time2',0);
nc_adddim(filename,'lon',360);
nc_adddim(filename,'lat',180);
if ~(ischar(mode) && strcmp(mode,'hdf4'))
	v.Name = 'time2';
	v.Dimension = {'time2'};
	nc_addvar(filename,v);
end


if getpref('SNCTOOLS','PRESERVE_FVD',false)
	v.Dimension = {'lat','lon','time2'};
else
	v.Dimension = {'time2','lon','lat'};
end
v.Name = 'heat';
nc_addvar(filename,v);


nc_attput(filename,nc_global,'creation_date',datestr(now));

%--------------------------------------------------------------------------
function create_dash_file(filename,mode)
if exist(filename,'file')
    delete(filename);
end

nc_create_empty(filename,mode);
nc_adddim(filename,'time',0);
if ~(ischar(mode) && strcmp(mode,'hdf4'))
	v.Name = 'time';
	v.Dimension = {'time'};
	nc_addvar(filename,v);
end


v.Dimension = {'time'};
v.Name = 'wind-x';
nc_addvar(filename,v);


nc_attput(filename,nc_global,'creation_date',datestr(now));
%--------------------------------------------------------------------------
function create_test_file(filename,mode)
if exist([pwd filesep filename],'file')
    delete(filename);
end

nc_create_empty(filename,mode);
nc_adddim(filename,'time',0);
nc_adddim(filename,'lon',360);
nc_adddim(filename,'lat',180);
if ~(ischar(mode) && strcmp(mode,'hdf4'))
	v.Name = 'time';
	v.Dimension = {'time'};
	nc_addvar(filename,v);
end


clear v
v.Name = 'heat';
if getpref('SNCTOOLS','PRESERVE_FVD',false)
	v.Dimension = {'lat','lon','time'};
else
	v.Dimension = {'time','lon','lat'};
end
nc_addvar(filename,v);


nc_attput(filename,nc_global,'creation_date',datestr(now));
