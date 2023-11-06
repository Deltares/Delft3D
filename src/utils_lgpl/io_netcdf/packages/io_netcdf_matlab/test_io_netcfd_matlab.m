[pathstr,name,ext] = fileparts(which(mfilename()));

%% Add library and header path
% TODO: AvD: later ship binary dlls in a more standard place than this:
addpath(fullfile(pathstr, '..', 'io_netcdf','dll','x64','Debug'));
addpath(fullfile(pathstr, '..', 'io_netcdf','include'));

%% Load library and inspect it
loadlibrary('io_netcdf');
libfunctions('io_netcdf','-full');

%% Prepare reading UGRID file
filename     = fullfile(pathstr, '..', 'test_data', 'hex7_ugrid_net.nc');
mode         = 0; % READ_ONLY
ioncidp      = libpointer('int32Ptr', 0);
iconvtypep   = libpointer('int32Ptr', 0);
convversionp = libpointer('doublePtr', 0.0);

%% Prepare for mesh geometry
nmeshp = libpointer('int32Ptr', 0);
nnodep = libpointer('int32Ptr', 0);
nedgep = libpointer('int32Ptr', 0);
nfacep = libpointer('int32Ptr', 0);
nmaxfacenodesp = libpointer('int32Ptr', 0);


%% Open the file
ierr = calllib('io_netcdf','ionc_open',filename,mode,ioncidp,iconvtypep,convversionp);
fprintf('Opened file ''%s'', status: %d\nDetected conventions: %d, v%2.1f\n', filename, ierr, iconvtypep.Value, convversionp.Value);

%% Inquire number of meshes in the file
ierr = calllib('io_netcdf','ionc_get_mesh_count',ioncidp,nmeshp);

%% Inquire the mesh dimensions
imesh = 1; % Only inquire mesh #1 for now
ierr = calllib('io_netcdf','ionc_get_node_count',ioncidp,imesh,nnodep);
ierr = calllib('io_netcdf','ionc_get_edge_count',ioncidp,imesh,nedgep);
ierr = calllib('io_netcdf','ionc_get_face_count',ioncidp,imesh,nfacep);
ierr = calllib('io_netcdf','ionc_get_max_face_nodes',ioncidp,imesh,nmaxfacenodesp);
fprintf('Reading grid #%d from file ''%s'', status: %d\n#nodes:\t%d\n#edges:\t%d\n#faces:\t%d\n', imesh, filename, ierr, nnodep.Value, nedgep.Value, nfacep.Value)

%% Allocate and read the actual grid coordinates
nodexp = libpointer('voidPtrPtr', zeros(1,nnodep.Value));
nodeyp = libpointer('voidPtrPtr', zeros(1,nnodep.Value));

ierr = calllib('io_netcdf', 'ionc_get_node_coordinates', ioncidp, imesh, nodexp, nodeyp, nnodep);

% TODO: AvD: read edge_nodes + face_nodes

%% Finalize
unloadlibrary('io_netcdf');
