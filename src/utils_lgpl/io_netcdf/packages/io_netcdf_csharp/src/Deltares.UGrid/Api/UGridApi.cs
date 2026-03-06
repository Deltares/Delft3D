using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Implementation of the <see cref="IUGridApi"/> using the native Fortran dll io_netcdf.dll
    /// </summary>
    public sealed class UGridApi : IUGridApi
    {
        private DataSetConventions convention = DataSetConventions.CONV_NULL;
        private double versionNumber = double.NaN;

        private int dataSetId;
        private int startIndex;

        private bool fileOpenForReading;
        private bool fileOpenForWriting;
        private char spaceReplacementCharacter = '%';

        /// <summary>
        /// The static constructor loads the native library in the current process (<see cref="NativeLibrary.LoadNativeDll"/>)
        /// </summary>
        static UGridApi()
        {
            NativeLibrary.LoadNativeDll(IoNetCfdImports.GRIDDLL_NAME, Path.GetDirectoryName(typeof(UGridApi).Assembly.Location));
        }

        /// <summary>
        /// Disposes the unmanaged resources
        /// </summary>
        ~UGridApi()
        {
            ReleaseUnmanagedResources();
        }

        private bool FileOpen
        {
            get { return fileOpenForReading || fileOpenForWriting; }
        }

        /// <inheritdoc/>
        public bool IsUGridFile()
        {
            return FileOpen && convention == DataSetConventions.CONV_UGRID;
        }

        /// <inheritdoc/>
        public void CreateFile(string filePath, FileMetaData fileMetaData)
        {
            var mode = (int) NetcdfOpenMode.nf90_write;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_create_dll(filePath, ref mode, ref dataSetId));

            var metaData = fileMetaData.CreateMetaData();
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_add_global_attributes_dll(ref dataSetId, ref metaData));

            fileOpenForReading = true;
            fileOpenForWriting = true;
        }

        /// <inheritdoc/>
        public void Open(string filePath, OpenMode mode = OpenMode.Reading)
        {
            Close();

            if (string.IsNullOrEmpty(filePath) || !File.Exists(filePath))
            {
                throw new FileNotFoundException("Could not find file", filePath);
            }

            var conventionTypeNumber = 0;
            var openMode = (int) GetNetcdfOpenMode(mode);

            DoIoNetCfdCall(() => IoNetCfdImports.ionc_open_dll(filePath, ref openMode, ref dataSetId, 
                ref conventionTypeNumber, ref versionNumber));

            fileOpenForReading = true;
            fileOpenForWriting = mode != OpenMode.Reading;

            convention = typeof(DataSetConventions).IsEnumDefined(conventionTypeNumber)
                ? (DataSetConventions) conventionTypeNumber
                : DataSetConventions.CONV_OTHER;

            if (convention == DataSetConventions.CONV_UGRID && versionNumber < 1.0d)
            {
                convention = DataSetConventions.CONV_OTHER;
            }
        }

        /// <inheritdoc/>
        public void Close()
        {
            if (!FileOpen)
            {
                return;
            }
            
            versionNumber = double.NaN;
            convention = DataSetConventions.CONV_NULL;
            fileOpenForReading = false;
            fileOpenForWriting = false;

            DoIoNetCfdCall(()=>IoNetCfdImports.ionc_close_dll(ref dataSetId));
        }

        /// <inheritdoc/>
        public double GetVersion()
        {
            return versionNumber;
        }

        /// <inheritdoc/>
        public int GetMeshCount()
        {
            int numberOfMeshes = 0;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_mesh_count_dll(ref dataSetId, ref numberOfMeshes));
            return numberOfMeshes;
        }

        /// <inheritdoc/>
        public int GetNumberOfMeshByType(UGridMeshType meshType)
        {
            var numberOfMeshes = 0;
            var type = (int) meshType;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_number_of_meshes_dll(ref dataSetId, ref type, ref numberOfMeshes));
            return numberOfMeshes;
        }

        /// <inheritdoc/>
        public int[] GetMeshIdsByMeshType(UGridMeshType meshType)
        {
            return GetArrayFromIoNetCdf<int>(
                () => GetNumberOfMeshByType(meshType),
                (pointer, count) =>
                {
                    var type = (int) meshType;
                    DoIoNetCfdCall(() => IoNetCfdImports.ionc_ug_get_mesh_ids_dll(ref dataSetId, ref type, ref pointer, ref count));
                });
        }

        /// <inheritdoc/>
        public int GetVarCount(int meshId, GridLocationType locationType)
        {
            var count = 0;
            var type = (int) locationType;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_var_count_dll(ref dataSetId, ref meshId, ref type, ref count));
            return count;
        }

        /// <inheritdoc/>
        public int[] GetVarIds(int meshId, GridLocationType locationType)
        {
            return GetArrayFromIoNetCdf<int>(
                ()=> GetVarCount(meshId, locationType),
                (pointer, count) =>
                {
                    var type = (int)locationType;
                    DoIoNetCfdCall(() => IoNetCfdImports.ionc_inq_varids_dll(ref dataSetId, ref meshId, ref type, ref pointer, ref count));
                });
        }

        /// <inheritdoc/>
        public double GetVariableNoDataValue(string variableName, int meshId, GridLocationType location)
        {
            var locationValue = (int)location;

            if (GetVariableId(variableName, meshId) == -1)
            {
                return IoNetCfdImports.DEFAULT_FILL_VALUE;
            }

            // no separate call yet for getting no data value
            var noDataValue = double.NaN;
            GetArrayFromIoNetCdf<double>(() => GetLocationCount(location, meshId),
                (p, i) => DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_var_dll(ref dataSetId, ref meshId, ref locationValue, variableName, ref p, ref i, ref noDataValue)));

            return !double.IsNaN(noDataValue) 
                ? noDataValue 
                : IoNetCfdImports.DEFAULT_FILL_VALUE;
        }

        /// <inheritdoc/>
        public double[] GetVariableValues(string variableName, int meshId, GridLocationType location)
        {
            var locationValue =(int) location;

            if (GetVariableId(variableName, meshId) == -1)
            {
                return new double[0];
            }

            var noData = double.NaN;
            var arrayFromIoNetCdf = GetArrayFromIoNetCdf<double>(()=> GetLocationCount(location, meshId), 
                (p,i)=> DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_var_dll(ref dataSetId, ref meshId, ref locationValue, variableName, ref p, ref i, ref noData)));

            return arrayFromIoNetCdf;
        }

        /// <inheritdoc/>
        public void SetVariableValues(string variableName, string standardName, string longName, string unit, int meshId, GridLocationType location, double[] values, double noDataValue = -999)
        {
            var locationNumber = (int) location;

            var expectedValueCount = GetLocationCount(location, meshId);
            if (expectedValueCount != values.Length)
            {
                throw new ArgumentException($"The number of values ({values.Length}) does not match the expected number of values for this mesh and location type ({expectedValueCount})");
            }

            var variableId = GetVariableId(variableName, meshId);

            if (variableId == -1) // does not exist yet
            {
                var isMesh1dVariable = GetMeshIdsByMeshType(UGridMeshType.Mesh1D).Contains(meshId);
                var networkId = isMesh1dVariable
                    ? GetNetworkIdFromMeshId(meshId)
                    : 0; // dummy value

                var nf90Double = IoNetCfdImports.NF90_DOUBLE;
                var defaultFillValueInt = IoNetCfdImports.DEFAULT_FILL_VALUE_INT;
                var defaultFillValue = noDataValue;

                DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_def_var_dll), () =>
                    IoNetCfdImports.ionc_def_var_dll(ref dataSetId, ref meshId, ref networkId,
                        ref variableId, ref nf90Double, ref locationNumber, 
                        variableName, standardName, longName, unit, 
                        ref defaultFillValueInt, ref defaultFillValue));
            }

            SetArrayToIoNetCdf(values,
                (p, count) =>
                {
                    DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_def_var_dll), 
                        () =>  IoNetCfdImports.ionc_put_var_dll(ref dataSetId, ref meshId, ref locationNumber, variableName, ref p, ref count));
                });
        }

        /// <inheritdoc/>
        public void ResetMeshVerticesCoordinates(int meshId, double[] xValues, double[] yValues)
        {
            SetArraysToIoNetCdf<double>(new List<double[]> {xValues, yValues},
                (pointers, lengths) =>
                {
                    DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_node_coordinates_dll),() =>
                    {
                        var xValuesPtr = pointers[0];
                        var yValuesPtr = pointers[1];
                        var nNode = lengths[0];

                        return IoNetCfdImports.ionc_put_node_coordinates_dll(ref dataSetId, ref meshId,
                                ref xValuesPtr, ref yValuesPtr, ref nNode);
                    });
                });
        }

        /// <inheritdoc/>
        public int GetCoordinateSystemCode()
        {
            int epsgCode = 0;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_coordinate_system_dll(ref dataSetId, ref epsgCode));

            return epsgCode;
        }

        /// <inheritdoc/>
        public void SetCoordinateSystemCode(int epsgCode)
        {
            throw new NotImplementedException("This function is not yet implemented by io_NetCdf.");
        }

        /// <inheritdoc />
        public string GetReadableError(int errorCode)
        {
            return Marshal.PtrToStringAnsi(IoNetCfdImports.ionc_strerror_dll(ref errorCode));
        }

        /// <inheritdoc/>
        public int[] GetNetworkIds()
        {
            return GetArrayFromIoNetCdf<int>(
                GetNumberOfNetworks,
                (pointer, count) =>
                {
                    DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_network_ids_dll(ref dataSetId, ref pointer, ref count));
                });
        }

        /// <inheritdoc/>
        public int GetNumberOfNetworks()
        {
            var numberOfNetworks = 0;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_number_of_networks_dll(ref dataSetId, ref numberOfNetworks));
            return numberOfNetworks;
        }

        /// <inheritdoc/>
        public DisposableNetworkGeometry GetNetworkGeometry(int networkId)
        {
            var geometryDimensions = new Network1DGeometryDimensions();

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_network_nodes_count_dll),
                () => IoNetCfdImports.ionc_get_1d_network_nodes_count_dll(ref dataSetId, ref networkId, ref geometryDimensions.NumberOfNodes));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_network_branches_count_dll),
                () => IoNetCfdImports.ionc_get_1d_network_branches_count_dll(ref dataSetId, ref networkId, ref geometryDimensions.NumberOfBranches));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_network_branches_geometry_coordinate_count_dll),
                () => IoNetCfdImports.ionc_get_1d_network_branches_geometry_coordinate_count_dll(ref dataSetId, ref networkId, ref geometryDimensions.NumberOfBranchGeometryPoints));

            var numberOfNodes = geometryDimensions.NumberOfNodes;
            var numberOfBranches = geometryDimensions.NumberOfBranches;
            var numberOfGeometryPoints = geometryDimensions.NumberOfBranchGeometryPoints;

            var disposableNetworkGeometry = new DisposableNetworkGeometry();

            // read name
            var type = typeof(DisposableNetworkGeometry);
            var stringBuilder = new StringBuilder(type.GetBufferSize(nameof(DisposableNetworkGeometry.NetworkName)));
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_network_name_dll(ref dataSetId, ref networkId, stringBuilder));

            disposableNetworkGeometry.NetworkName = stringBuilder.ToString().Replace(spaceReplacementCharacter, ' ');

            // set with empty arrays for setting in io_netcdf
            disposableNetworkGeometry.InitializeWithEmptyData(geometryDimensions);
            
            // get object with pinned object pointers
            var geometry = disposableNetworkGeometry.CreateNetwork1DGeometry();

            // read nodes
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_read_1d_network_nodes_v1_dll),
                () => IoNetCfdImports.ionc_read_1d_network_nodes_v1_dll(ref dataSetId, ref networkId, ref geometry.NodeX, ref geometry.NodeY, ref geometry.NodeIds, ref geometry.NodeLongNames, ref geometryDimensions.NumberOfNodes));
            Array.Copy(geometry.NodeX.CreateValueArray<double>(numberOfNodes), disposableNetworkGeometry.NodesX, disposableNetworkGeometry.NodesX.Length);
            Array.Copy(geometry.NodeY.CreateValueArray<double>(numberOfNodes), disposableNetworkGeometry.NodesY, disposableNetworkGeometry.NodesY.Length);

            Array.Copy(geometry.NodeIds.CreateValueArray<string>(numberOfNodes, type.GetBufferSize(nameof(DisposableNetworkGeometry.NodeIds))), disposableNetworkGeometry.NodeIds, disposableNetworkGeometry.NodeIds.Length);
            Array.Copy(geometry.NodeLongNames.CreateValueArray<string>(numberOfNodes, type.GetBufferSize(nameof(DisposableNetworkGeometry.NodeLongNames))), disposableNetworkGeometry.NodeLongNames, disposableNetworkGeometry.NodeLongNames.Length);

            // read branches
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_network_branches_v1_dll),
                () => IoNetCfdImports.ionc_get_1d_network_branches_v1_dll(ref dataSetId, ref networkId, ref geometry.SourceNodes,
                    ref geometry.TargetNodes, ref geometry.BranchLengths, ref geometry.BranchIds,
                    ref geometry.BranchLongNames, ref geometry.BranchGeometryCount, ref geometryDimensions.NumberOfBranches,
                    ref startIndex));

            Array.Copy(geometry.SourceNodes.CreateValueArray<int>(numberOfBranches), disposableNetworkGeometry.NodesFrom, disposableNetworkGeometry.NodesFrom.Length);
            Array.Copy(geometry.TargetNodes.CreateValueArray<int>(numberOfBranches), disposableNetworkGeometry.NodesTo, disposableNetworkGeometry.NodesTo.Length);
            Array.Copy(geometry.BranchGeometryCount.CreateValueArray<int>(numberOfBranches), disposableNetworkGeometry.BranchGeometryNodesCount, disposableNetworkGeometry.BranchGeometryNodesCount.Length);
            Array.Copy(geometry.BranchOrder.CreateValueArray<int>(numberOfBranches), disposableNetworkGeometry.BranchOrder, disposableNetworkGeometry.BranchOrder.Length);
            Array.Copy(geometry.BranchLengths.CreateValueArray<double>(numberOfBranches), disposableNetworkGeometry.BranchLengths, disposableNetworkGeometry.BranchLengths.Length);
            Array.Copy(geometry.BranchIds.CreateValueArray<string>(numberOfBranches, type.GetBufferSize(nameof(DisposableNetworkGeometry.BranchIds))), disposableNetworkGeometry.BranchIds, disposableNetworkGeometry.BranchIds.Length);
            Array.Copy(geometry.BranchLongNames.CreateValueArray<string>(numberOfBranches, type.GetBufferSize(nameof(DisposableNetworkGeometry.BranchLongNames))), disposableNetworkGeometry.BranchLongNames, disposableNetworkGeometry.BranchLongNames.Length);

            // read branch geometry
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_read_1d_network_branches_geometry_dll),
                () => IoNetCfdImports.ionc_read_1d_network_branches_geometry_dll(ref dataSetId, ref networkId, ref geometry.BranchGeometryX, ref geometry.BranchGeometryY, ref geometryDimensions.NumberOfBranchGeometryPoints));

            Array.Copy(geometry.BranchGeometryX.CreateValueArray<double>(numberOfGeometryPoints), disposableNetworkGeometry.BranchGeometryX, disposableNetworkGeometry.BranchGeometryX.Length);
            Array.Copy(geometry.BranchGeometryY.CreateValueArray<double>(numberOfGeometryPoints), disposableNetworkGeometry.BranchGeometryY, disposableNetworkGeometry.BranchGeometryY.Length);

            if (TryIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_network_branchorder_dll), () => IoNetCfdImports.ionc_get_1d_network_branchorder_dll(ref dataSetId, ref networkId, ref geometry.BranchOrder, ref numberOfBranches)))
            {
                Array.Copy(geometry.BranchOrder.CreateValueArray<int>(numberOfBranches), disposableNetworkGeometry.BranchOrder, disposableNetworkGeometry.BranchOrder.Length);
            }

            if (TryIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_network_branchtype_dll), () => IoNetCfdImports.ionc_get_1d_network_branchtype_dll(ref dataSetId, ref networkId, ref geometry.BranchTypes, ref numberOfBranches)))
            {
                Array.Copy(geometry.BranchTypes.CreateValueArray<int>(numberOfBranches), disposableNetworkGeometry.BranchTypes, disposableNetworkGeometry.BranchTypes.Length);
            }

            return disposableNetworkGeometry;
        }

        /// <inheritdoc/>
        public int WriteNetworkGeometry(DisposableNetworkGeometry networkGeometry)
        {
            var networkId = -1;
            var name = networkGeometry.NetworkName.Replace(' ', spaceReplacementCharacter);
            var geometry = networkGeometry.CreateNetwork1DGeometry();
            var geometryDimensions = networkGeometry.CreateNetwork1DGeometryDimensions();

            // create network based on dimensions
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_create_1d_network_dll),
                () => IoNetCfdImports.ionc_create_1d_network_dll(ref dataSetId, ref networkId, name, ref geometryDimensions.NumberOfNodes, ref geometryDimensions.NumberOfBranches, ref geometryDimensions.NumberOfBranchGeometryPoints));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_write_1d_network_nodes_v1_dll),
                () => IoNetCfdImports.ionc_write_1d_network_nodes_v1_dll(ref dataSetId, ref networkId, ref geometry.NodeX, ref geometry.NodeY, ref geometry.NodeIds, ref geometry.NodeLongNames, ref geometryDimensions.NumberOfNodes));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_1d_network_branches_v1_dll),
                () => IoNetCfdImports.ionc_put_1d_network_branches_v1_dll(ref dataSetId, ref networkId, ref geometry.SourceNodes, ref geometry.TargetNodes, ref geometry.BranchIds, ref geometry.BranchLongNames,
                    ref geometry.BranchLengths, ref geometry.BranchGeometryCount, ref geometryDimensions.NumberOfBranches, ref startIndex));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_1d_network_branchorder_dll),
                () => IoNetCfdImports.ionc_put_1d_network_branchorder_dll(ref dataSetId, ref networkId, ref geometry.BranchOrder, ref geometryDimensions.NumberOfBranches));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_1d_network_branchtype_dll),
                () => IoNetCfdImports.ionc_put_1d_network_branchtype_dll(ref dataSetId, ref networkId, ref geometry.BranchTypes, ref geometryDimensions.NumberOfBranches));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_write_1d_network_branches_geometry_dll),
                () => IoNetCfdImports.ionc_write_1d_network_branches_geometry_dll(ref dataSetId, ref networkId, ref geometry.BranchGeometryX, ref geometry.BranchGeometryY, ref geometryDimensions.NumberOfBranchGeometryPoints));
            
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_end_define_mode_dll),
                () => IoNetCfdImports.ionc_end_define_mode_dll(ref dataSetId));

            return networkId;
        }

        /// <inheritdoc/>
        public int GetNetworkIdFromMeshId(int meshId)
        {
            int networkId = 0;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_network_id_from_mesh_id_dll(ref dataSetId, ref meshId, ref networkId));
            
            return networkId;
        }

        /// <inheritdoc/>
        public Disposable1DMeshGeometry GetMesh1D(int meshId)
        {
            // get dimensions
            var mesh1dDimensions = new Mesh1DGeometryDimensions();
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_mesh_discretisation_points_count_dll),
                () => IoNetCfdImports.ionc_get_1d_mesh_discretisation_points_count_dll(ref dataSetId, ref meshId, ref mesh1dDimensions.NumberOfNodes));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_edge_count_dll),
                () => IoNetCfdImports.ionc_get_edge_count_dll(ref dataSetId, ref meshId, ref mesh1dDimensions.NumberOfEdges));

            var disposable1DMeshGeometry = new Disposable1DMeshGeometry();
            disposable1DMeshGeometry.InitializeWithEmptyData(mesh1dDimensions);

            // pin in memory
            var mesh1d = disposable1DMeshGeometry.CreateMesh1DGeometry();

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_mesh_discretisation_points_v2_dll),
                () => IoNetCfdImports.ionc_get_1d_mesh_discretisation_points_v2_dll(ref dataSetId, ref meshId, ref mesh1d.BranchIds,
                    ref mesh1d.BranchOffsets, ref mesh1d.NodeIds, ref mesh1d.NodeLongNames, ref mesh1dDimensions.NumberOfNodes,
                    ref startIndex, ref mesh1d.NodeX, ref mesh1d.NodeY));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_1d_mesh_edges_dll),
                () => IoNetCfdImports.ionc_get_1d_mesh_edges_dll(ref dataSetId, ref meshId,
                ref mesh1d.EdgeBranchIds, ref mesh1d.EdgeCenterPointOffset,
                ref mesh1dDimensions.NumberOfEdges, ref startIndex, ref mesh1d.EdgeCenterPointX,
                ref mesh1d.EdgeCenterPointY));

            var type = typeof(Disposable1DMeshGeometry);

            var stringBuilder = new StringBuilder(type.GetBufferSize(nameof(disposable1DMeshGeometry.Name)));
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_mesh_name_dll),
                () => IoNetCfdImports.ionc_get_mesh_name_dll(ref dataSetId, ref meshId, stringBuilder));

            disposable1DMeshGeometry.Name = stringBuilder.ToString().Replace(spaceReplacementCharacter, ' '); ;
            Array.Copy(mesh1d.NodeX.CreateValueArray<double>(mesh1dDimensions.NumberOfNodes), disposable1DMeshGeometry.NodesX, disposable1DMeshGeometry.NodesX.Length);
            Array.Copy(mesh1d.NodeY.CreateValueArray<double>(mesh1dDimensions.NumberOfNodes), disposable1DMeshGeometry.NodesY, disposable1DMeshGeometry.NodesY.Length);
            Array.Copy(mesh1d.NodeIds.CreateValueArray<string>(mesh1dDimensions.NumberOfNodes, type.GetBufferSize(nameof(Disposable1DMeshGeometry.NodeIds))), disposable1DMeshGeometry.NodeIds, disposable1DMeshGeometry.NodeIds.Length);
            Array.Copy(mesh1d.NodeLongNames.CreateValueArray<string>(mesh1dDimensions.NumberOfNodes, type.GetBufferSize(nameof(Disposable1DMeshGeometry.NodeLongNames))), disposable1DMeshGeometry.NodeLongNames, disposable1DMeshGeometry.NodeLongNames.Length);

            Array.Copy(mesh1d.BranchIds.CreateValueArray<int>(mesh1dDimensions.NumberOfNodes), disposable1DMeshGeometry.BranchIDs, disposable1DMeshGeometry.BranchIDs.Length);
            Array.Copy(mesh1d.BranchOffsets.CreateValueArray<double>(mesh1dDimensions.NumberOfNodes), disposable1DMeshGeometry.BranchOffsets, disposable1DMeshGeometry.BranchOffsets.Length);

            Array.Copy(mesh1d.EdgeBranchIds.CreateValueArray<int>(mesh1dDimensions.NumberOfEdges), disposable1DMeshGeometry.EdgeBranchIds, disposable1DMeshGeometry.EdgeBranchIds.Length);
            Array.Copy(mesh1d.EdgeCenterPointOffset.CreateValueArray<double>(mesh1dDimensions.NumberOfEdges), disposable1DMeshGeometry.EdgeCenterPointOffset, disposable1DMeshGeometry.EdgeCenterPointOffset.Length);
            Array.Copy(mesh1d.EdgeCenterPointX.CreateValueArray<double>(mesh1dDimensions.NumberOfEdges), disposable1DMeshGeometry.EdgeCenterPointX, disposable1DMeshGeometry.EdgeCenterPointX.Length);
            Array.Copy(mesh1d.EdgeCenterPointY.CreateValueArray<double>(mesh1dDimensions.NumberOfEdges), disposable1DMeshGeometry.EdgeCenterPointY, disposable1DMeshGeometry.EdgeCenterPointY.Length);

            return disposable1DMeshGeometry;
        }

        /// <inheritdoc/>
        public int WriteMesh1D(Disposable1DMeshGeometry mesh, int networkId)
        {
            int meshId = 0;

            var mesh1d = mesh.CreateMesh1DGeometry();
            var mesh1dDimensions = mesh.CreateMesh1DGeometryDimensions();
            
            var networkName = GetNetworkNameById(networkId);
            var writeXy = 1;
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_create_1d_mesh_v1_dll),
                () => IoNetCfdImports.ionc_create_1d_mesh_v1_dll(ref dataSetId, networkName, ref meshId, mesh.Name.Replace(' ',spaceReplacementCharacter),
                    ref mesh1dDimensions.NumberOfNodes, ref mesh1dDimensions.NumberOfEdges, ref writeXy));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_1d_mesh_discretisation_points_v2_dll),
                () => IoNetCfdImports.ionc_put_1d_mesh_discretisation_points_v2_dll(ref dataSetId, ref meshId, ref mesh1d.BranchIds,
                    ref mesh1d.BranchOffsets, ref mesh1d.NodeIds, ref mesh1d.NodeLongNames, ref mesh1dDimensions.NumberOfNodes,
                    ref startIndex, ref mesh1d.NodeX, ref mesh1d.NodeY));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_1d_mesh_edges_dll),
                () => IoNetCfdImports.ionc_put_1d_mesh_edges_dll(ref dataSetId, ref meshId, ref mesh1d.EdgeBranchIds,
                    ref mesh1d.EdgeCenterPointOffset, ref mesh1dDimensions.NumberOfEdges, ref startIndex, ref mesh1d.EdgeCenterPointX,
                    ref mesh1d.EdgeCenterPointY));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_write_mesh_1d_edge_nodes_dll),
                () => IoNetCfdImports.ionc_write_mesh_1d_edge_nodes_dll(ref dataSetId, ref meshId,  ref mesh1dDimensions.NumberOfEdges, 
                    ref mesh1d.EdgeNodes, ref startIndex));
            
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_end_define_mode_dll),
                () => IoNetCfdImports.ionc_end_define_mode_dll(ref dataSetId));

            return meshId;
        }

        /// <inheritdoc/>
        public Disposable2DMeshGeometry GetMesh2D(int meshId)
        {
            // get dimensions
            var mesh2dDimensions = new Mesh2DGeometryDimensions();

            var networkId = 0;
            DoIoNetCfdCall(()=> IoNetCfdImports.ionc_get_meshgeom_dim_dll(ref dataSetId, ref meshId, ref networkId, ref mesh2dDimensions));

            var stringBuilder = new StringBuilder(255);
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_mesh_name_dll(ref dataSetId, ref meshId, stringBuilder));

            var disposable2DMeshGeometry = new Disposable2DMeshGeometry
            {
                Name = stringBuilder.ToString().Replace(spaceReplacementCharacter, ' '),
                MaxNumberOfFaceNodes = mesh2dDimensions.maxnumfacenodes
            };

            disposable2DMeshGeometry.InitializeWithEmptyData(mesh2dDimensions);

            var mesh2d = disposable2DMeshGeometry.CreateMeshGeometry();
            
            var includeArrays = true;
            
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_meshgeom_dll(ref dataSetId, ref meshId, ref networkId, ref mesh2d, ref startIndex, ref includeArrays));
            Array.Copy(mesh2d.nodex.CreateValueArray<double>(mesh2dDimensions.numnode), disposable2DMeshGeometry.NodesX, disposable2DMeshGeometry.NodesX.Length);
            Array.Copy(mesh2d.nodey.CreateValueArray<double>(mesh2dDimensions.numnode), disposable2DMeshGeometry.NodesY, disposable2DMeshGeometry.NodesY.Length);

            Array.Copy(mesh2d.edge_nodes.CreateValueArray<int>(mesh2dDimensions.numedge * 2), disposable2DMeshGeometry.EdgeNodes, disposable2DMeshGeometry.EdgeNodes.Length);
            Array.Copy(mesh2d.face_nodes.CreateValueArray<int>(mesh2dDimensions.maxnumfacenodes * mesh2dDimensions.numface), disposable2DMeshGeometry.FaceNodes, disposable2DMeshGeometry.FaceNodes.Length);
            Array.Copy(mesh2d.facex.CreateValueArray<double>(mesh2dDimensions.numface), disposable2DMeshGeometry.FaceX, disposable2DMeshGeometry.FaceX.Length);
            Array.Copy(mesh2d.facey.CreateValueArray<double>(mesh2dDimensions.numface), disposable2DMeshGeometry.FaceY, disposable2DMeshGeometry.FaceY.Length);

            return disposable2DMeshGeometry;
        }

        /// <inheritdoc/>
        public int WriteMesh2D(Disposable2DMeshGeometry mesh)
        {
            int meshId = 0;
            int networkId = -1;

            var geometry = mesh.CreateMeshGeometry();
            var geometryDimensions = mesh.CreateMeshDimensions();

            var meshName = mesh.Name?.Replace(' ', spaceReplacementCharacter) ?? "Mesh2d";
            var networkName = "network";

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_meshgeom_dll),
                () => IoNetCfdImports.ionc_put_meshgeom_dll(ref dataSetId, ref meshId, ref networkId, ref geometry, ref geometryDimensions, meshName, networkName, ref startIndex));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_end_define_mode_dll),
                () => IoNetCfdImports.ionc_end_define_mode_dll(ref dataSetId));

            return meshId;
        }

        /// <inheritdoc/>
        public int GetLinksId()
        {
            var contactsId = 0;
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_contact_id_dll(ref dataSetId, ref contactsId));

            return contactsId;
        }

        /// <inheritdoc/>
        public DisposableLinksGeometry GetLinks(int linksId)
        {
            var linkDimensions = new LinksGeometryDimensions();

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_contacts_count_dll),
                () => IoNetCfdImports.ionc_get_contacts_count_dll(ref dataSetId, ref linksId, ref linkDimensions.NumberOfLinks));

            var disposableLinksGeometry = new DisposableLinksGeometry();

            disposableLinksGeometry.InitializeWithEmptyData(linkDimensions);

            var linkGeometry = disposableLinksGeometry.CreateLinksGeometry();

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_get_mesh_contact_v1_dll),
                () => IoNetCfdImports.ionc_get_mesh_contact_v1_dll(ref dataSetId, ref linksId, ref linkGeometry.Mesh1DFrom,
                    ref linkGeometry.Mesh2DTo, ref linkGeometry.LinkType, ref linkGeometry.LinkId,
                    ref linkGeometry.LinkLongName, ref linkDimensions.NumberOfLinks, ref startIndex));

            var type = typeof(DisposableLinksGeometry);

            Array.Copy(linkGeometry.Mesh2DTo.CreateValueArray<int>(linkDimensions.NumberOfLinks), disposableLinksGeometry.Mesh2DTo, disposableLinksGeometry.Mesh2DTo.Length);
            Array.Copy(linkGeometry.Mesh1DFrom.CreateValueArray<int>(linkDimensions.NumberOfLinks), disposableLinksGeometry.Mesh1DFrom, disposableLinksGeometry.Mesh1DFrom.Length);
            Array.Copy(linkGeometry.LinkType.CreateValueArray<int>(linkDimensions.NumberOfLinks), disposableLinksGeometry.LinkType, disposableLinksGeometry.LinkType.Length);
            Array.Copy(linkGeometry.LinkId.CreateValueArray<string>(linkDimensions.NumberOfLinks, type.GetBufferSize(nameof(DisposableLinksGeometry.LinkId))), disposableLinksGeometry.LinkId, disposableLinksGeometry.LinkId.Length);
            Array.Copy(linkGeometry.LinkLongName.CreateValueArray<string>(linkDimensions.NumberOfLinks, type.GetBufferSize(nameof(DisposableLinksGeometry.LinkLongName))), disposableLinksGeometry.LinkLongName, disposableLinksGeometry.LinkLongName.Length);

            return disposableLinksGeometry;
        }

        /// <inheritdoc/>
        public int WriteLinks(DisposableLinksGeometry links)
        {
            var geometry = links.CreateLinksGeometry();
            var geometryDimensions = links.CreateLinksDimensions();

            var contactId = 0;
            var contactName = "links";
            var firstMesh1dId = GetMeshIdsByMeshType(UGridMeshType.Mesh1D).FirstOrDefault();
            var firstMesh2dId = GetMeshIdsByMeshType(UGridMeshType.Mesh2D).FirstOrDefault();
            var location1D = (int) GridLocationType.Node;
            var location2D = (int) GridLocationType.Face;

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_def_mesh_contact_dll),
                () => IoNetCfdImports.ionc_def_mesh_contact_dll(ref dataSetId, ref contactId, contactName,
                    ref geometryDimensions.NumberOfLinks, ref firstMesh1dId, ref firstMesh2dId, ref location1D,
                    ref location2D));

            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_put_mesh_contact_v1_dll),
                () => IoNetCfdImports.ionc_put_mesh_contact_v1_dll(ref dataSetId, ref contactId, ref geometry.Mesh1DFrom,
                    ref geometry.Mesh2DTo, ref geometry.LinkType, ref geometry.LinkId, ref geometry.LinkLongName,
                    ref geometryDimensions.NumberOfLinks, ref startIndex));
            
            DoIoNetCfdCall(nameof(IoNetCfdImports.ionc_end_define_mode_dll),
                () => IoNetCfdImports.ionc_end_define_mode_dll(ref dataSetId));

            return contactId;
        }

        /// <inheritdoc/>
        public void Dispose()
        {
            ReleaseUnmanagedResources();
            GC.SuppressFinalize(this);
        }

        private static T[] GetArrayFromIoNetCdf<T>(Func<int> getSizeFunction, Action<IntPtr, int> setArrayFunction)
        {
            int count = getSizeFunction();

            var handle = GCHandle.Alloc(new T[count], GCHandleType.Pinned);

            try
            {
                var pointer = handle.AddrOfPinnedObject();

                setArrayFunction(pointer, count);

                return pointer.CreateValueArray<T>(count);
            }
            finally
            {
                handle.Free();
            }
        }

        private static void SetArraysToIoNetCdf<T>(ICollection<T[]> values, Action<IList<IntPtr>, IList<int>> setArraysFunction)
        {
            var handles = values.Select(v => GCHandle.Alloc(v, GCHandleType.Pinned)).ToArray();

            try
            {
                var pointers = handles.Select(h => h.AddrOfPinnedObject()).ToArray();
                var lengths = values.Select(v => v.Length).ToArray();
                setArraysFunction(pointers, lengths);
            }   
            finally
            {
                foreach (var handle in handles)
                {
                    handle.Free();
                }
            }
        }


        private static void SetArrayToIoNetCdf<T>(T[] values, Action<IntPtr, int> setArrayFunction)
        {
            SetArraysToIoNetCdf(new List<T[]>{values}, (p, l)=> setArrayFunction(p[0], l[0]));
        }

        private NetcdfOpenMode GetNetcdfOpenMode(OpenMode mode)
        {
            switch (mode)
            {
                case OpenMode.Reading:
                    return NetcdfOpenMode.nf90_nowrite;
                case OpenMode.Appending:
                    return NetcdfOpenMode.nf90_write;
                default:
                    throw new ArgumentOutOfRangeException(nameof(mode), mode, null);
            }
        }

        private string GetNetworkNameById(int networkId)
        {
            var bufferSize = typeof(DisposableNetworkGeometry).GetBufferSize(nameof(DisposableNetworkGeometry.NetworkName));
            
            var stringBuilder = new StringBuilder(bufferSize);
            DoIoNetCfdCall(() => IoNetCfdImports.ionc_get_network_name_dll(ref dataSetId, ref networkId, stringBuilder));

            return stringBuilder.ToString();
        }

        private int GetLocationCount(GridLocationType locationType, int meshId)
        {
            var locationCount = 0;
            switch (locationType)
            {
                case GridLocationType.Node:
                    DoIoNetCfdCall(() =>
                        IoNetCfdImports.ionc_get_node_count_dll(ref dataSetId, ref meshId, ref locationCount));
                    break;
                case GridLocationType.Edge:
                    DoIoNetCfdCall(() =>
                        IoNetCfdImports.ionc_get_edge_count_dll(ref dataSetId, ref meshId, ref locationCount));
                    break;
                case GridLocationType.Face:
                case GridLocationType.Volume:
                    DoIoNetCfdCall(() =>
                        IoNetCfdImports.ionc_get_face_count_dll(ref dataSetId, ref meshId, ref locationCount));
                    break;
                case GridLocationType.All2D:
                case GridLocationType.None:
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(locationType), locationType, null);
            }

            return locationCount;
        }

        /// <summary>
        /// Gets the variable id for the provided <paramref name="variableName"/> en <paramref name="meshId"/>
        /// </summary>
        /// <param name="variableName">Name of the variable</param>
        /// <param name="meshId">Id of the mesh</param>
        /// <returns>Variable id (-1 if the variable could not be found)</returns>
        private int GetVariableId(string variableName, int meshId)
        {
            try
            {
                var variableId = -1;
                DoIoNetCfdCall(() => IoNetCfdImports.ionc_inq_varid_dll(ref dataSetId, ref meshId, variableName, ref variableId));
                return variableId;
            }
            catch (IoNetCdfNativeError nativeError) when(nativeError.ErrorCode == -1015)
            {
                // Variable could not be found
                return -1;
            }
        }

        private static bool TryIoNetCfdCall(string ioNetCdfFunctionName, Func<int> ioNetCdfCall, [CallerMemberName] string cSharpFunctionName = null)
        {
            try
            {
                DoIoNetCfdCall(ioNetCdfFunctionName, ioNetCdfCall, cSharpFunctionName);
                return true;
            }
            catch (IoNetCdfNativeError netCdfNativeError) when (netCdfNativeError.ErrorCode == IoNetCfdImports.VariableNotFoundErrorCode)
            {
                // The optional variable could not be found
                return false;
            }
        }

        private static void DoIoNetCfdCall(string ioNetCdfFunctionName, Func<int> ioNetCdfCall, [CallerMemberName] string cSharpFunctionName = null)
        {
            var errorCode = ioNetCdfCall();
            if (errorCode != IoNetCfdImports.NoErrorCode)
            {
                string errorMessage = Marshal.PtrToStringAnsi(IoNetCfdImports.ionc_strerror_dll(ref errorCode));
                throw new IoNetCdfNativeError(errorCode, errorMessage, ioNetCdfFunctionName, cSharpFunctionName);
            }
        }

        private static void DoIoNetCfdCall(Expression<Func<int>> ioNetCdfCall, [CallerMemberName] string cSharpFunctionName = null)
        {
            var methodName = ioNetCdfCall.Body is MethodCallExpression methodCall
                ? methodCall.Method.Name
                : "";

            DoIoNetCfdCall(methodName, ioNetCdfCall.Compile(), cSharpFunctionName);
        }

        private void ReleaseUnmanagedResources()
        {
            Close();
        }
    }
};