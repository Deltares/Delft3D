using System;
using System.IO;
using System.Runtime.InteropServices;
using General.tests;
using NUnit.Framework;

namespace UGrid.tests
{
    public class PutAndGet
    {
        //Constructor loads the library
        static PutAndGet()
        {
            TestHelper.SetSharedPath(IoNetcdfLibWrapper.LibDetails.LIB_DEP);
            string filename = TestHelper.GetLibraryPath(IoNetcdfLibWrapper.LibDetails.LIB_NAME);
            m_libnetcdf = TestHelper.LoadLibrary(filename);
            //we should chek the pointer is not null
            Assert.That(m_libnetcdf, Is.Not.Null);
            filename = TestHelper.GetLibraryPath(GridGeomLibWrapper.LibDetails.LIB_NAME);
            m_libgridgeom = TestHelper.LoadLibrary(filename);
            Assert.That(m_libgridgeom, Is.Not.Null);
        }

        //pointer to the loaded dll
        public static IntPtr m_libnetcdf;
        public static IntPtr m_libgridgeom;


        //network name
        private string networkName = "network1d";

        //dimension info
        private int numNetworkNodes = 4;
        private int numNetworkBranches = 3;
        private int numGeometryPoints = 9;
        private int startIndex = 1;

        //node info
        private double[] networkNodesX = { 1.0, 5.0, 5.0, 8.0 };
        private double[] networkNodesY = { 4.0, 4.0, 1.0, 4.0 };
        private string[] networkNodesIds = { "node1", "node2", "node3", "node4" };
        private string[] networkNodesLongNames = { "nodelong1", "nodelong2", "nodelong3", "nodelong4" };
        private int[] networkSourceNodes = { 1, 2, 2 };
        private int[] networkTargetNodes = { 2, 3, 4 };
        private int[] networkEdgeNodes = { 1, 2, 2, 3, 2, 4 };

        //branches info
        private double[] networkBranchLengths = { 4.0, 3.0, 3.0 };
        private int[] networkGeometryPointsInBranches = { 3, 3, 3 };
        private string[] networkBranchIds = { "branch1", "branch2", "branch3" };
        private string[] networkBranchLongNames = { "branchlong1", "branchlong2", "branchlong3" };
        private int[] networkBranchOrder = { -1, -1, -1 };

        //geometry info
        double[] networkGeometryPointsX = { 1.0, 3.0, 5.0, 5.0, 5.0, 5.0, 5.0, 7.0, 8.0 };
        double[] networkGeometryPointsY = { 4.0, 4.0, 4.0, 4.0, 2.0, 1.0, 4.0, 4.0, 4.0 };

        //mesh name
        private string meshName = "1dmesh";

        //mesh dimension
        private int nmeshpoints = 8;
        private int nedgenodes = 7; // nmeshpoints - 1 

        //mesh geometry
        private int nmesh1dPoints = 8;
        private int[] branchidx = { 1, 1, 1, 1, 2, 2, 3, 3 };
        private double[] offset = { 0.0, 2.0, 3.0, 4.0, 1.5, 3.0, 1.5, 3.0 };
        private int[] edge_nodes = { 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7 };
        private double[] mesh1dCoordX = { 1, 1, 1, 1, 2, 2, 3, 3 };
        private double[] mesh1dCoordY = { 1, 1, 1, 1, 2, 2, 3, 3 };

        private string[] meshnodeids = { "node1_branch1", "node2_branch1", "node3_branch1", "node4_branch1", "node1_branch2",
                                         "node2_branch2", "node3_branch2", "node1_branch3", "node2_branch3", "node3_branch3" };

        private string[] meshnodelongnames = { "node1_branch1_long_name", "node2_branch1_long_name", "node3_branch1_long_name", "node4_branch1_long_name", "node1_branch2_long_name",
                                               "node2_branch2_long_name", "node3_branch2_long_name", "node1_branch3_long_name", "node2_branch3_long_name", "node3_branch3_long_name" };

        //netcdf file specifications 
        private int iconvtype = 2;

        private double convversion;

        //mesh links
        private string linkmeshname = "links";

        private int nlinks = 3;
        private int linkmesh1 = 1;
        private int linkmesh2 = 2;
        private int locationType1 = 1;
        private int locationType2 = 1;
        private int[] mesh1indexes = { 1, 2, 3 };
        private int[] mesh2indexes = { 1, 2, 3 };
        private string[] linksids = { "link1", "link2", "link3" };
        private string[] linkslongnames = { "linklong1", "linklong2", "linklong3" };
        private int[] contacttype = { 3, 3, 3 };

        // mesh2d
        private int numberOf2DNodes = 5;
        private int numberOfFaces = 2;
        private int numberOfMaxFaceNodes = 4;
        private double[] mesh2d_nodesX = { 0, 10, 15, 10, 5 };
        private double[] mesh2d_nodesY = { 0, 0, 5, 10, 5 };
        private double[,] mesh2d_face_nodes = { { 1, 2, 5, -999 }, { 2, 3, 4, 5 } };



        private void addglobalattributes(int ioncid, ref IoNetcdfLibWrapper wrapper)
        {
            string tmpstring;
            IoNetcdfLibWrapper.interop_metadata metadata;
            tmpstring = "Deltares";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.institution = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.source = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.references = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.version = tmpstring.ToCharArray();
            tmpstring = "Unknown";
            tmpstring = tmpstring.PadRight(IoNetcdfLibWrapper.metadatasize, ' ');
            metadata.modelname = tmpstring.ToCharArray();
            int ierr = wrapper.ionc_add_global_attributes(ref ioncid, ref metadata);
            Assert.That(ierr, Is.EqualTo(0));
        }



        // Check 1d network using meshgeom
        private void check1DNetwork(ref meshgeomdim networkdim, ref meshgeom network)
        {
            var l_networkNodesX = new double[networkNodesX.Length];
            var l_networkNodesY = new double[networkNodesY.Length];
            var l_networkEdgeNode = new int[networkEdgeNodes.Length];

            var l_networkBranchLengths = new double[networkBranchLengths.Length];
            var l_networkGeometryPointsInBranches = new int[networkGeometryPointsInBranches.Length];
            var l_networkBranchOrder = new int[networkBranchOrder.Length];

            var l_networkGeometryPointsX = new double[networkGeometryPointsX.Length];
            var l_networkGeometryPointsY = new double[networkGeometryPointsY.Length];


            Marshal.Copy(network.nnodex, l_networkNodesX, 0, l_networkNodesX.Length);
            Marshal.Copy(network.nnodey, l_networkNodesY, 0, l_networkNodesY.Length);
            Marshal.Copy(network.nedge_nodes, l_networkEdgeNode, 0, l_networkEdgeNode.Length);

            Marshal.Copy(network.nbranchlengths, l_networkBranchLengths, 0, l_networkBranchLengths.Length);
            Marshal.Copy(network.nbranchgeometrynodes, l_networkGeometryPointsInBranches, 0, l_networkGeometryPointsInBranches.Length);
            Marshal.Copy(network.nbranchorder, l_networkBranchOrder, 0, l_networkBranchOrder.Length);
            Marshal.Copy(network.ngeopointx, l_networkGeometryPointsX, 0, l_networkGeometryPointsX.Length);
            Marshal.Copy(network.ngeopointy, l_networkGeometryPointsY, 0, l_networkGeometryPointsY.Length);


            var l_networkNodesIds = StringBufferHandling.ParseString(network.nnodeids, networkdim.nnodes, IoNetcdfLibWrapper.idssize);
            var l_networkNodesLongNames = StringBufferHandling.ParseString(network.nnodelongnames, networkdim.nnodes, IoNetcdfLibWrapper.longnamessize);
            var l_branchids = StringBufferHandling.ParseString(network.nbranchids, networkdim.nbranches, IoNetcdfLibWrapper.idssize);
            var l_branchlongNames = StringBufferHandling.ParseString(network.nbranchlongnames, networkdim.nbranches, IoNetcdfLibWrapper.longnamessize);


            // checks
            for (int i = 0; i < networkdim.nnodes; i++)
            {
                Assert.That(l_networkNodesIds[i], Is.EqualTo(networkNodesIds[i].PadRight(IoNetcdfLibWrapper.idssize)));
                Assert.That(l_networkNodesLongNames[i], Is.EqualTo(networkNodesLongNames[i].PadRight(IoNetcdfLibWrapper.longnamessize)));
                Assert.That(l_networkNodesX[i], Is.EqualTo(networkNodesX[i]));
            }

            for (int i = 0; i < networkdim.nbranches; i++)
            {
                Assert.That(l_branchids[i], Is.EqualTo(networkBranchIds[i].PadRight(IoNetcdfLibWrapper.idssize)));
                Assert.That(l_branchlongNames[i], Is.EqualTo(networkBranchLongNames[i].PadRight(IoNetcdfLibWrapper.longnamessize)));
                Assert.That(l_networkBranchLengths[i], Is.EqualTo(networkBranchLengths[i]));
                Assert.That(l_networkGeometryPointsInBranches[i], Is.EqualTo(networkGeometryPointsInBranches[i]));
                Assert.That(l_networkBranchOrder[i], Is.EqualTo(networkBranchOrder[i]));
            }

            for (int i = 0; i < networkdim.ngeometry; i++)
            {
                Assert.That(l_networkGeometryPointsX[i], Is.EqualTo(networkGeometryPointsX[i]));
                Assert.That(l_networkGeometryPointsY[i], Is.EqualTo(networkGeometryPointsY[i]));
            }


            int edgeIndex = 0;
            for (int i = 0; i < networkdim.nbranches; i++)
            {
                //written 1 based, but retrived 0 based
                Assert.That(l_networkEdgeNode[edgeIndex] + 1, Is.EqualTo(networkEdgeNodes[edgeIndex]));
                edgeIndex++;
                Assert.That(l_networkEdgeNode[edgeIndex] + 1, Is.EqualTo(networkEdgeNodes[edgeIndex]));
                edgeIndex++;
            }

        }
        
        private void check1DMesh(ref meshgeomdim meshgeomdim, ref meshgeom meshgeom)
        {
            var l_branchidx = new int[branchidx.Length];
            var l_offset = new double[offset.Length];
            var l_edge_nodes = new int[edge_nodes.Length];

            var l_mesh1dCoordX = new double[mesh1dCoordX.Length];
            var l_mesh1dCoordY = new double[mesh1dCoordY.Length];

            Marshal.Copy(meshgeom.branchidx, l_branchidx, 0, l_branchidx.Length);
            Marshal.Copy(meshgeom.branchoffsets, l_offset, 0, l_offset.Length);
            Marshal.Copy(meshgeom.edge_nodes, l_edge_nodes, 0, l_edge_nodes.Length);
            Marshal.Copy(meshgeom.nodex, l_mesh1dCoordX, 0, l_mesh1dCoordX.Length);
            Marshal.Copy(meshgeom.nodey, l_mesh1dCoordY, 0, l_mesh1dCoordY.Length);

            var l_meshnodeids = StringBufferHandling.ParseString(meshgeom.nodeids, meshgeomdim.numnode, IoNetcdfLibWrapper.idssize);
            var l_meshnodelongnames = StringBufferHandling.ParseString(meshgeom.nodelongnames, meshgeomdim.numnode, IoNetcdfLibWrapper.longnamessize);

            // checks
            for (int i = 0; i < meshgeomdim.numnode; i++)
            {
                Assert.That(l_meshnodeids[i], Is.EqualTo(meshnodeids[i].PadRight(IoNetcdfLibWrapper.idssize)));
                Assert.That(l_meshnodelongnames[i], Is.EqualTo(meshnodelongnames[i].PadRight(IoNetcdfLibWrapper.longnamessize)));
                Assert.That(l_offset[i], Is.EqualTo(offset[i]));
                Assert.That(l_branchidx[i], Is.EqualTo(branchidx[i]));
            }
            
            int edgeIndex = 0;
            for (int i = 0; i < meshgeomdim.nbranches; i++)
            {
                //written 1 based, but retrived 0 based
                Assert.That(l_edge_nodes[edgeIndex], Is.EqualTo(edge_nodes[edgeIndex]));
                edgeIndex++;
                Assert.That(l_edge_nodes[edgeIndex], Is.EqualTo(edge_nodes[edgeIndex]));
                edgeIndex++;
            }

        }


        //To be completed
        private void checkLinks(ref meshgeomdim meshgeomdim, ref meshgeom meshgeom)
        {
        }

        // Create a 1d mesh using ionc_put_meshgeom: NOTE now meshgeom includes mesh and network array, this is not good.
        // To be refactored later
        [Test]
        [Category("PutAndGetMeshGeom")]
        public void Put1dMeshAndNetworkUsingPutMeshGeom()
        {
            using (var register = new UnmanagedMemoryRegister())
            {
                // Create a netcdf file 
                int ioncid = 0; // file variable 
                int mode = 1;   // create in write mode
                string c_path = TestHelper.TestDirectoryPath() + @"\Written1DMeshAndNetwork.nc";
                TestHelper.DeleteIfExists(c_path);
                Assert.IsFalse(File.Exists(c_path));
                var wrapper = new IoNetcdfLibWrapper();

                // Make a local copy of the variables 
                double[] l_nodesX = networkNodesX;
                double[] l_nodesY = networkNodesY;
                var l_edge_nodes = new int[networkSourceNodes.Length + networkTargetNodes.Length];
                networkSourceNodes.CopyTo(l_edge_nodes, 0);
                networkTargetNodes.CopyTo(l_edge_nodes, networkSourceNodes.Length);
                double[] l_branchlengths = networkBranchLengths;
                double[] l_geopointsX = networkGeometryPointsX;
                double[] l_geopointsY = networkGeometryPointsY;
                int[] l_branch_order = networkBranchOrder;
                int[] l_nbranchgeometrypoints = networkGeometryPointsInBranches;

                // Write a 1d network using ionc_put_meshgeom
                var networkdim = new meshgeomdim();
                networkdim.dim = 1;
                networkdim.nnodes = numNetworkNodes;
                networkdim.nbranches = numNetworkBranches;
                networkdim.ngeometry = numGeometryPoints;
                networkdim.numlinks = 0;
                networkdim.numface = 0;

                var network = new meshgeom();
                register.Add(ref l_nodesX, ref network.nnodex);
                register.Add(ref l_nodesY, ref network.nnodey);
                register.Add(ref l_edge_nodes, ref network.nedge_nodes);
                register.Add(ref l_branchlengths, ref network.nbranchlengths);
                register.Add(ref l_geopointsX, ref network.ngeopointx);
                register.Add(ref l_geopointsY, ref network.ngeopointy);
                register.Add(ref l_branch_order, ref network.nbranchorder);
                register.Add(ref l_nbranchgeometrypoints, ref network.nbranchgeometrynodes);


                // Strings are passed by pointers to avoid stack overflow
                var networkNodeidsBuffer = StringBufferHandling.MakeStringBuffer(ref networkNodesIds, IoNetcdfLibWrapper.idssize);
                var networkNodelongnamesBuffer = StringBufferHandling.MakeStringBuffer(ref networkNodesLongNames, IoNetcdfLibWrapper.longnamessize);
                var networkBranchesidsBuffer = StringBufferHandling.MakeStringBuffer(ref networkBranchIds, IoNetcdfLibWrapper.idssize);
                var networkBrancheslongnamesBuffer = StringBufferHandling.MakeStringBuffer(ref networkBranchLongNames, IoNetcdfLibWrapper.longnamessize);
                register.Add(ref networkNodeidsBuffer, ref network.nnodeids);
                register.Add(ref networkNodelongnamesBuffer, ref network.nnodelongnames);
                register.Add(ref networkBranchesidsBuffer, ref network.nbranchids);
                register.Add(ref networkBrancheslongnamesBuffer, ref network.nbranchlongnames);
                networkName = networkName.PadRight(IoNetcdfLibWrapper.namesize, ' ');
                networkdim.name = networkName.ToCharArray();

                // Create the file, will not add any dataset 
                var ierr = wrapper.ionc_create(c_path, ref mode, ref ioncid);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.IsTrue(File.Exists(c_path));

                // For reading the grid later on we need to add metadata to the netcdf file. 
                // The function ionc_add_global_attributes adds to the netCDF file the UGRID convention
                addglobalattributes(ioncid, ref wrapper);

                // Put 1d network
                int networkid = 0;
                ierr = wrapper.ionc_put_network(ref ioncid, ref networkid, ref network, ref networkdim);
                Assert.That(ierr, Is.EqualTo(0));

                // Write a 1d mesh using ionc_put_meshgeom
                var meshdim = new meshgeomdim();
                meshdim.dim = 1;
                meshdim.numnode = nmeshpoints;
                meshdim.numedge = nedgenodes;
                meshdim.nbranches = numNetworkBranches;
                meshdim.numedge = 6;
                meshdim.numlinks = 0;
                meshdim.numface = 0;

                var mesh = new meshgeom();

                // The arrays we provide in this test are 1 based, we need to tell to the put call
                mesh.startIndex = startIndex;

                // Copy arrays to unmanaged memory ad assign pointers
                register.Add(ref offset, ref mesh.branchoffsets);
                register.Add(ref branchidx, ref mesh.branchidx);
                register.Add(ref mesh1dCoordX, ref mesh.nodex);
                register.Add(ref mesh1dCoordY, ref mesh.nodey);
                register.Add(ref edge_nodes, ref mesh.edge_nodes);

                // Strings are passed by pointers to avoid overflow for large grids
                var nodeidsBuffer = StringBufferHandling.MakeStringBuffer(ref meshnodeids, IoNetcdfLibWrapper.idssize);
                var nodelongnamesBuffer = StringBufferHandling.MakeStringBuffer(ref meshnodelongnames, IoNetcdfLibWrapper.longnamessize);
                register.Add(ref nodeidsBuffer, ref mesh.nodeids);
                register.Add(ref nodelongnamesBuffer, ref mesh.nodelongnames);
                meshName = meshName.PadRight(IoNetcdfLibWrapper.namesize, ' ');
                meshdim.name = meshName.ToCharArray();

                int meshid = 0;
                ierr = wrapper.ionc_put_meshgeom(ref ioncid, ref meshid, ref networkid, ref mesh, ref meshdim);
                Assert.That(ierr, Is.EqualTo(0));
                ierr = wrapper.ionc_close(ref ioncid);
                Assert.That(ierr, Is.EqualTo(0));

            }
        }

        // Create a 2d mesh using ionc_put_meshgeom: NOTE now meshgeom includes mesh and network array, this is not good (To be refactored later)
        [Test]
        [Category("PutAndGetMeshGeom")]
        public void Put2dMeshUsingPutMeshGeom()
        {
            // Open a netcdf file
            string c_path = TestHelper.TestFilesDirectoryPath() + @"\Custom_Ugrid.nc";
            Assert.IsTrue(File.Exists(c_path));
            int ioncid = -1;
            int mode = 0; //read mode
            var wrapper = new IoNetcdfLibWrapper();
            var ierr = wrapper.ionc_open(c_path, ref mode, ref ioncid, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            // Get the mesh dimensions in meshdim
            int existingMeshId = 1;
            int existingNetworkId = -1;
            var meshdim = new meshgeomdim();
            ierr = wrapper.ionc_get_meshgeom_dim(ref ioncid, ref existingMeshId, ref existingNetworkId, ref meshdim);
            Assert.That(ierr, Is.EqualTo(0));

            // Allocate mesh
            var mesh = new meshgeom();
            var register = new UnmanagedMemoryRegister();
            register.Add(ref meshdim, ref mesh);

            // Get 0 based
            mesh.startIndex = 0;
            ierr = wrapper.ionc_get_meshgeom(ref ioncid, ref existingMeshId, ref existingNetworkId, ref mesh);
            Assert.That(ierr, Is.EqualTo(0));

            // Close the file
            ierr = wrapper.ionc_close(ref ioncid);
            Assert.That(ierr, Is.EqualTo(0));

            // Create a new netcdf file
            int targetioncid = -1; //file id  
            int targetmode = 1;    //create in write mode
            string target_path = TestHelper.TestDirectoryPath() + @"\Written2DMesh.nc";
            TestHelper.DeleteIfExists(target_path);
            Assert.IsFalse(File.Exists(target_path));
            ierr = wrapper.ionc_create(target_path, ref targetmode, ref targetioncid);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.IsTrue(File.Exists(target_path));

            // Write a 2d mesh using ionc_put_meshgeom
            string meshname = "my_mesh";
            meshname = meshname.PadRight(IoNetcdfLibWrapper.namesize, ' ');
            meshdim.name = meshname.ToCharArray();
            int meshid = -1;
            int networkid = -1;

            ierr = wrapper.ionc_put_meshgeom(ref targetioncid, ref meshid, ref networkid, ref mesh, ref meshdim);
            Assert.That(ierr, Is.EqualTo(0));

            // Close the file
            ierr = wrapper.ionc_close(ref targetioncid);
            Assert.That(ierr, Is.EqualTo(0));

            // Clean memory
            register.Dispose();
        }

        // Get only 1d network using ionc_get_meshgeom
        [Test]
        [Category("PutAndGetMeshGeom")]
        public void Get1dNetworkUsingGetMeshGeom()
        {
            using (var register = new UnmanagedMemoryRegister())
            {
                // Open a netcdf file
                string c_path = TestHelper.TestFilesDirectoryPath() + @"\write1dNetwork.nc";
                Assert.IsTrue(File.Exists(c_path));
                int ioncid = 0; //file variable 
                int mode = 0; //create in read mode
                var wrapper = new IoNetcdfLibWrapper();
                var ierr = wrapper.ionc_open(c_path, ref mode, ref ioncid, ref iconvtype, ref convversion);
                Assert.That(ierr, Is.EqualTo(0));

                // Get 1d mesh dimensions
                var networkdim = new meshgeomdim();
                int nnumNetworks = -1;
                ierr = wrapper.ionc_get_number_of_networks(ref ioncid, ref nnumNetworks);
                Assert.That(ierr, Is.EqualTo(0));
                Assert.That(nnumNetworks, Is.EqualTo(1));
                IntPtr c_networkids = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * nnumNetworks);

                // Get a valid networkid
                ierr = wrapper.ionc_get_network_ids(ref ioncid, ref c_networkids, ref nnumNetworks);
                Assert.That(ierr, Is.EqualTo(0));
                int[] l_networkid = new int[nnumNetworks];
                Marshal.Copy(c_networkids, l_networkid, 0, nnumNetworks);

                // Get network dimensions using meshgeom
                int networkid = -1; //set an invalid index
                ierr = wrapper.ionc_get_meshgeom_dim(ref ioncid, ref networkid, ref l_networkid[0], ref networkdim);
                Assert.That(ierr, Is.EqualTo(0));

                // Register unmanaged memory and pointers 
                var network = new meshgeom();
                register.Add(ref networkdim, ref network);

                // Client wants 0 based arrays
                network.startIndex = 0;
                ierr = wrapper.ionc_get_meshgeom(ref ioncid, ref networkid, ref l_networkid[0], ref network);
                Assert.That(ierr, Is.EqualTo(0));

                // Reconstruct the arrays
                int l_nnodes = 2;
                int l_nbranches = 1;
                int l_nGeometry = 25;
                double[] l_nodesX = new double[networkdim.nnodes];
                double[] l_nodesY = new double[networkdim.nnodes];
                int[] l_nedge_nodes = new int[networkdim.nbranches * 2];
                double[] l_nbranchlengths = new double[networkdim.nbranches];
                double[] l_ngeopointx = new double[networkdim.ngeometry];
                double[] l_ngeopointy = new double[networkdim.ngeometry];

                Marshal.Copy(network.nnodex, l_nodesX, 0, networkdim.nnodes);
                Marshal.Copy(network.nnodey, l_nodesY, 0, networkdim.nnodes);
                Marshal.Copy(network.nedge_nodes, l_nedge_nodes, 0, networkdim.nbranches * 2);
                Marshal.Copy(network.nbranchlengths, l_nbranchlengths, 0, networkdim.nbranches);
                Marshal.Copy(network.ngeopointx, l_ngeopointx, 0, networkdim.ngeometry);
                Marshal.Copy(network.ngeopointy, l_ngeopointy, 0, networkdim.ngeometry);
                var l_networkNodeIds = StringBufferHandling.ParseString(network.nnodeids, networkdim.nnodes, IoNetcdfLibWrapper.idssize);
                var l_networkLongNames = StringBufferHandling.ParseString(network.nnodelongnames, networkdim.nnodes, IoNetcdfLibWrapper.longnamessize);
                var l_branchids = StringBufferHandling.ParseString(network.nbranchids, networkdim.nbranches, IoNetcdfLibWrapper.idssize);
                var l_branchlongnames = StringBufferHandling.ParseString(network.nbranchlongnames, networkdim.nbranches, IoNetcdfLibWrapper.longnamessize);

                for (int i = 0; i < networkdim.nnodes; i++)
                {
                    Assert.That(l_networkNodeIds[i], Is.EqualTo(networkNodesIds[i].PadRight(IoNetcdfLibWrapper.idssize)));
                    Assert.That(l_networkLongNames[i], Is.EqualTo(networkNodesLongNames[i].PadRight(IoNetcdfLibWrapper.longnamessize)));
                }

                for (int i = 0; i < networkdim.nbranches; i++)
                {
                    Assert.That(l_branchids[i], Is.EqualTo(networkBranchIds[i].PadRight(IoNetcdfLibWrapper.idssize)));
                    Assert.That(l_branchlongnames[i], Is.EqualTo(networkBranchLongNames[i].PadRight(IoNetcdfLibWrapper.longnamessize)));
                }

                // Close the file
                ierr = wrapper.ionc_close(ref ioncid);
                Assert.That(ierr, Is.EqualTo(0));

                // Check the network values
                check1DNetwork(ref networkdim, ref network);
            }

        }

        // Get only 1d MESH using ionc_get_meshgeom
        [Test]
        [Category("PutAndGetMeshGeom")]
        public void Get1dMeshUsingGetMeshGeom()
        {
            using (var register = new UnmanagedMemoryRegister())
            {
                // Open a netcdf file
                string c_path = TestHelper.TestFilesDirectoryPath() + @"\write1d.nc";
                Assert.IsTrue(File.Exists(c_path));
                int ioncid = 0; //file variable 
                int mode = 0; //create in read mode
                var wrapper = new IoNetcdfLibWrapper();
                var ierr = wrapper.ionc_open(c_path, ref mode, ref ioncid, ref iconvtype, ref convversion);
                Assert.That(ierr, Is.EqualTo(0));

                // Get network dimensions using meshgeom
                int meshid = 1; //set an invalid meshid index, here we now is 1
                int networkid = -1; //set an invalid index because we do not want to get the network information
                var meshdim = new meshgeomdim();
                string meshname = "";
                meshname = meshname.PadRight(IoNetcdfLibWrapper.namesize, ' ');
                meshdim.name = meshname.ToCharArray();
                ierr = wrapper.ionc_get_meshgeom_dim(ref ioncid, ref meshid, ref networkid, ref meshdim);
                Assert.That(ierr, Is.EqualTo(0));

                // Register unmanaged memory and pointers 
                var mesh = new meshgeom();
                register.Add(ref meshdim, ref mesh);

                // Here we retrive 1 based arrays
                mesh.startIndex = 1;
                ierr = wrapper.ionc_get_meshgeom(ref ioncid, ref meshid, ref networkid, ref mesh);
                Assert.That(ierr, Is.EqualTo(0));

                // Check the network values
                check1DMesh(ref meshdim, ref mesh);
            }
        }

        //[Test]
        //[Category("PutAndGetMeshGeom")]
        //public void GetFirstMeshUsingMeshGeom()
        //{
        //    // Open a netcdf file
        //    string c_path = TestHelper.TestFilesDirectoryPath() + @"\1_net.nc";
        //    Assert.IsTrue(File.Exists(c_path));
        //    int ioncid = -1;
        //    int mode = 0; //read mode
        //    var wrapper = new IoNetcdfLibWrapper();
        //    var ierr = wrapper.ionc_open(c_path, ref mode, ref ioncid, ref iconvtype, ref convversion);
        //    Assert.That(ierr, Is.EqualTo(0));

        //    // Get the mesh dimensions in meshdim
        //    int existingMeshId = 1;
        //    int existingNetworkId = -1;
        //    var meshdim = new meshgeomdim();
        //    string meshname = "";
        //    meshname = meshname.PadRight(IoNetcdfLibWrapper.namesize, ' ');
        //    meshdim.name = meshname.ToCharArray();
        //    ierr = wrapper.ionc_get_meshgeom_dim(ref ioncid, ref existingMeshId, ref existingNetworkId, ref meshdim);
        //    Assert.That(ierr, Is.EqualTo(0));

        //    // Allocate mesh
        //    var mesh = new meshgeom();
        //    var register = new UnmanagedMemoryRegister();
        //    register.Add(ref meshdim, ref mesh);

        //    // Get 0 based
        //    mesh.startIndex = 0;
        //    ierr = wrapper.ionc_get_meshgeom(ref ioncid, ref existingMeshId, ref existingNetworkId, ref mesh);
        //    Assert.That(ierr, Is.EqualTo(0));

        //    // Close the file
        //    ierr = wrapper.ionc_close(ref ioncid);
        //    Assert.That(ierr, Is.EqualTo(0));

        //    // Write file
        //    int targetioncid = -1; //file id  
        //    int targetmode = 1;    //create in write mode
        //    string target_path = TestHelper.TestDirectoryPath() + @"\1_net_write.nc";
        //    TestHelper.DeleteIfExists(target_path);
        //    Assert.IsFalse(File.Exists(target_path));
        //    ierr = wrapper.ionc_create(target_path, ref targetmode, ref targetioncid);
        //    Assert.That(ierr, Is.EqualTo(0));
        //    Assert.IsTrue(File.Exists(target_path));

        //    // Write a 2d mesh using ionc_put_meshgeom
        //    meshname = meshname.PadRight(IoNetcdfLibWrapper.namesize, ' ');
        //    meshdim.name = meshname.ToCharArray();
        //    int meshid = -1;
        //    int networkid = -1;

        //    ierr = wrapper.ionc_put_meshgeom(ref targetioncid, ref meshid, ref networkid, ref mesh, ref meshdim);
        //    Assert.That(ierr, Is.EqualTo(0));

        //    // Close the file
        //    ierr = wrapper.ionc_close(ref targetioncid);
        //    Assert.That(ierr, Is.EqualTo(0));

        //    // Clean memory
        //    register.Dispose();

        //}


    }
}
