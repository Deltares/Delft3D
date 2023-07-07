using System;
using System.Globalization;
using System.IO;
using NUnit.Framework;
using System.Runtime.InteropServices;
using General.tests;

// The build of this test is disabled by default because it requires NUnit.
// If you decide to build the test make sure to install Nunit in your solution.


namespace ECModuleTests
{
    public class ECModuleTests
    {
       //The Constructor loads the library
        static ECModuleTests()
        {
            TestHelper.SetSharedPath(Ec_ModuleLibWrapper.LibDetails.NETCDF_DEP);
            string filename = TestHelper.GetLibraryPath(Ec_ModuleLibWrapper.LibDetails.LIB_NAME);
            __libecmodule = TestHelper.LoadLibrary(filename);
            Assert.That(__libecmodule, Is.Not.Null);

            //these libraries are needed to load the grid from a netcdf file, only for unit testing
            TestHelper.SetSharedPath(IoNetcdfLibWrapper.LibDetails.LIB_DEP);
            filename = TestHelper.GetLibraryPath(IoNetcdfLibWrapper.LibDetails.LIB_NAME);
            __libionetcdf = TestHelper.LoadLibrary(filename);
            Assert.That(__libionetcdf, Is.Not.Null);

            //load gridgeom (e.g. for finding the cells)
            filename = TestHelper.GetLibraryPath(GridGeomLibWrapper.LibDetails.LIB_NAME);
            _gridgeom_libptr = TestHelper.LoadLibrary(filename);
            //we should chek the pointer is not null
            Assert.That(_gridgeom_libptr, Is.Not.Null);
        }

        //some temp data used in the test

        //pointer to the loaded dll
        public static IntPtr __libecmodule;
        public static IntPtr __libionetcdf;
        public static IntPtr _gridgeom_libptr;

        public void readPoints(string pathIn,ref double[] x, ref double[] y, ref double[] values, ref int nSamples) 
        {
            string[] pathInLines = File.ReadAllLines(pathIn);
            string[] firstLineIn = pathInLines[0].Split(' '.ToString().ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            nSamples = Convert.ToInt32(firstLineIn[0].Trim(' '));
            x = new double[nSamples];
            y = new double[nSamples];
            values = new double[nSamples];

            /// Store the values
            for (int i = 0; i< nSamples; i++)
            {
                string[] val = pathInLines[i + 1].Split(' '.ToString().ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                x[i] = double.Parse(val[0], CultureInfo.InvariantCulture);
                y[i] = double.Parse(val[1], CultureInfo.InvariantCulture);
                values[i] = double.Parse(val[2], CultureInfo.InvariantCulture);
            }
        }

        private void getMeshid(int ioncid, ref int meshid, int meshType, ref IoNetcdfLibWrapper wrapper)
        {
            // get the number meshes 
            int numMeshes = -1;
            int ierr = wrapper.ionc_get_number_of_meshes(ref ioncid, ref meshType, ref numMeshes);
            Assert.That(ierr, Is.EqualTo(0));
            Assert.That(numMeshes, Is.EqualTo(1));
            // get the mesh id
            IntPtr c_meshids = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * numMeshes);
            ierr = wrapper.ionc_ug_get_mesh_ids(ref ioncid, ref meshType, ref c_meshids, ref numMeshes);
            Assert.That(ierr, Is.EqualTo(0));
            int[] rc_meshids = new int[numMeshes];
            Marshal.Copy(c_meshids, rc_meshids, 0, numMeshes);
            meshid = rc_meshids[0];
            Marshal.FreeCoTaskMem(c_meshids);
        }


        // Create the netcdf files
        //TODO: add in/out txt
        [Test]
        [NUnit.Framework.Category("TestTriangleInterpolation")]
        public void TestTriangleInterpolation()
        {
            string pathIn = TestHelper.TestFilesDirectoryPath() + @"\inTestTriangleInterpolation.txt";
            Assert.IsTrue(File.Exists(pathIn));
            string pathOut = TestHelper.TestFilesDirectoryPath() + @"\outTestTriangleInterpolation.txt";
            Assert.IsTrue(File.Exists(pathOut));

            int numSamples = 0;
            var sampleX = new double[numSamples];
            var sampleY = new double[numSamples];
            var sampleValues = new double[numSamples];

            readPoints(pathIn, ref sampleX, ref sampleY, ref sampleValues, ref numSamples);

            int numTargets = 0;
            var targetX = new double[numTargets];
            var targetY = new double[numTargets];
            var targetValues = new double[numTargets];

            readPoints(pathOut, ref targetX, ref targetY, ref targetValues, ref numTargets);

            IntPtr c_sampleX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
            IntPtr c_sampleY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
            IntPtr c_sampleValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
            Marshal.Copy(sampleX, 0, c_sampleX, numSamples);
            Marshal.Copy(sampleY, 0, c_sampleY, numSamples);
            Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

            IntPtr c_targetValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numTargets);
            Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

            int ioncid = 0; //file variable 
            int mode = 0;   //open in read mode
            var wrapperNetcdf = new IoNetcdfLibWrapper();
            int iconvtype = 2;
            double convversion = 0.0;
            string pathNetFile = TestHelper.TestFilesDirectoryPath() + @"\simplebox_net_ugrid.nc";
            Assert.IsTrue(File.Exists(pathIn));
            int ierr = wrapperNetcdf.ionc_open(pathNetFile, ref mode, ref ioncid, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            int meshid = -1;
            int meshType = 2;    //get the 2d
            int l_networkid = 0; // invalid network, not really intrested in getting the networ
            var meshtwoddim = new meshgeomdim();
            getMeshid(ioncid, ref meshid, meshType, ref wrapperNetcdf);
            ierr = wrapperNetcdf.ionc_get_meshgeom_dim(ref ioncid, ref meshid, ref l_networkid, ref meshtwoddim);
            Assert.That(ierr, Is.EqualTo(0));

            //You need to know in advance the number of mesh points
            var meshtwod = new meshgeom();
            meshtwod.nodex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshtwoddim.numnode);
            meshtwod.nodey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshtwoddim.numnode);
            meshtwod.nodez = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshtwoddim.numnode);
            meshtwod.edge_nodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshtwoddim.numedge * 2);

            //var gridGeomWrapper = new GridGeomLibWrapper();
            bool includeArrays = true;
            int startIndex = 1; //the base index of the arrays
            ierr = wrapperNetcdf.ionc_get_meshgeom(ref ioncid, ref meshid, ref l_networkid, ref meshtwod, ref startIndex, ref includeArrays);
            Assert.That(ierr, Is.EqualTo(0));

            // default parameters for averaging
            int jsferic = 0;
            int jsferic3D = 0;
            int locType = 1;

            var wrapper = new Ec_ModuleLibWrapper();
            ierr = wrapper.triang(ref meshtwoddim,
            ref meshtwod,
            ref startIndex,
            ref c_sampleX,
            ref c_sampleY,
            ref c_sampleValues,
            ref numSamples,
            ref c_targetValues,
            ref locType,
            ref jsferic,
            ref jsferic3D); 

            Assert.That(ierr, Is.EqualTo(0));

            //check the interpolation results
            double[] targetValuesResults = new double[numTargets];
            Marshal.Copy(c_targetValues, targetValuesResults, 0, numTargets);

            for (int i = 0; i < numTargets; i++)
            {
                Assert.That(targetValuesResults[i], Is.EqualTo(targetValues[i]).Within(1e-6));
            }

        }

        // Tests triangle interpolation on model
        [Test]
        [NUnit.Framework.Category("TestTriangleInterpolation")]
        public void TestTriangleInterpolationOnModel()
        {
            string pathIn = TestHelper.TestFilesDirectoryPath()  + @"\Manning.xyz";
            Assert.IsTrue(File.Exists(pathIn));
            string pathOut = TestHelper.TestFilesDirectoryPath() + @"\outTestTriangleInterpolationOnModel.txt";
            Assert.IsTrue(File.Exists(pathOut));

            int numSamples = 0;
            var sampleX = new double[numSamples];
            var sampleY = new double[numSamples];
            var sampleValues = new double[numSamples];

            readPoints(pathIn, ref sampleX, ref sampleY, ref sampleValues, ref numSamples);

            int numTargets = 0;
            var targetX = new double[numTargets];
            var targetY = new double[numTargets];
            var targetValues = new double[numTargets];
            readPoints(pathOut, ref targetX, ref targetY, ref targetValues, ref numTargets);

            IntPtr c_sampleX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
            IntPtr c_sampleY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
            IntPtr c_sampleValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
            Marshal.Copy(sampleX, 0, c_sampleX, numSamples);
            Marshal.Copy(sampleY, 0, c_sampleY, numSamples);
            Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

            IntPtr c_targetValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numTargets);
            Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

            int ioncid = 0; //file variable 
            int mode = 0;   //open in read mode
            var wrapperNetcdf = new IoNetcdfLibWrapper();
            int iconvtype = 2;
            double convversion = 0.0;
            string pathNetFile = TestHelper.TestFilesDirectoryPath() + @"\MeshModel.nc";
            Assert.IsTrue(File.Exists(pathIn));
            int ierr = wrapperNetcdf.ionc_open(pathNetFile, ref mode, ref ioncid, ref iconvtype, ref convversion);
            Assert.That(ierr, Is.EqualTo(0));

            int meshid = -1;
            int meshType = 2;    //get the 2d
            int l_networkid = 0; // invalid network, not really intrested in getting the networ
            var meshtwoddim = new meshgeomdim();
            getMeshid(ioncid, ref meshid, meshType, ref wrapperNetcdf);
            ierr = wrapperNetcdf.ionc_get_meshgeom_dim(ref ioncid, ref meshid, ref l_networkid, ref meshtwoddim);
            Assert.That(ierr, Is.EqualTo(0));

            //You need to know in advance the number of mesh points
            var meshtwod = new meshgeom();
            meshtwod.nodex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshtwoddim.numnode);
            meshtwod.nodey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshtwoddim.numnode);
            meshtwod.nodez = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshtwoddim.numnode);
            meshtwod.edge_nodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshtwoddim.numedge * 2);

            //var gridGeomWrapper = new GridGeomLibWrapper();
            bool includeArrays = true;
            int startIndex = 0; //the base index of the arrays
            ierr = wrapperNetcdf.ionc_get_meshgeom(ref ioncid, ref meshid, ref l_networkid, ref meshtwod, ref startIndex, ref includeArrays);
            Assert.That(ierr, Is.EqualTo(0));

            // default parameters for averaging
            int jsferic = 0;
            int jsferic3D = 0;
            int locType = 2;

            var wrapper = new Ec_ModuleLibWrapper();
            ierr = wrapper.triang(ref meshtwoddim,
            ref meshtwod,
            ref startIndex,
            ref c_sampleX,
            ref c_sampleY,
            ref c_sampleValues,
            ref numSamples,
            ref c_targetValues,
            ref locType,
            ref jsferic,
            ref jsferic3D);

            Assert.That(ierr, Is.EqualTo(0));

            //check the interpolation results
            double[] targetValuesResults = new double[numTargets];
            Marshal.Copy(c_targetValues, targetValuesResults, 0, numTargets);

            for (int i = 0; i < numTargets; i++)
            {
                Assert.That(targetValuesResults[i], Is.EqualTo(targetValues[i]).Within(1e-6));
            }

        }

        // Create the netcdf files
        [Test]
        //[TestCase(1)]
        //[TestCase(2)]
        [NUnit.Framework.Category("TestAverageInterpolation")]
        public void TestAverageInterpolation()
        {
            using (var unmanagedMemoryRegister = new UnmanagedMemoryRegister() )
            {
                string pathIn = TestHelper.TestFilesDirectoryPath() + @"\inTestAveragingInterpolation.txt";
                Assert.IsTrue(File.Exists(pathIn));
                string pathOut = TestHelper.TestFilesDirectoryPath() + @"\outTestAveragingInterpolation.txt";
                Assert.IsTrue(File.Exists(pathOut));

                int numSamples = 0;
                var sampleX = new double[numSamples];
                var sampleY = new double[numSamples];
                var sampleValues = new double[numSamples];

                readPoints(pathIn, ref sampleX, ref sampleY, ref sampleValues, ref numSamples);

                int numTargets = 0;
                var targetX = new double[numTargets];
                var targetY = new double[numTargets];
                var targetValues = new double[numTargets];

                readPoints(pathOut, ref targetX, ref targetY, ref targetValues, ref numTargets);
                IntPtr c_sampleX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                IntPtr c_sampleY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                IntPtr c_sampleValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                Marshal.Copy(sampleX, 0, c_sampleX, numSamples);
                Marshal.Copy(sampleY, 0, c_sampleY, numSamples);
                Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

                IntPtr c_targetValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numTargets);
                Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

                int ioncid = 0; //file variable 
                int mode = 0;   //open in read mode
                var wrapperNetcdf = new IoNetcdfLibWrapper();
                int iconvtype = 2;
                double convversion = 0.0;
                string pathNetFile = TestHelper.TestFilesDirectoryPath() + @"\simplebox_net_ugrid.nc";
                Assert.IsTrue(File.Exists(pathIn));
                int ierr = wrapperNetcdf.ionc_open(pathNetFile, ref mode, ref ioncid, ref iconvtype, ref convversion);
                Assert.That(ierr, Is.EqualTo(0));

                int meshid = -1;
                int meshType = 2;    //get the 2d
                int l_networkid = 0; // invalid network, not really intrested in getting the networ
                var meshtwoddim = new meshgeomdim();
                getMeshid(ioncid, ref meshid, meshType, ref wrapperNetcdf);
                ierr = wrapperNetcdf.ionc_get_meshgeom_dim(ref ioncid, ref meshid, ref l_networkid, ref meshtwoddim);
                Assert.That(ierr, Is.EqualTo(0));

                //You need to know in advance the number of mesh points
                var meshtwod = new meshgeom();
                unmanagedMemoryRegister.Add(ref meshtwoddim, ref meshtwod);

                //var gridGeomWrapper = new GridGeomLibWrapper();
                bool includeArrays = true;
                int startIndex = 1; //the base index of the arrays
                ierr = wrapperNetcdf.ionc_get_meshgeom(ref ioncid, ref meshid, ref l_networkid, ref meshtwod, ref startIndex, ref includeArrays);
                Assert.That(ierr, Is.EqualTo(0));

                // default parameters for averaging
                double Wu1Duni = 2.0;               // default value from flow_geom init
                int method = 1;                     // averaging 
                int minNumSamples = 1;
                double relativeSearchSize = 1.01;
                int jsferic = 0;
                int jasfer3D = 0;
                int locType = 2;

                var wrapper = new Ec_ModuleLibWrapper();
                ierr = wrapper.averaging(
                ref meshtwoddim,
                ref meshtwod,
                ref startIndex,
                ref c_sampleX,
                ref c_sampleY,
                ref c_sampleValues,
                ref numSamples,
                ref c_targetValues,
                ref locType,
                ref Wu1Duni,
                ref method,
                ref minNumSamples,
                ref relativeSearchSize,
                ref jsferic,
                ref jasfer3D);

                Assert.That(ierr, Is.EqualTo(0));

                //check the interpolation results
                double[] targetValuesResults = new double[numTargets];
                Marshal.Copy(c_targetValues, targetValuesResults, 0, numTargets);

                for (int i = 0; i < numTargets; i++)
                {
                    Assert.That(targetValuesResults[i], Is.EqualTo(targetValues[i]).Within(1e-6));
                }

                //test averaging on the edges
                Marshal.FreeCoTaskMem(c_targetValues);
                numTargets = meshtwoddim.numedge;
                //interpolate on edges
                locType = 2;
                c_targetValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numTargets);

                ierr = wrapper.averaging(
                    ref meshtwoddim,
                    ref meshtwod,
                    ref startIndex,
                    ref c_sampleX,
                    ref c_sampleY,
                    ref c_sampleValues,
                    ref numSamples,
                    ref c_targetValues,
                    ref locType,
                    ref Wu1Duni,
                    ref method,
                    ref minNumSamples,
                    ref relativeSearchSize,
                    ref jsferic,
                    ref jasfer3D);

                Assert.That(ierr, Is.EqualTo(0));
            }

            

        }


        // Test averaging interpolation on location type 2 (middle points of edges)
        [Test]
        [NUnit.Framework.Category("TestAverageInterpolation")]
        public void TestAverageInterpolationOnEdges()
        {
            using (var unmanagedMemoryRegister = new UnmanagedMemoryRegister())
            {
                //get the sample points
                string pathIn = TestHelper.TestFilesDirectoryPath() + @"\inTestAveragingInterpolation.txt";
                Assert.IsTrue(File.Exists(pathIn));

                int numSamples = 0;
                var sampleX = new double[numSamples];
                var sampleY = new double[numSamples];
                var sampleValues = new double[numSamples];

                readPoints(pathIn, ref sampleX, ref sampleY, ref sampleValues, ref numSamples);

                IntPtr c_sampleX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                IntPtr c_sampleY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                IntPtr c_sampleValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                Marshal.Copy(sampleX, 0, c_sampleX, numSamples);
                Marshal.Copy(sampleY, 0, c_sampleY, numSamples);
                Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

                int ioncid = 0; //file variable 
                int mode = 0; //open in read mode
                var wrapperNetcdf = new IoNetcdfLibWrapper();
                int iconvtype = 2;
                double convversion = 0.0;
                string pathNetFile = TestHelper.TestFilesDirectoryPath() + @"\simplebox_net_ugrid.nc";
                Assert.IsTrue(File.Exists(pathIn));
                int ierr = wrapperNetcdf.ionc_open(pathNetFile, ref mode, ref ioncid, ref iconvtype, ref convversion);
                Assert.That(ierr, Is.EqualTo(0));

                int meshid = -1;
                int meshType = 2; // get the 2d
                int l_networkid = 0; // invalid network, not intrested in 1d for now
                var meshtwoddim = new meshgeomdim();
                getMeshid(ioncid, ref meshid, meshType, ref wrapperNetcdf);
                ierr = wrapperNetcdf.ionc_get_meshgeom_dim(ref ioncid, ref meshid, ref l_networkid, ref meshtwoddim);
                Assert.That(ierr, Is.EqualTo(0));

                //You need to know in advance the number of mesh points
                var meshtwod = new meshgeom();
                unmanagedMemoryRegister.Add(ref meshtwoddim, ref meshtwod);

                //var gridGeomWrapper = new GridGeomLibWrapper();
                bool includeArrays = true;
                int startIndex = 1; //the base index of the arrays
                ierr = wrapperNetcdf.ionc_get_meshgeom(ref ioncid, ref meshid, ref l_networkid, ref meshtwod,
                    ref startIndex, ref includeArrays);
                Assert.That(ierr, Is.EqualTo(0));

                // default parameters for averaging
                double Wu1Duni = 2.0; // default value from flow_geom init
                int method = 1; // averaging 
                int minNumSamples = 1;
                double relativeSearchSize = 1.01;
                int jsferic = 0;
                int jasfer3D = 0;
                int locType = 2;

                //test averaging on the edges
                int numTargets = meshtwoddim.numedge;
                IntPtr c_targetValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numTargets);
                Marshal.FreeCoTaskMem(c_targetValues);

                //interpolate on edges
                c_targetValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numTargets);

                var wrapper = new Ec_ModuleLibWrapper();
                ierr = wrapper.averaging(
                    ref meshtwoddim,
                    ref meshtwod,
                    ref startIndex,
                    ref c_sampleX,
                    ref c_sampleY,
                    ref c_sampleValues,
                    ref numSamples,
                    ref c_targetValues,
                    ref locType,
                    ref Wu1Duni,
                    ref method,
                    ref minNumSamples,
                    ref relativeSearchSize,
                    ref jsferic,
                    ref jasfer3D);

                Assert.That(ierr, Is.EqualTo(0));
            }

        }

        // Test averaging interpolation on location type 2 (middle points of edges)
        [Test]
        [NUnit.Framework.Category("TestAverageInterpolation")]
        public void TestAverageInterpolationOnCellCenters()
        {
            using (var unmanagedMemoryRegister = new UnmanagedMemoryRegister())
            {
                //get the sample points
                string pathIn = TestHelper.TestFilesDirectoryPath() + @"\inTestAveragingInterpolation.txt";
                Assert.IsTrue(File.Exists(pathIn));

                int numSamples = 0;
                var sampleX = new double[numSamples];
                var sampleY = new double[numSamples];
                var sampleValues = new double[numSamples];

                readPoints(pathIn, ref sampleX, ref sampleY, ref sampleValues, ref numSamples);

                IntPtr c_sampleX = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                IntPtr c_sampleY = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                IntPtr c_sampleValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numSamples);
                Marshal.Copy(sampleX, 0, c_sampleX, numSamples);
                Marshal.Copy(sampleY, 0, c_sampleY, numSamples);
                Marshal.Copy(sampleValues, 0, c_sampleValues, numSamples);

                int ioncid = 0; //file variable 
                int mode = 0; //open in read mode
                var wrapperNetcdf = new IoNetcdfLibWrapper();
                int iconvtype = 2;
                double convversion = 0.0;
                string pathNetFile = TestHelper.TestFilesDirectoryPath() + @"\simplebox_net_ugrid.nc";
                Assert.IsTrue(File.Exists(pathNetFile));
                int ierr = wrapperNetcdf.ionc_open(pathNetFile, ref mode, ref ioncid, ref iconvtype, ref convversion);
                Assert.That(ierr, Is.EqualTo(0));

                int meshid = -1;
                int meshType = 2; // get the 2d
                int l_networkid = 0; // invalid network, not intrested in 1d for now
                var meshtwoddim = new meshgeomdim();
                getMeshid(ioncid, ref meshid, meshType, ref wrapperNetcdf);
                ierr = wrapperNetcdf.ionc_get_meshgeom_dim(ref ioncid, ref meshid, ref l_networkid, ref meshtwoddim);
                Assert.That(ierr, Is.EqualTo(0));

                //You need to know in advance the number of mesh points
                var meshtwod = new meshgeom();
                unmanagedMemoryRegister.Add(ref meshtwoddim, ref meshtwod);

                //var gridGeomWrapper = new GridGeomLibWrapper();
                bool includeArrays = true;
                int startIndex = 1; //the base index of the arrays
                ierr = wrapperNetcdf.ionc_get_meshgeom(ref ioncid, ref meshid, ref l_networkid, ref meshtwod,
                    ref startIndex, ref includeArrays);
                Assert.That(ierr, Is.EqualTo(0));

                //5. declare but do not allocate meshgeom. it will be allocated by gridgeom (fortran)
                var meshTwoDim = new meshgeomdim();
                
                //6. call find cells
                var wrapperGridgeom = new GridGeomLibWrapper();
                ierr = wrapperGridgeom.ggeo_count_cells(ref meshtwoddim, ref meshtwod, ref meshTwoDim);
                Assert.That(ierr, Is.EqualTo(0));

                meshtwod.face_nodes = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * meshTwoDim.numface * meshTwoDim.maxnumfacenodes);
                meshtwod.facex = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshTwoDim.numface);
                meshtwod.facey = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * meshTwoDim.numface);

                ierr = wrapperGridgeom.ggeo_find_cells(ref meshTwoDim, ref meshtwod);
                Assert.That(ierr, Is.EqualTo(0));

                //// default parameters for averaging
                double Wu1Duni = 2.0; // default value from flow_geom init
                int method = 1; // averaging 
                int minNumSamples = 1;
                double relativeSearchSize = 1.01;
                int jsferic = 0;
                int jasfer3D = 0;
                int locType = 0; // interpolate on the faces

                //test averaging on the faces
                int numTargets = meshTwoDim.numface;
                IntPtr c_targetValues = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numTargets);

                var wrapper = new Ec_ModuleLibWrapper();
                ierr = wrapper.averaging(
                    ref meshtwoddim,
                    ref meshtwod,
                    ref startIndex,
                    ref c_sampleX,
                    ref c_sampleY,
                    ref c_sampleValues,
                    ref numSamples,
                    ref c_targetValues,
                    ref locType,
                    ref Wu1Duni,
                    ref method,
                    ref minNumSamples,
                    ref relativeSearchSize,
                    ref jsferic,
                    ref jasfer3D);

                Assert.That(ierr, Is.EqualTo(0));
                Marshal.FreeCoTaskMem(c_targetValues);
            }
        }


    }

        //[Test]
        //[NUnit.Framework.Category("TestECModule2D")]
        //public void TestECModule2D()
        //{
        //    var wrapper = new Ec_ModuleLibWrapper();
        //    int ntargets = 1;

        //    // 1. ecModuleAddTimeSpaceRelation
        //    string c_name = "waterlevelbnd";
        //    var x = new [] { 0.75, 0.75, 0.75 };
        //    var y = new [] { 0.66, 1.00, 1.33 };
        //    int ncoordinatesSize = x.Length;
        //    int jsferic = 0;
        //    int vectormax = 1;
        //    string c_filename = TestHelper.TestFilesDirectoryPath() + @"\tfl_01.pli";
        //    int filetype = 11;
        //    int method = 3;
        //    int operand = 2;
        //    int src_refdate = 20180320;
        //    int dummInt = 0;
        //    double src_tzone = -999.0; //to be defined
        //    double missing_value = -999.0;
        //    var  targetItemArray = new int[ntargets];


        //    IntPtr c_x = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * ncoordinatesSize);
        //    IntPtr c_y = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * ncoordinatesSize);
        //    IntPtr c_targetItemArray = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * ntargets);
        //    Marshal.Copy(x, 0, c_x, ncoordinatesSize);
        //    Marshal.Copy(y, 0, c_y, ncoordinatesSize);
            
        //    bool success = wrapper.ecModuleAddTimeSpaceRelation(
        //        c_name,
        //        ref c_x,
        //        ref c_y,
        //        ref ncoordinatesSize,
        //        ref jsferic,
        //        ref vectormax,
        //        c_filename,
        //        ref filetype,
        //        ref method,
        //        ref operand,
        //        ref src_refdate,
        //        ref src_tzone,
        //        ref missing_value,
        //        ref c_targetItemArray,
        //        ref dummInt);
        //    Assert.That(success, Is.EqualTo(true));
        //   // Marshal.Copy(targetItemArray, 0, c_targetItemArray, ntargets);
        //    Marshal.Copy(c_targetItemArray, targetItemArray, 0, 1);

        //    // 2. ecGettimespacevalueByItemID
        //    double timesteps = 99.9;
        //    IntPtr c_target_array = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(int)) * ntargets);
        //    success = wrapper.ecGettimespacevalueByItemID(ref targetItemArray[0], ref timesteps, ref c_target_array, ref ntargets);
        //    // LC here it breaks, success is false
        //    Assert.That(success, Is.EqualTo(true));

        //    var target_array = new double[ntargets];
        //    Marshal.Copy(target_array, 0, c_target_array, ntargets);

        //    success = wrapper.ecInstancePrintStateToFile();
        //    Assert.That(success, Is.EqualTo(true));

        //    //Add clean ec module instance
        //}
}
