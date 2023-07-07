using System;
using System.IO;
using System.Runtime.InteropServices;
using NUnit.Framework;
using General.tests;
using System.Collections.Generic;

namespace DflowFmTests
{

    public class DflowFmTests
    {
        //pointer to the loaded dll
        public static IntPtr _libdflowfm;

        //Constructor loads the library
        static DflowFmTests()
        {
            DFlowFMLibWrapper.SetSharedPath();
            string filename = DFlowFMLibWrapper.GetLibraryPath();
            _libdflowfm = TestHelper.LoadLibrary(filename);
            Assert.That(_libdflowfm, Is.Not.Null);
        }

        [Test]
        [NUnit.Framework.Category("DFlowFMTests")]
        public void shouldSnapPolygonFlowlinks()
        {
            var dflowFMWrapper = new DFlowFMLibWrapper();

            //1. initialize the model
            string modelPath = DFlowFMLibWrapper.TestFilesDirectoryPath() + @"\rectangular_mesh.mdu";
            string originalCurrentDirectory = Environment.CurrentDirectory;
            Directory.SetCurrentDirectory(Path.GetDirectoryName(modelPath) ?? ".");
            dflowFMWrapper.initialize_model(Path.GetFileName(modelPath));

            //2. read the polygon file
            string polygonPath = DFlowFMLibWrapper.TestFilesDirectoryPath() + @"\testPolygon.pol";
            var polygonReader = new DFlowFMLibWrapper.PolygonFileReader();
            var xCoordinates = new List<double>();
            var yCoordinates = new List<double>();
            var zCoordinates = new List<double>();
            polygonReader.read(polygonPath, ref xCoordinates, ref yCoordinates, ref zCoordinates);

            //3. Allocate and assign the polygon
            int numberOfInputVertices = xCoordinates.Count;
            int numberOfOutputIndexes = 0;
            IntPtr c_xVerticesCoordinates = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numberOfInputVertices);
            IntPtr c_yVerticesCoordinates = Marshal.AllocCoTaskMem(Marshal.SizeOf(typeof(double)) * numberOfInputVertices);
            IntPtr c_indexes = IntPtr.Zero;
            Marshal.Copy(xCoordinates.ToArray(), 0, c_xVerticesCoordinates, numberOfInputVertices);
            Marshal.Copy(yCoordinates.ToArray(), 0, c_yVerticesCoordinates, numberOfInputVertices);

            //4. the retrived indexes should be 0 based
            int startIndex = 0;

            //4. snap the links
            int ierr = dflowFMWrapper.get_snapped_flow_links_indexes(
                ref numberOfInputVertices,
                ref c_xVerticesCoordinates,
                ref c_yVerticesCoordinates,
                ref startIndex, 
                ref numberOfOutputIndexes,
                ref c_indexes);      
            Assert.That(ierr, Is.EqualTo(0));

            //5. check the results
            int[] result_indexes = new int[numberOfOutputIndexes];
            Marshal.Copy(c_indexes, result_indexes, 0, numberOfOutputIndexes);
            Assert.That(result_indexes[0], Is.EqualTo(1));
            Assert.That(result_indexes[1], Is.EqualTo(3));
            Assert.That(result_indexes[2], Is.EqualTo(7));
            Assert.That(result_indexes[3], Is.EqualTo(10));

            //6. deallocate memory
            Marshal.FreeCoTaskMem(c_xVerticesCoordinates);
            Marshal.FreeCoTaskMem(c_yVerticesCoordinates);

            //7. reset original directory
            Directory.SetCurrentDirectory(originalCurrentDirectory);
        }
    }
}
