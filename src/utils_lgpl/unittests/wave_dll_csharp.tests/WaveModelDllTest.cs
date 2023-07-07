using System;
using System.Text;
using NUnit.Framework;
using System.IO;

//using DelftTools.Utils;

namespace wave_dll_csharp.tests
{
    [TestFixture]
    public class WaveModelDllTest
    {
        [Test]
        public void InitializeTest()
        {
            var dll = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"..\..\engines_gpl\d_hydro\bin\Debug\wave.dll");        
            string curPath = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(@"..\..\..\..\..\examples\07_wave");

            File.Delete("TMP_grid2swan01");
            int retval = WaveModelDll.initialize("obw.mdw");
            Assert.AreEqual(0, retval);
            Assert.AreEqual(true, File.Exists("TMP_grid2swan01"));
            retval = WaveModelDll.finalize();
            Assert.AreEqual(0, retval);

            Directory.SetCurrentDirectory(curPath);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll);
        }

        [Test]
        public void GetVersionStringTest()
        {
            var dll = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"..\..\engines_gpl\d_hydro\bin\Debug\wave.dll");
            string curPath = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(@"..\..\..\..\..\examples\07_wave");
            const int maxStrLen = WaveModelDll.MAXSTRLEN;
            //string version = "";
            var versionsb = new StringBuilder("tst".PadRight(maxStrLen));

            WaveModelDll.get_version_string(versionsb);
            string version = versionsb.ToString();
            Console.WriteLine(version);
            Assert.AreEqual("Deltares, Delft3D-WAVE Version", version.Substring(0, 30));

            Directory.SetCurrentDirectory(curPath);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll);
        }

        [Test]
        public void GetStartTimeTest()
        {
            var dll = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"..\..\engines_gpl\d_hydro\bin\Debug\wave.dll");
            string curPath = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(@"..\..\..\..\..\examples\07_wave");

            double t = 0.0;
            WaveModelDll.get_start_time(ref t);
            Assert.AreEqual(-999.0, t);

            Directory.SetCurrentDirectory(curPath);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll);
        }

        [Test]
        public void GetCurrentTimeTest()
        {
            var dll = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"..\..\engines_gpl\d_hydro\bin\Debug\wave.dll");
            string curPath = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(@"..\..\..\..\..\examples\07_wave");

            double t = 0.0;
            WaveModelDll.get_current_time(ref t);
            Assert.AreEqual(0.0, t);

            Directory.SetCurrentDirectory(curPath);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll);
        }

        [Test]
        public void SetModeTest()
        {
            var dll = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"..\..\engines_gpl\d_hydro\bin\Debug\wave.dll");
            string curPath = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(@"..\..\..\..\..\examples\07_wave");

            WaveModelDll.set_var("mode", "stand-alone");
            WaveModelDll.set_var("mode", "online with DflowFM");
            WaveModelDll.set_var("mode", "online with Delft3D-FLOW");

            Directory.SetCurrentDirectory(curPath);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll);
        }

        [Test]
        public void FinalizeTest()
        {
            var dll = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"..\..\engines_gpl\d_hydro\bin\Debug\wave.dll");
            string curPath = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(@"..\..\..\..\..\examples\07_wave");

            int retval = WaveModelDll.finalize();
            Assert.AreEqual(0, retval);

            Directory.SetCurrentDirectory(curPath);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll);
        }

        [Test]
        public void UpdateTest()
        {
            const string arch = "win32";
            const string d3dHome = @"c:\code\branches\wave_flowfm\bin";
            const string wavebindir = d3dHome + @"\" + arch + @"\" + @"wave\bin";
            const string swanbindir = d3dHome + @"\" + arch + @"\" + @"swan\bin";
            const string swanscriptdir = d3dHome + @"\" + arch + @"\" + @"swan\scripts";
            string orgarch = Environment.GetEnvironmentVariable("ARCH");
            string orgd3dHome = Environment.GetEnvironmentVariable("D3D_HOME");
            string orgPath = Environment.GetEnvironmentVariable("PATH");
            string newPath = swanbindir + @";" + swanscriptdir + @";" + wavebindir + @";" + @"c:\Program Files (x86)\Intel\Composer XE 2011 SP1\redist\ia32\compiler" + @";" + orgPath;
            Environment.SetEnvironmentVariable("ARCH", arch);
            Environment.SetEnvironmentVariable("D3D_HOME", d3dHome);
            Environment.SetEnvironmentVariable("PATH", newPath);
            var dll = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"..\..\engines_gpl\d_hydro\bin\Debug\wave.dll");
            // We don't want this, why is it needed (sometimes?)
            var dll2 = DelftTools.Utils.Interop.NativeLibrary.LoadLibrary(@"c:\Program Files (x86)\Intel\Composer XE 2011 SP1\redist\ia32\compiler\libifcoremdd.dll");
            string curPath = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(@"..\..\..\..\..\examples\07_wave");
            if (File.Exists("swn-diag.obw"))
                File.Delete("swn-diag.obw");
            if (File.Exists("wavm-obw.dat"))
                File.Delete("wavm-obw.dat");
            WaveModelDll.set_var("mode", "stand-alone");
            int retval = WaveModelDll.initialize("obw.mdw");
            const double dt = 3600.0;
            double t = -999.0;
            WaveModelDll.update(dt);
            WaveModelDll.get_current_time(ref t);
            Assert.AreEqual(3600.0, t);
            WaveModelDll.update(dt);
            WaveModelDll.get_current_time(ref t);
            Assert.AreEqual(7200.0, t);
            retval = WaveModelDll.finalize();
            Assert.AreEqual(0, retval);
            Assert.AreEqual(true, File.Exists("swn-diag.obw"));
            Assert.AreEqual(true, File.Exists("wavm-obw.dat"));

            Directory.SetCurrentDirectory(curPath);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll);
            DelftTools.Utils.Interop.NativeLibrary.FreeLibrary(dll2);
            Environment.SetEnvironmentVariable("ARCH", orgarch);
            Environment.SetEnvironmentVariable("D3D_HOME", orgd3dHome);
            Environment.SetEnvironmentVariable("PATH", orgPath);
        }


    }
}