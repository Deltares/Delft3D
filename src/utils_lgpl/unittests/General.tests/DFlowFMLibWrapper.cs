using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace General.tests
{
    public class DFlowFMLibWrapper
    {
        /// <summary>
        /// IoNetcdf specific constants
        /// </summary>
        public static class LibDetails
        {
            public const int MAXDIMS = 6;
            public const int MAXSTRLEN = 255;
            public const string LIB_NAME = "dflowfm";
            public const string LIB_DEP = "";
            public const string LIB_DLL_NAME = "dflowfm.dll";
        }


        public class PolygonFileReader
        {
            public void read(string path, ref List<double> xCoordinates, ref List<double> yCoordinates, ref List<double> zCoordinates)
            {

                using (TextReader reader = File.OpenText(path))
                {
                    string text;
                    int lineCount = 0;
                    while ((text = reader.ReadLine()) != null)
                    {
                        if (lineCount > 1)
                        {
                            string[] bits = text.Split(' ');
                            bool xFound = false;
                            for (int i = 0; i < bits.Length; i++)
                            {
                                if (bits[i] != "" && !xFound)
                                {
                                    xFound = true;
                                    xCoordinates.Add(double.Parse(bits[i]));
                                    continue;
                                }
                                if (bits[i] != "")
                                {
                                    yCoordinates.Add(double.Parse(bits[i]));
                                    break;
                                }
                            }
                            zCoordinates.Add(-999.0);
                        }
                        lineCount++;
                    }
                }
            }
        }

        public static void SetSharedPath()
        {
            FileInfo fileInfo = new FileInfo(AppDomain.CurrentDomain.BaseDirectory);
            string path = fileInfo.Directory.Parent.Parent.Parent.Parent.Parent.FullName;
            bool is64bit = Environment.Is64BitProcess;
            // If 64-bit process, load 64-bit DLL otherwise load the 32 bit dll 
            if (is64bit)
            {
                path = path + @"\bin\x64\Debug\dflowfm\bin";
            }
            else
            {
                path = path + @"\bin\x86\Debug\dflowfm\bin";
            }
            var envpath = Environment.GetEnvironmentVariable("PATH");
            if (envpath != null && envpath.Contains(path)) return;
            envpath = envpath + ";" + path;
            Environment.SetEnvironmentVariable("PATH", envpath, EnvironmentVariableTarget.Process);
        }


        public static string TestFilesDirectoryPath()
        {
            FileInfo fileInfo = new FileInfo(AppDomain.CurrentDomain.BaseDirectory);
            string path = fileInfo.Directory.Parent.Parent.Parent.Parent.Parent.FullName;
            path = path + @"\engines_gpl\dflowfm\tests\test_data";
            return path;
        }

        public static string GetLibraryPath()
        {
            FileInfo fileInfo = new FileInfo(AppDomain.CurrentDomain.BaseDirectory);
            string path = fileInfo.Directory.Parent.Parent.Parent.Parent.Parent.FullName;
            bool is64bit = Environment.Is64BitProcess;
            // If 64-bit process, load 64-bit DLL otherwise load the 32 bit dll 
            if (is64bit)
            {
                path = path + @"\bin\x64\Debug\dflowfm\bin";
            }
            else
            {
                path = path + @"\bin\x86\Debug\dflowfm\bin";
            }
            path = path + @"\" + LibDetails.LIB_DLL_NAME;
            return path;
        }

        /// <summary>
        /// Loads the model for all subsequent dflowfm.dll 
        /// </summary>
        /// <param name="configurationFile"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "initialize", CallingConvention = CallingConvention.Cdecl)]
        private static extern int initialize([In] string configurationFile);


        /// <summary>
        /// This function gets a polyline and returns an array with the indexes of the snapped flow links
        /// </summary>
        /// <param name="numberOfInputVertices"></param>
        /// <param name="c_xVerticesCoordinates"></param>
        /// <param name="c_yVerticesCoordinates"></param>
        /// <param name="numberOfOutputIndexes"></param>
        /// <param name="c_indexes"></param>
        /// <returns></returns>
        [DllImport(LibDetails.LIB_DLL_NAME, EntryPoint = "get_snapped_flow_links_indexes", CallingConvention = CallingConvention.Cdecl)]
        private static extern int get_snapped_flow_links_indexes_dll(
           [In] ref int numberOfInputVertices,
           [In] ref IntPtr c_xVerticesCoordinates, 
           [In] ref IntPtr c_yVerticesCoordinates,
           [In] ref int startIndex,
           [In, Out] ref int numberOfOutputIndexes, 
           [In, Out] ref IntPtr c_indexes);

        public int get_snapped_flow_links_indexes(
            ref int numberOfInputVertices,
            ref IntPtr c_xVerticesCoordinates,
            ref IntPtr c_yVerticesCoordinates,
            ref int startIndex,
            ref int numberOfOutputIndexes,
            ref IntPtr c_indexes)
        {
            return get_snapped_flow_links_indexes_dll( ref numberOfInputVertices, ref c_xVerticesCoordinates, ref  c_yVerticesCoordinates, ref  startIndex,
                                                       ref  numberOfOutputIndexes, ref  c_indexes );
        }

        public int initialize_model(string configurationFile)
        {
            return initialize(configurationFile);
        }
    }
}
