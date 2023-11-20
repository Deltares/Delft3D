using System;
using System.Runtime.InteropServices;

namespace General.tests
{
    //This class contains kernel functions to load/unload libraries and determine the build type
    public static class NativeLibrary
    {
        [DllImport("kernel32.dll")]
        public static extern IntPtr LoadLibrary(string dllToLoad);

        [DllImport("kernel32.dll")]
        public static extern bool FreeLibrary(IntPtr hModule);

        [DllImport("kernel32.dll")]
        public static extern uint GetLastError();

#if DEBUG
        public static string mode = "Debug";
#else
        public static string mode = "Release";
#endif
    }
}
