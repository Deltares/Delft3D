using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.UGrid.Helpers
{
    /// <summary>
    /// Helper functions for loading native libraries
    /// </summary>
    internal static class NativeLibrary
    {
        [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Unicode)]
        private static extern IntPtr LoadLibrary(string lpFileName);

        /// <summary>
        /// Loads the provided native dll (<paramref name="dllFileName"/> and <paramref name="directory"/>)
        /// for the current process.
        /// </summary>
        /// <remarks>
        /// Call this from a static constructor in a class that has DllImport external methods. This 
        /// method uses LoadLibrary to load the correct dll for the current process (32bit or 64bit) 
        /// before DllImport has the chance to resolve the external calls. As long as the dll name is 
        /// the same this works.
        /// </remarks>
        /// <param name="dllFileName">The dll file to load.</param>
        /// <param name="directory">The directory to load the dll from.</param>
        public static void LoadNativeDll(string dllFileName, string directory)
        {
            using (new SwitchDllSearchDirectoryHelper(directory))
            {
                // attempt to load the library
                var ptr = LoadLibrary(dllFileName);
                if (ptr != IntPtr.Zero) return;

                var error = Marshal.GetLastWin32Error();
                var exception = new Win32Exception(error);

                var messageCouldNotFind = $"Could not find / load {dllFileName}";
                var messageError = $"Error: {error} - {exception.Message}";
                var messageFile = $"{"File"}: {directory}\\{dllFileName}";

                throw new FileNotFoundException(string.Join(Environment.NewLine, messageCouldNotFind, messageError, messageFile));
            }
        }

        private sealed class SwitchDllSearchDirectoryHelper : IDisposable
        {
            [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
            private static extern int GetDllDirectory(int nBufferLength, StringBuilder lpPathName);

            [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
            private static extern void SetDllDirectory(string lpPathName);

            private readonly string oldDirectory;

            public SwitchDllSearchDirectoryHelper(string dllDirectory)
            {
                oldDirectory = GetDllDirectory();
                SetDllDirectory(dllDirectory);
            }

            public void Dispose()
            {
                SetDllDirectory(oldDirectory);
            }

            private static string GetDllDirectory()
            {
                var tmp = new StringBuilder(4096);
                GetDllDirectory(4096, tmp);
                return tmp.ToString();
            }
        }
    }
}