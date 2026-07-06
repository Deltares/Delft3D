using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Text;

namespace DFlowFM.IO.Native;

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
        using (new DllSearchDirectoryScope(directory))
        {
            IntPtr ptr = LoadLibrary(dllFileName);
            if (ptr != IntPtr.Zero)
            {
                return;
            }

            int error = Marshal.GetLastWin32Error();
            Win32Exception exception = new(error);

            throw new FileNotFoundException(
                string.Join(Environment.NewLine,
                    $"Could not find / load {dllFileName}",
                    $"Error: {error} - {exception.Message}",
                    $"File: {directory}\\{dllFileName}"));
        }
    }

    private sealed class DllSearchDirectoryScope : IDisposable
    {
        private readonly string _oldDirectory;

        public DllSearchDirectoryScope(string directory)
        {
            _oldDirectory = GetCurrentDirectory();
            SetDllDirectory(directory);
        }

        public void Dispose()
        {
            SetDllDirectory(_oldDirectory);
        }

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern int GetDllDirectory(int nBufferLength, StringBuilder lpPathName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        private static extern void SetDllDirectory(string lpPathName);

        private static string GetCurrentDirectory()
        {
            StringBuilder buffer = new(4096);
            GetDllDirectory(4096, buffer);
            return buffer.ToString();
        }
    }
}