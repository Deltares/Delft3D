using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Text;

namespace DFlowFM.IO.Native;

internal static class NativeLibrary
{
    [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Unicode)]
    private static extern IntPtr LoadLibrary(string lpFileName);

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
            var exception = new Win32Exception(error);

            throw new FileNotFoundException(
                string.Join(Environment.NewLine,
                            $"Could not find / load {dllFileName}",
                            $"Error: {error} - {exception.Message}",
                            $"File: {directory}\\{dllFileName}"));
        }
    }

    private sealed class DllSearchDirectoryScope : IDisposable
    {
        private readonly string oldDirectory;

        public DllSearchDirectoryScope(string directory)
        {
            oldDirectory = GetCurrentDirectory();
            SetDllDirectory(directory);
        }

        public void Dispose()
        {
            SetDllDirectory(oldDirectory);
        }

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern int GetDllDirectory(int nBufferLength, StringBuilder lpPathName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        private static extern void SetDllDirectory(string lpPathName);

        private static string GetCurrentDirectory()
        {
            var buffer = new StringBuilder(4096);
            GetDllDirectory(4096, buffer);
            return buffer.ToString();
        }
    }
}