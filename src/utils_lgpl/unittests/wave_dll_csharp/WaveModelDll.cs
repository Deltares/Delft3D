using System.Runtime.InteropServices;
using System.Text;

namespace wave_dll_csharp
{
    public static class WaveModelDll
    {
        public const int MAXDIMS = 6;
        public const int MAXSTRLEN = 1024;

        [DllImport("wave", EntryPoint = "initialize", CallingConvention = CallingConvention.Cdecl)]
        public static extern int initialize([In] string configFile);

        //[DllImport("wave", EntryPoint = "get_version_string", CallingConvention = CallingConvention.Cdecl)]
        //public static extern void get_version_string([In, Out] char[] versionString);

        [DllImport("wave", EntryPoint = "get_version_string", CallingConvention = CallingConvention.Cdecl)]
        public static extern void get_version_string([Out] StringBuilder vers);




        [DllImport("wave", EntryPoint = "update", CallingConvention = CallingConvention.Cdecl)]
        public static extern void update([In] double dt);

        [DllImport("wave", EntryPoint = "update_until", CallingConvention = CallingConvention.Cdecl)]
        public static extern void update_until([In] ref double dt);

        [DllImport("wave", EntryPoint = "finalize", CallingConvention = CallingConvention.Cdecl)]
        public static extern int finalize();

        [DllImport("wave", EntryPoint = "get_start_time", CallingConvention = CallingConvention.Cdecl)]
        public static extern void get_start_time([In, Out] ref double t);

        [DllImport("wave", EntryPoint = "get_end_time", CallingConvention = CallingConvention.Cdecl)]
        public static extern void get_end_time([In, Out] ref double t);

        [DllImport("wave", EntryPoint = "get_time_step", CallingConvention = CallingConvention.Cdecl)]
        public static extern void get_time_step([In, Out] ref double t);

        [DllImport("wave", EntryPoint = "get_current_time", CallingConvention = CallingConvention.Cdecl)]
        public static extern void get_current_time([In, Out] ref double t);

        [DllImport("wave", EntryPoint = "set_var", CallingConvention = CallingConvention.Cdecl)]
        public static extern void set_var([In] string key, [In] string value);

    }
}