using System.Runtime.InteropServices;

namespace Deltares.UGrid.Entities
{
    [StructLayout(LayoutKind.Sequential)]
    internal struct InteropMetadata
    {
        private const int metadatasize = 100;

        [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
        public char[] institution;

        [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
        public char[] source;

        [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
        public char[] references;

        [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
        public char[] version;

        [MarshalAs(UnmanagedType.ByValArray, SizeConst = metadatasize)]
        public char[] modelname;
    }
}