using System;
using System.Runtime.InteropServices;

namespace Deltares.UGrid.Helpers
{
    internal static class IntPtrExtensions
    {
        public static T[] CreateValueArray<T>(this IntPtr pointer, int size, int stringSize = 0)
        {
            var array = new T[size];

            if (typeof(T) == typeof(double))
            {
                Marshal.Copy(pointer, array as double[], 0, size);
            }
            else if (typeof(T) == typeof(int))
            {
                Marshal.Copy(pointer, array as int[], 0, size);
            }
            else if (typeof(T) == typeof(string))
            {
                var totalByteSize = size * stringSize;
                var byteArray = new byte[totalByteSize];

                Marshal.Copy(pointer, byteArray, 0, totalByteSize);
                
                byteArray.GetStringArrayFromFlattenedAsciiCodedStringArray(size).CopyTo(array, 0);
            }
            else
            {
                throw new NotSupportedException("Currently only double, int and string are supported");
            }

            return array;
        }
    }
}