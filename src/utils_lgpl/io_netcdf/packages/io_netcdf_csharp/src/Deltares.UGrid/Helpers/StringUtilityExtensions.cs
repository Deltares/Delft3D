using System;
using System.Linq;
using System.Text;
using System.Reflection;
using Deltares.UGrid.Api;

namespace Deltares.UGrid.Helpers
{
    internal static class StringUtilityExtensions
    {
        private const char FillChar = ' ';

        public static byte[] GetFlattenedAsciiCodedStringArray(this string[] strings, int bufferSize)
        {
            var flattenedAsciiCodedStringArray = string.Join("", strings.Select(s => s.ToFixedLengthString(bufferSize)));

            return Encoding.Convert(Encoding.Unicode, Encoding.ASCII,
                Encoding.Unicode.GetBytes(flattenedAsciiCodedStringArray));
        }

        public static string[] GetStringArrayFromFlattenedAsciiCodedStringArray(this byte[] byteArray, int bufferSize)
        {
            var asciiString = Encoding.ASCII.GetString(byteArray);
            var blokSize = byteArray.Length / bufferSize;
            var strings = Enumerable.Range(0, bufferSize)
                .Select(i => asciiString.Substring(i * blokSize, blokSize)).ToArray();

            return strings.ConvertFixedLengthArray();
        }

        public static string[] GetFixedLengthStringArray(this string[] strings, int bufferSize)
        {
            for (var index = 0; index < strings.Length; index++)
            {
                strings[index] = strings[index].ToFixedLengthString(bufferSize); 
            }

            return strings;
        }

        internal static string ToFixedLengthString(this string s, int bufferSize)
        {
            if (s == null)
            {
                return new string(FillChar, bufferSize);
            }

            return s.Length > bufferSize 
                ? s.Substring(0, bufferSize) // truncate
                : s.PadRight(bufferSize, FillChar); // pad
        }

        public static string[] ConvertFixedLengthArray(this string[] strings)
        {
            return strings.Select(s => s.TrimEnd(FillChar)).ToArray();
        }

        internal static int GetBufferSize(this Type type, string fieldName)
        {
            return type.GetField(fieldName)?.GetCustomAttribute<StringBufferSizeAttribute>()?.BufferSize ?? 0;
        }
    }
}