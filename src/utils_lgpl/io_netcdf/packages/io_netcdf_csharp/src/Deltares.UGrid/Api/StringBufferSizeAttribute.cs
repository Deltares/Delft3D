using System;

namespace Deltares.UGrid.Api
{
    internal class StringBufferSizeAttribute : Attribute
    {
        public int BufferSize { get; set; }
    }
}