namespace Deltares.UGrid.Api
{
    /// <summary>
    /// File open mode
    /// </summary>
    public enum OpenMode
    {
        /// <summary>
        /// Open only for reading
        /// </summary>
        Reading,

        /// <summary>
        /// Open for reading and writing
        /// </summary>
        /// <remarks>
        /// This can only be done in a very limited way. Removing or re-defining the
        /// dimensions can not be done because of NetCdf limitations
        /// </remarks>
        Appending
    }
}