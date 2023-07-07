using Deltares.UGrid.Entities;
using Deltares.UGrid.Helpers;
using ProtoBuf;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Class for storing meta data about the UGrid file (model, source and version)
    /// </summary>
    [ProtoContract(AsReferenceDefault = true)]
    public class FileMetaData
    {
        /// <summary>
        /// Construct a <see cref="FileMetaData"/> object for an unknown model/source
        /// </summary>
        public FileMetaData() : this("Unknown model", "Unknown Source", "-")
        {

        }

        /// <summary>
        /// Construct a <see cref="FileMetaData"/> object based on provided parameters
        /// </summary>
        /// <param name="modelName">Name of the model</param>
        /// <param name="source">Source of the model</param>
        /// <param name="version">Version of the model</param>
        public FileMetaData(string modelName, string source, string version)
        {
            ModelName = modelName;
            Source = source;
            Version = version;
        }

        /// <summary>
        /// Name of the model for which the file is created
        /// </summary>
        [ProtoMember(1)]
        public string ModelName { get; set; }

        /// <summary>
        /// Source (application) that creates this file
        /// </summary>
        [ProtoMember(2)]
        public string Source { get; set; }

        /// <summary>
        /// Version of the application that creates this file
        /// </summary>
        [ProtoMember(3)]
        public string Version { get; set; }

        internal InteropMetadata CreateMetaData()
        {
            return new InteropMetadata
            {
                institution = "Deltares".ToFixedLengthString(100).ToCharArray(),
                modelname = ModelName.ToFixedLengthString(100).ToCharArray(),
                references = "https://github.com/ugrid-conventions/ugrid-conventions".ToFixedLengthString(100).ToCharArray(),
                source = Source.ToFixedLengthString(100).ToCharArray(),
                version = Version.ToFixedLengthString(100).ToCharArray()
            };
        }
    }
}