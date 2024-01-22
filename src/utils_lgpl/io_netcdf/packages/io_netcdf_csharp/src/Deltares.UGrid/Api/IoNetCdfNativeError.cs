using System;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Error from the native IO_NetCDF library
    /// </summary>
    public class IoNetCdfNativeError : Exception
    {
        /// <summary>
        /// Creates an <see cref="Exception"/> for an native IoNetCdf dll error
        /// </summary>
        /// <param name="errorCode">Returned error code</param>
        /// <param name="errorMessage">Retrieved error message</param>
        /// <param name="nativeFunctionName">Name of the native function</param>
        /// <param name="memberName">CSharp function containing the native call</param>
        public IoNetCdfNativeError(int errorCode, string errorMessage, string nativeFunctionName , string memberName) : 
            base($"The io_netcdf library returned error code {errorCode} and error message {errorMessage} in {memberName ?? "unknown function"} ({nativeFunctionName})")
        {
            ErrorCode = errorCode;
            ErrorMessage = errorMessage;
            NativeFunctionName = nativeFunctionName;
        }

        /// <summary>
        /// Native error code
        /// </summary>
        public int ErrorCode { get; }

        /// <summary>
        /// Native (retrieved) error message
        /// </summary>
        public string ErrorMessage { get; }

        /// <summary>
        /// Name of the native function that returned an error code
        /// </summary>
        public string NativeFunctionName { get; }
    }
}