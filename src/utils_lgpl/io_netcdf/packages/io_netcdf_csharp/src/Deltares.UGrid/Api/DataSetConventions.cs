namespace Deltares.UGrid.Api
{
    internal enum DataSetConventions
    {
        CONV_NULL = 0,//Dataset conventions not yet detected
        CONV_CF = 1,
        CONV_UGRID = 2,//Dataset based on UGRID-conventions
        CONV_SGRID = 4,//Dataset based on SGRID-conventions
        CONV_OTHER = -99,//Dataset based on unknown or unsupported conventions (user should fall back to NetCDF native API calls)
        CONV_TEST = -111111 //Dataset Id for testing
    }
}