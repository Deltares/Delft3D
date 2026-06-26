namespace DFlowFM.IO.Tests.Mdu;

internal static class MduTestFixtures
{
    public const string InvalidMduContent =
        """
        [general]
        fileVersion = 1.09
        """;

    public const string ValidMduContent =
        """
        [general]
        fileType     = modelDef
        fileVersion  = 1.09
        program      = D-Flow FM

        [geometry]
        netFile      = f34_net.nc
        thinDamFile  = thd1.pli thd2.pli thd3.pli
        kmx          = 3
        useCaching   = 1

        [numerics]
        cflMax       = 1.5
        timeStepType = 3

        [time]
        refDate      = 20260101
        tUnit        = H

        [output]
        hisInterval  = 300.0 500.0
        """;
}