      FUNCTION MKDPNT (PNTNAM, LENGTH)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             MKDPNT (MaKe Double precision variable PoiNTer)
c
c Module description: Create address for variable in DPool
c
c                     This function creates an address for a double pre-
c                     cision variable in the DPool
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 length            P  -
c  0 mkdpnt            O  Allocated addres for double precision variable
c  1 pntnam            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c zzmkpt  ZZ MaKe PoinTer
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mkdpnt.pf,v $
c Revision 1.2  1995/05/30  07:03:38  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:50  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER  MKDPNT

      CHARACTER PNTNAM*(*)
      INTEGER   LENGTH

      include '..\include\pointrs.i'
      include '..\include\mempool.i'

      INTEGER   ZZMKPT
      EXTERNAL  ZZMKPT

      MKDPNT = ZZMKPT (PNTNAM, LENGTH, DPNTRS, DADDRS, DLENGT,
     +                 NDPNTR, MXDPNT, DPSIZE, DOLDPT)

      END
