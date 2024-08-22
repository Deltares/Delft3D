      FUNCTION MKCPNT (PNTNAM, LENGTH)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             MKCPNT (MaKe Character PoiNTer)
c
c Module description: Create address for variable in CPool
c
c                     This function creates an address for a character
c                     variable in the CPool
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 length            P  -
c  0 mkcpnt            O  Allocated addres for character variable
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
c $Log: mkcpnt.pf,v $
c Revision 1.2  1995/05/30  07:03:37  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:49  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER  MKCPNT

      CHARACTER PNTNAM*(*)
      INTEGER   LENGTH

      include '..\include\pointrs.i'
      include '..\include\mempool.i'

      INTEGER   ZZMKPT
      EXTERNAL  ZZMKPT

      MKCPNT = ZZMKPT (PNTNAM, LENGTH, CPNTRS, CADDRS, CLENGT,
     +                 NCPNTR, MXCPNT, CPSIZE, COLDPT)

      END
