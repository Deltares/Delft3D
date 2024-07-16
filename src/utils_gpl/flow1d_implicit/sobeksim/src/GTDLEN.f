      FUNCTION GTDLEN (PNTNAM)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             GTDLEN (GeT Double precision variable LENgth)
c
c Module description: Report length of variable in DPool
c
c                     This function reports the length of a double pre-
c                     cision variable allocated in the DPool
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 gtdlen            O  Length of double precision variable
c  1 pntnam            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c zzgtln  ZZ GeT LeNgth
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gtdlen.pf,v $
c Revision 1.2  1995/05/30  07:03:27  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:40  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:01  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER  GTDLEN

      CHARACTER PNTNAM*(*)

      include '..\include\pointrs.i'

      INTEGER   ZZGTLN
      EXTERNAL  ZZGTLN

      GTDLEN = ZZGTLN (PNTNAM, DPNTRS, DLENGT, NDPNTR, DOLDPT)

      END
