      FUNCTION GTLLEN (PNTNAM)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             GTLLEN (GeT Logical variable LENgth)
c
c Module description: Report length of variable in LPool
c
c                     This function reports the length of a logical
c                     variable allocated in the LPool
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 gtllen            O  Length of logical variable
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
c $Log: gtllen.pf,v $
c Revision 1.2  1995/05/30  07:03:30  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:43  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER  GTLLEN

      CHARACTER PNTNAM*(*)

      include '..\include\pointrs.i'

      INTEGER   ZZGTLN
      EXTERNAL  ZZGTLN

      GTLLEN = ZZGTLN (PNTNAM, LPNTRS, LLENGT, NLPNTR, LOLDPT)

      END
