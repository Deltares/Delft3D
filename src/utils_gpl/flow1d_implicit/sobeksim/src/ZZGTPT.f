      FUNCTION ZZGTPT (PNTNAM, PNTNMS, PNTADR, NPNTRS, OLDPTR)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             ZZGTPT (ZZ GeT PoinTer)
c
c Module description: This function locates a variable name in a pool
c
c                     This function is called from several user func-
c                     tions to find a variable name in a particular
c                     memory pool
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 npntrs            P  -
c  5 oldptr            P  -
c  3 pntadr            I  Variable addresses in pool
c  1 pntnam            P  -
c  2 pntnms            P  -
c  0 zzgtpt            O  Address of variable
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c zzfdpt  ZZ FinD PoinTer
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: zzgtpt.pf,v $
c Revision 1.3  1996/12/02  10:05:00  kuipe_j
c avoid negative pointers
c
c Revision 1.2  1995/05/30  07:03:46  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:57  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER  ZZGTPT

      CHARACTER PNTNMS(*)*(*)
      INTEGER   PNTADR(*)

      CHARACTER PNTNAM*(*)
      INTEGER   NPNTRS
     +         ,OLDPTR

      INTEGER   PNTR

      INTEGER   ZZFDPT
      EXTERNAL  ZZFDPT

      PNTR = ZZFDPT (PNTNAM, PNTNMS, NPNTRS, OLDPTR)

      IF (PNTR .NE. -1) THEN
         ZZGTPT = PNTADR(PNTR)

      ELSE
         ZZGTPT = -1
      ENDIF

      END
