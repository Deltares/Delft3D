      FUNCTION ZZFDPT (PNTNAM, PNTNMS, NPNTRS, OLDPNT)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             ZZFDPT (ZZ FinD PoinTer)
c
c Module description: This function locates the address of a variable
c
c                     This function is called from several user func-
c                     tions to find a variable name in a particular
c                     memory pool.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 npntrs            I  Number of variable names
c  4 oldpnt            IO Last index found of previous call
c  1 pntnam            I  Variable name
c  2 pntnms            I  Allocated variable names
c  0 zzfdpt            O  Address of variable
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: zzfdpt.pf,v $
c Revision 1.3  1999/03/15  15:52:37  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:03:44  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:56  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER   ZZFDPT

      CHARACTER PNTNMS(*)*(*)

      CHARACTER PNTNAM*(*)
      INTEGER   NPNTRS
     +         ,OLDPNT

      INTEGER   NLOOP

      DO 100 NLOOP = 1, NPNTRS
         OLDPNT = MOD(OLDPNT,NPNTRS) + 1
         IF (PNTNAM .EQ. PNTNMS(OLDPNT)) THEN
            ZZFDPT = OLDPNT
            GOTO 900
         ENDIF
 100  CONTINUE

      ZZFDPT = -1
      RETURN

 900  RETURN

      END
