      FUNCTION ZZMKPT (PNTNAM, LENGTH, PNTNMS, PNTADR, PNTLEN,
     +                 NPNTRS, MXPNTS, PLSIZE, OLDPNT)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             ZZMKPT (ZZ MaKe PoinTer)
c
c Module description: This function creates a variable name in a pool
c
c                     This function is called from several user func-
c                     tions to create a variable name in a particular
c                     memory pool. After the call zzmkpt will return one
c                     of the following values:
c
c                     >0: Address in memory pool
c                     -1: Name already allocated in memory pool
c                     -2: No more space in memory pool
c                     -3: No more space to store variable name
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 length            IO Length of variable
c  7 mxpnts            I  Maximum number of variables possible
c  6 npntrs            IO Number of variable names
c  9 oldpnt            P  -
c  8 plsize            I  Maximum pool size
c  4 pntadr            IO Variable addresses in pool
c  5 pntlen            O  Variable lengths in pool
c  1 pntnam            I  Variable name
c  3 pntnms            O  Allocated variable names
c  0 zzmkpt            O  Allocated address for variable
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: zzmkpt.pf,v $
c Revision 1.3  1999/03/15  15:52:38  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:03:47  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:58  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER  ZZMKPT

      CHARACTER PNTNMS(*)*(*)
      INTEGER   PNTADR(*)
      INTEGER   PNTLEN(*)

      CHARACTER PNTNAM*(*)
      INTEGER   LENGTH
     +         ,NPNTRS
     +         ,MXPNTS
     +         ,PLSIZE
     +         ,OLDPNT

      INTEGER   ZZFDPT

      IF (LENGTH .EQ. 0) THEN
         LENGTH = 1
      ENDIF

      IF (NPNTRS .LT. MXPNTS) THEN

         IF (PNTADR(NPNTRS+1)+LENGTH .LE. PLSIZE) THEN

            IF (ZZFDPT (PNTNAM, PNTNMS, NPNTRS, OLDPNT) .EQ. -1) THEN
               NPNTRS           = NPNTRS+1
               PNTNMS(NPNTRS)   = PNTNAM
               PNTLEN(NPNTRS)   = LENGTH
               PNTADR(NPNTRS+1) = PNTADR(NPNTRS)+LENGTH
               ZZMKPT           = PNTADR(NPNTRS)

            ELSE
               ZZMKPT = -1

            ENDIF

         ELSE
            ZZMKPT = -2

         ENDIF

      ELSE
         ZZMKPT = -3

      ENDIF

      END
