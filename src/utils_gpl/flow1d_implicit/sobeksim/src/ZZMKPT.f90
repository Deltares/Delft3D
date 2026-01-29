FUNCTION ZZMKPT (PNTNAM, LENGTH, PNTNMS, PNTADR, PNTLEN,&
&NPNTRS, MXPNTS, PLSIZE, OLDPNT)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             ZZMKPT (ZZ MaKe PoinTer)
!
! Module description: This function creates a variable name in a pool
!
!                     This function is called from several user func-
!                     tions to create a variable name in a particular
!                     memory pool. After the call zzmkpt will return one
!                     of the following values:
!
!                     >0: Address in memory pool
!                     -1: Name already allocated in memory pool
!                     -2: No more space in memory pool
!                     -3: No more space to store variable name
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 length            IO Length of variable
!  7 mxpnts            I  Maximum number of variables possible
!  6 npntrs            IO Number of variable names
!  9 oldpnt            P  -
!  8 plsize            I  Maximum pool size
!  4 pntadr            IO Variable addresses in pool
!  5 pntlen            O  Variable lengths in pool
!  1 pntnam            I  Variable name
!  3 pntnms            O  Allocated variable names
!  0 zzmkpt            O  Allocated address for variable
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: zzmkpt.pf,v $
! Revision 1.3  1999/03/15  15:52:38  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:03:47  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:58  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  ZZMKPT

   CHARACTER PNTNMS(*)*(*)
   INTEGER   PNTADR(*)
   INTEGER   PNTLEN(*)

   CHARACTER PNTNAM*(*)
   INTEGER   LENGTH&
   &,NPNTRS&
   &,MXPNTS&
   &,PLSIZE&
   &,OLDPNT

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
