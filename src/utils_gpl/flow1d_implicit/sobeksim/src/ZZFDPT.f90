FUNCTION ZZFDPT (PNTNAM, PNTNMS, NPNTRS, OLDPNT)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             ZZFDPT (ZZ FinD PoinTer)
!
! Module description: This function locates the address of a variable
!
!                     This function is called from several user func-
!                     tions to find a variable name in a particular
!                     memory pool.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 npntrs            I  Number of variable names
!  4 oldpnt            IO Last index found of previous call
!  1 pntnam            I  Variable name
!  2 pntnms            I  Allocated variable names
!  0 zzfdpt            O  Address of variable
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: zzfdpt.pf,v $
! Revision 1.3  1999/03/15  15:52:37  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:03:44  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:56  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER   ZZFDPT

   CHARACTER PNTNMS(*)*(*)

   CHARACTER PNTNAM*(*)
   INTEGER   NPNTRS&
   &,OLDPNT

   INTEGER   NLOOP

   DO 100 NLOOP = 1, NPNTRS
      OLDPNT = MOD(OLDPNT,NPNTRS) + 1
      IF (PNTNAM .EQ. PNTNMS(OLDPNT)) THEN
         ZZFDPT = OLDPNT
         GOTO 900
      ENDIF
100 CONTINUE

   ZZFDPT = -1
   RETURN

900 RETURN

END
