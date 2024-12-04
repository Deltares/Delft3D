FUNCTION ZZGTLN (PNTNAM, PNTNMS, PNTLEN, NPNTRS, OLDPTR)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             ZZGTLN (ZZ GeT LeNgth)
!
! Module description: This function determines the length of a variable
!
!                     This function is called from several user func-
!                     tions to find the length of variable name in a
!                     particular memory pool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 npntrs            P  -
!  5 oldptr            P  -
!  3 pntlen            I  Variable lengths in pool
!  1 pntnam            P  -
!  2 pntnms            P  -
!  0 zzgtln            O  Length of variable
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! zzfdpt  ZZ FinD PoinTer
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: zzgtln.pf,v $
! Revision 1.2  1995/05/30  07:03:45  hoeks_a
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
   INTEGER  ZZGTLN

   CHARACTER PNTNMS(*)*(*)
   INTEGER   PNTLEN(*)

   CHARACTER PNTNAM*(*)
   INTEGER   NPNTRS&
   &,OLDPTR

   INTEGER   PNTR

   INTEGER   ZZFDPT
   EXTERNAL  ZZFDPT

   PNTR = ZZFDPT (PNTNAM, PNTNMS, NPNTRS, OLDPTR)

   IF (PNTR .NE. -1) THEN
      ZZGTLN = PNTLEN(PNTR)

   ELSE
      ZZGTLN = -1

   ENDIF

END
