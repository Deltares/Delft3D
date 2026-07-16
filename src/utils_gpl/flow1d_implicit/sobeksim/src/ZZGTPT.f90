FUNCTION ZZGTPT (PNTNAM, PNTNMS, PNTADR, NPNTRS, OLDPTR)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             ZZGTPT (ZZ GeT PoinTer)
!
! Module description: This function locates a variable name in a pool
!
!                     This function is called from several user func-
!                     tions to find a variable name in a particular
!                     memory pool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 npntrs            P  -
!  5 oldptr            P  -
!  3 pntadr            I  Variable addresses in pool
!  1 pntnam            P  -
!  2 pntnms            P  -
!  0 zzgtpt            O  Address of variable
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
! $Log: zzgtpt.pf,v $
! Revision 1.3  1996/12/02  10:05:00  kuipe_j
! avoid negative pointers
!
! Revision 1.2  1995/05/30  07:03:46  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:57  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  ZZGTPT

   CHARACTER PNTNMS(*)*(*)
   INTEGER   PNTADR(*)

   CHARACTER PNTNAM*(*)
   INTEGER   NPNTRS&
   &,OLDPTR

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
