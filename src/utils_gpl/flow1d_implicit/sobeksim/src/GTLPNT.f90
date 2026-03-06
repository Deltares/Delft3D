FUNCTION GTLPNT (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTLPNT (GeT Logical variable PoiNTer)
!
! Module description: Report address of variable in LPool
!
!                     This function reports the address of a logical
!                     variable allocated in the LPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtlpnt            O  Address of logical variable
!  1 pntnam            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! zzgtpt  ZZ GeT PoinTer
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gtlpnt.pf,v $
! Revision 1.2  1995/05/30  07:03:31  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:44  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  GTLPNT

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTPT
   EXTERNAL  ZZGTPT

   GTLPNT = ZZGTPT (PNTNAM, LPNTRS, LADDRS, NLPNTR, LOLDPT)

END
