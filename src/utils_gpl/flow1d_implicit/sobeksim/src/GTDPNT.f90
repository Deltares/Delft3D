FUNCTION GTDPNT (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTDPNT (GeT Double precision variable PoiNTer)
!
! Module description: Report address of variable in DPool
!
!                     This function reports the address of a double pre-
!                     cision variable allocated in the DPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtdpnt            O  Address of double precision variable
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
! $Log: gtdpnt.pf,v $
! Revision 1.2  1995/05/30  07:03:28  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:41  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  GTDPNT

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTPT
   EXTERNAL  ZZGTPT

   GTDPNT = ZZGTPT (PNTNAM, DPNTRS, DADDRS, NDPNTR, DOLDPT)

END
