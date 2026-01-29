FUNCTION GTCPNT (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTCPNT (GeT Character PoiNTer)
!
! Module description: Report address of variable in CPool
!
!                     This function reports the address of a character
!                     variable allocated in the CPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtcpnt            O  Address of character variable
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
! $Log: gtcpnt.pf,v $
! Revision 1.2  1995/05/30  07:03:26  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:39  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:01  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  GTCPNT

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTPT
   EXTERNAL  ZZGTPT

   GTCPNT = ZZGTPT (PNTNAM, CPNTRS, CADDRS, NCPNTR, COLDPT)

END
