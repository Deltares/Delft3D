SUBROUTINE INIMEM ()

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             INIMEM (INItialise MEMory)
!
! Module description: Initialise memory
!
!                     Calling this routine will clear the memory pools
!                     and deletes all names from the allocated name
!                     tables.
!
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: inimem.pf,v $
! Revision 1.2  1995/05/30  07:03:34  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:46  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\pointrs.i'

   NCPNTR = 0
   NDPNTR = 0
   NIPNTR = 0
   NRPNTR = 0
   NLPNTR = 0

   COLDPT = 1
   DOLDPT = 1
   IOLDPT = 1
   ROLDPT = 1
   LOLDPT = 1

   CLENGT(1) = 0
   DLENGT(1) = 0
   ILENGT(1) = 0
   RLENGT(1) = 0
   LLENGT(1) = 0

   CADDRS(1) = 1
   DADDRS(1) = 1
   IADDRS(1) = 1
   RADDRS(1) = 1
   LADDRS(1) = 1

END
