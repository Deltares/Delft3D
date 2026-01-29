FUNCTION MKDPNT (PNTNAM, LENGTH)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             MKDPNT (MaKe Double precision variable PoiNTer)
!
! Module description: Create address for variable in DPool
!
!                     This function creates an address for a double pre-
!                     cision variable in the DPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 length            P  -
!  0 mkdpnt            O  Allocated addres for double precision variable
!  1 pntnam            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! zzmkpt  ZZ MaKe PoinTer
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mkdpnt.pf,v $
! Revision 1.2  1995/05/30  07:03:38  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:50  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  MKDPNT

   CHARACTER PNTNAM*(*)
   INTEGER   LENGTH

   include '..\include\pointrs.i'
   include '..\include\mempool.i'

   INTEGER   ZZMKPT
   EXTERNAL  ZZMKPT

   MKDPNT = ZZMKPT (PNTNAM, LENGTH, DPNTRS, DADDRS, DLENGT,&
   &NDPNTR, MXDPNT, DPSIZE, DOLDPT)

END
