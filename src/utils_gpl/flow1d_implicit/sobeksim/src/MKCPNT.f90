FUNCTION MKCPNT (PNTNAM, LENGTH)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             MKCPNT (MaKe Character PoiNTer)
!
! Module description: Create address for variable in CPool
!
!                     This function creates an address for a character
!                     variable in the CPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 length            P  -
!  0 mkcpnt            O  Allocated addres for character variable
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
! $Log: mkcpnt.pf,v $
! Revision 1.2  1995/05/30  07:03:37  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:49  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  MKCPNT

   CHARACTER PNTNAM*(*)
   INTEGER   LENGTH

   include '..\include\pointrs.i'
   include '..\include\mempool.i'

   INTEGER   ZZMKPT
   EXTERNAL  ZZMKPT

   MKCPNT = ZZMKPT (PNTNAM, LENGTH, CPNTRS, CADDRS, CLENGT,&
   &NCPNTR, MXCPNT, CPSIZE, COLDPT)

END
