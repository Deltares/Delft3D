FUNCTION MKRPNT (PNTNAM, LENGTH)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             MKRPNT (MaKe Real variable PoiNTer)
!
! Module description: Create address for variable in RPool
!
!                     This function creates an address for a real vari-
!                     able in the RPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 length            P  -
!  0 mkrpnt            O  Allocated addres for real variable
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
! $Log: mkrpnt.pf,v $
! Revision 1.2  1995/05/30  07:03:40  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:52  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  MKRPNT

   CHARACTER PNTNAM*(*)
   INTEGER   LENGTH

   include '..\include\pointrs.i'
   include '..\include\mempool.i'

   INTEGER   ZZMKPT
   EXTERNAL  ZZMKPT

   MKRPNT = ZZMKPT (PNTNAM, LENGTH, RPNTRS, RADDRS, RLENGT,&
   &NRPNTR, MXRPNT, RPSIZE, ROLDPT)

END
