FUNCTION GTRPNT (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTRPNT (GeT Real variable PoiNTer)
!
! Module description: Report address of variable in RPool
!
!                     This function reports the address of a real vari-
!                     able allocated in the RPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtrpnt            O  Address of real variable
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
! $Log: gtrpnt.pf,v $
! Revision 1.2  1995/05/30  07:03:33  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:45  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  GTRPNT

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTPT
   EXTERNAL  ZZGTPT

   GTRPNT = ZZGTPT (PNTNAM, RPNTRS, RADDRS, NRPNTR, ROLDPT)

END
