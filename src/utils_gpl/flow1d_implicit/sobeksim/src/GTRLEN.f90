FUNCTION GTRLEN (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTRLEN (GeT Real variable LENgth)
!
! Module description: Report length of variable in RPool
!
!                     This function reports the length of a real vari-
!                     able allocated in the RPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtrlen            O  Length of real variable
!  1 pntnam            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! zzgtln  ZZ GeT LeNgth
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gtrlen.pf,v $
! Revision 1.2  1995/05/30  07:03:32  hoeks_a
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
   INTEGER  GTRLEN

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTLN
   EXTERNAL  ZZGTLN

   GTRLEN = ZZGTLN (PNTNAM, RPNTRS, RLENGT, NRPNTR, ROLDPT)

END
