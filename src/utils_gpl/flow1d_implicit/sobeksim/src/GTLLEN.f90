FUNCTION GTLLEN (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTLLEN (GeT Logical variable LENgth)
!
! Module description: Report length of variable in LPool
!
!                     This function reports the length of a logical
!                     variable allocated in the LPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtllen            O  Length of logical variable
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
! $Log: gtllen.pf,v $
! Revision 1.2  1995/05/30  07:03:30  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:43  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  GTLLEN

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTLN
   EXTERNAL  ZZGTLN

   GTLLEN = ZZGTLN (PNTNAM, LPNTRS, LLENGT, NLPNTR, LOLDPT)

END
