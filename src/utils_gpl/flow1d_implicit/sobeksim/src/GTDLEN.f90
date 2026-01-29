FUNCTION GTDLEN (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTDLEN (GeT Double precision variable LENgth)
!
! Module description: Report length of variable in DPool
!
!                     This function reports the length of a double pre-
!                     cision variable allocated in the DPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtdlen            O  Length of double precision variable
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
! $Log: gtdlen.pf,v $
! Revision 1.2  1995/05/30  07:03:27  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:40  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:01  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  GTDLEN

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTLN
   EXTERNAL  ZZGTLN

   GTDLEN = ZZGTLN (PNTNAM, DPNTRS, DLENGT, NDPNTR, DOLDPT)

END
