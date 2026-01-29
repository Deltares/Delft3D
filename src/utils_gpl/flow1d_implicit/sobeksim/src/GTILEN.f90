FUNCTION GTILEN (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTILEN (GeT Integer variable LENgth)
!
! Module description: Report length of variable in IPool
!
!                     This function reports the length of a logical
!                     variable allocated in the IPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtilen            O  Length of integer variable
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
! $Log: gtilen.pf,v $
! Revision 1.2  1995/05/30  07:03:29  hoeks_a
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
   INTEGER  GTILEN

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTLN
   EXTERNAL  ZZGTLN

   GTILEN = ZZGTLN (PNTNAM, IPNTRS, ILENGT, NIPNTR, IOLDPT)

END
