FUNCTION GTCLEN (PNTNAM)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             GTCLEN (GeT Character variable LENgth)
!
! Module description: Report length of variable in CPool
!
!                     This function reports the length of a character
!                     variable allocated in the CPool
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 gtclen            O  Length of character variable
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
! $Log: gtclen.pf,v $
! Revision 1.2  1995/05/30  07:03:25  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:38  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:01  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   INTEGER  GTCLEN

   CHARACTER PNTNAM*(*)

   include '..\include\pointrs.i'

   INTEGER   ZZGTLN
   EXTERNAL  ZZGTLN

   GTCLEN = ZZGTLN (PNTNAM, CPNTRS, CLENGT, NCPNTR, COLDPT)

END
