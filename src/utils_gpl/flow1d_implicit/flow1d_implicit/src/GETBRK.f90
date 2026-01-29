DOUBLE PRECISION FUNCTION GETBRK()

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         Kian Tan
!
! Module:             GETBRK (GET BReaKdown parameter tolerance)
!
! Module description: Get breakdown parameter tolerance; for the test
!                     routine, set to machine precision.
!
!-----------------------------------------------------------------------
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: getbrk.pf,v $
! Revision 1.3  1995/10/18  08:59:39  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/09/22  10:03:00  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!     Get breakdown parameter tolerance; for the test  routine,
!     set to machine precision.
!
   DOUBLE PRECISION   EPS,  DLAMCH
!
   EPS = DLAMCH('EPS')
   GETBRK = EPS**2
!
   RETURN
!
END
