function SORPAR ( par, ix )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SORPAR (SObek Real PARameter)
!
! Module description: Extract real parameter from run array variable
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 ix                I  Index in array ...run or ...par
!  1 par               I  Parameter array (...par)
!  0 sorpar            O  Function value.
!=======================================================================
!

!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sorpar.pf,v $
! Revision 1.4  1995/09/22  10:04:30  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:04  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:58  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:24  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   real    par(*)
   integer ix
   real    sorpar
!
!     Extract
!
   sorpar =  par(ix)

   return
end
