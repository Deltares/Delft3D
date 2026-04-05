function FLIHIS(ind)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLIHIS (FLow Index renumbering structure HIStory)
!
! Module description: Index renumbering of controll parameters from
!                     array contrl to array strhis.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 flihis            O  Function value of function FLIHIS (=index for
!                         array strhis)
!  1 ind               I  index in array contrl.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flihis.pf,v $
! Revision 1.2  1995/09/22  10:01:43  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!     Function declaration:
!
   integer FLIHIS
!
!     Declaration of parameter
!
   integer ind
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
   if      ( ind .eq. ccpcrh ) then
      FLIHIS = 2
   else if ( ind .eq. ccpcrw ) then
      FLIHIS = 3
   else if ( ind .eq. ccpgat ) then
      FLIHIS = 1
   endif
!
end
