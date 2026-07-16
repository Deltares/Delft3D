function FLLWST(ncsrel ,cnstrl ,contnr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLLWST (FLow LoWest Structure)
!
! Module description: Determine lowest related structure for a given
!                     controller.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 cnstrl(2,ncsrel)  I  table for controller structure relations.
!                         (1,i) = Controller number of controller
!                                 related to structure at (2,i)
!                         (2,i) = Structure number of structure related
!                                 to controller at (1,i)
!  3 contnr            I  Controller number.
!  0 fllwst            O  Function value of function FLLWST (=Lowest
!                         related structure number for a given
!                         controller).
!  1 ncsrel            I  number of controller structure relations.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fllwst.pf,v $
! Revision 1.3  1996/05/30  09:56:38  kuipe_j
! general structure dlim, controllers
!
! Revision 1.2  1995/09/22  10:01:53  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!     Function declaration:
!
   integer FLLWST
!
!     Declaration of parameters:
!
   integer ncsrel, contnr
   integer cnstrl(2,*)
!
!     Declaration of local variables:
!
   integer irel, strunr
!
!     Determine lowest related structure
!
   strunr = 1000000
   do 10 irel = 1, ncsrel
      if ( cnstrl(1,irel) .eq. contnr ) then
         strunr = min(cnstrl(2,irel), strunr)
      endif
10 continue
!
   FLLWST = strunr
end
