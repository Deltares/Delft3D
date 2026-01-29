subroutine FLAREI (hact  ,htop  ,daext ,overlp,delA  ,iart  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAREI (FLow correction for ARea Extra Initial)
!
! Module description: The initial calculated flow or total area must be
!                     corrected with the extra area.
!
!                     Depending on the option the area is calculated in
!                     the concerning grid point.
!                     As well as, both "artop" and the extra area are
!                     determined.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 daext             I  Extra area (flow or total).
!  5 dela              O  Computed extra area depending on option.
!  1 hact              I  Actual water level at gridpoint in branch.
!  2 htop              I  top-level
!  6 iart              O  Status-variable indicating rising or falling
!                         water level.
!  4 overlp            I  adaptation height.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flarei.pf,v $
! Revision 1.4  1999/03/15  15:49:24  kuipe_j
! tabs removed
!
! Revision 1.3  1996/04/12  13:03:35  kuipe_j
! headers, minor changes
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  iart
   real     hact  ,htop  ,daext ,overlp,delA
!
!     Declaration of local variables:
!
   real htopp
!
   htopp = htop + overlp
!
   if (hact .gt. htopp) then
      iart  = 0
      delA  = daext
   else
      iart  = 1
      delA  = 0.
   endif
!
end
