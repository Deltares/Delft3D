subroutine gslari (dunehe ,deffec, redfac ,defmin)

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gslari.F,v $
! Revision 1.3  1996/06/07  11:56:29  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:12:31  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Graded Sediment calculate effective transport LAyer thickness
!     according to Ribberink

!     Declaration of parameters
!
   real       dunehe ,deffec ,redfac ,defmin
!
!     Declaration of local parameters
!
   real       epseff
   parameter (epseff=.5)

   deffec = max(epseff * dunehe, defmin*redfac)

end
