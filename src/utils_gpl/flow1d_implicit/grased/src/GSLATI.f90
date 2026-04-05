subroutine gslati(lathic ,dunehe ,deffec ,redfac ,defmin)

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gslati.F,v $
! Revision 1.3  1996/06/07  11:56:31  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:12:32  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment calculate effective transport LAyer ThIckness

!     Declaration of parameters
!
   integer    lathic
   real       dunehe ,deffec ,redfac ,defmin

!
!     Declaration of local parameters and constants
!
!                Layer thickness options
!                Ribberink  Karim & Kennedy
   integer    layrib    ,laykar
   parameter (layrib=1  ,laykar=2)

   if (lathic .eq. layrib) then

!        Thickness according to Ribberink

      call gslari (dunehe ,deffec ,redfac ,defmin)

   else if (lathic .eq. laykar) then

!        Thickness according to Karim and Kennedy

   endif

end
