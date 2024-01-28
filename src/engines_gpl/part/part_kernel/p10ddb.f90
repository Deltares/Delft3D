!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_p10ddb

implicit none

contains


      subroutine p10ddb ( nconn  , conn   , n0     , ddshift, np     ,      &
     &                    mp     , xp     , yp     )

!     Deltares Software Centre

!>/File
!>             Lets particle cross domain boundary
!>
!>             n0 is the array entry in conn. There the following is found:
!>             - the new (np,mp) in the grid
!>             - the information to determine new xp,yp in the new cell
!>             - ddshift 1 indicates a cross in m direction
!>             - ddshift 2 indicates a cross in n direction
!>             Both are possible for inner corners

!     system administration : Antoon Koster

!     Created               ! ????     ???? by Jan van Beek

!     modified              : October  2011 by Leo Postma: redesign

!     logical unit numbers  :

      use m_waq_precision       ! single and double precision
      use typos           ! the derived types
      use timers          ! performance timers

      implicit none

!     Arguments

!     kind           function         name               description

      integer ( int_wp ), intent(in   ) :: nconn            !< dimension of conn
      type (pnt)   , intent(in   ) :: conn  ( nconn )  !< array with dd shift entries
      integer ( int_wp ), intent(in   ) :: n0               !< entry in conn for this shift
      integer ( int_wp ), intent(in   ) :: ddshift          !< m (1) or n (2) shift
      integer ( int_wp ), intent(  out) :: np               !< new n of particle
      integer ( int_wp ), intent(  out) :: mp               !< new m of particle
      real    ( real_wp), intent(inout) :: xp               !< new x of particle in cell
      real    ( real_wp), intent(inout) :: yp               !< new y of particle in cell

!     Locals

      integer(4)  ithndl                       ! handle to time this subroutine
      data        ithndl / 0 /

      if ( timon ) call timstrt( "p10ddb", ithndl )

      select case ( ddshift )
         case ( 1 )
            np = conn(n0)%n1
            mp = conn(n0)%m1
            if ( conn(n0)%f1 .gt. 1 ) then            ! y changes by
               if ( conn(n0)%i1 .eq. 0 ) then         ! refinement
                  np = np + int( yp*conn(n0)%f1 )
                  yp = mod( yp*conn(n0)%f1, 1.0 )
               else                                   ! coarsening
                  yp = ( yp + float(conn(n0)%i1) - 1.0 ) / float( conn(n0)%f1 )
               endif
            endif
         case ( 2 )
            np = conn(n0)%n2
            mp = conn(n0)%m2
            if ( conn(n0)%f2 .gt. 1 ) then
               if ( conn(n0)%i2 .eq. 0 ) then
                  mp = mp + int( xp*conn(n0)%f2 )
                  xp = mod( xp*conn(n0)%f2, 1.0 )
               else
                  xp = ( xp + float(conn(n0)%i2) - 1.0 ) / float( conn(n0)%f2 )
               endif
            endif
      end select

      if ( timon ) call timstop( ithndl )

      return
      end

end module m_p10ddb
