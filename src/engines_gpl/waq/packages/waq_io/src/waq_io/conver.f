!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine conver ( ibrk  , nobrk , ifact , dtflg , dtflg3 )

!     Deltares Software Centre

!>\File
!>        Conversion of a relative integer DDHHMMSS or YYDDDHH time to seconds
!>
!>        DDDDHHMMSS allows up to 2146 days or some 5.8 years\n
!>        YYDDDHH allows more than needed (NB: a year is 365 days !)\n
!>        If date is false, result is multiplied by ifact.\n
!>        Absolute offset time should be chosen such that all relative
!>        times fit into 68 years from the absolute offset !


!     Logical units : none

      use timers       !   performance timers

      implicit none

!     Parameters

!     kind           function         name             Descriptipon

      integer   (4), intent(in   ) :: nobrk          !< number of breakpoints
      integer   (4), intent(inout) :: ibrk (nobrk)   !< breakpoint to convert
      integer   (4), intent(in   ) :: ifact          !< factor between time scales
      logical      , intent(in   ) :: dtflg          !< if true then 'date'-format
      logical      , intent(in   ) :: dtflg3         !< if true then YYDDDHH

!     Local

      integer   (4) isec    ! seconds
      integer   (4) imin    ! minutes
      integer   (4) ihour   ! hours
      integer   (4) iday    ! days
      integer   (4) iyear   ! years
      integer   (4) i       ! loop counter
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "conver", ithndl )

      if ( dtflg ) then
         if ( dtflg3 ) then
            do i = 1 , nobrk
               ihour   = mod (    ibrk(i),100)
               iday    = mod (int(ibrk(i)/100),1000)
               iyear   =      int(ibrk(i)/100000)
               ibrk(i) = 3600*ihour + 86400*iday + 31536000*iyear
            enddo
         else
            do i = 1 , nobrk
               isec    = mod (    ibrk(i),100)
               imin    = mod (int(ibrk(i)/100),100)
               ihour   = mod (int(ibrk(i)/10000),100)
               iday    = int (int(ibrk(i)/1000000))
               ibrk(i) = isec + 60*imin + 3600*ihour + 86400*iday
            enddo
         endif
      else
         if ( ifact .ne. 1 ) then
            do i = 1 , nobrk
               ibrk(i) = ifact * ibrk(i)
            enddo
         endif
      endif

      if (timon) call timstop( ithndl )
      return
      end
