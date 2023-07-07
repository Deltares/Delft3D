!!  Copyright (C)  Stichting Deltares, 2021-2023.
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

      subroutine dhnlun(istart,lun)

!     function : gives next free unit number, starting at istart till istart + 98

!     declaration of the arguments

      integer               , intent(in)    :: istart       ! start looking from here
      integer               , intent(out)   :: lun          ! next free unit number

!     local declaration

      integer                               :: ilun         ! loop counter
      logical                               :: lopen        ! opened indicator

      lun = 0
      do ilun = istart, istart + 1000
         inquire(ilun,opened=lopen)
         if ( .not. lopen ) then
            lun = ilun
            exit
         endif
      enddo

      return
      end
