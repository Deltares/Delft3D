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
      module m_file_unit_number

      implicit none

      contains

      subroutine create_new_file_unit_number(start_index, file_unit_number)
!     function : sets next free unit number, starting at start_index till start_index + 98

!     declaration of the arguments

      integer, intent(in)    :: start_index        !< start looking from here
      integer, intent(out)   :: file_unit_number   !< next free unit number

!     local declaration

      integer :: ilun   !< loop counter
      logical :: lopen  !< opened indicator

      file_unit_number = 0
      do ilun = start_index, start_index + 100000
         inquire(ilun,opened=lopen)
         if ( .not. lopen ) then
            file_unit_number = ilun
            exit
         endif
      enddo

      return
      end
      end module m_file_unit_number
