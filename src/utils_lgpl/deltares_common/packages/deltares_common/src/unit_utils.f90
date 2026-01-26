!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, 'Delft3D',
!  'D-Flow Flexible Mesh' and 'Deltares' are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!> Module for handling units.
module m_unit_utils
   use MessageHandling, only: IdLen

   implicit none
   private

   public :: is_correct_unit

   type t_unit_category
      character(len=IdLen) :: category !< Category of the unit, e.g., 'velocity', 'wind_direction'
      character(len=IdLen), dimension(:), allocatable :: units !< Unit string, e.g., 'm/s', 'degrees'
   end type t_unit_category

   type(t_unit_category), dimension(:), allocatable :: available_units
   
   contains
   !> Scan a unit string and return its type and correctness.
   function is_correct_unit(unit_category, unit_string) result(is_correct)
      use string_module, only: strcmpi, str_lower
      
      character(len=*), intent(in) :: unit_category !< Input unit type, e.g., 'velocity'
      character(len=*), intent(in) :: unit_string !< unit string, e.g., 'm/s'
      logical :: is_correct !< Flag indicating if the input unit was recognized
      
      type(t_unit_category), dimension(:), allocatable :: category
      character(len=IdLen), dimension(:), allocatable :: unit
      
      if (.not. allocated(available_units)) then
         call set_available_units()
      end if
      is_correct = .false.

      category = pack(available_units, strcmpi(available_units%category, unit_category))
      if (size(category) ==1) then
         unit = pack(category(1)%units, strcmpi(category(1)%units, unit_string))
         if (size(unit)==1) then
            is_correct = .true.
         end if 
      end if
   end function is_correct_unit
   
   !> Set the available units for different categories.
   !> The check 
   subroutine set_available_units()
       available_units = [ &
          t_unit_category('velocity', ['m/s', 'm s-1', 'ms-1', 'meter per second']), &
          t_unit_category('from_direction', ['degrees', 'deg', 'degree', 'degreen']) &
       ]
   end subroutine set_available_units   

end module m_unit_utils
