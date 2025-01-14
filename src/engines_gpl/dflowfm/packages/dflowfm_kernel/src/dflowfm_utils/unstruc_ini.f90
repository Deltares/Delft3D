!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module unstruc_ini
!! Some basic routines for reading an INI file.
!! Most work is done in startup and model modules.

   use messagehandling, only: err
   use properties

   implicit none
   private ! Prevent used modules from being exported

   public :: get_req_string, get_req_integer, get_req_integers, get_req_double
contains

!> Local error checker for get_req_*() subroutines.
   subroutine unstruc_ini_error_handler(chapter, prop_key, success)
      implicit none
      character(len=*), intent(in) :: chapter !< The chapter which was attempted to be accessed.
      character(len=*), intent(in) :: prop_key !< The property key which was attempted to be accessed.
      logical, intent(in) :: success !< Succes status of a prop_get/set() call.

      if (.not. success) then
         call err('ERROR READING INI-FILE, RESTORE CORRECT FILE OR CALL Deltares.', &
                  'NO VALUE FOUND FOR: ', '['//trim(chapter)//'] '//trim(prop_key))
      end if

   end subroutine unstruc_ini_error_handler

!> Reads the value for a string variable from a propery tree.
!! When not found, and error is logged and program stops.
   subroutine get_req_string(prop_ptr, chapter, key, value)
      type(tree_data), pointer, intent(in) :: prop_ptr
      character(*), intent(in) :: chapter
      character(*), intent(in) :: key
      character(*), intent(out) :: value

      logical :: success

      call prop_get(prop_ptr, chapter, key, value, success)
      call unstruc_ini_error_handler(chapter, key, success)

   end subroutine get_req_string

!> Reads the value for an integer variable from a propery tree.
!! When not found, and error is logged and program stops.
   subroutine get_req_integer(prop_ptr, chapter, key, value)
      type(tree_data), pointer, intent(in) :: prop_ptr
      character(*), intent(in) :: chapter
      character(*), intent(in) :: key
      integer, intent(out) :: value

      logical :: success

      call prop_get(prop_ptr, chapter, key, value, success)
      call unstruc_ini_error_handler(chapter, key, success)

   end subroutine get_req_integer

!> Reads the value for an integer-list variable from a propery tree.
!! When not found, and error is logged and program stops.
   subroutine get_req_integers(prop_ptr, chapter, key, value, valuelength)
      type(tree_data), pointer, intent(in) :: prop_ptr
      character(*), intent(in) :: chapter
      character(*), intent(in) :: key
      integer, dimension(:), intent(out) :: value
      integer, intent(in) :: valuelength

      logical :: success

      call prop_get(prop_ptr, chapter, key, value, valuelength, success)
      call unstruc_ini_error_handler(chapter, key, success)

   end subroutine get_req_integers

!> Reads the value for a real(kind=dp) variable from a propery tree.
!! When not found, and error is logged and program stops.
   subroutine get_req_double(prop_ptr, chapter, key, value)
      use precision, only: dp
      type(tree_data), pointer, intent(in) :: prop_ptr
      character(*), intent(in) :: chapter
      character(*), intent(in) :: key
      real(kind=dp), intent(out) :: value

      logical :: success

      call prop_get(prop_ptr, chapter, key, value, success)
      call unstruc_ini_error_handler(chapter, key, success)

   end subroutine get_req_double

end module unstruc_ini
