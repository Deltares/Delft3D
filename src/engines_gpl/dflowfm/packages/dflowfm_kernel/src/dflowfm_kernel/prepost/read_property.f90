!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_read_property
   implicit none
   private

   public :: read_property

contains

   !> Reads a key=value entry from a property block and tries to interpret the value.
   !! The (single!) property block should come from an already-parsed .ini file.
   !! The string value is always returned, if found, and an attempt is also made to
   !! parse it into a scalar double, or alternatively to check whether it is an existing file.
   subroutine read_property(prop_ptr, key, strvalue, dblvalue, is_double, typeandid, success, is_required)
      use precision, only: dp
      use properties, only: prop_get
      use messagehandling, only: msgbuf, msg_flush
      use tree_data_types, only: tree_data

      type(TREE_DATA), pointer, intent(in) :: prop_ptr !< Property tree as read from a single .ini block
      character(len=*), intent(in) :: key !< Property key that should be read.
      character(len=*), intent(inout) :: strvalue !< Returned string value for requested property key.
      real(kind=dp), intent(inout) :: dblvalue !< Returned scalar double value for requested property key, IF possible.
      logical, intent(out) :: is_double !< Tells whether the found value could be parsed into a scalar double value.
      character(len=*), intent(in) :: typeandid !< String with type and name, to be used in warning message to be printed if property key not found. Example: "gate 'Maeslant'"
      logical, intent(out) :: success !< Whether value was read successfully or not.
      logical, optional, intent(in) :: is_required !< Whether the property is required (default) or optional.

      real(kind=dp) :: tmpvalue
      integer :: ierr
      logical :: is_required_

      success = .false.
      is_double = .false.

      if (present(is_required)) then
         is_required_ = is_required
      else
         is_required_ = .true.
      end if

      call prop_get(prop_ptr, '', trim(key), strvalue, success)
      if (.not. success .or. len_trim(strvalue) == 0) then
         if (is_required_) then
            write (msgbuf, '(5a)') 'Field ''', trim(key), ''' is missing in ''', trim(typeandid), '''.'
            call msg_flush()
         end if

         goto 888
      else
         ! strvalue is now filled. Check that it does not start with a /
         if (index(strvalue, '/') /= 1) then
            read (strvalue, *, iostat=ierr) tmpvalue
            if (ierr == 0) then
               dblvalue = tmpvalue
               is_double = .true.
            end if
         end if
      end if

      success = .true.
888   continue

   end subroutine read_property
end module m_read_property
