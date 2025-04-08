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

module m_sysorlocalfil

   implicit none

   private

   public :: sysorlocalfil

contains

   subroutine sysorlocalfil(file_id, file_name, must_exist)
      use string_module, only: find_first_char
      use unstruc_files
      use m_filez, only: oldfil

      character(len=*), intent(in) :: file_name !< Name of file to be opened.
      integer, intent(out) :: file_id !< File unit of the opened file, 0 in case of error.
      integer, intent(in) :: must_exist !< Whether or not (1/0) the file must be checked whether it exists. When 1 and file does not exist, an error is given.

      integer :: k1, k2
      character(:), allocatable :: full_file_name
      logical ja

      file_id = 0
      inquire (file=file_name, exist=ja)
      if (ja) then
         call oldfil(file_id, file_name)
         write (msgbuf, '(2A)') 'Using Local File ', file_name; call msg_flush()
      else
         full_file_name = trim(pathdi)//trim(file_name)
         k1 = find_first_char(full_file_name)
         k2 = len_trim(full_file_name)
         inquire (file=full_file_name(k1:k2), exist=ja)
         if (ja) then
            call oldfil(file_id, full_file_name)
            call mess(level_info, 'Using Program File ', full_file_name(k1:k2))
         else if (must_exist == 1) then
            call mess(level_error, 'Program File ', full_file_name(k1:k2), ' Not Found')
         end if
      end if

   end subroutine sysorlocalfil

end module m_sysorlocalfil
