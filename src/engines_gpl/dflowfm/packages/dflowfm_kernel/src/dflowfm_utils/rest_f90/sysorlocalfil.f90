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

   subroutine SYSORLOCALFIL(LUNID, FILNAM, MUSTBE)
      use string_module, only: find_first_char
      use unstruc_files
      use m_filez, only: oldfil

      character(len=*), intent(in) :: FILNAM !< Name of file to be opened.
      integer, intent(out) :: lunid !< File unit of the opened file, 0 in case of error.
      integer, intent(in) :: mustbe !< Whether or not (1/0) the file must be checked whether it exists. When 1 and file does not exist, an error is given.

      integer :: istart
      integer :: k1
      integer :: k2
      character FULNAM * 180
      logical JA

      LUNID = 0
      inquire (FILE=FILNAM, EXIST=JA)
      if (JA) then
         call OLDFIL(LUNID, FILNAM)
         write (msgbuf, '(2A)') 'Using Local File ', FILNAM; call msg_flush()
      else

         FULNAM = PATHDI
         ISTART = len_trim(PATHDI) + 1
         write (FULNAM(ISTART:), '(A)') FILNAM
         K1 = find_first_char(FULNAM)
         K2 = len_trim(FULNAM)
         inquire (FILE=FULNAM(K1:K2), EXIST=JA)
         if (JA) then
            call OLDFIL(LUNID, FULNAM)
            call mess(LEVEL_INFO, 'Using Program File ', FULNAM(K1:K2))
         else if (MUSTBE == 1) then
            call mess(LEVEL_ERROR, 'Program File ', FULNAM(K1:K2), ' Not Found')
         end if

      end if

      return
   end

end module m_sysorlocalfil
