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

module m_filez

   implicit none

   private

   public :: oldfil, readandchecknextrecord, readerror, thisisanumber, &
             zoekdouble, zoekinteger, zoekja, zoekopt, doclose, numbersonline, newfil, message, &
             newnewfil, error, eoferror

   interface
      module subroutine oldfil(minp, filename)
         implicit none
         integer, intent(out) :: minp !< New file pointer to opened file. 0 in case of some error.
         character(*), intent(in) :: filename !< Name of the file to open.
      end subroutine oldfil

      module subroutine readandchecknextrecord(minp, rec, text, ja)
         implicit none
         integer, intent(in) :: minp
         character(len=*), intent(out) :: rec
         character(len=*), intent(in) :: text
         integer, intent(out) :: ja
      end subroutine readandchecknextrecord

      module subroutine readerror(w1, w2, minp)
         implicit none
         character(len=*), intent(in) :: w1
         character(len=*), intent(in) :: w2
         integer, intent(in) :: minp
      end subroutine readerror

      module function thisisanumber(rec)
         implicit none
         logical :: thisisanumber
         character(len=*), intent(in) :: rec
      end function thisisanumber

      module subroutine zoekdouble(minp, key, val, ja)
         use precision, only: dp
         implicit none
         integer, intent(in) :: minp !< File pointer
         character(*), intent(in) :: key
         real(kind=dp), intent(out) :: val !<
         integer, intent(out) :: ja !< Whether key was found or not.
      end subroutine zoekdouble

      module subroutine zoekinteger(minp, key, val, ja)
         implicit none
         integer, intent(in) :: minp !< File pointer
         character(*), intent(in) :: key
         integer, intent(out) :: val !<
         integer, intent(out) :: ja !< Whether key was found or not.
      end subroutine zoekinteger

      module subroutine zoekja(minp, rec, text, ja)
         implicit none
         integer, intent(in) :: minp
         character(len=*), intent(out) :: rec
         character(len=*), intent(in) :: text
         integer, intent(out) :: ja
      end subroutine zoekja

      module subroutine zoekopt(minp, value, key, ja)
         implicit none
         integer, intent(out) :: ja !< Whether key was found or not.
         integer, intent(in) :: minp !< File pointer
         character(*), intent(out) :: value !< value behind '=' character.
         character(*), intent(in) :: key !<
      end subroutine zoekopt

      module subroutine doclose(minp)
         implicit none
         integer, intent(inout) :: minp !< File unit of a (probably open) file. Will be set to 0 upon return.
      end subroutine doclose

      module subroutine message(w1, w2, w3)
         implicit none
         character(len=*), intent(in) :: w1
         character(len=*), intent(in) :: w2
         character(len=*), intent(in) :: w3
      end subroutine message

      module function numbersonline(rec)
         implicit none
         integer :: numbersonline
         character(len=*), intent(in) :: rec
      end function numbersonline

      module subroutine newfil(minp, filename)
         implicit none
         integer, intent(out) :: minp !< New file pointer to opened file. 0 in case of some error.
         character(*), intent(in) :: filename !< Name of the file to open.
      end subroutine newfil

      module subroutine newnewfil(minp, filename)
         implicit none
         integer, intent(out) :: minp !< New file pointer to opened file.
         character(*), intent(in) :: filename !< Name of the file to open.
      end subroutine newnewfil

      module subroutine error(w1, w2, w3)
         implicit none
         character(len=*), intent(in) :: w1
         character(len=*), intent(in) :: w2
         character(len=*), intent(in) :: w3
      end subroutine error

      module subroutine eoferror(minp)
         implicit none
         integer, intent(in) :: minp
      end subroutine eoferror

   end interface

end module m_filez
