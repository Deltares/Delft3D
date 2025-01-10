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

!> reverse indexing of selected polygon
module m_flippo
   use m_get_polstartend, only: get_polstartend

   implicit none

   private

   public :: flippo

contains

   subroutine flippo(ip)
      use precision, only: dp
      use m_polygon

      integer, intent(in) :: ip !< polygon point

      integer :: jpoint, jstart, jend, Num
      integer :: i, j, ierror

      real(kind=dp), dimension(:), allocatable :: xxp, yyp, zzp

      jpoint = 1
      jstart = 1
      jend = NPL
      if (ip == 0) then
         ierror = 0
      else
         ierror = 1

         call get_polstartend(NPL, XPL, YPL, ip, jstart, jend)

         if (jstart <= ip .and. jend >= ip) then
            ierror = 0
         end if
      end if

      if (ierror == 0) then
!      call savepol()
!     allocate
         Num = jend - jstart + 1
         allocate (xxp(Num), yyp(Num), zzp(Num))
         do j = 1, Num
            i = jend - j + 1
            xxp(j) = xpl(i)
            yyp(j) = ypl(i)
            zzp(j) = zpl(i)
         end do
         do i = jstart, jend
!         xpl(i) = xph(jend-i+jstart)
!         ypl(i) = yph(jend-i+jstart)
!         zpl(i) = zph(jend-i+jstart)
            j = i - jstart + 1
            xpl(i) = xxp(j)
            ypl(i) = yyp(j)
            zpl(i) = zzp(j)
         end do
!     deallocate
         deallocate (xxp, yyp, zzp)
      end if

      return
   end subroutine flippo

end module m_flippo
