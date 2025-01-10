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
module m_read_arc_info_block
   implicit none
   private
   public :: readarcinfoblock
contains
   subroutine readarcinfoblock(MINP, D, MC, NC, RMIS)
      use m_missing, only: dmiss
      use precision, only: dp
      use m_filez, only: readerror, doclose, eoferror

      integer :: i
      integer :: j
      integer :: mc
      integer :: minp
      integer :: nc
      real(kind=dp) :: rmis
      real(kind=dp) :: D(MC, NC)
      character(len=16) :: tex

      do J = NC, 1, -1
         read (minp, *, err=101, end=100) (D(I, J), I=1, MC)
      end do
      do i = 1, MC
         do j = 1, NC
            if (D(I, J) == RMIS) D(I, J) = dmiss
         end do
      end do
      call doclose(minp)
      return

100   continue
      call EOFERROR(minp)
101   continue
      write (tex, '(2I8)') I, J
      call READERROR('ERROR READING ARC-INFO BLOCK IN COLNR, ROWNR :', tex, minp)
      return
   end subroutine readarcinfoblock
end module m_read_arc_info_block
