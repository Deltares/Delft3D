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

module m_tidysamples

   implicit none

   private

   public :: tidysamples

contains

   subroutine TIDYSAMPLES(XS, YS, ZS, IPSAM, NS, MXSAM, MYSAM)
      use m_rmdouble, only: rmdouble
      use precision, only: dp
      use stdlib_sorting, only: sort_index
      use m_readyy

      integer :: ns
      real(kind=dp) :: XS(NS), YS(NS), ZS(NS) !< sample coordinates
      integer, dimension(NS), intent(out) :: IPSAM !< permutation array (increasing x-coordinate)
      integer, intent(in) :: MXSAM, MYSAM !< structured sample data dimensions (>0) or unstructured (0)
      real(kind=dp), allocatable :: xs_copy(:)

      xs_copy = xs
      call sort_index(xs_copy, IPSAM)

!     remove double/missing samples (non-structured sample data only)
      if (MXSAM * MYSAM /= NS) then
         call READYY(' ', 0.3d0)
         if (NS > 1) call RMDOUBLE(XS, YS, ZS, IPSAM, NS)
      end if

      call READYY(' ', 1d0)

      return
   end

end module m_tidysamples
