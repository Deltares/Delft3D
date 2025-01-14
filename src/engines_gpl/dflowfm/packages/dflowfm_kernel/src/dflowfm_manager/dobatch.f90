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

module m_dobatch

   implicit none

   private

   public :: dobatch

contains

   subroutine dobatch() !
      use precision, only: dp
      use m_flow
      use m_flowgeom
      use unstruc_api, only: api_loadmodel, flow

      integer :: k, ierr, mout, km(100)
      real(kind=dp) :: q30, q31, q32, q40, q41, q42, dt_max

      open (newunit=mout, file='tst.out')
      write (mout, '(a)') ' kmx     q30     q40    q31     q41     q32    q42  '

      km(1) = 1
      km(2) = 2
      km(3) = 3
      km(4) = 5
      km(5) = 8
      km(6) = 16
      km(7) = 32
      km(8) = 64
      km(9) = 128
      km(10) = 256
      km(11) = 512
      km(12) = 1024

      do k = 2, 12

         call api_loadmodel('tst.mdu'); kmx = km(k); iturbulencemodel = 3; jaustarint = 0; if (k > 10) dt_max = 1d0
         ierr = flow(); q30 = q1(1) / 47.434

         call api_loadmodel('tst.mdu'); kmx = km(k); iturbulencemodel = 4; jaustarint = 0; if (k > 10) dt_max = 1d0
         ierr = flow(); q40 = q1(1) / 47.434

         call api_loadmodel('tst.mdu'); kmx = km(k); iturbulencemodel = 3; jaustarint = 1; if (k > 10) dt_max = 1d0
         ierr = flow(); q31 = q1(1) / 47.434

         call api_loadmodel('tst.mdu'); kmx = km(k); iturbulencemodel = 4; jaustarint = 1; if (k > 10) dt_max = 1d0
         ierr = flow(); q41 = q1(1) / 47.434

         call api_loadmodel('tst.mdu'); kmx = km(k); iturbulencemodel = 3; jaustarint = 2; if (k > 10) dt_max = 1d0
         ierr = flow(); q32 = q1(1) / 47.434

         call api_loadmodel('tst.mdu'); kmx = km(k); iturbulencemodel = 4; jaustarint = 2; if (k > 10) dt_max = 1d0
         ierr = flow(); q42 = q1(1) / 47.434

         write (mout, '(i8,6F8.3)') kmx, q30, q40, q31, q41, q32, q42
      end do
      close (mout)
   end subroutine dobatch

end module m_dobatch
