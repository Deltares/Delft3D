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
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> detect ridges and reduce structured sample set
module m_detect_ridges
   use m_prepare_samplehessian, only: prepare_samplehessian

   implicit none

   private

   public :: detect_ridges

contains

   subroutine detect_ridges(jadeleteHessians)
      use precision, only: dp
      use m_deallocate_samplehessian, only: deallocate_samplehessian
      use m_plot_ridges, only: plot_ridges
      use m_samples, only: mxsam, mysam, ns, xs, ys, savesam
      use m_samples_refine, only: nsamplesmooth, zss
      use m_missing, only: dmiss
      use m_comp_sampleDh, only: comp_sampledh
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer, intent(in) :: jadeleteHessians !< delete Hessians upon completion (1) or not (0)

      integer :: i, j, ip
      integer :: ierror, Nsamplesmooth_bak

      real(kind=dp) :: Dh

      ierror = 1

!  store settings
      Nsamplesmooth_bak = Nsamplesmooth

!  default value
      Nsamplesmooth = 4

!  check if the sample set is structured and non-empty
      if (MXSAM * MYSAM /= NS .or. NS == 0) then
         goto 1234
      end if

!  compute sample mesh width
      Dh = min(dbdistance(xs(1), ys(1), xs(2), ys(2), jsferic, jasfer3D, dmiss), dbdistance(xs(1), ys(1), xs(1 + MXSAM), ys(1 + MXSAM), jsferic, jasfer3D, dmiss))

!  store samples
      call savesam()

      call prepare_sampleHessian(ierror)
      if (ierror /= 0) then
         goto 1234
      end if

!  plot ridges
      call plot_ridges(ierror)
!   if ( ierror.ne.0 ) goto 1234

!  remove samples from sample set that are not associated with a ridge
      do i = 1, MXSAM
         do j = 1, MYSAM
            ip = i + (j - 1) * MXSAM

            Dh = comp_sampleDh(i, j)

            if (abs(zss(5, i, j)) > 0.5_dp * Dh .or. zss(4, i, j) > -1.0e-8_dp .or. zss(5, i, j) == DMISS) then
               xs(ip) = DMISS
               ys(ip) = DMISS
!            zs(ip) = DMISS
            end if
         end do
      end do

      ierror = 0
1234  continue

!  restore settings
      Nsamplesmooth = Nsamplesmooth_bak

      if (jadeleteHessians == 1) then
         call deallocate_sampleHessian()
      end if

      return
   end subroutine detect_ridges

end module m_detect_ridges
