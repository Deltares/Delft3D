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

!> smooth structured sample data and put it in zss(1,:,:)
!!    D u/ D t = div grad u
module m_smooth_samples

   implicit none

   private

   public :: smooth_samples

contains

   subroutine smooth_samples(MXSAM, MYSAM, NS, NDIM, Nsamplesmooth, zs, zss)
      use precision, only: dp
      use m_missing, only: dmiss
      use m_readyy, only: readyy

      integer, intent(in) :: MXSAM, MYSAM !< structured block sizes (>0) or not structured (0)
      integer, intent(in) :: NS !< number of samples
      integer, intent(in) :: NDIM !< number of variable per sample in zss
      integer, intent(in) :: Nsamplesmooth !< number of smoothing iterations

      real(kind=dp), dimension(Ns), intent(in) :: zs !< sample input variables, dim(NS)
      real(kind=dp), dimension(NDIM, MXSAM, MYSAM), intent(inout) :: zss !< sample output variables, dim(NDIM,MXSAM,MYSAM), only first component will be smoothed

      real(kind=dp), dimension(:, :), allocatable :: zsdum

      integer :: iter, i, j
      real(kind=dp) :: c0, ciL, ciR, cjL, cjR, af

      integer :: ierror

      real(kind=dp), parameter :: sigma = 0.5_dp

      ierror = 1

!  check if samples are structured
      if (MXSAM * MYSAM /= NS) then
         goto 1234
      end if

!  allocate
      allocate (zsdum(MXSAM, MYSAM))

!  initialize zss(1,:,:)
      do i = 1, MXSAM
         do j = 1, MYSAM
            zss(1, i, j) = zs(i + MXSAM * (j - 1))
         end do
      end do

      call readyy('Smoothing samples', 0.0_dp)

!  Elliptic smoothing
      do iter = 1, Nsamplesmooth
         af = real(iter - 1, kind=dp) / real(max(Nsamplesmooth - 1, 1), kind=dp)
         call readyy('Smoothing samples', af)

!     copy zss(1,:,:) to zsdum
         do j = 1, MYSAM
            do i = 1, MXSAM
               zsdum(i, j) = zss(1, i, j)
            end do
         end do

         do j = 2, MYSAM - 1 ! inner nodes only
            do i = 2, MXSAM - 1 ! inner nodes only
               if (zsdum(i, j) == DMISS) then
                  cycle
               end if

!           compute weights
               ciL = 1.0_dp
               ciR = 1.0_dp
               cjL = 1.0_dp
               cjR = 1.0_dp
               if (zsdum(i - 1, j) == DMISS) then
                  ciL = 0.0_dp
               end if
               if (zsdum(i + 1, j) == DMISS) then
                  ciR = 0.0_dp
               end if
               if (zsdum(i, j - 1) == DMISS) then
                  cjL = 0.0_dp
               end if
               if (zsdum(i, j + 1) == DMISS) then
                  cjR = 0.0_dp
               end if

               if (ciL * ciR * cjL * cjR == 0.0_dp) then
                  cycle ! inner samples only
               end if

               c0 = ciL + ciR + cjL + cjR
               if (abs(c0) < 0.5_dp) then
                  cycle
               end if

               zss(1, i, j) = (1.0_dp - sigma) * zsdum(i, j) + &
                              sigma * ( &
                              ciL * zsdum(i - 1, j) + &
                              ciR * zsdum(i + 1, j) + &
                              cjL * zsdum(i, j - 1) + &
                              cjR * zsdum(i, j + 1) &
                              ) / c0
            end do
         end do
      end do

      ierror = 0
!   Nsamplesmooth_last = Nsamplesmooth

      call readyy('Smoothing samples', -1.0_dp)

1234  continue

!  deallocate
      if (allocated(zsdum)) then
         deallocate (zsdum)
      end if

      return
   end subroutine smooth_samples

end module m_smooth_samples
