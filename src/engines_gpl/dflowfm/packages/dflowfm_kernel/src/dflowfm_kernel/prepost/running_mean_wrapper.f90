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

!
! $URL$
!-------------------------------------------------------------------------------------------------------
module running_mean_wrapper
   use runsum
   use precision
   use MessageHandling
   implicit none

   private
   public :: init_running_mean, update_runsum, init_running_mean_admin, TRunSum
   public :: TRunningMeanMeta, TRunningMean

   type TRunningMeanMeta
      character(len=:), allocatable :: fieldname
      integer :: nx, nd
      real(kind=hp) :: tstart, tstop, ti_fou, tupdate
   end type TRunningMeanMeta

   type TRunningMean
      type(TRunningMeanMeta) :: RMmeta
      type(TRunSum), pointer :: runsum => null()
   end type TRunningMean

   ! internal types/data for holding all initialized instances
   integer, parameter :: firstSize = 10
   integer :: currentSize
   type TPRunningMean
      type(TRunningMean), pointer :: pntRunningMean => null()
   end type TPRunningMean

   type(TPRunningMean), allocatable :: listRunningMeans(:)

contains

   subroutine init_running_mean_admin
      call reallocRM(firstSize)
      currentSize = 0
   end subroutine init_running_mean_admin

   subroutine init_running_mean(running, RMmeta)
      type(TRunningMean), pointer :: running
      type(TRunningMeanMeta), intent(in) :: RMmeta

      integer :: timesteps, i, ifound

      if (size(listRunningMeans) == currentSize) then
         call reallocRM(2 * currentSize, .true.)
      end if

      timesteps = 1 + nint((RMmeta%tstop - RMmeta%tstart) / RMmeta%ti_fou)
      if (RMmeta%ti_fou < 0.0_fp) then
         msgbuf = 'min/max based on running mean only works for FouUpdateStep <> 1'
         call err_flush()
      else if (timesteps < RMmeta%nd) then
         write (msgbuf, '(a,i0,a)') 'not enough time steps for running mean with ', RMmeta%nd, ' points'
         call err_flush()
      end if

      ifound = -1
      do i = 1, currentSize
         if (compareRM(listRunningMeans(i)%pntRunningMean%RMmeta, RMmeta)) then
            ifound = i
            exit
         end if
      end do

      if (ifound > 0) then
         running => listRunningMeans(ifound)%pntRunningMean
      else
         allocate (running)
         allocate (running%runsum)
         call running%runsum%init(RMmeta%nx, RMmeta%nd)
         running%RMmeta = RMmeta
         running%RMmeta%tupdate = -999.0_hp
      end if
      currentSize = currentSize + 1
      listRunningMeans(currentSize)%pntRunningMean => running
   end subroutine init_running_mean

   subroutine update_runsum(running, rarray, ready2use, sum2mean, time0)
      real(kind=hp), intent(in) :: rarray(:), time0
      type(TRunningMean), pointer :: running
      logical, intent(out) :: ready2use
      real(kind=sp), intent(out) :: sum2mean

      integer :: cur_nd

      if (time0 > running%RMmeta%tupdate) then
         call running%runsum%update(rarray)
         running%RMmeta%tupdate = time0
      end if
      cur_nd = size(running%runsum%buffer, dim=2)
      ready2use = (running%runsum%nstep >= cur_nd)
      if (ready2use) then
         sum2mean = 1.0_sp / real(cur_nd, sp)
      else
         sum2mean = 1.0_sp / real(running%runsum%ndx, sp)
      end if
   end subroutine update_runsum

   subroutine reallocRM(nwSize, keep)
      integer, intent(in) :: nwSize
      logical, intent(in), optional :: keep

      logical :: keep_
      integer :: i
      type(TPRunningMean), allocatable :: buffer(:)

      keep_ = .false.
      if (present(keep)) keep_ = keep

      if (keep_) then
         allocate (buffer(currentSize))
         do i = 1, currentSize
            buffer(i)%pntRunningMean => listRunningMeans(i)%pntRunningMean
         end do
      end if

      if (allocated(listRunningMeans)) then
         if (size(listRunningMeans) == nwSize) then
            return
         end if
         deallocate (listRunningMeans)
      end if
      allocate (listRunningMeans(nwSize))

      if (keep_) then
         do i = 1, currentSize
            listRunningMeans(i)%pntRunningMean => buffer(i)%pntRunningMean
         end do
      end if

   end subroutine reallocRM

   logical function compareRM(RM1, RM2)
      type(TRunningMeanMeta), intent(in) :: RM1, RM2

      compareRM = &
         RM1%fieldname == RM2%fieldname .and. &
         RM1%nd == RM2%nd .and. &
         RM1%tstart == RM2%tstart .and. &
         RM1%tstop == RM2%tstop
   end function compareRM
end module running_mean_wrapper
