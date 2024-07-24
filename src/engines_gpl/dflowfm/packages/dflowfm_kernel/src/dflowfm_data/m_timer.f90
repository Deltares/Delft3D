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

module m_timer
   implicit none
   integer, parameter :: jatimer = 1 !< time parallel solver (1) or not (0)
   integer, parameter :: NUMT = 29 !< number of timings
   double precision, dimension(3, NUMT) :: t !< wall-clock timings, (1,:): start, (2,:): end, (3,:): sum
   double precision, dimension(3, NUMT) :: tcpu !< CPU        timings, (1,:): start, (2,:): end, (3,:): sum
   integer, dimension(NUMT) :: itstat !< timer status, 0: not timing (stopped), 1: timing (started)

   integer :: numtsteps !< number of time steps
   integer :: numcgits !< number of CG iterations

   integer, parameter :: IREDUCE = 1
   integer, parameter :: IMAKEMAT = 2
   integer, parameter :: IPACKMAT = 3
   integer, parameter :: IGAUSSEL = 4
   integer, parameter :: ICG = 5
   integer, parameter :: IGAUSSSU = 6
   integer, parameter :: IMPICOMM = 7
   integer, parameter :: ITOTALSOL = 8
   integer, parameter :: ITIMESTEP = 9
   integer, parameter :: ITOTAL = 10
   integer, parameter :: IPOSTMPI = 11
   integer, parameter :: IUPDSALL = 12
   integer, parameter :: IUPDU = 13
   integer, parameter :: IMPIREDUCE = 14
   integer, parameter :: IPARTINIT = 15
   integer, parameter :: IOUTPUT = 16
   integer, parameter :: IOUTPUTMPI = 17
   integer, parameter :: ITRANSPORT = 18
   integer, parameter :: IXBEACH = 19
   integer, parameter :: IAXPY = 20
   integer, parameter :: IDEBUG = 21
   integer, parameter :: IFMWAQ = 22
   integer, parameter :: IFILT_COEF = 23
   integer, parameter :: IFILT_SOLV = 24
   integer, parameter :: IFILT = 25
   integer, parameter :: IFILT_MAT = 26
   integer, parameter :: IFILT_COPYBACK = 27
   integer, parameter :: IFILT_OTHER = 28
   integer, parameter :: IEROSED = 29

   character(len=10), dimension(numt), parameter :: tnams = [character(len=10) :: &
                                                             'reduce', &
                                                             'make_mat.', &
                                                             'pack', &
                                                             'Gauss_elem', &
                                                             'CG', &
                                                             'Gauss_subs', &
                                                             'MPI_comm', &
                                                             'total_sol', &
                                                             'timestep', &
                                                             'total', &
                                                             'MPI_post', &
                                                             'MPI_sall', &
                                                             'MPI_u', &
                                                             'MPI_reduce', &
                                                             'part_init', &
                                                             'output', &
                                                             'MPI_output', &
                                                             'transport', &
                                                             'XBeach', &
                                                             'Axpy', &
                                                             'debug', &
                                                             'fmwaq', &
                                                             'filt_coef', &
                                                             'filt_solv', &
                                                             'filter', &
                                                             'filter_mat', &
                                                             'filter_cpb', &
                                                             'filter_oth', &
                                                             'erosed']
contains

!> initialize timers
   subroutine initimer()
      implicit none

      t = 0d0
      tcpu = 0d0
      itstat = 0

      numtsteps = 0
      numcgits = 0
   end subroutine initimer

!> start the timer
   subroutine starttimer(itvar)
      implicit none

      integer, intent(in) :: itvar !< timer number

      double precision :: tloc

!     check status
      if (itstat(itvar) /= 0) then
         write (6, '("starttimer: status error for timer ", I0)') itvar
      end if

      call klok(tloc)
      t(1, itvar) = tloc

      call cpu_time(tcpu(1, itvar))

!     set status
      itstat(itvar) = 1

      return
   end subroutine starttimer

!> stop the timer
   subroutine stoptimer(itvar)
      use MessageHandling

      implicit none
      integer, intent(in) :: itvar !< timer number

      double precision :: tloc
      double precision, parameter :: dtol = 1d-3 !< timer tolerance

!     check status
      if (itstat(itvar) /= 1) then
         write (6, '("stoptimer: status error for timer ", I0)') itvar
      else
         call klok(tloc)
         t(2, itvar) = tloc
         t(3, itvar) = t(3, itvar) + tloc - t(1, itvar)

         call cpu_time(tcpu(2, itvar))
         tcpu(3, itvar) = tcpu(3, itvar) + tcpu(2, itvar) - tcpu(1, itvar)

!         if ( (tcpu(3,itvar)-t(3,itvar)).gt.dtol  ) then   ! should not happen
!            write(6,*) tnams(itvar)
!            write(6,*) 't=',    t(:,itvar)
!            write(6,*) 'tcpu=', tcpu(:,itvar)
!            call mess(LEVEL_ERROR, 'stoptimer: timer error')
!         end if
      end if

!     set status
      itstat(itvar) = 0

      return
   end subroutine stoptimer

   !> get timer value
   double precision function gettimer(itype, itvar)
      implicit none
      integer, intent(in) :: itype !< timer type, cpu-time (0) or wall-clock time (other)
      integer, intent(in) :: itvar !< timer number

      if (itype == 0) then
         gettimer = tcpu(3, itvar)
      else
         gettimer = t(3, itvar)
      end if

      return
   end function gettimer

end module m_timer
