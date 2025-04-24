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

module m_soltest

   implicit none

   private

   public :: soltest

contains

   !> test iterative solver (as "mpitest")
   subroutine soltest(iCFL, icgsolver_loc, maxsubmatvecs, iepsdiff, iepscg)
      use m_make_matrix, only: make_matrix
      use m_solve_guus, only: solve_matrix
      use precision, only: dp
      use m_update_matrix, only: update_matrix
      use m_partitioninfo
      use m_timer
      use m_flowgeom
      use network_data, only: xzw
      use m_flowparameters
      use m_reduce
      use m_flow
      use m_alloc
      use m_flow_modelinit, only: flow_modelinit
      use m_solve_guus, only: pack_matrix

      integer, intent(in) :: iCFL !< wave-based Courant number
      integer, intent(in) :: icgsolver_loc ! icgsolver (if > 0)
      integer, intent(in) :: maxsubmatvecs ! maximum number of subiterations in Schwarz solver (if > 0)
      integer, intent(in) :: iepsdiff ! -10log(tolerance in Schwarz iterations) (if > 0)
      integer, intent(in) :: iepscg ! -10log(tolerance in inner iterations) (if > 0)

      real(kind=dp), dimension(:), allocatable :: sex ! exact solution at cell centers
      real(kind=dp), dimension(:), allocatable :: dmask ! used for masking ghost cells that are not being updated

      real(kind=dp) :: CFL
      real(kind=dp) :: diffmax

      integer :: NRUNS
      integer :: i, ii, irun
      integer :: ierror

      jarenumber = 0
      CFL = 10d0
!      maxdge = 0d0
!      icgsolver = 4
!      ipre = 0
      Nruns = 1

      if (iCFL > 0d0) then
         CFL = dble(iCFL)
      end if

!     settings from command line
      if (icgsolver_loc > 0) then
         icgsolver = icgsolver_loc
      end if

      if (iepsdiff > 0) then
         epsdiff = 10d0**(-iepsdiff)
      end if

      if (iepscg > 0) then
         epscg = 10d0**(-iepscg)
      end if

      if (maxsubmatvecs > 0) then
         maxmatvecs = maxsubmatvecs
      end if

      ierror = flow_modelinit()

!!      call initimer()
!
!      call resetflow()
!
!      if ( jampi.eq.0 ) then
!         call flow_geominit(0)
!
!         if (Ndx == 0) then
!           call mess(LEVEL_INFO,'no network')
!           goto 1234
!         end if
!
!      else
!         call flow_geominit(1)   ! first phase only
!
!         if ( Ndx.gt.0 ) then
!            if ( jatimer.eq.1 ) call starttimer(IPARTINIT)
!            call partition_init_1D2D('dummy', md_genpolygon, ierror)   ! both for 1D & 2D (hence the name, thanks to Herman for pointing this out)
!            if ( jatimer.eq.1 ) call stoptimer(IPARTINIT)
!
!            if ( ierror.ne.0 ) then
!              call mess(LEVEL_WARN,'Error in 2D partitioning initialization.')
!              goto 1234
!            end if
!
!            call update_geom(1)              ! update geometry in ghost area
!
!            call flow_geominit(2)            ! second phase
!            call update_geom(2)              ! update geometry in ghost area
!
!            call disable_invalid_ghostcells_with_wu() ! disable ghost cells that are not being synchronised by setting wu's to zero
!         else
!            call mess(LEVEL_INFO,'no network')
!            goto 1234
!         end if
!
!      end if

!      call flow_allocflow()

!     allocate solution and mask
      allocate (sex(Ndx))
      allocate (dmask(Ndx))

!     activate all cells
      hu = epshu + 1d0

!     set exact solution
      sex = xzw

      if (jatimer == 1) call starttimer(ITOTAL)

!!     prepare matrix
!      if ( jatimer.eq.1 ) call starttimer(IREDUCE)
!      call reducept(Ndx, Ndxi, Lnx)
!      if ( jatimer.eq.1 ) call stoptimer(IREDUCE)

!     construct matrix and rhs
      call make_matrix(CFL, sex)

!     update overlapping ghost-parts of matrix
      if (jampi == 1 .and. jaoverlap == 1) then
         call update_matrix(ierror)
      end if

!     pack matrix
      call pack_matrix()

      call realloc(ccrsav, ubound(ccr, 1), lbound(ccr, 1), keepExisting=.false., fill=0d0)
      ccrsav = ccr

!     solve system
      do irun = 1, Nruns
         s1 = 0d0
         ccr = ccrsav

!         if (icgsolver.eq.6) call setPETSCmatrixEntries()
!         call createPETSCPreconditioner(iprecond)

         call solve_matrix(s1, Ndx, itsol)

      end do
      if (jatimer == 1) call stoptimer(ITOTAL)

!     unmask all cells
      dmask = 0d0

      if (jampi == 1) then
         call update_ghosts(ITYPE_SALL, 1, Ndx, s1, ierror)

!        mask all ghost cells
         do i = 1, Ndx
            if (idomain(i) /= my_rank) then
               dmask(i) = 1d0
            end if
         end do

!        unmask ghost cells with updated values
         call update_ghosts(ITYPE_SALL, 1, Ndx, dmask, ierror)
      end if

      diffmax = 0d0
      do i = 1, Ndxi
         if (nd(i)%lnx > 0 .and. dmask(i) == 0d0) then
            if (abs(s1(i) - sex(i)) > 1d-10) then
               continue
            end if
            diffmax = max(diffmax, abs(s1(i) - sex(i)))
         end if
      end do

      do ii = 1, nghostlist_sall(ndomains - 1)
         i = ighostlist_sall(ii)
         if (abs(s1(i) - sex(i)) > 1d-10) then
            continue
         end if
      end do

      write (6, '("rank", I2, ", number of iterations: ", I4, ", max diff: ", E9.2)') my_rank, itsol, diffmax

      if (my_rank == 0) then
         write (6, '(a,E9.2,a,E9.2)') ' WC-time solver   [s]: ', gettimer(1, ITOTALSOL), ' CPU-time solver   [s]: ', gettimer(0, ITOTALSOL)
         write (6, '(a,E9.2,a,E9.2)') ' WC-time MPI comm [s]: ', gettimer(1, IMPICOMM), ' CPU-time MPI comm [s]: ', gettimer(0, IMPICOMM)
      end if

1234  continue

      if (allocated(sex)) then
         deallocate (sex)
      end if
      if (allocated(dmask)) then
         deallocate (dmask)
      end if

      return
   end subroutine soltest

end module m_soltest
