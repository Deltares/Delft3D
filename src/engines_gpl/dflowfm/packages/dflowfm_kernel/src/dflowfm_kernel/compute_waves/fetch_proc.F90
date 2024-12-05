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

! $Id: fetch_proc.F90 142166 2022-12-13 09:17:41Z markelov $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20221121_UNST-6506_parallel_fetch_helper/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_waves/fetch_proc.F90 $

!> holds variables and arrays for the fetch proc operation
module fetch_proc_operation_data

   use precision, only: dp
   integer, dimension(:), allocatable :: ndx_over_procs !< ndx data over procs
   integer, dimension(:, :), allocatable :: iglobal_s_procs !< iglobal_s data for each proc
   real(kind=dp), dimension(:, :), allocatable :: f_buffer !< buffer to send fetch/fetdp data
   real(kind=dp), dimension(:), allocatable :: s1_buffer !< buffer to send s1 data

   integer :: dflowfm_entire_group !< this group includes all procs
   integer :: dflowfm_group !< this group includes all procs except the last one which is the fetch proc

end module fetch_proc_operation_data

!> initialize data for the fetch proc operation
integer function initialise_fetch_proc_data() result(iresult)

   use fetch_proc_operation_data
#ifdef HAVE_MPI
   use mpi
   use m_partitioninfo, only: my_rank, fetch_proc_rank, DFM_COMM_ALLWORLD, iglobal_s
#endif
   use m_flow, only: s1
   use m_flowgeom, only: ndx, xz, yz
   use m_waves, only: nwf
   use m_alloc, only: aerr
   use dfm_error, only: DFM_NOERR, DFM_NOTIMPLEMENTED, DFM_WRONGINPUT
   use MessageHandling

   implicit none

#ifdef HAVE_MPI
   integer :: status(MPI_Status_size)
#endif
   integer :: ndx_max, index_loop, error, source, tag = 100, icount
   integer, dimension(:), allocatable :: iglobal_s_source
   integer :: flow_node, isearch, j0
   real(kind=dp), dimension(:), allocatable :: xz_proc, yz_proc
   logical :: iglobal_s_exist_on_fetch_proc = .false.
   real(kind=dp), parameter :: tolerance = 1.0d-3

#ifdef HAVE_MPI
   if (allocated(ndx_over_procs)) deallocate (ndx_over_procs)
   allocate (ndx_over_procs(0:fetch_proc_rank), stat=error)
   call mpi_gather(ndx, 1, mpi_integer, ndx_over_procs, 1, MPI_INTEGER, fetch_proc_rank, DFM_COMM_ALLWORLD, error)

   if (my_rank == fetch_proc_rank) then
      ndx_over_procs(fetch_proc_rank) = 0
      ndx_max = maxval(ndx_over_procs)

      if (allocated(iglobal_s_procs)) deallocate (iglobal_s_procs)
      allocate (iglobal_s_procs(ndx_max, 0:fetch_proc_rank - 1), stat=error)
      call aerr('iglobal_s_pros', error, fetch_proc_rank * ndx_max)

      if (allocated(s1_buffer)) deallocate (s1_buffer)
      allocate (s1_buffer(ndx_max), stat=error)
      call aerr('s1_buffer', error, ndx_max)

      if (allocated(f_buffer)) deallocate (f_buffer)
      allocate (f_buffer(nwf, ndx_max), stat=error)
      call aerr('f_buffer', error, nwf * ndx_max)

      if (.not. allocated(s1)) then
         allocate (s1(ndx), stat=error)
         call aerr('f_buffer', error, nwf * ndx_max)
      end if

   end if

   if (my_rank == fetch_proc_rank) then
      iglobal_s_exist_on_fetch_proc = allocated(iglobal_s)
   end if
   call mpi_bcast(iglobal_s_exist_on_fetch_proc, 1, MPI_LOGICAL, fetch_proc_rank, DFM_COMM_ALLWORLD, error)

   if (my_rank < fetch_proc_rank) then
      if (iglobal_s_exist_on_fetch_proc) then
         call mpi_send(iglobal_s, ndx, MPI_INTEGER, fetch_proc_rank, tag, DFM_COMM_ALLWORLD, error)
      else
         call mpi_send(xz, ndx, MPI_Double_precision, fetch_proc_rank, tag, DFM_COMM_ALLWORLD, error)
         call mpi_send(yz, ndx, MPI_Double_precision, fetch_proc_rank, tag + 1, DFM_COMM_ALLWORLD, error)
      end if
   else
      if (iglobal_s_exist_on_fetch_proc) then
         !receive iglobal_s from other procs.
         call mess(LEVEL_ERROR, 'An option with iglobal_s on the fetch proc is not tested. Stopping program.')
         iresult = DFM_NOTIMPLEMENTED
         return
         !This option is not tested and cleaned because a single proc net.nc file does not include iglobal_s data
         do index_loop = 1, fetch_proc_rank
            call mpi_probe(MPI_ANY_SOURCE, tag, DFM_COMM_ALLWORLD, status, error)
            call mpi_get_count(status, MPI_INTEGER, icount, error)
            source = status(MPI_SOURCE)
            if (allocated(iglobal_s_source)) deallocate (iglobal_s_source)
            allocate (iglobal_s_source(ndx_over_procs(source)))
            !call mpi_recv(iglobal_s_procs(1,source), ndx_over_procs(source), MPI_INTEGER, source, tag, DFM_COMM_ALLWORLD, status, error)
            call mpi_recv(iglobal_s_source, ndx_over_procs(source), MPI_INTEGER, source, tag, DFM_COMM_ALLWORLD, status, error)
            do j0 = 1, ndx
               iglobal_s_procs(j0, source) = iglobal_s_source(j0)
            end do
         end do
      else
         ! rebuild iglobal_s on the fetch proc
         if (allocated(xz_proc)) deallocate (xz_proc)
         allocate (xz_proc(ndx_max), stat=error)
         call aerr('xz_proc', error, ndx_max)
         if (allocated(yz_proc)) deallocate (yz_proc)
         allocate (yz_proc(ndx_max), stat=error)
         call aerr('yz_proc', error, ndx_max)

         do index_loop = 1, fetch_proc_rank
            call mpi_probe(MPI_ANY_SOURCE, tag, DFM_COMM_ALLWORLD, status, error)
            call mpi_get_count(status, MPI_Double_precision, icount, error)
            source = status(MPI_SOURCE)
            call mpi_recv(xz_proc, ndx_over_procs(source), MPI_Double_precision, source, tag, DFM_COMM_ALLWORLD, status, error)
            call mpi_recv(yz_proc, ndx_over_procs(source), MPI_Double_precision, source, tag + 1, DFM_COMM_ALLWORLD, status, error)
            do flow_node = 1, ndx_over_procs(source)
               do isearch = 1, ndx
                  ! a simple search assuming that everything is fine
                  if (abs(xz(isearch) - xz_proc(flow_node)) < tolerance .and. &
                      abs(yz(isearch) - yz_proc(flow_node)) < tolerance) then
                     iglobal_s_procs(flow_node, source) = isearch
                     exit
                  end if
               end do
            end do
         end do
         deallocate (xz_proc)
         deallocate (yz_proc)
      end if
   end if

   iresult = DFM_NOERR
#else

   iresult = DFM_WRONGINPUT
#endif

end function initialise_fetch_proc_data

!> sends s1 values to the fetch proc
subroutine send_s1_to_fetch_proc()

   use fetch_proc_operation_data
#ifdef HAVE_MPI
   use mpi
   use m_partitioninfo, only: my_rank, fetch_proc_rank, DFM_COMM_ALLWORLD
#endif
   use m_flowgeom, only: ndx
   use m_flow, only: s1
   use dfm_error, only: DFM_NOERR

   implicit none

#ifdef HAVE_MPI
   integer, dimension(MPI_Status_size) :: status
#endif
   integer :: index_loop, flow_node, error, source, tag = 200, icount

#ifdef HAVE_MPI
   if (my_rank < fetch_proc_rank) then
      call mpi_send(s1, ndx, MPI_Double_precision, fetch_proc_rank, tag, DFM_COMM_ALLWORLD, error)
   else
      do index_loop = 1, fetch_proc_rank
         call mpi_probe(MPI_ANY_SOURCE, tag, DFM_COMM_ALLWORLD, status, error)
         call mpi_get_count(status, MPI_Double_precision, icount, error)
         source = status(MPI_SOURCE)
         if (icount /= ndx_over_procs(source)) then
            stop
         end if
         call mpi_recv(s1_buffer, ndx_over_procs(source), MPI_Double_precision, source, tag, DFM_COMM_ALLWORLD, status, error)
         do flow_node = 1, ndx_over_procs(source)
            s1(iglobal_s_procs(flow_node, source)) = s1_buffer(flow_node)
         end do
      end do
   end if
#endif
end subroutine send_s1_to_fetch_proc

!> send fetch length and depth values calculated on the fetch proc to toher procs
subroutine get_fetch_values_from_fetch_proc()

   use fetch_proc_operation_data
#ifdef HAVE_MPI
   use mpi
   use m_partitioninfo, only: my_rank, fetch_proc_rank, DFM_COMM_ALLWORLD
#endif
   use m_flowgeom, only: ndx
   use m_waves, only: fetch, fetdp, nwf
   use dfm_error, only: DFM_NOERR

   implicit none

#ifdef HAVE_MPI
   integer :: status(MPI_Status_size)
#endif
   integer :: destination, flow_node, wind_direction, error, tag = 200

#ifdef HAVE_MPI
   if (my_rank /= fetch_proc_rank) then
      call mpi_recv(fetch, nwf * ndx, MPI_Double_precision, fetch_proc_rank, tag, DFM_COMM_ALLWORLD, status, error)
      call mpi_recv(fetdp, nwf * ndx, MPI_Double_precision, fetch_proc_rank, tag, DFM_COMM_ALLWORLD, status, error)
   else
      !a simple realization with buffer and blocking send
      do destination = 0, fetch_proc_rank - 1
         do flow_node = 1, ndx_over_procs(destination)
            do wind_direction = 1, nwf
               f_buffer(wind_direction, flow_node) = fetch(wind_direction, iglobal_s_procs(flow_node, destination))
            end do
         end do
         call mpi_send(f_buffer, nwf * ndx_over_procs(destination), MPI_Double_precision, destination, tag, DFM_COMM_ALLWORLD, error)
         do flow_node = 1, ndx_over_procs(destination)
            do wind_direction = 1, nwf
               f_buffer(wind_direction, flow_node) = fetdp(wind_direction, iglobal_s_procs(flow_node, destination))
            end do
         end do
         call mpi_send(f_buffer, nwf * ndx_over_procs(destination), MPI_Double_precision, destination, tag, DFM_COMM_ALLWORLD, error)
      end do
   end if
#endif
end subroutine get_fetch_values_from_fetch_proc

!> sets MPI enviromnent in cases with and without the fetch proc and jampi/jagui for the fetch proc
subroutine set_mpi_environment_wwo_fetch_proc()
#ifdef HAVE_MPI
   use mpi
   use fetch_proc_operation_data
   use m_partitioninfo, only: jampi, my_rank, numranks, fetch_proc_rank, use_fetch_proc, &
                              DFM_COMM_DFMWORLD, DFM_COMM_ALLWORLD
#endif
   use m_gui

   implicit none

   integer, dimension(:), allocatable :: list_of_procs ! a list of procs to create a new commmunicator without the fetch proc
   integer :: error, i

#ifdef HAVE_MPI
   DFM_COMM_ALLWORLD = DFM_COMM_DFMWORLD
   if (use_fetch_proc == 0) then
      ! all processors are used for the flow modelling
      call mpi_comm_rank(DFM_COMM_DFMWORLD, my_rank, error)
      call mpi_comm_size(DFM_COMM_DFMWORLD, numranks, error)
   else
      ! the last proc is the fetch proc that is dedicated for the fetch parameters calculation. It is not used for the flow modelling
      call mpi_comm_rank(DFM_COMM_ALLWORLD, my_rank, error)
      call mpi_comm_size(DFM_COMM_ALLWORLD, numranks, error)
      if (numranks > 2) then
         allocate (list_of_procs(numranks - 1), stat=error)
         list_of_procs = [(i, i=0, numranks - 2)]
         call MPI_Comm_group(DFM_COMM_ALLWORLD, dflowfm_entire_group, error)
         call MPI_Group_incl(dflowfm_entire_group, numranks - 1, list_of_procs, dflowfm_group, error)
         call MPI_Comm_create(DFM_COMM_ALLWORLD, dflowfm_group, DFM_COMM_DFMWORLD, error)
         deallocate (list_of_procs)
         fetch_proc_rank = numranks - 1
         if (my_rank /= fetch_proc_rank) then
            numranks = numranks - 1
         else
            jampi = 0 ! switch off partition etc for the fetch processor
            jagui = 0 ! switch off gui for the fetch proc
         end if
      else
         use_fetch_proc = 0
      end if
   end if
#endif
end subroutine set_mpi_environment_wwo_fetch_proc

!> clean settings done in the case of the fetch proc
subroutine clean_fetch_proc_settings()
#ifdef HAVE_MPI
   use mpi
   use fetch_proc_operation_data
   use m_partitioninfo, only: jampi, my_rank, fetch_proc_rank, use_fetch_proc, DFM_COMM_DFMWORLD
#endif
   use m_gui

   implicit none

   integer :: error

#ifdef HAVE_MPI
   if (use_fetch_proc > 0) then
      if (DFM_COMM_DFMWORLD /= MPI_COMM_NULL) call MPI_Comm_free(DFM_COMM_DFMWORLD, error)
      call MPI_Group_free(dflowfm_group, error)
      call MPI_Group_free(dflowfm_entire_group, error)
      use_fetch_proc = 0
      if (my_rank == fetch_proc_rank) then
         jampi = 1
         jagui = 1
      end if
   end if
#endif
end subroutine clean_fetch_proc_settings

!> provides communications between all procs to stop the fetch proc when other procs finished
logical function stop_fetch_computation(call_from_tauwavefetch)
#ifdef HAVE_MPI
   use mpi
   use m_partitioninfo, only: my_rank, fetch_proc_rank, DFM_COMM_ALLWORLD
#endif
   use fetch_proc_operation_data

   implicit none

   logical, intent(in) :: call_from_tauwavefetch !< It is true when the function is called from tauwavefetch subroutine
   logical :: result = .false.
   integer :: error

#ifdef HAVE_MPI
   if (call_from_tauwavefetch .or. my_rank /= fetch_proc_rank) then
      call MPI_Allreduce(call_from_tauwavefetch, result, 1, MPI_logical, MPI_land, DFM_COMM_ALLWORLD, error)
   end if
#endif

   stop_fetch_computation = .not. result

end function stop_fetch_computation

!> send a message to the fetch proc that other procs are finished, then it calls clean_fetch_proc_settings subroutine
subroutine finish_fetch_proc()
   use m_partitioninfo, only: my_rank, fetch_proc_rank, use_fetch_proc
   use fetch_proc_operation_data
   use m_flowtimes
   use MessageHandling
   use Timers

   implicit none

   logical, external :: stop_fetch_computation
   logical :: log_dump, call_from_tauwavefetch = .false.

   if (use_fetch_proc > 0) then
      if (my_rank /= fetch_proc_rank) then
         log_dump = stop_fetch_computation(call_from_tauwavefetch)
      end if
      call clean_fetch_proc_settings()
   end if

end subroutine finish_fetch_proc
