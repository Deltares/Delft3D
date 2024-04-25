!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
module m_mormerge_mpi

   use precision
   implicit none
   
   private
   
   integer, allocatable              :: buffers_sizes(:)               !< number of points (ndxi*lsedtot) over domains/procs
   integer, allocatable              :: displacement_over_procs(:)     !< displacement array for merge buffers over procs
   integer                           :: global_mergebuffer_size        !< size of global merge buffer
   real(hp), allocatable             :: global_mergebuffer(:)          !< global merge buffer
   integer                           :: mpi_type_real_fp               !< mpi datetype
   
   public :: update_mergebuffer, initialize_mormerge_mpi
   
contains

!> returns a total number of cells over subdomains
integer function total_number_of_cells(jampi, ndxi, DFM_COMM_DFMWORLD)
#ifdef HAVE_MPI
   use mpi
#endif

   implicit none
   integer, intent(in) :: jampi              !< 0: a single domain, 1: parallel computation
   integer, intent(in) :: ndxi               !< a number of flow nodes
   integer, intent(in) :: DFM_COMM_DFMWORLD  !< a MPI communicator 
   
   integer             :: ndxi_total         !< a total number of cells/flownodes over subdomains
   integer             :: error              !< error
    
#ifdef HAVE_MPI
   if ( jampi == 1 ) then
      call mpi_reduce(ndxi, ndxi_total, 1, MPI_integer, MPI_Sum, 0, DFM_COMM_DFMWORLD, error )
      total_number_of_cells = ndxi_total
   else
      total_number_of_cells = ndxi 
   end if 

#else
   total_number_of_cells = ndxi
#endif

end function total_number_of_cells

!> updates the merge buffer 
subroutine update_mergebuffer(mergehandle, buffer_size, mergebuffer, jampi, my_rank, DFM_COMM_DFMWORLD)
   use m_mormerge
#ifdef HAVE_MPI
   use mpi
#endif

   implicit none
   
   integer,          intent(in)      :: mergehandle                 !<  stream handle for communication with mormerge
   integer,          intent(in)      :: buffer_size                 !< size of merge buffer
   real(hp),         intent(inout)   :: mergebuffer(buffer_size)    !< a buffer for mormerge data 
   integer,          intent(in)      :: jampi                       !< 0: a single domain, 1: parallel computation
   integer,          intent(in)      :: my_rank                     !< a processor rank
   integer,          intent(in)      :: DFM_COMM_DFMWORLD           !< a MPI communicator 
   
   integer                           :: error                       !< error 

#ifdef HAVE_MPI
   if ( jampi == 0 ) then
      call put_get_mergebuffer(mergehandle, buffer_size, mergebuffer)
   else
      call mpi_gatherv(mergebuffer, buffer_size, mpi_type_real_fp, global_mergebuffer, &
            buffers_sizes, displacement_over_procs, mpi_type_real_fp, 0, DFM_COMM_DFMWORLD, error)
      if ( my_rank == 0 ) then
         call put_get_mergebuffer(mergehandle, global_mergebuffer_size, global_mergebuffer)
      end if
      call mpi_scatterv(global_mergebuffer, buffers_sizes, displacement_over_procs, mpi_type_real_fp, &
            mergebuffer, buffer_size, mpi_type_real_fp, 0, DFM_COMM_DFMWORLD, error)
   endif
#else
   call put_get_mergebuffer(mergehandle, buffer_size, mergebuffer)
#endif

end subroutine update_mergebuffer

!< initialization of mormerge and arrays allocation for its operation on a parallel computer
function initialize_mormerge_mpi(gdmorpar, lsedtot, ndxi, jampi, my_rank, ndomains, DFM_COMM_DFMWORLD) result (error)
   use morphology_data_module, only: morpar_type
   use m_mormerge
   use m_alloc
#ifdef HAVE_MPI
   use mpi
#endif

   implicit none

   type (morpar_type), pointer :: gdmorpar           !< morphology data
   integer, intent(in)         :: lsedtot            !< number of sediment variables
   integer, intent(in)         :: ndxi               !< a number of flow nodes
   integer, intent(in)         :: jampi              !< 0: a single domain, 1: parallel computation
   integer, intent(in)         :: my_rank            !< a processor rank
   integer, intent(in)         :: ndomains           !< a number of subdomains
   integer, intent(in)         :: DFM_COMM_DFMWORLD  !< a MPI communicator 
   
   integer                     :: error              !< error 
   
   integer                     :: ndxi_total         !< total number of cells over all procs
   integer                     :: subdomain          !< subdomain number


   error      = 0
   ndxi_total = total_number_of_cells(jampi, ndxi, DFM_COMM_DFMWORLD) 
   if ( my_rank == 0 ) then
      call initialize_mormerge(error, ndxi_total, lsedtot, "singledomain", gdmorpar)
   end if
           
#ifdef HAVE_MPI
   if ( jampi > 0 ) then
      allocate(buffers_sizes(ndomains), displacement_over_procs(ndomains), stat=error)
      call aerr('buffers_sizes, displacement_over_procs', error, ndomains)
      
      call mpi_gather(ndxi*lsedtot, 1, mpi_integer, buffers_sizes, 1, mpi_integer, 0, DFM_COMM_DFMWORLD, error)
      if ( my_rank == 0 ) then
         displacement_over_procs(1) = 0
         do subdomain = 1, ndomains - 1
            displacement_over_procs(subdomain + 1) = displacement_over_procs(subdomain) + buffers_sizes(subdomain)
         end do 
         global_mergebuffer_size = sum(buffers_sizes)
         allocate(global_mergebuffer(global_mergebuffer_size), stat=error)
         call aerr('global_mergebuffer', error, global_mergebuffer_size)
      else
        allocate(global_mergebuffer(1))
      end if
      
      if (fp == hp) then
         mpi_type_real_fp = MPI_DOUBLE_PRECISION
      else
         mpi_type_real_fp = MPI_REAL
      end if
   end if
#endif

end function initialize_mormerge_mpi

end module m_mormerge_mpi
