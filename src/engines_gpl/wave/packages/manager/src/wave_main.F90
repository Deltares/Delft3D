module wave_main
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use swan_flow_grid_maps
   use swan_input
   use flow_data
   use sync_flowwave
   use wave_data
   use wave_mpi
   use meteo
   use dwaves_version_module
   use m_swan_tot, only: swan_tot

   implicit none

   private

   public wave_main_init
   public wave_main_step
   public wave_main_finish
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
   public :: initialize_fm_coupling
   public :: is_fm_coupling_ongoing
   public :: advance_fm_time_window
#endif

   public wavedata
!
! Module variables
!
   integer                                      :: n_swan_grids ! number of SWAN grids
   integer                                      :: n_flow_grids ! number of FLOW grids
   type(wave_data_type),target                  :: wavedata
   integer :: tmpchar

contains

#if defined(HAS_PRECICE_WAVE_GREETER_COUPLING)
   subroutine couple_to_greeter_dummy()
      use m_alloc, only: realloc
      use precice, only: precicef_create, precicef_get_mesh_dimensions, precicef_initialize, &
                         precicef_set_vertices, precicef_read_data, &
                         precicef_get_data_dimensions, precicef_get_max_time_step_size

      integer(kind=c_int), parameter :: precice_component_name_length = 4
      character(kind=c_char, len=precice_component_name_length), parameter :: precice_component_name = "wave"
      integer(kind=c_int), parameter :: precice_config_name_length = 21
      character(kind=c_char, len=precice_config_name_length), parameter :: precice_config_name = "../precice_config.xml"
      integer(kind=c_int), parameter :: mesh_name_length = 9
      character(kind=c_char, len=mesh_name_length), parameter :: mesh_name = "wave-mesh"
      integer(kind=c_int), parameter :: max_greeting_length = 34
      integer(kind=c_int) :: mesh_dimensions
      real(kind=c_double), dimension(max_greeting_length*2) :: mesh_coordinates
      integer(kind=c_int), dimension(max_greeting_length) :: vertex_ids
      integer(kind=c_int), parameter :: data_name_length = 8
      character(kind=c_char, len=data_name_length), parameter :: data_name = "greeting"
      integer :: data_size, data_dimension
      real(kind=c_double), dimension(:), allocatable :: data_values
      character(kind=c_char), dimension(:), allocatable :: converted_data
      real(kind=c_double) :: precice_time_step

      call precicef_create(precice_component_name, precice_config_name, my_rank, numranks, precice_component_name_length, precice_config_name_length)
      call precicef_get_mesh_dimensions(mesh_name, mesh_dimensions, mesh_name_length)
      print *, '[wave] The number of dimensions of the wave-mesh is ', mesh_dimensions

      mesh_coordinates = 0.0_c_double
      call precicef_set_vertices(mesh_name, max_greeting_length, mesh_coordinates, vertex_ids, mesh_name_length)
      call precicef_initialize()

      call precicef_get_data_dimensions(mesh_name, data_name, data_dimension, mesh_name_length, data_name_length)
      data_size = data_dimension * max_greeting_length
      print *, '[wave] data dimension: ', data_dimension, ' data size: ', data_size
      call realloc(data_values, data_size)

      call precicef_get_max_time_step_size(precice_time_step)
      print *, '[wave] max time step: ', precice_time_step
      call precicef_read_data(mesh_name, data_name, data_size, vertex_ids, precice_time_step, data_values, &
                              mesh_name_length, data_name_length)

      converted_data = [(char(int(data_values(i)), kind=c_char), integer :: i=1, data_size)]
      print *, '[wave] message read: ', converted_data
   end subroutine couple_to_greeter_dummy
#endif

#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
   subroutine initialize_fm_coupling(mdw_file_name, precice_state)
      use precice, only: precicef_create, precicef_initialize
      use m_wave_precice_state_t, only: wave_precice_state_t
      use, intrinsic :: iso_c_binding, only: c_char
      implicit none(type, external)

      character(len=*), intent(in) :: mdw_file_name
      type(wave_precice_state_t), intent(out) :: precice_state

      character(kind=c_char, len=*), parameter :: precice_component_name = "wave"
      character(kind=c_char, len=*), parameter :: precice_config_name = "../precice_config.xml"

      call precicef_create(precice_component_name, precice_config_name, my_rank, numranks, len(precice_component_name), len(precice_config_name))

      call register_wave_nodes_with_precice(mdw_file_name, precice_state)
      call precicef_initialize()
   end subroutine initialize_fm_coupling

   function is_fm_coupling_ongoing() result(is_ongoing)
      use precice, only: precicef_is_coupling_ongoing
      use, intrinsic :: iso_c_binding, only: c_int
      integer(kind=c_int) :: is_ongoing

      call precicef_is_coupling_ongoing(is_ongoing)
   end function is_fm_coupling_ongoing

   subroutine advance_fm_time_window()
      use, intrinsic :: iso_c_binding, only: c_double
      use precice, only: precicef_get_max_time_step_size, precicef_advance

      real(kind=c_double) :: max_time_step

      call precicef_get_max_time_step_size(max_time_step)
      call precicef_advance(max_time_step)
   end subroutine advance_fm_time_window

   subroutine register_wave_nodes_with_precice(mdw_file_name, precice_state)
      use precice, only: precicef_set_vertices, precicef_set_mesh_triangles
      use m_wave_precice_state_t, only: wave_precice_state_t
      use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double
      use swan_flow_grid_maps, only: grid
      implicit none(type, external)

      character(len=*), intent(in) :: mdw_file_name
      type(wave_precice_state_t), intent(out) :: precice_state

      integer :: result
      type(grid) :: swan_grid
      real(kind=c_double), dimension(:), allocatable :: mesh_coordinates
      integer :: i, j, node_index, active_count

      integer(kind=c_int), dimension(:,:), allocatable :: node_indices
      integer(kind=c_int), dimension(:), allocatable :: triangle_nodes
      integer(kind=c_int), dimension(:), allocatable :: precice_triangle_nodes
      integer(kind=c_int) :: num_triangles, is_required

      precice_state = wave_precice_state_t()

      result = get_swan_grid(mdw_file_name, swan_grid, active_count)
      if (result /= 0) then
         write (*, '(a)') '[Wave] *** ERROR: Failed to get Swan grid for preCICE registration'
         return
      end if

      write (*, '(a,i0,a,i0,a)') '[Wave] Grid has ', active_count, ' active nodes out of ', swan_grid%npts, ' total'

      allocate (mesh_coordinates(2 * active_count))
      allocate (precice_state%vertex_ids(active_count))
      allocate (node_indices(swan_grid%mmax, swan_grid%nmax))

      node_index = 0
      do j = 1, swan_grid%nmax
         do i = 1, swan_grid%mmax
            if (swan_grid%kcs(i, j) == 1) then
               node_index = node_index + 1
               node_indices(i,j) = node_index
               mesh_coordinates(2 * node_index - 1) = real(swan_grid%x(i, j), kind=c_double)
               mesh_coordinates(2 * node_index) = real(swan_grid%y(i, j), kind=c_double)
            else
               node_indices(i,j) = 0
            end if
         end do
      end do

      print *, '[Wave] node_index = ', node_index
      call precicef_set_vertices(precice_state%swan_mesh_name, active_count, mesh_coordinates, precice_state%vertex_ids, len(precice_state%swan_mesh_name))
      write (*, '(a,i0,a)') '[Wave] Registered ', active_count, ' vertices with preCICE'

      ! Do Triangulation
      ! Allocate temporary buffer, at most we have two triangles per node.
      allocate (triangle_nodes(2 * active_count * 3))
      num_triangles = 0
      do j = 1, swan_grid%nmax - 1
         do i = 1, swan_grid%mmax - 1
            if (swan_grid%kcs(i, j) == 1 &
            & .and. swan_grid%kcs(i + 1, j) == 1 &
            & .and. swan_grid%kcs(i + 1, j + 1) == 1 &
            & .and. swan_grid%kcs(i, j + 1) == 1) then
               ! Triangle one (bottom left)
               triangle_nodes(3 * num_triangles + 1) = node_indices(i, j)
               triangle_nodes(3 * num_triangles + 2) = node_indices(i + 1, j + 1)
               triangle_nodes(3 * num_triangles + 3) = node_indices(i, j + 1)
               num_triangles = num_triangles + 1
               ! Triangle two (top right)
               triangle_nodes(3 * num_triangles + 1) = node_indices(i, j)
               triangle_nodes(3 * num_triangles + 2) = node_indices(i + 1, j)
               triangle_nodes(3 * num_triangles + 3) = node_indices(i + 1, j + 1)
               num_triangles = num_triangles + 1
            end if
         end do
      end do

      if (num_triangles <= 0) then
         print *, '[Wave] Error in triangulation'
         return
      else
         print *, '[Wave] Successfully generated ', num_triangles, ' triangles for ', node_index, ' nodes'
      end if

      allocate (precice_triangle_nodes(3 * num_triangles))
      do i = 1, 3 * num_triangles
         precice_triangle_nodes(i) = precice_state%vertex_ids(triangle_nodes(i))
      end do

      call precicef_set_mesh_triangles(precice_state%swan_mesh_name, num_triangles, precice_triangle_nodes, len(precice_state%swan_mesh_name))
      print *, '[Wave] Registered ', num_triangles, ' triangles with preCICE'
   end subroutine register_wave_nodes_with_precice

   function get_swan_grid(mdw_file, swan_grid, number_of_active_nodes) result(retval)
      use read_grids, only: read_grd
      use swan_flow_grid_maps, only: grid
      implicit none(type, external)

      character(*), intent(in) :: mdw_file ! filename mdw file
      type(grid), intent(out) :: swan_grid ! Swan grid structure
      integer, intent(out) :: number_of_active_nodes
      integer :: retval ! return value: 0=success, 1=error

      integer :: lun ! file unit for mdw file
      integer :: ios ! I/O status
      character(256) :: line ! line read from file
      character(256) :: grid_filename ! grid filename from mdw
      character(256) :: key ! key part of key=value
      character(256) :: value ! value part of key=value
      integer :: eq_pos ! position of '=' in line
      logical :: in_domain ! flag: in [Domain] section
      logical :: grid_found ! flag: grid filename found

      retval = 0
      number_of_active_nodes = 0
      in_domain = .false.
      grid_found = .false.
      grid_filename = ' '

      open (newunit=lun, file=mdw_file, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         write (*, '(3a)') '[Wave] *** ERROR: Unable to open mdw file: ', trim(mdw_file)
         retval = 1
         return
      end if

      do while (.true.)
         read (lun, '(A)', iostat=ios) line
         if (ios /= 0) exit

         line = adjustl(line)
         if (line(1:8) == '[Domain]') then
            in_domain = .true.
            cycle
         end if

         ! Check for start of new section (exits [Domain])
         if (line(1:1) == '[' .and. in_domain) then
            in_domain = .false.
         end if

         if (in_domain) then
            eq_pos = index(line, '=')
            if (eq_pos > 0) then
               key = adjustl(line(1:eq_pos - 1))
               value = adjustl(line(eq_pos + 1:))

               if (trim(key) == 'Grid') then
                  grid_filename = trim(value)
                  grid_found = .true.
                  exit
               end if
            end if
         end if
      end do

      close (lun)

      if (.not. grid_found) then
         write (*, '(a)') '[Wave] *** ERROR: Grid filename not found in [Domain] section of mdw file'
         retval = 1
         return
      end if

      write (*, '(3a)') '[Wave] Reading Swan grid from file: ', trim(grid_filename)
      call read_grd(grid_filename, swan_grid%x, swan_grid%y, swan_grid%kcs, swan_grid%covered, &
                    swan_grid%mmax, swan_grid%nmax, swan_grid%sferic, swan_grid%xymiss)

      swan_grid%grid_name = grid_filename
      swan_grid%grid_file_type = 'FLOW'
      swan_grid%xy_loc = 'CORNER'
      swan_grid%npts = swan_grid%mmax * swan_grid%nmax

      block
         integer :: coord_i, coord_j
         do coord_j = 1, swan_grid%nmax
            do coord_i = 1, swan_grid%mmax
               if (swan_grid%kcs(coord_i, coord_j) == 1) then
                  number_of_active_nodes = number_of_active_nodes + 1
               end if
            end do
         end do
      end block

      ! Write coordinates to file for debugging
      block
         integer :: coord_unit, coord_i, coord_j
         open(newunit=coord_unit, file='wave_coordinates_debug.txt', status='replace', action='write')
         write(coord_unit, '(a,i0)') 'x-coordinates: ', number_of_active_nodes
         do coord_j = 1, swan_grid%nmax
            do coord_i = 1, swan_grid%mmax
               if (swan_grid%kcs(coord_i, coord_j) == 1) then
                  write(coord_unit, *) swan_grid%x(coord_i, coord_j)
               end if
            end do
         end do
         write(coord_unit, '(a,i0)') 'y-coordinates: ', number_of_active_nodes
         do coord_j = 1, swan_grid%nmax
            do coord_i = 1, swan_grid%mmax
               if (swan_grid%kcs(coord_i, coord_j) == 1) then
                  write(coord_unit, *) swan_grid%y(coord_i, coord_j)
               end if
            end do
         end do
         close(coord_unit)
      end block

      write (*, '(a,i0,a,i0,a)') '[Wave] Swan grid dimensions: mmax=', swan_grid%mmax, ', nmax=', swan_grid%nmax
   end function get_swan_grid
#endif
!
! ====================================================================================
function wave_main_init(mode_in, mdw_file) result(retval)
   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! use ifcore
   ! 
   use deltares_common_version_module
   implicit none
!
! return value
!
   integer :: retval
!
! Global variables
!
   integer      :: mode_in
   character(*) :: mdw_file     ! filename mdw file
!
! Local variables
!
   integer                :: n
   character(256)         :: full_version  !  Version nr. of the module of the current package
   character(1024)        :: branch        !  Git branch containing the source code
   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
!
!! executable statements -----------------------------------------------
!
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following two lines
   ! See also use statement above
   !
   ! NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
   ! OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
   !
   retval = 0
   !
   full_version  = ' '
   call getfullversionstring_dwaves(full_version)
   call getbranch_dwaves(branch)
   write (*,'(a)')
   write (*,'(80a1)') ('*', n = 1, 80)
   write (*,'(a)')    '***'
   write (*,'(2a)')   '*** ',trim(full_version)
   write (*,'(2a)')   '***           built from : ', trim(branch)
   write (*,'(a)')    '***'
   write (*,'(2a)')   '***           runid      : ', trim(mdw_file)
   write (*,'(a)')    '***'
   write (*,'(80a1)') ('*', n = 1, 80)
   write (*,'(a)')
   !
   call initialize_wavedata(wavedata)
   call initialize_wave_mpi()
   retval = wave_init(mode_in, mdw_file)

#if defined(HAS_PRECICE_WAVE_GREETER_COUPLING)
   call couple_to_greeter_dummy()
#endif
end function wave_main_init

!
! ====================================================================================
function wave_init(mode_in, mdw_file) result(retval)
   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! use ifcore
   ! 
   implicit none
!
! return value
!
   integer :: retval
!
! Global variables
!
   integer      :: mode_in
   character(*) :: mdw_file     ! filename mdw file
!
! Local variables
!
   logical                                      :: success      ! flag indicating whether delftio communication went correct
   integer                                      :: i_icefrac    ! index of ice fraction
   integer                                      :: i_floe       ! index of floe diameter
   integer                                      :: i_flow       ! counter
   integer                                      :: i_extfo      ! counter
   integer                                      :: i_swan       ! counter
   integer                                      :: it01flow     ! reference date obtained from FLOW
   integer                                      :: mtdim
   real                                         :: tscaleflow   ! basic time unit == flow time step (s)
   real(fp)       , dimension(:,:), allocatable :: x_fp         ! Copy of x-coordinate of grid in flexible precision, needed for external forcing module
   real(fp)       , dimension(:,:), allocatable :: y_fp         ! Copy of y-coordinate of grid in flexible precision, needed for external forcing module
   character(60) , dimension(:), allocatable    :: extforce_quantities
   character(256), dimension(:), pointer        :: extforce_types
   
   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
!
!! executable statements -----------------------------------------------
!
   retval = 0
   call setmode(wavedata, mode_in)
   !
   ! Read mdw file
   !
   call read_dwaves_mdw(mdw_file, swan_run, wavedata)
   !
   n_swan_grids = swan_run%nnest
   if (wavedata%mode/=stand_alone .and. swan_run%flowgridfile/=' ') then
      swan_run%nttide = 1
   endif

   !
   ! All instances need to read the input, but the actual work is done by the master only
   !
   if (my_rank /= master) return
   !
   ! Initialisation from flow (write file runid(s))
   !
   if (wavedata%mode/=stand_alone .and. swan_run%flowgridfile==' ') then
      it01flow = 0
      call flow_init(wavedata%mode, it01flow, tscaleflow)
      call settscale(wavedata%time, tscaleflow)
      if (wavedata%time%refdate == 0) then
         !
         ! No reference date in mdw-file or waves_alone
         ! Use reference date from flow
         !
         call setrefdate(wavedata%time, it01flow)
      else
         if (wavedata%time%refdate == it01flow) then
            !
            ! Reference date from flow is identical to reference date from mdw-file/waves_alone
            !
         else
            write(*,'(a,i8,a,i8,a)') '*** ERROR: Reference date from FLOW (', &
                & it01flow, ') differs from WAVE (', wavedata%time%refdate, ').'
            call wavestop(1, '*** ERROR: Reference date from FLOW differs from WAVE')
         endif
      endif
   else
      !
      ! stand_alone, flow data may be used or written
      !
      if (swan_run%useflowdata .or. swan_run%swwav) then
         !
         ! In this case, refdate and tscale are read from the com-file
         ! Only tscale must be set in wavedata%time
         !
         call flow_init(wavedata%mode, it01flow, tscaleflow)
         if (swan_run%flowgridfile == ' ') then
            !
            ! tscaleflow is only set when reading from com-file
            ! not when reading from netcdf-file
            !
            call settscale (wavedata%time, tscaleflow)
         endif
      endif
   endif
   if (wavedata%time%refdate == 0) then
      write(*,'(a)') '*** ERROR: Reference date not set'
      write(*,'(a)') '           Use Delft3D-WAVE-GUI version 4.90.00 or higher to create the mdw-file.'
      call wavestop(1, '*** ERROR: Reference date not set')
   endif
   !
   ! Read wave grids and flow grids; make grid-maps
   !
   call grids_and_gridmaps(n_swan_grids, n_flow_grids, swan_run, wavedata%mode)
   !
   ! Allocate swan output fields defined on flow grids; they have to be
   ! stored and updated over multiple nested swan runs
   !
   do i_flow=1,n_flow_grids
      flow_output_fields(i_flow)%n_outpars = 0
      call alloc_output_fields(flow_grids(i_flow),flow_output_fields(i_flow))
   enddo
   !
   ! Set mode to spherical if first swan grid is spherical
   !
   swan_run%sferic = swan_grids(1)%sferic
   !
   ! External forcing data from file?
   ! Only if 1 or more external forcing have been specified.
   !
   do i_swan = 1, n_swan_grids
      if (swan_run%dom(i_swan)%n_extforces > 0) then
         !
         ! Grid coordinates of all swan grids are needed by the external forcing module
         !
         success  = initmeteo(swan_grids(i_swan)%grid_name)
         call checkmeteoresult_wave(success)
         !
         ! Read the external forcing files
         !
         do i_extfo = 1, swan_run%dom(i_swan)%n_extforces
            success = addmeteoitem(swan_grids(i_swan)%grid_name                , &
                                 & swan_run%dom(i_swan)%extforce_names(i_extfo), &
                                 & swan_grids(i_swan)%sferic                   , &
                                 & swan_grids(i_swan)%mmax                     , &
                                 & swan_grids(i_swan)%nmax                     )
            call checkmeteoresult_wave(success)
         enddo
         !
         ! Allocate local copies of coordinate arrays
         ! Must be in flexible precision for the external forcing module
         !
         allocate(x_fp(swan_grids(i_swan)%mmax,swan_grids(i_swan)%nmax))
         allocate(y_fp(swan_grids(i_swan)%mmax,swan_grids(i_swan)%nmax))
         x_fp = real(swan_grids(i_swan)%x, fp)
         y_fp = real(swan_grids(i_swan)%y, fp)
         !
         success  = gridtometeo(   swan_grids(i_swan)%grid_name, &
                              &    swan_grids(i_swan)%nmax     , &
                              &    swan_grids(i_swan)%mmax     , &
                              & 1, swan_grids(i_swan)%nmax     , &
                              & 1, swan_grids(i_swan)%mmax     , &
                              &    swan_grids(i_swan)%kcs      , &
                              &    x_fp                        , &
                              &    y_fp                        )
         call checkmeteoresult_wave(success)
         !
         ! Deallocate local copies of coordinate arrays
         !
         deallocate(x_fp)
         deallocate(y_fp)
         nullify(extforce_types)
         mtdim = 0
         success = getmeteotypes(swan_grids(i_swan)%grid_name, extforce_types,mtdim)
         call checkmeteoresult_wave(success)
         do i_extfo = 1, size(extforce_types)
            if (extforce_types(i_extfo) == "meteo_on_computational_grid") then
               write(*,'(a)') '*** ERROR: "meteo on computational grid" (flow grid) is not supported by Delft3D-WAVE'
               call wavestop(1, '*** ERROR: "meteo on computational grid" (flow grid) is not supported by Delft3D-WAVE')
            endif
         enddo
         deallocate(extforce_types)
         !
         ! Some ice checks
         !
         if (swan_run%icedamp > 0) then
            i_icefrac = 0
            i_floe    = 0
            success = getmeteoquantities(swan_grids(i_swan)%grid_name, extforce_quantities)
            call checkmeteoresult_wave(success)
            do i_extfo = 1, size(extforce_quantities)
               if (extforce_quantities(i_extfo) == "sea_ice_area_fraction") then
                  i_icefrac = i_extfo
                  swan_run%dom(i_swan)%n_extforces = swan_run%dom(i_swan)%n_extforces - 1
               elseif (extforce_quantities(i_extfo) == "floe_diameter") then
                  i_floe = i_extfo
                  swan_run%dom(i_swan)%n_extforces = swan_run%dom(i_swan)%n_extforces - 1
               endif
            enddo
            !
            if (swan_run%icedamp == 1) then
               if (i_icefrac == 0 .and. i_floe == 0) then
                  write(*,'(a)') '*** ERROR: ice cover effect needs "sea_ice_area_fraction" and "floe_diameter" specification'
                  stop
               elseif (i_icefrac /= 0 .and. i_floe == 0) then
                  write(*,'(a)') '*** ERROR: "sea_ice_area_fraction" specified by a external forcing but "floe_diameter" is missing'
                  stop
               elseif (i_icefrac == 0 .and. i_floe /= 0) then
                  write(*,'(a)') '*** ERROR: "floe_diameter" specified by a external forcing but "sea_ice_area_fraction" is missing'
                  stop
               else
                  write(*,'(a)') '*** MESSAGE: Modelling ice cover effect, using "sea_ice_area_fraction" and "floe_diameter", specified by external forcing'
                  swan_run%dom(i_swan)%ice = 1
               endif
            elseif (swan_run%icedamp == 2) then
               if (i_icefrac == 0) then
                  write(*,'(a)') '*** ERROR: ice cover effect needs "sea_ice_area_fraction" specification by external forcing'
                  stop
               else
                  write(*,'(a)') '*** MESSAGE: Modelling ice cover effect, using "sea_ice_area_fraction", specified by external forcing'
                  swan_run%dom(i_swan)%ice = 1
               endif
            endif
            !
            deallocate(extforce_quantities)
         endif
      endif
   enddo
   !
   ! ====================================================================================
   ! CHECK
   ! ====================================================================================
   !
   call check_input(swan_run, wavedata)
end function wave_init



!
! ====================================================================================
function  wave_main_step(stepsize, precice_state) result(retval)
   use m_wave_precice_state_t, only: wave_precice_state_t
   implicit none
!
! return value
!
   integer :: retval
!
! Globals
!
   real(hp) :: stepsize
   type(wave_precice_state_t), intent(in) :: precice_state
!
! Local variables
!
   integer                                      :: command
!
!! executable statements -----------------------------------------------
!
   if (my_rank == master) then
      !
      ! master node does all the work ...
      !
      retval = wave_master_step(stepsize, precice_state)
   else
      !
      ! nothing to do for slave nodes except for waiting and calling swan as needed
      !
      retval = 0
      do
         call run_swan_slave (command, retval)
         if (command == SWAN_DONE) exit
      enddo
   endif
end function wave_main_step


!
! ====================================================================================
function wave_master_step(stepsize, precice_state) result(retval)
   use m_wave_precice_state_t, only: wave_precice_state_t
   implicit none
!
! return value
!
   integer :: retval
!
! Globals
!
   real(hp) :: stepsize
   type(wave_precice_state_t), intent(in) :: precice_state
!
! Locals
!
   logical                                      :: mud
   logical                                      :: success      ! flag indicating whether delftio communication went correct
   integer                                      :: command
   integer                                      :: i
   integer                                      :: ierr
   integer                                      :: iold
   integer                                      :: timtscale    ! time in tscale units, integer representation
   real(hp)                                     :: tend
!
!! executable statements -----------------------------------------------
!
   retval = 0
   !
   !
   if (wavedata%mode /= stand_alone) then
      !
      ! In combination with flow, perform the swan computation (including mapping etc.)
      !
      command = flow_wave_comm_perform_step
      do while (command == flow_wave_comm_perform_step)
         if (swan_run%flowgridfile == ' ') then
            write(*,'(a)') 'Waiting for communication with Delft3D-FLOW ...'
            mud     = .false.
            success = wave_from_flow_command(command, mud, timtscale)
            if ( .not. success ) then
               write(*,'(a)') '*** ERROR: Communication with Delft3D-FLOW failed.'
               call wavestop(1, '*** ERROR: Communication with Delft3D-FLOW failed.')
            endif
            if (wavedata%mode == flow_mud_online) then
               write(*,'(a)') 'Waiting for communication with MUD ...'
               mud     = .true.
               success = wave_from_flow_command(command, mud, timtscale)
               if ( .not. success ) then
                  write(*,'(a)') '*** ERROR: Communication with MUD layer failed.'
                  call wavestop(1, '*** ERROR: Communication with MUD layer failed.')
               endif
            endif
         else
            command = 0
         endif
         !
         write(*,'(a)')'*****************************************************************'
         write(*,'(a)')'*  Start of Delft3D-WAVE ...'
         !
         ! Update wave and wind conditions
         !
         if (swan_run%flowgridfile == ' ') then
            call settimtscale(wavedata%time, timtscale, swan_run%modsim, swan_run%nonstat_interval)
         else
            call settimsec(wavedata%time, wavedata%time%timsec + real(stepsize,sp), swan_run%modsim, swan_run%nonstat_interval)
         endif
         !
         ! Run n_swan nested SWAN runs
         !
         call swan_tot(n_swan_grids, n_flow_grids, wavedata, 0, precice_state)
         write(*,'(a)')'*  End of Delft3D-WAVE'
         write(*,'(a)')'*****************************************************************'
         !
         if (swan_run%flowgridfile == ' ') then
            mud = .false.
            call wave_to_flow_status(flow_wave_comm_result_ok, mud)
            if (wavedata%mode == flow_mud_online) then
               mud = .true.
               call wave_to_flow_status(flow_wave_comm_result_ok, mud)
            endif
         endif
      enddo
      call deallocate_flow_data()
   else
      !
      ! Standalone swan computation
      !
      if (swan_run%flowgridfile == ' ') then
         call swan_tot(n_swan_grids, n_flow_grids, wavedata, 0, precice_state)
      elseif (swan_run%timwav(1) < 0.0) then
         !
         ! No times specified in mdw file: just do one computation with specified timestep
         !
         swan_run%timwav(1) = wavedata%time%timsec + stepsize / 60.0_hp
         swan_run%nttide    = 1
         call swan_tot(n_swan_grids, n_flow_grids, wavedata, 0, precice_state)
      else
         !
         ! Times specified in mdw file: compute all specified times between tcur en tcur+tstep
         !
         tend = wavedata%time%timmin + stepsize / 60.0_hp
         iold = 0
         do while (wavedata%time%timmin <= tend)
            !
            ! Search an i for which timwav(i) lies between the "current time" and the "step_end_time",
            ! including the boundaries "current time" and the "step_end_time":
            ! This is needed when timwav(1)=0
            !
            i = iold + 1
            do while (i <= swan_run%nttide)
               if (swan_run%timwav(i) >= wavedata%time%timmin .and. swan_run%timwav(i) <= tend) then
                  exit
               else
                  i = i + 1
               endif
            enddo
            if (i <= swan_run%nttide .and. i/=iold) then
               !
               ! Found an i to do a computation
               ! iold is needed when timwav(i)="current time" or "step_end_time",
               ! to avoid doing the same computation twice
               !
               call swan_tot(n_swan_grids, n_flow_grids, wavedata, i, precice_state)
               iold = i
            else
               !
               ! No computation left to be done
               ! Set the current time to "step_end_time" (to be sure it has the correct value)
               ! and exit the do-loop
               !
               call settimmin(wavedata%time, real(tend,hp), swan_run%modsim, swan_run%nonstat_interval)
               exit
            endif
         enddo
      endif
   endif
   if (numranks>1) call wave_mpi_bcast(SWAN_DONE, ierr)
end function wave_master_step



!
! ====================================================================================
function wave_main_finish() result(retval)
   use wave_mpi
   use precice, only: precicef_finalize
   implicit none
!
! return value
!
   integer :: retval
!
!! executable statements -----------------------------------------------
!
   if (my_rank == master) then
      !
      ! master node does all the work ...
      !
      retval = wave_master_finish()
   else
      !
      ! slave nodes only need to finalize MPI
      !
      retval = 0
   endif

#if defined(HAS_PRECICE_WAVE_GREETER_COUPLING) || defined(HAS_PRECICE_FM_WAVE_COUPLING)
   call precicef_finalize()
#endif
end function wave_main_finish


!
! ====================================================================================
function wave_master_finish() result(retval)
   use swan_input
   use wave_mpi
   implicit none
!
! return value
!
   integer :: retval
!
! local variables
!
   integer                                      :: i_swan       ! counter
!
!! executable statements -----------------------------------------------
!
   retval = 0
   !
   call del_temp_files(n_swan_grids)
   !
   ! Deallocate memory used by external forcing module
   !
   do i_swan = 1, n_swan_grids
      call deallocmeteo(swan_grids(i_swan)%grid_name)
   enddo
   !
   call dealloc_swan(swan_run)
   write(*,'(a)') 'Delft3D-WAVE finished normally.'
end function wave_master_finish


!
! ====================================================================================
function wave_main_set_com_interval(interval) result(retval)
   implicit none
!
! return value
!
   integer :: retval
!
! Globals
!
   real(hp) :: interval
!
!! executable statements -----------------------------------------------
!
   swan_run%deltcom = real( interval / 60.0_hp, sp)
   retval = 0
end function wave_main_set_com_interval

end module wave_main
