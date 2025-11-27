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
!
module unstruc_api
   use m_updatevaluesonsourcesinks, only: updatevaluesonsourcesinks
   use m_updatebalance, only: updatebalance
   use m_flow_usertimestep, only: flow_usertimestep
   use m_flow_externaloutput, only: flow_externaloutput
   use m_updatevaluesonrunupgauges_mpi, only: updatevaluesonrunupgauges_mpi
   use m_updatevaluesonrunupgauges, only: updatevaluesonrunupgauges
   use m_updatevaluesonlaterals, only: updatevaluesonlaterals
   use m_resetfullflowmodel, only: resetfullflowmodel
   use m_inidat, only: inidat, loadfile, savefile
   use m_write_some_final_output, only: write_some_final_output
   use m_writecdcoeffs, only: writeCdcoeffs
   use m_plotnu
   use m_choices
   use m_flowtimes
   use m_timer
   use m_flowgeom
   use unstruc_files, only: mdia
   use, intrinsic :: iso_c_binding, only: c_char, c_int, c_double

   implicit none

   real(kind=dp) :: cpuall0

   interface
#ifdef WIN32
#warning "C Preprocessor got here!"
      subroutine tricall(jatri, xs, ys, ns, indx, numtri, edgeidx, numedge, triedge, xs3, ys3, ns3, trisize) bind(C, name="TRICALL")
#else
      subroutine tricall(jatri, xs, ys, ns, indx, numtri, edgeidx, numedge, triedge, xs3, ys3, ns3, trisize) bind(C, name="tricall_")
#endif
         use, intrinsic :: iso_c_binding, only: c_int, c_double
         implicit none(type, external)
         integer(kind=c_int), intent(in) :: jatri
         real(kind=c_double), dimension(*), intent(in) :: xs
         real(kind=c_double), dimension(*), intent(in) :: ys
         integer(kind=c_int), intent(in) :: ns
         integer(kind=c_int), dimension(*), intent(out) :: indx
         integer(kind=c_int), intent(inout) :: numtri
         integer(kind=c_int), dimension(*), intent(out) :: edgeidx
         integer(kind=c_int), intent(out) :: numedge
         integer(kind=c_int), dimension(*), intent(out) :: triedge
         real(kind=c_double), dimension(*), intent(out) :: xs3
         real(kind=c_double), dimension(*), intent(out) :: ys3
         integer(kind=c_int), intent(inout) :: ns3
         real(kind=c_double), intent(in) :: trisize
      end subroutine tricall
   end interface
contains

   subroutine initialize_precice_coupling(precice_state)
      use precice, only: precicef_create, precicef_create_with_communicator, precicef_get_mesh_dimensions, precicef_set_vertices, &
                         precicef_initialize, precicef_write_data, precicef_advance, precicef_get_max_time_step_size, precicef_requires_initial_data
      use m_partitioninfo, only: jampi, numranks, my_rank, DFM_COMM_DFMWORLD
      use m_flowgeom, only: bl
      use m_flow, only: s1
      use m_fm_precice_state_t, only: fm_precice_state_t
      use, intrinsic :: iso_c_binding, only: c_int, c_char
      implicit none(type, external)

      type(fm_precice_state_t), intent(out) :: precice_state

      character(kind=c_char, len=*), parameter :: precice_config_name = "../precice_config.xml"
      integer(kind=c_int) :: is_initial_data_required

      precice_state = fm_precice_state_t()
      if (jampi == 0) then
         print *, '[FM] Initializing preCICE for serial execution'
         call precicef_create(precice_state%component_name, precice_config_name, my_rank, numranks, len(precice_state%component_name), len(precice_config_name))
      else
         print *, '[FM] Initializing preCICE for parallel execution with ', numranks, ' ranks. This is rank ', my_rank
         call precicef_create_with_communicator(precice_state%component_name, precice_config_name, my_rank, numranks, DFM_COMM_DFMWORLD, len(precice_state%component_name), len(precice_config_name))
      end if
      call register_flow_nodes_with_precice(precice_state%flow_vertex_ids)
      call precicef_requires_initial_data(is_initial_data_required)
      if (is_initial_data_required /= 0) then
         call precice_write_data(precice_state, bl, s1)
      end if
      call precicef_initialize()
   end subroutine initialize_precice_coupling

   subroutine register_flow_nodes_with_precice(flow_vertex_ids)
      use precice, only: precicef_set_vertices, precicef_set_mesh_triangles, precicef_requires_mesh_connectivity_for
      use m_flowgeom, only: xz, yz, ndxi
      implicit none(type, external)

      integer(kind=c_int), dimension(:), allocatable, intent(out) :: flow_vertex_ids

      character(kind=c_char, len=*), parameter :: mesh_name = "fm_flow_nodes"
      real(kind=c_double), dimension(:), allocatable :: x_coordinates
      real(kind=c_double), dimension(:), allocatable :: y_coordinates
      real(kind=c_double), dimension(:), allocatable :: mesh_coordinates
      integer(kind=c_int), dimension(:), allocatable :: triangle_nodes
      integer(kind=c_int), dimension(:), allocatable :: precice_triangle_nodes
      real(kind=c_double), dimension(1) :: dummy_xs3, dummy_ys3
      integer(kind=c_int), dimension(1) :: dummy_edge_nodes, dummy_triangle_edges
      integer(kind=c_int) :: dummy_ns3, dummy_num_edges
      real(kind=c_double) :: dummy_trisize
      integer(kind=c_int) :: num_triangles, jatri, is_required, i

      x_coordinates = xz(1:ndxi)
      y_coordinates = yz(1:ndxi)

      allocate (mesh_coordinates(2 * ndxi))
      do i = 1, ndxi
         mesh_coordinates(2 * i - 1) = x_coordinates(i)
         mesh_coordinates(2 * i) = y_coordinates(i)
      end do

      allocate (flow_vertex_ids(ndxi))
      call precicef_set_vertices(mesh_name, ndxi, mesh_coordinates, flow_vertex_ids, len(mesh_name))
      print *, '[FM] Registered ', ndxi, ' vertices with preCICE'

      call precicef_requires_mesh_connectivity_for(mesh_name, is_required, len(mesh_name))

      if (is_required == 0) then
         print *, '[FM] Mesh connectivity does not need to be registered with PreCICE.'
         return
      else
         print *, '[FM] Registering mesh with PreCICE.'
      end if

      ! Allocate arrays for triangulation output
      ! Maximum possible triangles for n points is approximately 2*n
      num_triangles = 2 * ndxi
      allocate (triangle_nodes(3 * num_triangles))

      dummy_ns3 = 0
      dummy_trisize = 0.0_c_double

      ! Call tricall with jatri=1 for Delaunay triangulation
      jatri = 1
      call tricall(jatri, x_coordinates, y_coordinates, ndxi, &
                   triangle_nodes, num_triangles, dummy_edge_nodes, dummy_num_edges, &
                   dummy_triangle_edges, dummy_xs3, dummy_ys3, dummy_ns3, dummy_trisize)

      if (num_triangles < 0) then
         print *, '[FM] Error in triangulation'
         return
      else
         print *, '[FM] Successfully generated ', num_triangles, ' triangles for ', ndxi, ' nodes'
      end if

      allocate (precice_triangle_nodes(3 * num_triangles))
      do i = 1, 3 * num_triangles
         precice_triangle_nodes(i) = flow_vertex_ids(triangle_nodes(i))
      end do

      call precicef_set_mesh_triangles(mesh_name, num_triangles, precice_triangle_nodes, len(mesh_name))
      print *, '[FM] Registered ', num_triangles, ' triangles with preCICE'
   end subroutine register_flow_nodes_with_precice

   function is_coupling_ongoing() result(is_ongoing)
      use precice, only: precicef_is_coupling_ongoing
      use, intrinsic :: iso_c_binding, only: c_int
      implicit none(type, external)
      integer(kind=c_int) :: is_ongoing

      call precicef_is_coupling_ongoing(is_ongoing)
   end function is_coupling_ongoing

   subroutine advance_precice_time_window(user_time_step, precice_state, bed_levels, water_levels)
      use precice, only: precicef_advance
      use precision, only: dp
      use, intrinsic :: iso_c_binding, only: c_double
      use m_fm_precice_state_t, only: fm_precice_state_t
      implicit none(type, external)

      real(kind=dp), intent(in) :: user_time_step
      type(fm_precice_state_t), intent(in) :: precice_state
      real(kind=dp), dimension(:), intent(in) :: bed_levels
      real(kind=dp), dimension(:), intent(in) :: water_levels

      if (is_coupling_ongoing() /= 0) then
         call precice_write_data(precice_state, bed_levels, water_levels)
         call precicef_advance(real(user_time_step, kind=c_double))
      end if
   end subroutine advance_precice_time_window

   subroutine finalize_precice_coupling()
      use precice, only: precicef_finalize
      implicit none(type, external)
      call precicef_finalize()
   end subroutine finalize_precice_coupling

   subroutine precice_write_data(precice_state, bed_levels, water_levels)
      use precision, only: dp
      use m_fm_precice_state_t, only: fm_precice_state_t
      implicit none(type, external)
      type(fm_precice_state_t), intent(in) :: precice_state
      real(kind=dp), dimension(:), intent(in) :: bed_levels
      real(kind=dp), dimension(:), intent(in) :: water_levels

      call precice_write_bed_levels(precice_state, bed_levels)
      call precice_write_water_levels(precice_state, water_levels)
      call precice_write_flow_velocities(precice_state)
      call precice_write_wind(precice_state)
      call precice_write_vegetation(precice_state)
   end subroutine precice_write_data

   subroutine precice_write_bed_levels(precice_state, bed_levels)
      use precision, only: dp
      use m_fm_precice_state_t, only: fm_precice_state_t
      use, intrinsic :: iso_c_binding, only: c_double
      use precice, only: precicef_write_data
      implicit none(type, external)
      type(fm_precice_state_t), intent(in) :: precice_state
      real(kind=dp), dimension(:), intent(in) :: bed_levels

      real(kind=c_double), dimension(:), allocatable :: inverted_bed_levels
      integer :: i

      allocate(inverted_bed_levels(size(precice_state%flow_vertex_ids)))
      do i = 1, size(precice_state%flow_vertex_ids)
         inverted_bed_levels(i) = -bed_levels(i)
      end do
      call precicef_write_data(precice_state%mesh_name, precice_state%bed_levels_name, &
                               size(precice_state%flow_vertex_ids), precice_state%flow_vertex_ids, &
                               inverted_bed_levels, len(precice_state%mesh_name), len(precice_state%bed_levels_name))
   end subroutine precice_write_bed_levels

   subroutine precice_write_water_levels(precice_state, water_levels)
      use precision, only: dp
      use m_fm_precice_state_t, only: fm_precice_state_t
      use precice, only: precicef_write_data
      implicit none(type, external)
      type(fm_precice_state_t), intent(in) :: precice_state
      real(kind=dp), dimension(:), intent(in) :: water_levels

      call precicef_write_data(precice_state%mesh_name, precice_state%water_levels_name, &
                               size(precice_state%flow_vertex_ids), precice_state%flow_vertex_ids, &
                               water_levels, len(precice_state%mesh_name), len(precice_state%water_levels_name))
   end subroutine precice_write_water_levels

   subroutine precice_write_flow_velocities(precice_state)
      use precision, only: dp
      use m_get_ucx_ucy_eul_mag, only: getucxucyeulmag
      use m_fm_precice_state_t, only: fm_precice_state_t
      use precice, only: precicef_write_data
      use m_flow, only: kmx
      use messagehandling, only: err
      implicit none(type, external)

      type(fm_precice_state_t), intent(in) :: precice_state
      real(kind=dp), dimension(:), allocatable :: flow_velocity_vector, flow_velocity_magnitude

      integer :: n_points

      ! n_points equals ndx (the number of flow nodes in 2D) but the velocity vector has ndkx elements (3D nodes)
      ! We do not support 3D FM models through preCICE, check and exit if it does contain one
      n_points = size(precice_state%flow_vertex_ids)
      if (kmx > 0) then
         call err('[FM] Error: preCICE coupling only supports 2D FM models. Current model contains 3D nodes.')
      end if
      allocate(flow_velocity_vector(n_points * 2))
      allocate(flow_velocity_magnitude(n_points))

      call getucxucyeulmag(n_points, flow_velocity_vector(1::2), flow_velocity_vector(2::2), flow_velocity_magnitude, 0, 0)

      call precicef_write_data(precice_state%mesh_name, precice_state%flow_velocity_name, &
                               n_points, precice_state%flow_vertex_ids, &
                               flow_velocity_vector, len(precice_state%mesh_name), len(precice_state%flow_velocity_name))
   end subroutine precice_write_flow_velocities

   subroutine precice_write_wind(precice_state)
      use, intrinsic :: iso_c_binding, only: c_double
      use m_fm_precice_state_t, only: fm_precice_state_t
      use m_wind, only: wx, wy, jawind
      use precice, only: precicef_write_data
      implicit none(type, external)

      type(fm_precice_state_t), intent(in) :: precice_state

      integer :: n_points, n, i
      real(kind=c_double), dimension(:), allocatable :: wind_velocity_vector

      if (jawind == 0) then
         return
      end if
      n_points = size(precice_state%flow_vertex_ids)
      allocate (wind_velocity_vector(2 * n_points), source=0.0_c_double)

      do n = 1, n_points
         if (nd(n)%lnx > 0) then
            do i = 1, nd(n)%lnx
               wind_velocity_vector(2 * n - 1) = wind_velocity_vector(2 * n - 1) + wx(abs(nd(n)%ln(i)))
               wind_velocity_vector(2 * n) = wind_velocity_vector(2 * n) + wy(abs(nd(n)%ln(i)))
            end do
            wind_velocity_vector(2 * n - 1) = wind_velocity_vector(2 * n - 1) / nd(n)%lnx
            wind_velocity_vector(2 * n) = wind_velocity_vector(2 * n) / nd(n)%lnx
         end if
      end do
      call precicef_write_data(precice_state%mesh_name, precice_state%wind_velocity_name, &
                               n_points, precice_state%flow_vertex_ids, &
                               wind_velocity_vector, len(precice_state%mesh_name), len(precice_state%wind_velocity_name))
   end subroutine precice_write_wind

   subroutine precice_write_vegetation(precice_state)
      use precice, only: precicef_write_data
      use m_vegetation, only: rnveg, diaveg, stemheight, javeg
      use m_fm_precice_state_t, only: fm_precice_state_t
      implicit none(type, external)

      type(fm_precice_state_t), intent(in) :: precice_state

      integer :: n_points
      if (javeg == 0) then
         return
      end if
      n_points = size(precice_state%flow_vertex_ids)

      call precicef_write_data(precice_state%mesh_name, precice_state%vegetation_stem_density_name, &
                               n_points, precice_state%flow_vertex_ids, &
                               rnveg, len(precice_state%mesh_name), len(precice_state%vegetation_stem_density_name))

      call precicef_write_data(precice_state%mesh_name, precice_state%vegetation_diameter_name, &
                               n_points, precice_state%flow_vertex_ids, &
                               diaveg, len(precice_state%mesh_name), len(precice_state%vegetation_diameter_name))

      call precicef_write_data(precice_state%mesh_name, precice_state%vegetation_height_name, &
                               n_points, precice_state%flow_vertex_ids, &
                               stemheight, len(precice_state%mesh_name), len(precice_state%vegetation_height_name))
   end subroutine precice_write_vegetation

!> Initializes global program/core data, not specific to a particular model.
   subroutine init_core()
      use network_data
      use m_polygon
      use m_landboundary
      use M_splines
      use m_monitoring_crosssections
      use unstruc_files
      use dflowfm_version_module, only: base_name
      use gridoperations
      use m_samples
      use m_increase_grid

!if (.not. allocated(xk)) then
      !   allocate( xk (1), yk (1), zk (1) , NOD (1) , KC (1) , NMK (1) , RNOD(1)   )
      !   allocate(nod(1)%lin(1))
      !   allocate( xk0(1), yk0(1), zk0(1) , NOD0(1) , KC0(1) , NMK0(1), KN0(1,1), LC0(1)  )
      !   allocate(nod0(1)%lin(1))
      !   nmk0 = 0
      !endif
      !

      call inidia(base_name)

      KMAX = 2
      LMAX = 2
      call INCREASENETW(KMAX, LMAX)

      call INCREASEPOL(MAXPOL, 0)
      write (*, *) 'increased pols'
      call INCREASEGRID(2, 2)
      write (*, *) 'increased grid'

      call increasespl(maxspl, maxsplen)
      write (*, *) 'increased spl'

      call increaseCrossSections(maxcrs)
      write (*, *) 'increased crs'

      call INCREASESAM(2)
      write (*, *) 'increased sam'

      call INCREASELAN(MAXLAN)
      write (*, *) 'increased lan'
      ! Required or we're stuck with unallocated xpl

   end subroutine init_core

!> Executes a batch of dflowfm model runs, defined by a .batdfm input file.
!! Contents of .batdfm file: a sequence of lines, with the following
!! supported keywords:
!! LOAD=basemodel.mdu : The main MDU file that is the starting point for all runs.
!! START PARAMETERS=variant.mdu : Small MDU-fragments file, overriding any settings read from the basemodel.
!!                                Will auto-save the complete file after reading, under the same name.
!! CHOICES=NUM NWHAT  : execute some GUI-menu commands, NUM = menu number, NWHAT = menu item number.
!! RUN                : Run the loaded model/variant.
!! SAVE=newfilename   : Saves an MDU/net.nc/other file.
   subroutine batch(batfile) !
      use m_flow
      use m_flowgeom
      use m_monitoring_crosssections
      use unstruc_model
      use m_qn_read_error
      use m_filez, only: oldfil, doclose, newfil
      use m_upotukinueaa, only: upotukinueaa

      implicit none
      integer :: ierr, minp, mout, L1, istat, i
      integer :: MODE, NUM, NWHAT, KEY
      real(kind=dp) :: QQQ, upot, ukin, ueaa
      character(len=*) :: batfile
      character(len=256) :: rec, filnam, basemdu, tex

      call resetFullFlowModel()

      call default_flowtimes ! anders crasht ie

      call oldfil(minp, batfile)
      call newfil(mout, trim(batfile)//'.out')
      write (mout, '(A)') '                   mdu  : , kmx, numtopsig, numtopsiguniform, keepzlayeringatbed, ihuz, ihuzcsig, dtav : '// &
         ' Qcrs1  (m3/s), ueaa (kg/(ms2)),  upot(kg/(ms2)), ukin (kg/(ms2)), utot (kg/(ms2)) '

111   read (minp, '( A )', end=999) rec

      if (index(rec, 'LOAD') > 0) then ! load a new model or file
         L1 = index(rec, '=') + 1
         read (rec(L1:), '(A)', err=888) filnam
         call loadfile(filnam)
         if (index(filnam, '.mdu') > 0) then
            basemdu = filnam
         end if
      end if

      if (index(rec, 'SAVE') > 0) then ! save a model or file
         L1 = index(rec, '=') + 1
         read (rec(L1:), '(A)', err=888) filnam
         call savefile(filnam)
      end if

      if (index(rec, 'CHOICES') > 0) then ! first check your choices
         L1 = index(rec, '=') + 1
         read (rec(L1:), *, err=888) NUM, NWHAT
         MODE = 1; KEY = 3
         call CHOICES(NUM, NWHAT, KEY)
      end if

      if (index(rec, 'START PARAMETERS') > 0) then ! specify new model with only few parameters changed through readmdufile
         call loadfile(basemdu)
         L1 = index(rec, '=') + 1
         read (rec(L1:), '(A)', err=888) filnam

         call readMDUFile(filnam, istat) ! , minp)          ! change few params from short mdu-like pieces in bat file

         call WriteMDUFile(filnam, istat) ! for logging, save new mdu file
         call loadfile(filnam) !
      end if

      if (index(rec, 'RUN') > 0) then

         ierr = flow()

         if (ncrs > 0) then

            QQQ = crs(1)%sumvalcur(1); i = 1
            write (tex(i:), '(i2.0)') kmx; i = i + 5
            write (tex(i:), '(i2.0)') numtopsig; i = i + 5
            write (tex(i:), '(i2.0)') janumtopsiguniform; i = i + 5
            write (tex(i:), '(i2.0)') keepzlayeringatbed; i = i + 5
            write (tex(i:), '(i2.0)') ihuz; i = i + 5
            write (tex(i:), '(i2.0)') ihuzcsig; i = i + 5
            write (tex(i:), '(F5.2)') (time1 - tstart_user) / max(1d0, dnt); i = i + 5

            call upotukinueaa(upot, ukin, ueaa)
            write (mout, '(A30,A, 5F14.3)') filnam(1:30), ' :    '//trim(tex)//' : ', QQQ, ueaa, upot, ukin, upot + ukin
         end if

      end if

      goto 111

888   call QNREADERROR('Trying to Read a filename but Getting', REC, MINP)
      call doclose(MINP)
      call doclose(Mout)
      return

999   call doclose(mout)
      return

   end subroutine batch

   integer function flow() result(iresult)
      use dfm_error
      use unstruc_display
      use messagehandling, only: warn_flush
      use unstruc_display
      use unstruc_model
      use m_fm_precice_state_t, only: fm_precice_state_t, global_fm_precice_state
      integer :: jastop

      iresult = DFM_NOERR
      call mess(LEVEL_INFO, 'Start of the computation time loop')
      iresult = flowinit(global_fm_precice_state)
      jastop = 0
      do while (time_user < tstop_user .and. jastop == 0 .and. iresult == DFM_NOERR) ! time loop
         call flowstep(jastop, iresult, global_fm_precice_state)
      end do
      if (iresult /= DFM_NOERR) then
         call mess(LEVEL_WARN, 'Error during computation time loop. Details follow:')
         call dfm_strerror(msgbuf, iresult)
         call warn_flush()
      end if

      call write_some_final_output()

      if (jagui > 0) then
         call plotnu(md_ident)
      end if

      if (jastop == 0 .and. jaGUI == 0) then
         call flowfinalize()
      end if

   end function flow

   subroutine api_loadmodel(filename)
      use unstruc_model
      character(len=*), intent(in) :: filename

      character(len=len(filename) + 5) :: file_name ! local copy of filename, as loadmodel inserts partition number in case of parallel computing

      call resetFullFlowModel()
      write (*, *) 'loading model'
      file_name = filename
      call loadmodel(file_name)
      write (*, *) 'model loaded'
   end subroutine api_loadmodel

   function flowinit(precice_state) result(iresult)
      use timers
      use unstruc_model
      use unstruc_netcdf, only: unc_write_net_flowgeom
      use m_crosssections
      use network_data
      use unstruc_files
      use waq
      use m_laterals, only: numlatsg
      use m_wind, only: jawind
      use dfm_error
      use m_partitioninfo, only: jampi
      use m_flowparameters, only: jahisbal, jatekcd, jahislateral, jawriteDetailedTimers
      use fm_statistical_output, only: out_variable_set_his, out_variable_set_map, out_variable_set_clm
      use m_update_values_on_cross_sections, only: update_values_on_cross_sections
      use m_statistical_output, only: update_source_input, update_statistical_output
      use m_wall_clock_time
      use m_flow_modelinit, only: flow_modelinit
      use m_fm_precice_state_t, only: fm_precice_state_t

      type(fm_precice_state_t), intent(out) :: precice_state
      integer :: iresult

      integer :: timerHandle, inner_timerhandle
      !call inidia('api')

      timerHandle = 0
      call timstrt('Initialise flow', timerHandle)
      iresult = DFM_NOERR
      if (ndx == 0) then
         write (*, *) 'Initializing flow: flow_modelinit'
         inner_timerhandle = 0
         call timstrt('Flow model initialisation', inner_timerhandle)
         iresult = flow_modelinit()
         call timstop(inner_timerhandle)

         if (jawind > 0 .and. jatekcD > 0) then
            inner_timerhandle = 0
            call timstrt('Write CD coefficients', inner_timerhandle)
            call writeCdcoeffs()
            call timstop(inner_timerhandle)
         end if

      end if
      if (ndx == 0) then
         call timstop(timerhandle)
         return ! No valid flow network was initialized
      end if

      call wall_clock_time(cpuall0)

      inner_timerhandle = 0
      call timstrt('Update various', inner_timerhandle)

      call update_values_on_cross_sections
      call updateValuesOnRunupGauges()
      if (jahisbal > 0) then ! Update WaterBalances etc.
         call updateBalance()
      end if
      call updateValuesonSourceSinks(time1)

      if (jahislateral > 0 .and. numlatsg > 0 .and. ti_his > 0) then
         call updateValuesOnLaterals(time1, dts)
      end if

      if (jampi == 1) then
         call updateValuesOnRunupGauges_mpi()
      end if
      call timstop(inner_timerhandle)

      call update_source_input(out_variable_set_his)
      call update_source_input(out_variable_set_map)
      call update_source_input(out_variable_set_clm)

      if (out_variable_set_his%count > 0) then
         call update_statistical_output(out_variable_set_his%statout, dts)
      end if
      !call update_statistical_output(out_variable_set_map%configs,dts)
      !call update_statistical_output(out_variable_set_clm%configs,dts)

      call mess(LEVEL_INFO, 'Writing initial output to file(s)...')
      inner_timerhandle = 0
      call timstrt('Write external output', inner_timerhandle)
      call flow_externaloutput(time1)
      call timstop(inner_timerhandle)

      call mess(LEVEL_INFO, 'Done writing initial output to file(s).')
      call timstop(timerhandle)
      call timstop(handle_all) ! This stores all time spent so far for model initialization.
      call timstrt('All', handle_all)
      if (jawriteDetailedTimers > 0) then
         call timdump(trim(defaultFilename('timers_init')), .true.)
      end if

      call initialize_precice_coupling(precice_state)
   end function flowinit

   subroutine flowstep(jastop, iresult, precice_state)
      use unstruc_display, only: ntek, plottofile
      use m_gui
      use dfm_error
      use m_drawthis
      use m_draw_nu
      use m_fm_precice_state_t, only: fm_precice_state_t
      use m_flowtimes, only: dt_user
      use m_flowgeom, only: bl
      use m_flow, only: s1
      implicit none(type, external)

      integer, intent(out) :: jastop !< Communicate back to caller: whether to stop computations (1) or not (0)
      integer, intent(out) :: iresult !< Return code: DFM_NOERR on success, error code otherwise
      type(fm_precice_state_t), intent(in) :: precice_state
      integer :: key

      jastop = 0
      iresult = DFM_GENERICERROR

      if (jatimer == 1) call starttimer(ITOTAL)

      if (ndx == 0) then ! No valid flow network was initialized
         jastop = 1
         goto 1234
      end if

      call flow_usertimestep(key, iresult) ! one user_step consists of several flow computational time steps

      call advance_precice_time_window(dt_user, precice_state, bl, s1)
      if (iresult /= DFM_NOERR) then
         jastop = 1
         goto 888
      end if

      if (key == 1) then
         jastop = 1
         goto 1234
      end if

      if (jaGUI == 1) then
         key = 3 ! this part is for online visualisation
         if (ntek > 0) then
            if (mod(int(dnt_user), ntek) == 0) then
               if (plottofile == 1) then
                  ndraw(10) = plottofile
               end if
               call drawnu(key)
               if (key == 1) then
                  goto 1234
               end if
            end if
         end if
      end if

1234  if (jatimer == 1) call stoptimer(ITOTAL)

      iresult = DFM_NOERR
      return ! Return with success

888   continue
      ! Error
   end subroutine flowstep

!> Finishes a run of the current model (timings/statistics).
!! For a restart, subsequently call a flowinit and flow/flowstep again.
   subroutine flowfinalize()
      use unstruc_files
      use unstruc_netcdf
      use MessageHandling, only: FinalizeMessageHandling
      use m_ec_module
      use m_meteo, only: ecInstancePtr
      use m_nearfield
      use m_laterals
      use fm_statistical_output, only: close_fm_statistical_output

      call dealloc_nfarrays()
      call dealloc_lateraldata()
      call close_fm_statistical_output()
      call finalize_precice_coupling()

      if (.not. ecFreeInstance(ecInstancePtr)) then
         continue
      end if
      call close_all_files()
      call unc_closeall()
      mapids%ncid = 0 !< Reset global map-file ncid
      ihisfile = 0 !< Reset global his-file ncid
      call FinalizeMessageHandling()
      close (mdia)
      mdia = 0
   end subroutine flowfinalize

end module unstruc_api
