module test_pillar_orientation
   use assertions_gtest
   use precision, only: dp
   use m_file_helpers, only: create_file

   implicit none(type, external)
   private
   public :: test_pillar_orientation_invariant

contains

   !$gtest
   !> The same flow setup must produce the same water levels for either pillar orientation.
   subroutine test_pillar_orientation_invariant() bind(C)
      use unstruc_api, only: init_core, api_loadmodel, flowinit, flowfinalize
      use fm_external_forcings_data, only: Cpil
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi, numranks, my_rank
      use unstruc_display, only: jagui

      real(kind=dp), dimension(:), allocatable :: waterlevel_sn, waterlevel_we
      integer :: status, stop_requested, step
      logical :: success

      jampi = 0
      numranks = 1
      my_rank = 0
      jagui = 0
      call create_pillar_inputs()
      call init_core()

      call api_loadmodel("pillar_sn.mdu")
      status = flowinit()
      if (.not. f90_assert_eq(status, DFM_NOERR, "The north-south pillar model must initialize")) then
         return
      end if
      if (.not. assert_pillars_active()) then
         call flowfinalize()
         return
      end if
      call run_model(waterlevel_sn, success)
      call flowfinalize()
      if (.not. f90_assert_true(success, "The north-south pillar model must reach its end time")) then
         return
      end if

      call api_loadmodel("pillar_we.mdu")
      status = flowinit()
      if (.not. f90_assert_eq(status, DFM_NOERR, "The west-east pillar model must initialize")) then
         return
      end if
      if (.not. assert_pillars_active()) then
         call flowfinalize()
         return
      end if
      call run_model(waterlevel_we, success)
      call flowfinalize()
      if (.not. f90_assert_true(success, "The west-east pillar model must reach its end time")) then
         return
      end if 
       
      if (.not. f90_assert_eq(size(waterlevel_sn), size(waterlevel_we), "Both models must use the same flow grid")) then
         return
      end if
      call f90_expect_near(waterlevel_sn, waterlevel_we, 1.0e-5_dp, &
                           "Water levels must be independent of pillar orientation")
   end subroutine test_pillar_orientation_invariant

   !> Check that each generated input actually initialized pillar drag.
   logical function assert_pillars_active()
      use fm_external_forcings_data, only: Cpil

      assert_pillars_active = .false.
      if (.not. f90_assert_true(allocated(Cpil), "Pillar drag coefficients must be allocated")) then
         return
      end if
      if (.not. f90_assert_true(any(abs(Cpil) > 0.0_dp), "Pillar drag must be active")) then
         return
      end if
      assert_pillars_active = .true.
   end function assert_pillars_active

   subroutine run_model(waterlevel, success)
      use unstruc_api, only: flowstep
      use m_flowtimes, only: time_user, tstop_user
      use m_flow, only: s1
      use m_flowgeom, only: ndx
      use dfm_error, only: DFM_NOERR
      use precision, only: dp

      real(kind=dp), dimension(:), allocatable, intent(out) :: waterlevel
      logical, intent(out) :: success
      integer :: status, stop_requested, step

      success = .false.
      do step = 1, 4000
         if (time_user >= tstop_user) then
            exit
         end if
         call flowstep(stop_requested, status)
         if (.not. f90_assert_eq(status, DFM_NOERR, "The model timestep must succeed")) then
            return
         end if
         if (.not. f90_assert_eq(stop_requested, 0, "The model must reach the requested end time")) then
            return
         end if
      end do
      call f90_expect_near(time_user, tstop_user, 1.0e-9_dp, "The model must reach its end time")

      allocate (waterlevel(ndx))
      waterlevel = s1(1:ndx)
      success = .true.
   end subroutine run_model

   subroutine create_pillar_inputs()
      call create_pillar_network()
      call create_file("pillar_sn.pliz", [character(len=80) :: &
                       "L1", "3 4", "20.0 25.0 -999 -999", "25.0 25.0 5.0 1.0", "30.0 25.0 -999 -999"])
      call create_file("pillar_we.pliz", [character(len=80) :: &
                       "L1", "3 4", "25.0 20.0 -999 -999", "25.0 25.0 5.0 1.0", "25.0 30.0 -999 -999"])
      call create_pillar_mdu_file("pillar_sn.mdu", "pillar_sn.pliz")
      call create_pillar_mdu_file("pillar_we.mdu", "pillar_we.pliz")
      call create_file("pillar.ext", [character(len=100) :: &
                       "[General]", "fileVersion = 2.01", "fileType = extForce", &
                       "[Boundary]", "quantity = dischargebnd", "locationFile = inflow.pli", "forcingFile = pillar.bc", &
                       "[Boundary]", "quantity = waterlevelbnd", "locationFile = outflow.pli", "forcingFile = pillar.bc"])
      call create_file("inflow.pli", ["inflow", "2 2", "0 0 inflow_0001", "0 50 inflow_0002"])
      call create_file("outflow.pli", ["outflow", "2 2", "50 0 outflow_0001", "50 50 outflow_0002"])
      call create_file("pillar.bc", [character(len=100) :: &
                       "[General]", "fileVersion = 1.01", "fileType = boundConds", &
                       "[Forcing]", "name = inflow_0001", "function = constant", "quantity = dischargebnd", "unit = m3/s", "50", &
                       "[Forcing]", "name = inflow_0002", "function = constant", "quantity = dischargebnd", "unit = m3/s", "50", &
                       "[Forcing]", "name = outflow_0001", "function = constant", "quantity = waterlevelbnd", "unit = m", "0", &
                       "[Forcing]", "name = outflow_0002", "function = constant", "quantity = waterlevelbnd", "unit = m", "0"])
   end subroutine create_pillar_inputs

   subroutine create_pillar_mdu_file(mdu_file, pillar_file)
      character(len=*), intent(in) :: mdu_file, pillar_file

      call create_file(mdu_file, [character(len=100) :: &
                       "[model]", "Program = D-Flow FM", "FileVersion = 1.09", &
                       "[geometry]", "NetFile = pillar_net.nc", "PillarFile = "//trim(pillar_file), &
                       "Bedlevuni = -5", "BedlevType = 1", "WaterLevIni = 0", "Conveyance2D = 0", "Nonlin2D = 0", &
                       "[numerics]", "CFLMax = 0.7", "PillarFarFieldVelocity = 0", &
                       "[physics]", "uniformFrictionCoefficient2D = 100", "uniformFrictionType = 0", &
                       "[time]", "RefDate = 20000101", "Tunit = S", "TStart = 0", "TStop = 3600", &
                       "DtUser = 1", "DtMax = 1", "DtInit = 1", &
                       "[external forcing]", "ExtForceFileNew = pillar.ext", &
                       "[output]", "MapInterval = 4000", "HisInterval = 0", "RstInterval = 0"])
   end subroutine create_pillar_mdu_file

   subroutine create_pillar_network()
      use netcdf

      integer, parameter :: nx = 5, ny = 5
      integer, parameter :: node_count = (nx + 1) * (ny + 1), edge_count = nx * (ny + 1) + (nx + 1) * ny, face_count = nx * ny
      integer :: ncid, node_dim, edge_dim, face_dim, max_face_nodes_dim, two_dim
      integer :: mesh_var, x_var, y_var, edge_nodes_var, face_nodes_var
      integer :: edge_nodes(2, edge_count), face_nodes(4, face_count)
      real(kind=dp) :: x(node_count), y(node_count)
      integer :: i, j, node, edge, face, bottom_left

      node = 0
      do j = 0, ny
         do i = 0, nx
            node = node + 1
            x(node) = 10.0_dp * real(i, dp)
            y(node) = 10.0_dp * real(j, dp)
         end do
      end do

      edge = 0
      do j = 0, ny
         do i = 0, nx - 1
            edge = edge + 1
            edge_nodes(:, edge) = [j * (nx + 1) + i + 1, j * (nx + 1) + i + 2]
         end do
      end do
      do j = 0, ny - 1
         do i = 0, nx
            edge = edge + 1
            edge_nodes(:, edge) = [j * (nx + 1) + i + 1, (j + 1) * (nx + 1) + i + 1]
         end do
      end do

      face = 0
      do j = 0, ny - 1
         do i = 0, nx - 1
            face = face + 1
            bottom_left = j * (nx + 1) + i + 1
            face_nodes(:, face) = [bottom_left, bottom_left + 1, bottom_left + nx + 2, bottom_left + nx + 1]
         end do
      end do

      call check_netcdf(nf90_create("pillar_net.nc", NF90_CLOBBER, ncid))
      call check_netcdf(nf90_put_att(ncid, NF90_GLOBAL, "Conventions", "CF-1.8 UGRID-1.0"))
      call check_netcdf(nf90_def_dim(ncid, "mesh2d_nNodes", node_count, node_dim))
      call check_netcdf(nf90_def_dim(ncid, "mesh2d_nEdges", edge_count, edge_dim))
      call check_netcdf(nf90_def_dim(ncid, "mesh2d_nFaces", face_count, face_dim))
      call check_netcdf(nf90_def_dim(ncid, "mesh2d_nMax_face_nodes", 4, max_face_nodes_dim))
      call check_netcdf(nf90_def_dim(ncid, "Two", 2, two_dim))
      call check_netcdf(nf90_def_var(ncid, "mesh2d", NF90_INT, mesh_var))
      call check_netcdf(nf90_put_att(ncid, mesh_var, "cf_role", "mesh_topology"))
      call check_netcdf(nf90_put_att(ncid, mesh_var, "topology_dimension", 2))
      call check_netcdf(nf90_put_att(ncid, mesh_var, "node_coordinates", "mesh2d_node_x mesh2d_node_y"))
      call check_netcdf(nf90_put_att(ncid, mesh_var, "edge_node_connectivity", "mesh2d_edge_nodes"))
      call check_netcdf(nf90_put_att(ncid, mesh_var, "face_node_connectivity", "mesh2d_face_nodes"))
      call check_netcdf(nf90_def_var(ncid, "mesh2d_node_x", NF90_DOUBLE, [node_dim], x_var))
      call check_netcdf(nf90_def_var(ncid, "mesh2d_node_y", NF90_DOUBLE, [node_dim], y_var))
      call check_netcdf(nf90_def_var(ncid, "mesh2d_edge_nodes", NF90_INT, [two_dim, edge_dim], edge_nodes_var))
      call check_netcdf(nf90_put_att(ncid, edge_nodes_var, "cf_role", "edge_node_connectivity"))
      call check_netcdf(nf90_put_att(ncid, edge_nodes_var, "start_index", 1))
      call check_netcdf(nf90_def_var(ncid, "mesh2d_face_nodes", NF90_INT, [max_face_nodes_dim, face_dim], face_nodes_var))
      call check_netcdf(nf90_put_att(ncid, face_nodes_var, "cf_role", "face_node_connectivity"))
      call check_netcdf(nf90_put_att(ncid, face_nodes_var, "start_index", 1))
      call check_netcdf(nf90_enddef(ncid))
      call check_netcdf(nf90_put_var(ncid, x_var, x))
      call check_netcdf(nf90_put_var(ncid, y_var, y))
      call check_netcdf(nf90_put_var(ncid, edge_nodes_var, edge_nodes))
      call check_netcdf(nf90_put_var(ncid, face_nodes_var, face_nodes))
      call check_netcdf(nf90_close(ncid))
   end subroutine create_pillar_network

   subroutine check_netcdf(status)
      use netcdf, only: nf90_noerr, nf90_strerror

      integer, intent(in) :: status

      if (status /= nf90_noerr) then
         error stop nf90_strerror(status)
      end if
   end subroutine check_netcdf

end module test_pillar_orientation
