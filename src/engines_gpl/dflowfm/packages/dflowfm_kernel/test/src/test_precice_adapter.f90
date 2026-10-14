module m_test_precice_adapter
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_missing, only: dmiss
   use m_alloc, only: realloc
   use m_network_helpers, only: t_grid_helper

   implicit none
contains

   ! Helper function copied from m_test_getprof_1d
   ! TODO: move to helper module?
   subroutine disable_timers_logging_and_mpi()
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use MessageHandling, only: SetMessageHandling

      call timini() ! Initialize timers (otherwise `flow_geominit` crashes)
      timon = .false. ! Disable timers because we're running unit tests.
      jampi = 0 ! Disable MPI because we're running unit tests.
      call SetMessageHandling(write2screen=.false.) ! Disable logging.
   end subroutine

   ! Helper function copied from test_pol_to_cellmask
   ! TODO: move to helper module?
   subroutine cleanup_netcells()
      use network_data, only: netcell, xk, yk
      integer :: i
      if (allocated(netcell)) then
         do i = 1, size(netcell)
            if (allocated(netcell(i)%nod)) then
               deallocate (netcell(i)%nod)
            end if
         end do
         deallocate (netcell)
      end if
      if (allocated(xk)) then
         deallocate (xk)
      end if
      if (allocated(yk)) then
         deallocate (yk)
      end if

   end subroutine cleanup_netcells


   !$f90tw TESTCODE(TEST, test_precice_adapter, test_pack_3d_sigma_fields, test_pack_3d_sigma_fields,
   subroutine test_pack_3d_sigma_fields() bind(C)
      use m_flow, only: kmx, kbot, ktop, laydefnr, laytyp, laymx, LAYTP_SIGMA
      use precice_adapter_utils, only: pack_cell_center_mesh_data

      real(kind=dp), dimension(10) :: flow_values
      real(kind=dp), dimension(6) :: scalar_values
      real(kind=dp), dimension(18) :: vector_values
      real(kind=dp), dimension(6), parameter :: expected = [104.0_dp, 105.0_dp, 106.0_dp, 108.0_dp, 109.0_dp, 110.0_dp]
      integer :: i

      kmx = 3
      call realloc(kbot, 2)
      call realloc(ktop, 2)
      call realloc(laydefnr, 2)
      call realloc(laytyp, 1)
      call realloc(laymx, 1)
      kbot = [4, 8]
      ktop = [6, 10]
      laydefnr = 1
      laytyp(1) = LAYTP_SIGMA
      laymx(1) = kmx
      flow_values = [(100.0_dp + real(i, dp), i=1, 10)]

      call pack_cell_center_mesh_data(2, kmx, flow_values, scalar_values)
      call f90_expect_near(scalar_values, expected, 1e-12_dp, "Exclude 2D prefix and column ghosts")

      call pack_cell_center_mesh_data(2, kmx, flow_values, vector_values(1::3))
      call pack_cell_center_mesh_data(2, kmx, 2.0_dp * flow_values, vector_values(2::3))
      call pack_cell_center_mesh_data(2, kmx, 3.0_dp * flow_values, vector_values(3::3))
      call f90_expect_near(vector_values(1::3), expected, 1e-12_dp, "Packed x velocity")
      call f90_expect_near(vector_values(2::3), 2.0_dp * expected, 1e-12_dp, "Packed y velocity")
      call f90_expect_near(vector_values(3::3), 3.0_dp * expected, 1e-12_dp, "Packed z velocity")
   end subroutine test_pack_3d_sigma_fields
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_precice_adapter, test_pack_3d_inactive_layers, test_pack_3d_inactive_layers,
   subroutine test_pack_3d_inactive_layers() bind(C)
      use m_flow, only: kmx, kbot, ktop, laydefnr, laytyp, laymx, nlaybn, nrlayn, LAYTP_Z
      use precice_adapter_utils, only: pack_cell_center_mesh_data, set_cell_center_mesh_zcoords

      real(kind=dp), dimension(8) :: flow_values
      real(kind=dp), dimension(8) :: mesh_values
      real(kind=dp), dimension(24) :: coordinates
      real(kind=dp), dimension(8), parameter :: expected = [0.0_dp, 0.0_dp, 4.0_dp, 5.0_dp, &
                                                          0.0_dp, 7.0_dp, 8.0_dp, 0.0_dp]
      real(kind=dp), dimension(8), parameter :: expected_z = [-999.0_dp, -999.0_dp, 3.5_dp, 4.5_dp, &
                                                            -999.0_dp, 6.5_dp, 7.5_dp, -999.0_dp]
      integer :: i

      kmx = 4
      call realloc(kbot, 2)
      call realloc(ktop, 2)
      call realloc(laydefnr, 2)
      call realloc(laytyp, 1)
      call realloc(laymx, 1)
      call realloc(nlaybn, 2)
      call realloc(nrlayn, 2)
      kbot = [4, 7]
      ktop = [5, 8]
      laydefnr = 1
      laytyp(1) = LAYTP_Z
      laymx(1) = kmx
      nlaybn = [3, 2]
      nrlayn = [2, 3]
      flow_values = [(real(i, dp), i=1, 8)]
      mesh_values = -1.0_dp
      coordinates = 0.0_dp

      call pack_cell_center_mesh_data(2, kmx, flow_values, mesh_values)
      call set_cell_center_mesh_zcoords(2, kmx, flow_values, coordinates)
      call f90_expect_near(mesh_values, expected, 1e-12_dp, "Only active z-layers receive data")
      call f90_expect_near(coordinates(3::3), expected_z, 1e-12_dp, "Coordinates and data use the same layer slots")
   end subroutine test_pack_3d_inactive_layers
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_precice_adapter, test_adapter_add_to_fm_administration, test_adapter_add_to_fm_administration,
   subroutine test_adapter_add_to_fm_administration() bind(C)
      use m_flow_geominit, only: flow_geominit
      use m_cellmask_from_polygon_set, only: t_netcell_set
      use precice_adapter
      use m_source_sink, only: SourceSinks, source_sinks, source_sink_all_discharges, FLOWCELL_SINK, FLOWCELL_SOURCE, SINK_SIDE, SOURCE_SIDE
      use m_alloc, only: realloc
      use m_resetfullflowmodel, only: resetfullflowmodel

      type(t_grid_helper) :: grid_helper
      type(precice_adapter_t) :: adapter
      type(t_netcell_set) :: netcell_cache
      integer :: expected_sink_cell
      integer :: expected_source_cell

      ! Setup grid
      call disable_timers_logging_and_mpi()
      call source_sinks%reset()
      call source_sinks%initialize(1)
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid( &
         bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
         rows=1, columns=2, array_size_margin=2 &
         )
      call flow_geominit(0)

      netcell_cache = t_netcell_set()
      expected_sink_cell = netcell_cache%find_netcell(5.0_dp, 5.0_dp)
      expected_source_cell = netcell_cache%find_netcell(15.0_dp, 7.0_dp)

      ! Setup adapter
      call precice_adapter_allocate_read_arrays(adapter, 1)
      adapter%vertex_ids_sources_sinks(1) = 123
      adapter%sinks_x(1) = 5.0
      adapter%sinks_y(1) = 5.0
      adapter%sinks_z_min(1) = -1.2
      adapter%sinks_z_max(1) = 3.4
      adapter%sources_x(1) = 15.0
      adapter%sources_y(1) = 7.0
      adapter%sources_z_min(1) = 5.6
      adapter%sources_z_max(1) = -7.8
      adapter%sources_sinks_discharge(1) = 9.10

      ! Actual call to test
      call precice_adapter_add_to_fm_administration(adapter)
      
      ! Assertions
      call f90_assert_eq(source_sinks%num_total, 1, "Unexpected number of total source sinks"//c_null_char)
      call f90_assert_eq(source_sinks%num_nearfield, 1, "Unexpected number of nearfield source sinks"//c_null_char)
      call f90_assert_streq(trim(source_sinks%name(1)), "preC-SUMO_0123", "Unexpected name for source sink 1"//c_null_char)
      call f90_assert_eq(source_sinks%indices(1, FLOWCELL_SINK), expected_sink_cell, "Unexpected indices(1,1) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_bottom(1, SINK_SIDE), -1.2_dp, 1e-5_dp, "Unexpected z_bottom(1,1) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_top(1, SINK_SIDE), 3.4_dp, 1e-5_dp, "Unexpected z_top(1,1) in source sinks"//c_null_char)
      call f90_assert_eq(source_sinks%indices(1, FLOWCELL_SOURCE), expected_source_cell, "Unexpected indices(1,4) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_bottom(1, SOURCE_SIDE), 5.6_dp, 1e-5_dp, "Unexpected z_bottom(1,2) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_top(1, SOURCE_SIDE), -7.8_dp, 1e-5_dp, "Unexpected z_top(1,2) in source sinks"//c_null_char)
      call f90_assert_near(source_sink_all_discharges(1, 1), 9.10_dp, 1e-5_dp, "Unexpected source_sink_all_discharges(1, 1) in source sinks"//c_null_char)

      ! Cleanup
      call cleanup_netcells()
      call precice_adapter_deallocate_read_arrays(adapter)
      call resetfullflowmodel()
   end subroutine test_adapter_add_to_fm_administration
   !$f90tw )

end module m_test_precice_adapter
