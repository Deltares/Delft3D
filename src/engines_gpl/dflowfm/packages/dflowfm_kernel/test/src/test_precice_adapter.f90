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


   !$f90tw TESTCODE(TEST, test_precice_adapter, test_adapter_add_to_fm_administration, test_adapter_add_to_fm_administration,
   subroutine test_adapter_add_to_fm_administration() bind(C)
      use m_flow_geominit, only: flow_geominit
      use m_cellmask_from_polygon_set, only: cleanup_cell_geom_polylines
      use precice_adapter
      use m_source_sink, only: source_sinks, source_sink_all_discharges
      use m_alloc, only: realloc

      type(t_grid_helper) :: grid_helper
      type(precice_adapter_t) :: adapter

      ! Setup grid
      call disable_timers_logging_and_mpi()
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid( &
         bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
         rows=1, columns=2, array_size_margin=2 &
         )
      call flow_geominit(0)

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
      call f90_assert_eq(source_sinks%indices(1, 1), 1, "Unexpected indices(1,1) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_bottom(1, 1), -1.2_dp, 1e-5_dp, "Unexpected z_bottom(1,1) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_top(1, 1), 3.4_dp, 1e-5_dp, "Unexpected z_top(1,1) in source sinks"//c_null_char)
      call f90_assert_eq(source_sinks%indices(1, 4), 2, "Unexpected indices(1,4) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_bottom(1, 2), 5.6_dp, 1e-5_dp, "Unexpected z_bottom(1,2) in source sinks"//c_null_char)
      call f90_assert_near(source_sinks%z_top(1, 2), -7.8_dp, 1e-5_dp, "Unexpected z_top(1,2) in source sinks"//c_null_char)
      call f90_assert_near(source_sink_all_discharges(1, 1), 9.10_dp, 1e-5_dp, "Unexpected source_sink_all_discharges(1, 1) in source sinks"//c_null_char)

      ! Cleanup
      call cleanup_cell_geom_polylines()
      call cleanup_netcells()
      call precice_adapter_deallocate_read_arrays(adapter)
   end subroutine test_adapter_add_to_fm_administration
   !$f90tw )

end module m_test_precice_adapter
