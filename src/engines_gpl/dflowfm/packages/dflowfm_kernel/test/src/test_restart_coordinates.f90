module test_restart_coordinates
   use assertions_gtest
   use precision, only: dp
   use unstruc_netcdf, only: check_flownodesorlinks_numbering_rst

   implicit none(type, external)
   private

contains

   !$gtest
   subroutine cartesian_roundoff() bind(C)
      call check_offset(0, 0, 5.0e-6_dp, 0)
   end subroutine cartesian_roundoff

   !$gtest
   subroutine cartesian_mismatch() bind(C)
      call check_offset(0, 0, 2.0e-5_dp, 1)
   end subroutine cartesian_mismatch

   !$gtest
   subroutine spherical_roundoff() bind(C)
      call check_offset(1, 0, 5.0e-6_dp, 0)
   end subroutine spherical_roundoff

   !$gtest
   subroutine spherical_mismatch() bind(C)
      call check_offset(1, 0, 2.0e-5_dp, 1)
   end subroutine spherical_mismatch

   !$gtest
   subroutine spherical_3d_roundoff() bind(C)
      call check_offset(1, 1, 5.0e-6_dp, 0)
   end subroutine spherical_3d_roundoff

   !$gtest
   subroutine spherical_3d_mismatch() bind(C)
      call check_offset(1, 1, 2.0e-5_dp, 1)
   end subroutine spherical_3d_mismatch

   !$gtest
   subroutine cross_build_centroids() bind(C)
      real(kind=dp), dimension(4), parameter :: x_restart = [ &
         -61.744791666666885_dp, -61.680555423696354_dp, -62.911458333333563_dp, -62.656249793041539_dp]
      real(kind=dp), dimension(4), parameter :: y_restart = [ &
         17.307291684726874_dp, 17.399305795726757_dp, 17.869791684921154_dp, 18.156250186920559_dp]
      real(kind=dp), dimension(4), parameter :: x_current = [ &
         -61.744791666667290_dp, -61.680555423696830_dp, -62.911458333332725_dp, -62.656249793042036_dp]
      real(kind=dp), dimension(4), parameter :: y_current = [ &
         17.307291684726227_dp, 17.399305795727138_dp, 17.869791684920223_dp, 18.156250186921160_dp]

      call check_coordinates(1, 1, x_current, y_current, x_restart, y_restart, 0)
      call check_coordinates(1, 1, x_current, y_current, x_restart(4:1:-1), y_restart(4:1:-1), 1)
   end subroutine cross_build_centroids

   subroutine check_offset(spherical, spherical_3d, offset_metres, expected_error)
      use physicalconsts, only: earth_radius
      use m_mathconstants, only: raddeg_hp

      integer, intent(in) :: spherical, spherical_3d, expected_error
      real(kind=dp), intent(in) :: offset_metres
      real(kind=dp), dimension(2) :: x, y, y_restart

      x = [-61.7_dp, -61.8_dp]
      y = [17.3_dp, 17.4_dp]
      y_restart = y
      if (spherical == 1) then
         y_restart(2) = y(2) + offset_metres / earth_radius * raddeg_hp
      else
         y_restart(2) = y(2) + offset_metres
      end if
      call check_coordinates(spherical, spherical_3d, x, y, x, y_restart, expected_error)
   end subroutine check_offset

   subroutine check_coordinates(spherical, spherical_3d, x, y, x_restart, y_restart, expected_error)
      use network_data, only: xzw, yzw
      use m_flowgeom, only: xu, yu
      use m_sferic, only: jsferic, jasfer3D
      use m_partitioninfo, only: jampi
      use messagehandling, only: SetMessageHandling, GetMessage_MH, GetMessageCount, LEVEL_ERROR

      integer, intent(in) :: spherical, spherical_3d, expected_error
      real(kind=dp), dimension(:), intent(in) :: x, y, x_restart, y_restart
      real(kind=dp), dimension(:), allocatable :: saved_xzw, saved_yzw, saved_xu, saved_yu
      integer :: saved_spherical, saved_spherical_3d, saved_parallel, location, ierr, level
      character(len=512) :: message

      saved_spherical = jsferic
      saved_spherical_3d = jasfer3D
      saved_parallel = jampi
      call move_alloc(xzw, saved_xzw)
      call move_alloc(yzw, saved_yzw)
      call move_alloc(xu, saved_xu)
      call move_alloc(yu, saved_yu)
      xzw = x
      yzw = y
      xu = x
      yu = y
      jsferic = spherical
      jasfer3D = spherical_3d
      jampi = 0

      do location = 0, 1
         call SetMessageHandling(useLog=.true., reset_counters=.true.)
         call check_flownodesorlinks_numbering_rst(size(x), location, x_restart, y_restart, ierr)
         call f90_expect_eq(ierr, expected_error)
         if (expected_error == 1) then
            level = GetMessage_MH(GetMessageCount(), message)
            call f90_expect_eq(level, LEVEL_ERROR)
            if (location == 1) then
               call f90_expect_true(index(message, 'Restart coordinate mismatch at flow node ') > 0)
            else
               call f90_expect_true(index(message, 'Restart coordinate mismatch at flow link ') > 0)
            end if
            call f90_expect_true(index(message, new_line('a')//'NetFile (x, y):') > 0)
            call f90_expect_true(index(message, new_line('a')//'RestartFile (x, y):') > 0)
            call f90_expect_true(index(message, new_line('a')//'Distance: ') > 0)
            call f90_expect_true(index(message, 'tolerance: ') > 0)
         end if
      end do
      call SetMessageHandling(reset_counters=.true.)

      call move_alloc(saved_xzw, xzw)
      call move_alloc(saved_yzw, yzw)
      call move_alloc(saved_xu, xu)
      call move_alloc(saved_yu, yu)
      jsferic = saved_spherical
      jasfer3D = saved_spherical_3d
      jampi = saved_parallel
   end subroutine check_coordinates

end module test_restart_coordinates
