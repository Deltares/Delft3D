module test_sedtrails_3d
   use precision, only: dp
   use assertions_gtest, only: f90_expect_near
   use m_sedtrails_stats, only: accumulate_sedtrails_profiles
   implicit none

contains

   !$gtest
   subroutine test_profile_samples() bind(C)
      integer, parameter :: KB(2) = [2, 6], KT(2) = [4, 7]
      real(kind=dp), parameter :: Z(7) = [-3.0_dp, -2.0_dp, -1.0_dp, 0.0_dp, -1.5_dp, -0.5_dp, 0.5_dp]
      real(kind=dp), parameter :: TOL = 1.0e-12_dp
      integer :: donors(2, 1), first_layer(2)
      real(kind=dp) :: weights(2, 1), velocity(3, 7), volume(7), sums(5, 3, 1), duration(3, 1)

      donors(:, 1) = [1, 2]
      weights(:, 1) = [0.25_dp, 0.75_dp]
      first_layer = [1, 2]
      volume = 1.0_dp
      velocity = 0.0_dp
      velocity(:, 3) = [2.0_dp, 4.0_dp, -2.0_dp]
      velocity(:, 6) = [6.0_dp, 8.0_dp, 2.0_dp]
      sums = 0.0_dp
      duration = 0.0_dp

      call accumulate_sedtrails_profiles(1.0_dp, donors, weights, KB, KT, first_layer, &
                                         velocity, Z, volume, -999.0_dp, sums, duration)
      call f90_expect_near(duration(:, 1), [0.0_dp, 1.0_dp, 1.0_dp], TOL, "missing bottom layer")
      call f90_expect_near(sums(:, 2, 1), [5.0_dp, 7.0_dp, 1.0_dp, -1.625_dp, -0.625_dp], &
                           TOL, "vector and geometry on the same mapped layer")

      first_layer(2) = 0
      call accumulate_sedtrails_profiles(2.0_dp, donors, weights, KB, KT, first_layer, &
                                         velocity, Z, volume, -999.0_dp, sums, duration)
      call f90_expect_near(duration(2, 1), 1.0_dp, TOL, "dry donor contributes no duration")

      first_layer(2) = 2
      velocity(1, 3) = 8.0_dp
      velocity(1, 6) = 8.0_dp
      call accumulate_sedtrails_profiles(3.0_dp, donors, weights, KB, KT, first_layer, &
                                         velocity, Z, volume, -999.0_dp, sums, duration)
      call f90_expect_near(duration(2, 1), 4.0_dp, TOL, "only wet timesteps contribute")
      call f90_expect_near(sums(1, 2, 1) / duration(2, 1), 7.25_dp, TOL, "duration weighted velocity")
   end subroutine test_profile_samples

end module test_sedtrails_3d
