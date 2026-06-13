module test_wave_friction_activation
   use assertions_gtest
   use precision, only: dp
   use m_tauwave, only: wave_friction_activation
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_wave_friction_activation, test_disabled_gate_returns_one, test_disabled_gate_returns_one,
   subroutine test_disabled_gate_returns_one() bind(C)
      call f90_expect_near(wave_friction_activation(0.0_dp, 0.0_dp, 0.0_dp), 1.0_dp, 1.0e-12_dp, &
                           "A disabled gate should preserve legacy wave friction.")
      call f90_expect_near(wave_friction_activation(0.0_dp, 0.1_dp, 0.05_dp), 1.0_dp, 1.0e-12_dp, &
                           "A reversed gate should be disabled.")
   end subroutine test_disabled_gate_returns_one
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_wave_friction_activation, test_smooth_transition, test_smooth_transition,
   subroutine test_smooth_transition() bind(C)
      call f90_expect_near(wave_friction_activation(0.05_dp, 0.1_dp, 0.3_dp), 0.0_dp, 1.0e-12_dp, &
                           "Values below the lower threshold should suppress wave friction.")
      call f90_expect_near(wave_friction_activation(0.2_dp, 0.1_dp, 0.3_dp), 0.5_dp, 1.0e-12_dp, &
                           "The midpoint of the smoothstep should be 0.5.")
      call f90_expect_near(wave_friction_activation(0.4_dp, 0.1_dp, 0.3_dp), 1.0_dp, 1.0e-12_dp, &
                           "Values above the upper threshold should preserve wave friction.")
   end subroutine test_smooth_transition
   !$f90tw)

end module test_wave_friction_activation
