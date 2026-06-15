module test_wave_friction_activation
   use assertions_gtest
   use precision, only: dp
   use m_tauwave, only: wave_friction_activation, wave_current_friction_coefficient
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

   !$f90tw TESTCODE(TEST, test_wave_friction_activation, test_friction_blend_floor, test_friction_blend_floor,
   subroutine test_friction_blend_floor() bind(C)
      call f90_expect_near(wave_current_friction_coefficient(0.02_dp, 0.08_dp, 0.0_dp), 0.02_dp, 1.0e-12_dp, &
                           "Inactive waves should keep current-only friction.")
      call f90_expect_near(wave_current_friction_coefficient(0.02_dp, 0.08_dp, 1.0_dp), 0.08_dp, 1.0e-12_dp, &
                           "Fully active waves should use wave-current friction.")
      call f90_expect_near(wave_current_friction_coefficient(0.02_dp, 0.08_dp, 0.25_dp), 0.035_dp, 1.0e-12_dp, &
                           "Partial activation should blend from current-only to wave-current friction.")
      call f90_expect_near(wave_current_friction_coefficient(0.02_dp, 0.0_dp, 1.0_dp), 0.02_dp, 1.0e-12_dp, &
                           "Invalid wave-current friction should fall back to current-only friction.")
      call f90_expect_near(wave_current_friction_coefficient(0.0_dp, 0.08_dp, 0.0_dp), 0.0_dp, 1.0e-12_dp, &
                           "Inactive waves should not inject wave-current friction when current-only friction is zero.")
   end subroutine test_friction_blend_floor
   !$f90tw)

end module test_wave_friction_activation
