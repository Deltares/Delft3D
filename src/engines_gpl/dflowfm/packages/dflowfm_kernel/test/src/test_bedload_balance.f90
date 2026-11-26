!> Exercise bedload balance accounting through the complete production timestep sequence.
module test_bedload_balance
   use assertions_gtest
   use precision, only: dp
   use m_bedload_balance_fixture, only: initialize_bedload_model

   implicit none(type, external)
   private
   public :: test_bedload_order

contains

   !$gtest
   !> Corrected boundary bedload and sediment layer mass must balance at every output time.
   subroutine test_bedload_order() bind(C)
      use unstruc_api, only: flowstep, flowfinalize
      use m_flowtimes, only: time_user, tstop_user
      use m_mass_balance_area_data, only: const_flux, mbabedmassend, mbasedfluxtot
      use mass_balance_areas_routines, only: mba_final
      use dfm_error, only: DFM_NOERR

      real(dp) :: initial_mass(2)
      integer :: status, stop_requested, step

      status = initialize_bedload_model(3, '4.8', separate_areas=.false.)
      call f90_expect_eq(status, DFM_NOERR, 'The three-cell bedload model must initialize')
      if (status /= DFM_NOERR) then
         return
      end if
      initial_mass = mbabedmassend(:, 1)

      ! Include fixed-composition spin-up, activation at 1.2 s and changing boundary fluxes.
      do step = 1, 32
         if (time_user >= tstop_user) then
            exit
         end if
         call flowstep(stop_requested, status)
         call f90_expect_eq(status, DFM_NOERR, 'The model timestep must succeed')
         call f90_expect_eq(stop_requested, 0, 'The model must reach the requested end time')
         if (status /= DFM_NOERR .or. stop_requested /= 0) then
            exit
         end if
         call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'Fine sediment must balance per output interval')
         call f90_expect_near(const_flux(2)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'Coarse sediment must balance per output interval')
         if (time_user < 1.2_dp - 1.0e-9_dp) then
            call f90_expect_near(sum(abs(mbasedfluxtot)), 0.0_dp, 1.0e-12_dp, 'Spin-up potential flux must not transfer sediment mass')
         end if
      end do
      call f90_expect_near(time_user, tstop_user, 1.0e-9_dp)
      call f90_expect_true(sum(abs(mbasedfluxtot)) > 1.0e-4_dp, 'The regression must exercise nonzero boundary bedload')
      call f90_expect_true(maxval(abs(mbabedmassend(:, 1) - initial_mass)) > 1.0e-4_dp, 'The regression must change bed composition')
      call mba_final(time_user)
      call flowfinalize()
      call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'The fine sediment whole-period budget must close')
      call f90_expect_near(const_flux(2)%bal_error, [0.0_dp, 0.0_dp], 1.0e-9_dp, 'The coarse sediment whole-period budget must close')
   end subroutine test_bedload_order

end module test_bedload_balance
