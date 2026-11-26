!> Exercise sediment balance reporting for bedload crossing interior area boundaries.
module test_bedload_areas
   use assertions_gtest, only: f90_expect_eq, f90_expect_near, f90_expect_true
   use precision, only: dp

   implicit none(type, external)
   private
   public :: test_bedload_exchange

   integer, parameter :: num_cells = 5
   real(dp), parameter :: mass_tolerance = 1.0e-9_dp

contains

   !$gtest
   !> Check both sediment fractions in five areas and verify exchange at the two interior interfaces.
   subroutine test_bedload_exchange() bind(C)
      use m_bedload_balance_fixture, only: initialize_bedload_model
      use unstruc_api, only: flowstep, flowfinalize
      use m_flowtimes, only: time_user, tstop_user
      use m_mass_balance_area_data, only: nomba, mbaarea, mbaname, mbasedfluxtot
      use mass_balance_areas_routines, only: mba_final
      use dfm_error, only: DFM_NOERR

      real(dp), parameter :: unit_cell_areas(num_cells) = 1.0_dp
      real(dp), parameter :: minimum_exchange = 1.0e-4_dp
      integer :: status, stop_requested, step, left_area, right_area, fraction
      character(len=:), allocatable :: interface_name

      status = initialize_bedload_model(num_cells, '7.2', separate_areas=.true.)
      call f90_expect_eq(status, DFM_NOERR, 'The five-cell bedload model must initialize')
      if (status /= DFM_NOERR) then
         return
      end if
      call f90_expect_eq(nomba, num_cells, 'The model must have five mass balance areas')
      if (nomba /= num_cells) then
         call flowfinalize()
         return
      end if
      call f90_expect_near(mbaarea, unit_cell_areas, 1.0e-12_dp, 'Each area must contain exactly one cell')

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
         call expect_closed_balances('at every output time')
      end do
      call f90_expect_near(time_user, tstop_user, 1.0e-9_dp)

      ! Cells 2, 3, and 4 each have neighbors on both sides and no open boundary.
      do left_area = 2, num_cells - 2
         right_area = left_area + 1
         interface_name = trim(mbaname(left_area))//' and '//trim(mbaname(right_area))
         call f90_expect_true(sum(mbasedfluxtot(:, 1, left_area, right_area)) > minimum_exchange, &
                              'Fine bedload must cross between '//interface_name)
         do fraction = 1, size(mbasedfluxtot, 2)
            call f90_expect_near(mbasedfluxtot(:, fraction, left_area, right_area), &
                                 mbasedfluxtot(2:1:-1, fraction, right_area, left_area), mass_tolerance, &
                                 'Export and import must agree between '//interface_name)
         end do
      end do

      call mba_final(time_user)
      call expect_closed_balances('over the full simulation')
      call flowfinalize()
   end subroutine test_bedload_exchange

   subroutine expect_closed_balances(period)
      use m_mass_balance_area_data, only: const_flux

      character(len=*), intent(in) :: period
      real(dp), parameter :: zero_error(num_cells + 1) = 0.0_dp

      call f90_expect_near(const_flux(1)%bal_error, zero_error, mass_tolerance, &
                           'Fine sediment must balance in every area '//period)
      call f90_expect_near(const_flux(2)%bal_error, zero_error, mass_tolerance, &
                           'Coarse sediment must balance in every area '//period)
   end subroutine expect_closed_balances
end module test_bedload_areas
