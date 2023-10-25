!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module test_airdensity
   use ftnunit
   use precision, only: hp
   use dfm_error, only: DFM_NOERR, DFM_GENERICERROR
   use m_airdensity

   implicit none

   real(hp), parameter :: tolerance = 1.0e-10_hp

   contains
!
!
!==============================================================================
!   
subroutine tests_compute_airdensity
   call test( test_get_airdensity, 'Test computation of varying air density.')
   call test( test_get_airdensity_exact, 'Test computation of varying air density in full precision.')
   call test( test_check_arraysizes, 'Test error message for arrays of unequal length')
end subroutine tests_compute_airdensity
!
!==============================================================================
!> tests computation of varying air density
subroutine test_get_airdensity

   real(kind=hp) :: p(3)               !< total atmospheric pressure [Pa]
   real(kind=hp) :: T2m(3)             !< temperature [K]
   real(kind=hp) :: rhoair(3)          !< air density [kg m-1]
   real(kind=hp) :: rhoair_expected(3) !< air density [kg m-1]
   integer :: i                        !< loop counter
   integer :: ierr                     !< error flag
   
   ! The values below are taken from NetCDF files downloaded from ECMWF
   ! as input for a testcase to be added for issue UNST-6593.
   ! These are measurements for the area around Bonaire.
   rhoair_expected = (/ 1.1594_hp, 1.1597_hp, 1.1604_hp /)
   p = (/101243.1719_hp, 101249.3_hp, 101251.2_hp /)
   T2m = (/27.7369_hp, 27.6104_hp, 27.5713_hp /) ![Celsius]

   call get_airdensity(p, T2m, rhoair, ierr)
   call assert_equal(ierr, DFM_NOERR, 'Something wrong in call get_airdensity().')
   do i = 1,size(rhoair) 
      ! computation - measurement < 0.01 is in accordance with previous investigation in Matlab
      call assert_comparable(rhoair(i), rhoair_expected(i), 1.e-2_hp, 'compute air density, point value')
   enddo

   end subroutine test_get_airdensity
!
!==============================================================================
!> tests computation of varying air density in full precision
subroutine test_get_airdensity_exact

   real(kind=hp) :: p(2)               !< total atmospheric pressure [Pa]
   real(kind=hp) :: T2m(2)             !< temperature [K]
   real(kind=hp) :: rhoair(2)          !< air density [kg m-1]
   real(kind=hp) :: rhoair_expected(2) !< air density [kg m-1]
   integer :: i                        !< loop counter
   integer :: ierr                     !< error flag
   
   ! synthetic data 
   rhoair_expected = (/  1.21718616275321_hp, 1.21246286454429_hp /)
   p = (/101325.0_hp, 101325.0_hp /)
   T2m = (/15.0_hp, 16.0_hp /) ![Celsius]

   call get_airdensity(p, T2m, rhoair, ierr)
   call assert_equal(ierr, DFM_NOERR, 'Something wrong in call get_airdensity().')
   do i = 1,size(rhoair) 
      write(*,*) rhoair(i), rhoair_expected(i)
      call assert_comparable(rhoair(i), rhoair_expected(i), tolerance, 'compute air density, point value')
   enddo

   end subroutine test_get_airdensity_exact
!
!==============================================================================
!> tests error handling for input arrays of unequal length
subroutine test_check_arraysizes

   real(kind=hp) :: p(2)               !< total atmospheric pressure [Pa]
   real(kind=hp) :: T2m(3)             !< temperature [K]
   real(kind=hp) :: rhoair(3)          !< air density [kg m-1]
   integer :: ierr                     !< error flag

   p = 0._hp   
   T2m= 274.15_hp
             
   call get_airdensity(p, T2m, rhoair, ierr)
   call assert_equal(ierr, DFM_GENERICERROR, 'Arrays of unequal size were not detected.')

   end subroutine test_check_arraysizes

   end module test_airdensity
