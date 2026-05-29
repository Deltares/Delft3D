!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!

module test_morphology
   use assertions_gtest
   use precision, only: fp
   use m_missing, only: dmiss
   use m_trab19, only: trab19
   use m_rdtrafrm, only: traparams
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_morphology, test_Van_Thiel_Van_Rijn, test_trab19,
   subroutine test_trab19() bind(C)
      integer, parameter :: npar = 25
      integer, parameter :: npardef = 15
      logical :: ubot_from_com = .false.
      real(fp) :: chezy = 65.0_fp ! Chezy coefficient [m^1/2/s]
      real(fp) :: d15 = 0.0001_fp ! D15 of the sediment [m]
      real(fp) :: di50 = 0.0002_fp ! D50 of the sediment [m]
      real(fp) :: d90 = 0.0003_fp ! D90 of the sediment [m]
      real(fp) :: dzbdt = 0.001_fp ! erosion/sedimentation velocity [m/s]
      real(fp) :: dzdx = 0.01_fp ! slope in x direction [-]
      real(fp) :: dzdy = 0.0_fp ! slope in y direction [-]
      real(fp) :: h = 1.0_fp ! water depth [m]
      real(fp) :: hrms = 1.1_fp ! Root mean square wave height  [m]
      real(fp) :: kwtur = 0.1_fp ! Breaker induced turbulence [m^2/s^2]
      real(fp) :: poros = 0.4_fp ! Porosity of the sediment [-]
      real(fp) :: rlabda = 60.0_fp ! Wave length [m]
      real(fp) :: teta = 0.1_fp ! angle between wave direction and x-axis [degrees]
      real(fp) :: tp = 5_fp ! Wave period   [s]
      real(fp) :: ubot = 0.1_fp ! velocity at the bed [m/s]
      real(fp) :: u = 0.1_fp ! velocity in x direction [m/s]
      real(fp) :: v = 0.0_fp ! velocity in y direction [m/s]
      real(fp) :: vicmol = 1e-6_fp ! kinematic viscosity of water [m^2/s]
      ! output variables
      real(fp) :: sbcu
      real(fp) :: sbcv
      real(fp) :: cesus
      real(fp) :: ua
      real(fp) :: va
      integer :: j

      integer :: iform = 19
      character(100) :: name
      integer :: nparreq
      integer :: nparopt
      real(fp), dimension(:), allocatable :: pardef
      character(25), dimension(:), allocatable :: parkeyw
      character(25), dimension(:, :), pointer :: parname

      real(fp), dimension(npar) :: par

      real(fp), parameter :: ag = 9.81_fp ! gravity acceleration [m/s^2]
      real(fp), parameter :: delta = 1.65_fp ! relative density of sediment [-]

      allocate (pardef(npardef))
      allocate (parkeyw(npardef))

      call traparams(iform, name, nparreq, nparopt, parkeyw, pardef)

      par(1) = ag
      par(4) = delta
      do j = 1, npardef
         par(j + 10) = pardef(j)
      end do

      call trab19(u, v, hrms, rlabda, teta, h, tp, &
                & di50, d15, d90, npar, par, dzbdt, vicmol, &
                & poros, chezy, dzdx, dzdy, sbcu, sbcv, cesus, &
                & ua, va, ubot, kwtur, ubot_from_com)

      call f90_assert_near(sbcu, 1.3433e-05_fp, 1.0e-08_fp, "sbcu is not near the expected value"//c_null_char)
      call f90_assert_near(sbcv, 1.8343e-08_fp, 1.0e-11_fp, "sbcv is not near the expected value"//c_null_char)
      call f90_assert_near(cesus, 5.2902e-04_fp, 1.0e-07_fp, "cesus is not near the expected value"//c_null_char)
      call f90_assert_near(ua, 0.359505_fp, 1.0e-04_fp, "ua is not near the expected value"//c_null_char)
      call f90_assert_near(va, 6.2746e-04_fp, 1.0e-08_fp, "va is not near the expected value"//c_null_char)
   end subroutine test_trab19
   !$f90tw)

end module test_morphology
