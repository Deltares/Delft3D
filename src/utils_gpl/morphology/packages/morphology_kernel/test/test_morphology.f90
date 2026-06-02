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
   use sediment_basics_module, only: NPARDEF

   implicit none

   type :: transport_defaults
      real(kind=fp) :: ag = 9.81_fp ! gravity acceleration [m/s^2]
      real(kind=fp) :: delta = 1.65_fp ! relative density of sediment [-]
   end type transport_defaults

   type, extends(transport_defaults) :: coastal_transport_defaults
      logical :: ubot_from_com = .false.
      real(kind=fp) :: chezy = 65.0_fp ! Chezy coefficient [m^1/2/s]
      real(kind=fp) :: d15 = 0.0001_fp ! D15 of the sediment [m]
      real(kind=fp) :: di50 = 0.0002_fp ! D50 of the sediment [m]
      real(kind=fp) :: d90 = 0.0003_fp ! D90 of the sediment [m]
      real(kind=fp) :: dzbdt = 0.001_fp ! erosion/sedimentation velocity [m/s]
      real(kind=fp) :: dzdx = 0.01_fp ! slope in x direction [-]
      real(kind=fp) :: dzdy = 0.0_fp ! slope in y direction [-]
      real(kind=fp) :: h = 1.0_fp ! water depth [m]
      real(kind=fp) :: hrms = 1.1_fp ! Root mean square wave height  [m]
      real(kind=fp) :: kwtur = 0.1_fp ! Breaker induced turbulence [m^2/s^2]
      real(kind=fp) :: poros = 0.4_fp ! Porosity of the sediment [-]
      real(kind=fp) :: rlabda = 60.0_fp ! Wave length [m]
      real(kind=fp) :: teta = 0.1_fp ! angle between wave direction and x-axis [degrees]
      real(kind=fp) :: tp = 5_fp ! Wave period   [s]
      real(kind=fp) :: ubot = 0.1_fp ! velocity at the bed [m/s]
      real(kind=fp) :: u = 0.1_fp ! velocity in x direction [m/s]
      real(kind=fp) :: v = 0.0_fp ! velocity in y direction [m/s]
      real(kind=fp) :: vicmol = 1e-6_fp ! kinematic viscosity of water [m^2/s]
      integer :: npar = 0 ! number of parameters

      real(kind=fp), dimension(:), allocatable :: par
      
      ! output variables
      real(kind=fp) :: sbcu = -999.0_fp
      real(kind=fp) :: sbcv = -999.0_fp
      real(kind=fp) :: cesus = -999.0_fp
      real(kind=fp) :: va = -999.0_fp
      real(kind=fp) :: ua = -999.0_fp
      
   end type coastal_transport_defaults

   contains

   function set_coastal_transport_defaults(iform) result(t)
      integer, intent(in) :: iform
      type(coastal_transport_defaults) :: t

      integer :: j

      character(100) :: name
      integer :: nparreq
      integer :: nparopt
      real(kind=fp), dimension(NPARDEF) :: pardef
      character(25), dimension(NPARDEF) :: parkeyw
   
      t = coastal_transport_defaults()

      call traparams(iform, name, nparreq, nparopt, parkeyw, pardef)

      t%npar = NPARDEF + 10
      allocate (t%par(t%npar))

      t%par(1) = t%ag
      t%par(4) = t%delta
      do j = 1, NPARDEF
         t%par(j + 10) = pardef(j)
      end do

   end function set_coastal_transport_defaults


   !$f90tw TESTCODE(TEST, test_morphology, test_rotation_Van_Thiel_Van_Rijn, test_trab19,
   subroutine test_trab19() bind(C)
      type(coastal_transport_defaults) :: t
      type(coastal_transport_defaults) :: t_r

      t = set_coastal_transport_defaults(19)
      t_r = set_coastal_transport_defaults(19) ! rotated

      t_r%u = -t%v
      t_r%v = t%u
      t_r%teta = t%teta + 90
      t_r%dzdx = -t%dzdy
      t_r%dzdy = t%dzdx

      call trab19(t%u, t%v, t%hrms, t%rlabda, t%teta, t%h, t%tp, &
                & t%di50, t%d15, t%d90, t%npar, t%par, t%dzbdt, t%vicmol, &
                & t%poros, t%chezy, t%dzdx, t%dzdy, t%sbcu, t%sbcv, t%cesus, &
                & t%ua, t%va, t%ubot, t%kwtur, t%ubot_from_com)

      call trab19(t_r%u, t_r%v, t_r%hrms, t_r%rlabda, t_r%teta, t_r%h, t_r%tp, &
                & t_r%di50, t_r%d15, t_r%d90, t_r%npar, t_r%par, t_r%dzbdt, t_r%vicmol, &
                & t_r%poros, t_r%chezy, t_r%dzdx, t_r%dzdy, t_r%sbcu, t_r%sbcv, t_r%cesus, &
                & t_r%ua, t_r%va, t_r%ubot, t_r%kwtur, t_r%ubot_from_com)

      call f90_assert_near(t_r%sbcu, -t%sbcv, 1.0e-08_fp, "sbcu is not near the expected value")
      call f90_assert_near(t_r%sbcv, t%sbcu, 1.0e-11_fp, "sbcv is not near the expected value")
      call f90_assert_near(t_r%cesus, t%cesus, 1.0e-07_fp, "cesus is not near the expected value")
      call f90_assert_near(t_r%ua, -t%va, 1.0e-04_fp, "ua is not near the expected value")
      call f90_assert_near(t_r%va, t%ua, 1.0e-08_fp, "va is not near the expected value")
     
   end subroutine test_trab19
   !$f90tw)

   
end module test_morphology
