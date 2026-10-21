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
   use precision, only: fp, hp, prec
   use m_missing, only: dmiss
   use m_trab19, only: trab19
   use m_rdtrafrm, only: traparams
   use morphology_data_module, only: NPARDEF
   use bedcomposition_module, only: ACTIVE_LAYER_TO_SUBSTRATE_FLUX_IMMOBILE_FIRST, &
                                   ACTIVE_LAYER_TO_SUBSTRATE_FLUX_UNIFORM, BED_LAYERED, BED_MIXED, &
                                   CONSOL_DECON, CONSOL_NONE, MOBILITY_DISCRETE, &
                                   allocmorlyr, bedcomp_data, clrmorlyr, initmorlyr, updmorlyr
   use message_module, only: clearstack, getmessage, initstack, isempty, message_len, message_stack

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

   !$f90tw TESTCODE(TEST, test_morphology, test_updmorlyr_mixed_bed_update, test_updmorlyr_mixed_bed_update,
   subroutine test_updmorlyr_mixed_bed_update() bind(C)
      type(bedcomp_data) :: bed
      type(message_stack) :: messages
      integer :: istat
      real(fp), dimension(2, 1) :: dbodsd
      real(fp), dimension(1) :: dz
      real(fp), dimension(1) :: dunelength
      real(fp), dimension(1, 2) :: sbot
      character(message_len) :: message

      call initstack(messages)
      call initialize_bedcomposition_test(bed, BED_MIXED, .false., CONSOL_NONE, istat)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "mixed bed setup failed")

      bed%state%bodsed(:, 1) = [1.0_prec, 2.0_prec]
      bed%state%dpsed(1) = 3.0_fp
      dbodsd(:, 1) = [0.5_fp, 0.25_fp]
      dz = 0.0_fp
      dunelength = 0.0_fp
      sbot = 0.0_fp

      istat = updmorlyr(bed, dbodsd, dz, dunelength, sbot, 0.0_fp, 0.0_hp, messages)

      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "mixed bed update failed")
      call f90_assert_near(dz(1), 0.75_fp, 1.0e-6_fp, "unexpected mixed bed level change")
      call f90_assert_near(real(bed%state%bodsed(1, 1), fp), 1.5_fp, 1.0e-6_fp, "unexpected first mixed fraction mass")
      call f90_assert_near(real(bed%state%bodsed(2, 1), fp), 2.25_fp, 1.0e-6_fp, "unexpected second mixed fraction mass")
      call f90_assert_true(isempty(messages), "unexpected message during mixed bed deposition")

      bed%settings%morlyrnum%mass_shortage_thresh = 0.1_fp
      bed%settings%morlyrnum%mass_shortage_max_num_warnings = 5
      dbodsd(:, 1) = [-2.5_fp, 0.0_fp]
      istat = updmorlyr(bed, dbodsd, dz, dunelength, sbot, 0.0_fp, 0.0_hp, messages)

      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "mixed bed erosion update failed")
      call f90_assert_near(dz(1), -1.5_fp, 1.0e-6_fp, "unexpected mixed bed erosion level change")
      call f90_assert_near(bed%state%sedshort(1, 1), -1.0_fp, 1.0e-6_fp, "unexpected mixed bed sediment shortage")
      call f90_assert_near(real(bed%settings%morlyrnum%mass_shortage_max_num_warnings, fp), 4.0_fp, 0.0_fp, &
                           "unexpected shortage warning count")
      call getmessage(messages, message)
      call f90_assert_true(index(message, "Sediment erosion shortage at NM") > 0, "missing sediment shortage message")
      call clearstack(messages)

      istat = clrmorlyr(bed)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "mixed bed cleanup failed")
   end subroutine test_updmorlyr_mixed_bed_update
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_morphology, test_updmorlyr_layered_bed_paths, test_updmorlyr_layered_bed_paths,
   subroutine test_updmorlyr_layered_bed_paths() bind(C)
      type(bedcomp_data) :: bed
      type(message_stack) :: messages
      integer :: istat
      real(fp), dimension(2, 1) :: dbodsd
      real(fp), dimension(1) :: dz
      real(fp), dimension(1) :: dunelength
      real(fp), dimension(1, 2) :: sbot

      call initstack(messages)
      call initialize_bedcomposition_test(bed, BED_LAYERED, .false., CONSOL_NONE, istat)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "layered bed setup failed")

      bed%settings%thtrlyr(1) = 2.5_fp
      bed%state%msed(:, 1, 1) = [1.0_fp, 1.0_fp]
      bed%state%msed(:, 2, 1) = [0.5_fp, 0.5_fp]
      bed%state%thlyr(:, 1) = [2.0_fp, 1.0_fp]
      bed%state%svfrac(:, 1) = 1.0_fp
      dbodsd(:, 1) = [0.5_fp, 0.0_fp]
      dz = 0.0_fp
      dunelength = 0.0_fp
      sbot = 0.0_fp

      istat = updmorlyr(bed, dbodsd, dz, dunelength, sbot, 0.0_fp, 0.0_hp, messages)

      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "layered bed update failed")
      call f90_assert_near(dz(1), 0.5_fp, 1.0e-6_fp, "unexpected layered bed level change")
      call f90_assert_near(bed%state%msed(1, 1, 1), 1.5_fp, 1.0e-6_fp, "unexpected layered top fraction mass")
      call f90_assert_near(bed%state%thlyr(1, 1), 2.5_fp, 1.0e-6_fp, "unexpected layered transport layer thickness")
      call f90_assert_true(bed%settings%initcl, "layered bed was not marked initialized")
      call f90_assert_true(isempty(messages), "unexpected message during layered bed deposition")

      istat = clrmorlyr(bed)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "layered bed cleanup failed")

      call initialize_bedcomposition_test(bed, BED_LAYERED, .true., CONSOL_NONE, istat)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "coarse-layer bed setup failed")

      bed%settings%thtrlyr(1) = 2.5_fp
      bed%settings%thclyr(1) = 1.5_fp
      bed%state%msed(:, 1, 1) = [1.0_fp, 1.0_fp]
      bed%state%msed(:, 2, 1) = [1.0_fp, 1.0_fp]
      bed%state%msed(:, 3, 1) = [0.5_fp, 0.5_fp]
      bed%state%thlyr(:, 1) = [2.0_fp, 2.0_fp, 1.0_fp]
      bed%state%svfrac(:, 1) = 1.0_fp
      bed%state%mobile(:, 1) = 0.5_fp
      bed%state%tdecon = 7.0_hp
      dbodsd = 0.0_fp
      dz = 0.0_fp
      dunelength = 1.0_fp
      sbot = 0.2_fp

      istat = updmorlyr(bed, dbodsd, dz, dunelength, sbot, 1.0_fp, 100.0_hp, messages)

      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "coarse-layer bed update failed")
      call f90_assert_near(dz(1), 0.0_fp, 1.0e-6_fp, "coarse-layer exchange changed total bed level")
      call f90_assert_near(bed%state%msed(1, 1, 1), 1.25_fp, 1.0e-6_fp, "unexpected first coarse-layer top mass")
      call f90_assert_near(bed%state%msed(2, 1, 1), 1.25_fp, 1.0e-6_fp, "unexpected second coarse-layer top mass")
      call f90_assert_near(bed%state%msed(1, 2, 1), 0.75_fp, 1.0e-6_fp, "unexpected first coarse-layer mass")
      call f90_assert_near(bed%state%msed(2, 2, 1), 0.75_fp, 1.0e-6_fp, "unexpected second coarse-layer mass")
      call f90_assert_near(real(bed%state%tdecon, fp), 7.0_fp, 1.0e-6_fp, "coarse-layer update changed DECON schedule")
      call f90_assert_true(bed%settings%initcl, "coarse-layer bed was not marked initialized")
      call f90_assert_true(isempty(messages), "unexpected message during coarse-layer exchange")

      istat = clrmorlyr(bed)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "coarse-layer bed cleanup failed")
   end subroutine test_updmorlyr_layered_bed_paths
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_morphology, test_updmorlyr_active_layer_to_substrate_flux, test_updmorlyr_active_layer_to_substrate_flux,
   subroutine test_updmorlyr_active_layer_to_substrate_flux() bind(C)
      type(bedcomp_data) :: bed
      type(message_stack) :: messages
      integer :: istat
      real(fp), dimension(2, 1) :: dbodsd
      real(fp), dimension(1) :: dz
      real(fp), dimension(1) :: dunelength
      real(fp), dimension(1, 2) :: sbot

      call initstack(messages)

      call initialize_bedcomposition_test(bed, BED_LAYERED, .false., CONSOL_NONE, istat)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "uniform-flux bed setup failed")
      call f90_assert_near(real(bed%settings%active_layer_to_substrate_flux, fp), &
                           real(ACTIVE_LAYER_TO_SUBSTRATE_FLUX_UNIFORM, fp), 0.0_fp, &
                           "active-layer substrate flux did not default to UNIFORM")

      bed%settings%thtrlyr(1) = 2.5_fp
      bed%state%msed(:, 1, 1) = [1.0_fp, 1.0_fp]
      bed%state%msed(:, 2, 1) = [0.5_fp, 0.5_fp]
      bed%state%thlyr(:, 1) = [2.0_fp, 1.0_fp]
      bed%state%svfrac(:, 1) = 1.0_fp
      dbodsd(:, 1) = [1.0_fp, 1.0_fp]
      dz = 0.0_fp
      dunelength = 0.0_fp
      sbot = 0.0_fp

      istat = updmorlyr(bed, dbodsd, dz, dunelength, sbot, 0.0_fp, 0.0_hp, messages)

      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "uniform-flux bed update failed")
      call f90_assert_near(bed%state%msed(1, 1, 1), 1.25_fp, 1.0e-6_fp, &
                           "uniform flux changed the first active-layer fraction")
      call f90_assert_near(bed%state%msed(2, 1, 1), 1.25_fp, 1.0e-6_fp, &
                           "uniform flux changed the second active-layer fraction")
      call f90_assert_near(sum(bed%state%msed(1, :, 1)), 2.5_fp, 1.0e-6_fp, &
                           "uniform flux did not conserve first-fraction mass")
      call f90_assert_near(sum(bed%state%msed(2, :, 1)), 2.5_fp, 1.0e-6_fp, &
                           "uniform flux did not conserve second-fraction mass")
      call f90_assert_true(isempty(messages), "unexpected message during uniform-flux transfer")
      istat = clrmorlyr(bed)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "uniform-flux bed cleanup failed")

      call initialize_bedcomposition_test(bed, BED_LAYERED, .false., CONSOL_NONE, istat, &
                                          ACTIVE_LAYER_TO_SUBSTRATE_FLUX_IMMOBILE_FIRST)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "immobile-first bed setup failed")
      bed%settings%imobility = MOBILITY_DISCRETE
      bed%settings%thtrlyr(1) = 2.5_fp
      bed%state%msed(:, 1, 1) = [1.0_fp, 1.0_fp]
      bed%state%msed(:, 2, 1) = [0.5_fp, 0.5_fp]
      bed%state%thlyr(:, 1) = [2.0_fp, 1.0_fp]
      bed%state%svfrac(:, 1) = 1.0_fp
      bed%state%mobile(:, 1) = [0.75_fp, 0.25_fp]
      dbodsd(:, 1) = [1.0_fp, 1.0_fp]

      istat = updmorlyr(bed, dbodsd, dz, dunelength, sbot, 0.0_fp, 0.0_hp, messages)

      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "immobile-first bed update failed")
      call f90_assert_near(bed%state%msed(1, 1, 1), 1.625_fp, 1.0e-6_fp, &
                           "immobile-first transfer did not prioritize immobile sediment")
      call f90_assert_near(bed%state%msed(2, 1, 1), 0.875_fp, 1.0e-6_fp, &
                           "immobile-first transfer did not preserve the requested thickness")
      call f90_assert_near(sum(bed%state%msed(1, :, 1)), 2.5_fp, 1.0e-6_fp, &
                           "immobile-first transfer did not conserve first-fraction mass")
      call f90_assert_near(sum(bed%state%msed(2, :, 1)), 2.5_fp, 1.0e-6_fp, &
                           "immobile-first transfer did not conserve second-fraction mass")
      call f90_assert_true(isempty(messages), "unexpected message during immobile-first transfer")
      istat = clrmorlyr(bed)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "immobile-first bed cleanup failed")

      call initialize_bedcomposition_test(bed, BED_LAYERED, .false., CONSOL_NONE, istat, &
                                          ACTIVE_LAYER_TO_SUBSTRATE_FLUX_IMMOBILE_FIRST)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "mobile-remainder bed setup failed")
      bed%settings%imobility = MOBILITY_DISCRETE
      bed%settings%thtrlyr(1) = 1.0_fp
      bed%state%msed(:, 1, 1) = [1.0_fp, 1.0_fp]
      bed%state%msed(:, 2, 1) = [0.5_fp, 0.5_fp]
      bed%state%thlyr(:, 1) = [2.0_fp, 1.0_fp]
      bed%state%svfrac(:, 1) = 1.0_fp
      bed%state%mobile(:, 1) = [0.5_fp, 0.5_fp]
      dbodsd(:, 1) = [1.0_fp, 1.0_fp]

      istat = updmorlyr(bed, dbodsd, dz, dunelength, sbot, 0.0_fp, 0.0_hp, messages)

      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "mobile-remainder bed update failed")
      call f90_assert_near(bed%state%msed(1, 1, 1), 0.5_fp, 1.0e-6_fp, &
                           "immobile-first transfer did not use mobile sediment for the remainder")
      call f90_assert_near(bed%state%msed(2, 1, 1), 0.5_fp, 1.0e-6_fp, &
                           "immobile-first transfer did not preserve the requested thickness")
      call f90_assert_near(sum(bed%state%msed(1, :, 1)), 2.5_fp, 1.0e-6_fp, &
                           "mobile-remainder transfer did not conserve first-fraction mass")
      call f90_assert_near(sum(bed%state%msed(2, :, 1)), 2.5_fp, 1.0e-6_fp, &
                           "mobile-remainder transfer did not conserve second-fraction mass")
      call f90_assert_true(isempty(messages), "unexpected message during mobile-remainder transfer")

      istat = clrmorlyr(bed)
      call f90_assert_near(real(istat, fp), 0.0_fp, 0.0_fp, "mobile-remainder bed cleanup failed")
   end subroutine test_updmorlyr_active_layer_to_substrate_flux
   !$f90tw)

   subroutine initialize_bedcomposition_test(bed, bed_mode, coarse_layer, consolidation_mode, istat, substrate_flux)
      type(bedcomp_data), intent(inout) :: bed
      integer, intent(in) :: bed_mode
      logical, intent(in) :: coarse_layer
      integer, intent(in) :: consolidation_mode
      integer, intent(out) :: istat
      integer, intent(in), optional :: substrate_flux

      istat = initmorlyr(bed)
      if (istat /= 0) then
         return
      endif

      bed%settings%nfrac = 2
      bed%settings%nmlb = 1
      bed%settings%nmub = 1
      bed%settings%iunderlyr = bed_mode
      bed%settings%crslyr = coarse_layer
      bed%settings%iconsolidate = consolidation_mode
      if (present(substrate_flux)) then
         bed%settings%active_layer_to_substrate_flux = substrate_flux
      endif

      istat = allocmorlyr(bed)
      if (istat == 0) then
         bed%settings%rhofrac = 1.0_fp
      endif
   end subroutine initialize_bedcomposition_test

end module test_morphology
