!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
module m_sethu
   use stdlib_kinds, only: dp

   private

   integer :: left_cell
   integer :: right_cell
   integer :: upstream_cell
   integer :: downstream_cell
   integer :: direction_sign
   integer :: upstream_cell_index
   integer :: link

   double precision :: velocity

   procedure(get_upstream_water_level_any), pointer :: get_upstream_water_level

   abstract interface
      double precision function get_upstream_water_level_any()
      end function
   end interface

   public :: calculate_hu_au_and_advection_for_dams_weirs

contains
!
!> Set upwind waterdepth hu and au
   subroutine calculate_hu_au_and_advection_for_dams_weirs(set_zws0, set_hu)
      use m_flowgeom
      use m_flow
      use m_fixedweirs
      use m_sobekdfm, only: nbnd1d2d, sethu_1d2d
      use unstruc_model, only: md_restartfile

      implicit none

      integer, intent(in) :: set_zws0
      logical, intent(in) :: set_hu !> Flag for updating `hu` (.true.) or not (.false.)

      integer, parameter :: DONT_SET_ZWS0 = 0
      integer, parameter :: EMPTY_NAME = 0
      integer, parameter :: USE_S0 = 0
      integer, parameter :: DO_NOT_SET_BlDepth = 0
      integer, parameter :: SEMI_SUBGRID = 21
      integer, parameter :: Rajaratnam = 23
      integer, parameter :: Tabellenboek = 24
      integer, parameter :: Villemonte = 25
      integer, parameter :: LINEAR_CONTINUITY = 1
      integer, parameter :: DRY_FLAG = 1
      integer, parameter :: CENTRAL_FROM_BED_TIL_SECOND_OR_FIRST_ABOVE_LOCAL_BOB = 1
      integer, parameter :: ALL_CENTRAL = 2
      integer, parameter :: CENTRAL_FROM_BED_TILL_HIGHEST_LAYER_WITH_EQUAL_LEVELS = 3
      double precision, parameter :: FOR_CUT_CELL = 0d0
      double precision, parameter :: TWO_THIRDS = 2d0 / 3d0

      integer :: link_in_3d
      integer :: kb, kb0, kt, Lb
      integer :: kbd, ktd, kbd0, LLbc, kkd
      integer :: L_lowest, nq, n, L
      double precision :: upstream_water_level
      double precision :: bed_level_at_u_point
      double precision :: water_height
      double precision :: water_height_no_weir
      double precision :: avolk
      double precision :: sigma
      double precision :: hub
      double precision :: zw0u
      double precision :: ucx_up, ucy_up, u_in, vhei, eup
      double precision :: lowest_bob
      logical :: dams_or_weirs, is_already_wet

      double precision, pointer :: velocity_pointer(:)

      if (set_zws0 == DONT_SET_ZWS0 .or. len_trim(md_restartfile) == EMPTY_NAME) then
         call sets01zbnd(USE_S0, DO_NOT_SET_BlDepth)
      end if

      if (uniformhu > 0d0) then
         hu(:) = uniformhu
         return
      end if

      call adjust_bobs_for_dams_and_structs()

      avolk = TWO_THIRDS * sqrt(TWO_THIRDS * ag)

      if (set_zws0 == DONT_SET_ZWS0) then
         velocity_pointer => u1
      else
         velocity_pointer => u0
      end if

      call set_upstream_water_level_getter(limtyphu)

      if (ncdamsg > 0 .or. ifixedweirscheme > 0) then
         dams_or_weirs = .true.
      else
         dams_or_weirs = .false.
      end if

      do link = 1, lnx

         if (wu(link) == FOR_CUT_CELL) then
            hu(link) = 0d0
            au(link) = 0d0
            cycle
         end if

         left_cell = ln(1, link)
         right_cell = ln(2, link)

         velocity = velocity_pointer(link)

         if (velocity == 0) then
            call get_upstream_downstream_cell_numbers(s0(left_cell) > s0(right_cell), left_cell, right_cell, &
                                                      upstream_cell_index, upstream_cell, downstream_cell, direction_sign)
         else
            call get_upstream_downstream_cell_numbers(velocity > 0, left_cell, right_cell, &
                                                      upstream_cell_index, upstream_cell, downstream_cell, direction_sign)
         end if

         upstream_water_level = get_upstream_water_level()

         call getblu_from_bob(link, upstream_cell_index, bed_level_at_u_point)
         if (jafullgridoutput > 0) then ! is it possible to move to other subroutine?
            blup(link) = bed_level_at_u_point
         end if

         water_height = upstream_water_level - bed_level_at_u_point

         if (water_height > epshu) then
            if (dams_or_weirs) then
               select case (iadv(link))
               case (SEMI_SUBGRID)
                  call calculate_advection_subgrid()
               case (Rajaratnam)
                  call calculate_advection_Rajaratnam()
               case (Tabellenboek)
                  call calculate_advection_Tabellenboek()
               case (Villemonte)
                  call calculate_advection_Villemonte()
               end select
            end if

            if (set_hu) then
               hu(link) = water_height
            end if

         else
            hu(link) = 0d0
            au(link) = 0d0
         end if

         if (kmx > 0) then ! only in sub3D
            call calculate_hu_au_3D()
         end if

      end do

      ! Make sure at least 1 flow link per discharge/velocity boundary is open
      do nq = 1, nqbnd
         is_already_wet = .false.
         if (l2qbnd(nq) >= l1qbnd(nq)) then
            lowest_bob = huge(1._dp)
            do n = L1qbnd(nq), L2qbnd(nq)
               L = kbndu(3, n)
               if (hu(L) > epshu) then
                  is_already_wet = .true.
                  exit
               end if
               if (max(bob(1, L), bob(2, L)) < lowest_bob) then
                  L_lowest = L
                  lowest_bob = max(bob(1, L), bob(2, L))
               end if
            end do
            if (.not. is_already_wet) then
               hu(L_lowest) = epshu
            end if
         end if
      end do

      do link = 1, lnx ! why is it here?- it is not hu
         huvli(link) = 1d0 / max(epshs, acl(link) * hs(ln(1, link)) + (1d0 - acl(link)) * hs(ln(2, link)))
      end do

      if (lincontin == LINEAR_CONTINUITY) then ! is this only for 2D? why not at the beginning?
         do link = 1, lnx
            hu(link) = -0.5d0 * (bob(1, link) + bob(2, link))
         end do
      end if

      if (nbnd1d2d > 0) then ! 1d2d boundary check for closed boundaries
         call sethu_1d2d()
      end if

      if (javeg > 0) then
         call setveg()
      end if

      ! Create an index array containing the wet flow links. Why is it here?
      call fill_onlyWetLinks()

   contains

!> calculate_advection_subgrid
      subroutine calculate_advection_subgrid()

         call getucxucynoweirs(upstream_cell, ucx_up, ucy_up)
         u_in = ucx_up * csu(link) + ucy_up * snu(link)
         call calculate_vhei_and_eup()
         call calculate_advection_block_subgrid_and_Rajaratnam()

      end subroutine calculate_advection_subgrid

!> calculate_vhei_and_eup
      subroutine calculate_vhei_and_eup()

         vhei = 0.5d0 * u_in * u_in / ag
         eup = water_height + vhei

      end subroutine calculate_vhei_and_eup

!> use_advection_block_subgrid_and_Rajaratnam
      subroutine calculate_advection_block_subgrid_and_Rajaratnam()

         double precision :: hu_crest
         double precision :: hup

         hu_crest = s0(downstream_cell) - bed_level_at_u_point
         if (hu_crest < water_height) then
            water_height = hu_crest
            if (water_height < TWO_THIRDS * eup) then ! supercritical
               water_height = TWO_THIRDS * eup
               hup = hu_crest - water_height
               if (hup < 0d0) then
                  adve(link) = adve(link) - direction_sign * hup * ag * dxi(link)
               end if
            end if
         end if

      end subroutine calculate_advection_block_subgrid_and_Rajaratnam

!> calculate_advection_Rajaratnam
      subroutine calculate_advection_Rajaratnam()
         double precision :: ufac
         double precision :: efac

         call calculate_u_in_and_upstream_ucx_ucy()
         call calculate_vhei_and_eup()
         call calculate_advection_block_subgrid_and_Rajaratnam()

         ufac = water_height_no_weir / water_height ! compensates for undisturbed field velocity
         efac = 1d0 - (1d0 / ufac**2)
         advi(link) = advi(link) + 0.5d0 * dxi(link) * abs(u1(link)) * ufac * ufac * efac
         water_height = water_height_no_weir

      end subroutine calculate_advection_Rajaratnam

!> calculates undisturbed velocity as if no weir present, WAQUA like
      subroutine calculate_u_in_and_upstream_ucx_ucy()

         water_height_no_weir = max(upstream_water_level - blu(link), water_height)
         ucx_up = ucx(upstream_cell)
         ucy_up = ucy(upstream_cell)
         u_in = abs(u1(link))

      end subroutine calculate_u_in_and_upstream_ucx_ucy

!> calculate_advection_Tabellenboek
      subroutine calculate_advection_Tabellenboek()

         call calculate_u_in_and_upstream_ucx_ucy()
         call calculate_vhei_and_eup()
         call calculate_advection_block_Tabellenboek_and_Villemonte()

      end subroutine calculate_advection_Tabellenboek

!> calculate_advection_block_Tabellenboek_and_Villemonte
      subroutine calculate_advection_block_Tabellenboek_and_Villemonte()
         integer :: nfw
         integer :: itel
         character(len=4) :: toest
         double precision :: wsbov
         double precision :: wsben
         double precision :: hkru_in
         double precision :: d1
         double precision :: energy_height_upstream
         double precision :: qvolk
         double precision :: qunit
         double precision :: vben
         double precision :: energy_height_downstream
         double precision :: hov
         double precision :: vov
         double precision :: hvolk
         double precision :: tol
         double precision :: qov
         double precision :: dte0
         double precision :: dtefri
         double precision :: vbov
         double precision :: agwdxi

         nfw = nfxwL(link)
         wsbov = upstream_water_level
         wsben = s0(downstream_cell)
         hkru_in = -bed_level_at_u_point

         ! determine sill height downstream of weir
         if (u_in >= 0d0) then
            d1 = shrxw(nfw)
         else
            d1 = shlxw(nfw)
         end if

         vhei = 0.5d0 * u_in * u_in / ag
         energy_height_upstream = max(0.000001d0, wsbov + hkru_in) + vhei
         qvolk = avolk * energy_height_upstream**1.5d0
         qunit = abs(u_in) * water_height_no_weir

         vben = qunit / max(0.000001d0, wsben - bl(downstream_cell))
         vhei = 0.5d0 * vben * vben / ag
         energy_height_downstream = max(0.000001d0, wsben + hkru_in) + vhei
         energy_height_downstream = min(energy_height_downstream, energy_height_upstream)

         hov = wsbov + hkru_in
         vov = qunit / hov
         if (vov < 0.5d0) then
            itel = 0
            hvolk = TWO_THIRDS * energy_height_upstream
            tol = 0.001d0 * max(0.0001d0, qunit)
            qov = 0d0
            do while (itel < 100 .and. (abs(qunit - qov)) > tol)
               itel = itel + 1
               vov = qunit / hov
               hov = max(hvolk, energy_height_upstream - (vov**2) / (2d0 * ag))
               qov = vov * hov
            end do
         end if
         dte0 = weirdte(nfw)
         dtefri = 0.0d0
         call enloss(ag, d1, energy_height_upstream, hkru_in, qunit, qvolk, toest, vov, &
                     energy_height_downstream, wsbov, wsben, weirdte(nfw), dtefri, iadv(link), crestlxw(nfw), &
                     taludlxw(nfw), taludrxw(nfw), vegxw(nfw), testfixedweirs)
         weirdte(nfw) = (1d0 - waquaweirthetaw) * weirdte(nfw) + waquaweirthetaw * dte0

         ! attention total waterdepth instead of water above crest
         vbov = abs(u_in)
         if (toest == 'volk') then
            vbov = qvolk / max(water_height_no_weir, 1d-6)
         end if
         if (vbov > 1d-8) then
            agwdxi = ag * weirdte(nfw) * dxi(link)
            if (kmx == 0) then
               advi(link) = advi(link) + agwdxi / vbov ! 1/s
            else
               do link_in_3d = Lbot(link), Ltop(link)
                  advi(link_in_3d) = advi(link_in_3d) + agwdxi / max(1d-4, abs(u1(link_in_3d)))
               end do
            end if
            map_fixed_weir_energy_loss(link) = weirdte(nfw)
         else
            map_fixed_weir_energy_loss(link) = 0
         end if

         water_height = water_height_no_weir

      end subroutine calculate_advection_block_Tabellenboek_and_Villemonte

!> calculate_advection_Villemonte
      subroutine calculate_advection_Villemonte()

         call calculate_u_in_and_upstream_ucx_ucy()
         call calculate_vhei_and_eup()
         call calculate_advection_block_Tabellenboek_and_Villemonte()

      end subroutine calculate_advection_Villemonte

      subroutine calculate_hu_au_3D()
         use m_qnerror

         integer, parameter :: TYPE_ALL_Z = 2
         integer, parameter :: AVERAGE_BED_CELLING = 2

         if (hu(link) <= 0d0) then
            Ltop(link) = DRY_FLAG
         else
            call get_lkbot_set_ltop_upwind(link, upstream_cell, upstream_cell_index, Lb, kb, kt)
            kb0 = kb - 1
            au(link) = 0d0
            hu(Lb - 1) = 0d0

            if (Lb == Ltop(link)) then
               call calculate_hu_au_for_one_layer()
            else
               if (Ltop(link) > Lb + kmxL(link) - 1) then
                  call qnerror('Ltop too large', ' ', ' ')
               end if
               ! UNST-5182: The code below has only been implemented for keepzlayeringatbed == 2
               ! To be implemented for keepzlayeringatbed == 0 and 1 as well, because layer distribution is independent of value of keepzlayeringatbed?
               !
               if (layertype == TYPE_ALL_Z .and. keepzlayeringatbed == AVERAGE_BED_CELLING) then
                  call calculate_using_split_central_and_sigma_parts()
               else
                  call calculate_hu_au_using_upwind_sigma()
               end if
            end if
         end if

      end subroutine calculate_hu_au_3D

!> calculate_hu_au_for_one_layer
      subroutine calculate_hu_au_for_one_layer()

         hu(Lb) = hu(link)
         au(Lb) = wu(link) * (hu(Lb) - hu(Lb - 1))
         au(link) = au(link) + au(Lb)

      end subroutine calculate_hu_au_for_one_layer

!> calculate hu and au using split in a central and sigma oriented parts to avoid flipflop
      subroutine calculate_using_split_central_and_sigma_parts()

         ktd = ktop(downstream_cell)
         kbd = min(ln0(3 - upstream_cell_index, Lb), ktd)
         kbd0 = kbd - 1
         hub = 0d0
         if (ktd == kbd) then
            ! downwind side one layer => default upwind sigma
            LLbc = Lb - 1
         else if (kt - kb == ktd - kbd .and. kt - kbot(upstream_cell) + 1 <= numtopsig .and. &
                  ktd - kbot(downstream_cell) + 1 <= numtopsig) then
            ! same number of layers on both sides within numtopsig => default upwind sigma
            LLbc = Lb - 1
         else
            select case (ihuz)
            case (CENTRAL_FROM_BED_TIL_SECOND_OR_FIRST_ABOVE_LOCAL_BOB)
               call calculate_for_upwind_cell_for_first_layer_above_local_bob()
            case (ALL_CENTRAL)
               LLbc = Ltop(link)
               hub = hu(link)
            case (CENTRAL_FROM_BED_TILL_HIGHEST_LAYER_WITH_EQUAL_LEVELS)
               call calculate_for_highest_layer_with_equal_zws()
            case (4)
               call calculate_central_bed_till_1_below_highest_downwind_layer()
            end select
            call calculate_hu_au_central_in_lower_part()
         end if
         call calculate_hu_au_upwind_in_upper_part()

      end subroutine calculate_using_split_central_and_sigma_parts

!> calculate_for_upwind_cell_for_first_layer_above_local_bob
      subroutine calculate_for_upwind_cell_for_first_layer_above_local_bob()

         do link_in_3d = Lb + 1, Ltop(link)
            hub = zws(kb + link_in_3d - Lb) - bed_level_at_u_point
            if (hub > 0) then
               LLbc = link_in_3d
               exit
            end if
         end do

      end subroutine calculate_for_upwind_cell_for_first_layer_above_local_bob

!> calculate_for_highest_layer_with_equal_zws
      subroutine calculate_for_highest_layer_with_equal_zws()

         LLbc = Ltop(link)
         do link_in_3d = Ltop(link) - 1, Lb + 1, -1
            if (zws(kb + link_in_3d - Lb) > bed_level_at_u_point .and. &
                abs(zws(kb + link_in_3d - Lb) - zws(kbd + link_in_3d - Lb)) < 1d-10) then
               LLbc = link_in_3d
               exit
            end if
         end do
         hub = zws(kb + LLbc - Lb) - bed_level_at_u_point

      end subroutine calculate_for_highest_layer_with_equal_zws

!> central from bed till one below highest downwind layer, much like 3
      subroutine calculate_central_bed_till_1_below_highest_downwind_layer()

         LLbc = Ltop(link)
         do link_in_3d = Ltop(link) - 1, Lb + 1, -1 ! search for second layer from top on downwind side
            if (zws(kb + link_in_3d - Lb) > bed_level_at_u_point .and. &
                ln(3 - upstream_cell_index, link_in_3d) == ktd - 1) then
               LLbc = link_in_3d
               exit
            end if
         end do

         hub = zws(kb + LLbc - Lb) - bed_level_at_u_point

      end subroutine calculate_central_bed_till_1_below_highest_downwind_layer

!> calculate_hu_au_central_in_lower_part
      subroutine calculate_hu_au_central_in_lower_part()

         integer, parameter :: option_AVERAGE = 1
         integer, parameter :: option_MAX = 2
         integer, parameter :: option_MIN = 3
         integer, parameter :: option_SIG = 4

         double precision :: sigma_downstream

         do link_in_3d = Lb, LLbc
            if (ihuzcsig == option_SIG) then
               sigma = dble(link_in_3d - Lb + 1) / dble(LLbc - Lb + 1) ! fifty/fifty, .33 or so
            else
               sigma = (zws(kb + link_in_3d - Lb) - zws(kb0)) / (zws(kb + LLbc - Lb) - zws(kb0)) ! sigmaup
               if (zws(kbd + link_in_3d - Lb) > zws(kbd0) .and. zws(kbd + LLbc - Lb) > zws(kbd0)) then
                  sigma_downstream = (zws(kbd + link_in_3d - Lb) - zws(kbd0)) / (zws(kbd + LLbc - Lb) - zws(kbd0))
                  if (ihuzcsig == option_AVERAGE) then
                     sigma = 0.5d0 * (sigma + sigma_downstream)
                  else if (ihuzcsig == option_MAX) then
                     sigma = max(sigma, sigma_downstream)
                  else if (ihuzcsig == option_MIN) then
                     sigma = min(sigma, sigma_downstream)
                  end if
               end if
            end if
            hu(link_in_3d) = sigma * hub
            call assign_au_3D()
         end do

      end subroutine calculate_hu_au_central_in_lower_part

!> assign_au_3D
      subroutine assign_au_3D()

         au(link_in_3d) = wu(link) * (hu(link_in_3d) - hu(link_in_3d - 1))
         au(link) = au(link) + au(link_in_3d)

      end subroutine assign_au_3D

!> calculate_hu_au_upwind_in_upper_part
      subroutine calculate_hu_au_upwind_in_upper_part()

         hub = hu(link) - hub
         do link_in_3d = LLbc + 1, Ltop(link)
            sigma = (zws(kb + link_in_3d - Lb) - zws(kb + LLbc - Lb)) / (zws(kt) - zws(kb + LLbc - Lb))
            hu(link_in_3d) = hu(LLbc) + sigma * hub
            call assign_au_3D()
         end do

      end subroutine calculate_hu_au_upwind_in_upper_part

!> default: upwind sigma oriented distribution of hu(link)
      subroutine calculate_hu_au_using_upwind_sigma()

         if (keepzlay1bedvol == 0 .or. &
             (kmxn(upstream_cell) == kmxn(downstream_cell) .and. kmxn(upstream_cell) <= numtopsig)) then
            call calculate_hu_au_upwind_based()
         else
            ! different numbers of layers
            ktd = ktop(downstream_cell)
            kbd = min(ln0(3 - upstream_cell_index, Lb), ktd)
            if (zws(ktd) - max(zws(kbd - 1), bl(downstream_cell)) > 0d0) then
               call calculate_hu_au_downwind_wet()
            else
               call calculate_hu_au_downwind_dry()
            end if
         end if

      end subroutine calculate_hu_au_using_upwind_sigma

!> calculate_hu_au_upwind_based
      subroutine calculate_hu_au_upwind_based()

         double precision :: hsku

         hsku = zws(kt) - zws(kb0)
         do link_in_3d = Lb, Ltop(link)
            sigma = (zws(kb + link_in_3d - Lb) - zws(kb0)) / hsku
            call assign_hu_3D()
            call assign_au_3D()
         end do

      end subroutine calculate_hu_au_upwind_based

!> assign_hu_3D
      subroutine assign_hu_3D()

         hu(link_in_3d) = sigma * hu(link)

      end subroutine assign_hu_3D

!> calculate_hu_au_downwind_wet
      subroutine calculate_hu_au_downwind_wet()
         double precision :: hskx

         zw0u = max(bl(upstream_cell), bl(downstream_cell))
         hskx = max(zws(kt), zws(ktd)) - zw0u
         do link_in_3d = Lb, Ltop(link)
            kkd = min(ktd, kbd + link_in_3d - Lb)
            sigma = (max(zws(kb + link_in_3d - Lb), zws(kkd)) - zw0u) / hskx
            call assign_hu_3D()
            call assign_au_3D()
         end do

      end subroutine calculate_hu_au_downwind_wet

!> calculate_hu_au_downwind_dry
      subroutine calculate_hu_au_downwind_dry()
         double precision :: hsku

         zw0u = max(zws(kb - 1), bl(upstream_cell))
         hsku = zws(kt) - zw0u
         do link_in_3d = Lb, Ltop(link)
            sigma = (zws(kb + link_in_3d - Lb) - zw0u) / hsku
            call assign_hu_3D()
            call assign_au_3D()
         end do

      end subroutine calculate_hu_au_downwind_dry

   end subroutine calculate_hu_au_and_advection_for_dams_weirs

!> set_upstream_water_level_getter
   subroutine set_upstream_water_level_getter(limiter_type_hu)

      implicit none

      integer, parameter :: UPWIND = 0
      integer, parameter :: CENTRAL = 21
      integer, parameter :: PEROT_ALFA = 22
      integer, parameter :: REGULAR_LINEAR_INTERPOLATION = 23

      integer, intent(in) :: limiter_type_hu

      select case (limiter_type_hu)
      case (UPWIND)
         get_upstream_water_level => get_upstream_water_level_upwind
      case (CENTRAL)
         get_upstream_water_level => get_upstream_water_level_central_limiter
      case (PEROT_ALFA)
         get_upstream_water_level => get_upstream_water_level_perot_alfa_limiter
      case (REGULAR_LINEAR_INTERPOLATION)
         get_upstream_water_level => get_upstream_water_level_regular_linear_interpolation
      case default
         ! usual limiters except 6
         get_upstream_water_level => get_upstream_water_level_usual_limiters
      end select

   end subroutine set_upstream_water_level_getter

!> get_upstream_water_level_upwind
   double precision function get_upstream_water_level_upwind() result(upstream_water_level)
      use m_flow, only: s0

      implicit none

      upstream_water_level = s0(upstream_cell)

   end function get_upstream_water_level_upwind

!> get_upstream_water_level_central_limiter
   double precision function get_upstream_water_level_central_limiter() result(upstream_water_level)
      use m_flow, only: s0

      implicit none

      upstream_water_level = 0.5d0 * (s0(left_cell) + s0(right_cell))

   end function get_upstream_water_level_central_limiter

   !> get_upstream_water_level_perot_alfa_limiter
   double precision function get_upstream_water_level_perot_alfa_limiter() result(upstream_water_level)
      use m_flow, only: s0
      use m_flowgeom, only: acl

      implicit none

      upstream_water_level = acl(link) * s0(left_cell) + (1d0 - acl(link)) * s0(right_cell)

   end function get_upstream_water_level_perot_alfa_limiter

!> get_upstream_water_level_regular_linear_interpolation
   double precision function get_upstream_water_level_regular_linear_interpolation() result(upstream_water_level)
      use m_flow, only: s0
      use m_flowgeom, only: acl

      implicit none

      upstream_water_level = acl(link) * s0(right_cell) + (1d0 - acl(link)) * s0(left_cell)

   end function get_upstream_water_level_regular_linear_interpolation

!> get_upstream_water_level_usual_limiters
   double precision function get_upstream_water_level_usual_limiters() result(upstream_water_level)
      use m_flowparameters, only: limtyphu
      use m_flow, only: s0
      use m_flowgeom, only: klnup, slnup
      use m_missing, only: dmiss
      use m_dslim

      implicit none

      integer :: klnup1
      integer :: klnup2
      integer :: ip
      double precision :: sku
      double precision :: ds1
      double precision :: ds2

      if (velocity > 0) then
         ip = 0
      else
         ip = 3
      end if

      klnup1 = klnup(1 + ip, link)
      sku = dmiss
      if (klnup1 < 0) then
         sku = s0(abs(klnup1))
      else if (klnup1 > 0) then
         klnup2 = abs(klnup(2 + ip, link))
         if (klnup2 > 0) then
            sku = s0(klnup1) * slnup(1 + ip, link) + s0(klnup2) * slnup(2 + ip, link)
         end if
      end if
      if (sku /= dmiss) then
         ds1 = (s0(upstream_cell) - sku) * slnup(3 + ip, link)
         ds2 = s0(downstream_cell) - s0(upstream_cell)
         upstream_water_level = s0(upstream_cell) + dslim(ds1, ds2, limtyphu)
      else
         upstream_water_level = s0(upstream_cell)
      end if

   end function get_upstream_water_level_usual_limiters

end module m_sethu
