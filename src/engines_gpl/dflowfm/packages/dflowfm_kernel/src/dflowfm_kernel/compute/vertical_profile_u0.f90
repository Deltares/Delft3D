!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_vertical_profile_u0
   use m_tridag, only: tridag

   implicit none

   private

   public :: vertical_profile_u0

contains

   subroutine vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)
      use precision, only: dp
      use m_flow, only: kmxx, jafilter, u0, javau, javau3onbnd, vicwwu, background_vertical_eddy_viscosity, jarhoxu, rhou, jawave, no_waves, jawavestokes, stokes_drift_2ndorder_visc_adve, flow_without_waves, ag, use_helmert, rhomean, s0, drop3d, hu, advi, adve, ru, fu
      use m_flowgeom, only: acl, ln, lnxi, iadv, yu, dxi, iadv_subgrid_weir, iadv_rajaratnam_weir, iadv_villemonte_weir, bob, teta
      use m_flowtimes, only: dti
      use m_waves, only: ustokes
      use m_sferic, only: jsferic, dg2rd
      use m_filter_data, only: ustar, itype
      implicit none
      integer :: Lb, Lt, kxL, LL
      real(kind=dp), dimension(kmxx) :: lower, diagonal, upper, rhs, normalized_upper
      real(kind=dp) :: dzu(kxL), womegu(kxL - 1)

      integer :: L, k, k1, k2
      real(kind=dp) :: dzLw, vstress, adv, adv1, tt, ustv, st2, agp, dzurho

      real(kind=dp) :: gdxi, gdxids, du, cu, ac1, ac2, hup, twot = 0.666666666666_dp, slopec

      real(kind=dp), dimension(kmxx) :: second_lower, second_upper ! Second off-diagonals of the pentadiagonal matrix.
      ! second_lower(i)*u(i-2)+lower(i)*u(i-1)+diagonal(i)*u(i)+upper(i)*u(i+1)+second_upper(i)*u(i+2)=rhs(i)

      integer :: jav3

      lower(1:kxL) = 0.0_dp
      diagonal(1:kxL) = dti
      upper(1:kxL) = 0.0_dp

      if (jafilter /= 0) then
         rhs(1:kxL) = ustar(Lb:Lt) * dti
      else
         rhs(1:kxL) = u0(Lb:Lt) * dti ! put u1 in ddk
      end if

      second_lower(1:kxL) = 0.0_dp
      second_upper(1:kxL) = 0.0_dp

      adv = 0.0_dp
      adv1 = 0.0_dp

      ac1 = acL(LL)
      ac2 = 1.0_dp - ac1

      jav3 = 0
      if (javau == 3) then
         jav3 = 1
      else if (javau3onbnd == 1) then
         if (LL > lnxi) then
            jav3 = 1
         end if
      else if (javau3onbnd == 2) then
         if (iadv(LL) == 6) then
            jav3 = 1
         end if
      end if

      do L = Lb, Lt - 1
         k = L - Lb + 1
         dzLw = 0.5_dp * (dzu(k + 1) + dzu(k))

         vstress = (vicwwu(L) + background_vertical_eddy_viscosity%get(LL)) / dzLw ! long time default like DPM,  finite volume weights, dim = (m/s)
         
         adv = 0.0_dp
         adv1 = 0.0_dp

         if (jav3 == 1) then ! vertical advection upwind implicit
            if (womegu(k) > 0.0_dp) then
               if (jarhoxu > 0) then
                  adv1 = womegu(k) * rhou(L) / rhou(L + 1)
                  adv = 0.0_dp
               else
                  adv1 = womegu(k)
                  adv = 0.0_dp ! here, omegu(k) lies above u point of same index
               end if
            else if (womegu(k) < 0.0_dp) then
               if (jarhoxu > 0) then
                  adv = -womegu(k) * rhou(L + 1) / rhou(L)
                  adv1 = 0.0_dp
               else
                  adv = -womegu(k)
                  adv1 = 0.0_dp
               end if
            end if

            tt = (vstress + adv1) / dzu(k + 1)
            diagonal(k + 1) = diagonal(k + 1) + tt
            lower(k + 1) = lower(k + 1) - tt

            tt = (vstress + adv) / dzu(k)
            diagonal(k) = diagonal(k) + tt
            upper(k) = upper(k) - tt

         else if (javau == 4) then ! vertical advection central implicit

            adv = 0.5_dp * womegu(k) ! here, omegu(k) lies above u point of same index

            diagonal(k + 1) = diagonal(k + 1) + (vstress - adv) / dzu(k + 1)
            lower(k + 1) = lower(k + 1) - (vstress + adv) / dzu(k + 1)

            diagonal(k) = diagonal(k) + (vstress + adv) / dzu(k)
            upper(k) = upper(k) - (vstress - adv) / dzu(k)

         else if (javau == 0 .or. javau >= 6) then ! 3D checkerboard

            if (jarhoxu < 3) then
               diagonal(k + 1) = diagonal(k + 1) + vstress / dzu(k + 1)
               lower(k + 1) = lower(k + 1) - vstress / dzu(k + 1)

               diagonal(k) = diagonal(k) + vstress / dzu(k)
               upper(k) = upper(k) - vstress / dzu(k)
            else if (jarhoxu == 3) then
               vstress = vstress * (rhou(L) * dzu(k) + rhou(L + 1) * dzu(k + 1)) / (2.0_dp * dzLw)
               dzurho = dzu(k + 1) * rhou(L + 1)
               diagonal(k + 1) = diagonal(k + 1) + vstress / dzurho
               lower(k + 1) = lower(k + 1) - vstress / dzurho

               dzurho = dzu(k) * rhou(L)
               diagonal(k) = diagonal(k) + vstress / dzurho
               upper(k) = upper(k) - vstress / dzurho
            else if (jarhoxu >= 4) then

               diagonal(k + 1) = diagonal(k + 1) + vstress / dzu(k + 1)
               lower(k + 1) = lower(k + 1) - vstress * rhou(L) / (dzu(k + 1) * rhou(L + 1))

               diagonal(k) = diagonal(k) + vstress / dzu(k)
               upper(k) = upper(k) - vstress * rhou(L + 1) / (dzu(k) * rhou(L))
            end if

         else if (javau == 5) then
            if (womegu(k) > 0) then
               if (jarhoxu > 0) then
                  adv1 = womegu(k) * rhou(L) / rhou(L + 1)
                  adv = 0.0_dp
               else
                  adv1 = womegu(k)
                  adv = 0.0_dp
               end if
               if (L == Lb) then
                  tt = adv1 / dzu(k + 1)
                  diagonal(k + 1) = diagonal(k + 1) + tt
                  lower(k + 1) = lower(k + 1) - tt
               else
                  tt = adv1 / dzu(k)
                  lower(k) = lower(k) - tt * 0.125_dp
                  diagonal(k) = diagonal(k) + tt * 0.750_dp - tt
                  upper(k) = upper(k) + tt * 0.375_dp
                  tt = adv1 / dzu(k + 1)
                  second_lower(k + 1) = second_lower(k + 1) + tt * 0.125_dp
                  lower(k + 1) = lower(k + 1) - tt * 0.750_dp
                  diagonal(k + 1) = diagonal(k + 1) - tt * 0.375_dp + tt
               end if
            else
               if (jarhoxu > 0) then
                  adv = -womegu(k) * rhou(L + 1) / rhou(L)
                  adv1 = 0.0_dp
               else
                  adv = -womegu(k)
                  adv1 = 0.0_dp
               end if
               if (L == Lt - 1) then
                  tt = adv / dzu(k)
                  diagonal(k) = diagonal(k) + tt
                  upper(k) = upper(k) - tt
               else
                  tt = -adv / dzu(k)
                  diagonal(k) = diagonal(k) + tt * 0.375_dp - tt
                  upper(k) = upper(k) + tt * 0.750_dp
                  second_upper(k) = second_upper(k) - tt * 0.125_dp
                  tt = -adv / dzu(k + 1)
                  lower(k + 1) = lower(k + 1) - tt * 0.375_dp
                  diagonal(k + 1) = diagonal(k + 1) - tt * 0.750_dp + tt
                  upper(k + 1) = upper(k + 1) + tt * 0.125
               end if
            end if

            tt = vstress / dzu(k + 1)
            diagonal(k + 1) = diagonal(k + 1) + tt
            lower(k + 1) = lower(k + 1) - tt
            tt = vstress / dzu(k)
            diagonal(k) = diagonal(k) + tt
            upper(k) = upper(k) - tt
         else
            adv = 0.0_dp
            adv1 = 0.0_dp
         end if

         if (jawave > NO_WAVES .and. jawaveStokes == STOKES_DRIFT_2NDORDER_VISC_ADVE .and. .not. flow_without_waves) then ! ustokes correction in vertical viscosity
            ustv = vstress * (ustokes(L+1) - ustokes(L))
            rhs(k + 1) = rhs(k + 1) + ustv / dzu(k + 1)
            rhs(k) = rhs(k) - ustv / dzu(k)
         end if

      end do

      agp = ag
      if (use_helmert > 0 .and. jsferic > 0) then ! possibly operationalise later for now avoid the checks
         st2 = sin(dg2rd * yu(LL))**2
         agp = 9.7803253359 * (1.0_dp + 0.00193185265241 * st2) / sqrt(1.0_dp - 0.00669437999013 * st2)
      end if
      gdxi = agp * dxi(LL)

      if (jarhoxu >= 2) then
         gdxi = gdxi * rhomean / rhou(L) ! top layer rho!
      end if

      k1 = ln(1, LL)
      k2 = ln(2, LL)
      gdxids = gdxi * (s0(k2) - s0(k1))

      slopec = 0.0_dp
      if (drop3D > 0.0_dp) then
         if (.not. (iadv(LL) == IADV_SUBGRID_WEIR .or. iadv(LL) >= IADV_RAJARATNAM_WEIR .and. iadv(LL) <= IADV_VILLEMONTE_WEIR)) then ! don't do this for weirs
            hup = s0(k2) - (min(bob(1, LL), bob(2, LL)) + drop3D * twot * hu(LL))
            if (hup < 0) then
               slopec = hup
            else
               hup = s0(k1) - (min(bob(1, LL), bob(2, LL)) + drop3D * twot * hu(LL))
               if (hup < 0) then
                  slopec = -hup
               end if
            end if
         end if
      end if

      cu = gdxi * teta(LL)
      du = gdxids * (1.0_dp - teta(LL)) - gdxi * slopec

      if (jafilter /= 0 .and. itype == 3) then
         do L = Lb, Lt
            k = L - Lb + 1
            diagonal(k) = diagonal(k) + advi(L)
            rhs(k) = rhs(k) - du
         end do
      else
         do L = Lb, Lt
            k = L - Lb + 1
            diagonal(k) = diagonal(k) + advi(L)
            rhs(k) = rhs(k) - adve(L) - du
         end do
      end if

      if (javau == 5) then
         call pentadiag(second_lower, lower, diagonal, upper, second_upper, rhs, Ru(Lb:), kxL)
         rhs(1:kxL) = cu
         call pentadiag(second_lower, lower, diagonal, upper, second_upper, rhs, Fu(Lb:), kxL)
      else
         call tridag(lower, diagonal, upper, rhs, normalized_upper, Ru(Lb:), kxL)
         rhs(1:kxL) = cu
         call tridag(lower, diagonal, upper, rhs, normalized_upper, Fu(Lb:), kxL)
      end if

   end subroutine vertical_profile_u0

end module m_vertical_profile_u0
