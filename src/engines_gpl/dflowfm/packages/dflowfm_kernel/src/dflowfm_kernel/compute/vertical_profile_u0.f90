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
!

! =================================================================================================
! =================================================================================================
module m_vertical_profile_u0
use m_tridag, only: tridag


   implicit none

   private

   public :: vertical_profile_u0

contains

   subroutine vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)
      use precision, only: dp
      use m_flow
      use m_flowgeom
      use m_flowtimes
      use m_missing
      use m_waves
      use m_sferic
      use m_filter_data, only: ustar, itype
      implicit none
      integer :: Lb, Lt, kxL, LL
      real(kind=dp) :: a(kmxx), b(kmxx), c(kmxx), d(kmxx), e(kmxx), dzu(kxL), womegu(kxL - 1), dzv(kmxx)

      integer :: L, k, k1, k2
      real(kind=dp) :: dzLw, vstress, adv, adv1, tt, ustv, st2, agp, dzurho

      real(kind=dp) :: gdxi, gdxids, du, cu, ac1, ac2, hup, twot = 0.666666666666d0, slopec

      real(kind=dp) :: aa(kmxx), cc(kmxx) ! for five-diaginal matrix
      ! aa(i)*u(i-2)+a(i)*u(i-1)+b(i)*u(i)+c(i)*u(i+1)+cc(i)*u(i+2)=d(i)

      integer :: jav3

      a(1:kxL) = 0d0; b(1:kxL) = dti; c(1:kxL) = 0d0

      if (jafilter /= 0) then
         d(1:kxL) = ustar(Lb:Lt) * dti
      else
         d(1:kxL) = u0(Lb:Lt) * dti ! put u1 in ddk
      end if

      aa(1:kxL) = 0d0; cc(1:kxL) = 0d0

      adv = 0d0; adv1 = 0d0

      ac1 = acL(LL); ac2 = 1d0 - ac1

      do L = Lb, 0 ! Lt
         k = L - Lb + 1
         k1 = ln(1, L); k2 = ln(2, L)
         dzv(k) = ac1 * (zws(k1) - zws(k1 - 1)) + ac2 * (zws(k2) - zws(k2 - 1)) ! volume weighted dzu , ok for pillar
      end do

      jav3 = 0
      if (javau == 3) then
         jav3 = 1
      else if (javau3onbnd == 1) then
         if (LL > lnxi) jav3 = 1
      else if (javau3onbnd == 2) then
         if (iadv(LL) == 6) jav3 = 1
      end if

      do L = Lb, Lt - 1
         k = L - Lb + 1
         dzLw = 0.5d0 * (dzu(k + 1) + dzu(k))

         vstress = (vicwwu(L) + vicoww) / dzLw ! long time default like DPM,  finite volume weights, dim = (m/s)

         ! vstress  = (vicwwu(L) + vicoww + viskin ) / dzLw                    ! 08-12-14 : add kinematic viscosity

         ! vstress  = ( max(vicwwu(L), vicoww) + viskin ) / dzLw                 ! 23-12-14 : D3D like

         if (jav3 == 1) then ! vertical advection upwind implicit
            if (womegu(k) > 0d0) then
               if (jarhoxu > 0) then
                  adv1 = womegu(k) * rhou(L) / rhou(L + 1); adv = 0d0
               else
                  adv1 = womegu(k); adv = 0d0 ! here, omegu(k) lies above u point of same index
               end if
            else if (womegu(k) < 0d0) then
               if (jarhoxu > 0) then
                  adv = -womegu(k) * rhou(L + 1) / rhou(L); adv1 = 0d0
               else
                  adv = -womegu(k); adv1 = 0d0
               end if
            end if

            ! adv = 0d0 ; adv1 = 0d0   ! noslip test

            !tt     = vstress/dzu(k+1) + adv1/dzv(k+1)
            tt = (vstress + adv1) / dzu(k + 1)
            b(k + 1) = b(k + 1) + tt
            a(k + 1) = a(k + 1) - tt

            !tt     = vstress/dzu(k  ) + adv/dzv(k  )
            tt = (vstress + adv) / dzu(k)
            b(k) = b(k) + tt
            c(k) = c(k) - tt

            !a(k+1) = a(k+1) - adv1/dzu(k+1)
            !b(k+1) = b(k+1) + adv/dzu(k+1)
            !
            !b(k) = b(k) + adv1/dzu(k)
            !c(k) = c(k) - adv/dzu(k)

            !d(k)   = d(k)   - adv1/dzu(k)   * u0(L) + adv/dzu(k)   * u0(L+1)
            !d(k+1) = d(k+1) + adv1/dzu(k+1) * u0(L) - adv/dzu(k+1) * u0(L+1)

         else if (javau == 4) then ! vertical advection central implicit

            adv = 0.5d0 * womegu(k) ! here, omegu(k) lies above u point of same index

            b(k + 1) = b(k + 1) + (vstress - adv) / dzu(k + 1)
            a(k + 1) = a(k + 1) - (vstress + adv) / dzu(k + 1)

            b(k) = b(k) + (vstress + adv) / dzu(k)
            c(k) = c(k) - (vstress - adv) / dzu(k)

         else if (javau == 0 .or. javau >= 6) then ! 3D checkerboard

            if (jarhoxu < 3) then
               b(k + 1) = b(k + 1) + vstress / dzu(k + 1)
               a(k + 1) = a(k + 1) - vstress / dzu(k + 1)

               b(k) = b(k) + vstress / dzu(k)
               c(k) = c(k) - vstress / dzu(k)
            else if (jarhoxu == 3) then
               vstress = vstress * (rhou(L) * dzu(k) + rhou(L + 1) * dzu(k + 1)) / (2d0 * dzLw)
               dzurho = dzu(k + 1) * rhou(L + 1)
               b(k + 1) = b(k + 1) + vstress / dzurho
               a(k + 1) = a(k + 1) - vstress / dzurho

               dzurho = dzu(k) * rhou(L)
               b(k) = b(k) + vstress / dzurho
               c(k) = c(k) - vstress / dzurho
            else if (jarhoxu >= 4) then

               b(k + 1) = b(k + 1) + vstress / dzu(k + 1)
               a(k + 1) = a(k + 1) - vstress * rhou(L) / (dzu(k + 1) * rhou(L + 1))

               b(k) = b(k) + vstress / dzu(k)
               c(k) = c(k) - vstress * rhou(L + 1) / (dzu(k) * rhou(L))
            end if

         else if (javau == 5) then
            if (womegu(k) > 0) then
               if (jarhoxu > 0) then
                  adv1 = womegu(k) * rhou(L) / rhou(L + 1); adv = 0d0
               else
                  adv1 = womegu(k); adv = 0d0
               end if
               if (L == Lb) then
                  tt = adv1 / dzu(k + 1)
                  b(k + 1) = b(k + 1) + tt
                  a(k + 1) = a(k + 1) - tt
               else
                  tt = adv1 / dzu(k)
                  a(k) = a(k) - tt * 0.125d0
                  b(k) = b(k) + tt * 0.750d0 - tt
                  c(k) = c(k) + tt * 0.375d0
                  tt = adv1 / dzu(k + 1)
                  aa(k + 1) = aa(k + 1) + tt * 0.125d0
                  a(k + 1) = a(k + 1) - tt * 0.750d0
                  b(k + 1) = b(k + 1) - tt * 0.375d0 + tt
               end if
            else
               if (jarhoxu > 0) then
                  adv = -womegu(k) * rhou(L + 1) / rhou(L); adv1 = 0d0
               else
                  adv = -womegu(k); adv1 = 0d0
               end if
               if (L == Lt - 1) then
                  tt = adv / dzu(k)
                  b(k) = b(k) + tt
                  c(k) = c(k) - tt
               else
                  tt = -adv / dzu(k)
                  b(k) = b(k) + tt * 0.375d0 - tt
                  c(k) = c(k) + tt * 0.750d0
                  cc(k) = cc(k) - tt * 0.125d0
                  tt = -adv / dzu(k + 1)
                  a(k + 1) = a(k + 1) - tt * 0.375d0
                  b(k + 1) = b(k + 1) - tt * 0.750d0 + tt
                  c(k + 1) = c(k + 1) + tt * 0.125
               end if
            end if

            tt = vstress / dzu(k + 1)
            b(k + 1) = b(k + 1) + tt
            a(k + 1) = a(k + 1) - tt
            tt = vstress / dzu(k)
            b(k) = b(k) + tt
            c(k) = c(k) - tt
         else
            adv = 0d0; adv1 = 0d0
         end if

         if (jawave > 0 .and. jawaveStokes == 4 .and. .not. flowWithoutWaves) then ! ustokes correction in vertical viscosity
            ustv = vstress * (ustokes(L) - ustokes(L - 1))
            d(k + 1) = d(k + 1) + ustv / dzu(k + 1)
            d(k) = d(k) - ustv / dzu(k)
         end if

      end do

      agp = ag
      if (jahelmert > 0 .and. jsferic > 0) then ! possibly operationalise later for now avoid the checks
         st2 = sin(dg2rd * yu(L))**2
         agp = 9.7803253359 * (1d0 + 0.00193185265241 * st2) / sqrt(1d0 - 0.00669437999013 * st2)
      end if
      gdxi = agp * dxi(LL)

      if (jarhoxu >= 2) then
         gdxi = gdxi * rhomean / rhou(L)
      end if

      k1 = ln(1, LL); k2 = ln(2, LL)
      gdxids = gdxi * (s0(k2) - s0(k1))

      slopec = 0d0
      if (drop3D > 0d0) then
         if (.not. (iadv(LL) == 21 .or. iadv(LL) >= 23 .and. iadv(LL) <= 25)) then ! don't do this for weirs
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
      du = gdxids * (1d0 - teta(LL)) - gdxi * slopec

      if (jafilter /= 0 .and. itype == 3) then
         do L = Lb, Lt
            k = L - Lb + 1
            b(k) = b(k) + advi(L)
            d(k) = d(k) - du
         end do
      else
         do L = Lb, Lt
            k = L - Lb + 1
            b(k) = b(k) + advi(L)
            d(k) = d(k) - adve(L) - du
         end do
      end if

      if (javau == 5) then
         call pentadiag(aa, a, b, c, cc, d, Ru(Lb:), kxL)
      else
         call tridag(a, b, c, d, e, Ru(Lb:), kxL)
      end if

      d(1:kxL) = cu

      if (javau == 5) then
         call pentadiag(aa, a, b, c, cc, d, Fu(Lb:), kxL)
      else
         call tridag(a, b, c, d, e, Fu(Lb:), kxL)
      end if

   end subroutine vertical_profile_u0

end module m_vertical_profile_u0
