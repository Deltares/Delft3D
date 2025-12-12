!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_anticreep

   implicit none

   private

   public :: anticreep

contains

   !> Reduces spurious horizontal layer motion ("creep") in stratified flows.
   subroutine anticreep(L)
      use precision, only: dp

      use m_flow, only: kmx, zws, BACKGROUNDWATERTEMPERATURE, BACKGROUNDSALINITY, ag, rhomean, adve, baroclinic_force_prev, dsall, dteml
      use m_flowgeom, only: ln, bob, acl, dx
      use m_transport, only: isalt, itemp, constituents
      use m_flowparameters, only: jasal, jatem, epshs
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_Lbot_Ltop, only: getLbotLtop
      use m_density_formulas, only: derivative_density_to_salinity_eckart, derivative_density_to_temperature_eckart

      integer, intent(in) :: L !< Horizontal link index

      real(kind=dp), allocatable, dimension(:) :: polal ! Z-coordinate horizontal layers in nm
      real(kind=dp), allocatable, dimension(:) :: pocol
      real(kind=dp), allocatable, dimension(:) :: polar ! Z-coordinate horizontal layers in nmu
      real(kind=dp), allocatable, dimension(:) :: pocor
      real(kind=dp), allocatable, dimension(:) :: poflu ! Z-coordinate gradient flux
      real(kind=dp), allocatable, dimension(:) :: point
      real(kind=dp), allocatable, dimension(:) :: drho, dsal, dtem
      real(kind=dp), allocatable, dimension(:) :: kicol, kicor

      integer :: k1, k2, kbl, kbr, ktl, ktr, kll, krr, kl, kr, kl1, kl2, kr1, kr2
      integer :: kpoint, kf, k, j, Lb, Lt, LL, kfmax, kfmax1, kflux

      real(kind=dp) :: grad, grad1, grad2, cl, cr, flux, flux1
      real(kind=dp) :: zbot, ztop, zmid, zbed, farea
      real(kind=dp) :: drho_dsalinity, drho_dtemperature, temp, sal, baroclinic_force

      allocate (polal(0:kmx), pocol(0:kmx), polar(0:kmx), pocor(0:kmx))
      allocate (poflu(0:2 * kmx + 1), kicol(0:2 * kmx + 1), kicor(0:2 * kmx + 1))
      allocate (point(0:2 * kmx + 1), drho(0:2 * kmx + 1), dsal(0:2 * kmx + 1), dtem(0:2 * kmx + 1))

      if (jasal == 0 .and. jatem == 0) then
         return
      end if

      k1 = ln(1, L)
      k2 = ln(2, L)
      call getkbotktop(k1, kbl, ktl)
      call getkbotktop(k2, kbr, ktr)
      call getLbotLtop(L, Lb, Lt)
      
      if (zws(ktl) - zws(kbl - 1) < epshs .or. zws(ktr) - zws(kbr - 1) < epshs) then
         return
      end if
      
      zbed = (bob(1, L) + bob(2, L)) * 0.5_dp ! interpolates the bed level on flow link
      !
      !***position horizontal interfaces left and right
      !
      polal = 0.0_dp
      pocol = 0.0_dp
      polar = 0.0_dp
      pocor = 0.0_dp
      polal(0) = zws(kbl - 1)
      polar(0) = zws(kbr - 1)
      do k = 1, kmx
         kl = kbl + k - 1
         kr = kbr + k - 1
         polal(k) = zws(kl)
         polar(k) = zws(kr)
         pocol(k) = (zws(kl) + zws(kl - 1)) * 0.5_dp
         pocor(k) = (zws(kr) + zws(kr - 1)) * 0.5_dp
      end do
      !
      !***merge polal and polar
      !
      kll = 0
      krr = 0
      do k = 0, 2 * kmx + 1
         j = 0
         if (polal(kll) < polar(krr)) then
            point(k) = polal(kll)
            kll = kll + 1
            if (kll > kmx) then
               kpoint = k + 1
               point(kpoint) = polar(krr)
               j = 1
               exit
            end if
         else
            point(k) = polar(krr)
            krr = krr + 1
            if (krr > kmx) then
               kpoint = k + 1
               point(kpoint) = polal(kll)
               j = 1
               exit
            end if
         end if
      end do
      if (j == 0) then
         kpoint = 2 * kmx + 1
      end if
      !
      !***position flux points
      !
      poflu = 0.0_dp
      kflux = kpoint
      do k = 1, kflux
         poflu(k) = 0.5_dp * (point(k) + point(k - 1))
      end do
      !
      !***k-index concentration points left and right for flux point
      !
      kll = 1
      krr = 1
      do kf = 1, kflux
         kicol(kf) = 0
         kicor(kf) = 0
         do k = kll, kmx
            if (poflu(kf) >= polal(k - 1) .and. poflu(kf) <= polal(k)) then
               kicol(kf) = k
               kll = k
               exit
            end if
         end do
         do k = krr, kmx
            if (poflu(kf) >= polar(k - 1) .and. poflu(kf) <= polar(k)) then
               kicor(kf) = k
               krr = k
               exit
            end if
         end do
      end do
      !
      !***computation diffusive flux using limiter
      !
      drho = 0.0_dp
      dsal = 0.0_dp
      dtem = 0.0_dp
      do kf = kflux, 1, -1
         kll = kicol(kf)
         krr = kicor(kf)
         if (kll * krr == 0) then
            cycle
         end if
         kl = kbl + kll - 1 ! changes the number of layer to number of cell
         kr = kbr + krr - 1
         if (point(kf) <= zbed) then
            exit
         end if
         drho(kf) = 0.0_dp
         dsal(kf) = 0.0_dp
         dtem(kf) = 0.0_dp
         !
         !***flux
         !
         if (pocor(krr) > pocol(kll)) then
            kl1 = ktl + 1
            do k = kl + 1, ktl
               if (pocol(k - kbl + 1) > pocor(krr)) then
                  kl1 = k
                  exit
               end if
            end do
            kl2 = kl1 - 1
         else
            kl1 = kbl - 1
            do k = kl - 1, kbl, -1
               if (pocol(k - kbl + 1) < pocor(krr)) then
                  kl1 = k
                  exit
               end if
            end do
            kl2 = kl1 + 1
         end if

         if (pocol(kll) > pocor(krr)) then
            kr1 = ktr + 1
            do k = kr + 1, ktr
               if (pocor(k - kbr + 1) > pocol(kll)) then
                  kr1 = k
                  exit
               end if
            end do
            kr2 = kr1 - 1
         else
            kr1 = kbr - 1
            do k = kr - 1, kbr, -1
               if (pocor(k - kbr + 1) < pocol(kll)) then
                  kr1 = k
                  exit
               end if
            end do
            kr2 = kr1 + 1
         end if

         if (jasal > 0) then
            cl = constituents(isalt, kl2)
            if (kl1 >= kbl .and. kl1 <= ktl) then
               cl = ((pocol(kl2 - kbl + 1) - pocor(krr)) * constituents(isalt, kl1) &
                     + (pocor(krr) - pocol(kl1 - kbl + 1)) * constituents(isalt, kl2)) &
                    / (pocol(kl2 - kbl + 1) - pocol(kl1 - kbl + 1))
            end if
            cr = constituents(isalt, kr2)
            if (kr1 >= kbr .and. kr1 <= ktr) then
               cr = ((pocor(kr2 - kbr + 1) - pocol(kll)) * constituents(isalt, kr1) &
                     + (pocol(kll) - pocor(kr1 - kbr + 1)) * constituents(isalt, kr2)) &
                    / (pocor(kr2 - kbr + 1) - pocor(kr1 - kbr + 1))
            end if
            grad1 = (constituents(isalt, kr) - cl) ! / dx(L)
            grad2 = (cr - constituents(isalt, kL)) ! / dx(L)
            grad = 0.0_dp
            if (grad1 * grad2 > 0.0_dp) then
               grad = 2.0_dp * grad1 * grad2 / (grad1 + grad2)
            end if
            sal = acl(L) * constituents(isalt, kl) + (1.0_dp - acl(L)) * constituents(isalt, kr)
            temp = backgroundwatertemperature
            if (jatem > 0) then
               temp = acl(L) * constituents(itemp, kl) + (1.0_dp - acl(L)) * constituents(itemp, kr)
            end if
            drho_dsalinity = derivative_density_to_salinity_eckart(sal, temp)
            drho(kf) = drho(kf) + drho_dsalinity * grad
            dsal(kf) = grad
         end if

         if (jatem > 0) then
            cl = constituents(itemp, kl2)
            if (kl1 >= kbl .and. kl1 <= ktl) then
               cl = ((pocol(kl2 - kbl + 1) - pocor(krr)) * constituents(itemp, kl1) &
                     + (pocor(krr) - pocol(kl1 - kbl + 1)) * constituents(itemp, kl2)) &
                    / (pocol(kl2 - kbl + 1) - pocol(kl1 - kbl + 1))
               cr = constituents(itemp, kr2)
            end if
            if (kr1 >= kbr .and. kr1 <= ktr) then
               cr = ((pocor(kr2 - kbr + 1) - pocol(kll)) * constituents(itemp, kr1) &
                     + (pocol(kll) - pocor(kr1 - kbr + 1)) * constituents(itemp, kr2)) &
                    / (pocor(kr2 - kbr + 1) - pocor(kr1 - kbr + 1))
            end if
            grad1 = (constituents(itemp, kr) - cl) ! / dx(L)
            grad2 = (cr - constituents(itemp, kl)) ! / dx(L)
            grad = 0.0_dp
            if (grad1 * grad2 > 0.0_dp) then
               grad = 2.0_dp * grad1 * grad2 / (grad1 + grad2)
            end if
            temp = acl(L) * constituents(itemp, kl) + (1.0_dp - acl(L)) * constituents(itemp, kr)
            sal = backgroundsalinity
            if (jasal > 0) then
               sal = acl(L) * constituents(isalt, kl) + (1.0_dp - acl(L)) * constituents(isalt, kr)
            end if
            drho_dtemperature = derivative_density_to_temperature_eckart(sal, temp)
            drho(kf) = drho(kf) + drho_dtemperature * grad
            dtem(kf) = grad
         end if
      end do

      baroclinic_force = 0.0_dp
      flux1 = 0.0_dp
      kfmax = kflux
      kfmax1 = kflux
      do k = kmx, 1, -1
         ztop = acl(L) * zws(kbl + k - 1) + (1.0_dp - acl(L)) * zws(kbr + k - 1)
         zbot = acl(L) * zws(kbl + k - 2) + (1.0_dp - acl(L)) * zws(kbr + k - 2)
         if (ztop - zbot < 1.0e-4_dp) then
            cycle
         end if
         zmid = (zbot + ztop) * 0.5_dp
         LL = Lb + k - 1
         do kf = kfmax, 1, -1 ! HK: double inside loop, same as D3D => too much work
            kll = kicol(kf)
            krr = kicor(kf)
            if (point(kf) <= zbed) then
               exit
            end if
            if (kll * krr == 0) then
               cycle
            end if
            if (zmid < point(kf - 1)) then
               flux = ag * (point(kf) - point(kf - 1)) * drho(kf) / rhomean
               flux1 = flux1 + flux
               baroclinic_force = flux1
            elseif (zmid < point(kf) .and. zmid >= point(kf - 1)) then
               flux = ag * (point(kf) - zmid) * drho(kf) / rhomean
               baroclinic_force = flux1 + flux
               kfmax = kf
               exit
            end if
         end do
         adve(LL) = adve(LL) + (1.5_dp * baroclinic_force - 0.5_dp * baroclinic_force_prev(LL)) / dx(L) ! To compensate for not dividing by dx above
         baroclinic_force_prev(LL) = baroclinic_force

         do kf = kfmax1, 1, -1
            farea = -max(point(kf) - ztop, 0.0_dp) & ! to find the flux area between the flux pieces and the sigma layer
                    + max(point(kf) - zbot, 0.0_dp) &
                    - max(point(kf - 1) - zbot, 0.0_dp)
            if (farea < 0) then
               kfmax1 = kf
               exit
            end if
            dsalL(LL) = dsalL(LL) + dsal(kf) * farea
            dtemL(LL) = dtemL(LL) + dtem(kf) * farea
         end do
         dsalL(LL) = dsalL(LL) / (ztop - zbot)
         dtemL(LL) = dtemL(LL) / (ztop - zbot)
      end do

      deallocate (polal, pocol, polar, pocor)
      deallocate (poflu, kicol, kicor)
      deallocate (point, drho, dsal, dtem)

   end subroutine anticreep

end module m_anticreep
