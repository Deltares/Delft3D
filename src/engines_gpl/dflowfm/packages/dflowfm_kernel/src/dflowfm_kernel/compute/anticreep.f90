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

! Anti-creep
module m_anticreep

   implicit none

   private

   public :: anticreep

contains

   subroutine anticreep(L)
      use precision, only: dp

      use m_flow
      use m_flowgeom
      use m_transport
      use m_flowparameters
      use m_get_kbot_ktop
      use m_get_Lbot_Ltop
      use m_dens_eck

      implicit none
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
      integer, intent(in) :: L
      real(kind=dp) :: grad, grad1, grad2, cl, cr, flux, flux1
      real(kind=dp) :: zbot, ztop, zmid, zbed, farea
      real(kind=dp) :: rhods, rhodt, temp, sal, dummy, dpbdx

      allocate (polal(0:kmx), pocol(0:kmx), polar(0:kmx), pocor(0:kmx))
      allocate (poflu(0:2 * kmx + 1), kicol(0:2 * kmx + 1), kicor(0:2 * kmx + 1))
      allocate (point(0:2 * kmx + 1), drho(0:2 * kmx + 1), dsal(0:2 * kmx + 1), dtem(0:2 * kmx + 1))

      if (jasal == 0 .and. jatem == 0) return

      k1 = ln(1, L); k2 = ln(2, L)
      call getkbotktop(k1, kbl, ktl)
      call getkbotktop(k2, kbr, ktr)
      call getLbotLtop(L, Lb, Lt)
      zbed = (bob(1, L) + bob(2, L)) * 0.5d0 ! interpolates the bed level on flow link
      !
      !***position horizontal interfaces left and right
      !
      polal = 0d0
      pocol = 0d0
      polar = 0d0
      pocor = 0d0
      polal(0) = zws(kbl - 1)
      polar(0) = zws(kbr - 1)
      do k = 1, kmx
         kl = kbl + k - 1
         kr = kbr + k - 1
         polal(k) = zws(kl)
         polar(k) = zws(kr)
         pocol(k) = (zws(kl) + zws(kl - 1)) * 0.5d0
         pocor(k) = (zws(kr) + zws(kr - 1)) * 0.5d0
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
      if (j == 0) kpoint = 2 * kmx + 1
      !
      !***position flux points
      !
      poflu = 0d0
      kflux = kpoint
      do k = 1, kflux
         poflu(k) = 0.5d0 * (point(k) + point(k - 1))
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
      drho = 0d0
      dsal = 0d0
      dtem = 0d0
      do kf = kflux, 1, -1
         kll = kicol(kf)
         krr = kicor(kf)
         if (kll * krr == 0) cycle
         kl = kbl + kll - 1 ! changes the number of layer to number of cell
         kr = kbr + krr - 1
         if (point(kf) <= zbed) exit
         drho(kf) = 0d0
         dsal(kf) = 0d0
         dtem(kf) = 0d0
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
            if (kl1 >= kbl .and. kl1 <= ktl) cl = ((pocol(kl2 - kbl + 1) - pocor(krr)) * constituents(isalt, kl1) &
                                                   + (pocor(krr) - pocol(kl1 - kbl + 1)) * constituents(isalt, kl2)) &
                                                  / (pocol(kl2 - kbl + 1) - pocol(kl1 - kbl + 1))
            cr = constituents(isalt, kr2)
            if (kr1 >= kbr .and. kr1 <= ktr) cr = ((pocor(kr2 - kbr + 1) - pocol(kll)) * constituents(isalt, kr1) &
                                                   + (pocol(kll) - pocor(kr1 - kbr + 1)) * constituents(isalt, kr2)) &
                                                  / (pocor(kr2 - kbr + 1) - pocor(kr1 - kbr + 1))
            grad1 = (constituents(isalt, kr) - cl) ! / dx(L)
            grad2 = (cr - constituents(isalt, kL)) ! / dx(L)
            grad = 0d0; if (grad1 * grad2 > 0d0) grad = 2.0d0 * grad1 * grad2 / (grad1 + grad2)
            sal = acl(L) * constituents(isalt, kl) + (1d0 - acl(L)) * constituents(isalt, kr)
            temp = backgroundwatertemperature
            if (jatem > 0) temp = acl(L) * constituents(itemp, kl) + (1d0 - acl(L)) * constituents(itemp, kr)
            call dens_eck(temp, sal, dummy, rhods, dummy)
            drho(kf) = drho(kf) + rhods * grad
            dsal(kf) = grad
         end if

         if (jatem > 0) then
            cl = constituents(itemp, kl2)
            if (kl1 >= kbl .and. kl1 <= ktl) cl = ((pocol(kl2 - kbl + 1) - pocor(krr)) * constituents(itemp, kl1) &
                                                   + (pocor(krr) - pocol(kl1 - kbl + 1)) * constituents(itemp, kl2)) &
                                                  / (pocol(kl2 - kbl + 1) - pocol(kl1 - kbl + 1))
            cr = constituents(itemp, kr2)
            if (kr1 >= kbr .and. kr1 <= ktr) cr = ((pocor(kr2 - kbr + 1) - pocol(kll)) * constituents(itemp, kr1) &
                                                   + (pocol(kll) - pocor(kr1 - kbr + 1)) * constituents(itemp, kr2)) &
                                                  / (pocor(kr2 - kbr + 1) - pocor(kr1 - kbr + 1))
            grad1 = (constituents(itemp, kr) - cl) ! / dx(L)
            grad2 = (cr - constituents(itemp, kl)) ! / dx(L)
            grad = 0d0; if (grad1 * grad2 > 0d0) grad = 2.0d0 * grad1 * grad2 / (grad1 + grad2)
            temp = acl(L) * constituents(itemp, kl) + (1d0 - acl(L)) * constituents(itemp, kr)
            sal = backgroundsalinity
            if (jasal > 0) sal = acl(L) * constituents(isalt, kl) + (1d0 - acl(L)) * constituents(isalt, kr)
            call dens_eck(temp, sal, dummy, dummy, rhodt)
            drho(kf) = drho(kf) + rhodt * grad
            dtem(kf) = grad
         end if
      end do

      dpbdx = 0d0
      flux1 = 0d0
      kfmax = kflux
      kfmax1 = kflux
      do k = kmx, 1, -1
         ztop = acl(L) * zws(kbl + k - 1) + (1d0 - acl(L)) * zws(kbr + k - 1)
         zbot = acl(L) * zws(kbl + k - 2) + (1d0 - acl(L)) * zws(kbr + k - 2)
         if (ztop - zbot < 1d-4) cycle
         zmid = (zbot + ztop) * 0.5d0
         LL = Lb + k - 1
         do kf = kfmax, 1, -1 ! HK: double inside loop, same as D3D => too much work
            kll = kicol(kf)
            krr = kicor(kf)
            if (point(kf) <= zbed) exit
            if (kll * krr == 0) cycle
            if (zmid < point(kf - 1)) then
               flux = ag * (point(kf) - point(kf - 1)) * drho(kf) / rhomean
               flux1 = flux1 + flux
               dpbdx = flux1
            elseif (zmid < point(kf) .and. zmid >= point(kf - 1)) then
               flux = ag * (point(kf) - zmid) * drho(kf) / rhomean
               dpbdx = flux1 + flux
               kfmax = kf
               exit
            end if
         end do
         if (jabaroctimeint <= 1) then ! explicit
            adve(LL) = adve(LL) + dpbdx / dx(L) !   to compensate for not dividing by dx above
         else
            adve(LL) = adve(LL) + (1.5d0 * dpbdx - 0.5d0 * dpbdx0(LL)) / dx(L) !   to compensate for not dividing by dx above
         end if
         if (abs(jabaroctimeint) >= 2) then
            dpbdx0(LL) = dpbdx
         end if

         do kf = kfmax1, 1, -1
            farea = -max(point(kf) - ztop, 0d0) & ! to find the flux area between the flux pieces and the sigma layer
                    + max(point(kf) - zbot, 0d0) &
                    - max(point(kf - 1) - zbot, 0d0)
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

      if (abs(jabaroctimeint) >= 2) then
         jabaroctimeint = abs(jabaroctimeint)
      end if

      deallocate (polal, pocol, polar, pocor)
      deallocate (poflu, kicol, kicor)
      deallocate (point, drho, dsal, dtem)

   end subroutine anticreep

end module m_anticreep
