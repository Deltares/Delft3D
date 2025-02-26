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
!> @file setlinktocornerweights.f90
!! Subroutine for allocating corner related link x- and y weights.
module m_setlinktocornerweights

   implicit none

   private

   public :: setlinktocornerweights

contains

   subroutine setlinktocornerweights() ! set corner related link x- and y weights
      use precision, only: dp
      use m_flow, only: Perot_weight_update, PEROT_STATIC, jacomp, irov
      use m_netw, only: nmk, numk, nod, lnn, xk, yk, kn
      use m_flowgeom, only: wcnx3, wcny3, wcnx4, wcny4, wcln, lnx1d, lnx, lncn, kcu, wu, dx, csu, snu, acn, cscnw, sncnw, jacorner, kcnw, lne2ln
      use geometry_module, only: normalin
      use m_sferic, only: jsferic, jasfer3D
      use m_missing, only: dxymis
      use gridoperations
      use m_lin2corx, only: lin2corx
      use m_lin2cory, only: lin2cory

      real(kind=dp) :: ax, ay, wuL, wud, csa, sna
      real(kind=dp), dimension(3, numk) :: wcnxy ! corner weight factors (3,numk), only for normalising
      integer :: k, L, nx
      integer :: k3, k4
      integer :: ka, kb, LL
      integer :: krcnw ! counter for cn points attached to 2 closed walls
      wcnx3 = 0
      wcny3 = 0
      wcnx4 = 0
      wcny4 = 0

      !if (kmx > 0 .and. jased > 0 .and. jased < 4) then
      wcLn = 0
      !endif

      nx = 0
      do L = lnx1D + 1, lnx
         k3 = lncn(1, L); k4 = lncn(2, L)
         nx = max(nx, k3, k4)
      end do
      wcnxy = 0

      do L = lnx1D + 1, lnx
         if (abs(kcu(L)) == 1) then
            cycle
         end if

         wud = wu(L) * dx(L)
         k3 = lncn(1, L); k4 = lncn(2, L)
         wcnxy(3, k3) = wcnxy(3, k3) + wud
         wcnxy(3, k4) = wcnxy(3, k4) + wud

         wcLn(1, L) = wud
         wcLn(2, L) = wud

!    csa  = max( 1d-6,abs(csu(L)) )
!    sna  = max( 1d-6,abs(snu(L)) )
         csa = max(1d-6, abs(lin2corx(L, 1, csu(L), snu(L))))
         sna = max(1d-6, abs(lin2cory(L, 1, csu(L), snu(L))))

         wuL = acn(1, L) * wud
         if (jacomp == 1) then
            ax = csa * wuL
            ay = sna * wuL
         else
            ax = 0.5d0 * wuL
            ay = ax
         end if
         wcnx3(L) = ax
         wcny3(L) = ay

         wcnxy(1, k3) = wcnxy(1, k3) + ax
         wcnxy(2, k3) = wcnxy(2, k3) + ay
!    wcnxy(1,k3) = wcnxy(1,k3) + lin2corx(L,1,ax,ay)
!    wcnxy(2,k3) = wcnxy(2,k3) + lin2cory(L,1,ax,ay)

!    csa  = max( 1d-6,abs(csu(L)) )
!    sna  = max( 1d-6,abs(snu(L)) )
         csa = max(1d-6, abs(lin2corx(L, 2, csu(L), snu(L))))
         sna = max(1d-6, abs(lin2cory(L, 2, csu(L), snu(L))))

         wuL = acn(2, L) * wud
         if (jacomp == 1) then
            ax = csa * wuL
            ay = sna * wuL
         else
            ax = 0.5d0 * wuL
            ay = ax
         end if
         wcnx4(L) = ax
         wcny4(L) = ay
         wcnxy(1, k4) = wcnxy(1, k4) + ax
         wcnxy(2, k4) = wcnxy(2, k4) + ay
!    wcnxy(1,k4) = wcnxy(1,k4) + lin2corx(L,2,ax,ay)
!    wcnxy(2,k4) = wcnxy(2,k4) + lin2cory(L,2,ax,ay)
      end do

      do L = lnx1D + 1, lnx
         if (abs(kcu(L)) == 1) cycle
         k3 = lncn(1, L); k4 = lncn(2, L)
         if (wcnxy(1, k3) /= 0) wcnx3(L) = wcnx3(L) / wcnxy(1, k3)
         if (wcnxy(2, k3) /= 0) wcny3(L) = wcny3(L) / wcnxy(2, k3)
         if (wcnxy(1, k4) /= 0) wcnx4(L) = wcnx4(L) / wcnxy(1, k4)
         if (wcnxy(2, k4) /= 0) wcny4(L) = wcny4(L) / wcnxy(2, k4)

         if (wcnxy(3, k3) /= 0) wcLn(1, L) = wcLn(1, L) / wcnxy(3, k3)
         if (wcnxy(3, k4) /= 0) wcLn(2, L) = wcLn(2, L) / wcnxy(3, k4)

         if (irov == 2) then ! zero cornervelocities for no-slip

            if (int(wcnxy(3, k3)) /= nmk(k3)) then
               wcnx3(L) = 0d0; wcny3(L) = 0d0
            end if

            if (int(wcnxy(3, k4)) /= nmk(k4)) then
               wcnx4(L) = 0d0; wcny4(L) = 0d0
            end if

         end if

      end do

      cscnw = 0
      sncnw = 0
      kcnw = 0
      !nwalcnw = 0
      !sfcnw = 0
      krcnw = 0
      do k = 1, numk ! set up admin for corner velocity alignment at closed walls

!    if ( nmk(k) - int(wcnxy (3,k)) == 2 ) then ! two more netlinks than flowlinks to this corner
         if (jacorner(k) == 1) then
            krcnw = krcnw + 1 ! cnw = cornerwall point (netnode)
            kcnw(krcnw) = k
            ka = 0; kb = 0
            do LL = 1, nmk(k)
               L = nod(k)%lin(LL) ! netstuff
               if (lnn(L) == 1) then
                  if (ka == 0) then
                     if (lne2ln(L) <= 0 .and. kn(3, L) /= 0) then ! SPvdP: closed boundaries used in determination of normal vector only
                        call othernode(k, L, ka) ! use other node on closed boundary
                     else
                        ka = k ! use own node on open boundary
                     end if
                  else if (kb == 0 .and. kn(3, L) /= 0) then
                     if (lne2ln(L) <= 0) then ! SPvdP: closed boundaries used in determination of normal vector only
                        call othernode(k, L, kb) ! use other node on closed boundary
                     else
                        kb = k ! use own node on closed boundary
                     end if
                  end if
               end if
            end do
            if (ka /= 0 .and. kb /= 0 .and. ka /= kb) then ! only for 2D netnodes
               call normalin(xk(ka), yk(ka), xk(kb), yk(kb), csa, sna, xk(k), yk(k), jsferic, jasfer3D, dxymis)
               cscnw(krcnw) = csa
               sncnw(krcnw) = sna
            end if

         end if

      end do

      if (Perot_weight_update == PEROT_STATIC) then
         deallocate (acn, jacorner)
      end if

   end subroutine setlinktocornerweights

end module m_setlinktocornerweights
