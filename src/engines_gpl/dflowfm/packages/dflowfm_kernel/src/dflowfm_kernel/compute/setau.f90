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

module m_setau

   implicit none

   private

   public :: setau

contains

   subroutine setau() ! get wet cross-sections at u points, after limiters, setau = vol12D with japerim == 1
      use precision, only: dp
      use m_vol12d, only: vol12d
      use m_get_upstream_downstream_cell_numbers
      use m_get_lkbot_set_ltop_upwind
      use m_getflowdir, only: getflowdir
      use m_addlink2d, only: addlink2D
      use m_flowgeom, only: ndx2d, ndxi, bl, ba, bob, wu, dxi, ln
      use m_flow, only: kmx, kmxl, s0, s1, u1, a1, vol1_f, nonlin, ChangeVelocityAtStructures, au, au_nostrucs, hu, &
         advi, lbot, ltop
      use m_flowparameters, only: epshu, jbasqbnddownwindhs
      use m_partitioninfo, only: jampi, idomain, my_rank, reduce_at_all, reduce_wwssav_all
      use m_timer, only: jatimer, starttimer, stoptimer, IMPIREDUCE
      use m_longculverts, only: reduceFlowAreaAtLongculverts
      use fm_external_forcings_data, only: ndambreaksignals, L1dambreaksg, L2dambreaksg, kdambreak, &
         ngatesg, L1gatesg, L2gatesg, kgate, zgate, ncgensg, zcgen, L1cgensg, L2cgensg, kcgen, &
         nklep, lklep, nvalv, lvalv, valv, nqbnd, L1qbnd, L2qbnd, kbndu, huqbnd, wwssav_all, japartqbnd, &
         zbndq, qbndhutrs, at_all, dambreakLinksActualLength

      integer :: n, nq, L, k2
      integer :: ng, Lnu, LL, iup, k
      real(kind=dp) :: at, ssav, wwav, fac, zlu, zgaten, sup, bupmin, bup, openfact, afac, hh
      integer :: upstream_cell
      integer :: upstream_cell_index
      integer :: kb
      integer :: kt
      integer :: Lb
      integer :: left_cell
      integer :: right_cell
      integer :: downstream_cell
      integer :: direction_sign
      real(kind=dp) :: velocity

      real(kind=dp), parameter :: FAC23 = 0.6666666666667d0

      if (kmx == 0) then

         if (nonlin == 0) then
            do n = ndx2d + 1, ndxi
               hh = max(0d0, s1(n) - bl(n))
               vol1_f(n) = ba(n) * hh
               a1(n) = ba(n)
            end do
         else
            vol1_f(ndx2D + 1:ndxi) = 0d0
         end if

         call vol12D(1)
         if (ChangeVelocityAtStructures) then
            au_nostrucs = au
         end if

         ! set correct flow areas for dambreaks, using the actual flow width
         do n = 1, ndambreaksignals
            do k = L1dambreaksg(n), L2dambreaksg(n)
               L = abs(kdambreak(3, k))
               au(L) = hu(L) * dambreakLinksActualLength(k)
            end do
         end do
         call reduceFlowAreaAtLongculverts()

      end if

      do ng = 1, ngatesg ! loop over gate signals, setau
         zgaten = zgate(ng); bupmin = 9d9
         do n = L1gatesg(ng), L2gatesg(ng)
            LL = kgate(3, n)
            if (hu(LL) > 0d0) then
               bup = min(bob(1, LL), bob(2, LL))
               bupmin = min(bupmin, bup)
               sup = bup + hu(LL)
               openfact = (min(sup, zgaten) - bup) / hu(LL)
               afac = min(1d0, max(0d0, 1d0 - openfact))
               if (sup > zgaten) then
                  hu(LL) = hu(LL) - (sup - zgaten); au(LL) = hu(LL) * wu(LL)
                  advi(LL) = advi(LL) + afac * 0.5d0 * abs(u1(LL)) * dxi(LL)
               end if
               if (hu(LL) < epshu) then
                  hu(LL) = 0d0; au(LL) = 0d0
               else if (kmx > 0) then
                  do L = Lbot(LL), Lbot(LL) + kmxL(LL) - 1
                     if (hu(L) > hu(L - 1)) then
                        ZLu = bup + hu(L - 1)
                        fac = (zgaten - zLu) / (hu(L) - hu(L - 1))
                        fac = max(0d0, min(1d0, fac))
                        Lnu = L
                        advi(L) = advi(L) + afac * 0.5d0 * abs(u1(L)) * dxi(LL)
                        if (fac < 0.1d0 .and. L > Lbot(LL)) then
                           Lnu = L - 1
                           !Ltop(LL) =    Lnu   ! keep total baroclinic pressure
                           hu(Lnu) = hu(Lnu) + fac * (hu(L) - hu(L - 1))
                           au(Lnu) = au(Lnu) + fac * au(L)
                           exit
                        else
                           hu(Lnu) = hu(Lnu - 1) + fac * (hu(Lnu) - hu(Lnu - 1))
                           au(Lnu) = fac * au(Lnu)
                        end if
                     end if
                  end do
                  au(Lnu + 1:Lbot(LL) + kmxL(LL) - 1) = 0d0 ! -12346d0 ! 6 not 5
                  hu(Lnu + 1:Lbot(LL) + kmxL(LL) - 1) = 0d0 ! -12346d0 ! 6 not 5

               end if
            end if
         end do
         if (bupmin /= 9d9) then
            zgate(ng) = max(zgate(ng), bupmin)
         end if

      end do

      do ng = 1, ncgensg ! loop over generalstruc signals, sethu
         zgaten = zcgen(3 * (ng - 1) + 2); bupmin = 9d9
         ! wufac  = zcgen(3*(ng-1)+3)
         do n = L1cgensg(ng), L2cgensg(ng)
            LL = kcgen(3, n)
            if (hu(LL) > 0d0) then
               bup = min(bob(1, LL), bob(2, LL))
               if (kmx > 0) then
                  do L = Lbot(LL), Lbot(LL) + kmxL(LL) - 1
                     if (hu(L) > hu(L - 1)) then
                        ZLu = bup + hu(L - 1)
                        fac = (zgaten - zLu) / (hu(L) - hu(L - 1))
                        fac = max(0d0, min(1d0, fac))
                        Lnu = L
                        if (fac < 0.1d0) then
                           Lnu = L - 1
                           ! Ltop(LL) =    Lnu
                           hu(Lnu) = hu(Lnu) + fac * (hu(L) - hu(L - 1))
                           au(Lnu) = au(Lnu) + fac * au(L)
                           exit
                        else
                           hu(Lnu) = hu(Lnu - 1) + fac * (hu(Lnu) - hu(Lnu - 1))
                           au(Lnu) = fac * au(Lnu)
                        end if
                     end if
                  end do
                  au(Ltop(LL) + 1:Lbot(LL) + kmxL(LL) - 1) = 0d0 ! -12346d0 ! 6 not 5
               end if
            end if
         end do
         if (bupmin /= 9d9) then
            zcgen(3 * (ng - 1) + 2) = max(zcgen(3 * (ng - 1) + 2), bupmin)
         end if

      end do

      do n = 1, nklep ! check valves
         L = abs(Lklep(n))
         call getflowdir(L, iup)
         if (iup * Lklep(n) < 0) then
            hu(L) = 0d0; au(L) = 0d0
            if (kmx > 0) then
               hu(Lbot(L):Ltop(L)) = 0d0
               au(Lbot(L):Ltop(L)) = 0d0
            end if
         end if
      end do

      do n = 1, nvalv ! smoren
         L = Lvalv(n)
         fac = max(0d0, min(1d0, valv(n)))
         if (fac > 1d-6) then
            au(L) = fac * au(L)
         else
            hu(L) = 0d0; au(L) = 0d0
            if (kmx > 0) then
               hu(Lbot(L):Ltop(L)) = 0d0
               au(Lbot(L):Ltop(L)) = 0d0
            end if
         end if
      end do

      if (nqbnd == 0) return

      huqbnd = 0d0

      if (jbasqbnddownwindhs == 0) then
         do nq = 1, nqbnd ! discharge normalising Manning conveyance
            at = 0d0

            ssav = 0d0; wwav = 0d0
            do n = L1qbnd(nq), L2qbnd(nq)
               L = kbndu(3, n)
               k2 = kbndu(2, n)

               if (jampi == 1) then
!            exclude ghost nodes
                  if (idomain(k2) /= my_rank) then
                     cycle
                  end if
               end if

               if (hu(L) > 0d0) then
                  ssav = ssav + s1(k2) * wu(L)
                  wwav = wwav + wu(L)
               end if
            end do
            wwssav_all(1, nq) = wwav
            wwssav_all(2, nq) = ssav
         end do

         if (jampi == 1 .and. japartqbnd == 1) then
            if (jatimer == 1) call starttimer(IMPIREDUCE)
            call reduce_wwssav_all()
            if (jatimer == 1) call stoptimer(IMPIREDUCE)
         end if
      end if

      do nq = 1, nqbnd ! discharge normalising Manning conveyance
         at = 0d0

         if (jbasqbnddownwindhs == 0) then
            wwav = wwssav_all(1, nq)
            ssav = wwssav_all(2, nq)

            if (wwav > 0) then
               ssav = ssav / wwav
               do n = L1qbnd(nq), L2qbnd(nq)
                  L = kbndu(3, n)
                  if (hu(L) > 0d0) then
!                hu(L) = max(0d0, ssav - min( bob(1,L), bob(2,L) ) )
                     huqbnd(n) = max(0d0, ssav - min(bob(1, L), bob(2, L)))
                  end if
               end do
            end if
         end if

         do n = L1qbnd(nq), L2qbnd(nq)
            L = kbndu(3, n)
            k2 = kbndu(2, n)

            if (jbasqbnddownwindhs == 1) then
               hu(L) = s1(k2) - bl(k2) !  Qbnd_downwind_hs
               if (hu(L) > 0.0) then
                  left_cell = ln(1, L)
                  right_cell = ln(2, L)
                  velocity = u1(L)
                  if (velocity == 0) then
                     call get_upstream_downstream_cell_numbers(s0(left_cell) > s0(right_cell), left_cell, right_cell, &
                                                               upstream_cell_index, upstream_cell, downstream_cell, direction_sign)
                  else
                     call get_upstream_downstream_cell_numbers(velocity > 0, left_cell, right_cell, &
                                                               upstream_cell_index, upstream_cell, downstream_cell, direction_sign)
                  end if
                  call get_lkbot_set_ltop_upwind(L, upstream_cell, upstream_cell_index, Lb, kb, kt)
               end if
               call addlink2D(L, 1)
               huqbnd(n) = hu(L)
            end if

            if (zbndq(n) < 0d0 .and. hu(L) < qbndhutrs) then
               hu(L) = 0d0; au(L) = 0d0
            else
               if (jampi == 0) then
!            at = at + au(L)*hu(L)**FAC23
                  at = at + au(L) * huqbnd(n)**FAC23
               else
!            exclude ghost nodes
                  if (idomain(k2) == my_rank) then
!               at = at + au(L)*hu(L)**FAC23
                     at = at + au(L) * huqbnd(n)**FAC23
                  end if
               end if
            end if
         end do
         at_all(nq) = at
      end do

      if (jampi == 1 .and. japartqbnd == 1) then
         if (jatimer == 1) call starttimer(IMPIREDUCE)
         call reduce_at_all()
         if (jatimer == 1) call stoptimer(IMPIREDUCE)
      end if

      do nq = 1, nqbnd
         at = at_all(nq)
         if (at /= 0) then
            do n = L1qbnd(nq), L2qbnd(nq)
               L = kbndu(3, n)
!          zbndu(n) = (zbndu(n)*hu(L)**FAC23)/at
               zbndq(n) = (zbndq(n) * huqbnd(n)**FAC23) / at
            end do
         end if
      end do

   end subroutine setau

end module m_setau
