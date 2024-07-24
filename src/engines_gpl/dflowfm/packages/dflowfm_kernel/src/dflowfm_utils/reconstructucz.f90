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

subroutine reconstructucz(k)
   ! Perot reconstruction of the vertical velocity, by Willem
   use m_flow
   use m_flowgeom
   use m_flowtimes
   implicit none

   integer, intent(in) :: k

   integer k1, k2 ! flow node counters (ndx)
   integer kk, kk1, kk2 ! flow node counter (ndkx)
   integer kb, kb1, kb2 ! bottom level flow node counters
   integer kt, kt1, kt2 ! top level flow node counters
   integer ko ! flow nodeat other side of flow link
   integer L, Lidx ! flow link counter (2D)
   integer LL ! flow link counter (3D)
   integer Lb, Lt ! counter of bottom/top level link
   integer Ls ! flow link counter (<0 indicates the flow link starts at the current flow node;
   !      >0 indicates the flow link ends at the current flow node  )
   integer ndlnx ! number of neighbouring flow links

   double precision dx1, dx2, dz1, dz2
   double precision dzL, dzL1, dzL2
   double precision zlc, zlc1, zlc2
   double precision zlu, dzhu
   double precision Lsign, Lsign1, Lsign2
   double precision wsigma1, wsigma2 ! interface velocity at lower and upper interface
   !
   if (k == 0) then
      !
      ! reconstruct ucz for whole domain
      !
      !initialize for flow nodes
      do k1 = 1, ndxi
         call getkbotktop(k1, kb, kt)
         dzL = zws(kb) - bl(k1); 
         wsigma2 = (zws(kb) - zws0(kb)) / dts
         ucz(kb) = (ww1(kb) + wsigma2) * 0.5d0 * dzL * ba(k1) ! ww1 at bed level = 0
         do kk = kb + 1, kt
            !
            dzL = zws(kk) - zws(kk - 1); 
            wsigma1 = (zws(kk - 1) - zws0(kk - 1)) / dts
            wsigma2 = (zws(kk) - zws0(kk)) / dts
            ucz(kk) = +(ww1(kk - 1) + wsigma1) * 0.5d0 * dzL * ba(k1) ! add velocity at surface level (kk-1)
            ucz(kk) = ucz(kk) + (ww1(kk) + wsigma2) * 0.5d0 * dzL * ba(k1) ! add velocity at surface level (kk)
            !
         end do
      end do
      !loop over flow links
      do L = 1, Lnx
         k1 = LN(1, L)
         k2 = LN(2, L)
         call getkbotktop(k1, kb1, kt1)
         call getkbotktop(k2, kb2, kt2)
         Lb = Lbot(L); 
         Lt = Ltop(L); 
         zLc1 = bl(k1)
         zLc2 = bl(k2)
         kk1 = kb1
         kk2 = kb2
         Lsign1 = 1d0
         Lsign2 = -1d0
         do LL = Lb, Lt
            zlu = min(bob(1, L), bob(2, L)) + hu(LL) * 0.5d0 + hu(LL - 1) * 0.5d0 ! update flow link elevation
            !
            ! update first flow node
            !
            dzL1 = zws(kk1) - zws(kk1 - 1)
            zlc1 = zlc1 + 0.5d0 * dzL1
            dz1 = zlu - zlc1
            ucz(kk1) = ucz(kk1) + Lsign1 * u1(LL) * wu(L) * (hu(LL) - hu(LL - 1)) * dz1
            !
            zlc1 = zlc1 + 0.5d0 * dzL1
            !
            ! update second flow node
            !
            dzL2 = zws(kk2) - zws(kk2 - 1)
            zlc2 = zlc2 + 0.5d0 * dzL2
            dz2 = zlu - zlc2
            ucz(kk2) = ucz(kk2) + Lsign2 * u1(LL) * wu(L) * (hu(LL) - hu(LL - 1)) * dz2
            !
            zlc2 = zlc2 + 0.5d0 * dzL2
            !
            ! update vertical counters
            !
            kk1 = kk1 + 1
            kk2 = kk2 + 1
         end do
      end do
      !finalize for flow nodes
      do k1 = 1, ndxi
         call getkbotktop(k1, kb, kt)
         do kk = kb, kt
            if (vol1(kk) > 0d0) then
               ucz(kk) = ucz(kk) / vol1(kk) ! divide by volume
            end if
         end do
      end do
   else
      !
      ! reconstruct ucz for single flow node
      !
      call getkbotktop(k, kb, kt)
      !ucz(kb) = 0.0d0
      dzL = zws(kb) - zws(kb - 1)
      wsigma2 = (zws(kb) - zws0(kb)) / dts
      ucz(kb) = (ww1(kb) + wsigma2) * 0.5d0 * dzL * ba(k) ! ww1 at bed level = 0
      do kk = kb + 1, kt
         !
         dzL = zws(kk) - zws(kk - 1); 
         wsigma1 = (zws(kk - 1) - zws0(kk - 1)) / dts
         wsigma2 = (zws(kk) - zws0(kk)) / dts
         ucz(kk) = +(ww1(kk - 1) + wsigma1) * 0.5d0 * dzL * ba(k) ! add velocity at surface level (kk-1)
         ucz(kk) = ucz(kk) + (ww1(kk) + wsigma2) * 0.5d0 * dzL * ba(k) ! add velocity at surface level (kk)
         !
      end do
      !get neighbouring flow links
      ndlnx = nd(k)%lnx ! number of flowlinks associated with current flow node.
      do Lidx = 1, ndlnx
         Ls = nd(k)%ln(Lidx) ! link number including its direction (<0 indicates the flow link starts at the current flow node;
         !                                      >0 indicates the flow link ends at the current flow node   )
         L = abs(nd(k)%ln(Lidx)) ! link numbers
         if (L == Ls) then ! determine horizontal distance dx1 from flow link to u point
            Lsign = -1d0 ! Lsign (velocity directed towards current flow node)
            dx1 = dx(L) * (1d0 - acl(L)) !
            dx2 = acl(L) * dx(L) !
            ko = LN(1, L) ! outside associated flow node number
         else
            Lsign = 1d0 ! Lsign (velocity directed away from current flow node)
            dx1 = dx(L) * acl(L) !
            dx2 = dx(L) * (1d0 - acl(L)) !
            ko = LN(2, L) ! outside flow node number
         end if

         !k2 = LN(2,L)                 ! second associated flow node number
         Lb = Lbot(L) ! --
         Lt = Ltop(L)
         kk = kb
         zlc = bl(k) ! bed level in flow node
         dzhu = hu(Lb) ! height of the bottom layer at flow link
         do LL = Lb, Lt ! get link numbers in the vertical
            dzL = zws(kk) - zws(kk - 1); 
            zlc = zlc + 0.5d0 * dzL
            !zlu = dx2*bl(k)*dxi(L) + dx1*bl(ko)*dxi(L) + hu(LL)*0.5+hu(LL-1)*0.5  (interpolated bed level at u-point)
            zlu = min(bob(1, L), bob(2, L)) + hu(LL) * 0.5d0 + hu(LL - 1) * 0.5d0
            !
            ! u_vertical,new  =  u_vertical,old + Lsign*u_horizontal*width*depth_layer
            !
            dz1 = zlu - zlc
            ucz(kk) = ucz(kk) + Lsign * u1(LL) * wu(L) * (hu(LL) - hu(LL - 1)) * dz1
            !
            zlc = zlc + 0.5d0 * dzL
            kk = kk + 1 ! update vertical counter
            !
         end do
      end do
      do kk = kb, kt
         if (vol1(kk) > 0) then
            ucz(kk) = ucz(kk) / vol1(kk) ! divide by volume
         end if
      end do

   end if

end subroutine reconstructucz
