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

subroutine connecthangingnodes()
   use m_netw
   use m_flowgeom
   use m_missing
   use gridoperations
   implicit none

   integer :: mout, np, kk, k, kk3, kkx, lnu, km, kp

   call findcells(0)
   call newfil(mout, 'hang.xyz')

   lnu = numL
   do np = 1, nump
      kk3 = 0
      kkx = netcell(np)%n
      if (kkx <= 4) then
         cycle
      end if
      do kk = 1, netcell(np)%n
         k = netcell(np)%nod(kk)
         if (nmk(k) == 3) then
            km = kk - 1; if (km < 1) km = km + kkx
            kp = kk + 1; if (kp > kkx) kp = kp - kkx
            km = netcell(np)%nod(km)
            kp = netcell(np)%nod(kp)
            if (abs(yk(km) - yk(k)) < 1d-10 .and. abs(yk(kp) - yk(k)) < 1d-10 .or. &
                abs(xk(km) - xk(k)) < 1d-10 .and. abs(xk(kp) - xk(k)) < 1d-10) then
               km = kk - 2; if (km < 1) km = km + kkx
               kp = kk + 2; if (kp > kkx) kp = kp - kkx
               km = netcell(np)%nod(km)
               kp = netcell(np)%nod(kp)
               lnu = lnu + 1
               kn(1, lnu) = k; kn(2, lnu) = km; kn(3, lnu) = 2
               lnu = lnu + 1
               kn(1, lnu) = k; kn(2, lnu) = kp; kn(3, lnu) = 2
               !call connectdbn(k,km,lnu)
               !call connectdbn(k,kp,lnu)
            end if
         end if
      end do
   end do
   numL = Lnu
   call doclose(mout)
   call findcells(0)

end subroutine connecthangingnodes

subroutine removelinksofhangingnodes()
   use m_netw
   use m_flowgeom

   implicit none

   integer :: L, k1, k2

   do L = 1, numL
      k1 = kn(1, L); k2 = kn(2, L)
      if (abs(xk(k1) - xk(k2)) > 1d-10 .and. abs(yk(k1) - yk(k2)) > 1d-10) then
         kn(1, L) = 0; kn(2, L) = 0; kn(3, L) = 0
      end if
   end do

   call setnodadm(0)
end subroutine removelinksofhangingnodes

subroutine makeZKbedlevels()
   use m_netw
   use m_sferic
   use m_flow

   implicit none

   integer :: k, k1, k2, ja
   double precision :: X3, Y3, X1, Y1, X2, Y2, disn, dist, XN, YN, rl, hh, phase, bedwid2, bedrepose, gridsize

   x1 = 0d0
   y1 = 0d0
   x2 = cos(bedslopedir * dg2rd)
   y2 = sin(bedslopedir * dg2rd)
   hh = abs(zkuni)
   bedwid2 = 0.5d0 * bedwidth
   k1 = kn(1, 1); k2 = kn(2, 1)
   call dbdistancehk(xk(k1), yk(k1), xk(k2), yk(k2), gridsize)

   do k = 1, numk

      x3 = xk(k); y3 = yk(k)
      call dLINEDIS2(X3, Y3, X1, Y1, -Y2, x2, JA, dist, XN, YN, rl)
      call dLINEDIS2(X3, Y3, X1, Y1, X2, Y2, JA, disn, XN, YN, rl)

      zk(k) = zkuni + bedslope * dist ! in tangential of vector

      if (bedwavelength /= 0d0) then ! idem
         phase = twopi * dist / bedwavelength
         zk(k) = zk(k) + bedwaveamplitude * cos(phase)
      end if

      if (bedwidth > 0d0) then
         bedrepose = hh * atan(0.5d0 * pi / bedwid2)
         zk(k) = zk(k) + hh * (1d0 - cos(disn * tan(bedrepose / hh))) ! normal to vector
         if (disn > bedwid2 + 2 * gridsize) then
            xk(k) = dmiss; yk(k) = dmiss; zk(k) = dmiss
         end if
      end if

   end do

   call setnodadm(0)
end subroutine makeZKbedlevels

