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

module m_netlinkvals

   implicit none

contains

   subroutine NETLINKVALS(MET)
      use precision, only: dp

      use m_makepdf
      use m_dhitext
      use m_flowgeom, only: XZ, YZ, lne2ln
      use m_missing, only: dmiss, dxymis
      use network_data
      use m_alloc
      use m_flow, only: cftrt
      use geometry_module, only: dbdistance, dcosphi
      use m_sferic, only: jsferic, jasfer3D
      use gridoperations
      use m_statistics
      use m_depmax
      use m_cconstants
      use m_makenetnodescoding
      use m_find1dcells, only: find1dcells

      integer :: MET
      integer :: jacftrt
      real(kind=dp) :: fsp
      integer :: k1, k2, L, jaxz, kL, kR
      integer :: i
      real(kind=dp) :: rd
      real(kind=dp) :: rek
      real(kind=dp) :: sp
      real(kind=dp) :: v
      real(kind=dp) :: X3, Y3, X4, Y4
      real(kind=dp) :: xd, YD, ZD
      real(kind=dp) :: areaL, areaR, xc, yc, aa
      real(kind=dp), external :: topo_info

      if (MET == 1) return

      jaxz = 0
      if (allocated(xz)) then
         jaxz = 1
      end if

      jacftrt = 0
      if (allocated(cftrt)) then
         jacftrt = 1
      end if

! refresh netcell administartion, based on module variable netstat
      if (netstat /= NETSTAT_OK .and. (met == 4 .or. met == 5 .or. (met >= 7 .and. met <= 9) .or. met >= 12)) then
         call findcells(100)
         call find1dcells()
         netstat = NETSTAT_OK
      end if

      if (MET == 15 .or. MET == 16) then ! topology information
         call makenetnodescoding()
      end if

      if (numL > size(rlin)) then
         call realloc(rlin, numL)
      end if

      do L = 1, NUML
         V = dmiss
         K1 = KN(1, L)
         K2 = KN(2, L)
         if (K1 /= 0 .and. K2 /= 0) then
            if (MET == 2) then
               V = L
            else if (MET == 3) then
               call DHITEXT(K1, XK(K1), YK(K1))
               call DHITEXT(K2, XK(K2), YK(K2))
            else if (MET == 4) then
               if (NUMP > 0 .and. jaxz == 1 .and. L <= size(LNN)) then
                  if (LNN(L) == 2) then
                     X3 = XZ(abs(LNE(1, L)))
                     X4 = XZ(abs(LNE(2, L)))
                     Y3 = YZ(abs(LNE(1, L)))
                     Y4 = YZ(abs(LNE(2, L)))
                     V = DCOSPHI(XK(K1), YK(K1), XK(K2), YK(K2), X3, Y3, X4, Y4, jsferic, jasfer3D, dxymis)
                     if (v /= dmiss) then
                        v = abs(v)
                     end if
                  end if
               end if
            else if (MET == 5) then
               if (size(lne2ln) >= numl) then
                  V = lne2ln(L)
               else
                  v = 0
               end if
            else if (MET == 6) then
               V = LC(L)
            else if (MET == 7) then
               if (lc(L) > 0) V = netbr(LC(L))%nx
            else if (MET >= 7 .and. MET <= 9) then
               XD = XK(K2) - XK(K1)
               YD = YK(K2) - YK(K1)
               ZD = ZK(K2) - ZK(K1)
               RD = sqrt(XD * XD + YD * YD + ZD * ZD)
               REK = 0 ! ( RD - RL(L) ) / RL(L)
               if (LC(L) == 0) REK = max(0d0, REK)
               SP = E0 * REK
               FSP = 0 ! SP*EA(L)/1e3  ! spanning in kN
               if (MET == 9) then
                  V = 0d0 ! TODO: AvD: Mag weg
               end if
            else if (MET == 10) then
               V = DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
            else if (MET == 11) then
               V = KN(3, L)
            else if (MET == 12) then
               V = 0
               if (L <= size(LNN)) V = LNN(L)
            else if (MET == 13) then
               V = 0
               if (L <= size(LNN)) V = LNE(1, L)
            else if (MET == 14) then
               V = 0
               if (L <= size(LNN)) V = LNE(2, L)
            else if (MET == 15) then ! topology information
               V = topo_info(L)
            else if (MET == 16) then ! area ratio
               if (lnn(L) < 2) then
                  V = dmiss
               else
                  kL = lne(1, L)
                  kR = lne(2, L)
                  call getcellsurface(kL, areaL, xc, yc)
                  call getcellsurface(kR, areaR, xc, yc)
                  if (areaL < 1d-12 .or. areaR < 1d-12) cycle
                  V = areaR / areaL
                  if (V < 1d0) V = 1d0 / V
               end if
            else if (MET == 17) then ! link size criterion
               if (lnn(L) < 2) then
                  V = dmiss
               else
                  kL = lne(1, L)
                  kR = lne(2, L)
                  call getcellsurface(kL, areaL, xc, yc)
                  call getcellsurface(kR, areaR, xc, yc)
                  if (areaL < 1d-12 .or. areaR < 1d-12) cycle
                  k1 = kn(1, L)
                  k2 = kn(2, L)
                  aa = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss) * dbdistance(xz(kL), yz(kL), xz(kR), yz(kR), jsferic, jasfer3D, dmiss)
                  V = aa / (areaR + areaL)
               end if
            else if (MET == 18) then
               if (jacftrt == 1) then
                  V = cftrt(L, 2)
               else
                  V = DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
                  if (v > 0) then
                     V = (zk(k2) - zk(k1)) / v
                  end if
               end if
            end if
            RLIN(L) = V
         end if
      end do

      if (met == 4) then
         npdf = 20
         if (allocated(xpdf)) deallocate (xpdf, ypdf); allocate (xpdf(npdf), ypdf(npdf)); xpdf = 0d0
         aa = 1d0
         do i = 1, npdf - 1
            aa = 0.6666d0 * aa
            ypdf(i) = aa
         end do
         ypdf(npdf) = 0d0
         call makepdf(rlin, numL)
      end if

      return
   end subroutine NETLINKVALS

end module m_netlinkvals
