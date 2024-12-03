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

module m_teknodevals

   implicit none

contains

   subroutine TEKNODEVALS(MET)
      use precision, only: dp
      use m_isosmoothnet
      use m_isocol
      use m_drcirc
      use m_dmovabs
      use m_dlnabs
      use m_missing
      use m_netw
      use geometry_module, only: getdxdy, getdx, getdy
      use m_sferic, only: jsferic
      use unstruc_colors ! , ONLY :NCOLWARN1, ncolhl
      use gridoperations
      use m_depmax
      use m_howtoview
      use m_halt2
      use m_three_two
      use m_cirr
      use m_pfiller
      use m_inview
      use m_getrcir
      use m_invnod

      implicit none
      integer :: MET
      real(kind=dp) :: d
      integer :: k1, k
      integer :: k2
      integer :: key
      integer :: l
      integer :: n
      integer :: ncol
      real(kind=dp) :: rd
      real(kind=dp) :: vv
      real(kind=dp) XD, YD, ZD, DX, DY, DZ, XX1, YY1, ZZ1, XX2, YY2, ZZ2, H
      real(kind=dp) :: X(4), Y(4), Z(4)

      KMOD = max(1, NUMK / 100)

      H = 0.5d0

      if (met == 3) then ! smooth iso of netnode stuff based upon netcells

         if (numk + numl /= lasttopology) then ! coarsening info
            if (ubound(lnn, 1) /= numL) then
               call findcells(100)
            end if
         end if

         do k = 1, nump
            if (mod(K, KMOD) == 0) then
               call HALT2(KEY)
               if (KEY == 1) then
                  return
               end if
            end if
            if (inview(xzw(k), yzw(k))) then
               call isosmoothnet(k)
            end if
         end do

      else if (MET > 3) then
         D = 0.5d0 * GETRCIR() !
         do K1 = 1, NUMK
            if (mod(K1, KMOD) == 0) then
               call HALT2(KEY)
               if (KEY == 1) then
                  return
               end if
            end if
            if (.not. INVNOD(K1)) cycle
            VV = RNOD(K1)
            XX1 = XK(K1)
            YY1 = YK(K1)
            ZZ1 = ZK(K1)
            if (VV /= dmiss) then
               call ISOCOL(VV, NCOL)
               if (MET == 3 .or. MET == 4 .or. &
                   MET == 6 .or. MET == 7) then
                  do N = 1, NMK(K1)
                     L = NOD(K1)%LIN(N)
                     call OTHERNODE(K1, L, K2)
                     if (K2 == 0) then
                        cycle
                     end if

                     XX2 = H * (XK(K2) + XX1)
                     YY2 = H * (YK(K2) + YY1)
                     ZZ2 = H * (ZK(K2) + ZZ1)
                     if (MET == 6) then
                        call DMOVABS(XX1, YY1, ZZ1)
                        call DLNABS(XX2, YY2, ZZ2)
                     else if (MET == 4 .or. MET == 7) then
                        ! XD = getdx (XX1, yy1, xx2, yy2)
                        ! YD = getdy (XX1, yy1, xx2, yy2)
                        call getdxdy(XX1, yy1, xx2, yy2, xd, yd, jsferic)
                        RD = sqrt(XD * XD + YD * YD)
                        if (RD /= 0) then
                           if (JVIEW == 1 .or. JVIEW == 4) then
                              DX = -D * YD / RD
                              DY = D * XD / RD
                              DZ = 0
                           else if (JVIEW == 2) then
                              DZ = -D * YD / RD
                              DY = D * ZD / RD
                              DX = 0
                           else if (JVIEW == 3) then
                              DX = -D * ZD / RD
                              DZ = D * XD / RD
                              DY = 0
                           end if
                           call DRIETWEE(XX2 + DX, YY2 + DY, ZZ2 + DZ, X(1), Y(1), Z(1))
                           call DRIETWEE(XX1 + DX, YY1 + DY, ZZ1 + DZ, X(2), Y(2), Z(2))
                           call DRIETWEE(XX1 - DX, YY1 - DY, ZZ1 - DZ, X(3), Y(3), Z(3))
                           call DRIETWEE(XX2 - DX, YY2 - DY, ZZ2 - DZ, X(4), Y(4), Z(4))
                           call pfiller(X, Y, 4, ncol, ncol)
                           !CALL IGRJOIN(real(x(1)),real(y(1)),real(x(2)),real(y(2)))
                        end if
                     end if
                  end do
               else if (MET == 5 .or. MET == 8) then
                  call DRCIRC(XX1, YY1, ZZ1)
               else if (MET == 9) then
                  if (VV /= dmiss .and. VV < vmin + 0.05d0 * (vmax - vmin)) then
                     call CIRR(Xx1, Yy1, ncolhl)
                  end if
               else if (MET == 10) then
                  if (VV /= dmiss .and. VV > vmax - 0.05d0 * (vmax - vmin)) then
                     call CIRR(Xx1, Yy1, ncolhl)
                  end if
               end if
            else
               call CIRR(XX1, YY1, NCOLWARN1)
            end if
         end do
      end if

      return
   end subroutine TEKNODEVALS

end module m_teknodevals
