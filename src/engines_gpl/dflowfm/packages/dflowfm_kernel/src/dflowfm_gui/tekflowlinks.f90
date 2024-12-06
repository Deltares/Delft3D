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

module m_tekflowlinks

   implicit none

contains

   subroutine tekflowlinks()
      use precision, only: dp
      use m_setisoscale2is1
      use m_minmxlns
      use m_isosmoothflownode2
      use m_isocol2
      use m_drcirc
      use m_dmovabs
      use m_dlnabs
      use m_dhtext
      use m_dhitext
      use m_copyzlintornod
      use m_netw, only: xk, yk
      use m_flowgeom
      use m_flow, only: hu, au
      use m_sferic
      use m_missing, only: dmiss
      use m_drawthis
      use m_halt2
      use m_three_two
      use m_inview
      use m_pfiller_core
      use m_zlin
      implicit none
      integer :: k, L, ja, k1, k2, ncol, linkmode
      real(kind=dp) :: zL
      real(kind=dp) :: xcl, ycl, zcl ! help only
      real(kind=dp) :: xx1, yy1, Zz1 ! help only
      real(kind=dp) :: xx2, yy2, Zz2 ! help only
      real(kind=dp) :: x3, y3, x4, y4 ! help only
      real(kind=dp) :: x(4), y(4), z(4), hw, cs, sn
      real :: xr(4), yr(4)

      linkmode = ndraw(11)
      if (LINKMODE > 1 .and. ndraw(29) >= 2) then ! show VALUES AT links
         if (NDRAW(7) == 1) call minmxlns() ! ONLY ADAPT VERTICAL LIMITS FOR FLOW links IF NO NET links ASKED

         call setisoscale2is1()

         if (linkmode == 3 .or. linkmode == 6) then

            call copyzlintornod()
            do k = 1, ndx2d
               if (mod(k, 200) == 0) then
                  call halt2(ja)
                  if (ja == 1) return
               end if
               if (inview(xz(k), yz(k))) then
                  call ISOSMOOTHflownode2(k)
               end if
            end do

         else
            do L = 1, lnx
               if (mod(L, 200) == 0) then
                  call halt2(ja)
                  if (ja == 1) return
               end if

               if (inview(xu(L), yu(L))) then
                  ZZ1 = 0d0 !Bob(1,L)
                  ZZ2 = 0d0 !Bob(2,L)

                  xcl = xu(L)
                  ycl = yu(L)
                  zcl = 0.5 * (ZZ1 + ZZ2)

                  zl = zlin(L)
                  if (zL == DMISS) cycle
                  call ISOCOL2(zl, NCOL)

                  k1 = ln(1, L)
                  xX1 = Xz(K1)
                  yY1 = Yz(K1)
                  k2 = ln(2, L)
                  xX2 = Xz(K2)
                  yY2 = Yz(K2)

                  if (linkmode == 3 .or. linkmode == 6) then
                     call DMOVABS(XX1, YY1, ZZ1)
                     call DLNABS(XX2, YY2, ZZ2)
                  else if (linkmode == 4 .or. linkmode == 7) then
                     if (L > Lnx1D) then ! 2D
                        k1 = lncn(1, L)
                        X3 = Xk(K1)
                        Y3 = Yk(K1)
                        k2 = lncn(2, L)
                        X4 = Xk(K2)
                        Y4 = Yk(K2)

                        call DRIETWEE(Xx1, Yy1, ZZ1, X(1), Y(1), Z(1))
                        call DRIETWEE(X3, Y3, ZZ1, X(2), Y(2), Z(2))
                        call DRIETWEE(Xx2, Yy2, ZZ2, X(3), Y(3), Z(3))
                        call DRIETWEE(X4, Y4, ZZ2, X(4), Y(4), Z(4))
                        xr = x
                        yr = y
                        call PFILLERCORE(Xr, Yr, 4)
                     else
!                     hw    = 0.25d0*( a1(k1) + a1(k2) )/dx(L)
                        if (hu(L) > 0d0) then
                           hw = 0.5d0 * Au(L) / hu(L) ! flat bed, half width
                        else
                           hw = 1d-3
                        end if
                        if (jsferic == 1) then
                           hw = hw * rd2dg / ra
                        end if

                        sn = snu(L)
                        cs = csu(L)
                        x(1) = xx1 + sn * hw
                        y(1) = yy1 - cs * hw
                        x(2) = xx2 + sn * hw
                        y(2) = yy2 - cs * hw
                        x(3) = xx2 - sn * hw
                        y(3) = yy2 + cs * hw
                        x(4) = xx1 - sn * hw
                        y(4) = yy1 + cs * hw
                        xr = x
                        yr = y
                        call PFILLERCORE(Xr, Yr, 4)
                     end if
                  else if (linkmode == 5 .or. linkmode == 8) then
                     call DRCIRC(XCL, YCL, ZCL)
                  end if
                  if (linkmode == 2 .or. linkmode == 6 .or. linkmode == 7 .or. linkmode == 8) then
                     if (NDRAW(29) == 12 .or. NDRAW(29) == 29 .or. NDRAW(29) == 33 .or. NDRAW(29) == 35 .or. NDRAW(29) == 36) then
                        call DHITEXT(int(zl), xCL, yCL)
                     else
                        call dhtext(zl, xCL, yCL, zCL)
                     end if
                  end if

               end if
            end do
         end if ! linkmode
      end if ! ndraw(29)
   end subroutine tekflowlinks

end module m_tekflowlinks
