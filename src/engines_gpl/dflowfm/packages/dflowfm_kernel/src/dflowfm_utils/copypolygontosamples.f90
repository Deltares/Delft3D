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

   subroutine COPYPOLYGONTOSAMPLES()
      use M_SAMPLES
      use M_POLYGON
      use m_missing
      use M_NETW, only: UNIDX1D
      use m_fixedweirs, only: SILLHEIGHTMIN
      use geometry_module, only: dbdistance, normalout
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: k, n, KU, KUU, KKN, KK
      double precision :: RX1, RY1, RX2, RY2, V, R, A, B, DL, DR, WIDL, WIDR

!  interpolate missing zpl values in polylines, if possible
      call interpolate_zpl_in_polylines()

      N = NS
      call INCREASESAM(NS + NPL)
      do K = 1, NPL - 1
         KU = K + 1; KUU = min(NPL, K + 2)
         if (XPL(K) /= DMISS .and. XPL(KU) /= DMISS) then

            if (jakol45 == 0) then
               N = N + 1
               if (N > NSMAX) then
                  call INCREASESAM(2 * N)
               end if
               XS(N) = XPL(K)
               YS(N) = YPL(K)
               ZS(N) = ZPL(K); if (ZS(N) == DMISS) ZS(N) = 1d0
            end if

            if (JAKOL45 > 0 .and. ZPL(K) /= DMISS) then
               if (.not. (XPL(K) == XPL(KU) .and. YPL(K) == YPL(KU))) then
                  call normalout(XPL(K), YPL(K), XPL(KU), YPL(KU), rx1, ry1, jsferic, jasfer3D, dmiss, dxymis)
                  RX2 = RX1; RY2 = RY1
                  if (K > 1) then
                     if (XPL(K - 1) /= DMISS) then
                        call normalout(XPL(K - 1), YPL(K - 1), XPL(K), YPL(K), rx2, ry2, jsferic, jasfer3D, dmiss, dxymis)
                        RX2 = 0.5d0 * (RX1 + RX2)
                        RY2 = 0.5d0 * (RY1 + RY2)
                     end if
                  end if

                  N = N + 1
                  if (N > NSMAX) then
                     call INCREASESAM(2 * N)
                  end if

                  WIDL = 0.1d0
                  WIDR = 0.1d0
                  if (DZR(K) > Sillheightmin .and. DZL(K) > Sillheightmin) then
                     WIDL = 2d0 * DZL(K)
                     WIDR = 2d0 * DZR(K)
                  end if

                  XS(N) = XPL(K) - RX2 * WIDL
                  YS(N) = YPL(K) - RY2 * WIDL
                  ZS(N) = ZPL(K) - DZL(K)
                  N = N + 1
                  XS(N) = XPL(K) + RX2 * WIDR
                  YS(N) = YPL(K) + RY2 * WIDR
                  ZS(N) = ZPL(K) - DZR(K)
               end if
            end if

            V = DBDISTANCE(XPL(K), YPL(K), XPL(KU), YPL(KU), jsferic, jasfer3D, dmiss)
            if (V > 0d0 .and. UNIDX1D > 0) then
               R = V / UNIDX1D
               if (R > 1d0) then
                  KKN = R + 1
                  do KK = 1, KKN - 1
                     A = dble(KK) / dble(KKN); B = 1d0 - A

                     if (jakol45 == 0) then
                        N = N + 1
                        if (N > NSMAX) then
                           call INCREASESAM(2 * N)
                        end if
                        XS(N) = B * XPL(K) + A * XPL(KU)
                        YS(N) = B * YPL(K) + A * YPL(KU)
                        ZS(N) = B * ZPL(K) + A * ZPL(KU)
                        if (ZPL(K) == DMISS .or. ZPL(KU) == DMISS) then
                           ZS(N) = 1d0
                        end if
                     end if

                     if (JAKOL45 > 0 .and. ZPL(K) /= DMISS .and. ZPL(KU) /= DMISS) then

                        WIDL = 0.1d0
                        WIDR = 0.1d0
                        DL = B * DZL(K) + A * DZL(KU)
                        DR = B * DZR(K) + A * DZR(KU)
                        if (DL > Sillheightmin .and. DR > Sillheightmin) then ! slope assumed
                           WIDL = 2d0 * DL
                           WIDR = 2d0 * DR
                        end if

                        N = N + 1
                        if (N > NSMAX) then
                           call INCREASESAM(2 * N)
                        end if

                        XS(N) = B * XPL(K) + A * XPL(KU) - RX1 * WIDL
                        YS(N) = B * YPL(K) + A * YPL(KU) - RY1 * WIDL
                        ZS(N) = B * ZPL(K) + A * ZPL(KU)
                        ZS(N) = ZS(N) - DL

                        N = N + 1
                        if (N > NSMAX) then
                           call INCREASESAM(2 * N)
                        end if

                        XS(N) = B * XPL(K) + A * XPL(KU) + RX1 * WIDR
                        YS(N) = B * YPL(K) + A * YPL(KU) + RY1 * WIDR
                        ZS(N) = B * ZPL(K) + A * ZPL(KU)
                        ZS(N) = ZS(N) - DR
                     end if

                  end do
               end if
            end if
            if (XPL(KUU) == DMISS .or. KU == NPL) then

               if (jakol45 == 0) then
                  N = N + 1
                  if (N > NSMAX) then
                     call INCREASESAM(2 * N)
                  end if
                  XS(N) = XPL(KU)
                  YS(N) = YPL(KU)
                  ZS(N) = ZPL(KU); if (ZS(N) == DMISS) ZS(N) = 1d0
               end if

               if (JAKOL45 > 0 .and. ZPL(KU) /= DMISS) then

                  WIDL = 0.1d0
                  WIDR = 0.1d0
                  DL = DZL(KU)
                  DR = DZR(KU)
                  if (DL > Sillheightmin .and. DR > Sillheightmin) then
                     WIDL = 2d0 * DL
                     WIDR = 2d0 * DR
                  end if

                  N = N + 1
                  XS(N) = XPL(KU) - RX1 * WIDL
                  YS(N) = YPL(KU) - RY1 * WIDL
                  ZS(N) = ZPL(KU) - DZL(KU)
                  N = N + 1
                  XS(N) = XPL(KU) + RX1 * WIDR
                  YS(N) = YPL(KU) + RY1 * WIDR
                  ZS(N) = ZPL(KU) - DZR(KU)
               end if

            end if

         end if
      end do

      NS = N
      call delpol()
   end subroutine COPYPOLYGONTOSAMPLES
