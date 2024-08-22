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

 subroutine setgrwflowexpl() ! groundwater flow explicit
    use m_flowgeom
    use m_flow
    use m_flowtimes
    use m_hydrology_data
    use horton
    implicit none
    double precision, parameter :: mmphr_to_mps = 1d-3 / 3600d0

    integer :: k1, k2, L, k
    integer :: ierr
    double precision :: z1, z2, h1, h2, dh, dQ, hunsat, hunsat1, hunsat2, fac, qgrw, h2Q
    double precision :: fc, conduct, h_upw, Qmx

    qingrw = 0d0; qoutgrw = 0d0; Volgrw = 0d0

    if (infiltrationmodel == DFM_HYD_INFILT_HORTON) then ! Horton's infiltration equation
       ierr = infiltration_horton_formula(ndx, HortonMinInfCap, HortonMaxInfCap, HortonDecreaseRate, HortonRecoveryRate, infiltcap0, infiltcap, &
                                          dts, hs, rain, jarain, HortonState)
       infiltcap = infiltcap * mmphr_to_mps
    end if

    if (infiltrationmodel == 1) then ! orig. interceptionmodel: no horizontal groundwater flow, and infiltration is instantaneous as long as it fits in unsat zone (called 'interception' here, but naming to be discussed)

       do k = 1, ndx2D
          hunsat = bl(k) - sgrw1(k)
          if (qin(k) > 0d0 .and. hunsat > 0d0) then
             h2Q = ba(k) / dts
             dh = min(hunsat, qin(k) / h2Q)
             dQ = dh * h2Q
             sgrw1(k) = sgrw1(k) + dh ! results in increase of sgrw
             qoutgrw = qoutgrw + dQ
             qin(k) = qin(k) - dQ
          end if
       end do
       return

    else if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) &
             .and. jagrw == 0) then ! spatially varying prescribed max infiltration capacity
       do k = 1, ndx2D
          Qmx = max(0d0, vol1(k) / dts + qin(k))
          infilt(k) = infiltcap(k) * ba(k) ! Prescribed infiltration flux m3/s
          infilt(k) = min(Qmx, infilt(k))
          qin(k) = qin(k) - infilt(k)
          qoutgrw = qoutgrw + infilt(k)
       end do
       return

    else

       sgrw0 = sgrw1

       do L = lnx1D + 1, Lnxi ! Horizontal Darcy flow in 2D only:

          !if (L =< Lnx1D) then
          !   if (kcu(L) == 5) cycle                                       ! pipe type 1D2D connection
          !   if ( prof1D(1,L) > 0 .and. prof1D(3,L) == 1) cycle           ! pipe profile
          !endif

          k1 = ln(1, L); k2 = ln(2, L)
          hunsat1 = bl(k1) - sgrw0(k1)
          fac = min(1d0, max(0d0, hunsat1 / h_transfer)) ! 0 at bed, 1 at sgrw
          z1 = sgrw1(k1) * fac + s1(k1) * (1d0 - fac)
          pgrw(k1) = z1

          hunsat2 = bl(k2) - sgrw0(k2)
          fac = min(1d0, max(0d0, hunsat2 / h_transfer))
          z2 = sgrw1(k2) * fac + s1(k2) * (1d0 - fac)
          pgrw(k2) = z2

          h1 = sgrw1(k1) - bgrw(k1)
          h2 = sgrw1(k2) - bgrw(k2)
          h_upw = max(0d0, min(h1, h2)) ! safe, not upw

          Qgrw = Conductivity * h_upw * wu(L) * (z1 - z2) * dxi(L) * dts ! (m3/s)*s
          sgrw1(k1) = sgrw1(k1) - Qgrw * bai(k1) / porosgrw
          sgrw1(k2) = sgrw1(k2) + Qgrw * bai(k2) / porosgrw
       end do

       do k = 1, ndx2D
          hunsat = bl(k) - sgrw1(k)
          h2Q = porosgrw * ba(k) / dts
          if (hunsat <= 0) then ! groundwater above bed => transfer to open water
             sgrw1(k) = sgrw1(k) + hunsat ! decrease sgrw to bedlevel
             qin(k) = qin(k) - hunsat * h2Q ! results in positive qin
             qingrw = qingrw - hunsat * h2Q
          else ! groundwater below bed => seepage from open water
             Qmx = vol1(k) / dts + qin(k)
             fac = min(1d0, max(0d0, (hunsat / h_transfer))) ! 0 at bed, 1 at sgrw
             if (infiltrationmodel == DFM_HYD_INFILT_CONST) then
                Qgrw = Infiltcap(k) * ba(k) ! Prescribed infiltration velocity m3/s
             else if (infiltrationmodel == DFM_HYD_INFILT_DARCY) then
                fc = min(1d0, max(0d0, (sgrw1(k) + h_capillair - bl(k)) / h_capillair))
                Conduct = Conductivity * (fc + unsatfac * (1 - fc)) ! lineair weight sat - unsat over capillair zone
                Qgrw = Conduct * (sgrw1(k) - bgrw(k)) * (s1(k) - bl(k)) ! Darcy in vertical m3/s
             end if
             Qgrw = min(Qmx, Qgrw)
             qin(k) = qin(k) - Qgrw ! negative qin
             qoutgrw = qoutgrw + Qgrw
             sgrw1(k) = sgrw1(k) + Qgrw / h2Q ! results in increase of sgrw
          end if
          Volgrw = Volgrw + porosgrw * ba(k) * (sgrw1(k) - bgrw(k))
       end do

       do L = lnxi + 1, lnx ! copy bnd values only if open surface water
          if (hu(L) > 0) then
             k = ln(1, L)
             sgrw1(k) = s1(k)
             pgrw(k) = s1(k)
          end if
       end do

    end if

 end subroutine setgrwflowexpl
