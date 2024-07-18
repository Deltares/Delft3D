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

   subroutine fm_adjust_bedload(sbn, sbt, avalan, slopecor)
      use m_physcoef, only: ag
      use m_sferic, only: pi
      use m_flowgeom, only: lnxi, kcs, ba, bl, Dx, wu_mor
      use m_flow, only: hu
      use m_flowtimes
      use m_turbulence, only: rhou
      use precision
      use m_fm_erosed, only: stmpar, has_bedload, alfabn, alfabs, ashld, bshld, cshld, dm, dshld, e_dzdn, e_dzdt, alfpa, avaltime, duneavalan, fixfac
      use m_fm_erosed, only: frac, hidexp, islope, morfac, rhosol, sedd50fld, sedd50, taurat, tratyp, ust2, wetslope, lsedtot, thcrpa
      use m_fm_erosed, only: lnx => lnx_mor
      use m_fm_erosed, only: ln => ln_mor
      use m_sediment, only: bermslopeindexbed, bermslopeindexsus
      use m_alloc

      implicit none

   !!
   !! Global variables
   !!
      logical, intent(in) :: avalan !<  do only once for avalanching fluxes
      logical, intent(in) :: slopecor !<  do only on bedload, cf UNST-7286
      real(fp), dimension(1:lnx, 1:lsedtot), intent(inout) :: sbn !<  sbcuu, sbwuu, or sswuu
      real(fp), dimension(1:lnx, 1:lsedtot), intent(inout) :: sbt !<  sbcvv, sbwvv, or sswvv
      real(fp), dimension(:), allocatable :: sbncor !<  corrected values
      real(fp), dimension(:), allocatable :: sbtcor
   !!
   !! Local variables
   !!
      logical :: di50spatial
      integer :: l, Lf, k1, k2, lb, lt

      double precision :: di50, phi, tphi, sbedm, depth, dzdp, dzds, bagnol, alfas
      double precision :: delta, dmloc, ftheta, hidexploc, shield, sina, cosa, tnorm, frc, fixf
      double precision :: sbedn, sbedt, tratio, sbedcorr, fnorm, ust2avg, slp, avflux
      double precision :: eps = 1.0d-6
      !
   !! executable statements -------------------------------------------------------
      !
      ! n: normal to cell face, t: along cell face
      call realloc(sbncor, lnx) ! corrected values
      call realloc(sbtcor, lnx)

      !
      ! Make assumptions for friction angle
      !
      phi = 30d0 / 180d0 * pi
      tphi = tan(phi)

      do Lf = 1, Lnx
         ! for cutcell
         if (wu_mor(Lf) == 0d0) cycle
         !
         ! no bed slope effects on links with bermslope adjustments
         ! fixfac and frac applied in bermslopenudging()
         if (stmpar%morpar%bermslopetransport) then
            if (bermslopeindexbed(Lf) .or. bermslopeindexsus(Lf)) cycle
         end if
         !
         if (hu(Lf) > 0d0) then
            k1 = ln(1, Lf)
            k2 = ln(2, Lf)
            call getLbotLtop(Lf, Lb, Lt)
            if (Lt < Lb) cycle
            do l = 1, lsedtot
               if (has_bedload(tratyp(l))) then
                  di50 = sedd50(l)
                  di50spatial = .false.
                  if (di50 < 0.0_fp .and. lsedtot == 1) di50spatial = .true.
                  !
                  ! Initialize variables
                  !
                  sbedcorr = 0d0
                  !
                  ! calculate bed gradient parallel and perpendicular to BED LOAD
                  ! TRANSPORT vector. This exists in the links: e_dzdn, e_dzdt.
                  ! Transports also exist in the links: e_sbcn, e_sbct, and so on.
                  !
                  sbedn = sbn(Lf, l) ! e_sxxn
                  sbedt = sbt(Lf, l) ! e_sxxt
                  depth = hu(Lf)
                  sbedm = sqrt(sbn(Lf, l)**2 + sbt(Lf, l)**2)
                  sbncor(Lf) = sbedn
                  sbtcor(Lf) = sbedt

                  if (sbedm > eps .and. slopecor) then
                     dzds = e_dzdn(Lf) * sbedn / sbedm + e_dzdt(Lf) * sbedt / sbedm ! in direction of transport (not n)
                     dzdp = -e_dzdn(Lf) * sbedt / sbedm + e_dzdt(Lf) * sbedn / sbedm ! perpendicular to transport direction (not t)
                     !
                     ! limit dzdn to 90% of phi
                     !
                     dzds = min(0.9 * tphi, dzds)
                     !
                     ! Apply bed slope effect according to
                     !   1: No correction
                     !   2: Bagnold (long. slope) and Ikeda / Van Rijn (transv. slope)
                     !   3: Van Bendegom and Koch & Flokstra
                     !   4: Parker and Andrews
                     !
                     select case (islope)
                     case (1)
                        !
                        ! no correction: default values
                        !
                     case (2)
                        !
                        ! adjust bed load for longitudinal bed slope (following Bagnold (1956))
                        ! note alfabs is user-specified scaling parameter
                        !
                        bagnol = tphi / (cos(atan(dzds)) * (tphi - dzds)) ! alternative: tphi * sqrt(1d0 + dzdn*dzdn) / (tphi-dzdn)
                        alfas = 1.0_fp + alfabs * (bagnol - 1.0_fp)
                        alfas = max(0.0_fp, alfas)
                        sbedn = alfas * sbedn
                        sbedt = alfas * sbedt
                        !
                        ! adjust bed load for transverse bed slope
                        ! note alfabn is user-specified scaling parameter
                        ! note taurat=(taubcw/taucrb) stored above
                        !
                        tratio = (taurat(k1, l) + taurat(k2, l)) / 2.0_fp
                        if (tratio >= 1.0) then
                           fnorm = alfabn * (1.0 / tratio)**0.5 * dzdp
                        else
                           fnorm = alfabn * dzdp
                        end if
                        !
                        ! note adjusted bedload put in temporary array so doesn't influence
                        ! surrounding points
                        !
                        sbncor(Lf) = sbedn - sbedt * fnorm ! to check
                        sbtcor(Lf) = sbedt + sbedn * fnorm
                     case (3, 4)
                        !                   !
                        ! 3: Formulation according Van Bendegom (1947), Koch & Flokstra (1980)
                        ! as described in Struiksma et al. (1985)
                        !
                        ! 4: Formulation according Parker & Andrews (1985)
                        !
                        ust2avg = (ust2(k1) + ust2(k2)) / 2d0
                        if (di50spatial) then
                           di50 = sqrt(sedd50fld(k1) * sedd50fld(k2))
                        end if
                        delta = (rhosol(l) - rhou(lb)) / rhou(lb)
                        shield = ust2avg / ag / delta / di50
                        !
                        if (shield /= 0.0_fp) then
                           if (islope == 3) then
                              dmloc = sqrt(dm(k1) * dm(k2))
                              if (comparereal(dmloc, 0d0) == 0) then
                                 if (kcs(k1) > 0) then
                                    dmloc = dm(k1)
                                 elseif (kcs(k2) > 0) then
                                    dmloc = dm(k2)
                                 end if
                              end if
                              ftheta = ashld * (shield**bshld) * &
                                 & ((di50 / depth)**cshld) * ((di50 / dmloc)**dshld)
                           else ! islope==4
                              hidexploc = (hidexp(k1, l) + hidexp(k2, l)) / 2.0_fp
                              ftheta = alfpa * sqrt(shield / &
                                 & max(shield * 0.1_fp, hidexploc * thcrpa))
                           end if
                        else
                           ftheta = 0.0_fp
                        end if
                        !
                        ! deal with exeptional case when ftheta, dzdv and dzdu are exactly
                        ! equal to zero
                        !
                        if (e_dzdt(Lf) /= 0.0_fp .or. e_dzdn(Lf) /= 0.0_fp) then
                           sina = ftheta * sbedn / sbedm + e_dzdn(Lf)
                           cosa = ftheta * sbedt / sbedm + e_dzdt(Lf)
                        else
                           sina = sbedn / sbedm
                           cosa = sbedt / sbedm
                        end if
                        tnorm = sqrt(sina**2 + cosa**2)
                        !
                        ! note adjusted bedload put in temporary array so doesn't influence
                        ! surrounding points
                        !
                        sbedm = sbedm * (1.0_fp + alfabs * dzds)
                        sbncor(Lf) = sbedm * (sina / tnorm)
                        sbtcor(Lf) = sbedm * (cosa / tnorm)
                     end select ! islope
                  end if ! sbedm
                  !               !
                  if (avalan .and. (.not. duneavalan) .and. wetslope < 9.99d0) then
                     ! Uses a maximum wet slope (keyword WetSlope in the mor file).
                     ! The default for Wetslope is 10.0 (i.e. 10:1, extremely steep, so no avalanching).
                     !
                     ! Sediment flux (avflux) equals volume exchange between two adjacent cells that is required
                     ! to reach maximum allowed slope, divided by avalanching time (if not set by avaltime, equal to 1 day). This avalanching time has
                     ! no real physical meaning! The sediment flux due to avalanching is added to the bed load transport.
                     !
                     ! The wet slope should really be a function of sediment characteristics. This has not yet been implemented.
                     !
                     slp = sqrt(e_dzdn(Lf) * e_dzdn(Lf) + e_dzdt(Lf) * e_dzdt(Lf))

                     if (slp > wetslope) then
                        avflux = ba(k1) * ba(k2) / (ba(k1) + ba(k2)) * (bl(k2) - bl(k1) + wetslope * (e_dzdn(Lf) / slp) * Dx(Lf)) / avaltime / max(morfac, 1d0)
                        sbncor(Lf) = sbncor(Lf) - avflux * rhosol(l) / wu_mor(Lf)
                     end if
                  end if ! avalan
                  !
                  ! Apply upwind frac and fixfac.
                  !
                  ! At inflow (open, dd, and partition) boundaries the fixfac should not be taken upwind.
                  !
                  if (Lf > lnxi) then ! boundary link, hu condition removed as 0.0<hu<epshu possible
                     fixf = fixfac(k2, l)
                     frc = frac(k2, l)
                  else ! interior link
                     if (sbncor(Lf) >= 0) then
                        fixf = fixfac(k1, l) ! outward positive
                        frc = frac(k1, l)
                     else
                        fixf = fixfac(k2, l)
                        frc = frac(k2, l)
                     end if
                  end if
                  !
                  sbncor(Lf) = sbncor(Lf) * frc * fixf
                  sbtcor(Lf) = sbtcor(Lf) * frc * fixf
                  !
               end if ! tratyp
               sbn(Lf, l) = sbncor(Lf)
               sbt(Lf, l) = sbtcor(Lf)
            end do ! lsedtot
         end if ! hu
      end do ! Lf

   end subroutine fm_adjust_bedload
