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
module m_get_equilibrium_transport_rates
use m_widarhyr, only: widarhyr
use m_check_einstein_garcia2, only: check_einstein_garcia2

   implicit none
contains
 subroutine getequilibriumtransportrates(kk, seq, wse, mx, hsk) ! get them for flowcell kk or ban kk
    use m_flowgeom
    use m_flow
    use m_netw
    use m_sediment
    use m_waves, only: twav, uorb
    use geometry_module, only: dbdistance
    use m_missing, only: dmiss
    use m_sferic, only: jsferic, jasfer3D
    use m_get_czz0

    integer, intent(in) :: kk, mx ! flowcell kk or ban kk, mx fracnr
    double precision, intent(out) :: seq(mx) ! seq(kg/m3)
    double precision, intent(out) :: wse(mx) ! effective fall velocity (m/s)
    double precision, intent(out) :: hsk ! waterdepth, flowcell or ban

    double precision :: cz, flx
    double precision :: ucr, ueff, Twave, Uwave, Ucur, Ucrw, Pmob, beta, D50h, sbeq
    double precision :: aref, Tmob, crefa, sseq, ustar, ustar2, rouse, sqcf, z0k, dks, hdune = 0
    double precision :: qsseq, eincheck2
    double precision :: seqbed
    double precision :: hpr, dzz, wu2, wid, ar, hyr, zbu
    double precision :: sumlay, dmorfacL, dh, ustar2swart, ustw2, fw, qeng, cf, wa, z00
    integer :: j, n, k, kg, nn, n1, L, LL, jabanhydrad = 0

    if (stm_included) return

    seq = 0d0; flx = 0d0

    if (jaceneqtr == 1) then
       k = kk
       if (ibedlevtyp == 1 .or. ibedlevtyp == 6) then ! tile type
          hsk = s1(k) - bl(k)
       else ! u-netwnodes / conv type
          hsk = 0d0; nn = 0
          do n = 1, netcell(k)%n
             n1 = netcell(k)%nod(n)
             dh = max(0d0, s1(k) - zk(n1))
             if (dh > 0d0) then
                nn = nn + 1
                hsk = hsk + dh
             end if
          end do
          if (nn > 0) then
             hsk = hsk / nn
          end if
       end if
    else
       n = nban(1, kk) ! net node
       k = nban(2, kk) ! flow node
       hsk = 0d0
       if (jabanhydrad == 1) then ! Hydraulic radius for this ban
          wu2 = dbdistance(xz(k), yz(k), xk(n), yk(n), jsferic, jasfer3D, dmiss)
          L = nban(3, kk)
          zbu = 9d9
          if (L > 0) then
             zbu = 0.5d0 * (bob(1, L) + bob(2, L))
          end if

          L = nban(4, kk)
          if (L > 0) then
             zbu = 0.5d0 * (bob(1, L) + bob(2, L)) + zbu
             zbu = 0.5d0 * zbu
          end if

          if (s1(k) > zbu) then
             hpr = s1(k) - zbu
             dzz = zk(n) - zbu
             call widarhyr(hpr, dzz, wu2, wid, ar, hyr)
             hsk = hyr
          end if
       else
          hsk = s1(k) - zk(n) ! todo make netnode oriented waterlevel
       end if
    end if

    if (hsk < epshs) then ! local waterdepth (m)
       return
    end if

    call getczz0(hsk, frcuni, ifrctypuni, cz, z0k) ! get roughness as specified in hydrodynamics
    sqcf = sag / cz ! sqrt(g)/C  ( )

    ! or whatever comes out of the roughness predictor, and van Rijn takes z0 = 3D90

    dks = 30d0 * z0k ! nikuradse roughness (m)
    if (dks > 0.5d0 * hsk) then !0.2
       return
    end if

    hdune = 0d0 ! half duneheight (m)
    wse = ws

    if (jaceneqtr == 1) then
       ucur = sqrt(ucx(k) * ucx(k) + ucy(k) * ucy(k)) !                                                            ! current (transport) velocity (m/s)
    else
       ucur = sqrt(ucnx(n) * ucnx(n) + ucny(n) * ucny(n))
    end if

    if (jased == 3) then ! Engelund:
       cf = sqcf * sqcf
       qeng = 0.05d0 * cf * sqcf * (ucur**5) / (D50(1) * (rhodelta(1) * ag)**2) ! (m2/s)

       sseq = qeng / (max(ucur, 1d-2) * hsk) ! ( ) dimensionless equilibrium 2D transport suspended sediment concentration
       sseq = alfasus * sseq
       seq(1) = rhosed(1) * sseq ! equilibrium transport concentration bed + suspended (kg/m3)
       wse(1) = wse(1) * crefcav
    else

       ueff = ucur; beta = 1d0; twave = 0d0 !
       ustar2swart = sqcf * sqcf * Ueff * Ueff
       if (jawave > 0 .and. ueff > 0d0 .and. .not. flowWithoutWaves) then
          if (twav(k) > 1d-2) then
             twave = twav(k)
             uwave = uorb(k) ! (m/s) for jased == 2, tauwav contains uorb
             do nn = 1, nd(k)%lnx
                LL = abs(nd(n)%ln(nn))
                if (hu(LL) > 0d0) then
                   ar = au(LL) * dx(LL)
                   wa = wa + ar ! area  weigthed
             !z00 = z00 + ar*hu(LL)*exp(-1d0 - vonkar*cz/sag)   ! z0ucur, to avoid double counting
             z00 = z00 + ar*z0ucur(LL)   ! z0ucur, to avoid double counting
                end if
             end do
             if (wa > 0) then
                z00 = z00 / wa
             end if
             z00 = max(z00, epsz0)

             beta = ucur / (ucur + uwave) ! ( )

             Ueff = Ucur + 0.4d0 * uwave ! (m/s) SvR 2007

             if (MxgrKrone > 0) then
                call Swart(Twave, uwave, z00, fw, ustw2)
                ustar2swart = ustar2swart + ustw2 ! Swart
             end if

          end if
       end if
       ustar = sqcf * Ueff

       if (kmx > 0) then
          ustar = ustbc(k)
          ustar2swart = ustar * ustar
       end if

       ustar2 = ustar * ustar

       do j = 1, mxgr ! loop over grainsizes

          if (j <= MxgrKrone) then ! following Krone/Swart
             if (ustar2swart > Ustcre2(j)) then ! eroderen
                Tmob = (ustar2swart - Ustcre2(j)) / Ustcre2(j) ! ( ) dimensionless mobility parameter
                flx = erosionpar(j) * Tmob ! kg/(m2s)
             end if
             seq(j) = flx / ws(j) ! equilibrium sediment concentration

          else ! soulsby van rijn 2007 ASCE,

             !Ucr=Accr(j)*log(12.d0*hsk/dks)   ! 3d0*D90(j))              ! (2007a (12) )
             !Ucr=Accr(j)*log(hsk/(ee*z0k)) / vonkar

             Ucr = Accr(j) / sqcf

             if (Twave > 0) then
                Ucrw = Awcr(j) * Twave**Bwcr(j) !  = 0.24d0*(rhodelta*ag)**0.66d0*D50**0.33d0
                Ucr = beta * Ucr + (1d0 - beta) * Ucrw ! (m/s)
             end if

             if (isusandorbed >= 2) then
                Pmob = (Ueff - Ucr) / sqsgd50(j) ! ( ) dimensionless mobility parameter
                sbeq = 0d0
                if (Pmob > 0) then

                   Pmob = Pmob**1.5d0 ! ( ) dimensionless mobility, old power was 2.4d0

                   D50h = (D50(j) / hsk)**1.2d0 ! ( )

                   sbeq = 0.015d0 * D50h * Pmob ! ( ) dimensionless equilibrium bedload concentration, formula 12 , so bed load transport =
                   !  qb = u.h.sbeq.rhosed ( (m/s) . m . ( ). (kg/m3) ) = ( kg/(sm) ), old alfa was .005

                   seq(j) = sbeq * rhosed(j) ! equilibrium concentration (kg/m3)

                end if
             end if
             ! reference height is max of (nikuradse and half dune height) (m)
             aref = max(dks, hdune) ! vRijns book page 7.65 (line 6)
             aref = max(aref, 0.01d0 * hsk) ! vRijns book page 7.64 (line 3)
             aref = min(aref, 0.25d0 * hsk) ! check, always < .25 waterdepth
             ! vrijns book pag 8.50 r 3 ????
             Tmob = (Ueff * Ueff - Ucr * Ucr) / (Ucr * Ucr) ! Mobility parameter T ( )
             if (Tmob > 0 .and. ustar > 0d0) then
                rouse = ws(j) / (vonkar * ustar)
                crefa = 0.015d0 * (D50(j) / aref) * (Tmob**1.5d0) * Dstar03(j) ! dimensionless reference concentration ( ), (book vRijn 1993, (7.3.31) )
                !crefa = min(crefa, 0.65d0)                                   ! max ref concentration ( )               or (book Garcia 2008, (2-226) )
                !crefa = min(crefa, 0.15d0)
                crefa = min(crefa, 0.05d0) ! vRijns book ?

                if (kmx == 0) then
                   call check_einstein_garcia2(aref, hsk, z0k, rouse, eincheck2) ! numerical check einstein integrals, now used as vertical integrator anyway

                   !qssevr84   = 0.012d0*Ueff*D50(j)*Pmob**2.4d0*Dstar(j)**-0.6d0       ! boek vanrijn (7.3.46), or 2007b

                   qsseq = eincheck2 * crefa * ustar / vonkar ! (conclusion : inaccuracy of einstein_garcia is about 10-20  )

                   !qsseq = qssevr84

                   sseq = qsseq / (max(ucur, 1d-2) * hsk) ! ( ) dimensionless equilibrium 2D transport suspended sediment concentration

                   ! call checksuspended_transport()

                   seq(j) = seq(j) + rhosed(j) * sseq ! equilibrium transport concentration bed + suspended (kg/m3)

                   wse(j) = ws(j) * crefa / (sseq + sbeq) ! effective 2Dh fall velocity er (m/s)*( )

                else

                   seq(j) = crefa

                   wse(j) = ws(j) ! *alfaT

                end if

             end if

          end if

       end do

    end if ! !jased 1, 2

    sumlay = 0d0 ! check bed material

    if (jaceneqtr == 1) then
       kg = k
    else
       kg = n
    end if
    sumlay = 0d0 ! check bed material
    do j = 1, mxgr
       sumlay = sumlay + grainlay(j, kg)
    end do

    dmorfacL = max(1d0, dmorfac)
    if (sumlay == 0d0) then
       seq(1:mxgr) = 0d0
    else
       do j = 1, mxgr
          seq(j) = seq(j) * grainlay(j, kg) / sumlay ! normed with erodable fraction (kg/m3)
          seqbed = rhosed(j) * grainlay(j, kg) * rhobulkrhosed / (hsk * dmorfacL) ! concentration if all bed material was suspended (kg/m3)

          if (grainlay(j, kg) < dks) then ! limiting below roughness thickness
             seqbed = seqbed * grainlay(j, kg) / dks
          end if

          if (seq(j) > seqbed) then
             seq(j) = seqbed
          end if

       end do
    end if

 end subroutine getequilibriumtransportrates
end module m_get_equilibrium_transport_rates
