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

!> fill constituent array
subroutine fill_constituents(jas) ! if jas == 1 do sources
   use m_transport, only: ISED1, ISPIR, NUMCONST, ISALT, ITEMP, ITRA1, ITRAN, constituents, const_sour, const_sink, difsedu, difsedw
   use m_flowgeom, only: ndx, ndxi, ba
   use m_flow, only: kmx, ndkx, zws, hs, sq, vol1, spirint, spirucm, spircrv, fcoris, czssf
   use m_wind, only: heatsrc
   use m_physcoef, only: dicouv, dicoww, difmolsal, difmoltem, difmoltracer, Jaallowcoolingbelowzero, ag, vonkar
   use m_nudge, only: nudge_rate, nudge_tem, nudge_sal
   use m_turbulence, only: sigsal, sigtem, sigtracer, sigdifi, sigsed, wsf
   use fm_external_forcings_data, only: wstracers, numsrc, ksrc, qsrc, ccsrc
   use m_sediment, only: sed, sedtra, stm_included, stmpar, jased, mxgr, ws
   use m_mass_balance_areas, only: jamba, mbadefdomain, mbafluxheat, mbafluxsorsin
   use m_partitioninfo, only: jampi, idomain, my_rank
   use m_sferic, only: jsferic, fcorio
   use m_flowtimes, only: dts, time1, tstart_user, tfac
   use m_flowparameters, only: janudge, jasecflow, jatem, jaequili, epshu, epshs, testdryflood, icorio
   use m_laterals, only: numlatsg, get_lateral_discharge, add_lateral_load_and_sink, apply_transport_is_used
   use m_missing, only: dmiss
   use timers, only: timon, timstrt, timstop
   use m_alloc, only: aerr

   implicit none

   integer, intent(in) :: jas

   double precision :: dvoli
   integer :: iconst, kk, kkk, k, kb, kt, n, kk2, imba, jamba_src, iostat
   integer :: jsed ! counter for suspended sediment fractions
   integer :: jtra ! counter for tracers
   double precision, parameter :: dtol = 1d-8
   double precision :: spir_ce, spir_be, spir_e, alength_a, time_a, alpha, fcoriocof, qsrck, qsrckk, dzss

   double precision :: Trefi
   double precision, allocatable, dimension(:, :, :) :: qin_over_laterals
   double precision, allocatable, dimension(:, :, :) :: qout_over_laterals

   integer(4) :: ithndl =  0
   
   if (timon) call timstrt("fill_constituents", ithndl)

   const_sour = 0d0
   const_sink = 0d0

   do k = 1, Ndkx

      if (jasecflow > 0 .and. jaequili == 0 .and. kmx == 0) then
         constituents(ISPIR, k) = spirint(k)
      end if

      if (ISED1 /= 0) then
         do jsed = 1, mxgr
            iconst = ISED1 + jsed - 1
            constituents(iconst, k) = sed(jsed, k)
         end do
      end if
   end do

   if (stm_included) then
      if (stmpar%morpar%bedupd .and. time1 >= tstart_user + stmpar%morpar%tmor * tfac) then
         if (ISED1 /= 0) then
            do k = 1, ndx
               if (hs(k) < stmpar%morpar%sedthr) then
                  do jsed = 1, mxgr
                     iconst = ISED1 + jsed - 1
                     call getkbotktop(k, kb, kt)
                     constituents(iconst, kb:kt) = 0d0
                  end do
               end if
            end do
         end if
      end if
   end if

   difsedu = 0d0; difsedw = 0d0; sigdifi = 0d0

!  diffusion coefficients

   if (ISALT /= 0) then
      if (dicouv >= 0d0) then
         difsedu(ISALT) = difmolsal
      end if
      if (dicoww >= 0d0) then
         difsedw(ISALT) = dicoww + difmolsal
         sigdifi(ISALT) = 1d0 / sigsal
      end if
   end if

   if (ITEMP /= 0) then
      if (dicouv >= 0d0) then
         difsedu(ITEMP) = difmoltem
      end if
      if (dicoww >= 0d0) then
         difsedw(ITEMP) = dicoww + difmoltem
         sigdifi(ITEMP) = 1d0 / sigtem
      end if
   end if

   if (jasecflow > 0 .and. jaequili == 0 .and. kmx == 0) then
      difsedu(ISPIR) = 0d0
      difsedw(ISPIR) = 0d0 !dicoww + difmoltem
      sigdifi(ISPIR) = 0d0 !/sigspi
   end if

   if (ISED1 /= 0) then
      do jsed = 1, mxgr
         iconst = ISED1 + jsed - 1
         if (dicouv >= 0d0) difsedu(iconst) = 0d0
         if (dicoww >= 0d0) then
            difsedw(iconst) = dicoww
            sigdifi(iconst) = 1d0 / sigsed(jsed)
         end if
         if (jased < 4) wsf(iconst) = ws(jsed)
      end do
   end if

   if (ITRA1 > 0) then
      do jtra = ITRA1, ITRAN
         difsedu(jtra) = difmoltracer
         if (dicoww >= 0d0) then
            difsedw(jtra) = dicoww + difmoltracer
            sigdifi(jtra) = 1d0 / sigtracer
         end if
         wsf(jtra) = wstracers(jtra - ITRA1 + 1)
      end do
   end if

   ! add lateral in- and outflow of constituents as sources and sinks
   if (apply_transport_is_used) then
      allocate (qin_over_laterals(max(1, kmx), numlatsg, ndxi), stat=iostat)
      call aerr('qin_over_laterals', iostat, numlatsg * ndxi, 'fill_constituents')
      allocate (qout_over_laterals(max(1, kmx), numlatsg, ndxi), stat=iostat)
      call aerr('qout_over_laterals', iostat, numlatsg * ndxi, 'fill_constituents')

      ! TODO UNST-8062: this can be simplified by using qqlat(numlayer, numlatsg, ndx)
      call get_lateral_discharge(qin_over_laterals, qout_over_laterals, vol1)
      call add_lateral_load_and_sink(const_sour, const_sink, qin_over_laterals, qout_over_laterals, vol1, dtol)

      deallocate (qin_over_laterals)
      deallocate (qout_over_laterals)

   end if

!  sources
   do kk = 1, Ndx

!     nudging
      Trefi = 0d0
      if (janudge == 1 .and. jas == 1) then
!        get reference time
         Trefi = nudge_rate(kk)
      end if

      call getkbotktop(kk, kb, kt)
      do k = kb, kt
         dvoli = 1d0 / max(vol1(k), dtol)
         if (testdryflood == 2) dvoli = 1d0 / max(vol1(k), epshu * ba(kk) / max(kt - kb + 1, 1))

!        temperature
         if (jatem > 1) then
            if (Jaallowcoolingbelowzero == 0) then ! default behaviour since 2017
               ! no cooling below 0 degrees
               if (heatsrc(k) > 0d0) then
                  const_sour(ITEMP, k) = heatsrc(k) * dvoli
               else if (heatsrc(k) < 0d0) then
                  const_sink(ITEMP, k) = -heatsrc(k) * dvoli / max(constituents(itemp, k), 0.001d0)
               end if
            else ! allowing cooling below 0 degrees
               const_sour(ITEMP, k) = heatsrc(k) * dvoli
            end if
         end if

!        nudging
         if (Trefi > 0d0) then
            if (ITEMP > 0 .and. nudge_tem(k) /= DMISS) then
               const_sour(ITEMP, k) = const_sour(ITEMP, k) + nudge_tem(k) * Trefi
               const_sink(ITEMP, k) = const_sink(ITEMP, k) + Trefi
            end if

            if (ISALT > 0 .and. nudge_sal(k) /= DMISS) then
               const_sour(ISALT, k) = const_sour(ISALT, k) + nudge_sal(k) * Trefi
               const_sink(ISALT, k) = const_sink(ISALT, k) + Trefi
            end if
         end if

!        terms due to non-conservative formulation
         do iconst = 1, NUMCONST
            const_sour(iconst, k) = const_sour(iconst, k) - constituents(iconst, k) * sq(k) * dvoli
         end do

      end do

      !     Note: from now on, only _add_ to sources

!     spiral flow source term
      if (jasecflow > 0 .and. jaequili == 0 .and. kmx == 0) then
         if (spirucm(kk) < 1d-3 .or. hs(kk) < epshu) then
            ! const_sour(ISPIR,kk) = 0d0
            ! const_sink(ISPIR,kk) = 0d0
         else
            fcoriocof = fcorio; if (icorio > 0 .and. jsferic == 1) fcoriocof = fcoris(kk)
            alpha = sqrt(ag) / vonkar / max(czssf(kk), 20.0d0)
            spir_ce = fcorio * hs(kk) * 0.5d0
            spir_be = hs(kk) * spircrv(kk) * spirucm(kk)
            spir_e = spir_be - spir_ce
            alength_a = (1.0d0 - 2.0d0 * alpha) * hs(kk) / (2.0d0 * vonkar * vonkar * alpha) !TO DO: this term should be expanded to prevent negative alength_a for alpha > 0.5
            time_a = alength_a / spirucm(kk)
            !const_sour(ISPIR,kk) =  ( spir_e - spirint(kk) ) / time_a !* dvoli    ! S=(I_eq - I)/Ta
            const_sour(ISPIR, kk) = const_sour(ISPIR, kk) + spir_e / time_a
            const_sink(ISPIR, kk) = const_sink(ISPIR, kk) + 1d0 / time_a
         end if
      end if

!     sediment (2D sources, also in 3D)
!
      if (stm_included) then
         if (ISED1 > 0 .and. kk <= Ndxi) then
            do jsed = 1, mxgr
               kkk = sedtra%kmxsed(kk, jsed)
               if (kkk > 0) then
                  iconst = jsed + ISED1 - 1
                  const_sour(iconst, kkk) = const_sour(iconst, kkk) + sedtra%sourse(kk, jsed)
                  const_sink(iconst, kkk) = const_sink(iconst, kkk) + sedtra%sinkse(kk, jsed)

                  if (stmpar%morpar%flufflyr%iflufflyr > 0) then
                     const_sour(iconst, kkk) = const_sour(iconst, kkk) + stmpar%morpar%flufflyr%sourf(jsed, kk)
                     const_sink(iconst, kkk) = const_sink(iconst, kkk) + stmpar%morpar%flufflyr%sinkf(jsed, kk)
                  end if

               end if
            end do
         end if
      end if
   end do

   if (jamba > 0 .and. jatem > 0) then ! Positive and negative sums for jamba, checking just once

      do kk = 1, Ndx
         imba = mbadefdomain(kk)
         if (imba > 0) then
            call getkbotktop(kk, kb, kt)
            do k = kb, kt
               if (heatsrc(k) > 0d0) then
                  mbafluxheat(1, imba) = mbafluxheat(1, imba) + heatsrc(k) * dts
               else if (heatsrc(k) < 0d0) then
                  mbafluxheat(2, imba) = mbafluxheat(2, imba) - heatsrc(k) * dts
               end if
            end do
         end if
      end do

   end if

   ! NOTE: apply_tracer_bc has been moved earlier to transport() routine,
   !       but apply_sediment_bc must still be done here, since above the boundary
   !       nodes's constituents(kb,:) = sed(kb,:) has reset it to 0.
   if (stm_included) call apply_sediment_bc()
   if (jas == 0) goto 1234 ! no sources from initialise

   do n = 1, numsrc
      kk = ksrc(1, n) ! 2D pressure cell nr FROM
      kk2 = ksrc(4, n) ! 2D pressure cell nr TO
      qsrckk = qsrc(n)
      qsrck = qsrckk

      jamba_src = jamba
      if (jampi == 1) then
         if (kk > 0) then
            if (idomain(kk) /= my_rank) jamba_src = 0
         else
            if (kk2 > 0) then
               if (idomain(kk2) /= my_rank) jamba_src = 0
            else
               jamba_src = 0
            end if
         end if
      end if

      if (kk > 0) then ! FROM Point
         do k = ksrc(2, n), ksrc(3, n)
            dvoli = 1d0 / max(vol1(k), dtol)
            if (kmx > 0) then
               dzss = zws(ksrc(3, n)) - zws(ksrc(2, n) - 1)
               if (dzss > epshs) then
                  qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
               else
                  qsrck = qsrckk / (ksrc(3, n) - ksrc(2, n) + 1)
               end if
            end if
            if (qsrck > 0) then ! FROM k to k2
               call set_sorsin(2, 1, n, k, -qsrck, dvoli)
            else if (qsrck < 0) then ! FROM k2 to k
               call set_sorsin(1, 1, n, k, -qsrck, dvoli)
            end if
         end do
      end if

      if (kk2 > 0) then ! TO Point
         do k = ksrc(5, n), ksrc(6, n)
            dvoli = 1d0 / max(vol1(k), dtol)
            if (kmx > 0) then
               dzss = zws(ksrc(6, n)) - zws(ksrc(5, n) - 1)
               if (dzss > epshs) then
                  qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
               else
                  qsrck = qsrckk / (ksrc(6, n) - ksrc(5, n) + 1)
               end if
            end if
            if (qsrck > 0) then
               call set_sorsin(2, 2, n, k, qsrck, dvoli)
            else if (qsrck < 0) then
               call set_sorsin(1, 2, n, k, qsrck, dvoli)
            end if
         end do
      end if

   end do

1234 continue

   if (timon) call timstop(ithndl)

contains

   subroutine set_sorsin(i1, i2, n, k, qsrck, dvoli)
      use m_mass_balance_areas, only: imbs2sed
      use m_fm_erosed, only: morfac

      integer, intent(in) :: i1 !< flow direction 1=from TO to FROM, 2=from FROM to TO
      integer, intent(in) :: i2 !< 1=FROM-point, 2=TO-point
      integer, intent(in) :: n !< source/sink number
      integer, intent(in) :: k !< layer number
      double precision, intent(in) :: qsrck !< flow
      double precision, intent(in) :: dvoli !<

      integer :: iconst !< constituent index, equal to imbs
      double precision :: flux !< flux
      double precision :: dt !< applicable time step

      do iconst = 1, numconst
         if (i1 == i2) then ! on outflow side
            const_sour(iconst, k) = const_sour(iconst, k) + qsrck * ccsrc(iconst, n) * dvoli
            flux = qsrck * ccsrc(iconst, n)
         else ! on inflow side
            const_sour(iconst, k) = const_sour(iconst, k) + qsrck * constituents(iconst, k) * dvoli
            flux = qsrck * constituents(iconst, k)
         end if
         if (jamba_src > 0) then
            if (imbs2sed(iconst) > 0) then
               dt = dts * morfac
            else
               dt = dts
            end if
            if (i1 == i2) then ! on outflow side
               mbafluxsorsin(i1, i2, iconst, n) = mbafluxsorsin(i1, i2, iconst, n) + flux * dt
            else ! on inflow side
               mbafluxsorsin(i1, i2, iconst, n) = mbafluxsorsin(i1, i2, iconst, n) - flux * dt
            end if
         end if
      end do
   end subroutine set_sorsin

end subroutine fill_constituents
