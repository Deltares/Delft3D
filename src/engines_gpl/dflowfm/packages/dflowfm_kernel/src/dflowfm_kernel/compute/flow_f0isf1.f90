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
module m_flow_f0isf1
use m_a1vol1tot, only: a1vol1tot

   implicit none
contains
 subroutine flow_f0isf1() ! Todo: make pointer stucture and reset pointers
    use m_flowgeom, only: ndxi
    use m_flow
    use m_flowtimes, only: dts
    use m_partitioninfo
    use m_sediment, only: jamorf, stm_included
    use m_sobekdfm, only: nbnd1d2d, kbnd1d2d
    use m_drawthis
    use m_get_kbot_ktop

    integer :: k, kk, kb, kt, Lf, i, k1, k2

    call a1vol1tot()

    ! TODO: UNST-904: sum only across *own* cells
    !           if ( idomain(n).ne.my_rank ) cycle
    ! UNST-904: AND, create some reduce_bal() subroutine

    hsaver = 0d0
    if (a1tot /= 0) then
       hsaver = vol1tot / a1tot
    end if

    ! basis spul
    vinbnd = qinbnd * dts ! do *dts here to avoid inconsistencies
    voutbnd = qoutbnd * dts
    vincel = qincel * dts ! do *dts here to avoid inconsistencies
    voutcel = qoutcel * dts
    !volerr      = vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel

    vinbndcum = vinbndcum + vinbnd
    voutbndcum = voutbndcum + voutbnd
    vincelcum = vincelcum + vincel
    voutcelcum = voutcelcum + voutcel

    if (jamorf == 1 .and. .not. stm_included) then
       !volerrcum = volerrcum + dvolbot
    end if

    !if (stm_included) then
    !   dvolbot = 0d0
    !   do ll=1,stmpar%lsedtot
    !      dvolbot = dvolbot + sum(sedtra%dbodsd(ll,:)/stmpar%sedpar%cdryb(ll)*ba)
    !   enddo
    !   !volerrcum = volerrcum + dvolbot
    !end if
    volerr = vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel !+ dvolbot
    volerrcum = volerrcum + volerr

    if (jahisbal > 0) then
       ! extra
       vinrain = qinrain * dts
       vinrainground = qinrainground * dts
       vouteva = qouteva * dts
       voutevaicept = qoutevaicept * dts
       vinlat(1:2) = qinlat(1:2) * dts
       voutlat(1:2) = qoutlat(1:2) * dts
       vingrw = qingrw * dts
       voutgrw = qoutgrw * dts

       qinsrc = 0d0
       qoutsrc = 0d0
       do i = 1, numsrc
          if (qsrc(i) > 0d0) then
             k1 = ksrc(1, i)
             k2 = ksrc(4, i)
          else
             k1 = ksrc(4, i)
             k2 = ksrc(1, i)
          end if

          if (k1 > 0) then
             qoutsrc = qoutsrc + abs(qsrc(i))
          end if
          if (k2 > 0) then
             qinsrc = qinsrc + abs(qsrc(i))
          end if
       end do

       vinsrc = qinsrc * dts
       voutsrc = qoutsrc * dts
       vinext(1:2) = qinext(1:2) * dts
       voutext(1:2) = qoutext(1:2) * dts

       ! Time-summed cumulative volumes (nowhere used)
       vinraincum = vinraincum + vinrain
       voutevacum = voutevacum + vouteva
       vinlatcum(1:2) = vinlatcum(1:2) + vinlat(1:2)
       voutlatcum(1:2) = voutlatcum(1:2) + voutlat(1:2)
       vingrwcum = vingrwcum + vingrw
       voutgrwcum = voutgrwcum + voutgrw
       vinsrccum = vinsrccum + vinsrc
       voutsrccum = voutsrccum + voutsrc
       vinextcum(1:2) = vinextcum(1:2) + vinext(1:2)
       voutextcum(1:2) = voutextcum(1:2) + voutext(1:2)

       ! Volume totals at current time (for his output)
       volcur(IDX_STOR) = vol1tot
       volcur(IDX_VOLTOT) = vol1tot
       volcur(IDX_VOLERR) = volerr
       volcur(IDX_BNDIN) = vinbnd
       volcur(IDX_BNDOUT) = voutbnd
       volcur(IDX_BNDTOT) = (vinbnd - voutbnd)
       volcur(IDX_EXCHIN) = 0d0
       volcur(IDX_EXCHOUT) = 0d0
       volcur(IDX_EXCHTOT) = 0d0
       do i = 1, nbnd1d2d
          Lf = kbnd1d2d(3, i)
          volcur(IDX_EXCHTOT) = volcur(IDX_EXCHTOT) + q1(Lf) * dts
          if (q1(Lf) > 0) then
             volcur(IDX_EXCHIN) = volcur(IDX_EXCHIN) + q1(Lf) * dts
          else
             volcur(IDX_EXCHOUT) = volcur(IDX_EXCHOUT) - q1(Lf) * dts
          end if
       end do
       volcur(IDX_PRECIP_TOTAL) = vinrain
       volcur(IDX_PRECIP_GROUND) = vinrainground
       volcur(IDX_EVAP) = vouteva
       volcur(IDX_SOUR) = vinsrc - voutsrc
       volcur(IDX_ICEPT) = vol1icept
       volcur(IDX_EVAP_ICEPT) = voutevaicept

       if (jaFrcInternalTides2D == 1) then
          volcur(IDX_InternalTidesDIssipation) = DissInternalTides * dts
       else
          volcur(IDX_InternalTidesDIssipation) = 0d0
       end if

       if (jatidep > 0 .or. jaselfal > 0) then
          volcur(IDX_GravInput) = GravInput * dts
       else
          volcur(IDX_GravInput) = 0d0
       end if

       if (jaselfal > 0) then
          volcur(IDX_SALInput) = SALInput * dts
          volcur(IDX_SALInput2) = SALInput2 * dts
       else
          volcur(IDX_SALInput) = 0d0
          volcur(IDX_SALInput2) = 0d0
       end if
       volcur(IDX_GRWIN) = vingrw
       volcur(IDX_GRWOUT) = voutgrw
       volcur(IDX_GRWTOT) = (vingrw - voutgrw)

       volcur(IDX_LATIN) = sum(vinlat(1:2))
       volcur(IDX_LATOUT) = sum(voutlat(1:2))
       volcur(IDX_LATTOT) = volcur(IDX_LATIN) - volcur(IDX_LATOUT)
       volcur(IDX_LATIN1D) = vinlat(1)
       volcur(IDX_LATOUT1D) = voutlat(1)
       volcur(IDX_LATTOT1D) = (vinlat(1) - voutlat(1))
       volcur(IDX_LATIN2D) = vinlat(2)
       volcur(IDX_LATOUT2D) = voutlat(2)
       volcur(IDX_LATTOT2D) = (vinlat(2) - voutlat(2))

       volcur(IDX_EXTIN) = sum(vinext(1:2))
       volcur(IDX_EXTOUT) = sum(voutext(1:2))
       volcur(IDX_EXTTOT) = volcur(IDX_EXTIN) - volcur(IDX_EXTOUT)
       volcur(IDX_EXTIN1D) = vinext(1)
       volcur(IDX_EXTOUT1D) = voutext(1)
       volcur(IDX_EXTTOT1D) = (vinext(1) - voutext(1))
       volcur(IDX_EXTIN2D) = vinext(2)
       volcur(IDX_EXTOUT2D) = voutext(2)
       volcur(IDX_EXTTOT2D) = (vinext(2) - voutext(2))

       ! cumulate
       cumvolcur = cumvolcur + volcur
    end if

    if (NDRAW(28) == 34) then ! Display values at flow nodes: volerror
       if (kmx == 0) then
          do kk = 1, ndxi
             if (jampi == 1) then
                if (idomain(kk) /= my_rank) cycle
             end if
             volerror(kk) = vol1(kk) - vol0(kk) - dts * (sqi(kk) - squ(kk)) ! array transfer
          end do
       else
          do kk = 1, ndxi
             if (jampi == 1) then
                if (idomain(kk) /= my_rank) cycle
             end if

             call getkbotktop(kk, kb, kt)
             do k = kb, kt
                volerror(k) = vol1(k) - vol0(k) - dts * (sqi(k) - squ(k))
             end do
          end do
       end if
    end if

    vol0 = vol1 ! array

    vol0tot = vol1tot ! scalar
    a0tot = a1tot

 end subroutine flow_f0isf1
end module m_flow_f0isf1
