!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_veg3du
    use m_waq_precision

    implicit none

contains


    subroutine veg3du     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)

        use m_evaluate_waq_attribute
        use layered_sediment

        ! function : vegetation module uptake of nutrients

        implicit none

        ! arguments          i/o description

        real(kind = real_wp) :: pmsa(*)      !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)        ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)    ! i  array of pointers in pmsa to get and store the data
        integer(kind = int_wp) :: increm(*)    ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg        ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux       ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)  ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)    ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1         ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        integer(kind = int_wp) :: noq2         ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3         ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4         ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

        ! from pmsa array

        real(kind = real_wp) :: depth        ! i  depth of segment                               (m)
        real(kind = real_wp) :: totaldepth   ! i  total depth water column                       (m)
        real(kind = real_wp) :: localdepth   ! i  depth from water surface to bottom of segment  (m)
        real(kind = real_wp) :: hmax         ! i  maxmimum length roots                          (m)
        real(kind = real_wp) :: delt         ! i  delt                                           (d)
        real(kind = real_wp) :: nh4          ! i  nh4                                         (g/m3)
        real(kind = real_wp) :: aap          ! i  aap                                         (g/m3)
        real(kind = real_wp) :: so4          ! i  so4                                         (g/m3)
        real(kind = real_wp) :: no3          ! i  no3                                         (g/m3)
        real(kind = real_wp) :: po4          ! i  po4                                         (g/m3)
        real(kind = real_wp) :: sud          ! i  sud                                         (g/m3)
        real(kind = real_wp) :: s1_nh4       ! i  nh4 in sediment                             (g/m3)
        real(kind = real_wp) :: s1_aap       ! i  aap in sediment                             (g/m3)
        real(kind = real_wp) :: s1_so4       ! i  so4 in sediment                             (g/m3)
        real(kind = real_wp) :: s1_no3       ! i  no3 in sediment                             (g/m3)
        real(kind = real_wp) :: s1_po4       ! i  po4 in sediment                             (g/m3)
        real(kind = real_wp) :: s1_sud       ! i  sud in sediment                             (g/m3)
        real(kind = real_wp) :: inicovvbxx   ! i  percentage coverage                            (%)
        real(kind = real_wp) :: vbxxnavail   ! i  available nitrogen                          (g/m2)
        real(kind = real_wp) :: vbxxpavail   ! i  available phosphorus                        (g/m2)
        real(kind = real_wp) :: vbxxsavail   ! i  available sulfur                            (g/m2)
        real(kind = real_wp) :: fnvbxxup     ! i  2d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fpvbxxup     ! i  2d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fsvbxxup     ! i  2d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fn1vbxxupy   ! o  3d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fn2vbxxupy   ! o  3d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fp1vbxxupy   ! o  3d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fp2vbxxupy   ! o  3d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fs1vbxxupy   ! o  3d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: fs2vbxxupy   ! o  3d uptake flux                            (g/m2/d)
        real(kind = real_wp) :: s1_fn1vbxxupy   ! o  uptake flux sediment                   (g/m2/d)
        real(kind = real_wp) :: s1_fn2vbxxupy   ! o  uptake flux sediment                   (g/m2/d)
        real(kind = real_wp) :: s1_fp1vbxxupy   ! o  uptake flux sediment                   (g/m2/d)
        real(kind = real_wp) :: s1_fp2vbxxupy   ! o  uptake flux sediment                   (g/m2/d)
        real(kind = real_wp) :: s1_fs1vbxxupy   ! o  uptake flux sediment                   (g/m2/d)
        real(kind = real_wp) :: s1_fs2vbxxupy   ! o  uptake flux sediment                   (g/m2/d)
        real(kind = real_wp) :: fl_fn1vbxxupy   ! o  3d uptake flux N pool 1                (g/m3/d)
        real(kind = real_wp) :: fl_fn2vbxxupy   ! o  3d uptake flux N pool 2                (g/m3/d)
        real(kind = real_wp) :: fl_fp1vbxxupy   ! o  3d uptake flux P pool 1                (g/m3/d)
        real(kind = real_wp) :: fl_fp2vbxxupy   ! o  3d uptake flux P pool 2                (g/m3/d)
        real(kind = real_wp) :: fl_fs1vbxxupy   ! o  3d uptake flux S pool 1                (g/m3/d)
        real(kind = real_wp) :: fl_fs2vbxxupy   ! o  3d uptake flux S pool 2                (g/m3/d)

        ! local declarations

        integer(kind = int_wp) :: iseg         !    local loop counter for computational element loop
        real(kind = real_wp) :: z2           !    height bottom segment from bottom              (m)
        real(kind = real_wp) :: z1           !    height top segment from bottom                 (m)
        integer(kind = int_wp) :: ikmrk1
        integer(kind = int_wp) :: ikmrk2
        real(kind = real_wp) :: zm           !    watersurface to top macropyte                  (-)
        real(kind = real_wp) :: frlay        !    fraction witin layer                           (-)
        integer(kind = int_wp) :: iq           !    loop counter
        integer(kind = int_wp) :: ifrom        !    from segment
        integer(kind = int_wp) :: ito          !    from segment
        integer(kind = int_wp) :: iflux        !    index in the fl array
        real(kind = real_wp) :: pnvbxxup     !    2d uptake percentage n pool 1 and 2            (-)
        real(kind = real_wp) :: ppvbxxup     !    2d uptake percentage p pool 1 and 2            (-)
        real(kind = real_wp) :: psvbxxup     !    2d uptake percentage s pool 1 and 2            (-)
        real(kind = real_wp) :: hroot        !    effective root length                          (m)
        real(kind = real_wp) :: hsed         !    total thickness of the sediment layers         (m)
        real(kind = real_wp) :: factor       !    auxiliary variable

        integer(kind = int_wp), parameter :: npnt = 31            ! number of pointers
        integer(kind = int_wp) :: ipnt(npnt)           ! local work array for the pointering
        integer(kind = int_wp) :: ibotseg              ! bottom segment for macrophyte
        integer(kind = int_wp) :: ilay                 ! index into layers

        real(kind = real_wp) :: hcum(0:nolay)

        ! accumulate mass in the rooting zone in the pool of the bottom segment

        hsed = sum(dl)
        hcum(0) = 0.0
        do ilay = 1, nolay
            hcum(ilay) = hcum(ilay - 1) + dl(ilay)
        enddo

        ipnt = ipoint(1:npnt)
        iflux = 0
        do iseg = 1, noseg

            fn1vbxxupy = 0.0
            fn2vbxxupy = 0.0
            fp1vbxxupy = 0.0
            fp2vbxxupy = 0.0
            fs1vbxxupy = 0.0
            fs2vbxxupy = 0.0
            fl_fn1vbxxupy = 0.0
            fl_fn2vbxxupy = 0.0
            fl_fp1vbxxupy = 0.0
            fl_fp2vbxxupy = 0.0
            fl_fs1vbxxupy = 0.0
            fl_fs2vbxxupy = 0.0

            s1_fn1vbxxupy = 0.0
            s1_fn2vbxxupy = 0.0
            s1_fp1vbxxupy = 0.0
            s1_fp2vbxxupy = 0.0
            s1_fs1vbxxupy = 0.0
            s1_fs2vbxxupy = 0.0

            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)

            ibotseg = NINT(pmsa(ipnt(4)))
            inicovvbxx = pmsa(ipoint(19) + (ibotseg - 1) * increm(19)) / 100.

            if (inicovvbxx > 0.001) then

                depth = pmsa(ipnt(1))
                totaldepth = pmsa(ipnt(2))
                localdepth = pmsa(ipnt(3))
                hmax = pmsa(ipnt(5))
                delt = pmsa(ipnt(6))
                nh4 = pmsa(ipnt(7))
                no3 = pmsa(ipnt(8))
                aap = pmsa(ipnt(9))
                po4 = pmsa(ipnt(10))
                so4 = pmsa(ipnt(11))
                sud = pmsa(ipnt(12))
                s1_nh4 = pmsa(ipnt(13))
                s1_no3 = pmsa(ipnt(14))
                s1_aap = pmsa(ipnt(15))
                s1_po4 = pmsa(ipnt(16))
                s1_so4 = pmsa(ipnt(17))
                s1_sud = pmsa(ipnt(18))

                vbxxnavail = pmsa(ipoint(20) + (ibotseg - 1) * increm(20))
                vbxxpavail = pmsa(ipoint(21) + (ibotseg - 1) * increm(21))
                vbxxsavail = pmsa(ipoint(22) + (ibotseg - 1) * increm(22))
                fnvbxxup = pmsa(ipoint(23) + (ibotseg - 1) * increm(23))
                fpvbxxup = pmsa(ipoint(24) + (ibotseg - 1) * increm(24))
                fsvbxxup = pmsa(ipoint(25) + (ibotseg - 1) * increm(25))

                ! percentage uptake = uptake/available/percentage coverage

                if (vbxxnavail > 1e-20) then
                    pNvbxxup = fNvbxxup * delt / vbxxNavail
                    !              pn5vbxxup=fn5vbxxup*delt/vbxxnavail
                else
                    pNvbxxup = 0.0
                    !              pn5vbxxup=0.0
                endif
                if (vbxxpavail > 1e-20) then
                    pPvbxxup = fPvbxxup * delt / vbxxPavail
                    !              pp5vbxxup=fp5vbxxup*delt/vbxxpavail
                else
                    pPvbxxup = 0.0
                    !              pp5vbxxup=0.0
                endif
                if (vbxxsavail > 1e-20) then
                    pSvbxxup = fSvbxxup * delt / vbxxSavail
                    !              ps5vbxxup=fs5vbxxup*delt/vbxxsavail
                else
                    pSvbxxup = 0.0
                    !              ps5vbxxup=0.0
                endif

                if (ikmrk1==1 .and. hmax > 0.0) then

                    ! active water segment

                    hmax = min(hmax, totaldepth)
                    zm = totaldepth - hmax
                    z1 = localdepth - depth
                    z2 = localdepth

                    if (zm > z2) then
                        ! not in segment:
                    elseif (zm < z1) then
                        ! partialy in segment:
                        frlay = (z2 - zm) / depth
                        fN1vbxxupy = pNvbxxup * nh4 * frlay * depth / delt
                        fN2vbxxupy = pNvbxxup * no3 * frlay * depth / delt
                        fP1vbxxupy = pPvbxxup * aap * frlay * depth / delt
                        fP2vbxxupy = pPvbxxup * po4 * frlay * depth / delt
                        fS1vbxxupy = pSvbxxup * so4 * frlay * depth / delt
                        fS2vbxxupy = pSvbxxup * sud * frlay * depth / delt
                    else
                        ! completely in segment:
                        fN1vbxxupy = pNvbxxup * nh4 * depth / delt
                        fN2vbxxupy = pNvbxxup * no3 * depth / delt
                        fP1vbxxupy = pPvbxxup * aap * depth / delt
                        fP2vbxxupy = pPvbxxup * po4 * depth / delt
                        fS1vbxxupy = pSvbxxup * so4 * depth / delt
                        fS2vbxxupy = pSvbxxup * sud * depth / delt
                    endif
                endif

                ! Alternative layered sediment - the segment might be dry or wet, does not matter

                if (ikmrk1 /= 3 .and. (ikmrk2 == 0 .or. ikmrk2 == 3) .and. hmax < 0.0) then
                    hroot = min(-hmax, hsed)
                    s1_fN1vbxxupy = pNvbxxup * s1_nh4 / delt      ! removed factor hroot ...
                    s1_fN2vbxxupy = pNvbxxup * s1_no3 / delt
                    s1_fP1vbxxupy = pPvbxxup * s1_aap / delt
                    s1_fP2vbxxupy = pPvbxxup * s1_po4 / delt
                    s1_fS1vbxxupy = pSvbxxup * s1_so4 / delt
                    s1_fS2vbxxupy = pSvbxxup * s1_sud / delt

                    ! Take this from the nutrient pool in the sediment

                    do ilay = 1, nolay
                        if (hcum(ilay) <= hroot) then
                            factor = delt
                        else
                            factor = 0.0
                        endif
                        sedconc(ilay, is_NH4, ibotseg) = sedconc(ilay, is_NH4, ibotseg) - s1_fN1vbxxupy * factor
                        sedconc(ilay, is_NO3, ibotseg) = sedconc(ilay, is_NO3, ibotseg) - s1_fN2vbxxupy * factor
                        sedconc(ilay, is_AAP, ibotseg) = sedconc(ilay, is_AAP, ibotseg) - s1_fP1vbxxupy * factor
                        sedconc(ilay, is_PO4, ibotseg) = sedconc(ilay, is_PO4, ibotseg) - s1_fP2vbxxupy * factor
                        sedconc(ilay, is_SO4, ibotseg) = sedconc(ilay, is_SO4, ibotseg) - s1_fS1vbxxupy * factor
                        sedconc(ilay, is_SUD, ibotseg) = sedconc(ilay, is_SUD, ibotseg) - s1_fS2vbxxupy * factor
                    enddo
                endif

                if (ikmrk1==3 .and. hmax < 0.0) then

                    ! delwaq-g segment, distribution over the bottom segments

                    hmax = -hmax
                    hmax = min(hmax, totaldepth)
                    z1 = localdepth - depth

                    if (hmax > localdepth) then
                        ! completely in segment:
                        fN1vbxxupy = pNvbxxup * nh4 * depth / delt
                        fN2vbxxupy = pNvbxxup * no3 * depth / delt
                        fP1vbxxupy = pPvbxxup * aap * depth / delt
                        fP2vbxxupy = pPvbxxup * po4 * depth / delt
                        fS1vbxxupy = pSvbxxup * so4 * depth / delt
                        fS2vbxxupy = pSvbxxup * sud * depth / delt
                    elseif (hmax > z1) then
                        ! partialy in segment:
                        frlay = (hmax - z1) / depth
                        fN1vbxxupy = pNvbxxup * nh4 * frlay * depth / delt
                        fN2vbxxupy = pNvbxxup * no3 * frlay * depth / delt
                        fP1vbxxupy = pPvbxxup * aap * frlay * depth / delt
                        fP2vbxxupy = pPvbxxup * po4 * frlay * depth / delt
                        fS1vbxxupy = pSvbxxup * so4 * frlay * depth / delt
                        fS2vbxxupy = pSvbxxup * sud * frlay * depth / delt
                    else
                        ! not in segment:
                    endif

                endif

                fl_fn1vbxxupy = fn1vbxxupy / depth
                fl_fn2vbxxupy = fn2vbxxupy / depth
                fl_fp1vbxxupy = fp1vbxxupy / depth
                fl_fp2vbxxupy = fp2vbxxupy / depth
                fl_fs1vbxxupy = fs1vbxxupy / depth
                fl_fs2vbxxupy = fs2vbxxupy / depth

            endif

            !
            ! Note: for the alternative layered sediment approach,
            !       the nutrients are extracted directly from the storage in the bottom
            !       but we need to show the total amount in the output
            !
            pmsa(ipnt(26)) = fn1vbxxupy + s1_fn1vbxxupy
            pmsa(ipnt(27)) = fn2vbxxupy + s1_fn2vbxxupy
            pmsa(ipnt(28)) = fp1vbxxupy + s1_fp1vbxxupy
            pmsa(ipnt(29)) = fp2vbxxupy + s1_fp2vbxxupy
            pmsa(ipnt(30)) = fs1vbxxupy + s1_fs1vbxxupy
            pmsa(ipnt(31)) = fs2vbxxupy + s1_fs2vbxxupy
            fl(iflux + 1) = fl_fn1vbxxupy
            fl(iflux + 2) = fl_fn2vbxxupy
            fl(iflux + 3) = fl_fp1vbxxupy
            fl(iflux + 4) = fl_fp2vbxxupy
            fl(iflux + 5) = fl_fs1vbxxupy
            fl(iflux + 6) = fl_fs2vbxxupy

            ipnt = ipnt + increm(1:npnt)
            iflux = iflux + noflux

        enddo

        return
    end

end module m_veg3du
