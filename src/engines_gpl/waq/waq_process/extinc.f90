!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
module m_extinc
    use m_waq_precision

    implicit none

contains


    subroutine extinc (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Calculation total and partial extinction coefficients

        !
        !     This module calculates the total and partial extinction coeffcients,
        !     optionally with additional module Spectral Light Attenuation (former UITZICHT)
        !
        !!  Two options: lineair and SLA (uitzicht)
        !!  SLA requires an outer loop over the horizontal segments followed by an inner loop over all segments in the underlyaing column (top2bottom)
        !!  which allows the passing of the spectral array from top to bottom within this routine
        !!  The linear extinction (not spectral) all segments are equal and follows the ordinary segmentloop
        !!
        !!  Subroutine EXTINC is restructured a bit in anticipation of additional (external) light modules (Inform project) keeping one table (interface only)
        !!  Nieuwe lichtmodellen leiden tot uitbreiding van EXINC interface???
        !!  Now there are two (LinearExt en UITZICHT).

        use m_uitzicht_spectrum
        use m_dhnolay

        implicit none
        !

        integer, parameter :: num_basic  = 41
        integer, parameter :: num_process_space_real   = num_basic + num_spectrum
        integer, parameter :: num_offset = num_process_space_real  - 18

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(num_process_space_real), increm(num_process_space_real), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local declaration
        !
        real(kind = dp) :: a1      !  i specific ext. inorganic suspended matter 1  [m2/gdm]
        real(kind = dp) :: a2      !  i specific ext. inorganic suspended matter 2  [m2/gdm]
        real(kind = dp) :: a3      !  i specific ext. inorganic suspended matter 3  [m2/gdm]
        real(kind = dp) :: ext     !  o total extinction                               [1/m]
        real(kind = dp) :: extim   !  o calculated extinction im                       [1/m]
        real(kind = dp) :: extpoc  !  o extinction poc                                 [1/m]
        real(kind = dp) :: extdoc  !  o extinction doc                                 [1/m]
        real(kind = dp) :: ext0    !  i background extinction                          [1/m]
        real(kind = dp) :: extbl   !  i extinction algae (bloom)                       [1/m]
        real(kind = dp) :: extdyn  !  i extinction algae (dynamo)                      [1/m]
        real(kind = dp) :: extpro  !  i extinction algae (protist)                     [1/m]
        real(kind = dp) :: extalg  !  i extinction algae                                [1/m]
        real(kind = dp) :: extmac  !  i extinction macrophytes                         [1/m]
        real(kind = dp) :: extsal  !  o extinction doc for fresh water fraction        [1/m]
        real(kind = dp) :: aim1    !  i suspended solids  fraction 1                [gdm/m3]
        real(kind = dp) :: aim2    !  i suspended solids  fraction 2                [gdm/m3]
        real(kind = dp) :: aim3    !  i suspended solids  fraction 3                [gdm/m3]
        real(kind = dp) :: poc1    !  i fast decomposing detritus                    [gc/m3]
        real(kind = dp) :: poc2    !  i medium decomposing detritus                  [gc/m3]
        real(kind = dp) :: poc3    !  i slow decomposing detritus                    [gc/m3]
        real(kind = dp) :: poc4    !  i refractory detritus                          [gc/m3]
        integer(kind = int_wp) :: sw_uit  ! extinction by uitzicht on (1) or off (0)              [-]
        real(kind = dp) :: doc     !  i dissolved organic carbon                     [gc/m3]
        real(kind = dp) :: adoc    !  i specific extinction of doc                   [m2/gc]
        real(kind = dp) :: diep1   !  i argument uitzicht
        real(kind = dp) :: diep2   !  i argument uitzicht
        real(kind = dp) :: corchl  !  i argument uitzicht
        real(kind = dp) :: c_det   !  i argument uitzicht
        real(kind = dp) :: c_gl1   !  i argument uitzicht
        real(kind = dp) :: c_gl2   !  i argument uitzicht
        real(kind = dp) :: helhum  !  i argument uitzicht
        real(kind = dp) :: tau     !  i argument uitzicht
        real(kind = dp) :: angle   !  i argument uitzicht
        real(kind = dp) :: detcdm  !  i dry matter carbon ratio detritus              [g/g]
        real(kind = dp) :: xtsal0  !  i extra vl extinction at salinity = 0           [1/m]
        real(kind = dp) :: salmax  !  i salinity value for extra extinction = 0      [g/kg]
        real(kind = dp) :: salin   !  i actual salinity                              [g/kg]
        real(kind = dp) :: apoc1   !  i specific extintion poc1                [1/m/(g/m3)]
        real(kind = dp) :: apoc2   !  i specific extintion poc2                [1/m/(g/m3)]
        real(kind = dp) :: apoc3   !  i specific extintion poc3                [1/m/(g/m3)]
        real(kind = dp) :: apoc4   !  i specific extintion poc4                [1/m/(g/m3)]

        real(kind = dp) :: chlorp, detric, gloeir, ah_380, chlorophyl
        real(kind = dp) :: secchi, d_1, extp_d, extdet, extgl, exthum, exth2o, extchl, extdoc2, dummy
        real(kind = dp) :: depth, localdepth, sechor1, sechor2, ext_last, sechor1_last, sechor2_last
        real(kind = dp) :: secver, secvermax, zthreshold
        integer(kind = int_wp) :: iflux, iseg, nosegl, nosegw, nolay, ihseg, i, ilay, ikmrk1, sw_uit3


        integer(kind = int_wp) :: ipnt(num_process_space_real)
        integer(kind = int_wp), save :: nr_mes = 0

        real(kind = dp), dimension(num_spectrum) :: DaylightPlanck
        real(kind = dp), dimension(num_spectrum) :: SpectrumTop, SpectrumBot

        ipnt = ipoint
        iflux = 0

        sw_uit    = nint(process_space_real(ipnt(13)))

        if ( sw_uit == 0 ) then

            exth2o = -999.0
            extchl = -999.0

            do iseg = 1, num_cells

                if (btest(iknmrk(iseg), 0)) then

                    call local_values

                    !
                    !  calculate extinction coefficients - no UITZICHT
                    !
                    extim = a1 * aim1 + a2 * aim2 + a3 * aim3
                    extpoc = apoc1 * poc1 + apoc2 * poc2 + apoc3 * poc3 + apoc4 * poc4
                    extdoc = adoc * doc
                    extalg = extbl + extdyn + extpro
                    salin = min(salin, salmax)
                    salin = max(salin, 0.0_dp)
                    extsal = xtsal0 * (1.0 - salin / salmax)
                    ext = ext0 + extim + extpoc + extdoc + extalg + extmac &
                            + extsal
                    !
                    if (ext < 1.0e-20) then
                        if (nr_mes < 25) then
                            nr_mes = nr_mes + 1
                            write(*, *) ' Warning : zero or negative extinction'
                            write(*, *) ' Extinction due to inorganic matter:', extim
                            write(*, *) ' Extinction due to organic matter  :', extpoc
                            write(*, *) ' Extinction due to algae           :', extalg
                            write(*, *) ' Background extinction             :', ext0
                            write(*, *) ' Extinction by macrophytes         :', extmac
                            write(*, *) ' in segment number                 :', iseg
                            write(*, *) ' Background extinction is assumed.'
                        endif
                        if (nr_mes == 25) then
                            nr_mes = nr_mes + 1
                            write(*, *) ' 25 warnings on extinction'
                            write(*, *) ' Further messages on extinction surpressed'
                        endif
                        if (ext0 < 1.e-20) then
                            ext = 1.e-15
                        else
                            ext = ext0
                        endif
                    endif

                    process_space_real(ipnt(num_offset+1)) = ext
                    process_space_real(ipnt(num_offset+2)) = extim
                    process_space_real(ipnt(num_offset+3)) = extpoc
                    process_space_real(ipnt(num_offset+4)) = extdoc
                    process_space_real(ipnt(num_offset+5)) = extalg
                    process_space_real(ipnt(num_offset+6)) = extsal
                    process_space_real(ipnt(num_offset+7)) = exth2o
                    process_space_real(ipnt(num_offset+8)) = extchl
                    process_space_real(ipnt(num_offset+9)) = d_1

                    do i = num_offset+10,num_process_space_real
                        process_space_real(ipnt(i)) = -999.0
                    enddo
                endif

                iflux = iflux + noflux
                ipnt = ipnt + increm
            enddo

        else

            !! AM: Is dit echt nodig? Kunnen we het spectrum niet gewoon uit de module halen?


            do i = 1,num_spectrum
                DaylightPlanck(i) = process_space_real(ipoint(num_basic+i))
            end do

            call dhnolay(nolay)
            nosegl = num_cells / nolay

            do ihseg = 1,nosegl
                SpectrumTop = DaylightPlanck
                SpectrumBot = DaylightPlanck

                localdepth = process_space_real(ipoint(35)+(ihseg+(nolay-1)*nosegl-1)*increm(35))
                secvermax  = localdepth

                ! Loop over the column starting with this segment (MJ: improement: use columns module from JvB)
                do ilay = 1, nolay
                    iseg = ihseg + (ilay-1) * nosegl

                    if (btest(iknmrk(iseg), 0)) then
                        ipnt = ipoint + (iseg-1) * increm

                        !
                        !  calculate extinction coefficients - with UITZICHT
                        !
                        chlorp = 0.0
                        detric = max (0.0_dp, detcdm * (poc1 + poc2 + poc3 + poc4))
                        ah_380 = doc * adoc
                        gloeir = aim1 + aim2 + aim3

                        !  Uitzicht requires a depth interval to calculate extinction characteristic for segment
                        !  Uizicht uses two depht, we do not want to use the cell boundaries (localdepth and localdepth-depth)
                        !  Best estimate of the raaklijn of the Extinction curve is small delta depth in middle of the segment
                        !  So determine diep1 and diep2 on interval of 0.1cm centrally around middle of the cell
                        !
                        !  we (mis)use SW_UIT3 to choose the switchg for the calculation of the KD
                        !     0   = do not use uizicht
                        !     <>0 = use uitzicht
                        !     1: Buiteveld z1%
                        !     2: Bbuiteveld z10%
                        !     3: Lee with zenith angle 30 eq 4  using 4x m
                        !     4: Lee with zenith angle 30 eq 5  using 4x n
                        !     5: Lee simplified eq 6 with 2 coeffs
                        !     6: Nechad and RUddick 2010
                        !

                        if ( sw_uit3 == 2 ) then
                            diep1 = max(localdepth-0.5*depth-0.01,localdepth-depth)
                            diep2 = min(localdepth-0.5*depth+0.01,localdepth)
                        elseif ( sw_uit3 == 1 ) then
                            diep1 = localdepth-depth
                            diep2 = localdepth
                        endif

                        !
                        !  SpectrumBot only calculated once per segment in a column and only with
                        !  ALL OAS present (full spectrum)
                        !

                        call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,         &
                                     c_det , helhum, tau   , corchl, chlorophyl,     &
                                     detric, gloeir, ah_380, sechor1, d_1   ,        &
                                     ext   , extp_d, 1     , spectrumbot,1,sw_uit3)

                        !  Repeated with different secchi calculation method
                        call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,         &
                                     c_det , helhum, tau   , corchl, chlorophyl,     &
                                     detric, gloeir, ah_380, sechor2, d_1   ,        &
                                     ext   , extp_d, 2     , spectrumtop,0,sw_uit3)

                        !
                        !  Contribution of individual OAS only when EXT calc is successfull (>-1)
                        !  if EXT with all OAS does not give result, keep all same as above layer
                        !
                        if (ilay == 1) then
                            ext_last = ext
                            sechor1_last = sechor1
                            sechor2_last = sechor2
                        endif

                        if (ext > -0.5) then
                            ext_last = ext
                            sechor1_last = sechor1
                            sechor2_last = sechor2

                            !
                            !  Total extinction coefficient of Chlfa (algae)
                            !
                            call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,      &
                                      c_det , helhum, tau   , corchl, 0.0_dp,         &
                                      detric, gloeir, ah_380, dummy, dummy,           &
                                      extchl, extp_d, 0     , spectrumtop,0,sw_uit3)
                            extchl= ext - extchl

                            !
                            !  Total extinction coefficient minus of detritus
                            !
                            call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,      &
                                      c_det , helhum, tau   , corchl, chlorophyl,     &
                                      0.0_dp, gloeir, ah_380, dummy, dummy,           &
                                      extdet, extp_d, 0     , spectrumtop,0,sw_uit3)
                            extdet = ext - extdet

                            !
                            !  Total extinction coefficient of inorganic sediment
                            !
                            call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,      &
                                      c_det , helhum, tau   , corchl, chlorophyl,     &
                                      detric, 0.0_dp, ah_380, dummy, dummy,           &
                                      extgl , extp_d, 0     , spectrumtop,0,sw_uit3)
                            extgl  = ext - extgl

                            !
                            !  Total extinction coefficient of DOC (humic acids)
                            !
                            call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,      &
                                      c_det , helhum, tau   , corchl, chlorophyl,     &
                                      detric, gloeir, 0.0_dp, dummy, dummy,           &
                                      extdoc, extp_d, 0     , spectrumtop,0,sw_uit3)
                            extdoc = ext - extdoc

                            !
                            !  Pure wate extinction  - not used, research only
                            !
                            call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,      &
                                     c_det , helhum, tau   , corchl, 0.0_dp,          &
                                     0.0_dp, 0.0_dp, 0.0_dp, dummy,  d_1   ,          &
                                     exth2o, extp_d  ,0    , spectrumtop,0,sw_uit3)

                            !
                            !  AH380 & water extinction / if linear same as EXTHUM - not used researh only
                            !
                            call uit_zi( diep1 , diep2 , angle , c_gl1 , c_gl2 ,      &
                                      c_det ,  helhum, tau   , corchl, 0.0_dp,        &
                                      0.0_dp,  0.0_dp, ah_380, dummy,  dummy,         &
                                      extdoc2, extp_d ,0     , spectrumtop,0,sw_uit3)

                            extdoc2 = extdoc2 - exth2o

                        else
                            ext = ext_last
                            sechor1 = sechor1_last
                            sechor2 = sechor2_last
                        endif ! valid EXT value (>-1)

                        !
                        !  Copy spectrum with all OAS to top of next layer
                        !
                        SpectrumTop = SpectrumBot

                        !
                        !  Determnine the column sechhi depth (named SECVer) from evaluation of
                        !  horizontal zicht diepte per segment (SECHor)
                        !  Secchi depth cannot exceed local depth!
                        !  Secchi depth column is limited by smallest SECHor of the above segments
                        !

                        if (sechor1 > -0.5) then
                           !secvermax = min(secvermax, sechor1)
                           if (sechor1 >= depth) then
                               if (sechor1 >= localdepth) then
                                  secver =secver + depth
                               elseif ( sechor1 >= (localdepth - depth) ) then
                                  secver = secver + sechor1 -localdepth + depth
                               endif
                           endif
                           !secver=min(secvermax,secver)
                        endif

                        extim  =  extgl
                        extpoc =  extdet
                        extsal =  0.0
                        ext    =  ext + ext0 + extalg + extmac
                    endif

                    process_space_real(ipnt(num_offset+1)) = ext
                    process_space_real(ipnt(num_offset+2)) = extim
                    process_space_real(ipnt(num_offset+3)) = extpoc
                    process_space_real(ipnt(num_offset+4)) = extdoc
                    process_space_real(ipnt(num_offset+5)) = extalg
                    process_space_real(ipnt(num_offset+6)) = extsal
                    process_space_real(ipnt(num_offset+7)) = exth2o
                    process_space_real(ipnt(num_offset+8)) = extchl
                    process_space_real(ipnt(num_offset+9)) = d_1

                    process_space_real(ipnt(num_offset+10)) = spectrumbot(61) !700
                    process_space_real(ipnt(num_offset+11)) = spectrumbot(45) !620
                    process_space_real(ipnt(num_offset+11)) = spectrumbot(31) !550
                    process_space_real(ipnt(num_offset+12)) = spectrumbot(23) !510
                    process_space_real(ipnt(num_offset+13)) = spectrumbot(13) !460
                    process_space_real(ipnt(num_offset+14)) = spectrumbot(1 ) !400
                    process_space_real(ipnt(num_offset+15)) = sechor1
                    process_space_real(ipnt(num_offset+16)) = sechor2
                    process_space_real(ipnt(num_offset+17)) = secver
                enddo
            enddo
        endif

    contains
    subroutine local_values

        a1 = process_space_real(ipnt(1))
        a2 = process_space_real(ipnt(2))
        a3 = process_space_real(ipnt(3))
        apoc1 = process_space_real(ipnt(4))
        ext0 = process_space_real(ipnt(5))
        extbl = process_space_real(ipnt(6))
        extdyn = process_space_real(ipnt(7))
        extpro = process_space_real(ipnt(8))
        extmac = process_space_real(ipnt(9))
        aim1 = process_space_real(ipnt(10))
        aim2 = process_space_real(ipnt(11))
        aim3 = process_space_real(ipnt(12))
        poc1 = process_space_real(ipnt(13))
        poc2 = process_space_real(ipnt(14))
        sw_uit = nint(process_space_real(ipnt(15)))
        doc = process_space_real(ipnt(16))
        adoc = process_space_real(ipnt(17))
        diep1 = process_space_real(ipnt(18))
        diep2 = process_space_real(ipnt(19))
        corchl = process_space_real(ipnt(20))
        c_det = process_space_real(ipnt(21))
        c_gl1 = process_space_real(ipnt(22))
        c_gl2 = process_space_real(ipnt(23))
        helhum = process_space_real(ipnt(24))
        tau = process_space_real(ipnt(25))
        angle = process_space_real(ipnt(26))
        detcdm = process_space_real(ipnt(27))
        xtsal0 = process_space_real(ipnt(28))
        salin = process_space_real(ipnt(29))
        salmax = process_space_real(ipnt(30))
        apoc2 = process_space_real(ipnt(31))
        apoc3 = process_space_real(ipnt(32))
        apoc4 = process_space_real(ipnt(33))
        poc3 = process_space_real(ipnt(34))
        poc4 = process_space_real(ipnt(35))

        depth      = process_space_real(ipnt(36))
        localdepth = process_space_real(ipnt(37))
        sw_uit3    = process_space_real(ipnt(38))
        zthreshold = process_space_real(ipnt(39))
        chlorophyl = max(process_space_real(ipnt(40)),0.0)

    end subroutine local_values

    end subroutine extinc

end module m_extinc
