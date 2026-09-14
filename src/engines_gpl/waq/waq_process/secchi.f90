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
module m_secchi
    use m_waq_precision

    implicit none

contains


    subroutine secchi (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Calculation secchi depth for visible-light (370-680nm)

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                              Units
        ! ----    --- -  -    -------------------                      ----
        ! EXT     R*4 1 I total extinction coefficient                   [1/m]
        ! AIM1    R*4 1 I inorganic suspended matter 1                  [g/m3]
        ! AIM2    R*4 1 I inorganic suspended matter 2                  [g/m3]
        ! AIM3    R*4 1 I inorganic suspended matter 3                  [g/m3]
        ! POC1    R*4 1 I fast decomposing detritus                    [gC/m3]
        ! POC2    R*4 1 I medium decomposing detritus                  [gC/m3]
        ! POC3    R*4 1 I slow decomposing detritus                    [gC/m3]
        ! POC4    R*4 1 I refractory detritus                          [gC/m3]
        ! AH_380  R*4 1 I extinction of dissolved organic matter         [1/m]
        ! CHLORP  R*4 1 I chlorophyll-a concentration                  [mg/m3]
        ! SW_UIT  R*4 1 I extinction by UITZUCHT on (1) or Off (0)         [-]
        ! DIEP1   R*4 1 I argument UITZICHT
        ! DIEP2   R*4 1 I argument UITZICHT
        ! CORCHL  R*4 1 I argument UITZICHT
        ! C_DET   R*4 1 I argument UITZICHT
        ! C_GL1   R*4 1 I argument UITZICHT
        ! C_GL2   R*4 1 I argument UITZICHT
        ! HELHUM  R*4 1 I argument UITZICHT
        ! TAU     R*4 1 I argument UITZICHT
        ! ANGLE   R*4 1 I argument UITZICHT
        ! DETCDM  R*4 1 I dry matter carbon ratio detritus               [g/g]
        ! PAC     R*4 1 I Poole-Atkins constant                            [-]
        ! SECCHI  R*4 1 O secchi depth                                     [m]
        !
        !     Logical Units : -
        !
        !     Modules called : -
        !
        !     Name     Type   Library
        !     ------   -----  ------------
        !
        use m_uitzicht_spectrum

        integer, parameter :: num_basic = 23
        integer, parameter :: num_pmsa = num_basic + num_spectrum

        real(kind = real_wp)   :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(num_pmsa), increm(num_pmsa)
        integer(kind = int_wp) :: num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        integer(kind = int_wp) :: ip(num_pmsa)
        integer(kind = int_wp) :: iflux, iseg

        real(kind = dp)        :: ah_380, ext, pac, secch, aim1, aim2, aim3, &
                poc1, poc2, poc3, poc4, chlorp, diep1, diep2, &
                corchl, c_det, c_gl1, c_gl2, helhum, tau, angle, &
                detcdm, gloeir, detric, extio, extp_d, d_1
        integer(kind = int_wp) ::  sw_uitz, sw_uit3, dosecc

        real(kind = dp), dimension(num_spectrum) :: DaylightPlanck
        real(kind = dp), dimension(num_spectrum) :: Spectrum

        ip = ipoint
        iflux = 0

        dosecc = 0 ! Detail concerning the "representative" depth
        swspec = 0

        do i = 1,num_spectrum
            DaylightPlanck(i) = pmsa(ipoint(num_basic+i))
        end do

        do iseg = 1, num_cells
            if (btest(iknmrk(iseg), 0)) then
                !
                sw_uitz = process_space_real(ip(11))
                if (nint(sw_uitz) == 0) then
                    !
                    !  calculate secchi depth without uitzicht
                    !
                    ext = process_space_real(ip(1))
                    pac = process_space_real(ip(22))
                    if (ext > 0.0) then
                        secch = pac / ext
                    else
                        secch = -999.
                    endif

                else
                    !
                    !  calculate secchi depth with uitzicht
                    !
                    aim1 = process_space_real(ip(2))
                    aim2 = process_space_real(ip(3))
                    aim3 = process_space_real(ip(4))
                    poc1 = process_space_real(ip(5))
                    poc2 = process_space_real(ip(6))
                    poc3 = process_space_real(ip(7))
                    poc4 = process_space_real(ip(8))
                    ah_380 = process_space_real(ip(9))
                    chlorp = process_space_real(ip(10))
                    diep1 = process_space_real(ip(12))
                    diep2 = process_space_real(ip(13))
                    corchl = process_space_real(ip(14))
                    c_det = process_space_real(ip(15))
                    c_gl1 = process_space_real(ip(16))
                    c_gl2 = process_space_real(ip(17))
                    helhum = process_space_real(ip(18))
                    tau = process_space_real(ip(19))
                    angle = process_space_real(ip(20))
                    detcdm = process_space_real(ip(21))
                    sw_uit3 = int(process_space_real(ip(23)))

                    detric = max (0.0, detcdm * (poc1 + poc2 + poc3 + poc4))
                    gloeir = aim1 + aim2 + aim3
                    !
                    !  calculate total extinction with uitzicht
                    !
                    call uit_zi(diep1, diep2, angle, c_gl1, c_gl2, &
                            c_det, helhum, tau, corchl, chlorp, &
                            detric, gloeir, ah_380, secch, d_1, &
                            extio, extp_d, .dosecc, Spectrum, swspec, sw_uit3 )

                endif

                process_space_real(ip(23)) = secch

            endif

            iflux = iflux + noflux
            ip = ip + increm

        end do
    end subroutine secchi

end module m_secchi
