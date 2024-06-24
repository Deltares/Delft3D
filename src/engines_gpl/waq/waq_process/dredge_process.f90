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
module m_dredge_process
    use m_waq_precision

    implicit none

contains


    subroutine dredge_process     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use m_extract_waq_attribute

        implicit none

        ! declaration of the arguments

        real(kind = real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)   ! I  Array of pointers in PMSA to get and store the data
        integer(kind = int_wp) :: increm(*)   ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the FL array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        integer(kind = int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

        ! variables from the pmsa array

        integer(kind = int_wp) :: max_basin                        ! I
        integer(kind = int_wp) :: basin_no                         ! I
        integer(kind = int_wp) :: no_basin                         ! I
        real(kind = real_wp) :: actths1                          ! I
        real(kind = real_wp) :: actths2                          ! I
        real(kind = real_wp) :: volume                           ! I
        real(kind = real_wp) :: surf                             ! I
        real(kind = real_wp) :: delt                             ! I
        integer(kind = int_wp) :: itime                            ! I
        integer(kind = int_wp) :: idt                              ! I
        integer(kind = int_wp) :: nim1                             ! I number of fractions IM1
        integer(kind = int_wp) :: nim2                             ! I number of fractions IM2
        integer(kind = int_wp) :: nim3                             ! I number of fractions IM3
        integer(kind = int_wp) :: nim1s1                           ! I number of fractions IM1S1
        real(kind = real_wp) :: im1s1                            ! I fractions IM1S1
        integer(kind = int_wp) :: nim2s1                           ! I number of fractions IM2S1
        real(kind = real_wp) :: im2s1                            ! I fractions IM1S1
        integer(kind = int_wp) :: nim3s1                           ! I number of fractions IM3S1
        real(kind = real_wp) :: im3s1                            ! I fractions IM3S1
        integer(kind = int_wp) :: nim1s2                           ! I number of fractions IM1S2
        real(kind = real_wp) :: im1s2                            ! I fractions IM1S2
        integer(kind = int_wp) :: nim2s2                           ! I number of fractions IM2S2
        real(kind = real_wp) :: im2s2                            ! I fractions IM1S2
        integer(kind = int_wp) :: nim3s2                           ! I number of fractions IM3S2
        real(kind = real_wp) :: im3s2                            ! I fractions IM3S2
        integer(kind = int_wp) :: it_start_dredge                  ! I it_start_dredge     per basin
        integer(kind = int_wp) :: it_freq_dredge                   ! I it_freq_dredge      per basin
        real(kind = real_wp) :: dredge_criterium                 ! I dredge_criterium    per basin
        integer(kind = int_wp) :: sws1s2_dredge                    ! I sws1s2_dredge       per basin
        integer(kind = int_wp) :: dumpsegment                      ! I dumpsegment         per basin
        real(kind = real_wp) :: dumpspeed                        ! I dumpspeed           per basin
        integer(kind = int_wp) :: relabel                          ! I relabel             per basin
        real(kind = real_wp) :: dredge_im1                       ! I/O storage of dredged im1 fractions per basin
        real(kind = real_wp) :: dredge_im2                       ! I/O storage of dredged im1 fractions per basin
        real(kind = real_wp) :: dredge_im3                       ! I/O storage of dredged im1 fractions per basin

        ! pointers in the pmsa array

        integer(kind = int_wp) :: ip_basin_no                      !
        integer(kind = int_wp) :: ip_actths1                       !
        integer(kind = int_wp) :: ip_actths2                       !
        integer(kind = int_wp) :: ip_volume                        !
        integer(kind = int_wp) :: ip_surf                          !
        integer(kind = int_wp) :: ip_delt                          !
        integer(kind = int_wp), dimension(:), allocatable :: ip0_im1s1                        !
        integer(kind = int_wp), dimension(:), allocatable :: ip0_im2s1                        !
        integer(kind = int_wp), dimension(:), allocatable :: ip0_im3s1                        !
        integer(kind = int_wp) :: ip_im1s1                         !
        integer(kind = int_wp) :: ip_im2s1                         !
        integer(kind = int_wp) :: ip_im3s1                         !
        integer(kind = int_wp), dimension(:), allocatable :: ip0_im1s2                        !
        integer(kind = int_wp), dimension(:), allocatable :: ip0_im2s2                        !
        integer(kind = int_wp), dimension(:), allocatable :: ip0_im3s2                        !
        integer(kind = int_wp) :: ip_im1s2                         !
        integer(kind = int_wp) :: ip_im2s2                         !
        integer(kind = int_wp) :: ip_im3s2                         !
        integer(kind = int_wp), dimension(:), allocatable :: ip_it_start_dredge               !
        integer(kind = int_wp), dimension(:), allocatable :: ip_it_freq_dredge                !
        integer(kind = int_wp), dimension(:), allocatable :: ip_dredge_criterium              !
        integer(kind = int_wp), dimension(:), allocatable :: ip_sws1s2_dredge                 !
        integer(kind = int_wp), dimension(:), allocatable :: ip_dumpsegment                   !
        integer(kind = int_wp), dimension(:), allocatable :: ip_dumpspeed                     !
        integer(kind = int_wp), dimension(:), allocatable :: ip_relabel                       !
        integer(kind = int_wp), dimension(:, :), allocatable :: ip0_dredge_im1                   !
        integer(kind = int_wp), dimension(:, :), allocatable :: ip0_dredge_im2                   !
        integer(kind = int_wp), dimension(:, :), allocatable :: ip0_dredge_im3                   !
        integer(kind = int_wp) :: ip_dredge_im1                    !
        integer(kind = int_wp) :: ip_dredge_im2                    !
        integer(kind = int_wp) :: ip_dredge_im3                    !
        integer(kind = int_wp) :: ipoff                            !
        integer(kind = int_wp) :: size_sum_dredge                      !
        real(kind = real_wp), dimension(:), allocatable :: sum_dredge

        integer(kind = int_wp) :: lunrep                           ! unit number of output file

        ! pointers in the flux array

        integer(kind = int_wp) :: iflux                            !
        integer(kind = int_wp) :: ipflux                           !
        integer(kind = int_wp) :: ifl_dump_im1                     !
        integer(kind = int_wp) :: ifl_dump_im2                     !
        integer(kind = int_wp) :: ifl_dump_im3                     !

        ! other local declarations

        real(kind = real_wp) :: dredge_tot       !
        real(kind = real_wp) :: fraction_dredge  !
        real(kind = real_wp) :: maxdump          !
        real(kind = real_wp) :: dump             !
        real(kind = real_wp) :: dump_im1         !
        real(kind = real_wp) :: dump_im2         !
        real(kind = real_wp) :: dump_im3         !
        integer(kind = int_wp) :: iseg             ! local loop counter for computational element loop
        integer(kind = int_wp) :: i_basin          ! local loop counter for basin
        integer(kind = int_wp) :: ikmrk2           ! second attribute
        logical, allocatable :: dredge_moment(:) ! indication per basin if the current step is a dredge step
        integer(kind = int_wp) :: ifrac_im1        ! fraction
        integer(kind = int_wp) :: ifrac_im2        ! fraction
        integer(kind = int_wp) :: ifrac_im3        ! fraction
        integer(kind = int_wp) :: ifrac_dump_im1   ! dump towards this fraction
        integer(kind = int_wp) :: ifrac_dump_im2   ! dump towards this fraction
        integer(kind = int_wp) :: ifrac_dump_im3   ! dump towards this fraction

        call get_log_unit_number(lunrep)

        ! initialise pointers in pmsa array

        max_basin = nint(pmsa(ipoint(1)))
        no_basin = nint(pmsa(ipoint(3)))

        ! initialisatie loop

        if (no_basin == -1) then
            no_basin = 0
            ip_basin_no = ipoint(2)
            do iseg = 1, noseg
                basin_no = nint(pmsa(ip_basin_no))
                if (basin_no > max_basin) then
                    write (lunrep, *) 'ERROR in dredge process'
                    write (lunrep, *) 'basin_no is greater than max_basin in dredge process'
                    write (*, *) 'ERROR in dredge process'
                    write (*, *) 'basin_no is greater than max_basin in dredge process'
                    call stop_with_error()
                endif
                no_basin = max(no_basin, basin_no)
                ip_basin_no = ip_basin_no + increm(2)
            enddo
            pmsa(ipoint(3)) = real(no_basin)
        endif

        ! if no basins then return

        if (no_basin == 0) return

        nim1 = nint(pmsa(ipoint(11)))
        nim2 = nint(pmsa(ipoint(12)))
        nim3 = nint(pmsa(ipoint(13)))
        nim1s1 = nint(pmsa(ipoint(14)))

        !     determine all pointers

        allocate(ip_it_start_dredge(max_basin), &
                ip_it_freq_dredge(max_basin), &
                ip_dredge_criterium(max_basin), &
                ip_sws1s2_dredge(max_basin), &
                ip_dumpsegment(max_basin), &
                ip_dumpspeed(max_basin), &
                ip_relabel(max_basin))
        allocate(ip0_dredge_im1(nim1, max_basin), &
                ip0_dredge_im2(nim2, max_basin), &
                ip0_dredge_im3(nim3, max_basin))
        allocate(ip0_im1s1(nim1), ip0_im2s1(nim2), ip0_im3s1(nim3))
        allocate(ip0_im1s2(nim1), ip0_im2s2(nim2), ip0_im3s2(nim3))

        do ifrac_im1 = 1, nim1
            ip0_im1s1(ifrac_im1) = 14 + ifrac_im1
        enddo

        nim2s1 = nint(pmsa(ipoint(14 + nim1s1 + 1)))
        do ifrac_im2 = 1, nim2
            ip0_im2s1(ifrac_im2) = 14 + nim1s1 + 1 + ifrac_im2
        enddo

        nim3s1 = nint(pmsa(ipoint(14 + nim1s1 + 1 + nim2s1 + 1)))
        do ifrac_im3 = 1, nim3
            ip0_im3s1(ifrac_im3) = 14 + nim1s1 + 1 + nim2s1 + 1 + ifrac_im3
        enddo

        ipoff = 17 + nim1s1 + nim2s1 + nim3s1

        nim1s2 = nint(pmsa(ipoint(ipoff)))
        do ifrac_im1 = 1, nim1
            ip0_im1s2(ifrac_im1) = ipoff + ifrac_im1
        enddo

        nim2s2 = nint(pmsa(ipoint(ipoff + nim1s2 + 1)))
        do ifrac_im2 = 1, nim2
            ip0_im2s2(ifrac_im2) = ipoff + nim1s2 + 1 + ifrac_im2
        enddo

        nim3s2 = nint(pmsa(ipoint(ipoff + nim1s2 + 1 + nim2s2 + 1)))
        do ifrac_im3 = 1, nim2
            ip0_im3s2(ifrac_im3) = ipoff + nim1s2 + 1 + nim2s2 + 1 + ifrac_im3
        enddo

        ipoff = 20 + nim1s1 + nim2s1 + nim3s1 + nim1s2 + nim2s2 + nim3s2

        do basin_no = 1, no_basin
            ip_it_start_dredge(basin_no) = ipoint(ipoff + basin_no - 1)
            ip_it_freq_dredge(basin_no) = ipoint(ipoff + 1 * max_basin + basin_no - 1)
            ip_dredge_criterium(basin_no) = ipoint(ipoff + 2 * max_basin + basin_no - 1)
            ip_sws1s2_dredge(basin_no) = ipoint(ipoff + 3 * max_basin + basin_no - 1)
            ip_dumpsegment(basin_no) = ipoint(ipoff + 4 * max_basin + basin_no - 1)
            ip_dumpspeed(basin_no) = ipoint(ipoff + 5 * max_basin + basin_no - 1)
            ip_relabel(basin_no) = ipoint(ipoff + 6 * max_basin + basin_no - 1)
            do ifrac_im1 = 1, nim1
                ip0_dredge_im1(basin_no, ifrac_im1) = (basin_no - 1) * nim1 + ifrac_im1
            enddo
            do ifrac_im2 = 1, nim2
                ip0_dredge_im2(basin_no, ifrac_im2) = max_basin * nim1 + (basin_no - 1) * nim2 + ifrac_im2
            enddo
            do ifrac_im3 = 1, nim3
                ip0_dredge_im3(basin_no, ifrac_im3) = max_basin * nim1 + max_basin * nim2 + (basin_no - 1) * nim3 + ifrac_im3
            enddo
        enddo

        !     copy sum_dredge (remaining dredge mass) from pmsa
        size_sum_dredge = max_basin * (nim1 + nim2 + nim3)
        allocate(sum_dredge(size_sum_dredge))
        sum_dredge = 0.0

        do i_basin = 1, no_basin
            do ifrac_im1 = 1, nim1
                sum_dredge(ip0_dredge_im1(i_basin, ifrac_im1)) = pmsa(ipoint(ipoff + 7 * max_basin + ip0_dredge_im1(i_basin, ifrac_im1)))
            enddo
            do ifrac_im2 = 1, nim2
                sum_dredge(ip0_dredge_im2(i_basin, ifrac_im2)) = pmsa(ipoint(ipoff + 7 * max_basin + ip0_dredge_im2(i_basin, ifrac_im2)))
            enddo
            do ifrac_im3 = 1, nim3
                sum_dredge(ip0_dredge_im3(i_basin, ifrac_im3)) = pmsa(ipoint(ipoff + 7 * max_basin + ip0_dredge_im3(i_basin, ifrac_im3)))
            enddo
        enddo
        ! check for each basin if it is a dredging moment

        itime = nint(pmsa(ipoint(9)))
        idt = nint(pmsa(ipoint(10)))
        allocate(dredge_moment(no_basin))
        dredge_moment = .false.
        do i_basin = 1, no_basin
            it_start_dredge = nint(pmsa(ip_it_start_dredge(i_basin)))
            it_freq_dredge = max(nint(pmsa(ip_it_freq_dredge(i_basin))), 1)
            if (itime >= it_start_dredge .and. &
                    mod(itime - it_start_dredge, it_freq_dredge) < idt) then
                dredge_moment(i_basin) = .true.
            endif
        enddo

        ! dredge loop

        ip_basin_no = ipoint(2)
        ip_actths1 = ipoint(4)
        ip_actths2 = ipoint(5)
        ip_volume = ipoint(6)
        ip_surf = ipoint(7)
        ip_delt = ipoint(8)

        iflux = 0
        do iseg = 1, noseg
            if (btest(iknmrk(iseg), 0)) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then
                    basin_no = nint(pmsa(ip_basin_no))
                    if (basin_no > 0) then
                        if (dredge_moment(basin_no)) then
                            dredge_criterium = pmsa(ip_dredge_criterium(basin_no))
                            sws1s2_dredge = nint(pmsa(ip_sws1s2_dredge(basin_no)))
                            actths1 = pmsa(ip_actths1)
                            actths2 = pmsa(ip_actths2)
                            volume = pmsa(ip_volume)
                            surf = pmsa(ip_surf)
                            delt = pmsa(ip_delt)
                            if (sws1s2_dredge == 1) then
                                if (actths1 > 1.e-15) then
                                    fraction_dredge = (actths1 - dredge_criterium) / actths1
                                    if (fraction_dredge > 0.0) then
                                        do ifrac_im1 = 1, nim1
                                            ip_im1s1 = ipoint(ip0_im1s1(ifrac_im1)) + (iseg - 1) * increm(ip0_im1s1(ifrac_im1))
                                            im1s1 = pmsa(ip_im1s1) * surf
                                            ip_dredge_im1 = ip0_dredge_im1(ifrac_im1, basin_no)

                                            sum_dredge(ip_dredge_im1) = sum_dredge(ip_dredge_im1) + im1s1 * fraction_dredge

                                            ipflux = iflux + ifrac_im1
                                            fl(ipflux) = im1s1 * fraction_dredge / volume / delt
                                        enddo
                                        do ifrac_im2 = 1, nim2
                                            ip_im2s1 = ipoint(ip0_im2s1(ifrac_im2)) + (iseg - 1) * increm(ip0_im2s1(ifrac_im2))
                                            im2s1 = pmsa(ip_im2s1) * surf
                                            ip_dredge_im2 = ip0_dredge_im2(ifrac_im2, basin_no)
                                            sum_dredge(ip_dredge_im2) = sum_dredge(ip_dredge_im2) + im2s1 * fraction_dredge
                                            ipflux = iflux + nim1 + ifrac_im2
                                            fl(ipflux) = im2s1 * fraction_dredge / volume / delt
                                        enddo
                                        do ifrac_im3 = 1, nim3
                                            ip_im3s1 = ipoint(ip0_im3s1(ifrac_im3)) + (iseg - 1) * increm(ip0_im3s1(ifrac_im3))
                                            im3s1 = pmsa(ip_im3s1) * surf
                                            ip_dredge_im3 = ip0_dredge_im3(ifrac_im3, basin_no)
                                            sum_dredge(ip_dredge_im3) = sum_dredge(ip_dredge_im3) + im3s1 * fraction_dredge
                                            ipflux = iflux + nim1 + nim2 + ifrac_im3
                                            fl(ipflux) = im3s1 * fraction_dredge / volume / delt
                                        enddo
                                    endif
                                endif
                            else
                                if (actths2 > 1.e-15) then
                                    fraction_dredge = (actths2 - dredge_criterium) / actths2
                                    if (fraction_dredge > 0.0) then
                                        do ifrac_im1 = 1, nim1
                                            ip_im1s2 = ipoint(ip0_im1s2(ifrac_im1)) + (iseg - 1) * increm(ip0_im1s2(ifrac_im1))
                                            im1s2 = pmsa(ip_im1s2) * surf
                                            ip_dredge_im1 = ip0_dredge_im1(ifrac_im1, basin_no)
                                            sum_dredge(ip_dredge_im1) = sum_dredge(ip_dredge_im1) + im1s2 * fraction_dredge
                                            ipflux = iflux + nim1 + nim2 + nim3 + ifrac_im1
                                            fl(ipflux) = im1s2 * fraction_dredge / volume / delt
                                        enddo
                                        do ifrac_im2 = 1, nim2
                                            ip_im2s2 = ipoint(ip0_im2s2(ifrac_im2)) + (iseg - 1) * increm(ip0_im2s2(ifrac_im2))
                                            im2s2 = pmsa(ip_im2s2) * surf
                                            ip_dredge_im2 = ip0_dredge_im2(ifrac_im2, basin_no)
                                            sum_dredge(ip_dredge_im2) = sum_dredge(ip_dredge_im2) + im2s2 * fraction_dredge
                                            ipflux = iflux + nim1 + nim2 + nim3 + nim1 + ifrac_im2
                                            fl(ipflux) = im2s2 * fraction_dredge / volume / delt
                                        enddo
                                        do ifrac_im3 = 1, nim3
                                            ip_im3s2 = ipoint(ip0_im3s2(ifrac_im3)) + (iseg - 1) * increm(ip0_im3s2(ifrac_im3))
                                            im3s2 = pmsa(ip_im3s2) * surf
                                            ip_dredge_im3 = ip0_dredge_im3(ifrac_im3, basin_no)
                                            sum_dredge(ip_dredge_im3) = sum_dredge(ip_dredge_im3) + im3s2 * fraction_dredge
                                            ipflux = iflux + nim1 + nim2 + nim3 + nim1 + nim2 + ifrac_im3
                                            fl(ipflux) = im3s2 * fraction_dredge / volume / delt
                                        enddo
                                    endif
                                endif
                            endif
                        endif
                    endif
                endif
            endif
            ip_basin_no = ip_basin_no + increm(2)
            ip_actths1 = ip_actths1 + increm(4)
            ip_actths2 = ip_actths2 + increm(5)
            ip_volume = ip_volume + increm(6)
            ip_surf = ip_surf + increm(7)
            ip_delt = ip_delt + increm(8)
            iflux = iflux + noflux
        enddo

        ! dump loop

        ip_volume = ipoint(6)
        ip_delt = ipoint(8)
        do i_basin = 1, no_basin

            dumpsegment = nint(pmsa(ip_dumpsegment(i_basin)))
            dumpspeed = pmsa(ip_dumpspeed(i_basin))
            relabel = nint(pmsa(ip_relabel(i_basin)))

            ! dump till all dredged material is finished with specified speed

            dredge_tot = 0.0
            do ifrac_im1 = 1, nim1
                ip_dredge_im1 = ip0_dredge_im1(ifrac_im1, i_basin)
                dredge_im1 = sum_dredge(ip_dredge_im1)
                dredge_tot = dredge_tot + dredge_im1
            enddo
            do ifrac_im2 = 1, nim2
                ip_dredge_im2 = ip0_dredge_im2(ifrac_im2, i_basin)
                dredge_im2 = sum_dredge(ip_dredge_im2)
                dredge_tot = dredge_tot + dredge_im2
            enddo
            do ifrac_im3 = 1, nim3
                ip_dredge_im3 = ip0_dredge_im3(ifrac_im3, i_basin)
                dredge_im3 = sum_dredge(ip_dredge_im3)
                dredge_tot = dredge_tot + dredge_im3
            enddo

            if (dredge_tot > 1e-20) then

                ip_volume = ipoint(6) + (dumpsegment - 1) * increm(6)
                ip_delt = ipoint(8) + (dumpsegment - 1) * increm(8)
                volume = pmsa(ip_volume)
                delt = pmsa(ip_delt)
                maxdump = dumpspeed * delt
                dump = min(dredge_tot, maxdump)

                do ifrac_im1 = 1, nim1
                    ip_dredge_im1 = ip0_dredge_im1(ifrac_im1, i_basin)
                    dredge_im1 = sum_dredge(ip_dredge_im1)
                    dump_im1 = dump * (dredge_im1 / dredge_tot)
                    dredge_im1 = max(0.0, dredge_im1 - dump_im1)
                    if (relabel > 0) then
                        ifrac_dump_im1 = max(1, min(nim1, relabel))
                    else
                        ifrac_dump_im1 = ifrac_im1
                    endif
                    sum_dredge(ip_dredge_im1) = dredge_im1
                    if (dumpsegment > 0) then
                        ifl_dump_im1 = (dumpsegment - 1) * noflux + nim1 + nim2 + nim3 + nim1 + nim2 + nim3 + ifrac_dump_im1
                        fl(ifl_dump_im1) = fl(ifl_dump_im1) + dump_im1 / volume / delt
                    endif
                enddo
                do ifrac_im2 = 1, nim2
                    ip_dredge_im2 = ip0_dredge_im2(ifrac_im2, i_basin)
                    dredge_im2 = sum_dredge(ip_dredge_im2)
                    dump_im2 = dump * (dredge_im2 / dredge_tot)
                    dredge_im2 = max(0.0, dredge_im2 - dump_im2)
                    if (relabel > 0) then
                        ifrac_dump_im2 = max(1, min(nim2, relabel))
                    else
                        ifrac_dump_im2 = ifrac_im2
                    endif
                    sum_dredge(ip_dredge_im2) = dredge_im2
                    if (dumpsegment > 0) then
                        ifl_dump_im2 = (dumpsegment - 1) * noflux + nim1 + nim2 + nim3 + nim1 + nim2 + nim3 + nim1 + ifrac_dump_im2
                        fl(ifl_dump_im2) = fl(ifl_dump_im2) + dump_im2 / volume / delt
                    endif
                enddo
                do ifrac_im3 = 1, nim3
                    ip_dredge_im3 = ip0_dredge_im3(ifrac_im3, i_basin)
                    dredge_im3 = sum_dredge(ip_dredge_im3)
                    dump_im3 = dump * (dredge_im3 / dredge_tot)
                    dredge_im3 = max(0.0, dredge_im3 - dump_im3)
                    if (relabel > 0) then
                        ifrac_dump_im3 = max(1, min(nim3, relabel))
                    else
                        ifrac_dump_im3 = ifrac_im3
                    endif
                    sum_dredge(ip_dredge_im3) = dredge_im3
                    if (dumpsegment > 0) then
                        ifl_dump_im3 = (dumpsegment - 1) * noflux + nim1 + nim2 + nim3 + nim1 + nim2 + nim3 + nim1 + nim2 + ifrac_dump_im3
                        fl(ifl_dump_im3) = fl(ifl_dump_im3) + dump_im3 / volume / delt
                    endif
                enddo

            endif

        enddo

        !     store remaining mass in pmsa, only if dumpsegment is in my domain

        do i_basin = 1, no_basin
            dumpsegment = nint(pmsa(ip_dumpsegment(i_basin)))

            do ifrac_im1 = 1, nim1
                pmsa(ipoint(ipoff + 7 * max_basin + ip0_dredge_im1(i_basin, ifrac_im1))) = sum_dredge(ip0_dredge_im1(i_basin, ifrac_im1))
            enddo
            do ifrac_im2 = 1, nim2
                pmsa(ipoint(ipoff + 7 * max_basin + ip0_dredge_im2(i_basin, ifrac_im2))) = sum_dredge(ip0_dredge_im2(i_basin, ifrac_im2))
            enddo
            do ifrac_im3 = 1, nim3
                pmsa(ipoint(ipoff + 7 * max_basin + ip0_dredge_im3(i_basin, ifrac_im3))) = sum_dredge(ip0_dredge_im3(i_basin, ifrac_im3))
            enddo
        enddo

        return
    end SUBROUTINE dredge_process

end module m_dredge_process
