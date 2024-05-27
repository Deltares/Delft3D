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
module m_get_effi
    use m_waq_precision

    implicit none

contains


    subroutine get_effi(SWEff, temper, radiat, ext, depthw, daylen, nspe, effi)
        !>\file
        !>       calculate and store efficiency for all species

        use m_maxprd
        use m_ebcalc
        use bloom_data_dim
        use bloom_data_size
        use bloom_data_arran
        use bloom_data_phyt
        use bloom_data_putin

        implicit none

        !     arguments

        integer(kind = int_wp) :: SWEff      ! input , Switch to use classic(1) or direct(2) BLOOM Efficiency calculation
        real(kind = real_wp) :: temper     ! input , temperature
        real(kind = real_wp) :: radiat     ! input , radiation
        real(kind = real_wp) :: ext        ! input , total extinction
        real(kind = real_wp) :: depthw     ! input , depth of the layer
        real(kind = real_wp) :: daylen     ! input , daylength in hours
        integer(kind = int_wp) :: nspe       ! output, number of bloom algea species
        real(kind = real_wp) :: effi(30)   ! output, calculated efficiencies per species

        !     local decalarations

        integer(kind = int_wp) :: lunrep

        real(kind = dp) :: temp       ! temperature
        real(kind = dp) :: csol       ! radiation
        real(kind = dp) :: dsol       ! radiation
        real(kind = dp) :: radtop     ! radiation at the top of the segment
        real(kind = dp) :: radmid     ! radiation at the middle of the segment
        real(kind = dp) :: radbot     ! radiation at the bottom of the segment
        real(kind = dp) :: radlay     ! radiation at the current effi layer of the segment
        real(kind = dp) :: effitop    ! efficiency at the top of the segment
        real(kind = dp) :: effimid    ! efficiency at the middle of the segment
        real(kind = dp) :: effibot    ! efficiency at the bottom of the segment
        real(kind = dp) :: effilay    ! efficiency at the current effi layer of the segment
        real(kind = dp) :: effitot    ! efficiency collector for calculation of the average
        integer(kind = int_wp) :: neffilay   ! number of effi layers
        integer(kind = int_wp) :: ilay       ! current effi layer number
        real(kind = dp) :: dep        ! depth
        real(kind = dp) :: exttot     ! total extinction
        real(kind = dp) :: day        ! daylength in hours
        real(kind = dp) :: dayl       ! daylength in fraction
        real(kind = dp) :: tcorr      ! tcorr
        real(kind = dp) :: surf_typ   ! scaled, converted and corrected radiation for a type
        integer(kind = int_wp) :: ntyp       ! number of bloom algea types
        integer(kind = int_wp) :: itype      ! index number of bloom algae type
        integer(kind = int_wp) :: igroup     ! index number of bloom algae group
        real(kind = dp) :: pmax20(mt), sdmixn(mt)

        real(kind = dp) :: phi_s      ! x value tabulated function at surface
        real(kind = dp) :: fun_s      ! function at surface
        real(kind = dp) :: der_s      ! derivative at sutface
        real(kind = dp) :: phi_d      ! x value tabulated function at dep
        real(kind = dp) :: fun_d      ! function at surface at dep
        real(kind = dp) :: der_d      ! derivative at sutface at dep

        dep = depthw
        exttot = ext
        temp = temper
        day = daylen
        dayl = day / 24.0d0
        nspe = nuecog
        ntyp = maxval(it2)
        effi = 0.0d0

        call maxprd (tefcur)
        do itype = 1, ntyp
            pmax20(itype) = pmax(itype)
            if (sdmix(itype) < 0.0) then
                sdmixn(itype) = 1.0d0 + sdmix(itype)
            else
                sdmixn(itype) = 0.0d0
            endif
        enddo
        call maxprd (temp)

        if (SWEff == 1) then
            ! classic BLOOM effi calculation
            dsol = 1428.57d0 * solaco * radiat  ! Conversion from W/m2 to J/cm2/7days
            do igroup = 1, nuecog
                do itype = it2(igroup, 1), it2(igroup, 2)
                    tcorr = pmax20(itype) / pmax(itype)
                    surf_typ = tcorr * dsol * dexp (- exttot * sdmixn(itype) * dep)
                    surf_typ = surf_typ / day
                    if (surf_typ > 1.0 .and. exttot * dep > 1.0d-10) then
                        phi_s = - dlog(surf_typ)
                        call ebcalc(phi_s, fun_s, der_s, igroup)
                        phi_d = exttot * dep - dlog(surf_typ)
                        call ebcalc(phi_d, fun_d, der_d, igroup)
                        effi(igroup) = max(effi(igroup), (fun_d - fun_s) / exttot / dep)
                    else
                        effi(igroup) = 0.0
                    endif
                enddo
            enddo
        else if (SWEff == 2) then
            ! direct effi lookup in light curve at top (SWEff == 2)
            do igroup = 1, nuecog
                do itype = it2(igroup, 1), it2(igroup, 2)
                    tcorr = pmax20(itype) / pmax(itype)
                    if (sdmixn(itype)==0.0d0) then
                        radtop = radiat / 0.0168d0  ! conversion from J/cm2/7days to J/m2/hour (*3600.0/60.48 = /0.0168)
                    else
                        radtop = (radiat / 0.0168d0) * dexp (- exttot * abs(sdmixn(itype)) * dep)
                    endif
                    call lookupeffi(tcorr * radtop, effitop, igroup)
                    effi(igroup) = max(effi(igroup), effitop)
                enddo
            enddo
        elseif (SWEff == 3) then
            ! direct effi lookup in light curve at top and bottom and take average (SWEff == 3)
            do igroup = 1, nuecog
                do itype = it2(igroup, 1), it2(igroup, 2)
                    tcorr = pmax20(itype) / pmax(itype)
                    if (sdmixn(itype)==0.0d0) then
                        radtop = radiat / 0.0168d0  ! conversion from J/cm2/7days to J/m2/hour (*3600.0/60.48 = /0.0168)
                    else
                        radtop = (radiat / 0.0168d0) * dexp (- exttot * abs(sdmixn(itype)) * dep)
                    endif
                    radbot = radtop * dexp (- exttot * abs(sdmix(itype)) * dep)
                    call lookupeffi(tcorr * radtop, effitop, igroup)
                    call lookupeffi(tcorr * radbot, effibot, igroup)
                    effi(igroup) = max(effi(igroup), (effitop + effibot) / 2.0)
                enddo
            enddo
        elseif (SWEff == 4) then
            ! direct effi lookup in light curve at top, middle and bottom and take weighted average (SWEff == 4)
            do igroup = 1, nuecog
                do itype = it2(igroup, 1), it2(igroup, 2)
                    tcorr = pmax20(itype) / pmax(itype)
                    if (sdmixn(itype)==0.0d0) then
                        radtop = radiat / 0.0168d0  ! conversion from J/cm2/7days to J/m2/hour (*3600.0/60.48 = /0.0168)
                    else
                        radtop = (radiat / 0.0168d0) * dexp (- exttot * abs(sdmixn(itype)) * dep)
                    endif
                    radmid = radtop * dexp (- exttot * 0.5d0 * abs(sdmix(itype)) * dep)
                    radbot = radtop * dexp (- exttot * abs(sdmix(itype)) * dep)
                    call lookupeffi(tcorr * radtop, effitop, igroup)
                    call lookupeffi(tcorr * radmid, effimid, igroup)
                    call lookupeffi(tcorr * radbot, effibot, igroup)
                    effi(igroup) = max(effi(igroup), (effitop + effimid + effimid + effibot) / 4.0)
                enddo
            enddo
        elseif (SWEff < 0) then
            ! direct effi lookup in light curve at top and abs(sweff) number of layers and take average (SWEff < 0)
            neffilay = abs (SWEff)
            do igroup = 1, nuecog
                do itype = it2(igroup, 1), it2(igroup, 2)
                    tcorr = pmax20(itype) / pmax(itype)
                    if (sdmixn(itype)==0.0d0) then
                        radtop = radiat / 0.0168d0  ! conversion from J/cm2/7days to J/m2/hour (*3600.0/60.48 = /0.0168)
                    else
                        radtop = (radiat / 0.0168d0) * dexp (- exttot * abs(sdmixn(itype)) * dep)
                    endif
                    call lookupeffi(tcorr * radtop, effitot, igroup)
                    do ilay = 1, neffilay - 1
                        radlay = radtop * dexp (- exttot * (1.0d0 / real(neffilay, 8)) * abs(sdmix(itype)) * dep)
                        call lookupeffi(tcorr * radlay, effilay, igroup)
                        effitot = effitot + effilay * 2
                        radtop = radlay
                    enddo
                    radlay = radtop * dexp (- exttot * (1.0d0 / real(neffilay, 8)) * abs(sdmix(itype)) * dep)
                    call lookupeffi(tcorr * radlay, effilay, igroup)
                    effitot = effitot + effilay
                    effi(igroup) = max(effi(igroup), (effitot / real(neffilay * 2, 8)) / dayl)
                enddo
            enddo

        endif
        return

    end subroutine get_effi

    subroutine lookupeffi(rad, effi, numgr)

        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use bloom_data_dim
        use bloom_data_arran

        implicit none

        real(kind = dp) :: rad
        real(kind = dp) :: effi
        integer(kind = int_wp) :: numgr, i
        integer(kind = int_wp) :: lunrep
        real(kind = dp) :: interpol
        logical, save :: first = .true.

        if (first) then
            if (power(npoint)==0.0) then
                call get_log_unit_number(lunrep)
                write(lunrep, *) &
                        'ERROR: the highest power in the light curve is 0.0.', &
                        'Check if your bloom.spe file contains light curves!'
                write(*, *) &
                        'ERROR: the highest power in the light curve is 0.0.', &
                        'Check if your bloom.spe file contains light curves!'
                call stop_with_error()
            endif
            first = .false.
        endif
        !
        !  lookup efficency in light curve
        !
        if (rad <= power(1)) then
            effi = effic(1, numgr)
        else if (rad >= power(npoint)) then
            effi = effic(npoint, numgr)
        else
            do i = 2, npoint
                if (rad>=power(i - 1).and.rad<=power(i)) then
                    interpol = (rad - power(i - 1)) / (power(i) - power(i - 1))
                    effi = effic(i - 1, numgr) + interpol * (effic(i, numgr) - effic(i - 1, numgr))
                    exit
                endif
            enddo
        endif
        return
    end


    subroutine get_nspe(nspe)

        use bloom_data_phyt

        implicit none

        integer(kind = int_wp) :: nspe       ! input , number of bloom algea types

        nspe = nuecog

        return
    end subroutine get_nspe

end module m_get_effi
