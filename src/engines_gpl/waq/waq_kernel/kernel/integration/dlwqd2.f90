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
module m_dlwqd2
    use m_waq_precision

    implicit none

contains


    !> Forester filter for the vertical.
    !! Then loops over the number of horizontal segments and over the
    !! the number of active substances are made. All these verticals
    !! are filtered at most MAXFIL times.
    !!  Everything starts with layer 2 (from the top in WAQ). IL is
    !! the counter here with starting values ISEG+NOSEG. ILU points
    !! to the layer upper (IL-NOSEG) and ILD points to the layer down-
    !! wards (IL+NOSEG).
    !!  The from- and to- lengths are in the ALENG array. The third
    !! direction last. Than means that NOQT-NOQ3+ISEG is the pointer
    !! for the exchange with the layer above and that value plus NOSEG
    !! is the pointer to the exchange downward. The from- value is
    !! the first one that is in the higher layer, The to- value is in
    !! the lower layer. You can check that the to- value for the upper
    !! exchange should equal the from- value for the downward exchange.
    !!  The filter starts action if the layer value is over DD larger
    !! than or over DD smaller than both bordering values. If that
    !! the IFIL flag signals a filter step.
    !! The filter corrects the largest difference
    !! by taking half of that difference, or the other difference
    !! which one is smallest. It multiplies this by the smallest
    !! thickness (and unknown A thus with the smallest volume). It
    !! corrects with this mass and divides by the volume (the unknown
    !! A and the thickness) to get a concentration again. This means
    !! that per step at most 0.5 times the largest difference is
    !! corrected.
    !!  Because WAQ has halflengths, you must read "half the volume"
    !! but that does not differ because 0.5/0.5 = 1.0. There is an
    !! upperbound to the correction that is at 1.0 m thickness in the
    !! original code. Because we work with half distances, it is 0.5
    !! here.
    !!  A maximum/minimum of DD remains.
    subroutine dlwqd2(lunut, nosys, notot, noseg, noq3, &
            kmax, conc, aleng, nowarn)

        use timers

        integer(kind = int_wp), intent(in   ) :: lunut  !< Unit number of log file
        integer(kind = int_wp), intent(in   ) :: nosys  !< Number of active substances
        integer(kind = int_wp), intent(in   ) :: notot  !< Number of substances
        integer(kind = int_wp), intent(in   ) :: noseg  !< Number of cells or segments
        integer(kind = int_wp), intent(in   ) :: noq3   !<  Number of exchanges in vertical direction
        integer(kind = int_wp), intent(inout) :: nowarn !< Number of warnings sent to the log file
        integer(kind = int_wp), intent(in   ) :: kmax   !< ????

        real(kind = real_wp),   intent(inout) :: conc(notot, noseg) !< Array with concentrations of all substances at all cells
        real(kind = real_wp),   intent(in   ) :: aleng(2, noq3)     !< Mixing length

        ! Local variables
        real(kind = real_wp) :: dd, dr, dr1, dr2, dz1, dz2, coef

        integer(kind = int_wp) :: iseg, isys, ifilt, il, is, ilu, ifil, ild, ilay
        integer(kind = int_wp) :: nhor, maxfil
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwqd2", ithandl)

        dd = 1.0e-02
        maxfil = 100

        ! only for structured 3d
        if (kmax <= 1) goto 9999
        nhor = noseg / kmax
        ! for all horizontal segments
        do iseg = 1, nhor
            ! for all active substances
            do isys = 1, nosys
                ! do until maximum iteration or untill satisfied
                do ifilt = 1, maxfil
                    ifil = 0
                    il = iseg
                    ilu = il - nhor
                    ild = il + nhor
                    do ilay = 2, kmax - 1
                        il = il + nhor
                        ilu = ilu + nhor
                        ild = ild + nhor
                        dr1 = conc(isys, il) - conc(isys, ilu)
                        dr2 = conc(isys, il) - conc(isys, ild)
                        ! test for local maximum
                        if (dr1 >  dd .and. dr2 >  dd) then
                            ifil = 1
                            if (dr1 > dr2) then
                                dr = min (0.5 * dr1, dr2)
                                dz1 = aleng(1, ilu)
                                dz2 = aleng(2, ilu)
                                coef = min (dz1, dz2, 0.5) * dr
                                conc(isys, ilu) = conc(isys, ilu) + coef / dz1
                                conc(isys, il) = conc(isys, il) - coef / dz2
                            else
                                dr = min (dr1, 0.5 * dr2)
                                dz1 = aleng(1, il)
                                dz2 = aleng(2, il)
                                coef = min (dz1, dz2, 0.5) * dr
                                conc(isys, il) = conc(isys, il) - coef / dz1
                                conc(isys, ild) = conc(isys, ild) + coef / dz2
                            endif
                        endif
                        ! test for local minimum
                        if (dr1 < -dd .and. dr2 < -dd) then
                            ifil = 1
                            if (dr1 < dr2) then
                                dr = max (0.5 * dr1, dr2)
                                dz1 = aleng(1, ilu)
                                dz2 = aleng(2, ilu)
                                coef = min (dz1, dz2, 0.5) * dr
                                conc(isys, ilu) = conc(isys, ilu) + coef / dz1
                                conc(isys, il) = conc(isys, il) - coef / dz2
                            else
                                dr = max (dr1, 0.5 * dr2)
                                dz1 = aleng(1, il)
                                dz2 = aleng(2, il)
                                coef = min (dz1, dz2, 0.5) * dr
                                conc(isys, il) = conc(isys, il) - coef / dz1
                                conc(isys, ild) = conc(isys, ild) + coef / dz2
                            endif
                        endif
                    end do
                    if (ifil == 0) goto 30
                end do
                if (ifil == 1) then
                    if (nowarn < 1000) write (lunut, 1010) isys, iseg, ilay
                    nowarn = nowarn + 1
                endif
                30 continue
            end do
        end do
        9999 if (timon) call timstop (ithandl)
        1010 FORMAT (' WARNING: Forester filter max. iterations reached for substance: ', &
                I2, '; segment: ', I6, '; layer: ', I2, ' !')
    end subroutine dlwqd2
end module m_dlwqd2
