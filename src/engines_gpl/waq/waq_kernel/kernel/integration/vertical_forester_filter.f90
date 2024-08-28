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
module m_vertical_forester_filter
    use m_waq_precision

    implicit none

    private
    public :: vertical_forester_filter

contains


    !> Forester filter for the vertical.
    !! Then loops over the number of horizontal segments and over the
    !! the number of active substances are made. All these verticals
    !! are filtered at most MAXFIL times.
    !!  Everything starts with layer 2 (from the top in WAQ). IL is
    !! the counter here with starting values cell_i+num_cells. ILU points
    !! to the layer upper (IL-num_cells) and ILD points to the layer down-
    !! wards (IL+num_cells).
    !!  The from- and to- lengths are in the ALENG array. The third
    !! direction last. Than means that NOQT-num_exchanges_z_dir+cell_i is the pointer
    !! for the exchange with the layer above and that value plus num_cells
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
    subroutine vertical_forester_filter(lunut, num_substances_transported, num_substances_total, num_cells, num_exchanges_z_dir, &
            num_layers_grid, conc, aleng, nowarn)

        use timers

        integer(kind = int_wp), intent(in) :: lunut  !< Unit number of log file
        integer(kind = int_wp), intent(in) :: num_substances_transported  !< Number of active substances
        integer(kind = int_wp), intent(in) :: num_substances_total  !< Number of substances
        integer(kind = int_wp), intent(in) :: num_cells  !< Number of cells or segments
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir   !<  Number of exchanges in vertical direction
        integer(kind = int_wp), intent(inout) :: nowarn !< Number of warnings sent to the log file
        integer(kind = int_wp), intent(in) :: num_layers_grid   !< ????

        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells) !< Array with concentrations of all substances at all cells
        real(kind = real_wp), intent(in) :: aleng(2, num_exchanges_z_dir)     !< Mixing length

        ! Local variables
        real(kind = real_wp) :: dd, dr, dr1, dr2, dz1, dz2, coef

        integer(kind = int_wp) :: cell_i, substance_i, ifilt, il, is, ilu, ifil, ild, ilay
        integer(kind = int_wp) :: nhor, maxfil
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("vertical_forester_filter", ithandl)

        dd = 1.0e-02
        maxfil = 100

        ! only for structured 3d
        if (num_layers_grid <= 1) goto 9999
        nhor = num_cells / num_layers_grid
        ! for all horizontal segments
        do cell_i = 1, nhor
            ! for all active substances
            do substance_i = 1, num_substances_transported
                ! do until maximum iteration or untill satisfied
                do ifilt = 1, maxfil
                    ifil = 0
                    il = cell_i
                    ilu = il - nhor
                    ild = il + nhor
                    do ilay = 2, num_layers_grid - 1
                        il = il + nhor
                        ilu = ilu + nhor
                        ild = ild + nhor
                        dr1 = conc(substance_i, il) - conc(substance_i, ilu)
                        dr2 = conc(substance_i, il) - conc(substance_i, ild)
                        ! test for local maximum
                        if (dr1 >  dd .and. dr2 >  dd) then
                            ifil = 1
                            if (dr1 > dr2) then
                                dr = min (0.5 * dr1, dr2)
                                dz1 = aleng(1, ilu)
                                dz2 = aleng(2, ilu)
                                coef = min (dz1, dz2, 0.5) * dr
                                conc(substance_i, ilu) = conc(substance_i, ilu) + coef / dz1
                                conc(substance_i, il) = conc(substance_i, il) - coef / dz2
                            else
                                dr = min (dr1, 0.5 * dr2)
                                dz1 = aleng(1, il)
                                dz2 = aleng(2, il)
                                coef = min (dz1, dz2, 0.5) * dr
                                conc(substance_i, il) = conc(substance_i, il) - coef / dz1
                                conc(substance_i, ild) = conc(substance_i, ild) + coef / dz2
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
                                conc(substance_i, ilu) = conc(substance_i, ilu) + coef / dz1
                                conc(substance_i, il) = conc(substance_i, il) - coef / dz2
                            else
                                dr = max (dr1, 0.5 * dr2)
                                dz1 = aleng(1, il)
                                dz2 = aleng(2, il)
                                coef = min (dz1, dz2, 0.5) * dr
                                conc(substance_i, il) = conc(substance_i, il) - coef / dz1
                                conc(substance_i, ild) = conc(substance_i, ild) + coef / dz2
                            endif
                        endif
                    end do
                    if (ifil == 0) goto 30
                end do
                if (ifil == 1) then
                    if (nowarn < 1000) write (lunut, 1010) substance_i, cell_i, ilay
                    nowarn = nowarn + 1
                endif
                30 continue
            end do
        end do

        9999 if (timon) call timstop (ithandl)
        1010 FORMAT (' WARNING: Forester filter max. iterations reached for substance: ', &
                I2, '; segment: ', I6, '; layer: ', I2, ' !')
    end subroutine vertical_forester_filter
end module m_vertical_forester_filter
