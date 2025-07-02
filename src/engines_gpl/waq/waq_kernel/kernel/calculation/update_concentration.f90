!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_update_concentration
    use m_waq_precision
    use timers

    implicit none

    private
    public :: update_concs_explicit_time_step, integrate_derivatives_explicitly

contains


    !> Updates concentrations after a time-step explicitly integrated
    !! - the mass array is increased with the deriv array * idt.
    !! - the deriv array is set to zero.
    !! - if applicable, computed volumes are evaluated.
    !! - the concentrations of water bound substances are mass/volume
    !! - the concentrations of bed susbtances are mass / surface
    subroutine update_concs_explicit_time_step(num_substances_transported, num_substances_total, num_substances_part, num_cells, volume, &
            surface, amass, conc, deriv, idt, &
            ivflag, file_unit_list)

        integer(kind = int_wp), intent(in) :: num_substances_transported                   !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                   !< total number of substances
        integer(kind = int_wp), intent(in) :: num_substances_part                  !< number of particle substances
        integer(kind = int_wp), intent(in) :: num_cells                   !< number of computational volumes
        real(kind = real_wp), intent(inout) :: volume (num_cells)         !< volumes of the segments
        real(kind = real_wp), intent(in) :: surface(num_cells)            !< horizontal surface area
        real(kind = real_wp), intent(inout) :: amass  (num_substances_total, num_cells)  !< masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc   (num_substances_total, num_cells)  !< concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: deriv  (num_substances_total, num_cells)  !< derivatives per substance per volume
        integer(kind = int_wp), intent(in) :: idt                     !< integration time step size
        integer(kind = int_wp), intent(in) :: ivflag                  !< if 1 computational volumes
        integer(kind = int_wp), intent(in) :: file_unit_list          !< unit number of the monitoring file

        !     local variables

        integer(kind = int_wp) :: substance_i          !< loop counter substances
        integer(kind = int_wp) :: cell_i          !< loop counter computational volumes
        real(kind = real_wp) :: surf            !< the horizontal surface area of the cell
        real(kind = real_wp) :: vol             !< help variable for this volume
        integer(kind = int_wp), save :: volume_messages_count  !< Number of messages logged about resetting zero volumes
        data       volume_messages_count  /0/
        integer(kind = int_wp), save :: ithandl ! timer handle
        data       ithandl  /0/

        if (timon) call timstrt ("update_concs_explicit_time_step", ithandl)

        ! set the time step
        amass = amass + idt * deriv
        deriv = 0.0

        ! loop across cells (segments) for the concentrations
        do cell_i = 1, num_cells
            ! compute volumes if necessary and check for positivity
            if (ivflag == 1) volume(cell_i) = amass(1, cell_i)
            vol = volume(cell_i)
            if (abs(vol) < 1.0e-25) then
                if (volume_messages_count < 25) then
                    volume_messages_count = volume_messages_count + 1
                    write (file_unit_list, 1000) cell_i, vol
                elseif (volume_messages_count == 25) then
                    volume_messages_count = volume_messages_count + 1
                    write (file_unit_list, 1001)
                endif
                volume (cell_i) = 1.0
                vol = 1.0
            endif

            ! transported substances
            do substance_i = 1, num_substances_transported
                conc(substance_i, cell_i) = amass(substance_i, cell_i) / vol
            enddo

            ! passive substances
            if (num_substances_total - num_substances_part > num_substances_transported) then
                surf = surface(cell_i)
                do substance_i = num_substances_transported + 1, num_substances_total - num_substances_part
                    conc(substance_i, cell_i) = amass(substance_i, cell_i) / surf
                enddo
            endif

        enddo

        if (timon) call timstop (ithandl)
        return

        1000 format ('Volume of segment:', I7, ' is:', E15.6, ' 1.0 assumed.')
        1001 format ('25 or more zero volumes, further messages supressed')

    end subroutine update_concs_explicit_time_step

    !> Sets an explicit time step from DERIV.
    !! This routine deviates from update_concs_explicit_time_step in the sense
    !! that the resulting masses are stored in CONC
    !! rather than in AMASS to allow for an implicit step.
    !! DERIV is set to the new diagonal. This procedure
    !! is required for the old ADI solver (nr. 4) and for
    !! the 2 solvers with implicit vertical (nrs. 11 & 12)
    subroutine integrate_derivatives_explicitly(num_substances_transported, num_substances_total, num_substances_part, &
            num_cells, volume, surface, amass, conc, deriv, idt, ivflag, file_unit_list)

        integer(kind = int_wp), intent(in) :: num_substances_transported           !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                 !< total number of substances
        integer(kind = int_wp), intent(in) :: num_substances_part                  !< number of particle substances
        integer(kind = int_wp), intent(in) :: num_cells                   !< number of computational volumes
        real(kind = real_wp), intent(inout) :: volume(num_cells)           !< volumes of the segments
        real(kind = real_wp), intent(in) :: surface(num_cells)          !< horizontal surface area
        real(kind = real_wp), intent(inout) :: amass (num_substances_total, num_cells)    !< masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc  (num_substances_total, num_cells)    !< concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: deriv (num_substances_total, num_cells)    !< derivatives per substance per volume
        integer(kind = int_wp), intent(in) :: idt                     !< integration time step size
        integer(kind = int_wp), intent(in) :: ivflag                  !< if 1 computational volumes
        integer(kind = int_wp), intent(in) :: file_unit_list          !< unit number of the monitoring file

        ! Local variables
        integer(kind = int_wp) :: substance_i          ! loopcounter substances
        integer(kind = int_wp) :: cell_i          ! loopcounter computational volumes
        real(kind = real_wp) :: surf            ! the horizontal surface area of the cell
        real(kind = real_wp) :: vol             ! helpvariable for this volume
        integer(kind = int_wp), save :: ivmess  ! number of messages printed
        data       ivmess  /0/
        integer(kind = int_wp), save :: ithandl ! timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("integrate_derivatives_explicitly", ithandl)

        ! loop accross the number of computational volumes for the concentrations
        do cell_i = 1, num_cells

            ! compute volumes if necessary and check for positivity
            if (ivflag == 1) volume(cell_i) = amass(1, cell_i) + idt * deriv(1, cell_i)
            vol = volume(cell_i)
            if (abs(vol) < 1.0e-25) then
                if (ivmess < 25) then
                    ivmess = ivmess + 1
                    write (file_unit_list, 1000) cell_i, vol
                elseif (ivmess == 25) then
                    ivmess = ivmess + 1
                    write (file_unit_list, 1001)
                end if
                volume (cell_i) = 1.0
                vol = 1.0
            end if

            ! transported substances first
            do substance_i = 1, num_substances_transported
                conc (substance_i, cell_i) = amass(substance_i, cell_i) + idt * deriv(substance_i, cell_i)
                deriv(substance_i, cell_i) = vol
            end do

            ! then the passive substances
            if (num_substances_total - num_substances_part > num_substances_transported) then
                surf = surface(cell_i)
                do substance_i = num_substances_transported + 1, num_substances_total - num_substances_part
                    amass(substance_i, cell_i) = amass(substance_i, cell_i) + idt * deriv(substance_i, cell_i)
                    conc (substance_i, cell_i) = amass(substance_i, cell_i) / surf
                    deriv(substance_i, cell_i) = 0.0
                end do
            end if
        end do
        ! output formats
        1000 format ('Volume of segment:', I7, ' is:', E15.6, ' 1.0 assumed.')
        1001 format ('25 or more zero volumes , further messages surpressed')

        if (timon) call timstop (ithandl)
    end subroutine integrate_derivatives_explicitly

end module m_update_concentration
