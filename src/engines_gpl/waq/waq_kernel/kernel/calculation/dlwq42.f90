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
module m_dlwq42
    use m_waq_precision

    implicit none

contains


    !> Sets an explicit time step from DERIV.
    !! This routine deviates from dlwq18 in the sense
    !! that the resulting masses are stored in CONC
    !! rather than in AMASS to allow for an implicit step.
    !! DERIV is set to the new diagonal. This procedure
    !! is required for the old ADI solver (nr. 4) and for
    !! the 2 solvers with implicit vertical (nrs. 11 & 12)
subroutine dlwq42(num_substances_transported, num_substances_total, num_substances_part, num_cells, volume, &
            surface, amass, conc, deriv, idt, &
            ivflag, file_unit_list)

        use timers

        implicit none

        integer(kind = int_wp), intent(in)    :: num_substances_transported                   !< number of transported substances
        integer(kind = int_wp), intent(in)    :: num_substances_total                   !< total number of substances
        integer(kind = int_wp), intent(in)    :: num_substances_part                  !< number of particle substances
        integer(kind = int_wp), intent(in)    :: num_cells                   !< number of computational volumes
        real(kind = real_wp),   intent(inout) :: volume(num_cells)           !< volumes of the segments
        real(kind = real_wp),   intent(in)    :: surface(num_cells)          !< horizontal surface area
        real(kind = real_wp),   intent(inout) :: amass (num_substances_total, num_cells)    !< masses per substance per volume
        real(kind = real_wp),   intent(inout) :: conc  (num_substances_total, num_cells)    !< concentrations per substance per volume
        real(kind = real_wp),   intent(inout) :: deriv (num_substances_total, num_cells)    !< derivatives per substance per volume
        integer(kind = int_wp), intent(in)    :: idt                     !< integration time step size
        integer(kind = int_wp), intent(in)    :: ivflag                  !< if 1 computational volumes
        integer(kind = int_wp), intent(in)    :: file_unit_list          !< unit number of the monitoring file

        ! Local variables
        integer(kind = int_wp) :: isys          ! loopcounter substances
        integer(kind = int_wp) :: iseg          ! loopcounter computational volumes
        real(kind = real_wp) :: surf            ! the horizontal surface area of the cell
        real(kind = real_wp) :: vol             ! helpvariable for this volume
        integer(kind = int_wp), save :: ivmess  ! number of messages printed
        data       ivmess  /0/
        integer(kind = int_wp), save :: ithandl ! timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("dlwq42", ithandl)

        ! loop accross the number of computational volumes for the concentrations
        do iseg = 1, num_cells

            ! compute volumes if necessary and check for positivity
            if (ivflag == 1) volume(iseg) = amass(1, iseg) + idt * deriv(1, iseg)
            vol = volume(iseg)
            if (abs(vol) < 1.0e-25) then
                if (ivmess < 25) then
                    ivmess = ivmess + 1
                    write (file_unit_list, 1000) iseg, vol
                elseif (ivmess == 25) then
                    ivmess = ivmess + 1
                    write (file_unit_list, 1001)
                end if
                volume (iseg) = 1.0
                vol = 1.0
            end if

            ! transported substances first
            do isys = 1, num_substances_transported
                conc (isys, iseg) = amass(isys, iseg) + idt * deriv(isys, iseg)
                deriv(isys, iseg) = vol
            end do

            ! then the passive substances
            if (num_substances_total - num_substances_part > num_substances_transported) then
                surf = surface(iseg)
                do isys = num_substances_transported + 1, num_substances_total - num_substances_part
                    amass(isys, iseg) = amass(isys, iseg) + idt * deriv(isys, iseg)
                    conc (isys, iseg) = amass(isys, iseg) / surf
                    deriv(isys, iseg) = 0.0
                end do
            end if
        end do
        ! output formats
        1000 format ('Volume of segment:', I7, ' is:', &
                E15.6, ' 1.0 assumed.')
        1001 format ('25 or more zero volumes , further messages surpressed')

        if (timon) call timstop (ithandl)
    end subroutine dlwq42
end module m_dlwq42
