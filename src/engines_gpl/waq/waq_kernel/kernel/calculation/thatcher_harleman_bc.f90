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
module m_thatcher_harleman_bc
    use m_waq_precision
    use timers

    implicit none

    private
    public :: thatcher_harleman_bc

contains


    !> Implements the Thatcher-Harleman boundary conditions
    !! For each open boundary condition:
    !! - at outflow, updates last saved outflow concentration bsave
    !! - at outflow, sets open boundary condition to this outflow value
    !! - at inflow, set open boundary condition to:
    !!       - prescribed value if inflow time larger than the time-lag
    !!       - evaluates Tatcher-Harleman boundary if inflow time is less than time-lag
    subroutine thatcher_harleman_bc(bset, bsave, ibpnt, num_boundary_conditions, num_substances_transported, &
            num_substances_total, idt, conc, flow, bound)

        use m_cli_utils, only: is_command_arg_specified

        integer(kind = int_wp), intent(in) :: num_substances_transported    !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total          !< total number of substances
        integer(kind = int_wp), intent(in) :: num_boundary_conditions       !< number of open boundary conditions
        real(kind = real_wp), intent(in) :: bset (num_substances_transported, num_boundary_conditions)     !< prescribed open boundary conditions
        real(kind = real_wp), intent(inout) :: bsave(num_substances_transported, num_boundary_conditions)  !< saved open boundaries at outflow
        integer(kind = int_wp), intent(inout) :: ibpnt(4, num_boundary_conditions)    !< 1 = timelags /n
        !< 2 = flow pointer (can be negative) /n
        !< 3 = segment pointer /n
        !< 4 = time on the cosine /n
        integer(kind = int_wp), intent(in) :: idt                   !< time step size in system clock units
        real(kind = real_wp), intent(in) :: conc (num_substances_total, *)         !< model concentrations
        real(kind = real_wp), intent(in) :: flow (*)                !< model flows
        real(kind = real_wp), intent(out) :: bound(num_substances_transported, num_boundary_conditions)    !< model open boundary conditions

        ! Local variables
        real(kind = real_wp), parameter :: pi = 3.141593
        integer(kind = int_wp) :: ibnd   !<  loop variable boundaries
        integer(kind = int_wp) :: isub   !<  loop variable (transported) substances
        integer(kind = int_wp) :: itlag  !<  time lag for this boundary
        integer(kind = int_wp) :: iflow  !<  flow number of this boundary (positive if towards boundary)
        real(kind = real_wp) :: aflow  !<  flow accross boundary, positive is 'out'
        real(kind = real_wp) :: at     !<  Tatcher Harleman half cosine value
        integer(kind = int_wp) :: cell_i   !<  active volume number associated with the boundary
        integer(kind = int_wp) :: ibtime !<  time since last outflow at this boundary

        logical, save :: init = .true.
        logical, save :: bndmirror = .false.

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq17", ithandl)

        if (init) then
            if (is_command_arg_specified('-bndmirror')) then
                write(*, *) 'Using mirroring boundaries'
                bndmirror = .true.
            else
                bndmirror = .false.
            endif
            init = .false.
        endif

        do ibnd = 1, num_boundary_conditions
            itlag = ibpnt(1, ibnd)
            if (itlag == 0) then                     !  time lag not used for this boundary
                bound(:, ibnd) = bset(:, ibnd)
            else
                iflow = ibpnt(2, ibnd)
                if (iflow == 0) then                     !  no flow associated with this boundary
                    bound(:, ibnd) = bset(:, ibnd)
                    cycle
                endif
                aflow = sign(1, iflow) * flow(abs(iflow))
                if (aflow >= 0.0) then                   !  outflow
                    ibpnt(4, ibnd) = 0
                    cell_i = ibpnt(3, ibnd)
                    bsave(:, ibnd) = conc(1:num_substances_transported, cell_i)
                    bound(:, ibnd) = conc(1:num_substances_transported, cell_i)
                else                                         !  inflow
                    ibtime = ibpnt(4, ibnd) + idt
                    ibpnt(4, ibnd) = ibtime
                    if (ibtime  >= itlag) then
                        bound(:, ibnd) = bset(:, ibnd)
                    else
                        at = 0.5 * cos(real(ibtime) / itlag * pi)
                        bound(:, ibnd) = (0.5 - at) * bset(:, ibnd) + (0.5 + at) * bsave(:, ibnd)
                    endif
                endif
            endif

            ! 'mirror' boundary for substances with negative boundary concentrations, initially for efficiency tracers
            if (bndmirror) then
                do isub = 1, num_substances_transported
                    if (bset (isub, ibnd) < 0.0) then
                        ! when a negative boundary concentration is set, use current internal segment concentration
                        ! as a boundary instead of what was determined above
                        cell_i = ibpnt(3, ibnd)
                        bound(isub, ibnd) = max(0.0, conc(isub, cell_i))
                    endif
                enddo
            endif
        enddo
        if (timon) call timstop (ithandl)
    end subroutine thatcher_harleman_bc
end module m_thatcher_harleman_bc
