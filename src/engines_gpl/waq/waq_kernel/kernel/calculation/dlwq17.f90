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
module m_dlwq17
    use m_waq_precision

    implicit none

contains


    subroutine dlwq17 (bset, bsave, ibpnt, nobnd, nosys, &
            notot, idt, conc, flow, bound)

        !     Deltares - Delft Software Department

        !>\file
        !>            Makes the Tatcher-Harleman boundary conditions
        !>
        !>            This routine performs per open boundary condition:
        !>            - at outflow, updates last saved outflow concentration bsave
        !>            - at outflow, sets open boundary condition to this outflow value
        !>            - at inflow, set open boundary condition to:
        !>                  - prescribed value if inflow time larger then time lag
        !>                  - evaluates Tatcher-Harleman boundary if inflow time is less

        !     Created     : April      4, 1988 by Leo Postma
        !     Modified    : September 12, 2012 by Leo Postma: Fortran 90 look and feel

        !     File-IO     : none

        !     Subroutines : none

        use m_cli_utils, only : retrieve_command_argument
        use timers
        implicit none

        !     Arguments

        !     kind        function         name                    description
        integer(kind = int_wp), intent(in) :: nosys                 !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                 !< total number of substances
        integer(kind = int_wp), intent(in) :: nobnd                 !< number of open boundary conditions
        real(kind = real_wp), intent(in) :: bset (nosys, nobnd)    !< prescribed open boundary conditions
        real(kind = real_wp), intent(inout) :: bsave(nosys, nobnd)    !< saved open boundaries at outflow
        integer(kind = int_wp), intent(inout) :: ibpnt(4, nobnd)    !< 1 = timelags /n
        !  2 = flow pointer (can be negative) /n
        !  3 = segment pointer /n
        !  4 = time on the cosine /n
        integer(kind = int_wp), intent(in) :: idt                   !< time step size in system clock units
        real(kind = real_wp), intent(in) :: conc (notot, *)    !< model concentrations
        real(kind = real_wp), intent(in) :: flow (*)          !< model flows
        real(kind = real_wp), intent(out) :: bound(nosys, nobnd)    !< model open boundary conditions

        !     Locals

        real(kind = real_wp), parameter :: pi = 3.141593
        integer(kind = int_wp) :: ibnd             !  loop variable boundaries
        integer(kind = int_wp) :: isub             !  loop variable (transported) substances
        integer(kind = int_wp) :: itlag            !  time lag for this boundary
        integer(kind = int_wp) :: iflow            !  flow number of this boundary (positive if towards boundary)
        real(kind = real_wp) :: aflow            !  flow accross boundary, positive is 'out'
        real(kind = real_wp) :: at               !  Tatcher Harleman half cosine value
        integer(kind = int_wp) :: iseg             !  active volume number associated with the boundary
        integer(kind = int_wp) :: ibtime           !  time since last outflow at this boundary

        logical, save :: init = .true.
        logical, save :: bndmirror = .false.
        logical :: lfound
        character :: cdummy
        integer(kind = int_wp) :: idummy
        real(kind = real_wp) :: rdummy
        integer(kind = int_wp) :: ierr2

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq17", ithandl)

        if (init) then
            call retrieve_command_argument ('-bndmirror', 0, lfound, idummy, rdummy, cdummy, ierr2)
            if (lfound) then
                write(*, *) 'Using mirroring boundaries'
                bndmirror = .true.
            else
                bndmirror = .false.
            endif
            init = .false.
        endif

        do ibnd = 1, nobnd
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
                    iseg = ibpnt(3, ibnd)
                    bsave(:, ibnd) = conc(1:nosys, iseg)
                    bound(:, ibnd) = conc(1:nosys, iseg)
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
                do isub = 1, nosys
                    if (bset (isub, ibnd) < 0.0) then
                        ! when a negative boundary concentration is set, use current internal segment concentration
                        ! as a boundary instead of what was determined above
                        iseg = ibpnt(3, ibnd)
                        bound(isub, ibnd) = max(0.0, conc(isub, iseg))
                    endif
                enddo
            endif
        enddo

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwq17
