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
module m_dlwq50
    use m_waq_precision

    implicit none

contains


    subroutine dlwq50 (nosys, notot, noseg, noq, novelo, &
            velo, area, flow, ipoint, ivpnt, &
            conc, bound, idt, deriv, iaflag, &
            amass2)

        !     Deltares Software Centre

        !>\file
        !>        Makes derivatives, upwind in space, advection only, for first step of FCT
        !>
        !>        First step of FCT consists of first order, upwind, monotoneous, advection
        !>        step, with numerical diffusion. In the correction step an anti diffusion
        !>        is computed, to arrive at 2nd order Lax Wendroff, if no artificial minima
        !>        and maxima are generated, otherwise the flux limiter will become active.
        !>        The desired diffusion is subtracted from the anti diffusion in the correction
        !>        step if a positive diffusion remains, then no correction takes place, if a
        !>        negative diffusion remains, it is applied to the degree possible.


        !     Files               : none

        !     Routines            : none

        use timers
        implicit none

        !     Parameters          :

        !     kind           function         name                   description

        integer(kind = int_wp), intent(in) :: nosys                !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                !< total number of substances
        integer(kind = int_wp), intent(in) :: noseg                !< number of computational volumes
        integer(kind = int_wp), intent(in) :: noq                  !< total number of interfaces
        integer(kind = int_wp), intent(in) :: novelo               !< number additional velocities
        real(kind = real_wp), intent(in) :: velo  (novelo, noq)   !< array with additional velocities
        real(kind = real_wp), intent(in) :: area  (noq)          !< exchange areas in m2
        real(kind = real_wp), intent(in) :: flow  (noq)          !< flows through the exchange areas in m3/s
        integer(kind = int_wp), intent(in) :: ipoint(4, noq)   !< from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: ivpnt (nosys)        !< additional velocity number per substance
        real(kind = real_wp), intent(in) :: conc  (notot, noseg)  !< concentrations at previous time level
        real(kind = real_wp), intent(in) :: bound (nosys, *)  !< open boundary concentrations
        integer(kind = int_wp), intent(in) :: idt                  !< time step in seconds
        real(kind = real_wp), intent(inout) :: deriv (notot, noseg)  !< derivatives of the concentraions
        integer(kind = int_wp), intent(in) :: iaflag               !< if 1 then accumulate mass in report array
        real(kind = real_wp), intent(inout) :: amass2(notot, 5)  !< report array for monitoring file

        !     Local variables     :

        integer(kind = int_wp) :: iq              ! loop counter exchanges
        integer(kind = int_wp) :: isys            ! loop counter substance
        integer(kind = int_wp) :: ifrom, ito    ! from and to volume numbers
        real(kind = real_wp) :: a               ! this area
        real(kind = real_wp) :: q               ! flow for this exchange
        real(kind = real_wp) :: v               ! flow for this substance
        real(kind = real_wp) :: dq              ! total flux from and to

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq50", ithandl)

        !         loop accross the number of exchanges

        do iq = 1, noq

            !         initialisations , check for transport anyhow

            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom <= 0 .and. ito <= 0) cycle

            a = area(iq)
            q = flow(iq)
            if (ifrom < 0) goto 20
            if (ito   < 0) goto 40

            !         the regular case

            do isys = 1, nosys
                v = q
                if (ivpnt(isys) > 0) v = v + velo(ivpnt(isys), iq) * a
                if (v > 0.0) then
                    dq = v * conc(isys, ifrom)
                else
                    dq = v * conc(isys, ito)
                endif
                deriv(isys, ifrom) = deriv(isys, ifrom) - dq
                deriv(isys, ito) = deriv(isys, ito) + dq
            enddo
            cycle

            !        The 'from' element was a boundary.

            20    do isys = 1, nosys
                v = q
                if (ivpnt(isys) > 0) v = v + velo(ivpnt(isys), iq) * a
                if (v > 0.0) then
                    dq = v * bound(isys, -ifrom)
                    if (iaflag == 1) amass2(isys, 4) = amass2(isys, 4) + dq * idt
                else
                    dq = v * conc (isys, ito)
                    if (iaflag == 1) amass2(isys, 5) = amass2(isys, 5) - dq * idt
                endif
                deriv(isys, ito) = deriv(isys, ito) + dq
            enddo
            cycle

            !        The 'to' element was a boundary.

            40    do isys = 1, nosys
                v = q
                if (ivpnt(isys) > 0) v = v + velo(ivpnt(isys), iq) * a
                if (v > 0.0) then
                    dq = v * conc (isys, ifrom)
                    if (iaflag == 1) amass2(isys, 5) = amass2(isys, 5) + dq * idt
                else
                    dq = v * bound(isys, -ito)
                    if (iaflag == 1) amass2(isys, 4) = amass2(isys, 4) - dq * idt
                endif
                deriv(isys, ifrom) = deriv(isys, ifrom) - dq
            enddo

            !        end of the loop over exchanges

        end do

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwq50
