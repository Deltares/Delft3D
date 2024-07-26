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
module m_dlwqm8
    use m_waq_precision

    implicit none

contains


    !> Flux correction according to Boris and Book
    subroutine dlwqm8(idt, isys, num_substances_transported, num_substances_total, num_cells, &
            conc, concvt, volnew, num_boundary_conditions, bound, &
            num_exchanges, iknmrk, ipoint, area, aleng, &
            theta, flowtot, integration_id, amass2, ndmpq, &
            iqdmp, dmpq)

        use timers

        integer(kind = int_wp), intent(in   ) :: idt                   !< Time step
        integer(kind = int_wp), intent(in   ) :: isys                  !< Index of current transported substance
        integer(kind = int_wp), intent(in   ) :: num_substances_transported                 !< Number of transported substances
        integer(kind = int_wp), intent(in   ) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                 !< Number of cells or segments
        real(kind = real_wp),   intent(inout) :: conc(num_substances_total, num_cells)    !< Concentrations
        real(kind = dp),        intent(inout) :: concvt(num_cells)         !< Estimation of first solution by means of local theta method
        real(kind = real_wp),   intent(in   ) :: volnew(num_cells)         !< Cell volumes at the new time
        integer(kind = int_wp), intent(in   ) :: num_boundary_conditions                 !< Number of boundary cells
        real(kind = real_wp),   intent(in   ) :: bound(num_substances_transported, num_boundary_conditions)   !< Boundary concentrations
        integer(kind = int_wp), intent(in   ) :: num_exchanges                   !< Number of exchanges
        integer(kind = int_wp), intent(in   ) :: iknmrk(num_cells)         !< Feature array
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)        !< Exchange indeces
        real(kind = real_wp),   intent(in   ) :: area(num_exchanges)             !< Surface areas
        real(kind = real_wp),   intent(in   ) :: aleng(2, num_exchanges)         !< From- and to lengths
        real(kind = real_wp),   intent(in   ) :: theta(num_exchanges)            !< Local theta coefficients
        real(kind = real_wp),   intent(in   ) :: flowtot(num_exchanges)          !< Flow plus additional velocities
        integer(kind = int_wp), intent(in   ) :: integration_id        !< Integration option
        real(kind = real_wp),   intent(inout) :: amass2(num_substances_total, 5)      !< Area-wide mass balance array
        integer(kind = int_wp), intent(in   ) :: ndmpq                 !< Number of dumped discharges
        integer(kind = int_wp), intent(in   ) :: iqdmp(num_exchanges)            !< Indeces dumped exchages
        real(kind = real_wp),   intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< Mass balance array for monitoring areas

        ! Local variables
        real(kind = real_wp) :: length   !< Length between midpoints of cells
        real(kind = real_wp)   :: cio    !< Old concentration from cell
        real(kind = real_wp)   :: cjo    !< Old concentration to cell
        real(kind = real_wp)   :: cin    !< New (local theta) concentration from cell
        real(kind = real_wp)   :: cjn    !< New (local theta) concentration to cell
        integer(kind = int_wp) :: ifrom  !< Index from-cell
        integer(kind = int_wp) :: ito    !< Index to-cell
        integer(kind = int_wp) :: ifrom1 !< Index from-1 cell
        integer(kind = int_wp) :: itopl1 !< Index to+1 cell
        integer(kind = int_wp) :: iseg   !< Index current cell
        integer(kind = int_wp) :: iq     !< Index current edge
        real(kind = real_wp) :: aflux    !< Corrective flux
        real(kind = real_wp) :: vfrom    !< 'from' new volume
        real(kind = real_wp) :: vto      !< 'to' new volume
        real(kind = real_wp) :: dq       !< Auxiliary variable for the limiter
        real(kind = real_wp) :: e1       !< Auxiliary variable for the limiter
        real(kind = real_wp) :: e3       !< Auxiliary variable for the limiter
        real(kind = real_wp) :: s        !< Auxiliary variable for the limiter
        real(kind = real_wp) :: cfrm1    !< Concentration of from-1 cell
        real(kind = real_wp) :: ctop1    !< Concentration of to+1 cell

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqm8", ithandl)

        ! loop accross the number of exchanges
        do iq = 1, num_exchanges
            ! initialisations , check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ifrom1 = ipoint(3, iq)
            itopl1 = ipoint(4, iq)
            if (ifrom  == 0 .or.  ito    == 0) cycle
            if (ifrom  <= 0 .and. ito    <= 0) cycle
            if (ifrom1 == 0 .or.  itopl1 == 0) cycle   ! no flux correction with closed edges
            if ((ifrom  < 0 .or.  ito    < 0) .and. btest(integration_id, 2)) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle       ! identified dry at start and end of timestep
            endif                                              ! aggregated time step can be wet in between
            if (ito   > 0) then                           ! start and end, that is why a check on 1 cm3/s
                if (.not. btest(iknmrk(ito), 0)) cycle       ! life is not easy
            endif

            ! Compute the difference flux towards 2nd order
            if (ifrom > 0) then
                cio = conc  (isys, ifrom)
                cin = concvt(ifrom)
            else
                cio = bound (isys, -ifrom)
                cin = bound (isys, -ifrom)
            endif

            if (ito   > 0) then
                cjo = conc  (isys, ito)
                cjn = concvt(ito)
            else
                cjo = bound (isys, -ito)
                cjn = bound (isys, -ito)
            endif

            if (theta(iq) < 1.0E-25) then ! Lax-Wendroff flux correction at `explicit' edges (theta = 0)
                length = aleng(1, iq) + aleng(2, iq)
                if (length > 1.0e-25) then
                    if (flowtot(iq) > 0) then          ! flow from i to j
                        aflux = (aleng(1, iq) / length - (flowtot(iq) * real(idt)) / (2 * area(iq) * length)) * flowtot(iq) * (cjo - cio)
                    else                                ! flow from j to i
                        aflux = (-aleng(2, iq) / length - (flowtot(iq) * real(idt)) / (2 * area(iq) * length)) * flowtot(iq) * (cjo - cio)
                    endif
                else
                    aflux = 0.0
                endif
            else                          ! central flux correction at implicit edges (theta > 0)
                if (flowtot(iq) > 0) then ! flow from i to j
                    aflux = (1.0 - theta(iq)) * (flowtot(iq) * (cio + cjo) / 2.0 - flowtot(iq) * cio) &
                            + theta(iq) * (flowtot(iq) * (cin + cjn) / 2.0 - flowtot(iq) * cin)
                else                      ! flow from j to i
                    aflux = (1.0 - theta(iq)) * (flowtot(iq) * (cio + cjo) / 2.0 - flowtot(iq) * cjo) &
                            + theta(iq) * (flowtot(iq) * (cin + cjn) / 2.0 - flowtot(iq) * cjn)
                endif
            endif

            if (aflux * (cin - cjn) > 0) aflux = 0.0 ! antidiffusion should not behave as diffusion.
            dq = aflux * real(idt)

            ! Flux correction at the open boundaries
            if (ifrom < 0) then
                if (itopl1 <= 0) cycle
                vto = volnew(ito)
                s = sign (1.0, dq)
                e3 = (concvt(itopl1) - concvt(ito)) * vto
                dq = s * max(0.0, min(s * dq, s * e3))
                concvt(ito) = concvt(ito) + dq / vto
                if (dq > 0) then
                    amass2(isys, 4) = amass2(isys, 4) + dq
                else
                    amass2(isys, 5) = amass2(isys, 5) - dq
                endif
                if (btest(integration_id, 3)) then ! balances active
                    if (iqdmp(iq) > 0) then        ! balances to be updated
                        if (dq > 0.0) then
                            dmpq(isys, iqdmp(iq), 1) = dmpq(isys, iqdmp(iq), 1) + dq
                        else
                            dmpq(isys, iqdmp(iq), 2) = dmpq(isys, iqdmp(iq), 2) - dq
                        endif
                    endif
                endif
                cycle
            endif
            if (ito   < 0) then
                if (ifrom1 <= 0) cycle
                vfrom = volnew(ifrom)
                s = sign (1.0, dq)
                e1 = (concvt(ifrom) - concvt(ifrom1)) * vfrom
                dq = s * max(0.0, min(s * e1, s * dq))
                concvt(ifrom) = concvt(ifrom) - dq / vfrom
                if (dq > 0) then
                    amass2(isys, 5) = amass2(isys, 5) + dq
                else
                    amass2(isys, 4) = amass2(isys, 4) - dq
                endif
                if (btest(integration_id, 3)) then ! balances active
                    if (iqdmp(iq) > 0) then        ! balances to be updated
                        if (dq > 0.0) then
                            dmpq(isys, iqdmp(iq), 1) = dmpq(isys, iqdmp(iq), 1) + dq
                        else
                            dmpq(isys, iqdmp(iq), 2) = dmpq(isys, iqdmp(iq), 2) - dq
                        endif
                    endif
                endif
                cycle
            endif

            ! Boris and Book for the inner area
            vfrom = volnew(ifrom)
            vto = volnew(ito)
            if (vfrom > 1.0e-25 .and. vto > 1.0e-25) then
                s = sign (1.0, dq)
                if      (ifrom1 > 0) then
                    cfrm1 = concvt(ifrom1)
                else if (ifrom1 == 0) then
                    if (s > 0) then
                        cfrm1 = 0.0
                    else
                        cfrm1 = 2.0 * concvt(ifrom)
                    endif
                else if (ifrom1 < 0) then
                    cfrm1 = bound(isys, -ifrom1)
                endif
                if      (itopl1 > 0) then
                    ctop1 = concvt(itopl1)
                else if (itopl1 == 0) then
                    if (s > 0) then
                        ctop1 = 2.0 * concvt(ito)
                    else
                        ctop1 = 0.0
                    endif
                else if (itopl1 < 0) then
                    ctop1 = bound(isys, -itopl1)
                endif

                e1 = (concvt(ifrom) - cfrm1) * vfrom
                e3 = (ctop1 - concvt(ito)) * vto
                dq = s * max(0.0, min(s * e1, s * dq, s * e3))

                concvt(ifrom) = concvt(ifrom) - dq / vfrom
                concvt(ito) = concvt(ito) + dq / vto

                if (btest(integration_id, 3)) then ! balances active
                    if (iqdmp(iq) > 0) then        ! balances to be updated
                        if (dq > 0.0) then
                            dmpq(isys, iqdmp(iq), 1) = dmpq(isys, iqdmp(iq), 1) + dq
                        else
                            dmpq(isys, iqdmp(iq), 2) = dmpq(isys, iqdmp(iq), 2) - dq
                        endif
                    endif
                endif
            endif
        end do

        do iseg = 1, num_cells
            if (btest(iknmrk(iseg), 0)) then
                conc(isys, iseg) = concvt(iseg)
            else
                conc(isys, iseg) = 0.0
            endif
        enddo

        if (timon) call timstop (ithandl)
    end subroutine dlwqm8
end module m_dlwqm8
