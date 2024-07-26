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
module m_dlwq51
    use m_waq_precision

    implicit none

contains

    !> Performs flux correction in the direction of flow according to Boris and Book.
    !! This routine makes for the num_substances_transported transported substaces the flux correction term after
    !! the upwind advection step is set by dlwq50/18 producing CONC2(num_substances_total,num_cells) array.
    !! The numerical diffusion of the upwind method amounts to 0.5*v*dx - 0.5*v*v*dt. The
    !! numerical diffusion is subtracted from the prescribed diffusion. The resulting flux
    !! correction term, if negative, is applied as negative diffusion. This negative diffusion
    !! term is limited to the value that new maxima and minima would result.
    !! the criterion against new maxima and minima is measured compared to the old level
    !! concentration field in CONC(num_substances_total,num_cells).
    !! This method of Boris and Book looks in the flow direction only to limit the flux
    !! correction term. Salezac's method looks around a cell, but that has disadvantages in
    !! stratified systems.
    !! The application is subject to the following switches:
    !! - 1) no dispersion accross open boundaries (bit 1 of integration_id is 1)
    !! - 2) first order processing accross open boundaries (bit 2 of integration_id is 1)
    !! Because the routine corrects the CONC2 array, the flux correction (as mass/timestep)
    !! is divided by the new volume of the cell to get concentratiosn again.
    !! This routine also accumulates on the fly the mass balance information for the whole area in
    !! the AMASS2(num_substances_total,5) array. This array is printed as header for every time step in the monitoring
    !! file.
    !! Furthermore the fluxes in and out of monitoring areas for detail balances are accumulated on
    !! the fly. IQDMP(num_exchanges) indicates which flux needs to be accumulated in what balance.
    subroutine dlwq51(num_substances_transported, num_substances_total, num_cells, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges, num_dispersion_arrays, num_velocity_arrays, disp, &
            disper, velo, volume, area, flow, &
            aleng, ipoint, iknmrk, idpnt, ivpnt, &
            conc, conc2, bound, integration_id, ilflag, &
            idt, iaflag, amass2, ndmpq, iqdmp, &
            dmpq)

        use timers

        integer(kind = int_wp), intent(in   ) :: num_substances_transported                 !< number of transported substances
        integer(kind = int_wp), intent(in   ) :: num_substances_total                 !< total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                 !< number of computational volumes
        integer(kind = int_wp), intent(in   ) :: num_exchanges_u_dir                  !< number of interfaces in direction 1
        integer(kind = int_wp), intent(in   ) :: num_exchanges_v_dir                  !< number of interfaces in direction 2
        integer(kind = int_wp), intent(in   ) :: num_exchanges_z_dir                  !< number of interfaces in direction 3
        integer(kind = int_wp), intent(in   ) :: num_exchanges                   !< total number of interfaces
        integer(kind = int_wp), intent(in   ) :: num_dispersion_arrays                !< number additional dispersions
        integer(kind = int_wp), intent(in   ) :: num_velocity_arrays                !< number additional velocities
        real(kind = real_wp),   intent(in   ) :: disp(3)               !< fixed dispersions in the 3 directions
        real(kind = real_wp),   intent(in   ) :: disper(num_dispersion_arrays, num_exchanges)   !< array with additional dispersions
        real(kind = real_wp),   intent(in   ) :: velo  (num_velocity_arrays, num_exchanges)   !< array with additional velocities
        real(kind = real_wp),   intent(in   ) :: volume(num_cells)         !< volumes at end of time step
        real(kind = real_wp),   intent(in   ) :: area(num_exchanges)             !< exchange areas in m2
        real(kind = real_wp),   intent(in   ) :: flow(num_exchanges)             !< flows through the exchange areas in m3/s
        real(kind = real_wp),   intent(in   ) :: aleng(2, num_exchanges)         !< mixing length to and from the exchange area
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)        !< from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in   ) :: iknmrk(num_cells)         !< feature array
        integer(kind = int_wp), intent(in   ) :: idpnt(num_substances_transported)          !< additional dispersion number per substance
        integer(kind = int_wp), intent(in   ) :: ivpnt(num_substances_transported)          !< additional velocity number per substance
        real(kind = real_wp),   intent(in   ) :: conc (num_substances_total, num_cells)   !< concentrations at previous time level
        real(kind = real_wp),   intent(inout) :: conc2(num_substances_total, num_cells)   !< first estimate to be flux corrected
        real(kind = real_wp),   intent(in   ) :: bound(num_substances_transported, *)       !< open boundary concentrations
        integer(kind = int_wp), intent(in   ) :: integration_id        !< bit 0: 1 if no dispersion at zero flow
                                                                       !< bit 1: 1 if no dispersion across boundaries
                                                                       !< bit 2: 1 if lower order across boundaries
                                                                       !< bit 3: 1 if mass balance output
        integer(kind = int_wp), intent(in   ) :: ilflag                !< if 0 then only 3 constant lenght values
        integer(kind = int_wp), intent(in   ) :: idt                   !< time step in seconds
        integer(kind = int_wp), intent(in   ) :: iaflag                !< if 1 then accumulate mass in report array
        real(kind = real_wp),   intent(inout) :: amass2(num_substances_total, 5)      !< report array for monitoring file
        integer(kind = int_wp), intent(in)    :: ndmpq                 !< number of dumped exchanges for mass balances
        integer(kind = int_wp), intent(in)    :: iqdmp(num_exchanges)            !< pointer from echange to dump location
        real(kind = real_wp),   intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< array with mass balance information

        ! Local variables
        integer(kind = int_wp) :: iq             !< loop counter exchanges
        integer(kind = int_wp) :: isys           !< loop counter substance
        integer(kind = int_wp) :: noq12          !< number of horizontal exchanges
        integer(kind = int_wp) :: ifrom, ito     !< from   and to   volume numbers
        real(kind = real_wp) :: vfrom, vto       !< from   and to   volumes
        integer(kind = int_wp) :: ifrom_1, ito_1 !< from-1 and to+1 volume numbers
        real(kind = real_wp) :: cfrm_1, cto_1    !< from-1 and to+1 concentration values
        real(kind = real_wp) :: e1, e2, e3       !< limiter help variable
        real(kind = real_wp) :: s                !< limiter sign variable
        real(kind = real_wp) :: a                !< this area
        real(kind = real_wp) :: q                !< flow for this exchange
        real(kind = real_wp) :: e                !< dispersion for this exchange
        real(kind = real_wp) :: al               !< this length
        real(kind = real_wp) :: dl               !< area / length
        real(kind = real_wp) :: d                !< dispersion for this substance
        real(kind = real_wp) :: v                !< flow for this substance
        real(kind = real_wp) :: dq               !< total flux from and to
        integer(kind = int_wp) :: ipb            !< pointer in the mass balance dump array
        real(kind = real_wp) :: f1, f2           !< correction factors central differences
        real(kind = real_wp) :: dqtr, dqtot      !< balances help variables

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq51", ithandl)

        ! loop accross the number of exchanges
        noq12 = num_exchanges_u_dir + num_exchanges_v_dir
        do iq = 1, num_exchanges
            ! initialisations , check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ifrom_1 = ipoint(3, iq)
            ito_1 = ipoint(4, iq)
            if (ifrom   == 0 .or. ito   == 0) cycle
            if (ifrom <= 0 .and. ito <= 0) cycle

            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle   ! identified dry at start and end of timestep
            end if
            if (ito   > 0) then
                if (.not. btest(iknmrk(ito), 0)) cycle
            end if

            a = area(iq)
            q = flow(iq)
            if (abs(q) < 10.0e-25 .and. iq <= noq12 .and. btest(integration_id, 0))  cycle
            ! thin dam option, no dispersion at zero flow
            ! Check if exchange is dump exchange, set IPB

            ipb = 0
            if (btest(integration_id, 3)) then
                if (iqdmp(iq) > 0) ipb = iqdmp(iq)
            end if

            ! initialize uniform values
            if (iq <= num_exchanges_u_dir) then
                e = disp (1)
                al = aleng(1, 1)
            elseif (iq <= noq12) then
                e = disp (2)
                al = aleng(2, 1)
            else
                e = disp (3)
                al = aleng(1, 2)
            end if
            if (iq > noq12 + num_exchanges_z_dir) e = 0.0     ! in the bed

            if (ilflag == 1) al = aleng(1, iq) + aleng(2, iq)
            if (al < 1.0e-25) cycle
            if (ilflag == 1) then
                f1 = aleng(1, iq) / al
            else
                f1 = 0.5
            end if
            dl = a / al
            e = e * dl                              ! in m3/s
            !
            if (ifrom < 0) goto 20
            if (ito   < 0) goto 40

            ! correction step
            vfrom = volume(ifrom)
            vto = volume(ito)
            do isys = 1, num_substances_transported
                v = q
                if (ivpnt(isys) > 0) v = v + velo  (ivpnt(isys), iq) * a
                f2 = f1
                if (v < 0.0) f2 = f2 - 1.0
                d = -f2 * v + e + 0.5 * v * v * idt / a / al
                d = min(d, e)
                if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                dq = (conc(isys, ifrom) - conc(isys, ito)) * d * idt
                if (d < 0.0) then
                    e2 = dq
                    s = sign (1.0, e2)
                    select case (ifrom_1)
                    case (1:)
                        cfrm_1 = conc2(isys, ifrom_1)
                    case (0)
                        if (s > 0) then
                            cfrm_1 = 0.0
                        else
                            cfrm_1 = 2.0 * conc2(isys, ifrom)
                        end if
                    case (:-1)
                        cfrm_1 = bound(isys, -ifrom_1)
                    end select
                    select case (ito_1)
                    case (1:)
                        cto_1 = conc2(isys, ito_1)
                    case (0)
                        if (s > 0) then
                            cto_1 = 2.0 * conc2(isys, ito)
                        else
                            cto_1 = 0.0
                        end if
                    case (:-1)
                        cto_1 = bound(isys, -ito_1)
                    end select

                    e1 = (conc2(isys, ifrom) - cfrm_1) * vfrom
                    e3 = (cto_1 - conc2(isys, ito)) * vto
                    dq = s * max(0.0, min(s * e1, s * e2, s * e3))
                end if

                conc2(isys, ifrom) = conc2(isys, ifrom) - dq / vfrom
                conc2(isys, ito) = conc2(isys, ito) + dq / vto

                if (ipb > 0) then                          ! recalculate
                    if (v > 0.0) then                       ! transport
                        dqtr = v * conc(isys, ifrom) * idt         ! for the mass
                    else                                         ! balance
                        dqtr = v * conc(isys, ito) * idt
                    end if
                    dqtot = dq + dqtr
                    if (dqtot > 0.0) then
                        dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dqtot
                    else
                        dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dqtot
                    end if
                end if
            end do
            cycle

            ! The 'from' element was a boundary. Note the 2 options.
            20    vto = volume(ito)
            do isys = 1, num_substances_transported
                v = q
                if (ivpnt(isys) > 0) v = v + velo  (ivpnt(isys), iq) * a
                d = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                end if
                if (.not. btest(integration_id, 2)) then
                    f2 = f1
                    if (v < 0.0) f2 = f2 - 1.0
                    d = d - f2 * v + 0.5 * v * v * idt / a / al
                    d = min(d, e)
                end if
                dq = d * (bound(isys, -ifrom) - conc(isys, ito))
                conc2(isys, ito) = conc2(isys, ito) + dq * idt / vto

                if (iaflag == 1) then
                    if (dq > 0) then
                        amass2(isys, 4) = amass2(isys, 4) + dq * idt
                    else
                        amass2(isys, 5) = amass2(isys, 5) - dq * idt
                    end if
                end if

                if (ipb > 0) then                          ! recalculate
                    if (v > 0.0) then                       ! transport
                        dqtr = v * bound(isys, -ifrom)             ! for the mass
                    else                                         ! balance
                        dqtr = v * conc (isys, ito)
                    end if
                    dqtot = dq + dqtr
                    if (dqtot > 0.0) then
                        dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dqtot * idt
                    else
                        dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dqtot * idt
                    end if
                end if
            end do
            cycle

            ! The 'to' element was a boundary.
            40    vfrom = volume(ifrom)
            do isys = 1, num_substances_transported
                v = q
                if (ivpnt(isys) > 0) v = v + velo  (ivpnt(isys), iq) * a
                d = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                end if
                if (.not. btest(integration_id, 2)) then
                    f2 = f1
                    if (v < 0) f2 = f2 - 1.0
                    d = -f2 * v + d + 0.5 * v * v * idt / a / al
                    d = min(d, e)
                end if
                dq = d * (conc(isys, ifrom) - bound(isys, -ito))
                conc2(isys, ifrom) = conc2(isys, ifrom) - dq * idt / vfrom

                if (iaflag == 1) then
                    if (dq > 0) then
                        amass2(isys, 5) = amass2(isys, 5) + dq * idt
                    else
                        amass2(isys, 4) = amass2(isys, 4) - dq * idt
                    end if
                end if

                if (ipb > 0) then                          ! recalculate
                    if (v > 0.0) then                       ! transport
                        dqtr = v * conc (isys, ifrom)             ! for the mass
                    else                                         ! balance
                        dqtr = v * bound(isys, -ito)
                    end if
                    dqtot = dq + dqtr
                    if (dqtot > 0.0) then
                        dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dqtot * idt
                    else
                        dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dqtot * idt
                    end if
                end if
            end do
            ! end of the loop over exchanges
        end do

        if (timon) call timstop (ithandl)
    end subroutine dlwq51
end module m_dlwq51
