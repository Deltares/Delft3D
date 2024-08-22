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
module m_dlwq16
    use m_waq_precision

    implicit none

contains

    !> Calculates transport with explicit derivatives for the advection diffusion equation.
    !! Besides the (main) water flow in the array FLOW(num_exchanges), there are optional additional velocities.
    !! These options are often used in the vertical for settling velocities of particulates or
    !! floating velocities of blue-green algae.
    !! Next to the constant diffusion terms in 3 directions contained in DISP(3), optionally
    !! additional dispersions may be specified for each substance. These options are often used
    !! in the vertical for time and space varying vertical diffusion as computed by the vertical 
    !! turbulence model in the hydrodynamic model.
    !! This option may also become common for the horizontal if the Horizontal Large Scale Eddy
    !! diffusivity as computed by the hydrodynamic model will commonly be used.
    !! This routine also accumulates on the fly the mass balance information for the whole area in
    !! the AMASS2(num_substances_total,5) array. This array is printed as header for every time step in the monitoring
    !! file.
    !! Furthermore the fluxes in and out of monitoring areas for detail balances are accumulated on
    !! the fly. IQDMP(num_exchanges) indicates Which flux needs to be accumulated in what balance.
    subroutine dlwq16(num_substances_transported, num_substances_total, num_cells, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges, num_dispersion_arrays, num_velocity_arrays, disp, &
            disper, velo, area, flow, aleng, &
            ipoint, iknmrk, idpnt, ivpnt, conc, &
            bound, integration_id, ilflag, idt, deriv, &
            iaflag, amass2, ndmpq, iqdmp, dmpq)

        use timers

        integer(kind = int_wp), intent(in   ) :: num_substances_transported                   !< Number of transported substances
        integer(kind = int_wp), intent(in   ) :: num_substances_total                   !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                   !< Number of cells or segments
        integer(kind = int_wp), intent(in   ) :: num_exchanges_u_dir                    !< Number of interfaces in direction 1
        integer(kind = int_wp), intent(in   ) :: num_exchanges_v_dir                    !< Number of interfaces in direction 2
        integer(kind = int_wp), intent(in   ) :: num_exchanges_z_dir                    !< Number of interfaces in direction 3
        integer(kind = int_wp), intent(in   ) :: num_exchanges                     !< Total number of interfaces
        integer(kind = int_wp), intent(in   ) :: num_dispersion_arrays                  !< Number of additional dispersions
        integer(kind = int_wp), intent(in   ) :: num_velocity_arrays                  !< Number of additional velocities
        real(kind = real_wp),   intent(in   ) :: disp(3)                 !< Fixed dispersions in the 3 directions
        real(kind = real_wp),   intent(in   ) :: disper(num_dispersion_arrays, num_exchanges)     !< Array with additional dispersions
        real(kind = real_wp),   intent(in   ) :: velo(num_velocity_arrays, num_exchanges)       !< Array with additional velocities
        real(kind = real_wp),   intent(in   ) :: area(num_exchanges)               !< Exchange areas in m2
        real(kind = real_wp),   intent(in   ) :: flow(num_exchanges)               !< Flows through the exchange areas in m3/s
        real(kind = real_wp),   intent(in   ) :: aleng(2, num_exchanges)           !< Mixing length to and from the exchange area
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)          !< From, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in   ) :: iknmrk(num_cells)           !< Feature array
        integer(kind = int_wp), intent(in   ) :: idpnt(num_substances_transported)            !< Index of additional dispersion per substance
                                                                         !< if <= 0, then no additional dispersion for that substance.
        integer(kind = int_wp), intent(in   ) :: ivpnt(num_substances_transported)            !< Index of additional velocity per substance
                                                                         !< if <= 0, then no additional velocity for that substance.
        real(kind = real_wp),   intent(in   ) :: conc(num_substances_total, num_cells)      !< Concentrations at previous time level
        real(kind = real_wp),   intent(in   ) :: bound(num_substances_transported, *)         !< Open boundary concentrations
        integer(kind = int_wp), intent(in   ) :: integration_id          !< Bit 0: 1 if no dispersion at zero flow
                                                                         !< Bit 1: 1 if no dispersion across boundaries
                                                                         !< Bit 3: 1 if mass balance output
        integer(kind = int_wp), intent(in   ) :: ilflag                  !< If 0 then only 3 constant lenght values
        integer(kind = int_wp), intent(in   ) :: idt                     !< Time step in seconds
        real(kind = real_wp),   intent(inout) :: deriv(num_substances_total, num_cells)     !< Explicit derivative in mass/s
        integer(kind = int_wp), intent(in   ) :: iaflag                  !< If 1 then accumulate mass in report array
        real(kind = real_wp),   intent(inout) :: amass2(num_substances_total, 5)        !< Report array for monitoring file
        integer(kind = int_wp), intent(in   ) :: ndmpq                   !< Number of dumped exchanges for mass balances
        integer(kind = int_wp), intent(in   ) :: iqdmp(num_exchanges)              !< Pointer from exchange to dump location
        real(kind = real_wp),   intent(inout) :: dmpq(num_substances_transported, ndmpq, 2)   !< Array with mass balance information

        ! Local variables
        integer(kind = int_wp) :: iq, k      !< Loop counter exchanges
        integer(kind = int_wp) :: isys       !< Loop counter substance
        integer(kind = int_wp) :: noq12      !< Number of horizontal exchanges
        integer(kind = int_wp) :: ifrom, ito !< From and to volume numbers
        real(kind = real_wp) :: a            !< This area
        real(kind = real_wp) :: q            !< Flow for this exchange
        real(kind = real_wp) :: e            !< Dispersion for this exchange
        real(kind = real_wp) :: al           !< This length
        real(kind = real_wp) :: dl           !< Area / length
        real(kind = real_wp) :: d            !< Dispersion for this substance
        real(kind = real_wp) :: v            !< Flow for this substance
        real(kind = real_wp) :: dq           !< Total flux from and to
        integer(kind = int_wp) :: ipb        !< Pointer in the mass balance dump array

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq16", ithandl)

        ! loop accross the number of exchanges
        noq12 = num_exchanges_u_dir + num_exchanges_v_dir
        do iq = 1, num_exchanges

            ! initialisations, check if transport will take place
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom <= 0 .and. ito <= 0) cycle

            a = area(iq)
            q = flow(iq)
            if (btest(integration_id, 0) .and. abs(q) < 1.0e-25)  cycle ! thin dam option, no dispersion at zero flow

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

            if (al > 1.0e-25) then
                dl = a / al
            else
                dl = 0.0
            end if
            e = e * dl ! in m3/s
            if (ifrom < 0) goto 20
            if (ito   < 0) goto 40

            ! The regular case
            do isys = 1, num_substances_transported
                d = e
                v = q
                if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                if (ivpnt(isys) > 0) v = v + velo  (ivpnt(isys), iq) * a

                ! upwinding
                if (v > 0.0) then
                    dq = (v + d) * conc(isys, ifrom) - d * conc(isys, ito)
                else
                    dq = (v - d) * conc(isys, ito) + d * conc(isys, ifrom)
                end if
                deriv(isys, ifrom) = deriv(isys, ifrom) - dq
                deriv(isys, ito) = deriv(isys, ito) + dq

                ! balances
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq * idt
                    else
                        dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq * idt
                    end if
                end if
            end do
            cycle

            ! The 'from' element was a boundary. Note the 2 options.
            20    do isys = 1, num_substances_transported
                v = q
                d = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                end if
                if (ivpnt(isys) > 0) v = v + velo  (ivpnt(isys), iq) * a

                ! upwinding
                if (v > 0.0) then
                    dq = (v + d) * bound(isys, -ifrom) - d * conc (isys, ito)
                else
                    dq = (v - d) * conc (isys, ito) + d * bound(isys, -ifrom)
                end if
                deriv(isys, ito) = deriv(isys, ito) + dq

                ! balances
                if (iaflag == 1) then
                    if (dq > 0.0) then
                        amass2(isys, 4) = amass2(isys, 4) + dq * idt
                    else
                        amass2(isys, 5) = amass2(isys, 5) - dq * idt
                    end if
                end if
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq * idt
                    else
                        dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq * idt
                    end if
                end if
            end do
            cycle

            ! The 'to' element was a boundary.
            40    do isys = 1, num_substances_transported
                v = q
                d = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                end if
                if (ivpnt(isys) > 0) v = v + velo  (ivpnt(isys), iq) * a

                ! upwinding
                if (v > 0.0) then
                    dq = (v + d) * conc(isys, ifrom) - d * bound(isys, -ito)
                else
                    dq = (v - d) * bound(isys, -ito) + d * conc(isys, ifrom)
                end if
                deriv(isys, ifrom) = deriv(isys, ifrom) - dq

                ! balances
                if (iaflag == 1) then
                    if (dq > 0.0) then
                        amass2(isys, 5) = amass2(isys, 5) + dq * idt
                    else
                        amass2(isys, 4) = amass2(isys, 4) - dq * idt
                    end if
                end if
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq * idt
                    else
                        dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq * idt
                    end if
                end if
            end do

        ! end of the loop over exchanges
        end do
        if (timon) call timstop (ithandl)
    end subroutine dlwq16
end module m_dlwq16
