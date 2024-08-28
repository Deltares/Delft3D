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
module m_scheme_23_utils
    use m_waq_precision
    use timers

    implicit none

    private
    public :: make_explicit_derivatives_leonard

contains

    !! Makes explicit derivatives according to Leonards QUICKEST
    subroutine make_explicit_derivatives_leonard (num_substances_transported, num_substances_total, num_cells, &
            num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges, num_dispersion_arrays, &
            num_velocity_arrays, disp, &
            disper, velo, area, flow, aleng, &
            ipoint, iknmrk, idpnt, ivpnt, conc, &
            bound, integration_id, ilflag, idt, deriv, &
            iaflag, amass2, ndmpq, iqdmp, dmpq)

        integer(kind = int_wp), intent(in) :: num_substances_transported          ! number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                ! total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                ! number of computational volumes
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir                 ! number of interfaces in direction 1
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir                 ! number of interfaces in direction 2
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir                 ! number of interfaces in direction 3
        integer(kind = int_wp), intent(in) :: num_exchanges                  ! total number of interfaces
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays               ! number additional dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays               ! number additional velocities
        real(kind = real_wp), intent(in) :: disp  (3)            ! fixed dispersions in the 3 directions
        real(kind = real_wp), intent(in) :: disper(num_dispersion_arrays, num_exchanges)! array with additional dispersions
        real(kind = real_wp), intent(in) :: velo  (num_velocity_arrays, num_exchanges)! array with additional velocities
        real(kind = real_wp), intent(in) :: area  (num_exchanges)          ! exchange areas in m2
        real(kind = real_wp), intent(in) :: flow  (num_exchanges)          ! flows through the exchange areas in m3/s
        real(kind = real_wp), intent(in) :: aleng (2, num_exchanges)   ! mixing length to and from the exchange area
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)   ! from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)        ! feature array
        integer(kind = int_wp), intent(in) :: idpnt (num_substances_transported)!additional dispersion number per substance
        integer(kind = int_wp), intent(in) :: ivpnt (num_substances_transported) ! additional velocity number per substance
        real(kind = real_wp), intent(in) :: conc  (num_substances_total, num_cells)  ! concentrations at previous time level
        real(kind = real_wp), intent(in) :: bound (num_substances_transported, *)  ! open boundary concentrations
        integer(kind = int_wp), intent(in) :: integration_id                 ! bit 0: 1 if no dispersion at zero flow
        ! bit 1: 1 if no dispersion across boundaries
        ! bit 2: 1 if lower order across boundaries
        ! bit 3: 1 if mass balance output
        integer(kind = int_wp), intent(in) :: ilflag               ! if 0 then only 3 constant lenght values
        integer(kind = int_wp), intent(in) :: idt                  ! time step in seconds
        real(kind = real_wp), intent(inout) :: deriv (num_substances_total, num_cells)  ! explicit derivative in mass/s
        integer(kind = int_wp), intent(in) :: iaflag               ! if 1 then accumulate mass in report array
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)  ! report array for monitoring file
        integer(kind = int_wp), intent(in) :: ndmpq                ! number of dumped exchanges for mass balances
        integer(kind = int_wp), intent(in) :: iqdmp (num_exchanges)          ! pointer from echange to dump location
        real(kind = real_wp), intent(inout) :: dmpq  (num_substances_transported, ndmpq, 2)! array with mass balance information

        ! Local variables
        integer(kind = int_wp) :: iq              ! loop counter exchanges
        integer(kind = int_wp) :: substance_i            ! loop counter substance
        integer(kind = int_wp) :: noq12           ! number of horizontal exchanges
        integer(kind = int_wp) :: ifrom, ito      ! from and to volume numbers
        integer(kind = int_wp) :: ifrom_1, ito_1  ! volume numbers one further at both sides
        real(kind = real_wp) :: a               ! this area
        real(kind = real_wp) :: q               ! flow for this exchange
        real(kind = real_wp) :: e               ! dispersion for this exchange
        real(kind = real_wp) :: al              ! this length
        real(kind = real_wp) :: f1, f2          ! interpolation factor central differences
        real(kind = real_wp) :: g1, g2          ! interpolation factor boundaries
        real(kind = real_wp) :: de, dv          ! help variables Lax Wendroff scheme
        real(kind = real_wp) :: dl              ! area / length
        real(kind = real_wp) :: d               ! dispersion for this substance
        real(kind = real_wp) :: v               ! flow for this substance
        real(kind = real_wp) :: dq              ! total flux from and to
        integer(kind = int_wp) :: ipb             ! pointer in the mass balance dump array
        real(kind = real_wp) :: c               ! velocity Courant number
        real(kind = real_wp) :: d2              ! diffusion courant number
        real(kind = real_wp) :: su              ! upwind half length

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("make_explicit_derivatives_leonard", ithandl)

        !         loop accross the number of exchanges

        noq12 = num_exchanges_u_dir + num_exchanges_v_dir
        do iq = 1, num_exchanges

            !         initialisations, check if transport will take place

            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom <= 0 .and. ito <= 0) cycle
            ifrom_1 = ipoint(3, iq)
            ito_1 = ipoint(4, iq)

            a = area(iq)
            q = flow(iq)
            if (abs(q) < 10.0e-25) then
                if (btest(integration_id, 0)) cycle                  ! thin dam option, no dispersion at zero flow
            endif
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle   ! identified dry at start and end of timestep
            endif
            if (ito   > 0) then
                if (.not. btest(iknmrk(ito), 0)) cycle
            endif

            !     Check if exchange is dump exchange, set IPB

            ipb = 0
            if (btest(integration_id, 3)) then
                if (iqdmp(iq) > 0) ipb = iqdmp(iq)
            endif

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
            endif
            su = al
            if (iq > noq12 + num_exchanges_z_dir) e = 0.0     ! in the bed

            if (ilflag == 1) then
                al = aleng(1, iq) + aleng(2, iq)
                if (q > 0) then
                    su = aleng(1, iq) * 2.0
                else
                    su = aleng(2, iq) * 2.0
                endif
            endif
            if (al < 1.0E-25) cycle
            if (ilflag == 1) then
                f1 = aleng(2, iq) / al
                f2 = aleng(1, iq) / al
            else
                f1 = 0.5
                f2 = 0.5
            endif
            de = 0.5 * q * q * idt / a / al                  ! 0.5 v^2 *dt
            dl = a / al
            e = e * dl                              ! in m3/s
            if (ifrom < 0) goto 20
            if (ito   < 0) goto 40

            ! The regular case
            do substance_i = 1, num_substances_transported
                d = e + de
                v = q
                if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                d2 = d * idt / dl / al / al                  ! diffusion Courant number without LW-additional diffusion
                if (ivpnt(substance_i) > 0) then
                    dv = velo  (ivpnt(substance_i), iq) * a
                    v = v + dv
                    d = d + 0.5 * dv * dv * idt / a / al
                endif
                c = v * idt / a / su                      ! velocity  Courant number ( 0.5 of upstream half cell here )
                if (v > 0) then
                    if (ifrom_1 > 0) then
                        dq = ((2.0 + c * c + 6.0 * c * d2) * conc (substance_i, ito) + &
                                (5.0 - 2.0 * c * c - 12.0 * c * d2) * conc (substance_i, ifrom) + &
                                (-1.0 + c * c + 6.0 * c * d2) * conc (substance_i, ifrom_1)) / 6.0
                    else if (ifrom_1 < 0) then
                        dq = ((2.0 + c * c + 6.0 * c * d2) * conc (substance_i, ito) + &
                                (5.0 - 2.0 * c * c - 12.0 * c * d2) * conc (substance_i, ifrom) + &
                                (-1.0 + c * c + 6.0 * c * d2) * bound(substance_i, -ifrom_1)) / 6.0
                    else
                        dq = (conc(substance_i, ito) + conc(substance_i, ifrom)) / 2.0
                    endif
                else
                    if (ito_1   > 0) then
                        dq = ((2.0 + c * c + 6.0 * c * d2) * conc (substance_i, ifrom) + &
                                (5.0 - 2.0 * c * c - 12.0 * c * d2) * conc (substance_i, ito) + &
                                (-1.0 + c * c + 6.0 * c * d2) * conc (substance_i, ito_1)) / 6.0
                    else if (ito_1   < 0) then
                        dq = ((2.0 + c * c + 6.0 * c * d2) * conc (substance_i, ifrom) + &
                                (5.0 - 2.0 * c * c - 12.0 * c * d2) * conc (substance_i, ito) + &
                                (-1.0 + c * c + 6.0 * c * d2) * bound(substance_i, -ito_1)) / 6.0
                    else
                        dq = (conc(substance_i, ito) + conc(substance_i, ifrom)) / 2.0
                    endif
                endif
                dq = v * dq + d * conc(substance_i, ifrom) - d * conc(substance_i, ito)
                deriv(substance_i, ifrom) = deriv(substance_i, ifrom) - dq
                deriv(substance_i, ito) = deriv(substance_i, ito) + dq

                ! balances
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq * idt
                    else
                        dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq * idt
                    endif
                endif
            enddo
            cycle

            ! The 'from' element was a boundary.
            20 do substance_i = 1, num_substances_transported
                v = q
                d = 0.0
                dv = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                endif
                if (ivpnt(substance_i) > 0) then
                    dv = velo  (ivpnt(substance_i), iq) * a
                    v = v + dv
                endif
                if (btest(integration_id, 2)) then
                    if (v > 0) then
                        g1 = 1.0
                        g2 = 0.0
                    else
                        g1 = 0.0
                        g2 = 1.0
                    endif
                else
                    g1 = f1
                    g2 = f2
                    d = d + de + 0.5 * dv * dv * idt / a / al
                endif
                dq = (v * g1 + d) * bound(substance_i, -ifrom) + (v * g2 - d) * conc(substance_i, ito)
                deriv(substance_i, ito) = deriv(substance_i, ito) + dq

                ! balances
                if (iaflag == 1) then
                    if (dq > 0.0) then
                        amass2(substance_i, 4) = amass2(substance_i, 4) + dq * idt
                    else
                        amass2(substance_i, 5) = amass2(substance_i, 5) - dq * idt
                    endif
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq * idt
                    else
                        dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq * idt
                    endif
                endif
            enddo
            cycle

            ! The 'to' element was a boundary.
            40 do substance_i = 1, num_substances_transported
                v = q
                d = 0.0
                dv = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                endif
                if (ivpnt(substance_i) > 0) then
                    dv = velo  (ivpnt(substance_i), iq) * a
                    v = v + dv
                endif
                if (btest(integration_id, 2)) then
                    if (v > 0) then
                        g1 = 1.0
                        g2 = 0.0
                    else
                        g1 = 0.0
                        g2 = 1.0
                    endif
                else
                    g1 = f1
                    g2 = f2
                    d = d + de + 0.5 * dv * dv * idt / a / al
                endif
                dq = (v * g1 + d) * conc(substance_i, ifrom) + (v * g2 - d) * bound(substance_i, -ito)
                deriv(substance_i, ifrom) = deriv(substance_i, ifrom) - dq

                ! balances
                if (iaflag == 1) then
                    if (dq > 0.0) then
                        amass2(substance_i, 5) = amass2(substance_i, 5) + dq * idt
                    else
                        amass2(substance_i, 4) = amass2(substance_i, 4) - dq * idt
                    endif
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq * idt
                    else
                        dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq * idt
                    endif
                endif
            enddo
            ! end of the loop over exchanges
        end do

        if (timon) call timstop (ithandl)
    end subroutine make_explicit_derivatives_leonard

end module m_scheme_23_utils
