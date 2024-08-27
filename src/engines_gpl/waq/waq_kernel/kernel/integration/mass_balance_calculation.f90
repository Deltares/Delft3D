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
module m_mass_balance_calculation
    use m_waq_precision
    use timers

    implicit none

    private
    public :: calculate_mass_balance_steady_state, calculate_mass_balance_space_central_difference, &
            calculate_mass_balance_implicit_schemes, calculate_mass_balance_copy_sol_to_concentration, &
            calculate_mass_balance_for_theta_algorithm

contains


    !> Makes a mass balance final to steady state solutions.
    subroutine calculate_mass_balance_steady_state(disp, disper, area, flow, aleng, &
            velo, conc, bound, ipoint, num_substances_transported, &
            num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, num_dispersion_arrays, &
            num_velocity_arrays, idpnt, ivpnt, integration_id, amass2, &
            ilflag, dmpq, ndmpq, iqdmp)

        real(kind = real_wp), intent (in) :: disp(3)     !< Main dispersion in the 3 directions
        real(kind = real_wp), intent (in) :: disper(*)   !< Additional dispersion (num_dispersion_arrays*num_exchanges)
        real(kind = real_wp), intent (in) :: area(*)        !< Exchange surface area
        real(kind = real_wp), intent (in) :: flow(*)        !< Flows accross exchange surfs
        real(kind = real_wp), intent (in) :: aleng(*)       !< From- and to lengths
        real(kind = real_wp), intent (in) :: velo(*)        !< Additional velocity
        real(kind = real_wp), intent (in) :: conc(*)        !< Concentrations
        real(kind = real_wp), intent (in) :: bound(*)       !< Boundary concentrations
        integer(kind = int_wp), intent(in) :: ipoint(4, *)   !< Exchange indices
        integer(kind = int_wp), intent(in) :: num_substances_transported          !< Number  of active substances
        integer(kind = int_wp), intent(in) :: num_substances_total          !< Number  of total substances
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir           !< Number of exchanges in first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir           !< Number of exchanges in second direction
        integer(kind = int_wp), intent(in) :: num_exchanges            !< Total number of exchanges
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays         !< Number of additional dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays         !< Number of additional velocities
        integer(kind = int_wp), intent(in) :: idpnt(*)       !< Pointer systems to dispersions
        integer(kind = int_wp), intent(in) :: ivpnt(*)       !< Pointer systems to velocities
        integer(kind = int_wp), intent(in) :: integration_id !< = 0, 2 DISP at zero flow
        !< = 1, 3 no DISP at zero flow
        !< = 0, 1 DISP over boundary
        !< = 2, 3 no DISP over boundary
        real(kind = real_wp), intent(inout) :: amass2(*)      !< Mass balance
        integer(kind = int_wp), intent(in) :: ilflag         !< If 0 then 3 length values
        real(kind = real_wp), intent(inout) :: dmpq(*)        !< Mass balance dumped exchange if intopt>7
        integer(kind = int_wp), intent(in) :: ndmpq          !< Number of dumped exchanges
        integer(kind = int_wp), intent(in) :: iqdmp(*)       !< Pointer dumped exchanges

        ! Local variables
        logical :: ibflag
        integer(kind = int_wp) :: i, j, iq, is, i3, i4, i5, i6
        integer(kind = int_wp) :: ioptm, ipb, ipq, k1, k2
        integer(kind = int_wp) :: ithandl = 0

        real(kind = real_wp) :: q, a, al, e, dl, d, v, dq

        if (timon) call timstrt ("calculate_mass_balance_steady_state", ithandl)

        ! Loop accross the number of exchanges
        i4 = 3 * num_substances_total
        i5 = 4 * num_substances_total
        i6 = num_substances_transported * ndmpq
        ibflag = mod(integration_id, 16) >= 8
        !
        do iq = 1, num_exchanges

            ! Initialistations, check for transport anyhow
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) goto 60

            ! Check if exchange is dump exchange, set ipb
            if (ibflag) then
                if (iqdmp(iq) > 0) then
                    ipb = iqdmp(iq)
                    ipq = (iqdmp(iq) - 1) * num_substances_transported
                else
                    ipb = 0
                end if
            else
                ipb = 0
            end if
            if (i > 0 .and. j > 0 .and. ipb == 0) goto 60
            a = area(iq)
            q = flow(iq)
            if (mod(integration_id, 2) == 1) then
                if (abs(q) < 10.0e-25)  goto 60
            end if
            e = disp(1)
            al = aleng(1)
            if (iq > num_exchanges_u_dir) then
                e = disp (2)
                al = aleng(2)
            end if
            if (iq > num_exchanges_u_dir + num_exchanges_v_dir) then
                e = disp (3)
                al = aleng(3)
            end if
            if (ilflag == 1) then
                dl = a / (aleng(2 * iq - 1) + aleng(2 * iq))
            else
                dl = a / al
            end if
            e = e * dl
            if (i < 0) goto 20
            if (j < 0) goto 40

            ! The regular case
            k1 = (i - 1) * num_substances_total
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)

                ! Dispersion
                if (idpnt(is) > 0) then
                    d = e + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                else
                    d = e
                end if

                ! Flow
                if (ivpnt(is) > 0) then
                    v = q + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                else
                    v = q
                end if

                ! Transport
                if (v > 0.0) then
                    dq = (v + d) * conc(k1 + i3) - d * conc(k2 + i3)
                else
                    dq = (v - d) * conc(k2 + i3) + d * conc(k1 + i3)
                end if

                ! Mass balance
                if (dq > 0.0) then
                    dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                else
                    dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                end if
                !
            end do
            goto 60

            ! The 'from' element was a boundary. Note the 2 options.
            20 if (j < 0) goto 60
            k1 = (-i - 1) * num_substances_total
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                v = q
                d = 0.0
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                if (mod(integration_id, 4) <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                end if
                if (v > 0.0) then
                    dq = (v + d) * bound(k1 + i3) - d * conc (k2 + i3)
                else
                    dq = (v - d) * conc (k2 + i3) + d * bound(k1 + i3)
                end if
                if (dq > 0.0) then
                    amass2(i3 + i4) = amass2(i3 + i4) + dq
                else
                    amass2(i3 + i5) = amass2(i3 + i5) - dq
                end if
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    end if
                end if
            end do
            goto 60

            ! The 'to' element was a boundary.
            40 k1 = (i - 1) * num_substances_total
            k2 = (-j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                v = q
                d = 0.0
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                if (mod(integration_id, 4)  <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                end if
                if (v > 0.0) then
                    dq = (v + d) * conc (k1 + i3) - d * bound(k2 + i3)
                else
                    dq = (v - d) * bound(k2 + i3) + d * conc (k1 + i3)
                end if
                if (dq > 0.0) then
                    amass2(i3 + i5) = amass2(i3 + i5) + dq
                else
                    amass2(i3 + i4) = amass2(i3 + i4) - dq
                end if
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    end if
                end if
            end do

            ! End of the loop over exchanges
            60 continue
        end do
        if (timon) call timstop (ithandl)
    end subroutine calculate_mass_balance_steady_state

    !> Makes a mass balance with central differencing in space.
    subroutine calculate_mass_balance_space_central_difference(disp, disper, area, flow, aleng, &
            velo, conc, bound, ipoint, num_substances_transported, &
            num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, num_dispersion_arrays, &
            num_velocity_arrays, idpnt, ivpnt, integration_id, amass2, &
            ilflag, dmpq, ndmpq, iqdmp)

        real(kind = real_wp), intent(in) :: disp(3)        !< Dispersion in 3 directions
        real(kind = real_wp), intent(in) :: disper(*)      !< Additional dispersion array (num_dispersion_arrays*num_exchanges)
        real(kind = real_wp), intent(in) :: area(*)        !< Exchange surface area (num_exchanges)
        real(kind = real_wp), intent(in) :: flow(*)        !< Flows accross exchange surfs (num_exchanges)
        real(kind = real_wp), intent(in) :: aleng(*)       !< From- and to lengths (2*num_exchanges)
        real(kind = real_wp), intent(in) :: velo(*)   !< Additional velocity array (num_velocity_arrays*num_exchanges)
        real(kind = real_wp), intent(in) :: conc (*)       !< Concentrations (num_substances_total*num_cells)
        real(kind = real_wp), intent(in) :: bound(*)       !< Boundary concentrations (num_substances_total*?)
        integer(kind = int_wp), intent(in) :: ipoint(4, *)   !< Exchange pointers
        integer(kind = int_wp), intent(in) :: num_substances_transported          !< Number of active substances
        integer(kind = int_wp), intent(in) :: num_substances_total          !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir           !< Nr of exchanges in first dir.
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir           !< Nr of exchanges in second dir.
        integer(kind = int_wp), intent(in) :: num_exchanges            !< Total number of exchanges
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays         !< Number  of additional dispers.
        integer(kind = int_wp), intent(in) :: num_velocity_arrays         !< Number  of additional velos.
        integer(kind = int_wp), intent(in) :: idpnt(*)    !< Pointer systems to dispersions (num_substances_transported)
        integer(kind = int_wp), intent(in) :: ivpnt(*)    !< Pointer systems to velocities (num_substances_transported)
        integer(kind = int_wp), intent(in) :: integration_id !< = 0, 2 DISP at zero flow
        !< = 1 or 3 no DISP at zero flow
        !< = 0 or 1 DISP over boundary
        !< = 2 or 3 no DISP over boundary
        real(kind = real_wp), intent(inout) :: amass2(*)      !< Mass balance array (num_substances_total*5)
        integer(kind = int_wp), intent(in) :: ilflag         !< If 0 then 3 length values
        real(kind = real_wp), intent(inout) :: dmpq(*)        !< Mass balance dumped exchange (num_substances_total*NDMPQ*?)
        !< If INTOPT > 7
        integer(kind = int_wp), intent(in) :: ndmpq          !< Number of dumped exchanges
        integer(kind = int_wp), intent(in) :: iqdmp(*)       !< Pointer dumped exchanges

        ! Local variables
        integer(kind = int_wp) :: i, i3, i4, i5, i6, is, iq, ibflag, ipb, ipq
        integer(kind = int_wp) :: j, k1, k2
        integer(kind = int_wp) :: ithandl = 0

        real(kind = real_wp) :: a, q, e, d, v, al, dl, dv, dq, f1, f2

        if (timon) call timstrt ("calculate_mass_balance_space_central_difference", ithandl)

        ! loop accross the number of exchanges
        i4 = 3 * num_substances_total
        i5 = 4 * num_substances_total
        i6 = num_substances_transported * ndmpq
        if (mod(integration_id, 16) >= 8) then
            ibflag = 1
        else
            ibflag = 0
        endif
        !
        do iq = 1, num_exchanges

            ! initialisations , check for transport anyhow
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) goto 60

            ! Check if exchange is dump exchange, set IPB
            if (ibflag == 1) then
                if (iqdmp(iq) > 0) then
                    ipb = iqdmp(iq)
                    ipq = (iqdmp(iq) - 1) * num_substances_transported
                else
                    ipb = 0
                endif
            else
                ipb = 0
            endif
            if (i > 0 .and. j > 0 .and. ipb == 0) goto 60
            a = area(iq)
            q = flow(iq)
            if (mod(integration_id, 2) == 1) then
                if (abs(q) < 10.0e-25)  goto 60
            endif
            e = disp(1)
            al = aleng(1)
            if (iq > num_exchanges_u_dir) then
                e = disp (2)
                al = aleng(2)
            endif
            if (iq > num_exchanges_u_dir + num_exchanges_v_dir) then
                e = disp (3)
                al = aleng(3)
            endif
            if (ilflag == 1) then
                dl = a / (aleng(2 * iq - 1) + aleng(2 * iq))
                f1 = aleng(2 * iq) * dl / a
                f2 = aleng(2 * iq - 1) * dl / a
            else
                dl = a / al
                f1 = 0.5
                f2 = 0.5
            endif
            e = e * dl
            if (i < 0) goto 20
            if (j < 0) goto 40

            ! the regular case
            k1 = (i - 1) * num_substances_total
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                d = e
                v = q
                if (idpnt(is) > 0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                if (ivpnt(is) > 0) then
                    dv = velo((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                    v = v + dv
                endif
                dq = (v * f1 + d) * conc(k1 + i3) + (v * f2 - d) * conc(k2 + i3)

                ! mass balance
                if (dq > 0.0) then
                    dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                else
                    dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                endif
                !
            end do
            goto 60

            ! The 'from' element was a boundary. Note the 2 options.
            20 if (j < 0) goto 60
            k1 = (-i - 1) * num_substances_total
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                v = q
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                d = 0.0
                if (mod(integration_id, 4) <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                endif
                if (mod(integration_id, 8) >=  4) then
                    if (v > 0.0) then
                        f1 = 1.0
                        f2 = 0.0
                    else
                        f1 = 0.0
                        f2 = 1.0
                    endif
                endif
                dq = (v * f1 + d) * bound(k1 + i3) + (v * f2 - d) * conc (k2 + i3)
                if (dq > 0.0) then
                    amass2(i3 + i4) = amass2(i3 + i4) + dq
                else
                    amass2(i3 + i5) = amass2(i3 + i5) - dq
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    endif
                endif
            end do
            goto 60

            ! The 'to' element was a boundary.
            40 if (i == 0) goto 60
            k1 = (i - 1) * num_substances_total
            k2 = (-j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                v = q
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                d = 0.0
                if (mod(integration_id, 4)  <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                endif
                if (mod(integration_id, 8) >=  4) then
                    if (v > 0.0) then
                        f1 = 1.0
                        f2 = 0.0
                    else
                        f1 = 0.0
                        f2 = 1.0
                    endif
                endif
                dq = (v * f1 + d) * conc (k1 + i3) + (v * f2 - d) * bound(k2 + i3)
                if (dq > 0.0) then
                    amass2(i3 + i5) = amass2(i3 + i5) + dq
                else
                    amass2(i3 + i4) = amass2(i3 + i4) - dq
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    endif
                endif
            end do

            ! end of the loop over exchanges
            60 continue
        end do
        !
        if (timon) call timstop (ithandl)
    end subroutine calculate_mass_balance_space_central_difference

    !! Makes a mass balance final to implicit integration methods.
    !! Identical to calculate_mass_balance_steady_state, but with dimension BOUND(num_substances_transported,*),
    !! with multiplication factor IDT on mass balances and with loop over active substances only (loops 10, 30
    !! and 50).
    subroutine calculate_mass_balance_implicit_schemes(disp, disper, area, flow, aleng, &
            velo, conc, bound, ipoint, num_substances_transported, &
            num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, num_dispersion_arrays, &
            num_velocity_arrays, idpnt, ivpnt, integration_id, amass2, &
            ilflag, dmpq, ndmpq, idt, iqdmp)

        !     DISP    REAL        3       INPUT   dispersion in 3 directions
        !     DISPER  REAL   num_dispersion_arrays*num_exchanges   INPUT   additional dispersion array
        !     AREA    REAL       num_exchanges      INPUT   exchange surface area
        !     FLOW    REAL       num_exchanges      INPUT   flows accross exchange surfs
        !     ALENG   REAL      2*num_exchanges     INPUT   from- and to lengthes
        !     VELO    REAL   num_velocity_arrays*num_exchanges   INPUT   additional velocity array
        !     CONC    REAL   num_substances_total*num_cells  INPUT   concentrations
        !     BOUND   REAL     num_substances_transported*?    INPUT   boundary concentrations
        !     IPOINT  INTEGER   4*num_exchanges     INPUT   exchange pointers
        !     num_substances_transported   INTEGER     1       INPUT   number  of active substances
        !     num_substances_total   INTEGER     1       INPUT   number  of total substances
        !     num_exchanges_u_dir    INTEGER     1       INPUT   nr of exchanges in first dir.
        !     num_exchanges_v_dir    INTEGER     1       INPUT   nr of exchanges in second dir.
        !     num_exchanges_z_dir    INTEGER     1       INPUT   nr of exchanges in third dir.
        !     num_exchanges     INTEGER     1       INPUT   total number of exchanges
        !     num_dispersion_arrays  INTEGER     1       INPUT   number  of additional dispers.
        !     num_velocity_arrays  INTEGER     1       INPUT   number  of additional velos.
        !     IDPNT   INTEGER   num_substances_transported     INPUT   pointer systems to dispersions
        !     IVPNT   INTEGER   num_substances_transported     INPUT   pointer systems to velocities
        !     integration_id    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
        !                                         = 1 or 3 no DISP at zero flow
        !                                         = 0 or 1 DISP over boundary
        !                                         = 2 or 3 no DISP over boundary
        !     AMASS2  REAL     num_substances_total*5    IN/OUT  mass balance array
        !     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
        !     DMPQ    REAL  num_substances_total*NDMPQ*? IN/OUT  mass balance dumped exchange
        !                                         if INTOPT > 7
        !     NDMPQ   INTEGER     1       INPUT   number of dumped exchanges
        !     IDT     INTEGER     1       INPUT   timestep (or 1 for steady state)
        !     IQDMP   INTEGER     *       INPUT   pointer dumped exchanges
        !

        integer(kind = int_wp) :: ndmpq
        integer(kind = int_wp) :: iqdmp   (*), ipoint(4, *), idpnt(*), ivpnt(*)
        real(kind = real_wp) :: disp  (3), disper(*), area (*), flow  (*), &
                aleng (*), velo  (*), conc (*), bound (*), &
                amass2(*), dmpq(*)
        integer(kind = int_wp) :: num_substances_transported, num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, num_dispersion_arrays, num_velocity_arrays
        integer(kind = int_wp) :: integration_id, ilflag, idt

        integer(kind = int_wp) :: i, i3, i4, i5, i6, iq, ipq, is
        integer(kind = int_wp) :: noq12, ibflag, j, ipb, k1, k2

        real(kind = real_wp) :: a, q, e, al, dl, d, v, dq

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("calculate_mass_balance_implicit_schemes", ithandl)

        ! loop accross the number of exchanges
        i4 = 3 * num_substances_total
        i5 = 4 * num_substances_total
        i6 = num_substances_transported * ndmpq
        noq12 = num_exchanges_u_dir + num_exchanges_v_dir
        if (mod(integration_id, 16) >= 8) then
            ibflag = 1
        else
            ibflag = 0
        endif

        do iq = 1, num_exchanges

            ! initialistations, check for transport anyhow
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) goto 60

            ! check if exchange is dump exchange, set ipb
            if (ibflag == 1) then
                if (iqdmp(iq) > 0) then
                    ipb = iqdmp(iq)
                    ipq = (iqdmp(iq) - 1) * num_substances_transported
                else
                    ipb = 0
                endif
            else
                ipb = 0
            endif
            if (i > 0 .and. j > 0 .and. ipb == 0) goto 60
            a = area(iq)
            q = flow(iq)
            if (mod(integration_id, 2) == 1 .and. iq <= noq12) then
                if (abs(q) < 10.0e-25)  goto 60
            endif
            e = disp(1)
            al = aleng(1)
            if (iq > num_exchanges_u_dir) then
                e = disp (2)
                al = aleng(2)
            endif
            if (iq > num_exchanges_u_dir + num_exchanges_v_dir) then
                e = disp (3)
                al = aleng(3)
            endif
            if (ilflag == 1) then
                dl = a / (aleng(2 * iq - 1) + aleng(2 * iq))
            else
                dl = a / al
            endif
            e = e * dl
            if (i < 0) goto 20
            if (j < 0) goto 40
            !
            !         the regular case
            !
            k1 = (i - 1) * num_substances_total
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_transported
                is = min (i3, num_substances_transported)
                !
                !        dispersion
                !
                if (idpnt(is) > 0) then
                    d = e + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                else
                    d = e
                endif
                !
                !        flow
                !
                if (ivpnt(is) > 0) then
                    v = q + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                else
                    v = q
                endif
                !
                !        transport
                !
                if (v > 0.0) then
                    dq = ((v + d) * conc(k1 + i3) - d * conc(k2 + i3)) * idt
                else
                    dq = ((v - d) * conc(k2 + i3) + d * conc(k1 + i3)) * idt
                endif
                !
                !        mass balance
                !
                if (dq > 0.0) then
                    dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                else
                    dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                endif
                !
            end do
            goto 60
            !
            !        the 'from' element was a boundary. note the 2 options.
            !
            20 if (j < 0) goto 60
            k1 = (-i - 1) * num_substances_transported
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_transported
                is = min (i3, num_substances_transported)
                v = q
                d = 0.0
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                if (mod(integration_id, 4) <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                endif
                if (v > 0.0) then
                    dq = ((v + d) * bound(k1 + i3) - d * conc (k2 + i3)) * idt
                else
                    dq = ((v - d) * conc (k2 + i3) + d * bound(k1 + i3)) * idt
                endif
                if (dq > 0.0) then
                    amass2(i3 + i4) = amass2(i3 + i4) + dq
                else
                    amass2(i3 + i5) = amass2(i3 + i5) - dq
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    endif
                endif
            end do
            goto 60
            !
            !        the 'to' element was a boundary.
            !
            40 k1 = (i - 1) * num_substances_total
            k2 = (-j - 1) * num_substances_transported
            do i3 = 1, num_substances_transported
                is = min (i3, num_substances_transported)
                v = q
                d = 0.0
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                if (mod(integration_id, 4)  <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                endif
                if (v > 0.0) then
                    dq = ((v + d) * conc (k1 + i3) - d * bound(k2 + i3)) * idt
                else
                    dq = ((v - d) * bound(k2 + i3) + d * conc (k1 + i3)) * idt
                endif
                if (dq > 0.0) then
                    amass2(i3 + i5) = amass2(i3 + i5) + dq
                else
                    amass2(i3 + i4) = amass2(i3 + i4) - dq
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    endif
                endif
            end do

            60 continue
        end do

        if (timon) call timstop (ithandl)

    end subroutine calculate_mass_balance_implicit_schemes

    !> Calculates mass balance of transport and copies solution in the concentration array
    !! Iterative solver solves 1 substance at a time (per thread in parallel mode).
    !! This routine saves the double precision solution in the single precision cons array.
    !! The tricky part is that for dry cells 0.0 is taken. In previous versions the old
    !! value remained untouched or if influenced by the iteration it got a strange value.
    !! Furthermore with the concentration values the mass balance aarays are updated.
    !! The update makes use of the pre-computed flowtot and disptot arrays for this substance.
    !! They contain the flow and disp plus additional velocity and dispersion terms.
    !! This avoids to have that logics again in this routine.
    subroutine calculate_mass_balance_copy_sol_to_concentration(substance_i, num_substances_transported, &
            num_substances_total, num_cells, conc, &
            concvt, num_boundary_conditions, bound, num_exchanges, ipoint, &
            flowtot, disptot, amass2, ndmpq, iqdmp, &
            dmpq, iknmrk, idt)

        integer(kind = int_wp), intent(in) :: substance_i                  !< current transported substance
        integer(kind = int_wp), intent(in) :: num_substances_transported                 !< number of active substances
        integer(kind = int_wp), intent(in) :: num_substances_total                 !< total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                 !< number of cells or computational volumes
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells)    !< concentration vector to store results
        real(kind = dp), intent(in) :: concvt(num_cells)         !< newly obtained concentration from the solver
        integer(kind = int_wp), intent(in) :: num_boundary_conditions         !< number of volumes with open boundaries
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, num_boundary_conditions)   !< boundary concentrations
        integer(kind = int_wp), intent(in) :: num_exchanges                   !< number of exchanges between volumes
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)        !< exchange pointers
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)          !< flows plus additional velos.
        real(kind = real_wp), intent(in) :: disptot(num_exchanges)          !< dispersion plus additional dipers.
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)      !< mass balance array for the whole model area
        integer(kind = int_wp), intent(in) :: ndmpq                 !< number of dumped exchanges
        integer(kind = int_wp), intent(in) :: iqdmp(num_exchanges)            !< pointers dumped exchages
        real(kind = real_wp), intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< flux accumulation array for monitoring areas
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)         !< feature array, bit zero indicates wet or not
        integer(kind = int_wp), intent(in) :: idt                   !< time step

        ! Local variables
        real(kind = real_wp) :: cin, cjn     !< from- and to concentrations
        real(kind = real_wp) :: fluxij       !< flux from i to j
        integer(kind = int_wp) :: ifrom, ito !< from- and to volume indices
        integer(kind = int_wp) :: cell_i       !< current computational volume
        integer(kind = int_wp) :: iq         !< current edge

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("calculate_mass_balance_copy_sol_to_concentration", ithandl)
        ! put result in the concentration array
        do cell_i = 1, num_cells
            if (btest(iknmrk(cell_i), 0)) then
                conc(substance_i, cell_i) = concvt(cell_i)
            else
                conc(substance_i, cell_i) = 0.0
            end if
        end do

        ! flow and diffusion
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ! only compute where needed
            if (ifrom > 0 .and. ito > 0 .and. iqdmp(iq) == 0) cycle
            if (ifrom == 0 .or. ito == 0) cycle

            if (ifrom > 0) then
                cin = concvt(ifrom)
            else
                cin = bound(substance_i, -ifrom)
            end if

            if (ito > 0) then
                cjn = concvt(ito)
            else
                cjn = bound(substance_i, -ito)
            end if

            if (flowtot(iq) > 0) then ! flow from i to j
                fluxij = real(idt) * ((flowtot(iq) + disptot(iq)) * cin - disptot(iq) * cjn)
            else ! flow from j to i
                fluxij = real(idt) * ((flowtot(iq) - disptot(iq)) * cjn + disptot(iq) * cin)
            end if

            ! mass balance of the whole area
            if (ifrom < 0) then
                if (fluxij > 0) then
                    amass2(substance_i, 4) = amass2(substance_i, 4) + fluxij
                else                                             ! amass2(*,1) masses of the substances in the model
                    amass2(substance_i, 5) = amass2(substance_i, 5) - fluxij      ! amass2(*,2) change by processes
                end if                                            ! amass2(*,3) change by discharges
            end if                                               ! amass2(*,4) incoming boundary transport
            if (ito < 0) then                            ! amass2(*,5) outgoing boundary transport
                if (fluxij > 0) then
                    amass2(substance_i, 5) = amass2(substance_i, 5) + fluxij
                else
                    amass2(substance_i, 4) = amass2(substance_i, 4) - fluxij
                end if
            end if

            ! mass balance of selected monitoring areas
            if (iqdmp(iq) > 0) then ! dmpq(*,*,1) incoming transport in a monitoring area
                if (fluxij > 0) then ! dmpq(*,*,2) outgoing transport from a monitoring area
                    dmpq(substance_i, iqdmp(iq), 1) = dmpq(substance_i, iqdmp(iq), 1) + fluxij
                else
                    dmpq(substance_i, iqdmp(iq), 2) = dmpq(substance_i, iqdmp(iq), 2) - fluxij
                end if
            end if
        end do
        if (timon) call timstop(ithandl)
    end subroutine calculate_mass_balance_copy_sol_to_concentration

    !> Adjust mass balance for adjusting theta algorithm
    subroutine calculate_mass_balance_for_theta_algorithm(substance_i, num_substances_transported, &
            num_substances_total, num_cells, conc, concvt, num_boundary_conditions, bound, num_exchanges, ipoint, &
            theta, flowtot, disptot, amass2, ndmpq, &
            iqdmp, dmpq, idt)

        integer(kind = int_wp), intent(in) :: substance_i                  !< Current active substance
        integer(kind = int_wp), intent(in) :: num_substances_transported                 !< Number of active substances
        integer(kind = int_wp), intent(in) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                 !< Number of segments
        real(kind = real_wp), intent(in) :: conc(num_substances_total, num_cells)    !< Old concentrations
        real(kind = dp), intent(in) :: concvt(num_cells)         !< First solution estimation by means of local theta method
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                 !< Number of boundary segments
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, num_boundary_conditions)   !< Boundary concentrations
        integer(kind = int_wp), intent(in) :: num_exchanges                   !< Number of exchanges
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)        !< Exchange pointers
        real(kind = real_wp), intent(in) :: theta(num_exchanges)            !< Local theta coefficients
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)          !< Flows plus additional velos.
        real(kind = real_wp), intent(in) :: disptot(num_exchanges)          !< Dispersion plus additional dipers.
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)      !< amass2(*,1) masses
        !< amass2(*,2) processes
        !< amass2(*,3) discharges
        !< amass2(*,4) incoming boundary transport
        !< amass2(*,5) outgoing boundary transport
        integer(kind = int_wp), intent(in) :: ndmpq                 !< Number of dumped exchanges
        integer(kind = int_wp), intent(in) :: iqdmp(num_exchanges)            !< Indeces dumped exchages
        real(kind = real_wp), intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< dmpq(*,*,1) incoming transport
        !< dmpq(*,*,2) outgoing transport
        integer(kind = int_wp), intent(in) :: idt                   !< Time step

        ! Local variables
        real(kind = real_wp) :: cio    !< Old from concentration
        real(kind = real_wp) :: cjo    !< Old to concentration
        real(kind = real_wp) :: cin    !< New from concentration
        real(kind = real_wp) :: cjn    !< New to concentration
        real(kind = real_wp) :: fluxij !< Flux from i to j
        integer(kind = int_wp) :: ifrom  !< Index from cell
        integer(kind = int_wp) :: ito    !< Index to cell
        integer(kind = int_wp) :: iq     !< Current edge
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("calculate_mass_balance_for_theta_algorithm", ithandl)

        ! flow and diffusion
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            ! only compute where needed
            if (ifrom > 0 .and. ito > 0 .and. iqdmp(iq) == 0) cycle
            if (ifrom == 0 .or. ito == 0) cycle

            if (ifrom > 0) then
                cio = conc(substance_i, ifrom)
                cin = concvt(ifrom)
            else
                cio = bound(substance_i, -ifrom)
                cin = bound(substance_i, -ifrom)
            end if

            if (ito > 0) then
                cjo = conc(substance_i, ito)
                cjn = concvt(ito)
            else
                cjo = bound(substance_i, -ito)
                cjn = bound(substance_i, -ito)
            end if

            if (flowtot(iq) > 0) then ! flow from i to j
                fluxij = theta(iq) * (flowtot(iq) * cin - disptot(iq) * (cjn - cin)) &
                        + (1 - theta(iq)) * (flowtot(iq) * cio - disptot(iq) * (cjo - cio))
            else                      ! flow from j to i
                fluxij = theta(iq) * (flowtot(iq) * cjn - disptot(iq) * (cjn - cin)) &
                        + (1 - theta(iq)) * (flowtot(iq) * cjo - disptot(iq) * (cjo - cio))
            end if
            if (ifrom < 0) then
                if (fluxij > 0) then
                    amass2(substance_i, 4) = amass2(substance_i, 4) + real(idt) * fluxij
                else
                    amass2(substance_i, 5) = amass2(substance_i, 5) - real(idt) * fluxij
                end if
            end if
            if (ito < 0) then
                if (fluxij > 0) then
                    amass2(substance_i, 5) = amass2(substance_i, 5) + real(idt) * fluxij
                else
                    amass2(substance_i, 4) = amass2(substance_i, 4) - real(idt) * fluxij
                end if
            end if
            if (iqdmp(iq) > 0) then
                if (fluxij > 0) then
                    dmpq(substance_i, iqdmp(iq), 1) = dmpq(substance_i, iqdmp(iq), 1) + real(idt) * fluxij
                else
                    dmpq(substance_i, iqdmp(iq), 2) = dmpq(substance_i, iqdmp(iq), 2) - real(idt) * fluxij
                end if
            end if
        end do
        if (timon) call timstop(ithandl)
    end subroutine calculate_mass_balance_for_theta_algorithm

end module m_mass_balance_calculation
