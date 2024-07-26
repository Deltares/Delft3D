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
module m_dlwqd1
    use m_waq_precision

    implicit none

contains


    !> Implicit solution of the vertical by double sweep,
    !! central discretisation of the advection terms in the water,
    !! upwind discretisation of the advection terms in the bed.
    !! Because of the bed layers, a tridiagonal matrix is filled first
    !! Then, for the water phase, all forward substitutions of the lower
    !! codiagonal are performed.
    !! This method required strict ordering from the top of the water
    !! column towards the bed. The adminitration is generally per layer
    !! downwards for all cells, but also per column downwards for all
    !! columns will work. The NOQW water exchanges should contain all
    !! single exchanges. This should be inclusive of the exchanges from
    !! the water with the first bed cell.
    !! The exchanges in the bed are double. So both are taken together.
    !! The bed columns are probably per column, because each column can
    !! have different amount of layers, but per layer could do as well
    !! as long as layers count from the top of the bed to below.
    !! For the sweep back the reverse order is followed.
    !! Note the option to have settling substances modelled 'upwind'
    !! whereas the water velocity is taken centrally.
    subroutine dlwqd1(num_substances_transported, num_substances_total, num_cells, noqw, num_exchanges, &
            num_dispersion_arrays, num_velocity_arrays, disp, disper, velo, &
            area, flow, aleng, ipoint, iknmrk, &
            idpnt, ivpnt, conc, bound, integration_id, &
            ilflag, idt, deriv, iaflag, amass2, &
            file_unit_list, ndmpq, iqdmp, &
            dmpq, rhs, diag, acodia, bcodia)


        use m_cli_utils, only : is_command_arg_specified
        use timers

        integer(kind = int_wp), intent(in   ) :: num_substances_transported                 !< Number of transported substances
        integer(kind = int_wp), intent(in   ) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                 !< Number of computational volumes
        integer(kind = int_wp), intent(in   ) :: noqw                  !< Number of interfaces waterphase
        integer(kind = int_wp), intent(in   ) :: num_exchanges                   !< Total number of interfaces
        integer(kind = int_wp), intent(in   ) :: num_dispersion_arrays                !< Number additional dispersions
        integer(kind = int_wp), intent(in   ) :: num_velocity_arrays                !< Number additional velocities
        real(kind = real_wp),   intent(in   ) :: disp(3)               !< Fixed dispersions in the 3 directions
        real(kind = real_wp),   intent(in   ) :: disper(num_dispersion_arrays, num_exchanges)   !< Array with additional dispersions
        real(kind = real_wp),   intent(in   ) :: velo(num_velocity_arrays, num_exchanges)     !< Array with additional velocities
        real(kind = real_wp),   intent(in   ) :: area(num_exchanges)             !< Exchange areas in m2
        real(kind = real_wp),   intent(in   ) :: flow(num_exchanges)             !< Flows through the exchange areas in m3/s
        real(kind = real_wp),   intent(in   ) :: aleng(2, num_exchanges)         !< Mixing length to and from the exchange area
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)        !< From, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in   ) :: iknmrk(num_cells)         !< Feature array
        integer(kind = int_wp), intent(in   ) :: idpnt(num_substances_transported)          !< Additional dispersion number per substance
        integer(kind = int_wp), intent(in   ) :: ivpnt(num_substances_transported)          !< Additional velocity number per substance
        real(kind = real_wp),   intent(inout) :: conc(num_substances_total, num_cells)    !< Masses after horizontal transport step
        real(kind = real_wp),   intent(in   ) :: bound(num_substances_transported, *)       !< Open boundary concentrations
        integer(kind = int_wp), intent(in   ) :: integration_id        !< bit 0: 1 if no dispersion at zero flow
                                                                       !< bit 1: 1 if no dispersion across boundaries
                                                                       !< bit 2: 1 if lower order across boundaries
                                                                       !< bit 3: 1 if mass balance output
        integer(kind = int_wp), intent(in   ) :: ilflag                !< If 0 then only 3 constant lenght values
        integer(kind = int_wp), intent(in   ) :: idt                   !< Time step in seconds
        real(kind = real_wp),   intent(inout) :: deriv(num_substances_total, num_cells)   !< Workspace containing the diagonal
        integer(kind = int_wp), intent(in   ) :: iaflag                !< If 1 then accumulate mass in report array
        real(kind = real_wp),   intent(inout) :: amass2(num_substances_total, 5)      !< Report array for monitoring file
        integer(kind = int_wp), intent(in   ) :: file_unit_list        !< Unit number of monitoring file
        integer(kind = int_wp), intent(in   ) :: ndmpq                 !< Number of dumped exchanges
        integer(kind = int_wp), intent(in   ) :: iqdmp(num_exchanges)            !< Pointers dumped exchages
        real(kind = real_wp),   intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< dmpq(*,*,1) incoming transport
                                                                       !< dmpq(*,*,2) outgoing transport
        real(kind = dp),        intent(inout) :: rhs(num_substances_total, num_cells)     !< Local right hand side
        real(kind = dp),        intent(inout) :: diag(num_substances_total, num_cells)    !< Local diagonal filled with volumes
        real(kind = dp),        intent(inout) :: acodia(num_substances_total, num_exchanges)    !< Local work array lower codiagonal
        real(kind = dp),        intent(inout) :: bcodia(num_substances_total, num_exchanges)    !< Local work array upper codiagonal

        ! Local variables
        integer(kind = int_wp) :: iq         !< loop counter exchanges
        integer(kind = int_wp) :: isys       !< loop counter substance
        integer(kind = int_wp) :: iseg       !< loop counter computational volumes
        integer(kind = int_wp) :: ifrom, ito !< from and to volume numbers
        integer(kind = int_wp) :: iq2, iq3   !< auxiliary variables to identify first or second pointers
        integer(kind = int_wp) :: iqd        !< auxiliary variable for dump pointers
        real(kind = dp) :: a                 !< this exchange area
        real(kind = dp) :: q                 !< flow for this exchange
        real(kind = dp) :: e                 !< dispersion for this exchange
        real(kind = dp) :: al                !< this distance between cell mids
        real(kind = dp) :: dl                !< area / length
        real(kind = dp) :: d                 !< dispersion for this substance
        real(kind = dp) :: v                 !< flow for this substance
        real(kind = dp) :: f1, f2            !< linear interpolation variables
        real(kind = dp) :: v1, v2            !< factors for 'upwind' or 'central'
        real(kind = dp) :: q3, q4            !< helpvariables to fill the matrix
        real(kind = dp) :: dq                !< auxiliary variable balances
        real(kind = dp) :: pivot             !< auxiliary variable matrix inversion
        logical :: disp0q0                   !< bit zero no disp if q is zero
        logical :: disp0bnd                  !< bit one  no disp accross bounds
        logical :: loword                    !< bit two  lower order accross bounds
        logical :: abound                    !< is it a boundary?

        logical   , save :: sw_settling   ! if true, settling should be dealt with upwind
        integer(kind=int_wp), save  ::init = 1      ! first call ?

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqd1", ithandl)

        ! get special option from command line
        if ( init == 1 ) then
            init = 0
            sw_settling = is_command_arg_specified('-settling_backwards')
            if ( sw_settling ) write( file_unit_list, * ) ' option -settling_backwards found'
        end if

        ! Initialisation
        rhs = conc
        diag = deriv
        acodia = 0.0
        bcodia = 0.0
        disp0q0 = btest(integration_id, 0)
        disp0bnd = btest(integration_id, 1)
        loword = btest(integration_id, 2)

        ! Loop over exchanges to fill the matrices
        do iq = 1, num_exchanges
            ! Initialisations, check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom < 0 .and. ito < 0) cycle
            abound = .false.
            if (ifrom < 0 .or. ito < 0) abound = .true.

            a = area(iq)
            q = flow(iq)
            e = disp (1)
            al = aleng(1, 1)
            if (ilflag == 1) then
                al = aleng(1, iq) + aleng(2, iq)
            end if
            f1 = 0.5
            f2 = 0.5
            if (al > 1.0E-25) then
                if (ilflag == 1) then
                    f1 = aleng(2, iq) / al
                    f2 = 1.0 - f1
                end if
                dl = a / al
            else
                dl = 0.0
            end if
            e = e * dl
            if (iq > noqw) e = 0.0      !  no constant water diffusion in the bottom

            do isys = 1, num_substances_transported

                ! advection
                v = 0.0
                if (ivpnt(isys) > 0) v = velo  (ivpnt(isys), iq) * a
                if (iq > noqw .or. & ! in the bed upwind
                        (abound .and. loword)) then
                    if (q + v > 0.0) then
                        v1 = q + v
                        v2 = 0.0
                    else
                        v1 = 0.0
                        v2 = q + v
                    end if
                else
                    if (sw_settling) then         !  additional velocity upwind
                        if (v > 0.0) then
                            v1 = q * f1 + v
                            v2 = q * f2
                        else
                            v1 = q * f1
                            v2 = q * f2 + v
                        end if
                    else
                        v1 = (q + v) * f1
                        v2 = (q + v) * f2
                    end if
                end if

                ! diffusion
                d = e
                if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                if (disp0q0 .and. abs(q + v) < 10.0E-25) d = 0.0
                if (abound  .and. disp0bnd) d = 0.0

                ! fill the tridiag matrix
                q3 = (v1 + d) * idt
                q4 = (v2 - d) * idt
                if (.not. abound) then   ! the regular case
                    diag  (isys, ifrom) = diag  (isys, ifrom) + q3
                    bcodia(isys, iq) = bcodia(isys, iq) + q4
                    diag  (isys, ito) = diag  (isys, ito) - q4
                    acodia(isys, iq) = acodia(isys, iq) - q3
                else
                    if (ito   > 0) then
                        q3 = q3 * bound(isys, -ifrom)
                        diag  (isys, ito) = diag  (isys, ito) - q4
                        rhs   (isys, ito) = rhs   (isys, ito) + q3
                    end if
                    if (ifrom > 0) then
                        q4 = q4 * bound(isys, -ito)
                        diag  (isys, ifrom) = diag  (isys, ifrom) + q3
                        rhs   (isys, ifrom) = rhs   (isys, ifrom) - q4
                    end if
                end if
            end do
            ! End of loop over exchanges
        end do
        !    Now make the solution:  loop over exchanges in the water
        do iq = 1, noqw
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            do isys = 1, num_substances_transported
                pivot = acodia(isys, iq) / diag(isys, ifrom)
                diag(isys, ito) = diag(isys, ito) - pivot * bcodia(isys, iq)
                rhs (isys, ito) = rhs (isys, ito) - pivot * rhs   (isys, ifrom)
            end do
        end do

        ! loop over exchanges in the bed
        do iq = noqw + 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq + 1, num_exchanges                 !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                end if
            end do                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do isys = 1, num_substances_transported
                pivot = acodia(isys, iq) + acodia(isys, iq3)
                pivot = pivot / diag(isys, ifrom)
                rhs (isys, ito) = rhs (isys, ito) - pivot * rhs(isys, ifrom)
                diag(isys, ito) = diag(isys, ito) - pivot * &
                        (bcodia(isys, iq) + bcodia(isys, iq3))
            end do
        end do
        ! inverse loop over exchanges in the bed
        do iq = num_exchanges, noqw + 1, -1
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq - 1, noqw + 1, -1          !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                end if
            end do                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do isys = 1, num_substances_transported
                pivot = diag(isys, ito)
                rhs (isys, ito) = rhs(isys, ito) / pivot
                diag(isys, ito) = 1.0
            end do
            if (ifrom <= 0) cycle
            do isys = 1, num_substances_transported
                pivot = bcodia(isys, iq) + bcodia(isys, iq3)
                rhs (isys, ifrom) = rhs (isys, ifrom) - pivot * rhs(isys, ito)
            end do
        end do

        ! Inverse loop over exchanges in the water phase
        do iq = noqw, 1, -1
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito   <= 0) cycle
            do isys = 1, num_substances_transported
                pivot = diag(isys, ito)
                rhs (isys, ito) = rhs(isys, ito) / pivot
                diag(isys, ito) = 1.0
            end do
            if (ifrom <= 0) cycle
            do isys = 1, num_substances_transported
                pivot = bcodia(isys, iq)
                rhs (isys, ifrom) = rhs (isys, ifrom) - pivot * rhs(isys, ito)
            end do
        end do

        do iseg = 1, num_cells       !  for if some diagonal entries are not 1.0
            do isys = 1, num_substances_transported
                rhs (isys, iseg) = rhs(isys, iseg) / diag(isys, iseg)
                diag(isys, iseg) = 1.0
            end do
        end do
        conc = rhs
        deriv = diag

        ! Mass balances ?
        if (iaflag == 0) goto 9999

        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom < 0 .and. ito < 0) cycle            ! trivial
            abound = .false.
            iqd = iqdmp(iq)
            if (ifrom >= 0 .and. ito >= 0) then             ! internal
                if (iqd <= 0) cycle                            ! no dump required
            else
                abound = .true.                                    ! is boundary
            end if
            a = area(iq)
            q = flow(iq)
            e = disp (1)
            al = aleng(1, 1)
            if (ilflag == 1) al = aleng(1, iq) + aleng(2, iq)
            f1 = 0.5
            f2 = 0.5
            if (al > 1.0E-25) then
                if (ilflag == 1) then
                    f1 = aleng(2, iq) / al
                    f2 = 1.0 - f1
                end if
                dl = a / al
            else
                dl = 0.0
            end if
            e = e * dl
            if (iq > noqw) e = 0.0      !  no constant water diffusion in the bottom

            do isys = 1, num_substances_transported

                v = 0.0
                if (ivpnt(isys) > 0) v = velo  (ivpnt(isys), iq) * a
                if (iq > noqw         .or.          & ! in the bed upwind
                        (abound .and. loword)) then
                    if (q + v > 0.0) then
                        v1 = q + v
                        v2 = 0.0
                    else
                        v1 = 0.0
                        v2 = q + v
                    end if
                else
                    if (sw_settling) then         !  additional velocity upwind
                        if (v > 0.0) then
                            v1 = q * f1 + v
                            v2 = q * f2
                        else
                            v1 = q * f1
                            v2 = q * f2 + v
                        end if
                    else
                        v1 = (q + v) * f1
                        v2 = (q + v) * f2
                    end if
                end if

                d = e
                if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                if (disp0q0 .and. abs(q + v) < 10.0E-25) d = 0.0
                if (abound  .and. disp0bnd) d = 0.0

                q3 = (v1 + d) * idt
                q4 = (v2 - d) * idt
                if (abound) then
                    if (ito   > 0)  then
                        dq = q3 * bound(isys, -ifrom) + q4 * rhs  (isys, ito)
                        if (dq > 0.0) then
                            amass2(isys, 4) = amass2(isys, 4) + dq
                        else
                            amass2(isys, 5) = amass2(isys, 5) - dq
                        end if
                    else
                        dq = q3 * rhs  (isys, ifrom) + q4 * bound(isys, -ito)
                        if (dq > 0.0) then
                            amass2(isys, 5) = amass2(isys, 5) + dq
                        else
                            amass2(isys, 4) = amass2(isys, 4) - dq
                        end if
                    end if
                else
                    dq = q3 * rhs  (isys, ifrom) + q4 * rhs  (isys, ito)
                end if
                if (iqd > 0) then
                    if (dq > 0) then
                        dmpq(isys, iqd, 1) = dmpq(isys, iqd, 1) + dq
                    else
                        dmpq(isys, iqd, 2) = dmpq(isys, iqd, 2) - dq
                    end if
                end if
            end do

        end do

        9999 if (timon) call timstop (ithandl)
    end subroutine dlwqd1
end module m_dlwqd1
