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


    subroutine dlwqd1 (nosys, notot, noseg, noqw, noq, &
            nodisp, novelo, disp, disper, velo, &
            area, flow, aleng, ipoint, iknmrk, &
            idpnt, ivpnt, conc, bound, integration_id, &
            ilflag, idt, deriv, iaflag, amass2, &
            file_unit_list, ndmpq, iqdmp, &
            dmpq, rhs, diag, acodia, bcodia)

        !> Implicit solution of the vertical by double sweep,
        !>                central discretisation of the advection terms in the water,
        !>                upwind discretisation of the advection terms in the bed.
        !>
        !>                Because of the bed layers, a tridiagonal matrix is filled first
        !>                Then for the water phase all forward substitutions of the lower
        !>                codiagonal are performed.\n
        !>                This method required strict ordering from the top of the water
        !>                column towards the bed. The adminitration is generally per layer
        !>                downwards for all cells, but also per column downwards for all
        !>                columns will work. The NOQW water exchanges should contain all
        !>                single exchanges. This should be inclusive of the exchanges from
        !>                the water with the first bed cell.\n
        !>                The exchanges in the bed are double. So both are taken together.
        !>                The bed columns are probably per column, because each column can
        !>                have different amount of layers, but per layer could do as well
        !>                as long as layers count from the top of the bed to below.\n
        !>                For the sweep back the reverse order is followed.\n
        !>                Note the option to have settling substances modelled 'upwind'
        !>                whereas the water velocity is taken centrally.

        use m_cli_utils, only : retrieve_command_argument
        use timers

        integer(kind = int_wp), intent(in) :: nosys                !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                !< total number of substances
        integer(kind = int_wp), intent(in) :: noseg                !< number of computational volumes
        integer(kind = int_wp), intent(in) :: noqw                 !< number of interfaces waterphase
        integer(kind = int_wp), intent(in) :: noq                  !< total number of interfaces
        integer(kind = int_wp), intent(in) :: nodisp               !< number additional dispersions
        integer(kind = int_wp), intent(in) :: novelo               !< number additional velocities
        real(kind = real_wp), intent(in) :: disp  (3)            !< fixed dispersions in the 3 directions
        real(kind = real_wp), intent(in) :: disper(nodisp, noq)   !< array with additional dispersions
        real(kind = real_wp), intent(in) :: velo  (novelo, noq)   !< array with additional velocities
        real(kind = real_wp), intent(in) :: area  (noq)          !< exchange areas in m2
        real(kind = real_wp), intent(in) :: flow  (noq)          !< flows through the exchange areas in m3/s
        real(kind = real_wp), intent(in) :: aleng (2, noq)   !< mixing length to and from the exchange area
        integer(kind = int_wp), intent(in) :: ipoint(4, noq)   !< from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: iknmrk(noseg)        !< feature array
        integer(kind = int_wp), intent(in) :: idpnt (nosys)        !< additional dispersion number per substance
        integer(kind = int_wp), intent(in) :: ivpnt (nosys)        !< additional velocity number per substance
        real(kind = real_wp), intent(inout) :: conc  (notot, noseg)  !< masses after horizontal transport step
        real(kind = real_wp), intent(in) :: bound (nosys, *)  !< open boundary concentrations
        integer(kind = int_wp), intent(in) :: integration_id                 !< bit 0: 1 if no dispersion at zero flow
        !< bit 1: 1 if no dispersion across boundaries
        !< bit 2: 1 if lower order across boundaries
        !< bit 3: 1 if mass balance output
        integer(kind = int_wp), intent(in) :: ilflag               !< if 0 then only 3 constant lenght values
        integer(kind = int_wp), intent(in) :: idt                  !< time step in seconds
        real(kind = real_wp), intent(inout) :: deriv (notot, noseg)  !< workspace containing the diagonal
        integer(kind = int_wp), intent(in) :: iaflag               !< if 1 then accumulate mass in report array
        real(kind = real_wp), intent(inout) :: amass2(notot, 5)  !< report array for monitoring file
        integer(kind = int_wp), intent(in) :: file_unit_list                  !< unit number of monitoring file
        integer(kind = int_wp), intent(in) :: ndmpq                !< number of dumped exchanges
        integer(kind = int_wp), intent(in) :: iqdmp (noq)        !< pointers dumped exchages
        real(kind = real_wp), intent(inout) :: dmpq  (nosys, ndmpq, 2)!< dmpq(*,*,1) incoming transport
        !< dmpq(*,*,2) outgoing transport
        real(kind = dp), intent(inout) :: rhs   (notot, noseg)  !< local right hand side
        real(kind = dp), intent(inout) :: diag  (notot, noseg)  !< local diagonal filled with volumes
        real(kind = dp), intent(inout) :: acodia(notot, noq)    !< local workarray under codiagonal
        real(kind = dp), intent(inout) :: bcodia(notot, noq)    !< local workarray upper codiagonal

        !         local variables

        integer(kind = int_wp) :: iq                    ! loop counter exchanges
        integer(kind = int_wp) :: isys                  ! loop counter substance
        integer(kind = int_wp) :: iseg                  ! loop counter computational volumes
        integer(kind = int_wp) :: ifrom, ito            ! from and to volume numbers
        integer(kind = int_wp) :: iq2, iq3              ! help variables to identify first or second pointers
        integer(kind = int_wp) :: iqd                   ! help variable for dump pointers
        real(kind = dp) :: a                     ! this exchange area
        real(kind = dp) :: q                     ! flow for this exchange
        real(kind = dp) :: e                     ! dispersion for this exchange
        real(kind = dp) :: al                    ! this distance between cell mids
        real(kind = dp) :: dl                    ! area / length
        real(kind = dp) :: d                     ! dispersion for this substance
        real(kind = dp) :: v                     ! flow for this substance
        real(kind = dp) :: f1, f2                ! linear interpolation variables
        real(kind = dp) :: v1, v2                ! factors for 'upwind' or 'central'
        real(kind = dp) :: q3, q4                ! helpvariables to fill the matrix
        real(kind = dp) :: dq                    ! help variable balances
        real(kind = dp) :: pivot                 ! help variable matrix inversion
        logical       disp0q0               ! bit zero no disp if q is zero
        logical       disp0bnd              ! bit one  no disp accross bounds
        logical       loword                ! bit two  lower order accross bounds
        logical       abound                ! is it a boundary?

        logical, save :: sw_settling   ! if true, settling should be dealt with upwind
        integer(kind = int_wp), save :: init = 1      ! first call ?
        character :: cdummy        !
        integer(kind = int_wp) :: idummy        !
        real(kind = real_wp) :: rdummy        !
        integer(kind = int_wp) :: ierr2         !

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqd1", ithandl)

        !         get special option from command line

        if (init == 1) then
            init = 0
            call retrieve_command_argument('-settling_backwards', 0, sw_settling, idummy, rdummy, cdummy, ierr2)
            if (sw_settling) write(file_unit_list, *) ' option -settling_backwards found'
        endif

        !         Initialisation

        rhs = conc
        diag = deriv
        acodia = 0.0
        bcodia = 0.0
        disp0q0 = btest(integration_id, 0)
        disp0bnd = btest(integration_id, 1)
        loword = btest(integration_id, 2)

        !         Loop over exchanges to fill the matrices

        do iq = 1, noq

            !         Initialisations, check for transport anyhow

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
            endif
            f1 = 0.5
            f2 = 0.5
            if (al > 1.0E-25) then
                if (ilflag == 1) then
                    f1 = aleng(2, iq) / al
                    f2 = 1.0 - f1
                endif
                dl = a / al
            else
                dl = 0.0
            endif
            e = e * dl
            if (iq > noqw) e = 0.0      !  no constant water diffusion in the bottom

            do isys = 1, nosys

                !           advection

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
                    endif
                else
                    if (sw_settling) then         !  additional velocity upwind
                        if (v > 0.0) then
                            v1 = q * f1 + v
                            v2 = q * f2
                        else
                            v1 = q * f1
                            v2 = q * f2 + v
                        endif
                    else
                        v1 = (q + v) * f1
                        v2 = (q + v) * f2
                    endif
                endif

                !           diffusion

                d = e
                if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                if (disp0q0 .and. abs(q + v) < 10.0E-25) d = 0.0
                if (abound  .and. disp0bnd) d = 0.0

                !           fill the tridiag matrix

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
                    endif
                    if (ifrom > 0) then
                        q4 = q4 * bound(isys, -ito)
                        diag  (isys, ifrom) = diag  (isys, ifrom) + q3
                        rhs   (isys, ifrom) = rhs   (isys, ifrom) - q4
                    endif
                endif
            enddo

            !        End of loop over exchanges

        end do

        !    Now make the solution:  loop over exchanges in the water

        do iq = 1, noqw
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            do isys = 1, nosys
                pivot = acodia(isys, iq) / diag(isys, ifrom)
                diag(isys, ito) = diag(isys, ito) - pivot * bcodia(isys, iq)
                rhs (isys, ito) = rhs (isys, ito) - pivot * rhs   (isys, ifrom)
            enddo
        enddo

        !    loop over exchanges in the bed

        do iq = noqw + 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq + 1, noq                 !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                endif
            enddo                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do isys = 1, nosys
                pivot = acodia(isys, iq) + acodia(isys, iq3)
                pivot = pivot / diag(isys, ifrom)
                rhs (isys, ito) = rhs (isys, ito) - pivot * rhs(isys, ifrom)
                diag(isys, ito) = diag(isys, ito) - pivot * &
                        (bcodia(isys, iq) + bcodia(isys, iq3))
            enddo
        enddo

        !    inverse loop over exchanges in the bed

        do iq = noq, noqw + 1, -1
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq - 1, noqw + 1, -1          !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                endif
            enddo                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do isys = 1, nosys
                pivot = diag(isys, ito)
                rhs (isys, ito) = rhs(isys, ito) / pivot
                diag(isys, ito) = 1.0
            enddo
            if (ifrom <= 0) cycle
            do isys = 1, nosys
                pivot = bcodia(isys, iq) + bcodia(isys, iq3)
                rhs (isys, ifrom) = rhs (isys, ifrom) - pivot * rhs(isys, ito)
            enddo
        enddo

        !     Inverse loop over exchanges in the water phase

        do iq = noqw, 1, -1
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito   <= 0) cycle
            do isys = 1, nosys
                pivot = diag(isys, ito)
                rhs (isys, ito) = rhs(isys, ito) / pivot
                diag(isys, ito) = 1.0
            enddo
            if (ifrom <= 0) cycle
            do isys = 1, nosys
                pivot = bcodia(isys, iq)
                rhs (isys, ifrom) = rhs (isys, ifrom) - pivot * rhs(isys, ito)
            enddo
        enddo

        do iseg = 1, noseg       !  for if some diagonal entries are not 1.0
            do isys = 1, nosys
                rhs (isys, iseg) = rhs(isys, iseg) / diag(isys, iseg)
                diag(isys, iseg) = 1.0
            enddo
        enddo
        conc = rhs
        deriv = diag

        !     Mass balances ?

        if (iaflag == 0) goto 9999

        do iq = 1, noq

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
            endif
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
                endif
                dl = a / al
            else
                dl = 0.0
            endif
            e = e * dl
            if (iq > noqw) e = 0.0      !  no constant water diffusion in the bottom

            do isys = 1, nosys

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
                    endif
                else
                    if (sw_settling) then         !  additional velocity upwind
                        if (v > 0.0) then
                            v1 = q * f1 + v
                            v2 = q * f2
                        else
                            v1 = q * f1
                            v2 = q * f2 + v
                        endif
                    else
                        v1 = (q + v) * f1
                        v2 = (q + v) * f2
                    endif
                endif

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
                        endif
                    else
                        dq = q3 * rhs  (isys, ifrom) + q4 * bound(isys, -ito)
                        if (dq > 0.0) then
                            amass2(isys, 5) = amass2(isys, 5) + dq
                        else
                            amass2(isys, 4) = amass2(isys, 4) - dq
                        endif
                    endif
                else
                    dq = q3 * rhs  (isys, ifrom) + q4 * rhs  (isys, ito)
                endif
                if (iqd > 0) then
                    if (dq > 0) then
                        dmpq(isys, iqd, 1) = dmpq(isys, iqd, 1) + dq
                    else
                        dmpq(isys, iqd, 2) = dmpq(isys, iqd, 2) - dq
                    endif
                endif
            enddo

        enddo

        9999 if (timon) call timstop (ithandl)
        return
    end

end module m_dlwqd1
