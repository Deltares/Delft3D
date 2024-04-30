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
module m_dlwq19
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine dlwq19(file_unit, nosys, notot, nototp, noseg, &
            nosss, noq1, noq2, noq3, noq, &
            noq4, nodisp, novelo, disp, disper, &
            velo, volold, volnew, area, flow, &
            surface, aleng, ipoint, idpnt, ivpnt, &
            amass, conc, dconc2, bound, idt, &
            ibas, ibaf, work, volint, iords, &
            iordf, deriv, wdrawal, iaflag, amass2, &
            ndmpq, ndmps, nowst, iqdmp, dmpq, &
            isdmp, dmps, iwaste, wstdmp, integration_id, &
            ilflag, rhs, diag, acodia, bcodia, &
            nvert, ivert, nocons, coname, const)

        !> Computational core of the flexible step method.
        !>
        !>        - Per time step it is determined what time step should be set for which computational cell.
        !>          Each cell is assigned the box number of the time step that it should use.
        !>        - A separate procedure is applied for flooding cells, since they can have both an inflow and
        !>          an outflow, but may not yet have realistic concentrations. This procedure steps with the
        !>          highest necessary frequency.
        !>        - Per box the following steps are set:
        !>          - The horizontal advective transport is set in the mass array with appropriate time step.
        !>          - The vertical water velocities are applied, but implicit and in this case with central
        !>            differences
        !>          - This results in a first guess for concentrations using upwind horizontal advection
        !>          - The flux correction step is set for the boxes of this sub-step
        !>        - For the whole area the additional vertical (e.g. settling-) velocities and (space varying)
        !>          vertical diffusion is set using an implicit method and central differences unless specified
        !>          differently
        !>        - The whole routine computes in double precision, but what comes in (concentrations, masses,
        !>          volumes, areas, flows, velocities and diffusions) are still in single precision and also the
        !>          resulting concentrations and masses that are given back to DELWAQ are still in real(4).
        !>        - The time step of the bed layer is still the overall time step (typically 1 hour). That may be
        !>          too long. It is good possible to have an input variable that specifies a shorter time step
        !>          for the bed underneith all cells only. Please indicate if that is interesting.

        use m_cli_utils, only : retrieve_command_argument
        use timers

        integer(kind = int_wp), intent(in) :: file_unit                  !< unit number of the monitoring file
        integer(kind = int_wp), intent(in) :: nosys                  !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                  !< total number of substances
        integer(kind = int_wp), intent(in) :: nototp                 !< number of particle substances
        integer(kind = int_wp), intent(in) :: noseg                  !< number of computational volumes
        integer(kind = int_wp), intent(in) :: nosss                  !< noseg + bed-computational volumes
        integer(kind = int_wp), intent(in) :: noq1                   !< number of interfaces in direction 1
        integer(kind = int_wp), intent(in) :: noq2                   !< number of interfaces in direction 2
        integer(kind = int_wp), intent(in) :: noq3                   !< number of interfaces in direction 3
        integer(kind = int_wp), intent(in) :: noq                    !< total number of interfaces
        integer(kind = int_wp), intent(in) :: noq4                   !< number of interfaces in the bed
        integer(kind = int_wp), intent(in) :: nodisp                 !< number additional dispersions
        integer(kind = int_wp), intent(in) :: novelo                 !< number additional velocities
        real(kind = real_wp), intent(in) :: disp   (3)             !< fixed dispersions in the 3 directions
        real(kind = real_wp), intent(in) :: disper (nodisp, noq)  !< array with additional dispersions
        real(kind = real_wp), intent(in) :: velo   (novelo, noq)  !< array with additional velocities
        real(kind = real_wp), intent(in) :: volold (nosss)        !< volumes of the segments at start of step
        real(kind = real_wp), intent(inout) :: volnew (nosss)        !< volumes of the segments at stop of step
        real(kind = real_wp), intent(in) :: area   (noq)        !< exchange areas in m2
        real(kind = real_wp), intent(in) :: flow   (noq)        !< flows through the exchange areas in m3/s
        real(kind = real_wp), intent(in) :: surface(nosss)        !< horizontal surface area
        real(kind = real_wp), intent(inout) :: aleng  (2, noq)  !< mixing length to and from the exchange area
        integer(kind = int_wp), intent(in) :: ipoint (4, noq)  !< from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: idpnt  (nosys)        !< additional dispersion number per substance
        integer(kind = int_wp), intent(in) :: ivpnt  (nosys)        !< additional velocity number per substance
        real(kind = real_wp), intent(inout) :: amass  (notot, nosss)  !< masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc   (notot, nosss)  !< concentrations at previous time level
        real(kind = dp) :: dconc2 (notot, nosss)  !< estimate used in flux correction
        real(kind = real_wp), intent(in) :: bound  (nosys, *)  !< open boundary concentrations
        integer(kind = int_wp), intent(in) :: idt                    !< time step in seconds
        integer(kind = int_wp) :: ibas   (noseg)        !< in which basket is my cell
        integer(kind = int_wp) :: ibaf   (noq)        !< in which basket is my flow
        real(kind = dp) :: work   (3, noseg)  !< work array
        real(kind = dp) :: volint (noseg)        !< fractional migrating volume
        integer(kind = int_wp) :: iords  (noseg)        !< order of segments
        integer(kind = int_wp) :: iordf  (noq)        !< order of fluxes
        real(kind = real_wp), intent(inout) :: deriv  (notot, nosss)  !< derivatives of the concentrations
        real(kind = real_wp), intent(inout) :: wdrawal(noseg)       !< withdrawals applied to all substances
        integer(kind = int_wp), intent(in) :: iaflag                 !< if 1 then accumulate mass in report array
        real(kind = real_wp), intent(inout) :: amass2 (notot, 5)  !< report array for monitoring file
        integer(kind = int_wp), intent(in) :: ndmpq                  !< number of dumped exchanges for mass balances
        integer(kind = int_wp), intent(in) :: ndmps                  !< number of dumped volumes for balances
        integer(kind = int_wp), intent(in) :: nowst                  !< number of wastes
        integer(kind = int_wp), intent(in) :: iqdmp  (noq)           !< pointer from echange to dump location
        real(kind = real_wp), intent(inout) :: dmpq   (nosys, ndmpq, 2)!< array with mass balance information
        integer(kind = int_wp), intent(in) :: isdmp  (noseg)        !< volume to dump-location pointer
        real(kind = real_wp), intent(inout) :: dmps   (notot, ndmps, *)!< dumped segment fluxes if integration_id > 7
        integer(kind = int_wp), intent(in) :: iwaste (nowst)        !< volume numbers of the waste locations
        real(kind = real_wp), intent(inout) :: wstdmp (notot, nowst, 2)!< accumulated wasteloads 1/2 in and out
        integer(kind = int_wp), intent(in) :: integration_id           !< integration
        integer(kind = int_wp), intent(in) :: ilflag                 !< if 0 then only 3 constant lenght values
        real(kind = dp), intent(inout) :: rhs    (notot, nosss)  !< local right hand side
        real(kind = dp), intent(inout) :: diag   (notot, nosss)  !< local diagonal filled with volumes
        real(kind = dp), intent(inout) :: acodia (notot, max(noq3 + noq4, 1))    !< local workarray under codiagonal
        real(kind = dp), intent(inout) :: bcodia (notot, max(noq3 + noq4, 1))    !< local workarray upper codiagonal
        integer(kind = int_wp) :: nvert  (2, noseg) !< Number of vertical cells per column, entry point in ivert
        integer(kind = int_wp) :: ivert  (noseg)        !< Number of vertical columns
        integer(kind = int_wp), intent(in) :: nocons                 !< Number of constants used
        character(20), intent(in) :: coname (nocons)        !< Constant names
        real(kind = real_wp), intent(in) :: const  (nocons)        !< Constants

        integer(kind = int_wp) :: i, j, k         ! general loop counter
        integer(kind = int_wp) :: noqh            ! total number of horizontal interfaces
        integer(kind = int_wp) :: noqv            ! total number of vertical interfaces in the water
        integer(kind = int_wp) :: iq              ! loop counter exchanges
        integer(kind = int_wp) :: iq2, iq3        ! help variables to identify first or second pointers
        integer(kind = int_wp) :: iqv             ! help variables in vertical arrays
        integer(kind = int_wp) :: isys            ! loop counter substance
        integer(kind = int_wp) :: iseg, iseg2     ! loopcounter computational volumes
        integer(kind = int_wp) :: ifrom, ito    ! from and to volume numbers
        real(kind = dp) :: vfrom, vto    ! from   and to   volumes
        integer(kind = int_wp) :: ifrom_1, ito_1  ! from-1 and to+1 volume numbers
        real(kind = dp) :: cfrm_1, cto_1  ! from-1 and to+1 concentration values
        integer(kind = int_wp) :: ipb             ! pointer in the mass balance dump array
        integer(kind = int_wp) :: iqd             ! help variable for dump pointers
        real(kind = dp) :: a               ! this area
        real(kind = dp) :: q               ! flow for this exchange
        real(kind = dp) :: e               ! didpersion for this exchange
        real(kind = dp) :: al              ! this length
        real(kind = dp) :: dl              ! area / length
        real(kind = dp) :: d               ! didpersion for this substance
        real(kind = dp) :: dq              ! total flux from and to
        real(kind = dp) :: pivot           ! help variable matrix inversion
        real(kind = dp) :: vol             ! helpvariable for this volume
        real(kind = dp) :: e1, e2, e3      ! limiter help variable
        real(kind = dp) :: s               ! limiter sign variable
        real(kind = dp) :: f1, f2         ! correction factors central differences
        real(kind = dp) :: q1, q2, q3, q4  ! helpvariables to fill the matrix
        logical       disp0q0         ! bit zero  of integration_id: 1 if no dispersion at zero flow
        logical       disp0bnd        ! bit one   of integration_id: 1 if no dispersion across boundaries
        logical       loword          ! bit two   of integration_id: 1 if lower order across boundaries
        logical       fluxes          ! bit three of integration_id: 1 if mass balance output
        logical       abound          ! is it a boundary?
        logical       wetting         ! are cells becoming wet?
        logical, save :: sw_settling   ! if true, settling should be dealt with upwind
        integer(kind = int_wp), save :: init = 0      ! first call ?
        character :: cdummy        !
        integer(kind = int_wp) :: idummy        !
        real(kind = real_wp) :: rdummy        !

        integer(kind = int_wp), save :: nob       ! number of baskets for transportables
        integer(kind = int_wp), allocatable, save :: its  (:)  ! baskets accumulator cells
        integer(kind = int_wp), allocatable, save :: itf  (:)  ! baskets accumulator flows    , nob+2 stays dry
        real(kind = dp), allocatable, save :: dt   (:)         ! delta time value of baskets  , nob+1 becomes wet
        integer(kind = int_wp), allocatable, save :: iqsep(:)! separation point flows in 3rd direction
        integer(kind = int_wp), save :: nosegl            ! number of cells per layer
        integer(kind = int_wp) :: isums, isumf             ! accumulators
        integer(kind = int_wp) :: ibox, nb                 ! help variable for boxes
        integer(kind = int_wp) :: iofs, ioff               ! offsets in the arrays
        integer(kind = int_wp) :: fbox, lbox, nbox         ! box range
        real(kind = dp) :: fact                            ! interpolation factor for volumes
        integer(kind = int_wp) :: istep, nstep             ! fractional step variables
        integer(kind = int_wp) :: is1, is2, if1, if2       ! loop variables per box
        integer(kind = int_wp) :: ih1, ih2                 ! help variables parallellism
        integer(kind = int_wp) :: ilay                     ! loop counter layers
        integer(kind = int_wp) :: maxlay                   ! maximum number of layers observed in this model
        integer(kind = int_wp) :: bmax                     ! maximum box number in a column
        integer(kind = int_wp) :: changed, remained, iter  ! flooding help variables
        real(kind = dp), allocatable, save :: low(:), dia(:), upr(:)  !  matrix of one column
        logical             massbal                     ! set .true. if iaflag eq 1
        logical, save :: report                            ! write iteation reports in monitoring file
        integer(kind = int_wp) :: ierr2                    !
        real(kind = real_wp) :: acc_remained, acc_changed  ! For reporting: accumulated/averaged reporting parameters
        logical :: vertical_upwind             ! Set .true. for upwind scheme in the vertical
        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp) :: ithand1 = 0
        integer(kind = int_wp) :: ithand2 = 0
        integer(kind = int_wp) :: ithand3 = 0
        integer(kind = int_wp) :: ithand4 = 0
        integer(kind = int_wp) :: ithand5 = 0
        integer(kind = int_wp) :: ithand6 = 0
        integer(kind = int_wp) :: ithand7 = 0
        if (timon) call timstrt ("dlwq19", ithandl)

        !         Initialisations

        if (timon) call timstrt ("administration", ithand1)
        noqh = noq1 + noq2
        massbal = iaflag == 1
        disp0q0 = btest(integration_id, 0)
        disp0bnd = btest(integration_id, 1)
        loword = btest(integration_id, 2)
        fluxes = btest(integration_id, 3)
        vertical_upwind = .not. btest(integration_id, 18)

        if (init == 0) then
            write (file_unit, '(A)') ' Using local flexible time step method (scheme 24)'
            if (vertical_upwind) then
                write (file_unit, '(A)') ' Using upwind discretisation for vertical advection.'
            else
                write (file_unit, '(A)') ' Using central discretisation for vertical advection.'
            endif
            call retrieve_command_argument('-settling_backwards', 0, sw_settling, idummy, rdummy, cdummy, ierr2)
            if (sw_settling) write(file_unit, *) ' option -settling_backwards found'
            i = index_in_array('Number_of_baskets   ', coname)
            if (i > 0) then
                nob = const(i)
                write (file_unit, '(A,i3)') ' Number of baskets         : ', nob
            else
                nob = 13
                write (file_unit, '(A,i3)') ' Default number of baskets : ', nob
            endif
            allocate (its(nob + 2), itf(nob + 2), iqsep(nob + 2), dt(nob + 1))
            report = .false.
            i = index_in_array('Iteration report    ', coname)
            if (i > 0) then
                report = const(i) > 0
            endif
            if (report) then
                write (file_unit, '(A)') ' Iteration report          : switched on'
            else
                write (file_unit, '(A)') ' Iteration report          : switched off'
            endif

            if (.not. disp0q0) then
                write (file_unit, '(/3A)') &
                        ' WARNING: Dispersion allowed if flow rate is zero', &
                        '          This is known to cause problems in some cases'
            endif

            if (noq3 == 0) then         ! vertically integrated model
                do iseg = 1, noseg
                    nvert(1, iseg) = iseg
                    nvert(2, iseg) = iseg
                    ivert(iseg) = iseg
                enddo
                nosegl = noseg
                write (file_unit, '(A)') ' This model is vertically integrated!'
            else                            ! model with (per cell varying nr of) layers
                ivert = 0
                nvert = -1                                    !  Determine whether cells have a horizontal exchange
                do iq = 1, noqh
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    if (ifrom > 0) then
                        nvert(1, ifrom) = 0
                        nvert(2, ifrom) = 0
                    endif
                    if (ito   > 0) then
                        nvert(1, ito) = 0
                        nvert(2, ito) = 0
                    endif
                enddo
                do iq = noqh + 1, noq                           !  Make the vertical administration
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    if (ifrom <= 0 .or. ito <= 0) cycle
                    nvert(1, ifrom) = ito                       !  this is the one cell below 'ifrom'
                    nvert(2, ito) = ifrom                     !  this is the one cell above 'ito'
                enddo
                ioff = 0
                do iseg = 1, noseg
                    if (nvert(2, iseg) == 0) then           !  this cell starts a column (has no 'ifrom')
                        ioff = ioff + 1
                        nvert(2, iseg) = ioff                  !  column starts at ioff in ivert
                        ivert(ioff) = iseg
                        i = nvert(1, iseg)                       !  this is the cell below
                        do while (i > 0)
                            ioff = ioff + 1
                            ivert(ioff) = i
                            i = nvert(1, i)
                        enddo
                    else
                        nvert(2, iseg) = 0
                    endif
                enddo
                nosegl = 0
                do iseg = 1, noseg
                    if (nvert(2, iseg) > 0) then
                        nosegl = nosegl + 1
                        nvert(1, nosegl) = nvert(2, iseg)         !  to find head of column
                        nvert(2, iseg) = nosegl                !  point to column number
                    endif
                enddo
                if (nosegl < noseg) nvert(1, nosegl + 1) = ioff + 1
                write (file_unit, '(A,i8,A)') ' This model has            : ', nosegl, ' columns of cells'
                maxlay = 0
                do i = 1, nosegl
                    is1 = nvert(1, i)                         !  offset of the cell that heads the column in ivert table
                    if (i < noseg) then
                        is2 = nvert(1, i + 1)                      !  offset of the cell that heads next column
                    else
                        is2 = noseg + 1
                    endif
                    maxlay = max(maxlay, is2 - is1)
                    do j = is1 + 1, is2 - 1
                        iseg = ivert(j)
                        nvert(2, iseg) = -i                      !  for non-head of column cells point to minus the column #
                    enddo
                enddo
                allocate (low(maxlay), dia(maxlay), upr(maxlay))
                write (file_unit, '(A,i4,A)') ' This model has at most    : ', maxlay, ' layers'
            endif
            !    after this all: ivert(1:noseg)     contains all water cell numbers in their order of appearance in the columns
            !                    nvert(1,1:nosegl)  contains the start locations in ivert of columns 1:nosegl
            !                    nvert(1,nosegl+1)  contains the start location of the non existing column nosegl+1
            !                    nvert(2,1:noseg)   contains the column number of each cell, negative if not head of column
            !    the procedure works for any cell numbering if: the columns all are 1D-vertical so all 1-cell wide stacks
            !                                                   the vertical exchanges run from noq1+noq2+1 to noq1+noq2+noq3
            !                                                   the positive velocity or flow is from ipoint(1,iq) to ipoint(2,iq)
            !    it is easily seen that for 2D-horizontal models ivert and nvert(1:2,*) just contain the sequential cell numbers and
            !                    nosegl = noseg. Since nvert(1,noseg+1) is out of range, you will find statements that deal with this.
            write (file_unit, '(A)') ' '
            init = 1    !   do this only once per simulation
        endif

        !     PART 1 : make the administration for the variable time step approach
        !          1a: fill the array with time-tresholds per basket, 13 baskets span 1 hour - 0.9 second

        dt(1) = real(idt, kind=dp)
        do ibox = 2, nob
            dt(ibox) = dt(ibox - 1) / 2.0d0
        enddo

        !          1b: sum the outgoing (1,..) and ingoing (2,..) horizontal flows and constant diffusions (3,..) per cell

        work = 0.0d0
        d = disp (1)
        al = aleng(1, 1)
        do iq = 1, noq

            !
            ! Note: If the model uses an unstructured grid, noq2 may be zero, so noqh == noq1.
            !       Therefore first check for the vertical direction, then for the second
            !       horizontal direction
            !
            if (iq == noqh + 1) then
                d = 0.0d0
                al = aleng(1, 2)
            elseif (iq == noq1 + 1) then
                d = disp (2)
                al = aleng(2, 1)
            endif
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom < 0 .and. ito < 0) cycle
            a = area(iq)
            q = flow(iq)
            if (ilflag == 1) al = aleng(1, iq) + aleng(2, iq)
            e = d * a / al
            if (ifrom < 0) then                           ! Boundary
                if (flow(iq) > 0.0d0) then
                    work(2, ito) = work(2, ito) + flow(iq)
                else
                    work(1, ito) = work(1, ito) - flow(iq)
                endif
                if (.not. disp0bnd) then
                    if (flow(iq) /= 0.0 .or. .not. disp0q0) then
                        work(3, ito) = work(3, ito) + e
                    endif
                endif
                cycle
            endif
            if (ito   < 0) then                           ! Boundary
                if (flow(iq) > 0.0) then
                    work(1, ifrom) = work(1, ifrom) + flow(iq)
                else
                    work(2, ifrom) = work(2, ifrom) - flow(iq)
                endif
                if (.not. disp0bnd) then
                    if (flow(iq) /= 0.0 .or. .not. disp0q0) then
                        work(3, ifrom) = work(3, ifrom) + e
                    endif
                endif
                cycle
            endif                                              ! Internal
            if (flow(iq) > 0.0) then
                if (ifrom > 0) work(1, ifrom) = work(1, ifrom) + flow(iq)
                if (ito   > 0) work(2, ito) = work(2, ito) + flow(iq)
            else
                if (ifrom > 0) work(2, ifrom) = work(2, ifrom) - flow(iq)
                if (ito   > 0) work(1, ito) = work(1, ito) - flow(iq)
            endif
            if (flow(iq) /= 0.0 .or. .not. disp0q0) then
                work(3, ifrom) = work(3, ifrom) + e
                work(3, ito) = work(3, ito) + e
            endif
        enddo
        ! Add withdrawals to outgoing (1,..) too
        do iseg = 1, noseg
            work(1, iseg) = work(1, iseg) + wdrawal(iseg)
        end do

        !          1c: assign a basket number to each cell

        ibas = 0
        wetting = .false.
        do iseg = 1, noseg
            if (work(1, iseg) <= 0.0d0 .and. &
                    work(2, iseg) <= 0.0d0 .and. &
                    work(3, iseg) <= 0.0d0) then
                ibas(iseg) = nob + 2          !  cell is dry, number is 1 higher than
                cycle                       !  the nr of wet and 'wetting' basket
            endif
            if ((work(1, iseg) + work(3, iseg)) * dt(1) < volold(iseg)) then    !  box 1 works even if volnew(iseg) is zero
                ibas(iseg) = 1
                cycle
            endif
            if (volnew(iseg) >= volold(iseg)) then          !  test only with volold(iseg) to determine fractional step
                do ibox = 2, nob
                    if ((work(1, iseg) + work(3, iseg)) * dt(ibox) < volold(iseg)) then
                        ibas(iseg) = ibox        !  this cell in the basket of this dt(ibox)
                        exit
                    endif
                    if (ibox == nob) then   !  no suitable time step in range
                        ibas(iseg) = nob + 1       !  so cell is considered becoming wet
                        wetting = .true.      !  by simultaneous inflow: 'wetting' basket.
                    endif
                enddo
            else                                                !  also the last fractional step should be stable
                do ibox = 2, nob
                    if ((work(1, iseg) + work(3, iseg) - (volold(iseg) - volnew(iseg)) / dt(1)) * dt(ibox) < volnew(iseg)) then
                        ibas(iseg) = ibox        !  this cell in the basket of this dt(ibox)
                        exit
                    endif
                    if (ibox == nob) then   !  no suitable time step in range
                        ibas(iseg) = nob + 1       !  so cell is considered becoming dry
                        wetting = .true.      !  by simultaneous inflow: 'wetting' basket.
                    endif
                enddo
            endif
        enddo

        !          1d: give each cell of a column the highest basket nr. of the column

        do i = 1, nosegl
            is1 = nvert(1, i)
            if (i < noseg) then
                is2 = nvert(1, i + 1)
            else
                is2 = noseg + 1
            endif
            bmax = 0
            do j = is1, is2 - 1
                iseg = ivert(j)
                if (ibas(iseg) <= nob + 1) then
                    bmax = max(bmax, ibas(iseg))
                endif
            enddo
            if (bmax == 0) cycle
            do j = is1, is2 - 1
                iseg = ivert(j)
                if (ibas(iseg) <= nob + 1) then
                    ibas(iseg) = bmax
                endif
            enddo
        enddo
        if (wetting .and. report) then
            if (nosegl == noseg) then
                write (file_unit, '(/A/A)') &
                        ' WARNING in dlwq19, next cells are becoming wet or dry:', &
                        '  cell       outflow         inflow          diffusion       volume-1        volume-2'
            else
                write (file_unit, '(/A/A)') &
                        ' WARNING in dlwq19, next cells and the cells underneith are becoming wet or dry:', &
                        '  cell       outflow         inflow          diffusion       volume-1        volume-2'
            endif
            do i = 1, nosegl
                iseg = ivert(nvert(1, i))
                if (ibas(iseg) == nob + 1) write (file_unit, '(i10,5e16.7)') &
                        iseg, work(1, iseg), work(2, iseg), work(3, iseg), volold(iseg), volnew(iseg)
            enddo
        endif

        !          1e: count the size of the baskets for segments

        its = 0
        do iseg = 1, noseg
            its(ibas(iseg)) = its(ibas(iseg)) + 1
        enddo

        !          1f: determine size of the baskets for fluxes (highest of 'from' and 'to')

        itf = 0
        ibaf = 0
        do iq = 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ibox = 0
            if (ifrom > 0) ibox = ibas(ifrom)
            if (ito   > 0) ibox = max (ibox, ibas(ito))
            if (ibox  == 0) ibox = nob + 2
            ibaf(iq) = ibox
            itf (ibox) = itf(ibox) + 1
        enddo

        !          1g: write report on basket sizes

        if (report) then
            write (file_unit, *) ' box       cells    fluxes'
            isums = 0
            isumf = 0
            do ibox = 1, nob + 2
                write (file_unit, '(i5,2x,2i10)') ibox, its(ibox), itf(ibox)
                if (ibox == nob) write (file_unit, '(A)') ' '
                isums = isums + its(ibox)
                isumf = isumf + itf(ibox)
            enddo
            write (file_unit, '(/a,2i9)') 'Total number of cells & fluxes: ', isums, isumf
        endif

        !          1h: determine execution order of the segments and fluxes

        iofs = 0
        ioff = 0
        do ibox = nob + 2, 1, -1         ! start with highest frequency
            do iseg = 1, noseg            ! array of segments in this basket
                if (ibas(iseg) == ibox) then
                    iofs = iofs + 1
                    iords(iofs) = iseg
                endif
            enddo
            do iq = 1, noqh               ! array of horizontal fluxes in this basket
                if (ibaf(iq) == ibox) then
                    ioff = ioff + 1
                    iordf(ioff) = iq
                endif
            enddo
            iqsep(ibox) = ioff            ! now the vertical fluxes start
            do iq = noqh + 1, noq           ! array of the vertical fluxes
                if (ibaf(iq) == ibox) then
                    ioff = ioff + 1
                    iordf(ioff) = iq
                endif
            enddo
        enddo
        do ibox = 1, nob                 ! lowest active box number (largest time step)
            if (its(ibox) > 0) then  !                          (last box to evaluate)
                lbox = ibox
                exit
            endif
        enddo
        do ibox = nob, 1, -1             ! highest active box number (smallest time step)
            if (its(ibox) > 0) then  !                           (first box to evaluate)
                fbox = ibox
                exit
            endif
        enddo
        do ibox = nob + 1, 1, -1           ! accumulate the counts in reversed order
            its(ibox) = its(ibox) + its(ibox + 1)
            itf(ibox) = itf(ibox) + itf(ibox + 1)
        enddo
        nb = fbox - lbox + 1          ! so many boxes are used
        nstep = 1
        do ibox = 2, fbox
            nstep = nstep * 2               ! so many sub-time steps will be set
        enddo
        if (report) then
            write (file_unit, '(a,i2,A,i2,A,i2)') 'Nr of boxes: ', nb, ',first: ', lbox, ', last: ', fbox
            write (file_unit, '(a,e15.7/)')       'Smallest time step in sec.: ', dt(fbox)
        endif
        dt(nob + 1) = dt(fbox)             ! boxes running wet do so with the smallest step size

        !          1i: Create backpointers from cell to order of execution and to box nr.

        !      The backpointer became obsolete, ibas can be reused directly

        !         1j: Fill the off-diagonals of the matrix for the vertical advection of water only
        !             (Note that the variable work is reused with a different meaning (JvG 2016)

        work = 0.0
        do ibox = nob, lbox, -1         ! Fill the off-diagonals only once per time step
            if1 = iqsep(ibox) + 1
            if2 = itf  (ibox)
            do i = if1, if2
                iq = iordf(i)
                ifrom = ipoint(1, iq)               !  The diagonal now is the sum of the
                ito = ipoint(2, iq)               !  new volume that increments with each step
                if (ifrom == 0 .or.  ito == 0) cycle
                q = flow(iq) * dt(ibox)
                work(3, ifrom) = q                 ! flow through lower surface (central or upwind now arranged in one spot, further down)
                work(1, ito) = q                 ! flow through upper surface
            enddo
        enddo
        if (timon) call timstop (ithand1)

        !     PART2: set the fractional step loop for this time step

        do iseg = 1, noseg
            do isys = 1, nosys
                dconc2(isys, iseg) = conc (isys, iseg)      ! Initialize dconc2. Becomes new estimate
                rhs   (isys, iseg) = amass(isys, iseg)
            enddo
        enddo

        acc_remained = 0.0
        acc_changed = 0.0
        volint = volold                                 ! Initialize volint. Becomes the volume 'in between'.
        do istep = 1, nstep                         ! Big loop over the substeps
            fact = real(istep) / real(nstep, kind=dp)         ! Interpolation factor of this step
            ! istep:  boxes to integrate:           modulo logic
            !         last boxe to integrate for this sub step           !   1     fbox
            !   2     fbox, fbox-1                  mod(2    ) = 0
            nbox = fbox                                         !   3     fbox
            ioff = 1                                            !   4     fbox, fbox-1, fbox-2          mod(2&4  ) = 0
            do ibox = 1, nb - 1                                   !   5     fbox
                ioff = ioff * 2                                    !   6     fbox, fbox-1                  mod(2    ) = 0
                if (mod(istep, ioff) == 0) nbox = nbox - 1      !   7     fbox
            enddo                                               !   8     fbox, fbox-1, fbox-2, fbox-3  mod(2&4&8) = 0
            !  etc.
            !     PART2a: deal with those cells that are running wet

            if (timon) call timstrt ("flooding", ithand2)   !  'flooding' is evaluated with highest frequency
            if1 = itf(nob + 2) + 1                                  !  loop limiters for fluxes in this box
            if2 = iqsep(nob + 1)
            is1 = its(nob + 2) + 1                                  !  loop limiters for cells in this box
            is2 = its(nob + 1)

            !     PART2a1: sum the mass and volume vertically in the first layer and
            !              make the column averaged concentrations

            do i = is1, is2
                iseg = iords(i)
                j = nvert(2, iseg)                      ! column number if iseg = head of column
                if (j <= 0) cycle                     !    zero if not head of column
                ih1 = nvert(1, j)                          ! pointer to first cell of this column
                if (j < noseg) then
                    ih2 = nvert(1, j + 1)                     ! pointer to first cell of next column
                else
                    ih2 = noseg + 1                          ! or to just over the edge if j = last column
                endif
                do j = ih1 + 1, ih2 - 1
                    iseg2 = ivert(j)                                       ! cell number of this cell in column
                    volint(iseg) = volint(iseg) + volint(iseg2)            ! sum volumes to volumes of 'head of column'
                    do isys = 1, nosys
                        rhs(isys, iseg) = rhs(isys, iseg) + rhs(isys, iseg2)   ! sum masses  to masses  of 'head of column'
                    enddo
                enddo
            enddo
            do i = is1, is2
                iseg = iords(i)
                j = nvert(2, iseg)
                if (j <= 0) cycle
                if (abs(volint(iseg)) > 1.0d-25) then
                    do isys = 1, nosys
                        conc (isys, iseg) = rhs(isys, iseg) / volint(iseg)      ! column averaged concentrations
                    enddo
                else                                                      ! dry
                    do isys = 1, nosys
                        rhs (isys, iseg) = 0.0d0
                        conc(isys, iseg) = 0.0d0
                    enddo
                endif
            enddo

            !     PART2a1: apply all influxes to the cells first

            remained = 1
            iter = 0
            do while (remained > 0)
                changed = 0
                remained = 0
                iter = iter + 1
                do i = if1, if2
                    iq = iordf(i)
                    if (iq < 0) cycle                                 ! this flux has been resolved already
                    if (flow(iq) == 0.0) cycle
                    q = flow(iq) * dt(fbox)
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    ipb = 0
                    if (fluxes) then
                        if (iqdmp(iq) > 0) ipb = iqdmp(iq)
                    endif
                    if (ifrom < 0) then
                        if (q > 0.0d0) then
                            ito = ivert(nvert(1, abs(nvert(2, ito))))   ! cell-nr at offset of head of collumn in ivert
                            volint(ito) = volint(ito) + q
                            do isys = 1, nosys
                                dq = q * bound(isys, -ifrom)
                                rhs (isys, ito) = rhs(isys, ito) + dq
                                conc(isys, ito) = rhs(isys, ito) / volint(ito)
                                if (massbal) amass2(isys, 4) = amass2(isys, 4) + dq
                                if (ipb > 0) dmpq  (isys, ipb, 1) = dmpq  (isys, ipb, 1) + dq
                            enddo
                            iordf(i) = -iordf(i)                             ! this flux is resolved now
                            changed = changed + 1                          ! nr. of handled fluxes
                        endif
                        cycle
                    endif
                    if (ito   < 0) then
                        if (q < 0.0d0) then
                            ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                            volint(ifrom) = volint(ifrom) - q
                            do isys = 1, nosys
                                dq = q * bound(isys, -ito)
                                rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                                conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                                if (massbal) amass2(isys, 4) = amass2(isys, 4) - dq
                                if (ipb > 0) dmpq  (isys, ipb, 2) = dmpq  (isys, ipb, 2) - dq
                            enddo
                            iordf(i) = -iordf(i)
                            changed = changed + 1
                        endif
                        cycle
                    endif
                    if (q > 0) then                                   ! Internal volumes
                        if (ibas(ito) == nob + 1) then                  !    'to' should be wetting if q > 0
                            if (ibas(ifrom) == nob + 1) then               !       'from' is also wetting in this time step
                                ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                                ito = ivert(nvert(1, abs(nvert(2, ito))))
                                if (volint(ifrom) >=  q) then             !          it should then have enough volume
                                    volint(ifrom) = volint(ifrom) - q
                                    volint(ito) = volint(ito) + q
                                    do isys = 1, nosys
                                        dq = q * conc(isys, ifrom)
                                        rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                                        rhs (isys, ito) = rhs(isys, ito) + dq
                                        if (volint(ifrom) > 1.0d-25) conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                                        conc(isys, ito) = rhs(isys, ito) / volint(ito)
                                        if (ipb > 0) dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq
                                    enddo
                                    iordf(i) = -iordf(i)
                                    changed = changed + 1
                                else
                                    remained = remained + 1                   !          flux not resolved yet by lack of volume
                                endif
                            else                                             !       'from' is not 'wetting' so it has enough volume
                                ito = ivert(nvert(1, abs(nvert(2, ito))))
                                volint(ifrom) = volint(ifrom) - q
                                volint(ito) = volint(ito) + q
                                do isys = 1, nosys
                                    dq = q * conc(isys, ifrom)
                                    rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                                    rhs (isys, ito) = rhs(isys, ito) + dq
                                    if (volint(ifrom) > 1.0d-25) conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                                    conc(isys, ito) = rhs(isys, ito) / volint(ito)
                                    if (ipb > 0) dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq
                                enddo
                                iordf(i) = -iordf(i)
                                changed = changed + 1
                            endif
                        endif
                    else                                                   ! same procedure but now mirrorred for q < 0
                        if (ibas(ifrom) == nob + 1) then
                            if (ibas(ito) == nob + 1) then
                                ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                                ito = ivert(nvert(1, abs(nvert(2, ito))))
                                if (volint(ito) > -q) then
                                    volint(ifrom) = volint(ifrom) - q
                                    volint(ito) = volint(ito) + q
                                    do isys = 1, nosys
                                        dq = q * conc(isys, ito)
                                        rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                                        rhs (isys, ito) = rhs(isys, ito) + dq
                                        conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                                        if (volint(ito) > 1.0d-25) conc(isys, ito) = rhs(isys, ito) / volint(ito)
                                        if (ipb > 0) dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq
                                    enddo
                                    iordf(i) = -iordf(i)
                                    changed = changed + 1
                                else
                                    remained = remained + 1
                                endif
                            else
                                ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                                volint(ifrom) = volint(ifrom) - q
                                volint(ito) = volint(ito) + q
                                do isys = 1, nosys
                                    dq = q * conc(isys, ito)
                                    rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                                    rhs (isys, ito) = rhs(isys, ito) + dq
                                    conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                                    if (volint(ito) > 1.0d-25) conc(isys, ito) = rhs(isys, ito) / volint(ito)
                                    if (ipb > 0) dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq
                                enddo
                                iordf(i) = -iordf(i)
                                changed = changed + 1
                            endif
                        endif
                    endif
                end do
                if (changed /= 0 .or. remained /= 0) then
                    acc_remained = acc_remained + remained
                    acc_changed = acc_changed + changed
                    if (remained > 0 .and. changed == 0) then
                        if (report) then
                            write (file_unit, *) 'Warning: No further progress in the wetting procedure!'
                        endif
                        exit
                    endif
                endif
            end do

            !     PART2a2: apply all outfluxes to the outer world from these cells that should have reasonable concentrations
            !                                                                                       and enough volume now
            do i = if1, if2
                iq = iordf(i)
                if (iq < 0) cycle
                if (flow(iq) == 0.0) cycle
                q = flow(iq) * dt(fbox)
                ifrom = ipoint(1, iq)
                ito = ipoint(2, iq)
                ipb = 0
                if (fluxes) then
                    if (iqdmp(iq) > 0) ipb = iqdmp(iq)
                endif
                if (ifrom < 0) then                                  ! The 'from' element was a boundary.
                    if (q < 0.0d0) then
                        ito = ivert(nvert(1, abs(nvert(2, ito))))
                        volint(ito) = volint(ito) + q
                        do isys = 1, nosys
                            dq = q * conc(isys, ito)
                            rhs (isys, ito) = rhs(isys, ito) + dq
                            if (volint(ito) > 1.0d-25) conc(isys, ito) = rhs(isys, ito) / volint(ito)
                            if (massbal) amass2(isys, 5) = amass2(isys, 5) - dq
                            if (ipb > 0) dmpq  (isys, ipb, 2) = dmpq  (isys, ipb, 2) - dq
                        enddo
                    endif
                    cycle
                endif
                if (ito   < 0) then                                  ! The 'to'   element was a boundary.
                    if (q > 0.0d0) then
                        ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                        volint(ifrom) = volint(ifrom) - q
                        do isys = 1, nosys
                            dq = q * conc(isys, ifrom)
                            rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                            if (volint(ifrom) > 1.0d-25) conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                            if (massbal) amass2(isys, 5) = amass2(isys, 5) + dq
                            if (ipb > 0) dmpq  (isys, ipb, 1) = dmpq  (isys, ipb, 1) + dq
                        enddo
                    endif
                    cycle
                endif
                if (q > 0) then                                      ! The normal case
                    ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))         !    'from' should be wetting if q > 0
                    volint(ifrom) = volint(ifrom) - q
                    volint(ito) = volint(ito) + q
                    do isys = 1, nosys
                        dq = q * conc(isys, ifrom)
                        rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                        rhs (isys, ito) = rhs(isys, ito) + dq
                        if (volint(ifrom) > 1.0d-25) conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                        conc(isys, ito) = rhs(isys, ito) / volint(ito)
                        if (ipb > 0) dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq
                    enddo
                else                                                      ! The mirrorred case
                    ito = ivert(nvert(1, abs(nvert(2, ito))))         !    'to' should be wetting if q < 0
                    volint(ifrom) = volint(ifrom) - q
                    volint(ito) = volint(ito) + q
                    do isys = 1, nosys
                        dq = q * conc(isys, ito)
                        rhs (isys, ifrom) = rhs(isys, ifrom) - dq
                        rhs (isys, ito) = rhs(isys, ito) + dq
                        conc(isys, ifrom) = rhs(isys, ifrom) / volint(ifrom)
                        if (volint(ito) > 1.0d-25) conc(isys, ito) = rhs(isys, ito) / volint(ito)
                        if (ipb > 0) dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq
                    enddo
                endif
            end do
            do i = if1, if2                                             ! All fluxes of the 'wetting-group' should have been resolved
                iordf(i) = abs(iordf(i))                                  ! Reset the flux pointer to its positive value
            enddo

            !     PART2a3: apply all withdrawals that were present in the hydrodynamics as negative wasteload rather than as open boundary flux

            do i = is1, is2
                iseg2 = iords(i)                                          ! cell number
                if (wdrawal(iseg2) == 0.0) cycle
                q = wdrawal(iseg2) * dt(fbox)
                iseg = ivert(nvert(1, abs(nvert(2, iseg2))))             ! cell number of head of column
                if (q <= volint(iseg)) then
                    volint(iseg) = volint(iseg) - q
                else
                    write (file_unit, '(A,i8,E16.7,A,E16.7,A)') 'Warning: trying to withdraw from cell', iseg2, q, &
                            ' m3. Available is', volint(iseg), ' m3!'
                    q = volint(iseg)
                    volint(iseg) = 0.0d0
                endif
                ipb = isdmp(iseg2)
                do isys = 1, nosys
                    dq = q * conc(isys, iseg)
                    rhs (isys, iseg) = rhs(isys, iseg) - dq
                    if (massbal) amass2(isys, 3) = amass2(isys, 3) - dq
                    if (ipb > 0) dmps  (isys, ipb, 3) = dmps  (isys, ipb, 3) + dq
                enddo
                do k = 1, nowst
                    if (iseg2 == iwaste(k)) then
                        do isys = 1, nosys
                            wstdmp(isys, k, 2) = wstdmp(isys, k, 2) + q * conc(isys, iseg)
                        enddo
                        exit
                    endif
                enddo
            enddo

            !     PART2a4: expand the depth averaged result to all layers for this group of cells

            do i = is1, is2
                iseg = iords(i)
                j = nvert(2, iseg)
                if (j > 0) then                                      ! this is head of column
                    ih1 = nvert(1, j)
                    if (j < noseg) then
                        ih2 = nvert(1, j + 1)
                    else
                        ih2 = noseg + 1
                    endif
                    vol = 0.0d0                                            ! determine new integrated volume in the flow-file
                    do j = ih1, ih2 - 1
                        iseg2 = ivert(j)
                        vol = vol + fact * volnew(iseg2) + (1.0d0 - fact) * volold(iseg2)
                        do isys = 1, nosys                                  !    apply the derivatives (also wasteloads)
                            rhs (isys, iseg) = rhs(isys, iseg) + deriv(isys, iseg2) * dt(fbox)
                        enddo
                    enddo
                    if (vol > 1.0d-25) then
                        do isys = 1, nosys                                  !    the new concentrations
                            conc(isys, iseg) = rhs(isys, iseg) / vol
                        enddo
                    endif
                    do j = ih1, ih2 - 1
                        iseg2 = ivert(j)
                        f1 = fact * volnew(iseg2) + (1.0d0 - fact) * volold(iseg2)
                        volint(iseg2) = f1
                        do isys = 1, nosys
                            conc(isys, iseg2) = conc(isys, iseg)
                            if (f1 > 1.0d-25) then
                                rhs (isys, iseg2) = conc(isys, iseg) * f1
                            else
                                rhs (isys, iseg2) = 0.0d0
                            endif
                        enddo
                    enddo
                endif
            enddo
            if (timon) call timstop (ithand2)

            !     PART2b: set a first order initial horizontal step for all cells in the boxes of this time step

            if (timon) call timstrt ("explicit hor-step", ithand3)
            do ibox = fbox, nbox, -1
                if1 = itf  (ibox + 1) + 1
                if2 = iqsep(ibox)
                do i = if1, if2
                    iq = iordf(i)
                    if (flow(iq) == 0.0) cycle
                    q = flow(iq) * dt(ibox)
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    if (ifrom == 0 .or. ito == 0) cycle
                    if (ifrom < 0) then                               ! The 'from' element was a boundary.
                        volint(ito) = volint(ito) + q
                        if (q > 0.0d0) then
                            do isys = 1, nosys
                                dq = q * bound(isys, -ifrom)
                                rhs  (isys, ito) = rhs  (isys, ito) + dq
                            enddo
                        else
                            do isys = 1, nosys
                                dq = q * conc (isys, ito)
                                rhs  (isys, ito) = rhs  (isys, ito) + dq
                            enddo
                        endif
                        cycle
                    endif
                    if (ito   < 0) then                               ! The 'to' element was a boundary.
                        volint(ifrom) = volint(ifrom) - q
                        if (q > 0.0d0) then
                            do isys = 1, nosys
                                dq = q * conc (isys, ifrom)
                                rhs  (isys, ifrom) = rhs  (isys, ifrom) - dq
                            enddo
                        else
                            do isys = 1, nosys
                                dq = q * bound(isys, -ito)
                                rhs  (isys, ifrom) = rhs  (isys, ifrom) - dq
                            enddo
                        endif
                        cycle
                    endif
                    volint(ifrom) = volint(ifrom) - q                      ! The regular case
                    volint(ito) = volint(ito) + q
                    if (q > 0.0d0) then
                        do isys = 1, nosys
                            dq = q * conc(isys, ifrom)
                            rhs  (isys, ifrom) = rhs  (isys, ifrom) - dq
                            rhs  (isys, ito) = rhs  (isys, ito) + dq
                        enddo
                    else
                        do isys = 1, nosys
                            dq = q * conc(isys, ito)
                            rhs  (isys, ifrom) = rhs  (isys, ifrom) - dq
                            rhs  (isys, ito) = rhs  (isys, ito) + dq
                        enddo
                    endif
                end do                                                  ! End of the loop over exchanges
            end do                                                     ! End of the loop over boxes
            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    iseg = iords(i)
                    if (volint(iseg) > 1.0d-25) then
                        do isys = 1, nosys
                            dconc2(isys, iseg) = rhs  (isys, iseg) / volint(iseg)
                        enddo
                    else
                        do isys = 1, nosys
                            dconc2(isys, iseg) = conc (isys, iseg)
                        enddo
                    endif
                enddo
            enddo
            if (timon) call timstop (ithand3)

            !     PART2c: apply the horizontal flux correction for all cells in the boxes of this time step

            if (timon) call timstrt ("flux correction", ithand4)
            do ibox = fbox, nbox, -1
                if1 = itf(ibox + 1) + 1
                if2 = iqsep(ibox)
                do i = if1, if2

                    !                initialisations

                    iq = iordf(i)
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    ifrom_1 = ipoint(3, iq)
                    ito_1 = ipoint(4, iq)
                    if (ifrom <= 0 .and. ito <= 0) cycle
                    if (ifrom == 0 .or.  ito == 0) cycle
                    a = area(iq)
                    q = flow(iq)
                    if (abs(q) < 10.0d-25 .and. disp0q0)  cycle   ! thin dam option, no dispersion at zero flow
                    ipb = 0
                    if (fluxes) then
                        if (iqdmp(iq) > 0) ipb = iqdmp(iq)
                    endif

                    if (iq <= noq1) then
                        e = disp (1)
                        al = aleng(1, 1)
                    else
                        e = disp (2)
                        al = aleng(2, 1)
                    endif
                    if (ilflag == 1) then
                        al = aleng(1, iq) + aleng(2, iq)
                        if (al < 1.0d-25) cycle
                        f1 = aleng(1, iq) / al
                    else
                        f1 = 0.5
                    endif
                    e = e * a / al                             !  constant dispersion in m3/s

                    if (ifrom < 0) then
                        vto = volint(ito)
                        d = 0.0d0
                        if (.not. disp0bnd) d = e
                        if (.not. loword) then
                            f2 = f1
                            if (q < 0.0d0) f2 = f2 - 1.0
                            d = d + min(-f2 * q + 0.5d0 * q * q * dt(ibox) / a / al, 0.0d0)
                        endif
                        d = d * dt(ibox)
                        do isys = 1, nosys
                            dq = d * (bound(isys, -ifrom) - conc(isys, ito))
                            rhs   (isys, ito) = rhs   (isys, ito) + dq
                            dconc2(isys, ito) = dconc2(isys, ito) + dq / vto
                            if (q > 0.0d0) then
                                dq = dq + q * bound(isys, -ifrom) * dt(ibox)
                            else
                                dq = dq + q * conc (isys, ito) * dt(ibox)
                            endif
                            if (dq > 0.0d0) then
                                if (massbal) amass2(isys, 4) = amass2(isys, 4) + dq
                                if (ipb > 0) dmpq  (isys, ipb, 1) = dmpq  (isys, ipb, 1) + dq
                            else
                                if (massbal) amass2(isys, 5) = amass2(isys, 5) - dq
                                if (ipb > 0) dmpq  (isys, ipb, 2) = dmpq  (isys, ipb, 2) - dq
                            endif
                        enddo
                        cycle
                    endif

                    if (ito   < 0) then
                        vfrom = volint(ifrom)
                        d = 0.0d0
                        if (.not. disp0bnd) d = e
                        if (.not. loword) then
                            f2 = f1
                            if (q < 0) f2 = f2 - 1.0d0
                            d = d + min(-f2 * q + 0.5d0 * q * q * dt(ibox) / a / al, 0.0d0)
                        endif
                        d = d * dt(ibox)
                        do isys = 1, nosys
                            dq = d * (conc(isys, ifrom) - bound(isys, -ito))
                            rhs   (isys, ifrom) = rhs   (isys, ifrom) - dq
                            dconc2(isys, ifrom) = dconc2(isys, ifrom) - dq / vfrom
                            if (q > 0.0d0) then
                                dq = dq + q * conc (isys, ifrom) * dt(ibox)
                            else
                                dq = dq + q * bound(isys, -ito) * dt(ibox)
                            endif
                            if (dq > 0.0d0) then
                                if (massbal) amass2(isys, 5) = amass2(isys, 5) + dq
                                if (ipb > 0) dmpq  (isys, ipb, 1) = dmpq  (isys, ipb, 1) + dq
                            else
                                if (massbal) amass2(isys, 4) = amass2(isys, 4) - dq
                                if (ipb > 0) dmpq  (isys, ipb, 2) = dmpq  (isys, ipb, 2) - dq
                            endif
                        enddo
                        cycle
                    endif

                    vfrom = volint(ifrom)
                    vto = volint(ito)
                    f2 = f1
                    if (q < 0.0d0) f2 = f2 - 1.0d0
                    d = e + min(- f2 * q + 0.5d0 * q * q * dt(ibox) / a / al, 0.0d0)
                    d = d * dt(ibox)
                    do isys = 1, nosys
                        if (d < 0.0d0) then
                            e2 = d * (conc(isys, ifrom) - conc(isys, ito))
                            s = sign (1.0d0, e2)
                            select case (ifrom_1)
                            case (1:)
                                cfrm_1 = dconc2(isys, ifrom_1)
                            case (0)
                                if (s > 0) then
                                    cfrm_1 = 0.0d0
                                else
                                    cfrm_1 = 2.0d0 * dconc2(isys, ifrom)
                                endif
                            case (:-1)
                                cfrm_1 = bound (isys, -ifrom_1)
                            end select
                            select case (ito_1)
                            case (1:)
                                cto_1 = dconc2(isys, ito_1)
                            case (0)
                                if (s > 0) then
                                    cto_1 = 2.0 * dconc2(isys, ito)
                                else
                                    cto_1 = 0.0d0
                                endif
                            case (:-1)
                                cto_1 = bound (isys, -ito_1)
                            end select
                            e1 = (dconc2(isys, ifrom) - cfrm_1) * vfrom
                            e3 = (cto_1 - dconc2(isys, ito)) * vto
                            dq = s * max(0.0d0, min(s * e1, s * e2, s * e3))
                        else
                            dq = d * (dconc2(isys, ifrom) - dconc2(isys, ito))
                        endif
                        rhs   (isys, ifrom) = rhs   (isys, ifrom) - dq
                        rhs   (isys, ito) = rhs   (isys, ito) + dq
                        dconc2(isys, ifrom) = dconc2(isys, ifrom) - dq / vfrom
                        dconc2(isys, ito) = dconc2(isys, ito) + dq / vto
                        if (ipb > 0) then
                            if (q > 0.0d0) then
                                dq = dq + q * conc(isys, ifrom) * dt(ibox)
                            else
                                dq = dq + q * conc(isys, ito) * dt(ibox)
                            endif
                            if (dq > 0.0d0) then
                                dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq
                            else
                                dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq
                            endif
                        endif
                    enddo

                end do
            end do
            if (timon) call timstop (ithand4)

            !     PART2c1: Set the vertical advection of water only for all cells in the boxes of this time step

            if (timon) call timstrt ("implicit ver-step", ithand5)
            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    iseg = iords(i)
                    j = nvert(2, iseg)
                    if (j <= 0) cycle                                  ! Do this only for head of columns
                    ih1 = nvert(1, j)
                    if (j < noseg) then
                        ih2 = nvert(1, j + 1)
                    else
                        ih2 = noseg + 1
                    endif
                    if (ih2 == ih1 + 1) then                             ! One cell in the column
                        do isys = 1, nosys
                            rhs(isys, iseg) = dconc2(isys, iseg)
                        enddo
                    else
                        ilay = 0
                        low = 0.0d0  ;  dia = 0.0d0   ;  upr = 0.0d0          ! Span the tridiagonal system for this column
                        do j = ih1, ih2 - 1
                            iseg = ivert(j)
                            volint(iseg) = volint(iseg) - work(3, iseg) + work(1, iseg)    ! Valid for Upwind AND Central (JvG)
                            ilay = ilay + 1
                            if (vertical_upwind) then
                                dia(ilay) = volint(iseg)
                                if (work(1, iseg)>0.0d0) then
                                    low(ilay) = low(ilay) - work(1, iseg)
                                else
                                    dia(ilay) = dia(ilay) - work(1, iseg)
                                endif
                                if (work(3, iseg)>0.0d0) then
                                    dia(ilay) = dia(ilay) + work(3, iseg)
                                else
                                    upr(ilay) = upr(ilay) + work(3, iseg)
                                endif
                            else
                                upr(ilay) = work(3, iseg) / 2.0d0
                                dia(ilay) = volint(iseg) + work(3, iseg) / 2.0d0 - work(1, iseg) / 2.0d0
                                low(ilay) = -work(1, iseg) / 2.0d0
                            endif
                        enddo

                        !              The forward sweep of the double sweep procedure

                        ilay = 0
                        do j = ih1, ih2 - 2
                            iseg = ivert(j)
                            iseg2 = ivert(j + 1)
                            ilay = ilay + 1
                            pivot = low(ilay + 1) / dia(ilay)
                            dia(ilay + 1) = dia(ilay + 1) - pivot * upr(ilay)
                            do isys = 1, nosys
                                rhs(isys, iseg2) = rhs(isys, iseg2) - pivot * rhs(isys, iseg)
                            enddo
                        enddo

                        !              The backward sweep of the double sweep procedure.

                        do j = ih2 - 2, ih1, -1
                            iseg = ivert(j)
                            iseg2 = ivert(j + 1)
                            pivot = upr  (ilay)
                            do isys = 1, nosys
                                rhs(isys, iseg2) = rhs(isys, iseg2) / dia(ilay + 1)
                                rhs(isys, iseg) = rhs(isys, iseg) - pivot * rhs(isys, iseg2)
                            enddo
                            ilay = ilay - 1
                        enddo
                        do isys = 1, nosys
                            rhs(isys, iseg) = rhs(isys, iseg) / dia(1)
                        enddo
                    endif

                    !   The new concentrations are stored and rhs contains the mass of them again

                    do j = ih1, ih2 - 1
                        iseg = ivert(j)
                        do isys = 1, nosys
                            dconc2(isys, iseg) = rhs(isys, iseg)
                            rhs   (isys, iseg) = rhs(isys, iseg) * volint(iseg)
                        enddo
                    enddo
                enddo
            end do
            if (timon) call timstop (ithand5)

            !     PART2c2: apply all withdrawals that were present in the hydrodynamics as negative wasteload rather than as open boundary flux

            if (timon) call timstrt ("massbal", ithand6)
            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    iseg = iords(i)                                          ! cell number
                    if (wdrawal(iseg) == 0.0) cycle
                    q = wdrawal(iseg) * dt(ibox)
                    if (q <= volint(iseg)) then
                        volint(iseg) = volint(iseg) - q
                    else
                        write (file_unit, '(A,i8,E16.7,A,E16.7,A)') 'Warning: trying to withdraw from cell', iseg, &
                                q, ' m3. Available is', volint(iseg), ' m3!'
                        q = volint(iseg)
                        volint(iseg) = 0.0d0
                    endif
                    ipb = isdmp(iseg)
                    do isys = 1, nosys
                        dq = q * dconc2(isys, iseg)
                        rhs (isys, iseg) = rhs(isys, iseg) - dq
                        if (massbal) amass2(isys, 3) = amass2(isys, 3) - dq
                        if (ipb > 0) dmps  (isys, ipb, 3) = dmps  (isys, ipb, 3) + dq
                    enddo
                    do k = 1, nowst
                        if (iseg == iwaste(k)) then
                            do isys = 1, nosys
                                wstdmp(isys, k, 2) = wstdmp(isys, k, 2) + q * dconc2(isys, iseg)
                            enddo
                            exit
                        endif
                    enddo
                enddo
                if (.not. fluxes) cycle
                if1 = iqsep(ibox) + 1                                     ! Reconstruct vertical transport for mass balances
                if2 = itf  (ibox)
                do i = if1, if2
                    iq = iordf(i)
                    ipb = iqdmp(iq)
                    if (ipb == 0) cycle
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    if (ifrom == 0 .or.  ito == 0) cycle
                    if (vertical_upwind) then
                        q = flow(iq) * dt(ibox)                      ! This is the upwind differences version
                        if (q > 0.0) then
                            do isys = 1, nosys
                                dq = q * dconc2(isys, ifrom)
                                dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq
                            enddo
                        else
                            do isys = 1, nosys
                                dq = q * dconc2(isys, ito)
                                dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq
                            enddo
                        endif
                    else
                        q = flow(iq) * dt(ibox) / 2.0d0              ! This is the central differences version
                        if (q > 0.0) then
                            do isys = 1, nosys
                                dq = q * (dconc2(isys, ifrom) + dconc2(isys, ito))
                                dmpq(isys, ipb, 1) = dmpq(isys, ipb, 1) + dq
                            enddo
                        else
                            do isys = 1, nosys
                                dq = q * (dconc2(isys, ifrom) + dconc2(isys, ito))
                                dmpq(isys, ipb, 2) = dmpq(isys, ipb, 2) - dq
                            enddo
                        endif
                    endif
                enddo
            enddo

            !     PART2e: store the final results in the appropriate arrays for next fractional steps

            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    iseg = iords(i)
                    vol = fact * volnew(iseg) + (1.0d0 - fact) * volold(iseg)
                    volint(iseg) = vol
                    if (vol > 1.0d-25) then
                        do isys = 1, nosys
                            rhs (isys, iseg) = rhs(isys, iseg) + deriv(isys, iseg) * dt(ibox)
                            conc(isys, iseg) = rhs(isys, iseg) / vol
                        enddo
                    else
                        do isys = 1, nosys
                            rhs (isys, iseg) = rhs(isys, iseg) + deriv(isys, iseg) * dt(ibox)
                            conc(isys, iseg) = dconc2(isys, iseg)
                        enddo
                    endif
                enddo
            enddo
            if (timon) call timstop (ithand6)

            !        End of loop over fractional time steps

        end do

        if (report .and. (acc_changed > 0.0 .or. acc_remained > 0.0)) then
            write (file_unit, '(a)') 'Averaged over all steps in this iteration:'
            write (file_unit, '(a,2g12.4)') 'Number of segments changed:  ', acc_changed / nstep
            write (file_unit, '(a,2g12.4)') 'Number of segments remained: ', acc_remained / nstep
        endif

        do i = 1, its(nob + 2)                  !  update mass of box of dry cells
            iseg = iords(i)
            do isys = 1, nosys
                rhs(isys, iseg) = rhs(isys, iseg) + deriv(isys, iseg) * idt
            enddo
        enddo

        !     PART3:  set now the implicit step of additional velocities and diffusions per substance in the vertical
        !             There is also an implicit part in the bed if NOQ4 > 0.

        noqv = noq - noqh + noq4
        if (noqv <= 0) goto 9999
        if (timon) call timstrt ("vert.add.fluxes", ithand7)

        !         adjust the vertical distances in the grid

        do ibox = nob + 1, nbox, -1
            if1 = iqsep(ibox) + 1
            if2 = itf  (ibox)
            do i = if1, if2
                iq = iordf(i)
                ifrom = ipoint(1, iq)
                ito = ipoint(2, iq)
                if (ifrom == 0 .or. ito == 0) cycle
                aleng(1, iq) = 0.5 * volnew(ifrom) / surface(ifrom)
                aleng(2, iq) = 0.5 * volnew(ito) / surface(ito)
            enddo
        enddo

        !         Prepare implicit step additional velocities and dispersions, finalize passive substances (dlwq42)

        do iseg = 1, nosss
            vol = volnew(iseg)
            do isys = 1, nosys
                diag (isys, iseg) = vol
                if (iseg > noseg) rhs  (isys, iseg) = amass(isys, iseg) + deriv(isys, iseg) * idt
            enddo
        enddo

        !         Initialisation

        acodia(:, 1:noqv) = 0.0d0
        bcodia(:, 1:noqv) = 0.0d0

        !         Loop over exchanges to fill the matrices

        do iq = noqh + 1, noq + noq4

            !         Initialisations, check for transport anyhow

            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom < 0 .and. ito < 0) cycle
            abound = .false.
            if (ifrom < 0 .or. ito < 0) abound = .true.

            a = area(iq)
            q = 0.0
            e = disp (3)
            al = aleng(1, 2)
            if (ilflag == 1) then
                al = aleng(1, iq) + aleng(2, iq)
            endif
            f1 = 0.5d0
            f2 = 0.5d0
            if (al > 1.0d-25) then
                if (ilflag == 1) then
                    f1 = aleng(2, iq) / al
                    f2 = 1.0d0 - f1
                endif
                dl = a / al
            else
                dl = 0.0d0
            endif
            e = e * dl
            if (iq > noq) e = 0.0d0        !  no constant water diffusion in the bed

            do isys = 1, nosys

                !           advection

                q = 0.0d0
                if (ivpnt(isys) > 0) q = velo  (ivpnt(isys), iq) * a
                if (sw_settling) then         !  additional velocity upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    endif
                else if (iq > noq .or. (abound .and. loword)) then  ! in the bed upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    endif
                else
                    if (vertical_upwind) then
                        if (q > 0.0d0) then  ! upwind
                            q1 = q
                            q2 = 0.0d0
                        else
                            q1 = 0.0d0
                            q2 = q
                        endif
                    else
                        q1 = q * f1                 ! central velocities in the water phase
                        q2 = q * f2
                    endif
                endif

                !           diffusion

                d = e
                if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                if (abound  .and. disp0bnd) d = 0.0d0

                !           fill the tridiag matrix

                q3 = (q1 + d) * idt
                q4 = (q2 - d) * idt
                if (.not. abound) then   ! the regular case
                    diag  (isys, ifrom) = diag  (isys, ifrom) + q3
                    bcodia(isys, iqv) = bcodia(isys, iqv) + q4
                    diag  (isys, ito) = diag  (isys, ito) - q4
                    acodia(isys, iqv) = acodia(isys, iqv) - q3
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

        !    Now make the solution:  loop over vertical exchanges in the water

        do iq = noqh + 1, noq
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            do isys = 1, nosys
                pivot = acodia(isys, iqv) / diag(isys, ifrom)
                diag(isys, ito) = diag(isys, ito) - pivot * bcodia(isys, iqv)
                rhs (isys, ito) = rhs (isys, ito) - pivot * rhs   (isys, ifrom)
            enddo
        enddo

        !    loop over exchanges in the bed

        do iq = noq + 1, noq + noq4
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq + 1, noq + noq4            !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                endif
            enddo                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do isys = 1, nosys
                pivot = acodia(isys, iqv) + acodia(isys, iq3 - noqh)
                pivot = pivot / diag(isys, ifrom)
                rhs (isys, ito) = rhs (isys, ito) - pivot * rhs(isys, ifrom)
                diag(isys, ito) = diag(isys, ito) - pivot * (bcodia(isys, iqv) + bcodia(isys, iq3 - noqh))
            enddo
        enddo

        !    inverse loop over exchanges in the bed

        do iq = noq + noq4, noq + 1, -1
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq - 1, noq + 1, -1          !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                endif
            enddo                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do isys = 1, nosys
                pivot = diag(isys, ito) + tiny(pivot)
                rhs (isys, ito) = rhs(isys, ito) / pivot
                diag(isys, ito) = 1.0
            enddo
            if (ifrom <= 0) cycle
            do isys = 1, nosys
                pivot = bcodia(isys, iqv) + bcodia(isys, iq3 - noqh)
                rhs (isys, ifrom) = rhs (isys, ifrom) - pivot * rhs(isys, ito)
            enddo
        enddo

        !     Inverse loop over exchanges in the water phase

        do iq = noq, noqh + 1, -1
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito   <= 0) cycle
            do isys = 1, nosys
                pivot = diag(isys, ito) + tiny(pivot)
                rhs (isys, ito) = rhs(isys, ito) / pivot
                diag(isys, ito) = 1.0
            enddo
            if (ifrom <= 0) cycle
            do isys = 1, nosys
                pivot = bcodia(isys, iqv)
                rhs (isys, ifrom) = rhs (isys, ifrom) - pivot * rhs(isys, ito)
            enddo
        enddo

        do iseg = 1, nosss       !  for if some diagonal entries are not 1.0
            do isys = 1, nosys
                rhs (isys, iseg) = rhs(isys, iseg) / diag(isys, iseg)
            enddo
        enddo

        !     Mass balances ?

        if (.not. massbal) goto 9998

        do iq = noqh + 1, noq + noq4

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
            e = disp (3)
            al = aleng(1, 2)
            if (ilflag == 1) al = aleng(1, iq) + aleng(2, iq)
            f1 = 0.5
            f2 = 0.5
            if (al > 1.0d-25) then
                if (ilflag == 1) then
                    f1 = aleng(2, iq) / al
                    f2 = 1.0d0 - f1
                endif
                dl = a / al
            else
                dl = 0.0d0
            endif
            e = e * dl
            if (iq > noq) e = 0.0d0      !  no constant water diffusion in the bottom

            do isys = 1, nosys

                !           advection

                q = 0.0d0
                if (ivpnt(isys) > 0) q = velo  (ivpnt(isys), iq) * a
                if (sw_settling) then         !  additional velocity upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    endif
                else if (iq > noq .or. (abound .and. loword)) then  ! in the bed upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    endif
                else
                    if (vertical_upwind) then
                        if (q > 0.0d0) then    ! Upwind
                            q1 = q
                            q2 = 0.0d0
                        else
                            q1 = 0.0d0
                            q2 = q
                        endif
                    else
                        q1 = q * f1                 ! central velocities in the water phase
                        q2 = q * f2
                    endif
                endif

                !           diffusion

                d = e
                if (idpnt(isys) > 0) d = d + disper(idpnt(isys), iq) * dl
                if (abound  .and. disp0bnd) d = 0.0d0

                !           fill the tridiag matrix

                q3 = (q1 + d) * idt
                q4 = (q2 - d) * idt
                if (abound) then
                    if (ito   > 0)  then
                        dq = q3 * bound(isys, -ifrom) + q4 * rhs  (isys, ito)
                        if (dq > 0.0d0) then
                            amass2(isys, 4) = amass2(isys, 4) + dq
                        else
                            amass2(isys, 5) = amass2(isys, 5) - dq
                        endif
                    else
                        dq = q3 * rhs  (isys, ifrom) + q4 * bound(isys, -ito)
                        if (dq > 0.0d0) then
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

        !         take care that rhs of water cells contains the mass again

        9998 do iseg = 1, noseg
            vol = volnew(iseg)
            do isys = 1, nosys
                rhs  (isys, iseg) = rhs (isys, iseg) * vol
            enddo
        enddo

        !         assign the double precisison results to the single precision system arrays
        !                                                          for the bed phase only
        do iseg = noseg + 1, nosss
            vol = volnew(iseg)
            do isys = 1, nosys
                amass(isys, iseg) = rhs (isys, iseg) * vol
                conc (isys, iseg) = rhs (isys, iseg)
            enddo
            do isys = nosys + 1, notot - nototp         ! all passive substances
                amass(isys, iseg) = amass(isys, iseg) + deriv(isys, iseg) * idt
                conc (isys, iseg) = amass(isys, iseg) / surface(iseg)
            enddo
        enddo
        if (timon) call timstop (ithand7)

        !         assign the double precisison results to the single precision system arrays
        !                                                          for the water phase only

        9999 do iseg = 1, noseg
            vol = volnew(iseg)
            if (report) then
                if (abs(vol - volint(iseg)) > 1.0e-6 * max(vol, volint(iseg))) &
                        write (file_unit, '(A,i8,A,e16.7,A,e16.7)') &
                                ' cell: ', iseg, '; computed volume: ', volint(iseg), '; in file: ', vol
            endif
            do isys = 1, nosys
                amass(isys, iseg) = rhs(isys, iseg)
                if (abs(vol) > 1.0d-25) then
                    conc(isys, iseg) = rhs(isys, iseg) / vol
                else
                    conc(isys, iseg) = dconc2(isys, iseg)
                endif
            enddo
            do isys = nosys + 1, notot - nototp         ! all passive substances
                amass(isys, iseg) = amass(isys, iseg) + deriv(isys, iseg) * idt
                conc (isys, iseg) = amass(isys, iseg) / surface(iseg)
            enddo
        enddo
        deriv = 0.0d0

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwq19
