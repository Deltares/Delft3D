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
module m_locally_adaptive_time_step
    use m_waq_precision
    use m_string_utils

    implicit none

contains

    !> Self-adjusting time-step method.
    !! () Per time step it is determined what time step should be set for which computational cell.
    !!   Each cell is assigned the box number of the time step that it should use.
    !! () A separate procedure is applied for flooding cells, since they can have both an inflow and
    !!   an outflow, but may not yet have realistic concentrations. This procedure steps with the
    !!   highest necessary frequency.
    !! () Per box the following steps are set:
    !!   - The horizontal advective transport is set in the mass array with appropriate time step.
    !!   - The vertical water velocities are applied, but implicit and in this case with central
    !!     differences.
    !!   - This results in a first guess for concentrations using upwind horizontal advection.
    !!   - The flux correction step is set for the boxes of this sub-step.
    !! () For the whole area the additional vertical (e.g. settling-) velocities and (space varying)
    !!   vertical diffusion is set using an implicit method and central differences unless specified
    !!   differently.
    !! () The whole routine computes in double precision, but what comes in (concentrations, masses,
    !!   volumes, areas, flows, velocities and diffusions) are still in single precision and also the
    !!   resulting concentrations and masses that are given back to DELWAQ are still in real(4).
    !! () The time step of the bed layer is still the overall time step (typically 1 hour). That may be
    !!   too long. It is possible to have an input variable that specifies a shorter time step
    !!   for the bed underneath all cells only. Please indicate if that is interesting.
    subroutine locally_adaptive_time_step(file_unit, num_substances_transported, num_substances_total, num_substances_part, num_cells, &
            nosss, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges, &
            num_exchanges_bottom_dir, num_dispersion_arrays, num_velocity_arrays, disp, disper, &
            velo, volold, volnew, area, flow, &
            surface, aleng, ipoint, idpnt, ivpnt, &
            amass, conc, dconc2, bound, idt, &
            ibas, ibaf, work, volint, iords, &
            iordf, deriv, wdrawal, iaflag, amass2, &
            ndmpq, num_monitoring_cells, num_waste_loads, iqdmp, dmpq, &
            isdmp, dmps, iwaste, wstdmp, integration_id, &
            ilflag, rhs, diag, acodia, bcodia, &
            nvert, ivert, num_constants, coname, const)

        use m_cli_utils, only: is_command_arg_specified
        use timers

        integer(kind = int_wp), intent(in) :: file_unit               !< Unit number of the monitoring file
        integer(kind = int_wp), intent(in) :: num_substances_transported                   !< Number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                   !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_substances_part                  !< Number of particle substances
        integer(kind = int_wp), intent(in) :: num_cells                   !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: nosss                   !< num_cells + bed-computational volumes
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir                    !< Number of interfaces in direction 1
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir                    !< Number of interfaces in direction 2
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir                    !< Number of interfaces in direction 3
        integer(kind = int_wp), intent(in) :: num_exchanges                     !< Total number of interfaces
        integer(kind = int_wp), intent(in) :: num_exchanges_bottom_dir                    !< Number of interfaces in the bed
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays                  !< Number additional dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays                  !< Number additional velocities
        real(kind = real_wp), intent(in) :: disp(3)                 !< Fixed dispersions in the 3 directions
        real(kind = real_wp), intent(in) :: disper(num_dispersion_arrays, num_exchanges)     !< Additional dispersions
        real(kind = real_wp), intent(in) :: velo(num_velocity_arrays, num_exchanges)       !< Additional velocities
        real(kind = real_wp), intent(in) :: volold(nosss)           !< Volumes of the segments at start of step
        real(kind = real_wp), intent(in) :: volnew(nosss)           !< Volumes of the segments at stop of step
        real(kind = real_wp), intent(in) :: area(num_exchanges)               !< Exchange areas in m2
        real(kind = real_wp), intent(in) :: flow(num_exchanges)               !< Flows through the exchange areas in m3/s
        real(kind = real_wp), intent(in) :: surface(nosss)          !< Horizontal surface area
        real(kind = real_wp), intent(inout) :: aleng(2, num_exchanges)           !< Mixing length to and from the exchange area
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)          !< From, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: idpnt(num_substances_transported)            !< Additional dispersion number per substance
        integer(kind = int_wp), intent(in) :: ivpnt(num_substances_transported)            !< Additional velocity number per substance
        real(kind = real_wp), intent(inout) :: amass(num_substances_total, nosss)     !< Masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, nosss)      !< Concentrations at previous time level
        real(kind = dp), intent(inout) :: dconc2(num_substances_total, nosss)    !< Estimate used in flux correction
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, *)         !< Open boundary concentrations
        integer(kind = int_wp), intent(in) :: idt                     !< Time step in seconds
        integer(kind = int_wp), intent(inout) :: ibas(num_cells)             !< In which basket is my cell
        integer(kind = int_wp), intent(inout) :: ibaf(num_exchanges)               !< In which basket is my flow
        real(kind = dp), intent(inout) :: work(3, num_cells)          !< Work array
        real(kind = dp), intent(inout) :: volint(num_cells)           !< Fractional migrating volume
        integer(kind = int_wp), intent(inout) :: iords(num_cells)            !< Order of segments
        integer(kind = int_wp), intent(inout) :: iordf(num_exchanges)              !< Order of fluxes
        real(kind = real_wp), intent(inout) :: deriv(num_substances_total, nosss)     !< Derivatives of the concentrations
        real(kind = real_wp), intent(inout) :: wdrawal(num_cells)          !< Withdrawals applied to all substances
        integer(kind = int_wp), intent(in) :: iaflag                  !< If 1 then accumulate mass in report array
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)        !< Report array for monitoring file
        integer(kind = int_wp), intent(in) :: ndmpq                   !< Number of dumped exchanges for mass balances
        integer(kind = int_wp), intent(in) :: num_monitoring_cells
        integer(kind = int_wp), intent(in) :: num_waste_loads                   !< Number of wastes
        integer(kind = int_wp), intent(in) :: iqdmp(num_exchanges)              !< Pointer from echange to dump location
        real(kind = real_wp), intent(inout) :: dmpq(num_substances_transported, ndmpq, 2)   !< Array with mass balance information
        integer(kind = int_wp), intent(in) :: isdmp(num_cells)            !< Volume to dump-location pointer
        real(kind = real_wp), intent(inout) :: dmps(num_substances_total, num_monitoring_cells, *)   !< Dumped segment fluxes if integration_id > 7
        integer(kind = int_wp), intent(in) :: iwaste(num_waste_loads)           !< Volume numbers of the waste locations
        real(kind = real_wp), intent(inout) :: wstdmp(num_substances_total, num_waste_loads, 2) !< Accumulated wasteloads 1/2 in and out
        integer(kind = int_wp), intent(in) :: integration_id          !< Integration
        integer(kind = int_wp), intent(in) :: ilflag                  !< If 0 then only 3 constant lenght values
        real(kind = dp), intent(inout) :: rhs(num_substances_total, nosss)       !< Local right hand side
        real(kind = dp), intent(inout) :: diag(num_substances_total, nosss)      !< Local diagonal filled with volumes
        real(kind = dp), intent(inout) :: acodia(num_substances_total, max(num_exchanges_z_dir + num_exchanges_bottom_dir, 1)) !< Local work array lower codiagonal
        real(kind = dp), intent(inout) :: bcodia(num_substances_total, max(num_exchanges_z_dir + num_exchanges_bottom_dir, 1)) !< Local work array upper codiagonal
        integer(kind = int_wp), intent(inout) :: nvert(2, num_cells)         !< Number of vertical cells per column, entry point in ivert
        integer(kind = int_wp), intent(inout) :: ivert(num_cells)            !< Number of vertical columns
        integer(kind = int_wp), intent(in) :: num_constants                  !< Number of constants used
        character(20), intent(in) :: coname(num_constants)          !< Constant names
        real(kind = real_wp), intent(in) :: const(num_constants)           !< Constants

        ! Local variables
        integer(kind = int_wp) :: i, j, k                     !< General loop counter
        integer(kind = int_wp) :: noqh                        !< Total number of horizontal interfaces
        integer(kind = int_wp) :: noqv                        !< Total number of vertical interfaces in the water
        integer(kind = int_wp) :: iq                          !< Loop counter exchanges
        integer(kind = int_wp) :: iq2, iq3                    !< Help variables to identify first or second pointers
        integer(kind = int_wp) :: iqv                         !< Help variables in vertical arrays
        integer(kind = int_wp) :: substance_i                        !< Loop counter substance
        integer(kind = int_wp) :: cell_i, iseg2                 !< Loopcounter computational volumes
        integer(kind = int_wp) :: ifrom, ito                  !< From and to volume numbers
        real(kind = dp) :: vfrom, vto                         !< From and to   volumes
        integer(kind = int_wp) :: ifrom_1, ito_1              !< From-1 and to+1 volume numbers
        real(kind = dp) :: cfrm_1, cto_1                      !< From-1 and to+1 concentration values
        integer(kind = int_wp) :: ipb                         !< Pointer in the mass balance dump array
        integer(kind = int_wp) :: iqd                         !< Help variable for dump pointers
        real(kind = dp) :: a                                  !< This area
        real(kind = dp) :: q                                  !< Flow for this exchange
        real(kind = dp) :: e                                  !< Didpersion for this exchange
        real(kind = dp) :: al                                 !< This length
        real(kind = dp) :: dl                                 !< Area / length
        real(kind = dp) :: d                                  !< Didpersion for this substance
        real(kind = dp) :: dq                                 !< Total flux from and to
        real(kind = dp) :: pivot                              !< Help variable matrix inversion
        real(kind = dp) :: vol                                !< Helpvariable for this volume
        real(kind = dp) :: e1, e2, e3                         !< Limiter help variable
        real(kind = dp) :: s                                  !< Limiter sign variable
        real(kind = dp) :: f1, f2                             !< Correction factors central differences
        real(kind = dp) :: q1, q2, q3, q4                     !< Helpvariables to fill the matrix
        logical disp0q0                                     !< Bit zero  of integration_id: 1 if no dispersion at zero flow
        logical disp0bnd                                    !< Bit one   of integration_id: 1 if no dispersion across boundaries
        logical loword                                      !< Bit two   of integration_id: 1 if lower order across boundaries
        logical fluxes                                      !< Bit three of integration_id: 1 if mass balance output
        logical abound                                      !< Is it a boundary?
        logical wetting                                     !< Are cells becoming wet?
        logical, save :: sw_settling                        !< Should settling be dealt with upwind?
        integer(kind = int_wp), save :: init = 0              !< First call ?
        integer(kind = int_wp), save :: nob                   !< Number of baskets for transportables
        integer(kind = int_wp), allocatable, save :: its(:)   !< Baskets accumulator cells
        integer(kind = int_wp), allocatable, save :: itf(:)   !< Baskets accumulator flows    , nob+2 stays dry
        real(kind = dp), allocatable, save :: dt(:)           !< Delta time value of baskets  , nob+1 becomes wet
        integer(kind = int_wp), allocatable, save :: iqsep(:) !< Separation point flows in 3rd direction
        integer(kind = int_wp), save :: nosegl                !< Number of cells per layer
        integer(kind = int_wp) :: isums, isumf                !< Accumulators
        integer(kind = int_wp) :: ibox, nb                    !< Help variable for boxes
        integer(kind = int_wp) :: iofs, ioff                  !< Offsets in the arrays
        integer(kind = int_wp) :: fbox, lbox, nbox            !< Box range
        real(kind = dp) :: fact                               !< Interpolation factor for volumes
        integer(kind = int_wp) :: istep, nstep                !< Fractional step variables
        integer(kind = int_wp) :: is1, is2, if1, if2          !< Loop variables per box
        integer(kind = int_wp) :: ih1, ih2                    !< Help variables parallellism
        integer(kind = int_wp) :: ilay                        !< Loop counter layers
        integer(kind = int_wp) :: maxlay                      !< Maximum number of layers observed in this model
        integer(kind = int_wp) :: bmax                        !< Maximum box number in a column
        integer(kind = int_wp) :: changed, remained, iter     !< Flooding help variables
        real(kind = dp), allocatable, save :: low(:), dia(:), upr(:) !< Matrix of one column
        logical massbal                                     !< Set .true. if iaflag eq 1
        logical, save :: report                             !< Write iteation reports in monitoring file
        real(kind = real_wp) :: acc_remained, acc_changed     !< For reporting: accumulated/averaged reporting parameters
        logical :: vertical_upwind                          !< Set .true. for upwind scheme in the vertical
        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp) :: ithand1 = 0
        integer(kind = int_wp) :: ithand2 = 0
        integer(kind = int_wp) :: ithand3 = 0
        integer(kind = int_wp) :: ithand4 = 0
        integer(kind = int_wp) :: ithand5 = 0
        integer(kind = int_wp) :: ithand6 = 0
        integer(kind = int_wp) :: ithand7 = 0
        if (timon) call timstrt("locally_adaptive_time_step", ithandl)

        ! Initialisations
        if (timon) call timstrt("administration", ithand1)
        noqh = num_exchanges_u_dir + num_exchanges_v_dir
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
            end if

            sw_settling = is_command_arg_specified('-settling_backwards')
            if (sw_settling) write (file_unit, *) ' option -settling_backwards found'
            i = index_in_array('Number_of_baskets   ', coname)
            if (i > 0) then
                nob = const(i)
                write (file_unit, '(A,i3)') ' Number of baskets         : ', nob
            else
                nob = 13
                write (file_unit, '(A,i3)') ' Default number of baskets : ', nob
            end if
            allocate (its(nob + 2), itf(nob + 2), iqsep(nob + 2), dt(nob + 1))
            report = .false.
            i = index_in_array('Iteration report    ', coname)
            if (i > 0) then
                report = const(i) > 0
            end if
            if (report) then
                write (file_unit, '(A)') ' Iteration report          : switched on'
            else
                write (file_unit, '(A)') ' Iteration report          : switched off'
            end if

            if (.not. disp0q0) then
                write (file_unit, '(/3A)') &
                        ' WARNING: Dispersion allowed if flow rate is zero', &
                        '          This is known to cause problems in some cases'
            end if

            if (num_exchanges_z_dir == 0) then         ! vertically integrated model
                do cell_i = 1, num_cells
                    nvert(1, cell_i) = cell_i
                    nvert(2, cell_i) = cell_i
                    ivert(cell_i) = cell_i
                end do
                nosegl = num_cells
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
                    end if
                    if (ito > 0) then
                        nvert(1, ito) = 0
                        nvert(2, ito) = 0
                    end if
                end do
                do iq = noqh + 1, num_exchanges                           !  Make the vertical administration
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    if (ifrom <= 0 .or. ito <= 0) cycle
                    nvert(1, ifrom) = ito                       !  this is the one cell below 'ifrom'
                    nvert(2, ito) = ifrom                     !  this is the one cell above 'ito'
                end do
                ioff = 0
                do cell_i = 1, num_cells
                    if (nvert(2, cell_i) == 0) then           !  this cell starts a column (has no 'ifrom')
                        ioff = ioff + 1
                        nvert(2, cell_i) = ioff                  !  column starts at ioff in ivert
                        ivert(ioff) = cell_i
                        i = nvert(1, cell_i)                       !  this is the cell below
                        do while (i > 0)
                            ioff = ioff + 1
                            ivert(ioff) = i
                            i = nvert(1, i)
                        end do
                    else
                        nvert(2, cell_i) = 0
                    end if
                end do
                nosegl = 0
                do cell_i = 1, num_cells
                    if (nvert(2, cell_i) > 0) then
                        nosegl = nosegl + 1
                        nvert(1, nosegl) = nvert(2, cell_i)         !  to find head of column
                        nvert(2, cell_i) = nosegl                !  point to column number
                    end if
                end do
                if (nosegl < num_cells) nvert(1, nosegl + 1) = ioff + 1
                write (file_unit, '(A,i8,A)') ' This model has            : ', nosegl, ' columns of cells'
                maxlay = 0
                do i = 1, nosegl
                    is1 = nvert(1, i)                         !  offset of the cell that heads the column in ivert table
                    if (i < num_cells) then
                        is2 = nvert(1, i + 1)                      !  offset of the cell that heads next column
                    else
                        is2 = num_cells + 1
                    end if
                    maxlay = max(maxlay, is2 - is1)
                    do j = is1 + 1, is2 - 1
                        cell_i = ivert(j)
                        nvert(2, cell_i) = -i                      !  for non-head of column cells point to minus the column #
                    end do
                end do
                allocate (low(maxlay), dia(maxlay), upr(maxlay))
                write (file_unit, '(A,i4,A)') ' This model has at most    : ', maxlay, ' layers'
            end if
            !    after this all: ivert(1:num_cells)     contains all water cell numbers in their order of appearance in the columns
            !                    nvert(1,1:nosegl)  contains the start locations in ivert of columns 1:nosegl
            !                    nvert(1,nosegl+1)  contains the start location of the non existing column nosegl+1
            !                    nvert(2,1:num_cells)   contains the column number of each cell, negative if not head of column
            !    the procedure works for any cell numbering if: the columns all are 1D-vertical so all 1-cell wide stacks
            !                                                   the vertical exchanges run from num_exchanges_u_dir+num_exchanges_v_dir+1 to num_exchanges_u_dir+num_exchanges_v_dir+num_exchanges_z_dir
            !                                                   the positive velocity or flow is from ipoint(1,iq) to ipoint(2,iq)
            !    it is easily seen that for 2D-horizontal models ivert and nvert(1:2,*) just contain the sequential cell numbers and
            !                    nosegl = num_cells. Since nvert(1,num_cells+1) is out of range, you will find statements that deal with this.
            write (file_unit, '(A)') ' '
            init = 1    !   do this only once per simulation
        end if

        ! PART 1 : make the administration for the variable time step approach
        !   1a: fill the array with time-tresholds per basket, 13 baskets span 1 hour - 0.9 second

        dt(1) = real(idt, kind = dp)
        do ibox = 2, nob
            dt(ibox) = dt(ibox - 1) / 2.0d0
        end do

        !   1b: sum the outgoing (1,..) and ingoing (2,..) horizontal flows and constant diffusions (3,..) per cell

        work = 0.0d0
        d = disp(1)
        al = aleng(1, 1)
        do iq = 1, num_exchanges

            ! Note: If the model uses an unstructured grid, num_exchanges_v_dir may be zero, so noqh == num_exchanges_u_dir.
            ! Therefore first check for the vertical direction, then for the second
            ! horizontal direction
            if (iq == noqh + 1) then
                d = 0.0d0
                al = aleng(1, 2)
            elseif (iq == num_exchanges_u_dir + 1) then
                d = disp(2)
                al = aleng(2, 1)
            end if
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom < 0 .and. ito < 0) cycle
            a = area(iq)
            q = flow(iq)
            if (ilflag == 1) al = aleng(1, iq) + aleng(2, iq)
            e = d * a / al
            if (ifrom < 0) then ! Boundary
                if (flow(iq) > 0.0d0) then
                    work(2, ito) = work(2, ito) + flow(iq)
                else
                    work(1, ito) = work(1, ito) - flow(iq)
                end if
                if (.not. disp0bnd) then
                    if (flow(iq) /= 0.0 .or. .not. disp0q0) then
                        work(3, ito) = work(3, ito) + e
                    end if
                end if
                cycle
            end if
            if (ito < 0) then ! Boundary
                if (flow(iq) > 0.0) then
                    work(1, ifrom) = work(1, ifrom) + flow(iq)
                else
                    work(2, ifrom) = work(2, ifrom) - flow(iq)
                end if
                if (.not. disp0bnd) then
                    if (flow(iq) /= 0.0 .or. .not. disp0q0) then
                        work(3, ifrom) = work(3, ifrom) + e
                    end if
                end if
                cycle
            end if ! Internal
            if (flow(iq) > 0.0) then
                if (ifrom > 0) work(1, ifrom) = work(1, ifrom) + flow(iq)
                if (ito > 0) work(2, ito) = work(2, ito) + flow(iq)
            else
                if (ifrom > 0) work(2, ifrom) = work(2, ifrom) - flow(iq)
                if (ito > 0) work(1, ito) = work(1, ito) - flow(iq)
            end if
            if (flow(iq) /= 0.0 .or. .not. disp0q0) then
                work(3, ifrom) = work(3, ifrom) + e
                work(3, ito) = work(3, ito) + e
            end if
        end do
        ! Add withdrawals to outgoing (1,..) too
        do cell_i = 1, num_cells
            work(1, cell_i) = work(1, cell_i) + wdrawal(cell_i)
        end do

        !   1c: assign a basket number to each cell
        ibas = 0
        wetting = .false.
        do cell_i = 1, num_cells
            if (work(1, cell_i) <= 0.0d0 .and. &
                    work(2, cell_i) <= 0.0d0 .and. &
                    work(3, cell_i) <= 0.0d0) then
                ibas(cell_i) = nob + 2 !  cell is dry, number is 1 higher than
                cycle !  the nr of wet and 'wetting' basket
            end if
            if ((work(1, cell_i) + work(3, cell_i)) * dt(1) < volold(cell_i)) then    !  box 1 works even if volnew(cell_i) is zero
                ibas(cell_i) = 1
                cycle
            end if
            if (volnew(cell_i) >= volold(cell_i)) then          !  test only with volold(cell_i) to determine fractional step
                do ibox = 2, nob
                    if ((work(1, cell_i) + work(3, cell_i)) * dt(ibox) < volold(cell_i)) then
                        ibas(cell_i) = ibox        !  this cell in the basket of this dt(ibox)
                        exit
                    end if
                    if (ibox == nob) then   !  no suitable time step in range
                        ibas(cell_i) = nob + 1       !  so cell is considered becoming wet
                        wetting = .true.      !  by simultaneous inflow: 'wetting' basket.
                    end if
                end do
            else !  also the last fractional step should be stable
                do ibox = 2, nob
                    if ((work(1, cell_i) + work(3, cell_i) - (volold(cell_i) - volnew(cell_i)) / dt(1)) * dt(ibox) < volnew(cell_i)) then
                        ibas(cell_i) = ibox        !  this cell in the basket of this dt(ibox)
                        exit
                    end if
                    if (ibox == nob) then   !  no suitable time step in range
                        ibas(cell_i) = nob + 1       !  so cell is considered becoming dry
                        wetting = .true.      !  by simultaneous inflow: 'wetting' basket.
                    end if
                end do
            end if
        end do

        !   1d: give each cell of a column the highest basket nr. of the column
        do i = 1, nosegl
            is1 = nvert(1, i)
            if (i < num_cells) then
                is2 = nvert(1, i + 1)
            else
                is2 = num_cells + 1
            end if
            bmax = 0
            do j = is1, is2 - 1
                cell_i = ivert(j)
                if (ibas(cell_i) <= nob + 1) then
                    bmax = max(bmax, ibas(cell_i))
                end if
            end do
            if (bmax == 0) cycle
            do j = is1, is2 - 1
                cell_i = ivert(j)
                if (ibas(cell_i) <= nob + 1) then
                    ibas(cell_i) = bmax
                end if
            end do
        end do
        if (wetting .and. report) then
            if (nosegl == num_cells) then
                write (file_unit, '(/A/A)') &
                        ' WARNING in locally_adaptive_time_step, next cells are becoming wet or dry:', &
                        '  cell       outflow         inflow          diffusion       volume-1        volume-2'
            else
                write (file_unit, '(/A/A)') &
                        ' WARNING in locally_adaptive_time_step, next cells and the cells underneith are becoming wet or dry:', &
                        '  cell       outflow         inflow          diffusion       volume-1        volume-2'
            end if
            do i = 1, nosegl
                cell_i = ivert(nvert(1, i))
                if (ibas(cell_i) == nob + 1) write (file_unit, '(i10,5e16.7)') &
                        cell_i, work(1, cell_i), work(2, cell_i), work(3, cell_i), volold(cell_i), volnew(cell_i)
            end do
        end if

        !   1e: count the size of the baskets for segments
        its = 0
        do cell_i = 1, num_cells
            its(ibas(cell_i)) = its(ibas(cell_i)) + 1
        end do

        !   1f: determine size of the baskets for fluxes (highest of 'from' and 'to')
        itf = 0
        ibaf = 0
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ibox = 0
            if (ifrom > 0) ibox = ibas(ifrom)
            if (ito > 0) ibox = max(ibox, ibas(ito))
            if (ibox == 0) ibox = nob + 2
            ibaf(iq) = ibox
            itf(ibox) = itf(ibox) + 1
        end do

        ! 1g: write report on basket sizes
        if (report) then
            write (file_unit, *) ' box       cells    fluxes'
            isums = 0
            isumf = 0
            do ibox = 1, nob + 2
                write (file_unit, '(i5,2x,2i10)') ibox, its(ibox), itf(ibox)
                if (ibox == nob) write (file_unit, '(A)') ' '
                isums = isums + its(ibox)
                isumf = isumf + itf(ibox)
            end do
            write (file_unit, '(/a,2i9)') 'Total number of cells & fluxes: ', isums, isumf
        end if

        ! 1h: determine execution order of the segments and fluxes
        iofs = 0
        ioff = 0
        do ibox = nob + 2, 1, -1         ! start with highest frequency
            do cell_i = 1, num_cells            ! array of segments in this basket
                if (ibas(cell_i) == ibox) then
                    iofs = iofs + 1
                    iords(iofs) = cell_i
                end if
            end do
            do iq = 1, noqh               ! array of horizontal fluxes in this basket
                if (ibaf(iq) == ibox) then
                    ioff = ioff + 1
                    iordf(ioff) = iq
                end if
            end do
            iqsep(ibox) = ioff            ! now the vertical fluxes start
            do iq = noqh + 1, num_exchanges           ! array of the vertical fluxes
                if (ibaf(iq) == ibox) then
                    ioff = ioff + 1
                    iordf(ioff) = iq
                end if
            end do
        end do
        do ibox = 1, nob                 ! lowest active box number (largest time step)
            if (its(ibox) > 0) then  !                          (last box to evaluate)
                lbox = ibox
                exit
            end if
        end do
        do ibox = nob, 1, -1             ! highest active box number (smallest time step)
            if (its(ibox) > 0) then  !                           (first box to evaluate)
                fbox = ibox
                exit
            end if
        end do
        do ibox = nob + 1, 1, -1           ! accumulate the counts in reversed order
            its(ibox) = its(ibox) + its(ibox + 1)
            itf(ibox) = itf(ibox) + itf(ibox + 1)
        end do
        nb = fbox - lbox + 1          ! so many boxes are used
        nstep = 1
        do ibox = 2, fbox
            nstep = nstep * 2               ! so many sub-time steps will be set
        end do
        if (report) then
            write (file_unit, '(a,i2,A,i2,A,i2)') 'Nr of boxes: ', nb, ',first: ', lbox, ', last: ', fbox
            write (file_unit, '(a,e15.7/)') 'Smallest time step in sec.: ', dt(fbox)
        end if
        dt(nob + 1) = dt(fbox)             ! boxes running wet do so with the smallest step size

        !   1i: Create backpointers from cell to order of execution and to box nr.

        !   The backpointer became obsolete, ibas can be reused directly

        !   1j: Fill the off-diagonals of the matrix for the vertical advection of water only
        !    (Note that the variable work is reused with a different meaning (JvG 2016)
        work = 0.0
        do ibox = nob, lbox, -1         ! Fill the off-diagonals only once per time step
            if1 = iqsep(ibox) + 1
            if2 = itf(ibox)
            do i = if1, if2
                iq = iordf(i)
                ifrom = ipoint(1, iq)               !  The diagonal now is the sum of the
                ito = ipoint(2, iq)               !  new volume that increments with each step
                if (ifrom == 0 .or. ito == 0) cycle
                q = flow(iq) * dt(ibox)
                work(3, ifrom) = q                 ! flow through lower surface (central or upwind now arranged in one spot, further down)
                work(1, ito) = q                 ! flow through upper surface
            end do
        end do
        if (timon) call timstop(ithand1)

        ! PART2: set the fractional step loop for this time step
        do cell_i = 1, num_cells
            do substance_i = 1, num_substances_transported
                dconc2(substance_i, cell_i) = conc(substance_i, cell_i)      ! Initialize dconc2. Becomes new estimate
                rhs(substance_i, cell_i) = amass(substance_i, cell_i)
            end do
        end do

        acc_remained = 0.0
        acc_changed = 0.0
        volint = volold                                 ! Initialize volint. Becomes the volume 'in between'.
        do istep = 1, nstep                         ! Big loop over the substeps
            fact = real(istep) / real(nstep, kind = dp)         ! Interpolation factor of this step
            ! istep:  boxes to integrate:           modulo logic
            !         last boxe to integrate for this sub step           !   1     fbox
            !   2     fbox, fbox-1                  mod(2    ) = 0
            nbox = fbox                                         !   3     fbox
            ioff = 1                                            !   4     fbox, fbox-1, fbox-2          mod(2&4  ) = 0
            do ibox = 1, nb - 1                                   !   5     fbox
                ioff = ioff * 2                                    !   6     fbox, fbox-1                  mod(2    ) = 0
                if (mod(istep, ioff) == 0) nbox = nbox - 1      !   7     fbox
            end do                                               !   8     fbox, fbox-1, fbox-2, fbox-3  mod(2&4&8) = 0
            !  etc.
            !  PART2a: deal with those cells that are running wet

            if (timon) call timstrt("flooding", ithand2)   !  'flooding' is evaluated with highest frequency
            if1 = itf(nob + 2) + 1                                  !  loop limiters for fluxes in this box
            if2 = iqsep(nob + 1)
            is1 = its(nob + 2) + 1                                  !  loop limiters for cells in this box
            is2 = its(nob + 1)

            !  PART2a1: sum the mass and volume vertically in the first layer and
            !  make the column averaged concentrations
            do i = is1, is2
                cell_i = iords(i)
                j = nvert(2, cell_i)                      ! column number if cell_i = head of column
                if (j <= 0) cycle                     !    zero if not head of column
                ih1 = nvert(1, j)                          ! pointer to first cell of this column
                if (j < num_cells) then
                    ih2 = nvert(1, j + 1)                     ! pointer to first cell of next column
                else
                    ih2 = num_cells + 1                          ! or to just over the edge if j = last column
                end if
                do j = ih1 + 1, ih2 - 1
                    iseg2 = ivert(j)                                       ! cell number of this cell in column
                    volint(cell_i) = volint(cell_i) + volint(iseg2)            ! sum volumes to volumes of 'head of column'
                    do substance_i = 1, num_substances_transported
                        rhs(substance_i, cell_i) = rhs(substance_i, cell_i) + rhs(substance_i, iseg2)   ! sum masses  to masses  of 'head of column'
                    end do
                end do
            end do
            do i = is1, is2
                cell_i = iords(i)
                j = nvert(2, cell_i)
                if (j <= 0) cycle
                if (abs(volint(cell_i)) > 1.0d-25) then
                    do substance_i = 1, num_substances_transported
                        conc(substance_i, cell_i) = rhs(substance_i, cell_i) / volint(cell_i)      ! column averaged concentrations
                    end do
                else                                                      ! dry
                    do substance_i = 1, num_substances_transported
                        rhs(substance_i, cell_i) = 0.0d0
                        conc(substance_i, cell_i) = 0.0d0
                    end do
                end if
            end do

            ! PART2a1: apply all influxes to the cells first
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
                    end if
                    if (ifrom < 0) then
                        if (q > 0.0d0) then
                            ito = ivert(nvert(1, abs(nvert(2, ito))))   ! cell-nr at offset of head of collumn in ivert
                            volint(ito) = volint(ito) + q
                            do substance_i = 1, num_substances_transported
                                dq = q * bound(substance_i, -ifrom)
                                rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                                conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                                if (massbal) amass2(substance_i, 4) = amass2(substance_i, 4) + dq
                                if (ipb > 0) dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                            end do
                            iordf(i) = -iordf(i)                             ! this flux is resolved now
                            changed = changed + 1                          ! nr. of handled fluxes
                        end if
                        cycle
                    end if
                    if (ito < 0) then
                        if (q < 0.0d0) then
                            ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                            volint(ifrom) = volint(ifrom) - q
                            do substance_i = 1, num_substances_transported
                                dq = q * bound(substance_i, -ito)
                                rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                                conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                                if (massbal) amass2(substance_i, 4) = amass2(substance_i, 4) - dq
                                if (ipb > 0) dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                            end do
                            iordf(i) = -iordf(i)
                            changed = changed + 1
                        end if
                        cycle
                    end if
                    if (q > 0) then                                   ! Internal volumes
                        if (ibas(ito) == nob + 1) then                  !    'to' should be wetting if q > 0
                            if (ibas(ifrom) == nob + 1) then               !       'from' is also wetting in this time step
                                ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                                ito = ivert(nvert(1, abs(nvert(2, ito))))
                                if (volint(ifrom) >= q) then             !          it should then have enough volume
                                    volint(ifrom) = volint(ifrom) - q
                                    volint(ito) = volint(ito) + q
                                    do substance_i = 1, num_substances_transported
                                        dq = q * conc(substance_i, ifrom)
                                        rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                                        rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                                        if (volint(ifrom) > 1.0d-25) conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                                        conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                                        if (ipb > 0) dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                                    end do
                                    iordf(i) = -iordf(i)
                                    changed = changed + 1
                                else
                                    remained = remained + 1                   !          flux not resolved yet by lack of volume
                                end if
                            else                                             !       'from' is not 'wetting' so it has enough volume
                                ito = ivert(nvert(1, abs(nvert(2, ito))))
                                volint(ifrom) = volint(ifrom) - q
                                volint(ito) = volint(ito) + q
                                do substance_i = 1, num_substances_transported
                                    dq = q * conc(substance_i, ifrom)
                                    rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                                    rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                                    if (volint(ifrom) > 1.0d-25) conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                                    conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                                    if (ipb > 0) dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                                end do
                                iordf(i) = -iordf(i)
                                changed = changed + 1
                            end if
                        end if
                    else                                                   ! same procedure but now mirrorred for q < 0
                        if (ibas(ifrom) == nob + 1) then
                            if (ibas(ito) == nob + 1) then
                                ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                                ito = ivert(nvert(1, abs(nvert(2, ito))))
                                if (volint(ito) > -q) then
                                    volint(ifrom) = volint(ifrom) - q
                                    volint(ito) = volint(ito) + q
                                    do substance_i = 1, num_substances_transported
                                        dq = q * conc(substance_i, ito)
                                        rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                                        rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                                        conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                                        if (volint(ito) > 1.0d-25) conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                                        if (ipb > 0) dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                                    end do
                                    iordf(i) = -iordf(i)
                                    changed = changed + 1
                                else
                                    remained = remained + 1
                                end if
                            else
                                ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                                volint(ifrom) = volint(ifrom) - q
                                volint(ito) = volint(ito) + q
                                do substance_i = 1, num_substances_transported
                                    dq = q * conc(substance_i, ito)
                                    rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                                    rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                                    conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                                    if (volint(ito) > 1.0d-25) conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                                    if (ipb > 0) dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                                end do
                                iordf(i) = -iordf(i)
                                changed = changed + 1
                            end if
                        end if
                    end if
                end do
                if (changed /= 0 .or. remained /= 0) then
                    acc_remained = acc_remained + remained
                    acc_changed = acc_changed + changed
                    if (remained > 0 .and. changed == 0) then
                        if (report) then
                            write (file_unit, *) 'Warning: No further progress in the wetting procedure!'
                        end if
                        exit
                    end if
                end if
            end do

            ! PART2a2: apply all outfluxes to the outer world from 
            ! these cells that should have reasonable concentrations
            ! and enough volume now
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
                end if
                if (ifrom < 0) then                                  ! The 'from' element was a boundary.
                    if (q < 0.0d0) then
                        ito = ivert(nvert(1, abs(nvert(2, ito))))
                        volint(ito) = volint(ito) + q
                        do substance_i = 1, num_substances_transported
                            dq = q * conc(substance_i, ito)
                            rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                            if (volint(ito) > 1.0d-25) conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                            if (massbal) amass2(substance_i, 5) = amass2(substance_i, 5) - dq
                            if (ipb > 0) dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                        end do
                    end if
                    cycle
                end if
                if (ito < 0) then                                  ! The 'to'   element was a boundary.
                    if (q > 0.0d0) then
                        ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))
                        volint(ifrom) = volint(ifrom) - q
                        do substance_i = 1, num_substances_transported
                            dq = q * conc(substance_i, ifrom)
                            rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                            if (volint(ifrom) > 1.0d-25) conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                            if (massbal) amass2(substance_i, 5) = amass2(substance_i, 5) + dq
                            if (ipb > 0) dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                        end do
                    end if
                    cycle
                end if
                if (q > 0) then                                      ! The normal case
                    ifrom = ivert(nvert(1, abs(nvert(2, ifrom))))         !    'from' should be wetting if q > 0
                    volint(ifrom) = volint(ifrom) - q
                    volint(ito) = volint(ito) + q
                    do substance_i = 1, num_substances_transported
                        dq = q * conc(substance_i, ifrom)
                        rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                        rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                        if (volint(ifrom) > 1.0d-25) conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                        conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                        if (ipb > 0) dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                    end do
                else                                                      ! The mirrorred case
                    ito = ivert(nvert(1, abs(nvert(2, ito))))         !    'to' should be wetting if q < 0
                    volint(ifrom) = volint(ifrom) - q
                    volint(ito) = volint(ito) + q
                    do substance_i = 1, num_substances_transported
                        dq = q * conc(substance_i, ito)
                        rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                        rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                        conc(substance_i, ifrom) = rhs(substance_i, ifrom) / volint(ifrom)
                        if (volint(ito) > 1.0d-25) conc(substance_i, ito) = rhs(substance_i, ito) / volint(ito)
                        if (ipb > 0) dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                    end do
                end if
            end do
            do i = if1, if2                                             ! All fluxes of the 'wetting-group' should have been resolved
                iordf(i) = abs(iordf(i))                                  ! Reset the flux pointer to its positive value
            end do

            ! PART2a3: apply all withdrawals that were present in the hydrodynamics as negative wasteload rather than as open boundary flux
            do i = is1, is2
                iseg2 = iords(i)                                          ! cell number
                if (wdrawal(iseg2) == 0.0) cycle
                q = wdrawal(iseg2) * dt(fbox)
                cell_i = ivert(nvert(1, abs(nvert(2, iseg2))))             ! cell number of head of column
                if (q <= volint(cell_i)) then
                    volint(cell_i) = volint(cell_i) - q
                else
                    write (file_unit, '(A,i8,E16.7,A,E16.7,A)') 'Warning: trying to withdraw from cell', iseg2, q, &
                            ' m3. Available is', volint(cell_i), ' m3!'
                    q = volint(cell_i)
                    volint(cell_i) = 0.0d0
                end if
                ipb = isdmp(iseg2)
                do substance_i = 1, num_substances_transported
                    dq = q * conc(substance_i, cell_i)
                    rhs(substance_i, cell_i) = rhs(substance_i, cell_i) - dq
                    if (massbal) amass2(substance_i, 3) = amass2(substance_i, 3) - dq
                    if (ipb > 0) dmps(substance_i, ipb, 3) = dmps(substance_i, ipb, 3) + dq
                end do
                do k = 1, num_waste_loads
                    if (iseg2 == iwaste(k)) then
                        do substance_i = 1, num_substances_transported
                            wstdmp(substance_i, k, 2) = wstdmp(substance_i, k, 2) + q * conc(substance_i, cell_i)
                        end do
                        exit
                    end if
                end do
            end do

            ! PART2a4: expand the depth averaged result to all layers for this group of cells
            do i = is1, is2
                cell_i = iords(i)
                j = nvert(2, cell_i)
                if (j > 0) then                                      ! this is head of column
                    ih1 = nvert(1, j)
                    if (j < num_cells) then
                        ih2 = nvert(1, j + 1)
                    else
                        ih2 = num_cells + 1
                    end if
                    vol = 0.0d0                                            ! determine new integrated volume in the flow-file
                    do j = ih1, ih2 - 1
                        iseg2 = ivert(j)
                        vol = vol + fact * volnew(iseg2) + (1.0d0 - fact) * volold(iseg2)
                        do substance_i = 1, num_substances_transported                                  !    apply the derivatives (also wasteloads)
                            rhs(substance_i, cell_i) = rhs(substance_i, cell_i) + deriv(substance_i, iseg2) * dt(fbox)
                        end do
                    end do
                    if (vol > 1.0d-25) then
                        do substance_i = 1, num_substances_transported                                  !    the new concentrations
                            conc(substance_i, cell_i) = rhs(substance_i, cell_i) / vol
                        end do
                    end if
                    do j = ih1, ih2 - 1
                        iseg2 = ivert(j)
                        f1 = fact * volnew(iseg2) + (1.0d0 - fact) * volold(iseg2)
                        volint(iseg2) = f1
                        do substance_i = 1, num_substances_transported
                            conc(substance_i, iseg2) = conc(substance_i, cell_i)
                            if (f1 > 1.0d-25) then
                                rhs(substance_i, iseg2) = conc(substance_i, cell_i) * f1
                            else
                                rhs(substance_i, iseg2) = 0.0d0
                            end if
                        end do
                    end do
                end if
            end do
            if (timon) call timstop(ithand2)

            ! PART2b: set a first order initial horizontal step for all cells in the boxes of this time step
            if (timon) call timstrt("explicit hor-step", ithand3)
            do ibox = fbox, nbox, -1
                if1 = itf(ibox + 1) + 1
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
                            do substance_i = 1, num_substances_transported
                                dq = q * bound(substance_i, -ifrom)
                                rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                            end do
                        else
                            do substance_i = 1, num_substances_transported
                                dq = q * conc(substance_i, ito)
                                rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                            end do
                        end if
                        cycle
                    end if
                    if (ito < 0) then                               ! The 'to' element was a boundary.
                        volint(ifrom) = volint(ifrom) - q
                        if (q > 0.0d0) then
                            do substance_i = 1, num_substances_transported
                                dq = q * conc(substance_i, ifrom)
                                rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                            end do
                        else
                            do substance_i = 1, num_substances_transported
                                dq = q * bound(substance_i, -ito)
                                rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                            end do
                        end if
                        cycle
                    end if
                    volint(ifrom) = volint(ifrom) - q                      ! The regular case
                    volint(ito) = volint(ito) + q
                    if (q > 0.0d0) then
                        do substance_i = 1, num_substances_transported
                            dq = q * conc(substance_i, ifrom)
                            rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                            rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                        end do
                    else
                        do substance_i = 1, num_substances_transported
                            dq = q * conc(substance_i, ito)
                            rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                            rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                        end do
                    end if
                end do                                                  ! End of the loop over exchanges
            end do                                                     ! End of the loop over boxes
            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    cell_i = iords(i)
                    if (volint(cell_i) > 1.0d-25) then
                        do substance_i = 1, num_substances_transported
                            dconc2(substance_i, cell_i) = rhs(substance_i, cell_i) / volint(cell_i)
                        end do
                    else
                        do substance_i = 1, num_substances_transported
                            dconc2(substance_i, cell_i) = conc(substance_i, cell_i)
                        end do
                    end if
                end do
            end do
            if (timon) call timstop(ithand3)

            ! PART2c: apply the horizontal flux correction for all cells in the boxes of this time step

            if (timon) call timstrt("flux correction", ithand4)
            do ibox = fbox, nbox, -1
                if1 = itf(ibox + 1) + 1
                if2 = iqsep(ibox)
                do i = if1, if2

                    ! initialisations
                    iq = iordf(i)
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    ifrom_1 = ipoint(3, iq)
                    ito_1 = ipoint(4, iq)
                    if (ifrom <= 0 .and. ito <= 0) cycle
                    if (ifrom == 0 .or. ito == 0) cycle
                    a = area(iq)
                    q = flow(iq)
                    if (abs(q) < 10.0d-25 .and. disp0q0) cycle   ! thin dam option, no dispersion at zero flow
                    ipb = 0
                    if (fluxes) then
                        if (iqdmp(iq) > 0) ipb = iqdmp(iq)
                    end if

                    if (iq <= num_exchanges_u_dir) then
                        e = disp(1)
                        al = aleng(1, 1)
                    else
                        e = disp(2)
                        al = aleng(2, 1)
                    end if
                    if (ilflag == 1) then
                        al = aleng(1, iq) + aleng(2, iq)
                        if (al < 1.0d-25) cycle
                        f1 = aleng(1, iq) / al
                    else
                        f1 = 0.5
                    end if
                    e = e * a / al                             !  constant dispersion in m3/s

                    if (ifrom < 0) then
                        vto = volint(ito)
                        d = 0.0d0
                        if (.not. disp0bnd) d = e
                        if (.not. loword) then
                            f2 = f1
                            if (q < 0.0d0) f2 = f2 - 1.0
                            d = d + min(-f2 * q + 0.5d0 * q * q * dt(ibox) / a / al, 0.0d0)
                        end if
                        d = d * dt(ibox)
                        do substance_i = 1, num_substances_transported
                            dq = d * (bound(substance_i, -ifrom) - conc(substance_i, ito))
                            rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                            dconc2(substance_i, ito) = dconc2(substance_i, ito) + dq / vto
                            if (q > 0.0d0) then
                                dq = dq + q * bound(substance_i, -ifrom) * dt(ibox)
                            else
                                dq = dq + q * conc(substance_i, ito) * dt(ibox)
                            end if
                            if (dq > 0.0d0) then
                                if (massbal) amass2(substance_i, 4) = amass2(substance_i, 4) + dq
                                if (ipb > 0) dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                            else
                                if (massbal) amass2(substance_i, 5) = amass2(substance_i, 5) - dq
                                if (ipb > 0) dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                            end if
                        end do
                        cycle
                    end if

                    if (ito < 0) then
                        vfrom = volint(ifrom)
                        d = 0.0d0
                        if (.not. disp0bnd) d = e
                        if (.not. loword) then
                            f2 = f1
                            if (q < 0) f2 = f2 - 1.0d0
                            d = d + min(-f2 * q + 0.5d0 * q * q * dt(ibox) / a / al, 0.0d0)
                        end if
                        d = d * dt(ibox)
                        do substance_i = 1, num_substances_transported
                            dq = d * (conc(substance_i, ifrom) - bound(substance_i, -ito))
                            rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                            dconc2(substance_i, ifrom) = dconc2(substance_i, ifrom) - dq / vfrom
                            if (q > 0.0d0) then
                                dq = dq + q * conc(substance_i, ifrom) * dt(ibox)
                            else
                                dq = dq + q * bound(substance_i, -ito) * dt(ibox)
                            end if
                            if (dq > 0.0d0) then
                                if (massbal) amass2(substance_i, 5) = amass2(substance_i, 5) + dq
                                if (ipb > 0) dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                            else
                                if (massbal) amass2(substance_i, 4) = amass2(substance_i, 4) - dq
                                if (ipb > 0) dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                            end if
                        end do
                        cycle
                    end if

                    vfrom = volint(ifrom)
                    vto = volint(ito)
                    f2 = f1
                    if (q < 0.0d0) f2 = f2 - 1.0d0
                    d = e + min(-f2 * q + 0.5d0 * q * q * dt(ibox) / a / al, 0.0d0)
                    d = d * dt(ibox)
                    do substance_i = 1, num_substances_transported
                        if (d < 0.0d0) then
                            e2 = d * (conc(substance_i, ifrom) - conc(substance_i, ito))
                            s = sign(1.0d0, e2)
                            select case (ifrom_1)
                            case (1:)
                                cfrm_1 = dconc2(substance_i, ifrom_1)
                            case (0)
                                if (s > 0) then
                                    cfrm_1 = 0.0d0
                                else
                                    cfrm_1 = 2.0d0 * dconc2(substance_i, ifrom)
                                end if
                            case (:-1)
                                cfrm_1 = bound(substance_i, -ifrom_1)
                            end select
                            select case (ito_1)
                            case (1:)
                                cto_1 = dconc2(substance_i, ito_1)
                            case (0)
                                if (s > 0) then
                                    cto_1 = 2.0 * dconc2(substance_i, ito)
                                else
                                    cto_1 = 0.0d0
                                end if
                            case (:-1)
                                cto_1 = bound(substance_i, -ito_1)
                            end select
                            e1 = (dconc2(substance_i, ifrom) - cfrm_1) * vfrom
                            e3 = (cto_1 - dconc2(substance_i, ito)) * vto
                            dq = s * max(0.0d0, min(s * e1, s * e2, s * e3))
                        else
                            dq = d * (dconc2(substance_i, ifrom) - dconc2(substance_i, ito))
                        end if
                        rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - dq
                        rhs(substance_i, ito) = rhs(substance_i, ito) + dq
                        dconc2(substance_i, ifrom) = dconc2(substance_i, ifrom) - dq / vfrom
                        dconc2(substance_i, ito) = dconc2(substance_i, ito) + dq / vto
                        if (ipb > 0) then
                            if (q > 0.0d0) then
                                dq = dq + q * conc(substance_i, ifrom) * dt(ibox)
                            else
                                dq = dq + q * conc(substance_i, ito) * dt(ibox)
                            end if
                            if (dq > 0.0d0) then
                                dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                            else
                                dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                            end if
                        end if
                    end do
                end do
            end do
            if (timon) call timstop(ithand4)

            ! PART2c1: Set the vertical advection of water only for all cells in the boxes of this time step
            if (timon) call timstrt("implicit ver-step", ithand5)
            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    cell_i = iords(i)
                    j = nvert(2, cell_i)
                    if (j <= 0) cycle                                  ! Do this only for head of columns
                    ih1 = nvert(1, j)
                    if (j < num_cells) then
                        ih2 = nvert(1, j + 1)
                    else
                        ih2 = num_cells + 1
                    end if
                    if (ih2 == ih1 + 1) then                             ! One cell in the column
                        do substance_i = 1, num_substances_transported
                            rhs(substance_i, cell_i) = dconc2(substance_i, cell_i)
                        end do
                    else
                        ilay = 0
                        low = 0.0d0; dia = 0.0d0; upr = 0.0d0          ! Span the tridiagonal system for this column
                        do j = ih1, ih2 - 1
                            cell_i = ivert(j)
                            volint(cell_i) = volint(cell_i) - work(3, cell_i) + work(1, cell_i)    ! Valid for Upwind AND Central (JvG)
                            ilay = ilay + 1
                            if (vertical_upwind) then
                                dia(ilay) = volint(cell_i)
                                if (work(1, cell_i) > 0.0d0) then
                                    low(ilay) = low(ilay) - work(1, cell_i)
                                else
                                    dia(ilay) = dia(ilay) - work(1, cell_i)
                                end if
                                if (work(3, cell_i) > 0.0d0) then
                                    dia(ilay) = dia(ilay) + work(3, cell_i)
                                else
                                    upr(ilay) = upr(ilay) + work(3, cell_i)
                                end if
                            else
                                upr(ilay) = work(3, cell_i) / 2.0d0
                                dia(ilay) = volint(cell_i) + work(3, cell_i) / 2.0d0 - work(1, cell_i) / 2.0d0
                                low(ilay) = -work(1, cell_i) / 2.0d0
                            end if
                        end do

                        ! The forward sweep of the double sweep procedure
                        ilay = 0
                        do j = ih1, ih2 - 2
                            cell_i = ivert(j)
                            iseg2 = ivert(j + 1)
                            ilay = ilay + 1
                            pivot = low(ilay + 1) / dia(ilay)
                            dia(ilay + 1) = dia(ilay + 1) - pivot * upr(ilay)
                            do substance_i = 1, num_substances_transported
                                rhs(substance_i, iseg2) = rhs(substance_i, iseg2) - pivot * rhs(substance_i, cell_i)
                            end do
                        end do

                        ! The backward sweep of the double sweep procedure.

                        do j = ih2 - 2, ih1, -1
                            cell_i = ivert(j)
                            iseg2 = ivert(j + 1)
                            pivot = upr(ilay)
                            do substance_i = 1, num_substances_transported
                                rhs(substance_i, iseg2) = rhs(substance_i, iseg2) / dia(ilay + 1)
                                rhs(substance_i, cell_i) = rhs(substance_i, cell_i) - pivot * rhs(substance_i, iseg2)
                            end do
                            ilay = ilay - 1
                        end do
                        do substance_i = 1, num_substances_transported
                            rhs(substance_i, cell_i) = rhs(substance_i, cell_i) / dia(1)
                        end do
                    end if

                    !   The new concentrations are stored and rhs contains the mass of them again
                    do j = ih1, ih2 - 1
                        cell_i = ivert(j)
                        do substance_i = 1, num_substances_transported
                            dconc2(substance_i, cell_i) = rhs(substance_i, cell_i)
                            rhs(substance_i, cell_i) = rhs(substance_i, cell_i) * volint(cell_i)
                        end do
                    end do
                end do
            end do
            if (timon) call timstop(ithand5)

            ! PART2c2: apply all withdrawals that were present in the hydrodynamics as negative wasteload rather than as open boundary flux
            if (timon) call timstrt("massbal", ithand6)
            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    cell_i = iords(i)                                          ! cell number
                    if (wdrawal(cell_i) == 0.0) cycle
                    q = wdrawal(cell_i) * dt(ibox)
                    if (q <= volint(cell_i)) then
                        volint(cell_i) = volint(cell_i) - q
                    else
                        write (file_unit, '(A,i8,E16.7,A,E16.7,A)') 'Warning: trying to withdraw from cell', cell_i, &
                                q, ' m3. Available is', volint(cell_i), ' m3!'
                        q = volint(cell_i)
                        volint(cell_i) = 0.0d0
                    end if
                    ipb = isdmp(cell_i)
                    do substance_i = 1, num_substances_transported
                        dq = q * dconc2(substance_i, cell_i)
                        rhs(substance_i, cell_i) = rhs(substance_i, cell_i) - dq
                        if (massbal) amass2(substance_i, 3) = amass2(substance_i, 3) - dq
                        if (ipb > 0) dmps(substance_i, ipb, 3) = dmps(substance_i, ipb, 3) + dq
                    end do
                    do k = 1, num_waste_loads
                        if (cell_i == iwaste(k)) then
                            do substance_i = 1, num_substances_transported
                                wstdmp(substance_i, k, 2) = wstdmp(substance_i, k, 2) + q * dconc2(substance_i, cell_i)
                            end do
                            exit
                        end if
                    end do
                end do
                if (.not. fluxes) cycle
                if1 = iqsep(ibox) + 1                                     ! Reconstruct vertical transport for mass balances
                if2 = itf(ibox)
                do i = if1, if2
                    iq = iordf(i)
                    ipb = iqdmp(iq)
                    if (ipb == 0) cycle
                    ifrom = ipoint(1, iq)
                    ito = ipoint(2, iq)
                    if (ifrom == 0 .or. ito == 0) cycle
                    if (vertical_upwind) then
                        q = flow(iq) * dt(ibox)                      ! This is the upwind differences version
                        if (q > 0.0) then
                            do substance_i = 1, num_substances_transported
                                dq = q * dconc2(substance_i, ifrom)
                                dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                            end do
                        else
                            do substance_i = 1, num_substances_transported
                                dq = q * dconc2(substance_i, ito)
                                dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                            end do
                        end if
                    else
                        q = flow(iq) * dt(ibox) / 2.0d0              ! This is the central differences version
                        if (q > 0.0) then
                            do substance_i = 1, num_substances_transported
                                dq = q * (dconc2(substance_i, ifrom) + dconc2(substance_i, ito))
                                dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dq
                            end do
                        else
                            do substance_i = 1, num_substances_transported
                                dq = q * (dconc2(substance_i, ifrom) + dconc2(substance_i, ito))
                                dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dq
                            end do
                        end if
                    end if
                end do
            end do

            ! PART2e: store the final results in the appropriate arrays for next fractional steps
            do ibox = fbox, nbox, -1
                is1 = its(ibox + 1) + 1
                is2 = its(ibox)
                do i = is1, is2
                    cell_i = iords(i)
                    vol = fact * volnew(cell_i) + (1.0d0 - fact) * volold(cell_i)
                    volint(cell_i) = vol
                    if (vol > 1.0d-25) then
                        do substance_i = 1, num_substances_transported
                            rhs(substance_i, cell_i) = rhs(substance_i, cell_i) + deriv(substance_i, cell_i) * dt(ibox)
                            conc(substance_i, cell_i) = rhs(substance_i, cell_i) / vol
                        end do
                    else
                        do substance_i = 1, num_substances_transported
                            rhs(substance_i, cell_i) = rhs(substance_i, cell_i) + deriv(substance_i, cell_i) * dt(ibox)
                            conc(substance_i, cell_i) = dconc2(substance_i, cell_i)
                        end do
                    end if
                end do
            end do
            if (timon) call timstop(ithand6)
            ! End of loop over fractional time steps
        end do

        if (report .and. (acc_changed > 0.0 .or. acc_remained > 0.0)) then
            write (file_unit, '(a)') 'Averaged over all steps in this iteration:'
            write (file_unit, '(a,2g12.4)') 'Number of segments changed:  ', acc_changed / nstep
            write (file_unit, '(a,2g12.4)') 'Number of segments remained: ', acc_remained / nstep
        end if

        do i = 1, its(nob + 2)                  !  update mass of box of dry cells
            cell_i = iords(i)
            do substance_i = 1, num_substances_transported
                rhs(substance_i, cell_i) = rhs(substance_i, cell_i) + deriv(substance_i, cell_i) * idt
            end do
        end do

        ! PART3:  set now the implicit step of additional velocities and diffusions per substance in the vertical
        ! There is also an implicit part in the bed if num_exchanges_bottom_dir > 0.

        noqv = num_exchanges - noqh + num_exchanges_bottom_dir
        if (noqv <= 0) goto 9999
        if (timon) call timstrt("vert.add.fluxes", ithand7)

        ! adjust the vertical distances in the grid
        do ibox = nob + 1, nbox, -1
            if1 = iqsep(ibox) + 1
            if2 = itf(ibox)
            do i = if1, if2
                iq = iordf(i)
                ifrom = ipoint(1, iq)
                ito = ipoint(2, iq)
                if (ifrom == 0 .or. ito == 0) cycle
                aleng(1, iq) = 0.5 * volnew(ifrom) / surface(ifrom)
                aleng(2, iq) = 0.5 * volnew(ito) / surface(ito)
            end do
        end do

        ! Prepare implicit step additional velocities and dispersions, finalize passive substances (set_explicit_time_step)
        do cell_i = 1, nosss
            vol = volnew(cell_i)
            do substance_i = 1, num_substances_transported
                diag(substance_i, cell_i) = vol
                if (cell_i > num_cells) rhs(substance_i, cell_i) = amass(substance_i, cell_i) + deriv(substance_i, cell_i) * idt
            end do
        end do

        ! Initialisation
        acodia(:, 1:noqv) = 0.0d0
        bcodia(:, 1:noqv) = 0.0d0

        ! Loop over exchanges to fill the matrices
        do iq = noqh + 1, num_exchanges + num_exchanges_bottom_dir

            ! Initialisations, check for transport anyhow
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom < 0 .and. ito < 0) cycle
            abound = .false.
            if (ifrom < 0 .or. ito < 0) abound = .true.

            a = area(iq)
            q = 0.0
            e = disp(3)
            al = aleng(1, 2)
            if (ilflag == 1) then
                al = aleng(1, iq) + aleng(2, iq)
            end if
            f1 = 0.5d0
            f2 = 0.5d0
            if (al > 1.0d-25) then
                if (ilflag == 1) then
                    f1 = aleng(2, iq) / al
                    f2 = 1.0d0 - f1
                end if
                dl = a / al
            else
                dl = 0.0d0
            end if
            e = e * dl
            if (iq > num_exchanges) e = 0.0d0        !  no constant water diffusion in the bed

            do substance_i = 1, num_substances_transported

                ! advection
                q = 0.0d0
                if (ivpnt(substance_i) > 0) q = velo(ivpnt(substance_i), iq) * a
                if (sw_settling) then         !  additional velocity upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    end if
                else if (iq > num_exchanges .or. (abound .and. loword)) then  ! in the bed upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    end if
                else
                    if (vertical_upwind) then
                        if (q > 0.0d0) then  ! upwind
                            q1 = q
                            q2 = 0.0d0
                        else
                            q1 = 0.0d0
                            q2 = q
                        end if
                    else
                        q1 = q * f1                 ! central velocities in the water phase
                        q2 = q * f2
                    end if
                end if

                ! diffusion
                d = e
                if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                if (abound .and. disp0bnd) d = 0.0d0

                !           fill the tridiag matrix

                q3 = (q1 + d) * idt
                q4 = (q2 - d) * idt
                if (.not. abound) then   ! the regular case
                    diag(substance_i, ifrom) = diag(substance_i, ifrom) + q3
                    bcodia(substance_i, iqv) = bcodia(substance_i, iqv) + q4
                    diag(substance_i, ito) = diag(substance_i, ito) - q4
                    acodia(substance_i, iqv) = acodia(substance_i, iqv) - q3
                else
                    if (ito > 0) then
                        q3 = q3 * bound(substance_i, -ifrom)
                        diag(substance_i, ito) = diag(substance_i, ito) - q4
                        rhs(substance_i, ito) = rhs(substance_i, ito) + q3
                    end if
                    if (ifrom > 0) then
                        q4 = q4 * bound(substance_i, -ito)
                        diag(substance_i, ifrom) = diag(substance_i, ifrom) + q3
                        rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - q4
                    end if
                end if
            end do

            ! End of loop over exchanges
        end do

        ! Now make the solution:  loop over vertical exchanges in the water
        do iq = noqh + 1, num_exchanges
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            do substance_i = 1, num_substances_transported
                pivot = acodia(substance_i, iqv) / diag(substance_i, ifrom)
                diag(substance_i, ito) = diag(substance_i, ito) - pivot * bcodia(substance_i, iqv)
                rhs(substance_i, ito) = rhs(substance_i, ito) - pivot * rhs(substance_i, ifrom)
            end do
        end do

        ! loop over exchanges in the bed
        do iq = num_exchanges + 1, num_exchanges + num_exchanges_bottom_dir
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq + 1, num_exchanges + num_exchanges_bottom_dir            !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                end if
            end do                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do substance_i = 1, num_substances_transported
                pivot = acodia(substance_i, iqv) + acodia(substance_i, iq3 - noqh)
                pivot = pivot / diag(substance_i, ifrom)
                rhs(substance_i, ito) = rhs(substance_i, ito) - pivot * rhs(substance_i, ifrom)
                diag(substance_i, ito) = diag(substance_i, ito) - pivot * (bcodia(substance_i, iqv) + bcodia(substance_i, iq3 - noqh))
            end do
        end do

        ! inverse loop over exchanges in the bed
        do iq = num_exchanges + num_exchanges_bottom_dir, num_exchanges + 1, -1
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito <= 0) cycle
            iq3 = 0                            !  find the second equivalent
            do iq2 = iq - 1, num_exchanges + 1, -1          !  pointer
                if (ipoint(1, iq2) == ifrom .and. &
                        ipoint(2, iq2) == ito) then
                    iq3 = iq2
                    exit
                end if
            end do                              !  if not found, this was the
            if (iq3 == 0) cycle            !  the second and must be skipped
            do substance_i = 1, num_substances_transported
                pivot = diag(substance_i, ito) + tiny(pivot)
                rhs(substance_i, ito) = rhs(substance_i, ito) / pivot
                diag(substance_i, ito) = 1.0
            end do
            if (ifrom <= 0) cycle
            do substance_i = 1, num_substances_transported
                pivot = bcodia(substance_i, iqv) + bcodia(substance_i, iq3 - noqh)
                rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - pivot * rhs(substance_i, ito)
            end do
        end do

        ! Inverse loop over exchanges in the water phase
        do iq = num_exchanges, noqh + 1, -1
            iqv = iq - noqh
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ito <= 0) cycle
            do substance_i = 1, num_substances_transported
                pivot = diag(substance_i, ito) + tiny(pivot)
                rhs(substance_i, ito) = rhs(substance_i, ito) / pivot
                diag(substance_i, ito) = 1.0
            end do
            if (ifrom <= 0) cycle
            do substance_i = 1, num_substances_transported
                pivot = bcodia(substance_i, iqv)
                rhs(substance_i, ifrom) = rhs(substance_i, ifrom) - pivot * rhs(substance_i, ito)
            end do
        end do

        do cell_i = 1, nosss       !  for if some diagonal entries are not 1.0
            do substance_i = 1, num_substances_transported
                rhs(substance_i, cell_i) = rhs(substance_i, cell_i) / diag(substance_i, cell_i)
            end do
        end do

        ! Mass balances ?
        if (.not. massbal) goto 9998

        do iq = noqh + 1, num_exchanges + num_exchanges_bottom_dir

            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom < 0 .and. ito < 0) cycle            ! trivial
            abound = .false.
            iqd = iqdmp(iq)
            if (ifrom >= 0 .and. ito >= 0) then             ! internal
                if (iqd <= 0) cycle                            ! no dump required
            else
                abound = .true.                                    ! is boundary
            end if
            a = area(iq)
            e = disp(3)
            al = aleng(1, 2)
            if (ilflag == 1) al = aleng(1, iq) + aleng(2, iq)
            f1 = 0.5
            f2 = 0.5
            if (al > 1.0d-25) then
                if (ilflag == 1) then
                    f1 = aleng(2, iq) / al
                    f2 = 1.0d0 - f1
                end if
                dl = a / al
            else
                dl = 0.0d0
            end if
            e = e * dl
            if (iq > num_exchanges) e = 0.0d0      !  no constant water diffusion in the bottom

            do substance_i = 1, num_substances_transported

                ! advection
                q = 0.0d0
                if (ivpnt(substance_i) > 0) q = velo(ivpnt(substance_i), iq) * a
                if (sw_settling) then         !  additional velocity upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    end if
                else if (iq > num_exchanges .or. (abound .and. loword)) then  ! in the bed upwind
                    if (q > 0.0d0) then
                        q1 = q
                        q2 = 0.0d0
                    else
                        q1 = 0.0d0
                        q2 = q
                    end if
                else
                    if (vertical_upwind) then
                        if (q > 0.0d0) then    ! Upwind
                            q1 = q
                            q2 = 0.0d0
                        else
                            q1 = 0.0d0
                            q2 = q
                        end if
                    else
                        q1 = q * f1                 ! central velocities in the water phase
                        q2 = q * f2
                    end if
                end if

                ! diffusion
                d = e
                if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                if (abound .and. disp0bnd) d = 0.0d0

                ! fill the tridiag matrix
                q3 = (q1 + d) * idt
                q4 = (q2 - d) * idt
                if (abound) then
                    if (ito > 0) then
                        dq = q3 * bound(substance_i, -ifrom) + q4 * rhs(substance_i, ito)
                        if (dq > 0.0d0) then
                            amass2(substance_i, 4) = amass2(substance_i, 4) + dq
                        else
                            amass2(substance_i, 5) = amass2(substance_i, 5) - dq
                        end if
                    else
                        dq = q3 * rhs(substance_i, ifrom) + q4 * bound(substance_i, -ito)
                        if (dq > 0.0d0) then
                            amass2(substance_i, 5) = amass2(substance_i, 5) + dq
                        else
                            amass2(substance_i, 4) = amass2(substance_i, 4) - dq
                        end if
                    end if
                else
                    dq = q3 * rhs(substance_i, ifrom) + q4 * rhs(substance_i, ito)
                end if
                if (iqd > 0) then
                    if (dq > 0) then
                        dmpq(substance_i, iqd, 1) = dmpq(substance_i, iqd, 1) + dq
                    else
                        dmpq(substance_i, iqd, 2) = dmpq(substance_i, iqd, 2) - dq
                    end if
                end if
            end do

        end do

        ! take care that rhs of water cells contains the mass again

        9998 do cell_i = 1, num_cells
            vol = volnew(cell_i)
            do substance_i = 1, num_substances_transported
                rhs(substance_i, cell_i) = rhs(substance_i, cell_i) * vol
            end do
        end do

        ! assign the double precisison results to the single precision system arrays
        ! for the bed phase only
        do cell_i = num_cells + 1, nosss
            vol = volnew(cell_i)
            do substance_i = 1, num_substances_transported
                amass(substance_i, cell_i) = rhs(substance_i, cell_i) * vol
                conc(substance_i, cell_i) = rhs(substance_i, cell_i)
            end do
            do substance_i = num_substances_transported + 1, num_substances_total - num_substances_part         ! all passive substances
                amass(substance_i, cell_i) = amass(substance_i, cell_i) + deriv(substance_i, cell_i) * idt
                conc(substance_i, cell_i) = amass(substance_i, cell_i) / surface(cell_i)
            end do
        end do
        if (timon) call timstop(ithand7)

        !         assign the double precisison results to the single precision system arrays
        !                                                          for the water phase only

        9999 do cell_i = 1, num_cells
            vol = volnew(cell_i)
            if (report) then
                if (abs(vol - volint(cell_i)) > 1.0e-6 * max(vol, volint(cell_i))) &
                        write (file_unit, '(A,i8,A,e16.7,A,e16.7)') &
                                ' cell: ', cell_i, '; computed volume: ', volint(cell_i), '; in file: ', vol
            end if
            do substance_i = 1, num_substances_transported
                amass(substance_i, cell_i) = rhs(substance_i, cell_i)
                if (abs(vol) > 1.0d-25) then
                    conc(substance_i, cell_i) = rhs(substance_i, cell_i) / vol
                else
                    conc(substance_i, cell_i) = dconc2(substance_i, cell_i)
                end if
            end do
            do substance_i = num_substances_transported + 1, num_substances_total - num_substances_part         ! all passive substances
                amass(substance_i, cell_i) = amass(substance_i, cell_i) + deriv(substance_i, cell_i) * idt
                conc(substance_i, cell_i) = amass(substance_i, cell_i) / surface(cell_i)
            end do
        end do
        deriv = 0.0d0
        if (timon) call timstop(ithandl)
    end subroutine locally_adaptive_time_step

end module m_locally_adaptive_time_step
