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
module m_dlwq15
    use m_waq_precision
    use m_string_utils
    use m_wascal

    implicit none

contains

    !> Adds waste loads to the derivative vector DERIV.
    !! This routine identifies which wasteflows are -999.0 (missing value).
    !! Those flows are replaced by the detected closure error / nr of
    !! waste flows it concerns. This is an inherent weakness of the approach
    !! that all get the same value.
    !! The user wasteload dll is called. The user may insert dynamic wasteload processing.
    !! The original processing takes place if iwstkind (see below) is zero:
    !!  - if wasteflow >  0 then load is wasteflow * prescribed value
    !!  - if wasteflow == 0 then load is prescribed value
    !!  - if wasteflow <  0 then
    !!      - withdrawal is wasteflow * model-conc if prescribed value == 0.0
    !!      - withdrawal is wasteflow * prescribed value if that != 0.0
    !! Normal procedures apply for the update of balances.
    !! New is the steering of the wasteload processing with iwstkind:
    !!    0 = PRES : normal processing
    !!    1 = MASS : values are always used as mass also if a flow exist
    !!    2 = CONC : values are always used as concentration also if flow = 0
    !!    3 = RAIN : concentrations if flow > 0 and zero if flow < 0 (evaporation)
    !!    4 = WELL : concentrations if flow > 0 and model conc's if flow < 0 (to groundwater)
    !! Positive flow is considered directed into the model (inflow).
    !! Negative flow is considered directed out of the model (outflow).
    !! Surface and bed loads require the presence of parameter SURF.
    !! Bank loads require the presence of parameter LENGTH.
    subroutine dlwq15(num_substances_transported, num_substances_total, num_cells, num_exchanges, num_waste_loads, &
            num_waste_load_types, num_monitoring_cells, intopt, idt, itime, &
            iaflag, syname, conc, volume, vol2, &
            flow, ipoint, wastid, wstnam, wsttyp, &
            iwtype, iwaste, iwstkind, waste, deriv, &
            iknmrk, num_spatial_parameters, paname, param, num_spatial_time_fuctions, &
            sfname, segfun, isdmp, dmps, amass2, &
            wstdmp, isys, nsys)

        use m_logger_helper, only : stop_with_error
        use m_extract_waq_attribute
        use timers
        implicit none

        integer(kind = int_wp), intent(in   ) :: num_substances_transported                   !< Number of transported substances
        integer(kind = int_wp), intent(in   ) :: num_substances_total                   !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                   !< Number of cells or segments
        integer(kind = int_wp), intent(in   ) :: num_exchanges                     !< Number of flows
        integer(kind = int_wp), intent(in   ) :: num_waste_loads                   !< Number of wastes
        integer(kind = int_wp), intent(in   ) :: num_waste_load_types                  !< Number of waste types
        integer(kind = int_wp), intent(in   ) :: num_monitoring_cells
        integer(kind = int_wp), intent(in   ) :: intopt                  !< Integration suboptions
        integer(kind = int_wp), intent(in   ) :: idt                     !< Integration time-step
        integer(kind = int_wp), intent(in   ) :: itime                   !< Current time
        integer(kind = int_wp), intent(in   ) :: iaflag                  !< If 1 then accumulation of balances
        character(20),          intent(in   ) :: syname(num_substances_total)           !< Names of the substances
        real(kind = real_wp),   intent(in   ) :: conc(num_substances_total, num_cells)      !< Concentrations for withdrawals
        real(kind = real_wp),   intent(in   ) :: volume(num_cells)           !< Volumes at start of time step
        real(kind = real_wp),   intent(in   ) :: vol2(num_cells)             !< Volumes at end   of time step
        real(kind = real_wp),   intent(in   ) :: flow(num_exchanges)               !< Flows between comp. volumes
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)          !< From-to pointer
        character(20),          intent(in   ) :: wastid(num_waste_loads)           !< IDs   of the wasteloads
        character(40),          intent(in   ) :: wstnam(num_waste_loads)           !< Names of the wasteloads
        character(40),          intent(in   ) :: wsttyp(num_waste_loads)           !< Types of the wasteloads
        integer(kind = int_wp), intent(in   ) :: iwtype(num_waste_loads)           !< Type numbers of the wasteloads
        integer(kind = int_wp), intent(in   ) :: iwaste(num_waste_loads)           !< Volume numbers of the waste locations
                                                                         !<   -1 = a load/withdrawal over the whole water surface. Values are /m2.
                                                                         !<   -2 = a load/withdrawal along the bank length of a volume. Values are /m.
                                                                         !<   -3 = a load/withdrawal over the whole water bed. Values are /m2.
        integer(kind = int_wp), intent(in   ) :: iwstkind(num_waste_loads)         !< Meaning of the flow-concentration combination
                                                                         !< 0 = PRES : original implementation
                                                                         !< 1 = MASS : values are always used as mass, even if a flow exists
                                                                         !< 2 = CONC : values are always used as concentration, even if flow == 0
                                                                         !< 3 = RAIN : concentrations if flow > 0 and zero if flow < 0 (evaporation)
                                                                         !< 4 = WELL : concentrations if flow > 0 and model concentrations if flow < 0 (to groundwater)
        real(kind = real_wp),   intent(inout) :: waste(0:num_substances_total, num_waste_loads)   !< Waste masses/concs per system clock
                                                                         !< Zero-th element is 'flow'
        real(kind = real_wp),   intent(inout) :: deriv(num_substances_total, num_cells)     !< Derivatives to be updated
        integer(kind = int_wp), intent(in   ) :: iknmrk(num_cells)           !< Feature array
        integer(kind = int_wp), intent(in   ) :: num_spatial_parameters                    !< Number of parameters
        character(20),          intent(in   ) :: paname(num_spatial_parameters)            !< Names of the parameters
        real(kind = real_wp),   intent(in   ) :: param(num_spatial_parameters, num_cells)      !< Parameter values
        integer(kind = int_wp), intent(in   ) :: num_spatial_time_fuctions                  !< Number of segment functions
        character(20),          intent(in   ) :: sfname(num_spatial_time_fuctions)          !< Names of the segment functions
        real(kind = real_wp),   intent(in   ) :: segfun(num_cells, num_spatial_time_fuctions)   !< Segment function values
        integer(kind = int_wp), intent(in   ) :: isdmp(num_cells)            !< Volume to dump-location pointer
        real(kind = real_wp),   intent(inout) :: dmps(num_substances_total, num_monitoring_cells, *)   !< Dumped segment fluxes if INTOPT > 7
        real(kind = real_wp),   intent(inout) :: amass2(num_substances_total, 5)        !< Mass balance array
        real(kind = real_wp),   intent(inout) :: wstdmp(num_substances_total, num_waste_loads, 2) !< Accumulated wasteloads 1/2 in and out
        integer(kind = int_wp), intent(in   ) :: isys                    !< First substance in array
        integer(kind = int_wp), intent(in   ) :: nsys                    !< Number of substances  to deal with

        ! Local variables
        logical                :: fluxes     !< Set .true. if intopt > 7
        logical                :: massbal    !< Set .true. if iaflag eq 1
        logical                :: surfbed    !< Set true is surface or bed effects are wanted
        integer(kind = int_wp) :: nrdetec    !< Number of wasteloads detected for missing flow value
        integer(kind = int_wp) :: i, i1      !< Loop counter
        integer(kind = int_wp) :: iwst       !< Index of cell for waste load:
                                             !< iwst == -1 => over the whole water surface. Values are /m2.
                                             !< iwst == -2 => along the bank length of a cell. Values are /m.
                                             !< iwst == -3 => over the whole water bed. Values are /m2.
        integer(kind = int_wp) :: ip         !< Auxiliary variable for 'from' and 'to' cell number
        integer(kind = int_wp) :: ipb        !< Auxiliary variable for balances
        real(kind = real_wp)   :: wasteflow  !< Auxiliary variable for the flow of 'this' waste
        integer(kind = int_wp) :: ierr_alloc !< Error indicator for allocations
        real(kind = real_wp)   :: ahelp      !< Auxiliary variable for waste mass detection
        integer(kind = int_wp) :: indx       !< Index in parameter or segment function arrays
        integer(kind = int_wp) :: istrt      !< Loop bounds
        integer(kind = int_wp) :: istop      !< Loop bounds
        integer(kind = int_wp) :: ikmrk2     !< Second feature (second decimal in feature array)
        integer(kind = int_wp) :: icel       !< Loop counter computational volumes
        type DetectStruct                    !< Wasteflow detection items
            integer(kind = int_wp) :: icell  !< Cell index
            integer(kind = int_wp) :: iwast  !< Index of wasteload
            real(kind = real_wp)   :: flow   !< Associated flow
        end type DetectStruct

        integer(kind = int_wp), save :: MaxDetec !< maximum number of detection items
        type(DetectStruct), pointer, save :: IDetec (:), Itemp  (:)
        integer(kind = int_wp), allocatable, save :: IBpoint(:), IWpoint(:)
        real(kind = real_wp), allocatable, save :: wflow  (:), surf   (:), length(:)
        data     MaxDetec / 0 /
        integer(kind = int_wp) :: ithandl
        data       ithandl  /0/
        if (timon) call timstrt ("dlwq15", ithandl)
        ! No wasteloads
        if (num_waste_loads == 0) goto 9999

        ! in steady state mode this subroutine is called substance by substance
        ! preparations only in the call for the first substance
        if (isys == 1) then
            ! Create and dimension a backpointering array for load detection
            ! Dimension a work array for surface and bottom loads
            if (.not. allocated(ibpoint)) then
                allocate(IBpoint(num_cells), stat = ierr_alloc)
                allocate(IWpoint(num_waste_loads), stat = ierr_alloc)
                allocate(wflow  (num_cells), stat = ierr_alloc)
                allocate(surf   (num_cells), stat = ierr_alloc)
                allocate(length (num_cells), stat = ierr_alloc)
                if (ierr_alloc /= 0) then
                    write(*, *) 'ERROR: allocating work array in DLWQ15'
                    write(*, *) 'ierr_alloc :', ierr_alloc
                    write(*, *) 'num_cells,num_waste_loads:', num_cells, num_waste_loads
                    call stop_with_error()
                end if
            end if
            IBpoint = 0
            IWpoint = 0

            ! Detect all locations with missing wasteflow
            NrDetec = 0
            surfbed = .false.
            do i = 1, num_waste_loads
                iwst = iwaste(i)
                if (iwst < 0) then
                    surfbed = .true.
                    cycle
                end if
                if (abs(waste(0, i) + 999.0) < 0.001) then   ! flow not known yet
                    NrDetec = NrDetec + 1                    ! fill array with cells
                    if (NrDetec > MaxDetec) then             ! to detect flow in
                        allocate (Itemp(MaxDetec + 10))      ! resize if needed
                        do i1 = 1, MaxDetec
                            Itemp(i1)%icell = IDetec(i1)%icell
                            Itemp(i1)%iwast = IDetec(i1)%iwast
                            Itemp(i1)%flow = IDetec(i1)%flow
                        end do
                        MaxDetec = MaxDetec + 10
                        if (associated(IDetec)) deallocate (IDetec)
                        IDetec => Itemp ! end of resizing operation
                    end if
                    IDetec(NrDetec)%icell = iwst
                    IDetec(NrDetec)%iwast = i
                    IDetec(NrDetec)%flow = (vol2(iwst) - volume(iwst)) / idt
                    IBpoint(iwst) = NrDetec     ! backpointering
                    IWpoint(i) = IWpoint(i) + 1 ! multiple loads
                end if
            end do

            ! Make wasteflows if missings have been detected
            if (NrDetec > 0) then
                do i = 1, num_exchanges
                    ip = ipoint(1, i)
                    if (ip > 0) then
                        if (IBpoint(ip) > 0) then    ! this is a cell with unknown loads
                            do i1 = 1, NrDetec       ! accumulate in all unknown loads in this cell
                                if (IDetec(i1)%icell == ip) IDetec(i1)%flow = IDetec(i1)%flow + Flow(i)
                            end do
                        end if
                    end if
                    ip = ipoint(2, i) ! same for outflow
                    if (ip > 0) then
                        if (IBpoint(ip) > 0) then
                            do i1 = 1, NrDetec
                                if (IDetec(i1)%icell == ip) IDetec(i1)%flow = IDetec(i1)%flow - Flow(i)
                            end do
                        end if
                    end if
                end do
                ! And insert them where needed
                do i = 1, NrDetec
                    iwst = IDetec(i)%iwast
                    waste(0, iwst) = IDetec(i)%flow / IWpoint(iwst) ! divide by number of loads in this cell
                end do
            end if

            ! call the user wasteload dll
            call wascal(num_waste_loads, num_substances_total, num_substances_transported, num_cells, syname, &
                    conc, itime, num_waste_load_types, wastid, wstnam, &
                    wsttyp, iwaste, iwtype, waste)

            ! Process for all waste locations
            if (surfbed) then
                surf = 0.0
                length = 0.0
                indx = index_in_array('SURF      ', paname)
                if (indx > 0) then
                    surf = param(indx, :)
                else
                    indx = index_in_array('SURF      ', sfname)
                    if (indx > 0) then
                        surf = segfun(:, indx)
                    end if
                end if
                indx = index_in_array('LENGTH    ', paname)
                if (indx > 0) then
                    length = param(indx, :)
                else
                    indx = index_in_array('LENGTH    ', sfname)
                    if (indx > 0) then
                        length = segfun(:, indx)
                    end if
                end if
            end if
            ! end of preparations for first substance call only
        end if

        ! Normal processing
        ! accumulation?
        massbal = iaflag == 1
        fluxes = btest(intopt, 3)

        do i = 1, num_waste_loads
            iwst = iwaste(i)
            select case (iwstkind(i))
            case (0) ! Original situation
                if (iwst <= 0) cycle
                WasteFlow = waste(0, i)
                ipb = isdmp(iwst)
                if (abs(WasteFlow) <= 1.0E-30) then       ! just load of mass
                    do i1 = isys, isys + nsys - 1
                        deriv (i1, iwst) = deriv (i1, iwst) + waste(i1, i)
                        if (massbal) amass2(i1, 3) = amass2(i1, 3) + waste(i1, i) * idt
                        if (ipb > 0 .and. fluxes) then
                            if (waste(i1, i) < 0.0) then     ! withdrawal
                                dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - waste(i1, i) * idt
                                wstdmp(i1, i, 2) = wstdmp(i1, i, 2) - waste(i1, i) * idt
                            else                                 ! load
                                dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * idt
                                wstdmp(i1, i, 1) = wstdmp(i1, i, 1) + waste(i1, i) * idt
                            end if
                        end if
                    end do
                end if

                if (WasteFlow >  1.0E-30) then ! mass = flow * conc
                    do i1 = isys, isys + nsys - 1
                        deriv (i1, iwst) = deriv (i1, iwst) + waste(i1, i) * WasteFlow
                        if (massbal) amass2(i1, 3) = amass2(i1, 3) + waste(i1, i) * WasteFlow * idt
                        if (ipb > 0  .and. fluxes) &
                                dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                        if (fluxes) wstdmp(i1, i, 1) = wstdmp(i1, i, 1) + waste(i1, i) * WasteFlow * idt
                    end do
                end if

                if (WasteFlow < -1.0E-30) then ! withdrawal
                    do i1 = isys, isys + nsys - 1
                        ahelp = 0.0
                        if (abs(waste(i1, i)) < 1.0E-30) then !  with model concentration
                            if (i1 <= num_substances_transported) ahelp = conc(i1, iwst) * WasteFlow ! transported substances
                        else !  with prescribed concentration
                            ahelp = waste(i1, i) * WasteFlow
                        end if
                        deriv (i1, iwst) = deriv (i1, iwst) + ahelp
                        if (massbal) amass2(i1, 3) = amass2(i1, 3) + ahelp * idt
                        if (ipb > 0  .and. fluxes) &
                                dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - ahelp * idt
                        if (fluxes) wstdmp(i1, i, 2) = wstdmp(i1, i, 2) - ahelp * idt
                    end do
                end if

            case (1) ! always MASS
                wflow = 0.0
                if (iwst > 0) then               ! normal processing
                    wflow(iwst) = 1.0
                    istrt = iwst
                    istop = iwst
                else                                  ! surface or bed processing
                    do i1 = 1, num_cells
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)               ! surface processing
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = surf(i1)
                            case (-2)               ! bank processing
                                wflow(i1) = length(i1)
                            case (-3)               ! bed processing
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = surf(i1)
                            end select
                        end if
                    end do
                    istrt = 1
                    istop = num_cells
                end if
                do icel = istrt, istop
                    WasteFlow = wflow(icel)
                    ipb = isdmp(icel)
                    do i1 = isys, isys + nsys - 1
                        deriv (i1, icel) = deriv (i1, icel) + waste(i1, i) * WasteFlow
                        if (massbal) amass2(i1, 3) = amass2(i1, 3) + waste(i1, i) * WasteFlow * idt
                        if (fluxes) then
                            if (waste(i1, i) < 0.0) then     ! withdrawal
                                wstdmp(i1, i, 1) = wstdmp(i1, i, 2) - waste(i1, 2) * WasteFlow * idt
                            else                                 ! load
                                wstdmp(i1, i, 1) = wstdmp(i1, i, 1) + waste(i1, i) * WasteFlow * idt
                            end if
                        end if
                        if (ipb > 0  .and. fluxes) then
                            if (waste(i1, i) < 0.0) then     ! withdrawal
                                dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - waste(i1, i) * WasteFlow * idt
                            else                                 ! load
                                dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                            end if
                        end if
                    end do
                end do

            case (2) ! always CONC
                wflow = 0.0
                if (iwst > 0) then ! normal processing
                    wflow(iwst) = waste(0, i)
                    istrt = iwst
                    istop = iwst
                else ! surface or bed processing
                    do i1 = 1, num_cells
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = waste(0, i) * surf(i1)
                            case (-2)
                                wflow(i1) = waste(0, i) * length(i1)
                            case (-3)
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = waste(0, i) * surf(i1)
                            end select
                        end if
                    end do
                    istrt = 1
                    istop = num_cells
                end if
                do icel = istrt, istop
                    WasteFlow = wflow(icel)
                    ipb = isdmp(icel)
                    do i1 = isys, isys + nsys - 1
                        deriv (i1, icel) = deriv (i1, icel) + waste(i1, i) * WasteFlow
                        if (massbal) amass2(i1, 3) = amass2(i1, 3) + waste(i1, i) * WasteFlow * idt
                        if (fluxes) then
                            if (waste(i1, i) * WasteFlow < 0.0) then     ! withdrawal
                                wstdmp(i1, i, 1) = wstdmp(i1, i, 2) - waste(i1, 2) * WasteFlow * idt
                            else                                 ! load
                                wstdmp(i1, i, 1) = wstdmp(i1, i, 1) + waste(i1, i) * WasteFlow * idt
                            end if
                        end if
                        if (ipb > 0 .and. fluxes) then
                            if (waste(i1, i) * WasteFlow < 0.0) then     ! withdrawal
                                dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - waste(i1, i) * WasteFlow * idt
                            else                                 ! load
                                dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                            end if
                        end if
                    end do
                end do

            case (3) ! RAIN ( load is CONC, withdrawal is zero )
                wflow = 0.0
                if (iwst > 0) then ! normal processing
                    wflow(iwst) = waste(0, i)
                    istrt = iwst
                    istop = iwst
                else ! surface or bed processing
                    do i1 = 1, num_cells
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = waste(0, i) * surf(i1)
                            case (-2)
                                wflow(i1) = waste(0, i) * length(i1)
                            case (-3)
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = waste(0, i) * surf(i1)
                            end select
                        end if
                    end do
                    istrt = 1
                    istop = num_cells
                end if
                do icel = istrt, istop
                    WasteFlow = wflow(icel)
                    ipb = isdmp(icel)
                    if (WasteFlow >  1.0E-30) then ! mass = flow * conc
                        do i1 = isys, isys + nsys - 1
                            deriv (i1, icel) = deriv (i1, icel) + waste(i1, i) * WasteFlow
                            if (massbal) amass2(i1, 3) = amass2(i1, 3) + waste(i1, i) * WasteFlow * idt
                            if (ipb > 0 .and. fluxes) &
                                    dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                            if (fluxes) wstdmp(i1, i, 1) = wstdmp(i1, i, 1) + waste(i1, i) * WasteFlow * idt
                        end do
                    end if
                end do

            case (4) ! WELL ( load is CONC, withdrawal is with model conc
                wflow = 0.0
                if (iwst > 0) then               ! normal processing
                    wflow(iwst) = waste(0, i)
                    istrt = iwst
                    istop = iwst
                else                                  ! surface or bed processing
                    do i1 = 1, num_cells
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = waste(0, i) * surf(i1)
                            case (-2)
                                wflow(i1) = waste(0, i) * length(i1)
                            case (-3)
                                call extract_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = waste(0, i) * surf(i1)
                            end select
                        end if
                    end do
                    istrt = 1
                    istop = num_cells
                end if
                do icel = istrt, istop
                    WasteFlow = wflow(icel)
                    ipb = isdmp(icel)
                    if (WasteFlow >  1.0E-30) then           ! mass = flow * conc
                        do i1 = isys, isys + nsys - 1
                            deriv (i1, icel) = deriv (i1, icel) + waste(i1, i) * WasteFlow
                            if (massbal) amass2(i1, 3) = amass2(i1, 3) + waste(i1, i) * WasteFlow * idt
                            if (ipb > 0 .and. fluxes) &
                                    dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                            if (fluxes) wstdmp(i1, i, 1) = wstdmp(i1, i, 1) + waste(i1, i) * WasteFlow * idt
                        end do
                    else
                        do i1 = isys, isys + nsys - 1
                            deriv (i1, iwst) = deriv (i1, iwst) + conc(i1, iwst) * WasteFlow
                            if (massbal) amass2(i1, 3) = amass2(i1, 3) + conc(i1, iwst) * WasteFlow * idt
                            if (ipb > 0 .and. fluxes) &
                                    dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - conc(i1, iwst) * WasteFlow * idt
                            if (fluxes) wstdmp(i1, i, 2) = wstdmp(i1, i, 2) - conc(i1, iwst) * WasteFlow * idt
                        end do
                    end if
                end do
            end select
        end do
        9999 if (timon) call timstop (ithandl)
    end subroutine dlwq15
end module m_dlwq15
