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
module m_dlwq15a
    use m_waq_precision
    use m_string_utils
    use m_wascal

    implicit none

contains


    subroutine dlwq15a(nosys, notot, noseg, noq, nowst, &
            nowtyp, ndmps, intopt, idt, itime, &
            iaflag, syname, conc, volume, vol2, &
            flow, ipoint, wastid, wstnam, wsttyp, &
            iwtype, iwaste, iwstkind, waste, deriv, &
            wdrawal, iknmrk, nopa, paname, param, &
            nosfun, sfname, segfun, isdmp, dmps, &
            amass2, wstdmp, isys, nsys)

        !     Deltares Software Centre

        !> \file
        !>                          Adds waste loads to the derivative vector DERIV
        !>
        !>                          This routine identifies which wasteflows are -999.0 (missing value).
        !>                          Those flows are replaces by the detected closure error / nr of
        !>                          wasteflows it concerns. This is an inherent weakness of the approach
        !>                          that all get the same value.\n
        !>                          The user wasteload dll is called. The user may insert dynamic wasteload processing.\n
        !>                          The normal processing takes place if iwstkind (see below) is zero:
        !>                             - if wasteflow .gt. 0 then load is wasteflow * prescribed value
        !>                             - if wasteflow .eq. 0 then load is prescribed value
        !>                             - if wasteflow .lt. 0 then
        !>                                   - withdrawal is wasteflow * model-conc if prescribed value eq 0.0\n
        !>                                   - withdrawal is wasteflow * prescribed value if that ne 0.0\n
        !>                             .
        !>                          Normal procedures apply for the update of balances.\n
        !>                          New is the steering of the wasteload processing with iwstkind:
        !>                             - 0 = PRES : above described situation
        !>                             - 1 = MASS : values are always used as mass also if a flow exist
        !>                             - 2 = CONC : values are always used as concentration also if flow = 0
        !>                             - 3 = RAIN : concentrations if flow > 0 and zero if flow < 0 (evaporation)
        !>                             - 4 = WELL : concentrations if flow > 0 and model conc's if flow < 0 (to groundwater)
        !>                          New also are the following location numbers:
        !>                             - -1 = a load/withdrawal over the whole water surface. Values are /m2.
        !>                             - -2 = a load/withdrawal along the bank lengthe of a volume. Values are /m.
        !>                             - -3 = a load/withdrawal over the whole water bed. Values are /m2.
        !>                             - positive flow is always directed towards the model (so influx)
        !>                             - negative flow is aways directed out of the model (so outflux)
        !>                             - surface and bed loads require the presence of a parameter SURF
        !>                             - bank loads require the presence of a parameter LENGTH


        !     Function            : Adds the wasteloads to DERIV.

        !     Logical units       : none

        !     Subroutines called  : wascal : the user specified wasteload dll

        use m_logger, only : terminate_execution
        use m_evaluate_waq_attribute
        use timers
        implicit none

        !     Parameters          :
        !     type     kind  function         name                      description

        integer(kind = int_wp), intent(in) :: nosys                    !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                    !< total number of substances
        integer(kind = int_wp), intent(in) :: noseg                    !< number of volumes
        integer(kind = int_wp), intent(in) :: noq                      !< number of flows
        integer(kind = int_wp), intent(in) :: nowst                    !< number of wastes
        integer(kind = int_wp), intent(in) :: nowtyp                   !< number of waste types
        integer(kind = int_wp), intent(in) :: ndmps                    !< number of dumped volumes for balances
        integer(kind = int_wp), intent(in) :: intopt                   !< integration suboptions
        integer(kind = int_wp), intent(in) :: idt                      !< integration time step size
        integer(kind = int_wp), intent(in) :: itime                    !< current time
        integer(kind = int_wp), intent(in) :: iaflag                   !< if 1 then accumulation of balances
        character(20), intent(in) :: syname (notot)         !< names of the substances
        real(kind = real_wp), intent(in) :: conc   (notot, noseg)   !< concentrations for withdrawals
        real(kind = real_wp), intent(in) :: volume (noseg)         !< volumes at start of time step
        real(kind = real_wp), intent(in) :: vol2   (noseg)         !< volumes at end   of time step
        real(kind = real_wp), intent(in) :: flow   (noq)         !< flows between comp. volumes
        integer(kind = int_wp), intent(in) :: ipoint (4, noq)         !< from-to pointer
        character(20), intent(in) :: wastid (nowst)         !< IDs   of the wasteloads
        character(40), intent(in) :: wstnam (nowst)         !< names of the wasteloads
        character(40), intent(in) :: wsttyp (nowst)         !< types of the wasteloads
        integer(kind = int_wp), intent(in) :: iwtype (nowst)         !< type numbers of the wasteloads
        integer(kind = int_wp), intent(in) :: iwaste (nowst)         !< volume numbers of the waste locations
        integer(kind = int_wp), intent(in) :: iwstkind(nowst)         !< treatment of the flow-conc combination
        real(kind = real_wp), intent(inout) :: waste  (0:notot, nowst)   !< waste masses/concs per system clock
        !< zero-th element is 'flow'
        real(kind = real_wp), intent(inout) :: deriv  (notot, noseg)   !< derivatives to be updated
        real(kind = real_wp), intent(out) :: wdrawal(noseg)         !< withdrawals applied to all substances
        integer(kind = int_wp), intent(in) :: iknmrk (noseg)         !< feature array
        integer(kind = int_wp), intent(in) :: nopa                     !< nr of parameters
        character(20), intent(in) :: paname (nopa)         !< names of the parameters
        real(kind = real_wp), intent(in) :: param  (nopa, noseg)   !< parameter values
        integer(kind = int_wp), intent(in) :: nosfun                   !< nr of segment functions
        character(20), intent(in) :: sfname (nosfun)         !< names of the segment functions
        real(kind = real_wp), intent(in) :: segfun (noseg, nosfun)  !< segment function values
        integer(kind = int_wp), intent(in) :: isdmp  (noseg)         !< volume to dump-location pointer
        real(kind = real_wp), intent(inout) :: dmps   (notot, ndmps, *) !< dumped segment fluxes if INTOPT > 7
        real(kind = real_wp), intent(inout) :: amass2 (notot, 5)     !< mass balance array
        real(kind = real_wp), intent(inout) :: wstdmp (notot, nowst, 2)   !< accumulated wasteloads 1/2 in and out
        integer(kind = int_wp), intent(in) :: isys                     !< first substance in array
        integer(kind = int_wp), intent(in) :: nsys                     !< number of substances  to deal with

        !     local simple variables

        logical          fluxes     ! set .true. if intopt > 7
        logical          massbal    ! set .true. if iaflag eq 1
        logical          surfbed    ! set true is surface or bed effects are wanted
        logical          withdrawal ! set true for withdrawals of model concentrations of all substances
        integer(kind = int_wp) :: nrdetec    ! number of wasteloads detected for missing flow value
        integer(kind = int_wp) :: i, i1      ! loop counter
        integer(kind = int_wp) :: iwst       ! wasteload location volume nr.
        integer(kind = int_wp) :: ip         ! help variable for 'from' and 'to' volume nr
        integer(kind = int_wp) :: ipb        ! help variable for balances
        real(kind = real_wp) :: wasteflow  ! help variable for the flow of 'this' waste
        integer(kind = int_wp) :: ierr_alloc ! error indicator for allocations
        real(kind = real_wp) :: ahelp      ! help variable for waste mass detection
        integer(kind = int_wp) :: indx       ! index in parameter or segment function arrays
        integer(kind = int_wp) :: istrt      ! loop bounds
        integer(kind = int_wp) :: istop      ! loop bounds
        integer(kind = int_wp) :: ikmrk2     ! second feature (second decimal in feature array)
        integer(kind = int_wp) :: icel       ! loop counter computational volumes

        !     Wasteflow detection items

        type DetectStruct
            integer(kind = int_wp) :: icell               ! number of the comp. volume
            integer(kind = int_wp) :: iwast               ! wasteload reference nr
            real(kind = real_wp) :: flow                ! the associated flow
        end type DetectStruct

        integer(kind = int_wp), save :: MaxDetec               ! maximum number of detection items
        type(DetectStruct), pointer, save :: IDetec (:), Itemp  (:)
        integer(kind = int_wp), allocatable, save :: IBpoint(:), IWpoint(:)
        real(kind = real_wp), allocatable, save :: wflow  (:), surf   (:), length(:)
        data     MaxDetec / 0 /

        integer(kind = int_wp) :: ithandl
        data       ithandl  /0/
        if (timon) call timstrt ("dlwq15", ithandl)

        !          Set the array with withdrawal flow rates in any case

        wdrawal = 0.0

        !          No wasteloads, then no glory

        if (nowst == 0) goto 9999

        !     in steady state mode this subroutine is called substance by substance
        !     preparations only in the call for the first substance

        if (isys == 1) then


            !          Create and dimension a backpointering array for load detection
            !          Dimension a work array for surface and bottom loads

            if (.not. allocated(ibpoint)) then
                allocate(IBpoint(noseg), stat = ierr_alloc)
                allocate(IWpoint(nowst), stat = ierr_alloc)
                allocate(wflow  (noseg), stat = ierr_alloc)
                allocate(surf   (noseg), stat = ierr_alloc)
                allocate(length (noseg), stat = ierr_alloc)
                if (ierr_alloc /= 0) then
                    write(*, *) 'ERROR: allocating work array in DLWQ15'
                    write(*, *) 'ierr_alloc :', ierr_alloc
                    write(*, *) 'noseg,nowst:', noseg, nowst
                    call terminate_execution(1)
                endif
            endif
            IBpoint = 0
            IWpoint = 0

            !          Detect all locations with missing wasteflow

            NrDetec = 0
            surfbed = .false.
            do i = 1, nowst
                iwst = iwaste(i)
                if (iwst < 0) then
                    surfbed = .true.
                    cycle
                endif
                if (abs(waste(0, i) + 999.0) < 0.001) then      ! flow not known yet
                    NrDetec = NrDetec + 1                          ! fill array with cells
                    if (NrDetec > MaxDetec) then              ! to detect flow in
                        allocate (Itemp(MaxDetec + 10))             ! resize if needed
                        do i1 = 1, MaxDetec
                            Itemp(i1)%icell = IDetec(i1)%icell
                            Itemp(i1)%iwast = IDetec(i1)%iwast
                            Itemp(i1)%flow = IDetec(i1)%flow
                        enddo
                        MaxDetec = MaxDetec + 10
                        if (associated(IDetec)) deallocate (IDetec)
                        IDetec => Itemp                             ! end of resizing operation
                    endif
                    IDetec(NrDetec)%icell = iwst
                    IDetec(NrDetec)%iwast = i
                    IDetec(NrDetec)%flow = (vol2(iwst) - volume(iwst)) / idt
                    IBpoint(iwst) = NrDetec                ! backpointering
                    IWpoint(i) = IWpoint(i) + 1       ! multiple loads
                endif
            enddo

            !        Make wasteflows if missings have been detected

            if (NrDetec > 0) then
                do i = 1, noq
                    ip = ipoint(1, i)
                    if (ip > 0) then
                        if (IBpoint(ip) > 0) then    ! this is a cell with unknown loads
                            do i1 = 1, NrDetec            ! accumulate in all unknown loads in this cell
                                if (IDetec(i1)%icell == ip) IDetec(i1)%flow = IDetec(i1)%flow + Flow(i)
                            enddo
                        endif
                    endif
                    ip = ipoint(2, i)                    ! same for outflow
                    if (ip > 0) then
                        if (IBpoint(ip) > 0) then
                            do i1 = 1, NrDetec
                                if (IDetec(i1)%icell == ip) IDetec(i1)%flow = IDetec(i1)%flow - Flow(i)
                            enddo
                        endif
                    endif
                enddo
                !           And insert them where needed
                do i = 1, NrDetec
                    iwst = IDetec(i)%iwast
                    waste(0, iwst) = IDetec(i)%flow / IWpoint(iwst) ! divide by nr of loads in this cell
                enddo
            endif

            ! call the user wasteload dll

            call wascal (nowst, notot, nosys, noseg, syname, &
                    conc, itime, nowtyp, wastid, wstnam, &
                    wsttyp, iwaste, iwtype, waste)

            !         Process for all waste locations

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
                    endif
                endif
                indx = index_in_array('LENGTH    ', paname)
                if (indx > 0) then
                    length = param(indx, :)
                else
                    indx = index_in_array('LENGTH    ', sfname)
                    if (indx > 0) then
                        length = segfun(:, indx)
                    endif
                endif
            endif

            !         end of preparations for first substance call only

        endif

        !         Normal processing
        !         accumulation?

        massbal = iaflag == 1
        fluxes = btest(intopt, 3)

        do i = 1, nowst

            iwst = iwaste(i)

            select case (iwstkind(i))

            case (0)                               ! old situation
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
                            endif
                        endif
                    enddo
                endif

                if (WasteFlow >  1.0E-30) then           ! mass = flow * conc
                    do i1 = isys, isys + nsys - 1
                        deriv (i1, iwst) = deriv (i1, iwst) + waste(i1, i) * WasteFlow
                        if (massbal) amass2(i1, 3) = amass2(i1, 3) + waste(i1, i) * WasteFlow * idt
                        if (ipb > 0  .and. fluxes) &
                                dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                        if (fluxes) wstdmp(i1, i, 1) = wstdmp(i1, i, 1) + waste(i1, i) * WasteFlow * idt
                    enddo
                endif

                if (WasteFlow < -1.0E-30) then           ! withdrawal
                    withdrawal = .true.
                    do i1 = isys, isys + nsys - 1
                        if (abs(waste(i1, i)) > 1.0E-30) withdrawal = .false.
                    enddo
                    if (withdrawal) then
                        wdrawal(iwst) = wdrawal(iwst) - WasteFlow
                    else
                        do i1 = isys, isys + nsys - 1
                            ahelp = 0.0
                            if (abs(waste(i1, i)) < 1.0E-30) then      !  with model concentration
                                if (i1 <= nosys) ahelp = conc(i1, iwst) * WasteFlow  ! transported substances
                            else                                           !  with prescribed concentration
                                ahelp = waste(i1, i) * WasteFlow
                            endif
                            deriv (i1, iwst) = deriv (i1, iwst) + ahelp
                            if (massbal) amass2(i1, 3) = amass2(i1, 3) + ahelp * idt
                            if (ipb > 0  .and. fluxes) &
                                    dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - ahelp * idt
                            if (fluxes) wstdmp(i1, i, 2) = wstdmp(i1, i, 2) - ahelp * idt
                        enddo
                    endif
                endif

            case (1)         ! always MASS
                wflow = 0.0
                if (iwst > 0) then               ! normal processing
                    wflow(iwst) = 1.0
                    istrt = iwst
                    istop = iwst
                else                                  ! surface or bed processing
                    do i1 = 1, noseg
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)               ! surface processing
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = surf(i1)
                            case (-2)               ! bank processing
                                wflow(i1) = length(i1)
                            case (-3)               ! bed processing
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = surf(i1)
                            end select
                        endif
                    enddo
                    istrt = 1
                    istop = noseg
                endif
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
                            endif
                        endif
                        if (ipb > 0  .and. fluxes) then
                            if (waste(i1, i) < 0.0) then     ! withdrawal
                                dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - waste(i1, i) * WasteFlow * idt
                            else                                 ! load
                                dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                            endif
                        endif
                    enddo
                enddo

            case (2)         ! always CONC
                wflow = 0.0
                if (iwst > 0) then               ! normal processing
                    wflow(iwst) = waste(0, i)
                    istrt = iwst
                    istop = iwst
                else                                  ! surface or bed processing
                    do i1 = 1, noseg
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = waste(0, i) * surf(i1)
                            case (-2)
                                wflow(i1) = waste(0, i) * length(i1)
                            case (-3)
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = waste(0, i) * surf(i1)
                            end select
                        endif
                    enddo
                    istrt = 1
                    istop = noseg
                endif
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
                            endif
                        endif
                        if (ipb > 0 .and. fluxes) then
                            if (waste(i1, i) * WasteFlow < 0.0) then     ! withdrawal
                                dmps(i1, ipb, 3) = dmps(i1, ipb, 3) - waste(i1, i) * WasteFlow * idt
                            else                                 ! load
                                dmps(i1, ipb, 2) = dmps(i1, ipb, 2) + waste(i1, i) * WasteFlow * idt
                            endif
                        endif
                    enddo
                enddo

            case (3)         ! RAIN ( load is CONC, withdrawal is zero )
                wflow = 0.0
                if (iwst > 0) then               ! normal processing
                    wflow(iwst) = waste(0, i)
                    istrt = iwst
                    istop = iwst
                else                                  ! surface or bed processing
                    do i1 = 1, noseg
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = waste(0, i) * surf(i1)
                            case (-2)
                                wflow(i1) = waste(0, i) * length(i1)
                            case (-3)
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = waste(0, i) * surf(i1)
                            end select
                        endif
                    enddo
                    istrt = 1
                    istop = noseg
                endif
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
                        enddo
                    endif
                enddo

            case (4)         ! WELL ( load is CONC, withdrawal is with model conc
                wflow = 0.0
                if (iwst > 0) then               ! normal processing
                    wflow(iwst) = waste(0, i)
                    istrt = iwst
                    istop = iwst
                else                                  ! surface or bed processing
                    do i1 = 1, noseg
                        if (btest(iknmrk(i1), 0)) then
                            select case (iwst)
                            case (-1)
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 0 .or. ikmrk2 == 1) wflow(i1) = waste(0, i) * surf(i1)
                            case (-2)
                                wflow(i1) = waste(0, i) * length(i1)
                            case (-3)
                                call evaluate_waq_attribute(2, iknmrk(i1), ikmrk2)
                                if (ikmrk2 == 3 .or. ikmrk2 == 0) wflow(i1) = waste(0, i) * surf(i1)
                            end select
                        endif
                    enddo
                    istrt = 1
                    istop = noseg
                endif
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
                        enddo
                    else
                        wdrawal(icel) = wdrawal(icel) - WasteFlow
                    endif
                enddo

            end select

        end do

        9999 if (timon) call timstop (ithandl)

        return
    end

end module m_dlwq15a
