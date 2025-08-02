!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_time_dependent_variables
    use m_waq_precision
    use timers
    use m_update_condition, only: update_condition
    use m_syst          ! Time function flags
    use delwaq2_data

    implicit none

    private
    public :: update_volumes_and_time_step, update_time_dependent_external_forcing

contains


    !> Makes values at ITIME for volumes only
    !! Routine is a stripped version of update_time_dependent_external_forcing to read volumes
    !! at the end of the time step only. Remainder of the time
    !! varying info is updated after the time step has finished.
    !! Ratio is that the end-volume of a time step is often needed
    !! to determine drying and flooding and for a number of
    !! numerical schemes.
    subroutine update_volumes_and_time_step(file_unit_list, itime, itimel, harmat, array, &
            iharm, nrharm, nrftot, num_cells, volume, &
            ipoint, luntxt, ftype, isflag, ivflag, &
            updatv, inwspc, anwspc, inwtyp, iwork, &
            lstrec, lrewin, vollst, dlwqd)


        integer(kind = int_wp), intent(in) :: file_unit_list(*) !< Array with unit numbers
        integer(kind = int_wp), intent(in) :: itime             !< The model timer
        integer(kind = int_wp), intent(in) :: itimel            !< The model timer last step
        real(kind = real_wp), intent(inout) :: harmat(*)         !< Matrices harmonic components
        real(kind = real_wp), intent(inout) :: array (*)         !< Set of double file buffers
        integer(kind = int_wp), intent(in) :: iharm (*)         !< Harmonic time space
        integer(kind = int_wp), intent(in) :: nrharm(*)         !< Set of nrs of harmonic records
        integer(kind = int_wp), intent(in) :: nrftot(*)         !< Set of record lengthes
        integer(kind = int_wp), intent(in) :: num_cells             !< Nr of computational volumes
        real(kind = real_wp), intent(out) :: volume(num_cells)     !< Array of volumes per gridcell
        integer(kind = int_wp), intent(in) :: ipoint(*)         !< Set of pointers to destination
        character(len = *), intent(in) :: luntxt(*)         !< Text with the unit numbers
        integer(kind = int_wp), intent(in) :: ftype (*)         !< Type of file to read
        integer(kind = int_wp), intent(in) :: isflag            !< = 1 then 'ddhhmmss' format
        integer(kind = int_wp), intent(in) :: ivflag            !< = 1 then computed volumes
        logical, intent(out) :: updatv            !< set to T if volume is updated
        integer(kind = int_wp), intent(inout) :: inwspc(*)         !< Integer space new time functions
        real(kind = real_wp), intent(inout) :: anwspc(*)         !< Real space new time functions
        integer(kind = int_wp), intent(in) :: inwtyp(*)         !< Types of items
        integer(kind = int_wp), intent(inout) :: iwork (*)         !< Integer workspace
        logical, intent(in) :: lstrec            !< Switch last record on rewind wanted
        logical, intent(out) :: lrewin            !< If T then rewindtook place
        real(kind = real_wp), intent(out) :: vollst(num_cells)     !< Last volume record before rewind
        type(delwaq_data), intent(inout) :: dlwqd             !< derived type for persistent storage

        ! Local variables
        integer(kind = int_wp) :: iph, ipf, ipa, ipi  ! pointers in the arrays
        integer(kind = int_wp) :: ifflag              ! if 1, then it was the first invoke
        logical     update, ldum(2)                   ! logicals on rewind
        integer(kind = int_wp) :: ierr                ! error indicator

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("update_volumes_and_time_step", ithandl)

        ! initialisation
        iph = 1
        ipf = 1
        ipa = 1
        ipi = 1
        ifflag = 0
        updatv = .false.
        lrewin = .false.

        ! integration step size IDT
        if (nrftot(1) > 0) then
            ipa = ipa + 2
            ipi = ipi + 4
        endif

        !volumes
        if (nrharm(2) >= 0) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), volume, 1, nrharm(2), &
                    num_cells, nrftot(2), ipa, iph, ipf, &
                    ipi, luntxt, 7, isflag, ifflag, &
                    update, .false., 0, iwork, lstrec, &
                    lrewin, vollst, ftype, dlwqd)

            if (update) then
                updatv = .true.
            endif
        endif

        if (timon) call timstop (ithandl)
    end subroutine update_volumes_and_time_step

    !>  Updates time level of all external forcing
    !!  Besides this routine, there is also the update_volumes_and_time_step routine that does
    !!  same for volumes only. This routine is campletely used once at
    !!  the start of simulation. Furthermore it is only used without
    !!  reading the new volumes, that is then done by update_volumes_and_time_step.
    subroutine update_time_dependent_external_forcing(file_unit_list, itime, itimel, harmat, array, &
            iharm, nrharm, nrftot, idt, volume, &
            disper, area, flow, velo, aleng, &
            wastes, bounds, consts, param, funcs, &
            sfuncs, ipoint, luntxt, luntx2, ftype, &
            intsrt, isflag, ifflag, ivflag, ilflag, &
            update, iktim, iknmrk, inwspc, anwspc, &
            inwtyp, iwork, lstrec, lrewin, vollst, &
            rdvolu, gridps, dlwqd)

        use m_logger_helper, only: stop_with_error
        use m_open_waq_files
        use m_grid_utils_external
        use m_waq_memory_dimensions
        use m_attribute_array, only: update_attribute_array
        use m_dlwqta

        integer(kind = int_wp), intent(inout) :: file_unit_list(*)             !< Array with unit numbers
        integer(kind = int_wp), intent(in) :: itime                         !< Model timer
        integer(kind = int_wp), intent(inout) :: itimel                        !< Model timer one step ago
        real(kind = real_wp), intent(inout) :: harmat(harmonics_arr_len)                !< Matrices harmonic components
        real(kind = real_wp), intent(inout) :: array (nlines)                !< Set of double file buffers
        integer(kind = int_wp), intent(in) :: iharm (num_harmonics)                !< Harmonics time space
        integer(kind = int_wp), intent(inout) :: nrharm(num_items_time_fn)                !< set of nrs of harmonic records
        integer(kind = int_wp), intent(in) :: nrftot(num_items_time_fn)                !< set of record lengths
        integer(kind = int_wp), intent(out) :: idt                           !< Integration time step size
        real(kind = real_wp), intent(out) :: volume(num_cells + num_cells_bottom)         !< Array of segment volumes
        real(kind = real_wp), intent(out) :: disper(num_dispersion_arrays, num_exchanges + num_exchanges_bottom_dir)    !< Array of dispersions
        real(kind = real_wp), intent(out) :: area(num_exchanges + num_exchanges_bottom_dir)              !< Array of exchange surfaces
        real(kind = real_wp), intent(out) :: flow(num_exchanges + num_exchanges_bottom_dir)              !< Array of flows
        real(kind = real_wp), intent(out) :: velo(num_velocity_arrays, num_exchanges + num_exchanges_bottom_dir)      !< Array of velocities
        real(kind = real_wp), intent(out) :: aleng(2, num_exchanges + num_exchanges_bottom_dir)          !< Array of from and to lengths
        real(kind = real_wp), intent(out) :: wastes(num_substances_total + 2, num_waste_loads)      !< Array of wasteloads
        real(kind = real_wp), intent(out) :: bounds(num_substances_transported, num_boundary_conditions)          !< Array of boundary conditions
        real(kind = real_wp), intent(out) :: consts(num_constants)                !< Array of constant values
        real(kind = real_wp), intent(out) :: param (num_spatial_parameters, num_cells + num_cells_bottom)   !< Array of parameter values
        real(kind = real_wp), intent(out) :: funcs (num_time_functions)                 !< Array of function values
        real(kind = real_wp), intent(out) :: sfuncs(num_cells + num_cells_bottom, num_spatial_time_fuctions) !< Array of segment functions
        integer(kind = int_wp), intent(in) :: ipoint(num_indices)                !< Set of pointers to destination
        character(len = *), intent(in) :: luntxt(*)                     !< text with the unit numbers
        character(len = 200), intent(in) :: luntx2(*)                     !< text with the binary files
        integer(kind = int_wp), intent(in) :: ftype (*)                     !< type of files to be opened
        integer(kind = int_wp), intent(in) :: intsrt                        !< integration option
        integer(kind = int_wp), intent(in) :: isflag                        !< = 1 then 'ddhhmmss' format
        integer(kind = int_wp), intent(inout) :: ifflag                        !< = 1 then first invocation
        integer(kind = int_wp), intent(in) :: ivflag                        !< = 1 then computed volumes
        integer(kind = int_wp), intent(in) :: ilflag                        !< = 0 then constant lengths
        logical, intent(inout) :: update                        !< TRUE if update took place
        integer(kind = int_wp), intent(inout) :: iktim (3)                     !< Timers in file
        integer(kind = int_wp), intent(inout) :: iknmrk(num_cells + num_cells_bottom)         !< Kenmerk array
        integer(kind = int_wp), intent(inout) :: inwspc(newisp)                !< Integer(kind=int_wp) ::space new time funs
        real(kind = real_wp), intent(inout) :: anwspc(newrsp)                !< Real(kind=real_wp) ::space new time functions
        integer(kind = int_wp), intent(in) :: inwtyp(num_boundary_conditions + num_waste_loads)         !< Types of items
        integer(kind = int_wp), intent(inout) :: iwork (*)                     !< Integer(kind=int_wp) ::workspace
        logical, intent(in) :: lstrec                        !< TRUE: last record on rewind wanted
        logical, intent(out) :: lrewin                        !< TRUE: rewind took place
        real(kind = real_wp), intent(inout) :: vollst(*)                     !< Last volume record before rewind
        logical, intent(in) :: rdvolu                        !< TRUE: also read volumes
        type(GridPointerColl), intent(inout) :: GridPs                        !< collection of all grid definitions
        type(delwaq_data), intent(inout) :: dlwqd                         !< derived type for persistent storage

        ! Local variables
        real(kind = real_wp) :: rdummy(1), adummy(1), adt   (1)
        logical      lstdum, lredum, ldum  (3)
        integer(kind = int_wp) :: iph, ipf, ipa, ipi, ipni, ipna           !  incremental pointers
        integer(kind = int_wp) :: isnul, isnul2, idummy                    !  dummy variables
        integer(kind = int_wp) :: nosss, it, nosubs, is                    !  helpvariables
        integer(kind = int_wp) :: ierr                                     !  error flag (not tested)

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("update_time_dependent_external_forcing", ithandl)

        ! open the harmonics and pointer files
        if (ifflag == 1) then
            bndset = .false.
            wstset = .false.
            funset = .false.
            othset = .false.
            call open_waq_files (file_unit_list(3), luntxt(3), 3, 2, ierr)
            call open_waq_files (file_unit_list(4), luntxt(4), 4, 2, ierr)
        endif

        ! initialisation
        iph = 1
        ipf = 1
        ipa = 1
        ipi = 1
        ipni = 1
        ipna = 1
        it = 1
        isnul = 0
        isnul2 = 0
        idummy = 0
        lstdum = .false.
        update = .false.

        ! integration step size IDT
        if (nrftot(1) > 0) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), adt, 1, nrharm(1), &
                    1, nrftot(1), ipa, iph, ipf, &
                    ipi, luntxt, 5, isflag, ifflag, &
                    update, othset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)

            if (othset) then
                is = 5
                goto 10
            endif
            idt = adt(1) + 0.5
        endif

        ! volumes

        ! if read anyway or ( read-requested and there is something to read )
        if (nrharm(2) >= 0) then
            if   (rdvolu) then
                ! if .not. computed volumes .or. this is the first time
                if (ivflag     == 0 .or. ifflag == 1) then
                    call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                            array(ipa), ipoint(ipi), volume, 1, nrharm(2), &
                            num_cells, nrftot(2), ipa, iph, ipf, &
                            ipi, luntxt, 7, isflag, ifflag, &
                            update, othset, 0, iwork, lstrec, &
                            lrewin, vollst, ftype, dlwqd)
                    ldum(1) = update
                    ldum(2) = othset
                    ldum(3) = lrewin

                    update = ldum(1)
                    othset = ldum(2)
                    lrewin = ldum(3)
                endif
            else
                ipa = ipa + nrftot(2) * 2
                ipi = ipi + num_cells + 3
            endif
            if (othset) then
                is = 7
                goto 10
            endif
        endif

        !         dispersions
        if (nrharm(3) >= 0) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), disper, num_dispersion_arrays, nrharm(3), &
                    num_exchanges, nrftot(3), ipa, iph, ipf, &
                    ipi, luntxt, 9, isflag, ifflag, &
                    update, othset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)

            if (othset) then
                is = 9
                goto 10
            endif
        endif

        ! area
        if (nrharm(4) >= 0) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), area, 1, nrharm(4), &
                    num_exchanges, nrftot(4), ipa, iph, ipf, &
                    ipi, luntxt, 10, isflag, ifflag, &
                    update, othset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)

            if (othset) then
                is = 10
                goto 10
            endif
        endif

        ! flow
        if (nrharm(5) >= 0) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), flow, 1, nrharm(5), &
                    num_exchanges, nrftot(5), ipa, iph, ipf, &
                    ipi, luntxt, 11, isflag, ifflag, &
                    update, othset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)

            if (othset) then
                is = 11
                goto 10
            endif
        endif

        ! velocities
        if (nrharm(6) >= 0) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), velo, num_velocity_arrays, nrharm(6), &
                    num_exchanges, nrftot(6), ipa, iph, ipf, &
                    ipi, luntxt, 12, isflag, ifflag, &
                    update, othset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)

            if (othset) then
                is = 12
                goto 10
            endif
        endif

        ! 'from'- and 'to'-length
        if (nrharm(7) >= 0 .and. ilflag == 1) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), aleng, 2, nrharm(7), &
                    num_exchanges, nrftot(7), ipa, iph, ipf, &
                    ipi, luntxt, 13, isflag, ifflag, &
                    update, othset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)
            if (othset) then
                is = 13
                goto 10
            endif
        endif

        ! boundaries
        if (intsrt == 6 .or. intsrt == 7) then
            nosubs = num_substances_total
        else
            nosubs = num_substances_transported
        endif
        if (nrharm(8) >= 0 .and. .not. bndset) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), bounds, nosubs, nrharm(8), &
                    num_boundary_conditions, nrftot(8), ipa, iph, ipf, &
                    ipi, luntxt, 14, isflag, ifflag, &
                    update, bndset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset
            ldum(3) = bndset

            update = ldum(1)
            othset = ldum(2)
            bndset = ldum(3)
        endif

        if (bndset) then
            call update_condition (file_unit_list, itime, itimel, inwspc(ipni), anwspc(ipna), &
                    adummy, inwtyp(it), bounds, nosubs, isnul2, &
                    num_boundary_conditions, isnul, ipni, ipna, idummy, &
                    ibndmx, luntxt, 14, isflag, ifflag, &
                    update, bndset, 0, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)

            it = it + num_boundary_conditions
        endif

        ! wastes
        if (nrharm(9) >= 0 .and. .not. wstset) then
            call update_condition (file_unit_list, itime, itimel, iharm(ipf), harmat(iph), &
                    array(ipa), ipoint(ipi), wastes, num_substances_total + 1, nrharm(9), &
                    num_waste_loads, nrftot(9), ipa, iph, ipf, &
                    ipi, luntxt, 15, isflag, ifflag, &
                    update, wstset, 1, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset
            ldum(3) = wstset

            update = ldum(1)
            othset = ldum(2)
            wstset = ldum(3)

        endif
        isnul = 0
        isnul2 = 0
        if (wstset) then
            call update_condition (file_unit_list, itime, itimel, inwspc(ipni), anwspc(ipna), &
                    adummy, inwtyp(it), wastes, num_substances_total + 1, isnul2, &
                    num_waste_loads, isnul, ipni, ipna, idummy, &
                    iwstmx, luntxt, 15, isflag, ifflag, &
                    update, wstset, 1, iwork, lstdum, &
                    lredum, rdummy, ftype, dlwqd)
            ldum(1) = update
            ldum(2) = othset

            update = ldum(1)
            othset = ldum(2)

            it = it + num_waste_loads
        endif

        ! functions
        nosss = num_cells + num_cells_bottom
        if (nrharm(10) >= 0) then
            call dlwqta (file_unit_list(16), luntxt(16), file_unit_list(19), nosss, num_constants, &
                    num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, consts, param, &
                    funcs, sfuncs, isflag, ifflag, itime, &
                    gridps, dlwqd, ierr)
        endif


        ! kenmerk array
        call update_attribute_array(file_unit_list, itime, iktim, iknmrk, nosss, &
                40, luntxt, isflag, ifflag, file_option_attributes)

        ! close the harmonics and pointer files
        10 if (ifflag == 1) then
            close (file_unit_list(3))
            close (file_unit_list(4))
            if (othset) then
                write (file_unit_list(19), *) ' error, new time series processing', &
                        ' wanted for an unsupported item: ', luntxt(is)
                call stop_with_error()
            endif
        endif

        itimel = itime
        if (timon) call timstop (ithandl)

    end subroutine update_time_dependent_external_forcing
end module m_time_dependent_variables
