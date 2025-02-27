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
module m_dlwqp1
    use m_waq_precision
    use m_wr_proceswrk
    use m_wrwrko
    use m_wrtoys
    use m_set_stat_output
    use m_set_old_items
    use m_set_fractions
    use m_set_active
    use m_set_grid_all_processes, only: set_grid_all_processes
    use m_setopp
    use m_setopo
    use m_setdvp
    use m_repuse
    use m_reaalg
    use m_rd_tabs
    use m_prsort
    use m_prprop
    use m_proc_totals
    use m_primpro
    use m_outbo2
    use m_makbar
    use m_getinv
    use m_error_status

    implicit none

contains

    subroutine dlwqp1(file_unit_list, file_name_list, &
                      statprocesdef, allitems, &
                      ioutps, outputs, &
                      nomult, imultp, &
                      constants, &
                      refday, status)
        !> Defines process steering for all water quality processing
        !>
        !> This routine processes all information of
        !>    - processes that have been switched on
        !>    - constants and functions that have been supplied
        !>    - output variables that have been asked to become available
        !>      to a consistent set of sequential processes for the simulation part

        use m_fill_old_items
        use m_cnfrep
        use m_blmeff
        use m_algrep
        use m_actrep
        use m_logger_helper, only: stop_with_error
        use m_working_files, only: read_working_file_4
        use m_cli_utils, only: get_command_argument_by_name, &
                               is_command_arg_specified
        use m_open_waq_files
        use timers
        use m_waq_data_structure
        use processet
        use results, only: OutputPointers
        use partable
        use string_module
        use m_waq_memory_dimensions          ! System characteristics
        use m_timer_variables          ! Timer characteristics

        implicit none

        ! declaration of arguments

        integer(kind=int_wp), intent(inout) :: file_unit_list(*)           !< unit numbers
        character(len=*), intent(inout) :: file_name_list(*)        !< filenames
        type(procespropcoll), intent(in) :: statprocesdef   !< the statistical proces definition
        type(itempropcoll), intent(inout) :: allitems        !< all items of the proces system
        integer(kind=int_wp), intent(inout) :: ioutps(7, *)      !< (old) output structure
        type(OutputPointers), intent(inout) :: outputs         !< output structure
        integer(kind=int_wp), intent(in) :: nomult           !< number of multiple substances
        integer(kind=int_wp), intent(in) :: imultp(2, nomult) !< multiple substance administration
        type(t_waq_item), intent(inout) :: constants       !< delwaq constants list
        integer(kind=int_wp), intent(in) :: refday           !< reference day, varying from 1 till 365

        type(error_status) :: status !< current error status

        ! local declarations

        real(kind=real_wp), parameter :: versip = 5.07   ! version process system
        real(kind=real_wp) :: verspe = 1.0    ! version bloom.spe file
        integer(kind=int_wp), parameter :: novarm = 15000   ! max number of variables overall
        integer(kind=int_wp), parameter :: nbprm = 1750    ! max number of processes

        integer(kind=int_wp) :: noqtt            ! total number of exhanges
        integer(kind=int_wp) :: nosss            ! total number of segments
        integer(kind=int_wp) :: no_in            ! number of input items
        integer(kind=int_wp) :: no_out           ! number of output items
        integer(kind=int_wp) :: no_ins           ! number of output items
        integer(kind=int_wp) :: no_ine           ! number of output items
        integer(kind=int_wp) :: no_ous           ! number of output items
        integer(kind=int_wp) :: no_oue           ! number of output items
        integer(kind=int_wp) :: no_flu           ! number of output items
        integer(kind=int_wp) :: no_sto           ! number of output items
        integer(kind=int_wp) :: no_dis           ! number of output items
        integer(kind=int_wp) :: no_vel           ! number of output items
        integer(kind=int_wp) :: noconm           ! number of constants plus some extra max
        integer(kind=int_wp) :: nocon2           ! number of constants plus some extra
        integer(kind=int_wp) :: nmis             ! number of missing items
        integer(kind=int_wp) :: maxdef           ! length defaul array

        integer(kind=int_wp) :: lurep            ! unit number report file
        integer(kind=int_wp) :: lunblm           ! unit number bloom file
        integer(kind=int_wp) :: lunfrm           ! unit number bloom frm file
        integer(kind=int_wp) :: lund09           ! unit number bloom d09 file

        integer(kind=int_wp) :: isys             ! index variable
        integer(kind=int_wp) :: igrp             ! index variable
        integer(kind=int_wp) :: iatyp            ! index variable
        integer(kind=int_wp) :: ialg             ! index variable
        integer(kind=int_wp) :: icof             ! index variable
        integer(kind=int_wp) :: ico              ! index variable
        integer(kind=int_wp) :: iconf            ! index variable
        integer(kind=int_wp) :: istat            ! index variable
        integer(kind=int_wp) :: iioitem          ! index variable
        integer(kind=int_wp) :: ioutp            ! index variable
        integer(kind=int_wp) :: i                ! index variable
        integer(kind=int_wp) :: iitem            ! index variable
        integer(kind=int_wp) :: iindx            ! index variable
        integer(kind=int_wp) :: ix_act           ! index variable
        integer(kind=int_wp) :: ix_dbl           ! index variable
        integer(kind=int_wp) :: ioff             ! offset for index item
        integer(kind=int_wp) :: ioffx            ! offset for index item on exchange
        integer(kind=int_wp) :: idef             ! offset to defualt items
        integer(kind=int_wp) :: iflx             ! offset to flux items
        integer(kind=int_wp) :: iret             ! return value
        integer(kind=int_wp) :: ierr2            ! error count
        integer(kind=int_wp) :: ierr_alloc       ! error
        integer(kind=int_wp) :: ierr_dalloc      ! error

        integer(kind=int_wp) :: idummy           ! dummy variable
        real(kind=real_wp) :: rdummy           ! dummy variable
        character :: cdummy            ! dummy variable
        logical :: parsing_error

        integer(kind=int_wp), allocatable :: idpnt(:)         ! dispersion pointers
        integer(kind=int_wp), allocatable :: ivpnt(:)         ! velocity pointers
        integer(kind=int_wp), allocatable :: grdref(:)        ! reference grid
        integer(kind=int_wp), allocatable :: sysgrd(:)        ! substance grid
        integer(kind=int_wp), allocatable :: sysndt(:)        ! substance timestep multiplier

        character(len=40) :: modid(4)       ! model id
        character(len=20), allocatable :: syname(:)       ! substance names
        character(len=20), allocatable :: coname(:)       ! constant names
        character(len=20), allocatable :: paname(:)       ! parameter names
        character(len=20), allocatable :: funame(:)       ! function names
        character(len=20), allocatable :: sfname(:)       ! segm.func. names
        character(len=20), allocatable :: diname(:)       ! dispersion names
        character(len=20), allocatable :: vename(:)       ! velocity names
        character(len=20), allocatable :: dename(:)       ! default array names
        character(len=20), allocatable :: locnam(:)       ! local array names
        character(len=20), allocatable :: ainame(:)       ! all item names names in the proc_def
        character(len=20) :: subname         ! substance name
        character(len=100), allocatable :: substdname(:)   ! substance standard name
        character(len=40), allocatable :: subunit(:)      ! substance unit
        character(len=60), allocatable :: subdescr(:)     ! substance description
        character(len=20) :: outname         ! output name

        ! proces definition structure

        type(procespropcoll) :: procesdef       ! the complete process definition
        integer(kind=int_wp) :: nbpr             ! number of processes
        integer(kind=int_wp) :: no_act           ! number of activated processes
        integer(kind=int_wp) :: serial           ! serial number process definition
        integer(kind=int_wp) :: target_serial    ! target serial number process definition
        real(kind=real_wp) :: versio           ! version process defintion
        character(len=20), allocatable :: actlst(:)

        ! proces "output" structure

        integer(kind=int_wp), pointer :: idpnw(:)
        integer(kind=int_wp), pointer :: ivpnw(:)
        real(kind=real_wp), pointer :: defaul(:)
        real(kind=real_wp), pointer :: dsto(:)
        real(kind=real_wp), pointer :: vsto(:)

        ! settings

        character(len=80) :: swinam
        character(len=80) :: blmnam
        character(len=80) :: line
        character(:), allocatable :: pdffil
        character(:), allocatable :: config
        logical :: lfound, laswi, swi_nopro
        integer(kind=int_wp) :: blm_act                        ! index of ACTIVE_BLOOM_P

        ! information

        character(len=20) :: rundat
        logical :: ex

        ! bloom-species database

        character(:), allocatable :: blmfil
        logical :: l_eco
        integer(kind=int_wp) :: maxtyp, maxcof
        parameter(maxtyp=500, maxcof=50)
        integer(kind=int_wp) :: notyp, nocof, nogrp
        character(len=10) :: alggrp(maxtyp), algtyp(maxtyp)
        character(len=5) :: abrgrp(maxtyp), abrtyp(maxtyp)
        character(len=80) :: algdsc(maxtyp)
        character(len=10) :: cofnam(maxcof)
        real(kind=real_wp) :: algcof(maxcof, maxtyp)
        integer(kind=int_wp) :: algact(maxtyp)
        integer(kind=int_wp) :: noutgrp, nouttyp
        character(len=10) :: outgrp(maxtyp), outtyp(maxtyp)
        integer(kind=int_wp) :: noprot, nopralg
        character(len=10) :: namprot(maxtyp), nampact(maxtyp), nampralg(maxtyp)

        ! actual algae

        integer(kind=int_wp) :: noalg
        character(len=10) :: name10
        character(len=10) :: grpnam(maxtyp)
        character(len=5) :: grpabr(maxtyp)
        character(len=10) :: typnam(maxtyp)
        character(len=5) :: typabr(maxtyp)

        ! output things

        character(len=20) :: parnam                    ! output parameter name
        integer(kind=int_wp) :: parindx                    ! index in output parameter name array
        integer(kind=int_wp), pointer :: proref(:, :)
        integer(kind=int_wp) :: nothread                   ! nr of threads

        ! old_items and replacent things

        type(old_item_coll) :: old_items        ! the old_items table

        ! performance timer

        integer(kind=int_wp) :: ithndl = 0
        if (timon) call timstrt("dlwqp1", ithndl)

        ! how many threads ?
        nothread = num_threads

        ! allocate

        allocate (actlst(nbprm))

        ! start

        file_name_list(34) = 'proc_def'
        num_local_vars = 0
        num_defaults = 0
        num_dispersion_arrays_extra = 0
        num_velocity_arrays_extra = 0
        num_local_vars_exchange = 0
        num_dispersion_arrays_new = 0
        num_velocity_arrays_new = 0
        noqtt = num_exchanges + num_exchanges_bottom_dir
        nosss = num_cells + num_cells_bottom
        procesdef%current_size = 0
        procesdef%maxsize = 0
        old_items%current_size = 0
        old_items%maxsize = 0

        ! open report file

        call open_waq_files(file_unit_list(35), file_name_list(35), 35, 1, ierr2)
        lurep = file_unit_list(35)
        line = ' '
        call set_log_unit_number(lurep)
        call write_log_message(line)

        ! command line settingen , commands

        ! active processes only switch

        if (is_command_arg_specified('-a')) then
            write (line, '(a)') ' found -a command line switch'
            call write_log_message(line)
            write (line, '(a)') ' only activated processes are switched on'
            call write_log_message(line)
            laswi = .true.
        else
            laswi = .false.
        end if

        ! no processes

        if (is_command_arg_specified('-np')) then
            swi_nopro = .true.
            write (line, '(a)') ' found -np command line switch'
            call write_log_message(line)
            write (line, '(a)') ' no processes from the process definition file are switched on'
            call write_log_message(line)
            versio = versip
        else
            swi_nopro = .false.
        end if

        ! process definition file

        if (.not. swi_nopro) then
            if (get_command_argument_by_name('-p', pdffil, parsing_error)) then
                if (parsing_error) then
                    pdffil = ' '
                end if
            else
                pdffil = ' '
            end if
            if (pdffil /= ' ') then
                file_name_list(34) = pdffil
                write (line, '(a)') ' found -p command line switch'
                call write_log_message(line)
            else
                pdffil = file_name_list(34)
            end if
            ierr2 = status%ierr
            call rd_tabs(pdffil, lurep, versio, serial, status)
            if (status%ierr > ierr2) then
                write (lurep, *) ' '
                write (lurep, *) ' ERROR: Could not read the process definition file.'
                write (lurep, *) '        Check if the filename after -p is correct, and exists.'
                write (lurep, *) '        Use -np if you want to run without processes.'
                write (lurep, *) ' '
                write (*, *) ' error opening nefis file(s):', trim(pdffil)
                write (*, *) ' '
                write (*, *) ' ERROR: Could not read the process definition file.'
                write (*, *) '        Check if the filename after -p is correct, and exists.'
                write (*, *) '        Use -np if you want to run without processes.'
                write (*, *) ' '
                call stop_with_error()
            else
                write (lurep, *)
                write (lurep, 2001) trim(file_name_list(34))
                write (lurep, 2002) versio
                write (lurep, 2003) serial
                write (lurep, *)

                ! fill the old_items conversion table

                call fill_old_items(old_items)
            end if
        end if

        ! old serial definitions

        if (.not. swi_nopro) then
            if (get_command_argument_by_name('-target_serial', target_serial, parsing_error)) then
                write (line, '(a)') ' found -target_serial command line switch'
                call write_log_message(line)
                if (parsing_error) then
                    old_items%target_serial = target_serial
                    write (line, '(a)') ' no serial number given, using current'
                    call write_log_message(line)
                    old_items%target_serial = serial
                else
                    write (line, '(a,i13)') ' using target serial number: ', target_serial
                    call write_log_message(line)
                    old_items%target_serial = target_serial
                end if
            else
                old_items%target_serial = serial
            end if
        end if

        ! configuration

        if (get_command_argument_by_name('-conf', config, parsing_error)) then
            write (line, '(a)') ' found -conf command line switch'
            call write_log_message(line)
            if (parsing_error) then
                write (line, '(a)') ' no configuration id given, using default'
                call write_log_message(line)
                config = ' '
            else
                write (line, '(a25,a10)') ' using configuration id: ', config
                call write_log_message(line)
            end if
        else
            config = ' '
        end if

        ! eco coupling

        if (get_command_argument_by_name('-eco', blmfil, parsing_error)) then
            l_eco = .true.
            line = ' '
            call write_log_message(line)
            write (line, '(a)') ' found -eco command line switch'
            call write_log_message(line)
            if (parsing_error) then
                blmfil = 'bloom.spe'
                write (line, '(a30,a50)') ' using default eco input file:', blmfil
                call write_log_message(line)
            else
                write (line, '(a22,a58)') ' using eco input file:', blmfil
                call write_log_message(line)
            end if
        else
            blmnam = 'ACTIVE_BLOOM_P'
            blm_act = constants%find(blmnam)
            if (blm_act > 0 .and. .not. swi_nopro) then
                l_eco = .true.
                line = ' '
                call write_log_message(line)
                write (line, '(a)') ' found constant ACTIVE_BLOOM_P without -eco command line switch'
                call write_log_message(line)
                blmfil = 'bloom.spe'
                write (line, '(a39,a41)') ' will try using default eco input file:', blmfil
                call write_log_message(line)
            else
                l_eco = .false.
                noprot = 0
                nopralg = 0
            end if
        end if

        ! read the bloom-species database.
        if (l_eco) then
            open (newunit=lunblm, file=blmfil, status='old', iostat=ierr2)
            if (ierr2 /= 0) then
                call status%increase_error_count()
                write (line, '(3a)') ' eco input file - ', trim(blmfil), ' not found! Exiting'
                call write_log_message(line)
                return
            end if

            read (lunblm, '(a)') line
            verspe = 1.0
            ioff = index(line, 'BLOOMSPE_VERSION_')
            if (ioff == 0) then
                rewind (lunblm)
            else
                read (line(ioff + 17:), *, err=100) verspe
100             continue
            end if

            call reaalg(lurep, lunblm, verspe, maxtyp, maxcof, &
                        notyp, nocof, noutgrp, nouttyp, alggrp, &
                        abrgrp, algtyp, abrtyp, algdsc, cofnam, &
                        algcof, outgrp, outtyp, noprot, namprot, &
                        nampact, nopralg, nampralg)
        end if

        ! check local dimensions

        allocate (idpnt(num_substances_total))
        allocate (ivpnt(num_substances_total))
        allocate (grdref(num_grids))
        allocate (sysgrd(num_substances_total))
        allocate (sysndt(num_substances_total))
        allocate (syname(num_substances_total))
        noconm = num_constants + 1000
        allocate (coname(noconm))
        allocate (paname(num_spatial_parameters))
        allocate (funame(num_time_functions))
        allocate (sfname(num_spatial_time_fuctions))
        allocate (diname(num_dispersion_arrays))
        allocate (vename(num_velocity_arrays))

        ! read ( rest ) of relevant delwaq files

        call open_waq_files(file_unit_list(2), file_name_list(2), 2, 2, ierr2)
        call read_working_file_4(file_unit_list(2), lurep, modid, syname, num_substances_total, &
                num_monitoring_points, num_substances_transported, num_boundary_conditions, num_waste_loads, num_constants, &
                num_spatial_parameters, num_cells, num_cells_bottom, coname, paname, &
                funame, num_time_functions, sfname, num_spatial_time_fuctions, num_dispersion_arrays, &
                num_velocity_arrays, diname, vename, idpnt, ivpnt, &
                ndmpar, ntdmpq, ntdmps, noqtt, num_transects, &
                num_transect_exchanges, num_boundary_types, num_waste_load_types, num_grids, grdref, &
                sysgrd, sysndt)
        write (lurep, 2020) (modid(i), i = 1, 2)
        write (lurep, 2030) (modid(i), i = 3, 4)
        close (file_unit_list(2))

        ! change names according to old_items table
        nocon2 = num_constants
        call set_old_items(lurep, old_items, num_substances_total, num_spatial_parameters, num_time_functions, &
                num_spatial_time_fuctions, num_dispersion_arrays, num_velocity_arrays, syname, paname, &
                funame, sfname, diname, vename, constants)

        ! replace proto with actual processes

        if (l_eco) then

            ! set algal type list, order is the (prescribed) order in the bloom database

            noalg = 0
            do ialg = 1, notyp
                name10 = algtyp(ialg)
                isys = index_in_array(name10, syname)
                if (isys > 0) then
                    noalg = noalg + 1
                    algact(ialg) = 1
                    typnam(noalg) = algtyp(ialg)
                    typabr(noalg) = abrtyp(ialg)
                else
                    algact(ialg) = 0
                end if
            end do

            ! when no algae were found, turn of eco mode
            if (noalg == 0) then
                write (line, '(a)') ' no BLOOM algae were found, switching off eco mode.'
                call write_log_message(line)
                l_eco = .false.
            else
                ! set algal group list
                nogrp = 0
                do iatyp = 1, notyp
                    if (algact(iatyp) == 1) then
                        igrp = index_in_array(alggrp(iatyp), grpnam)
                        if (igrp <= 0) then
                            nogrp = nogrp + 1
                            grpnam(nogrp) = alggrp(iatyp)
                            grpabr(nogrp) = abrgrp(iatyp)
                        end if
                    end if
                end do

                ! replace proto with actual processes in constant list
                call actrep(noalg, noprot, namprot, nampact, nopralg, nampralg, constants)
            end if
        end if

        ! active only switch set trough a constant

        swinam = 'only_active'
        ix_act = constants%find(swinam)
        if (ix_act > 0) then
            write (line, '(a)') ' found only_active constant'
            call write_log_message(line)
            write (line, '(a)') ' only activated processes are switched on'
            call write_log_message(line)
            laswi = .true.
        end if

        ! if active only make list of active processes

        no_act = 0
        if (laswi) then
            call set_active(constants, nbprm, no_act, actlst)
        end if

        ! if not active only and no configuration set default

        if (.not. laswi) then
            if (config == ' ') then
                if (l_eco) then
                    config = 'eco'
                else
                    config = 'waq'
                end if
                write (line, '(a,a10)') ' using default configuration: ', config
                call write_log_message(line)
            end if
        end if

        ! from nefis tables to proces definition structure

        if (.not. swi_nopro) then

            ! copy the configuration info for the eco proto processes to the actual processes

            if (l_eco) then
                call cnfrep(noalg, noprot, namprot, nampact, nopralg, nampralg)
            end if

            ! add the processes in the structure

            call prprop(lurep, laswi, config, no_act, actlst, allitems, procesdef, &
                        old_items, status)

            nbpr = procesdef%current_size
        else
            nbpr = 0
        end if

        ! add the statistical processes in the structure

        if (statprocesdef%current_size > 0) then
            do istat = 1, statprocesdef%current_size
                statprocesdef%procesprops(istat)%sfrac_type = 0
                iret = procespropcolladd(procesdef, statprocesdef%procesprops(istat))
                actlst(no_act + istat) = statprocesdef%procesprops(istat)%name
            end do
            nbpr = nbpr + statprocesdef%current_size
            no_act = no_act + statprocesdef%current_size
        end if

        ! set processes and fluxes for the substance fractions, this adds and alters processes in procesdef!

        call set_fraction(lurep, num_substances_total, syname, nomult, imultp, &
                procesdef, allitems, no_act, actlst, nbpr)

        ! sort processes according to input - output relation

        call prsort(lurep, procesdef, num_substances_total, num_spatial_parameters, num_spatial_time_fuctions, &
                syname, num_constants, num_time_functions, constants, paname, &
                funame, sfname, status)

        ! handle output from statistical processes

        call set_stat_output(statprocesdef, num_output_files, ioutps, num_output_variables_extra, outputs)

        ! set output boot dimensions, attention !!!!! is new char_arr_buffer_len written to work file?


        call outbo2(num_output_files, ioutps, nosss, num_monitoring_points, num_cells_u_dir, &
                num_cells_v_dir, num_output_variables_extra, output_buffer_len, ndmpar, num_substances_total, &
                char_arr_buffer_len, num_transects)

        ! replace names of bloom algea with actual names

        if (l_eco .and. nbpr > 0) then

            ! now replace process parameters

            call algrep(procesdef, notyp, nocof, algtyp, algact, &
                        abrtyp, cofnam, algcof, maxcof, alggrp, &
                        nogrp, grpnam, grpabr, nouttyp, outtyp, &
                        noutgrp, outgrp)

            ! write the bloom efficiency file

            open (newunit=lunfrm, file='bloominp.frm')
            call blmeff(lurep, lunblm, verspe, lunfrm, grpnam, nogrp, typnam, noalg)
            close (lunblm)
            close (lunfrm)
        end if

        ! calculate new totals

        call proc_totals(lurep, procesdef, no_ins, no_ine, no_ous, &
                         no_oue, no_flu, no_sto, no_dis, no_vel)

        ! set offset local array

        ioff = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + num_substances_total

        ! check which processes can be turned on

        call makbar(procesdef, num_substances_total, syname, num_constants, constants, &
                num_spatial_parameters, paname, num_time_functions, funame, num_spatial_time_fuctions, &
                sfname, num_dispersion_arrays, diname, num_velocity_arrays, vename, &
                noqtt, laswi, no_act, actlst, &
                status)
        deallocate (actlst, stat = ierr_dalloc)

        ! determine wich primary processes must be turned on

        ioffx = 4 + num_dispersion_arrays + num_velocity_arrays + num_time_functions + num_constants
        allocate (idpnw(num_substances_total))
        allocate (ivpnw(num_substances_total))
        allocate (dsto(num_substances_transported * no_dis))
        allocate (vsto(num_substances_transported * no_vel))
        idpnw = 0
        ivpnw = 0
        dsto = 0.0
        vsto = 0.0
        call primpro(procesdef, num_substances_total, syname, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                ioffx, num_substances_transported, dsto, vsto, num_dispersion_arrays_new, &
                idpnw, num_velocity_arrays_new, ivpnw, noqtt, &
                status)

        ! determine wich processes must be turned on for output purposes

        call setopp(procesdef, outputs, ioff)

        ! set pointers to input variables and output variables, if nessacary turn processes on.

        nmis = 0
        num_local_vars = 1
        num_local_vars_exchange = 0
        num_defaults = nopred
        maxdef = num_defaults + no_ins + no_ine
        allocate (defaul(maxdef))
        allocate (dename(maxdef))
        defaul = 0.0
        defaul(5) = real(itstrt)
        defaul(6) = real(itstop)
        allocate (locnam(novarm))

        ! put theta in local array if wanted for output, the value will be filled by the integration routine
        ! num_local_vars is already 1?, use this space!

        parnam = 'theta'
        parindx = index_in_array(parnam, outputs%names)
        if (parindx > 0 .and. (intsrt == 21 .or. intsrt == 22)) then
            locnam(1) = parnam
            outputs%pointers(parindx) = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + num_substances_total + 1
            write (line, '(3a)') ' output [', parnam, '] will be generated by numerical scheme'
            call write_log_message(line)
        end if

        call getinv(procesdef, num_substances_total, syname, num_constants, constants, &
                num_spatial_parameters, paname, num_time_functions, funame, num_spatial_time_fuctions, &
                sfname, num_dispersion_arrays, diname, num_velocity_arrays, vename, &
                nmis, defaul, num_local_vars, num_defaults, dename, outputs, &
                num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, locnam, refday)

        ! report on the use of the delwaq input

        call repuse(procesdef, num_constants, coname, num_spatial_parameters, paname, &
                num_time_functions, funame, num_spatial_time_fuctions, sfname, status%noinfo)

        ! a table will be made on selected processes
        ! to ensure resolved inputs with parallel processing
        call partab(procesdef, num_substances_total, syname, num_constants, constants, &
                num_spatial_parameters, paname, num_time_functions, funame, num_spatial_time_fuctions, &
                sfname, proref, num_input_ref, status, nothread, &
                nopred, num_local_vars, num_defaults)

        ! set output pointers to process arrays parloc and defaul

        idef = ioff + num_local_vars
        iflx = idef + num_defaults
        call setopo(procesdef, outputs, ioff, idef, iflx, status)

        ! if not all input present , stop with exit code

        if (nmis > 0) then
            call open_waq_files(file_unit_list(24), file_name_list(24), 24, 1, ierr2)
            close (file_unit_list(24))
            write (lurep, *) ' not all input available.'
            write (lurep, *) ' number off missing variables :', nmis
            write (lurep, *) ' simulation impossible.'
            call stop_with_error()
        end if

        ! set new pointer for dispersion and velocity

        call setdvp(num_dispersion_arrays, idpnt, num_dispersion_arrays_new, idpnw, num_substances_transported, num_dispersion_arrays_extra, dsto)
        call setdvp(num_velocity_arrays, ivpnt, num_velocity_arrays_new, ivpnw, num_substances_transported, num_velocity_arrays_extra, vsto)

        ! set grid for processes

        call set_grid_all_processes(procesdef, num_grids, num_substances_total, grdref, sysgrd, sysndt)
        deallocate (grdref, sysgrd, sysndt)

        ! write proces work file
        call wr_proceswrk(lurep, procesdef, num_defaults, defaul, idpnw, &
                ivpnw, dsto, vsto, locnam, nopred, &
                num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, num_substances_total, &
                num_local_vars, num_dispersion_arrays, num_velocity_arrays, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                num_local_vars_exchange, num_substances_transported, num_grids, dename, coname, paname, &
                funame, sfname, syname, intopt, file_unit_list, &
                file_name_list, num_output_files, ioutps, outputs, ndmpar, &
                output_buffer_len, versio, num_dispersion_arrays_new, num_velocity_arrays_new, num_input_ref, &
                proref, num_processes_activated, num_fluxes, num_vars, process_space_int_len)
        deallocate (defaul, dsto, vsto)
        deallocate (idpnw, ivpnw)
        deallocate (locnam)

        ! num_output_variables_extra is in the boot sysn common

        num_output_variables_extra = outputs%current_size

        ! Prepare descrtion and unit information for output from the proces library to be written in the NetCDF-file

        ! Extract names list from allitems
        allocate (ainame(allitems%current_size))
        do iitem = 1, allitems%current_size
            ainame(iitem) = allitems%itemproppnts(iitem)%pnt%name
        end do

        ! Get location of FixAlg in algcof
        name10 = 'FixAlg'
        icof = index_in_array(name10, cofnam)

        ! Get information about the substances
        allocate (substdname(num_substances_total))
        allocate (subunit(num_substances_total))
        allocate (subdescr(num_substances_total))
        do isys = 1, num_substances_total
            subname = syname(isys)
            call str_lower(subname)
            iindx = index_in_array(subname, ainame)
            if (iindx > 0) then
                substdname(isys) = allitems%itemproppnts(iindx)%pnt%stdn
                subunit(isys) = allitems%itemproppnts(iindx)%pnt%stdu
                subdescr(isys) = trim(allitems%itemproppnts(iindx)%pnt%text)//' '//allitems%itemproppnts(iindx)%pnt%unit
                if (substdname(isys) == ' ') then
                    substdname(isys) = allitems%itemproppnts(iindx)%pnt%text
                end if
                if (subunit(isys) == ' ') then
                    subunit(isys) = allitems%itemproppnts(iindx)%pnt%unit
                end if
            else
                ! Is it an algae?
                ialg = index_in_array(subname(1:10), algtyp)
                if (ialg > 0) then
                    if (algcof(icof, ialg) >= 0) then
                        substdname(isys) = ' '
                        subunit(isys) = 'g m-3'
                        subdescr(isys) = algdsc(ialg)//' (gC/m3)'
                    else
                        substdname(isys) = ' '
                        subunit(isys) = 'g m-2'
                        subdescr(isys) = algdsc(ialg)//' (gC/m2)'
                    end if
                else
                    substdname(isys) = ' '
                    subunit(isys) = ' '
                    subdescr(isys) = syname(isys)
                end if
            end if
        end do

        ! Lookup output names in names list
        do ioutp = 1, outputs%current_size
            outname = outputs%names(ioutp)
            call str_lower(outname)
            iindx = index_in_array(outname, ainame)
            if (iindx > 0) then
                outputs%std_var_name(ioutp) = allitems%itemproppnts(iindx)%pnt%stdn
                outputs%units(ioutp) = allitems%itemproppnts(iindx)%pnt%stdu
                outputs%description(ioutp) = trim(allitems%itemproppnts(iindx)%pnt%text)//' '//allitems%itemproppnts(iindx)%pnt%unit
            else if (outname == 'theta') then
                outputs%std_var_name(ioutp) = ' '
                outputs%units(ioutp) = ' '
                outputs%description(ioutp) = 'Local-theta, generated by numerical scheme (-)'
            else
                ! Is it an algae?
                ialg = index_in_array(outname(1:10), algtyp)
                if (ialg > 0) then
                    if (algcof(icof, ialg) >= 0) then
                        outputs%std_var_name(ioutp) = ' '
                        outputs%units(ioutp) = 'g m-3'
                        outputs%description(ioutp) = trim(algdsc(ialg))//' (gC/m3)'
                    else
                        outputs%std_var_name(ioutp) = ' '
                        outputs%units(ioutp) = 'g m-2'
                        outputs%description(ioutp) = trim(algdsc(ialg))//' (gC/m2)'
                    end if
                else
                    outputs%std_var_name(ioutp) = ' '
                    outputs%units(ioutp) = ' '
                    outputs%description(ioutp) = outputs%names(ioutp)
                end if
            end if
        end do
        ! write updated output work file ( output.wrk )

        call open_waq_files(file_unit_list(25), file_name_list(25), 25, 1, ierr2)
        call wrwrko(file_unit_list(25), num_output_files, output_buffer_len, ioutps, outputs, &
                num_substances_total, substdname, subunit, subdescr)
        close (file_unit_list(25))

        ! write altoys input files, only for old balance file
        ! ( altoys.inp batoys.inp altoys.ini altoys.fil)

        if (btest(intopt, 3) .and. .not. btest(intopt, 4)) then
            call wrtoys(file_name_list, file_unit_list, num_substances_total, syname, num_output_files, ioutps, outputs)
        end if

        if (timon) call timstop(ithndl)
        return
2001    format(' Using process definition file : ', a)
2002    format(' Version number                : ', f10.2)
2003    format(' Serial                        : ', i10)
2020    format(//' Model :            ', a40, /20x, a40)
2030    format(//' Run   :            ', a40, /20x, a40//)
    end subroutine dlwqp1

end module m_dlwqp1
