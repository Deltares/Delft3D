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
module m_wq_processes_initialise
    use m_waq_precision
    use m_string_utils, only : index_in_array

    implicit none

contains


    subroutine wq_processes_initialise (lunlsp, pdffil, shared_dll_so, blmfil, blmoutfil, sttfil, statprocesdef, outputs, &
            nomult, imultp, constants, refday, noinfo, nowarn, ierr)

        !       Deltares Software Centre

        !>\file
        !>                          Defines process steering for all water quality processing
        !>
        !>                          This routine processes all information of
        !>                             - processes that have been switched on
        !>                             - constants and functions that have been supplied
        !>                             - output variables that have been asked to become available
        !>                             .
        !>                          to a consistent set of sequential processes for the simulation part

        use m_set_old_items
        use m_set_fractions
        use m_set_active
        use m_setvat
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
        use m_makbar
        use m_intoou
        use m_getinv
        use m_fill_old_items
        use m_cnfrep
        use m_blmeff
        use m_algrep
        use m_actrep
        use m_dattim
        use m_srstop
        use m_rd_stt
        use m_monsys
        use m_getidentification
        use m_cli_utils, only : retrieve_command_argument
        use processes_input
        use processes_pointers
        use process_registration

        use dlwq_hyd_data
        use date_time_utils, only : simulation_start_time_scu, simulation_stop_time_scu, system_time_factor_seconds, &
                base_julian_time
        use bloom_data_io, only : runnam
        use processet
        use results, only : OutputPointers
        use string_module
        use m_alloc
        use timers
        use m_error_status

        implicit none


        ! declaration of arguments

        integer(kind = int_wp), intent(inout) :: lunlsp           !< unit number spe
        character(len = *), intent(in) :: pdffil          !< filename proc_def
        character(len = *), intent(in) :: shared_dll_so      !< name of the open processes library dll/so to be loaded during runtime
        character(len = *), intent(inout) :: blmfil          !< filename spe
        character(len = *), intent(in) :: blmoutfil       !< base name for bloom output files
        character(len = *), intent(inout) :: sttfil          !< filename stt

        type(procespropcoll), intent(inout) :: statprocesdef   !< the statistical proces definition
        type(OutputPointers), intent(inout) :: outputs         !< output structure
        character(len = 20) :: statproc        !< name of statistics proces
        character(len = 20) :: statname        !< name of stat output variable
        integer(kind = int_wp) :: statival         !< pointer in waq arrays of stat output
        integer(kind = int_wp), intent(in) :: nomult           !< number of multiple substances
        integer(kind = int_wp), intent(in) :: imultp(2, nomult) !< multiple substance administration
        type(t_dlwq_item), intent(inout) :: constants       !< delwaq constants list
        integer(kind = int_wp), intent(inout) :: noinfo           !< count of informative message
        integer(kind = int_wp), intent(inout) :: nowarn           !< count of warnings
        integer(kind = int_wp), intent(inout) :: ierr             !< error count

        ! local declarations
        type(itempropcoll) :: allitems        !< all items of the proces system

        real(kind = real_wp) :: verspe = 1.0    ! version bloom.spe file
        integer(kind = int_wp), parameter :: novarm = 15000   ! max number of variables overall
        integer(kind = int_wp), parameter :: nbprm = 1750    ! max number of processes
        integer(kind = int_wp), parameter :: nopred = 6       ! number of pre-defined variables
        integer(kind = int_wp) :: open_shared_library

        integer(kind = int_wp) :: noqtt            ! total number of exhanges
        integer(kind = int_wp) :: no_ins           ! number of output items
        integer(kind = int_wp) :: no_ine           ! number of output items
        integer(kind = int_wp) :: no_ous           ! number of output items
        integer(kind = int_wp) :: no_oue           ! number of output items
        integer(kind = int_wp) :: no_sto           ! number of output items
        integer(kind = int_wp) :: no_dis           ! number of output items
        integer(kind = int_wp) :: no_vel           ! number of output items
        integer(kind = int_wp) :: nocon2           ! number of constants plus some extra
        integer(kind = int_wp) :: nmis             ! number of missing items
        integer(kind = int_wp) :: maxdef           ! length defaul array

        integer(kind = int_wp) :: lunblm           ! unit number bloom file
        integer(kind = int_wp) :: lunfrm           ! unit number bloom frm file

        integer(kind = int_wp) :: isys             ! index variable
        integer(kind = int_wp) :: igrp             ! index variable
        integer(kind = int_wp) :: iatyp            ! index variable
        integer(kind = int_wp) :: ialg             ! index variable
        integer(kind = int_wp) :: icof             ! index variable
        integer(kind = int_wp) :: istat            ! index variable
        integer(kind = int_wp) :: ioutp            ! index variable
        integer(kind = int_wp) :: iitem            ! index variable
        integer(kind = int_wp) :: iproc            ! index variable
        integer(kind = int_wp) :: iindx            ! index variable
        integer(kind = int_wp) :: ix_act           ! index variable
        integer(kind = int_wp) :: ioff             ! offset for index item
        integer(kind = int_wp) :: ioffx            ! offset for index item on exchange
        integer(kind = int_wp) :: idef             ! offset to defualt items
        integer(kind = int_wp) :: iflx             ! offset to flux items
        integer(kind = int_wp) :: nflx             ! offset to flux items
        integer(kind = int_wp) :: ifluxsys         ! index of flux items
        integer(kind = int_wp) :: istochi          ! offset to stochi
        integer(kind = int_wp) :: mxpmsa           ! maximum size of ipmsa (=max nr of input variables)
        integer(kind = int_wp) :: iret             ! return value
        integer(kind = int_wp) :: ierr2            ! error count

        integer(kind = int_wp) :: idummy           ! dummy variable
        real(kind = real_wp) :: rdummy           ! dummy variable
        character :: cdummy          ! dummy variable

        character*20, allocatable :: ainame(:)       ! all item names names in the proc_def
        character*20 :: subname         ! substance name
        character*100, allocatable :: substdname(:)   ! substance standard name
        character*40, allocatable :: subunit(:)      ! substance unit
        character*60, allocatable :: subdescr(:)     ! substance description
        character*20 :: outname         ! output name
        integer(kind = int_wp), intent(in) :: refday           ! reference day, varying from 1 till 365

        type(error_status) :: main_status
        type(error_status) :: temp_status

        ! proces definition structure

        type(procespropcoll) :: procesdef       ! the complete process definition
        type(procesprop), pointer :: proc            ! process description
        type(arrayprop) :: aarrayprop      !  one array property to add into collection
        real(kind = real_wp) :: scale            ! stochi factor
        character(len = 20) :: flxnam          ! output buffer
        integer(kind = int_wp) :: nbpr             ! number of processes
        integer(kind = int_wp) :: no_act           ! number of activated processes
        integer(kind = int_wp) :: serial           ! serial number process definition
        integer(kind = int_wp) :: target_serial    ! target serial number process definition
        real(kind = real_wp) :: versio           ! version process defintion
        character*20, allocatable :: actlst(:)

        ! settings

        character*80   swinam
        character*80   blmnam
        character*80   line
        character*80   idstr
        character*20   rundat
        character*10   config
        logical        lfound, laswi, swi_nopro
        integer(kind = int_wp) :: blm_act                        ! index of ACTIVE_BLOOM_P

        ! information

        logical        ex

        ! bloom-species database

        logical        l_eco
        integer(kind = int_wp) :: maxtyp, maxcof
        parameter   (maxtyp = 500, maxcof = 50)
        integer(kind = int_wp) :: notyp, nocof, nogrp
        character*10  alggrp(maxtyp), algtyp(maxtyp)
        character*5   abrgrp(maxtyp), abrtyp(maxtyp)
        character*80  algdsc(maxtyp)
        character*10  cofnam(maxcof)
        real(kind = real_wp) :: algcof(maxcof, maxtyp)
        integer(kind = int_wp) :: algact(maxtyp)
        integer(kind = int_wp) :: noutgrp, nouttyp
        character*10  outgrp(maxtyp), outtyp(maxtyp)
        integer(kind = int_wp) :: noprot, nopralg
        character*10  namprot(maxtyp), nampact(maxtyp), nampralg(maxtyp)
        character(256) filnam       ! File name with extention

        ! actual algae

        integer(kind = int_wp) :: noalg
        character*10  name10
        character*10  grpnam(maxtyp)
        character*5   grpabr(maxtyp)
        character*10  typnam(maxtyp)
        character*5   typabr(maxtyp)

        ! output things

        character(len = 20) :: parnam                    ! output parameter name
        integer(kind = int_wp) :: parindx                    ! index in output parameter name array

        ! old_items and replacent things

        type(old_item_coll) :: old_items        ! the old_items table

        integer(kind = int_wp), save :: ithndl = 0
        if (timon) call timstrt("wq_processes_initialise", ithndl)

        call main_status%initialize(0, nowarn, noinfo)
        call temp_status%initialize(0, 0, 0)

        ! allocate

        call realloc(actlst, nbprm, keepExisting = .false., Fill = ' ')

        ! start

        noloc = 0
        nodef = 0
        ndspx = 0
        nvelx = 0
        nlocx = 0
        ndspn = 0
        nveln = 0
        noqtt = 1
        !      nosss  = noseg + nseg2
        allitems%cursize = 0
        allitems%maxsize = 0
        procesdef%cursize = 0
        procesdef%maxsize = 0
        old_items%cursize = 0
        old_items%maxsize = 0

        ! open report file

        ! Header for lsp
        call getidentification(idstr)
        write(lunlsp, '(XA/)') idstr
        call dattim(rundat)
        write (lunlsp, '(A,A/)') ' Execution start: ', rundat

        ! Active/inactive substance list
        write (lunlsp, 2080) nosys, notot - nosys, notot
        write (lunlsp, 2100)
        do isys = 1, nosys
            write(lunlsp, 2110) isys, '  active      ', syname_sub(isys)
        end do
        do isys = nosys + 1, notot
            write(lunlsp, 2110) isys, '  inactive    ', syname_sub(isys)
        end do
        write(lunlsp, '(/)')
        ! command line settingen , commands

        ! monitoring level
        call setmmo (10)

        ! active processes only switch
        ! only activated processes are switched on
        laswi = .true.

        ! initialise statistical processes
        statprocesdef%cursize = 0
        statprocesdef%maxsize = 0
        if (sttfil/=' ') then
            simulation_start_time_scu = itstrt_process
            simulation_stop_time_scu = itstop_process
            system_time_factor_seconds = isfact
            base_julian_time = otime

            write(lunlsp, *) ' '
            write(lunlsp, *) ' Reading statistics definition file: ', trim(sttfil)
            call rd_stt(lunlsp, sttfil, statprocesdef, allitems, temp_status)
            if (temp_status%ierr /= 0) then
                write(lunlsp, *) ' ERROR: Could not read the statistics definition file.'
                write(*, *) ' ERROR: Could not read the statistics definition file.'
                call main_status%increase_error_count()
                call main_status%sync(ierr, nowarn, noinfo)
                return
            else
            endif
        endif

        ! read process definition file

        call rd_tabs(pdffil, lunlsp, versio, serial, temp_status)

        if (temp_status%ierr /= 0) then
            write(lunlsp, *) ' '
            write(lunlsp, *) ' ERROR: Could not read the process definition file.'
            write(lunlsp, *) '        Check if the filename after -p is correct, and exists.'
            write(lunlsp, *) '        Use -np if you want to run without processes.'
            write(lunlsp, *) ' '
            write(*, *) ' error opening nefis file(s):', trim(pdffil)
            write(*, *) ' '
            write(*, *) ' ERROR: Could not read the process definition file.'
            write(*, *) '        Check if the filename after -p is correct, and exists.'
            write(*, *) '        Use -np if you want to run without processes.'
            write(*, *) ' '
            call main_status%increase_error_count()
            call main_status%sync(ierr, nowarn, noinfo)
            return
        else
            write (lunlsp, *)
            write (lunlsp, 2001) trim(pdffil)
            write (lunlsp, 2002) versio
            write (lunlsp, 2003) serial
            write (lunlsp, *)

            ! fill the old_items conversion table

            call fill_old_items(old_items)
        endif

        ! open openpb dll

        if (shared_dll_so/=' ') then
            dll_opb = 0 ! in C this one could be 4 or 8 bytes, so make sure the last bytes are zero
            ierr2 = open_shared_library(dll_opb, shared_dll_so)
            if (ierr2 /= 0) then
                write(lunlsp, *) 'ERROR: opening open process library dll/so: ', trim(shared_dll_so)
                write(lunlsp, *) 'Try specifying the full path'
                ierr = ierr + 1
            else
                write(lunlsp, *) 'Successfully loaded open process library dll/so: ', trim(shared_dll_so)
            endif
        else
            write(lunlsp, *) 'No open process library dll/so specified'
        endif
        write(lunlsp, *) ' '

        ! old serial definitions
        swi_nopro = .false.
        if (.not. swi_nopro) then
            call retrieve_command_argument ('-target_serial', 1, lfound, target_serial, rdummy, cdummy, ierr2)
            if (lfound) then
                write(line, '(a)') ' found -target_serial command line switch'
                call monsys(line, 1)
                if (ierr2/= 0) then
                    old_items%target_serial = target_serial
                    write(line, '(a)')' no serial number given, using current'
                    call monsys(line, 1)
                    old_items%target_serial = serial
                else
                    write(line, '(a,i13)') ' using target serial number: ', target_serial
                    call monsys(line, 1)
                    old_items%target_serial = target_serial
                endif
            else
                old_items%target_serial = serial
            endif
        endif

        ! configuration

        call retrieve_command_argument ('-conf', 3, lfound, idummy, rdummy, config, ierr2)
        if (lfound) then
            write(line, '(a)') ' found -conf command line switch'
            call monsys(line, 1)
            if (ierr2/= 0) then
                write(line, '(a)')' no configuration id given, using default'
                call monsys(line, 1)
                config = ' '
            else
                write(line, '(a25,a10)') ' using configuration id: ', config
                call monsys(line, 1)
            endif
        else
            config = ' '
        endif

        ! eco coupling

        l_eco = blmfil /= ' '

        if (.not.l_eco) then
            blmnam = 'ACTIVE_BLOOM_P'
            blm_act = dlwq_find(constants, blmnam)
            if (blm_act > 0 .and. .not.swi_nopro) then
                blmfil = 'bloom.spe'
                inquire(file = blmfil, exist = l_eco)
                if (l_eco) then
                    line = ' '
                    call monsys(line, 1)
                    write(line, '(a)') ' found constant ACTIVE_BLOOM_P without -eco command line switch'
                    call monsys(line, 1)
                    write(line, '(a)') ' and found default file bloom.spe. Will using default BLOOM file.'
                    call monsys(line, 1)
                else
                    l_eco = .false.
                    noprot = 0
                    nopralg = 0
                endif
            else
                l_eco = .false.
                noprot = 0
                nopralg = 0
            endif
        endif
        ! read the bloom-species database.

        if (l_eco) then
            write (lunlsp, 2004) trim(blmfil)
            open (newunit = lunblm, file = blmfil)
            read (lunblm, '(a)') line
            verspe = 1.0
            ioff = index(line, 'BLOOMSPE_VERSION_')
            if(ioff==0) then
                rewind(lunblm)
            else
                read (line(ioff + 17:), *, err = 100) verspe
                100         continue
            endif

            call reaalg (lunlsp, lunblm, verspe, maxtyp, maxcof, &
                    notyp, nocof, noutgrp, nouttyp, alggrp, &
                    abrgrp, algtyp, abrtyp, algdsc, cofnam, &
                    algcof, outgrp, outtyp, noprot, namprot, &
                    nampact, nopralg, nampralg)
        endif

        ! check local dimensions

        call realloc(idpnt, notot, keepExisting = .false., Fill = 0)
        call realloc(ivpnt, notot, keepExisting = .false., Fill = 0)

        ! change names according to old_items table

        nocon2 = nocons
        call set_old_items(lunlsp, old_items, notot, nopa, nofun, &
                nosfun, nodisp, novelo, syname, paname, &
                funame, sfunname, diname, vename, constants)

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
                endif
            enddo

            ! when no algae were found, turn of eco mode
            if (noalg == 0) then
                write(line, '(a)') ' no BLOOM algae were found, switching off eco mode.'
                call monsys(line, 1)
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
                        endif
                    endif
                enddo

                ! replace proto with actual processes in constant list
                call actrep(noalg, noprot, namprot, nampact, nopralg, nampralg, constants)
            endif
        endif

        ! active only switch set trough a constant

        swinam = 'only_active'
        ix_act = dlwq_find(constants, swinam)
        if (ix_act > 0) then
            write(line, '(a)') ' found only_active constant'
            call monsys(line, 1)
            write(line, '(a)') ' only activated processes are switched on'
            call monsys(line, 1)
            laswi = .true.
        endif

        ! if active only make list of active processes

        no_act = 0
        if (laswi) then
            call set_active(constants, nbprm, no_act, actlst)
        endif

        ! if not active only and no configuration set default

        if (.not. laswi) then
            if (config == ' ') then
                if (l_eco) then
                    config = 'eco'
                else
                    config = 'waq'
                endif
                write(line, '(a,a10)') ' using default configuration: ', config
                call monsys(line, 1)
            endif
        endif

        ! from nefis tables to proces definition structure

        if (.not. swi_nopro) then

            ! copy the configuration info for the eco proto processes to the actual processes

            if (l_eco) then
                call cnfrep(noalg, noprot, namprot, nampact, nopralg, nampralg)
            endif

            ! add the processes in the strucure

            call prprop (lunlsp, laswi, config, no_act, actlst, allitems, procesdef, &
                    old_items, temp_status)
            if (temp_status%ierr /= 0) then
                call main_status%increase_error_count()
                temp_status%ierr = 0
            end if
            nbpr = procesdef%cursize

        else
            nbpr = 0
        endif

        ! add the statistical processes in the structure

        if (statprocesdef%cursize > 0) then
            do istat = 1, statprocesdef%cursize
                statprocesdef%procesprops(istat)%sfrac_type = 0
                iret = procespropcolladd(procesdef, statprocesdef%procesprops(istat))
                actlst(no_act + istat) = statprocesdef%procesprops(istat)%name
            enddo
            nbpr = nbpr + statprocesdef%cursize
            no_act = no_act + statprocesdef%cursize
        endif

        ! set processes and fluxes for the substance fractions, this adds and alters processes in procesdef!

        call set_fraction(lunlsp, notot, syname, nomult, imultp, procesdef, allitems, no_act, actlst, nbpr)

        ! sort processes according to input - output relation

        call prsort (lunlsp, procesdef, notot, nopa, nosfun, &
                syname, nocons, nofun, constants, paname, &
                funame, sfunname, main_status)

        ! handle output from statistical processes

        noout_statt = 0
        noout_state = 0
        !     first statistics with temporal output
        if (statprocesdef%cursize > 0) then
            do istat = 1, statprocesdef%cursize
                do iitem = 1, statprocesdef%procesprops(istat)%no_output
                    if (statprocesdef%procesprops(istat)%output_item(iitem)%type == iotype_segment_output) then
                        statproc = statprocesdef%procesprops(istat)%routine
                        if (statproc=='STADAY'.or.statproc=='STADPT') then
                            statname = statprocesdef%procesprops(istat)%output_item(iitem)%name
                            noout = outputs%cursize + 1
                            noout_statt = noout_statt + 1
                            call reallocP(outputs%names, noout, keepExisting = .true., fill = statname)
                            call reallocP(outputs%std_var_name, noout, keepExisting = .true., fill = ' ')
                            call reallocP(outputs%pointers, noout, keepExisting = .true., fill = -1)
                            call reallocP(outputs%units, noout, keepExisting = .true., fill = ' ')
                            call reallocP(outputs%description, noout, keepExisting = .true., fill = ' ')
                            outputs%cursize = noout
                        endif
                    endif
                enddo
            enddo
        endif
        !     then statistics with end output
        if (statprocesdef%cursize > 0) then
            do istat = 1, statprocesdef%cursize
                do iitem = 1, statprocesdef%procesprops(istat)%no_output
                    if (statprocesdef%procesprops(istat)%output_item(iitem)%type == iotype_segment_output) then
                        statproc = statprocesdef%procesprops(istat)%routine
                        if (.not.(statproc=='STADAY'.or.statproc=='STADPT')) then
                            statname = statprocesdef%procesprops(istat)%output_item(iitem)%name
                            noout = outputs%cursize + 1
                            noout_state = noout_state + 1
                            call reallocP(outputs%names, noout, keepExisting = .true., fill = statname)
                            call reallocP(outputs%std_var_name, noout, keepExisting = .true., fill = ' ')
                            call reallocP(outputs%pointers, noout, keepExisting = .true., fill = -1)
                            call reallocP(outputs%units, noout, keepExisting = .true., fill = ' ')
                            call reallocP(outputs%description, noout, keepExisting = .true., fill = ' ')
                            outputs%cursize = noout
                        endif
                    endif
                enddo
            enddo
        endif

        ! replace names of bloom algea with actual names
        if (l_eco .and. nbpr > 0) then

            ! now replace process parameters

            call algrep (procesdef, notyp, nocof, algtyp, algact, &
                    abrtyp, cofnam, algcof, maxcof, alggrp, &
                    nogrp, grpnam, grpabr, nouttyp, outtyp, &
                    noutgrp, outgrp)

            runnam = blmoutfil

            ! write the bloom efficiency file
            filnam = trim(runnam) // '.frm'
            open (newunit = lunfrm, file = filnam)
            call blmeff (lunlsp, lunblm, verspe, lunfrm, grpnam, nogrp, typnam, noalg)
            close(lunblm)
            close(lunfrm)
        endif

        ! calculate new totals

        call proc_totals(lunlsp, procesdef, no_ins, no_ine, no_ous, &
                no_oue, no_flu, no_sto, no_dis, no_vel)

        ! set offset local array

        ioff = nopred + nocons + nopa + nofun + nosfun + notot

        ! check which processes can be turned on

        call makbar (procesdef, notot, syname, nocons, constants, &
                nopa, paname, nofun, funame, nosfun, &
                sfunname, nodisp, diname, novelo, vename, &
                noqtt, laswi, no_act, actlst, &
                temp_status)

        if (temp_status%ierr /= 0) then
            call main_status%increase_error_count()
            temp_status%ierr = 0
        end if
        deallocate(actlst)

        ! determine wich primary processes must be turned on

        ioffx = 4 + nodisp + novelo + nofun + nocons
        call realloc(idpnw, notot, keepExisting = .false., Fill = 0)
        call realloc(ivpnw, notot, keepExisting = .false., Fill = 0)
        call realloc(dsto, nosys * no_dis, keepExisting = .false., Fill = 0.0e0)
        call realloc(vsto, nosys * no_vel, keepExisting = .false., Fill = 0.0e0)
        idpnw = 0
        ivpnw = 0
        dsto = 0.0
        vsto = 0.0

        call primpro (procesdef, notot, syname, ndspx, nvelx, &
                ioffx, nosys, dsto, vsto, ndspn, &
                idpnw, nveln, ivpnw, noqtt, &
                temp_status)

        if (temp_status%ierr /= 0) then
            call main_status%increase_error_count()
            temp_status%ierr = 0
        end if

        ! determine wich processes must be turned on for output purposes

        call setopp (procesdef, outputs, ioff)

        ! set pointers to input variables and output variables, if nessacary turn processes on.

        nmis = 0
        noloc = 1
        nlocx = 0
        nodef = nopred
        maxdef = nodef + no_ins + no_ine
        call realloc(defaul, maxdef, keepExisting = .false., Fill = 0.0e0)
        call realloc(dename, maxdef, keepExisting = .false., Fill = ' ')

        defaul = 0.0
        defaul(5) = float(itstrt_process)
        defaul(6) = float(itstop_process)
        call realloc(locnam, novarm, keepExisting = .false., Fill = ' ')

        ! put theta in local array if wanted for output, the value will be filled by the integration routine
        ! noloc is already 1?, use this space!

        call getinv (procesdef, notot, syname, nocons, constants, &
                nopa, paname, nofun, funame, nosfun, &
                sfunname, nodisp, diname, novelo, vename, &
                nmis, defaul, noloc, nodef, dename, outputs, &
                ndspx, nvelx, nlocx, locnam, refday)

        ! report on the use of the delwaq input

        call repuse (procesdef, nocons, coname, nopa, paname, nofun, funame, nosfun, sfunname, main_status%noinfo)

        ! set output pointers to process arrays parloc and defaul

        idef = ioff + noloc
        iflx = idef + nodef
        call setopo (procesdef, outputs, ioff, idef, iflx, main_status)

        ! if not all input present , stop with exit code

        if (nmis > 0) then
            write(lunlsp, *) ' not all input available.'
            write(lunlsp, *) ' number off missing variables :', nmis
            write(lunlsp, *) ' simulation impossible.'
            call srstop(1)
        endif

        ! set new pointer for dispersion and velocity

        call setdvp (nodisp, idpnt, ndspn, idpnw, nosys, ndspx, dsto)
        call setdvp (novelo, ivpnt, nveln, ivpnw, nosys, nvelx, vsto)

        ! set grid for processes
        procesdef%procesprops%grid = 1

        ! write proces work file
        nproc = 0
        nflux = 0

        nbpr = 0
        do iproc = 1, procesdef%cursize
            if (procesdef%procesprops(iproc)%active) then
                nbpr = nbpr + 1
            endif
        enddo

        ! calculate new totals

        call proc_totals(lunlsp, procesdef, no_ins, no_ine, no_ous, &
                no_oue, no_flu, no_sto, no_dis, no_vel)

        ! calculate and fill output structure

        nipmsa = 0
        ioffx = nopred + nocons + nopa + nofun + nosfun + notot + noloc + nodef
        mxpmsa = no_ine + no_ins + no_ous + no_oue + no_flu
        call realloc(prvnio, nbpr, keepExisting = .false., Fill = 0)
        call realloc(iflux, nbpr, keepExisting = .false., Fill = 0)
        call realloc(ipmsa, mxpmsa, keepExisting = .false., Fill = 0)
        call realloc(ipssa, mxpmsa, keepExisting = .false., Fill = 0)
        call realloc(prvvar, mxpmsa, keepExisting = .false., Fill = 0)
        call realloc(prvtyp, mxpmsa, keepExisting = .false., Fill = 0)
        call realloc(progrd, nbpr, keepExisting = .false., Fill = 0)
        call realloc(prondt, nbpr, keepExisting = .false., Fill = 0)
        call realloc(pronam, nbpr, keepExisting = .false., Fill = ' ')
        call intoou (procesdef, nproc, nflux, prvnio, pronam, &
                iflux, ipmsa, ipssa, nipmsa, ioffx, &
                nocons, nopa, nofun, nosfun, notot, &
                nodisp, novelo, nodef, noloc, ndspx, &
                nvelx, nlocx, nopred, prvvar, prvtyp, &
                novar, progrd, prondt)

        deallocate(ipmsa, ipssa)

        ! set variables attribute's for aggregation dis-aggregation

        call realloc(varnam, novar, keepExisting = .false., Fill = ' ')
        varnam = ' '
        call realloc(vararr, novar, keepExisting = .false., Fill = 0)
        call realloc(varidx, novar, keepExisting = .false., Fill = 0)
        call realloc(vartda, novar, keepExisting = .false., Fill = 0)
        call realloc(vardag, novar, keepExisting = .false., Fill = 0)
        call realloc(vartag, novar, keepExisting = .false., Fill = 0)
        call realloc(varagg, novar, keepExisting = .false., Fill = 0)
        call setvat (lunlsp, nocons, nopa, nofun, nosfun, &
                nosys, notot, nodisp, novelo, nodef, &
                noloc, ndspx, nvelx, nlocx, nflux, &
                nopred, novar, vararr, varidx, vartda, &
                vardag, vartag, varagg, nogrid, coname, &
                paname, funame, sfunname, dename, syname, &
                locnam, varnam)

        ! determine stochi

        call realloc(stochi, (/notot, nflux/), keepExisting = .false., Fill = 0.0e0)
        call realloc(fluxname, nflux, keepExisting = .false., Fill = ' ')
        call realloc(fluxprocname, nflux, keepExisting = .false., Fill = ' ')
        do iflx = 1, nflux
            do isys = 1, notot
                stochi(isys, iflx) = 0.0
            enddo
        enddo

        nflx = 0
        totfluxsys = 0
        do iproc = 1, procesdef%cursize
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                do istochi = 1, proc%no_fluxstochi
                    flxnam = proc%fluxstochi(istochi)%ioitem
                    isys = proc%fluxstochi(istochi)%subindx
                    scale = proc%fluxstochi(istochi)%scale
                    if (isys>0 .and. abs(scale)>1e-10) then
                        call zoekio (flxnam, proc%no_fluxoutput, proc%fluxoutput, 20, iflx)
                        stochi(isys, nflx + iflx) = scale
                        fluxname(nflx + iflx) = flxnam(1:10)
                        fluxprocname(nflx + iflx) = proc%name(1:10)
                        totfluxsys = totfluxsys + 1
                    endif
                enddo
                nflx = nflx + proc%no_fluxoutput
            endif
        enddo

        call realloc(nfluxsys, notot, keepExisting = .false., Fill = 0)
        call realloc(ipfluxsys, notot, keepExisting = .false., Fill = 0)
        call realloc(fluxsys, totfluxsys, keepExisting = .false., Fill = 0)

        ifluxsys = 0
        do isys = 1, notot
            do iflx = 1, nflux
                if(stochi(isys, iflx)/=0.0) then
                    ifluxsys = ifluxsys + 1
                    nfluxsys(isys) = nfluxsys(isys) + 1
                    fluxsys(ifluxsys) = iflx
                endif
            enddo
        enddo

        ! nrvart is in the boot sysn common

        nrvart = outputs%cursize

        ! Prepare descrtion and unit information for output from the proces library to be written in the NetCDF-file

        ! Extract names list from allitems
        call realloc(ainame, allitems%cursize, keepExisting = .false., Fill = ' ')
        do iitem = 1, allitems%cursize
            ainame(iitem) = allitems%itemproppnts(iitem)%pnt%name
        enddo

        ! Get location of FixAlg in algcof
        name10 = 'FixAlg'
        icof = index_in_array(name10, cofnam)

        ! Get information about the substances
        call realloc (substdname, notot, keepExisting = .false., Fill = ' ')
        call realloc (subunit, notot, keepExisting = .false., Fill = ' ')
        call realloc (subdescr, notot, keepExisting = .false., Fill = ' ')
        do isys = 1, notot
            subname = syname(isys)
            call str_lower(subname)
            iindx = index_in_array(subname, ainame)
            if (iindx > 0) then
                substdname(isys) = allitems%itemproppnts(iindx)%pnt%stdn
                subunit(isys) = allitems%itemproppnts(iindx)%pnt%stdu
                subdescr(isys) = trim(allitems%itemproppnts(iindx)%pnt%text) // ' ' // &
                        allitems%itemproppnts(iindx)%pnt%unit
            else
                ! Is it an algae?
                ialg = index_in_array(subname(1:10), algtyp)
                if (ialg > 0) then
                    if (algcof(icof, ialg) >= 0) then
                        substdname(isys) = ' '
                        subunit(isys) = 'g m-3'
                        subdescr(isys) = algdsc(ialg) // ' (gC/m3)'
                    else
                        substdname(isys) = ' '
                        subunit(isys) = 'g m-2'
                        subdescr(isys) = algdsc(ialg) // ' (gC/m2)'
                    endif
                else
                    substdname(isys) = ' '
                    subunit(isys) = ' '
                    subdescr(isys) = syname(isys)
                endif
            endif
        enddo

        ! Lookup output names in names list
        do ioutp = 1, outputs%cursize
            outname = outputs%names(ioutp)
            call str_lower(outname)
            iindx = index_in_array(outname, ainame)
            if (iindx > 0) then
                outputs%std_var_name(ioutp) = allitems%itemproppnts(iindx)%pnt%stdn
                outputs%units(ioutp) = allitems%itemproppnts(iindx)%pnt%unit
                outputs%description(ioutp) = allitems%itemproppnts(iindx)%pnt%text // ' ' // allitems%itemproppnts(iindx)%pnt%unit
            else if (outname=='theta') then
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
                        outputs%description(ioutp) = algdsc(ialg) // ' (gC/m3)'
                    else
                        outputs%std_var_name(ioutp) = ' '
                        outputs%units(ioutp) = 'g m-2'
                        outputs%description(ioutp) = algdsc(ialg) // ' (gC/m2)'
                    endif
                else
                    outputs%std_var_name(ioutp) = ' '
                    outputs%units(ioutp) = ' '
                    outputs%description(ioutp) = outputs%names(ioutp)
                endif
            endif
        enddo

        ! Determine pointer from prvnio, and promnr from pronam
        call realloc(prvpnt, nproc, keepExisting = .false., Fill = 0)
        call realloc(promnr, nproc, keepExisting = .false., Fill = 0)
        prvpnt(1) = 1
        do iproc = 2, nproc
            prvpnt(iproc) = prvpnt(iproc - 1) + prvnio(iproc - 1)
        end do
        do iproc = 1, nproc
            call pronrs(pronam(iproc), promnr(iproc))
        end do

        if (timon) call timstop(ithndl)

        call main_status%sync(ierr, nowarn, noinfo)
        return
        2001 format(' Using process definition file : ', a)
        2002 format(' Version number                : ', f10.2)
        2003 format(' Serial                        : ', i10)
        2004 format(' Using BLOOM definition file   : ', a    /)
        2080 format (/' Number of active (transported) WQ substances       :', I3, / &
                ' Number of inactive (not transported) WQ substances :', I3, / &
                ' Total number of WQ substances                      :', I3)
        2100 format (/' NOTE: The numbering of FM constituents may differ from the WQ numbering here! Check the dia-file!', // &
                ' WQ Number  (in)active  name')
        2110 format (i7, 3x, a, a)
    end

end module m_wq_processes_initialise
