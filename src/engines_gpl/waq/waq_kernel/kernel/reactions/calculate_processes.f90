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
module m_process_calculation
    use m_waq_precision
    use timers
    use m_provel
    use m_integrate_areas_fluxes, only: integrate_fluxes_for_dump_areas
    use m_profld
    use m_prodr2

    implicit none

    private
    public :: calculate_processes

contains

    !> Process sub-system of DELWAQ water-quality modelling system.
    !! Routine deals with:
    !! - Processes that act on different spatial grids (important application is layered bed)
    !! - Processes that act with coarser time steps (notably the Bloom algal growth model).
    !! - Paralellisation of the different processes on shared memory multi core machines.
    subroutine calculate_processes(num_substances_total, num_cells, conc, volume, itime, &
            idt, deriv, ndmpar, num_processes_activated, noflux, &
            process_space_int, prvnio, promnr, iflux, increm, &
            flux, flxdmp, stochi, ibflag, bloom_status_ind, &
            bloom_ind, amass, num_substances_transported, &
            itfact, amass2, iaflag, intopt, flxint, &
            iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
            num_exchanges_bottom_dir, num_dispersion_arrays_new, idpnew, dispnw, num_dispersion_arrays, &
            idpnt, disper, num_dispersion_arrays_extra, dspx, dsto, &
            num_velocity_arrays_new, ivpnew, velonw, num_velocity_arrays, ivpnt, &
            velo, num_velocity_arrays_extra, velx, vsto, dmps, &
            isdmp, ipdmp, ntdmpq, defaul, prondt, &
            progrd, prvvar, prvtyp, vararr, varidx, &
            vartda, vardag, vartag, varagg, arrpoi, &
            arrknd, arrdm1, arrdm2, vgrset, grdnos, &
            grdseg, num_vars, a, num_grids, num_monitoring_cells, &
            pronam, intsrt, &
            prvpnt, done, num_input_ref, proref, num_defaults, &
            surfac, lunrep)

        use m_scale_derivatives_steady_state, only: scale_processes_derivs_and_update_balances
        use m_cli_utils, only: get_command_argument_by_name
        use aggregation, only: aggregate, aggregate_extended, resample, aggregate_attributes
        use m_get_variable_index_number, only: get_variable_index_number
        use m_array_manipulation, only: set_array_parameters
        use m_logger_helper, only: stop_with_error, get_log_unit_number
        use process_registration
        use omp_lib

        integer(kind = int_wp), intent(in) :: num_grids                      !< Number of computational grids
        integer(kind = int_wp), intent(in) :: num_substances_total           !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                      !< Nr. of computational volumes
        integer(kind = int_wp), intent(in) :: num_defaults                   !< Number of values in the deafult array
        integer(kind = int_wp), intent(in) :: num_vars                       !< Total number of variables
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells, num_grids) !< Model concentrations
        real(kind = real_wp), intent(inout) :: volume(num_cells, num_grids)         !< Segment volumes
        integer(kind = int_wp), intent(in) :: itime                         !< Time in system clock units
        integer(kind = int_wp), intent(in) :: idt                           !< Time step system clock units
        real(kind = real_wp), intent(out) :: deriv(num_substances_total, num_cells, num_grids)   !< Model derivatives
        integer(kind = int_wp), intent(in) :: ndmpar                        !< Number of dump areas
        integer(kind = int_wp), intent(in) :: num_processes_activated       !< Number of processes
        integer(kind = int_wp), intent(in) :: noflux                        !< Number of fluxes
        integer(kind = int_wp), intent(in) :: process_space_int (:)         !< Direct pointer in DELWAQ arrays
        integer(kind = int_wp), intent(in) :: prvnio(num_processes_activated) !< Nr. of state variables per proces
        integer(kind = int_wp), intent(in) :: promnr(num_processes_activated) !< Proces module number per proces
        integer(kind = int_wp), intent(in) :: iflux (num_processes_activated) !< Offset in flux array per process
        integer(kind = int_wp), intent(in) :: increm(:)                       !< Direct increment in DELWAQ arrays
        real(kind = real_wp), intent(inout) :: flux  (noflux, num_cells, num_grids) !< Proces fluxes
        real(kind = real_wp), intent(inout) :: flxdmp(num_monitoring_cells, noflux)         !< Fluxes at dump segments
        real(kind = real_wp), intent(in) :: stochi(num_substances_total, noflux)         !< Proces stochiometry
        integer(kind = int_wp), intent(in) :: ibflag                        !< if 1 then mass balance output
        integer(kind = int_wp), intent(in) :: bloom_status_ind              !< Number of Bloom module  (if >0)
        integer(kind = int_wp), intent(in) :: bloom_ind                     !< Offset in process_space_int for Bloom
        real(kind = real_wp), intent(inout) :: amass(num_substances_total, num_cells, num_grids)   !< mass array to be updated
        integer(kind = int_wp), intent(in) :: num_substances_transported    !< number of active substances
        integer(kind = int_wp), intent(in) :: itfact                        !< time scale factor processes
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)              !< mass balance array
        integer(kind = int_wp), intent(in) :: iaflag                        !< if 1 then accumulation
        integer(kind = int_wp), intent(in) :: intopt                        !< Integration suboptions
        real(kind = real_wp), intent(inout) :: flxint(ndmpar, noflux)        !< Integrated fluxes at dump areas
        integer(kind = int_wp), intent(in) :: iexpnt(:)                     !< Exchange pointer
        integer(kind = int_wp), intent(inout) :: iknmrk(:)                     !< Integration suboptions
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir           !< Number of exchanges first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir           !< Number of exchanges second direction
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir           !< Number of exchanges vertical
        integer(kind = int_wp), intent(in) :: num_exchanges_bottom_dir      !< Number of exchanges in the bed
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays_new
        integer(kind = int_wp), intent(in) :: idpnew(num_substances_transported)    !< Pointer to new disp array
        real(kind = real_wp), intent(out) :: dispnw(num_dispersion_arrays_new, *)  !< New dispersion array
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays                 !< Nr. of original dispersions
        integer(kind = int_wp), intent(in) :: idpnt (num_substances_transported)    !< Pointer to original dispersion
        real(kind = real_wp), intent(in) :: disper(num_dispersion_arrays, *)      !< Original dispersions
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays_extra           !< Nr. of calculated dispersions
        real(kind = real_wp), intent(in) :: dspx  (:)                     !< Calculated dispersions
        real(kind = real_wp), intent(in) :: dsto(num_substances_transported, num_dispersion_arrays_extra)          !< Factor for calc. dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_new
        integer(kind = int_wp), intent(in) :: ivpnew(num_substances_transported)          !< Pointer to new velo array
        real(kind = real_wp), intent(out) :: velonw(num_velocity_arrays_new, *)              !< New velocity array
        integer(kind = int_wp), intent(in) :: num_velocity_arrays                        !< Nr. of original velocities
        integer(kind = int_wp), intent(in) :: ivpnt (num_substances_transported)                 !< pointer to original velo
        real(kind = real_wp), intent(in) :: velo  (num_velocity_arrays, *)             !< Original velocities
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_extra                  !< Nr. of calculated velocities
        real(kind = real_wp), intent(in) :: velx  (num_velocity_arrays_extra, *)       !< Calculated velocities
        real(kind = real_wp), intent(in) :: vsto  (num_substances_transported, num_velocity_arrays_extra)!< Factor for velocitie
        real(kind = real_wp), intent(inout) :: dmps  (num_substances_total, num_monitoring_cells)!< dumped segment fluxes
        integer(kind = int_wp), intent(in) :: isdmp (num_cells)                 !< pointer dumped segments
        integer(kind = int_wp), intent(in) :: ipdmp (*)                     !< pointer structure dump area's
        integer(kind = int_wp), intent(in) :: ntdmpq                        !< total number exchanges in dump area
        real(kind = real_wp), intent(inout) :: defaul(num_defaults)                 !< Default proces parameters
        integer(kind = int_wp), intent(inout) :: prondt(num_processes_activated)                 !< Time-step for each process wrt global time-step
        integer(kind = int_wp), intent(in) :: progrd(num_processes_activated)                 !< Grid per process
        integer(kind = int_wp), intent(in) :: prvvar(*)                     !< Index of variable
        integer(kind = int_wp), intent(in) :: prvtyp(*)                     !< Type of variable
        integer(kind = int_wp), intent(in) :: vararr(num_vars)                 !< Variable array number
        integer(kind = int_wp), intent(in) :: varidx(num_vars)                 !< Variable index in array
        integer(kind = int_wp), intent(in) :: vartda(num_vars)                 !< Type of disaggregation
        integer(kind = int_wp), intent(in) :: vardag(num_vars)                 !< Variable disaggr. weight var.
        integer(kind = int_wp), intent(in) :: vartag(num_vars)                 !< Variable type of aggregation
        integer(kind = int_wp), intent(in) :: varagg(num_vars)                 !< Variable aggregation variable
        integer(kind = int_wp), intent(in) :: arrpoi(*)                     !< Pointer (=index) to the start of the array
        integer(kind = int_wp), intent(in) :: arrknd(*)                     !< Kind of array 1=(num_vars), 2=(num_vars,num_cells) or 3=(num_cells,num_vars), switch which type of increment should be used
        integer(kind = int_wp), intent(in) :: arrdm1(*)                     !< Dimension in the 1st direction
        integer(kind = int_wp), intent(in) :: arrdm2(*)                     !< Dimension in the 2nd direction
        integer(kind = int_wp), intent(inout) :: vgrset(num_vars, num_grids)         !< Local flag for variables and grid
        integer(kind = int_wp), intent(in) :: grdnos(num_grids)                !< Number of segments per grid
        integer(kind = int_wp), intent(in) :: grdseg(num_cells, num_grids)         !< Aggregation pointer per grid
        real(kind = real_wp), intent(in) :: a     (:)                     !< Real workspace variable
        integer(kind = int_wp), intent(in) :: num_monitoring_cells
        character(20), intent(in) :: pronam(*)                     !< Name of called module
        integer(kind = int_wp), intent(in) :: intsrt                        !< Number of integration routine used
        integer(kind = int_wp), intent(in) :: prvpnt(num_processes_activated)                 !< entry in process pointers OMP
        integer(kind = int_wp), intent(inout) :: done  (num_processes_activated)                 !< flag whether a process has ran
        integer(kind = int_wp), intent(in) :: num_input_ref                         !< maximum nr of back references
        integer(kind = int_wp), intent(in) :: proref(num_input_ref, num_processes_activated)          !< the back references
        real(kind = real_wp), intent(in) :: surfac(num_cells)                 !< horizontal surface
        integer(kind = int_wp), intent(in) :: lunrep                        !< Logical unit number of report-file

        ! Local variables
        integer(kind = int_wp) :: maxgrid    ! Highest grid number in progrd array
        integer(kind = int_wp) :: iiknmr     ! Pointer somewhere into the array tree
        integer(kind = int_wp) :: ix_hlp, ia_hlp, iv_hlp, ik_hlp, ip_hlp, & !  array pointers
                id1hlp, id2hlp                          !
        integer(kind = int_wp) :: ivar, iarr, iv_idx, ip_arr          !  help variables
        integer(kind = int_wp) :: ix_cnc, ia_cnc, iv_cnc, ip_arh          !  help variables
        integer(kind = int_wp) :: ipndt, ndtblo, igrblo, ndtcha, igrcha  !  help variables
        integer(kind = int_wp) :: substance_i, igrid, isysh, nototh, igrd    !  help variables
        integer(kind = int_wp) :: noseg2, nfluxp, iswcum, ifracs, iproc   !  help variables
        integer(kind = int_wp) :: k
        integer(kind = int_wp) :: actually_done
        integer(kind = int_wp) :: idtpro    ! fractional step idt
        integer(kind = int_wp) :: ipp_idt    ! pointer in default array to process specific idt
        integer(kind = int_wp) :: ipp_delt   ! pointer in default array to process specific delt
        INTEGER(kind = int_wp) :: ISTEP, num_exchanges, IERR
        integer(kind = int_wp), allocatable, save :: velndt(:) ! fractional step per velocity
        integer(kind = int_wp), allocatable, save :: dspndt(:) ! fractional step per dispersion
        integer(kind = int_wp) :: open_shared_library
        integer(kind = int_wp) :: perf_function
        integer(kind = int_wp), save :: ifirst = 1
        integer(c_intptr_t), save :: dll_opb     ! open proces library dll handle
        integer(kind = int_wp) :: ierror
        character(:), allocatable :: shared_dll

        logical :: parsing_error
        logical :: l_stop


        !     LOGICAL PROFLG
        SAVE    ISTEP
        DATA    ISTEP  / 0 /
        !     DATA    PROFLG / .TRUE. /

        ! jvb  Store fractional step flag in common CFRACS
        COMMON /CFRACS/ IFRACS
        logical                 run                !< lp for OMP
        integer(kind = int_wp) :: aproc            !< lp for OMP

        logical timon_old
        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp) :: ithand2 = 0
        if (timon) call timstrt ("calculate_processes", ithandl)

        ! If no processes, get out of here
        IF (num_processes_activated == 0) goto 9999

        ! open openpb dll
        if (ifirst == 1) then
            call get_log_unit_number(lunrep)
            if (get_command_argument_by_name('-openpb', shared_dll, parsing_error)) then
                if (.not. parsing_error) then
                    write(lunrep, *) ' -openpb command line argument found'
                    write(lunrep, *) ' using dll : ', trim(shared_dll)
                else
                    shared_dll = 'd3dwaq_openpb.dll'
                    write(lunrep, *) ' WARNING : -openpb command line argument without filename'
                    write(lunrep, *) ' using default dll : ', trim(shared_dll)
                endif
                l_stop = .true.
            else
                shared_dll = 'd3dwaq_openpb.dll'
                l_stop = .false.
                write(lunrep, *) ' using default dll : ', trim(shared_dll)
            endif
            dll_opb = 0 ! in C this one could be 4 or 8 bytes, so make sure the last bytes are zero
            ierror = open_shared_library(dll_opb, shared_dll)
            if (ierror /= 0 .and. l_stop) then
                write(*, *) 'ERROR : opening process library DLL'
                write(*, *) 'DLL   : ', trim(shared_dll)
                write(*, *) 'dll handle: ', dll_opb
                write(lunrep, *) 'ERROR : opening process library DLL'
                write(lunrep, *) 'DLL   : ', trim(shared_dll)
                write(lunrep, *) 'dll handle: ', dll_opb
                call stop_with_error()
            endif
            ifirst = 0
        endif

        ! Count calls of this module
        ISTEP = ISTEP + 1
        done = 0 ! this zeroes the whole array

        ! Start timings

        ! allocate velndt, dspndt
        if (.not. allocated(velndt)) then
            allocate(velndt(num_velocity_arrays_extra))
            velndt = 1
        endif
        if (.not. allocated(dspndt)) then
            allocate(dspndt(num_dispersion_arrays_extra))
            dspndt = 1
        endif

        ! aggregate kenmerk array
        !grd  only if there is a process on a higher grid
        maxgrid = maxval(progrd(1:num_processes_activated))
        iiknmr = 78 + 30                 !   = iasize + 30
        if (maxgrid > 1) then
            call aggregate_attributes (num_cells, arrdm2(iiknmr), num_grids, iknmrk, grdnos, &
                    grdseg)
        endif

        ! Get the general local work array, first index of LOCAL array
        ix_hlp = 1
        ia_hlp = 33
        call get_variable_index_number(ia_hlp, ix_hlp, iv_hlp)
        ik_hlp = arrknd(ia_hlp)
        ip_hlp = arrpoi(ia_hlp)
        id1hlp = arrdm1(ia_hlp)
        id2hlp = arrdm2(ia_hlp)

        ! Fill some specific variables absolute in the real array
        defaul(2) = real(itime)
        num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

        ! BLOOM fractional step (derivs assumed zero at entry)
        if (bloom_status_ind > 0) then         ! Check presence of BLOOM module for this run
            ivar = prvvar(bloom_ind)
            iarr = vararr(ivar)
            iv_idx = varidx(ivar)
            ip_arr = arrpoi(iarr)
            ipndt = ip_arr + iv_idx - 1
            ndtblo = nint(a(ipndt))
            prondt(bloom_status_ind) = ndtblo

            ! This timestep fractional step ?
            if (mod(istep - 1, ndtblo) == 0) then

                ! Set CONC on the Bloom grid if that is not the first grid
                igrblo = progrd(bloom_status_ind)
                if (igrblo > 1) then
                    noseg2 = grdnos(igrblo)
                    ix_cnc = 1
                    ia_cnc = 6
                    call get_variable_index_number(ia_cnc, ix_cnc, iv_cnc)
                    call set_array_parameters(iv_hlp, ia_hlp, ik_hlp, ix_hlp, id1hlp, &
                            id2hlp, ip_hlp, igrblo, isysh, nototh, &
                            ip_arh)

                    ! actives and inactives if applicable
                    call aggregate(num_cells, noseg2, num_substances_total, 1, nototh, &
                            num_substances_total, 1, 1, isysh, 1, &
                            num_substances_transported, grdseg(1, igrblo), 3, conc, volume, &
                            a(ip_arh:), conc(1, 1, igrblo))
                    if (num_substances_total - num_substances_transported > 0)      & !   inactives
                            call aggregate(num_cells, noseg2, num_substances_total, 1, nototh, &
                                    num_substances_total, num_substances_transported + 1, 1, isysh, num_substances_transported + 1, &
                                    num_substances_total - num_substances_transported, grdseg(1, igrblo), 3, conc, surfac, &
                                    a(ip_arh:), conc(1, 1, igrblo))
                    do substance_i = 1, num_substances_total
                        ivar = iv_cnc + substance_i - 1
                        vgrset(ivar, igrblo) = 1
                    enddo
                endif

                flux = 0.0
                if (ibflag > 0) flxdmp = 0

                ! set idt and delt, bloom itself will multiply with prondt
                idtpro = prondt(bloom_status_ind) * idt
                ipp_idt = num_defaults - 2 * num_processes_activated + bloom_status_ind
                ipp_delt = num_defaults - num_processes_activated + bloom_status_ind
                defaul(ipp_idt) = real(idt)
                defaul(ipp_delt) = real(idt) / real(itfact)
                if (timon) call timstrt ("calculate_single_process", ithand2)
                call calculate_single_process(bloom_status_ind, bloom_ind, idt, itfact, progrd, &
                        grdnos, prvnio, prvtyp, prvvar, vararr, &
                        varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                        vgrset, num_grids, vartda, vardag, num_cells, &
                        grdseg, a, varagg, process_space_int, increm, &
                        noflux, iflux, promnr, flux, iexpnt, &
                        iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
                        num_processes_activated, num_substances_total, deriv, stochi, volume, &
                        prondt, ibflag, isdmp, flxdmp, num_vars, &
                        vartag, iiknmr, pronam, &
                        dspndt, velndt, dll_opb)
                done(bloom_status_ind) = 1
                if (timon) call timstop(ithand2)
                igrid = progrd(bloom_status_ind)
                noseg2 = grdnos(igrid)
                if (bloom_status_ind /= num_processes_activated) then
                    nfluxp = iflux(bloom_status_ind + 1) - iflux(bloom_status_ind)
                else
                    nfluxp = noflux - iflux(bloom_status_ind) + 1
                endif
                if (nfluxp > 0) then

                    ! If necessary set volume for this grid. Volume is always variable 1

                    if (vgrset(1, igrid) /= 1) then
                        call aggregate_extended(num_cells, noseg2, 1, 1, 1, &
                                1, 1, 1, 1, 1, &
                                grdseg(1, igrid), 1, volume, volume, volume, &
                                volume(1, igrid))
                        vgrset(1, igrid) = 1
                    endif

                    ! Construct derivatives for these fluxes on this grid
                    call prodr2(deriv(1, 1, igrid), num_substances_total, noflux, stochi, iflux (bloom_status_ind), &
                            nfluxp, flux(1, 1, igrid), noseg2, volume(1, igrid), prondt(bloom_status_ind))

                    ! For balances store FLXDMP
                    if (ibflag > 0) then
                        call profld(noflux, iflux (bloom_status_ind), nfluxp, igrid, noseg2, &
                                num_cells, prondt(bloom_status_ind), isdmp, grdseg, flux(1, 1, igrid), &
                                volume, flxdmp)
                    endif
                endif

                !           If processes on other grid convert derivs to base grid

                igrblo = progrd(bloom_status_ind)
                if (noflux > 0 .and. igrblo > 1) then
                    iswcum = 1
                    noseg2 = grdnos(igrblo)
                    call resample(num_cells, noseg2, num_substances_total, num_substances_total, num_substances_total, &
                            num_substances_total, 1, 1, 1, 1, &
                            num_substances_total, grdseg(1, igrblo), 2, deriv(1, 1, igrblo), amass, &
                            iswcum, amass (1, 1, igrblo), deriv)
                    deriv(:, :, igrblo) = 0.0   !     Zero derivs higher grids
                endif

                ! Scale fluxes and update "processes" accumulation arrays
                call scale_processes_derivs_and_update_balances (deriv, num_substances_total, num_cells, itfact, amass2, &
                        idt, iaflag, dmps, intopt, isdmp)

                ! Integration (derivs are zeroed)
                call set_explicit_time_step_for_derivatives (conc, amass, deriv, volume, idt, &
                        num_substances_transported, num_substances_total, num_cells, 0, 0, &
                        surfac)

                ! Integrate the fluxes at dump segments
                if (ibflag > 0) then
                    call integrate_fluxes_for_dump_areas(noflux, ndmpar, idt, itfact, flxdmp, &
                            flxint, isdmp, ipdmp, ntdmpq)
                    flxdmp = 0.0
                endif

                ! Set CONC not actual for higer grids
                ix_cnc = 1
                ia_cnc = 6
                call get_variable_index_number(ia_cnc, ix_cnc, iv_cnc)
                do igrid = 2, num_grids
                    do substance_i = 1, num_substances_total
                        ivar = iv_cnc + substance_i - 1
                        vgrset(ivar, igrid) = 0
                    enddo
                enddo
            else
                done (bloom_status_ind) = 1
            endif
        endif

        ! See if converting CONC in one step speeds up. Only in case of no fractional step
        if (ifracs == 0 .and. maxgrid > 1) then
            ix_cnc = 1
            ia_cnc = 6
            call get_variable_index_number(ia_cnc, ix_cnc, iv_cnc)
            do igrid = 2, num_grids
                noseg2 = grdnos(igrid)
                call set_array_parameters(iv_hlp, ia_hlp, ik_hlp, ix_hlp, id1hlp, &
                        id2hlp, ip_hlp, igrid, isysh, nototh, &
                        ip_arh)

                ! actives
                call aggregate(num_cells, noseg2, num_substances_total, 1, nototh, &
                        num_substances_total, 1, 1, isysh, 1, &
                        num_substances_transported, grdseg(1, igrid), 3, conc, volume, &
                        a(ip_arh:), conc(1, 1, igrid))
                ! inactives, if applicable
                if (num_substances_total - num_substances_transported > 0) then
                    call aggregate(num_cells, noseg2, num_substances_total, 1, nototh, &
                            num_substances_total, num_substances_transported + 1, 1, isysh, num_substances_transported + 1, &
                            num_substances_total - num_substances_transported, grdseg(1, igrid), 3, conc, surfac, &
                            a(ip_arh:), conc(1, 1, igrid))
                end if
                do substance_i = 1, num_substances_total
                    ivar = iv_cnc + substance_i - 1
                    vgrset(ivar, igrid) = 1
                enddo
            enddo
        endif

        ! The processes fractional step
        flux = 0.0
        if (ibflag > 0) flxdmp = 0

        if (timon) call timstrt ("calculate_single_process", ithand2)
        timon_old = timon
        if (OMP_GET_MAX_THREADS() > 1) timon = .false.
        !$OMP PARALLEL
        !$OMP DO  PRIVATE(run,idtpro,k,nfluxp,ipp_idt,ipp_delt)  SCHEDULE(DYNAMIC)
        do iproc = 1, num_processes_activated

            ! NOT bloom
            if (iproc /= bloom_status_ind) then

                ! Check fractional step
                if (mod(istep - 1, prondt(iproc)) == 0) then
                    run = .false.                             ! to get the loop running
                    do while (.not. run)                      ! wait untill all input is resolved
                        run = .true.                          ! we are optimistic
                        do k = 1, num_input_ref                       ! maximum number of references / proc
                            if (proref(k, iproc) == 0) exit   ! no references left

                            !
                            ! Flush the array done:
                            ! The Intel Fortran compiler seems to optimise this
                            ! loop too aggressively under Linux, if we use the array done()
                            ! directly.
                            !

                            !$omp flush(done)

                            if (done(proref(k, iproc)) == 0) then  ! an unresolved one found
                                run = .false.                      ! so no run yet
                                exit
                            endif                                  ! everything is resolved
                        enddo                                      ! for this processs
                    enddo

                    ! set idt and delt for this process in the default array
                    ipp_idt = num_defaults - 2 * num_processes_activated + iproc
                    ipp_delt = num_defaults - num_processes_activated + iproc
                    IDTPRO = PRONDT(IPROC) * IDT
                    DEFAUL(ipp_idt) = real(IDTPRO)
                    DEFAUL(ipp_delt) = real(IDTPRO) / real(ITFACT)

                    call calculate_single_process(iproc, prvpnt(iproc), idt, itfact, progrd, &
                            grdnos, prvnio, prvtyp, prvvar, vararr, &
                            varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                            vgrset, num_grids, vartda, vardag, num_cells, &
                            grdseg, a, varagg, process_space_int, increm, &
                            noflux, iflux, promnr, flux, iexpnt, &
                            iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
                            num_processes_activated, num_substances_total, deriv, stochi, volume, &
                            prondt, ibflag, isdmp, flxdmp, num_vars, &
                            vartag, iiknmr, pronam, &
                            dspndt, velndt, dll_opb)

                    done(iproc) = 1                           ! this process has resolved its output
                    !$omp flush(done)

                endif
            endif
        enddo
        !$OMP END DO
        !$OMP END PARALLEL

        timon = timon_old
        if (timon) call timstop (ithand2)

        ! Now update the derivatives and the dumps of the fluxes from
        ! all processes together outside of the parallel region
        call update_derivaties_and_dump_fluxes(num_processes_activated, num_grids, noflux, num_vars, num_cells, &
                num_substances_total, progrd, grdnos, iflux, vgrset, &
                grdseg, volume, deriv, stochi, flux, &
                prondt, ibflag, isdmp, flxdmp, &
                bloom_status_ind, istep)

        ! Store fluxes and elaborate mass balances set fractional step
        ! Vraag , doen we nu altijd fractional step? of moeten we als we geen
        ! processen hebben met een grotere tijdstap de integratie samen met het
        ! transport doen.
        if (noflux > 0 .and. maxgrid > 1) then
            do igrd = 2, num_grids
                iswcum = 1
                noseg2 = grdnos(igrd)
                call resample(num_cells, noseg2, num_substances_total, num_substances_total, num_substances_total, &
                        num_substances_total, 1, 1, 1, 1, &
                        num_substances_total, grdseg(1, igrd), 2, deriv(1, 1, igrd), amass, &
                        iswcum, amass (1, 1, igrd), deriv)
            enddo

            !        Zero derivs higher grids
            deriv(:, :, 2:num_grids) = 0.0
        endif

        !     Set fractional step
        if (noflux > 0 .and. ifracs == 1) then

            ! no fluxes at first step of fractional step
            if (istep == 1) then
                deriv(:, :, 1) = 0.0
                if (ibflag > 0) flxdmp = 0.0
            else

                ! Scale fluxes and update "processes" accumulation arrays
                call scale_processes_derivs_and_update_balances(deriv, num_substances_total, num_cells, itfact, amass2, &
                        idt, iaflag, dmps, intopt, isdmp)

                ! Integration (derivs are zeroed)
                call set_explicit_time_step_for_derivatives (conc, amass, deriv, volume, idt, &
                        num_substances_transported, num_substances_total, num_cells, 0, 0, &
                        surfac)

                ! Integrate the fluxes at dump segments
                if (ibflag > 0) then
                    call integrate_fluxes_for_dump_areas(noflux, ndmpar, idt, itfact, flxdmp, &
                            flxint, isdmp, ipdmp, ntdmpq)
                    flxdmp = 0.0
                endif
            endif
        endif

        ! Calculate new dispersions
        if (num_dispersion_arrays_new  > 0) then
            call provel (dispnw, num_dispersion_arrays_new, idpnew, disper, num_dispersion_arrays, &
                    idpnt, dspx, num_dispersion_arrays_extra, dsto, num_substances_transported, &
                    num_exchanges, dspndt, istep)
        endif

        ! Calculate new velocities
        if (num_velocity_arrays_new  > 0) then
            call provel (velonw, num_velocity_arrays_new, ivpnew, velo, num_velocity_arrays, &
                    ivpnt, velx, num_velocity_arrays_extra, vsto, num_substances_transported, &
                    num_exchanges, velndt, istep)
        endif

        9999 if (timon) call timstop (ithandl)

    end subroutine calculate_processes

    subroutine calculate_single_process(iproc, k, idt, itfact, progrd, &
            grdnos, prvnio, prvtyp, prvvar, vararr, &
            varidx, arrknd, arrpoi, arrdm1, arrdm2, &
            vgrset, num_grids, vartda, vardag, num_cells, &
            grdseg, a, varagg, process_space_int, increm, &
            noflux, iflux, promnr, flux, iexpnt, &
            iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
            num_processes_activated, num_substances_total, deriv, stochi, volume, &
            prondt, ibflag, isdmp, flxdmp, num_vars, &
            vartag, iiknmr, pronam, &
            dspndt, velndt, dll_opb)

        use process_registration
        use aggregation, only: aggregate_extended, resample, resample_v2
        use m_array_manipulation, only: set_array_parameters
        use m_get_variable_index_number, only: get_variable_index_number

        integer(kind = int_wp) :: iproc
        integer(kind = int_wp) :: k
        integer(kind = int_wp) :: idt
        integer(kind = int_wp) :: itfact
        integer(kind = int_wp) :: num_grids
        integer(kind = int_wp) :: num_cells
        integer(kind = int_wp) :: noflux
        integer(kind = int_wp) :: num_exchanges_u_dir
        integer(kind = int_wp) :: num_exchanges_v_dir
        integer(kind = int_wp) :: num_exchanges_z_dir
        integer(kind = int_wp) :: num_exchanges_bottom_dir
        integer(kind = int_wp) :: num_processes_activated
        integer(kind = int_wp) :: num_substances_total
        integer(kind = int_wp) :: ibflag
        integer(kind = int_wp) :: num_vars
        integer(kind = int_wp) :: iiknmr

        integer(kind = int_wp) :: progrd(*), grdnos(*), &
                prvnio(*), prvtyp(*), &
                prvvar(*), vararr(*), &
                varidx(*), arrknd(*), &
                arrpoi(*), arrdm1(*), &
                arrdm2(*), vgrset(num_vars, *), &
                vartda(*), vardag(*), &
                grdseg(num_cells, *), varagg(*), &
                process_space_int (:), increm(:), &
                iflux (*), promnr(*), &
                iexpnt(:), iknmrk(:), &
                prondt(*), isdmp (*), &
                vartag(*), &
                dspndt(*), velndt(*)
        real(kind = real_wp) :: a(:), flux(*), &
                deriv(*), stochi(*), &
                volume(*), flxdmp(*)
        character(len = 10)        pronam(*)
        integer(c_intptr_t), intent(in) :: dll_opb     ! open proces library dll handle

        ! Local
        integer(kind = int_wp) :: idtpro, ityp, ix_hlp, ia_hlp, iv_hlp, ik_hlp, ip_hlp, id1hlp, id2hlp
        integer(kind = int_wp) :: noseg2, ivario, igrid, igr3, noseg3, isysi, nototi
        integer(kind = int_wp) :: ivar, iarr, iv_idx, iarknd, ip_arr, idim1, idim2
        integer(kind = int_wp) :: iv_ag, ia_ag, ix_ag, ik_ag, ip_ag, id1_ag, id2_ag
        integer(kind = int_wp) :: ip_ari, nototo, isyso, ip_aro, idatyp, iv_da, ia_da, ik_da
        integer(kind = int_wp) :: ix_da, ip_da, id1_da, id2_da, nototw, isysw, ip_arw
        integer(kind = int_wp) :: nototh, isysh, ip_arh, iswcum, iagtyp, ipflux, ipknmr, igr2

        ! get the general local work array, first index of LOCAL array
        ix_hlp = 1
        ia_hlp = 33
        call get_variable_index_number(ia_hlp, ix_hlp, iv_hlp)
        ik_hlp = arrknd(ia_hlp)
        ip_hlp = arrpoi(ia_hlp)
        id1hlp = arrdm1(ia_hlp)
        id2hlp = arrdm2(ia_hlp)

        ! which grid
        igrid = progrd(iproc)
        noseg2 = grdnos(igrid)

        ! set the variable for this grid
        do ivario = 1, prvnio(iproc)
            ityp = prvtyp(k + ivario - 1)
            ivar = prvvar(k + ivario - 1)
            iarr = vararr(ivar)
            iv_idx = varidx(ivar)
            iarknd = arrknd(iarr)
            ip_arr = arrpoi(iarr)
            idim1 = arrdm1(iarr)
            idim2 = arrdm2(iarr)
            if (ityp == 1) then

                ! only for space varying array's
                if (iarknd >= 2) then

                    ! only if variable isn't actual set for this grid
                    if (vgrset(ivar, igrid) == 0) then

                        ! set variable for base grid
                        if (vgrset(ivar, 1) == 0) then
                            do igr3 = 2, num_grids
                                if (vgrset(ivar, igr3) == 1) then
                                    noseg3 = grdnos(igr3)

                                    ! determine characteristics of variable
                                    call set_array_parameters(ivar, iarr, &
                                            iarknd, iv_idx, &
                                            idim1, idim2, &
                                            ip_arr, igr3, &
                                            isysi, nototi, &
                                            ip_ari)
                                    call set_array_parameters(ivar, iarr, &
                                            iarknd, iv_idx, &
                                            idim1, idim2, &
                                            ip_arr, 1, &
                                            isyso, nototo, &
                                            ip_aro)
                                    ! determine characteristics of weight variable
                                    ! (don't mind if this one is actual ?)
                                    idatyp = vartda(ivar)
                                    if (idatyp == 2) then
                                        iv_da = vardag(ivar)
                                        ia_da = vararr(iv_da)
                                        ik_da = arrknd(ia_da)
                                        if (ik_da == 1) then

                                            ! not variable in space use help var
                                            idatyp = 3
                                            iv_da = iv_hlp
                                            ia_da = vararr(iv_da)
                                            ik_da = arrknd(ia_da)
                                        endif
                                        ix_da = varidx(iv_da)
                                        ip_da = arrpoi(ia_da)
                                        id1_da = arrdm1(ia_da)
                                        id2_da = arrdm2(ia_da)
                                        call set_array_parameters(iv_da, ia_da, &
                                                ik_da, ix_da, &
                                                id1_da, id2_da, &
                                                ip_da, 1, &
                                                isysw, nototw, &
                                                ip_arw)
                                        call set_array_parameters(iv_hlp, ia_hlp, &
                                                ik_hlp, ix_hlp, &
                                                id1hlp, id2hlp, &
                                                ip_hlp, igr3, &
                                                isysh, nototh, &
                                                ip_arh)
                                    elseif (idatyp == 3) then
                                        iv_da = iv_hlp
                                        ia_da = vararr(iv_da)
                                        ik_da = arrknd(ia_da)
                                        ix_da = varidx(iv_da)
                                        ip_da = arrpoi(ia_da)
                                        id1_da = arrdm1(ia_da)
                                        id2_da = arrdm2(ia_da)
                                        call set_array_parameters(iv_da, ia_da, &
                                                ik_da, ix_da, &
                                                id1_da, id2_da, &
                                                ip_da, 1, &
                                                isysw, nototw, &
                                                ip_arw)
                                        call set_array_parameters(iv_hlp, ia_hlp, &
                                                ik_hlp, ix_hlp, &
                                                id1hlp, id2hlp, &
                                                ip_hlp, igr3, &
                                                isysh, nototh, &
                                                ip_arh)
                                    else
                                        ! weight and help arrays dummies
                                        ! so set to the variable itself
                                        isysw = isyso
                                        isysh = isysi
                                        nototw = nototo
                                        nototh = nototi
                                        ip_arw = ip_aro
                                        ip_arh = ip_ari
                                    endif
                                    iswcum = 0
                                    call resample_v2(num_cells, noseg3, &
                                            nototi, nototw, &
                                            nototh, nototo, &
                                            isysi, isysw, &
                                            isysh, isyso, &
                                            grdseg(1, igr3), idatyp, &
                                            a(ip_ari:), a(ip_arw:), &
                                            iswcum, a(ip_arh:), &
                                            a(ip_aro:))
                                    vgrset(ivar, 1) = 1
                                endif
                            enddo
                        endif

                        ! set the variable for this grid
                        if (igrid /= 1) then

                            ! determine characteristics of variable
                            call set_array_parameters(ivar, iarr, &
                                    iarknd, iv_idx, &
                                    idim1, idim2, &
                                    ip_arr, 1, &
                                    isysi, nototi, &
                                    ip_ari)
                            call set_array_parameters(ivar, iarr, &
                                    iarknd, iv_idx, &
                                    idim1, idim2, &
                                    ip_arr, igrid, &
                                    isyso, nototo, &
                                    ip_aro)

                            ! determine characteristics of weight variable
                            iagtyp = vartag(ivar)
                            if (iagtyp == 2 .or. iagtyp == 3) then
                                iv_ag = varagg(ivar)
                                ia_ag = vararr(iv_ag)
                                ix_ag = varidx(iv_ag)
                                ik_ag = arrknd(ia_ag)
                                ip_ag = arrpoi(ia_ag)
                                id1_ag = arrdm1(ia_ag)
                                id2_ag = arrdm2(ia_ag)
                                call set_array_parameters(iv_ag, ia_ag, &
                                        ik_ag, ix_ag, &
                                        id1_ag, id2_ag, &
                                        ip_ag, 1, &
                                        isysw, nototw, &
                                        ip_arw)
                                call set_array_parameters(iv_hlp, ia_hlp, &
                                        ik_hlp, ix_hlp, &
                                        id1hlp, id2hlp, &
                                        ip_hlp, igrid, &
                                        isysh, nototh, &
                                        ip_arh)
                            else
                                !
                                !                       Weight and help array's dummy's
                                !                       so set to the variable itself
                                !
                                isysw = isyso
                                isysh = isysi
                                nototw = nototo
                                nototh = nototi
                                ip_arw = ip_aro
                                ip_arh = ip_ari
                                !
                            endif
                            !
                            call aggregate_extended(num_cells, noseg2, &
                                    nototi, nototw, &
                                    nototh, nototo, &
                                    isysi, isysw, &
                                    isysh, isyso, &
                                    grdseg(1, igrid), iagtyp, &
                                    a(ip_ari:), a(ip_arw:), &
                                    a(ip_arh:), a(ip_aro:))
                            vgrset(ivar, igrid) = 1
                        endif
                        !
                    endif
                endif
            endif

            ! Zet pointer structuur voor procesmodule, dit hoeft eigenlijk maar 1 keer
            if (iarknd == 1) then
                process_space_int (k + ivario - 1) = ip_arr + iv_idx - 1
                increm(k + ivario - 1) = 0
            elseif (iarknd == 2) then
                process_space_int (k + ivario - 1) = ip_arr + (igrid - 1) * idim1 * idim2 + &
                        iv_idx - 1
                increm(k + ivario - 1) = idim1
            elseif (iarknd == 3) then
                process_space_int (k + ivario - 1) = ip_arr + (igrid - 1) * idim1 * idim2 + &
                        (iv_idx - 1) * idim1
                increm(k + ivario - 1) = 1
            endif
            !
        enddo

        ! compute fluxes
        ipflux = (igrid - 1) * noflux * num_cells + iflux(iproc)
        ipknmr = (igrid - 1) * arrdm1(iiknmr) * arrdm2(iiknmr) + 1
        call procal (a, promnr(iproc), flux(ipflux:(noflux * num_cells * num_grids)), process_space_int(k:), increm(k:), &
                noseg2, noflux, iexpnt, iknmrk(ipknmr:), num_exchanges_u_dir, &
                num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, pronam(iproc), &
                iproc, dll_opb)

        ! the used grid is now the only actual value for the output
        do ivario = 1, prvnio(iproc)
            ityp = prvtyp(k + ivario - 1)
            if (ityp == 3 .or. ityp == 4 .or. ityp == 5) then
                ivar = prvvar(k + ivario - 1)
                iarr = vararr(ivar)
                iarknd = arrknd(iarr)

                ! only for space varying array's
                if (iarknd >= 2) then
                    do igr2 = 1, num_grids
                        vgrset(ivar, igr2) = 0
                    enddo
                    vgrset(ivar, igrid) = 1
                endif
            endif

            ! set fractional step array for dispersion and velocities from the processes

            if (ityp == 4) then
                iarr = vararr(ivar)
                if (iarr == 40) then
                    iv_idx = varidx(ivar)
                    if (iv_idx > 0) then
                        dspndt(iv_idx) = prondt(iproc)
                    endif
                endif
                if (iarr == 41) then
                    iv_idx = varidx(ivar)
                    if (iv_idx > 0) then
                        velndt(iv_idx) = prondt(iproc)
                    endif
                endif
            endif
        enddo

        ! Scale fluxes with fractional step
        !
        !     Dis-aggregate fluxes to base grid
        !     Dit is niet volgens ontwerp, flux zou op stof moeten werken
        !     En de stof daarna herverdeeld over volgens de oude verdeling.
        !     dus is deze herverdeling voor twee stoffen anders dus kunnen
        !     we niet eerst de flux op het basis nivo brengen
        !
        !     Oplossing zou zijn de flux voor iedere stof te disaggregeren
        !     met als gewicht de massa van die stof en dan meteen de deriv
        !     voor die stof met de fractional step te vullen. Vervolgens
        !     voor de volgende stof hetzelfde geintje te herhalen. Dus een
        !     loop over de stochi's toe te voegen. Op deze manier loop je voor een
        !     stof met veel fluxen wel eindeloss te aggregeren natuurlijk.
        !
        !     Dus zou je ook eerst deriv op geaggregeerd grid kunnen cummuleren en
        !     daarna eenmaal naar het basis grid. Dit betekend dat ook DERIV
        !     voor alle grids gedefinieerd moet zijn. We moeten er nu wel voor zorgen
        !     dat alle derivs voor een stof op hetzelfde grid komt of dat we alle
        !     derivs bij disaggregatie mee moeten nemen. Als we dan een vlaggetje
        !     per stof per grid meenemen of de deriv gevuld is doen we geen extra werk.
        !     Kunnen we de disaggregatie zo maken dat deze cummuleert in de variable op
        !     het target grid. PRODER vervalt hiermee, wat doen we met met de FLXDMP
        !     functionaliteit van PRODER hier is theoretisch nog een probleem .
        !     verder is nodig STOCHI, DTSTEP, VOLUME
        !
        !     NOG checken of VOLUME OP HET GRID ACTUEEL IS !!! PROBLEEM we
        !     weten het variable nummer van vol niet.
    end subroutine calculate_single_process

    subroutine update_derivaties_and_dump_fluxes(num_processes_activated, num_grids, noflux, num_vars, num_cells, &
            num_substances_total, progrd, grdnos, iflux, vgrset, &
            grdseg, volume, deriv, stochi, flux, &
            prondt, ibflag, isdmp, flxdmp, &
            bloom_status_ind, istep)

        !This routine has been split off from the 'calculate_single_process' routine.
        ! calculate_single_process is used in a parallel setting in such a way that previous processes
        ! always have completed the generation of input for the following processes.
        ! Conflicts nevertheless arose because more parallel instances of 'calculate_single_process'
        ! could together want to update the same derivative array. This is prevented
        ! by isolation of the update of the derivative array for all processes together
        ! in this separate routine outside of the parallel region of 'calculate_single_process'.

        !     Modified  :

        !     Subroutines called :  aggregate_extended - fills a variable on a specific grid from its values on another grid
        !                           prodr2 - updates the derivatives from the fluxes
        !                           profld - fills the dump array for fluxes used in a mass balance
        use aggregation, only: aggregate_extended, resample

        integer(kind = int_wp), intent(in) :: num_processes_activated                         !< Total number of processes
        integer(kind = int_wp), intent(in) :: num_grids                        !< Total number of grids
        integer(kind = int_wp), intent(in) :: noflux                        !< Total number of fluxes
        integer(kind = int_wp), intent(in) :: num_vars                         !< Total number of variables
        integer(kind = int_wp), intent(in) :: num_cells                         !< Total number of computational volumes
        integer(kind = int_wp), intent(in) :: num_substances_total                         !< Total number of substances
        integer(kind = int_wp), intent(in) :: progrd(num_processes_activated)                 !< The grid number of each process
        integer(kind = int_wp), intent(in) :: grdnos(num_grids)                !< The nummber of volumes in each grid
        integer(kind = int_wp), intent(in) :: iflux (num_processes_activated)!< Offset in the flux array per process
        integer(kind = int_wp), intent(inout) :: vgrset(num_vars, num_grids)!< Indicates whether a variable for a grid is set
        integer(kind = int_wp), intent(in) :: grdseg(num_cells, num_grids)!< Probably the aggregation pointer of the grids
        real(kind = real_wp), intent(inout) :: volume(num_cells, num_grids)         !< Computational volumes
        real(kind = real_wp), intent(inout) :: deriv (num_substances_total, num_cells, num_grids)!< Array with derivatives
        real(kind = real_wp), intent(in) :: stochi(num_substances_total, noflux)     !< Stoichiometric factors per flux
        real(kind = real_wp), intent(in) :: flux  (noflux, num_cells, num_grids) !< Process fluxes
        integer(kind = int_wp), intent(in) :: prondt(num_processes_activated)          !< Time step size of the process
        integer(kind = int_wp), intent(in) :: ibflag                        !< If > 0 then balances are required
        integer(kind = int_wp), intent(in) :: isdmp (num_cells)                 !< Segment to dumped segment pointer
        real(kind = real_wp), intent(inout) :: flxdmp(noflux, *)             !< Dumped fluxes
        integer(kind = int_wp), intent(in) :: bloom_status_ind                        !< The BLOOM  process if any
        integer(kind = int_wp), intent(in) :: istep                         !< Time step nr.

        ! Local variables
        integer(kind = int_wp) :: iproc              !< Index (loop counter) over processes
        integer(kind = int_wp) :: igrid              !< Grid nr of this process
        integer(kind = int_wp) :: noseg2             !< Number of computational volumes in this grid
        integer(kind = int_wp) :: nfluxp             !< Number of fluxes in this process
        integer(kind = int_wp), save :: ithandl = 0
        if (timon) call timstrt ("update_derivaties_and_dump_fluxes", ithandl)

        do iproc = 1, num_processes_activated
            if (iproc == bloom_status_ind) cycle
            if (mod(istep - 1, prondt(iproc)) /= 0) cycle

            ! See if this process produces fluxes
            if (iproc /= num_processes_activated) then
                nfluxp = iflux(iproc + 1) - iflux(iproc)
            else
                nfluxp = noflux - iflux(iproc) + 1
            endif
            if (nfluxp == 0) cycle

            ! If necessary set volume for this grid.
            igrid = progrd(iproc)
            noseg2 = grdnos(igrid)
            if (vgrset(1, igrid) /= 1) then  !
                call aggregate_extended(num_cells, noseg2, 1, 1, 1, &
                        1, 1, 1, 1, 1, &
                        grdseg(1, igrid), 1, volume, volume, volume, &
                        volume(1, igrid))
                vgrset(1, igrid) = 1              !  Volume is always variable 1
            endif

            ! Construct derivatives from these fluxes on this grid
            call prodr2(deriv(1, 1, igrid), num_substances_total, noflux, stochi, iflux (iproc), &
                    nfluxp, flux(1, 1, igrid), noseg2, volume(1, igrid), prondt(iproc))

            ! For the use in balances, store fluxes in 'flxdmp' using aggregation pointer 'isdmp'
            if (ibflag > 0) then
                call profld(noflux, iflux (iproc), nfluxp, igrid, noseg2, &
                        num_cells, prondt(iproc), isdmp, grdseg, flux(1, 1, igrid), &
                        volume, flxdmp)
            endif
        enddo

        if (timon) call timstop (ithandl)

    end subroutine update_derivaties_and_dump_fluxes

    !> Sets an explicit time step from DERIV.
    subroutine set_explicit_time_step_for_derivatives (conc, amass, deriv, volume, idt, &
            num_substances_transported, num_substances_total, num_cells, file_unit_list, ivflag, &
            surfac)

        integer(kind = int_wp), intent(in) :: num_substances_transported          !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                !< total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                   !< number of computational volumes
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells)!< concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: amass(num_substances_total, num_cells)!< masses per substance per volume
        real(kind = real_wp), intent(inout) :: deriv(num_substances_total, num_cells)!< derivatives per substance per volume
        real(kind = real_wp), intent(inout) :: volume(num_cells)          !< volumes of the segments
        integer(kind = int_wp), intent(in) :: idt                         !< integration time step size
        integer(kind = int_wp), intent(in) :: file_unit_list                     !< unit number of the monitoring file
        integer(kind = int_wp), intent(in) :: ivflag                  !< if 1 computational volumes
        real(kind = real_wp), intent(in) :: surfac(num_cells)           !< horizontal surface

        integer(kind = int_wp) :: cell_i                    !  segment loop counter
        integer(kind = int_wp) :: i                       !  substance loop counter
        real(kind = real_wp) :: v1                      !  segment volume
        real(kind = real_wp) :: a                       !  segment mass
        integer(kind = int_wp), save :: ivmess = 0              !  count messages on small volumes

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("set_explicit_time_step_for_derivatives", ithandl)

        ! loop accross the number of computational elements

        do cell_i = 1, num_cells
            ! compute volumes if necessary

            if (ivflag == 1) volume(cell_i) = amass(1, cell_i) + idt * deriv(1, cell_i)
            v1 = volume(cell_i)
            if (abs(v1)<1.0e-25) then
                if (ivmess < 25) then
                    ivmess = ivmess + 1
                    write (file_unit_list, 1000) cell_i, v1
                elseif (ivmess == 25) then
                    ivmess = ivmess + 1
                    write (file_unit_list, 1001)
                endif
                volume (cell_i) = 1.0
                v1 = 1.0
            endif

            !  active substances first
            do i = 1, num_substances_transported
                a = amass(i, cell_i) + idt * deriv(i, cell_i)
                amass(i, cell_i) = a
                conc (i, cell_i) = a / v1
                deriv(i, cell_i) = 0.0
            enddo

            ! then the inactive substances
            do i = num_substances_transported + 1, num_substances_total
                amass(i, cell_i) = amass(i, cell_i) + idt * deriv(i, cell_i)
                conc (i, cell_i) = amass(i, cell_i) / max(tiny(1.0), surfac(cell_i))
                deriv(i, cell_i) = 0.0
            enddo

        enddo

        1000 format ('volume of segment:', i7, ' is:', e15.6, ' 1.0 assumed.')
        1001 format ('25 or more zero volumes , further messages surpressed')

        if (timon) call timstop (ithandl)
    end subroutine set_explicit_time_step_for_derivatives

end module m_process_calculation
