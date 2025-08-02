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
module m_write_output
    use m_waq_precision
    use m_exchange_values, only: exchange_values
    use m_write_balance_output
    use m_write_monitoring_output
    use m_write_binary_output, only: write_binary_history_output, write_binary_map_output
    use m_write_nefis_output, only: write_nefis_history_output, write_nefis_map_output
    use m_write_netcdf_output
    use m_prepare_output_data, only: write_concentrations_in_grid_layout, fill_output_buffer_base_grid, &
            fill_transport_terms_transects, fill_output_buffer_sub_grid, fill_transect_output_buffer, &
            fill_dump_areas_balances, update_base_grid_local_array, calculate_balance_terms, &
            write_balance_history_output
    use m_write_restart_map_file, only: write_restart_map_file
    use timers, only: evaluate_timers

    implicit none

    private
    public :: write_output

contains


    !> Manages output: verifies which output is required for the current time,
    !! and calls the corresponding subroutines to generate that output.
    subroutine write_output(num_substances_total, num_cells, num_spatial_parameters, num_spatial_time_fuctions, itime, &
            moname, syname, duname, idump, num_monitoring_points, &
            conc, cons, param, func, segfun, &
            volume, num_constants, num_time_functions, idt, num_output_files, &
            file_name_list, file_unit_list, ioutps, iopoin, riobuf, &
            ousnm, ouuni, oudsc, sysnm, syuni, sydsc, &
            ounam, num_cells_u_dir, num_cells_v_dir, lgrid, cgrid, &
            num_substances_transported, bound, ip, amass, amass2, &
            asmass, noflux, flxint, isflag, iaflag, &
            ibflag, imstrt, imstop, imstep, idstrt, &
            idstop, idstep, ihstrt, ihstop, ihstep, &
            imflag, idflag, ihflag, num_local_vars, proloc, &
            num_defaults, defaul, itstrt, itstop, ndmpar, &
            danam, ndmpq, num_monitoring_cells, iqdmp, isdmp, &
            ipdmp, dmpq, dmps, flxdmp, ntdmpq, &
            nambuf, num_transects, num_transect_exchanges, ioraai, nqraai, &
            iqraai, trraai, ranam, stochi, num_grids, &
            num_vars, vararr, varidx, vartda, vardag, &
            arrknd, arrpoi, arrdm1, arrdm2, vgrset, &
            grdnos, grdseg, a, num_boundary_conditions, num_boundary_types, &
            bndtyp, inbtyp, coname, num_exchanges, ipoint, &
            intopt, paname, funame, sfname, dmpbal, &
            num_waste_loads, num_waste_load_types, wsttyp, iwaste, inwtyp, &
            wstdmp, iknmrk, isegcol)

        !
        use m_array_manipulation, only : initialize_real_array
        use m_logger_helper, only : stop_with_error
        use m_cli_utils, only : is_command_arg_specified
        use timers
        use results
        use nan_check_module

        integer(kind = int_wp), intent(in   ) :: num_substances_total                   !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                   !< Number of cells or segments
        integer(kind = int_wp), intent(in   ) :: num_spatial_parameters                    !< Number of parameters
        integer(kind = int_wp), intent(in   ) :: num_spatial_time_fuctions                  !< Number of segment functions
        integer(kind = int_wp), intent(in   ) :: itime                   !< Time in system clock units
        integer(kind = int_wp), intent(in   ) :: num_monitoring_points                  !< Number of dump locations
        character(len=40)     , intent(in   ) :: moname(4)               !< Model and run names
        character(len=20)     , intent(in   ) :: syname(num_substances_total)           !< Names of substances, syname(*)
        character(len=20)     , intent(in   ) :: duname(num_monitoring_points)          !< Names of dump locations, duname(*)
        integer(kind = int_wp), intent(in   ) :: idump(num_monitoring_points)           !< Dump segment numbers
        real(kind = real_wp)  , intent(inout) :: conc(num_substances_total,num_cells)       !< Model concentrations
        real(kind = real_wp)  , intent(inout) :: cons(*)                 !< Model constants
        real(kind = real_wp)  , intent(inout) :: param(num_spatial_parameters,num_cells)       !< Model parameters
        real(kind = real_wp)  , intent(inout) :: func(*)                 !< Model functions at ITIME
        real(kind = real_wp)  , intent(inout) :: segfun(num_cells,num_spatial_time_fuctions)    !< Segment functions at ITIME
        real(kind = real_wp)  , intent(in   ) :: volume(num_cells)           !< Segment volumes, volume(*)
        integer(kind = int_wp), intent(in   ) :: num_constants                  !< Number of constants used
        integer(kind = int_wp), intent(in   ) :: num_time_functions                   !< Number of functions ( user )
        integer(kind = int_wp), intent(in   ) :: idt                     !< Simulation timestep
        integer(kind = int_wp), intent(in   ) :: num_output_files                   !< Number of output files
        character(len=*)      , intent(in   ) :: file_name_list(*)       !< File names
        integer(kind = int_wp), intent(inout) :: file_unit_list(*)       !< Uint numbers
        integer(kind = int_wp), intent(inout) :: ioutps(7, num_output_files)        !< Output structure, ioutps(7, *)
                                                                         !< Index 1 = start time
                                                                         !< Index 2 = stop time
                                                                         !< Index 3 = time step
                                                                         !< Index 4 = number of vars
                                                                         !< Index 5 = kind of output
                                                                         !< Index 6 = grid of output
                                                                         !< Index 7 = initialize flag
        integer(kind = int_wp), intent(in   ) :: iopoin(*)               !< Pointer to DELWAQ array's
        real(kind=real_wp),     intent(inout) :: riobuf(*)               !< Buffer for input and output
        character(len=20)     , intent(in   ) :: ounam (*)               !< Name of output variable
        integer(kind = int_wp), intent(in   ) :: num_cells_u_dir                      !< Width of output grid
        integer(kind = int_wp), intent(in   ) :: num_cells_v_dir                      !< Depth of output grid
        integer(kind = int_wp), intent(in   ) :: lgrid(num_cells_u_dir, num_cells_v_dir)           !< Grid-layout, lgrid(*)
        integer(kind = int_wp), intent(in   ) :: num_substances_transported                   !< Number of active substances
        real(kind = real_wp)  , intent(in   ) :: bound(*)                !< Bounary conditions
        integer(kind = int_wp), intent(inout) :: ip(*)                   !< Paging structure
        real(kind = real_wp)  , intent(in   ) :: amass(num_substances_total,num_cells)      !< Mass array
        real(kind = real_wp)  , intent(inout) :: amass2(num_substances_total,5)         !< Cummulative balance on whole
        real(kind = real_wp)  , intent(inout) :: asmass(*)               !< Cummulative balance per segment
        integer(kind = int_wp), intent(in   ) :: noflux                  !< Number of fluxes
        real(kind = real_wp)  , intent(inout) :: flxint(*)               !< Integrated fluxes at dump segments, flxint(noflux,ndmpar)
        integer(kind = int_wp), intent(in   ) :: isflag                  !< If 1 then dd-hh:mm'ss"
        integer(kind = int_wp), intent(  out) :: iaflag                  !< If 1 then accumulate mass bal
        integer(kind = int_wp), intent(in   ) :: ibflag                  !< Flag = 1 then balances
        integer(kind = int_wp), intent(in   ) :: imstrt                  !< Monitoring start time ( scu )
        integer(kind = int_wp), intent(in   ) :: imstop                  !< Monitoring stop time ( scu )
        integer(kind = int_wp), intent(in   ) :: imstep                  !< Monitoring time step ( scu )
        integer(kind = int_wp), intent(in   ) :: idstrt                  !< Dump start time ( scu )
        integer(kind = int_wp), intent(in   ) :: idstop                  !< Dump stop time ( scu )
        integer(kind = int_wp), intent(in   ) :: idstep                  !< Dump time step ( scu )
        integer(kind = int_wp), intent(in   ) :: ihstrt                  !< History start time ( scu )
        integer(kind = int_wp), intent(in   ) :: ihstop                  !< History stop time ( scu )
        integer(kind = int_wp), intent(in   ) :: ihstep                  !< History time step ( scu )
        logical               , intent(  out) :: imflag                  !< If .T. then monitor step
        logical               , intent(  out) :: idflag                  !< If .T. then dump step
        logical               , intent(  out) :: ihflag                  !< If .T. then history step
        integer(kind = int_wp), intent(in   ) :: num_local_vars                   !< Number of variables in PROLOC
        integer(kind = int_wp), intent(in   ) :: num_defaults                   !< Number of used defaults
        real(kind = real_wp)  , intent(in   ) :: defaul(*)               !< Default proces parameters
        integer(kind = int_wp), intent(in   ) :: itstrt                  !< Start time
        integer(kind = int_wp), intent(in   ) :: itstop                  !< Stop time
        integer(kind = int_wp), intent(in   ) :: ndmpar                  !< Number of dump areas
        character(len=20)     , intent(in   ) :: danam(ndmpar)           !< Dump area names, danam(*)
        integer(kind = int_wp), intent(in   ) :: ndmpq                   !< Number of dumped exchanges
        integer(kind = int_wp), intent(in   ) :: num_monitoring_cells
        integer(kind = int_wp), intent(in   ) :: iqdmp(*)                !< Exchange to dumped exchange pointer
        integer(kind = int_wp), intent(in   ) :: isdmp(*)                !< Segment to dumped segment pointer
        integer(kind = int_wp), intent(in   ) :: ipdmp(*)                !< Pointer structure dump areas
        real(kind = real_wp)  , intent(in   ) :: dmpq(*)                 !< Mass balance dumped segments
        real(kind = real_wp)  , intent(in   ) :: dmps(*)                 !< Mass balance dumped exchange
        real(kind = real_wp)  , intent(in   ) :: flxdmp(*)               !< Integrated fluxes
        integer(kind = int_wp), intent(in   ) :: ntdmpq                  !< Total number exchanges in dump area
        character(len=20)     , intent(inout) :: nambuf(*)               !< Buffer for names
        integer(kind = int_wp), intent(in   ) :: num_transects
        integer(kind = int_wp), intent(in   ) :: num_transect_exchanges
        integer(kind = int_wp), intent(in   ) :: ioraai(*)               !< Output option for transect
        integer(kind = int_wp), intent(in   ) :: nqraai(*)               !< Number of exchanges in transect
        integer(kind = int_wp), intent(in   ) :: iqraai(*)               !< Exchanges in transect
        real(kind = real_wp)  , intent(inout) :: trraai(num_substances_transported, *)        !< Cummulative transport over transect
        character(len=20)     , intent(in   ) :: ranam(*)                !< transects names
        real(kind = real_wp)  , intent(in   ) :: stochi(num_substances_total, noflux)   !< Proces stochiometry
        integer(kind = int_wp), intent(in   ) :: intopt                  !< Integration and balance suboptions
        integer(kind = int_wp), intent(in   ) :: num_grids                  !< Number of grids
        integer(kind = int_wp), intent(in   ) :: num_vars                   !< Number of variables
        integer(kind = int_wp), intent(in   ) :: num_boundary_conditions                   !< Number of boundaries
        integer(kind = int_wp), intent(in   ) :: num_boundary_types                  !< Number of boundary types
        integer(kind = int_wp), intent(in   ) :: num_exchanges                     !< Number of exchanges
        integer(kind = int_wp), intent(in   ) :: iknmrk(num_cells)           !< Feature array. Bit zero set means active.
        integer(kind = int_wp), intent(in   ) :: vararr(num_vars)           !< Variable array number
        integer(kind = int_wp), intent(in   ) :: varidx(num_vars)           !< Variable index in array
        integer(kind = int_wp), intent(in   ) :: vartda(num_vars)           !< Type of disaggregation
        integer(kind = int_wp), intent(in   ) :: vardag(num_vars)           !< Variable disaggr. weight var.
        integer(kind = int_wp), intent(in   ) :: arrknd(*)               !< Kind of array
        integer(kind = int_wp), intent(in   ) :: arrpoi(*)               !< Array pointer in A
        integer(kind = int_wp), intent(in   ) :: arrdm1(*)               !< First dimension
        integer(kind = int_wp), intent(in   ) :: arrdm2(*)               !< Second dimension
        integer(kind = int_wp), intent(in   ) :: vgrset(num_vars, *)        !< Actual indication (inout in actloc????)
        integer(kind = int_wp), intent(in   ) :: grdnos(num_grids)          !< Number of segments in grid
        integer(kind = int_wp), intent(in   ) :: grdseg(num_cells, num_grids)   !< Segment pointering
        integer(kind = int_wp), intent(in   ) :: inbtyp(*)               !< Index for boundary type
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)          !< Pointer array
        real(kind = real_wp),   intent(inout) :: proloc(*)               !< Process local array for output
        real(kind = real_wp),   intent(inout) :: a(*)                    !< Real array work space
        character(len=20),      intent(inout) :: cgrid(*)                !< Char buffer for dmp output
        character(len=20)     , intent(in   ) :: bndtyp(*)               !< Boundary types names
        character(len=20)     , intent(in   ) :: coname(*)               !< Constant names
        character(len=20)     , intent(in   ) :: paname(*)               !< Parameters names
        character(len=20)     , intent(in   ) :: funame(*)               !< Function names
        character(len=20)     , intent(in   ) :: sfname(*)               !< Segment function names
        character(len=100)    , intent(in   ) :: ousnm(*)                !< Output variables standard names
        character(len=100)    , intent(in   ) :: sysnm(*)                !< Standard substances names
        character(len=40)     , intent(in   ) :: ouuni(*)                !< Units of output variables
        character(len=40)     , intent(in   ) :: syuni(*)                !< Standard substances units
        character(len=60)     , intent(in   ) :: oudsc(*)                !< Output descriptions
        character(len=60)     , intent(in   ) :: sydsc(*)                !< Standard substances descriptions
        integer(kind = int_wp), intent(in   ) :: dmpbal(ndmpar)          !< If ==1, then dump area is included in the balance
        integer(kind = int_wp), intent(in   ) :: num_waste_loads                   !< Number of wasteloads
        integer(kind = int_wp), intent(in   ) :: num_waste_load_types                  !< Number of wasteload types
        character(len = 20),    intent(in   ) :: wsttyp(num_waste_load_types)          !< Wasteload types names
        integer(kind = int_wp), intent(in   ) :: iwaste(num_waste_loads)           !< Segment numbers of the wasteloads
        integer(kind = int_wp), intent(in   ) :: inwtyp(num_waste_loads)           !< Wasteload type number (index in wsttyp)
        real(kind = real_wp),   intent(in   ) :: wstdmp(num_substances_total, num_waste_loads, 2) !< Accumulated wasteloads 1/2 in and out
        integer(kind = int_wp), intent(in   ) :: isegcol(*)              !< Pointer from segment to top of column

        ! Local variables
        character(len = 100), allocatable :: hnc_standard(:)
        character(len = 40), allocatable :: hnc_unit(:)
        character(len = 60), allocatable :: hnc_description(:)
        integer(kind = int_wp), parameter :: igseg = 1
        integer(kind = int_wp), parameter :: igmon = 2
        integer(kind = int_wp), parameter :: iggrd = 3
        integer(kind = int_wp), parameter :: igsub = 4
        integer(kind = int_wp), parameter :: luoff = 18
        integer(kind = int_wp), parameter :: luoff2 = 36
        integer(kind = int_wp) :: k1, iostrt, iostop, iostep, nrvar, &
                isrtou, igrdou, iniout, lunout, iout, &
                ierr, ierr2, i, i1, i2, &
                ifi, ncout, nrvar2, nrvar3, ip1, &
                iof, nsegou
        character(len = 255) lchout
        character(len = 20)  name
        logical       loflag, lmfirs, ldfirs, lhfirs, ldummy, lnonans
        logical       lget, lread
        real(kind = real_wp), allocatable :: surf(:)
        
        logical, save :: lnancheck ! Do check on NAN in conc array
        logical, save :: first = .true.

        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp), save :: mncrec = 0  !< netCDF map
        integer(kind = int_wp), save :: hncrec = 0  !< netCDF history
        integer(kind = int_wp), save :: timeid      
        integer(kind = int_wp), save :: bndtimeid   !< netCDF map
        integer(kind = int_wp), save :: timeidh
        integer(kind = int_wp), save :: bndtimeidh  !< netCDF history

        integer(kind = int_wp), allocatable, save :: mncwqid1(:, :), mncwqid2(:, :) ! netCDF map
        integer(kind = int_wp), allocatable, save :: hncwqid1(:, :), hncwqid2(:, :) ! netCDF history
        
        real(kind = dp) :: damass2(num_substances_total, 5)

        if (timon) call timstrt ("write_output", ithandl)

        if (first) then
            allocate(mncwqid1(num_substances_total, 3), mncwqid2(num_vars, 3))
            allocate(hncwqid1(num_substances_total, 2), hncwqid2(num_vars, 2))
            ! allow switching of NAN concentrations check
            lnancheck = .not. is_command_arg_specified('-nonancheck')
            first = .false.
        endif

        if (lnancheck) then
            ! Check for NANs in the concentration array
            lunout = file_unit_list(19)
            lnonans = nan_check(conc, 'conc(num_substances_total, num_cells)', lunout, 1, 1)
            if (.not. lnonans) then
                write(lunout, '(/A)')  '  ERROR : NAN found the concentration array, ending calculation.'
                write(*, '(/A)')  '  ERROR : NAN found the concentration array, ending calculation. See location in mon-file.'
                write(lunout, '(A)')   '          Current concentration fields written to _res.map.'
                write(*, '(A)')   '          Current concentration fields written to _res.map.'
                write(lunout, '(/A/)') '  INFO  : If you don''t want NAN checks, use -nonancheck at command line.'
                write(*, '(/A/)') '  INFO  : If you don''t want NAN checks, use -nonancheck at command line.'
                call write_restart_map_file(file_unit_list, file_name_list, conc, itime, moname, syname, num_substances_total, num_cells)
                call stop_with_error()
            endif
        endif

        ! Evaluate standard DELWAQ output timers
        call evaluate_timers(itime, idt, imstrt, imstop, imstep, imflag, lmfirs)
        call evaluate_timers(itime, idt, idstrt, idstop, idstep, idflag, ldfirs)
        call evaluate_timers(itime, idt, ihstrt, ihstop, ihstep, ihflag, lhfirs)

        ! Fill mass in AMASS2 array by summing AMASS over all segments
        if (imflag) then
            damass2 = amass2
            iaflag = 1
            do i2 = 1, num_substances_total
                amass2(i2, 1) = 0.0
                do i1 = 1, num_cells
                    damass2(i2, 1) = damass2(i2, 1) + amass(i2, i1)
                enddo
            enddo
            amass2 = damass2
        endif

        ! Fill mass in ASMASS array using DMPQ and DMPS
        if (imflag .or. (ihflag .and. num_transects > 0)) then
            if (ibflag == 1) then
                call fill_dump_areas_balances(num_substances_total, num_substances_transported, noflux, ndmpar, ndmpq, &
                        num_monitoring_cells, ntdmpq, iqdmp, isdmp, ipdmp, &
                        dmpq, amass, dmps, flxdmp, asmass, &
                        flxint)
            endif

            if (num_transects > 0) then
                if (lhfirs) then
                    call initialize_real_array(trraai, num_transects * num_substances_transported)
                else
                    call fill_transport_terms_transects(num_substances_transported, ndmpq, num_transects, num_transect_exchanges, ioraai, &
                            nqraai, iqraai, iqdmp, dmpq, trraai)
                endif
            endif

        endif

        ! Initialize K1, pointer in IOPOIN and OUNAM
        lread = .true.
        k1 = 1

        ! Loop over the output files
        do iout = 1, num_output_files
            !
            !        Map output structure to single variables part 1
            !
            iostrt = ioutps(1, iout)
            iostop = ioutps(2, iout)
            iostep = ioutps(3, iout)
            nrvar = ioutps(4, iout)
            !
            !        Output required ?
            !
            call evaluate_timers (itime, idt, iostrt, iostop, iostep, &
                    loflag, ldummy)
            !
            if (.not. loflag) goto 100
            !
            !        Map output structure to single variables part 2
            !
            isrtou = ioutps(5, iout)
            igrdou = ioutps(6, iout)
            iniout = ioutps(7, iout)
            if (iout <= 4) then
                ifi = iout + luoff
            elseif (iout <= 7) then
                ifi = iout + luoff2 - 4
            else
                ifi = iout + luoff2 - 2
            endif
            lunout = file_unit_list(ifi)
            lchout = file_name_list(ifi)

            ! No balance output if they are not active
            if ((isrtou == ibal .or. isrtou == iba2 .or. &
                    isrtou == iba2) .and. ibflag /= 1) goto 100

            ! Set all local variables used active on base grid
            call update_base_grid_local_array (iopoin, nrvar, num_constants, num_spatial_parameters, num_time_functions, &
                    num_spatial_time_fuctions, num_substances_total, num_cells, num_local_vars, num_grids, &
                    num_vars, vararr, varidx, vartda, vardag, &
                    arrknd, arrpoi, arrdm1, arrdm2, vgrset, &
                    grdnos, grdseg, a)

            ! Fill output buffer
            if (isrtou == iba2) then

                call calculate_balance_terms(num_substances_total, noflux, ndmpar, nrvar, stochi, flxint, asmass, riobuf)

            elseif (isrtou == iba3) then
                !     jos doet het zelf
            elseif (igrdou == igsub) then
                if (isrtou == imo3 .or. &
                        isrtou == ihi3 .or. &
                        isrtou == ihnc3 .or. &
                        isrtou == ihn3) then
                    ncout = num_substances_total
                else
                    ncout = 0
                endif
                nrvar2 = nrvar / 2
                ! For the dump area's
                call fill_output_buffer_sub_grid(riobuf, iopoin(k1), nrvar2, num_constants, num_spatial_parameters, &
                        num_time_functions, num_spatial_time_fuctions, num_substances_total, conc, segfun, &
                        func, param, cons, idt, itime, &
                        volume, num_cells, num_substances_transported, ndmpar, ipdmp, &
                        bound, num_local_vars, proloc, num_defaults, defaul, &
                        ncout, ntdmpq, paname, sfname, funame, &
                        danam)

                ! For the transects
                if ((isrtou == ihi3 .or. &
                        isrtou == ihnc3 .or. &
                        isrtou == ihn3) .and. &
                        num_transects > 0) then
                    nrvar3 = num_substances_total + nrvar2
                    ip1 = (ncout + nrvar2) * ndmpar + 1
                    call fill_transect_output_buffer(riobuf(ip1), nrvar3, trraai, num_transects, num_substances_transported)
                endif
            else
                nrvar2 = nrvar
                call fill_output_buffer_base_grid(riobuf, iopoin(k1), nrvar, num_constants, num_spatial_parameters, &
                        num_time_functions, num_spatial_time_fuctions, num_substances_total, conc, segfun, &
                        func, param, cons, idt, itime, &
                        volume, num_cells, num_substances_transported, num_monitoring_points, idump, &
                        num_cells_u_dir, num_cells_v_dir, lgrid, igrdou, bound, &
                        num_local_vars, proloc, num_defaults, defaul)
            endif

            ! Fill character buffer with substance names and output names
            if (isrtou == imnf .or. &
                    isrtou == ihnf .or. &
                    isrtou == ihnf .or. &
                    isrtou == ihnc3 .or. &
                    isrtou == imo3 .or. &
                    isrtou == ihi3 .or. &
                    isrtou == ihn3) then

                if (allocated(hnc_standard)) then
                    deallocate(hnc_standard)
                    deallocate(hnc_unit)
                    deallocate(hnc_description)
                endif

                allocate(hnc_standard(num_substances_total + nrvar2))
                allocate(hnc_unit(num_substances_total + nrvar2))
                allocate(hnc_description(num_substances_total + nrvar2))

                do i = 1, num_substances_total
                    nambuf(i) = syname(i)
                    hnc_standard(i) = sysnm(i)
                    hnc_unit(i) = syuni(i)
                    hnc_description(i) = sydsc(i)
                end do
                do i = 1, nrvar2
                    nambuf(num_substances_total + i) = ounam(k1 + i - 1)
                    hnc_standard(num_substances_total + i) = ousnm(k1 + i - 1)
                    hnc_unit(num_substances_total + i) = ouuni(k1 + i - 1)
                    hnc_description(num_substances_total + i) = oudsc(k1 + i - 1)
                end do
            endif

            ! Perform output
            if (isrtou == imon) then

                call write_monitoring_output (lunout, idump, conc, amass2, itime, &
                        duname, syname, moname, num_monitoring_points, num_substances_total, &
                        ip, isflag, asmass, ibflag, nrvar, &
                        ounam(k1), riobuf, itstrt, itstop, ndmpar, &
                        danam)

            elseif (isrtou == imo2) then

                call write_monitoring_output (lunout, idump, conc, amass2, itime, &
                        duname, syname, moname, num_monitoring_points, 0, &
                        ip, isflag, asmass, ibflag, nrvar, &
                        ounam(k1), riobuf, itstrt, itstop, ndmpar, &
                        danam)
                !
            elseif (isrtou == imo3) then
                !
                call outmo3 (lunout, amass2, itime, syname, moname, &
                        num_substances_total, ip, isflag, asmass, ibflag, &
                        nrvar2, ounam(k1), riobuf, itstrt, itstop, &
                        ndmpar, danam)
                !
            elseif (isrtou == imo4) then
                !
                call outmo3 (lunout, amass2, itime, syname, moname, &
                        0, ip, isflag, asmass, ibflag, &
                        nrvar2, ounam(k1), riobuf, itstrt, itstop, &
                        ndmpar, danam)
                !
            elseif (isrtou == idmp) then
                !
                call write_concentrations_in_grid_layout (lunout, lchout, itime, moname, num_cells_u_dir, &
                        num_cells_v_dir, lgrid, cgrid, num_substances_total, num_substances_transported, &
                        syname, conc, bound, nrvar, ounam(k1), &
                        riobuf, ip(5), isflag, iniout)
                !
            elseif (isrtou == idm2) then
                !
                call write_concentrations_in_grid_layout (lunout, lchout, itime, moname, num_cells_u_dir, &
                        num_cells_v_dir, lgrid, cgrid, 0, 0, &
                        syname, conc, bound, nrvar, ounam(k1), &
                        riobuf, ip(5), isflag, iniout)
                !
            elseif (isrtou == ihis) then
                !
                call write_binary_history_output(lunout, lchout, itime, moname, num_monitoring_points, &
                        idump, duname, num_substances_total, syname, conc, &
                        nrvar, ounam(k1), riobuf, iniout)

            elseif (isrtou == ihnf) then

                iof = nrvar * num_monitoring_points + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, num_cells, &
                        num_substances_total, conc, nambuf, nrvar, riobuf, &
                        iostrt, iostop, iostep, num_monitoring_points, idump, &
                        duname, riobuf(iof), iniout)

            elseif (isrtou == ihnc) then
                hncrec = hncrec + 1
                iof = nrvar * num_monitoring_points + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, duname, num_monitoring_points, num_substances_total, &
                        conc, syname, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar, riobuf, &
                        ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), &
                        hncwqid2, file_unit_list(19))

            elseif (isrtou == ihi2) then

                call write_binary_history_output(lunout, lchout, itime, moname, num_monitoring_points, &
                        idump, duname, 0, syname, conc, &
                        nrvar, ounam(k1), riobuf, iniout)

            elseif (isrtou == ihn2) then

                iof = nrvar * num_monitoring_points + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, num_cells, &
                        0, conc, ounam(k1), nrvar, riobuf, &
                        iostrt, iostop, iostep, num_monitoring_points, idump, &
                        duname, riobuf(iof), iniout)

            elseif (isrtou == ihnc2) then

                hncrec = hncrec + 1
                iof = nrvar * num_monitoring_points + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, duname, num_monitoring_points, 0, &
                        conc, syname, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar, riobuf, &
                        ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), &
                        hncwqid2, file_unit_list(19))
                !
            elseif (isrtou == ihi3) then

                ! Let op RANAM achter DANAM
                nrvar3 = num_substances_total + nrvar2
                nsegou = ndmpar + num_transects
                call write_binary_history_output(lunout, lchout, itime, moname, nsegou, &
                        idump, danam, 0, syname, conc, &
                        nrvar3, nambuf, riobuf, iniout)

            elseif (isrtou == ihn3) then

                ! Let op RANAM achter DANAM
                nrvar3 = num_substances_total + nrvar2
                nsegou = ndmpar + num_transects
                iof = nrvar3 * nsegou + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, num_cells, &
                        0, conc, nambuf, nrvar3, riobuf, &
                        iostrt, iostop, iostep, nsegou, idump, &
                        danam, riobuf(iof), iniout)

            elseif (isrtou == ihnc3) then

                ! Let op RANAM achter DANAM
                hncrec = hncrec + 1
                nrvar3 = num_substances_total + nrvar2
                nsegou = ndmpar + num_transects
                iof = nrvar3 * nsegou + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, danam, nsegou, 0, &
                        conc, nambuf, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar3, riobuf, &
                        nambuf, hnc_standard, hnc_unit, &
                        hnc_description, hncwqid2, file_unit_list(19))

            elseif (isrtou == ihi4) then

                call write_binary_history_output(lunout, lchout, itime, moname, ndmpar, &
                        idump, danam, 0, syname, conc, &
                        nrvar2, ounam(k1), riobuf, iniout)

            elseif (isrtou == ihn4) then
                iof = nrvar2 * ndmpar + 1
                call write_nefis_history_output(lunout, lchout, itime, moname, num_cells, &
                        0, conc, ounam(k1), nrvar2, riobuf, &
                        iostrt, iostop, iostep, ndmpar, idump, &
                        danam, riobuf(iof), iniout)

            elseif (isrtou == ihnc4) then

                hncrec = hncrec + 1
                iof = nrvar2 * ndmpar + 1
                call write_netcdf_history_output(file_unit_list(47), file_name_list(47), file_name_list(46), timeidh, &
                        bndtimeidh, hncrec, itime, moname, &
                        idump, danam, nsegou, 0, &
                        conc, syname, sysnm, syuni, &
                        sydsc, hncwqid1, nrvar2, riobuf, &
                        ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), &
                        hncwqid2, file_unit_list(19))

            elseif (isrtou == imap) then

                call write_binary_map_output (lunout, lchout, itime, moname, num_cells, &
                        num_substances_total, conc, syname, nrvar, riobuf, &
                        ounam(k1), iknmrk, iniout)

            elseif (isrtou == imnf) then

                iof = nrvar * num_cells + 1
                call write_nefis_map_output(lunout, lchout, itime, moname, num_cells, num_substances_total, conc, syname, nrvar, riobuf, &
                        ounam(k1), iostrt, iostop, iostep, riobuf(iof), iniout)

            elseif (isrtou == imnc) then

                mncrec = mncrec + 1
                call write_netcdf_map_output(file_unit_list(49), file_name_list(49), file_name_list(46), timeid, bndtimeid, mncrec, &
                        itime, moname, num_cells, num_substances_total, conc, syname, sysnm, syuni, sydsc, mncwqid1, nrvar, &
                        riobuf, ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), mncwqid2, volume, iknmrk, file_unit_list(19))

            elseif (isrtou == ima2) then

                call write_binary_map_output (lunout, lchout, itime, moname, num_cells, 0, conc, syname, nrvar, riobuf, &
                        ounam(k1), iknmrk, iniout)

            elseif (isrtou == imn2) then

                iof = nrvar * num_cells + 1
                call write_nefis_map_output(lunout, lchout, itime, moname, num_cells, 0, conc, syname, nrvar, riobuf, &
                        ounam(k1), iostrt, iostop, iostep, riobuf(iof), iniout)

            elseif (isrtou == imnc2) then

                mncrec = mncrec + 1
                call write_netcdf_map_output(file_unit_list(49), file_name_list(49), file_name_list(46), timeid, bndtimeid, mncrec, &
                        itime, moname, num_cells, 0, &
                        conc, syname, sysnm, syuni, sydsc, mncwqid1, nrvar, &
                        riobuf, ounam(k1), ousnm(k1), ouuni(k1), oudsc(k1), mncwqid2, &
                        volume, iknmrk, file_unit_list(19))

            elseif (isrtou == ibal) then

                call write_balance_history_output(lunout, itime, moname, num_substances_total, noflux, syname, ndmpar, danam, asmass, &
                        flxint, nrvar2, riobuf, iniout)

            elseif (isrtou == iba2) then

                call write_binary_history_output(lunout, lchout, itime, moname, ndmpar, idump, danam, 0, syname, conc, &
                        nrvar, ounam(k1), riobuf, iniout)
            elseif (isrtou == iba3) then
                allocate(surf(num_cells))
                name = 'SURF'
                lget = .true.
                call exchange_values (name, num_cells, surf, num_constants, num_spatial_parameters, &
                        num_time_functions, num_spatial_time_fuctions, cons, coname, param, &
                        paname, func, funame, segfun, sfname, &
                        lget, ierr)

                call write_balance_text_output(num_substances_total, itime, num_substances_transported, noflux, ndmpar, &
                        ndmpq, ntdmpq, itstop, imstrt, imstop, &
                        iqdmp, ipdmp, asmass, flxint, stochi, &
                        syname, danam, moname, dmpq, num_boundary_conditions, &
                        num_boundary_types, bndtyp, inbtyp, num_constants, coname, &
                        cons, num_exchanges, ipoint, ounam(k1), intopt, &
                        volume, surf, num_cells, lunout, lchout, &
                        iniout, dmpbal, num_waste_loads, num_waste_load_types, wsttyp, &
                        iwaste, inwtyp, wstdmp, isegcol, imstep)

                file_unit_list(ifi) = lunout ! Ad hoc: routine open_waq_files sets the LU-number via newunit
                deallocate (surf)

            endif

            ioutps(7, iout) = iniout

            100    continue

            ! Update K1, pointer in IOPOIN and OUNAM
            k1 = k1 + nrvar

        end do

        if (timon) call timstop (ithandl)
    end subroutine write_output
end module m_write_output
