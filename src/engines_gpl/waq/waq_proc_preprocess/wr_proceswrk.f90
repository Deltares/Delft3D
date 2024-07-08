module m_wr_proceswrk
    use m_waq_precision

    implicit none

contains

    !----- GPL ---------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2011-2024.
    !
    !  This program is free software: you can redistribute it and/or modify
    !  it under the terms of the GNU General Public License as published by
    !  the Free Software Foundation version 3.
    !
    !  This program is distributed in the hope that it will be useful,
    !  but WITHOUT ANY WARRANTY; without even the implied warranty of
    !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !  GNU General Public License for more details.
    !
    !  You should have received a copy of the GNU General Public License
    !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
    !
    !  contact: delft3d.support@deltares.nl
    !  Stichting Deltares
    !  P.O. Box 177
    !  2600 MH Delft, The Netherlands
    !
    !  All indications and logos of, and references to, "Delft3D" and "Deltares"
    !  are registered trademarks of Stichting Deltares, and remain the property of
    !  Stichting Deltares. All rights reserved.
    !
    !-------------------------------------------------------------------------------
    !
    !

    subroutine wr_proceswrk(lurep, procesdef, num_defaults, defaul, idpnw, &
            ivpnw, dsto, vsto, locnam, nopred, &
            num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, num_substances_total, &
            num_local_vars, num_dispersion_arrays, num_velocity_arrays, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
            num_local_vars_exchange, num_substances_transported, num_grids, dename, coname, paname, &
            funame, sfname, syname, intopt, file_unit_list, &
            file_name_list, num_output_files, ioutps, outputs, ndmpar, &
            output_buffer_len, versio, num_dispersion_arrays_new, num_velocity_arrays_new, num_input_ref, &
            proref, num_processes_activated, num_fluxes, num_vars, process_space_int_len)

        !     Deltares Software Centre

        !>/File
        !>      write proces work file

        !     Created   : Aug   2012 by Jan van Beek

        use m_wrstoc
        use m_wripro
        use m_setvat
        use m_proc_totals
        use m_intoou
        use m_open_waq_files
        use timers         !< performance timers
        use processet      !< use processet definitions
        use results, only : OutputPointers         !< use results definitions
        implicit none

        ! arguments

        integer(kind = int_wp), intent(in) :: lurep                  !< unit number report file
        type(procespropcoll), intent(in) :: procesdef              !< the proces definition
        integer(kind = int_wp), intent(in) :: num_defaults                  !< number of default values
        real(kind = real_wp), intent(in) :: defaul(*)              !< array with default
        integer(kind = int_wp), intent(in) :: idpnw(*)               !< new dispersion pointers
        integer(kind = int_wp), intent(in) :: ivpnw(*)               !< new velocity pointers
        real(kind = real_wp), intent(in) :: dsto(*)                !< dispersion stochi factors
        real(kind = real_wp), intent(in) :: vsto(*)                !< velocity stochi factors
        character(len = 20), intent(in) :: locnam(*)              !< name of the local variables
        integer(kind = int_wp), intent(in) :: nopred                 !< number of predfined values
        integer(kind = int_wp), intent(in) :: num_constants                 !< number of constants
        integer(kind = int_wp), intent(in) :: num_spatial_parameters                   !< number of parameters
        integer(kind = int_wp), intent(in) :: num_time_functions                  !< number of functions
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions                 !< number of segment functions
        integer(kind = int_wp), intent(in) :: num_substances_total                  !< number of substances
        integer(kind = int_wp), intent(in) :: num_local_vars                  !< number of local values
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays                 !< number of dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays                 !< number of velocities
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays_extra                  !< number of dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_extra                  !< number of velocities
        integer(kind = int_wp), intent(in) :: num_local_vars_exchange
        integer(kind = int_wp), intent(in) :: num_substances_transported                  !< number of active substances
        integer(kind = int_wp), intent(in) :: num_grids                 !< number of grids
        character(len = 20), intent(in) :: dename(*)              !< default names
        character(len = 20), intent(in) :: coname(*)              !< constant names
        character(len = 20), intent(in) :: paname(*)              !< parameter names
        character(len = 20), intent(in) :: funame(*)              !< function names
        character(len = 20), intent(in) :: sfname(*)              !< segm.func. names
        character(len = 20), intent(in) :: syname(*)              !< substance names
        integer(kind = int_wp), intent(in) :: intopt                 !< integration sub options
        integer(kind = int_wp), intent(inout) :: file_unit_list(*)                 !< unit numbers
        character(len = *), intent(in) :: file_name_list(*)               !< filenames
        integer(kind = int_wp), intent(in) :: num_output_files                  !< total number of output files
        integer(kind = int_wp), intent(in) :: ioutps(7, *)            !< (old) output structure
        type(OutputPointers), intent(in) :: outputs                !< output structure
        integer(kind = int_wp), intent(in) :: ndmpar                 !< number of dump areas
        integer(kind = int_wp), intent(in) :: output_buffer_len
        real(kind = real_wp), intent(in) :: versio                 !< version number proces definition file
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays_new                  !< number of new dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_new
        integer(kind = int_wp), intent(in) :: num_input_ref                  !< maximum nr of references to be resolved
        integer(kind = int_wp), intent(in) :: proref(num_input_ref, *)        !< input items to be resolved for each process
        integer(kind = int_wp), intent(out) :: num_processes_activated                  !< number of processes
        integer(kind = int_wp), intent(out) :: num_fluxes                  !< number of fluxes
        integer(kind = int_wp), intent(out) :: num_vars                  !< number of variables
        integer(kind = int_wp), intent(out) :: process_space_int_len                 !< actual length of process_space_real array

        ! local

        integer(kind = int_wp) :: mxpmsa                 !  maximum length of process_space_real array
        integer(kind = int_wp) :: nbpr                   !  number of active processes
        integer(kind = int_wp) :: ioffx                  !  offset to the exchange items
        integer(kind = int_wp) :: no_ins          ! number of output items
        integer(kind = int_wp) :: no_ine          ! number of output items
        integer(kind = int_wp) :: no_ous          ! number of output items
        integer(kind = int_wp) :: no_oue          ! number of output items
        integer(kind = int_wp) :: no_flu          ! number of output items
        integer(kind = int_wp) :: no_sto          ! number of output items
        integer(kind = int_wp) :: no_dis          ! number of output items
        integer(kind = int_wp) :: no_vel          ! number of output items
        integer(kind = int_wp), pointer :: nsvar(:)
        integer(kind = int_wp), pointer :: iflux(:)
        integer(kind = int_wp), pointer :: process_space_int(:)
        integer(kind = int_wp), pointer :: ipssa(:)
        integer(kind = int_wp), pointer :: prvvar(:)
        integer(kind = int_wp), pointer :: prvtyp(:)
        integer(kind = int_wp), pointer :: progrd(:)
        integer(kind = int_wp), pointer :: prondt(:)
        real(kind = real_wp), pointer :: stochi(:)
        character(len=10), allocatable :: pronam(:)
        character(len=20), allocatable :: varnam(:)       ! variable names
        integer(kind = int_wp), allocatable :: vararr(:)       ! variable array
        integer(kind = int_wp), allocatable :: varidx(:)       ! variable index
        integer(kind = int_wp), allocatable :: vartda(:)       ! variable type of dis-aggregation
        integer(kind = int_wp), allocatable :: vardag(:)       ! variable dis-aggregation variable
        integer(kind = int_wp), allocatable :: vartag(:)       ! variable type of aggregation
        integer(kind = int_wp), allocatable :: varagg(:)       ! variable aggregation variable
        integer(kind = int_wp) :: iproc                  !  loop counter processes
        integer(kind = int_wp) :: ierr2                  !  local error indication
        integer(kind = int_wp) :: ithndl = 0        ! handle for performance timer
        if (timon) call timstrt("wr_proceswrk", ithndl)

        ! count active processes (merge nbpr and num_processes_activated?)

        num_processes_activated = 0
        num_fluxes = 0

        nbpr = 0
        do iproc = 1, procesdef%current_size
            if (procesdef%procesprops(iproc)%active) then
                nbpr = nbpr + 1
            endif
        enddo

        ! calculate new totals

        call proc_totals(lurep, procesdef, no_ins, no_ine, no_ous, &
                no_oue, no_flu, no_sto, no_dis, no_vel)

        ! calculate and fill output structure

        process_space_int_len = 0
        ioffx = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + num_substances_total + num_local_vars + num_defaults
        mxpmsa = no_ine + no_ins + no_ous + no_oue + no_flu
        allocate (nsvar(nbpr))
        allocate (iflux(nbpr))
        allocate (process_space_int(mxpmsa))
        allocate (ipssa(mxpmsa))
        allocate (prvvar(mxpmsa))
        allocate (prvtyp(mxpmsa))
        allocate (progrd(nbpr))
        allocate (prondt(nbpr))
        allocate (pronam(nbpr))
        call intoou (procesdef, num_processes_activated, num_fluxes, nsvar, pronam, &
                iflux, process_space_int, ipssa, process_space_int_len, ioffx, &
                num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, num_substances_total, &
                num_dispersion_arrays, num_velocity_arrays, num_defaults, num_local_vars, num_dispersion_arrays_extra, &
                num_velocity_arrays_extra, num_local_vars_exchange, nopred, prvvar, prvtyp, &
                num_vars, progrd, prondt)

        deallocate(process_space_int, ipssa)

        ! set variables attribute's for aggregation dis-aggregation

        allocate(varnam(num_vars))
        allocate(vararr(num_vars))
        allocate(varidx(num_vars))
        allocate(vartda(num_vars))
        allocate(vardag(num_vars))
        allocate(vartag(num_vars))
        allocate(varagg(num_vars))
        call setvat (lurep, num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                nopred, num_vars, vararr, varidx, vartda, &
                vardag, vartag, varagg, num_grids, coname, &
                paname, funame, sfname, dename, syname, &
                locnam, varnam)
        deallocate(varnam)

        ! write stochi file, set stochi array, balance output settings

        if (btest(intopt, 3) .and. .not. btest(intopt, 4)) then
            call open_waq_files (file_unit_list(36), file_name_list(36), 36, 1, ierr2)
        endif
        allocate(stochi(num_substances_total * no_flu))
        call wrstoc (procesdef, file_unit_list(36), num_substances_total, syname, stochi, &
                num_output_files, ioutps, outputs, ndmpar, output_buffer_len, &
                intopt)
        if (btest(intopt, 3) .and. .not. btest(intopt, 4)) then
            close (file_unit_list(36))
        endif

        ! write process intermediate file

        call open_waq_files (file_unit_list(24), file_name_list(24), 24, 1, ierr2)
        call wripro (num_processes_activated, nsvar, iflux, process_space_int_len, prvvar, &
                prvtyp, num_local_vars, num_defaults, defaul, pronam, &
                num_fluxes, file_unit_list(24), versio, stochi, num_substances_total, &
                num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, dsto, &
                vsto, num_dispersion_arrays_new, idpnw, num_velocity_arrays_new, ivpnw, &
                progrd, prondt, num_vars, vararr, varidx, &
                vartda, vardag, vartag, varagg, num_input_ref, &
                proref)
        close (file_unit_list(24))
        deallocate(stochi, nsvar, iflux)
        deallocate(prvvar, prvtyp, progrd, prondt)
        deallocate(pronam)
        deallocate(vararr, varidx, vartda, vardag, vartag, varagg)

        if (timon) call timstop(ithndl)
        return
    end

end module m_wr_proceswrk
