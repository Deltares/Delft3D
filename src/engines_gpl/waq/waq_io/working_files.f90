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
module m_working_files
    use m_waq_precision

    implicit none

    private
    public :: create_work_file_one, read_working_file_4

contains


    subroutine create_work_file_one(file_unit_list, file_name_list, num_file_units, runid)
        !! Reads the input filename* ( keyboard /command line ) ;
        !! sets filenames* ; opens system files
        !! the subroutine creates the following files lst, delwaq04.wrk, harmonic.wrk, pointers.wrk, filenaam.wrk files
        !! Logical units     : 5       = keyboard
        !!                     file_unit_list(26) = unit user input file
        !!                     file_unit_list(27) = unit stripped input file
        !!                     file_unit_list(29) = unit formatted output file
        !!                     file_unit_list( 2) = unit system-intermediate file
        !!                     file_unit_list( 3) = unit intermediate file (harmonics)
        !!                     file_unit_list( 4) = unit intermediate file (pointers)

        use m_logger_helper, only : set_log_unit_number, stop_with_error
        use m_cli_utils, only : get_command_argument_by_name, get_input_filename
        use waq_file_utils_external, only : get_filepath_and_pathlen
        use m_open_waq_files
        use timers
        use data_processing, only : delete_file

        implicit none

        integer(kind = int_wp), intent(in) :: num_file_units                     !< Amount of unit numbers
        integer(kind = int_wp), intent(inout) :: file_unit_list(num_file_units)  !< Unit numbers
        character(*), intent(inout) :: file_name_list(num_file_units)            !< File names
        character(:), allocatable, intent(inout) :: runid                        !< Runid

        ! Local

        integer(kind = int_wp) :: ilun
        integer(kind = int_wp) :: ioerr
        character(len=93) :: check
        logical :: specout
        integer(kind = int_wp) :: idummy
        real (kind = real_wp) :: rdummy
        character(:), allocatable :: outputpath
        character(len=256):: outputpath2
        character(len=256) :: runidpath
        integer(kind = int_wp) :: pathlen
        integer(kind = int_wp) :: outpathlen
        character(len=256) :: outid
        logical :: parsing_error
        integer(kind = int_wp) :: ierr2

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("create_work_file_one", ithndl)

        ! Get filename  ( keyboard / command line )

        check = file_name_list(29)
        call get_input_filename(runid, check)

        ! Specific output dir?
        specout = get_command_argument_by_name('-output', outputpath, parsing_error)
        if (specout) then
            if (parsing_error) then
                write (*, '(A/)') 'Found -output switch but not path specified. This will be ignored.'
                specout = .false.
            else
                write (*, '(A)') 'Found -output switch with the following path:'
                write (*, '(/A)') trim(outputpath)
                write (*, '(/A/)') 'Make sure this path exists, or DELWAQ will not run!'
                call get_filepath_and_pathlen (runid, runidpath, pathlen)
                call get_filepath_and_pathlen (outputpath, outputpath2, outpathlen)
                if (outpathlen == 0 .or. outputpath(1:1) == '.') then
                    ! No dir indicators found or it starts with a dot, asume it is a local subdir
                    outputpath = trim(runidpath) // trim(outputpath)
                endif
                if (pathlen == 0) then
                    outid = trim(outputpath) // '/' // runid
                else
                    outid = trim(outputpath) // '/' // trim(runid(pathlen + 1:))
                endif
            endif
        endif

        !  Pad the model name in the file names
        do ilun = 1, num_file_units
            if (specout .and. index(file_name_list(ilun), '.wrk') == 0 .and. index(file_name_list(ilun), '.inp') == 0) then
                file_name_list(ilun) = trim(outid) // file_name_list(ilun)
            else
                file_name_list(ilun) = trim(runid) // file_name_list(ilun)
            endif
        enddo

        ! Remove any existing work files

        do ilun = 1, num_file_units
            if (index(file_name_list(ilun), '.wrk') > 0) call delete_file(file_name_list(ilun), ioerr)
        enddo

        ! Open the neccessary unit numbers
        ! create the lst file
        call open_waq_files(file_unit_list(29), file_name_list(29), 29, 1, ioerr)
        !
        call set_log_unit_number(file_unit_list(29))
        ! open the input file (.inp)
        call open_waq_files(file_unit_list(26), file_name_list(26), 26, 1, ioerr)
        if (ioerr > 0) then
            write (file_unit_list(29), 1000) file_unit_list(26), file_name_list(26)
            call stop_with_error()
        endif
        ! create the delwaq04.wrk binary file
        call open_waq_files(file_unit_list(2), file_name_list(2), 2, 1, ioerr)
        ! create the harmonic.wrk file
        call open_waq_files(file_unit_list(3), file_name_list(3), 3, 1, ioerr)
        ! create the pointers.wrk file
        call open_waq_files(file_unit_list(4), file_name_list(4), 4, 1, ioerr)
        ! create the filenaam.wrk file
        call open_waq_files(file_unit_list(41), file_name_list(41), 41, 1, ioerr)

        if (timon) call timstop(ithndl)
        return

        ! Output formats

        1000 format (/' ERROR input file on unit:', I3, &
                /' Filename = ', A, &
                /' Does not exist.', &
                /' EXECUTION HALTED !!!!!!!!!!!!!')
    end subroutine create_work_file_one

    subroutine read_working_file_4 (iin, lurep, modid, sysid, num_substances_total, &
            num_monitoring_points, num_substances_transported, num_boundary_conditions, num_waste_loads, num_constants, &
            num_spatial_parameters, num_cells, num_cells_bottom, coname, paname, &
            funame, num_time_functions, sfname, num_spatial_time_fuctions, num_dispersion_arrays, &
            num_velocity_arrays, diname, vename, idpnt, ivpnt, &
            ndmpar, ntdmpq, ntdmps, noqtt, num_transects, &
            num_transect_exchanges, num_boundary_types, num_waste_load_types, num_grids, grdref, &
            sysgrd, sysndt)
        !!  Reads part of the DelwaQ system file
        !!
        !!                          This routine re-reads the DelwaQ system file.\n
        !!                          It is called by dlwqp1, to get the names of the
        !!                          substances, constants, parameters, functions and
        !!                          segment functions.\n
        !!                          This information is used by dlwqp1 to set up the
        !!                          administratration around processes and to sort the
        !!                          processes in appropriate order.
        !!     LOGICAL UNITNUMBERS : IIN     - system intermediate file
        !!                           LUREP   - monitoring output file
        !!     SUBROUTINES CALLED  : stop_with_error, stops execution

        use m_grid_utils_external
        use timers       !   performance timers
        use m_logger_helper, only : stop_with_error

        integer(kind = int_wp), intent(in) :: iin                !< system intermediate file
        integer(kind = int_wp), intent(in) :: lurep              !< unit number report file
        integer(kind = int_wp), intent(in) :: num_substances_total              !< Number of systems
        integer(kind = int_wp), intent(in) :: num_grids             !< Number of grids
        integer(kind = int_wp), intent(in) :: num_monitoring_points             !< Number of dump segments
        integer(kind = int_wp), intent(in) :: num_substances_transported              !< Number of active systems
        integer(kind = int_wp), intent(in) :: num_boundary_conditions              !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: num_waste_loads              !< Number of load locations
        integer(kind = int_wp), intent(in) :: num_constants             !< Number of constants used
        integer(kind = int_wp), intent(in) :: num_spatial_parameters               !< Number of parameters
        integer(kind = int_wp), intent(in) :: num_cells              !< Number of segments
        integer(kind = int_wp), intent(in) :: num_cells_bottom              !< Number of layered bed segments
        integer(kind = int_wp), intent(in) :: num_time_functions              !< Number of functions ( user )
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions             !< Number of segment functions
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays             !< Number of dispersion array's
        integer(kind = int_wp), intent(in) :: num_velocity_arrays             !< Number of velocity array's
        integer(kind = int_wp), intent(in) :: ndmpar             !< number of dump areas
        integer(kind = int_wp), intent(in) :: ntdmpq             !< total number exchanges in dump area
        integer(kind = int_wp), intent(in) :: ntdmps             !< total number segments in dump area
        integer(kind = int_wp), intent(in) :: noqtt              !< total number of exchanges inclusive num_exchanges_bottom_dir
        integer(kind = int_wp), intent(in) :: num_transects
        integer(kind = int_wp), intent(in) :: num_transect_exchanges
        integer(kind = int_wp), intent(in) :: num_boundary_types             !< Number of boundarie types
        integer(kind = int_wp), intent(in) :: num_waste_load_types             !< Number of waste load types
        integer(kind = int_wp), intent(out) :: idpnt (num_substances_transported)     !< Pointers to dispersion array
        integer(kind = int_wp), intent(out) :: ivpnt (num_substances_transported)     !< Pointers to velocity array
        character(40), intent(out) :: modid (4)         !< Model and run-ID
        character(20), intent(out) :: sysid (num_substances_total)    !< Systems ID
        character(20), intent(out) :: coname(num_constants)    !< Constant names
        character(20), intent(out) :: paname(num_spatial_parameters)      !< Parameter names
        character(20), intent(out) :: funame(num_time_functions)    !< Function names
        character(20), intent(out) :: sfname(num_spatial_time_fuctions)    !< Segment function names
        character(20), intent(out) :: diname(num_dispersion_arrays)    !< Dispersion array names
        character(20), intent(out) :: vename(num_velocity_arrays)    !< Velocity array names
        integer(kind = int_wp), intent(out) :: grdref(num_grids)     !< Reference grid number
        integer(kind = int_wp), intent(out) :: sysgrd(num_substances_total)     !< Grid number substance
        integer(kind = int_wp), intent(out) :: sysndt(num_substances_total)     !< Step size substance

        integer(kind = int_wp) :: idummy         !  dummy integer
        real(kind = real_wp) :: rdummy         !  dummy real
        character(20) c20dum        !  dummy 20 byte character
        character(40) c40dum        !  dummy 40 byte character
        integer(kind = int_wp) :: nosss          !  total number of computational volumes
        integer(kind = int_wp) :: i, k           !  loop variables
        integer(kind = int_wp) :: igrid          !  loop variable
        integer(kind = int_wp) :: iseg           !  loop variable
        integer(kind = int_wp) :: isys           !  loop variable
        integer(kind = int_wp) :: ierror         !  error return variable
        type(t_grid) :: aGrid  !  a single grid
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_working_file_4", ithndl)

        ! read from the system file
        nosss = num_cells + num_cells_bottom

        ! => group 1

        read (iin, end = 20, err = 20)   modid(1), modid(2), modid(3), modid(4)
        read (iin, end = 20, err = 20) (sysid(k), k = 1, num_substances_total)
        if (num_monitoring_points > 0) read (iin, end = 20, err = 20) (idummy, c20dum, k = 1, num_monitoring_points)

        ! => group 2
        if (ndmpar > 0) read (iin, end = 20, err = 20) (c20dum, k = 1, ndmpar)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, k = 1, ndmpar)
        if (num_transects > 0) read (iin, end = 20, err = 20) (c20dum, k = 1, num_transects)

        ! => group 3
        ! sub-grid
        do igrid = 1, num_grids
            read (iin, end = 20, err = 20)  idummy, grdref(igrid), (idummy, iseg = 1, nosss)
        enddo

        ! dummy, the grid structures immediately deallocate the pointers
        do igrid = 1, num_grids
            ierror = aGrid%read(iin, nosss)
            if (ierror /= 0) goto 20
            deallocate(aGrid%finalpointer)
            if (aGrid%space_var_nolay) deallocate(aGrid%nolay_var)
        enddo
        read (iin, end = 20, err = 20) (sysgrd(isys), isys = 1, num_substances_total)
        read (iin, end = 20, err = 20) (sysndt(isys), isys = 1, num_substances_total)

        ! => group attributes
        read (iin, end = 20, err = 20) (idummy, k = 1, nosss)
        if (num_dispersion_arrays > 0) read (iin, end = 20, err = 20) (diname(k), k = 1, num_dispersion_arrays)
        if (num_velocity_arrays > 0) read (iin, end = 20, err = 20) (vename(k), k = 1, num_velocity_arrays)
        read (iin, end = 20, err = 20) (idpnt(k), k = 1, num_substances_transported)
        read (iin, end = 20, err = 20) (ivpnt(k), k = 1, num_substances_transported)
        if (num_boundary_conditions  > 0) read (iin, end = 20, err = 20) (idummy, k = 1, num_boundary_conditions)
        if (num_boundary_conditions  > 0) read (iin, end = 20, err = 20) (idummy, k = 1, num_boundary_conditions)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, i = 1, ndmpar), (idummy, i = 1, ntdmpq)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, i = 1, ndmpar), (idummy, i = 1, ntdmps)
        if (num_transects > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, num_transects)
            read (iin, end = 20, err = 20)  (idummy, i = 1, num_transects)
            read (iin, end = 20, err = 20)  (idummy, i = 1, num_transect_exchanges)
        endif
        if (num_transects > 0 .or. ndmpar > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, noqtt)
        endif
        if (ndmpar > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, nosss)
        endif
        read (iin, end = 20, err = 20) idummy, (rdummy, k = 1, 3)
        read (iin, end = 20, err = 20) idummy, (rdummy, k = 1, 3)

        if (num_boundary_conditions  > 0) then
            ! id's and names  (=new! (ver 4.900))
            do i = 1, num_boundary_conditions
                read (iin, end = 20, err = 20) c20dum, c40dum
            enddo
            ! type-strings  (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (c20dum, k = 1, num_boundary_types)
            ! type-integers per boundary (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (idummy, k = 1, num_boundary_conditions)
            ! read time lags
            read (iin, end = 20, err = 20) (idummy, k = 1, num_boundary_conditions)
        endif

        if (num_waste_loads  > 0) then
            ! segnums, id's and names  (=new! (ver 4.900))
            do i = 1, num_waste_loads
                read (iin, end = 20, err = 20) idummy, idummy, c20dum, c40dum
            enddo
            ! type-strings  (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (c20dum, k = 1, num_waste_load_types)
            ! type-integers per wasteload (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (idummy, k = 1, num_waste_loads)
        endif

        if (num_constants > 0) read (iin, end = 20, err = 20) (coname(k), k = 1, num_constants)
        if (num_spatial_parameters   > 0) read (iin, end = 20, err = 20) (paname(k), k = 1, num_spatial_parameters)
        if (num_time_functions  > 0) read (iin, end = 20, err = 20) (funame(k), k = 1, num_time_functions)
        if (num_spatial_time_fuctions > 0) read (iin, end = 20, err = 20) (sfname(k), k = 1, num_spatial_time_fuctions)

        ! completion successful
        if (timon) call timstop(ithndl)
        return

        ! unsuccessful read
        20 write (lurep, 2010)
        call stop_with_error()

        ! output formats
        2010 format ('1  ERROR reading binary system file !!'/ &
                '   initialisation NOT successful    !!'/ &
                '   simulation impossible            !!')

    end subroutine read_working_file_4

end module m_working_files
