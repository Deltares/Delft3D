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
module m_dlwqta
    use m_waq_precision

    implicit none

contains


    subroutine dlwqta(file_unit_list, lch, lunrep, noseg, nocons, &
            nopa, nofun, nosfun, const, param, &
            funcs, sfuncs, isflag, ifflag, itime, &
            GridPs, dlwqd, ierr)

        !! Makes values at time = itime for the process parameters

        use m_dlwqt4
        use m_logger, only : terminate_execution
        use m_open_waq_files
        use waq_file_utils_external, only : create_new_file_unit_number
        use timers
        use delwaq2_data
        use m_grid_utils_external
        use m_waq_data_structure

        integer(kind = int_wp), intent(inout) :: file_unit_list                  !< unit number binary input file
        character(len = *), intent(in) :: lch                  !< name input file
        integer(kind = int_wp), intent(in) :: lunrep               !< unit number report file
        integer(kind = int_wp), intent(in) :: noseg                !< number of segments
        integer(kind = int_wp), intent(in) :: nocons               !< number of constants
        integer(kind = int_wp), intent(in) :: nopa                 !< number of parameters
        integer(kind = int_wp), intent(in) :: nofun                !< number of functions
        integer(kind = int_wp), intent(in) :: nosfun               !< number of segment functions
        real(kind = real_wp), intent(out) :: const(nocons)        !< constants array
        real(kind = real_wp), intent(out) :: param(nopa, noseg)    !< parameters array
        real(kind = real_wp), intent(out) :: funcs(nofun)         !< functions array
        real(kind = real_wp), intent(out) :: sfuncs(noseg, nosfun) !< segment functions array
        integer(kind = int_wp), intent(in) :: isflag               !< = 1 then 'ddhhmmss' format
        integer(kind = int_wp), intent(in) :: ifflag               !< = 1 then first invocation
        integer(kind = int_wp), intent(in) :: itime                !< system timer
        type(GridPointerColl), intent(in) :: GridPs               !< collection off all grid definitions
        type(delwaq_data), target, intent(inout) :: dlwqd                !< derived type for persistent storage
        integer(kind = int_wp), intent(inout) :: ierr                 !< error count

        !     Local declarations
        integer(kind = int_wp) :: no_proc_pars         ! number of process parameters data blocks
        type(t_data_block), pointer :: proc_par             ! a pointer to one data block
        integer(kind = int_wp) :: ntotal               ! number of process parameters
        character(len = 12) :: chlp
        integer(kind = int_wp) :: ftype                ! the equivalent of the ftype array elsewhere
        integer(kind = int_wp) :: ierr2                ! io error indicator
        integer(kind = int_wp) :: i, i2
        integer(kind = int_wp) :: ntt, ndim1, ndim2, nobrk
        integer(kind = int_wp), allocatable :: ipntloc(:)
        integer(kind = int_wp) :: ia_dummy(1)
        logical :: ldummy, ldumm2
        real(kind = real_wp), allocatable :: ra_dummy(:)
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwqta", ithandl)

        ! read the data from file

        ntotal = nocons + nopa + nofun + nosfun
        if (ntotal > 0) then
            if (ifflag == 1) then
                call open_waq_files (file_unit_list, lch, 16, 2, ierr2)
                if (ierr2 /= 0) then
                    write(lunrep, *) 'error in dlwqta, opening file'
                    write(lunrep, *) 'file    :', lch
                    write(lunrep, *) 'unit    :', file_unit_list
                    call terminate_execution(1)
                endif
                read (file_unit_list, iostat = ierr2) chlp
                if (ierr2/=0 .or. chlp(1:6) /= ' 5.000') then
                    write(lunrep, *) 'error in dlwqta, file not new'
                    call terminate_execution(1)
                endif

                read(file_unit_list, iostat = ierr2) no_proc_pars
                if (ierr2 /= 0) then
                    write(lunrep, 1000) trim(lch)
                    call terminate_execution(1)
                endif
                allocate(dlwqd%proc_pars%data_block(no_proc_pars))
                dlwqd%proc_pars%maxsize = no_proc_pars
                dlwqd%proc_pars%current_size = no_proc_pars

                do i = 1, no_proc_pars
                    ierr2 = dlwqd%proc_pars%data_block(i)%read(lunrep, file_unit_list)
                    if (ierr2 /= 0) then
                        write(lunrep, 1000) trim(lch)
                        call terminate_execution(1)
                    endif
                    proc_par => dlwqd%proc_pars%data_block(i)
                    if (proc_par%is_external .and. (mod(proc_par%filetype, 10) == FILE_BINARY .or. &
                            mod(proc_par%filetype, 10) == FILE_UNFORMATTED)) then
                        if (proc_par%iorder == ORDER_PARAM_LOC) then
                            ndim1 = proc_par%num_parameters
                            ndim2 = proc_par%num_locations
                        else
                            ndim1 = proc_par%num_locations
                            ndim2 = proc_par%num_parameters
                        endif
                        nobrk = 1
                        proc_par%num_breakpoints = nobrk
                        allocate(proc_par%values(ndim1, ndim2, nobrk))

                        ! Some obscure magic going on with LU-numbers - keep it for the moment
                        call create_new_file_unit_number(801, proc_par%lun)
                        ftype = 2
                        if (mod(proc_par%filetype, 10) == FILE_UNFORMATTED) ftype = ftype + 10
                        if (proc_par%filetype / 10 == 1) ftype = ftype + 20       ! I am in for a better solution (lp)
                        call open_waq_files (proc_par%lun, proc_par%filename, 40, ftype, ierr2)
                    endif
                enddo

                close (file_unit_list)

            endif

            ! evaluate data
            do i = 1, dlwqd%proc_pars%current_size

                ! update external data to values
                proc_par => dlwqd%proc_pars%data_block(i)
                if (proc_par%is_external .and. (mod(proc_par%filetype, 10) == FILE_BINARY .or. &
                        mod(proc_par%filetype, 10) == FILE_UNFORMATTED)) then
                    if (proc_par%iorder == ORDER_PARAM_LOC) then
                        ndim1 = proc_par%num_parameters
                        ndim2 = proc_par%num_locations
                    else
                        ndim1 = proc_par%num_locations
                        ndim2 = proc_par%num_parameters
                    endif
                    ntt = ndim1 * ndim2
                    allocate(ipntloc(ntt))
                    do i2 = 1, ntt; ipntloc(i2) = -i2;
                    enddo
                    call dlwqt4(ia_dummy, proc_par%filename, ia_dummy, lunrep, proc_par%lun, &
                            itime, proc_par%values, ipntloc, ndim1, ntt, &
                            isflag, ifflag, ldummy, 0, .false., &
                            ldumm2, ra_dummy, dlwqd)
                    deallocate(ipntloc)
                endif

                if (proc_par%subject == SUBJECT_CONSTANT .and. ifflag == 1) then
                    ierr2 = proc_par%evaluate(GridPs, itime, nocons, 1, const)
                    if (ierr2 /= 0) then
                        write(lunrep, 1010)
                        call terminate_execution(1)
                    endif
                elseif (proc_par%subject == SUBJECT_FUNCTION) then
                    ierr2 = proc_par%evaluate(GridPs, itime, nofun, 1, funcs)
                    if (ierr2 /= 0) then
                        write(lunrep, 1010)
                        call terminate_execution(1)
                    endif
                elseif (proc_par%subject == SUBJECT_PARAMETER .and. ifflag == 1) then
                    ierr2 = proc_par%evaluate(GridPs, itime, nopa, noseg, param)
                    if (ierr2 /= 0) then
                        write(lunrep, 1010)
                        call terminate_execution(1)
                    endif
                elseif (proc_par%subject == SUBJECT_SEGFUNC) then
                    ierr2 = proc_par%evaluate(GridPs, itime, noseg, nosfun, sfuncs)
                    if (ierr2 /= 0) then
                        write(lunrep, 1010)
                        call terminate_execution(1)
                    endif
                endif
            enddo

        endif

        if (timon) call timstop (ithandl)
        return
        1000 format (/' ERROR: reading process parameters system file:', A)
        1010 format (/' ERROR: evaluating process parameters')
    end subroutine dlwqta

end module m_dlwqta
