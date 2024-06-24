!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

program agrhyd

    use m_logger_helper, only: stop_with_error, set_log_unit_number
    use m_hydmod
    use m_banner_helper
    use m_banner_information
    use waq_static_version_info
    use m_cli_utils, only: is_command_arg_specified
    use data_processing, only: extract_value_from_group, extract_logical, extract_integer, extract_real
    use time_module
    use io_ugrid
    use system_utils, only: makedir
    use m_date_time_utils_external, only: write_date_time
    use agrhyd_version_module, only: getfullversionstring_agrhyd

    implicit none

    type(t_hydrodynamics) :: input_hyd     ! description of the input hydrodynamics
    type(t_hydrodynamics) :: input_patch_hyd(0:9) ! description of the input hydrodynamics patches
    type(t_hydrodynamics) :: output_hyd    ! description of the output hydrodynamics
    integer, allocatable :: ipnt_h(:, :)   ! horizontal aggregation pointer
    integer, allocatable :: ipnt_v(:)     ! vertical aggregation pointer
    integer, allocatable :: ipnt(:)       ! aggregation pointer segments
    integer, allocatable :: ipnt_q(:)     ! aggregation pointer exchanges
    integer, allocatable :: ipnt_vdf(:)   ! aggregation pointer used for minimum vertical diffusion
    integer, allocatable :: ipnt_tau(:)   ! aggregation pointer used for tau
    integer, allocatable :: ipnt_b(:)     ! aggregation pointer boundaries
    integer :: itime         ! relative time in file
    real :: rday          ! time in days
    integer :: nosegb        ! number of old boundaries total
    integer :: iend          ! end of file indicator
    character(len=256) :: input_file    ! base name of the input files
    logical :: exist_ini
    logical :: exist_src
    character(len=256) :: name          ! base name of the output files
    integer :: output_start  ! output start time
    integer :: output_stop   ! output stop time
    integer :: output_shift  ! output shift time
    character(14) :: output_t0     ! output reference time
    real(8) :: output_t0_jul ! output reference time (julian)
    integer :: output_t0_d   ! output reference date (ymd)
    integer :: output_t0_t   ! output reference time (hms)
    real(8) :: input_t0_jul  ! input reference time (julian)
    integer :: input_t0_d    ! input reference date (ymd)
    integer :: input_t0_t    ! input reference time (hms)
    real :: wdayshift     ! time in days (after shift)
    logical :: l_regular     ! regular aggregartion option
    logical :: l_expand      ! expand to full matrix
    logical :: l_lenlen      ! take length from length
    integer :: m_fact        ! aggregation factor m direction
    integer :: n_fact        ! aggregation factor n direction
    integer :: m_offset      ! offset aggregation m direction
    integer :: n_offset      ! offset aggregation n direction
    logical :: l_patch(0:10) ! found patch file(s)
    integer :: ipatch        ! patch counter
    integer :: cpatch        ! current active patch (-1 = still in original file)
    integer :: npatch        ! next active patch (10 = no more patches active)
    integer :: itime_first_patch(0:10) ! start time of each patch

    logical :: lfound        ! command line option found
    integer :: ipos
    integer :: ierr
    integer :: ierr2
    integer :: luninp        ! unit number input file
    integer :: lunrep        ! unit number report file
    integer :: ilay          ! layer index
    character(len=20) :: rundat        ! date and time string
    integer :: ierr_alloc    !
    type(t_file) :: new_lga               ! aggregation-file
    type(t_file) :: new_cco               ! aggregation-file
    type(t_file) :: new_grd               ! new grd file
    logical :: singapore_rename_discharges ! special option rename discharges for singapore
    character(len=256) :: singapore_discharge_names   ! special option filename for singapore
    character(len=99) :: version       ! version string
    type(banner_information) :: info

    ! sing_z variables
    integer :: iseg1, iseg2, iseg, ik1, ik2, isegb, lenname

    ! Show startup screen
    info = banner_information(name="AgrHyd", &
                              description="Aggregation of hydrodynamic data sets for D-Water Quality", &
                              suite_name="Delft3D / D-HYDRO", &
                              version_string=trim(major_minor_buildnr), &
                              built_on=trim(build_date_time), &
                              copyright=copyright)

    write (*, "(A)") generate_banner(info)

    call getfullversionstring_agrhyd(version)

    ! get input file from commandline
    lunrep = 9

    call get_command_argument(1, input_file)
    if (input_file == ' ') then
        write (*, '(a,$)') ' Enter agrhyd ini-filename: '
        read (*, '(a)') input_file
        write (*, *)
    end if
    if (input_file == ' ') then
        open (lunrep, file='agrhyd.rep', recl=132)
        call write_date_time(rundat)
        write (lunrep, '(a,a)') ' (c) ', trim(version)
        write (lunrep, '(a,a)') ' execution start: ', rundat
        write (lunrep, '(a)') ' error: no command line argument or interactive input with name of ini-filename'
        write (*, '(a)') ' error: no command line argument or interactive input with name of ini-filename'

        call stop_with_error()
    end if

    inquire (file=input_file, exist=exist_ini)
    if (.not. exist_ini) then
        open (lunrep, file='agrhyd.rep', recl=132)
        call write_date_time(rundat)
        write (lunrep, '(a,a)') ' (c) ', trim(version)
        write (lunrep, '(a,a)') ' execution start: ', rundat
        write (lunrep, '(a,a)') ' error: ini-file not found: ', trim(input_file)
        write (*, '(a,a)') ' error: ini-file not found: ', trim(input_file)

        call stop_with_error()
    end if

    luninp = 10
    open (luninp, file=input_file)

    ! first get base name output and open report file

    call extract_value_from_group(luninp, 'General', 'output', name)
    lenname = len_trim(name)
    if (lenname > 4) then
        if (name(lenname - 3:lenname) == '.hyd') then
            name(lenname - 3:lenname) = '    '
        end if
    end if
    ipos = index(name, '/', BACK=.true.)
    ipos = max(ipos, index(name, char(92), BACK=.true.)) ! char(92)==backslash
    if (ipos > 0) then
        call makedir(name(1:ipos - 1))
    end if

    open (lunrep, file=trim(name)//'-agrhyd.rep', recl=132)
    call set_log_unit_number(lunrep)
    call SetMessageHandling(lunMessages=lunrep)
    write (lunrep, '(a,a)') ' (c) ', trim(version)
    call write_date_time(rundat)
    write (lunrep, '(2a)') ' execution start: ', rundat
    write (lunrep, *)
    write (lunrep, *) 'input file name          : ', trim(input_file)
    write (lunrep, *) 'base name of the output  : ', trim(name)

    ! handle rest of the input

    call extract_value_from_group(luninp, 'General', 'inputhyd', input_hyd%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatchhyd', input_patch_hyd(0)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch1hyd', input_patch_hyd(1)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch2hyd', input_patch_hyd(2)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch3hyd', input_patch_hyd(3)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch4hyd', input_patch_hyd(4)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch5hyd', input_patch_hyd(5)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch6hyd', input_patch_hyd(6)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch7hyd', input_patch_hyd(7)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch8hyd', input_patch_hyd(8)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'inputpatch9hyd', input_patch_hyd(9)%file_hyd%name)
    call extract_value_from_group(luninp, 'General', 'horizontal_aggregation_file', output_hyd%file_dwq%name)
    call extract_value_from_group(luninp, 'General', 'vertical_aggregation_file', output_hyd%file_vag%name)
    call extract_logical(luninp, 'General', 'expand', l_expand)
    call extract_logical(luninp, 'General', 'lenlen', l_lenlen)
    call extract_logical(luninp, 'General', 'regular', l_regular)
    call extract_integer(luninp, 'General', 'm_fact', m_fact)
    call extract_integer(luninp, 'General', 'n_fact', n_fact)
    call extract_integer(luninp, 'General', 'm_offset', m_offset)
    call extract_integer(luninp, 'General', 'n_offset', n_offset)
    call extract_real(luninp, 'General', 'minimum-dispersion-length', output_hyd%min_disp_len)
    call extract_integer(luninp, 'General', 'start', output_start)
    call extract_integer(luninp, 'General', 'stop', output_stop)
    call extract_value_from_group(luninp, 'General', 'reference_time_output', output_t0)
    call extract_logical(luninp, 'General', 'ascii-output', output_hyd%l_ascii)
    call extract_value_from_group(luninp, 'General', 'river_flow_file', output_hyd%file_rfl%name)
    call extract_logical(luninp, 'General', 'singapore_rename_discharges', singapore_rename_discharges)
    call extract_value_from_group(luninp, 'General', 'singapore_discharge_names', singapore_discharge_names)
    close (luninp)

    ! read hydrodynamic description file

    write (lunrep, *) 'input hydrodynamics      : ', trim(input_hyd%file_hyd%name)
    write (*, *) 'input hydrodynamics      : ', trim(input_hyd%file_hyd%name)
    call read_hyd(input_hyd)

    ! read hydrodynamic description file patches, and check if they are there

    l_patch = .false.
    do ipatch = 0, 9
        if (input_patch_hyd(ipatch)%file_hyd%name /= ' ') then
            inquire (file=input_patch_hyd(ipatch)%file_hyd%name, exist=l_patch(ipatch))
            if (l_patch(ipatch)) then
                write (lunrep, *) 'input hydrodynamics patch: '//trim(input_patch_hyd(ipatch)%file_hyd%name)
                write (*, *) 'input hydrodynamics patch: '//trim(input_patch_hyd(ipatch)%file_hyd%name)
                call read_hyd(input_patch_hyd(ipatch))
            else
                write (lunrep, *) 'warning: input hydrodynamics patch not found (patch ignored): '// &
                    trim(input_patch_hyd(ipatch)%file_hyd%name)
            end if
        end if
    end do

    ! report and check

    if (input_hyd%geometry /= HYD_GEOM_CURVI .and. &
        input_hyd%geometry /= HYD_GEOM_UNSTRUC) then
        write (lunrep, *) 'error: unknown geometry specification, agrhyd only supports "curvilinear-grid" and "unstructured"'
        write (*, *) 'error: unknown geometry specification, agrhyd only supports "curvilinear-grid" and "unstructured"'
        call stop_with_error()
    end if
    if (output_hyd%file_dwq%name /= ' ') then
        write (lunrep, *) 'horizontal aggregation   : ', trim(output_hyd%file_dwq%name)
    else
        write (lunrep, *) 'horizontal aggregation   : none'
    end if
    if (output_hyd%file_vag%name /= ' ') then
        write (lunrep, *) 'vertical aggregation     : ', trim(output_hyd%file_vag%name)
    else
        write (lunrep, *) 'vertical aggregation     : none'
    end if
    write (lunrep, *) 'minimum dispersion length: ', output_hyd%min_disp_len
    if (l_expand) then
        write (lunrep, *) 'expand option selected'
        if (input_hyd%geometry /= HYD_GEOM_CURVI) then
            write (lunrep, *) 'error: expand option only possible on curvilinear grids'
            write (*, *) 'error: regular option only possible on curvilinear grids'
            call stop_with_error()
        end if
        if (output_hyd%file_dwq%name /= ' ') then
            write (lunrep, *) 'error: expand option not allowed in combination with horizontal aggregation file'
            write (*, *) 'error: expand option not allowed in combination with horizontal aggregation file'
            call stop_with_error()
        end if
    end if
    if (l_regular) then
        write (lunrep, *) 'regular aggregation selected'
        if (input_hyd%geometry /= HYD_GEOM_CURVI) then
            write (lunrep, *) 'error: regular option only possible on curvilinear grids'
            write (*, *) 'error: regular option only possible on curvilinear grids'
            call stop_with_error()
        end if
        if (output_hyd%file_dwq%name /= ' ') then
            write (lunrep, *) 'error: regular option not allowed in combination with horizontal aggregation file'
            write (*, *) 'error: regular option not allowed in combination with horizontal aggregation file'
            call stop_with_error()
        end if
        if (l_expand) then
            write (lunrep, *) 'error: regular option not allowed in combination with expand option'
            write (*, *) 'error: regular option not allowed in combination with expand option'
            call stop_with_error()
        end if
        write (lunrep, *) 'aggregation factor m dir.: ', m_fact
        write (lunrep, *) 'aggregation factor n dir.: ', n_fact
        write (lunrep, *) 'offset m direction       : ', m_offset
        write (lunrep, *) 'offset m direction       : ', n_offset
    end if
    if (output_start /= -999.) then
        write (lunrep, *) 'start of output in sec.  : ', output_start
    else
        write (lunrep, *) 'start of output in sec.  : start of input (default)'
        output_start = -2000000000
    end if
    if (output_stop /= -999.) then
        write (lunrep, *) 'stop of output in sec.   : ', output_stop
    else
        write (lunrep, *) 'stop of output in sec.   : stop of input (default)'
        output_stop = 2000000000
    end if
    if (output_t0 /= ' ') then
        write (lunrep, *) 'Conversion reference time: ', output_t0
    else
        write (lunrep, *) 'Conversion reference time: no change (default)'
    end if
    if (output_hyd%l_ascii) then
        write (lunrep, *) 'ascii output option selected'
    end if
    write (lunrep, *)

    ! special singapore, set discharge names

    if (singapore_rename_discharges) then
        call sing_dis(input_hyd)
    end if

    ! read administration

    write (*, '(a)') 'Reading administration ...'
    call read_hyd_init(input_hyd)

    do ipatch = 0, 9
        if (l_patch(ipatch)) then
            write (*, '(a)') 'Reading patch administration ...'
            call read_hyd_init(input_patch_hyd(ipatch))
            ! some basic checks to see if the patch is for the same domain
            if (input_hyd%noseg /= input_patch_hyd(ipatch)%noseg) then
                write (lunrep, *) 'error: patch hyd file does not contain the same number of segments as the main hyd-file!'
                write (*, *) 'error: patch hyd file does not contain the same number of segments as the main hyd-file!'
                call stop_with_error()
            end if
            if (input_hyd%noq /= input_patch_hyd(ipatch)%noq) then
                write (lunrep, *) 'error: patch hyd file does not contain the same number of exchanges as the main hyd-file!'
                write (*, *) 'error: patch hyd file does not contain the same number of exchanges as the main hyd-file!'
                call stop_with_error()
            end if
        end if
    end do

    ! read sources

    if (input_hyd%wasteload_coll%current_size > 0) then
        if (.not. any(l_patch)) then
            inquire (file=input_hyd%file_src%name, exist=exist_src)
            if (exist_src) then
                write (lunrep, *) 'reading src-file: '//trim(input_hyd%file_src%name)
                write (*, *) 'reading src-file: '//trim(input_hyd%file_src%name)
                call read_src(input_hyd%file_src, input_hyd%nolay, input_hyd%wasteload_coll, &
                              input_hyd%wasteload_data, input_hyd%time_in_seconds)
            else
                write (lunrep, *) 'warning: could not find file: '//trim(input_hyd%file_src%name)
                write (*, *) 'warning: could not find file: '//trim(input_hyd%file_src%name)
                input_hyd%file_src%name = 'TMP_'//trim(input_hyd%file_src%name)
                inquire (file=input_hyd%file_src%name, exist=exist_src)
                if (exist_src) then
                    write (lunrep, *) 'reading TMP_src-file: '//trim(input_hyd%file_src%name)
                    write (*, *) 'reading TMP_src-file: '//trim(input_hyd%file_src%name)
                    call read_src_tmp(input_hyd%file_src, input_hyd%nolay, input_hyd%wasteload_coll, &
                                      input_hyd%wasteload_data)
                else
                    write (lunrep, *) 'warning: could not find TMP_src-file: '//trim(input_hyd%file_src%name)
                    write (*, *) 'warning: could not find TMP_src-file: '//trim(input_hyd%file_src%name)
                    input_hyd%wasteload_coll%current_size = 0
                end if
            end if
        else
            write (lunrep, *) 'warning: you seem to be using patches. Agrhyd will try to handle the (TMP) src-files.'
            write (*, *) 'warning: you seem to be using patches. Agrhyd will try to handle the (TMP) src-files.'
            do ipatch = 9, 0, -1
                if (l_patch(ipatch)) then
                    exit
                end if
            end do
            ! read
            cpatch = ipatch
            inquire (file=input_patch_hyd(cpatch)%file_src%name, exist=exist_src)
            if (exist_src) then
                write (lunrep, *) 'reading src-file: '//trim(input_patch_hyd(cpatch)%file_src%name)
                write (*, *) 'reading src-file: '//trim(input_patch_hyd(cpatch)%file_src%name)
                call read_src(input_patch_hyd(cpatch)%file_src, input_hyd%nolay, input_hyd%wasteload_coll, &
                              input_hyd%wasteload_data, input_hyd%time_in_seconds)
            else
                write (lunrep, *) 'warning: could not find src-file: '//trim(input_patch_hyd(cpatch)%file_src%name)
                write (*, *) 'warning: could not find src-file: '//trim(input_patch_hyd(cpatch)%file_src%name)
                input_hyd%wasteload_coll%current_size = 0
            end if
            do ipatch = cpatch - 1, 0, -1
                if (input_hyd%wasteload_coll%current_size > 0 .and. l_patch(ipatch)) then
                    input_patch_hyd(ipatch)%file_src%name = 'TMP_'//trim(input_patch_hyd(ipatch)%file_src%name)
                    inquire (file=input_patch_hyd(ipatch)%file_src%name, exist=exist_src)
                    if (exist_src) then
                        write (lunrep, *) 'reading TMP_src-file: '//trim(input_patch_hyd(ipatch)%file_src%name)
                        write (*, *) 'reading TMP_src-file: '//trim(input_patch_hyd(ipatch)%file_src%name)
                        call read_src_tmp(input_patch_hyd(ipatch)%file_src, input_hyd%nolay, input_hyd%wasteload_coll, &
                                          input_hyd%wasteload_data)
                    else
                        write (lunrep, *) 'warning: could not find TMP_src-file: '//trim(input_patch_hyd(cpatch)%file_src%name)
                        write (*, *) 'warning: could not find TMP_src-file: '//trim(input_patch_hyd(cpatch)%file_src%name)
                        input_hyd%wasteload_coll%current_size = 0
                    end if
                end if
            end do
            if (input_hyd%wasteload_coll%current_size > 0) then
                input_hyd%file_src%name = 'TMP_'//trim(input_hyd%file_src%name)
                inquire (file=input_hyd%file_src%name, exist=exist_src)
                if (exist_src) then
                    write (lunrep, *) 'reading TMP_src-file: '//trim(input_hyd%file_src%name)
                    call read_src_tmp(input_hyd%file_src, input_hyd%nolay, input_hyd%wasteload_coll, input_hyd%wasteload_data)
                else
                    write (lunrep, *) 'warning: could not find TMP_src-file: '//trim(input_hyd%file_src%name)
                    write (*, *) 'warning: could not find TMP_src-file: '//trim(input_hyd%file_src%name)
                    input_hyd%wasteload_coll%current_size = 0
                end if
            end if
            if (input_hyd%wasteload_coll%current_size == 0) then
                write (lunrep, *) 'warning: agrhyd could not merge the (TMP) src-files. Data ignored.'
                write (*, *) 'warning: agrhyd could not merge the (TMP) src-files. Data ignored.'
                input_hyd%wasteload_data%num_breakpoints = 0
            end if
        end if
    end if

    ! allocate aggregation pointers

    allocate (ipnt_h(input_hyd%nmax, input_hyd%mmax), stat=ierr_alloc)
    if (ierr_alloc /= 0) then; write (*, *) ' error allocating memory'; call stop_with_error(); 
    end if
    allocate (ipnt_v(input_hyd%nolay), stat=ierr_alloc)
    if (ierr_alloc /= 0) then; write (*, *) ' error allocating memory'; call stop_with_error(); 
    end if

    ! read or set horizontal aggregation

    write (*, '(a)') 'Reading horizontal aggregation information ...'
    if (l_regular) then
        call agr_reg(input_hyd, output_hyd, m_fact, n_fact, m_offset, n_offset, ipnt_h)
    elseif (l_expand) then
        call agr_exp(input_hyd, output_hyd, ipnt_h)
    else
        if (output_hyd%file_dwq%name /= ' ') then
            output_hyd%file_dwq%type = FT_ASC
            call read_dwq(output_hyd%file_dwq, input_hyd%mmax, input_hyd%nmax, ipnt_h)
            ! Always calculate dispersion lenghts with unstructured grid and horizontal aggregation
            if (input_hyd%geometry == HYD_GEOM_UNSTRUC) then
                l_lenlen = .false.
            end if
        else
            ipnt_h = input_hyd%lgrid
            ! Automaticly use original dispersion lenghts with unstructured grid and no horizontal aggregation
            if (input_hyd%geometry == HYD_GEOM_UNSTRUC) then
                l_lenlen = .true.
            end if
        end if
    end if

    ! read vertical aggregation

    if (output_hyd%file_vag%name /= ' ') then
        output_hyd%file_vag%type = FT_ASC
        call read_vag(output_hyd%file_vag, input_hyd%nolay, ipnt_v, lunrep)
    else
        do ilay = 1, input_hyd%nolay
            ipnt_v(ilay) = ilay
        end do
    end if

    ! set aggregation pointers

    allocate (ipnt(input_hyd%noseg), stat=ierr_alloc)
    if (ierr_alloc /= 0) then; write (*, *) ' error allocating memory'; call stop_with_error(); 
    end if
    allocate (ipnt_vdf(input_hyd%noseg), stat=ierr_alloc)
    if (ierr_alloc /= 0) then; write (*, *) ' error allocating memory'; call stop_with_error(); 
    end if
    allocate (ipnt_tau(input_hyd%noseg), stat=ierr_alloc)
    if (ierr_alloc /= 0) then; write (*, *) ' error allocating memory'; call stop_with_error(); 
    end if
    !     nosegb     = -minval(ipnt_h)*input_hyd%nolay
    !     nosegb     = -minval(input_hyd%lgrid)*input_hyd%nolay
    if (minval(input_hyd%ipoint) < 0) then
        nosegb = -minval(input_hyd%ipoint)
        !         nosegb     = -minval(input_hyd%ipoint(1:2,1:input_hyd%nosegl))*input_hyd%nolay
        allocate (ipnt_b(nosegb), stat=ierr_alloc)
    else
        nosegb = 0
        allocate (ipnt_b(1), stat=ierr_alloc)
        ipnt_b(1) = 0
    end if
    if (ierr_alloc /= 0) then; write (*, *) ' error allocating memory'; call stop_with_error(); 
    end if

    call set_aggr_pnts(input_hyd, ipnt_h, ipnt_v, ipnt, ipnt_vdf, &
                       ipnt_b, nosegb, output_hyd, l_regular, ipnt_tau, &
                       l_expand, lunrep)

    ! aggregate time independent data

    write (*, '(a)') 'Starting aggregation ...'
    allocate (ipnt_q(input_hyd%noq), stat=ierr_alloc)
    if (ierr_alloc /= 0) then; write (*, *) ' error allocating memory'; call stop_with_error(); 
    end if
    call agr_hyd_init(input_hyd, ipnt, ipnt_h, ipnt_q, ipnt_vdf, ipnt_b, ipnt_v, output_hyd, l_regular, l_expand, l_lenlen)

    ! correct tau pointers for z model, the tau is only in the top layer, find the aggregated bottom segment

    if (is_command_arg_specified('-sing_z')) then
        write (*, *) ' commandline option -sing_z correct tau for z model'
        write (lunrep, *) ' commandline option -sing_z correct tau for z model'
        ipnt_tau = 0
        do iseg1 = 1, input_hyd%nosegl
            iseg2 = ipnt(iseg1)
            if (iseg2 > 0) then
                isegb = 0
                do ilay = 1, output_hyd%nolay
                    iseg = (ilay - 1) * output_hyd%nosegl + iseg2
                    ik1 = mod(output_hyd%attributes(iseg), 10)
                    ik2 = mod(output_hyd%attributes(iseg), 100) / 10
                    if (ik1 == 1) then
                        if (ik2 == 0 .or. ik2 == 3) then
                            isegb = iseg
                            exit
                        end if
                    end if
                end do
                ipnt_tau(iseg1) = isegb
            end if
        end do
    end if

    ! set the filenames in the hyd file

    call set_hyd(output_hyd, name)

    ! special singapore, write discharge names

    if (singapore_discharge_names /= ' ') then
        call write_sing_dis(output_hyd, singapore_discharge_names)
    end if

    ! determine ref time shift and new start/stop times
    output_shift = 0
    if (output_t0 /= ' ') then
        read (output_hyd%hyd_ref, '(I8,I6)', IOSTAT=ierr) input_t0_d, input_t0_t
        read (output_t0, '(I8,I6)', IOSTAT=ierr2) output_t0_d, output_t0_t
        if (ierr == 0 .and. ierr2 == 0) then
            input_t0_jul = julian_with_leapyears(input_t0_d, input_t0_t)
            output_t0_jul = julian_with_leapyears(output_t0_d, output_t0_t)
            if (output_t0_jul > 0.0 .and. input_t0_jul > 0.0) then
                output_shift = nint((input_t0_jul - output_t0_jul) * 86400)
                output_hyd%hyd_ref = output_t0
                output_hyd%cnv_ref = output_t0
            end if
        end if
        if (output_shift == 0 .and. output_hyd%cnv_ref == output_t0) then
            write (lunrep, *) 'warning: interpet reference_time_output is identical to current reference time'
            write (*, *) 'warning: interpet reference_time_output is identical to current reference time'
        else if (output_shift == 0) then
            write (lunrep, *) 'warning: could not properly interpet reference_time_output'
            write (*, *) 'warning: could not properly interpet reference_time_output'
        end if
    end if

    ! write hyd file
    call write_hyd(output_hyd, version)

    ! write time independent data

    call write_hyd_init(output_hyd)

    ! write ddp file, ddbound administration for part

    if (l_expand) then
        output_hyd%domain_coll = input_hyd%domain_coll
        output_hyd%dd_bound_coll = input_hyd%dd_bound_coll
        call write_ddp(output_hyd)
    end if

    ! write wasteload data to src file

    call write_src(output_hyd)

    ! write optionally src the file in waq format

    if (output_hyd%file_rfl%name /= ' ' .and. .not. any(l_patch)) then
        output_hyd%file_rfl%type = FT_ASC
        call write_rfl(output_hyd)
    end if

    ! write new lga on old grid also cco
    if (input_hyd%geometry == HYD_GEOM_CURVI) then
        if (l_regular) then
            new_lga%name = trim(name)//'_on_original_grid.lga'
            new_lga%type = ft_bin
            call write_lga(new_lga, input_hyd%mmax, input_hyd%nmax, output_hyd%nolay, output_hyd%nosegl, &
                           output_hyd%noq1, output_hyd%noq2, output_hyd%noq3, ipnt_h)
            new_cco%name = trim(name)//'_on_original_grid.cco'
            new_cco%type = ft_bin
            call write_cco(new_cco, input_hyd%mmax, input_hyd%nmax, input_hyd%xdepth, input_hyd%ydepth, &
                           output_hyd%nolay)
            new_grd%name = trim(name)//'.grd'
            new_grd%type = ft_asc
            call write_grd(new_grd, output_hyd%mmax, output_hyd%nmax, output_hyd%xdepth, output_hyd%ydepth)
        else
            new_grd%name = trim(name)//'.grd'
            new_grd%type = ft_asc
            call write_grd(new_grd, input_hyd%mmax, input_hyd%nmax, input_hyd%xdepth, input_hyd%ydepth)
        end if
    end if

    itime_first_patch = 2000000000
    cpatch = -1
    npatch = -1
    do ipatch = 0, 9
        if (l_patch(ipatch)) then
            ! read the first timestep of the patch to see where the patch starts
            call read_hyd_step(input_patch_hyd(ipatch), itime_first_patch(ipatch), iend)

            if (iend /= 0) then
                write (lunrep, *) 'error: could not read first timestep of input hydrodynamics patch : '// &
                    trim(input_patch_hyd(ipatch)%file_hyd%name)
                write (*, *) 'error: could not read first timestep of input hydrodynamics patch : '// &
                    trim(input_patch_hyd(ipatch)%file_hyd%name)
                call stop_with_error()
            end if
        end if
    end do

    ! look for the next patch
    do
        npatch = npatch + 1
        if (l_patch(npatch) .or. npatch == 10) then
            exit
        end if
    end do

    ! time loop

    write (*, '(A)') 'timestamp       seconds        days'
    write (lunrep, '(A)') 'timestamp       seconds        days'

    do
        if (cpatch == -1) then
            call read_hyd_step(input_hyd, itime, iend)
        else
            call read_hyd_step(input_patch_hyd(cpatch), itime, iend)
        end if
        if (l_patch(npatch)) then
            if (itime >= itime_first_patch(npatch)) then
                !              switch a the next patch
                cpatch = npatch
                write (lunrep, *) 'switching to input hydrodynamics patch: '// &
                    trim(input_patch_hyd(cpatch)%file_hyd%name)
                write (*, *) 'switching to input hydrodynamics patch: '// &
                    trim(input_patch_hyd(cpatch)%file_hyd%name)
                !              and look for the next patch after this one
                do
                    npatch = npatch + 1
                    if (l_patch(npatch) .or. npatch == 10) then
                        if (itime_first_patch(npatch) > itime_first_patch(cpatch)) then
                            exit
                        end if
                        cpatch = npatch
                        write (lunrep, *) 'warning: next patch has same starting time!'
                        write (*, *) 'warning: next patch has same starting time!'
                        write (lunrep, *) 'switching to input hydrodynamics patch: '// &
                            trim(input_patch_hyd(cpatch)%file_hyd%name)
                        write (*, *) 'switching to input hydrodynamics patch: '// &
                            trim(input_patch_hyd(cpatch)%file_hyd%name)
                    end if
                end do
            end if
        end if

        if (iend /= 0) exit
        rday = itime / 86400.
        write (*, '(A,I10,F12.3)') 'reading step:', itime, rday
        write (lunrep, '(A,I10,F12.3)') 'reading step:', itime, rday

        if (itime >= output_start .and. itime <= output_stop) then

            if (cpatch == -1) then
                call agr_hyd_step(input_hyd, ipnt, ipnt_q, ipnt_vdf, ipnt_tau, output_hyd)
            else
                call agr_hyd_step(input_patch_hyd(cpatch), ipnt, ipnt_q, ipnt_vdf, ipnt_tau, output_hyd)
            end if

            if (output_shift == 0) then
                write (*, '(A,I10,F12.3)') 'writing step:', itime, rday
                write (lunrep, '(A,I10,F12.3)') 'writing step:', itime, rday
            else
                wdayshift = (itime + output_shift) / 86400.
                write (*, '(A,I10,F12.3,A)') 'writing step:', itime + output_shift, wdayshift, ' (shifted)'
                write (lunrep, '(A,I10,F12.3,A)') 'writing step:', itime + output_shift, wdayshift, ' (shifted)'
            end if
            call write_hyd_step(output_hyd, itime + output_shift)

        elseif (itime > output_stop) then
            write (lunrep, *) 'not used step and exit time loop:', itime, rday
            exit
        else
            write (lunrep, *) 'not used step:', itime, rday
        end if

    end do

    ! finished

    call write_date_time(rundat)
    write (lunrep, *)
    write (lunrep, '(a)') ' normal end of execution'
    write (lunrep, '(2a)') ' execution stop : ', rundat
    write (*, *)
    write (*, '(a)') ' normal end of execution'
    write (*, '(2a)') ' execution stop : ', rundat

end program
