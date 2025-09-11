!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
!
module m_read_nf2ff_files
    use m_nfl_data
    implicit none
contains
!
!
!==============================================================================
subroutine read_nf2ff_files()
    !
    ! Locals
    integer :: idis
    logical :: error
    logical :: waitlog
    !
    ! Body
    call mess(LEVEL_INFO, "Reading NF2FF files")
    waitlog = .true.
    do
        call wait_until_finished(nf_num_dif, waitfiles, idis, filename(2), waitlog, error)
        !
        ! Error: just try again
        !
        if (error) cycle
        !
        ! idis=0 when all files are processed
        !
        if (idis == 0) exit
        !
        call nf_2_flow(filename(2), idis, error)
        !
        ! Error: just try again
        !
        if (error) cycle
        !
        ! No error appeared: Remove the processed file from the waitfiles
        !
        waitfiles(idis) = ' '
    enddo
end subroutine read_nf2ff_files
!
!
!==============================================================================
subroutine wait_until_finished (nf_num_dif, waitfiles, idis, filename, waitlog, error)
    !
    ! Parameters
    integer                        , intent(in)  :: nf_num_dif
    character(*), dimension(nf_num_dif)          :: waitfiles
    integer                        , intent(out) :: idis
    character(*)                   , intent(out) :: filename
    logical                        , intent(in)  :: waitlog
    logical                        , intent(out) :: error
    !
    ! Locals
    integer                 :: lun
    integer                 :: ios
    integer                 :: i
    logical                 :: ex_file    ! true: file exists
    logical                 :: fileok     ! true: file contains the end tag </NF2FF>
    character(300)          :: line
    !
    ! Body
    error = .false.
    !
    ! Check for how many files we are waiting to appear
    idis = 0
    do i=1, nf_num_dif
        if (waitfiles(i) /= ' ') then
            idis = i
            if (waitlog) then
                call mess(LEVEL_INFO, "Waiting for file '", trim(waitfiles(i)), "' to appear ...")
            endif
        endif
    enddo
    !
    ! Return when all files did appear (and waitfiles is empty). idis must be 0.
    if (idis == 0) return
    !
    fileok = .false.
    do while (.not.fileok)
        ex_file = .false.
        !
        ! Examine if one of the files exists
        ! This will cost CPU time, but there is nothing else to do (Cosumo/Cormix run on another machine)
        !
        idis     = 0
        filename = ' '
        do while (.not. ex_file)
           do i=1, nf_num_dif
               if (waitfiles(i) /= ' ') then
                   inquire (file=waitfiles(i), exist=ex_file)
                   if (ex_file) then
                       filename     = waitfiles(i)
                       idis         = i
                       !
                       ! Do not remove the found file from the waitfiles here:
                       ! It will be removed in near_field, when the full reading of the file finished successfully
                       !
                       exit
                   endif
               endif
           enddo
        enddo
        !
        ! File found: open file and search for the end tag </NF2FF>
        !
        open (newunit=lun,file=filename,iostat=ios)
        if (ios /= 0) then
            ! try again
            cycle
        endif
        do while (ios == 0)
            read (lun,'(a)',iostat=ios) line
            if (index(line,'</NF2FF>') >= 1) then
                fileok = .true.
            endif
        enddo
        close(lun)
    enddo
    call mess(LEVEL_INFO, "Scanned    file '", trim(filename), "'")
end subroutine wait_until_finished
!
!
!==============================================================================
subroutine nf_2_flow(filename, idis, error)
    use precision
    use properties
    use string_module
    use message_module
    use m_alloc
    !
    ! Parameters
    character(len=*), intent(in)    :: filename
    integer         , intent(in)    :: idis
    logical         , intent(out)   :: error
    !
    ! Locals
    integer                                :: i
    integer                                :: idim
    integer                                :: istat
    integer                                :: numrealonline
    real(sp)                               :: version
    real(hp), dimension(:), allocatable    :: r_input
    character(40)                          :: type_string
    character(50)                          :: key
    character(300)                         :: cdummy
    character(300)                         :: errmsg
    character(1000)                        :: line
    character(len=1), dimension(:),pointer :: data_ptr
    type(tree_data), pointer               :: file_ptr
    type(tree_data), pointer               :: nf2ff_ptr
    type(tree_data), pointer               :: nfresult_ptr
    type(tree_data), pointer               :: node_ptr
    !
    ! Body
    error = .false.
    allocate(r_input(max(2,fm_numconst)), stat=istat)
    !
    ! Create Cosumo input tree
    !
    call mess(LEVEL_INFO, "Reading file '", trim(filename), "' ...")
    call tree_create( 'TransportFormula Input', file_ptr )
    call tree_put_data(file_ptr, transfer(trim(filename),node_value), 'STRING')
    !
    ! Put file in input tree
    !
    call prop_file('xml',trim(filename),file_ptr,istat)
    if (istat /= 0) then
        select case (istat)
        case(1)
            errmsg = FILE_NOT_FOUND // trim(filename)
        case(3)
            errmsg = PREMATURE_EOF // trim(filename)
        case default
            errmsg = FILE_READ_ERROR // trim(filename)
        endselect
        call mess(LEVEL_ERROR, errmsg)
        error = .true.
    endif
    !
    call tree_get_node_by_name(file_ptr, 'nf2ff', nf2ff_ptr )
    if (.not.associated(nf2ff_ptr)) then
        call mess(LEVEL_ERROR, "Tag '<NF2FF>' not found")
        error = .true.
    endif
    version     = -999.9
    call prop_get(file_ptr, 'NF2FF/fileVersion', version)
    if (comparereal(version, 0.3_sp) /= 0) then
        call mess(LEVEL_ERROR, "Unexpected FileVersion number read: ", version)
        error = .true.
    endif
    !
    nf_q_source(idis) = -1.0_fp
    call prop_get(file_ptr, 'NF2FF/discharge/Qsource', nf_q_source(idis))
    if (comparereal(nf_q_source(idis), 0.0_fp) == -1) then
        ! Trying old keyword M3s
        call prop_get(file_ptr, 'NF2FF/discharge/M3s', nf_q_source(idis))
        if (comparereal(nf_q_source(idis), 0.0_fp) == -1) then
            call mess(LEVEL_ERROR, "'<NF2FF> / <discharge> / <Qsource>' expected with a value >= zero")
            error = .true.
        endif
    endif
    call prop_get(file_ptr, 'NF2FF/discharge/Qintake', nf_q_intake(idis))
    if (comparereal(nf_q_intake(idis), 0.0_fp) == -1) then
        if (comparereal(nf_q_intake(idis), -999.0_fp) == 0) then
            nf_q_intake(idis) = 0.0_fp
            call mess(LEVEL_WARN, "'<NF2FF> / <discharge> / <Qintake>' not specified. Set to zero")
        else
            call mess(LEVEL_ERROR, "'<NF2FF> / <discharge> / <Qintake>' expected with a value >= zero")
            error = .true.
       endif
    endif
    call prop_get(file_ptr, 'NF2FF/discharge/constituentsoperator', cdummy)
    call str_lower(cdummy)
    nf_const_operator(idis) = cdummy(:10)
    !
    r_input = -999.0_fp
    call prop_get(file_ptr, 'NF2FF/discharge/constituents', r_input, fm_numconst)
    do i=1,fm_numconst
        nf_const(idis,i) = r_input(i)
    enddo
    !
    call tree_get_node_by_name(nf2ff_ptr, 'nfresult', nfresult_ptr)
    !
    ! Intakes
    ! dim2=3: X,Y,Z
    ! The number of lines is stored as the data in intakes itself
    ! The data is stored in childnodes with name 1, 2, 3, ...
    !
    idim = 0
    call tree_get_node_by_name(nfresult_ptr, 'intakes', node_ptr)
    if (associated(node_ptr)) then
        call tree_get_data_ptr(node_ptr, data_ptr, type_string)
        if (type_string == "STRING:XMLNUMDATALINES") then
            call prop_get(file_ptr, 'NF2FF/NFResult/intakes', idim)
        else
            idim = 1
        endif
    endif
    if (idim == 0) then
        ! No intake points in the NF2FF file, use the single intake point from the Cosumo file
        idim = 1
        nf_intake_idimMAX = max(nf_intake_idimMAX, idim)
        call reallocP(nf_intake, (/ idis,nf_intake_idimMAX,3  /), keepExisting=.true., fill = 0.0_fp)
        nf_intake(idis,1,1) = x_intake(idis)
        nf_intake(idis,1,2) = y_intake(idis)
        nf_intake(idis,1,3) = 0.0_fp
    else  
        nf_intake_idimMAX = max(nf_intake_idimMAX, idim)
        call reallocP(nf_intake, (/ idis,nf_intake_idimMAX,3  /), keepExisting=.true., fill = 0.0_fp)
        if (idim == 1) then
            call prop_get(file_ptr, 'NF2FF/NFResult/intakes', nf_intake(idis,idim,:), 3)
        else
            do i=1,idim
                write(key,'(a,i0)') 'NF2FF/NFResult/intakes/', i
                call prop_get(file_ptr, trim(key), nf_intake(idis,i,:), 3)
            enddo
        endif
    endif
    !
    ! Sinks
    ! dim2=6: X,Y,Z,S,H,B
    ! The number of lines is stored as the data in intakes itself
    ! The data is stored in childnodes with name 1, 2, 3, ...
    !
    idim = 0
    call tree_get_node_by_name(nfresult_ptr, 'sinks', node_ptr)
    if (associated(node_ptr)) then
        call tree_get_data_ptr(node_ptr, data_ptr, type_string)
        if (type_string == "STRING:XMLNUMDATALINES") then
            call prop_get(file_ptr, 'NF2FF/NFResult/sinks', idim)
        else
            idim = 1
        endif
        nf_sink_idimMAX = max(nf_sink_idimMAX, idim)
        call reallocP(nf_sink, (/ idis,nf_sink_idimMAX,6  /), keepExisting=.true., fill = 0.0_fp)
        if (idim == 1) then
            call prop_get(file_ptr, 'NF2FF/NFResult/sinks', nf_sink(idis,idim,:), 6)
        else
            do i=1,idim
                write(key,'(a,i0)') 'NF2FF/NFResult/sinks/', i
                call prop_get(file_ptr, trim(key), nf_sink(idis,i,:), 6)
            enddo
        endif
    endif
    !
    ! Sources
    ! dim2=6 or 8: X,Y,Z,S,H,B,Umag (optionally), Udir (optionally)
    ! The number of lines is stored as the data in intakes itself
    ! The data is stored in childnodes with name 1, 2, 3, ...
    !
    idim = 0
    call tree_get_node_by_name(nfresult_ptr, 'sources', node_ptr)
    if (associated(node_ptr)) then
        call tree_get_data_ptr(node_ptr, data_ptr, type_string)
        if (type_string == "STRING:XMLNUMDATALINES") then
            call prop_get(file_ptr, 'NF2FF/NFResult/sources', idim)
            call prop_get(file_ptr, 'NF2FF/NFResult/sources/1', line)
        else
            idim = 1
            call prop_get(file_ptr, 'NF2FF/NFResult/sources', line)
        endif
        numrealonline = count_words(trim(line))
        if (numrealonline == 6) then
            nf_src_mom(idis) = .false.
        elseif (numrealonline == 8) then
            nf_src_mom(idis) = .true.
        else
            write(errmsg,'(i0)') numrealonline
            call mess(LEVEL_ERROR, "<NF2FF> / <NFResult> / <sources> has ", errmsg, " columns; expecting 6 (X,Y,Z,S,H,B) or 8 (+Umag, Udir).")
            error = .true.
        endif
        nf_sour_idimMAX = max(nf_sour_idimMAX, idim)
        call reallocP(nf_sour, (/ idis,nf_sour_idimMAX,8  /), keepExisting=.true., fill = 0.0_fp)
        if (idim == 1) then
            call prop_get(file_ptr, 'NF2FF/NFResult/sources', nf_sour(idis,idim,:), numrealonline)
        else
            do i=1,idim
                write(key,'(a,i0)') 'NF2FF/NFResult/sources/', i
                call prop_get(file_ptr, trim(key), nf_sour(idis,i,:), numrealonline)
            enddo
        endif
    else
        ! There must be at least 1 source point
        !
        error = .true.
    endif
    !
    call tree_destroy(file_ptr)
    deallocate(r_input, stat=istat)
end subroutine nf_2_flow


end module m_read_nf2ff_files
