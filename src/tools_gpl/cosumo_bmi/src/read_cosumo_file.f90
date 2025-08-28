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
module m_read_cosumo_file
    use m_nfl_data
    implicit none
contains
!
!
!==============================================================================
subroutine read_cosumo_file(mode)
    !
    ! Parameters
    integer, intent(in) :: mode !< READ_DIMENSIONS_ONLY or READ_FULL_CONTENTS
    !
    ! Body
    call read_file(mode)
    if (mode == READ_FULL_CONTENTS) then
        call find_n()
    endif
end subroutine read_cosumo_file
!
!
!==============================================================================
subroutine read_file(mode)
    use m_alloc
    !
    ! Parameters
    integer, intent(in) :: mode !< READ_DIMENSIONS_ONLY or READ_FULL_CONTENTS
    !
    ! Locals
    logical                             :: error
    integer                             :: i
    integer                             :: idis
    integer                             :: istat
    integer                             :: j
    integer                             :: no_dis_read
    real(sp)                            :: version
    real(hp), dimension(:), allocatable :: r_input
    character(300)                      :: cdummy
    character(300)                      :: errmsg
    type(tree_data), pointer            :: cosumoblock_ptr
    type(tree_data), pointer            :: data_ptr
    type(tree_data), pointer            :: node_ptr
    !
    ! Body
    write(errmsg,'(a,i0,2a)') "Reading NearField file part ", mode, ": ", trim(infile)
    call mess(LEVEL_INFO, trim(errmsg))
    error = .false.
    !
    select case (mode)
    case(READ_DIMENSIONS_ONLY)
        !
        ! Reset dimensions
        nf_num_dif = 0
        no_amb_max = 0
        call reallocP(no_amb, 1, keepExisting=.false., fill = 0)
        !
        ! Delete data read at the previous time; it will be reread
        if (associated(cosumofile_ptr)) then
            call tree_destroy(cosumofile_ptr)
            cosumofile_ptr => null ()
        endif
        call tree_create( 'NearField Input', cosumofile_ptr )
        call tree_put_data( cosumofile_ptr, transfer(trim(infile),node_value), 'STRING' )
        !
        ! Put file in input tree
        !
        call prop_file('xml',trim(infile),cosumofile_ptr,istat)
        if (istat /= 0) then
            select case (istat)
            case(1)
                call mess(LEVEL_ERROR, "File not found: ", trim(infile))
            case(3)
                call mess(LEVEL_ERROR, "Premature EOF in file: ", trim(infile))
            case default
                call mess(LEVEL_ERROR, "Read error from file: ", trim(infile))
            endselect
            call throwexception()
        endif
        !
        ! Read dimensions
        call tree_get_node_by_name( cosumofile_ptr, 'cosumo', cosumoblock_ptr )
        if (.not.associated(cosumoblock_ptr)) then
            call mess(LEVEL_ERROR, "Tag '<COSUMO>' not found in file: ", trim(infile))
            nullify(cosumofile_ptr)
            return
        endif
        do i=1, size(cosumoblock_ptr%child_nodes)
            if (tree_get_name(cosumoblock_ptr%child_nodes(i)%node_ptr) == "settings") then
                nf_num_dif = nf_num_dif + 1
                no_amb(1)  = 0
                nullify(data_ptr)
                call tree_get_node_by_name(cosumoblock_ptr%child_nodes(i)%node_ptr, 'data', data_ptr )
                if (.not. associated(data_ptr)) cycle
                do j=1, size(data_ptr%child_nodes)
                    if (tree_get_name(data_ptr%child_nodes(j)%node_ptr) == "xyambient") then
                        no_amb(1) = no_amb(1) + 1
                    endif
                enddo
                no_amb_max = max(no_amb_max, no_amb(1))
            endif
        enddo
        !
    case(READ_FULL_CONTENTS)
        call reallocP(no_amb, nf_num_dif, keepExisting=.false., fill = 0)
        allocate(r_input(max(2,fm_numconst)), stat=istat)
        !
        call tree_get_node_by_name( cosumofile_ptr, 'cosumo', cosumoblock_ptr )
        version     = -999.9
        no_dis_read = 0
        do i=1, size(cosumoblock_ptr%child_nodes)
            if (tree_get_name(cosumoblock_ptr%child_nodes(i)%node_ptr) == "settings") then
                no_dis_read = no_dis_read + 1
            endif
        enddo
        if (no_dis_read /= nf_num_dif) then
            write(errmsg,'(a,i0)') "Unexpected number of discharges read: ", no_dis_read
            call mess(LEVEL_ERROR, trim(errmsg))
            call throwexception()
        endif
        !
        call prop_get(cosumofile_ptr, 'COSUMO/fileVersion', version)
        if (comparereal(version, 0.3_sp) /= 0) then
            write(errmsg,'(a,f5.2)') "Unexpected FileVersion number read: ", version
            call mess(LEVEL_ERROR, trim(errmsg))
            call throwexception()
        endif
        !
        ! Momentum Relaxation is currently not supported for FM
        !momrelax = -999.0_fp
        !call prop_get(cosumofile_ptr, 'COSUMO/momentumRelaxation', momrelax)
        !if (comparereal(momrelax, 0.0_fp) == 1) then
        !   write(errmsg,'(a,f5.2,a)') "Message: Nearfield Momentum Relaxation = 1/(", momrelax, "*dt)"
        !   call mess(LEVEL_INFO, trim(errmsg))
        !else
        !   momrelax = 5.0_fp
        !endif
        !
        idis = 0
        do i=1, size(cosumoblock_ptr%child_nodes)
            node_ptr => cosumoblock_ptr%child_nodes(i)%node_ptr
            if (tree_get_name(node_ptr) /= "settings") cycle
            idis = idis + 1
            !
            ! Read position diffuser
            !
            r_input = -999.0_fp
            call prop_get(node_ptr, 'data/xydiff', r_input, 2)
            x_diff(idis) = r_input(1)
            y_diff(idis) = r_input(2)
            !
            ! Read positions ambient conditions
            !
            no_amb(idis) = 0
            nullify(data_ptr)
            call tree_get_node_by_name(cosumoblock_ptr%child_nodes(i)%node_ptr, 'data', data_ptr )
            if (.not. associated(data_ptr)) then
                call mess(LEVEL_ERROR, "<settings> block does not contain a <data> block.")
                error = .true.
                cycle
            endif
            do j=1, size(data_ptr%child_nodes)
                if (tree_get_name(data_ptr%child_nodes(j)%node_ptr) == "xyambient") then
                    no_amb(idis) = no_amb(idis) + 1
                    r_input = -999.0_fp
                    call prop_get(data_ptr%child_nodes(j)%node_ptr, 'xyambient', r_input, 2)
                    x_amb(idis,no_amb(idis)) = r_input(1)
                    y_amb(idis,no_amb(idis)) = r_input(2)
                endif
            enddo
            !
            ! Read intake location
            !
            r_input = -999.0_fp
            call prop_get(node_ptr, 'data/xyintake', r_input, 2)
            x_intake(idis) = r_input(1)
            y_intake(idis) = r_input(2)
            !
            ! Read discharge characteristics
            !
            call prop_get(node_ptr, 'data/discharge/m3s', q_diff(idis))
            !
            ! constituentsoperator of the Cosumo config file is not used
            ! The operator specified in the NF2FF file is passed through to FM
            !cdummy = ' '
            !call prop_get(node_ptr, 'data/discharge/constituentsoperator', cdummy)
            !call str_lower(cdummy)
            !if (cdummy == "absolute") then
            !   const_operator(idis) = NFLCONSTOPERATOR_ABS
            !elseif (cdummy == "excess") then
            !   const_operator(idis) = NFLCONSTOPERATOR_EXC
            !else
            !   call mess(LEVEL_ERROR, "'<settings> / <data> / <discharge> / <constituentsOperator>' expected with value 'excess' or 'absolute'")
            !   error = .true.
            !endif
            r_input = -999.0_fp
            call prop_get(node_ptr, 'data/discharge/constituents', r_input, fm_numconst)
            do j=1,fm_numconst
                const_diff(idis,j) = r_input(j)
            enddo
            !
            ! Read remainder of cormix general input
            !
            call prop_get(node_ptr, 'data/d0', d0(idis))
            call prop_get(node_ptr, 'data/h0', h0(idis))
            call prop_get(node_ptr, 'data/theta0', theta0(idis))
            call prop_get(node_ptr, 'data/sigma0', sigma0(idis))
            call prop_get(node_ptr, 'comm/ff2nfdir', base_path(idis))
            call prop_get(node_ptr, 'comm/ffrundir', basecase(idis))
            if (trim(basecase(idis)) == "rundir") then
                call getcwd(cdummy)
                basecase(idis) = trim(cdummy)//slash
                write(errmsg,'(3a)') "corinp_gen2: 'rundir' substituted by '", trim(basecase(idis)), "'"
                call mess(LEVEL_DEBUG, trim(errmsg))
            endif
        enddo
        !
        deallocate(r_input, stat=istat)
        deallocate(r_input, stat=istat)
        !
        if (error) then
            call throwexception()
        endif
    endselect
end subroutine read_file
!
!
!==============================================================================
!> Get D-Flow FM node ids related to x,y-values of the diffusers, intakes and ambient points
subroutine find_n()
    use m_ec_basic_interpolation
    !
    ! Locals
    integer :: idis
    !
    ! Body
    call nearest_neighbour(nf_num_dif, x_diff  , y_diff  , null(), n_diff  , Huge(1.d0), fm_xzw, fm_yzw, fm_ndx, 0, 0)
    call nearest_neighbour(nf_num_dif, x_intake, y_intake, null(), n_intake, Huge(1.d0), fm_xzw, fm_yzw, fm_ndx, 0, 0)
    do idis = 1, nf_num_dif
        call nearest_neighbour(no_amb(idis), x_amb(idis,:), y_amb(idis,:), null(), n_amb(idis,:), Huge(1.d0), fm_xzw, fm_yzw, fm_ndx, 0, 0)
    enddo
end subroutine find_n

end module m_read_cosumo_file
