subroutine nf_2_flow(filename, error, gdp)
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
!  $Id: nf_2_flow.f90 6830 2016-12-07 16:34:59Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/nf_2_flow.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use string_module
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer                  , pointer :: lundia
    integer                  , pointer :: lstsc
    real(fp)                 , pointer :: nf_q_source
    real(fp)                 , pointer :: nf_q_intake
	integer                  , pointer :: nf_const_operator
    real(fp), dimension(:)   , pointer :: nf_const 
    real(fp), dimension(:,:) , pointer :: nf_intake
    real(fp), dimension(:,:) , pointer :: nf_sink  
    real(fp), dimension(:,:) , pointer :: nf_sour  
	logical                  , pointer :: nf_src_mom
!
! Global variables
!
    character(len=*), intent(in)             :: filename
    logical         , intent(out)            :: error
!
! Local variables
!
    integer                                :: i
    integer                                :: idim
    integer                                :: ierror
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
!! executable statements -------------------------------------------------------
!
    lundia            => gdp%gdinout%lundia
    lstsc             => gdp%d%lstsc
    nf_q_source       => gdp%gdnfl%nf_q_source
    nf_q_intake       => gdp%gdnfl%nf_q_intake
    nf_const_operator => gdp%gdnfl%nf_const_operator
    nf_const          => gdp%gdnfl%nf_const 
    nf_sink           => gdp%gdnfl%nf_sink  
    nf_sour           => gdp%gdnfl%nf_sour  
    nf_src_mom        => gdp%gdnfl%nf_src_mom
    !
    error = .false.
    allocate(r_input(max(2,lstsc)), stat=istat)
    !
    ! Create Cosumo input tree
    !
    write(lundia,'(3a)') "Reading file '", trim(filename), "' ..."
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
          call write_error(errmsg, unit=lundia)
       case(3)
          errmsg = PREMATURE_EOF // trim(filename)
          call write_error(errmsg, unit=lundia)
       case default
          errmsg = FILE_READ_ERROR // trim(filename)
          call write_error(errmsg, unit=lundia)
       endselect
       error = .true.
    endif
    !
    call tree_get_node_by_name(file_ptr, 'nf2ff', nf2ff_ptr )
    if (.not.associated(nf2ff_ptr)) then
       write(lundia, '(a)') "ERROR: Tag '<NF2FF>' not found"
       error = .true.
    endif
    version     = -999.9
    call prop_get(file_ptr, 'NF2FF/fileVersion', version)
    if (comparereal(version, 0.3_sp) /= 0) then
       write(lundia,'(a,f5.2)') "ERROR: Unexpected FileVersion number read: ", version
       error = .true.
    endif
    !
    call prop_get(file_ptr, 'NF2FF/discharge/Qsource', nf_q_source)
    if (comparereal(nf_q_source, 0.0_fp) == -1) then
       write(lundia,'(a)') "ERROR: '<NF2FF> / <discharge> / <Qsource>' expected with a value >= zero"
       error = .true.
    endif
    call prop_get(file_ptr, 'NF2FF/discharge/Qintake', nf_q_intake)
    if (comparereal(nf_q_intake, 0.0_fp) == -1) then
       if (comparereal(nf_q_intake, -999.0_fp) == 0) then
          nf_q_intake = 0.0_fp
          write(lundia,'(a)') "WARNING: '<NF2FF> / <discharge> / <Qintake>' not specified. Set to zero"
       else
          write(lundia,'(a)') "ERROR: '<NF2FF> / <discharge> / <Qintake>' expected with a value >= zero"
          error = .true.
       endif
    endif
    call prop_get(file_ptr, 'NF2FF/discharge/constituentsoperator', cdummy)
    call str_lower(cdummy)
    if (cdummy == "absolute") then
       nf_const_operator = NFLCONSTOPERATOR_ABS
    elseif (cdummy == "excess") then
       nf_const_operator = NFLCONSTOPERATOR_EXC
    else
       write(lundia,'(a)') "ERROR: '<NF2FF> / <discharge> / <constituentsOperator>' expected with value 'excess' or 'absolute'"
       error = .true.
    endif
    !
    r_input = -999.0_fp
    call prop_get(file_ptr, 'NF2FF/discharge/constituents', r_input, lstsc)
    do i=1,lstsc
       nf_const(i) = r_input(i)
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
    if (associated(gdp%gdnfl%nf_intake)) then
       deallocate (gdp%gdnfl%nf_intake, stat = istat)
    endif
    allocate (gdp%gdnfl%nf_intake(idim,3), stat = istat)
    nf_intake => gdp%gdnfl%nf_intake
    if (idim == 1) then
       call prop_get(file_ptr, 'NF2FF/NFResult/intakes', nf_intake(idim,:), 3)
    else
       do i=1,idim
          write(key,'(a,i0)') 'NF2FF/NFResult/intakes/', i
          call prop_get(file_ptr, trim(key), nf_intake(i,:), 3)
       enddo
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
       if (associated(gdp%gdnfl%nf_sink)) then
          deallocate (gdp%gdnfl%nf_sink, stat = istat)
       endif
       allocate (gdp%gdnfl%nf_sink(idim,6), stat = istat)
       nf_sink => gdp%gdnfl%nf_sink
       if (idim == 1) then
          call prop_get(file_ptr, 'NF2FF/NFResult/sinks', nf_sink(idim,:), 6)
       else
          do i=1,idim
             write(key,'(a,i0)') 'NF2FF/NFResult/sinks/', i
             call prop_get(file_ptr, trim(key), nf_sink(i,:), 6)
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
          nf_src_mom = .false.
       elseif (numrealonline == 8) then
          nf_src_mom = .true.
       else
          write(lundia, '(a,i0,a)') "ERROR: <NF2FF> / <NFResult> / <sources> has ", numrealonline, " columns; expecting 6 (X,Y,Z,S,H,B) or 8 (+Umag, Udir)."
          error = .true.
       endif
       if (associated(gdp%gdnfl%nf_sour)) then
          deallocate (gdp%gdnfl%nf_sour, stat = istat)
       endif
       allocate (gdp%gdnfl%nf_sour(idim,numrealonline), stat = istat)
       nf_sour => gdp%gdnfl%nf_sour
       if (idim == 1) then
          call prop_get(file_ptr, 'NF2FF/NFResult/sources', nf_sour(idim,:), numrealonline)
       else
          do i=1,idim
             write(key,'(a,i0)') 'NF2FF/NFResult/sources/', i
             call prop_get(file_ptr, trim(key), nf_sour(i,:), numrealonline)
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
    !open( newunit=lun, file = filename )
    !
    !do i = 1,no_val
    !    read( lun, *, iostat = ierr ) x_jet(i), y_jet(i), z_jet(i), s_jet(i), h_jet(i), b_jet(i)
    !    if ( ierr /= 0 ) then
    !        no_val = i - 1
    !        exit
    !    endif
    !enddo
    !close( lun )
end subroutine nf_2_flow
