program corinp_gen2
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!    Function: Reads input needed for the coupling of Corjet/Cortime/Cormix
!              with Delft3d-Flow
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use precision
    use properties
    use string_module
    !
    implicit none
    !
    integer                                    :: no_dis
    real(fp)      ,dimension(:)  , allocatable :: x_diff
    real(fp)      ,dimension(:)  , allocatable :: y_diff
    real(fp)      ,dimension(:,:), allocatable :: xy_amb
    real(sp)      ,dimension(:)  , allocatable :: q_diff
    real(hp)      ,dimension(:,:), allocatable :: s0_diff
    real(hp)      ,dimension(:,:), allocatable :: intakes
    real(hp)      ,dimension(:,:), allocatable :: sinks
    real(hp)      ,dimension(:,:), allocatable :: sources
    character(256),dimension(:)  , allocatable :: linkDir
!
! Global variables
!
    logical   :: error
    logical   :: exist
    logical   :: test
!
! Local variables
!
    integer                :: luntmp
    integer, external      :: newunit
    integer                :: i
    integer                :: j
    integer                :: idis
    integer                :: istat
    integer                :: numlines
    integer                :: numrealonline
    real(fp)               :: version
    real(fp), dimension(3) :: real_input
    character(50)          :: key
    character(256)         :: filename
    character(300)         :: errmsg
    character(1000)        :: line
    type(tree_data), pointer :: cosumofile_ptr
    type(tree_data), pointer :: cosumoblock_ptr
    type(tree_data), pointer :: node_ptr
!
!! executable statements -------------------------------------------------------
!

    !
    ! Create Cosumo input tree
    !
    filename = "COSUMOsettings.xml"
    write(*,*) "Reading XML file '", trim(filename), "'"
    call tree_create( 'COSUMO Input', cosumofile_ptr )
    call tree_put_data( cosumofile_ptr, transfer(trim(filename),node_value), 'STRING:XML' )
    !
    ! Put file in input tree
    !
    call prop_file('xml',trim(filename),cosumofile_ptr,istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          errmsg = "FILE_NOT_FOUND" // trim(filename)
          write(*,*) trim(errmsg)
       case(3)
          errmsg = "PREMATURE_EOF" // trim(filename)
          write(*,*) trim(errmsg)
       case default
          errmsg = "FILE_READ_ERROR" // trim(filename)
          write(*,*) trim(errmsg)
       endselect
       error = .true.
       stop
    endif
    call tree_get_node_by_name( cosumofile_ptr, 'COSUMO', cosumoblock_ptr )
    if (.not.associated(cosumoblock_ptr)) then
       error = .true.
       stop
    endif
    test    = .false.
    version = -999.9
    no_dis  = 0
    do i=1, size(cosumoblock_ptr%child_nodes)
       if (tree_get_name(cosumoblock_ptr%child_nodes(i)%node_ptr) == "settings") then
          no_dis = no_dis + 1
       endif
    enddo
    allocate(x_diff(no_dis), stat=istat); x_diff = -999.9
    allocate(y_diff(no_dis), stat=istat); y_diff = -999.9
    allocate(xy_amb(no_dis,2), stat=istat); xy_amb = -999.9
    allocate(q_diff(no_dis), stat=istat); q_diff = -999.9
    allocate(s0_diff(no_dis,3), stat=istat); s0_diff = -999.9
    allocate(linkDir(no_dis), stat=istat); linkDir = ' '
    
    call prop_get(cosumofile_ptr, 'COSUMO/test', test)
    call prop_get(cosumofile_ptr, 'COSUMO/version', version)
    
    idis = 0
    do i=1, size(cosumoblock_ptr%child_nodes)
       node_ptr => cosumoblock_ptr%child_nodes(i)%node_ptr
       if (tree_get_name(node_ptr) /= "settings") cycle
       idis = idis + 1
       call prop_get(node_ptr, 'data/XYdiff', real_input, 2)
       x_diff(idis) = real_input(1)
       y_diff(idis) = real_input(2)
       call prop_get(node_ptr, 'data/xyambient', xy_amb(idis,:), 2)
       call prop_get(node_ptr, 'data/discharge/M3S', q_diff(idis))
       call prop_get(node_ptr, 'data/discharge/constituents', s0_diff(idis,:), 3)
       call prop_get(node_ptr, 'data/linkInpDir', linkDir(idis))
    enddo
    
    write(*,*) "Test: ", test
    write(*,'(a,f10.5)') "Version: ", version
    do i=1, no_dis
       write(*,'(a,i0)') "Diffusor: ", i
       write(*,'(a,f10.5,a,f10.5,a)') "(Xdiff,Ydiff): (", x_diff(i), ",", y_diff(i), ")"
       write(*,'(a,f10.5,a,f10.5,a)') "(Xamb ,Yamb ): (", xy_amb(i,1), ",", xy_amb(i,2), ")"
       write(*,'(a,f10.5)') "Q: ", q_diff(i)
       write(*,'(a,3f10.5)') "s0: ", s0_diff(i,:)
       write(*,'(a,a)') "linkdir: ", trim(linkdir(i))
    enddo
    
    luntmp = newunit()
    filename = "COSUMOsettings_output.xml"
    inquire(file=trim(filename),exist=exist)
    if (exist) then
       open (luntmp, file=trim(filename), iostat=istat, status='old')
       close (luntmp, status='delete')
    endif
    open (luntmp,file="COSUMOsettings_output.xml",iostat=istat,status='new')
    call prop_write_xmlfile(luntmp, cosumofile_ptr, 0, istat)
    close (luntmp)
    !
    call tree_destroy(cosumofile_ptr)
    
    
    
    
    
    
    ! Create NF2FF input tree
    !
    filename = "NF2FF_local_001_SubMod002_87960.000_4.xml"
    write(*,*) "Reading XML file '", trim(filename), "'"
    call tree_create( 'COSUMO Input', cosumofile_ptr )
    call tree_put_data( cosumofile_ptr, transfer(trim(filename),node_value), 'STRING:XML' )
    !
    ! Put file in input tree
    !
    call prop_file('xml',trim(filename),cosumofile_ptr,istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          errmsg = "FILE_NOT_FOUND" // trim(filename)
          write(*,*) trim(errmsg)
       case(3)
          errmsg = "PREMATURE_EOF" // trim(filename)
          write(*,*) trim(errmsg)
       case default
          errmsg = "FILE_READ_ERROR" // trim(filename)
          write(*,*) trim(errmsg)
       endselect
       error = .true.
       stop
    endif
    call tree_get_node_by_name( cosumofile_ptr, 'NF2FF', cosumoblock_ptr )
    if (.not.associated(cosumoblock_ptr)) then
       error = .true.
       stop
    endif
    test    = .false.
    version = -999.9
    call prop_get(cosumofile_ptr, 'NF2FF/fileVersion', version)
    write(*,'(a,f10.5)') "Version: ", version
   
    call prop_get(cosumoblock_ptr, 'NFResult/intakes', numlines)
    call prop_get(cosumoblock_ptr, 'NFResult/intakes/1', line)
    numrealonline = count_words(trim(line))
    write(*,*) "Intakes:", numlines, "  x  ", numrealonline
    allocate(intakes(numlines,numrealonline), stat=istat); intakes = -999.9
    do i=1,numlines
       write(key,'(a,i0)') 'NFResult/intakes/', i
       call prop_get(cosumoblock_ptr, trim(key), intakes(i,:), numrealonline)
       write(*,'(i4,a,3(E25.17,1X))') i, ": ", (intakes(i,j),j=1,numrealonline)
    enddo
   
    call prop_get(cosumoblock_ptr, 'NFResult/sinks', numlines)
    call prop_get(cosumoblock_ptr, 'NFResult/sinks/1', line)
    numrealonline = count_words(trim(line))
    write(*,*) "Sinks:", numlines, "  x  ", numrealonline
    allocate(sinks(numlines,numrealonline), stat=istat); intakes = -999.9
    do i=1,numlines
       write(key,'(a,i0)') 'NFResult/sinks/', i
       call prop_get(cosumoblock_ptr, trim(key), sinks(i,:), numrealonline)
       write(*,'(i4,a,3(E25.17,1X))') i, ": ", (sinks(i,j),j=1,numrealonline)
    enddo
   
    call prop_get(cosumoblock_ptr, 'NFResult/sources', numlines)
    call prop_get(cosumoblock_ptr, 'NFResult/sources/1', line)
    numrealonline = count_words(trim(line))
    write(*,*) "Sources:", numlines, "  x  ", numrealonline
    allocate(sources(numlines,numrealonline), stat=istat); intakes = -999.9
    do i=1,numlines
       write(key,'(a,i0)') 'NFResult/sources/', i
       call prop_get(cosumoblock_ptr, trim(key), sources(i,:), numrealonline)
       write(*,'(i4,a,3(E25.17,1X))') i, ": ", (sources(i,j),j=1,numrealonline)
    enddo

    luntmp = newunit()
    filename = "NF2FF_local_001_SubMod002_87960.000_4_output.xml"
    inquire(file=trim(filename),exist=exist)
    if (exist) then
       open (luntmp, file=trim(filename), iostat=istat, status='old')
       close (luntmp, status='delete')
    endif
    open (luntmp,file="NF2FF_local_001_SubMod002_87960.000_4_output.xml",iostat=istat,status='new')
    call prop_write_xmlfile(luntmp, cosumofile_ptr, 0, istat)
    close (luntmp)
    !
    call tree_destroy(cosumofile_ptr)    
    
    
    
    
    
    
    ! Create FF2NF input tree
    !
    filename = "FF2NF_2dis_001_SubMod001_5.000.xml"
    write(*,*) "Reading XML file '", trim(filename), "'"
    call tree_create( 'COSUMO Input', cosumofile_ptr )
    call tree_put_data( cosumofile_ptr, transfer(trim(filename),node_value), 'STRING:XML' )
    !
    ! Put file in input tree
    !
    call prop_file('xml',trim(filename),cosumofile_ptr,istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          errmsg = "FILE_NOT_FOUND" // trim(filename)
          write(*,*) trim(errmsg)
       case(3)
          errmsg = "PREMATURE_EOF" // trim(filename)
          write(*,*) trim(errmsg)
       case default
          errmsg = "FILE_READ_ERROR" // trim(filename)
          write(*,*) trim(errmsg)
       endselect
       error = .true.
       stop
    endif
    call tree_get_node_by_name( cosumofile_ptr, 'COSUMO', cosumoblock_ptr )
    if (.not.associated(cosumoblock_ptr)) then
       error = .true.
       stop
    endif
    test    = .false.
    version = -999.9
    call prop_get(cosumofile_ptr, 'COSUMO/fileVersion', version)
    write(*,'(a,f10.5)') "Version: ", version
   
    luntmp = newunit()
    filename = "FF2NF_2dis_001_SubMod001_5.000_output.xml"
    inquire(file=trim(filename),exist=exist)
    if (exist) then
       open (luntmp, file=trim(filename), iostat=istat, status='old')
       close (luntmp, status='delete')
    endif
    open (luntmp,file="FF2NF_2dis_001_SubMod001_5.000_output.xml",iostat=istat,status='new')
    call prop_write_xmlfile(luntmp, cosumofile_ptr, 0, istat)
    close (luntmp)
    !
    call tree_destroy(cosumofile_ptr)    
end program corinp_gen2
