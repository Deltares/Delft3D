subroutine det_num_dis(no_dis, no_amb_max, gdp)
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
!  $Id: det_num_dis.f90 6185 2016-06-08 15:20:28Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/det_num_dis.f90 $
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
    integer                      , pointer :: lundia
    character(256)               , pointer :: filename
    type(tree_data)              , pointer :: cosumofile_ptr
!
! Global variables
!
    integer                :: no_dis
    integer                :: no_amb_max
!
! Local variables
!
    integer                  :: i
    integer                  :: j
    integer                  :: istat
    integer                  :: luntmp
    integer                  :: no_amb
    integer, external        :: newlun
    type(tree_data), pointer :: cosumoblock_ptr
    type(tree_data), pointer :: data_ptr
    character(300)           :: errmsg
!
!! executable statements -------------------------------------------------------
!
    lundia         => gdp%gdinout%lundia
    filename       => gdp%gdnfl%infile
    !
    ! Initialize
    !
    no_dis = 0
    no_amb_max = 0
    !
    ! Create Cosumo input tree
    !
    write(lundia,'(3a)') "Reading file '", trim(filename), "' ..."
    call tree_create( 'TransportFormula Input', cosumofile_ptr )
    call tree_put_data( cosumofile_ptr, transfer(trim(filename),node_value), 'STRING' )
    !
    ! Put file in input tree
    !
    call prop_file('xml',trim(filename),cosumofile_ptr,istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call prterr(lundia, 'G004', filename)
       case(3)
          call prterr(lundia, 'G006', filename)
       case default
          call prterr(lundia, 'G007', filename)
       endselect
       call d3stop(1, gdp)
    endif
    !
    call tree_get_node_by_name( cosumofile_ptr, 'cosumo', cosumoblock_ptr )
    if (.not.associated(cosumoblock_ptr)) then
       nullify(gdp%gdnfl%cosumofile_ptr)
       return
    endif
    do i=1, size(cosumoblock_ptr%child_nodes)
       if (tree_get_name(cosumoblock_ptr%child_nodes(i)%node_ptr) == "settings") then
          no_dis = no_dis + 1
          no_amb = 0
          nullify(data_ptr)
          call tree_get_node_by_name(cosumoblock_ptr%child_nodes(i)%node_ptr, 'data', data_ptr )
          if (.not. associated(data_ptr)) cycle
          do j=1, size(data_ptr%child_nodes)
             if (tree_get_name(data_ptr%child_nodes(j)%node_ptr) == "xyambient") then
                no_amb = no_amb + 1
             endif
          enddo
          no_amb_max = max(no_amb_max, no_amb)
       endif
    enddo
    !
    ! Do not store the file data(-pointer) in GDP
    ! This will force rereading the Cosumo file at the next Cosumo calculation
    ! 
    !
    nullify(gdp%gdnfl%cosumofile_ptr)
end subroutine det_num_dis
