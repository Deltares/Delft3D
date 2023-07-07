!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
module m_waq_multi_instance

implicit none

contains


!> @file
!!    SE-functions for instance handling
!!
!!    Note:
!!    Not all routines can be in a module, they have to
!!    be callable from outside Fortran.
!<


function Create_Instance() result(instance_id)
    !DEC$ ATTRIBUTES DLLEXPORT :: Create_Instance

    use m_delwaq_2_openda

    implicit none
    !
    ! result
    integer :: instance_id  ! instance identifier (instanceId >= 0 : success)
    !

    instance_id = dlwq_create_instance()


end function Create_Instance
!
!
!
!==============================================================================

subroutine Select_Instance(instance_id)
    !DEC$ ATTRIBUTES DLLEXPORT :: Select_Instance

    use m_delwaq_2_openda

    implicit none
    !
    ! arguments
    integer, intent(in) :: instance_id  ! instance identifier

    call dlwq_select_new_instance(instance_id)

end subroutine Select_Instance

!======================================================================
!
subroutine Set_Max_Instances_In_Memory(max_instances)
    !DEC$ ATTRIBUTES DLLEXPORT::Set_Max_Instances_in_Memory
    !DEC$ ATTRIBUTES ALIAS : '_SET_MAX_INSTANCES_IN_MEMORY' :: Set_Max_Instances_in_Memory

    use m_delwaq_2_openda

    implicit none
    !
    ! result
    integer :: max_instances  ! max #instances to be kept in memory
    !
    max_instances_in_memory = max_instances

end subroutine Set_Max_Instances_In_Memory
!
!
!======================================================================
function Get_Instance_Core_State(corestate, size_corestate) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: Get_Instance_Core_State

    use m_delwaq_2_openda

    implicit none


    ! result
    integer :: retVal, size_corestate

    double precision, dimension(size_corestate)   :: corestate
    !

    call dlwq_getcorestate(corestate,size_corestate,retval)


end function Get_Instance_Core_State




!======================================================================
function Set_Instance_Core_State(corestate,size_corestate) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: Set_Instance_Core_State

    use m_delwaq_2_openda

    implicit none


    ! result
    integer :: retVal,size_corestate

    double precision, dimension(size_corestate), intent(in)  :: corestate
    !

    call dlwq_setcorestate(corestate,size_corestate,retVal)


end function  Set_Instance_Core_State

!==========================================================

function Get_Instance_Size() result (inst_size)
    !DEC$ ATTRIBUTES DLLEXPORT :: Get_Instance_Size

    use m_delwaq_2_openda

    implicit none


    ! result
    integer :: inst_size



    call dlwq_getinstancesize(inst_size)



end function Get_Instance_Size


!==============================================================================

subroutine GetCurrentTime(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: GetCurrentTime

    use delwaq2_global_data

    implicit none

    ! result
    double precision :: retVal   ! Current Model time

    retVal = dlwqd%otime + dlwqd%itime / dlwqd%tscale

end subroutine GetCurrentTime


!==============================================================================

subroutine GetNextTime(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: GetNextTime

    use delwaq2_global_data

    implicit none
    include 'sysi_ff.inc'

    ! result
    double precision :: retVal   ! Current Model time

    retVal = dlwqd%otime + (dlwqd%itime + idt) / dlwqd%tscale

end subroutine GetNextTime

!==============================================================================

function Store_Current_Instance(storage_level) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: Store_Current_Instance

    use m_delwaq_2_openda

    implicit none

    ! argument
    integer :: storage_level

    ! result
    integer :: retVal
    !

    call dlwq_store_current_instance(storage_level)
    retVal = 0

end function Store_Current_Instance

!======================================================================
subroutine Export_Current_Instance(doappend)
    !DEC$ ATTRIBUTES DLLEXPORT :: Export_Current_Instance
    !DEC$ ATTRIBUTES ALIAS : '_EXPORT_CURRENT_INSTANCE' :: Export_Current_Instance
   use m_delwaq_2_openda


   integer :: ierr
   logical :: doappend

   ! to do: export in append mode

   call dlwq_ctastate_to_netcdf(ierr)


end subroutine Export_Current_Instance
!========================================
end module m_waq_multi_instance
