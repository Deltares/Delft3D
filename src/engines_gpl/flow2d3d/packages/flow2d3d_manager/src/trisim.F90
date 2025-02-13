module get_cloc_esm
   use precision
   real(fp), dimension(:,:), pointer    :: esm_array_pointer

contains

subroutine get_esm_pointer2d(dim1, dim2, array)
   implicit none
   integer                               , intent(in)    :: dim1
   integer                               , intent(in)    :: dim2
   real(fp), dimension(dim1,dim2), target, intent(in)    :: array

   esm_array_pointer => array
end subroutine get_esm_pointer2d
    

subroutine get_cloc_esm2d(index1, index2, cloc)
   use iso_c_binding, only: c_ptr, c_loc
   implicit none
   !
   ! parameters
   integer                               , intent(in)    :: index1
   integer                               , intent(in)    :: index2
   type(c_ptr)                           , intent(inout) :: cloc           !< Pointer (by reference) to requested value data, NULL if not available.
   !
   ! body
   cloc = c_loc(esm_array_pointer(index1,index2))
end subroutine get_cloc_esm2d

end module get_cloc_esm
!
!
!
!===============================================================================
subroutine trisim (numdom, nummap, context_id, fsm_flags, runid, &
                 & initonly, gdpC)
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
!!--description-----------------------------------------------------------------
!
!    Function: Main routine for the 2d / 3d program
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use mod_trisim
    use precision
    use dfparall
    use d3d_olv_class
    use iso_c_binding, only: c_loc

    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following line
    ! See also statements below
    !
    ! use ifcore
    !
    ! global data declaration; compare with include 'globdat.igd'
    !
    use globaldata
    implicit none
    type(globDat),pointer :: gdp
!
! Parameters
!
    integer       , intent(in)  :: context_id
    integer       , intent(in)  :: fsm_flags
    integer       , intent(in)  :: initonly      ! 0: full execution, 1: initonly
    integer       , intent(in)  :: numdom        ! Number of subdomains (0 means single domain) as detected by hydra
    integer       , intent(in)  :: nummap        ! Number of mappers (one for each DD boundaries connected with this subdomain) as detected by hydra
    character(*)                :: runid
    type(c_ptr)   , intent(out) :: gdpC
!
! Local variables
!
    integer         :: ierr
    integer         :: retval
    type(OLVHandle) :: olv_handle
    !
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following line
    ! See also statements below
    !
    ! INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
!
!! executable statements -------------------------------------------------------

    ! Initialize MPI
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following two lines
    ! See also use statement above
    !
    ! NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
    ! OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
    !
    ! create and initialize GDP structure
    !
    allocate(gdp)
    gdpC = c_loc(gdp)
    !
    ! gdp%runid must be nullified before calling trisim_init
    ! When trisim_init is called via OpenDA/OpenMI, gdp%runid is set before the call
    !
    nullify(gdp%runid)
    !
    retval = trisim_init(numdom, nummap, context_id, fsm_flags, runid, olv_handle, gdp)
    if (retval /= 0) then
       return
    endif
    !
    if (initonly == 1) then
       retval = trisim_initialise_single_step(gdp)
       return
    endif
    !
    retval = trisim_step(olv_handle, gdp)
    if (retval /= 0) then
       return
    endif
    !
    retval = trisim_finish(olv_handle, gdp)
    if (retval /= 0) then
       return
    endif
    !
    !call gdp_dealloc(gdp)
    !deallocate(gdp, stat=ierr)
    !
    ! Finish using a semaphore
    ! Related psemnefis is in tricom.f90
    !
    call vsemfinish
    !
    ! Finalizes MPI
    !
    if (usempi) then
       call dfexitmpi(0)
    endif
end subroutine trisim
!
!
!
!===============================================================================
integer function trisim_update(dt, gdp)
    use precision
    use globaldata
    use d3d_olv_class
    use mod_trisim
    use iso_c_binding, only: c_double
    !
    implicit none
    !
    ! parameters
    real(c_double), value, intent(in) :: dt
    type(globDat) , pointer           :: gdp
    !
    ! locals
    integer         :: ierr
    real(fp)        :: tend
    type(OLVHandle) :: olv_handle
    !
    ! body
    ierr   = 0
    tend   = gdp%gdinttim%timsec + real(dt,fp)
    do while (gdp%gdinttim%timsec < tend)
       ierr = trisim_step(olv_handle, gdp)
       if (ierr /= 0) exit
       ierr = trisim_prepare_next_step(gdp)
       if (ierr /= 0) exit
       ierr = trisim_check_step(gdp)
       if (ierr /= 0) exit
    enddo
    trisim_update = ierr
end function trisim_update
!
!
!
!===============================================================================
integer function trisim_finalize(gdp)
    use precision
    use globaldata
    use d3d_olv_class
    use mod_trisim
    use dfparall
    !
    implicit none
    !
    ! parameters
    type(globDat) , pointer           :: gdp
    !
    ! locals
    integer         :: ierr
    integer         :: retval
    type(OLVHandle) :: olv_handle
    !
    ! body
    retval = 0
    retval = trisim_finish(olv_handle, gdp)
    if (retval /= 0) then
       trisim_finalize = retval
       return
    endif
    !
    !call gdp_dealloc(gdp)
    !deallocate(gdp, stat=ierr)
    !
    ! Finish using a semaphore
    ! Related psemnefis is in tricom.f90
    !
    call vsemfinish
    !
    ! Finalizes MPI
    !
    if (usempi) then
       call dfexitmpi(0)
    endif
    trisim_finalize = retval
end function trisim_finalize
!
!
!
!===============================================================================
subroutine trisim_get_start_time(t, gdp)
    use precision
    use globaldata
    use iso_c_binding, only: c_double
    !
    implicit none
    !
    ! parameters
    real(c_double), intent(out) :: t
    type(globDat) , pointer     :: gdp
    !
    ! body
    !call c_f_pointer(gdpC, gdp, [1])
    !
    ! In seconds:
    t = real(gdp%gdexttim%tstart*60.0_fp, hp)
end subroutine trisim_get_start_time
!
!
!
!===============================================================================
subroutine trisim_get_end_time(t, gdp)
    use precision
    use globaldata
    use iso_c_binding, only: c_double
    !
    implicit none
    !
    ! parameters
    real(c_double), intent(out) :: t
    type(globDat) , pointer     :: gdp
    !
    ! body
    ! In seconds:
    t = real(gdp%gdexttim%tstop*60.0_fp, hp)
end subroutine trisim_get_end_time
!
!
!
!===============================================================================
subroutine trisim_get_time_step(t, gdp)
    use precision
    use globaldata
    use iso_c_binding, only: c_double
    !
    implicit none
    !
    ! parameters
    real(c_double), intent(out) :: t
    type(globDat) , pointer     :: gdp
    !
    ! body
    ! In seconds:
    t = real(gdp%gdexttim%dt*gdp%gdexttim%tunit, hp)
end subroutine trisim_get_time_step
!
!
!
!===============================================================================
subroutine trisim_get_current_time(t, gdp)
    use precision
    use globaldata
    use iso_c_binding, only: c_double
    !
    implicit none
    !
    ! parameters
    real(c_double), intent(out) :: t
    type(globDat),pointer :: gdp
    !
    ! body
    !call c_f_pointer(gdpC, gdp, [1])

    ! In seconds:
    t = real(gdp%gdinttim%timsec, hp)
end subroutine trisim_get_current_time
!
!
!
!===============================================================================
subroutine trisim_get_var(c_var_name, c_var_ptr, gdp)
    use precision
    use globaldata
    use iso_c_binding, only: c_double, c_char, c_loc
    use iso_c_utils
    use string_module
    use get_cloc_esm
    use dfparall, only: engine_comm_world
    !
    implicit none
    !
    ! parameters
    character(kind=c_char), intent(in)    :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field"
    type(c_ptr)           , intent(inout) :: c_var_ptr
    type(globDat),pointer                 :: gdp
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer(pntrsize)             , pointer :: cbuvrt
    integer(pntrsize)             , pointer :: nambar
    integer(pntrsize)             , pointer :: namsrc
    integer(pntrsize)             , pointer :: qsrcrt
    !
    ! locals
    integer                           :: dim1
    integer                           :: dim2
    integer                           :: namlen
    integer                           :: namdim
    integer                           :: item_index
    character(len=strlen(c_var_name)) :: var_name
    character(len=strlen(c_var_name)) :: tmp_var_name
    character(len=strlen(c_var_name)) :: set_name     !< For parsing compound variable names.
    character(len=strlen(c_var_name)) :: item_name    !< For parsing compound variable names.
    character(len=strlen(c_var_name)) :: field_name   !< For parsing compound variable names.
    !
    ! body
    !
    var_name = char_array_to_string(c_var_name)
    ! Try to parse variable name as slash-separated id (e.g., 'weirs/Lith/crest_level')
    tmp_var_name = var_name
    call str_lower(tmp_var_name)
    call str_token(tmp_var_name, set_name, DELIMS='/')
    call str_token(tmp_var_name, item_name, DELIMS='/')
    call str_token(tmp_var_name, field_name, DELIMS='/')
    !
    c_var_ptr = c_null_ptr
    select case(set_name)
    case ("engine_comm_world")
       c_var_ptr = c_loc(engine_comm_world)
    case ("filrtc")
       namlen = 20
       namdim = gdp%gdrtc%stacnt
       call getStructureIndex(namlen, namdim, gdp%gdrtc%namrtcsta, item_name, item_index)
       if (item_index == 0) then
           return
       endif
       select case(field_name)
       case("water_level")
           c_var_ptr = c_loc(gdp%gdrtc%s1rtcsta(item_index))
           return
       end select
    case ("filbar")
       cbuvrt => gdp%gdr_i_ch%cbuvrt
       nambar => gdp%gdr_i_ch%nambar
       !
       namlen = 20
       namdim = gdp%d%nsluv
       call getStructureIndex(namlen, namdim, ch(nambar), item_name, item_index)
       if (item_index == 0) then
           return
       endif
       select case(field_name)
       case("gate_level")
           dim1 = 2
           dim2 = gdp%d%nsluv
           call get_esm_pointer2d(dim1, dim2, r(cbuvrt))
           call get_cloc_esm2d(2, item_index, c_var_ptr)
           return
       end select
    case ("filsrc")
       namsrc => gdp%gdr_i_ch%namsrc
       qsrcrt => gdp%gdr_i_ch%qsrcrt
       !
       namlen = 20
       namdim = gdp%d%nsrc
       call getStructureIndex(namlen, namdim, ch(namsrc), item_name, item_index)
       if (item_index == 0) then
           return
       endif
       select case(field_name)
       case("discharge")
           dim1 = 2
           dim2 = gdp%d%nsrc
           call get_esm_pointer2d(dim1, dim2, r(qsrcrt))
           call get_cloc_esm2d(2, item_index, c_var_ptr)
           return
       end select
    case default
       return
    end select
end subroutine trisim_get_var
!
!
!
!===============================================================================
subroutine getStructureIndex(namlen, namdim, namarray, item_name, item_index)
    use string_module
   implicit none
   !
   ! parameters
   integer                             , intent(in)  :: namlen
   integer                             , intent(in)  :: namdim
   character(namlen), dimension(namdim), intent(in)  :: namarray
   character(*)                        , intent(in)  :: item_name
   integer                             , intent(out) :: item_index
   !
   ! locals
   integer           :: i
   character(namlen) :: item_name_lower
   character(namlen) :: elt_name_lower
   !
   ! body
   item_index = 0
   item_name_lower = item_name
   call str_lower(item_name_lower)
   do i = 1, namdim
      elt_name_lower = namarray(i)
      call str_lower(elt_name_lower)
      if (elt_name_lower == item_name_lower) then
         item_index = i
         exit
      endif
   enddo
end subroutine getStructureIndex

