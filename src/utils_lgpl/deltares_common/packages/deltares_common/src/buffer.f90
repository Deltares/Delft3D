module buffer
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision
implicit none
!
! module parameters
!
integer                          , save :: bufflen1 ! Length of 1st dimension of allocated arrays sbuff and hbuff
integer                          , save :: bufflen2 ! Length of 2nd dimension of allocated arrays sbuff and hbuff
real(sp), dimension(:)  , pointer, save :: sbuff_1d ! 1D single precision buffer to read from
                                                    ! or to write to, depending on the write
                                                    ! switch (WRILOG).
real(sp), dimension(:,:), pointer, save :: sbuff_2d ! 2D single precision buffer to read from
                                                    ! or to write to, depending on the write
                                                    ! switch (WRILOG).
real(hp), dimension(:)  , pointer, save :: hbuff_1d ! 1D double precision buffer to read from
                                                    ! or to write to, depending on the write
                                                    ! switch (WRILOG).
real(hp), dimension(:,:), pointer, save :: hbuff_2d ! 2D double precision buffer to read from
                                                    ! or to write to, depending on the write
                                                    ! switch (WRILOG).
!
! interfaces
!
   interface get_buffer
      module procedure get_buffer_1d_sp
      module procedure get_buffer_2d_sp
      module procedure get_buffer_1d_hp
      module procedure get_buffer_2d_hp
   end interface
contains

subroutine init_buffer()
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    bufflen1 = 0
    bufflen2 = 0
    deallocate (sbuff_1d, STAT = istat)
    deallocate (sbuff_2d, STAT = istat)
    deallocate (hbuff_1d, STAT = istat)
    deallocate (hbuff_2d, STAT = istat)
end subroutine init_buffer

!
! Subroutine to get buffer for 1-dimensional single precision array
!
subroutine get_buffer_1d_sp(buff, lenused1)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                         :: lenused1
    real(sp), dimension(:), pointer :: buff
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(sbuff_1d)) then
       deallocate (sbuff_1d, stat = istat)
    endif
    bufflen1 = lenused1
    allocate(sbuff_1d(bufflen1))
    sbuff_1d = 0.0_sp
    buff     => sbuff_1d
end subroutine get_buffer_1d_sp

!
! Subroutine to get buffer for 2-dimensional single precision array
!
subroutine get_buffer_2d_sp(buff, lenused1, lenused2)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                           :: lenused1
    integer                           :: lenused2
    real(sp), dimension(:,:), pointer :: buff
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(sbuff_2d)) then
       deallocate (sbuff_2d, stat = istat)
    endif
    bufflen1 = lenused1
    bufflen2 = lenused2
    allocate(sbuff_2d(bufflen1, bufflen2))
    sbuff_2d = 0.0_sp
    buff     => sbuff_2d
end subroutine get_buffer_2d_sp

!
! Subroutine to get buffer for 1-dimensional double precision array
!
subroutine get_buffer_1d_hp(buff, lenused1)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                         :: lenused1
    real(hp), dimension(:), pointer :: buff
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(hbuff_1d)) then
       deallocate (hbuff_1d, stat = istat)
    endif
    bufflen1 = lenused1
    allocate(hbuff_1d(bufflen1))
    hbuff_1d = 0.0_hp
    buff     => hbuff_1d
end subroutine get_buffer_1d_hp

!
! Subroutine to get buffer for 2-dimensional double precision array
!
subroutine get_buffer_2d_hp(buff, lenused1, lenused2)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                           :: lenused1
    integer                           :: lenused2
    real(hp), dimension(:,:), pointer :: buff
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(hbuff_2d)) then
       deallocate (hbuff_2d, stat = istat)
    endif
    bufflen1 = lenused1
    bufflen2 = lenused2
    allocate(hbuff_2d(bufflen1, bufflen2))
    hbuff_2d = 0.0_hp
    buff     => hbuff_2d
end subroutine get_buffer_2d_hp


subroutine dealloc_buffer()
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    !
    ! The check on associated and setting it on zero is needed
    ! Otherwise a crash will occur in multi threaded mode, because
    ! sbuff or hbuff is then deallocated more than once
    !
    if (associated(sbuff_1d)) then
       deallocate (sbuff_1d, stat = istat)
    endif
    if (associated(sbuff_2d)) then
       deallocate (sbuff_2d, stat = istat)
    endif
    if (associated(hbuff_1d)) then
       deallocate (hbuff_1d, stat = istat)
    endif
    if (associated(hbuff_2d)) then
       deallocate (hbuff_2d, stat = istat)
    endif
    !
    bufflen1  = 0
    bufflen2  = 0
end subroutine dealloc_buffer

end module buffer
