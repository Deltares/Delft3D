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
module d3d_olv_class

use precision

implicit none

type, public ::  olv_handle_t
    integer :: runningFlag =  0  ! 0=Flow not running (waiting); 1=Flow is iterating time steps
    integer :: currentStep =  0
    integer :: endTimeStep =  0
    integer :: endFlag     =  0  ! 0 = Flow simulation has not finished yet; 1 = Flow has finished
    integer :: timeStepInt = -1
end type

type, public :: OLVHandle
    type(olv_handle_t), pointer :: fields => null()
end type

private

public new_olv
public free_olv
public isNull_olv

contains
!
!------------------------------------------------------------------------------
subroutine new_olv(handle)
    type(OLVHandle) :: handle
    !
    ! locals
    type(olv_handle_t), pointer :: olv
    !
    ! body
    allocate(olv)
    handle%fields => olv
end subroutine new_olv
!
!------------------------------------------------------------------------------
subroutine free_olv(handle)
    type(OLVHandle) :: handle
    !
    ! locals
    integer                     :: istat
    !
    ! body
    if (isNull_olv(handle)) return
    !
    ! Deallocate the olv structure
    ! Catch stat to avoid aborts; don't bother whether it succeeded or not
    ! 
    deallocate(handle%fields,stat=istat)
    nullify(handle%fields)
end subroutine
!
!------------------------------------------------------------------------------
logical function isNull_olv(handle) result(res)
    type(OLVHandle) :: handle

    res = .NOT. associated(handle%fields)
end function
!
!------------------------------------------------------------------------------
end module d3d_olv_class
