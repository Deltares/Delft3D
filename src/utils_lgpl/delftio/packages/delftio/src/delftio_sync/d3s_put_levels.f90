subroutine D3S_put_levels(nst, &
                          mlb, mub, &
                          nlb, nub, &
                          s1 , kfs  )
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
    use d3d_sobek
    !
    implicit none
!
! Global variables
!
    integer                             , intent(in)  :: nst
    integer                             , intent(in)  :: mlb
    integer                             , intent(in)  :: mub
    integer                             , intent(in)  :: nlb
    integer                             , intent(in)  :: nub
    integer , dimension(nlb:nub,mlb:mub), intent(in)  :: kfs
    real(fp), dimension(nlb:nub,mlb:mub), intent(in)  :: s1
!
! Local variables
!
    integer                               :: i
    integer                               :: j
    real(sp), dimension(:,:), allocatable :: s1_sp
    character(256)                        :: errstring 
    logical                               :: success
!
!! executable statements -------------------------------------------------------
!
    allocate(s1_sp(nlb:nub,mlb:mub))
    do j = nlb, nub
       do i = mlb, mub
          s1_sp(j,i) = real(s1(j,i),sp)
       enddo
    enddo
    success = D3S_PutWaterlevels(nst, s1_sp, kfs, mlb, mub, nlb, nub)
    if ( .not. success ) then
        call D3S_LastError(errstring)
        write(*,'(2a)') '*** ERROR D3S_PutWaterlevels: ', trim(errstring)
    endif
    deallocate(s1_sp)
end subroutine D3S_put_levels 
