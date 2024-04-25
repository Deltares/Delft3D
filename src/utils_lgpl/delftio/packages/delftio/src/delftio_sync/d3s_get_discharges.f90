subroutine d3s_get_discharges( nst, nto, kcd, hydrbc )
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
    use d3d_sobek
    !
    implicit none
!
! Global variables
!
    integer                         , intent(in)  :: nst
    integer                         , intent(in)  :: nto
    integer                         , intent(in)  :: kcd
    real(fp), dimension(4, nto, kcd), intent(out) :: hydrbc !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    real   , dimension(nto) :: discharges
    logical, dimension(nto) :: mask
    logical                 :: success
    character(256)          :: errstring
!
!! executable statements -------------------------------------------------------
!
    success = D3S_GetAllDischarges(nst, discharges, mask )
    if ( .not. success ) then
        call D3S_LastError(errstring)
        write(*,'(2a)') '*** ERROR D3S_GetAllDischarges: ', trim(errstring)
    else
        where ( mask ) hydrbc(1,:,1) = discharges
        where ( mask ) hydrbc(2,:,1) = discharges
    endif
end subroutine d3s_get_discharges
