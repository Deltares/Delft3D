subroutine fxfydr(dirc      ,dirs      ,dir       ,n         ,pi        )
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
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer           , intent(in)  :: n
    real              , intent(in)  :: pi
    real, dimension(*), intent(out) :: dir
    real, dimension(*), intent(in)  :: dirc
    real, dimension(*), intent(in)  :: dirs
!
! Local variables
!
    integer :: i
    real    :: hulp
!
!! executable statements -------------------------------------------------------
!
    do i = 1, n
       if (abs(dirc(i))>=1.0E-6 .or. abs(dirs(i))>=1.0E-6) then
          hulp = atan2(dirs(i), dirc(i))*180./pi
          if (hulp<0.) hulp = hulp + 360.
          dir(i) = hulp
       else
          dir(i) = 0.
       endif
    enddo
end subroutine fxfydr
