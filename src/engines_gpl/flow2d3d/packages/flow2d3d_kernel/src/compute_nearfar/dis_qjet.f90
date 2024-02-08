subroutine det_qjet(q, v, b)
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
!    Function: Numerical integration to determine jet discharge
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use precision
    use mathconsts
!
! Global variables
!
    real(fp)   ::   b
    real(fp)   ::   q
    real(fp)   ::   v
!
! Local variables
!
    integer    ::   ib
    integer    ::   iphi
    real(fp)   ::   bstep
    real(fp)   ::   pstep
    real(fp)   ::   uuu
!
!! executable statements -------------------------------------------------------
!
    q  = 0.0_fp
    !
    pstep = pi / 100.0_fp
    bstep = b  / 100.0_fp
    !
    do iphi = 1, 100
       do ib = -100, 99
         uuu = v * exp(-1.0_fp*((real(ib,fp) + 0.5_fp)*bstep/b)**2)
         q   = q + uuu*bstep*pstep*abs((real(ib,fp)+0.5_fp)*bstep)
       enddo
    enddo
end subroutine det_qjet
