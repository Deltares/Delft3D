subroutine corrht(hrm       ,deph      ,tp        ,wavel     ,wavek     , &
                & ldep      ,dish      ,dismax    ,choice    ,rho       , &
                & grav      )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!
!     Input:
!     -------
!     HSH,DEPH,TP
!     I/O  I    I/O
!
!     Output:
!     --------
!     WAVEL,WAVEK,LDEP
!     LDEP  : logical variable, .true. when depth or wave height too small
!
!     Adapt wave period TP and wave height HSH
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use mathconsts, only: twopi_sp, sqrt2_sp
    implicit none
!
! Global variables
!
    logical, intent(in)            :: choice
    logical, intent(out)           :: ldep
    real                           :: deph
    real                           :: dish
    real                           :: dismax
    real                           :: grav
    real                           :: hrm
    real   , intent(in)            :: rho
    real                           :: tp
    real                           :: wavek
    real   , intent(out)           :: wavel
!
! Local variables
!
    real :: hmax
    real :: hs
    real :: tpmin
!
!! executable statements -------------------------------------------------------
!
    ldep   = .false.
    dismax = 0.0
    if (deph>0.05 .and. hrm>=0.01 .and. tp>0.0) then
       call wavenr_htk(deph, tp, wavek)
       !
       wavel = twopi_sp/wavek
    else
       !
       ! Too shallow water or waves too small
       !
       ldep = .true.
    endif
end subroutine corrht
