!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
    subroutine wave_length(  hrm, deph, tp, wavel, wavek, ldep, grav  )
    !!--description-----------------------------------------------------------------
    !
    !
    !     Input:
    !     -------
    !     hrm, deph, tp, grav
    !
    !     Output:
    !     --------
    !     wavel, wavek, ldep
    !     LDEP  : logical variable, .true. when depth or wave height too small
    !
    !     Compute wave lenght (for high enough waves and for deep enough water)
    !
    !!--pseudo code and references--------------------------------------------------
    ! NONE
    !!--declarations----------------------------------------------------------------
    use mathconsts, only: twopi_sp, sqrt2_sp
    implicit none
    !
    ! Global variables
    !
    logical, intent(out)           :: ldep
    real                           :: deph
    real                           :: grav
    real                           :: hrm
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
    if (deph>0.05 .and. hrm>=0.01 .and. tp>0.0) then
        call wavenr(deph, tp, wavek, grav)
        !
        wavel = twopi_sp/wavek
    else
        !
        ! Too shallow water or waves too small
        !
        ldep = .true.
    endif
    end subroutine wave_length