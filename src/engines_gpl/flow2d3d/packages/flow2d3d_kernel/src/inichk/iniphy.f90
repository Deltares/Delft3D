subroutine iniphy(iro       ,z0        ,vonkar    ,vicmol    ,sboltz    , &
                & sferic    ,anglat    ,grdang    ,temeqs    ,gdp       )
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
!    Function: - Define some physical constants
!              - SFERIC = T re-define ANGLAT and GRDANG (= 0.)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer , intent(out) :: iro    !  Description and declaration in physco.igs
    logical , intent(in)  :: sferic !  Description and declaration in tricom.igs
    real(fp)              :: anglat !!  - Angle of latitude of the model
                                    !!    centre (used to determine the coef.
                                    !!    for the coriolis force)
                                    !!  - In spherical coordinates this para-
                                    !!    meter equals the angle of latitude
                                    !!    for the origin (water level point)
                                    !!    after INIPHY anglat = 0.
    real(fp)              :: grdang !  Description and declaration in tricom.igs
    real(fp), intent(out) :: sboltz !  Description and declaration in physco.igs
    real(fp), intent(in)  :: temeqs !  Description and declaration in physco.igs
    real(fp), intent(out) :: vicmol !  Description and declaration in physco.igs
    real(fp), intent(out) :: vonkar !  Description and declaration in physco.igs
    real(fp), intent(out) :: z0     !  Description and declaration in physco.igs
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    ! initialize global parameters
    !
    iro    = 1
    z0     = 1.0e-6
    vonkar = 0.41
    !
    ! vicmol computed according to Van Rijn (2004) sediment tranport
    !
    vicmol = (4.0e-5)/(20.0+temeqs)
    sboltz = 5.6697e-8
    !
    ! Re-define ANGLAT and GRDANG for SFERIC = .true.,
    ! Only in this case they are used for origin-coordinates
    !
    if (sferic) then
       anglat = 0.0
       grdang = 0.0
    else
       !
       ! This test/warning was originally in subroutine rdxyzo
       ! There, the warning was also generated for sferic = true
       !
       if (comparereal(anglat , 0.0_fp) == 0) then
          call prterr(gdp%gdinout%lundia, 'U136', ' ')
       endif
    endif
end subroutine iniphy
