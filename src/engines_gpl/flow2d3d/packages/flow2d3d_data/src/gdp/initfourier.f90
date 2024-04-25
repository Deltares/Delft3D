subroutine initfourier(gdp)
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
! NONE
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
!! executable statements -------------------------------------------------------
!
    !
    ! Initialize Fourier parameters
    !
    gdp%gdfourier%fouwrt   = -1
    gdp%gdfourier%iblwl    =  0
    gdp%gdfourier%ibleh    =  0
    gdp%gdfourier%iblcn    =  0
    gdp%gdfourier%ibluv    =  0
    gdp%gdfourier%iblqf    =  0
    gdp%gdfourier%iblbs    =  0
    gdp%gdfourier%iblep    =  0
    !
    ! Nullify arrays. 
    ! Allocation is done in gdp_alloc_arrays.f90 and initialisation in initarrays.f90
    !
    nullify(gdp%gdfourier%fconno )
    nullify(gdp%gdfourier%flayno )
    nullify(gdp%gdfourier%fnumcy )
    nullify(gdp%gdfourier%ftmsto )
    nullify(gdp%gdfourier%ftmstr )
    nullify(gdp%gdfourier%ifoupt )
    nullify(gdp%gdfourier%iofset )
    nullify(gdp%gdfourier%fknfac )
    nullify(gdp%gdfourier%foucomp)
    nullify(gdp%gdfourier%foufas )
    nullify(gdp%gdfourier%fousma )
    nullify(gdp%gdfourier%fousmb )
    nullify(gdp%gdfourier%fouvec )
    nullify(gdp%gdfourier%fv0pu  )
    nullify(gdp%gdfourier%fouelp )
    nullify(gdp%gdfourier%founam )
    nullify(gdp%gdfourier%foutyp )
    !
end subroutine initfourier
