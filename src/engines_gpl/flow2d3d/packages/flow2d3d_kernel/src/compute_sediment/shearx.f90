subroutine shearx(tau       ,nm        ,gdp       )
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
! Adds extra bottom stress according to user input
! At initialization :
! Calls Input() to load user input file.
! Check flag and returns if OFF.
! Get points and NM-indices from Input()
! Allocates local memory
! Continues processing :
! If no point given,
! copies taubmx(*)
! Else
! Checks if point can be found in list.
! If so, calculates and returns extra stress
!
! If compiled with option /d_lines the routine will produce
! a file = shearX.rpt, with debug information.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: nmmax
    integer                , pointer :: nof
    integer, dimension(:)  , pointer :: nmapp
    integer, dimension(:)  , pointer :: nmref
    real(fp), dimension(:) , pointer :: factor
    real(fp), dimension(:) , pointer :: tauv
!
! Global variables
!
    integer                 :: nm
    real(fp), dimension(*)  :: tau    ! generic 2-way REAL data port
                                      ! at first    CALL   copies  taubmx(*)
                                      ! at all next CALL's returns tauv( nm)
!
! Local variables
!
    integer :: istat
    integer :: nml
    integer :: np
!
!! executable statements -------------------------------------------------------
!
    nmmax      => gdp%d%nmmax
    nof        => gdp%gdscour%nof
    nmapp      => gdp%gdscour%nmapp
    nmref      => gdp%gdscour%nmref
    factor     => gdp%gdscour%factor
    tauv       => gdp%gdscour%tauv
    !
    if (nm>0) then
       tau(1) = 0.0
       do np = 1, nof
          if (nmapp(np)==nm) then
             !
             ! apply extra bottom stress using reference index
             ! and multiplying factor
             !
             tau(1) = tauv(nmref(np))*factor(np)
          endif
       enddo
    else
       do nml = 1, nmmax
          tauv(nml) = tau(nml)
       enddo
    endif
end subroutine shearx
