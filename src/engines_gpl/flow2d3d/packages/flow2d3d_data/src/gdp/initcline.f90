subroutine initcline(gdp       )
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
! NONE
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
    integer                  , pointer :: ifirst
    integer                  , pointer :: ndim
    integer                  , pointer :: md
    integer                  , pointer :: loop
    integer,  dimension(:)   , pointer :: inc
    real(fp), dimension(:,:) , pointer :: ud
    real(fp), dimension(:,:) , pointer :: xd
    real(fp), dimension(:)   , pointer :: rdep
!
!! executable statements -------------------------------------------------------
!
    ifirst  => gdp%gdcline%ifirst
    ndim    => gdp%gdcline%ndim
    md      => gdp%gdcline%md
    loop    => gdp%gdcline%loop
    inc     => gdp%gdcline%inc
    ud      => gdp%gdcline%ud
    xd      => gdp%gdcline%xd
    rdep    => gdp%gdcline%rdep
    !
    ifirst =  1
    ndim   =  2
    md     =  3
    loop   = 60
    !
    nullify(gdp%gdcline %inc)
    nullify(gdp%gdcline %ud)
    nullify(gdp%gdcline %xd)
    nullify(gdp%gdcline %rdep)
end subroutine initcline
