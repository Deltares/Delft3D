subroutine calbed(rhow      ,mmax      ,nmaxus    ,kmax      , &
                & bedstr    ,taubpu    ,taubsu    ,u1        ,gdp       )
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
!    Function: Calculate bedstresses
! Method used:
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
!
! Global variables
!
    integer                                                              , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                              , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                              , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                             , intent(in)  :: rhow   !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(out) :: bedstr !!  Work array containing bedstresses
                                                                                                 !!  U-velocities at new time level
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: taubpu !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: taubsu !  Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer         :: m       ! Loop counter MMAX 
    integer         :: n       ! Loop counter NMAXUS 
!
!
!! executable statements -------------------------------------------------------
!
!
    !
    !-----calculate DPS depth in zeta point as the mean of 4 surrounding
    !
    do n = 1, nmaxus
       do m = 1, mmax
          bedstr(n, m) = (taubpu(n, m)*u1(n, m, kmax) + taubsu(n, m))*rhow
       enddo
    enddo
end subroutine calbed
