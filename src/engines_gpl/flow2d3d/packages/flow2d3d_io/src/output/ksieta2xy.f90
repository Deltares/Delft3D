subroutine ksieta2xy(mmax    ,nmaxus    ,kcs      , &
                   & alfas   ,comp1     ,comp2    ,gdp      )
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
!    Function: - Rotate the vector components from (ksi-eta)-coordinates 
!                to (x,y)-coordinates
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
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
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kcs    !! Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in) :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in) :: nmaxus !  Description and declaration in esm_alloc_int.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     :: comp1  !  First component of vector (u, qxk or taubpu)
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     :: comp2  !  Second component of vector (v, qyk or taubpv)
!
! Local variables
!
    integer         :: m       ! Loop counter over MMAX 
    integer         :: n       ! Loop counter over NMAXUS
    real(fp)        :: alfa    ! Value of ALFAS(N,M) in radials 
    real(fp)        :: uzeta   ! U component in zeta point (u1, qxk, taubpu)
    real(fp)        :: vzeta   ! V component in zeta point (v1, qyk, taubpv)
!
!! executable statements -------------------------------------------------------
!
    !
    ! Rotate the vector components from (ksi-eta)-coordinates to (x,y)-coordinates
    !
    do n = 1, nmaxus
       do m = 1, mmax
          if (kcs(n, m)==1) then
             alfa = alfas(n, m)*degrad
             uzeta = comp1(n, m)
             vzeta = comp2(n, m)
             comp1(n, m) = uzeta*cos(alfa) - vzeta*sin(alfa)
             comp2(n, m) = uzeta*sin(alfa) + vzeta*cos(alfa)
          endif
       enddo
    enddo
    !
end subroutine ksieta2xy
