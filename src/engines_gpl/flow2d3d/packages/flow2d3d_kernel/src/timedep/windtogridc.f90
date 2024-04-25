subroutine windtogridc(mmax      ,nmax      ,nmaxus    ,kcs       ,alfas     , &
                     & windsu    ,windsv    ,gdp       )
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
!    Function: Conversion of wind(stresses) from x/y-coordinates
!              to ksi/eta-coordinates
! Method used: -
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
    integer                                                      , intent(in) :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in) :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)             :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)             :: windsv !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer      :: m      ! Loop variable 
    integer      :: n      ! Loop variable 
    real(fp)     :: alfa   ! Current alfas 
    real(fp)     :: cwsu   ! Current windsu 
    real(fp)     :: cwsv   ! Current windsv 
!
!! executable statements -------------------------------------------------------
!
    do m = 1, mmax
       do n = 1, nmaxus
          !
          ! For all "active" points (including open boundaries) the
          ! transformation should be executed (Turbulence model)
          ! The transformation is trivial for (rectilinear) spherical coordinates
          !
          if (kcs(n,m) > 0) then
             cwsu = windsu(n, m)
             cwsv = windsv(n, m)
             alfa = alfas (n, m) * degrad
             !
             ! In the routines in directory REK3D and REKEN the wind
             ! components are used conform the WAQUA convention.
             ! Hence the calculation of windsu/v have to
             ! be multiplied by -1. here (vihtur, cucnp & uzd)
             !
             windsu(n, m) = -(   cwsu*cos(alfa) + cwsv*sin(alfa))
             windsv(n, m) = -( - cwsu*sin(alfa) + cwsv*cos(alfa))
          endif
       enddo
    enddo
end subroutine windtogridc
