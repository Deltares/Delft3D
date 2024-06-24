subroutine upbdps(mmax      ,nmax      ,kcs       ,&
                & nmaxus    ,dpd       ,dps       ,gdp       )
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
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    
    implicit none

    type(globdat),target :: gdp
!
! Local parameters
!
    integer       :: ddb
    integer       :: m, n
!
! Global variables
!
    integer                     :: ierr   !  Flag for error when writing to Communication file 
    integer, intent(in)         :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)         :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)         :: nmaxus !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   :: dpd !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) :: dps !  Description and declaration in esm_alloc_real.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcs !  Description and declaration in esm_alloc_int.f90
!
!
! Local variables
!
!
!
!! executable statements -------------------------------------------------------
!
    ddb = gdp%d%ddbound
    do n = 1 - ddb, nmaxus
       do m = 1 - ddb, mmax
          dps(n, mmax) = real(dps(n, mmax-1),fp)
          dpd(n, mmax) = real(dpd(n, mmax-1),fp)
       enddo
    enddo
end subroutine upbdps
