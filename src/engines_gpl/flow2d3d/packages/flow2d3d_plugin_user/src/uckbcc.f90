subroutine uckbcc(ltur      ,kmax      ,nto       ,nrob      ,nob       , &
                & ubnd      )
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
!    Function: - Test values of user defined boundary conditions
!                towards direction definition of open boundaries
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: ltur !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nrob !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nto !  Description and declaration in esm_alloc_int.f90
    integer, dimension(8, nrob), intent(in) :: nob !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(2, ltur, 0:kmax, 2, nto), intent(out) :: ubnd !  Description and declaration in trisol.igs
!
!
! Local variables
!
    integer                        :: into                 ! Initial boundary number 
    integer                        :: k                    ! Loop variable 
    integer                        :: kpc
    integer                        :: kpr
    integer                        :: l                    ! Loop variable 
    integer                        :: n                    ! Loop variable 
    integer                        :: n1
    real(fp)                       :: rdef                 ! Default value for boundary condition input for turbulent quantities should be < 0 !! 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Initialize local parameters
    !
    rdef = -999.999
    into = 0
    !
    !-----Reset boundary conditions for the in-active direction
    !
    do n = 1, nrob
       n1 = nob(8, n)
       if (n1/=into) then
          into = n1
          kpr = nob(4, n)
          kpc = nob(6, n)
          !
          !-----------If row boundary not available
          !           then re-define UBND (1,L,K,1/2,INTO)
          !
          if (kpr==0) then
             do l = 1, ltur
                do k = 0, kmax
                   ubnd(1, l, k, 1, into) = rdef
                   ubnd(1, l, k, 2, into) = rdef
                enddo
             enddo
          endif
          !
          !-----------If column boundary not available
          !           then re-define UBND (2,L,K,1/2,INTO)
          !
          if (kpc==0) then
             do l = 1, ltur
                do k = 0, kmax
                   ubnd(2, l, k, 1, into) = rdef
                   ubnd(2, l, k, 2, into) = rdef
                enddo
             enddo
          endif
       endif
    enddo
end subroutine uckbcc
