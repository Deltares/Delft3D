subroutine wrtbch(lunbch    ,ntof      ,kc        ,omega     ,hydrbc    , &
                & mxnto     ,mxkc      )
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
!    Function: write frequencies, amplitudes and phases to
!              intermediate boundary conditions file (lunbch)
! Method used:
!              1. write all frequencies
!              2. write all amplitudes at every A-end
!              3. write all amplitudes at every B-end
!              4. write all phases at every A-end
!              5. write all phases at every B-end
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)            :: kc !  Description and declaration in dimens.igs
    integer, intent(in)            :: lunbch
                                   !!  Logical unit number of intermediate
                                   !!  boundary condition file
    integer, intent(in)            :: mxkc
                                   !!  Maximum of number of frequencies
    integer, intent(in)            :: mxnto
                                   !!  Maximum of number of open boundaries
    integer, intent(in)            :: ntof !  Description and declaration in dimens.igs
    real(fp), dimension(4, mxnto, mxkc), intent(in) :: hydrbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(mxkc), intent(in) :: omega !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: i
    integer                        :: k
    integer                        :: n
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !
    !     write number of frequencies, used in routine dimrd
    !
    !
    write (lunbch) kc
    !
    !
    !     write frequencies
    !
    !
    write (lunbch) (omega(i), i = 1, kc)
    !
    !
    !     write amplitudes at all ends A of tidal openings
    !
    !
    write (lunbch)
    !
    do n = 1, ntof
       write (lunbch) (hydrbc(1, n, k), k = 1, kc)
    enddo
    !
    !
    !     write amplitudes at all ends B of tidal openings
    !
    !
    do n = 1, ntof
       write (lunbch) (hydrbc(2, n, k), k = 1, kc)
    enddo
    !
    !
    !     write phases at all ends A of tidal openings
    !
    !
    write (lunbch)
    !
    do n = 1, ntof
       write (lunbch) (hydrbc(3, n, k), k = 1, kc)
    enddo
    !
    !
    !     write phases at all ends B of tidal openings
    !
    !
    do n = 1, ntof
       write (lunbch) (hydrbc(4, n, k), k = 1, kc)
    enddo
end subroutine wrtbch
