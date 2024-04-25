subroutine nm_to_diag(gdp)
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
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global parameters
!
!
! Local parameters
!
    integer :: m
    integer :: n
    integer :: nm
!
!! executable statements -------------------------------------------------------
!
    write(gdp%gdinout%lundia,'(a)') '      nm to       n and   m :'
    do nm = gdp%d%nmlb, gdp%d%nmub
       call nm_to_n_and_m(nm, n, m, gdp)
       write(gdp%gdinout%lundia,'(i8,a,i8,i8)') nm, ' : ', n, m
    enddo
    write(gdp%gdinout%lundia,'(a)') ' '
    write(gdp%gdinout%lundia,'(a)') '       n and   m to      nm :'
    do n = gdp%d%nlb, gdp%d%nub
       do m = gdp%d%mlb, gdp%d%mub
          call n_and_m_to_nm(n, m, nm, gdp)
          write(gdp%gdinout%lundia,'(i8,i8,a,i8)') n, m, ' : ', nm
       enddo
    enddo
end subroutine nm_to_diag
