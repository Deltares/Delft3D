subroutine wrivec(nammap, u, v, xb, yb, m, n, np)
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
    use precision_basics
    !
    implicit none
!
! Global variables
!
    integer             , intent(in)  :: m
    integer             , intent(in)  :: n
    integer             , intent(in)  :: np
    real, dimension(m,n), intent(in)  :: u
    real, dimension(m,n), intent(in)  :: v
    real(kind=hp), dimension(m,n), intent(in)  :: xb
    real(kind=hp), dimension(m,n), intent(in)  :: yb
    character(20)       , intent(in)  :: nammap
!
! Local variables
!
    integer           :: i
    integer           :: j
    integer           :: lun
!
!! executable statements -------------------------------------------------------
!
    open(newunit=lun,file=nammap)
    write(lun,'(a4)') 'VECT'
    write(lun,*)np,4,n,m
    do i=1,m
       do j=1,n
          write(lun,'(2f10.1,2f9.3)') xb(i,j),yb(i,j),u(i,j),v(i,j)
       enddo
    enddo
    close(lun)
end subroutine wrivec
