subroutine hunt(xx        ,n         ,x         ,jlo       )
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
    integer                                  :: jlo
    integer                    , intent(in)  :: n
    real(kind=hp)              , intent(in)  :: x
    real(kind=hp), dimension(n), intent(in)  :: xx
!
! Local variables
!
    integer :: inc
    integer :: jhi
    integer :: jm
    logical :: ascnd
!
!! executable statements -------------------------------------------------------
!
    ascnd = xx(n)>=xx(1)
    if (jlo<=0 .or. jlo>n) then
       jlo = 0
       jhi = n + 1
       goto 3
    endif
    inc = 1
    if (x>=xx(jlo) .eqv. ascnd) then
    1  continue
       jhi = jlo + inc
       if (jhi>n) then
          jhi = n + 1
       elseif (x>=xx(jhi) .eqv. ascnd) then
          jlo = jhi
          inc = inc + inc
          goto 1
       else
       endif
    else
       jhi = jlo
    2  continue
       jlo = jhi - inc
       if (jlo<1) then
          jlo = 0
       elseif (x<xx(jlo) .eqv. ascnd) then
          jhi = jlo
          inc = inc + inc
          goto 2
       else
       endif
    endif
    3 continue
    if (jhi - jlo==1) then
       return
    endif
    jm = (jhi + jlo)/2
    if (x>xx(jm) .eqv. ascnd) then
       jlo = jm
    else
       jhi = jm
    endif
    goto 3
end subroutine hunt
