function fgyint(a         ,b         ,z         ,eps       ,terfgy    )
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
! determines fgyint, a contribution to
! the bijker transport relation
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Arguments
!
    real(fp), intent(in)  :: a
    real(fp), intent(in)  :: b
    real(fp), intent(in)  :: eps    !  Description and declaration in numeco.igs
    real(fp), intent(in)  :: z
    real(fp)              :: fgyint
    real(hp), external    :: terfgy
!
! Local variables
!
    integer  :: i      ! running index
    real(hp) :: ad
    real(hp) :: aps
    real(hp) :: bd
    real(hp) :: hulp1
    real(hp) :: hulp2
    real(hp) :: rest
    real(hp) :: som
    real(hp) :: zd
!
!! executable statements -------------------------------------------------------
!
    ad  = a
    bd  = b
    zd  = z
    som = 0.
    i   = 0
    aps = eps
  100 continue
    i = i + 1
    if (i>100) then
       write (*, *) ' No convergence in fgyint'
       goto 200
    endif
    hulp1 = terfgy(i, bd, zd)
    hulp2 = terfgy(i, ad, zd)
    rest  = hulp1 - hulp2
    som   = som + rest
    if (rest>=0. .and. rest<=0. .or. som>=0. .and. som<=0.) goto 200
    if (abs(rest/som)>=aps) goto 100
  200 continue
    fgyint = real(som, fp)
end function fgyint
