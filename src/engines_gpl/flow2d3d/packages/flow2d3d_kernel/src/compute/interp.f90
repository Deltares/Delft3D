function interp (x, y, kfmin, kfmax, kfstep, kmax, xval)
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
! interpolate y values for xval in the range of x array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Function result
    real(fp) :: interp
!
!
! Global variables
!
    integer                       , intent(in) :: kfmin
    integer                       , intent(in) :: kfmax
    integer                       , intent(in) :: kfstep
    integer                       , intent(in) :: kmax
    real(fp)                      , intent(in) :: xval
    real(fp), dimension (0:kmax+1), intent(in) :: x
    real(fp), dimension (kmax)    , intent(in) :: y
!
! Local variables
!
    integer :: k
!
!! executable statements -------------------------------------------------------
!
    interp = -999.0_fp
    if (xval < x(0)) then
       !
       ! outside extrapolation region lower side
       !
       interp = 0.0_fp
    elseif (xval <= x(kfmin)) then
       !
       ! inside extrapolation region lower side
       !
       interp = y(kfmin)
    elseif (xval > x(kfmax+1)) then
       !
       ! outside extrapolation region upper side
       !
       interp = 0.0_fp
    elseif (xval >= x(kfmax)) then
       !
       ! inside extrapolation region upper side
       !
       interp = y(kfmax)
    else
       !
       ! interpolation
       !
       do k = kfmin+kfstep, kfmax, kfstep
          if (xval >= x(k-kfstep) .and. xval <= x(k)) then
             interp = y(k-kfstep) + ((xval - x(k-kfstep))/(x(k) - x(k-kfstep))) &
                    &               * (y(k) - y(k-kfstep))
             exit
          endif
       enddo
    endif
end function interp
