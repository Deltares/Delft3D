subroutine sortindices(nm     ,npnt   ,val    ,nmlb   ,nmub   ,increasing)
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
! if argument increasing=true then
!    sort the indices nm(:) such that val(nm(i))<=val(nm(i+1)) for all 1<=i<npnt.
!    (increasing)
! else
!    sort the indices nm(:) such that val(nm(i))>=val(nm(i+1)) for all 1<=i<npnt.
!    (decreasing)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                                              , intent(in)   :: npnt
    integer   , dimension(1:npnt)                        , intent(inout):: nm
    integer                                              , intent(in)   :: nmlb
    integer                                              , intent(in)   :: nmub
    real(fp)  , dimension(nmlb:nmub)                     , intent(in)   :: val
    logical                                              , intent(in)   :: increasing
!
! Local variables
!
    integer   :: i
    integer   :: ibi
    integer   :: imin
    integer   :: imax
    integer   :: j
    integer   :: nmi
    real(fp)  :: vi
    real(fp)  :: sign
!
!! executable statements -------------------------------------------------------
!
    sign = -1.0_fp
    if (increasing) sign = 1.0_fp
    !
    do i = 2, npnt
       nmi = nm(i)
       vi  = sign*val(nmi)
       !
       ! vi bigger than the currently biggest value
       !
       if (vi>=sign*val(nm(i-1))) cycle
       !
       ! vi smaller than the currently smallest value
       !
       if (vi<sign*val(nm(1))) then
          do j = i,2,-1
             nm(j) = nm(j-1)
          enddo
          nm(1) = nmi
          cycle
       endif
       !
       ! vi somewhere in between
       !
       imin = 1
       imax = i-1
       do while (imax>imin+1)
          ibi = (imax+imin)/2
          if (vi>=sign*val(nm(ibi))) then
             imin = ibi
          else
             imax = ibi
          endif
       enddo
       !
       ! vi between imin and imax
       !
       do j = i,imax+1,-1
          nm(j) = nm(j-1)
       enddo
       nm(imax) = nmi
    enddo
    !
end subroutine sortindices
