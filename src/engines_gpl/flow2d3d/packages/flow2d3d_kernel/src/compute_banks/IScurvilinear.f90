subroutine IScurvilinear(guu,gvv,icx,icy,nmlb,nmub,nmmax, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   Copy the total sediment transport from the water level (periodic) boundary
!               to the discharge (periodic) boundary
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    logical, pointer :: curvMESH
!
! global variables
!
    real(fp)  , dimension(nmlb:nmub)       , intent(inout)   :: guu  
    real(fp)  , dimension(nmlb:nmub)       , intent(inout)   :: gvv
    integer                                , intent(in)      :: icx
    integer                                , intent(in)      :: icy
    integer                                , intent(in)      :: nmlb
    integer                                , intent(in)      :: nmub
    integer                                , intent(in)      :: nmmax
!
! local variables
!
    integer                    :: nm
    integer                    :: nmd
    integer                    :: ndm
!
! executable statements -------------------------------------------------------
!                 
    curvMESH => gdp%gdimbound%curvMESH
    !
    ! Consider to change it and use alfas to check if its curvivlinear (all angle alfas with x has to be the same. 
    ! Check how alfas is computed or check is value for a cell of zero area in the periodical circular channel)
    !        
    curvMESH =.false.        
    do nm=1,nmmax
       nmd = nm-icx
       ndm = nm-icy
       if (comparereal(guu(nm),0._fp).eq.0.or.comparereal(guu(nmd),0._fp).eq.0) cycle ! guu(nm) or guu(nmd) are not defined
       if (comparereal(abs(guu(nm)-guu(nmd)),0.00000001_fp).gt.0) then
          curvMESH = .true.
          exit
       endif
       if (comparereal(gvv(nm),0._fp).eq.0.or.comparereal(gvv(ndm),0._fp).eq.0) cycle ! guu(nm) or guu(nmd) are not defined
       if (comparereal(abs(gvv(nm)-gvv(ndm)),0.00000001_fp).gt.0) then
          curvMESH = .true.
          exit
       endif
    enddo
    !
    return
end subroutine IScurvilinear
