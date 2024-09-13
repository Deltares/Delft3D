subroutine RECOMPqkz(w1,qzk,gsqs,agsqs,kmax,nmmax,nmlb,nmub,nst, gdp) 

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
!   Function:   recompute qxk at donor and receiving cell after merging
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
    integer, dimension(:)  , pointer :: Nmerged_w
    integer, dimension(:,:), pointer :: NMlistMERGED_w
!
! global variables
!
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: kmax
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: nmmax
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gsqs
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs
    real(fp), dimension(nmlb:nmub,0:kmax)                , intent(out)   :: qzk
    real(fp), dimension(nmlb:nmub,0:kmax)                , intent(in)    :: w1
 
!
! local variables
!
    integer  :: nm 
    integer  :: k
    integer  :: N
    integer  :: nmN
!
!
! executable statements -------------------------------------------------------
!
    Nmerged_w      => gdp%gdimbound%Nmerged_w
    NMlistMERGED_w => gdp%gdimbound%NMlistMERGED_w
!
    do nm = 1,nmmax
       if (Nmerged_w(nm)>1) then
          DO N = 1,Nmerged_w(nm)  
             nmN = NMlistMERGED_w(N,nm)  
             do k=1,kmax
                qzk(nmN, k) = w1(nmN, k)*gsqs(nmN)*agsqs(nmN)   
             enddo
          ENDDO
       endif
    enddo
 
!
RETURN
end subroutine RECOMPqkz
