subroutine resetV1toV0subr(v0,v1,agvv,mGPv1,nGPv1,totGHOSTv1,kmax,nlb,nub,mlb,mub)
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
!  Ndryact: delft3d.support@deltares.nl                                         
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
!   Function:  Copy  v0 back to v1 (to try well balancing)
!
!   Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: totGHOSTv1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(in)    :: v0   
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(out)   :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(out)   :: agvv
    integer, dimension(totGHOSTv1)                                      , intent(in)    :: mGPv1
    integer, dimension(totGHOSTv1)                                      , intent(in)    :: nGPv1
!
!
! local variables
!
  integer                    :: I,mGP,nGP
!
! executable statements -------------------------------------------------------
!
!
! set u-velocity mask to constant
!
    do i = 1,totGHOSTv1   
       mGP = mGPv1(i)
       nGP = nGPv1(i) 
       if  (comparereal(agvv(nGP,mGP),0.5_fp).le.0.and.comparereal(agvv(nGP,mGP),0._fp).gt.0)  then   !i dont want to change it if aguu=0
          v1(nGP,mGP,1:kmax) = v0(nGP,mGP,1:kmax) 
       endif
    enddo
 
!
    return 
end subroutine resetV1toV0subr
