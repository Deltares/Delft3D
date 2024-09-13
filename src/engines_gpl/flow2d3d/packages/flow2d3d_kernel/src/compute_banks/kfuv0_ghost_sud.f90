subroutine kfuv0_ghost_sud(kfu,kfv,nlb,nub,mlb,mub, gdp)
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
!   Function: set the values of  kfu,kfv for u1/v1 ghost  at zero in sud ( !equivalent to  call kfsuv_ghost, just to avoid to pass Umean and other stuff)
!
!   Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: totGHOSTu1
    integer                 , pointer :: totGHOSTv1
    integer, dimension(:)   , pointer :: nGPu1
    integer, dimension(:)   , pointer :: mGPu1
    integer, dimension(:)   , pointer :: nGPv1
    integer, dimension(:)   , pointer :: mGPv1
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: agvv
!
! global variables
!
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfv
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
!
!
! local variables
!
  integer                    :: I,mGP,nGP

!  real(fp)                   :: dxADJ

!
! executable statements -------------------------------------------------------
!
    totGHOSTu1 => gdp%gdimbound%totGHOSTu1
    totGHOSTv1 => gdp%gdimbound%totGHOSTv1
    nGPu1      => gdp%gdimbound%nGPu1
    mGPu1      => gdp%gdimbound%mGPu1
    nGPv1      => gdp%gdimbound%nGPv1
    mGPv1      => gdp%gdimbound%mGPv1
    aguu       => gdp%gdimbound%aguu
    agvv       => gdp%gdimbound%agvv
!
! set u-velocity mask to one
!
    do i = 1,totGHOSTu1   
       mGP = mGPu1(i)
       nGP = nGPu1(i) 
       if  (comparereal(aguu(nGP,mGP),0._fp).eq.0)  then   
         kfu(nGP,mGP) = 0 
       endif
    enddo
!
! set v-velocity mask to one
!
    do i = 1,totGHOSTv1   
       mGP = mGPv1(i)
       nGP = nGPv1(i) 
       if  (comparereal(agvv(nGP,mGP),0._fp).eq.0)  then   
         kfv(nGP,mGP) = 0 
       endif
    enddo
!
    return 
end subroutine kfuv0_ghost_sud
