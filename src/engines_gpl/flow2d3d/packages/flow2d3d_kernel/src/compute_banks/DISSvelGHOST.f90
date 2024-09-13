subroutine DISSvelGHOST(u, v, umean, vmean, nlb, nub, mlb, mub, kmax, gdp)
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
!   Function: Set velocity at ghost points equal to zero if lower than certain threshVELghost
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
    integer              , pointer :: totGHOSTu1
    integer              , pointer :: totGHOSTv1
    integer, dimension(:), pointer :: mGPv1
    integer, dimension(:), pointer :: nGPv1
    integer, dimension(:), pointer :: mGPu1
    integer, dimension(:), pointer :: nGPu1
    real(fp)             , pointer :: threshVELghost
!
! global variables 
!
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(out)   :: u   
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(out)   :: v 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(out)   :: umean 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(out)   :: vmean 
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
!
! local variables
!
   integer :: i
   integer :: mGP
   integer :: nGP
   real(fp):: maxABSu
   real(fp):: maxABSv
!
! executable statements -------------------------------------------------------
!   
    totGHOSTu1     => gdp%gdimbound%totGHOSTu1
    totGHOSTv1     => gdp%gdimbound%totGHOSTv1
    mGPv1          => gdp%gdimbound%mGPv1
    nGPv1          => gdp%gdimbound%nGPv1
    mGPu1          => gdp%gdimbound%mGPu1
    nGPu1          => gdp%gdimbound%nGPu1
    threshVELghost => gdp%gdimbound%threshVELghost
   do i = 1,totGHOSTu1       
!        
      mGP = mGPu1(i)
      nGP = nGPu1(i)
      maxABSu = maxval(abs(u(nGP,mGP,:)))
      if (maxABSu.lt.threshVELghost) then
         u(nGP,mGP,:) = 0._fp
         umean(nGP,mGP) = 0._fp
      endif
   enddo
   do i = 1,totGHOSTv1       
!        
      mGP = mGPv1(i)
      nGP = nGPv1(i)
      maxABSv = maxval(abs(v(nGP,mGP,:)))
      if (maxABSv.lt.threshVELghost) then
         v(nGP,mGP,:) = 0._fp
         vmean(nGP,mGP) = 0._fp
      endif 
   enddo
   ! 
end subroutine DISSvelGHOST
