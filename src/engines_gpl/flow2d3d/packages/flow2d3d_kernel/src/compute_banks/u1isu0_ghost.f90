  subroutine u1isu0_ghost(u0,u1,nlb,nub,mlb,mub,kmax, gdp) 
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
!   Function: set u1=u0 at ghost points
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
    integer, dimension(:), pointer :: nGPu1
    integer, dimension(:), pointer :: mGPu1
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: u0
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
!
    integer           :: i,mGP,nGP
!
    totGHOSTu1 => gdp%gdimbound%totGHOSTu1
    nGPu1      => gdp%gdimbound%nGPu1
    mGPu1      => gdp%gdimbound%mGPu1
   !
    do i = 1,totGHOSTu1   
      mGP = mGPu1(i)
      nGP = nGPu1(i) 
      u1(nGP,mGP) = u0(nGP,mGP)
    enddo

  return 
end subroutine u1isu0_ghost
