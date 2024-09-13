    subroutine v0isv1_ghost(v0,v1,nlb,nub,mlb,mub,kmax, gdp) 
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
!   Function: set v0=v1 at ghost points
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer              , pointer :: totGHOSTv1
    integer, dimension(:), pointer :: nGPv1
    integer, dimension(:), pointer :: mGPv1
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: v0
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
!
    integer           :: i,mGP,nGP
!
    totGHOSTv1 => gdp%gdimbound%totGHOSTv1
    nGPv1      => gdp%gdimbound%nGPv1
    mGPv1      => gdp%gdimbound%mGPv1
   !
    do i = 1,totGHOSTv1   
      mGP = mGPv1(i)
      nGP = nGPv1(i) 
      v0(nGP,mGP) = v1(nGP,mGP)
    enddo

  return 
end subroutine v0isv1_ghost
