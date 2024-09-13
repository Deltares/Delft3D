  subroutine u1isuFULL(uFULL,u1,nlb,nub,mlb,mub,kmax, gdp) 
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
!   Function: set u1=uFULL at FULLY DRY ghost points
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: aguu
    integer, dimension(:)   , pointer :: mGPu1
    integer, dimension(:)   , pointer :: nGPu1
    integer                 , pointer :: totGHOSTu1
    logical, dimension(:,:) , pointer :: newGHOSTu
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(in)    :: uFULL
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(out)   :: u1
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
!
    integer           :: i,mGP,nGP,k
!
    aguu       => gdp%gdimbound%aguu
    mGPu1      => gdp%gdimbound%mGPu1
    nGPu1      => gdp%gdimbound%nGPu1
    totGHOSTu1 => gdp%gdimbound%totGHOSTu1
    newGHOSTu  => gdp%gdimbound%newGHOSTu
   !
    do i = 1,totGHOSTu1   
      mGP = mGPu1(i)
      nGP = nGPu1(i) 
      if (comparereal(aguu(nGP,mGP),0._fp)==0.and..not.newGHOSTu(nGP,mGP)) then
         do k=1,kmax
            u1(nGP,mGP,k) = uFULL(nGP,mGP,k)
         enddo
      endif
    enddo

  return 
end subroutine u1isuFULL
