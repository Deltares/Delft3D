  subroutine v1isvFULL(vFULL,v1,nlb,nub,mlb,mub,kmax, gdp) 
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
!   Function: set v1=vFULL at FULLY DRY ghost points
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: agvv
    integer, dimension(:)   , pointer :: mGPv1
    integer, dimension(:)   , pointer :: nGPv1
    integer                 , pointer :: totGHOSTv1
    logical, dimension(:,:) , pointer :: newGHOSTv
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(in)    :: vFULL
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(out)   :: v1
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
!
    integer           :: i,mGP,nGP,k
!
    agvv       => gdp%gdimbound%agvv
    mGPv1      => gdp%gdimbound%mGPv1
    nGPv1      => gdp%gdimbound%nGPv1
    totGHOSTv1 => gdp%gdimbound%totGHOSTv1
    newGHOSTv  => gdp%gdimbound%newGHOSTv
   !
    do i = 1,totGHOSTv1   
      mGP = mGPv1(i)
      nGP = nGPv1(i) 
      if (comparereal(agvv(nGP,mGP),0._fp)==0.and..not.newGHOSTv(nGP,mGP)) then
         do k=1,kmax
            v1(nGP,mGP,k) = vFULL(nGP,mGP,k)
         enddo
      endif
    enddo

  return 
end subroutine v1isvFULL
