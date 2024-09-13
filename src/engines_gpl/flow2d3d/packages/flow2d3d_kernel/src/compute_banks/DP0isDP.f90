SUBROUTINE DP0isDP(dpu,dpv,dps,mmax,nmaxus,nlb,nub,mlb,mub,gdp)
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
!   Function: set the new values of  kfu,kfv for u1/v1/s1 ghost cells plus
!             u1/v1 of the s1 ghost cells.
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:)  , pointer :: dpu0
    real(fp), dimension(:,:)  , pointer :: dpv0
    real(prec), dimension(:,:), pointer :: dps0
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: dpu 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: dpv 
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(in)    :: dps
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus 
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
!
! local variables
!
    integer :: n,m

    dpu0 => gdp%gdimbound%dpu0
    dpv0 => gdp%gdimbound%dpv0
    dps0 => gdp%gdimbound%dps0
   !
    do m = 1, mmax 
       do n = 1, nmaxus
          dpu0(n,m) = dpu(n,m)  
          dpv0(n,m) = dpv(n,m) 
          dps0(n,m) = dps(n,m) !needed for local mass conserv
       enddo
    enddo

  return 
end subroutine DP0isDP
