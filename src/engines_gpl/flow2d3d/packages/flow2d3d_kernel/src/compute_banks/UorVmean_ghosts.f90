subroutine UorVmean_ghosts(u1,mmax,nmax,nmaxus,kmax,nst,totGHOSTu1,mGPu1,nGPu1,Umean,thick,nlb,nub,mlb,mub,nmlb,nmub)
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
!   Function:   Compute depth average velocity at ghost U-velocity point
!               
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: Umean 
    real(fp), dimension(kmax)                                           , intent(in)    :: thick 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: totGHOSTu1
    integer                                                             , intent(in)    :: mGPu1(totGHOSTu1)
    integer                                                             , intent(in)    :: nGPu1(totGHOSTu1)
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
!
!
! local variables
!
  integer                    :: i
  integer                    :: k
  integer                    :: mGP
  integer                    :: nGP
   

!
! executable statements -------------------------------------------------------
!  
    do i = 1,totGHOSTu1   
       mGP = mGPu1(i)
       nGP = nGPu1(i) 
       umean(nGP,mGP) = 0._fp 
       do k = 1, kmax
          umean(nGP,mGP) = umean(nGP,mGP) + thick(k)*u1(nGP,mGP, k)
       enddo
    enddo       
RETURN
END