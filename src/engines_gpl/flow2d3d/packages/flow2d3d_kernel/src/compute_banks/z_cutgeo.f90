subroutine z_cutgeo(zk,kfu,kfv,icx,icy,lunscr,nst,nmmax,nmlb,nmub,kmax, gdp)
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
!   Function: Z-layer only. Find the lowermost and uppermost vertical layer for which ghost points are still present (kGPumin,kGPumax) (kGPvmin,kGPvmax)
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
    integer, dimension(:)     , pointer :: kGPumin
    integer, dimension(:)     , pointer :: kGPvmin
    integer, dimension(:)     , pointer :: kGPumax
    integer, dimension(:)     , pointer :: kGPvmax
    real(fp), dimension(:,:)  , pointer :: z_aguu
    real(fp), dimension(:,:)  , pointer :: z_agvv
    real(fp), dimension(:,:)  , pointer :: dpL
    real(fp), dimension(:,:)  , pointer :: dpH
    real(fp), dimension(:,:,:), pointer :: EDGElenWET
    real(fp), dimension(:,:)  , pointer :: aguu
    real(fp), dimension(:,:)  , pointer :: agvv
!
! global variables
!
!
    real(fp), dimension(0:kmax)                                         , intent(in)    :: zk
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfv
    integer                                                             , intent(in)    :: lunscr
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmmax
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: kmax
!
! local variables
!
  integer                    :: nm
  integer                    :: nmj
  integer                    :: k   
  integer                    :: nmk(4)
  
!
! executable statements -------------------------------------------------------
!  
    kGPumin    => gdp%gdimbound%kGPumin
    kGPvmin    => gdp%gdimbound%kGPvmin
    kGPumax    => gdp%gdimbound%kGPumax
    kGPvmax    => gdp%gdimbound%kGPvmax
    z_aguu     => gdp%gdimbound%z_aguu
    z_agvv     => gdp%gdimbound%z_agvv
    dpL        => gdp%gdimbound%dpL
    dpH        => gdp%gdimbound%dpH
    EDGElenWET => gdp%gdimbound%EDGElenWET
    aguu       => gdp%gdimbound%aguu
    agvv       => gdp%gdimbound%agvv
  ! along x  
   CALL z_aguuCOMP(EDGElenWET,zk,kfu,z_aguu,aguu,dpL,dpH,icx,icy,lunscr,nst,nmmax,nmlb,nmub,kmax)
         
   CALL z_aguuCOMP(EDGElenWET,zk,kfv,z_agvv,agvv,dpL,dpH,icy,icx,lunscr,nst,nmmax,nmlb,nmub,kmax)


RETURN
end subroutine z_cutgeo
