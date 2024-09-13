subroutine periodic_dryWET_poly(gdp)
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
!   Function:  makes Ndry,Nwet,INTx_GRS,INTy_GRS,INTwx_GRS,INTwy_GRS periodic  
!               
!    Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                   , pointer :: nrPER
    integer, dimension(:)     , pointer :: mPH_ext
    integer, dimension(:)     , pointer :: nPH_ext
    integer, dimension(:)     , pointer :: mPQ_ext
    integer, dimension(:)     , pointer :: nPQ_ext
    integer, dimension(:)     , pointer :: mPH_int
    integer, dimension(:)     , pointer :: mPQ_int
    integer, dimension(:)     , pointer :: nPH_int
    integer, dimension(:)     , pointer :: nPQ_int
    integer, dimension(:,:)   , pointer :: Ndry_GRS
    integer, dimension(:,:)   , pointer :: Nwet_GRS
    real(fp), dimension(:,:,:), pointer :: INTx_GRS
    real(fp), dimension(:,:,:), pointer :: INTwx_GRS
    real(fp), dimension(:,:,:), pointer :: INTy_GRS
    real(fp), dimension(:,:,:), pointer :: INTwy_GRS
    real(fp)                  , pointer :: LchanPERprojX
    real(fp)                  , pointer :: LchanPERprojY
!
! global variables
! 
!
! local variables
!
  integer                    :: k 
  real(fp)                   :: dz
!
! executable statements -------------------------------------------------------
!
    nrPER     => gdp%gdimbound%nrPER
    mPH_ext   => gdp%gdimbound%mPH_ext
    nPH_ext   => gdp%gdimbound%nPH_ext
    mPQ_ext   => gdp%gdimbound%mPQ_ext
    nPQ_ext   => gdp%gdimbound%nPQ_ext
    mPH_int   => gdp%gdimbound%mPH_int
    mPQ_int   => gdp%gdimbound%mPQ_int
    nPH_int   => gdp%gdimbound%nPH_int
    nPQ_int   => gdp%gdimbound%nPQ_int
    Ndry_GRS  => gdp%gdimbound%Ndry_GRS
    Nwet_GRS  => gdp%gdimbound%Nwet_GRS
    INTx_GRS  => gdp%gdimbound%INTx_GRS
    INTwx_GRS => gdp%gdimbound%INTwx_GRS
    INTy_GRS  => gdp%gdimbound%INTy_GRS
    INTwy_GRS => gdp%gdimbound%INTwy_GRS
    LchanPERprojX    => gdp%gdimbound%LchanPERprojX
    LchanPERprojY    => gdp%gdimbound%LchanPERprojY
!
      do k=1,nrPER 
!                 
         Ndry_GRS(nPQ_ext(k),mPQ_ext(k)) = Ndry_GRS(nPH_int(k),mPH_int(k))    
         Nwet_GRS(nPQ_ext(k),mPQ_ext(k)) = Nwet_GRS(nPH_int(k),mPH_int(k))    
         INTx_GRS(:,nPQ_ext(k),mPQ_ext(k)) = INTx_GRS(:,nPH_int(k),mPH_int(k)) - LchanPERprojX
         INTy_GRS(:,nPQ_ext(k),mPQ_ext(k)) = INTy_GRS(:,nPH_int(k),mPH_int(k)) - LchanPERprojY     
         INTwx_GRS(:,nPQ_ext(k),mPQ_ext(k))= INTwx_GRS(:,nPH_int(k),mPH_int(k)) - LchanPERprojX          
         INTwy_GRS(:,nPQ_ext(k),mPQ_ext(k))= INTwy_GRS(:,nPH_int(k),mPH_int(k)) - LchanPERprojY                     
         Ndry_GRS(nPH_ext(k),mPH_ext(k)) = Ndry_GRS(nPQ_int(k),mPQ_int(k))
         Nwet_GRS(nPH_ext(k),mPH_ext(k)) = Nwet_GRS(nPQ_int(k),mPQ_int(k))
         INTx_GRS(:,nPH_ext(k),mPH_ext(k)) = INTx_GRS(:,nPQ_int(k),mPQ_int(k)) + LchanPERprojX
         INTy_GRS(:,nPH_ext(k),mPH_ext(k)) = INTy_GRS(:,nPQ_int(k),mPQ_int(k)) + LchanPERprojY  
         INTwx_GRS(:,nPH_ext(k),mPH_ext(k))= INTwx_GRS(:,nPQ_int(k),mPQ_int(k)) + LchanPERprojX 
         INTwy_GRS(:,nPH_ext(k),mPH_ext(k))= INTwy_GRS(:,nPQ_int(k),mPQ_int(k)) + LchanPERprojY 
!
      enddo        
!
RETURN
end subroutine periodic_dryWET_poly
