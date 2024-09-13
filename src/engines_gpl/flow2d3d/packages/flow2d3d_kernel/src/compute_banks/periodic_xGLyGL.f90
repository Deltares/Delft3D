subroutine periodic_xGLyGL(xG_L,yG_L,nlb,nub,mlb,mub,kmax, gdp)
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
!  contact: delft3d.support@deltares.nl                                         
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
!   Function:    
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer              , pointer :: nrPER
    integer, dimension(:), pointer :: mPH_ext
    integer, dimension(:), pointer :: nPH_ext
    integer, dimension(:), pointer :: mPQ_ext
    integer, dimension(:), pointer :: nPQ_ext
    integer, dimension(:), pointer :: mPH_int
    integer, dimension(:), pointer :: mPQ_int
    integer, dimension(:), pointer :: nPH_int
    integer, dimension(:), pointer :: nPQ_int
    real(fp)             , pointer :: LchanPERprojX
    real(fp)             , pointer :: LchanPERprojY
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub)            ,intent(inout)   :: xG_L  
  real(fp)  , dimension(nlb:nub,mlb:mub)            ,intent(inout)   :: yG_L
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
  integer, intent(in)        :: kmax
!  integer, intent(in)        :: icx
!
! local variables
!
  integer                    :: k 
!
! executable statements -------------------------------------------------------
!                         
    nrPER         => gdp%gdimbound%nrPER
    mPH_ext       => gdp%gdimbound%mPH_ext
    nPH_ext       => gdp%gdimbound%nPH_ext
    mPQ_ext       => gdp%gdimbound%mPQ_ext
    nPQ_ext       => gdp%gdimbound%nPQ_ext
    mPH_int       => gdp%gdimbound%mPH_int
    mPQ_int       => gdp%gdimbound%mPQ_int
    nPH_int       => gdp%gdimbound%nPH_int
    nPQ_int       => gdp%gdimbound%nPQ_int
    mPH_ext       => gdp%gdimbound%mPH_ext
    LchanPERprojX => gdp%gdimbound%LchanPERprojX
    LchanPERprojY => gdp%gdimbound%LchanPERprojY
      do k=0,nrPER+1  
         xG_L(nPH_ext(k),mPH_ext(k)) = xG_L(nPQ_int(k),mPQ_int(k)) + LchanPERprojX 
         xG_L(nPQ_ext(k),mPQ_ext(k)) = xG_L(nPH_int(k),mPH_int(k)) - LchanPERprojX   
         yG_L(nPH_ext(k),mPH_ext(k)) = yG_L(nPQ_int(k),mPQ_int(k)) + LchanPERprojY 
         yG_L(nPQ_ext(k),mPQ_ext(k)) = yG_L(nPH_int(k),mPH_int(k)) - LchanPERprojY   
      enddo 
RETURN
end subroutine periodic_xGLyGL
