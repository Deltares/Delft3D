subroutine periodSPIRAL(r0,r1,nlb,nub,mlb,mub,kmax,lsecfl,lstsci, gdp)
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
!   Function:   Copy intensity of spiral flow in r0 and r1 (double check if r0 is needed)
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
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub, kmax, lstsci)            ,intent(inout)   :: r0
  real(fp)  , dimension(nlb:nub,mlb:mub, kmax, lstsci)            ,intent(inout)   :: r1
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
  integer, intent(in)        :: kmax
  integer, intent(in)        :: lsecfl
  integer, intent(in)        :: lstsci
!
! local variables
!
  integer                    :: k 
!
! executable statements -------------------------------------------------------
!               
    nrPER   => gdp%gdimbound%nrPER
    mPH_ext => gdp%gdimbound%mPH_ext
    nPH_ext => gdp%gdimbound%nPH_ext
    mPQ_ext => gdp%gdimbound%mPQ_ext
    nPQ_ext => gdp%gdimbound%nPQ_ext
    mPH_int => gdp%gdimbound%mPH_int
    mPQ_int => gdp%gdimbound%mPQ_int
    nPH_int => gdp%gdimbound%nPH_int
    nPQ_int => gdp%gdimbound%nPQ_int
    mPH_ext => gdp%gdimbound%mPH_ext
 
   do k=1,nrPER   
      r0(nPH_ext(k),mPH_ext(k),1,lsecfl) = r0(nPQ_int(k),mPQ_int(k),1,lsecfl)  !only 2D, layer 1
      r1(nPH_ext(k),mPH_ext(k),1,lsecfl) = r0(nPQ_int(k),mPQ_int(k),1,lsecfl)
      r0(nPQ_ext(k),mPQ_ext(k),1,lsecfl) = r0(nPH_int(k),mPH_int(k),1,lsecfl)  !only 2D, layer 1 NOTE r0 GOES TO R1 SINCE R1 IS STILL UNDEFINED
      r1(nPQ_ext(k),mPQ_ext(k),1,lsecfl) = r0(nPH_int(k),mPH_int(k),1,lsecfl)  !only 2D, layer 1 NOTE r0 GOES TO R1 SINCE R1 IS STILL UNDEFINED
   enddo 
 
!
RETURN
end subroutine periodSPIRAL
