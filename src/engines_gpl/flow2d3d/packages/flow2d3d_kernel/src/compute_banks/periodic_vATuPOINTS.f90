subroutine periodic_vATuPOINTS(vATu,nlb,nub,mlb,mub,kmax, gdp)
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
!   Function:   Copy the transversal velocity component  from 
!               the water level internal boundary to the water discharge external one,
!               and from the water discharge internal boundary to the water level external one
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
    logical              , pointer :: twoCELLSperiod
    integer, dimension(:), pointer :: mPH_intint
    integer, dimension(:), pointer :: mPQ_intint
    integer, dimension(:), pointer :: nPH_intint
    integer, dimension(:), pointer :: nPQ_intint
    integer, dimension(:), pointer :: mPH_extext
    integer, dimension(:), pointer :: mPQ_extext
    integer, dimension(:), pointer :: nPH_extext
    integer, dimension(:), pointer :: nPQ_extext
    integer              , pointer :: shiftHM
    integer              , pointer :: shiftHN
    integer              , pointer :: shiftQM
    integer              , pointer :: shiftQN
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(inout)   :: vATu  
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
    nrPER          => gdp%gdimbound%nrPER
    twoCELLSperiod => gdp%gdimbound%twoCELLSperiod
    mPH_intint     => gdp%gdimbound%mPH_intint
    mPQ_intint     => gdp%gdimbound%mPQ_intint
    nPH_intint     => gdp%gdimbound%nPH_intint
    nPQ_intint     => gdp%gdimbound%nPQ_intint
    mPH_extext     => gdp%gdimbound%mPH_extext
    mPQ_extext     => gdp%gdimbound%mPQ_extext
    nPH_extext     => gdp%gdimbound%nPH_extext
    nPQ_extext     => gdp%gdimbound%nPQ_extext
    shiftHM        => gdp%gdimbound%shiftHM
    shiftHN        => gdp%gdimbound%shiftHN
    shiftQM        => gdp%gdimbound%shiftQM
    shiftQN        => gdp%gdimbound%shiftQN
!                            
      do k=0,nrPER-1
         if (twoCELLSperiod) THEN
            vATu(nPH_extext(k)+shiftHN,mPH_extext(k)+shiftHM,1:kmax) =  vATu(nPQ_intint(k)+shiftHN,mPQ_intint(k)+shiftHM,1:kmax) 
            vATu(nPQ_extext(k)+shiftQN,mPQ_extext(k)+shiftQM,1:kmax) =  vATu(nPH_intint(k)+shiftQN,mPH_intint(k)+shiftQM,1:kmax)  
         endif
      enddo 
RETURN
end subroutine periodic_vATuPOINTS
