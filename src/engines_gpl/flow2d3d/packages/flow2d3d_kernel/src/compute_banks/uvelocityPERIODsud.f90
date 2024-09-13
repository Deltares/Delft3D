subroutine uvelocityPERIODsud(u1,icx,nlb,nub,mlb,mub,kmax, gdp)
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
!   Function:   Copy the othogonal velocity component 
!               the water discharge internal boundary to the water level external one
!               in order to force it in sud
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
    integer, dimension(:), pointer :: mPH_ext
    integer, dimension(:), pointer :: nPH_ext
    integer, dimension(:), pointer :: mPQ_ext
    integer, dimension(:), pointer :: nPQ_ext
    integer, dimension(:), pointer :: mPH_int
    integer, dimension(:), pointer :: mPQ_int
    integer, dimension(:), pointer :: mPH_intint
    integer, dimension(:), pointer :: mPQ_intint
    integer, dimension(:), pointer :: nPH_int
    integer, dimension(:), pointer :: nPQ_int
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
    logical              , pointer :: FORCEuAThPERbnd
    logical              , pointer :: PERIODICorthVEL
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(inout)   :: u1  
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
  integer, intent(in)        :: kmax
  integer, intent(in)        :: icx
!
! local variables
!
  integer                    :: k 
!
! executable statements -------------------------------------------------------
!     
    nrPER           => gdp%gdimbound%nrPER
    twoCELLSperiod  => gdp%gdimbound%twoCELLSperiod
    mPH_ext         => gdp%gdimbound%mPH_ext
    nPH_ext         => gdp%gdimbound%nPH_ext
    mPQ_ext         => gdp%gdimbound%mPQ_ext
    nPQ_ext         => gdp%gdimbound%nPQ_ext
    mPH_int         => gdp%gdimbound%mPH_int
    mPQ_int         => gdp%gdimbound%mPQ_int
    mPH_intint      => gdp%gdimbound%mPH_intint
    mPQ_intint      => gdp%gdimbound%mPQ_intint
    nPH_int         => gdp%gdimbound%nPH_int
    nPQ_int         => gdp%gdimbound%nPQ_int
    nPH_intint      => gdp%gdimbound%nPH_intint
    nPQ_intint      => gdp%gdimbound%nPQ_intint
    mPH_extext      => gdp%gdimbound%mPH_extext
    mPH_ext         => gdp%gdimbound%mPH_ext
    mPQ_extext      => gdp%gdimbound%mPQ_extext
    nPH_extext      => gdp%gdimbound%nPH_extext
    nPQ_extext      => gdp%gdimbound%nPQ_extext
    shiftHM         => gdp%gdimbound%shiftHM
    shiftHN         => gdp%gdimbound%shiftHN
    shiftQM         => gdp%gdimbound%shiftQM
    shiftQN         => gdp%gdimbound%shiftQN
    FORCEuAThPERbnd => gdp%gdimbound%FORCEuAThPERbnd
    PERIODICorthVEL => gdp%gdimbound%PERIODICorthVEL
                    
      if (FORCEuAThPERbnd) then
         do k=1,nrPER  !without +1 cause  it is the orthogonal velocity
          ! note: the same shift (shiftQM,shiftQN) is used for both right and left hand sides when prescribing something at Q bounadry. The same for H boundary.
            u1(nPH_ext(k)+shiftHN,mPH_ext(k)+shiftHM,1:kmax) = u1(nPQ_int(k)+shiftHN,mPQ_int(k)+shiftHM,1:kmax) 
           ! u1(nPQ_ext(k)+shiftQN,mPQ_ext(k)+shiftQM,1:kmax) = u1(nPH_int(k)+shiftQN,mPH_int(k)+shiftQM,1:kmax) it is forced at Q boundary
         enddo
      endif
      !non needed actually.  at H boundary (H is forced so U is not used.)U0 before sud is used in the centered advective terms.
      !                      at Q boundary: u is prescribed as boundary condtion, so its disconnected. Not useo (U0 before sud is used in the centered advective terms.)
      if (twoCELLSperiod) THEN
         if(PERIODICorthVEL) then
            do k=1,nrPER
               u1(nPH_extext(k)+shiftHN,mPH_extext(k)+shiftHM,1:kmax) =  u1(nPQ_intint(k)+shiftHN,mPQ_intint(k)+shiftHM,1:kmax) 
               u1(nPQ_extext(k)+shiftQN,mPQ_extext(k)+shiftQM,1:kmax) =  u1(nPH_intint(k)+shiftQN,mPH_intint(k)+shiftQM,1:kmax) ! not used if waqua 
            enddo
         endif
      endif
RETURN
end subroutine uvelocityPERIODsud
