subroutine WATERlevelPERIOD(s1,dps,icx,nlb,nub,mlb,mub,kmax, gdp)
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
!   Function:   Copy water level /depth
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
    logical              , pointer :: PERIODICwaterDEPTH
    integer, dimension(:), pointer :: mPH_ext
    integer, dimension(:), pointer :: nPH_ext
    integer, dimension(:), pointer :: mPQ_ext
    integer, dimension(:), pointer :: nPQ_ext
    integer, dimension(:), pointer :: mPH_int
    integer, dimension(:), pointer :: mPQ_int
    integer, dimension(:), pointer :: nPH_int
    integer, dimension(:), pointer :: nPQ_int
    logical              , pointer :: FORCEuAThPERbnd
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub)            ,intent(inout)   :: s1
  real(prec), dimension(nlb:nub,mlb:mub)            ,intent(inout)   :: dps
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
    nrPER              => gdp%gdimbound%nrPER
    twoCELLSperiod     => gdp%gdimbound%twoCELLSperiod
    PERIODICwaterDEPTH => gdp%gdimbound%PERIODICwaterDEPTH
    mPH_ext            => gdp%gdimbound%mPH_ext
    nPH_ext            => gdp%gdimbound%nPH_ext
    mPQ_ext            => gdp%gdimbound%mPQ_ext
    nPQ_ext            => gdp%gdimbound%nPQ_ext
    mPH_int            => gdp%gdimbound%mPH_int
    mPQ_int            => gdp%gdimbound%mPQ_int
    nPH_int            => gdp%gdimbound%nPH_int
    nPQ_int            => gdp%gdimbound%nPQ_int
    mPH_ext            => gdp%gdimbound%mPH_ext
    FORCEuAThPERbnd    => gdp%gdimbound%FORCEuAThPERbnd
    !    As for the water surface: At the H boundary it is prescribed in sud. At the Q boundary if Q is forced instead of u, H is needed to
    !                              compute the right hu that is used to obtain u from Q.
    !                              If FORCEuAThPERbnd = true v is prescribed in both sud and uzd, otherwise I should make s1 periodic. 
    !                              However, I need to prescribe it to have the correct hu (or hv in case the boundary is parallel to x axis)
    !                              that is used in both uzd (for bed friction, but overwritten if FORCEuAThPERbnd = true) and 
    !                              sud (for continuity, always used to compute flux, never overwritten!!!)) !!
    !                              But it should converge at the second iteration in sud so I should be fine,
    !                              except maybe the first call of the first time step in uzd. 
    !                              However, in order to compute the correct ghost, i should also make s1 periodic here. However 
    !                              I am planning to remove water surface ghost points since they are unlikely ever  used at all. Try to remove it and see.
    !                              Note: with FORCEdisch=true I have that hu(nmd)  (i.e. the depth at the boundary) should not be used anymore
    !                      
    if (PERIODICwaterDEPTH) then
       !
       ! Perodic depth
       !
       do k=0,nrPER+1
          !
          ! 0 and +1 because we prescribe also a possible ghost point
          !
          s1(nPH_ext(k),mPH_ext(k)) = s1(nPQ_int(k),mPQ_int(k))+dps(nPQ_int(k),mPQ_int(k))-dps(nPH_ext(k),mPH_ext(k))
          s1(nPQ_ext(k),mPQ_ext(k)) = s1(nPH_int(k),mPH_int(k))+dps(nPH_int(k),mPH_int(k))-dps(nPQ_ext(k),mPQ_ext(k))  
          !if (twoCELLSperiod) no need of two lines of peridoic water surfaces
       enddo          
    else
       !
       ! Perodic water level
       !
       do k=0,nrPER+1
          !
          ! 0 and +1 because we prescribe also a possible ghost point
          !
          s1(nPH_ext(k),mPH_ext(k)) = s1(nPQ_int(k),mPQ_int(k)) 
          s1(nPQ_ext(k),mPQ_ext(k)) = s1(nPH_int(k),mPH_int(k))   
          !if (twoCELLSperiod) no need of two lines of peridoic water surfaces
       enddo 
    endif
   !
end subroutine WATERlevelPERIOD
