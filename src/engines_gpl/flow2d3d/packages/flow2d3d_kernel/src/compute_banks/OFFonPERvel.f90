subroutine OFFonPERvel(kfu,kfv,icx,nlb,nub,mlb,mub,kvalue, gdp)
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
!  Function:   Activate of deactivate kfu/kfv at periodic velocity boundaries
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
    integer              , pointer :: PERIODalongM
    logical              , pointer :: twoCELLSperiod
    integer, dimension(:), pointer :: mPH_ext
    integer, dimension(:), pointer :: nPH_ext
    integer, dimension(:), pointer :: mPQ_ext
    integer, dimension(:), pointer :: nPQ_ext
    integer, dimension(:), pointer :: mPH_extext
    integer, dimension(:), pointer :: mPQ_extext
    integer, dimension(:), pointer :: nPH_extext
    integer, dimension(:), pointer :: nPQ_extext
    integer              , pointer :: shiftHM
    integer              , pointer :: shiftHN
    integer              , pointer :: shiftQM
    integer              , pointer :: shiftQN
    logical              , pointer :: PERIODICorthVEL
    logical              , pointer :: PERIODICtangVEL
!
! global variables
!
  integer, dimension(nlb:nub,mlb:mub)            ,intent(out)  :: kfu 
  integer, dimension(nlb:nub,mlb:mub)            ,intent(out)  :: kfv
  integer, intent(in)        :: kvalue
  integer, intent(in)        :: icx
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub 
!
! local variables
!
  integer                    :: k  
  integer                    :: nPQext   
  integer                    :: mPQext  
  integer                    :: nPHext  
  integer                    :: mPHext   
  integer                    :: nPQextext  
  integer                    :: mPQextext 
  integer                    :: mPHextext  
  integer                    :: nPHextext 
!
! executable statements -------------------------------------------------------
!
    nrPER           => gdp%gdimbound%nrPER
    PERIODalongM    => gdp%gdimbound%PERIODalongM
    twoCELLSperiod  => gdp%gdimbound%twoCELLSperiod
    mPH_ext         => gdp%gdimbound%mPH_ext
    nPH_ext         => gdp%gdimbound%nPH_ext
    mPQ_ext         => gdp%gdimbound%mPQ_ext
    nPQ_ext         => gdp%gdimbound%nPQ_ext
    mPH_extext      => gdp%gdimbound%mPH_extext
    mPH_ext         => gdp%gdimbound%mPH_ext
    mPQ_extext      => gdp%gdimbound%mPQ_extext
    nPH_extext      => gdp%gdimbound%nPH_extext
    nPQ_extext      => gdp%gdimbound%nPQ_extext
    shiftHM         => gdp%gdimbound%shiftHM
    shiftHN         => gdp%gdimbound%shiftHN
    shiftQM         => gdp%gdimbound%shiftQM
    shiftQN         => gdp%gdimbound%shiftQN
    PERIODICorthVEL => gdp%gdimbound%PERIODICorthVEL
    PERIODICtangVEL => gdp%gdimbound%PERIODICtangVEL
!     here kfv is always the location of the tangential velocity at the bounadary, and kfu the othogonal.
!
      do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell
         if(PERIODICtangVEL) then
            !external Q boundary
            kfv(nPQ_ext(k),mPQ_ext(k)) = kvalue  
            !external H boundary 
            kfv(nPH_ext(k),mPH_ext(k)) = kvalue    
         endif
         if(PERIODICorthVEL) then
            !external Q boundary
           ! kfu(nPQ_ext(k)+shiftQN,mPQ_ext(k)+shiftQM) = kvalue !already on and I do not have to turn it off!
            !external H boundary 
           ! kfu(nPHext+shiftHN,mPHext+shiftHM) = kvalue  !already on and I do not have to turn it off!  
         endif
         if (twoCELLSperiod) THEN
            if(PERIODICorthVEL) then
               if (k.ne.nrPER+1) then ! u does not have to be prescribed outside the boundary (k=nrPER+1)
                  !external Q boundary
                   kfu(nPQ_extext(k)+shiftQN,mPQ_extext(k)+shiftQM) = kvalue
                  !external H boundary 
                   kfu(nPH_extext(k)+shiftHN,mPH_extext(k)+shiftHM) = kvalue
               endif
            endif
            if(PERIODICtangVEL) then
               !external Q boundary
               kfv(nPQ_extext(k)        ,mPQ_extext(k)        ) = kvalue
               !external H boundary 
               kfv(nPH_extext(k)        ,mPH_extext(k)        ) = kvalue
            endif
         endif
      enddo 

!      do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell   
!         if (PERIODalongM==1) then !I am solving in the opposite direction of periodic BC (i.e. opposite direction of axis of the channel)n
!            if (icx==1) then !solving vel along y
!               !external Q boundary
!               kfu(nPQ_ext(k),mPQ_ext(k)) = kvalue
!               kfv(nPQ_ext(k),mPQ_ext(k)) = kvalue  !needed for computing velocity at the boundary as average of 4 adjacents
!               !external H boundary 
!               kfu(nPH_ext(k),mPH_ext(k)) = kvalue
!               kfv(nPH_ext(k),mPH_ext(k)) = kvalue  !needed for computing velocity at the boundary as average of 4 adjacents
!!
!               if (twoCELLSperiod) THEN
!                  !external Q boundary
!                  kfu(nPQ_extext(k),mPQ_extext(k)) = kvalue
!                  !external H boundary 
!                  kfu(nPH_extext(k),mPH_extext(k)) = kvalue
!                 !kfv(nPH_extext(k),mPH_extext(k)) not needed  
!               endif
!
!            elseif (icx/=1) then !solving vel along x
!               
!               !external Q boundary
!               kfu(nPQ_ext(k),mPQ_ext(k)) = kvalue
!               kfv(nPQ_ext(k),mPQ_ext(k)) = kvalue   !needed for computing velocity at the boundary as average of 4 adjacents
!               !external H boundary 
!               kfu(nPH_ext(k),mPH_ext(k)) = kvalue !not really needed since advection is upwind
!               kfv(nPH_ext(k),mPH_ext(k)) = kvalue !needed for computing velocity at the boundary as average of 4 adjacents
!
!               if (twoCELLSperiod) THEN
!                  !external Q boundary
!                  kfu(nPQ_extext(k),mPQ_extext(k)) = kvalue    !not really needed since advection is upwind
!                  !external H boundary 
!                  kfu(nPH_extext(k),mPH_extext(k)) = kvalue    !not really needed since advection is upwind
!                  ! here kfv(nPH_extext(k),mPH_extext(k)) is not needed since in the advective terms its never used
!               endif
!            endif
!         else ! periodic along y
!            if (icx==1) then !solving vel along y
!               !external Q boundary
!               kfu(nPQ_ext(k),mPQ_ext(k)) = kvalue
!               kfv(nPQ_ext(k),mPQ_ext(k)) = kvalue  !needed for computing velocity at the boundary as average of 4 adjacents
!               !external H boundary 
!               kfu(nPH_ext(k),mPH_ext(k)) = kvalue
!               kfv(nPH_ext(k),mPH_ext(k)) = kvalue  !needed for computing velocity at the boundary as average of 4 adjacents
!!
!               if (twoCELLSperiod) THEN
!                  !external Q boundary
!                  kfu(nPQ_extext(k),mPQ_extext(k)) = kvalue
!                  !external H boundary 
!                  kfu(nPH_extext(k),mPH_extext(k)) = kvalue
!                 !kfv(nPH_extext(k),mPH_extext(k)) not needed  
!               endif
!
!            elseif (icx/=1) then !solving vel along x
!               
!               !external Q boundary
!               kfu(nPQ_ext(k),mPQ_ext(k)) = kvalue
!               kfv(nPQ_ext(k),mPQ_ext(k)) = kvalue   !needed for computing velocity at the boundary as average of 4 adjacents
!               !external H boundary 
!               kfu(nPH_ext(k),mPH_ext(k)) = kvalue !not really needed since advection is upwind
!               kfv(nPH_ext(k),mPH_ext(k)) = kvalue !needed for computing velocity at the boundary as average of 4 adjacents
!
!               if (twoCELLSperiod) THEN
!                  !external Q boundary
!                  kfu(nPQ_extext(k),mPQ_extext(k)) = kvalue    !not really needed since advection is upwind
!                  !external H boundary 
!                  kfu(nPH_extext(k),mPH_extext(k)) = kvalue    !not really needed since advection is upwind
!                  ! here kfv(nPH_extext(k),mPH_extext(k)) is not needed since in the advective terms its never used
!               endif
!            endif
!            
!         endif
!      enddo
!!
RETURN
end subroutine OFFonPERvel
