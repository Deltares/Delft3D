subroutine forcePERvelUZD(u,bbk,ddk,aak,buux,bux,bdx,bddx,buuy,buy,bdy,bddy,cck,hdt,icx,nlb,nub,mlb,mub,kmax, gdp)
            
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
!   Function:   Force the values of bbk and ddk at periodic transversal velocity 
!               points
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
    logical              , pointer :: FORCEuAThPERbnd
    logical              , pointer :: PERIODICorthVEL
    logical              , pointer :: PERIODICtangVEL
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: bbk
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: ddk 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: aak 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: buux 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: bux 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: bdx 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: bddx 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: buuy 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: buy 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: bdy 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: bddy 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: cck
!
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(in)   :: u   
  integer, intent(in)        :: icx
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
  integer, intent(in)        :: kmax
  real(fp), intent(in)       :: hdt
 
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
    FORCEuAThPERbnd => gdp%gdimbound%FORCEuAThPERbnd
    PERIODICorthVEL => gdp%gdimbound%PERIODICorthVEL
    PERIODICtangVEL => gdp%gdimbound%PERIODICtangVEL
!  note: i can prescribe only bbk and ddk since all the others are initialized to zero and 
!        they are never modified since in periodic cells velocity points are permanently inactive (kcu=kcv=0)
!
   if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then 
      if (PERIODICtangVEL) then
         !u is the tangential velocity, I force it 
         do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell
            !external Q boundary
            bbk(nPQ_ext(k),mPQ_ext(k), 1:kmax) = 1._fp/hdt  
            ddk(nPQ_ext(k),mPQ_ext(k), 1:kmax) = u(nPQ_ext(k),mPQ_ext(k),1:kmax)/hdt 
            !external H boundary 
            bbk (nPH_ext(k),mPH_ext(k), 1:kmax) = 1._fp/hdt  
            ddk (nPH_ext(k),mPH_ext(k), 1:kmax) = u(nPH_ext(k),mPH_ext(k),1:kmax)/hdt  
!
            aak (nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp !all (or some) needed since at the boundary they are different from zero
            buux(nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            bux (nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            bdx (nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            bddx(nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            buuy(nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            buy (nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            bdy (nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            bddy(nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
            cck (nPH_ext(k),mPH_ext(k),1:kmax) = 0._fp
!            
            aak (nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp !all (or some) needed since at the boundary they are different from zero
            buux(nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            bux (nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            bdx (nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            bddx(nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            buuy(nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            buy (nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            bdy (nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            bddy(nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp
            cck (nPQ_ext(k),mPQ_ext(k),1:kmax) = 0._fp   
!            
            if (twoCELLSperiod) THEN
               nPQextext = nPQ_extext(k) 
               mPQextext = mPQ_extext(k) 
               nPHextext = nPH_extext(k) 
               mPHextext = mPH_extext(k)                 
               !external Q boundary
               bbk(nPQ_extext(k),mPQ_extext(k), 1:kmax) = 1._fp/hdt  
               ddk(nPQ_extext(k),mPQ_extext(k), 1:kmax) = u(nPQ_extext(k),mPQ_extext(k),1:kmax)/hdt 
               !external H boundary 
               bbk(nPH_extext(k),mPH_extext(k), 1:kmax) = 1._fp/hdt  
               ddk(nPH_extext(k),mPH_extext(k), 1:kmax) = u(nPH_extext(k),mPH_extext(k),1:kmax)/hdt  
!               
               aak (nPHextext,mPHextext,1:kmax) = 0._fp ! they are needed since mom_cycl is entered with kfu at periodic external points
               buux(nPHextext,mPHextext,1:kmax) = 0._fp
               bux (nPHextext,mPHextext,1:kmax) = 0._fp
               bdx (nPHextext,mPHextext,1:kmax) = 0._fp
               bddx(nPHextext,mPHextext,1:kmax) = 0._fp
               buuy(nPHextext,mPHextext,1:kmax) = 0._fp
               buy (nPHextext,mPHextext,1:kmax) = 0._fp
               bdy (nPHextext,mPHextext,1:kmax) = 0._fp
               bddy(nPHextext,mPHextext,1:kmax) = 0._fp
               cck (nPHextext,mPHextext,1:kmax) = 0._fp              
               aak (nPQextext,mPQextext,1:kmax) = 0._fp ! they are needed since mom_cycl is entered with kfu at periodic external points
               buux(nPQextext,mPQextext,1:kmax) = 0._fp
               bux (nPQextext,mPQextext,1:kmax) = 0._fp
               bdx (nPQextext,mPQextext,1:kmax) = 0._fp
               bddx(nPQextext,mPQextext,1:kmax) = 0._fp
               buuy(nPQextext,mPQextext,1:kmax) = 0._fp
               buy (nPQextext,mPQextext,1:kmax) = 0._fp
               bdy (nPQextext,mPQextext,1:kmax) = 0._fp
               bddy(nPQextext,mPQextext,1:kmax) = 0._fp
               cck (nPQextext,mPQextext,1:kmax) = 0._fp                   
            endif
         enddo
      endif
   else   
      if (PERIODICorthVEL) then
      !u is the orthogonal velocity (orthogonal to the boundary). I force it 
         do k=1,nrPER !without +1 cause this is the orthogonal velocity 
            !external Q boundary
            nPQext  = nPQ_ext(k)+shiftQN
            mPQext  = mPQ_ext(k)+shiftQM
            nPHext  = nPH_ext(k)+shiftHN
            mPHext  = mPH_ext(k)+shiftHM
        !    bbk(nPQext,mPQext,1:kmax) = 1._fp/hdt                !note I do not need to force it at Q boundary since there velocity is already forced (even if explicitely)
        !    ddk(nPQext,mPQext,1:kmax) = u(nPQext,mPQext,1:kmax)/hdt  !note I do not need to force it at Q boundary since there velocity is already forced (even if explicitely)
            !external H boundary 
            if (FORCEuAThPERbnd) then
               bbk (nPHext,mPHext,1:kmax) = 1._fp/hdt  
               ddk (nPHext,mPHext,1:kmax) = u(nPHext,mPHext,1:kmax)/hdt  
               aak (nPHext,mPHext,1:kmax) = 0._fp !all (or some) needed since at the boundary they are different from zero
               buux(nPHext,mPHext,1:kmax) = 0._fp
               bux (nPHext,mPHext,1:kmax) = 0._fp
               bdx (nPHext,mPHext,1:kmax) = 0._fp
               bddx(nPHext,mPHext,1:kmax) = 0._fp
               buuy(nPHext,mPHext,1:kmax) = 0._fp
               buy (nPHext,mPHext,1:kmax) = 0._fp
               bdy (nPHext,mPHext,1:kmax) = 0._fp
               bddy(nPHext,mPHext,1:kmax) = 0._fp
               cck (nPHext,mPHext,1:kmax) = 0._fp
            endif
   !
            if (twoCELLSperiod) THEN
               nPQextext = nPQ_extext(k)+shiftQN
               mPQextext = mPQ_extext(k)+shiftQM
               nPHextext = nPH_extext(k)+shiftHN
               mPHextext = mPH_extext(k)+shiftHM
               !external Q boundary
               bbk(nPQextext,mPQextext,1:kmax) = 1._fp/hdt  
               ddk(nPQextext,mPQextext,1:kmax) = u(nPQextext,mPQextext,1:kmax)/hdt 
               !external H boundary 
               bbk(nPHextext,mPHextext,1:kmax) = 1._fp/hdt  
               ddk(nPHextext,mPHextext,1:kmax) = u(nPHextext,mPHextext,1:kmax)/hdt  
               ! 
               aak (nPHextext,mPHextext,1:kmax) = 0._fp ! they are needed since mom_cycl is entered with kfu at periodic external points
               buux(nPHextext,mPHextext,1:kmax) = 0._fp
               bux (nPHextext,mPHextext,1:kmax) = 0._fp
               bdx (nPHextext,mPHextext,1:kmax) = 0._fp
               bddx(nPHextext,mPHextext,1:kmax) = 0._fp
               buuy(nPHextext,mPHextext,1:kmax) = 0._fp
               buy (nPHextext,mPHextext,1:kmax) = 0._fp
               bdy (nPHextext,mPHextext,1:kmax) = 0._fp
               bddy(nPHextext,mPHextext,1:kmax) = 0._fp
               cck (nPHextext,mPHextext,1:kmax) = 0._fp              
               aak (nPQextext,mPQextext,1:kmax) = 0._fp ! they are needed since mom_cycl is entered with kfu at periodic external points
               buux(nPQextext,mPQextext,1:kmax) = 0._fp
               bux (nPQextext,mPQextext,1:kmax) = 0._fp
               bdx (nPQextext,mPQextext,1:kmax) = 0._fp
               bddx(nPQextext,mPQextext,1:kmax) = 0._fp
               buuy(nPQextext,mPQextext,1:kmax) = 0._fp
               buy (nPQextext,mPQextext,1:kmax) = 0._fp
               bdy (nPQextext,mPQextext,1:kmax) = 0._fp
               bddy(nPQextext,mPQextext,1:kmax) = 0._fp
               cck (nPQextext,mPQextext,1:kmax) = 0._fp                 
            endif
         enddo
      endif
    
!
   endif
!
RETURN
end subroutine forcePERvelUZD
