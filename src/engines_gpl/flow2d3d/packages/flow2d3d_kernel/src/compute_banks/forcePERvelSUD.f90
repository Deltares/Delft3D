subroutine forcePERvelSUD(u,aa,bb,cc,dd,aak,bbk,cck,ddk,thick,icx,nlb,nub,mlb,mub,kmax, gdp)
            
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
!   Function:   Force the values of aa,bb,cc,dd,aak,bbk,cck,ddk at the periodic H boundary if FORCEuAThPERbnd=true
!               UPDATE MARCH 2015: I THINK THIS SUBR CAN BE REMOVED since kfu is set to zero before it at periodic 
!               velocity points, and FORCEuAThPERbnd will never be true I  guess
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
    integer, dimension(:), pointer :: mPH_extext
    integer, dimension(:), pointer :: mPQ_extext
    integer, dimension(:), pointer :: nPH_extext
    integer, dimension(:), pointer :: nPQ_extext
    integer              , pointer :: shiftHM
    integer              , pointer :: shiftHN
    integer              , pointer :: shiftQM
    integer              , pointer :: shiftQN
    logical              , pointer :: FORCEuAThPERbnd
!
! global variables

  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: aak 
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: bbk
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: cck
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(out)  :: ddk 
!
  real(fp)  , dimension(nlb:nub,mlb:mub)            ,intent(out)  :: bb
  real(fp)  , dimension(nlb:nub,mlb:mub)            ,intent(out)  :: dd
  real(fp)  , dimension(nlb:nub,mlb:mub)            ,intent(out)  :: aa
  real(fp)  , dimension(nlb:nub,mlb:mub)            ,intent(out)  :: cc
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(in)   :: u  
!
  real(fp)  , dimension(kmax)                            ,intent(in)   :: thick
!
  integer, intent(in)        :: icx
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
  integer, intent(in)        :: kmax
 
!
! local variables
!
  integer                    :: k,kk
  integer                    :: nPQext   
  integer                    :: mPQext  
  integer                    :: nPHext  
  integer                    :: mPHext   
  integer                    :: nPQextext  
  integer                    :: mPQextext 
  integer                    :: mPHextext  
  integer                    :: nPHextext  
  real(fp)                   :: umean
!
! executable statements -------------------------------------------------------
!
    nrPER           => gdp%gdimbound%nrPER
    twoCELLSperiod  => gdp%gdimbound%twoCELLSperiod
    mPH_ext         => gdp%gdimbound%mPH_ext
    nPH_ext         => gdp%gdimbound%nPH_ext
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

      !u is the orthogonal velocity (orthogonal to the boundary). I force it 
      do k=1,nrPER !without +1 cause this is the orthogonal velocity 
         IF (FORCEuAThPERbnd) THEN
            nPHext  = nPH_ext(k)+shiftHN
            mPHext  = mPH_ext(k)+shiftHM
            !external H boundary 
            aak(nPHext,mPHext,1:kmax) = 0._fp  
            bbk(nPHext,mPHext,1:kmax) = 1._fp     
            cck(nPHext,mPHext,1:kmax) = 0._fp
            ddk(nPHext,mPHext,1:kmax) = u(nPHext,mPHext,1:kmax) 
            umean = 0._fp
            do kk= 1, kmax
               umean = umean + thick(kk)*u(nPHext,mPHext,kk)
            enddo
            aa(nPHext,mPHext) = 0._fp  
            bb(nPHext,mPHext) = 1._fp     
            cc(nPHext,mPHext) = 0._fp
            dd(nPHext,mPHext) = umean
         ENDIF
!
         if (twoCELLSperiod) THEN
            nPQextext = nPQ_extext(k)+shiftQN
            mPQextext = mPQ_extext(k)+shiftQM
            nPHextext = nPH_extext(k)+shiftHN
            mPHextext = mPH_extext(k)+shiftHM
            !external Q boundary
            aak(nPQextext,mPQextext, 1:kmax) = 0._fp  
            bbk(nPQextext,mPQextext, 1:kmax) = 1._fp     
            cck(nPQextext,mPQextext, 1:kmax) = 0._fp  
            ddk(nPQextext,mPQextext, 1:kmax) = u(nPQextext,mPQextext,1:kmax) 
            !external H boundary 
            aak(nPHextext,mPHextext, 1:kmax) = 0._fp
            bbk(nPHextext,mPHextext, 1:kmax) = 1._fp
            cck(nPHextext,mPHextext, 1:kmax) = 0._fp
            ddk(nPHextext,mPHextext, 1:kmax) = u(nPHextext,mPHextext,1:kmax) 
            !not sure if umean is needed
         endif
   !     
      enddo
 
!
RETURN
end subroutine forcePERvelSUD
