subroutine velocityPERIOD(u1,v1,icx,nlb,nub,mlb,mub,kmax, gdp)
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
    integer              , pointer :: PERIODalongM
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
    logical              , pointer :: perHwaterRIGHT
    logical              , pointer :: FORCEuAThPERbnd
    logical              , pointer :: PERIODICorthVEL
    logical              , pointer :: PERIODICtangVEL
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(inout)   :: u1  
  real(fp)  , dimension(nlb:nub,mlb:mub,kmax)            ,intent(inout)   :: v1
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
    PERIODalongM    => gdp%gdimbound%PERIODalongM
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
    perHwaterRIGHT  => gdp%gdimbound%perHwaterRIGHT
    FORCEuAThPERbnd => gdp%gdimbound%FORCEuAThPERbnd
    PERIODICorthVEL => gdp%gdimbound%PERIODICorthVEL
    PERIODICtangVEL => gdp%gdimbound%PERIODICtangVEL
!
!     this is the rolled modality, to be used by calling the subroutine with (u,v,....) and by switching u and v       
!     if (.not.(icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) in the way
!     that u1 is always the perpendicular to the boundary while v1 is always the tangential component
!     u1 has to be shifted depending from the location of the boundaries.
!     Some of this are not needed, but it is a easier and compact code. See the unrolled code commented below
!
!                            
      do k=0,nrPER+1 !+1 cause  I copy the last value on the lower edge of the cell, and the ghost value. Same for 0. The v in zero should not be necessary.
       ! note: the same shift (shiftQM,shiftQN) is used for both right and left hand sides when prescribing something at Q bounadry. The same for H boundary.
         if (FORCEuAThPERbnd) then
            u1(nPH_ext(k)+shiftHN,mPH_ext(k)+shiftHM,1:kmax) = u1(nPQ_int(k)+shiftHN,mPQ_int(k)+shiftHM,1:kmax) ! i write this first, so in case I am hortogonal to the boundary I copy veloc from dich edge to veloc to H edge and then back so nothing really happens to the Q edge
            u1(nPQ_ext(k)+shiftQN,mPQ_ext(k)+shiftQM,1:kmax) = u1(nPH_int(k)+shiftQN,mPH_int(k)+shiftQM,1:kmax)
         endif
         if (PERIODICtangVEL) then
            v1(nPH_ext(k),mPH_ext(k),1:kmax) = v1(nPQ_int(k),mPQ_int(k),1:kmax) 
            v1(nPQ_ext(k),mPQ_ext(k),1:kmax) = v1(nPH_int(k),mPH_int(k),1:kmax)  
         endif 
         if (twoCELLSperiod) THEN
            if (PERIODICorthVEL) then
               u1(nPH_extext(k)+shiftHN,mPH_extext(k)+shiftHM,1:kmax) =  u1(nPQ_intint(k)+shiftHN,mPQ_intint(k)+shiftHM,1:kmax) 
               u1(nPQ_extext(k)+shiftQN,mPQ_extext(k)+shiftQM,1:kmax) =  u1(nPH_intint(k)+shiftQN,mPH_intint(k)+shiftQM,1:kmax) ! not used if waqua 
            endif
            if (PERIODICtangVEL) then
               v1(nPH_extext(k),mPH_extext(k),1:kmax) =  v1(nPQ_intint(k),mPQ_intint(k),1:kmax)
               v1(nPQ_extext(k),mPQ_extext(k),1:kmax) =  v1(nPH_intint(k),mPH_intint(k),1:kmax) 
            endif 
         endif
      enddo 
!
!     this is the unrolled modality, to be used by calling the subroutine with (u,v,....) without swtiching u and v
!
!      !a check is done at the beginning to be sure that BC are ordered from the biggest to the smallest m (if along y) or n (if along x)
!      !here  I prescribe the last velocity value on the lower edge of the cell
!      if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then 
!         if (perHwaterRIGHT) then
!            do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell
!               u1(nPQ_ext(k),mPQ_ext(k),1:kmax) =  u1(nPH_int(k),mPH_int(k),1:kmax)
!               u1(nPH_ext(k),mPH_ext(k),1:kmax) =  u1(nPQ_int(k),mPQ_int(k),1:kmax) 
!               !v1(nPQ_ext(k),mPQ_ext(k)-1,1:kmax) is the imposed BC
!               v1(nPH_ext(k),mPH_ext(k),1:kmax) =  v1(nPQ_int(k),mPQ_int(k),1:kmax)    
!               if (twoCELLSperiod) THEN
!                  u1(nPQ_extext(k),mPQ_extext(k),1:kmax) =  u1(nPH_intint(k),mPH_intint(k),1:kmax) ! not used if waqua 
!                  u1(nPH_extext(k),mPH_extext(k),1:kmax) =  u1(nPQ_intint(k),mPQ_intint(k),1:kmax) 
!                 ! v1(nPQ_extext(k)+shiftQN,mPQ_extext(k)+shiftQM,1:kmax) =  v1(nPH_intint(k)+shiftQN,mPH_intint(k)+shiftQM,1:kmax) !never used 
!                 ! v1(nPH_extext(k),mPH_extext(k),1:kmax) =  v1(nPQ_intint(k),mPQ_intint(k),1:kmax) !never used 
!               endif
!            enddo 
!         else
!            do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell
!               u1(nPQ_ext(k),mPQ_ext(k),1:kmax) =  u1(nPH_int(k),mPH_int(k),1:kmax)
!               u1(nPH_ext(k),mPH_ext(k),1:kmax) =  u1(nPQ_int(k),mPQ_int(k),1:kmax) 
!               !v1(nPQ_ext(k),mPQ_ext(k),1:kmax) is the imposed BC
!               v1(nPH_ext(k)+shiftHN,mPH_ext(k)+shiftHM,1:kmax) =  v1(nPQ_int(k)+shiftHN,mPQ_int(k)+shiftHM,1:kmax) ! u perpend to boundary is shifted
!               if (twoCELLSperiod) THEN
!                  u1(nPQ_extext(k),mPQ_extext(k),1:kmax) =  u1(nPH_intint(k),mPH_intint(k),1:kmax)  ! not used if waqua 
!                  u1(nPH_extext(k),mPH_extext(k),1:kmax) =  u1(nPQ_intint(k),mPQ_intint(k),1:kmax)  
!            !      v1(nPQ_extext(k),mPQ_extext(k),1:kmax) =  v1(nPH_intint(k),mPH_intint(k),1:kmax)     !never used
!           !       v1(nPH_extext(k),mPH_extext(k)-1,1:kmax) =  v1(nPQ_intint(k),mPQ_intint(k)-1,1:kmax) !never used 
!               endif
!            enddo 
!         endif 
!      else ! if (.not. ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) ) then
!         if (perHwaterRIGHT) then
!            do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell
!            !   u1(nPQ_ext(k),mPQ_ext(k)-1,1:kmax) is the imposed BC
!               u1(nPH_ext(k),mPH_ext(k),1:kmax) =  u1(nPQ_int(k),mPQ_int(k),1:kmax) 
!               v1(nPQ_ext(k),mPQ_ext(k),1:kmax) =  v1(nPH_int(k),mPH_int(k),1:kmax) ! never used since forced u as BC
!               v1(nPH_ext(k),mPH_ext(k),1:kmax) =  v1(nPQ_int(k),mPQ_int(k),1:kmax) ! never used since forced peridic u 
!               if (twoCELLSperiod) THEN
!                  u1(nPQ_extext(k)+shiftQN,mPQ_extext(k)+shiftQM,1:kmax) =  u1(nPH_intint(k)+shiftQN,mPH_intint(k)+shiftQM,1:kmax)  
!                  u1(nPH_extext(k),mPH_extext(k),1:kmax) =  u1(nPQ_intint(k),mPQ_intint(k),1:kmax) ! not used since water exiting and upwind. With waqua not used either.  
!                !  v1(nPQ_extext(k),mPQ_extext(k),1:kmax) =  v1(nPH_intint(k),mPH_intint(k),1:kmax) !never used
!                !  v1(nPH_extext(k),mPH_extext(k),1:kmax) =  v1(nPQ_intint(k),mPQ_intint(k),1:kmax) !never used
!               endif
!            enddo 
!         else
!            do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell
!               !u1(nPQ_ext(k),mPQ_ext(k),1:kmax) =  u1(nPH_int(k),mPH_int(k),1:kmax) prescribed BC
!               u1(nPH_ext(k)+shiftHN,mPH_ext(k)+shiftHM,1:kmax) =  u1(nPQ_int(k)+shiftHN,mPQ_int(k)+shiftHM,1:kmax) 
!              ! v1(nPQ_ext(k),mPQ_ext(k),1:kmax) ! never used since forced u as BC
!               v1(nPH_ext(k),mPH_ext(k),1:kmax) =  v1(nPQ_int(k),mPQ_int(k),1:kmax) ! never used since forced peridic u 
!               if (twoCELLSperiod) THEN
!                  u1(nPQ_extext(k),mPQ_extext(k),1:kmax) =  u1(nPH_intint(k),mPH_intint(k),1:kmax)
!                  u1(nPH_extext(k)+shiftHN,mPH_extext(k)+shiftHM,1:kmax) =  u1(nPQ_intint(k)+shiftHN,mPQ_intint(k)+shiftHM,1:kmax)  
!                  !v1(nPQ_extext(k),mPQ_extext(k),1:kmax) =  v1(nPH_intint(k),mPH_intint(k),1:kmax)  !never used
!                  !v1(nPH_extext(k),mPH_extext(k),1:kmax) =  v1(nPQ_intint(k),mPQ_intint(k),1:kmax) !never used
!               endif
!            enddo 
!         endif 
!
!      endif
!
RETURN
end subroutine velocityPERIOD
