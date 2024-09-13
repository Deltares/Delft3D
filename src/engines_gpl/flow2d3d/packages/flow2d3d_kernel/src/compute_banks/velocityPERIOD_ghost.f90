subroutine velocityPERIOD_ghost(u1,v1,icx,nlb,nub,mlb,mub,kmax, gdp)
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
!   Function:   Copy the velocity ghost points from 
!               the water level internal boundary to the water discharge external one,
!               and from the water discharge internal boundary to the water level external one. 
!               Orthogonal velocity are copied from the boundary in which the normal to the 
!               immersed boundary points inside the domain, since on the other side it points 
!               outside and wrong interpolation is performed.
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
    integer                , pointer :: nrPER
    integer                , pointer :: PERIODalongM
    logical                , pointer :: twoCELLSperiod
    integer, dimension(:)  , pointer :: mPH_ext
    integer, dimension(:)  , pointer :: nPH_ext
    integer, dimension(:)  , pointer :: mPQ_ext
    integer, dimension(:)  , pointer :: nPQ_ext
    integer, dimension(:)  , pointer :: mPH_int
    integer, dimension(:)  , pointer :: mPQ_int
    integer, dimension(:)  , pointer :: mPH_intint
    integer, dimension(:)  , pointer :: mPQ_intint
    integer, dimension(:)  , pointer :: nPH_int
    integer, dimension(:)  , pointer :: nPQ_int
    integer, dimension(:)  , pointer :: nPH_intint
    integer, dimension(:)  , pointer :: nPQ_intint
    integer, dimension(:)  , pointer :: mPH_extext
    integer, dimension(:)  , pointer :: mPQ_extext
    integer, dimension(:)  , pointer :: nPH_extext
    integer, dimension(:)  , pointer :: nPQ_extext
    integer                , pointer :: shiftHM
    integer                , pointer :: shiftHN
    integer                , pointer :: shiftQM
    integer                , pointer :: shiftQN
    logical                , pointer :: perHwaterRIGHT
    logical                , pointer :: PERIODICorthVEL
    logical                , pointer :: PERIODICtangVEL
    integer, dimension(:,:), pointer :: ghostU1
    integer, dimension(:,:), pointer :: FROMmnTOghostU1
    real(fp), dimension(:) , pointer :: nxG_U1
    real(fp), dimension(:) , pointer :: nyG_U1
    logical, dimension(:)  , pointer :: COPY_H_TO_Q
    integer                , pointer :: TYPEfreeSLIP
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
  integer                    :: i
  integer                    :: k 
  integer                    :: mH_ext 
  integer                    :: nH_ext 
  integer                    :: mQ_ext  
  integer                    :: nQ_ext   
  integer                    :: mH_int  
  integer                    :: nH_int  
  integer                    :: mQ_int  
  integer                    :: nQ_int
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
    PERIODICorthVEL => gdp%gdimbound%PERIODICorthVEL
    PERIODICtangVEL => gdp%gdimbound%PERIODICtangVEL
    ghostU1         => gdp%gdimbound%ghostU1
    FROMmnTOghostU1 => gdp%gdimbound%FROMmnTOghostU1
    nxG_U1          => gdp%gdimbound%nxG_U1
    nyG_U1          => gdp%gdimbound%nyG_U1
    COPY_H_TO_Q     => gdp%gdimbound%COPY_H_TO_Q
    TYPEfreeSLIP    => gdp%gdimbound%TYPEfreeSLIP
      do k=0,nrPER+1 !+1 cause  I copy the last value on the lower edge of the cell, and the ghost value. Same for 0. The v in zero should not be necessary.
       ! note: the same shift (shiftQM,shiftQN) is used for both right and left hand sides when prescribing something at Q bounadry. The same for H boundary.
         mH_ext = mPH_ext(k)+shiftHM  !note for u1 int and ext concide (it is the boundary edge)
         nH_ext = nPH_ext(k)+shiftHN
         mQ_ext = mPQ_ext(k)+shiftQM
         nQ_ext = nPQ_ext(k)+shiftQN
         mH_int = mPH_int(k)+shiftQM
         nH_int = nPH_int(k)+shiftQN
         mQ_int = mPQ_int(k)+shiftHM
         nQ_int = nPQ_int(k)+shiftHN
!         !these bunch of ifs can be optimized if called using vectors (nm) not matrices (n,m)
!         !they are only valid for mesh that has gridlines alinged with x. Otherwise a normal (npsiG_U1,netaG_U1) should be defined instead of (nxG_U1,nyG_U1)
!         if (ghostU1(nH_ext,mH_ext)==1) then
!            i = FROMmnTOghostU1(nH_ext,mH_ext)
!            if (PERIODalongM==1) then
!               if (perHwaterRIGHT) then !water is for larger m
!                 ! posNORM = nxG_U1(i) .gt. 0._fp
!                !  waterABOVE = ghostU1(nH_ext+1,mH_ext) == 0
!                 ! if (posNORM.EQV.waterABOVE) then
!                  if (nxG_U1(i) .gt. 0._fp) then
!                     COPY_H_TO_Q = .true.
!                  else
!                     COPY_H_TO_Q = .false.
!                  endif
!               else
!                  if (nxG_U1(i) .gt. 0._fp) then
!                     COPY_H_TO_Q = .false.
!                  else
!                     COPY_H_TO_Q = .true.
!                  endif
!               endif
!            else
!               if (perHwaterRIGHT) then !water is for larger m
!                  if (nyG_U1(i)  .gt. 0._fp) then
!                     COPY_H_TO_Q = .true.
!                  else
!                     COPY_H_TO_Q = .false.
!                  endif
!               else
!                  if (nyG_U1(i)  .gt. 0._fp) then
!                     COPY_H_TO_Q = .false.
!                  else
!                     COPY_H_TO_Q = .true.
!                  endif
!               endif
!            endif
         if (TYPEfreeSLIP/=2) then
            if (COPY_H_TO_Q(k)) then
               if (ghostU1(nQ_ext,mQ_ext)==1) u1(nQ_ext,mQ_ext,1:kmax) = u1(nH_int,mH_int,1:kmax)
            else
               if (ghostU1(nH_ext,mH_ext)==1) u1(nH_ext,mH_ext,1:kmax) = u1(nQ_int,mQ_int,1:kmax) ! i write this first, so in case I am hortogonal to the boundary I copy veloc from dich edge to veloc to H edge and then back so nothing really happens to the Q edge
            endif
         else !TYPEfreeSLIP/=2
            !here I should be ok, the hartman condition has been used and if all the needed fluid velocity point are made periodic, the value of the ghost cell should be the same at both the Q and H boundary
         endif         
!            
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

RETURN
end subroutine velocityPERIOD_ghost
