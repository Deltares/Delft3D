subroutine extrapPERIODICghost(gdp)
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
!  Ndryact: delft3d.support@deltares.nl                                         
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
!   Function:  extrapolate ghost cells to the respective external periodic boundary 
!               
!    Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: nrPER
    integer                 , pointer :: PERIODalongM
    integer, dimension(:,:) , pointer :: FROMmnTOghostU1
    integer, dimension(:,:) , pointer :: FROMmnTOghostV1
    integer, dimension(:,:) , pointer :: ghostU1
    integer, dimension(:,:) , pointer :: ghostV1
    real(fp), dimension(:,:), pointer :: ETAx
    integer, dimension(:)   , pointer :: mPH_ext
    integer, dimension(:)   , pointer :: nPH_ext
    integer, dimension(:)   , pointer :: mPQ_ext
    integer, dimension(:)   , pointer :: nPQ_ext
    integer, dimension(:)   , pointer :: mPH_int
    integer, dimension(:)   , pointer :: mPQ_int
    integer, dimension(:)   , pointer :: mPH_intint
    integer, dimension(:)   , pointer :: mPQ_intint
    integer, dimension(:)   , pointer :: nPH_int
    integer, dimension(:)   , pointer :: nPQ_int
    integer, dimension(:)   , pointer :: nPH_intint
    integer, dimension(:)   , pointer :: nPQ_intint
    integer, dimension(:)   , pointer :: mPH_extext
    integer, dimension(:)   , pointer :: mPQ_extext
    integer, dimension(:)   , pointer :: nPH_extext
    integer, dimension(:)   , pointer :: nPQ_extext
    logical                 , pointer :: twoCELLSperiod
    integer                 , pointer :: shiftHM
    integer                 , pointer :: shiftHN
    integer                 , pointer :: shiftQM
    integer                 , pointer :: shiftQN
    logical, dimension(:)   , pointer :: COPY_H_TO_Q
    logical                 , pointer :: perHwaterRIGHT
    real(fp), dimension(:)  , pointer :: nxG_U1
    real(fp), dimension(:)  , pointer :: nYG_U1
    real(fp), dimension(:,:), pointer :: xG_U1
    real(fp), dimension(:,:), pointer :: yG_U1
    real(fp), dimension(:)  , pointer :: xBIu1
    real(fp), dimension(:)  , pointer :: yBIu1
    real(fp), dimension(:,:), pointer :: xG_V1
    real(fp), dimension(:,:), pointer :: yG_V1
    real(fp), dimension(:,:), pointer :: shiftBIu_x
    real(fp), dimension(:,:), pointer :: shiftBIu_y
    real(fp), dimension(:,:), pointer :: shiftBIv_x
    real(fp), dimension(:,:), pointer :: shiftBIv_y
    integer                 , pointer :: TYPEfreeSLIP
!
! global variables
! 
!
! local variables
!
  integer                    :: iH_U1
  integer                    :: iQ_U1
  integer                    :: iH_V1
  integer                    :: iQ_V1
  integer                    :: k
  integer                    :: mH_ext  
  integer                    :: nH_ext
  integer                    :: mQ_ext
  integer                    :: nQ_ext
  integer                    :: mH_int  
  integer                    :: nH_int
  integer                    :: mQ_int
  integer                    :: nQ_int
  integer                    :: mH_extext  
  integer                    :: nH_extext
  integer                    :: mQ_extext
  integer                    :: nQ_extext
  integer                    :: mH_intint
  integer                    :: nH_intint
  integer                    :: mQ_intint
  integer                    :: nQ_intint
  logical                    :: hasQghostU
  logical                    :: isGHOST_H
  logical                    :: isGHOST_Q
!
! executable statements -------------------------------------------------------
!
    nrPER           => gdp%gdimbound%nrPER
    PERIODalongM    => gdp%gdimbound%PERIODalongM
    FROMmnTOghostU1 => gdp%gdimbound%FROMmnTOghostU1
    FROMmnTOghostV1 => gdp%gdimbound%FROMmnTOghostV1
    ghostU1         => gdp%gdimbound%ghostU1
    ghostV1         => gdp%gdimbound%ghostV1
    ETAx            => gdp%gdimbound%ETAx
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
    twoCELLSperiod  => gdp%gdimbound%twoCELLSperiod
    shiftHM         => gdp%gdimbound%shiftHM
    shiftHN         => gdp%gdimbound%shiftHN
    shiftQM         => gdp%gdimbound%shiftQM
    shiftQN         => gdp%gdimbound%shiftQN
    COPY_H_TO_Q     => gdp%gdimbound%COPY_H_TO_Q
    perHwaterRIGHT  => gdp%gdimbound%perHwaterRIGHT
    nxG_U1          => gdp%gdimbound%nxG_U1
    nYG_U1          => gdp%gdimbound%nYG_U1
    xG_U1           => gdp%gdimbound%xG_U1
    yG_U1           => gdp%gdimbound%yG_U1
    xBIu1           => gdp%gdimbound%xBIu1
    yBIu1           => gdp%gdimbound%yBIu1
    nxG_U1          => gdp%gdimbound%nxG_U1
    nyG_U1          => gdp%gdimbound%nyG_U1
    xG_V1           => gdp%gdimbound%xG_V1
    yG_V1           => gdp%gdimbound%yG_V1
    xG_U1           => gdp%gdimbound%xG_U1
    yG_U1           => gdp%gdimbound%yG_U1
    shiftBIu_x      => gdp%gdimbound%shiftBIu_x
    shiftBIu_y      => gdp%gdimbound%shiftBIu_y
    shiftBIv_x      => gdp%gdimbound%shiftBIv_x
    shiftBIv_y      => gdp%gdimbound%shiftBIv_y
    TYPEfreeSLIP    => gdp%gdimbound%TYPEfreeSLIP
    shiftBIu_x = 0._fp
    shiftBIu_y = 0._fp
    shiftBIv_x = 0._fp
    shiftBIv_y = 0._fp
    do k=0,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell, and also in the upper 
       !  
       ! Periodic U (orthogonal to boundary)
       !
       ! First determine COPY_H_TO_Q, based on the direction of the bank. If normal points to the water, interpolation can be done
       ! and I copy values from that ghost 
       !
       mH_ext = mPH_ext(k)+shiftHM  !note for u1 int and ext concide (it is the boundary edge)
       nH_ext = nPH_ext(k)+shiftHN
       mQ_ext = mPQ_ext(k)+shiftQM
       nQ_ext = nPQ_ext(k)+shiftQN
       mH_int = mPH_int(k)+shiftQM
       nH_int = nPH_int(k)+shiftQN
       mQ_int = mPQ_int(k)+shiftHM
       nQ_int = nPQ_int(k)+shiftHN
       !
       if (comparereal(ETAx(nH_ext,mH_ext),0._fp).ne.0) then
          !
          ! IMPORTANT!!they are only valid for mesh that has gridlines alinged with x. 
          ! Otherwise a normal (npsiG_U1,netaG_U1) should be defined instead of (nxG_U1,nyG_U1)
          !
          write(*,*) 'Periodic ghosts only valid if gridlines alinged with x '
          call d3stop(1, gdp)
       endif         
       if (PERIODalongM==1) then
          isGHOST_H = ghostU1(nH_ext,mH_ext)==1
       else
          isGHOST_H = ghostV1(nH_ext,mH_ext)==1
       endif
       !
       ! These bunch of ifs can be optimized if called using vectors (nm) not matrices (n,m)
       !
       COPY_H_TO_Q(k) = .false.
       if (isGHOST_H.and.TYPEfreeSLIP/=2) then
          iH_U1 = FROMmnTOghostU1(nH_ext,mH_ext)
          if (PERIODalongM==1) then
             if (perHwaterRIGHT) then !water is for larger m
                !
                ! posNORM = nxG_U1(i) .gt. 0._fp
                ! waterABOVE = ghostU1(nH_ext+1,mH_ext) == 0
                ! if (posNORM.EQV.waterABOVE) then
                !
                if (comparereal(nxG_U1(iH_U1),0._fp).gt.0) then
                   COPY_H_TO_Q(k) = .true.
                else
                   continue !default .false.
                endif
             else
                if (comparereal(nxG_U1(iH_U1),0._fp).lt.0) then
                   COPY_H_TO_Q(k) = .true.
                else
                   continue !default .false.
                endif
             endif
          else
             if (perHwaterRIGHT) then !water is for larger m
                if (comparereal(nyG_U1(iH_U1),0._fp).gt.0) then
                   COPY_H_TO_Q(k) = .true.
                else
                   continue !default .false.
                endif
             else
                if (comparereal(nyG_U1(iH_U1),0._fp).lt.0) then
                   COPY_H_TO_Q(k) = .true.
                else
                   continue !default .false.
                endif
             endif
          endif
       endif
       !
       ! note: the same shift (shiftQM,shiftQN) is used for both right and left hand sides when prescribing something at Q bounadry. 
       ! The same for H boundary.
       !
       ! i is already defined above
       ! this are needed to compute correctly the adjacent internal u-ghost point in interpG if it uses the boundary one. 
       ! The boundary itself will be overwritten in velocityPERIOD_ghost
       !
       if (.not.COPY_H_TO_Q(k)) then ! copy Q in H, ONLY IF Q IS GHOST POINT
          if (PERIODalongM==1) then
             isGHOST_Q = ghostU1(nQ_ext,mQ_ext)==1
          else
             isGHOST_Q = ghostV1(nQ_ext,mQ_ext)==1
          endif
          if (isGHOST_Q) then
             !iQ_U1 = FROMmnTOghostU1(nQ_ext,mQ_ext)
             FROMmnTOghostU1(nH_ext,mH_ext) = FROMmnTOghostU1(nQ_int,mQ_int)
             !BI can be wrong. I use the correct one, and I shift it by using the distance between the ghost points (that is different in general between teh distance between IB since one iB is wrong)
             shiftBIu_x(nH_ext,mH_ext) = - xG_U1(nQ_int,mQ_int) + xG_U1(nH_ext,mH_ext)
             shiftBIu_y(nH_ext,mH_ext) = - yG_U1(nQ_int,mQ_int) + yG_U1(nH_ext,mH_ext)
          endif
       else ! copy H in Q
          FROMmnTOghostU1(nQ_ext,mQ_ext) = FROMmnTOghostU1(nH_int,mH_int)
          !BI can be wrong. I use the correct one, and I shift it by using the distance between the ghost points (that is different in general between teh distance between IB since one iB is wrong)
          shiftBIu_x(nQ_ext,mQ_ext) =  xG_U1(nQ_ext,mQ_ext) - xG_U1(nH_int,mH_int)
          shiftBIu_y(nQ_ext,mQ_ext) =  yG_U1(nQ_ext,mQ_ext) - yG_U1(nH_int,mH_int)
          !  nxG_U1(iQ) = nxG_U1(iH)
          !  nyG_U1(iQ) = nyG_U1(iH) 
          !  xBIu1(iQ)  = xBIu1(iH) + (xG_U1(nPQ_ext(k)+shiftQN,mPQ_ext(k)+shiftQM) - xG_U1(nH_ext,mH_ext))
          !  yBIu1(iQ)  = yBIu1(iH) + (xG_U1(nPQ_ext(k)+shiftQN,mPQ_ext(k)+shiftQM) - xG_U1(nH_ext,mH_ext))
       endif
       !
       if (.not.COPY_H_TO_Q(k).and.isGHOST_Q) then ! copy Q in H
          ghostU1(nH_ext,mH_ext) = ghostU1(nQ_int,mQ_int)
       else ! copy H in Q
          ghostU1(nQ_ext,mQ_ext) = ghostU1(nH_int,mH_int)  
       endif
       !  
       !PERIODIC V (PARALLEL TO BOUNDARY)
       !
       mH_ext = mPH_ext(k) 
       nH_ext = nPH_ext(k) 
       mQ_ext = mPQ_ext(k) 
       nQ_ext = nPQ_ext(k) 
       mH_int = mPH_int(k) 
       nH_int = nPH_int(k) 
       mQ_int = mPQ_int(k) 
       nQ_int = nPQ_int(k) 
       FROMmnTOghostV1(nH_ext,mH_ext) = FROMmnTOghostV1(nQ_int,mQ_int)
       shiftBIv_x(nH_ext,mH_ext) = - xG_V1(nQ_int,mQ_int) + xG_V1(nH_ext,mH_ext)
       shiftBIv_y(nH_ext,mH_ext) = - yG_V1(nQ_int,mQ_int) + yG_V1(nH_ext,mH_ext)
       !
       FROMmnTOghostV1(nQ_ext,mQ_ext) = FROMmnTOghostV1(nH_int,mH_int)
       shiftBIv_x(nQ_ext,mQ_ext) = xG_V1(nQ_ext,mQ_ext) - xG_V1(nH_int,mH_int)
       shiftBIv_y(nQ_ext,mQ_ext) = yG_V1(nQ_ext,mQ_ext) - yG_V1(nH_int,mH_int)
       !
       ghostV1(nQ_ext,mQ_ext) = ghostV1(nH_int,mH_int)
       ghostV1(nH_ext,mH_ext) = ghostV1(nQ_int,mQ_int)  
          !  
       if (twoCELLSperiod) then
          !  
          ! PERIODIC U (ORTH TO BOUNDARY)
          !
          mH_extext = mPH_extext(k)+shiftHM  
          nH_extext = nPH_extext(k)+shiftHN
          mQ_extext = mPQ_extext(k)+shiftQM
          nQ_extext = nPQ_extext(k)+shiftQN
          mH_intint = mPH_intint(k)+shiftQM
          nH_intint = nPH_intint(k)+shiftQN
          mQ_intint = mPQ_intint(k)+shiftHM
          nQ_intint = nPQ_intint(k)+shiftHN
          !
          FROMmnTOghostU1(nH_extext,mH_extext) = FROMmnTOghostU1(nQ_intint,mQ_intint)       
          shiftBIu_x(nH_extext,mH_extext) = - xG_U1(nQ_intint,mQ_intint) + xG_U1(nH_extext,mH_extext) !BI can be wrong. I use the correct one, and I shift it by using the distance between the ghost points (that is different in general between teh distance between IB since one iB is wrong)
          shiftBIu_y(nH_extext,mH_extext) = - yG_U1(nQ_intint,mQ_intint) + yG_U1(nH_extext,mH_extext)
          !
          FROMmnTOghostU1(nQ_extext,mQ_extext) = FROMmnTOghostU1(nH_intint,mH_intint)            
          shiftBIu_x(nQ_extext,mQ_extext) =  xG_U1(nQ_extext,mQ_extext) - xG_U1(nH_intint,mH_intint) !BI can be wrong. I use the correct one, and I shift it by using the distance between the ghost points (that is different in general between teh distance between IB since one iB is wrong)
          shiftBIu_y(nQ_extext,mQ_extext) =  yG_U1(nQ_extext,mQ_extext) - yG_U1(nH_intint,mH_intint)
          !
          ghostU1(nQ_extext,mQ_extext) = ghostU1(nH_intint,mH_intint)
          ghostU1(nH_extext,mH_extext) = ghostU1(nQ_intint,mQ_intint)
          !  
          ! PERIODIC V (PARALLEL TO BOUNDARY)
          !
          mH_extext = mPH_extext(k) 
          nH_extext = nPH_extext(k) 
          mQ_extext = mPQ_extext(k) 
          nQ_extext = nPQ_extext(k) 
          mH_intint = mPH_intint(k)
          nH_intint = nPH_intint(k) 
          mQ_intint = mPQ_intint(k) 
          nQ_intint = nPQ_intint(k) 
          !
          FROMmnTOghostV1(nH_extext,mH_extext) = FROMmnTOghostV1(nQ_intint,mQ_intint)
          shiftBIv_x(nH_extext,mH_extext) = - xG_V1(nQ_intint,mQ_intint) + xG_U1(nH_extext,mH_extext)
          shiftBIv_y(nH_extext,mH_extext) = - yG_V1(nQ_intint,mQ_intint) + yG_U1(nH_extext,mH_extext)
          !
          FROMmnTOghostV1(nQ_extext,mQ_extext) = FROMmnTOghostV1(nH_intint,mH_intint)
          shiftBIv_x(nQ_extext,mQ_extext) = xG_V1(nQ_extext,mQ_extext) - xG_V1(nH_intint,mH_intint)
          shiftBIv_y(nQ_extext,mQ_extext) = yG_V1(nQ_extext,mQ_extext) - yG_V1(nH_intint,mH_intint)
          !
          ghostV1(nH_extext,mH_extext) = ghostV1(nQ_intint,mQ_intint)    
          ghostV1(nQ_extext,mQ_extext) = ghostV1(nH_intint,mH_intint)
          !
       endif
    enddo
    !
end subroutine extrapPERIODICghost
