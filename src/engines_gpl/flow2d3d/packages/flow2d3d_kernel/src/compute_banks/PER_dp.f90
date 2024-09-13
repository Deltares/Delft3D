subroutine PER_dp(dps, xz, yz, alfas, nlb, mlb, nub, mub, ddb, nmaxddb, nrper, gdp)  
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
!   Function:  extrapolate time-independent GRID property at periodic boundary 
!               
!    Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use mathconsts, only: pi, degrad
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: xg_L
    real(fp), dimension(:,:), pointer :: yg_L
    real(fp), dimension(:,:), pointer :: xg_H
    real(fp), dimension(:,:), pointer :: yg_H
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: dpH
    integer, dimension(:)   , pointer :: nPQ_int
    integer, dimension(:)   , pointer :: mPQ_int
    integer, dimension(:)   , pointer :: nPH_int
    integer, dimension(:)   , pointer :: mPH_int
    real(fp), dimension(:,:), pointer :: poros
    logical                 , pointer :: perCIRC
    integer                 , pointer :: cutcell
    real(fp)                , pointer :: slopeCIRC
    real(fp)                , pointer :: thresMERGE_zb
    integer, dimension(:)   , pointer :: Nmerged_bed
    real(fp)                , pointer :: distanceBOUNDper
!
! global variables
!
  integer                                    , intent(in)   :: nrper
  integer                                    , intent(in)   :: nlb
  integer                                    , intent(in)   :: nub
  integer                                    , intent(in)   :: mlb
  integer                                    , intent(in)   :: mub 
  integer                                    , intent(in)   :: ddb
  integer                                    , intent(in)   :: nmaxddb
  real(prec), dimension(nlb:nub,mlb:mub)     , intent(out)  :: dps      
  real(fp)  , dimension(nlb:nub,mlb:mub)     , intent(in)   :: alfas 
  real(fp)  , dimension(nlb:nub,mlb:mub)     , intent(in)   :: xz
  real(fp)  , dimension(nlb:nub,mlb:mub)     , intent(in)   :: yz
!
! local variables
!
  integer                    :: k 
  integer                    :: m
  integer                    :: n
  integer                    :: nmH
  integer                    :: nmQ
  integer                    :: nCHAN
  integer                    :: nFLOOD
  integer                    :: nQint  
  integer                    :: mQint  
  integer                    :: nHint  
  integer                    :: mHint
  real(fp)                   :: distGx
  real(fp)                   :: distGy
  real(fp)                   :: distGm
  real(fp)                   :: distBOUND
  real(fp)                   :: ANGdistBOUND 
  real(fp)                   :: ANGdistINTERNAL 
  real(fp)                   :: slope
  real(fp)                   :: slopeM
  real(fp)                   :: alfa
  real(fp)                   :: angFIRSTbaric
  real(fp)                   :: angLASTbaric
  real(fp)                   :: dz
  real(fp)                   :: xg_LQ 
  real(fp)                   :: yg_LQ 
  real(fp)                   :: xg_LH  
  real(fp)                   :: yg_LH
  real(fp)                   :: xg_HQ 
  real(fp)                   :: yg_HQ 
  real(fp)                   :: xg_HH  
  real(fp)                   :: yg_HH
  real(fp)                   :: dzprov_L(nrPER)
  real(fp)                   :: dzprov_H(nrPER)
  real(fp)                   :: dpL_H
  real(fp)                   :: dpL_Q
  real(fp)                   :: dpH_H
  real(fp)                   :: dpH_Q
  logical                    :: NmergedQ
  logical                    :: NmergedH
  logical                    :: skipped_L(nrPER)
  logical                    :: skipped_H(nrPER)  
!
! executable statements -------------------------------------------------------
!
    xg_L             => gdp%gdimbound%xg_L
    yg_L             => gdp%gdimbound%yg_L
    xg_H             => gdp%gdimbound%xg_H
    yg_H             => gdp%gdimbound%yg_H
    dpL              => gdp%gdimbound%dpL
    dpH              => gdp%gdimbound%dpH
    nPQ_int          => gdp%gdimbound%nPQ_int
    mPQ_int          => gdp%gdimbound%mPQ_int
    nPH_int          => gdp%gdimbound%nPH_int
    mPH_int          => gdp%gdimbound%mPH_int
    poros            => gdp%gdimbound%poros
    perCIRC          => gdp%gdimbound%perCIRC
    cutcell          => gdp%gdimbound%cutcell
    slopeCIRC        => gdp%gdimbound%slopeCIRC
    thresMERGE_zb    => gdp%gdimbound%thresMERGE_zb
    Nmerged_bed      => gdp%gdimbound%Nmerged_bed
    distanceBOUNDper => gdp%gdimbound%distanceBOUNDper
    !
    ! Determine average slope of the channel, and average dz. Note: if an analytical axysimmetrical bed is prescribed with longitudinlal slope S_teta,
    ! all the cells close to the boundary have the same slope,and all the cell have to be shifted by the same dz=S_teta*2*pi. THerefore I compute an 
    ! average channel slope using dp_L and that shift is given to all boundary cells. In this way floodplains are shifted as the channel, and avoid the
    ! problem to have to find slope of floodplain, that can in general be different from slope of channel. But the shift has to be the same otherwise I
    ! deform the cross-section
    !
    ! This approach works also for curvilinear mesh (curvMESH=true)   
    !
    dz           = 0._fp
    nCHAN        = 0
    nFLOOD       = 0
    skipped_L(:) = .false.
    skipped_H(:) = .false.
    do k=1,nrPER 
       !        
       nQint = nPQ_int(k)
       mQint = mPQ_int(k)
       nHint = nPH_int(k)
       mHint = mPH_int(k) 
       if (cutcell==2) then
          xg_LQ = xg_L(nQint,mQint)
          yg_LQ = yg_L(nQint,mQint)
          xg_LH = xg_L(nHint,mHint)
          yg_LH = yg_L(nHint,mHint)
          xg_HQ = xg_H(nQint,mQint)
          yg_HQ = yg_H(nQint,mQint)
          xg_HH = xg_H(nHint,mHint)
          yg_HH = yg_H(nHint,mHint)
          dpL_H = dpL (nHint,mHint)
          dpL_Q = dpL (nQint,mQint)
          dpH_H = dpH (nHint,mHint)
          dpH_Q = dpH (nQint,mQint)
       else
          !
          ! xg_L,yg_L undefined. Consider to initialized them equal to xz and yz in inchkr
          !
          xg_LQ = xz (nQint,mQint)
          yg_LQ = yz (nQint,mQint)
          xg_LH = xz (nHint,mHint)
          yg_LH = yz (nHint,mHint)  
          dpL_H = dps(nHint,mHint)
          dpL_Q = dps(nQint,mQint)
       endif
       !
       ! Determine dz, variation of depth (-bed elevation) at boundary
       ! Only if there is channel in both sides I can compute the slope
       ! Note I commented above and I use the thresMERGE_zb and I exclude small cells and receiving cells merged with small cells, 
       ! because otherwise I have to compute merged baricenter and if moving bank than the baricenter are not on the same circle 
       ! (might be merged on the right and non merged on the left
       !
       !if (comparereal(poros(nQint,mQint),0._fp)>0.and.comparereal(poros(nHint,mHint),0._fp)>0) then 
         if (cutcell > 0) then
            nmH      = (nHint  + ddb) + (mHint + ddb)  * nmaxddb  - nmaxddb
            nmQ      = (nQint  + ddb) + (mQint + ddb)  * nmaxddb  - nmaxddb
            NmergedH = Nmerged_bed(nmH)==1
            NmergedQ = Nmerged_bed(nmQ)==1
         else
            !
            ! Nmerged_bed not defined for non cutcells
            !
            NmergedH = .true.
            NmergedQ = .true.
         endif
         !
         ! Only if there is channel in both sides I can compute the slope
         !
         if (comparereal(poros(nQint,mQint),thresMERGE_zb)>0.and.comparereal(poros(nHint,mHint),thresMERGE_zb)>0.and.NmergedQ.and.NmergedH) then   
            if (perCIRC) then
               !
               ! Circular periodic channel with center in (zero,zero)
               !
               ! river bed (dpL)
               !
               nCHAN         = nCHAN + 1
               !
               ! Use 1/2*pi to have the zero on the North, but then use differences of angles so it should be general
               !
               angFIRSTbaric   = mod(1._fp/2._fp*pi-atan2(yg_LQ,xg_LQ),2._fp*pi) 
               angLASTbaric    = mod(1._fp/2._fp*pi-atan2(yg_LH,xg_LH),2._fp*pi) ! da controllare mod should be modulo I guess!!!
               !
               ! Angular distance
               !
               ANGdistBOUND    =  (abs(angLASTbaric)+abs(angFIRSTbaric))
               ANGdistINTERNAL = 2._fp*pi - ANGdistBOUND
               !
               ! Angular slope. It's positive since bottom elvation is smaller at H, so depth below reference is larger at H
               !
               slopeCIRC       = (dpL_H - dpL_Q)/ANGdistINTERNAL 
               dzPROV_L(k)     = slopeCIRC*2._fp*pi
            else
               !
               ! Straight periodic channel
               !
               ! Note: this approach is not perfect, in the sense that the like distINTERNAL is not parallel to the banks but almost, 
               !       so slope is slightly off the real slope. I could not find a way to do it exact. It can be done exact for non
               !       cut cells just computing  slopeM = (dps(int)-dps(intint))/dm and extrapolate in m, but if the cell is cut thats not 
               !       exact anymore. I think the only way to do it exact is taking 3 internal cells and do the plane between the 3 baricenter,
               !       too complicated and maybe not worth it.            
               nCHAN           = nCHAN + 1
               distGx          = xg_LH-xg_LQ
               distGy          = yg_LH-yg_LQ
               ANGdistINTERNAL = sqrt(distGx**2 + distGy**2)
               alfa            = alfas(nHint, mHint)*degrad ! any (nH,mH) is ok since the grid is cartesian non curvilinear!!(perCIRC=false)
               distGm          = distGx*cos(alfa) - distGy*sin(alfa)
               !distGn          = distGx*sin(alfa) + distGy*cos(alfa)
               !
               ! Linear slope in x or y. It is positive since bottom elvation is smaller at H, so depth below reference is larger at H
               !
               !slope           = (dps(nH,mH) - dps(nQ,mQ))/distINTERNAL 
               slope  = (dpL_H - dpL_Q)/ANGdistINTERNAL
               !slopeM = slope*cos(alfa) ! only because its supposed to have dz/dn =0 normally to the wall,so dz/dx is simply dz/dn*nx
               !if (comparereal(distGx,0_fp).ne.0) then
               !   slopex = cos(atan(distGy/distGx))*slope
               !   !
               !   ! Distance along x between baricenter at the boundary as it was an infinite long channel
               !   !
               !   distBOUND = distanceBOUNDper - distGx
               !else
               !   !
               !   ! Else the channel is perfectly vertical i use along y slope
               !   !
               !   slopey = slope
               !   !
               !   ! Distance along x between baricenter at the boundary as it was an infinite long channel
               !   !
               !   distBOUND = distanceBOUNDper - distGy
               !endif 
               distBOUND   = distanceBOUNDper !- distGm
               dzPROV_L(k) = distBOUND*slope  !M
            endif
         else
             SKIPPED_L(k) = .true.
         endif
         if (comparereal(poros(nQint,mQint),1._fp)<0.and.comparereal(poros(nHint,mHint),1._fp)<0) then
            !  
            ! floodplains (dpH)
            !
            nFLOOD          = nFLOOD + 1
            !
            ! Put 1/2*pi to have the zero on the North, but then use differences of angles so it should be general
            !
            angFIRSTbaric   = mod(1._fp/2._fp*pi-atan2(yg_HQ,xg_HQ),2._fp*pi) 
            angLASTbaric    =  mod(1._fp/2._fp*pi-atan2(yg_HH,xg_HH),2._fp*pi) ! da controllare mod should be modulo I guess!!!
            !
            ! Angular distance
            !
            ANGdistBOUND    =  (abs(angLASTbaric)+abs(angFIRSTbaric)) 
            ANGdistINTERNAL = 2._fp*pi - ANGdistBOUND
            !
            ! Angular slope. It's positive since bottom elvation is smaller at H, so depth below reference is larger at H
            !
            slopeCIRC       = (dpH_H - dpH_Q)/ANGdistINTERNAL
            dzPROV_H(k)     = slopeCIRC*2._fp*pi  
         else 
            SKIPPED_H(k)    = .TRUE.
         endif
    enddo
    !
    ! If  a couple is skipped use average DZ. It's not second order for cut cells but once one is skipped it will never be
    !
    ! Same slope for channel and floodplains (we want to shift upstream geometry as a whole and prescribe it on the halo downstream, and viceversa)
    !
    dz = sum(dzPROV_L(:),.not.SKIPPED_L)/max(1,nCHAN)  
    slopeCIRC = dz/(2._fp*pi)!recompute slopecirc from average dz
    do k=1,nrPER 
       if (skipped_L(k)) dzPROV_L(k) = dz
       if (skipped_H(k)) dzPROV_H(k) = dz
    endDO
    !dz = 0.06_fp*2._fp*pi     !exact value
    if (cutcell==0) then
       CALL PER_dps(dps , dzPROV_L, nlb, mlb, nub, mub, nrPER, gdp) 
    else
       CALL PER_dpLH(dpL, dzPROV_L, nlb, mlb, nub, mub, nrPER, gdp) 
       CALL PER_dpLH(dpH, dzPROV_H, nlb, mlb, nub, mub, nrPER, gdp) 
       CALL PER_dps(dps , dzPROV_L, nlb, mlb, nub, mub, nrPER, gdp) 
    endif
    !
end subroutine PER_dp
