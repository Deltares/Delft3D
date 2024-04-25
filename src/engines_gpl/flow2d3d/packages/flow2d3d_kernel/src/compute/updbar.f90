subroutine updbar(nsluv     ,mnbar     ,cbuv      ,cbuvrt    ,nmax      , &
                & mmax      ,kmax      ,thick     ,kspu      ,kspv      , &
                & kfumin    ,kfumax    ,kfvmin    ,kfvmax    ,ubrlsu    , &
                & ubrlsv    ,hu        ,hv        ,dpu       ,dpv       , &
                & zk        ,zcor      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Calculates for all barrier points :
!              - open or closed in mask arrays KSPU and KSPV
!              - extra energy losses due to quadratic friction
!              as a function of gate height and waterdepth
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                       , pointer :: zmodel
    integer                       , pointer :: rtcmod
    integer                       , pointer :: rtcact
    integer                       , pointer :: lundia
!
! Global variables
!
    integer                                                             , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in) :: nsluv  !  Description and declaration in dimens.igs
    integer, dimension(5, nsluv)                                        , intent(in) :: mnbar  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(out):: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(out):: kspv   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(4, nsluv)                                                    :: cbuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(2, nsluv)                                       , intent(in) :: cbuvrt !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in) :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in) :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in) :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(out):: ubrlsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(out):: ubrlsv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                           , intent(in) :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                        :: zcor
    real(fp), dimension(0:kmax)                                         , intent(in) :: zk
!
!
! Local variables
!
    integer                        :: btype                ! Type of barrier; 0 for U, 1 for V 
    integer                        :: ibar                 ! Loop variable 
    integer                        :: inc                  ! loop counter 
    integer                        :: incx                 ! Increment between M1,M2 
    integer                        :: incy                 ! Increment between N1,N2 
    integer                        :: dir                  ! Direction of barrier; 0 for U, 1 for V 
    integer                        :: k                    ! Loop counter over KMAX 
    integer                        :: ktop
    integer                        :: kbot
    integer                        :: kspuv                ! Local variable for KSPU or KSPV
    integer                        :: kstep
    integer                        :: m                    ! loop counter 
    integer                        :: m1                   ! First m-index for barrier 
    integer                        :: m2                   ! Last m-index for barrier 
    integer                        :: maxinc               ! Maximum of (increment between M1,M2 & increment between N1,N2) 
    integer                        :: n                    ! loop counter 
    integer                        :: n1                   ! First n-index for barrier 
    integer                        :: n2                   ! Last n-index for barrier 
    logical                        :: error
    real(fp)                       :: brlosc               ! Barrier energy loss coefficient 
    real(fp)                       :: dgate
    real(fp)                       :: dpuv                 ! Local variable for DPU or DPV
    real(fp)                       :: fact                 ! Multiplier for energy loss 
    real(fp)                       :: hgate                ! Gate height for barrier 
    real(fp)                       :: huv                  ! Local variable for HU or HV
    real(fp)                       :: muc
    real(fp)                       :: ratio
    real(fp)                       :: thsum                ! Tempory sum of layer thichnesses 
    real(fp)                       :: ubrlsuv              ! Local variable for UBRLSU or UBRLSV
    real(fp)                       :: zk_bot               ! Lower vertical coordinate relative to reference plane of layer k
    real(fp)                       :: zk_top               ! Upper vertical coordinate relative to reference plane of layer k
    character(80)                  :: errmsg
!
!! executable statements -------------------------------------------------------
!
    zmodel     => gdp%gdprocs%zmodel
    rtcmod     => gdp%gdrtc%rtcmod
    rtcact     => gdp%gdrtc%rtcact
    lundia     => gdp%gdinout%lundia
    !
    do ibar = 1, nsluv
       !
       ! barrier location is defined in M,N coordinates
       !
       m1 = mnbar(1, ibar)
       n1 = mnbar(2, ibar)
       m2 = mnbar(3, ibar)
       n2 = mnbar(4, ibar)
       dir= mnbar(5, ibar)
       !
       ! Increments from coordinate pairs (tested in subroutine filbar)
       !
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error     )
       m = m1 - incx
       n = n1 - incy
       !
       ! Overrule barrier height if it has been set by RTC
       !
       if (btest(rtcmod,dataFromRTCToFLOW) .and. cbuvrt(1,ibar)>0) then
          cbuv(1, ibar) = cbuvrt(2, ibar)
       endif
       hgate = cbuv(1, ibar)
       !
       do inc = 1, maxinc + 1
          m = m + incx
          n = n + incy
          !
          if (dir==0) then
             !
             ! U-direction
             !
             huv  = hu (n, m)
             dpuv = dpu(n, m)
             if (zmodel) then
                ktop  = kfumax(n, m)
                kbot  = kfumin(n, m)
                kstep = -1
             else
                ktop  = 1
                kbot  = kmax
                kstep = 1
             endif
          elseif (dir==1) then
             !
             ! V-direction
             !
             huv  = hv (n, m)
             dpuv = dpv(n, m)
             if (zmodel) then
                ktop  = kfvmax(n, m)
                kbot  = kfvmin(n, m)
                kstep = -1
             else
                ktop  = 1
                kbot  = kmax
                kstep = 1
             endif
          endif
          !
          btype = nint(cbuv(2,ibar))
          if (btype==1) then
             !
             ! Loss coefficient based on local velocity
             !
             brlosc = cbuv(3, ibar)
             !
          elseif (btype==2) then
             !
             ! Compute size of gate opening
             !
             dgate = max(0.0_fp,hgate+dpuv)
             !
             ! Compute ratio of gate opening and water depth
             !
             ratio = min(1.0_fp,dgate/huv)
             !
             ! Compute mu_c
             !
             muc = cbuv(3, ibar)+ cbuv(4, ibar)*ratio
             !
             ! Determine the loss coefficient lambda
             !
             if (muc*ratio<1.0e-10_fp) then
                brlosc = 0.5e20_fp
             else
                brlosc = 0.5_fp*(1.0_fp/(muc*ratio) - 1.0_fp)**2
             endif
             !
             ! Since lambda will be applied to the local velocity
             ! instead of the upstream, free flow velocity correct
             ! it for the ratio of gate opening / water depth
             !
             brlosc = brlosc * ratio**2
             !
          endif
          !
          ! determine z-levels of layer interfaces:
          ! zcor(k) is z-level of the bottom of layer k
          ! zk_bot is initially set to z-level of top of layer ktop
          !
          thsum = 0.0_fp
          if (zmodel) then
             !
             ! zk is z-level of the top of layer k (not yet limited
             ! to water column)
             !
             zk_bot = min(max(zk(ktop),-dpuv),huv-dpuv)
             do k = ktop, kbot, kstep
                zcor(k) = min(max(zk(k-1),-dpuv),huv-dpuv)
             enddo
          else
             !
             ! thick is thickness of layer k
             !
             zk_bot = huv - dpuv
             do k = ktop, kbot, kstep
                thsum = thsum + thick(k)
                zcor(k) = (1.0_fp-thsum)*huv - dpuv
             enddo
          endif
          !
          ! masks and coefficient for barrier as 3D-gate
          !
          do k = ktop, kbot, kstep
             zk_top = zk_bot
             zk_bot = zcor(k)
             !
             ! effective blockage can be implemented by means of
             ! increased loss coefficient or by means of porosity
             ! this implementation increases the loss coefficients
             !
             if (hgate<=zk_bot) then
                !
                ! closed layer: gate below bottom of layer
                !
                ubrlsuv = 0.0_fp
                kspuv = 1
                !porosuv = 0.0_fp
             elseif (hgate<zk_top) then
                !
                ! partially open layer: gate ends in this layer
                !
                fact = (zk_top-zk_bot)/(hgate-zk_bot)
                ubrlsuv = brlosc * fact * fact
                kspuv = 0
                !porosuv = (hgate-zk_bot)/(zk_top-zk_bot)
             elseif (hgate<huv-dpuv) then
                !
                ! open layer, but gate in water
                !
                ubrlsuv = brlosc
                kspuv = 0
                !porosuv = 1.0_fp
             else
                !
                ! gate above water
                !
                ubrlsuv = 0.0_fp
                kspuv = 0
                !porosuv = 1.0_fp
             endif
             !
             if (dir==0) then
                !
                ! U-direction
                !
                ubrlsu(n, m, k) = ubrlsuv
                kspu(n, m, k) = kspuv
             elseif (dir==1) then
                !
                ! V-direction
                !
                ubrlsv(n, m, k) = ubrlsuv
                kspv(n, m, k) = kspuv
             endif
             !
             ! next layer
             !
          enddo
          !
          ! next barrier segment
          !
       enddo
       !
       ! next barrier
       !
    enddo
end subroutine updbar
