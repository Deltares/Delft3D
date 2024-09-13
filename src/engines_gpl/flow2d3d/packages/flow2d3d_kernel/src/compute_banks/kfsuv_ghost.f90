subroutine kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,INTvalue,RESETghost,nlb,nub,mlb,mub,nmlb,nmub, gdp)
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
!   Function: set the new values of  kfu,kfv for u1/v1/s1 ghost cells plus
!             u1/v1 of the s1 ghost cells.
!
!   Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                   , pointer :: continuity_cc
    integer                   , pointer :: totGHOSTu1
    integer                   , pointer :: totGHOSTv1
    integer                   , pointer :: totGHOSTs1
    integer, dimension(:,:)   , pointer :: kfs_cc
    integer, dimension(:,:)   , pointer :: por012
    integer, dimension(:)     , pointer :: nGPs1
    integer, dimension(:)     , pointer :: mGPs1
    integer, dimension(:)     , pointer :: nGPu1
    integer, dimension(:)     , pointer :: mGPu1
    integer, dimension(:)     , pointer :: nGPv1
    integer, dimension(:)     , pointer :: mGPv1
    real(fp), dimension(:,:)  , pointer :: dpU0
    real(fp), dimension(:,:)  , pointer :: dpV0
    real(fp), dimension(:,:)  , pointer :: dpH
    real(fp), dimension(:,:)  , pointer :: dpL
    real(fp), dimension(:,:)  , pointer :: aguu
    real(fp), dimension(:,:)  , pointer :: agvv
    real(fp), dimension(:,:,:), pointer :: u1_FLLYghst
    real(fp), dimension(:,:,:), pointer :: v1_FLLYghst
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: s1 
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: s0 
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u0
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v0
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(in)    :: dps
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfv
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcv 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: dpu 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: dpv 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hv 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: qxk  
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: qyk 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: Umean 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: Vmean 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: INTvalue
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
     integer                                                            , intent(in)    :: RESETghost 
!
!
! local variables
!
  integer                    :: I,mGP,nGP,k

!  real(fp)                   :: dxADJ

!
! executable statements -------------------------------------------------------
!
    continuity_cc => gdp%gdimbound%continuity_cc
    totGHOSTu1    => gdp%gdimbound%totGHOSTu1
    totGHOSTv1    => gdp%gdimbound%totGHOSTv1
    totGHOSTs1    => gdp%gdimbound%totGHOSTs1
    kfs_cc        => gdp%gdimbound%kfs_cc
    por012        => gdp%gdimbound%por012
    nGPs1         => gdp%gdimbound%nGPs1
    mGPs1         => gdp%gdimbound%mGPs1
    nGPu1         => gdp%gdimbound%nGPu1
    mGPu1         => gdp%gdimbound%mGPu1
    nGPv1         => gdp%gdimbound%nGPv1
    mGPv1         => gdp%gdimbound%mGPv1
    dpU0          => gdp%gdimbound%dpU0
    dpV0          => gdp%gdimbound%dpV0
    dpH           => gdp%gdimbound%dpH
    dpL           => gdp%gdimbound%dpL
    aguu          => gdp%gdimbound%aguu
    agvv          => gdp%gdimbound%agvv
    u1_FLLYghst   => gdp%gdimbound%u1_FLLYghst
    v1_FLLYghst   => gdp%gdimbound%v1_FLLYghst
!
! set water surface mask to one if not cut cell!!!!!!
!
!    do i = 1,totGHOSTs1   
!       mGP = mGPs1(i)
!       nGP = nGPs1(i) 
     !  if(kfs_cc(nGP,mGP).ne.0) then ! if cut cell I want to keep it active anyways.
!          kfs(nGP,mGP) = INTvalue 
     !  endif
!    enddo
!
! set u-velocity mask to one
!
    do i = 1,totGHOSTu1   
       mGP = mGPu1(i)
       nGP = nGPu1(i) 
       if  (comparereal(aguu(nGP,mGP),0._fp).eq.0)  then   
         kfu(nGP,mGP) = INTvalue 
       endif
    enddo
!
! set v-velocity mask to one
!
    do i = 1,totGHOSTv1   
       mGP = mGPv1(i)
       nGP = nGPv1(i) 
       if  (comparereal(agvv(nGP,mGP),0._fp).eq.0)  then   
         kfv(nGP,mGP) = INTvalue 
       endif
    enddo
!
! reset the values  dps,dpu,dpv,hu and hv to dry conditions. Not sure if it is needed.
!
    if (RESETghost==1) then !it could be optimized , like put one RESETghostVEL e RESETghostSURF. Cause after the call to uzd the water surface ghosts were not modified
       do i = 1,totGHOSTs1   
          mGP = mGPs1(i)
          nGP = nGPs1(i) 
       !   if (por012(nGP,mGP).eq.0) then
        !     dps(nGP,mGP) = dpH(nGP,mGP) !=dpL
        !     s1(nGP,mGP) = real(dps(nGP,mGP),fp) 
        !     s0(nGP,mGP) = real(dps(nGP,mGP),fp)
        !  else
           !  dps(nGP,mGP) = dpL(nGP,mGP)
      !       s1(nGP,mGP) = -dpH(nGP,mGP) !-10 !real(dps(nGP,mGP),fp) ! it gives problems since the ghost becomes high while dsp is low and a high value of hu is computed in checku (average of normal and high value)
      !       s0(nGP,mGP) = -dpH(nGP,mGP) !-10 !real(dps(nGP,mGP),fp)! it gives problems since the ghost becomes high while dsp is low and a high value of hu is computed in checku (average of normal and high value)
        !  endif
       enddo
       do i = 1,totGHOSTu1   
          mGP = mGPu1(i)
          nGP = nGPu1(i) 
        !  hu(nGP,mGP) =  0._fp  ! Note that if I updated s1 in sud they are wrong I have to recompute them
        !  dpu(nGP,mGP) = dpu0(nGP,mGP)
          !Note it should not go OUT OF BOUND with mGP+1 since U in mGP+1 cannot be a ghost
          if  (comparereal(aguu(nGP,mGP),0._fp).ne.0)  then !   ((kcs(nGP,mGP)==2.or.kcs(nGP,mGP+1)==2).and.continuity_cc==1)  then !i.e. if it is a BC dont reset it to zero (it is used in incbc to smooth out the BC at the beginning of the simulation)
              continue
          else
             Umean(nGP,mGP)  = 0._fp
             do k=1,kmax
                qxk(nGP,mGP,k)  = 0._fp  
                u1_FLLYghst(nGP,mGP,k) = u1(nGP,mGP,k)
                u1(nGP,mGP,k) = 0._fp 
                u0(nGP,mGP,k) = 0._fp 
             enddo 
          endif
       enddo
       do i = 1,totGHOSTv1   
          mGP = mGPv1(i)
          nGP = nGPv1(i) 
         ! hv(nGP,mGP) = 0._fp  !  Note that if I updated s1 in sud they are wrong I have to recompute them
         ! dpv(nGP,mGP) = dpv0(nGP,mGP)
         !Note it should not go OUT OF BOUND with nGP+1 since V in nGP+1 cannot be a ghost
          if  (comparereal(agvv(nGP,mGP),0._fp).ne.0)  then   !if  ((kcs(nGP,mGP)==2.or.kcs(nGP+1,mGP)==2).and.continuity_cc==1)  then !i.e. if it is a BC dont reset it to zero (it is used in incbc to smooth out the BC at the beginning of the simulation)
              continue
          else
             Vmean(nGP,mGP) = 0._fp        
             do k=1,kmax
                qyk(nGP,mGP,k)  = 0._fp  
                v1_FLLYghst(nGP,mGP,k) = v1(nGP,mGP,k)
                v1(nGP,mGP,k) = 0._fp 
                v0(nGP,mGP,k) = 0._fp 
             enddo 
         endif
       enddo
    endif

RETURN
end subroutine kfsuv_ghost
