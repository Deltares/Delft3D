 subroutine update_qfilt(stage,oneEXIT,qxk,qyk,u1,v1,hu,hv,guu,gvv,thick,nrob,nob,distr_qtq,distr_qtq_per,kmax,mlb,mub,nlb,nub, gdp) 
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2014.                                
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
!  $Id: update_qfilt.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/update_qfilt.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Update qfilt due to bank erosion
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: agvv
    integer                 , pointer :: distQHm
    integer                 , pointer :: distQHn
    real(fp), dimension(:,:), pointer :: qfilt
    real(fp)                , pointer :: reltim_qtq
    real(fp), dimension(:,:), pointer :: aguu0
    real(fp), dimension(:,:), pointer :: agvv0
!
! Global variables
!
    character(8)                                                       , intent(in)    :: stage     
    integer                                                            , intent(in)    :: nlb
    integer                                                            , intent(in)    :: nub
    integer                                                            , intent(in)    :: mlb
    integer                                                            , intent(in)    :: mub
    integer                                                            , intent(in)    :: kmax
    integer                                                            , intent(in)    :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(8, nrob)                                       , intent(in)    :: nob    !  Description and declaration in esm_alloc_int.f90
    logical                                                            , intent(in)    :: distr_qtq
    logical                                                            , intent(in)    :: distr_qtq_per
    real(fp), dimension(nlb:nub, mlb:mub)                              , intent(in)    :: guu  
    real(fp), dimension(nlb:nub, mlb:mub)                              , intent(in)    :: gvv
    real(fp), dimension(kmax)                                          , intent(in)    :: thick 
    logical,  dimension(nlb:nub, mlb:mub)                              , intent(in)    :: oneEXIT
    real(fp), dimension(nlb:nub, mlb:mub, kmax)                        , intent(in)    :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub, mlb:mub, kmax)                        , intent(in)    :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub, mlb:mub)                              , intent(in)    :: hu  
    real(fp), dimension(nlb:nub, mlb:mub)                              , intent(inout) :: hv
    real(fp), dimension(nlb:nub, mlb:mub, kmax)                        , intent(out)   :: qxk   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub, mlb:mub, kmax)                        , intent(out)   :: qyk    !  Description and declaration in esm_alloc_real.f90
! Local variables
!
    integer                             :: k
    integer                             :: n
    integer                             :: mpbAL          ! M index of 1st velocity point inside domain ALigned with the direction of the boundary discharge
    integer                             :: mpbORT         ! M index of the first of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge
    integer                             :: mpbORTm1       ! M index of the second of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge (if this is the orthogonal direction it is lowered by 1)
    integer                             :: npbORT         ! N index of the first of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge
    integer                             :: npbORTm1       ! N index of the second of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge (if this is the orthogonal direction it is lowered by 1)
    integer                             :: mpbi           ! M index of 1st water level point inside domain
    integer                             :: mpbt           ! M index of boundary velocity point
    integer                             :: npbi           ! M index of 1st water level point inside domain
    integer                             :: npbt           ! M index of boundary velocity point
    integer                             :: npbAL          ! M index of 1st velocity point inside domain ALigned with the direction of the boundary discharge
!
!! executable statements -------------------------------------------------------
!
    aguu       => gdp%gdimbound%aguu
    agvv       => gdp%gdimbound%agvv
    distQHm    => gdp%gdimbound%distQHm
    distQHn    => gdp%gdimbound%distQHn
    qfilt      => gdp%gdimbound%qfilt
    reltim_qtq => gdp%gdimbound%reltim_qtq
    aguu0      => gdp%gdimbound%aguu0
    agvv0      => gdp%gdimbound%agvv0
    !
    ! initialize local parameters
    ! omega in deg/hour & time in seconds !!, alfa = in minuten
    ! TIMSCL will not been used in UPDBCC
    !
    do n = 1, nrob
       ! only do something for total discharge boundaries (7) and water level boundaries (2) of type QH
       if (nob(3, n)/=7 ) then !.and. nob(3, n)/=2) then for now exclude water level boundaries (2) (that can be of type QH, otherwise for qtq_per it is going out of array when doing qtfracV(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k)
          cycle
       endif
       mpbt      = nob(1, n)
       npbt      = nob(2, n)
       if (nob(4, n)==2) then
          mpbt = mpbt - 1
          mpbi = mpbt
          mpbAL = mpbt - 1
          mpbORT = mpbAL
          mpbORTm1 = mpbAL 
       elseif (nob(4, n)==1) then
          mpbi = mpbt + 1
          mpbAL = mpbt + 1
          mpbORT = mpbAL
          mpbORTm1 = mpbAL 
       else
          mpbi = mpbt
          mpbAL = mpbt
          mpbORT = mpbAL
          mpbORTm1 = mpbORT-1
       endif
       if (nob(6, n)==2) then
          npbt = npbt - 1
          npbi = npbt
          npbAL = npbt - 1
          npbORT = npbAL
          npbORTm1 = npbAL
       elseif (nob(6, n)==1) then
          npbi = npbt + 1
          npbAL = npbt + 1
          npbORT = npbAL
          npbORTm1 = npbAL
       else
          npbi = npbt
          npbAL = npbt
          npbORT = npbAL
          npbORTm1 = npbORT-1
       endif
       !
       ! Determine direction dependent parameters
       !
       if (nob(4,n) > 0) then
          if ((distr_qtq.or.distr_qtq_per).and.reltim_qtq>0) then
             !here I use the value of discharge in the new formed edge. I should sum up the inner dishcarges but since the adjacent edge is computed from summing up and
             ! and I use it to find the value on the small edge I should be fine
             if (comparereal(aguu0(npbt, mpbt),0._fp)==0.and.comparereal(aguu(npbt, mpbt),0._fp)>0) then !new edge, qfilt is undefined
                if (stage=='stage2') then !qxk undefined at the end of stage 2 everywhere if new cut edge, so I redefined it (on the rest of the boundary it is still the old value but its should be not that different, and also this is only used to compute the proportion). One might think to update all the boundary, but then if its distr_qtqNNprism=Y then also the internals should be updated so it gets complicated.
                                          !ALTERNATIVELY, UNCOMMENT SECOND CALL TO freshqx that overwites this subr and see if that helps
                   if (oneEXIT(npbt, mpbt).or.oneEXIT(npbi, mpbi)) then
                      qxk(npbt, mpbt,1:kmax) = 0._fp !set to zero discharges for new cut edges having only one exit
                   else
                      do k = 1, kmax
                         qxk(npbt, mpbt,k) = aguu(npbt, mpbt)*guu(npbt, mpbt)*hu(npbt, mpbt)*thick(k)*u1(npbt, mpbt, k) !If I am finishing stage1, this will be used in sud in stage2. Otherwise if I am finishing stage 2, this is used only to print output
                      enddo
                   endif
                endif
                qfilt(1:kmax,n) = qxk(npbt, mpbt,1:kmax) 
             elseif (comparereal(aguu0(npbt, mpbt),0._fp)>0) then 
                qfilt(1:kmax,n) = qfilt(1:kmax,n)*aguu(npbt, mpbt)/aguu0(npbt, mpbt)  !if aguu = aguu0 nothing changes
             endif
          endif
       elseif (nob(6,n) > 0) then
          if ((distr_qtq.or.distr_qtq_per).and.reltim_qtq>0) then
             if (comparereal(agvv0(npbt, mpbt),0._fp)==0.and.comparereal(agvv(npbt, mpbt),0._fp)>0) then !new edge, qfilt is undefined
                if (stage=='stage1') then !qxk undefined at the end of stage 2 everywhere if new cut edge, so I redefined it (on the rest of the boundary it is still the old value but its should be not that different, and also this is only used to compute the proportion). One might think to update all the boundary, but then if its distr_qtqNNprism=Y then also the internals should be updated so it gets complicated.
                   if (oneEXIT(npbt, mpbt).or.oneEXIT(npbi, mpbi)) then
                      qyk(npbt, mpbt,1:kmax) = 0._fp !set to zero discharges for new cut edges having only one exit
                   else
                      do k = 1, kmax
                         qyk(npbt, mpbt,k) = agvv(npbt, mpbt)*gvv(npbt, mpbt)*hv(npbt, mpbt)*thick(k)*v1(npbt, mpbt, k) !If I am finishing stage1, this will be used in sud in stage2. Otherwise if I am finishing stage 2, this is used only to print output
                      enddo
                   endif
                endif
                qfilt(1:kmax,n) = qyk(npbt, mpbt,1:kmax) 
             elseif (comparereal(aguu0(npbt, mpbt),0._fp)>0) then 
                qfilt(1:kmax,n) = qfilt(1:kmax,n)*agvv(npbt, mpbt)/agvv0(npbt, mpbt) !if agvv = agvv0 nothing changes
             endif 
          endif        
       endif     
    enddo
  return
end subroutine update_qfilt
