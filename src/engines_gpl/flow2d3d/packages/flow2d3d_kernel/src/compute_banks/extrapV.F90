  SUBROUTINE extrapV(nst,kcs,kfv,v0,v1,Vmean,qyk,hv,thick,gvv,mmax,nmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov,KFis1,aa,bb,cc,dd,aak,bbk,cck,ddk,gdp) 
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
!   Function: extrapolate u1/u0 and v1/v0 for edges with active part less then 50%. 
!
!
!   Author: Alberto Canestrelli
!
!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)                      , pointer :: THRESextCUTedge
    integer                       , pointer :: continuity_cc
    integer                       , pointer :: extrapGHOST1fluid2
    integer                       , pointer :: idebugCUThardINI
    integer                       , pointer :: idebugCUThardFIN
    integer, dimension(:,:)       , pointer :: GHOSTu1
    integer, dimension(:,:)       , pointer :: GHOSTv1
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp), dimension(:,:,:)    , pointer :: qyk_tinyCUT
    real(fp), dimension(:,:)      , pointer :: xcor0
    real(fp), dimension(:,:)      , pointer :: ycor0
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: gvv
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: vmean
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v0
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v1
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfv
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hv 
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: qyk   
    real(fp), dimension(kmax)                                           , intent(in)    :: thick 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: aa                                                                                  
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: bb    
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: cc
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: dd
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: aak                                                                                     
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: bbk     
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: cck
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: ddk
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: Irov
    integer                                                             , intent(in)    :: nst
!
!
! local variables
!
  integer                    :: I,mGP,nGP,k,m,n,nad,mad,KFis1
  logical                    :: OUTbound
  real(fp)                   :: Lpart,Lad,Ltot,Vcut(1:kmax),Vvertex(1:kmax),aguuOK,agvvOK
!
! executable statements -------------------------------------------------------
!
    THRESextCUTedge    => gdp%gdimbound%THRESextCUTedge
    continuity_cc      => gdp%gdimbound%continuity_cc
    extrapGHOST1fluid2 => gdp%gdimbound%extrapGHOST1fluid2
    idebugCUThardINI   => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN   => gdp%gdimbound%idebugCUThardFIN
    GHOSTu1            => gdp%gdimbound%GHOSTu1
    GHOSTv1            => gdp%gdimbound%GHOSTv1
    EDGExyBANK         => gdp%gdimbound%EDGExyBANK
    aguu               => gdp%gdimbound%aguu
    agvv               => gdp%gdimbound%agvv
    qyk_tinyCUT        => gdp%gdimbound%qyk_tinyCUT
    xcor0              => gdp%gdimbound%xcor0
    ycor0              => gdp%gdimbound%ycor0
      do m=1,mmax          
         do n=1,nmaxus-1 
      !   if (GHOSTu1(n,m).eq.3) then !it is at a wet/dry interface NO! I DO IT FOR ANY CUT EDGE WITH AGUU<0.5
            if (comparereal(agvv(n,m),0._fp).eq.0) then 
               IF (KFis1==0) then
                  kfv(n,m) = 0 
                  v1(n,m,1:kmax) = 0._fp
                  v0(n,m,1:kmax) = 0._fp
               ENDIF
               qyk_tinyCUT(n,m,1:kmax) = 0._fp !it might be not zero from the previous cycle and bank moved
               Vmean(n,m)  = 0._fp
               if (ghostv1(n,m)==1) then
                  aa(n,m) = 0._fp
                  bb(n,m) = 1._fp
                  cc(n,m) = 0._fp
                  dd(n,m) = 0._fp
                  DO K=1,kmax
                     aak(n,m, k) = 0._fp  
                     bbk(n,m, k) = 1._fp
                     cck(n,m, k) = 0._fp
                     ddk(n,m, k) = 0._fp
                  ENDDO
               endif
            elseif ((comparereal(agvv(n,m),THRESextCUTedge).lt.0).and.(comparereal(agvv(n,m),0._fp).gt.0)) then             
               IF (KFis1==0) kfv(n,m) = 0
               !  v1(n,m,1:kmax) = 0 
               !  v0(n,m,1:kmax) = 0     
               if  ((kcs(n,m)==2.or.kcs(n+1,m)==2).and.continuity_cc==1)  cycle !i.e. if it is a BC dont change qkx and qky (it is used in incbc to smooth out the BC at the beginning of the simulation)
               if ( (comparereal(EDGExyBANK(n,m,3,2,1),xcor0(n,m)).eq.0) .and. (comparereal(EDGExyBANK(n,m,3,2,2),ycor0(n,m)).eq.0) ) then    !(3,2,1) => (edge=3,2:second extreme,x coord).  K=3 for v
                  !it is coincident with the right-hand side vertex, the water is on the left
                  if (extrapGHOST1fluid2==2) then
                     mAD = m-1
                     outBOUND = (mAD.lt.1)
                  elseif (extrapGHOST1fluid2==1) then !I use the cell itself, it should be a ghost, if its not it have velocity=0
                     mAD = m
                     outBOUND = .false.
                     if(ghostv1(n,m).ne.1) then
                        write(*,*) 'Error: mo ghost point to extrap velocity'
                        call d3stop(1, gdp)
                     endif
                  endif
               else
                  !it is coincident with the left-hand side vertex, the water is on the right
                  if (extrapGHOST1fluid2==2) then
                     mAD = m+1
                     outBOUND = (mAD.gt.mmax)
                  elseif (extrapGHOST1fluid2==1) then
                     mAD = m
                     outBOUND = .false.
                  endif
               endif
               if (.not.outBOUND) then
                  Lpart = agvv(n,m)*gvv(n,m)
                  if (irov==0.or.irov==3) then !free slip
                     Vcut(1:kmax) = v0(n,mAD,1:kmax)
                  elseif (irov==2) then !no-slip (linearly to zero from the v point)
                     if (extrapGHOST1fluid2==2) then
                        Lad  = gvv(n,m)*0.5_fp
                        Ltot = Lpart+Lad
                        Vvertex(1:kmax) = v0(n,m,1:kmax)*Lpart/Ltot
                         ! Vvertex(1:kmax)= v0(n,m,1:kmax) !this if I consider it constant in the adjacent element
                        Vcut(1:kmax) = Vvertex(1:kmax)*0.5_fp                     
                     elseif (extrapGHOST1fluid2==1) then
                        agvvOK =min(0.49_fp,agvv(n,m)) ! cause it aguu -> 0.5 => division by zero and huge value (but v0 is very small, try different threshoald and see)
                        Vvertex(1:kmax) = v0(n,m,1:kmax)* agvvOK/(0.5_fp-agvvOK) 
                        Vcut(1:kmax) = Vvertex(1:kmax)*0.5_fp                                
                     endif
                  else
                     write(*,*) 'option for qyk_tinyCUT not implemented'
                     call d3stop(1, gdp)
                  endif

                  vmean(n,m) = 0._fp
                  if (KFis1==1) then
                     v1(n,m,1:kmax) = Vcut(1:kmax)
                     do k = 1, kmax
                        vmean(n,m) = vmean(n,m) + thick(k)*Vcut(k)
                     enddo
                     !qyk_tinyCUT(n,m,1:kmax) = 0._fp not needed since qyk_tinyCUT is never prescribed in any iteration
                  else
                     v1(n,m,1:kmax) = 0._fp
                     v0(n,m,1:kmax) = 0._fp
                     qyk_tinyCUT(n,m,1:kmax) = Vcut(1:kmax)*Lpart*hv(n,m) 
                     qyk(n,m,1:kmax) = qyk_tinyCUT(n,m,1:kmax)
                  endif
               else
                  qyk_tinyCUT(n,m,1:kmax) = 0._fp
                  qyk(n,m,1:kmax) = 0._fp
                  vmean(n,m) = 0._fp
                  if (KFis1==0) then
                     v1(n,m,1:kmax) = 0._fp
                     v0(n,m,1:kmax) = 0._fp
                  endif
               endif
               aa(n,m) = 0._fp
               bb(n,m) = 1._fp
               cc(n,m) = 0._fp
               dd(n,m) = vmean(n,m)  
               DO K=1,kmax
                  aak(n,m, k) = 0._fp  
                  bbk(n,m, k) = 1._fp
                  cck(n,m, k) = 0._fp
                  ddk(n,m, k) = v1(n,m,k) 
               ENDDO
            else
               qyk_tinyCUT(n,m,1:kmax) = 0._fp !it might be not zero from the previous cycle and bank moved
            endif
      !    endif
               if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN)  then
               write(9893003,'(3i6,15f21.15)') nst,n,m,(qyk_tinyCUT(n,m, k),k = 1, kmax)
               endif  
!
         enddo
      enddo 
   RETURN
end subroutine extrapV
