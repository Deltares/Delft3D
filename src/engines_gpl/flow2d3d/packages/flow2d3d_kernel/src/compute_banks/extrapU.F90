 SUBROUTINE extrapU(nst,kcs,kfu,u0,u1,Umean,qxk,hu,thick,guu,mmax,nmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov,KFis1,aa,bb,cc,dd,aak,bbk,cck,ddk,gdp) 
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
    integer                       , pointer :: GhostMethod
    integer                       , pointer :: idebugCUThardINI
    integer                       , pointer :: idebugCUThardFIN
    integer, dimension(:,:)       , pointer :: GHOSTu1
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:,:)    , pointer :: qxk_tinyCUT
    real(fp), dimension(:,:,:)    , pointer :: qyk_tinyCUT
    real(fp), dimension(:,:)      , pointer :: xcor0
    real(fp), dimension(:,:)      , pointer :: ycor0
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: guu   
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: umean
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u0
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfu 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: qxk    !  Description and declaration in esm_alloc_real.f90
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
  real(fp)                   :: Lpart,Lad,Ltot,Uvertex(1:kmax),Ucut(1:kmax),aguuOK,agvvOK
!
! executable statements -------------------------------------------------------
!
    THRESextCUTedge    => gdp%gdimbound%THRESextCUTedge
    continuity_cc      => gdp%gdimbound%continuity_cc
    extrapGHOST1fluid2 => gdp%gdimbound%extrapGHOST1fluid2
    GhostMethod        => gdp%gdimbound%GhostMethod
    idebugCUThardINI   => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN   => gdp%gdimbound%idebugCUThardFIN
    GHOSTu1            => gdp%gdimbound%GHOSTu1
    EDGExyBANK         => gdp%gdimbound%EDGExyBANK
    aguu               => gdp%gdimbound%aguu
    qxk_tinyCUT        => gdp%gdimbound%qxk_tinyCUT
    qyk_tinyCUT        => gdp%gdimbound%qyk_tinyCUT
    xcor0              => gdp%gdimbound%xcor0
    ycor0              => gdp%gdimbound%ycor0
    do m=1,mmax-1           
      do n=1,nmaxus
      !   if (GHOSTu1(n,m).eq.3) then !it is at a wet/dry interface NO! I DO IT FOR ANY CUT EDGE WITH AGUU<0.5               
            if (comparereal(aguu(n,m),0._fp).eq.0) then  
               IF (KFis1==0) then
                  kfu(n,m) = 0 
                  u1(n,m,1:kmax) = 0._fp
                  u0(n,m,1:kmax) = 0._fp
               ENDIF
               qxk_tinyCUT(n,m,1:kmax) = 0._fp !it might be not zero from the previous cycle and bank moved
               Umean(n,m)  = 0._fp
               !this should be done only on the ghost point,for the other dry they are already all zero except bb,bbk 
               if (ghostu1(n,m)==1) then
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
            elseif ((comparereal(aguu(n,m),THRESextCUTedge).lt.0).and.(comparereal(aguu(n,m),0._fp).gt.0)) then       
               IF (KFis1==0) kfu(n,m) = 0._fp 
               !I commented these, so for ghostmethod 1 (i.e. ghost cells in sud) I retain the values of vel  at the ghost points)
               ! u1(n,m,1:kmax) = 0 !since kfu is zero, it is computed =0 in sud anyway (I set it to zero since I am not sure if it is used by the adjacent in the advective terms even if kfu=0). 
               ! u0(n,m,1:kmax) = 0 !since kfu is zero, it is computed =0 in sud anyway (I set it to zero since I am not sure if it is used by the adjacent in the advective terms even if kfu=0)   
               if  ((kcs(n,m)==2.or.kcs(n,m+1)==2).and.continuity_cc==1)  cycle !i.e. if it is a BC dont change qkx and qky (it is used in incbc to smooth out the BC at the beginning of the simulation). Note I still set kfu=0 on the boundary. This should not affect cause cucbp does not have kfu and also qyk qyk_cut in d0k are used everywhere not only where kfu=0
               if ( (comparereal(EDGExyBANK(n,m,2,2,1),xcor0(n,m)).eq.0) .and. (comparereal(EDGExyBANK(n,m,2,2,2),ycor0(n,m)).eq.0) ) then    !(2,2,1) => (edge=2,2:second extreme,x coord).  K=2 for U. Second extreme is always on the vertex.
                  !it is coincident with the upper vertex, the water is below
                  if (extrapGHOST1fluid2==2) then
                     nAD = n-1
                     outBOUND = (nAD.lt.1)
                  elseif (extrapGHOST1fluid2==1) then
                     nAD = n
                     outBOUND = .false.
                  endif
               else
                  !it is coincident with the lower vertex, the water is above
                  if (extrapGHOST1fluid2==2) then
                     nAD = n+1
                     outBOUND = (nAD.gt.nmaxus)
                  elseif (extrapGHOST1fluid2==1) then
                     nAD = n
                     outBOUND = .false.
                     if(ghostu1(n,m).ne.1) then
                        write(*,*) 'Error: no ghost point to extrap velocity'
                        call d3stop(1, gdp)
                     endif
                  endif
               endif
               if (.not.outBOUND) then
                  Lpart = aguu(n,m)*guu(n,m)
                  if (irov==0.or.irov==3) then !free slip
                     Ucut(1:kmax) = u0(nAD,m,1:kmax)
                  elseif (irov==2) then !no-slip (linearly to zero from the U point)
                     if (extrapGHOST1fluid2==2) then
                        Lad  = guu(nAD,m)*0.5_fp
                        Ltot = Lpart+Lad
                        Uvertex(1:kmax) = u0(nAD,m,1:kmax)*Lpart/Ltot
                        !Uvertex(1:kmax) = u0(nAD,m,1:kmax) !this if I consider it constant in the adjacent element
                        Ucut(1:kmax) = Uvertex(1:kmax)*0.5_fp                        
                     else if (extrapGHOST1fluid2==1) then
                        aguuOK =min(0.49_fp,aguu(n,m)) ! cause it aguu -> 0.5 => division by zero and huge value (but v0 is very small, try different threshoald and see)
                        Uvertex(1:kmax) = u0(n,m,1:kmax)* aguuOK/(0.5_fp-aguuOK)    
                        Ucut(1:kmax) = Uvertex(1:kmax)*0.5_fp                         
                     endif
                  else
                     write(*,*) 'option for qxk_tinyCUT not implemented'
                     call d3stop(1, gdp)
                  endif
                  umean(n,m) = 0._fp
                  if (KFis1==1) then
                     u1(n,m,1:kmax) = Ucut(1:kmax)
                     do k = 1, kmax
                        umean(n,m) = umean(n,m) + thick(k)*Ucut(k)
                     enddo
                    !qxk_tinyCUT(n,m,1:kmax) = 0._fp not needed since qyk_tinyCUT is never prescribed in any iteration
                  else
                     u1(n,m,1:kmax) = 0._fp
                     u0(n,m,1:kmax) = 0._fp
                     qxk_tinyCUT(n,m,1:kmax) = Ucut(1:kmax)*Lpart*hu(n,m)
                     qxk(n,m,1:kmax) = qxk_tinyCUT(n,m,1:kmax) 
                  endif
               else
                  qxk_tinyCUT(n,m,1:kmax) = 0._fp
                  qxk(n,m,1:kmax) = 0._fp
                  umean(n,m) = 0._fp
                  if (KFis1==0) then
                     u1(n,m,1:kmax) = 0._fp
                     u0(n,m,1:kmax) = 0._fp
                  endif
               endif
               aa(n,m) = 0._fp
               bb(n,m) = 1._fp
               cc(n,m) = 0._fp
               dd(n,m) = umean(n,m)  
               DO K=1,kmax
                  aak(n,m, k) = 0._fp  
                  bbk(n,m, k) = 1._fp
                  cck(n,m, k) = 0._fp
                  ddk(n,m, k) = u1(n,m,k) 
               ENDDO
            else
               qxk_tinyCUT(n,m,1:kmax) = 0._fp !it might be not zero from the previous cycle and bank moved
            endif
               if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN)  then
               write(9893003,'(3i6,15f21.15)') nst,n,m,(qxk_tinyCUT(n,m, k),k = 1, kmax)
               endif   
      !    endif
         enddo
      enddo

  RETURN
end subroutine extrapU
