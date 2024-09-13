   subroutine extrapUV(nst,guu,gvv,kcs,kfu,kfv,umean,vmean,hu,hv,u1,v1,u0,v0,qxk,qyk,thick,mmax,nmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov, gdp)
!
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
    real(fp)                  , pointer :: THRESextCUTedge
    integer                   , pointer :: continuity_cc
    integer                   , pointer :: extrapGHOST1fluid2
    integer                   , pointer :: GhostMethod
    integer, dimension(:,:)   , pointer :: GHOSTu1
    real(fp), dimension(:,:)  , pointer :: aguu
    real(fp), dimension(:,:)  , pointer :: agvv
    real(fp), dimension(:,:,:), pointer :: qxk_tinyCUT
    real(fp), dimension(:,:,:), pointer :: qyk_tinyCUT
!
! global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: guu   
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: gvv
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: umean
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: vmean
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: u0
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: v0 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inOUT) :: kfv
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: hv 
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(inout) :: qyk   
    real(fp), dimension(kmax)                                           , intent(in)    :: thick 
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
    integer                    :: I,mGP,nGP,k,m,n,nad,mad
    logical                    :: OUTbound
    real(fp)                   :: Lpart,Lad,Ltot,Uvertex(1:kmax),Vvertex(1:kmax),aguuOK,agvvOK
!   dummy to pass to extrapV/extrapU, to be removed
    real(fp), allocatable, dimension(:,:)                      :: aa_dummy                                                                                     
    real(fp), allocatable, dimension(:,:)                      :: bb_dummy     
    real(fp), allocatable, dimension(:,:)                      :: cc_dummy 
    real(fp), allocatable, dimension(:,:)                      :: dd_dummy 
    real(fp), allocatable, dimension(:,:, :)                :: aak_dummy                                                                                     
    real(fp), allocatable, dimension(:,:, :)                :: bbk_dummy     
    real(fp), allocatable, dimension(:,:, :)                :: cck_dummy 
    real(fp), allocatable, dimension(:,:, :)                :: ddk_dummy 

!
   !if (extrapGHOST1fluid2==0) THEN !
    THRESextCUTedge    => gdp%gdimbound%THRESextCUTedge
    continuity_cc      => gdp%gdimbound%continuity_cc
    extrapGHOST1fluid2 => gdp%gdimbound%extrapGHOST1fluid2
    GhostMethod        => gdp%gdimbound%GhostMethod
    GHOSTu1            => gdp%gdimbound%GHOSTu1
    aguu               => gdp%gdimbound%aguu
    agvv               => gdp%gdimbound%agvv
    qxk_tinyCUT        => gdp%gdimbound%qxk_tinyCUT
    qyk_tinyCUT        => gdp%gdimbound%qyk_tinyCUT
   !
   if (continuity_cc.eq.1.and.GhostMethod.le.1) then      !with ghost cells in sud
   !
   !  no extrapolation is performed, since the water surface  in general cannot be solved in sud for small cut cells 
   ! (except where free_s1=1)  since I keep fixed the water surface in the ghost. Moreover I can have s1 point active 
   !  but still cut edges with active part <50% on that cells. In this edges the ghost velocity is used.
   !  Note that here I still have to set GHOSTu1(n,m).eq.3 inactive for stability purposes (see notes)
   !
   !  One thing that can be done (not sure if it makes sense) for extrapGHOST1fluid2>0 it is prescribing on the ghost points 
   !  in sud not the ghost condition but the condition extrapolated below at each iteration iter. In this way the 
   !  points adjacent to the ghost in cucnp feels the ghost values in the advective terms, while for the continuity
   !  equation and s1 the velocity (i.e flux) is used. It is some sort of explicit velocity.   
   !  
   !
   else if (continuity_cc.eq.1.and.GhostMethod.eq.2) then  !no ghosts in sud, only uzd. Explicit ghosts are used 
   !
   !   set cells with less then THRESextCUTedge (<=0.5, THRESextCUTedge=0.5 by default) active edge as inactive and vel=0
   !   Define fluxes of small inactive edges. Note: setting the point GHOSTu1(n,m).eq.3 is automatically the acase comparereal(aguu(n,m),0._fp).eq.0 (and the same for v)

      if (extrapGHOST1fluid2.eq.1.or.extrapGHOST1fluid2.eq.2) then
        
         ALLOCATE(aa_dummy(nlb:nub,mlb:mub),bb_dummy(nlb:nub,mlb:mub),cc_dummy(nlb:nub,mlb:mub),dd_dummy(nlb:nub,mlb:mub),&
            aak_dummy(nlb:nub,mlb:mub, kmax),bbk_dummy(nlb:nub,mlb:mub, kmax),cck_dummy(nlb:nub,mlb:mub, kmax),ddk_dummy(nlb:nub,mlb:mub, kmax)) 
   !
   !   U direction
   !
         CALL extrapU(nst,kcs,kfu,u0,u1,Umean,qxk,hu,thick,guu,mmax,nmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov,0,aa_dummy,bb_dummy,cc_dummy,dd_dummy,aak_dummy,bbk_dummy,cck_dummy,ddk_dummy,gdp) 
   !
   !   v direction
   !
         CALL extrapV(nst,kcs,kfv,v0,v1,Vmean,qyk,hv,thick,gvv,mmax,nmax,kmax,nmaxus,nlb,nub,mlb,mub,nmlb,nmub,Irov,0,aa_dummy,bb_dummy,cc_dummy,dd_dummy,aak_dummy,bbk_dummy,cck_dummy,ddk_dummy,gdp) 
         !
         DEALLOCATE(aa_dummy,bb_dummy,cc_dummy,dd_dummy,&
                    aak_dummy,bbk_dummy,cck_dummy,ddk_dummy)
!
      elseif (extrapGHOST1fluid2==0) then
         do m=1,mmax-1           
            do n=1,nmaxus
               !u dir
               if ((comparereal(aguu(n,m),THRESextCUTedge).lt.0).and.(comparereal(aguu(n,m),0._fp).ge.0)) then
                  kfu(n,m) = 0
                  qxk(n,m,1:kmax) = 0._fp
                  qxk_tinyCUT(n,m,1:kmax) = 0._fp
                  u1(n,m,1:kmax) = 0._fp   !maybe not needed it should not be used since kfu = 0
                  u0(n,m,1:kmax) = 0._fp   !maybe not needed it should not be used since kfu = 0
                  Umean(n,m)  = 0._fp 
               endif
               !v dir
               if ((comparereal(agvv(n,m),THRESextCUTedge).lt.0).and.(comparereal(agvv(n,m),0._fp).ge.0)) then
                  kfv(n,m) = 0
                  qyk_tinyCUT(n,m,1:kmax) = 0._fp
                  qyk(n,m,1:kmax) = 0._fp
                  v1(n,m,1:kmax) = 0._fp   !maybe not needed it should not be used since kfu = 0
                  v0(n,m,1:kmax) = 0._fp   !maybe not needed it should not be used since kfu = 0
                  Vmean(n,m)  = 0._fp 
               endif
            enddo
         enddo
      endif
!
   endif
 RETURN
end subroutine extrapUV
