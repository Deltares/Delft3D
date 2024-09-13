subroutine wphys(s1        ,u1        ,v1        ,w1        ,wphy      , &
               & irocol    ,norow     ,nocol     ,icx       ,icy       , &
               & j         ,nmmaxj    ,kmax      ,nsrc      ,zmodel    , &
               & mnksrc    ,disch     ,thick     ,sig       ,guu       , &
               & gvv       ,gsqs      ,dps       ,nmmax     ,kcs       , &
               & dpu       ,dpv       ,kfsmin    ,kfsmax    ,gsqsR     , &
               & porosu    ,porosv    ,aguu      ,agvv      ,agsqs     , &
               & kfs       ,nst       ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: wphys.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/wphys.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: The omega velocities are transformed to vertical
!              velocities in the original Cartesian coordinate
!              system.
! Method used: Vertical velocities are computed using the conti-
!              nuity equation (see E.L. Deleersnijder, Upwelling
!              and upsloping in three dimensional marine models,
!              Appl. Math. Modelling, 13, 1989).
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
   !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
     integer                              , pointer :: lundia
    integer                 , pointer :: dim_nmlist
    integer                 , pointer :: typeVIRTmergeUPDvert
    logical                 , pointer :: virtualMERGEupdVERT
    integer                 , pointer :: cutcell
    real(fp)                , pointer :: thresMERGE_w
    integer, dimension(:,:) , pointer :: NMlistMERGED_w
    integer, dimension(:)   , pointer :: Nmerged_w
    integer, dimension(:)   , pointer :: isMERGEDu_w
    integer, dimension(:)   , pointer :: isMERGEDv_w
    integer, dimension(:)   , pointer :: MERGEDwith_w
    logical                 , pointer :: virtualLINK
    real(fp), dimension(:)  , pointer :: agsqs_link
    logical                 , pointer :: virtuallinkSMOw1
    integer, dimension(:,:) , pointer :: NMlistMERGED_d
    integer, dimension(:)   , pointer :: Nmerged_d
    real(fp), dimension(:,:), pointer :: Dwrka0k
    real(fp), dimension(:,:), pointer :: wphyT
!
! Global variables
!
    integer                                       , intent(in)  :: nst  
    integer                                       , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                          !!  then computation proceeds in the X-
                                                                          !!  dir. If icx=1 then computation pro-
                                                                          !!  ceeds in the Y-dir.
    integer                                       , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                       , intent(in)  :: j      !!  Begin pointer for arrays which have
                                                                          !!  been transformed into 1D arrays.
                                                                          !!  Due to the shift in the 2nd (M-)
                                                                          !!  index, J = -2*NMAX + 1
    integer                                       , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                       , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                       , intent(in)  :: nmmaxj !  Description and declaration in dimens.igs
    integer                                       , intent(in)  :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                       , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                       , intent(in)  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, norow + nocol)          , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                   , intent(in)  :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)     , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)     , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90 
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)     , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)     , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    logical                                       , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gsqsR
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: agsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: aguu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: agvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: porosu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: porosv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: wphy   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(j:nmmaxj)                     , intent(in)  :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(j:nmmaxj)                     , intent(in)  :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                         , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                         , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: disch  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: nm8(1:8)
    integer :: nm4(1:4)
    integer :: nm4c(1:4)
    integer :: nmj
    integer :: nmOK
    integer :: ddb
    integer :: nmaxddb
    integer :: i
    integer :: jj
    integer :: ibf
    integer :: ibl
    integer :: ic
    integer :: icxy
    integer :: k
    integer :: kk
    integer :: m
    integer :: mf
    integer :: mfu
    integer :: ml
    integer :: n
    integer :: ndm
    integer :: ndmd
    integer :: nf
    integer :: nfm
    integer :: nfu
    integer :: nfum
    integer :: nl
    integer :: nlm
    integer :: nlum
    integer :: nm
    integer :: nmd
    integer :: nmf
    integer :: nmfu
    integer :: nml
    integer :: nmlu
    integer :: nmu
    integer :: num
    real(fp):: h0
    real(fp):: q
    real(fp):: www
    real(fp):: wphyAV
    integer                 :: Idummy,Idummyy,Idummyyy
    logical                 :: Ldummy
    real(fp), dimension(kmax)  :: Rdummy
!
!! executable statements -------------------------------------------------------
!
    dim_nmlist           => gdp%gdimbound%dim_nmlist
    typeVIRTmergeUPDvert => gdp%gdimbound%typeVIRTmergeUPDvert
    virtualMERGEupdVERT  => gdp%gdimbound%virtualMERGEupdVERT
    cutcell              => gdp%gdimbound%cutcell
    virtualMERGEupdVERT  => gdp%gdimbound%virtualMERGEupdVERT
    typeVIRTmergeUPDvert => gdp%gdimbound%typeVIRTmergeUPDvert
    thresMERGE_w         => gdp%gdimbound%thresMERGE_w
    NMlistMERGED_w       => gdp%gdimbound%NMlistMERGED_w
    Nmerged_w            => gdp%gdimbound%Nmerged_w
    isMERGEDu_w          => gdp%gdimbound%isMERGEDu_w
    isMERGEDv_w          => gdp%gdimbound%isMERGEDv_w
    MERGEDwith_w         => gdp%gdimbound%MERGEDwith_w
    virtualLINK          => gdp%gdimbound%virtualLINK
    agsqs_link           => gdp%gdimbound%agsqs_link
    virtuallinkSMOw1     => gdp%gdimbound%virtuallinkSMOw1
    NMlistMERGED_d       => gdp%gdimbound%NMlistMERGED_d
    Nmerged_d            => gdp%gdimbound%Nmerged_d
    Dwrka0k              => gdp%gdimbound%Dwrka0k
    wphyT                => gdp%gdimbound%Dwrkak1_T
    lundia              => gdp%gdinout%lundia
    ! INITIALISATION
    !
    ddb = gdp%d%ddbound
    nmaxddb = gdp%d%nmax + 2*ddb
    icxy = max(icx, icy)
    !
    ! COMPUTATION OF PHYSICAL VERTICAL VELOCITIES
    ! WITHOUT SOURCES AND SINKS; Differentiate
    ! approach for ZMODEL and SIGMA model
    !
    if (zmodel) then
       !
       do k = 1, kmax
          do nm = 1, nmmax
             if (kcs(nm)==1 .and.                  &
               & k>=kfsmin(nm) .and. k<=kfsmax(nm)) then
                wphy(nm, k) = 0.5*(w1(nm, k) + w1(nm, k - 1))
             else
                wphy(nm, k) = 0.0
             endif
          enddo
       enddo
    else
       !
       ! SIGMA model
       !
       do k = 1, kmax
          ndm = -icy
          nmd = -icx
          num = icy
          nmu = icx
          ndmd = -icx - icy
          do nm = 1, nmmax
             ndm = ndm + 1
             nmd = nmd + 1
             num = num + 1
             nmu = nmu + 1
             ndmd = ndmd + 1
             if (kcs(nm)==1) then
                h0 = real(dps(nm),fp) + s1(nm)
                www = 0.5*(w1(nm, k) + w1(nm, k - 1))
                !
                !***COMPUTATION VERTICAL VELOCITIES
                !     IN THE MIDDLE OF THE LAYER
                !
                !   ( IN THE CASE OF A VELOCITY BOUNDARY CONDITION IN THE KSI-DIRECTION,
                !     THE ARRAY U CONTAINS IN THE BOUNDARY POINT THE VALUE AT T-HDT )
                !
                wphy(nm, k) = www                                                     &
                            & - (sig(k)*h0 + s1(nm))                                  &
                            &   *(  aguu(nm )*guu(nm )*u1(nm , k)*porosu(nm , k)                &
                            &     - aguu(nmd)*guu(nmd)*u1(nmd, k)*porosu(nmd, k)                &
                            &     + agvv(nm )*gvv(nm )*v1(nm , k)*porosv(nm , k)                &
                            &     - agvv(ndm)*gvv(ndm)*v1(ndm, k)*porosv(ndm, k))               &
                            &   /(gsqs(nm)*agsqs_link(nm))                                             &
                            & - (sig(k) + 1.0) * (w1(nm, k - 1) - w1(nm, k))/thick(k) &
                            & - (  aguu(nm )*guu(nm )*u1(nm , k)*dpu(nm )*porosu(nm , k)        &
                            &    - aguu(nmd)*guu(nmd)*u1(nmd, k)*dpu(nmd)*porosu(nmd, k)        &
                            &    + agvv(nm )*gvv(nm )*v1(nm , k)*dpv(nm )*porosv(nm , k)        &
                            &    - agvv(ndm)*gvv(ndm)*v1(ndm, k)*dpv(ndm)*porosv(ndm, k))       &
                            &   /(gsqs(nm)*agsqs_link(nm)) 
             else
                wphy(nm, k) = 0.0
             endif
          enddo
       enddo
       !
       ! ADDING SOURCES AND SINKS
       !
       do i = 1, nsrc
          nm = (mnksrc(5, i) + ddb) + ((mnksrc(4, i) - 1) + ddb)*icxy
          k = mnksrc(6, i)
          if (k==-1) then
             cycle
          else if (k==0) then
             do kk = 1, kmax
                q = thick(kk)*disch(i)
                wphy(nm, kk) = wphy(nm, kk) + (sig(kk) + 1.0)                   &
                             & *q/(agsqs_link(nm)*gsqs(nm)*thick(kk))
             enddo
          else
             q = disch(i)
             wphy(nm, k) = wphy(nm, k) + (sig(k) + 1.0)*q/(agsqs_link(nm)*gsqs(nm)*thick(k))
          endif
          !
          ! in case of an intake for an intake/outfall combination:
          !
          if (mnksrc(7, i)>=2) then
             nm = (mnksrc(2, i) + ddb) + ((mnksrc(1, i) + ddb) - 1)*icxy
             k = mnksrc(3, i)
             if (k==0) then
                do kk = 1, kmax
                   q = -thick(kk)*disch(i)
                   wphy(nm, kk) = wphy(nm, kk) + (sig(kk) + 1.0)                &
                                & *q/(agsqs_link(nm)*gsqs(nm)*thick(kk))
                enddo
             else
                q = -disch(i)
                wphy(nm, k) = wphy(nm, k) + (sig(k) + 1.0)*q/(agsqs_link(nm)*gsqs(nm)*thick(k))
             endif
          endif
       enddo
    endif
    !
   ! for small cutcells (agsqs<thres): they are virtually merged with large cells
    if (cutcell.gt.0.and.(virtualMERGEupdVERT.or.virtuallink)) THEN
          CALL COMPUTEmergingCARATT(kcs,kfs,agsqs,aguu,agvv,icx,icy,nmmax,gdp%d%nmlb,gdp%d%nmub,nst,lundia,&
                            virtualMERGEupdVERT,typeVIRTmergeUPDvert,thresMERGE_w,NMlistMERGED_w,Nmerged_w,&
                            isMERGEDu_w,isMERGEDv_w,MERGEDwith_w,1._fp,dim_nmlist,gdp)  
          call REDUCEgsqs(gsqs,agsqs_link,gsqsR,gdp%d%nmlb,gdp%d%nmub)  !virtMERG wants gsqsR
          if (.NOT.virtualLINK) THEN
             !virtual merge of wphy
             CALL TRANSPOSE_wrapper(wphy,gdp%d%nmlb,gdp%d%nmub,1,kmax,wphyT)
             CALL virtMERG(wphyT,gsqsR,s1,dps,Rdummy,icx,icy,nmmax,gdp%d%nmlb,gdp%d%nmub,nst,1,kmax,1,kmax,lundia,Ldummy,&  !1,kmax,1,kmax: ini vector,end vector,iniCYCLE,endCYCLE
                           Idummy,Idummyy,Idummyyy,0,nmaxddb,gdp%d%ddbound,& !0 do not check large bed variations
                           NMlistMERGED_w,Nmerged_w, dim_nmlist)
             CALL TRANSPOSE_wrapper(wphyT,1,kmax,gdp%d%nmlb,gdp%d%nmub,wphy)
          else
              if (virtuallinkSMOw1) then
                  call VIRTUALlinkAVER_vert(gsqsR,agsqs,gsqs,Dwrka0k,wphy,NMlistMERGED_w,Nmerged_w,icx,icy,nmmax,gdp%d%nmlb,gdp%d%nmub,nst,1,kmax, dim_nmlist) 
              else
                  call VIRTUALlinkAVER_vert(gsqsR,agsqs,gsqs,Dwrka0k,wphy,NMlistMERGED_d,Nmerged_d,icx,icy,nmmax,gdp%d%nmlb,gdp%d%nmub,nst,1,kmax, dim_nmlist) 
              endif
          endif
    endif

    !
    !-REFLECTION AT OPEN BOUNDARIES; ZMODEL and SIGMA model
    ! identical approach
    !
    !-LOOP OVER GRID ROWS FOR WATERLEVEL BOUNDARY
    !
    do k = 1, kmax
       do ic = 1, norow
          n = irocol(1, ic)
          mfu = irocol(2, ic)
          ml = irocol(3, ic)
          ibf = irocol(4, ic)
          ibl = irocol(5, ic)
          if (ibf==2) then
             mf = mfu - 1
             nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
             nmfu = nmf + icx
             wphy(nmf, k) = wphy(nmfu, k)
          endif
          if (ibl==2) then
             nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
             nmlu = nml + icx
             wphy(nmlu, k) = wphy(nml, k)
          endif
       enddo
       !
       ! LOOP OVER GRID COLUMNS FOR WATERLEVEL BOUNDARY
       !
       do ic = 1 + norow, norow + nocol
          m = irocol(1, ic)
          nfu = irocol(2, ic)
          nl = irocol(3, ic)
          ibf = irocol(4, ic)
          ibl = irocol(5, ic)
          if (ibf==2) then
             nf = nfu - 1
             nfm = (nf + ddb)*icy + (m + ddb)*icx - icxy
             nfum = nfm + icy
             wphy(nfm, k) = wphy(nfum, k)
          endif
          if (ibl==2) then
             nlm = (nl + ddb)*icy + (m + ddb)*icx - icxy
             nlum = nlm + icy
             wphy(nlum, k) = wphy(nlm, k)
          endif
       enddo
    enddo
end subroutine wphys
