SUBROUTINE PLIC_VOF_STEP(gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,dps,dpU,dpV,xcor,ycor,alfas,&
                         lunscr,lundia,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,drycrt,&
                         thick,guu,gvv,hu,hv,porosu,porosv,qxk,qyk,Umean,Vmean,stage,kfumn0,kfvmn0,kfumx0,kfvmx0,ddbound,nmmax,Zmodel, gdp)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)                      , pointer :: thresMERGE_d
    real(fp)                      , pointer :: thresMERGE_zb
    real(fp)                      , pointer :: facMERGElink
    integer                       , pointer :: dim_nmlist
    integer                       , pointer :: totGHOSTu1
    integer                       , pointer :: totGHOSTv1
    integer                       , pointer :: cutcell
    integer                       , pointer :: GhostMethod
    integer                       , pointer :: TYPEfreeSLIP
    integer                       , pointer :: typeVIRTmergeUPDbed
    integer                       , pointer :: typeVIRTmergeUPDdepth
    integer, dimension(:,:)       , pointer :: kfs_cc
    integer, dimension(:,:)       , pointer :: kWDu
    integer, dimension(:,:)       , pointer :: kWDv
    integer, dimension(:)         , pointer :: MERGEDwith_d
    integer, dimension(:)         , pointer :: MERGEDwith_bed
    integer, dimension(:)         , pointer :: isMERGEDu_bed
    integer, dimension(:)         , pointer :: isMERGEDv_bed
    integer, dimension(:)         , pointer :: isMERGEDu_d
    integer, dimension(:)         , pointer :: isMERGEDv_d
    integer, dimension(:,:)       , pointer :: NMlistMERGED_bed
    integer, dimension(:,:)       , pointer :: NMlistMERGED_d
    integer, dimension(:)         , pointer :: Nmerged_bed
    integer, dimension(:)         , pointer :: Nmerged_d
    integer, dimension(:,:)       , pointer :: GHOSTu1
    integer, dimension(:,:)       , pointer :: GHOSTv1
    real(fp), dimension(:,:)      , pointer :: dpL
    real(fp), dimension(:,:)      , pointer :: POROS
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: agsqs
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp), dimension(:,:)      , pointer :: xcorU1
    real(fp), dimension(:,:)      , pointer :: ycorU1
    real(fp), dimension(:,:)      , pointer :: xcorV1
    real(fp), dimension(:,:)      , pointer :: ycorV1
    real(fp), dimension(:,:)      , pointer :: PSIcorU1
    real(fp), dimension(:,:)      , pointer :: ETAcorV1
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:,:)      , pointer :: psiG_V1
    real(fp), dimension(:,:)      , pointer :: etaG_U1
    logical                       , pointer :: periodSURFACE
    logical                       , pointer :: periodGHOST
    logical                       , pointer :: virtualMERGEupdBED
    logical                       , pointer :: virtualMERGEupdDEPTH
    logical                       , pointer :: virtualLINK
    logical                       , pointer :: shift_xycor
    real(fp), dimension(:)        , pointer :: gsqsR
!
   integer itstrt
!
    real(fp), dimension(kmax)                                           , intent(inout) :: thick 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: xcor
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: ycor
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in)    :: porosu 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in)    :: porosv
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: qxk  
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: qyk 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: Umean 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: Vmean 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: alfas
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: dpu 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: dpv 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(inout) :: dps
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfu
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfv
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcu
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcv
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfumn0 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfvmn0 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfumx0 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfvmx0 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lundia   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    real(fp)                                                            , intent(in)    :: drycrt
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: ddbound
    integer                                                             , intent(in)    :: nmmax
    character(8)                                                        , intent(in)    :: stage   !!  First or Second half time step
    logical                                                             , intent(in)    :: Zmodel
!
!   local variables
!
    integer                             :: m
    integer                             :: n
    integer                             :: icx
    integer                             :: icy
    integer                             :: iPROV
    integer                             :: nmaxddb
    integer                             :: Idummy
    integer                             :: Idummyy
    integer                             :: Idummyyy
    logical,SAVE                        :: firstCALL = .TRUE.
    !real(fp), dimension(nlb:nub,mlb:mub):: gsqsR
    real(fp), dimension(1)              :: Rdummy1
    logical                             :: Ldummy

!
    thresMERGE_d          => gdp%gdimbound%thresMERGE_d
    thresMERGE_zb         => gdp%gdimbound%thresMERGE_zb
    facMERGElink          => gdp%gdimbound%facMERGElink
    dim_nmlist            => gdp%gdimbound%dim_nmlist
    totGHOSTu1            => gdp%gdimbound%totGHOSTu1
    totGHOSTv1            => gdp%gdimbound%totGHOSTv1
    cutcell               => gdp%gdimbound%cutcell
    GhostMethod           => gdp%gdimbound%GhostMethod
    TYPEfreeSLIP          => gdp%gdimbound%TYPEfreeSLIP
    typeVIRTmergeUPDbed   => gdp%gdimbound%typeVIRTmergeUPDbed
    typeVIRTmergeUPDdepth => gdp%gdimbound%typeVIRTmergeUPDdepth
    kfs_cc                => gdp%gdimbound%kfs_cc
    kWDu                  => gdp%gdimbound%kWDu
    kWDv                  => gdp%gdimbound%kWDv
    MERGEDwith_d          => gdp%gdimbound%MERGEDwith_d
    MERGEDwith_bed        => gdp%gdimbound%MERGEDwith_bed
    isMERGEDu_bed         => gdp%gdimbound%isMERGEDu_bed
    isMERGEDv_bed         => gdp%gdimbound%isMERGEDv_bed
    isMERGEDu_d           => gdp%gdimbound%isMERGEDu_d
    isMERGEDv_d           => gdp%gdimbound%isMERGEDv_d
    NMlistMERGED_bed      => gdp%gdimbound%NMlistMERGED_bed
    NMlistMERGED_d        => gdp%gdimbound%NMlistMERGED_d
    Nmerged_bed           => gdp%gdimbound%Nmerged_bed
    Nmerged_d             => gdp%gdimbound%Nmerged_d
    GHOSTu1               => gdp%gdimbound%GHOSTu1
    GHOSTv1               => gdp%gdimbound%GHOSTv1
    dpL                   => gdp%gdimbound%dpL
    POROS                 => gdp%gdimbound%POROS
    PSIx                  => gdp%gdimbound%PSIx
    PSIy                  => gdp%gdimbound%PSIy
    ETAx                  => gdp%gdimbound%ETAx
    ETAy                  => gdp%gdimbound%ETAy
    EDGExyBANK            => gdp%gdimbound%EDGExyBANK
    agsqs                 => gdp%gdimbound%agsqs
    aguu                  => gdp%gdimbound%aguu
    agvv                  => gdp%gdimbound%agvv
    xcorU1                => gdp%gdimbound%xcorU1
    ycorU1                => gdp%gdimbound%ycorU1
    xcorV1                => gdp%gdimbound%xcorV1
    ycorV1                => gdp%gdimbound%ycorV1
    PSIcorU1              => gdp%gdimbound%PSIcorU1
    ETAcorV1              => gdp%gdimbound%ETAcorV1
    xG_V1                 => gdp%gdimbound%xG_V1
    xG_U1                 => gdp%gdimbound%xG_U1
    yG_V1                 => gdp%gdimbound%yG_V1
    yG_U1                 => gdp%gdimbound%yG_U1
    psiG_V1               => gdp%gdimbound%psiG_V1
    etaG_U1               => gdp%gdimbound%etaG_U1
    periodSURFACE         => gdp%gdimbound%periodSURFACE
    periodGHOST           => gdp%gdimbound%periodGHOST
    virtualMERGEupdBED    => gdp%gdimbound%virtualMERGEupdBED
    virtualMERGEupdDEPTH  => gdp%gdimbound%virtualMERGEupdDEPTH
    virtualLINK           => gdp%gdimbound%virtualLINK
    shift_xycor           => gdp%gdimbound%shift_xycor
    gsqsR                 => gdp%gdimbound%Dwrka1
   !
    Idummy = 0;Idummyy=0;Idummyyy=0; Rdummy1=0._fp; Ldummy = .true. !initialized but never needed
    nmaxddb = nmax + 2*ddbound
    icx   = nmaxddb
    icy   = 1
    iPROV = itstrt
999 continue
    call checkDRY(gsqs,kfs,kfu,kfv,kcs,s1,u1,v1,dps,alfas,lunscr,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,drycrt,Zmodel,gdp)   
    call reconVOF(gsqs  , kfs   , kcs   , kfu , kfv  , kcu   , kcv   , &
                & s1    , u1    , v1    , qxk , qyk  , dps   , hu    , hv  , &
                & dpu   , dpv   , guu   , gvv , thick, lunscr, lundia, irov, &
                & mmax  , nmax  , nmaxus, kmax, nst  , &
                & nlb   , nub   , mlb   , mub , nmlb , nmub  , &
                & zmodel, drycrt, gdp   )
    if (firstCALL) then
    !      correct depths only if first call (chkdry calls upwhu with aguu/agvv still 1 everywhere), so hu wrong there
       do m=1,mmax
          do n = 1,nmax
             if (zmodel) then
                write(*,*) 'shuould be correct, but check if aguu has to be used to set hu to zero'
             endif
             if (hu(n,m).gt.0._fp.and.comparereal(aguu(n,m),0._fp)==0) then
                hu(n,m) =-10._fp !random negative value
             endif
             if (hv(n,m).gt.0._fp.and.comparereal(agvv(n,m),0._fp)==0) then
                hv(n,m) =-10._fp !random negative value
             endif
          enddo
       enddo
    endif
    if (firstCALL.and.(virtualMERGEupdBED.or.virtualMERGEupdDEPTH)) then
       !THE first time I need to average out the bed in the small cut and adjacent,in a way that they always have the same bed elevation
       ! since virtMERG change the value of dpL, it might be that checkDRY give different type of kfs_cc (and so also reconVOF
       ! would be different and so they are recomputed
       !I DO IT WITH POROS AT THE END SO I THINK THE GOTO IS NOT NEEDED ANYMORE 
       call COMPUTEmergingCARATT(kcs,kfs,poros,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,& !poros will be agsqs
                                 virtualMERGEupdBED,typeVIRTmergeUPDbed,thresMERGE_zb,NMlistMERGED_bed,Nmerged_bed,&
                                 isMERGEDu_bed,isMERGEDv_bed,MERGEDwith_bed,1._fp,dim_nmlist,gdp) 
       call REDUCEgsqs(gsqs,poros,gsqsR,nmlb,nmub) !virtMERG wants gsqs*agsqs as actual argument
       !merge dps
       CALL virtMERG(dps,gsqsR,s1,dps,Rdummy1,icx,icy,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,&  !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                     Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,& !0 do not check large bed variations
                     NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
       !merge dpL
       CALL virtMERG(dpL,gsqsR,s1,dps,Rdummy1,icx,icy,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,&  !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                     Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,& !0 do not check large bed variations
                     NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
       !merge s1
       if (virtualMERGEupdDEPTH.or.virtualLINK) then
         CALL COMPUTEmergingCARATT(kcs,kfs,poros,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,&
                           virtualMERGEupdDEPTH,typeVIRTmergeUPDdepth,thresMERGE_d,NMlistMERGED_d,Nmerged_d,&
                           isMERGEDu_d,isMERGEDv_d,MERGEDwith_d,facMERGElink,dim_nmlist,gdp)  
        ! call REDUCEgsqs(gsqs,agsqs,gsqsR,nmlb,nmub) !virtMERG wants gsqs*agsqs as actual argument
         !virtual merge of s1 (SINCE dps is the same in 
         CALL virtMERG(s1,gsqsR,s1,dps,Rdummy1,icx,icy,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,& !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                     Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,& !0 do not check large bed variations
                     NMlistMERGED_d,Nmerged_d, dim_nmlist)
          
       endif

       firstCALL = .false.
       goto 999 
    endif
    if (zmodel) then
     write(*,*) 'Initialize kgp,maybe needed for GhostMethod==3'
    endif
    if(shift_xycor.and.(TYPEfreeSLIP.ne.2)) then
       !shift coordinates of baricenter of edge (xcorV1(n-1,m),ycorV1(n-1,m) are the coord of the U1(n,m) velocity point)
       call SHIFT_xycorU1(PSIx        ,PSIy        ,guu         ,aguu        ,xcorV1      ,ycorV1     ,&   
                        & xG_U1       ,yG_U1       ,EDGExyBANK  ,ETAcorV1    ,etaG_U1     ,nst        ,&
                        & kmax        ,nmmax       ,&
                        & nmlb        ,nmub        ,icx         ,icy         ,xcor        ,ycor       ,gdp)
       call SHIFT_xycorU1(ETAx        ,ETAy        ,gvv         ,agvv        ,xcorU1      ,ycorU1     ,&   
                        & xG_V1       ,yG_V1       ,EDGExyBANK  ,PSIcorU1    ,psiG_V1     ,nst        ,&
                        & kmax        ,nmmax       ,&
                        & nmlb        ,nmub        ,icy         ,icx         ,xcor        ,ycor       ,gdp)
    endif

    call FindGhostPoints(gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,Zmodel,gdp)
    if (cutcell.gt.0.and.(GhostMethod.eq.0.or.GhostMethod.eq.1.or.GhostMethod.eq.2)) THEN
       if (zmodel) call z_defineKGP(GHOSTu1,GHOSTv1,kfumn0,kfvmn0,kfumx0,kfvmx0,kfu,kfv,icx,icy,lunscr,nst,nmmax,nmlb,nmub, gdp)

     !  IF (TYPEfreeSLIP/=2) then   !if scommented release crashes, probably nIP mIP are indefined and samwhere are used (printing?)
          call Find_BI_PI(gsqs,kfs,kcs,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,Zmodel,gdp)
     !  ENDIF
       if (periodSURFACE) then
          if(periodGHOST) call extrapPERIODICghost(gdp) !before Compute_inSTENCILuv so it has the right values of ghostu1 and ghostv1 to define stancilu and stencilv
       endif
       call Compute_inSTENCILuv(lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,zmodel,gdp)
       call compute_kwduv(icx,icy,kWDu,ghostu1,totGHOSTu1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,gdp)
       call compute_kwduv(icy,icx,kWDv,ghostv1,totGHOSTv1,kfs_cc,lunscr,kmax,nst,nmlb,nmub,ddbound,gdp)
    endif
    !I want to set kfsuv to the right value on the cut cell throuh ghosttype_is_kf
  !  call ghosttype_is_kf(kfs,kfu,kfv,mmax,nmax,nmaxus,kmax,nlb,nub,mlb,mub,nmlb,nmub)      
  !  call Find_BI_PI(gsqs,kfs,kcs,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub)
  !  call InterpGhost_s_u_v(gsqs,kfs,kcs,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub)
    firstCALL = .false.
RETURN
END



