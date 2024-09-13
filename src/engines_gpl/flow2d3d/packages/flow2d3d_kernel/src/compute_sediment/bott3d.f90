subroutine bott3d(nmmax     ,kmax      ,lsed      ,timnow    ,lsedtot  , &
                & lsal      ,ltem      ,kfs       ,kfu       ,kfv       , &
                & r1        ,s0        ,kcs       , &
                & dps       ,gsqs      ,guu       ,agsqs     ,&
                & gvv       ,s1        ,thick     ,dp        , &
                & umean     ,vmean     ,sbuu      ,sbvv      , &
                & depchg    ,nst       ,hu        , &
                & hv        ,sig       ,u1        ,v1        , &
                & sscomp    ,kcsbot    , &
                & guv       ,gvu       ,kcu       , &
                & kcv       ,icx       ,icy       ,timhr     , &
                & nto       ,volum0    ,volum1    ,dt        ,aguu      , &
                & agvv      ,dpL       ,dpH       ,poros     ,kfs_cc    , &
                & gdp       )
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
!  $Id: bott3d.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/bott3d.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes suspended sediment transport correction
!              vector for sand sediment fractions
!              Computes depth integrated suspended sediment
!              transport vector for output to map file
!              Computes change in BODSED based on source and sink
!              terms calculated in EROSED, and new concentrations.
!              Calculates new mixing layer thickness based on
!              change in BODSED values
!              Calculates new depth values based on changes
!              in bottom sediment.
!              Includes erosion of dry points and associated
!              bathymetry changes
! Method used: Attention: pointer ll for 'standard' FLOW
!              arrays is shifted with lstart
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use flow_tables
    use bedcomposition_module
    use globaldata
    use dfparall
    use sediment_basics_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'flow_steps_f.inc'
    integer                              , pointer :: itstrt
    integer                              , pointer :: itlfsm
    integer                              , pointer :: lundia
    real(hp)                             , pointer :: hydrt
    real(hp)                             , pointer :: morft
    real(fp)                             , pointer :: morfac
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: tmor
    real(fp)                             , pointer :: thetsd
    real(fp)                             , pointer :: sedthr
    real(fp)                             , pointer :: hmaxth
    real(fp)                             , pointer :: tstart
    integer                              , pointer :: mergehandle
    integer                              , pointer :: itmor
    integer                              , pointer :: MAXnpnt
    type (handletype)                    , pointer :: bcmfile
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
    real(hp)              , dimension(:) , pointer :: mergebuf
    logical                              , pointer :: bedupd
    logical                              , pointer :: cmpupd
    logical                              , pointer :: neglectentrainment
    logical                              , pointer :: multi
    logical                              , pointer :: distr_bdl_per
    logical                              , pointer :: wind
    logical                              , pointer :: temp
    logical                              , pointer :: const
    logical                              , pointer :: dredge
    logical                              , pointer :: struct
    logical                              , pointer :: sedim
    logical                              , pointer :: scour
    logical                              , pointer :: snelli
    real(fp), dimension(:)               , pointer :: factor
    real(fp)                             , pointer :: slope
    real(fp), dimension(:)               , pointer :: bc_mor_array
    real(fp), dimension(:,:)             , pointer :: dbodsd
    real(fp), dimension(:)               , pointer :: dm
    real(fp), dimension(:)               , pointer :: dg
    real(fp), dimension(:,:)             , pointer :: fixfac
    real(fp), dimension(:,:)             , pointer :: frac
    integer , dimension(:)               , pointer :: kfsed
    integer , dimension(:,:)             , pointer :: kmxsed
    real(fp), dimension(:)               , pointer :: mudfrac
    real(fp), dimension(:,:)             , pointer :: sbuuc
    real(fp), dimension(:,:)             , pointer :: sbvvc
    real(fp), dimension(:,:)             , pointer :: ssuu
    real(fp), dimension(:,:)             , pointer :: ssvv
    real(fp), dimension(:,:)             , pointer :: ssuuc
    real(fp), dimension(:,:)             , pointer :: ssvvc
    real(fp), dimension(:,:)             , pointer :: sucor
    real(fp), dimension(:,:)             , pointer :: svcor
    real(fp), dimension(:,:)             , pointer :: aks
    real(fp), dimension(:,:)             , pointer :: rca
    real(fp), dimension(:,:)             , pointer :: sinkse
    real(fp), dimension(:,:)             , pointer :: sourse
    integer                              , pointer :: nmudfrac
    real(fp)      , dimension(:)         , pointer :: rhosol
    real(fp)      , dimension(:)         , pointer :: cdryb
    integer       , dimension(:)         , pointer :: sedtyp
    integer                              , pointer :: julday
    integer                              , pointer :: ntstep
    real(fp), dimension(:,:,:)           , pointer :: fluxu
    real(fp), dimension(:,:,:)           , pointer :: fluxv
    real(fp), dimension(:)               , pointer :: duneheight
    integer                              , pointer :: iflufflyr
    real(fp), dimension(:,:)             , pointer :: mfluff
    real(fp), dimension(:,:)             , pointer :: sinkf
    real(fp), dimension(:,:)             , pointer :: sourf
!
! Local parameters
!
    integer, parameter :: bedchangemessmax = 50
    integer                 , pointer :: cutcell
    integer                 , pointer :: dim_nmlist
    logical                 , pointer :: virtualMERGEupdBED
    integer                 , pointer :: distQHn
    integer                 , pointer :: distQHm
    real(fp), dimension(:,:), pointer :: qfilt_bdl
    real(fp)                , pointer :: reltim_qtq_bdl
    integer                 , pointer :: nrPER
    logical                 , pointer :: bedUPDandFIXdepth
    real(fp)                , pointer :: thresMERGE_zb
    real(fp)                , pointer :: perSMOfac_Qb
    logical                 , pointer :: distr_qtq_bdl_NNprism
    integer, dimension(:,:) , pointer :: NMlistMERGED_bed
    integer, dimension(:)   , pointer :: Nmerged_bed
    logical                 , pointer :: USEfixedBEDequilQb
    integer                 , pointer :: idebugCUThardINI
    integer                 , pointer :: idebugCUThardFIN
    real(fp)                , pointer :: DELAYfixedBEDequilQS
    real(fp), dimension(:)  , pointer :: depchgH
    real(fp), dimension(:)  , pointer :: depth0
!
! Global variables
!
    integer                                            , intent(in)  :: icx
    integer                                            , intent(in)  :: icy
    integer                                            , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                            , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: lsedtot!  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                            , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                            , intent(in)  :: nto    !  Number of open boundaries (esm_alloc_int.igs)
    integer                                            , intent(in)  :: nst
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcsbot
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    logical                                            , intent(in)  :: sscomp
    real(fp)                                           , intent(in)  :: timnow !!  Current timestep (multiples of dt)
    real(fp)                                           , intent(in)  :: dt
    real(fp)                                                         :: timhr
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: agsqs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: aguu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: agvv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(out) :: dpL       
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(out) :: dpH       
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: poros     
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kfs_cc 
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: depchg !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: dp     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                     :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(inout)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(inout)  :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: volum0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: volum1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)              :: sbuu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)              :: sbvv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                          , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                          , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: nmaxddb
    integer  :: i
    integer  :: ib
    integer  :: icond
    integer  :: idir_scalar
    integer  :: jb
    integer  :: k
    integer  :: kvalue
    integer  :: l
    integer  :: J
    integer  :: li
    integer  :: ll
    integer  :: lsedbed
    integer  :: lstart
    integer  :: m
    integer  :: n
    integer  :: ndm
    integer  :: nmORT
    integer  :: nmORTm1
    integer  :: nhystp
    integer  :: nm
    integer  :: nmd
    integer  :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    integer  :: nmu
    integer  :: num
    integer  :: numu
    integer  :: nxmx
    integer  :: bedchangemesscount
    logical  :: bedload
    logical  :: from_ndm
    logical  :: from_nmd
    logical  :: from_nmu
    logical  :: from_num
    integer  :: nm8(1:8)
    integer  :: nm4(1:4)
    integer  :: nm4c(1:4)
    integer  :: nmj
    integer  :: nmOK
    integer  :: nmEX
    integer  :: shiftPERnm
    real(fp) :: itlfsm_per_Qs
    real(fp) :: tdif
    real(fp) :: Qini
    real(fp) :: tfrac
    real(fp) :: diff
    real(fp) :: aksu
    real(fp) :: apower
    real(fp) :: cavg
    real(fp) :: cavg1
    real(fp) :: cavg2
    real(fp) :: ceavg
    real(fp) :: cumflux
    real(fp) :: dhmax
    real(fp) :: alfa_dist
    real(fp) :: alfa_mag
    real(fp) :: dsdnm
    real(fp) :: dv
    real(fp) :: dz
    real(fp) :: eroflx
    real(fp) :: fact
    real(fp) :: gsqsmin
    real(fp) :: gsqsinv
    real(fp) :: h1
    real(fp) :: h2
    real(fp) :: dtmor
    real(fp) :: htdif
    real(fp) :: rate
    real(fp) :: rateAD
    real(fp) :: rateADi(gdp%gdmorpar%MAXnpnt)
    real(fp) :: r1avg
    real(fp) :: sedflx
    real(fp) :: thet
    real(fp) :: thick0
    real(fp) :: thick1
    real(fp) :: totdbodsd
    real(fp) :: totfixfrac
    real(fp) :: trndiv
    real(fp) :: width(gdp%gdmorpar%MAXnpnt)
    real(fp) :: widthAD(gdp%gdmorpar%MAXnpnt)
    real(fp) :: widthtot
    real(fp) :: z
    real(fp) :: dbodsdAV
    real(fp) :: rateNEW
    real(fp) :: a
    real(fp),allocatable,save :: Qb_EQ(:,:)
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: depchgH ! variation of HIGH bed eleveation in cutcell method
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: depth0
    logical,SAVE :: firstCALL = .TRUE.
    logical  :: exceeded
    integer  :: nxmxB(gdp%gdmorpar%MAXnpnt)
    integer  :: nxmxAD(gdp%gdmorpar%MAXnpnt)
!
!! executable statements -------------------------------------------------------
!
    cutcell               => gdp%gdimbound%cutcell
    dim_nmlist            => gdp%gdimbound%dim_nmlist
    virtualMERGEupdBED    => gdp%gdimbound%virtualMERGEupdBED
    distQHn               => gdp%gdimbound%distQHn
    distQHm               => gdp%gdimbound%distQHm
    qfilt_bdl             => gdp%gdimbound%qfilt_bdl
    reltim_qtq_bdl        => gdp%gdimbound%reltim_qtq_bdl
    nrPER                 => gdp%gdimbound%nrPER
    bedUPDandFIXdepth     => gdp%gdimbound%bedUPDandFIXdepth
    thresMERGE_zb         => gdp%gdimbound%thresMERGE_zb
    perSMOfac_Qb          => gdp%gdimbound%perSMOfac_Qb
    distr_qtq_bdl_NNprism => gdp%gdimbound%distr_qtq_bdl_NNprism
    NMlistMERGED_bed      => gdp%gdimbound%NMlistMERGED_bed
    Nmerged_bed           => gdp%gdimbound%Nmerged_bed
    USEfixedBEDequilQb    => gdp%gdimbound%USEfixedBEDequilQb
    idebugCUThardINI      => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN      => gdp%gdimbound%idebugCUThardFIN
    DELAYfixedBEDequilQS  => gdp%gdimbound%DELAYfixedBEDequilQS
    depchgH               => gdp%gdimbound%Dwrka1
    depth0                => gdp%gdimbound%Dwrka2
    itstrt              => gdp%gdinttim%itstrt
    itlfsm              => gdp%gdinttim%itlfsm
    tstart              => gdp%gdexttim%tstart
    lundia              => gdp%gdinout%lundia
    hydrt               => gdp%gdmorpar%hydrt
    morft               => gdp%gdmorpar%morft
    morfac              => gdp%gdmorpar%morfac
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    tmor                => gdp%gdmorpar%tmor
    thetsd              => gdp%gdmorpar%thetsd
    sedthr              => gdp%gdmorpar%sedthr
    hmaxth              => gdp%gdmorpar%hmaxth
    mergehandle         => gdp%gdmorpar%mergehandle
    itmor               => gdp%gdmorpar%itmor
    bcmfile             => gdp%gdmorpar%bcmfile
    morbnd              => gdp%gdmorpar%morbnd
    mergebuf            => gdp%gdmorpar%mergebuf
    bedupd              => gdp%gdmorpar%bedupd
    cmpupd              => gdp%gdmorpar%cmpupd
    neglectentrainment  => gdp%gdmorpar%neglectentrainment
    multi               => gdp%gdmorpar%multi
    MAXnpnt             => gdp%gdmorpar%MAXnpnt
    wind                => gdp%gdprocs%wind
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    dredge              => gdp%gdprocs%dredge
    struct              => gdp%gdprocs%struct
    sedim               => gdp%gdprocs%sedim
    snelli              => gdp%gdprocs%snelli
    scour               => gdp%gdscour%scour
    factor              => gdp%gdscour%factor
    slope               => gdp%gdscour%slope
    bc_mor_array        => gdp%gderosed%bc_mor_array
    dbodsd              => gdp%gderosed%dbodsd
    dm                  => gdp%gderosed%dm
    dg                  => gdp%gderosed%dg
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    kfsed               => gdp%gderosed%kfsed
    kmxsed              => gdp%gderosed%kmxsed
    mudfrac             => gdp%gderosed%mudfrac
    sbuuc               => gdp%gderosed%e_sbnc
    sbvvc               => gdp%gderosed%e_sbtc
    ssuu                => gdp%gderosed%e_ssn
    ssvv                => gdp%gderosed%e_sst
    ssuuc               => gdp%gderosed%e_ssnc
    ssvvc               => gdp%gderosed%e_sstc
    sucor               => gdp%gderosed%e_scrn
    svcor               => gdp%gderosed%e_scrt
    aks                 => gdp%gderosed%aks
    rca                 => gdp%gderosed%rca
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    nmudfrac            => gdp%gdsedpar%nmudfrac
    rhosol              => gdp%gdsedpar%rhosol
    cdryb               => gdp%gdsedpar%cdryb
    sedtyp              => gdp%gdsedpar%sedtyp
    julday              => gdp%gdinttim%julday
    ntstep              => gdp%gdinttim%ntstep
    fluxu               => gdp%gdflwpar%fluxu
    fluxv               => gdp%gdflwpar%fluxv
    duneheight          => gdp%gdbedformpar%duneheight
    iflufflyr           => gdp%gdmorpar%flufflyr%iflufflyr
    mfluff              => gdp%gdmorpar%flufflyr%mfluff
    sinkf               => gdp%gdmorpar%flufflyr%sinkf
    sourf               => gdp%gdmorpar%flufflyr%sourf
    distr_bdl_per       => gdp%gdbcdat%distr_bdl_per
    !
    lstart  = max(lsal, ltem)
    bedload = .false.
    dtmor   = dt*morfac
    nm_pos  = 1
    if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) then
       do k = 1,nmmax
          write(98919900,'(2i6,15f21.15)') nst,k,dps(k),dpL(k),dpH(k)
       enddo
    endif
    !
    !   Calculate suspended sediment transport correction vector (for SAND)
    !   Note: uses GLM velocites, consistent with DIFU
    !
    !   Correct suspended sediment transport rates by estimating the
    !   quantity of suspended sediment transported in the grid cells below
    !   Van Rijn's reference height (aks) and making a vector of this in the
    !   opposite direction to the suspended sediment transport.
    !
    !   ensure suspended sediment correction arrays and suspended sediment
    !   vector arrays are blank
    !
    !
    depchgH = 0._fp
    sucor = 0.0_fp
    svcor = 0.0_fp
    ssuu  = 0.0_fp
    ssvv  = 0.0_fp
    !
    ! calculate corrections
    !
    if (sus /= 0.0_fp) then
       !
       ! suspension transport correction vector only for 3D
       !
       if (kmax > 1) then
          do l = 1, lsed
             ll = lstart + l
             if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
                do nm = 1, nmmax
                   nmu = nm + icx
                   num = nm + icy
                   !
                   ! try new approach - should be smoother
                   ! don't worry about direction of the flow
                   ! use concentration at velocity point=average of the
                   ! two adjacent concentrations
                   ! use aks height at velocity point = average of the
                   ! two adjacent aks values
                   !
                   ! note correction vector only computed for velocity
                   ! points with active sediment cells on both sides
                   !
                   ! u direction
                   !
                   if ((kfu(nm)*kfsed(nm)*kfsed(nmu)) /= 0) then
                      cumflux = 0.0_fp
                      if (kcs(nmu) == 3) then
                         aksu = aks(nm, l)
                      elseif (kcs(nm) == 3) then
                         aksu = aks(nmu, l)
                      else
                         aksu = (aks(nm, l) + aks(nmu, l)) / 2.0_fp
                      endif
                      !
                      ! work up through layers integrating transport
                      ! below aksu
                      !
                      do k = kmax, 1, -1
                         kvalue = k
                         htdif  = aksu/hu(nm) - (1.0_fp + sig(k) - thick(k) / 2.0_fp)
                         !
                         ! if layer containing aksu
                         !
                         if (htdif <= thick(k)) then
                            cumflux = cumflux + fluxu(nm, k, ll)*htdif/thick(k)
                            exit
                         else
                            cumflux = cumflux + fluxu(nm, k, ll)
                         endif
                      enddo
                      if (comparereal(aguu(nm),0._fp)/=0) then !to be removed when reconvof fixed (see below)
                         cumflux = cumflux / (guu(nm)*aguu(nm)) !Here aguu has to be the % of active wet edge that is never zero when kfu=1. Otherwise I have div by zero 
                      else 
                         cumflux = 0._fp
                      endif
                      !
                      ! integration finished
                      ! suspended transport correction = opposite of the
                      ! transport below aksu
                      !
                      ! Approximation of the additional transport between bottom of
                      ! kmaxsd layer and za has been included in the correction vector
                      !
                      k = kvalue
                      if (k > 1) then
                         if (kcs(nmu) == 3) then
                            !
                            ! correction for domain decomposition:
                            !
                            ceavg = rca(nm, l)
                         elseif (kcs(nm) == 3) then
                            ceavg = rca(nmu, l)
                         else
                            ceavg = (rca(nm, l) + rca(nmu, l))/2.0_fp
                         endif
                         r1avg = (r1(nm, k-1, ll) + r1(nmu, k-1, ll)) / 2.0_fp
                         if (ceavg>r1avg*1.1_fp .and. ceavg>0.05_fp) then
                            z      = (1.0_fp + sig(k-1)) * hu(nm)
                            apower = log(max(r1avg/ceavg,1.0e-5_fp)) / log(z/aksu)
                            z      = (1.0_fp + sig(k-1) - 0.5_fp*thick(k-1)) * hu(nm)
                            dz     = (thick(k)-htdif) * hu(nm)
                            if (apower>-1.05_fp .and. apower<=-1.0_fp) then
                               apower = -1.05_fp
                            elseif (apower>=-1.0_fp .and. apower<-0.95_fp) then
                               apower = -0.95_fp
                            else
                            endif
                            apower  = min(max(-10.0_fp , apower), 10.0_fp)
                            cavg1   = (ceavg/(apower+1.0_fp)) * (1.0_fp/aksu)**apower
                            cavg2   = z**(apower+1.0_fp) - aksu**(apower+1.0_fp)
                            cavg    = cavg1 * cavg2 / dz
                            cumflux = cumflux - u1(nm,k)*(cavg-r1avg)*dz
                         endif
                      endif
                      sucor(nm,l) = -cumflux
                      !
                      ! bedload will be reduced in case of sediment transport
                      ! over a non-erodible layer (no sediment in bed) in such
                      ! a case, the suspended sediment transport vector must
                      ! also be reduced.
                      !
                      if ((sucor(nm,l)>0.0_fp .and. kcs(nm)==1) .or. kcs(nmu)/=1) then
                         sucor(nm,l) = sucor(nm,l) * fixfac(nm,l)
                      else
                         sucor(nm,l) = sucor(nm,l) * fixfac(nmu,l)
                      endif
                   endif
                   !
                   ! v direction
                   !
                   if ((kfv(nm)*kfsed(nm)*kfsed(num)) /= 0) then
                      cumflux = 0.0_fp
                      if (kcs(num) == 3) then
                         aksu = aks(nm, l)
                      elseif (kcs(nm) == 3) then
                         aksu = aks(num, l)
                      else
                         aksu = (aks(nm, l)+aks(num, l)) / 2.0_fp
                      endif
                      !
                      ! work up through layers integrating transport
                      ! below aksu
                      !
                      do k = kmax, 1, -1
                         kvalue = k
                         htdif  = aksu/hv(nm) - (1.0_fp + sig(k) - thick(k)/2.0_fp)
                         !
                         ! if layer containing aksu
                         !
                         if (htdif <= thick(k)) then
                            cumflux = cumflux + fluxv(nm,k,ll)*htdif/thick(k)
                            exit
                         else
                            cumflux = cumflux + fluxv(nm,k,ll)
                         endif
                      enddo
                      if (comparereal(agvv(nm),0._fp)/=0) then !to be removed when reconvof fixed (see below)
                         cumflux = cumflux / (gvv(nm)*agvv(nm))
                      else
                         cumflux = 0._fp
                      endif
                      !
                      ! integration finished
                      ! suspended transport correction = opposite of the
                      ! transport below aksu
                      !
                      ! Approximation of the additional transport between bottom of
                      ! kmaxsd layer and za has been included in the correction vector
                      !
                      k = kvalue
                      if (k > 1) then
                         if (kcs(num) == 3) then
                            !
                            ! correction for domain decomposition:
                            !
                            ceavg = rca(nm,l)
                         elseif (kcs(nm) == 3) then
                            ceavg = rca(num,l)
                         else
                            ceavg = (rca(nm,l)+rca(num,l)) / 2.0_fp
                         endif
                         r1avg = (r1(nm, k-1, ll) + r1(num, k-1, ll)) / 2.0_fp
                         if (ceavg>r1avg*1.1_fp .and. ceavg>0.05_fp) then
                            z      = (1.0_fp + sig(k-1)) * hv(nm)
                            apower = log(max(r1avg/ceavg,1.0e-5_fp)) / log(z/aksu)
                            z      = (1.0_fp + sig(k-1) - 0.5_fp*thick(k-1)) * hv(nm)
                            dz     = (thick(k)-htdif) * hv(nm)
                            if (apower>-1.05_fp .and. apower<=-1.0_fp) then
                               apower = -1.05_fp
                            elseif (apower>=-1.0_fp .and. apower<-0.95_fp) then
                               apower = -0.95_fp
                            else
                            endif
                            apower  = min(max(-10.0_fp , apower), 10.0_fp)
                            cavg1   = (ceavg/(apower+1.0_fp)) * (1.0_fp/aksu)**apower
                            cavg2   = z**(apower+1.0_fp) - aksu**(apower+1.0_fp)
                            cavg    = cavg1 * cavg2 / dz
                            cumflux = cumflux - v1(nm,k)*(cavg-r1avg)*dz
                         endif
                      endif
                      svcor(nm, l) = -cumflux
                      !
                      ! bedload will be reduced in case of sediment transport
                      ! over a non-erodible layer (no sediment in bed) in such
                      ! a case, the suspended sediment transport vector must
                      ! also be reduced.
                      !
                      if ((svcor(nm,l) > 0.0_fp .and. abs(kcs(nm))==1) .or. abs(kcs(num))/=1) then
                         svcor(nm, l) = svcor(nm, l)*fixfac(nm, l)
                      else
                         svcor(nm, l) = svcor(nm, l)*fixfac(num, l)
                      endif
                   endif
                enddo ! nm
             endif    ! sedtyp = SEDTYP_NONCOHESIVE_SUSPENDED
          enddo       ! l
       endif          ! kmax>1
       !
       ! Calculate suspended sediment transport vector components for
       ! output
       ! Note: uses DIFU fluxes
       ! if suspended sediment vector is required this half timestep
       ! note, will be required if nst.ge.itmor for cumulative
       ! transports
       !
       if (sscomp .or. nst>=itmor) then
          do l = 1, lsed
             ll = lstart + l
             do nm = 1, nmmax
                nmu = nm + icx
                num = nm + icy
                !
                ! u component
                !
                if (kfu(nm) == 1 .and. kcu(nm)/=-1) then
                   cumflux = 0.0_fp
                   do k = 1, kmax
                      cumflux = cumflux + fluxu(nm, k, ll)
                   enddo
                   !
                   ! total suspended transport
                   !
                   if (comparereal(aguu(nm),0._fp)/=0) then !to be removed when reconvof fixed (see below)
                      ssuu(nm, l) = cumflux/(guu(nm)*aguu(nm)) + sucor(nm, l) !Here aguu has to be the % of active wet edge that is never zero when kfu=1. Otherwise I have div by zero 
                   else
                      ssuu(nm, l) = 0._fp
                   endif
                endif
                !
                ! v component
                !
                if (kfv(nm) == 1 .and. kcv(nm)/=-1) then
                   cumflux = 0.0_fp
                   do k = 1, kmax
                      cumflux = cumflux + fluxv(nm, k, ll)
                   enddo
                   !
                   ! total suspended transport
                   !
                   if (comparereal(agvv(nm),0._fp)/=0) then !to be removed when reconvof fixed (see below)
                      ssvv(nm, l) = cumflux/(gvv(nm)*agvv(nm)) + svcor(nm, l)
                   else
                      ssvv(nm, l) = 0._fp
                   endif
                endif
             enddo  ! nm
          enddo     ! l
       endif        ! sscomp .or. nst>=itmor
    endif           ! sus /= 0.0
    !
    if (USEfixedBEDequilQb.and.itmor-itstrt==0.and.DELAYfixedBEDequilQS>0._fp) then !move it at the beginning of the code
       write(*,*) 'If USEfixedBEDequilQb=Y then itmor must be >0'
       !pause
       stop
    endif
    !
    ! if morphological computations have started
    !
    if (1==1) then !(nst >= itmor) then !removed for output purposes and for computing Qb_EQ if USEfixedBEDequilQb=Y
       !
       ! Increment morphological time
       ! Note: dtmor in seconds, hydrt and morft in days!
       !
       morft = morft + real(dtmor,hp)/86400.0_hp
       !
       ! Increment hydraulic time if morfac>0; don't include morfac=0 periods while computing average morfac.
       !
       if (morfac > 0.0_fp) hydrt = hydrt + real(dt,hp)/86400.0_hp
       !
       ! Bed boundary conditions: transport condition
       !
       do jb = 1, nto
          icond = morbnd(jb)%icond
          if (icond == 4 .or. icond == 5) then
             !
             ! Open boundary with transport boundary condition:
             ! Get data from table file
             !
             call flw_gettabledata(bcmfile  , morbnd(jb)%ibcmt(1) , &
                      & morbnd(jb)%ibcmt(2) , morbnd(jb)%ibcmt(3) , &
                      & morbnd(jb)%ibcmt(4) , bc_mor_array        , &
                      & timhr      ,julday  , gdp        )
             !
             ! Prepare loop over boundary points
             !
             do ib = 1, morbnd(jb)%npnt
                alfa_dist   = morbnd(jb)%alfa_dist(ib)
                alfa_mag    = morbnd(jb)%alfa_mag(ib)
                idir_scalar = morbnd(jb)%idir(ib)
                nm          = morbnd(jb)%nm(ib)
                nxmx        = morbnd(jb)%nxmx(ib)
                !
                nmu = nm + icx
                num = nm + icy
                nmd = nm - icx
                ndm = nm - icy
                !
                ! If the computed transport is directed outward, do not
                ! impose the transport rate (at outflow boundaries the
                ! "free bed level boundary" condition is imposed. This
                ! check is carried out for each individual boundary point.
                !
                ! Detect the case based on the value of nxmx.
                !
                if (nxmx == nmu) then
                   if (umean(nm)<0.0_fp) cycle
                elseif (nxmx == nmd) then
                   if (umean(nmd)>0.0_fp) cycle
                elseif (nxmx == num) then
                   if (vmean(nm)<0.0_fp) cycle
                elseif (nxmx == ndm) then
                   if (vmean(ndm)>0.0_fp) cycle
                endif
                !
                ! The velocity/transport points to the left and top are part
                ! of this cell. nxmx contains by default the index of the
                ! neighbouring grid cell, so that has to be corrected. This
                ! correction is only carried out locally since we need the
                ! unchanged nxmx value further down for the bed level updating
                !
                if (nxmx == nmu .or. nxmx == num) nxmx = nm
                !
                li      = 0
                lsedbed = lsedtot - nmudfrac
                do l = 1, lsedtot
                   !
                   ! bed load transport always zero for mud fractions
                   !
                   if (sedtyp(l) == SEDTYP_COHESIVE) cycle
                   li = li + 1
                   !
                   if (morbnd(jb)%ibcmt(3) == lsedbed) then
                      rate = bc_mor_array(li)
                   elseif (morbnd(jb)%ibcmt(3) == 2*lsedbed) then
                      rate = bc_mor_array(li) + &
                           & alfa_dist * (bc_mor_array(li+lsedbed)-bc_mor_array(li))
                   endif
                   rate = alfa_mag * rate
                   !
                   if (icond == 4) then
                      !
                      ! transport including pores
                      !
                      rate = rate*cdryb(l)
                   else
                      !
                      ! transport excluding pores
                      !
                      rate = rate*rhosol(l)
                   endif
                   !
                   ! impose boundary condition
                   !
                   if (idir_scalar == 1) then
                      sbuu(nxmx, l) = rate
                   else
                      sbvv(nxmx, l) = rate
                   endif
                enddo ! l (sediment fraction)
             enddo    ! ib (boundary point)
          elseif (icond == 6 .or. icond == 7) then
             !
             ! Open boundary with transport boundary condition:
             ! Get data from table file
             !
             call flw_gettabledata(bcmfile  , morbnd(jb)%ibcmt(1) , &
                      & morbnd(jb)%ibcmt(2) , morbnd(jb)%ibcmt(3) , &
                      & morbnd(jb)%ibcmt(4) , bc_mor_array        , &
                      & timhr      ,julday  , gdp        )
             !
             ! loop over sediment fractions
             !
             li = 0
             lsedbed = lsedtot - nmudfrac
             do l = 1, lsedtot
                !
                ! bed load transport always zero for mud fractions
                !
                if (sedtyp(l) == SEDTYP_COHESIVE) cycle
                li = li + 1
                !
                rate = bc_mor_array(li)
                !
                if (icond == 6) then
                   !
                   ! transport including pores
                   !
                   rate = rate*cdryb(l)
                else
                   !
                   ! transport excluding pores
                   !
                   rate = rate*rhosol(l)
                endif
                !
                ! Loop over boundary points and compute total bed transport
                !
                rateAD    = 0.0_fp
                widthtot = 0.0_fp
                do ib = 1, morbnd(jb)%npnt
                   idir_scalar = morbnd(jb)%idir(ib)
                   nxmx        = morbnd(jb)%nxmx(ib)
                   !width       = morbnd(jb)%width(ib)
                   nm          = morbnd(jb)%nm(ib)
                   nmu = nm + icx
                   num = nm + icy
                   nmd = nm - icx
                   ndm = nm - icy
                   if (distr_bdl_per) then
                      shiftPERnm = icy*distQHn+icx*distQHm
                   else
                      shiftPERnm = 0
                   endif
                   if (nxmx == nmu .or. nxmx == num) then !beginning of boundary (small n or m)
                      nxmxAD(ib) = nxmx-shiftPERnm ! it is the first velocity point inside the domain next to the boundary
                      nxmxB(ib)  = nm     ! it is the velocity point at the boundary
                   else  !end of boundary (high n or m)
                      if (nxmx == nmd) then !along m
                        nxmxAD(ib) = nxmx-icx-shiftPERnm ! it is the first velocity point inside the domain next to the boundary (if shiftPERnm=0, otherwise it is the velocity point in the corresponding periodic H boundary)
                      else !if (nxmx == ndm) then !along n
                        nxmxAD(ib) = nxmx-icy-shiftPERnm ! it is the first velocity point inside the domain next to the boundary (if shiftPERnm=0, otherwise it is the velocity point in the corresponding periodic H boundary)
                      endif
                      nxmxB(ib) = nxmx       ! it is the velocity point at the boundary
                   endif
                   if (idir_scalar == 1) then
                      widthAD(ib) = aguu(nxmxAD(ib))*guu(nxmxAD(ib))
                      width  (ib) = aguu(nxmxB (ib))*guu(nxmxB (ib))
                   else
                      widthAD(ib) = agvv(nxmxAD(ib))*gvv(nxmxAD(ib))
                      width  (ib) = agvv(nxmxB (ib))*gvv(nxmxB (ib))
                   endif
                   !
                   widthtot = widthtot + widthAD(ib)
                   if (idir_scalar == 1) then
                      rateADi(ib) = sbuu(nxmxAD(ib), l)*widthAD(ib)   !rate for 1 single boundary cell
                      if (distr_qtq_bdl_NNprism) then 
                         if (nxmx == nmu) then ! beginning of boundary
                            nmORT   = nmu
                            nmORTm1 = ndm + icx !it is basically ndmu
                         else !end of boundary
                            nmORT   = nm
                            nmORTm1 = ndm  
                         endif
                         if (comparereal(aguu(nxmxB(ib)),0._fp).gt.0) then !if its width at the boundary is zero the rate has to be zero even if adjacent have flux
                            rateADi(ib) = rateADi(ib) + sbvv(nmORT,l)*agvv(nmORT)*gvv(nmORT) - sbvv(nmORTm1,l)*agvv(nmORTm1)*gvv(nmORTm1)  
                         else
                            rateADi(ib) = 0._fp
                         endif
                      endif
                      rateAD = rateAD + rateADi(ib)
                   else
                      rateADi(ib) = sbvv(nxmxAD(ib), l)*widthAD(ib)  
                      if (distr_qtq_bdl_NNprism) then 
                         if (nxmx == num) then ! beginning of boundary
                            nmORT   = num
                            nmORTm1 = num - icx !it is basically numd
                         else !end of boundary
                            nmORT   = nm
                            nmORTm1 = nmd  
                         endif
                         if (comparereal(agvv(nxmxB(ib)),0._fp).gt.0) then !if its width at the boundary is zero the rate has to be zero even if adjacent have flux
                            rateADi(ib) = rateADi(ib) + sbuu(nmORT,l)*aguu(nmORT)*guu(nmORT) - sbuu(nmORTm1,l)*aguu(nmORTm1)*guu(nmORTm1)  
                         else
                            rateADi(ib) = 0._fp
                         endif
                      endif
                      rateAD = rateAD + rateADi(ib) 
                   endif

                enddo ! ib (boundary point)
                if (USEfixedBEDequilQb.and.((nst < itmor .and. DELAYfixedBEDequilQS>0._fp)   .or. nst == itstrt )) then !DELAYfixedBEDequilQS is one for susp and bedload, since for bedload it counts only if DELAYfixedBEDequilQS<0, otherwise boundary condition does not affect bedload transport inside the domain
                   if (.not. allocated(Qb_EQ)) allocate(Qb_EQ(lsedtot,nto))
                 !  if (nst < itmor) then 
                      Qb_EQ(l,jb) =  rateAD                     
                 !  endif                   
                endif
                if (USEfixedBEDequilQb) rate = Qb_EQ(l,jb) ! I overwrite boundary condition from the internal or periodic discharge, and it will be fixed in time once the morphodynamic will start (nst>itmor)
                !
                ! smoothing/  I presecribe rate as BC. While the first cell inside (or the periodic downstream boundary) has rateAD. I go smoothly from rateAD to rate.
                !
                itlfsm_per_Qs = itlfsm*perSMOfac_Qb 
                tdif = timnow*dt*2._fp/60._fp - tstart !dt IS IN SECONDS (if ADI then dt="hdt")
                if (itlfsm_per_Qs>0 .and. tdif<=itlfsm_per_Qs*dt*2._fp/60._fp) then
                   Qini = rateAD 
                   tfrac = tdif/(itlfsm_per_Qs*dt*2._fp/60._fp)
                   diff  = rate - Qini
                   rate = Qini + tfrac*diff             
                endif
                !
                ! convert absolute flux into a relative ratio of the computed flux (rateAD)
                ! if the computed flux is zero, then use a constant rate per unit width (rateAD)
                !
                if (abs(rateAD).gt.0._fp) then
                   do ib = 1, morbnd(jb)%npnt
                      rateADi(ib) = rateADi(ib)/rateAD 
                      !
                      !if periodical BC use filter to remove oscillation (probably filter on periodic water discharge is enough)
                      if (distr_bdl_per) then 
                         !this is valid only for 1 boundary with prescribed totalload. If periodic BC, only one boundary should be present. However, a check should be done at the beginning to make sure this is the case
                         rateNEW = rateADi(ib)  
                         if (reltim_qtq_bdl>0) then
                            if (morbnd(jb)%npnt.ne.nrPER) then ! to be moved to the beginning
                               write(*,*) 'Error in bott3D' 
                               !pause
                               stop
                            endif
                            if (.NOT.firstCALL) then
                               a = exp( - dt/reltim_qtq_bdl/60.0_fp) !dt (= "hdt" for ADI) in sec, reltim in minutes
                               qfilt_bdl(ib,l) = a*qfilt_bdl(ib,l) + (1._fp - a)*rateNEW
                            else
                               qfilt_bdl(ib,l) = rateNEW
                            endif
                         else
                            qfilt_bdl(ib,l) = rateNEW 
                         endif     
                         rateADi(ib) = qfilt_bdl(ib,l)   
                      endif
                   enddo
                   firstCALL = .FALSE.
                   rateAD  = 0.0_fp
                else
                   rateADi(1:morbnd(jb)%npnt) = 0._fp ! they could be alternatively positive and negative and sum up to zero
                   rateAD = rate/widthtot
                endif
                !
                ! Loop over boundary points and impose bed transport
                !
                do ib = 1, morbnd(jb)%npnt
                   idir_scalar = morbnd(jb)%idir(ib)
                   !nxmx        = morbnd(jb)%nxmx(ib)
                   !
                   if (idir_scalar == 1) then
                      if (comparereal(width(ib),0._fp).gt.0) then
                         sbuu(nxmxB(ib), l) = rate*rateADi(ib)/width(ib) + rateAD !divided by width(ib) since rate*rateADi(ib) it is in kg/s, but I want it for unit width.
                      else
                         sbuu(nxmxB(ib), l) = 0._fp
                      endif
                   else
                      if (comparereal(width(ib),0._fp).gt.0) then
                         sbvv(nxmxB(ib), l) = rate*rateADi(ib)/width(ib) + rateAD !divided by width(ib) since rate*rateADi(ib) it is in kg/s, but I want it for unit width.
                      else
                         sbvv(nxmxB(ib), l) = 0._fp
                      endif
                   endif
                enddo ! ib (boundary point)
             enddo    ! l (sediment fraction)
          endif       ! icond = 4, 5, 6 or 7 (boundary with transport condition)
       enddo          ! jb (open boundary) 
       !
    endif
    if (nst >= itmor) then
       !
       ! Update quantity of bottom sediment
       !
       dbodsd = 0.0_fp
       !
       ! compute change in bodsed (dbodsd)
       !
       bedchangemesscount = 0
       do l = 1, lsedtot
          bedload = sedtyp(l)==SEDTYP_NONCOHESIVE_TOTALLOAD
          ll = lstart + l
          do nm = 1, nmmax
             !
             ! note: do not update bottom sediment at open boundary pnts
             !
             if (abs(kcs(nm))*kfs(nm) /= 1) cycle
             !
             nmu     = nm + icx
             num     = nm + icy
             nmd     = nm - icx
             ndm     = nm - icy
             trndiv  = 0.0_fp
             sedflx  = 0.0_fp
             eroflx  = 0.0_fp
             gsqsinv = 1.0_fp/gsqs(nm)
             if (sus/=0.0_fp .and. .not. bedload) then
                if (neglectentrainment) then
                   !
                   ! mass balance based on fluxes: entrainment and deposition
                   ! does not lead to erosion/sedimentation.
                   !
                   if (snelli) then
                     !
                     ! Only cross-shore component
                     !
                     trndiv = trndiv + gsqsinv                                   &
                            & *(  ssuu(nmd, l)*guu(nmd) - ssuu(nm, l)*guu(nm))
                   else
                     trndiv = trndiv + gsqsinv                                   &
                            & *(  ssuu(nmd, l)*guu(nmd)*aguu(nmd) - ssuu(nm, l)*guu(nm)*aguu(nm)    &
                            &   + ssvv(ndm, l)*gvv(ndm)*agvv(ndm) - ssvv(nm, l)*gvv(nm)*agvv(nm))
                   endif
                else
                   !
                   ! mass balance includes entrainment and deposition
                   !
                   if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
                      !
                      ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
                      ! The first lsed fractions are the suspended fractions,
                      ! so this goes right
                      !
                      k = kmxsed(nm, l)
                   else
                      k = kmax
                   endif
                   thick0 = volum0(nm,k) * gsqsinv
                   thick1 = volum1(nm,k) * gsqsinv
                   sedflx = sinkse(nm,l) * r1(nm,k,ll) * thick1
                   eroflx = sourse(nm,l)               * thick0
                   !
                   ! Update fluff layer
                   !
                   if (iflufflyr>0) then
                      mfluff(l, nm) = mfluff(l, nm) + &
                                    & dt*(  sinkf(l, nm)*r1(nm, k, ll)*thick1   &
                                    &     - sourf(l, nm)              *thick0  )
                   endif
                   !
                   ! add suspended transport correction vector
                   !
                   if (snelli) then
                      !
                      ! Only cross-shore component
                      !
                      trndiv = trndiv + gsqsinv                                     &
                             &         *( sucor(nmd,l)*guu(nmd)-sucor(nm,l)*guu(nm))
                   else
                      trndiv = trndiv + gsqsinv                                     &
                             &         *( sucor(nmd,l)*guu(nmd)*aguu(nmd)-sucor(nm,l)*guu(nm)*aguu(nm) &
                             &           +svcor(ndm,l)*gvv(ndm)*agvv(ndm)-svcor(nm,l)*gvv(nm)*agvv(nm))
                   endif
                endif
             endif
             if (bed /= 0.0_fp) then
                if (snelli) then
                  !
                  ! Only cross-shore component
                  !
                  trndiv = trndiv + gsqsinv                                     &
                         &         *( sbuu(nmd,l)*guu(nmd)-sbuu(nm,l)*guu(nm))
                else
                  trndiv = trndiv + gsqsinv                                     &
                         &         *( sbuu(nmd,l)*guu(nmd)*aguu(nmd)-sbuu(nm,l)*guu(nm)*aguu(nm)   &
                         &           +sbvv(ndm,l)*gvv(ndm)*agvv(ndm)-sbvv(nm,l)*gvv(nm)*agvv(nm))
                endif
             endif
             !
             dsdnm = (trndiv+sedflx-eroflx) * dtmor
             !
             ! Warn if bottom changes are very large,
             ! depth change NOT LIMITED
             !
             dhmax = 0.05_fp
             h1 = max(0.01_fp, s1(nm) + real(dps(nm),fp))
             if (abs(dsdnm) > dhmax*h1*cdryb(l) .and. bedupd.and.(agsqs(nm).gt.thresMERGE_zb.or..not.virtualMERGEupdBED)) then 
                !
                ! Only write bed change warning when bed updating is true
                ! (otherwise no problem)
                ! Limit the number of messages with bedchangemessmax
                ! (otherwise tri-diag will grow very fast)
                !
                bedchangemesscount = bedchangemesscount + 1
                if (bedchangemesscount <= bedchangemessmax) then
                   call nm_to_n_and_m(nm, n, m, gdp)
                   write (lundia, '(a,f5.1,a,i0,a,i0,a,i0,a)') &
                       & '*** WARNING Bed change exceeds ' , dhmax*100.0_fp, ' % of waterdepth after ', ntstep,  &
                       & ' timesteps, location (m,n) = (', m,',',n,')'
                endif
             endif
             !
             ! Update dbodsd value at nm
             !
             dbodsd(l, nm) = dbodsd(l, nm) + dsdnm
             !
             call updwaqflxsed(nst, nm, l, trndiv, sedflx, eroflx, gdp)
          enddo    ! nm
       enddo       ! l
       if (bedchangemesscount > bedchangemessmax) then
          write (lundia,'(12x,a,i0,a)') 'Bed change messages skipped (more than ',bedchangemessmax,')'
          write (lundia,'(12x,2(a,i0))') 'Total number of Bed change messages for timestep ',ntstep,' : ',bedchangemesscount
       endif
       !
       call fluff_burial(gdp%gdmorpar%flufflyr, dbodsd, lsed, lsedtot, gdp%d%nmlb, gdp%d%nmub, dt, morfac)
       !
       ! Re-distribute erosion near dry and shallow points to allow erosion
       ! of dry banks
       !
       do nm = 1, nmmax
          !
          ! If this is a cell in which sediment processes are active then ...
          !
          if (abs(kcs(nm))*kfs(nm)*kfsed(nm) /= 1) cycle
          !
          nmu = nm + icx
          num = nm + icy
          nmd = nm - icx
          ndm = nm - icy
          totdbodsd = 0.0_fp
          do l = 1, lsedtot
             totdbodsd = totdbodsd + dbodsd(l, nm)
          enddo
          !
          ! If this is a cell in erosion is occuring (accretion is not
          ! distributed to dry points) then...
          !
          if (totdbodsd < 0.0_fp) then
             !
             ! Note: contrary to the previous implementation, this new
             ! implementation erodes the sediment from nm and
             ! re-distributes the eroded volume based on the composition
             ! of the neighbouring cells, replenishing the sediment volume
             ! at grid point nm with sediment of a different composition
             ! than that what was eroded. This new implementation is mass
             ! conserving per fraction. Furthermore, re-distribution takes
             ! place only in case of net TOTAL erosion, i.e. not of
             ! individual fractions.
             !
             gsqsmin    = gsqs(nm)
             totfixfrac = 0.0_fp
             !
             from_ndm = kfsed(ndm)==0 .and. kcs(ndm) /= 0 .and. kcs(ndm)<3 .and. kcv(ndm)==1 .and. dps(ndm)<dps(nm)
             if (from_ndm) then
                gsqsmin = min(gsqsmin,gsqs(ndm))
                do l = 1, lsedtot
                   totfixfrac = totfixfrac + fixfac(ndm, l)*frac(ndm, l)
                enddo
             endif
             !
             from_nmd = kfsed(nmd)==0 .and. kcs(nmd) /= 0 .and. kcs(nmd)<3 .and. kcu(nmd)==1 .and. dps(nmd)<dps(nm)
             if (from_nmd) then
                gsqsmin = min(gsqsmin,gsqs(nmd))
                do l = 1, lsedtot
                   totfixfrac = totfixfrac + fixfac(nmd, l)*frac(nmd, l)
                enddo
             endif
             !
             from_nmu = kfsed(nmu)==0 .and. kcs(nmu) /= 0 .and. kcs(nmu)<3 .and. kcu(nm)==1 .and. dps(nmu)<dps(nm)
             if (from_nmu) then
                gsqsmin = min(gsqsmin,gsqs(nmu))
                do l = 1, lsedtot
                   totfixfrac = totfixfrac + fixfac(nmu, l)*frac(nmu, l)
                enddo
             endif
             !
             from_num = kfsed(num)==0 .and. kcs(num) /= 0 .and. kcs(num)<3 .and. kcv(nm)==1 .and. dps(num)<dps(nm)
             if (from_num) then
                gsqsmin = min(gsqsmin,gsqs(num))
                do l = 1, lsedtot
                   totfixfrac = totfixfrac + fixfac(num, l)*frac(num, l)
                enddo
             endif
             !
             ! Re-distribute THET % of erosion in nm to surrounding cells
             ! THETSD is a user-specified maximum value, range 0-1
             !
             if (totfixfrac > 1.0e-7_fp) then
                !
                ! Compute local re-distribution factor THET
                !
                if (hmaxth > sedthr) then
                   h1   = real(dps(nm),fp) + s1(nm)
                   thet = (h1 - sedthr)/(hmaxth - sedthr)*thetsd
                   thet = min(thet, thetsd)
                else
                   thet = thetsd
                endif
                !
                ! Combine some constant factors in variable THET
                ! Note: TOTDBODSD<0.0 and thus THET>0.0 !
                !
                thet = -gsqsmin * totdbodsd * thet / totfixfrac
                !
                do l = 1, lsedtot
                   !
                   ! update dbodsd values in this cell and surrounding cells
                   ! adjust bedload transport rates to include this erosion
                   ! process.
                   !
                   if (from_ndm) then
                      dv             = thet*fixfac(ndm, l)*frac(ndm, l)
                      dbodsd(l, ndm) = dbodsd(l, ndm) - dv/gsqs(ndm)
                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
                      sbvv(ndm, l)   = sbvv(ndm, l) + dv/(dtmor*gvv(ndm)) !use agvv?
                   endif
                   !
                   if (from_nmd) then
                      dv             = thet*fixfac(nmd, l)*frac(nmd, l)
                      dbodsd(l, nmd) = dbodsd(l, nmd) - dv/gsqs(nmd)
                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
                      sbuu(nmd, l)   = sbuu(nmd, l) + dv/(dtmor*guu(nmd))  !use aguu?
                   endif
                   !
                   if (from_nmu) then
                      dv             = thet*fixfac(nmu, l)*frac(nmu, l)
                      dbodsd(l, nmu) = dbodsd(l, nmu) - dv/gsqs(nmu)
                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
                      sbuu(nm, l)    = sbuu(nm, l) - dv/(dtmor*guu(nm)) !use aguu?
                   endif
                   !
                   if (from_num) then
                      dv             = thet*fixfac(num, l)*frac(num, l)
                      dbodsd(l, num) = dbodsd(l, num) - dv/gsqs(num)
                      dbodsd(l, nm ) = dbodsd(l, nm ) + dv/gsqs(nm)
                      sbvv(nm, l)    = sbvv(nm, l) - dv/(dtmor*gvv(nm))  !use agvv?
                   endif
                enddo ! l
             endif    ! totfixfrac > 1.0e-7
          endif       ! totdbodsd < 0.0
       enddo          ! nm
       !
       nm_pos = 2
       call dfexchg(dbodsd, 1, lsedtot, dfloat, nm_pos, gdp)
       nm_pos = 1
       !
       ! Modifications for running parallel conditions
       !
       !
       if (multi) then
          i = 0
          do l = 1, lsedtot
             do nm = 1, nmmax
                i = i + 1
                mergebuf(i) = real(dbodsd(l, nm),hp)
             enddo
          enddo
          call putarray (mergehandle,mergebuf(1:nmmax*lsedtot),nmmax*lsedtot)
          call getarray (mergehandle,mergebuf(1:nmmax*lsedtot),nmmax*lsedtot)
          i = 0
          do l = 1, lsedtot
             do nm = 1, nmmax
                i = i + 1
                dbodsd(l, nm) = real(mergebuf(i),fp)
             enddo
          enddo
       endif
       !
       ! Add transports to cumulative transports
       !
       do l = 1, lsedtot
          do nm = 1, nmmax
             sbuuc(nm, l) = sbuuc(nm, l) + sbuu(nm, l) * dtmor
             sbvvc(nm, l) = sbvvc(nm, l) + sbvv(nm, l) * dtmor
          enddo
       enddo
       do l = 1, lsed
          do nm = 1, nmmax
             ssuuc(nm, l) = ssuuc(nm, l) + ssuu(nm, l) * dtmor
             ssvvc(nm, l) = ssvvc(nm, l) + ssvv(nm, l) * dtmor
          enddo
       enddo
       !
       ! for small cutcells (agsqs<thresMERGE_zb): they are virtually merged with large cells
       if (cutcell.gt.0.and.virtualMERGEupdBED) THEN
          nmaxddb = gdp%d%nmax + 2*gdp%d%ddbound
          CALL virtMERG(dbodsd,gsqs,s1,dps,cdryb,icx,icy,nmmax,gdp%d%nmlb,gdp%d%nmub,nst,1,lsedtot,1,lsedtot,lundia,bedupd,&   
                        bedchangemesscount,bedchangemessmax,ntstep,1,nmaxddb,gdp%d%ddbound,& !1 check large bed variations
                        NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
       endif
       !
       ! Apply erosion and sedimentation to bookkeeping system
       !
       if (cmpupd) then
          !
          ! Determine new thickness of transport layer
          !
          call compthick(dps       ,s1        , &
                       & nmmax     ,gdp       )
          !
          ! Update layers and obtain the depth change
          !
          if (updmorlyr(gdp%gdmorlyr, dbodsd, depchg, gdp%messages) /= 0) then
             call writemessages(gdp%messages, lundia)
             call d3stop(1, gdp)
          else
             call writemessages(gdp%messages, lundia)
          endif
          call lyrdiffusion(gdp%gdmorlyr, dtmor)
          !
          ! Apply composition boundary conditions
          !
          call bndmorlyr(lsedtot   ,timhr        , &
                       & nto       ,bc_mor_array , &
                       & gdp       )
       else
          !
          ! Compute bed level changes without actually updating the bed composition
          !
          depchg = 0.0_fp
          do nm = 1, nmmax
             if (kcs(nm)/=0 .and. kcs(nm)<=2) then
                do l = 1, lsedtot
                   depchg(nm) = depchg(nm) + dbodsd(l, nm)/cdryb(l)
                enddo
             endif
          enddo
       endif
       !
       ! Bed boundary conditions
       !
       do jb = 1, nto
          icond = morbnd(jb)%icond
          !
          ! In case of an open boundary with bed level condition
          ! described by time series: get data from table file
          !
          if (icond == 2 .or. icond == 3) then
             call flw_gettabledata(bcmfile  , morbnd(jb)%ibcmt(1)    , &
                      & morbnd(jb)%ibcmt(2) , morbnd(jb)%ibcmt(3)    , &
                      & morbnd(jb)%ibcmt(4) , bc_mor_array           , &
                      & timhr      ,julday  , gdp        )
          endif
          !
          ! Prepare loop over boundary points
          !
          do ib = 1, morbnd(jb)%npnt
             alfa_dist   = morbnd(jb)%alfa_dist(ib)
             alfa_mag    = morbnd(jb)%alfa_mag(ib)**2
             idir_scalar = morbnd(jb)%idir(ib)
             nm          = morbnd(jb)%nm(ib)
             nxmx        = morbnd(jb)%nxmx(ib)
             !
             nmu = nm + icx
             num = nm + icy
             nmd = nm - icx
             ndm = nm - icy
             !
             ! Bed change in open boundary point
             ! Any boundary condition is changed into a "free bed level
             ! boundary" if the computed transport is directed outward.
             !
             ! Detect the case based on the value of nxmx. In case of a
             ! diagonal water level boundary, there will be two separate
             ! entries in the morbnd structure. The sum of alfa_mag(ib)**2
             ! will be equal to 1.
             !
             if (nxmx == nmu) then
                if (umean(nm)<0.0) icond = 0
             elseif (nxmx == nmd) then
                if (umean(nmd)>0.0) icond = 0
             elseif (nxmx == num) then
                if (vmean(nm)<0.0) icond = 0
             elseif (nxmx == ndm) then
                if (vmean(ndm)>0.0) icond = 0
             endif
             !
             select case(icond)
             case (0,4,5,6,7)
                !
                ! outflow or free boundary (0)
                ! or prescribed transport with pores (4,6)
                ! or prescribed transport without pores (5,7)
                !
                depchg(nm) = depchg(nm) + depchg(nxmx) * alfa_mag
             case (1)
                !
                ! fixed bed level: no update
                !
                ! depchg(nm) = depchg(nm) + 0.0 * alfa_mag
             case (2)
                !
                ! prescribed depth
                ! temporarily store "bed levels" in variable "rate"
                !
                if (morbnd(jb)%ibcmt(3) == 1) then
                   rate = bc_mor_array(1)
                elseif (morbnd(jb)%ibcmt(3) == 2) then
                   rate = bc_mor_array(1) + &
                        & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
                endif
                !
                depchg(nm) = depchg(nm) + (real(dps(nm),fp)-rate) * alfa_mag
             case (3)
                !
                ! prescribed depth change rate
                !
                if (morbnd(jb)%ibcmt(3) == 1) then
                   rate = bc_mor_array(1)
                elseif (morbnd(jb)%ibcmt(3) == 2) then
                   rate = bc_mor_array(1) + &
                        & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
                endif
                !
                depchg(nm) = depchg(nm) - rate * alfa_mag * dtmor
             end select
          enddo ! ib (boundary point)
       enddo    ! jb (open boundary)
    else
       !
       ! if morphological computations haven't started yet
       !
       do nm = 1, nmmax
          depchg(nm) = 0.0_fp
       enddo
    endif ! nst >= itmor
    !
    ! store depth0 in case we want to do an update of the bottom and keep the depth constant instead of the water surface
    !
    if (bedUPDandFIXdepth) then
       do nm = 1, nmmax
          if (comparereal(depchg(nm),0._fp).ne.0) then
             depth0(nm) = s0(nm) + dps(nm)
          endif
       enddo
    endif
    !
    ! Update bottom elevations
    !
    if (bedupd) then
       !
       ! note: dps and dp are positive downwards.
       !
       do nm = 1, nmmax
          !
          ! note: if kcs(nm)=0 then depchg(nm)=0.0
          ! should change to following test because depchg may be small
          ! due to truncation errors
          !
          if (abs(depchg(nm)) > 0.0_fp) then
             dps(nm) = dps(nm) - real(depchg(nm),prec)
          endif
       enddo
       if (scour) then
          !
          ! -Check bottom slopes and apply an avalance effect if needed
          ! -Depths at waterlevel points (dps) will be updated,
          !  to be used for dpu and dpv
          ! -Depth changes will be added to depchg,to be used for dp
          !
          call avalan(dps       ,depchg    ,gvu       ,guv       , &
                    & icx       ,icy       ,gsqs      ,kcs       ,gdp       )
       endif
       do nm = 1, nmmax
          !
          ! note: if kcs(nm)=0 then depchg(nm)=0.0
          ! should change to following test because depchg may be small
          ! due to truncation errors
          !
          if (abs(depchg(nm)) >= 0.0) then
             s1(nm) = max(s1(nm), -real(dps(nm),fp))
             s0(nm) = max(s0(nm), -real(dps(nm),fp))
             !
             ! if dry cells are eroded then bring water level down to
             ! bed or maximum water level in surrounding wet cells
             ! (whichever is higher)
             !
             if (kfs(nm) == 0) then
                s1(nm) = s1(nm) + depchg(nm)
                s0(nm) = s0(nm) + depchg(nm)
             endif
          endif
          !
          ! set flag for updating dp points below (note does not = 2 at
          ! open boundaries)
          !
          if (kcs(nm) == 0) then
             kcsbot(nm) = 0
          else
             kcsbot(nm) = 1
          endif
       enddo
       !
       ! Dredging and Dumping
       !
       if (dredge) then
          call dredgedump(dbodsd    ,cdryb     ,nst       ,timhr     ,morft     , &
                        & gdp       )
       endif
    endif
    ! -----------------------------------------------------------
    ! DD_mapper: copy dps and depchg at zeta points
    ! -----------------------------------------------------------
    nhystp = nxtstp(d3dflow_bottom3d, gdp)
    if (bedupd) then
       !
       ! CALDPU is called after BOTT3D in TRISOL when BEDUPD = TRUE
       ! instead of updating dpu/dpv here
       !
       ! Update dp points
       !
       do nm = 1, nmmax
          nmu  = nm  + icx
          num  = nm  + icy
          numu = num + icx
          fact =   kcsbot(nm) *gsqs(nm)  + kcsbot(num) *gsqs(num)  &
               & + kcsbot(nmu)*gsqs(nmu) + kcsbot(numu)*gsqs(numu)
          if (fact > 0.0_fp) then
             dp(nm) = dp(nm) - (  depchg(nm) *gsqs(nm)  + depchg(num) *gsqs(num)     &
                    &           + depchg(nmu)*gsqs(nmu) + depchg(numu)*gsqs(numu))/fact
          endif
       enddo
    endif
!   for cutcell: compute dpL and dpH from dps and correct boundaries
    if (cutcell.gt.0) then 
       do nm = 1, nmmax
          if (kcs(nm)==2) then !only for boundary!!!this is needed because some cell just inside the boundary could be partially active but the adjacent halo cell is fully dry with dpH=dpL, therefore the veriation of dps does not have to be copied.
             if (comparereal(abs(depchgH(nm)),0._fp).gt.0) then
                if (comparereal(poros(nm),0._fp)==0) then
                   dpL(nm) = dpH(nm)  !they concide there is no lower elevation!
                   dps(nm) = dpH(nm)  ! it was maybe changed but it should not
                else if (comparereal(poros(nm),0._fp)==0) then
                   dpL(nm) = dps(nm) ! dps might have changed
                   dpH(nm) = dpL(nm) ! if I change dpH above I have to rechange it here
                endif
             else
                if (comparereal(poros(nm),0._fp)==0) then
                   dps(nm) = dpL(nm) !dps was changed but thats not possible!
                   s1(nm)  = - dpL(nm) !s1 was changed but thats not possible since depchgH=0 and dpH didnt change
                   s0(nm)  = - dpL(nm) !s0 was changed but thats not possible since depchgH=0 and dpH didnt change
                endif
             endif
          else ! if kcs(n,m).ne.2
             if (kfs_cc(nm)==0) then !partially dry cut cell (kcs(n,m)==2 already treated above
                dpL(nm) = dps(nm)
                !dpH does not change since its dry
             elseif (kfs_cc(nm)==1) then !flooded cut cell
                !here dps was the average of dpL and dpH. I go back to dpL and dpH and it is like if I deposited the same abount of dz in both dpL and dpH
                dpL(nm) = dpL(nm) - depchg(nm) !poros(nm)*dps(nm) 
                dpH(nm) = dpH(nm) - depchg(nm) !(1._fp-poros(nm))*dps(nm)
             elseif (kfs_cc(nm)==2.or.kfs_cc(nm)==3) then ! 2,-2, 3, -3
                dpL(nm) = dps(nm)
                dpH(nm) = dps(nm)
             else !if (kfs_cc(nm)==-1,-2,-3   !dry cell nothing happens
   !            nothing happens
             endif
          endif
       enddo
    endif
    if (bedUPDandFIXdepth) then
       do nm = 1, nmmax
          if (comparereal(depchg(nm),0._fp).ne.0) then
             s1(nm) = depth0(nm) - dps(nm)
          endif
       enddo
    endif
    if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) then
       do k = 1,nmmax
          write(98919901,'(2i6,15f21.15)') nst,k,dps(k),dpL(k),dpH(k)
       enddo
    endif
end subroutine bott3d
