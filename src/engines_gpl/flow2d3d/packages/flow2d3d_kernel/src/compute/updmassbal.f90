subroutine updmassbal(newvol    ,qxk       ,qyk       ,kcs       ,r1        , &
                    & volum1    ,sbuu      ,sbvv      , &
                    & gsqs      ,guu       ,gvv       ,dps       ,nst       , &
                    & lsal      ,ltem      ,s0        ,s1        ,agsqs     , &
                    & aguu      ,agvv      ,disch     ,mnksrc    ,nsrc      , &
                    & r0        ,dps0      ,kfsed     ,kfs       ,lsecfl    , &
                    & volum0    ,icx       ,icy       ,gdp       )
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
!  $Id: updmassbal.f90 7020 2017-02-16 21:50:45Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/updmassbal.f90 $
!!--description-----------------------------------------------------------------
!
!

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
    integer              :: nst         !!  Current time step counter
    integer              :: lsal         !!   
    integer              :: ltem         !!  
    integer              :: nsrc    !  Description and declaration in esm_alloc_int.f90
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: itmor
    integer                        , pointer :: nmax
    integer                        , pointer :: kmax
    integer                        , pointer :: nmmax
    integer                        , pointer :: lsed
    integer                        , pointer :: lsedtot
    integer                        , pointer :: lstsci
    integer                        , pointer :: lstsc
    real(fp)                       , pointer :: hdt
    logical                        , pointer :: massbal
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
    logical                        , pointer :: resetfluxes
    integer      , dimension(:)    , pointer :: volnr
    integer      , dimension(:,:)  , pointer :: exchnr
    integer      , dimension(:,:)  , pointer :: neighb
    real(fp)     , dimension(:)    , pointer :: accdps
    real(fp)     , dimension(:)    , pointer :: volumes
    real(fp)     , dimension(:,:)  , pointer :: mass_r1
    real(fp)     , dimension(:,:)  , pointer :: fluxes
    real(fp)     , dimension(:,:,:), pointer :: fluxes_r1
    real(fp)     , dimension(:,:,:), pointer :: fluxes_sd
    real(fp)     , dimension(:)    , pointer :: rhofrac
    real(fp)     , dimension(:)    , pointer :: cdryb
    !
    real(fp)                       , pointer :: morfac
    real(fp)     , dimension(:,:,:), pointer :: fluxu
    real(fp)     , dimension(:,:,:), pointer :: fluxv
    real(fp)     , dimension(:,:)  , pointer :: ssuu
    real(fp)     , dimension(:,:)  , pointer :: ssvv
    real(fp)     , ALLOCATABLE, SAVE         :: fluxesLOC(:)
    character(1)                        , pointer :: forfuv
    logical                , pointer :: massbalLOC
    real(fp)               , pointer :: THRlocalMASSbal
    real(fp)               , pointer :: thresMERGE_d
    integer, dimension(:)  , pointer :: Nmerged_d
    integer, dimension(:,:), pointer :: NMLISTMERGED_D
    integer                , pointer :: cutcell
    logical                , pointer :: virtualMERGEupdDEPTH
    integer, dimension(:)  , pointer :: Nmerged_bed
    integer, dimension(:,:), pointer :: NMLISTMERGED_bed
    real(fp)               , pointer :: thresMERGE_zb
    logical                , pointer :: virtualMERGEupdCONC
    integer, dimension(:)  , pointer :: neuMERG
    logical                , pointer :: virtualMERGEupdBED
    logical                , pointer :: virtualLINK
!
! Global variables
!
    logical                                                     :: newvol
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                :: kcs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)    , intent(in)  :: kfsed  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)    , intent(in)  :: kfs  !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax,1:gdp%d%lstsci) :: r1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax,1:gdp%d%lstsci) :: r0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)   :: volum1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)   :: volum0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)   :: qxk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)   :: qyk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsedtot)  :: sbuu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsedtot)  :: sbvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: aguu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: agvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: gsqs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: agsqs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: s1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: s0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: guu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: gvv
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                :: dps
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                :: dps0
    real(fp)  , dimension(nsrc)                                 :: disch   !  Description and declaration in esm_alloc_real.f90
    integer   , dimension(7, nsrc)                              :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer                                                     :: lsecfl
    integer                                                     :: icx
    integer                                                     :: icy
!
! Local variables
!
    integer                                 :: nmR
    integer                                 :: lstart
    integer                                 :: ex
    integer                                 :: ibnd
    integer                                 :: j
    integer                                 :: nmS
    integer                                 :: i
    integer                                 :: ivol
    integer                                 :: jvol
    integer                                 :: k
    integer                                 :: l
    integer                                 :: nm
    integer                                 :: nm2
    integer                                 :: nmaxddb
    integer                                 :: n,m,nmd,ndm,nmi
    real(fp)                                :: varBOTTOMmerged
    real(fp)                                :: balanceMERGED 
    real(fp)                                :: EXITINGvolumeMERGED
    real(fp)                                :: STOREDvolumeMERGED
    real(fp)                                :: hdtmor
    real(fp)                                :: q,Qloc
    real(fp)                                :: vol
    real(fp)                                :: QxUP,QxDOWN,QyUP,QyDOWN
    real(fp)                                :: QxUP_R,QxDOWN_R,QyUP_R,QyDOWN_R
    real(fp), allocatable,dimension(:)      :: balance,EXITINGvolume,STOREDvolume,varBOTTOM
!
!! executable statements -------------------------------------------------------
!
    massbalLOC           => gdp%gdimbound%massbalLOC
    THRlocalMASSbal      => gdp%gdimbound%THRlocalMASSbal
    thresMERGE_d         => gdp%gdimbound%thresMERGE_d
    Nmerged_d            => gdp%gdimbound%Nmerged_d
    NMLISTMERGED_D       => gdp%gdimbound%NMLISTMERGED_D
    cutcell              => gdp%gdimbound%cutcell
    virtualMERGEupdDEPTH => gdp%gdimbound%virtualMERGEupdDEPTH
    Nmerged_bed          => gdp%gdimbound%Nmerged_bed
    NMLISTMERGED_bed     => gdp%gdimbound%NMLISTMERGED_bed
    thresMERGE_zb        => gdp%gdimbound%thresMERGE_zb
    virtualMERGEupdCONC  => gdp%gdimbound%virtualMERGEupdCONC
    neuMERG              => gdp%gdimbound%neuMERG
    virtualMERGEupdBED   => gdp%gdimbound%virtualMERGEupdBED
    virtualLINK          => gdp%gdimbound%virtualLINK
    massbal     => gdp%gdmassbal%massbal
    if (.not. massbal) return
    !
    forfuv         => gdp%gdtricom%forfuv
    kmax           => gdp%d%kmax
    nmmax          => gdp%d%nmmax
    lsed           => gdp%d%lsed
    lsedtot        => gdp%d%lsedtot
    lstsci         => gdp%d%lstsci
    lstsc          => gdp%d%lstsc
    hdt            => gdp%gdnumeco%hdt
    !
    nbalpol        => gdp%gdmassbal%nbalpol
    nneighb        => gdp%gdmassbal%nneighb
    resetfluxes    => gdp%gdmassbal%resetfluxes
    volnr          => gdp%gdmassbal%volnr
    exchnr         => gdp%gdmassbal%exchnr
    neighb         => gdp%gdmassbal%neighb
    accdps         => gdp%gdmassbal%accdps
    volumes        => gdp%gdmassbal%volumes
    rhofrac        => gdp%gdmorlyr%settings%rhofrac
    mass_r1        => gdp%gdmassbal%mass_r1
    fluxes         => gdp%gdmassbal%fluxes
    fluxes_r1      => gdp%gdmassbal%fluxes_r1
    fluxes_sd      => gdp%gdmassbal%fluxes_sd
    cdryb          => gdp%gdsedpar%cdryb
    nmax           => gdp%d%nmax
    !
    itmor          => gdp%gdmorpar%itmor
    morfac         => gdp%gdmorpar%morfac
    fluxu          => gdp%gdflwpar%fluxu
    fluxv          => gdp%gdflwpar%fluxv
    ssuu           => gdp%gderosed%e_ssn
    ssvv           => gdp%gderosed%e_sst
    !
    if (.not. allocated(fluxesLOC)) allocate(fluxesLOC(gdp%gdmassbal%nneighb))
    lstart  = max(lsal, ltem)
    ibnd = nbalpol+1
    nmaxddb = gdp%d%nmax + 2*gdp%d%ddbound
    !
    ! If volumes were determined during previous call, then the accumulation of
    ! the fluxes should start over again.
    !
    if (resetfluxes) then
       fluxes = 0.0_fp
       fluxes_r1 = 0.0_fp
       fluxes_sd = 0.0_fp
       resetfluxes = .false.
    endif
    
    if (massbalLOC.and.nst.ge.0) then
      allocate(balance(nmmax),EXITINGvolume(nmmax),STOREDvolume(nmmax),varBOTTOM(nmmax))
      do nm = 1, nmmax
         if (kcs(nm)==1) then
            Qloc = 0._fp
            if (nsrc.gt.0) then  !add local immission of discharge to the balance
               do i = 1, nsrc !not optimized should be done outside
                 ! nm   = (mnksrc(5, i) + ddb) + ((mnksrc(4, i) - 1) + ddb)*icxy this is the one in sud
                   nmS = (mnksrc(4, i)-1)*gdp%d%nmax + mnksrc(5, i)
                   if (nm ==nmS) then
                      Qloc = disch(i)
                   endif
               enddo
            endif
            QxUP   = sum(qxk(nm        ,1:kmax)) 
            QxDOWN = sum(qxk(nm-nmaxddb,1:kmax))
            QyUP   = sum(qyk(nm        ,1:kmax)) 
            QyDOWN = sum(qyk(nm-1      ,1:kmax)) 
            EXITINGvolume(nm) = (QxUP-QxDOWN+QyUP-QyDOWN-Qloc)*hdt
            STOREDvolume(nm) = (s1(nm)-s0(nm))*agsqs(nm)*gsqs(nm)
            balance(nm) = EXITINGvolume(nm)+STOREDvolume(nm)
         !   write(89898989,'(i6,45f35.15)') nst,QxUP,QxDOWN,QyUP,QyDOWN,s1(nm),s0(nm),agsqs(nm),EXITINGvolume(nm),(s1(nm)-s0(nm))*agsqs(nm)*gsqs(nm),balance(nm)
         endif
      enddo

      do nm = 1, nmmax
         if (kcs(nm)==1) then
          !  if (neuMERG(nm+icx)==2.or.neuMERG(nm-icx)==1.or.neuMERG(nm+icy)==4.or.neuMERG(nm-icy)==3) cycle 
            if (cutcell>0.and.(virtualMERGEupdDEPTH.or.virtualLINK)) then

               if(kcs(nm)*kfs(nm) == 1 .and. comparereal(agsqs(nm),0._fp).gt.0  .and. agsqs(nm).lt.thresMERGE_d) cycle !cycle, since they are merged. note agsqs is poros in sudSURFslopeFAC         

               balanceMERGED  = 0._fp
               EXITINGvolumeMERGED = 0._fp
               STOREDvolumeMERGED = 0._fp
               do i=1,Nmerged_d(nm) 
                  nmi = NMlistMERGED_d(i,nm) 
                  balanceMERGED = balanceMERGED + balance(nmi)
                  EXITINGvolumeMERGED = EXITINGvolumeMERGED + EXITINGvolume(nmi)
                  STOREDvolumeMERGED = STOREDvolumeMERGED + (s1(nmi)-s0(nmi))*agsqs(nmi)*gsqs(nmi)
               enddo
           !elseif (virtualMERGEneu.and.neuMERG(nm)>0)then
           !   if (neuMERG(nm)==1) then      ! merging in the ADI direction
           !      nmR = nm + icx
           !   elseif (neuMERG(nm)==2) then  ! merging in the ADI direction
           !      nmR = nm - icx
           !   elseif (neuMERG(nm)==3) then  ! lateral merging
           !      nmR = nm + icy
           !   elseif (neuMERG(nm)==4) then  ! lateral merging
           !      nmR = nm - icy
           !   endif
           !   if(icy==1) then !I solved along x, lateral fluxes are along x
           !      QyUP     = sum(qyk(nm           ,1:kmax)) 
           !      QyDOWN   = sum(qyk(nm-icy       ,1:kmax)) 
           !      QxUP_R   = sum(qxk(nmR          ,1:kmax)) !here I dont care about icx, all 4 fluxes are needed
           !      QxDOWN_R = sum(qxk(nmR-icx      ,1:kmax))
           !      QyUP_R   = sum(qyk(nmR          ,1:kmax)) 
           !      QyDOWN_R = sum(qyk(nmR-icy      ,1:kmax)) 
           !   else
           !      QyUP     = sum(qxk(nm           ,1:kmax)) 
           !      QyDOWN   = sum(qxk(nm-icy       ,1:kmax)) 
           !      QxUP_R   = sum(qyk(nmR          ,1:kmax)) !here I dont care about icx, all 4 fluxes are needed
           !      QxDOWN_R = sum(qyk(nmR-icx      ,1:kmax))
           !      QyUP_R   = sum(qxk(nmR          ,1:kmax)) 
           !      QyDOWN_R = sum(qxk(nmR-icy      ,1:kmax)) 
           !   endif
           !   !fix terms for nm and nmR (nm of Receiving cell)
           !   EXITINGvolume(nmR)  = (QxUP_R-QxDOWN_R+QyUP_R-QyDOWN_R+QyUP-QyDOWN)*hdt !right balance for nmR
           !   EXITINGvolume(nm)   = EXITINGvolume(nmR)  !nm does not satisfy mass conservation, I give it the nmR values to pass the test
           !
           !   STOREDvolume(nmR)   = STOREDvolume(nm)  + STOREDvolume(nmR) 
           !   STOREDvolume(nm)    = STOREDvolume(nmR)
           !   balance(nmR)        = EXITINGvolume(nm)+STOREDvolume(nm)
           !   balance(nm)         = balance(nm)    
           !   EXITINGvolumeMERGED = EXITINGvolume(nmR)
           !   STOREDvolumeMERGED  = STOREDvolume(nmR)
           !   balanceMERGED       = balance(nmR)    
            else
              balanceMERGED = balance(nm)
              EXITINGvolumeMERGED = EXITINGvolume(nm)
              STOREDvolumeMERGED = STOREDvolume(nm)
            endif
            !write(79898989,'(2i6,3f25.15)') nst,nm,EXITINGvolumeMERGED,STOREDvolumeMERGED,balanceMERGED   
            if (abs(balanceMERGED).gt.max(THRlocalMASSbal,THRlocalMASSbal*abs(EXITINGvolumeMERGED))) then
               call nm_to_n_and_m(nm, n, m, gdp)
               write(*,*) 'local mass balance not satisfied at cell (n,m) = (',n,m,'). Residual =',balance(nm),'nst=',nst,'icx=',icx
               !call d3stop(1, gdp)
            endif
         endif
      enddo
!      !
!      ! local check for passive scalars and I
!      !
!      if (lstsci>0) then
!         if (forfuv=='Y') then
!            write(*,*) 'Forester filter has to be off when checking for local mass conservation of constituents' !otherwise mass is moved from one cell to the others with the filter
!            call d3stop(1, gdp)
!         endif
!      endif
!      do l = 1,lstsci
!         if (l==lsecfl) cycle !elical intensity is not conservative
!         do nm = 1, nmmax
!            if (kfs(nm)*kcs(nm)==1) then
!               Qloc = 0._fp
!               QxUP   = sum(fluxu(nm        ,1:kmax,l)) 
!               QxDOWN = sum(fluxu(nm-nmaxddb,1:kmax,l))
!               QyUP   = sum(fluxv(nm        ,1:kmax,l)) 
!               QyDOWN = sum(fluxv(nm-1      ,1:kmax,l)) 
!               EXITINGvolume(nm) = (QxUP-QxDOWN+QyUP-QyDOWN-Qloc)*hdt
!               STOREDvolume(nm) = 0._fp
!               do k=1,kmax
!                  STOREDvolume(nm) = STOREDvolume(nm) + (volum1(nm,k)*r1(nm,k,l)-volum0(nm,k)*r0(nm,k,l))  !(s1(nm)-s0(nm))*agsqs(nm)*gsqs(nm)
!               enddo
!               balance(nm) = EXITINGvolume(nm)+STOREDvolume(nm)
!               !write(89898991,'(i6,45f35.15)') nst,QxUP,QxDOWN,QyUP,QyDOWN,s1(nm),s0(nm),agsqs(nm),EXITINGvolume(nm),(s1(nm)-s0(nm))*agsqs(nm)*gsqs(nm),balance(nm)
!            endif
!         enddo
!         do nm = 1, nmmax
!            if (kfs(nm)*kcs(nm)==1) then
!             !  if (neuMERG(nm+icx)==2.or.neuMERG(nm-icx)==1.or.neuMERG(nm+icy)==4.or.neuMERG(nm-icy)==3) cycle 
!               if (cutcell>0.and.virtualMERGEupdCONC.and..not.virtualMERGEneu) then
!
!                  if(kcs(nm)*kfs(nm) == 1 .and. comparereal(agsqs(nm),0._fp).gt.0  .and. agsqs(nm).lt.thresMERGE_zb) cycle !cycle, since they are merged. note agsqs is poros in sudSURFslopeFAC         
!
!                  balanceMERGED  = 0._fp
!                  EXITINGvolumeMERGED = 0._fp
!                  STOREDvolumeMERGED  = 0._fp
!                  do i=1,Nmerged_bed(nm) 
!                     nmi = NMlistMERGED_bed(i,nm) 
!                     balanceMERGED = balanceMERGED + balance(nmi)
!                     EXITINGvolumeMERGED = EXITINGvolumeMERGED + EXITINGvolume(nmi)
!                     do k=1,kmax
!                        STOREDvolumeMERGED = STOREDvolumeMERGED + STOREDvolume(nm) !(volum1(nm,k)*r1(nm,k,l)-volum0(nm,k)*r0(nm,k,l)) 
!                     enddo
!                  enddo
!              ! elseif (virtualMERGEneu.and.neuMERG(nm)>0) THEN 
!              !    if (neuMERG(nm)==1) then       ! merging in the ADI direction
!              !       nmR = nm + icx
!              !    elseif (neuMERG(nm)==2) then  ! merging in the ADI direction
!              !       nmR = nm - icx
!              !    elseif (neuMERG(nm)==3) then  !lateral merging
!              !       nmR = nm + icy
!              !    elseif (neuMERG(nm)==4) then  !lateral merging
!              !       nmR = nm - icy
!              !    endif
!              !    !fix terms for nm and nmR (nm of Receiving cell)
!              !    QxUP_R   = sum(fluxu(nmR      ,1:kmax,l)) 
!              !    QxDOWN_R = sum(fluxu(nmR-icx  ,1:kmax,l))
!              !    QyUP_R   = sum(fluxv(nmR      ,1:kmax,l)) 
!              !    QyDOWN_R = sum(fluxv(nmR-icy  ,1:kmax,l)) 
!              !    QyUP     = sum(fluxv(nm       ,1:kmax,l)) 
!              !    QyDOWN   = sum(fluxv(nm-icy   ,1:kmax,l)) 
!              !    !fix terms for nm and nmR (nm of Receiving cell)
!              !    EXITINGvolume(nmR)  = (QxUP_R-QxDOWN_R+QyUP_R-QyDOWN_R+QyUP-QyDOWN)*hdt !right balance for nmR
!              !    EXITINGvolume(nm)   = EXITINGvolume(nmR)  !nm does not satisfy mass conservation, I give it the nmR values to pass the test
!              !   
!              !    STOREDvolume(nmR)   = STOREDvolume(nm)  + STOREDvolume(nmR) 
!              !    STOREDvolume(nm)    = STOREDvolume(nmR)
!              !    balance(nmR)        = EXITINGvolume(nm)+STOREDvolume(nm)
!              !    balance(nm)         = balance(nm)    
!              !    EXITINGvolumeMERGED = EXITINGvolume(nmR)
!              !    STOREDvolumeMERGED  = STOREDvolume(nmR)
!              !    balanceMERGED       = balance(nmR)    
!               else
!                  balanceMERGED = balance(nm)
!                  EXITINGvolumeMERGED = EXITINGvolume(nm)
!                  STOREDvolumeMERGED = STOREDvolume(nm)
!               endif
!            !   write(79898991,'(2i6,3f25.15)') nst,nm,EXITINGvolumeMERGED,STOREDvolumeMERGED,balanceMERGED   
!               if (abs(balanceMERGED).gt.max(THRlocalMASSbal,THRlocalMASSbal*EXITINGvolumeMERGED)) then
!                   call nm_to_n_and_m(nm, n, m, gdp)
!                   write(*,*) 'local mass balance for PASSIVE solute not satisfied at cell (n,m) =(',n,m,'). Residual =',balance(nm),'nst=',nst
!                   call d3stop(1, gdp)
!               endif
!
!            endif
!         enddo
!      enddo
!      
!!
!!     local check for sediments
!!
!      if (lsedtot>0) then
!         if(nst>=itmor) then
!            hdtmor  = hdt*morfac
!         else
!            hdtmor = 0._fp
!         endif
!         !
!         do l = 1,lsedtot
!            do nm = 1, nmmax
!               if (kcs(nm)*kfs(nm)*kfsed(nm) == 1) then
!                  !along x
!                  !right
!                  q = sbuu(nm,l)
!                  if (l<=lsed) q = q+ssuu(nm,l)
!                  q = q*guu(nm)*aguu(nm)   
!                  QxUP = q
!                  !left
!                  nmd = nm-nmaxddb
!                  q = sbuu(nmd,l)
!                  if (l<=lsed) q = q+ssuu(nmd,l)
!                  q = q*guu(nmd)*aguu(nmd)   
!                  QxDOWN = q
!                  !along y
!                  !upper
!                  q = sbvv(nm,l)
!                  if (l<=lsed) q = q+ssvv(nm,l) 
!                  q = q*gvv(nm)*agvv(nm)  
!                  QyUP = q
!                  !lower
!                  ndm = nm-1
!                  q = sbvv(ndm,l)
!                  if (l<=lsed) q = q+ssvv(ndm,l) 
!                  q = q*gvv(ndm)*agvv(ndm)   
!                  QyDOWN= q
!   !
!                  EXITINGvolume(nm) = (QxUP-QxDOWN+QyUP-QyDOWN)*hdtmor/rhofrac(l) !/cdryb(l)
!                  STOREDvolume(nm) = 0._fp
!                  !strictly valid if no concentration. if susp conc, I have to add the erosion-deposit term and the stored volume of sedim in the vertical as follows:
!                  !STOREDvolume(nm) = (volum1(nm,k)*r1(nm,k,lstart+l)-volum0(nm,k)*r0(nm,k,lstart+l) !pass r0
!                  varBOTTOM(nm) = - agsqs(nm)*gsqs(nm)*(real(dps(nm),fp)-real(dps0(nm),fp)) !- cause dps is minus the elevation
!                  balance(nm) = EXITINGvolume(nm)+STOREDvolume(nm)+varBOTTOM(nm)
!               !   write(89898990,'(2i6,45f35.15)') nst,nm,QxUP,QxDOWN,QyUP,QyDOWN,dps(nm),dps0(nm),agsqs(nm),EXITINGvolume(nm),varBOTTOM,STOREDvolume(nm) ,balance(nm)
!               endif
!            enddo
!         enddo
!
!
!         do nm = 1, nmmax
!            if (kcs(nm)==1) then
!              ! if (neuMERG(nm+icx)==2.or.neuMERG(nm-icx)==1.or.neuMERG(nm+icy)==4.or.neuMERG(nm-icy)==3) cycle 
!               if (cutcell>0.and.virtualMERGEupdBED) then
!
!                  if(kcs(nm)*kfs(nm) == 1 .and. comparereal(agsqs(nm),0._fp).gt.0  .and. agsqs(nm).lt.thresMERGE_zb) cycle !cycle, since they are merged. note agsqs is poros in sudSURFslopeFAC         
!
!                  balanceMERGED  = 0._fp
!                  EXITINGvolumeMERGED = 0._fp
!                  STOREDvolumeMERGED  = 0._fp
!                  varBOTTOMmerged = 0._fp
!                  do i=1,Nmerged_bed(nm) 
!                     nmi = NMlistMERGED_bed(i,nm) 
!                     balanceMERGED = balanceMERGED + balance(nmi)
!                     EXITINGvolumeMERGED = EXITINGvolumeMERGED + EXITINGvolume(nmi)
!                     STOREDvolumeMERGED =  STOREDvolumeMERGED + STOREDvolume(nm)
!                     varBOTTOMmerged = varBOTTOMmerged + varBOTTOM(nm) ! - agsqs(nm)*gsqs(nm)*(real(dps(nm),fp)-real(dps0(nm),fp))
!                  enddo
!              ! elseif (virtualMERGEneu.and.neuMERG(nm)>0) THEN 
!              !    if (neuMERG(nm)==1) then       ! merging in the ADI direction
!              !       nmR = nm + icx
!              !    elseif (neuMERG(nm)==2) then  ! merging in the ADI direction
!              !       nmR = nm - icx
!              !    elseif (neuMERG(nm)==3) then  !lateral merging
!              !       nmR = nm + icy
!              !    elseif (neuMERG(nm)==4) then  !lateral merging
!              !       nmR = nm - icy
!              !    endif
!              !    !fix terms for nm and nmR (nm of Receiving cell)
!              !    !along x
!              !    !right
!              !    q = sbuu(nmR,l)
!              !    if (l<=lsed) q = q+ssuu(nmR,l)
!              !    q = q*guu(nmR)*aguu(nmR)   
!              !    QxUP_R = q
!              !    !left
!              !    nmd = nmR-nmaxddb
!              !    q = sbuu(nmd,l)
!              !    if (l<=lsed) q = q+ssuu(nmd,l)
!              !    q = q*guu(nmd)*aguu(nmd)   
!              !    QxDOWN_R = q
!              !    !along y
!              !    !upper
!              !    q = sbvv(nmR,l)
!              !    if (l<=lsed) q = q+ssvv(nmR,l) 
!              !    q = q*gvv(nmR)*agvv(nmR)  
!              !    QyUP_R = q
!              !    !lower
!              !    ndm = nmR-1
!              !    q = sbvv(ndm,l)
!              !    if (l<=lsed) q = q+ssvv(ndm,l) 
!              !    q = q*gvv(ndm)*agvv(ndm)   
!              !    QyDOWN_R= q
!              !    !along y DONOR CELL
!              !    !upper
!              !    q = sbvv(nm,l)
!              !    if (l<=lsed) q = q+ssvv(nm,l) 
!              !    q = q*gvv(nm)*agvv(nm)  
!              !    QyUP = q
!              !    !lower
!              !    ndm = nm-1
!              !    q = sbvv(ndm,l)
!              !    if (l<=lsed) q = q+ssvv(ndm,l) 
!              !    q = q*gvv(ndm)*agvv(ndm)   
!              !    QyDOWN= q
!              !    EXITINGvolume(nmR)  = (QxUP_R-QxDOWN_R+QyUP_R-QyDOWN_R+QyUP-QyDOWN)*hdtmor/rhofrac(l) 
!              !    EXITINGvolume(nm) = EXITINGvolume(nmR)
!              !    balance(nm)  = balance(nm) + balance(nmR) 
!              !    balance(nmR) = balance(nm)
!              !    EXITINGvolume(nm)  = EXITINGvolume(nm) + EXITINGvolume(nmR) 
!              !    EXITINGvolume(nmR) = EXITINGvolume(nm)
!              !    STOREDvolume(nm)   = STOREDvolume(nm)  + STOREDvolume(nmR) 
!              !    STOREDvolume(nmR)  = STOREDvolume(nm)
!              !    varBOTTOM(nm) = varBOTTOM(nm) + varBOTTOM(nmR)
!              !    varBOTTOM(nmR) = varBOTTOM(nm)
!              !    balanceMERGED = balance(nm)
!              !    EXITINGvolumeMERGED = EXITINGvolume(nm)
!              !    STOREDvolumeMERGED  = STOREDvolume(nm)
!              !    varBOTTOMmerged     = varBOTTOM(nm)
!               else
!                  balanceMERGED = balance(nm)
!                  EXITINGvolumeMERGED = EXITINGvolume(nm)
!                  STOREDvolumeMERGED = STOREDvolume(nm)
!                  varBOTTOMmerged     = varBOTTOM(nm)
!               endif
!             !  write(79898990,'(2i6,3f25.15)') nst,nm,EXITINGvolumeMERGED,STOREDvolumeMERGED,balanceMERGED   
!               if (abs(balanceMERGED).gt.max(THRlocalMASSbal,THRlocalMASSbal*EXITINGvolumeMERGED)) then
!                   call nm_to_n_and_m(nm, n, m, gdp)
!                   write(*,*) 'local mass balance for sediments not satisfied at cell(',n,m,'). Residual =',balance(nm),'nst=',nst
!                   call d3stop(1, gdp)
!               endif
!            endif
!         enddo
!      endif   
      deallocate(balance,EXITINGvolume,STOREDvolume,varBOTTOM)
    endif
    !
    ! Accumulate water fluxes
    !
    do k = 1,kmax
       do nm = 1, nmmax
          ivol = volnr(nm)
          if (ivol==0) cycle
          !
          do j=1,2
             if (j==1) then
                nm2 = nm+nmaxddb
                q = qxk(nm,k)*hdt
             else
                nm2 = nm+1
                q = qyk(nm,k)*hdt
             endif
             !
             jvol = volnr(nm2)
             if (jvol==0) cycle
             ex = exchnr(j,nm)
             if (ex==0) cycle
             !
             if (ivol==neighb(2,ex)) q = -q
             if (q>0.0_fp) then
                fluxes(1,ex) = fluxes(1,ex) + q
             else
                fluxes(2,ex) = fluxes(2,ex) - q
             endif
          enddo
       enddo
    enddo
    !
    ! Accumulate constituent fluxes; the fluxu array is not yet available upon
    ! the first call from INCHKR (tested by means of associated). However,
    ! that's no problem since no fluxes have to be accumulated at that time.
    !
    if (associated(fluxu)) then
       do l = 1,lstsci
          do k = 1,kmax
             do nm = 1, nmmax
                ivol = volnr(nm)
                if (ivol==0) cycle
                !
                do j = 1,2
                   if (j==1) then
                      nm2 = nm+nmaxddb
                      q = fluxu(nm,k,l)*hdt
                   else
                      nm2 = nm+1
                      q = fluxv(nm,k,l)*hdt
                   endif
                   !
                   jvol = volnr(nm2)
                   if (jvol==0) cycle
                   ex = exchnr(j,nm)
                   if (ex==0) cycle
                   !
                   if (ivol==neighb(2,ex)) q = -q
                   if (q>0.0_fp) then
                      fluxes_r1(1,ex,l) = fluxes_r1(1,ex,l) + q
                   else
                      fluxes_r1(2,ex,l) = fluxes_r1(2,ex,l) - q
                   endif
                enddo
             enddo
          enddo
       enddo
       !
       ! TODO: accumulate 2D fluxes (e.g. anticreep)
       !
    endif
    !
    ! Accumulate bed load and suspended sediment fluxes.
    ! Why not accumulate bed load only since the suspended part is also
    ! included in the FLUX_R1 part? Main reason: SSUU includes SUCOR whereas
    ! the FLUX_R1 computed above doesn't include the SUCOR part. It would be
    ! inconsistent if we were to include SUCOR here in the SBUU accumulation
    ! but in SSUU on the map-file.
    !
    if (lsedtot>0 .and. associated(ssuu)) then
       hdtmor  = hdt*morfac
       !
       do l = 1,lsedtot
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol==0) cycle
             !
             do j = 1,2
                if (j==1) then
                   nm2 = nm+nmaxddb
                   q = sbuu(nm,l)
                   if (l<=lsed) q = q+ssuu(nm,l)
                   q = q*aguu(nm)*guu(nm)*hdtmor
                else
                   nm2 = nm+1
                   q = sbvv(nm,l)
                   if (l<=lsed) q = q+ssvv(nm,l)
                   q = q*agvv(nm)*gvv(nm)*hdtmor
                endif
                !
                jvol = volnr(nm2)
                if (jvol==0) cycle
                ex = exchnr(j,nm)
                if (ex==0) cycle
                !
                if (ivol==neighb(2,ex)) q = -q
                if (q>0.0_fp) then
                   fluxes_sd(1,ex,l) = fluxes_sd(1,ex,l) + q
                else
                   fluxes_sd(2,ex,l) = fluxes_sd(2,ex,l) - q
                endif
             enddo
          enddo
       enddo
    endif
    !
    ! TODO: Accumulate fluxes across domains/partitions.
    !
    ! accumulate immission of discharges
    if (nst.lt.0) fluxesLOC(:) = 0._fp
    do k = 1,kmax
      do nm = 1, nmmax
         ivol = volnr(nm)
         if (ivol>0 .and. kcs(nm)==1) then
            if (nsrc.gt.0.and.nst.ge.0) then 
               do i = 1, nsrc !not optimized should be done outside,its ok for few sources
               ! nm   = (mnksrc(5, i) + ddb) + ((mnksrc(4, i) - 1) + ddb)*icxy this is the one in sud
                  nmS = (mnksrc(4, i)-1)*gdp%d%nmax + mnksrc(5, i)
                  if (nm ==nmS) then
                     fluxesLOC(ivol) = fluxesLOC(ivol) + disch(i)*hdt
                     write(1919191,'(4i7,15f35.15)') nst,nm,mnksrc(4, i)-1, mnksrc(5, i),fluxesLOC(ivol),disch(i)*hdt
                  endif
               enddo
            endif
         endif
      enddo
    enddo
    !
    ! If requested: compute total volume and mass
    !
    if (newvol) then
       volumes = 0.0_fp
       mass_r1 = 0.0_fp
       accdps  = 0.0_fp
       !
       ! Determine volumes
       !
       do k = 1,kmax
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol>0 .and. kcs(nm)==1) then
                volumes(ivol)   = volumes(ivol) + volum1(nm,k)
             endif
          enddo
       enddo
       !
       ! Determine "mass" (i.e. volume*concentration)
       !
       do l = 1, lstsci
          do k = 1,kmax
             do nm = 1, nmmax
                ivol = volnr(nm)
                if (ivol>0 .and. kcs(nm)==1) then
                   mass_r1(ivol,l) = mass_r1(ivol,l) + volum1(nm,k)*r1(nm,k,l)
                endif
             enddo
          enddo
       enddo
       !
       ! Determine cumulative depth
       !
       if (lsedtot>0) then
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol>0 .and. kcs(nm)==1) then
                accdps(ivol) = accdps(ivol) + agsqs(nm)*gsqs(nm)*real(dps(nm),fp)
             endif
          enddo
       endif
!
       if (nneighb/=0) then
          !write(9676767,'(i6,15f35.15)') nst,fluxes(1,1),fluxes(2,1),fluxes(2,1)-fluxes(1,1),volumes(1),volumes(2),fluxesLOC(1),volumes(1)-(fluxes(2,1)-fluxes(1,1))-fluxesLOC(1)
          if (lsedtot>0) then
             !write(9676768,'(i6,45f35.15)')nst,(fluxes_sd(1,1,L)/rhofrac(l),fluxes_sd(2,1,L)/rhofrac(l),l = 1,lsedtot),(fluxes_sd(2,1,L)/rhofrac(l)-fluxes_sd(1,1,L)/rhofrac(l),l = 1,lsedtot),-accdps(1),sum(mass_r1(1,lstart+1:lstart+lsedtot)),-accdps(1)+sum(mass_r1(1,lstart+1:lstart+lsedtot))-sum(fluxes_sd(2,1,1:lsedtot)/rhofrac(1:lsedtot)-fluxes_sd(1,1,1:lsedtot)/rhofrac(1:lsedtot)) !think if mass_r1 has to be converted to cubic meters from kg
          endif
          if (lstsc.gt.0)  then
         !    write(*,*) lstart,lsedtot,lstsci
             write(9676769,'(i6,45f35.15)')nst,(fluxes_r1(1,1,L),fluxes_r1(2,1,L),l = lstart+lsedtot+1,lstsc),(fluxes_r1(2,1,L)-fluxes_r1(1,1,L),l = lstart+lsedtot+1,lstsc),(mass_r1(1,l),l = lstart+lsedtot+1,lstsc),sum(mass_r1(1,lstart+lsedtot+1:lstsc))-sum(fluxes_r1(2,1,lstart+lsedtot+1:lstsc)-fluxes_r1(1,1,lstart+lsedtot+1:lstsc))
          endif
       else !fluxes are allocated of dimension 0 in subr rdmassbal since there are no exiting boundaries (all no-flux boundaries)
          !write(9676767,'(i6,15f35.15)') nst, volumes(1),volumes(2),volumes(1)
          if (lsedtot>0) then
             write(9676768,'(i6,45f35.15)')nst,-accdps(1),sum(mass_r1(1,lstart+1:lstart+lsedtot)),-accdps(1)+sum(mass_r1(1,lstart+1:lstart+lsedtot))  
          endif
          if (lstsc.gt.0)  then
         !    write(*,*) lstart,lsedtot,lstsci
             write(9676769,'(i6,45f35.15)')nst,(mass_r1(1,l),l = lstart+lsedtot+1,lstsc),sum(mass_r1(1,lstart+lsedtot+1:lstsc)) 
          endif
       endif
       !
       ! TODO: Accumulate volumes and masses across domains/partitions.
       !
       ! At start of next step: reset fluxes to 0.
       !
       !resetfluxes = .true.
    endif
end subroutine updmassbal
