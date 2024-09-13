    subroutine UPDATEbank(INTERFtype_l,gsqs,kfs,kcs,r1,s1,u1,v1,kfu,kfv,Umean,Vmean,thick,dps,frac,sourseBANK,Irov,nmmax,mmax,nmax,nmaxus,kmax,dt,MF,nst,nlb,nub,mlb,mub,nmlb,nmub,nmaxddb,ddbound,icx,icy,lundia,itmor,lsedtot,lsed,stage,lstsci, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id: UPDATEbank.f90 7020 2017-02-16 21:50:45Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: erode banks by polygon intersection
!
!    Author: Alberto Canestrelli
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use dfparall
    use Cplusplus
    use mathconsts, only: pi
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                       , pointer :: dim_nmlist
    real(fp)                      , pointer :: fracBANKsuspWASH
    real(fp)                      , pointer :: fracBANKdepos
    real(fp)                      , pointer :: Kbank
    real(fp)                      , pointer :: TAUcrBANKcnst
    real(fp)                      , pointer :: thresMERGE_d
    real(fp)                      , pointer :: thresMERGE_zb
    real(fp)                      , pointer :: t
    real(fp)                      , pointer :: facMERGElink
    integer                       , pointer :: TYPEtauCRbank
    integer                       , pointer :: ERODsubmBANKS
    integer                       , pointer :: Nedge
    integer                       , pointer :: TYPErecVOF
    integer                       , pointer :: TYPEdistrBANKerod
    integer                       , pointer :: ADVECTbank
    integer                       , pointer :: nstREST
    integer                       , pointer :: idebugCUTini
    integer                       , pointer :: idebugCUTfin
    integer                       , pointer :: IstencBANKer
    integer                       , pointer :: typeVIRTmergeUPDbed
    integer                       , pointer :: typeVIRTmergeUPDdepth
    integer                       , pointer :: itmorB
    integer, dimension(:)         , pointer :: edge6
    integer, dimension(:,:)       , pointer :: kfs_cc
    integer, dimension(:,:)       , pointer :: multEXITu
    integer, dimension(:,:)       , pointer :: multEXITv
    integer, dimension(:)         , pointer :: EDGEdry
    integer, dimension(:,:)       , pointer :: Ndry_GRS
    integer, dimension(:,:,:)     , pointer :: nAD
    integer, dimension(:,:,:)     , pointer :: mAD
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANKerod
    integer, dimension(:,:)       , pointer :: por012
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
    real(fp), dimension(:,:)      , pointer :: Eb
    real(fp), dimension(:,:,:)    , pointer :: EbK
    real(fp), dimension(:,:)      , pointer :: dpH
    real(fp), dimension(:,:)      , pointer :: dpL
    real(fp), dimension(:,:)      , pointer :: dpsi
    real(fp), dimension(:,:)      , pointer :: deta
    real(fp), dimension(:,:)      , pointer :: POROS
    real(fp), dimension(:,:)      , pointer :: POROSold
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    real(fp), dimension(:,:,:)    , pointer :: INTx_GRS
    real(fp), dimension(:,:,:)    , pointer :: INTy_GRS
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANKerod
    real(fp), dimension(:,:)      , pointer :: taucr
    real(fp), dimension(:,:,:)    , pointer :: tauBANK
    real(fp), dimension(:,:)      , pointer :: agsqs
    real(fp), dimension(:)        , pointer :: agsqs_link
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp), dimension(:,:)      , pointer :: xcor0
    real(fp), dimension(:,:)      , pointer :: ycor0
    real(fp), dimension(:,:)      , pointer :: xG
    real(fp), dimension(:,:)      , pointer :: yG
    real(fp), dimension(:,:,:)    , pointer :: xcell
    real(fp), dimension(:,:,:)    , pointer :: ycell
    real(fp), dimension(:,:)      , pointer :: Npsi
    real(fp), dimension(:,:)      , pointer :: Neta
    real(fp), dimension(:,:)      , pointer :: Nx
    real(fp), dimension(:,:)      , pointer :: Ny
    real(fp), dimension(:,:)      , pointer :: dpLnew
    real(fp), dimension(:,:)      , pointer :: VOLeros
    logical, dimension(:,:)       , pointer :: CELLtoRECON
    logical, dimension(:,:)       , pointer :: updatedBANK
    logical, dimension(:,:)       , pointer :: oneEXIT
    logical                       , pointer :: DEPOSbankMATERIAL
    logical                       , pointer :: periodSURFACE
    logical                       , pointer :: virtualMERGEupdBED
    logical                       , pointer :: virtualMERGEupdDEPTH
    logical                       , pointer :: virtualLINK
    logical                       , pointer :: forceEb
    real(fp), dimension(:,:)      , pointer :: gsqsR
    real(fp), dimension(:,:)      , pointer :: AreaDistrib
    real(fp), dimension(:,:)      , pointer :: VOLerosEMER
    real(fp), dimension(:,:)      , pointer :: s1new
    real(fp), dimension(:,:,:,:)  , pointer :: Cnew
    logical, dimension(:,:)       , pointer :: updatedBANKloc
!   start IBM_research variables, most of them will be eventually removed    
    logical                       , pointer :: FIXEDcoastBANKS
!   end IBM_research     
!
! Global variables
!
    real(fp), dimension(nlb:nub,mlb:mub, kmax, lstsci)                  , intent(inout) :: r1 !
    real(fp), dimension(nlb:nub,mlb:mub, lsed)                          , intent(out)   :: sourseBANK
    real(fp), dimension(nlb:nub,mlb:mub, lsedtot)                       , intent(in)    :: frac
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in)    :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in)    :: v1
    real(fp), dimension(nmlb:nmub)                                      , intent(out)   :: umean
    real(fp), dimension(nmlb:nmub)                                      , intent(out)   :: vmean  
    real(fp), dimension(kmax)                                           , intent(in)    :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(inout) :: dps
    real(fp)                                                            , intent(in)    :: dt
    real(fp)                                                            , intent(in)    :: MF
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: INTERFtype_l
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfv 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: itmor
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: nmaxddb
    integer                                                             , intent(in)    :: ddbound
    integer                                                             , intent(in)    :: nmmax 
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: lundia
    integer                                                             , intent(in)    :: lsedtot
    integer                                                             , intent(in)    :: lsed
    integer                                                             , intent(in)    :: lstsci
    character(8)                                                        , intent(in)    :: stage       !
!
! Local variables
!
    integer                        :: cont
    integer                        :: m
    integer                        :: n
    integer                        :: k
    integer                        :: kk
    integer                        :: VERTpEND
    integer                        :: L
    integer                        :: M1
    integer                        :: M2
    integer                        :: n1
    integer                        :: n2
    integer                        :: mER
    integer                        :: nER
    integer                        :: km
    integer                        :: kn
    integer                        :: nADJ
    integer                        :: kADJ
    integer                        :: mADJ
    integer                        :: contLL
    integer                        :: signN(1:4) = (/ 1,-1,-1,1/)
    integer                             :: Idummy
    integer                             :: Idummyy
    integer                             :: Idummyyy
    real(fp), dimension(1)              :: Rdummy1
    logical                             :: Ldummy
!
    real(fp)                       :: MFloc
    real(fp)                       :: DHsource
    real(fp)                       :: DHdt_source(1:lsed)
    real(fp)                       :: VOLUMEsource
    real(fp)                       :: ds1
    real(fp)                       :: ADDEDwater
    real(fp)                       :: WATERtoADD
    real(fp)                       :: recovVOLbed
    real(fp)                       :: VOLUMEonTHEbed
    real(fp)                       :: dbed
    real(fp)                       :: AreaDistr
    real(fp)                       :: depSPEED
    real(fp)                       :: erosSPEED
    real(fp)                       :: dpDRY
    real(fp)                       :: dpWET
    real(fp)                       :: AREAprov
    real(fp)                       :: dummyR,dummyR2
    real(fp)                       :: angle
    real(fp)                       :: dx
    real(fp)                       :: dy
    real(fp)                       :: ExDT
    real(fp)                       :: EyDT
    real(fp)                       :: displ
    real(fp)                       :: LL
    real(fp)                       :: shear
    real(fp)                       :: Ledge
    real(fp)                       :: tauc
    real(fp)                       :: AREAeros
    real(fp)                       :: EbSIDE
    real(fp)                       :: normx
    real(fp)                       :: normy
    real(fp)                       :: dpLL
    real(fp)                       :: s11
    real(fp)                       :: XtauPEAK(2) 
    real(fp)                       :: YtauPEAK(2) 
    real(fp)                       :: peak(2)  
    real(fp)                       :: sig(2)  
    real(fp)                       :: radius
    real(fp)                       :: Ebb
    real(fp)                       :: corr45_Nx 
    real(fp)                       :: corr45_Ny  
    real(fp)                       :: corr45_1x 
    real(fp)                       :: corr45_1y 
    real(fp)                       :: facMERGElink_loc
!
    logical                        :: INSIDE
    logical                        :: EDGEtypeBANK02
    logical                        :: MOVEbank
!
    character*100                  :: FILEtypeCELL
!
!   declaration of variable to be passed to C subroutine 
!
    INTEGER(kind = C_INT) NwAD
    !13 columns is the WORST CASE SCENARIO: 9 POLYGONS FROM THE INTERFACES OF THE STENCIL PLUS 4 FROM THE 4 EDGES (OR PART OF EDGES) OF THE CELL n,m. Vertices of unions are maximum 4 per number of poligons.
    REAL(kind=C_DOUBLE) :: POLYintersX(100,13),POLYintersY(100,13),POLYStoBEjoinedX(100,13),POLYStoBEjoinedY(100,13),POLYSunionX(100,13),POLYSunionY(100,13)  
    REAL(kind=C_DOUBLE) :: POLYresX(100,13),POLYresY(100,13) 
    INTEGER(kind=c_int), dimension(13) ::VERTinters,VERTres,VERTtoBEjoined,VERTunion
    INTEGER(kind=c_int) :: NPOLYStoBEjoined,NPOLYSinters,NPOLYSunion
    REAL(kind=C_DOUBLE) :: xclip(5)
    REAL(kind=C_DOUBLE) :: yclip(5)
    integer(kind=c_int) :: Ndry
    REAL(kind=C_DOUBLE) :: absMAXx(1)
    REAL(kind=C_DOUBLE) :: absMAXy(1)
    !
    ! compute INTERFtype_l(:,:)
    !      
    dim_nmlist            => gdp%gdimbound%dim_nmlist
    fracBANKsuspWASH      => gdp%gdimbound%fracBANKsuspWASH
    fracBANKdepos         => gdp%gdimbound%fracBANKdepos
    Kbank                 => gdp%gdimbound%Kbank
    TAUcrBANKcnst         => gdp%gdimbound%TAUcrBANKcnst
    thresMERGE_d          => gdp%gdimbound%thresMERGE_d
    thresMERGE_zb         => gdp%gdimbound%thresMERGE_zb
    t                     => gdp%gdimbound%t
    facMERGElink          => gdp%gdimbound%facMERGElink
    TYPEtauCRbank         => gdp%gdimbound%TYPEtauCRbank
    ERODsubmBANKS         => gdp%gdimbound%ERODsubmBANKS
    Nedge                 => gdp%gdimbound%Nedge
    TYPErecVOF            => gdp%gdimbound%TYPErecVOF
    TYPEdistrBANKerod     => gdp%gdimbound%TYPEdistrBANKerod
    ADVECTbank            => gdp%gdimbound%ADVECTbank
    nstREST               => gdp%gdimbound%nstREST
    idebugCUTini          => gdp%gdimbound%idebugCUTini
    idebugCUTfin          => gdp%gdimbound%idebugCUTfin
    IstencBANKer          => gdp%gdimbound%IstencBANKer
    typeVIRTmergeUPDbed   => gdp%gdimbound%typeVIRTmergeUPDbed
    typeVIRTmergeUPDdepth => gdp%gdimbound%typeVIRTmergeUPDdepth
    itmorB                => gdp%gdimbound%itmorB
    edge6                 => gdp%gdimbound%edge6
    kfs_cc                => gdp%gdimbound%kfs_cc
    multEXITu             => gdp%gdimbound%multEXITu
    multEXITv             => gdp%gdimbound%multEXITv
    EDGEdry               => gdp%gdimbound%EDGEdry
    Ndry_GRS              => gdp%gdimbound%Ndry_GRS
    nAD                   => gdp%gdimbound%nAD
    mAD                   => gdp%gdimbound%mAD
    EDGEtypeBANKerod      => gdp%gdimbound%EDGEtypeBANKerod
    por012                => gdp%gdimbound%por012
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
    Eb                    => gdp%gdimbound%Eb
    EbK                   => gdp%gdimbound%EbK
    dpH                   => gdp%gdimbound%dpH
    dpL                   => gdp%gdimbound%dpL
    dpsi                  => gdp%gdimbound%dpsi
    deta                  => gdp%gdimbound%deta
    POROS                 => gdp%gdimbound%POROS
    POROSold              => gdp%gdimbound%POROSold
    PSIx                  => gdp%gdimbound%PSIx
    PSIy                  => gdp%gdimbound%PSIy
    ETAx                  => gdp%gdimbound%ETAx
    ETAy                  => gdp%gdimbound%ETAy
    INTx_GRS              => gdp%gdimbound%INTx_GRS
    INTy_GRS              => gdp%gdimbound%INTy_GRS
    EDGExyBANKerod        => gdp%gdimbound%EDGExyBANKerod
    taucr                 => gdp%gdimbound%taucr
    tauBANK               => gdp%gdimbound%tauBANK
    agsqs                 => gdp%gdimbound%agsqs
    agsqs_link            => gdp%gdimbound%agsqs_link
    aguu                  => gdp%gdimbound%aguu
    agvv                  => gdp%gdimbound%agvv
    xcor0                 => gdp%gdimbound%xcor0
    ycor0                 => gdp%gdimbound%ycor0
    xG                    => gdp%gdimbound%xG
    yG                    => gdp%gdimbound%yG
    xcell                 => gdp%gdimbound%xcell
    ycell                 => gdp%gdimbound%ycell
    Npsi                  => gdp%gdimbound%Npsi
    Neta                  => gdp%gdimbound%Neta
    Nx                    => gdp%gdimbound%Nx
    Ny                    => gdp%gdimbound%Ny
    dpLnew                => gdp%gdimbound%dpLnew
    VOLeros               => gdp%gdimbound%VOLeros
    CELLtoRECON           => gdp%gdimbound%CELLtoRECON
    updatedBANK           => gdp%gdimbound%updatedBANK
    oneEXIT               => gdp%gdimbound%oneEXIT
    DEPOSbankMATERIAL     => gdp%gdimbound%DEPOSbankMATERIAL
    periodSURFACE         => gdp%gdimbound%periodSURFACE
    virtualMERGEupdBED    => gdp%gdimbound%virtualMERGEupdBED
    virtualMERGEupdDEPTH  => gdp%gdimbound%virtualMERGEupdDEPTH
    virtualLINK           => gdp%gdimbound%virtualLINK
    forceEb               => gdp%gdimbound%forceEb
    gsqsR                 => gdp%gdimbound%Dwrka2_E
    AreaDistrib           => gdp%gdimbound%Dwrka3_E
    VOLerosEMER           => gdp%gdimbound%Dwrka4_E
    s1new                 => gdp%gdimbound%Dwrka5_E
    Cnew                  => gdp%gdimbound%DwrkakL1_E
    updatedBANKloc        => gdp%gdimbound%Lwrka1_E
!   start IBM_research variables, most of them will be eventually removed    
    FIXEDcoastBANKS       => gdp%gdimbound%FIXEDcoastBANKS
!   end IBM_research     
    !
    IF (ERODsubmBANKS) then
       do m=2,mmax-1
          do n=2,nmaxus-1
             INTERFtype_l(n,m)   = 0
             !maybe ifkfs(n,m)==1 not needed, double check     
             if (kfs(n,m)==1) then    
                IF (kfs_cc(n,m).ge.0) THEN 
                   !erode submerged banks
                   INTERFtype_l(n,m)   = 1
                ENDIF
             endif
          enddo
       enddo
    else
       do m=2,mmax-1
          do n=2,nmaxus-1
             INTERFtype_l(n,m)   = 0
             !maybe if kfs(n,m)==1 not needed, double check     
             if (kfs(n,m)==1) then   
                IF (kfs_cc(n,m).eq.0) THEN 
                   !erode submerged banks
                   INTERFtype_l(n,m)   = 1
                ENDIF
             endif
          enddo
       enddo
    endif
    !
    !   compute tau on the bank
    !
    CALL COMPUTEtau(tauBANK,oneEXIT,multEXITu,multEXITv,s1,u1,v1,kfu,kfv,Umean,Vmean,thick,nx,ny,EDGEtypeBANKerod,ETAx,ETAy,PSIx,PSIy,por012,kfs_cc,&
                    kfs,kcs,nmmax,nlb,nub,mlb,mub,nmlb,nmub,kmax,nmaxddb,ddbound,gdp)
    !
    !   TAUCR idealized
    !
    SELECT CASE(TYPEtauCRbank)
    CASE(0) 
       !CONSTANT EVERYWHERE
       taucr(:,:) = TAUcrBANKcnst
       !
    CASE(1) 
       !
       !CIRCLE WITH HIGH INNER BANK  AND LOW OUTER BANK
       !
       DO m=1,mmax 
          DO n=1,nmaxus
             radius = sqrt(xg(n,m)**2+yg(n,m)**2)
             if (radius>60) then
                taucr(n,m) = TAUcrBANKcnst
             else
                taucr(n,m) = 100
             endif
          ENDDO
       ENDDO
       !
    CASE(2) 
        !
        !Read TAUcrBANK from file       
        !
    CASE DEFAULT 
       write(*,*) 'TYPEtauCRbank non admitted, TYPEtauCRbank=',TYPEtauCRbank
       call d3stop(1, gdp)
    END SELECT
    !
    ! store porosity in porosOLD and reset Eb and EbK
    !
    !porosOLD(:,:) = POROS(:,:)
    do m = 1, mmax 
       do n = 1, nmaxus
          porosOLD(n,m) = POROS(n,m)
       enddo
    enddo
    Eb(:,:)    = 0._fp
    EbK(:,:,:) = 0._fp
    !    
    ! compute actual MF and MOVEbank
    !
    if ( MF<0.000001_fp) then !
       MFloc = 1._fp
       MOVEbank = .false.
    else
       MFloc = MF 
       if (nst < itmorB) then
          MOVEbank = .false.
       else
          MOVEbank = .true.
       endif
    endif
    !
    ! LATERAL EROSION
    !   
    ADVECTbank  =1
    SELECT CASE(ADVECTbank)
    CASE(1) 
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (kcs(n,m)==1) then
                !   
                !NOTE: there is no point to erode on the interface if the poros=0 (i.e. if edge has zero length) or 
                !      if poros=1 (no veget/cohesive cell)
                !
                if (INTERFtype_l(n,m)==1.and.por012(n,m) == 2) then    
                   !tauBANK(5...) contains shear at interface ,if existing
                   if (tauBANK(5,n,m).gt.taucr(n,m)) then  
                      Eb(n,m) = MFloc*Kbank*(tauBANK(5,n,m)/taucr(n,m)-1._fp)
                      if(forceEb) then
                         Eb(n,m) = Kbank   
                      endif 
                   endif
                endif
                !
                do k=1,4  
                   nADJ = nAD(n,m,K)
                   mADJ = mAD(n,m,K)
                   kADJ = edge6(k+2)
                   IF (ERODsubmBANKS.EQ.1) THEN
                     EDGEtypeBANK02  =  ABS(EDGEtypeBANKerod(k,n,m)).le.2 !ABS(EDGEtypeBANKerod(k,n,m)).le.1.or.abs(EDGEtypeBANKerod(k,n,m)).eq.2 
                   ELSE
                     EDGEtypeBANK02  =  EDGEtypeBANKerod(k,n,m).eq.0.or.EDGEtypeBANKerod(k,n,m).eq.-2 .or.EDGEtypeBANKerod(k,n,m).eq.-1
                   ENDIF
                   !there is no point to erode on the edge if the poros=0 (i.e. if edge has zero length)
                   if ((EDGEtypeBANK02.and.(EDGEtypeBANKerod(kADJ,nADJ,mADJ).eq.3)).or.(EDGEtypeBANKerod(K,n,m).eq.4)) then   
                      !  if ( ( (EDGEtypeBANKerod(K,n,m).eq.0.or.EDGEtypeBANKerod(K,n,m).eq.2).and.(EDGEtypeBANKerod(kADJ,nADJ,mADJ).eq.1) ) .or.(EDGEtypeBANKerod(K,n,m).eq.3)) then   
                      !
                      !note that cells might be both dry but still a vegetated - non vegetated interface. basically it erodes if
                      ! tauBANK(nADJ,mADJ) is larger than taucr otherwise nothing happens
                      !
                      if (tauBANK(kADJ,nADJ,mADJ).gt.taucr(n,m)) then
                         EbK(K,n,m) = MF*Kbank*(tauBANK(kADJ,nADJ,mADJ)/taucr(n,m)-1._fp)
                      endif     
                      if(forceEb) then
                         EbK(k,n,m) = Kbank  
                      endif                                    
                   endif
                enddo
             else 
                !if kcs=2 or 0
                EbK(:,n,m) = 0._fp
                Eb(n,m) = 0._fp
             endif
          enddo
       enddo
       !
       ! Smooth Eb
       !
       call SUBRsmoothEb(Eb,oneEXIT,kcs,kfu,kfv,kfs,nmmax,nlb,nub,mlb,mub,nmlb,nmub,kmax,nmaxddb,ddbound, gdp)
       !open(7787878,file='all_polygons.pol',form = 'formatted',status='replace')
       !
       ! Periodic Eb
       !
       if (periodSURFACE) then 
          !the values of Eb in the periodic halo are needed for average 
          CALL perCELLvar2D(Eb,nlb,nub,mlb,mub,kmax, gdp)
       endif
       !
       ! Erode banks
       !
       do m=2,mmax-1
          do n=2,nmaxus-1
            updatedBANK(n,m) = .false.
            NPOLYStoBEjoined=0
            if (kcs(n,m)==1) then
               ! IF there are no vegeted areas (i.e. poros=1) I do no compute erosion
               if(comparereal(poros(n,m),1._fp).lt.0) then 
                  do km = m-IstencBANKer,m+IstencBANKer   
                     do kn = n-IstencBANKer,n+IstencBANKer
                        if (Eb(kn,km).gt.0._fp) then 
                           !note that Eb is zero for kcs not equal to one so the boundary is skipped 
                           NwAD = Ndry_GRS(kn,km)
                           !
                           ! Can be avoided checking before for empty areas
                           !
                           !CALL A_G_Poly(INTx_GRS(1:NwAD,kn,km),INTy_GRS(1:NwAD,kn,km),NwAD,AREAprov,dummyR,dummyR,1) 
                           !if (comparereal(AREAprov,0._fp).ne.0) then
                              !Note: angle, dx and dy can be precomputed
                              dx = INTx_GRS(NwAD,kn,km) - INTx_GRS(1,kn,km) 
                              dy = INTy_GRS(NwAD,kn,km) - INTy_GRS(1,kn,km)  
                              angle = atan2(dx*Ny(kn,km)-dy*Nx(kn,km),dx*Nx(kn,km)+dy*Ny(kn,km)) 
                              displ = Eb(kn,km)*dt
                              ExDT = displ*Nx(kn,km)  
                              EyDT = displ*Ny(kn,km)  
                              ! To be checcked if ROTATE was needed cause Npsi(n,m),Neta(n,m) is along the edge I think
                              NPOLYStoBEjoined = NPOLYStoBEjoined + 1
                              if (angle.lt.0.d0) then 
                                 !
                                 ! the interface INT(1)---INT(NwAD)   turns  anticlockwise around the normal
                                 !
                                 ! The four corr45_XX termes make the erosion polygon to be a trapezium with 45 degree angle
                                 !
                                 corr45_Nx = - displ*Ny(kn,km) 
                                 corr45_Ny =   displ*Nx(kn,km)
                                 corr45_1x =   displ*Ny(kn,km)
                                 corr45_1y = - displ*Nx(kn,km)
                                 POLYStoBEjoinedX(1:4,NPOLYStoBEjoined) =  (/ INTx_GRS(1,kn,km) , INTx_GRS(NwAD,kn,km) , INTx_GRS(NwAD,kn,km) + ExDT + corr45_Nx , INTx_GRS(1,kn,km) + ExDT + corr45_1x /)
                                 POLYStoBEjoinedY(1:4,NPOLYStoBEjoined) =  (/ INTy_GRS(1,kn,km) , INTy_GRS(NwAD,kn,km) , INTy_GRS(NwAD,kn,km) + EyDT + corr45_Ny , INTy_GRS(1,kn,km) + EyDT + corr45_1y /)
                              else
                                 corr45_Nx =   displ*Ny(kn,km)  
                                 corr45_Ny = - displ*Nx(kn,km)
                                 corr45_1x = - displ*Ny(kn,km)
                                 corr45_1y =   displ*Nx(kn,km)
                                 POLYStoBEjoinedX(1:4,NPOLYStoBEjoined) =  (/ INTx_GRS(NwAD,kn,km) , INTx_GRS(1,kn,km) , INTx_GRS(1,kn,km) + ExDT + corr45_1x , INTx_GRS(NwAD,kn,km) + ExDT + corr45_Nx /) 
                                 POLYStoBEjoinedY(1:4,NPOLYStoBEjoined) =  (/ INTy_GRS(NwAD,kn,km) , INTy_GRS(1,kn,km) , INTy_GRS(1,kn,km) + EyDT + corr45_1y , INTy_GRS(NwAD,kn,km) + EyDT + corr45_Ny /)                                    
                              endif
                              VERTtoBEjoined(NPOLYStoBEjoined) = 4
                              ! FOR DEBUGGING:
                              !if (nst==-72000) then
                              !      write(7787878,*) 'L001'
                              !      write(7787878,*)VERTtoBEjoined(NPOLYStoBEjoined),' 2'
                              !      do kk = 1,VERTtoBEjoined(NPOLYStoBEjoined)
                              !         write(7787878,*)POLYStoBEjoinedX(kk,NPOLYStoBEjoined),POLYStoBEjoinedY(kk,NPOLYStoBEjoined)
                              !      enddo
                              !endif
                              if(km.eq.m.and.kn.eq.n) then
                                 !
                                 ! check if courant number for bank erosion is<1
                                 ! It was for checking that everything was correct. 
                                 ! Now it can maybe be replaced with a less expensive check on the length of Ebx and Eby compared to  (dx,dy)
                                 !
                                 CALL rectPOLinRECTstenc(POLYStoBEjoinedX(1:4,NPOLYStoBEjoined),POLYStoBEjoinedY(1:4,NPOLYStoBEjoined),m,n,INSIDE, gdp)
                                 IF (INSIDE.EQ..FALSE.) THEN
                                    WRITE(*,*) 'Courant condition on bank erosion not respected. Decrease Morphological factor. In the cell:'
                                    write(*,*) nst,m,n,km,kn 
                                    open(78,file='subj.txt',form = 'formatted',status='replace')
                                    write(78,*)NPOLYStoBEjoined
                                    do k = 1,NPOLYStoBEjoined
                                       write(78,*)VERTtoBEjoined(K)
                                       do kk = 1,VERTtoBEjoined(K)
                                          write(78,*)POLYStoBEjoinedX(kk,k),POLYStoBEjoinedY(kk,k)
                                       enddo
                                    enddo
                                    call d3stop(1, gdp)
                                 ENDIF
                              endif
                           !endif
                        endif
                     enddo
                  enddo
                  !
                  ! now only for cell n,m:
                  ! erode on the other edges
                  !
                  DO K=1,4   
                     if (EbK(K,n,m).gt.0._fp) then
                        dx = EDGExyBANKerod(n,m,k,2,1)-EDGExyBANKerod(n,m,k,1,1)
                        dy = EDGExyBANKerod(n,m,k,2,2)-EDGExyBANKerod(n,m,k,1,2)
                        IF (mod(K,2).eq.0) then
                           Normx = PSIx(n,m)*signN(K)
                           Normy = PSIy(n,m)*signN(K)
                        else
                           Normx = ETAx(n,m)*signN(K)
                           Normy = ETAy(n,m)*signN(K)
                        endif
                        !
                        ! atan2(y, x) is the the angle in radians between the positive x-axis of a plane 
                        ! and the point given by the coordinates (x, y) on it. 
                        ! The angle is positive for counter-clockwise angles (upper half-plane, y > 0), 
                        ! and negative for clockwise angles (lower half-plane, y < 0).
                        ! if you want angle between 0 and 2pi use angle = mod(atan2(x1*y2-x2*y1,x1*x2+y1*y2),2*pi) 
                        ! (from http://www.mathworks.com/matlabcentral/newsreader/view_thread/151925)
                        !  
                        angle = atan2(dx*Normy-dy*Normx,dx*Normx+dy*Normy)    
                        displ = EbK(K,n,m)*dt
                        ExDT = displ*Normx  
                        EyDT = displ*Normy 
                        ! To be checcked if ROTATE was needed cause Npsi(n,m),Neta(n,m) is along the edge I think
                        NPOLYStoBEjoined = NPOLYStoBEjoined + 1
                        if (angle.lt.0.d0) then 
                           !
                           ! The interface INT(1)---INT(NwAD)   turns  anticlockwise around the normal
                           !
                           !
                           ! The four corr45_XX termes make the erosion polygon to be a trapezium with 45 degree angle
                           !                     
                           corr45_Nx = - displ*Normy   
                           corr45_Ny =   displ*Normx
                           corr45_1x =   displ*Normy
                           corr45_1y = - displ*Normx
                           POLYStoBEjoinedX(1:4,NPOLYStoBEjoined) =  (/ EDGExyBANKerod(n,m,k,1,1) , EDGExyBANKerod(n,m,k,2,1) , EDGExyBANKerod(n,m,k,2,1) + ExDT + corr45_Nx , EDGExyBANKerod(n,m,k,1,1) + ExDT + corr45_1x /)
                           POLYStoBEjoinedY(1:4,NPOLYStoBEjoined) =  (/ EDGExyBANKerod(n,m,k,1,2) , EDGExyBANKerod(n,m,k,2,2) , EDGExyBANKerod(n,m,k,2,2) + EyDT + corr45_Ny , EDGExyBANKerod(n,m,k,1,2) + EyDT + corr45_1y /) 
                        else
                           corr45_Nx =   displ*Normy  
                           corr45_Ny = - displ*Normx
                           corr45_1x = - displ*Normy
                           corr45_1y =   displ*Normx
                           POLYStoBEjoinedX(1:4,NPOLYStoBEjoined) =  (/ EDGExyBANKerod(n,m,k,2,1) , EDGExyBANKerod(n,m,k,1,1) , EDGExyBANKerod(n,m,k,1,1) + ExDT + corr45_1x , EDGExyBANKerod(n,m,k,2,1) + ExDT + corr45_Nx /) 
                           POLYStoBEjoinedY(1:4,NPOLYStoBEjoined) =  (/ EDGExyBANKerod(n,m,k,2,2) , EDGExyBANKerod(n,m,k,1,2) , EDGExyBANKerod(n,m,k,1,2) + EyDT + corr45_1y , EDGExyBANKerod(n,m,k,2,2) + EyDT + corr45_Ny /)                                    
                        endif
                        VERTtoBEjoined(NPOLYStoBEjoined) = 4
                        ! FOR DEBUGGING:
                        !if (nst==72000) then
                        !   write(7787878,*) 'L001'
                        !   write(7787878,*)VERTtoBEjoined(NPOLYStoBEjoined),' 2'
                        !   do kk = 1,VERTtoBEjoined(NPOLYStoBEjoined)
                        !      write(7787878,*)POLYStoBEjoinedX(kk,NPOLYStoBEjoined),POLYStoBEjoinedY(kk,NPOLYStoBEjoined)
                        !   enddo
                        !endif
                     ENDIF
                  ENDDO                  
               endif ! end if(comparereal(poros(n,m),1._fp).lt.0) then

               !
               IF (NPOLYStoBEjoined.GT.0) THEN  
                  absMAXx = MAXVAL( (/ abs(xcor0(n-1,m-1)), abs(xcor0(n-1,m)), abs(xcor0(n,m)), abs(xcor0(n,m-1)) /) ) 
                  absMAXy = MAXVAL( (/ abs(ycor0(n-1,m-1)), abs(ycor0(n-1,m)), abs(ycor0(n,m)), abs(ycor0(n,m-1)) /) )
                  !CALL wrapClipperRes(1,POLYresX,POLYresY,VERTres,absMAXx,absMAXy) !1:union of all the subject polygons
                  if (comparereal(poros(n,m),0._fp).eq.0) then 
                    ! if completely dry cell I use the entire cell (I did not do a reconstruction there so I do not have the dry part in INTx_GRS,INTy_GRS
                     Ndry = 4
                     ! The Fortran 2003 Handbook, pag 567 says: "The requirement for explicit shape or assumed size rules out assumed-shape 
                     ! and deferred-shape arrays, plus all array sections". So I cannot pass array section to C++ subroutine. I copy to xclip/yclip.
                     xclip(1:4) = xcell(1:4,n,m) 
                     yclip(1:4) = ycell(1:4,n,m) 
                  else            
                     ! If interfacial cell I use its dry part.
                     Ndry = Ndry_GRS(n,m)  
                     xclip(1:Ndry) = INTx_GRS(1:Ndry,n,m)
                     yclip(1:Ndry) = INTy_GRS(1:Ndry,n,m)
                  endif 
                  !call C++ wrapping routine that colls polygon clipping
                  CALL wrapUNIintersNM(Ndry,xclip,yclip, &
                        POLYStoBEjoinedX,POLYStoBEjoinedY,NPOLYStoBEjoined,VERTtoBEjoined,&  !polygons to join (union)
                        POLYSunionX,POLYSunionY,NPOLYSunion,VERTunion, &                     !union of polygons 
                        POLYintersX,POLYintersY,NPOLYSinters,VERTinters,&                    !intersection of union with dry cell
                        absMAXx,absMAXy)                    
                  AREAeros = 0.D0
                  DO K =1,NPOLYSinters
                     ! All vertices (plus end vertex=first vertex)
                     VERTpEND = VERTinters(K)+1 
                     POLYintersX(VERTpEND,K) = POLYintersX(1,K)
                     POLYintersY(VERTpEND,K) = POLYintersY(1,K)
                     dummyR = dble(m)
                     dummyR2 = dble(n)
                     CALL A_G_Poly(POLYintersX(1:VERTpEND,K),POLYintersY(1:VERTpEND,K),VERTpEND,AREAprov,dummyR,dummyR2,11,9999,gdp) !  lunscr=9999
                     ! The intersection is an anti-clockwise polygon, unless I have a hole inside a polygon in that case it is negative area and has to be taken out (see my notes: UNION_OFerodingPOLYGONS(THEsmallerISclockwise).bmp)
                     AREAeros =  AREAeros + AREAprov 
                  ENDDO
                  if (AREAeros.gt.0._fp) then 
                     updatedBANK(n,m) = .true.  
                     ! FOR DEBUGGING PURPOSES
                     !if(m.eq.39.and.n.eq.76.and.nst.eq.72000) then      !.and.nst.eq.75
                     !   open(78,file='solution.txt',form = 'formatted',status='replace')
                     !   write(78,*)NPOLYSinters
                     !   do k = 1,NPOLYSinters
                     !      write(78,*)VERTinters(K)
                     !      do kk = 1,VERTinters(K)
                     !         write(78,*)POLYintersX(kk,k),POLYintersY(kk,k)
                     !      enddo
                     !   enddo
                     !   close(78)
                     !   open(78,file='subj.txt',form = 'formatted',status='replace')
                     !   write(78,*)NPOLYStoBEjoined
                     !   do k = 1,NPOLYStoBEjoined
                     !      write(78,*)VERTtoBEjoined(K)
                     !      do kk = 1,VERTtoBEjoined(K)
                     !         write(78,*)POLYStoBEjoinedX(kk,k),POLYStoBEjoinedY(kk,k)
                     !      enddo
                     !   enddo
                     !   close(78)
                     !   open(78,file='CLIP.txt',form = 'formatted',status='replace')
                     !   write(78,*)1
                     !   do k = 1,1
                     !      write(78,*)Ndry
                     !      do kk = 1,Ndry
                     !         write(78,*)xclip(kk),yclip(kk)
                     !      enddo
                     !   enddo
                     !   close(78)
                     !   open(78,file='union.txt',form = 'formatted',status='replace')
                     !   write(78,*)NPOLYSunion
                     !   do k = 1,NPOLYSunion
                     !      write(78,*)VERTunion(K)
                     !      do kk = 1,VERTunion(K)
                     !         write(78,*)POLYSunionX(kk,k),POLYSunionY(kk,k)
                     !      enddo
                     !   enddo
                     !   close(78)
                     !   write(55554444,'(2i8,10f25.15)')nst,NPOLYSinters,AREAeros,gsqs(n,m), poros(n,m),AREAeros/gsqs(n,m) 
                     !endif
                     !
                     !compute vertical extension of erosion (basically the new bed elevation) as the verage of adjacent dpL in cut cells  
                     !
                     if (comparereal(poros(n,m),0._fp)==0) then 
                        dpLnew(n,m) = 0._fp
                        s11 = 0._fp
                        Cnew(n,m,:,:)  = 0._fp
                        contLL = 0
                        !3x3 stencil
                        do km = m-1,m+1   
                           do kn = n-1,n+1
                              if (comparereal(porosOLD(kn,km),0._fp)>0) then 
                                 !
                                 !this excludes also cell (n,m). I use old so I dont get adjacent that have already changed porosity cause of erosion 
                                 !
                                 dpLnew(n,m) = dpLnew(n,m) + dpL(kn,km)
                                 s11  = s11 + s1(kn,km)     
                                 Cnew(n,m,1:kmax,1:lstsci) = Cnew(n,m,1:kmax,1:lstsci) + r1(kn,km,1:kmax,1:lstsci)  
                                 contLL = contLL + 1
                              endif
                           enddo
                        enddo
                        if (contLL==0) then
                           write(*,*) 'No adjacent non vegetated cells. Error in UpdateBANK.(M.N)=',M,N
                           call d3stop(1, gdp)
                        else
                           dpLnew(n,m) = dpLnew(n,m)/contLL 
                           !s1new only used for VOLerosEMER
                           s1new(n,m) = s11/contLL 
                           Cnew(n,m,1:kmax,1:lstsci) = Cnew(n,m,1:kmax,1:lstsci)/contLL
                        endif
                     else
                        !
                        !the cell is cut I take dpL of the cell itself
                        !
                        dpLnew(n,m) = dpL(n,m)
                        s1new(n,m) = s1(n,m)
                        Cnew(n,m,1:kmax,1:lstsci) = r1(n,m,1:kmax,1:lstsci)
                     endif
                     !
                     !NOTE: clearly I cannot prescribe dpL(n,m) = dpLnew(n,m) directly here because adjacent values will be influenced by this change
                     !
                     !Eroded volume:  ( dpL(n,m)-dpH(n,m) is always positive since dpL>=dpH (depth is minus bed elevation) therefore VOLeros always >0 )
                     VOLeros(n,m)     = AREAeros * (  dpLnew(n,m)-dpH(n,m))   
                     !emerged part of falling bank:
                     VOLerosEMER(n,m) = AREAeros * (- dpH(n,m)-s1new(n,m))    
                     !
                     ! update porosity
                     ! 
                     if (MOVEbank) then 
                        !MOVEbank=true. Instead if MF=0 or or nst<itmorB MOVEbank is false and porosity is not changed
                        poros(n,m) = poros(n,m) + AREAeros/gsqs(n,m)
                        if (poros(n,m).lt.-0.00001_fp) then
                           write(*,*) 'Error: negative porosity, value=',poros(n,m)
                           call d3stop(1, gdp)
                        elseif (poros(n,m).gt.1.00001_fp) then
                           write(*,*) 'Error: Porosity value>1: poros =', poros(n,m),m,n
                           call d3stop(1, gdp)
                        elseif (poros(n,m).lt.0.0000000000001_fp) then
                           poros(n,m) = 0._fp 
                           updatedBANK(n,m) =.false. ! it was true but i send poros to 0
                        elseif (poros(n,m).gt.0.999999999_fp) then
                           ! Otherwise porosity goes to one very slowly (unless the reconstructed side is parallel to the edge. 
                           ! If slightly inclined the erosion rectangular does not cut the entire cell. 
                           ! This was solved by giving the angle of 45 degrees, i.e. using trapezoidal erosional areas) 
                           poros(n,m) = 1._fp 
                        endif
                     endif
                  else 
                     !joined polygons do not intersect land =>no ersosion
                     VOLeros(n,m) = 0._fp
                  endif
               ELSE 
                  !no polygons to be joined == no erosion
                  VOLeros(n,m) = 0._fp
               ENDIF
             else !if kcs==0 or 2
                VOLeros(n,m) = 0._fp
             endif
          enddo
       enddo
       !
    END SELECT
    !
    !   distribute eroded sediment on adjacent cells or suspended load  
    !
    !   exclude boundary points, they are never eroded and their values of dpL and dpH are prescribed in subroutine BC_VOF 
    !
    SELECT CASE(TYPEdistrBANKerod)
    CASE(1)
      ! 
      !just use the average adjacent dpL and lose the volume eroded
      !
       do m=2,mmax-1
          do n=2,nmaxus-1
             s11 = 0._fp
             dpLL = 0._fp
             contLL = 0
             if (comparereal(porosOLD(n,m),0._fp)==0.and.comparereal(poros(n,m),0._fp)>0) then
             !i compute dpL and s1 from average of adjacent
               do km = m-1,m+1  
                  do kn = n-1,n+1
                     if (kcs(kn,km)==1.and.(kn.ne.n.or.km.ne.m)) then !if kcs=2, at boundary periodical dpL and s1 are discontinuous!!
                        if (abs(kfs_cc(kn,km)).le.1.or.abs(kfs_cc(kn,km)).eq.3) then  !if there is no-bank part
                           dpLL = dpLL + dpL(kn,km)  
                           s11  = s11 + s1(kn,km)  
                           contLL = contLL +1
                        endif
                     endif
                  enddo
               enddo
               if (contLL.gt.0) then
                 !this is not strictly correct since it depends from the direction in which I proceed, since I am reusing new dpL on the 
                 !average
                 dpL(n,m) = dpLL/contLL 
                 s1(n,m)  = s11 /contLL 
               else
                 write(*,*) 'no adjacent wet, dpL cannot be defined, cell (m,n)',m,n
                 !pause 
                 call d3stop(1, gdp)
               endif

             endif
          enddo
       enddo    
    CASE(2)
       write(*,*) 'Make floodplains with a slope to make it axisymmetrical'
       !update dpL if bank was updated
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (updatedBANK(n,m)) then  
                dpL(n,m) = dpLnew(n,m)
             endif
          enddo
       enddo
       !update again dpL by distributing the eroded volume in the adjacent cells
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (updatedBANK(n,m)) then  
                AreaDistr = 0._fp
                s11 = 0._fp
                contLL = 0
                do km = m-1,m+1 
                   do kn = n-1,n+1
                      if (comparereal(poros(kn,km),0._fp)>0) then
                         AreaDistr = AreaDistr +  poros(kn,km)*gsqs(kn,km)       
                         s11  = s11 + s1(kn,km)     
                         contLL = contLL + 1  
                      endif
                   enddo
                enddo
                if (comparereal(AreaDistr,0._fp)==0) then
                   write(*,*) 'no adjacent wet, dbed cannot be defined, cell (m,n)',m,n
                   call d3stop(1, gdp)
                endif
                if (contLL.gt.0) then
                !this is not strictly correct since it depends from the direction in which I proceed, since I am reusing new s1. 
                ! But it is already approximated no big deal
                  s1(n,m)  = s11 /contLL   
                else
                  write(*,*) 'no adjacent wet, s11 cannot be defined, cell (m,n)',m,n
                  call d3stop(1, gdp)
                endif
                dbed = VOLeros(n,m)/AreaDistr
                do km = m-1,m+1   !3x3 stencil
                   do kn = n-1,n+1
                      if (comparereal(poros(kn,km),0._fp)>0) then
                         !- dbed is negative since dpL is positive downward
                         dpL(kn,km) = dpL(kn,km) - dbed     
                      endif
                   enddo
                enddo
             endif
          enddo
       enddo
    CASE(3)
       if (mod(nst,2)==100) write(*,*) 'Make floodplains with a slope to make it axisymmetrical'
       !
       ! Update dpL if bank was updated
       !
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (updatedBANK(n,m).and.MOVEbank) then  
                dpL(n,m) = dpLnew(n,m)
                r1(n,m,:,:) = Cnew(n,m,:,:) 
             endif
          enddo
       enddo
       !
       ! Compute AreaDistrib used for distributing the eroded volume into the adjacent cells
       !
       AreaDistrib(:,:) = 0._fp
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (updatedBANK(n,m)) then  
                AreaDistrib(n,m) = 0._fp
                contLL = 0
                do km = m-1,m+1  
                   do kn = n-1,n+1
                      if (comparereal(poros(kn,km),0._fp)>0) then
                         AreaDistrib(n,m) = AreaDistrib(n,m) +  poros(kn,km)*gsqs(kn,km)           
                         contLL = contLL + 1  
                      endif
                   enddo
                enddo
                if (contLL<1) then
                  write(*,*) 'no adjacent wet, AreaDistrib cannot be defined, cell (m,n)',m,n
                  call d3stop(1, gdp)
                endif               
             endif
          enddo
       enddo
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (updatedBANK(n,m).and.MOVEbank) then  
                s1(n,m) = s1new(n,m)                
             endif
          enddo
       enddo
       !
       !   add sediment to the bed in the 3x3 stencil and (ptionally) move water surface in order to satisfy mass conservation
       !
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (updatedBANK(n,m).and.(MOVEbank.or.(nst>itmor.and.DEPOSbankMATERIAL))) then  
                VOLUMEonTHEbed = fracBANKdepos*VOLeros(n,m)
                dbed = VOLUMEonTHEbed/AreaDistrib(n,m)
                AREAeros = VOLeros(n,m)/(dpLnew(n,m)-dpH(n,m))               
                recovVOLbed = dbed*AREAeros !LAYER recovered since of deposited sediment 
                ADDEDwater = VOLeros(n,m) - recovVOLbed - VOLerosEMER(n,m) !always >0, ADDED water laterally cause of bank removal
                if (ADDEDwater<0) then
                   !write(*,*) 'Error,  ADDEDwater is negative',ADDEDwater,n,m, VOLeros(n,m),recovVOLbed,VOLerosEMER(n,m),nst,stage
                   !call d3stop(1, gdp)
                endif
                WATERtoADD = VOLUMEonTHEbed - ADDEDwater ! if >0 it means that I have added more sediment vertically than water laterally, so I need to increase water level for mass conservation
                ds1 = WATERtoADD/AreaDistrib(n,m)   
                ds1=0             
                do km = m-1,m+1   !3x3 stencil
                   do kn = n-1,n+1
                      if (comparereal(poros(kn,km),0._fp)>0) then
                         dpL(kn,km) = dpL(kn,km) - dbed     !negative since dpL is positive downward
                         !dps(kn,km) = dps(kn,km) - dbed     if cut cell is fully floodded i have to change only dpL. And if not the same, since in checkdry dps becomes dpL
                         s1(kn,km) = s1(kn,km) + ds1   
                      endif
                   enddo
                enddo
             endif
          enddo
       enddo
      !
      ! NOTE: IN THIS WAY I CHANGE VATER VOLUMES IN THE 3X3 STANCEILS AND THEREFORE THE TOTAL MASS OF CONCENTRATION IS NOT CONSERVED
      !       IN GENERAL. BUT THIS IS THE SAME PROBLEM WHEN I CHANGE THE BED LEVEL AND I DO NOT CHANGE VOLUM0 AND VOLUM1        
      !       Therefore, when I update the banks, I do not update Volum1 and areau/areav either. This is consistent with what it 
      !       is done by standard Delft3D with moving-bed morphodynamics. Not sure they are mass conservative approaches, they maybe are 
      !       cause i solve trisol with the correct C0*volum0 (before bank erosion or bed erosion occurred) and the new volum1 that is correct (bank or bed is eroded)
      !
      ! For multiple grainsize, frac in the lower bed has to be provided for cut cells
      !
      !
      ! Compute source term for suspended concentration
      !
       sourseBANK(:,:,:) = 0._fp
      !
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (updatedBANK(n,m)) then 
                do l = 1, lsed
                    VOLUMEsource = (1._fp-fracBANKsuspWASH)*(1._fp-fracBANKdepos)*VOLeros(n,m)*frac(n,m,l)/MFloc !MFloc never zero. divided MF analogously to bed entreinment !USE FRACupper here! or better intgrate on the bank
                    DHsource = VOLUMEsource/AreaDistrib(n,m)
                    !dt is hdt here
                    DHdt_source(l) = DHsource/dt 
                enddo
                do km = m-1,m+1   
                   do kn = n-1,n+1
                      if (comparereal(poros(kn,km),0._fp)>0) then
                         sourseBANK(kn,km,1:lsed) = sourseBANK(kn,km,1:lsed) + DHdt_source(1:lsed)  
                      endif
                   enddo
                enddo                 
             endif
          enddo
       enddo
       
    CASE DEFAULT
       write(*,*) 'wrong value of TYPEdistrBANKerod'
       call d3stop(1, gdp)
    END SELECT
    !
    ! if cut cell is fully eroded,dpH has the wrong value since it has to be equal to dpL, I  compute dpH from dpL 
    !
    do m=2,mmax-1
       do n=2,nmaxus-1
          if (comparereal(poros(n,m),1._fp).eq.0) then 
             dpH(n,m) = dpL(n,m)  
          endif
       enddo
    enddo
    !
    ! If bed elevation raised above water surface, I set s1=dpL like in bott3D 
    !
    do m=2,mmax-1
       do n=2,nmaxus-1
          s1(n,m) = max(s1(n,m), -dpL(n,m))
          !s0(n,m) = max(s0(n,m), -dpL(n,m))
       enddo
    enddo
    !
    ! In case I use Parker and Young or other non-local reconstructions, set updateBANK to true in all the stencil areound the updated bank. 
    ! In this way a new normal is found and a new dry region is found again (alphaVOF is called again)
    !  
    SELECT CASE(TYPErecVOF)
    CASE(1) !Parker and Young
       do m=2,mmax-1
          do n=2,nmaxus-1
             updatedBANKloc(n,m) = .true.
             if (.not.updatedBANK(n,m)) then
                updatedBANKloc(n,m) = .false.
                if (por012(n,m) == 2) then
                   do_km : do km = max(m-1,2),min(m+1,mmax-1)   !3x3 stencil
                      do kn = max(n-1,2),min(n+1,nmaxus-1) 
                        !note updatedBANK its already true on the bounadries so it does not matter if I overwrite
                         if (km.ne.m.or.kn.ne.n) then 
                            if(updatedBANK(kn,km)) then
                               updatedBANKloc(n,m) = .true.
                               exit do_km
                            endif
                         endif
                      enddo
                   enddo do_km
                endif
             endif
          enddo
       enddo
       do m=2,mmax-1
          do n=2,nmaxus-1
             updatedBANK(n,m) = updatedBANKloc(n,m)
          enddo
       enddo
       !
    CASE DEFAULT
       WRITE(*,*) 'In updatebank.f90, updatedBANK for neighbours has still to be defined'
       call d3stop(1, gdp)
    END SELECT
    !
    ! Update porosity due to encroachment of vegetation
    !
    CALL POROSupdate(kcs,dps,s1,nub,mub,nlb,mlb,nmaxus,mmax,dt,MF, gdp)
    !
    ! I need to update kfs, since virtual merging below is done only using active cell (kfs==1) (and also virtual link)
    !
    ! NOTE: UPDATE OF KFS IS DONE ALSO IN KFS AND ONLY FOR  CELLS THAT NEED IT, SO THIS MIGHT BE REMOVED
    !
    do m=2,mmax-1
       do n=2,nmaxus-1
          if(kcs(n,m)==1) then
             if (comparereal(poros(n,m),0._fp)>0 .and. comparereal(s1(n,m),real(-dps(n,m),fp))>0 ) then
                kfs(n,m)=1
             else
                kfs(n,m)=0
             endif
          endif
       enddo
    enddo
    ! 
    ! Merge bed elevations (using porosity, since agsqs has not been updated yet!!! check for consistency with flooded floodplains)
    !
    ! IMPORTANT: KFS AT KCS==2 HAS ALSO TO BE UPDATED, TOGETHER WITH
    !
    ! Note: all the  merging has to be done after the quantities as aguu and agvv are updated! in fact if I erode a bit a fully bank
    !       cell, the first time it will find no cell to be merged with since aguu and agvv are all zero here!!!
    !
    !call checkDRY(gsqs,kfs,kfu,kfv,kcs,s1,u1,v1,dps,alfas,lunscr,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,dryflc,Zmodel)   
    !
    if (virtualMERGEupdBED.or.virtualMERGEupdDEPTH)  then
       !
       ! THE first time I need to average out the bed in the small cut and adjacent,in a way that they always have the same bed elevation
       ! since virtMERG change the value of dpL, it might be that checkDRY give different type of kfs_cc (and so also reconVOF
       ! would be different and so they are recomputed 
       !
       call COMPUTEmergingCARATT(kcs,kfs,poros,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,&
                                 virtualMERGEupdBED,typeVIRTmergeUPDbed,thresMERGE_zb,NMlistMERGED_bed,Nmerged_bed,&
                                 isMERGEDu_bed,isMERGEDv_bed,MERGEDwith_bed,1._fp,dim_nmlist,gdp) 
       !virtMERG wants gsqs*agsqs as actual argument
       call REDUCEgsqs(gsqs,poros,gsqsR,nmlb,nmub)
       !merge dps
       CALL virtMERG(dps,gsqsR,s1,dps,Rdummy1,icx,icy,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,&  !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                     Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,&                                   !0 do not check large bed variations
                     NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
       !merge dpL
       CALL virtMERG(dpL,gsqsR,s1,dps,Rdummy1,icx,icy,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,&  !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                     Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,&                                   !0 do not check large bed variations
                     NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
    endif
    !
    if (virtualMERGEupdDEPTH.or.virtualLINK) then
       
       if (virtualMERGEupdDEPTH) then
          facMERGElink_loc =  1._fp
       else !if(virtualLINK) then
          facMERGElink_loc = facMERGElink
       endif
       !
       CALL COMPUTEmergingCARATT(kcs,kfs,poros,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,& !note aguu and agvv and icx and icy are inverted at each stage. for merging of type 3 nothing change(i choose the maximum) but for other it might)
                         virtualMERGEupdDEPTH,typeVIRTmergeUPDdepth,thresMERGE_d,NMlistMERGED_d,Nmerged_d,&
                         isMERGEDu_d,isMERGEDv_d,MERGEDwith_d,facMERGElink_loc,dim_nmlist,gdp)   
       if (virtualLINK) then
           call VIRTUALlinkAREAS(kfs,agsqs_link,poros,gsqs,NMlistMERGED_d,Nmerged_d,thresMERGE_d,icx,icy,nmmax,nmlb,nmub,nst, dim_nmlist, gdp)
       endif
       !
       ! WE SHOULD ALSO SPREAD S1 C AND W IF virtualLINK
       !
    endif
    !
    ! Update volum1: (it will be volum0 in next time step). This is not mass conservative but it avoid the problem of having in difu:
    !                ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) * timesti = 0 in a fresh cell since volum0 is zero.
    !                This can trigger a zero concentration
    !                in fresh cells if there is a flux exiting from the cell in the solved ADI direction icx (and zero transversal fluxes)
    !                since the upwind implicit flux gives (since the upwind flux goes in bbkl as bbk(nmu, k) = bbk(nmu, k) - qxu*j1  ):
    !                bbkl*C_nm = ddkl(nm, k, l) (all the other terms are zero). My choice is consistent with hydrodynamic having water volume=/0 
    !                since s0>dps in a fresh cell. Not consistent with vertical variation of bed since there volum1 (that will be volum0) is not updated.
    !
    ! Update volum1 moved to trisol it is done in comvol
    !
    return
end subroutine UPDATEbank
