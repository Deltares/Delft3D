subroutine adi(dischy    ,solver    ,icreep    ,stage     ,nst       , &
             & nfltyp    ,lsecfl    ,betac     ,mmax      ,nmax      , &
             & nmaxus    ,xcor      ,ycor                            , &
             & zmodel    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
             & lstsci    ,nocol     ,norow     ,nsrc      ,dismmt    , &
             & irocol    ,mnksrc    ,kfu       ,kfv       ,kfs       , &
             & kcu       ,kcv       ,kcs       ,kfumin    ,kfumax    , &
             & kfvmin    ,kfvmax    ,kspu      ,kspv      ,kadu      , &
             & kadv      ,porosu    ,porosv    ,areau     ,areav     , &
             & volum1    ,s0        ,s1        ,u0        ,u1        , &
             & v0        ,v1        ,w1        ,hu        ,hv        , &
             & umean     ,vmean     ,qxk       ,qyk       ,qzk       , &
             & circ2d    ,circ3d    ,dps       ,dpu       ,dpv       , &
             & evap      ,hkru      ,hkrv      ,dteu      ,dtev      , &
             & disch     ,umdis     ,vmdis     ,sig       ,thick     , &
             & guu       ,guv       ,gvv       ,gvu       ,guz       , &
             & gvz       ,gud       ,gvd       ,gsqs      ,gsqiu     , &
             & gsqiv     ,taubpu    ,taubpv    ,taubsu    ,taubsv    , &
             & rho       ,sumrho    ,dddksi    ,dddeta    ,dzdksi    , &
             & dzdeta    ,wsu       ,wsv       ,hu0       ,hv0       , &
             & fxw       ,fyw       ,crbc      ,dfu       ,dfv       , &
             & deltau    ,deltav    ,tp        ,rlabda    ,dzu1      , &
             & dzv1      ,vicuv     ,vnu2d     ,vicww     ,rxx       , &
             & rxy       ,ryy       ,cfurou    ,cfvrou    , &
             & r0        ,diapl     ,rnpl      ,wsbodyu   ,wsbodyv   , &
             & windsu    ,windsv    ,patm      ,fcorio    ,dpdksi    , &
             & dpdeta    ,ubrlsu    ,ubrlsv    ,uwtypu    ,uwtypv    , &
             & pship     ,tgfsep    ,soumud    ,excbed    ,wrka1     , &
             & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
             & wrka7     ,wrka8     ,wrka9     ,wrka15    ,wrka16    , &
             & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
             & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
             & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,wrkb15    , &
             & wrkb16    ,sbkol     ,dis_nf    ,precip    ,            &
             & dzs1      ,dp        ,alfas     ,timnow    ,gdp          )
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
!  $Id: adi.f90 6388 2016-08-01 01:05:36Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/adi.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: ADI performs one time step of the Alternating
!              Direction Implicit (ADI) method
! Method used: A.D.I. method is used.
!              Upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp) , pointer :: dt
    real(fp), dimension(:,:,:) , pointer :: rttfu
    real(fp), dimension(:,:,:) , pointer :: rttfv
    real(fp), dimension(:,:)   , pointer :: ustokes
    real(fp), dimension(:,:)   , pointer :: vstokes
    real(fp)                   , pointer :: drycrt
    real(fp)                   , pointer :: hdt
    integer                    , pointer :: lunscr
    integer                    , pointer :: lundia
    integer                    , pointer :: irov
    integer                    , pointer :: itstrt
    integer                    , pointer :: nmlb
    integer                    , pointer :: nmub
    integer                    , pointer :: mlb
    integer                    , pointer :: mub
    integer                    , pointer :: nlb
    integer                    , pointer :: nub
    include 'flow_steps_f.inc'
    integer                       , pointer :: cutcell
    integer                       , pointer :: ghostmethod
    integer                       , pointer :: totGHOSTv1
    integer, dimension(:)         , pointer :: mGPv1
    integer, dimension(:)         , pointer :: nGPv1
    integer                       , pointer :: totGHOSTu1
    integer, dimension(:)         , pointer :: mGPu1
    integer, dimension(:)         , pointer :: nGPu1
    integer, dimension(:,:)       , pointer :: kFLcut
    integer                       , pointer :: idebugCUThardINI
    integer                       , pointer :: idebugCUThardFIN
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp), dimension(:,:,:)    , pointer :: qxk_tinyCUT
    real(fp), dimension(:,:,:)    , pointer :: qyk_tinyCUT
    real(fp), dimension(:,:)      , pointer :: agsqs
    logical                       , pointer :: QUARTERdt
    integer, dimension(:,:)       , pointer :: GHOSTs1
    integer, dimension(:,:)       , pointer :: GHOSTu1
    integer, dimension(:,:)       , pointer :: GHOSTv1
    integer                       , pointer :: doNOTdebugGHOSTS
    logical                       , pointer :: printINTERMghost
    integer                       , pointer :: iprintINTERMghost01
    real(fp), dimension(:,:)      , pointer :: dpH
    real(fp), dimension(:,:)      , pointer :: dpL
    logical                       , pointer :: periodSURFACE
    logical                       , pointer :: TRANSVperIMPL
    integer                       , pointer :: PERIODalongM
    logical                       , pointer :: callSUBR_WATERlevelPERIOD
    real(fp), dimension(:,:)      , pointer :: xG_L
    real(fp), dimension(:,:)      , pointer :: yG_L
    integer                       , pointer :: free_S1_sud
    logical                       , pointer :: constSOLUTION
    integer, dimension(:,:)       , pointer :: kfs_cc
    real(fp), dimension(:,:)      , pointer :: poros
    integer, dimension(:,:)       , pointer :: kWDu
    integer, dimension(:,:)       , pointer :: kWDv
    logical                       , pointer :: testGHOSTaccur
    logical                       , pointer :: changeKFUVcut
    logical                       , pointer :: deactGHOST_smallcut
    logical                       , pointer :: dontRESETghost
    logical                       , pointer :: resetV1toV0
    real(fp), dimension(:,:)      , pointer :: ETAcorV1
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: PSIcorU1
    real(fp), dimension(:,:)      , pointer :: etaG_U1
    real(fp), dimension(:,:)      , pointer :: psiG_V1
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    real(fp), dimension(:,:)      , pointer :: xCORV1
    real(fp), dimension(:,:)      , pointer :: yCORV1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:,:)      , pointer :: xCORU1
    real(fp), dimension(:,:)      , pointer :: yCORU1
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    logical                       , pointer :: onlyUZD
    logical                       , pointer :: noUZD
    real(fp), dimension(:,:)      , pointer :: u0INTv
    real(fp), dimension(:,:)      , pointer :: u1INTv
    real(fp), dimension(:,:)      , pointer :: v0INTu
    real(fp), dimension(:,:)      , pointer :: v1INTu
    real(fp), dimension(:,:)      , pointer :: vel00
!
! Global variables
!
    integer                                                  :: icreep  !  Description and declaration in tricom.igs
    integer                                                  :: j       !!  Begin pointer for arrays which have
                                                                        !!  been transformed into 1D arrays.
                                                                        !!  Due to the shift in the 2nd (M-)
                                                                        !!  index, J = -2*NMAX + 1
    integer                                                  :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: lsecfl  !  Description and declaration in dimens.igs
    integer                                                  :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nfltyp  !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nmmax   !  Description and declaration in dimens.igs
    integer                                                  :: nmaxus
    integer                                                  :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                                  :: nocol   !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nst     !!  Time step number
    integer    , dimension(7, norow + nocol)                 :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(7, nsrc)                          :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfumax  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfumin  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfvmax  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)            :: kfvmin  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: kspv    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: kadv    !  Description and declaration in esm_alloc_int.f90
    logical                                                  :: sbkol   !  Description and declaration in procs.igs
    logical                                                  :: zmodel  !  Description and declaration in procs.igs
    real(fp)                                                 :: betac   !  Description and declaration in tricom.igs
    real(fp), dimension(12, norow + nocol)                   :: crbc    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow + nocol)                    :: circ2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: ycor
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dddeta  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dddksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: deltav  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dfv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dp     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)             :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dpv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dteu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dtev    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dzdeta  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: dzdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: evap    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: excbed  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: fyw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gsqiv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: guz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hkru    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hkrv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hu0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: hv0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: precip  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: soumud  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubpv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: taubsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: tgfsep  !!  Water elev. induced by tide gen.force
                                                                        !!  Internal work array WRKB17 used
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: uwtypu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: uwtypv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: vmean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: windsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka15  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka16  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka2   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka3   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka4   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka5   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka6   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka7   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka8   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrka9   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: wsbodyv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               :: alfas
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: qzk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)            :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)            :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)     :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: areau   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: areav   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dpdeta  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dpdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dzu1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dzv1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: porosu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: porosv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rho     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: ryy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: sumrho  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: ubrlsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: v0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: volum1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb10  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb11  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb12  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb13  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb14  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb15  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb16  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb2   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb3   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb4   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb5   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb6   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb7   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb8   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: wrkb9   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: dis_nf  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, 2, norow + nocol)              :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: umdis   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: vmdis   !  Description and declaration in esm_alloc_real.f90
    real(fp)                                    ,intent(in)  :: timnow 
    character(1), dimension(nsrc)                            :: dismmt  !  Description and declaration in esm_alloc_char.f90
    character(8)                                             :: dischy  !  Description and declaration in tricom.igs
    character(8)                                             :: solver  !  Description and declaration in tricom.igs
    character(8)                               , intent(in)  :: stage   !!  First or Second half time step
!
! Local variables
!
    integer , save :: contIDRY
    integer :: nm
    integer :: KFVALUE
    integer :: k,n,m
    integer :: iter,exitloop
    integer :: icx
    integer :: icy
    integer :: idry
    integer :: nhystp
    integer :: nmaxddb
    logical :: flood   ! Flag for activating flooding part of checku subroutine
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0INTv
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u1INTv
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v0INTu
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1INTu
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: vel00
    integer , dimension(0)                                          :: dummy0  !dummy 0-dimensional array
!
!! executable statements -------------------------------------------------------
!
    cutcell                   => gdp%gdimbound%cutcell
    ghostmethod               => gdp%gdimbound%ghostmethod
    totGHOSTv1                => gdp%gdimbound%totGHOSTv1
    mGPv1                     => gdp%gdimbound%mGPv1
    nGPv1                     => gdp%gdimbound%nGPv1
    totGHOSTu1                => gdp%gdimbound%totGHOSTu1
    mGPu1                     => gdp%gdimbound%mGPu1
    nGPu1                     => gdp%gdimbound%nGPu1
    kFLcut                    => gdp%gdimbound%kFLcut
    idebugCUThardINI          => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN          => gdp%gdimbound%idebugCUThardFIN
    aguu                      => gdp%gdimbound%aguu
    agvv                      => gdp%gdimbound%agvv
    qxk_tinyCUT               => gdp%gdimbound%qxk_tinyCUT
    qyk_tinyCUT               => gdp%gdimbound%qyk_tinyCUT
    agsqs                     => gdp%gdimbound%agsqs
    QUARTERdt                 => gdp%gdimbound%QUARTERdt
    GHOSTs1                   => gdp%gdimbound%GHOSTs1
    GHOSTu1                   => gdp%gdimbound%GHOSTu1
    GHOSTv1                   => gdp%gdimbound%GHOSTv1
    doNOTdebugGHOSTS          => gdp%gdimbound%doNOTdebugGHOSTS
    printINTERMghost          => gdp%gdimbound%printINTERMghost
    iprintINTERMghost01       => gdp%gdimbound%iprintINTERMghost01
    dpH                       => gdp%gdimbound%dpH
    dpL                       => gdp%gdimbound%dpL
    periodSURFACE             => gdp%gdimbound%periodSURFACE
    TRANSVperIMPL             => gdp%gdimbound%TRANSVperIMPL
    PERIODalongM              => gdp%gdimbound%PERIODalongM
    callSUBR_WATERlevelPERIOD => gdp%gdimbound%callSUBR_WATERlevelPERIOD
    xG_L                      => gdp%gdimbound%xG_L
    yG_L                      => gdp%gdimbound%yG_L
    free_S1_sud               => gdp%gdimbound%free_S1_sud
    constSOLUTION             => gdp%gdimbound%constSOLUTION
    kfs_cc                    => gdp%gdimbound%kfs_cc
    poros                     => gdp%gdimbound%poros
    kWDu                      => gdp%gdimbound%kWDu
    kWDv                      => gdp%gdimbound%kWDv
    testGHOSTaccur            => gdp%gdimbound%testGHOSTaccur
    changeKFUVcut             => gdp%gdimbound%changeKFUVcut
    deactGHOST_smallcut       => gdp%gdimbound%deactGHOST_smallcut
    deactGHOST_smallcut       => gdp%gdimbound%deactGHOST_smallcut
    dontRESETghost            => gdp%gdimbound%dontRESETghost
    resetV1toV0               => gdp%gdimbound%resetV1toV0
    ETAcorV1                  => gdp%gdimbound%ETAcorV1
    EDGExyBANK                => gdp%gdimbound%EDGExyBANK
    PSIcorU1                  => gdp%gdimbound%PSIcorU1
    etaG_U1                   => gdp%gdimbound%etaG_U1
    psiG_V1                   => gdp%gdimbound%psiG_V1
    PSIx                      => gdp%gdimbound%PSIx
    PSIy                      => gdp%gdimbound%PSIy
    ETAx                      => gdp%gdimbound%ETAx
    ETAy                      => gdp%gdimbound%ETAy
    xCORV1                    => gdp%gdimbound%xCORV1
    yCORV1                    => gdp%gdimbound%yCORV1
    xG_U1                     => gdp%gdimbound%xG_U1
    yG_U1                     => gdp%gdimbound%yG_U1
    xCORU1                    => gdp%gdimbound%xCORU1
    yCORU1                    => gdp%gdimbound%yCORU1
    xG_V1                     => gdp%gdimbound%xG_V1
    yG_V1                     => gdp%gdimbound%yG_V1
    onlyUZD                   => gdp%gdimbound%onlyUZD
    noUZD                     => gdp%gdimbound%noUZD
    u0INTv                    => gdp%gdimbound%Dwrkak1
    u1INTv                    => gdp%gdimbound%Dwrkak2
    v0INTu                    => gdp%gdimbound%Dwrkak3
    v1INTu                    => gdp%gdimbound%Dwrkak4
    vel00                     => gdp%gdimbound%Dwrkak5
    rttfu         => gdp%gdtrachy%rttfu
    rttfv         => gdp%gdtrachy%rttfv
    ustokes       => gdp%gdtrisol%ustokes
    vstokes       => gdp%gdtrisol%vstokes
    dt            => gdp%gdexttim%dt
    lunscr        => gdp%gdinout%lunscr
    irov          => gdp%gdphysco%irov
    itstrt        => gdp%gdinttim%itstrt
    lundia        => gdp%gdinout%lundia
    nmlb          => gdp%d%nmlb  
    nmub          => gdp%d%nmub
    mlb           => gdp%d%mlb  
    nlb           => gdp%d%nlb  
    mub           => gdp%d%mub  
    nub           => gdp%d%nub  
    drycrt        => gdp%gdnumeco%drycrt
    hdt           => gdp%gdnumeco%hdt
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    !
    ! 
    if (constSOLUTION) then
       CALL CHECKdry(gsqs,kfs,kfu,kfv,kcs,s1,u1,v1,dps,alfas,lunscr,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,drycrt,Zmodel,gdp) !only to have correct kfu and kfv  since cells can be both partially wet but they can have a common dry edge
       do nm = 1, nmmax
          call nm_to_n_and_m(nm, n, m, gdp)
          do k = 1, kmax
             qxk(nm, k) = aguu(n,m)*guu(nm)*hu(nm)*thick(k)*u1(nm, k)
             qyk(nm, k) = agvv(n,m)*gvv(nm)*hv(nm)*thick(k)*v1(nm, k)
          enddo
       enddo
       !comment this part if you dont want to change
       qzk = 0.0
       w1  = 0.0
       !
       icx   = 1
       icy   = nmaxddb
       do k = 1, kmax
          do nm = 1, nmmax
             if (kcs(nm)==1) then
                call nm_to_n_and_m(nm, n, m, gdp)
                if (comparereal(agsqs(n,m),0._fp)>0) then
                    w1(nm, k) = w1(nm, k - 1) + (qxk(nm, k) - qxk(nm - icx, k)  +  qyk(nm, k) - qyk(nm - icy, k)   ) / (agsqs(n,m)*gsqs(nm))
                endif
                qzk(nm, k) = w1(nm, k)*gsqs(nm)*agsqs(n,m) 
             endif
          enddo
       enddo
       return
    endif
    
    do nm=1,nmmax  
       if (stage=='stage1') then
          ! write(10203040,'(i8,i8,15F25.15)')nst,nm,s0(nm),s1(nm),u0(nm,1),u1(nm,1),v0(nm,1),v1(nm,1),hu(nm),hv(nm),dpu(nm),dpv(nm),qxk(nm,1),qyk(nm,1)
       else
          ! write(10203041,'(i8,i8,15F25.15)')nst,nm,s0(nm),s1(nm),u0(nm,1),u1(nm,1),v0(nm,1),v1(nm,1),hu(nm),hv(nm),dpu(nm),dpv(nm),qxk(nm,1),qyk(nm,1)
       endif
    enddo
    !
    ! =====================================
    ! COMPUTATION OF STAGE 1 FOR ADI METHOD
    ! Computation of V1, i.e. evaluate momentum equation for one half timestep
    ! =====================================
    !
    if (stage=='stage1') then
       !
       ! Calculate HV and set KFV = 0 for HV < HTRSH (.5*DRYFLC)
       ! hv is already calculated in SUD for the wet points (kfv=1)
       ! Calling checku is only necessary because the hv calculation
       ! is performed on the full computational domain (kcv=1) instead of
       ! only the wet points (kfv=1)
       !
       if (resetV1toV0)vel00 = v0
       contIDRY = 0
       idry  = 0
       flood = .false.
       icx   = 1
       icy   = nmaxddb
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891980,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
!
       if (periodSURFACE) then  ! prescribe periodic velocity components. I do it here so the ghost stencil already has it. And checku needs the periodic water level to compute the correct hv
          if (PERIODalongM==1) then 
             call velocityPERIOD(u0,v0,icx,nlb,nub,mlb,mub,kmax, gdp) ! the second argument has to be the tangential velocity
          else
             call velocityPERIOD(v0,u0,icx,nlb,nub,mlb,mub,kmax, gdp) ! the second argument has to be the tangential velocity
          endif
          if (callSUBR_WATERlevelPERIOD)  call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax, gdp) !it should not be needed, after sud its already periodic
       endif
!
       call timer_start(timer_checku, gdp)
       call checku(hv        ,s0        ,dpv       ,vmean     , &  ! check velocity at V-point
                 & kfv       ,kcs       ,kcv       , &
                 & kspv      ,hkrv      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & agvv      ,gdp       )
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891981,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       call timer_stop(timer_checku, gdp)
!
!      cutcell modification
       if (cutcell.gt.0) then
          if (GhostMethod.eq.1.or.GhostMethod.eq.2) then
             !compute u0 and v0 at ghost points
             call cutcell_pre_uzd_stage1(icx        ,icy        ,u0         ,v0         ,u1         ,&
                                       & u0INTv     ,v0INTu     ,guu        ,gvv        ,xcor       ,&
                                       & ycor       ,&
                                       & v1         ,gsqs       ,kcs        ,dpu        ,dpv        ,&
                                       & Umean      ,Vmean      ,thick      ,qxk        ,qyk        ,&
                                       & hu         ,hv         ,s0         ,s1         ,dps        ,&
                                       & kfs        ,kfu        ,kfv        ,kcu        ,kcv        ,&
                                       & mmax       ,nmax       ,nmmax      ,nmaxus     ,kmax       ,&
                                       & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                                       & nmlb       ,nmub       ,gdp%d%ddbound,lunscr   ,Irov       ,gdp)
              if(testGHOSTaccur) call PRINTtestGHOSTaccur(u0,v0,nlb,nub,mlb,mub,kmax, gdp)
          endif
          if (deactGHOST_smallcut) then !deactivate small partially cut edges before uzd to turn off update there
             call kfuv_ghost_smallcut (kfu,kfv,nlb,nub,mlb,mub,0, gdp)
          endif
       endif

       IF (printINTERMghost) then
          !THERE IS NO POINT TO PRINT s1,u1,v1 they are printed in quarterdt
      
         ! CALL postpr_ghost(nst+1 , s0 , v0INTu , u0INTv , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
              CALL postpr_ghost(nst+1 , s0 , u0 , v0 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &               
                           & kmax,nlb,nub,mlb,mub,gdp)
       ENDIF
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891982,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       call timer_start(timer_uzd, gdp)
       gdp%dd%uzditer = 0
       icx            = 1
       icy            = nmaxddb
       call timer_start(timer_1stuzd, gdp)
      ! if (.not.testGHOSTaccur) then
       if (.NOT.noUZD) then      
       call uzd(icreep    ,dpdeta    ,s0        ,v0        , &
              & v1        ,u0        ,v0INTu    ,u0INTv    ,w1        ,vmean     , &
              & hv        ,hu        ,gvv       ,guu       ,guv       ,gsqs      , &
              & gud       ,gvd       ,guz       ,gsqiv     ,qyk       ,qxk       , &
              & disch     ,vmdis     ,dismmt    ,mnksrc    ,kcv       , &
              & kcs       ,kfv       ,kfu       ,kfs       , &
              & kspv      ,kadv      ,kadu      ,nocol     ,icx       ,icy       , &
              & irocol(1, norow + 1) ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & nmax      ,mmax      ,nmaxus    ,&
              & nsrc      ,lsecfl    ,lstsci    ,betac     ,nst       , &
              & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
              & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
              & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,wrkb15    , &
              & wrkb16    ,taubpv    ,taubsv    ,rho       ,sumrho    , &
              & thick     ,sig       ,dps       ,wsv       ,fyw       ,wsbodyv   , &
              & vicuv     ,vnu2d     ,vicww     ,ryy       ,rxy       , &
              & dfv       ,deltav    ,tp        ,rlabda    , &
              & diapl     ,rnpl      ,ghostV1   ,ghostU1   , &
              & cfvrou    ,cfurou    ,rttfv     ,r0        ,windsv    , &
              & patm      ,fcorio    ,ubrlsv    ,hkrv      , &
              & pship     ,tgfsep    ,dtev      ,vstokes   ,.false.   , &
              & yG_L      ,xG_L      ,kWDv      ,kWDu      ,agvv      , &
              & xcor      ,ycor      ,gdp       )
       else
         V1 = V0
       endif
              if(testGHOSTaccur) call PRINTtestGHOSTaccur(u0,v1,nlb,nub,mlb,mub,kmax, gdp)
      ! endif
     !    do k =1,nmmax
      ! write(9111111,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v1(k,1)
      !    enddo
       call timer_stop(timer_1stuzd, gdp)
       call timer_stop(timer_uzd, gdp)
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891983,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif

       if (QUARTERdt) then        
         ! call postpr_hdt(s0,nst, gdp)         
         !in this way i can print u0 (ghost point), otherwhise i print u1 that is not defined yet    
          CALL postpr_ghost(nst+1 , s0 , u0 , v1 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &               
               & kmax,nlb,nub,mlb,mub,gdp)         
       endif
       !this now is only for checku. double check if its still needed
       if (cutcell.gt.0) then
          if(GhostMethod.eq.1.or.GhostMethod.eq.2) then
             if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,0,nlb,nub,mlb,mub,nmlb,nmub, gdp) !set kfs,kfu,kfv NOT active in ghost points. Only s0,v1,v0 and u0 should be reset
          endif
          if (deactGHOST_smallcut) then !activate small partially cut edges for sud
             call kfuv_ghost_smallcut (kfu,kfv,nlb,nub,mlb,mub,1, gdp)
          endif
       endif
       
       !
       !     computation proceeds in X direction
       !
       ! CHECK FOR FLOODING AND DRYING IN "U" POINTS
       !
       flood = .true.
       icx   = nmaxddb
       icy   = 1
       call timer_start(timer_checku, gdp)
        !I guess checku is checking the previous half time step, since uzd just updated the along y velocity. 
       call checku(hu        ,s0        ,dpu       ,umean     , &
                 & kfu       ,kcs       ,kcu       , &
                 & kspu      ,hkru      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & aguu      ,gdp       )
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891984,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       call timer_stop(timer_checku, gdp)
!       
       if (periodSURFACE) then  ! prescribe periodic velocity components.
          ! It is actually not necessary to recompute v1 if implicit periodic, since it should have been converged from uzd up to given tolerance.
          ! u1 could just be copied from u0 since nothing changed but its not much cheaper so I just call the subroutine
          if (.not.TRANSVperIMPL.or.testGHOSTaccur) then
             if (PERIODalongM/=1) then 
                call velocityPERIOD(v1,u1,icx,nlb,nub,mlb,mub,kmax, gdp)    
             else
                call velocityPERIOD(u1,v1,icx,nlb,nub,mlb,mub,kmax, gdp)   
             endif
          endif
          !if (callSUBR_WATERlevelPERIOD)  call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax) not needed nothing changed
       endif
       !
       if (.not.onlyUZD) then  
          !
          ! Skip sud and pre-sud stuff 
          ! Cutcell modification
          !
          CALL cutcell_pre_sud_stage1(icx        ,icy        ,u0         ,v0         ,u1         ,&
                                    & u0INTv     ,v0INTu     ,v1INTu                             ,&
                                    & v1         ,gsqs       ,kcs        ,dpu        ,dpv        ,&
                                    & Umean      ,Vmean      ,thick      ,qxk        ,qyk        ,&
                                    & hu         ,hv         ,s0         ,s1         ,dps        ,&
                                    & guu        ,gvv        ,xcor       ,ycor                   ,&       
                                    & kfs        ,kfu        ,kfv        ,kcu        ,kcv        ,&
                                    & mmax       ,nmax       ,nmmax      ,nmaxus     ,kmax       ,&
                                    & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                                    & nmlb       ,nmub       ,gdp%d%ddbound,lunscr   ,Irov       ,gdp)
          if(testGHOSTaccur) call PRINTtestGHOSTaccur(u1,v1,nlb,nub,mlb,mub,kmax, gdp)
          if (cutcell.gt.0.and.GhostMethod.le.1) then
             IF (printINTERMghost) then
                IF (iprintINTERMghost01 ==0) THEN
                   CALL postpr_ghost(nst+1 , s0 , u0 , v0 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                                   & kmax,nlb,nub,mlb,mub,gdp)
                ELSEIF (iprintINTERMghost01 ==1) THEN
                   CALL postpr_ghost(nst+1 , s1 , u1 , v1 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                                   & kmax,nlb,nub,mlb,mub,gdp)
                ELSE
                   CALL postpr_ghost(nst+1 , s0 , u0 , v0 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                                   & kmax,nlb,nub,mlb,mub,gdp)
                   CALL postpr_ghost(nst+1 , s1 , u1 , v1 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                                   & kmax,nlb,nub,mlb,mub,gdp)
                ENDIF
             ENDIF
          endif
!         
          if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
             do k =1,nmmax
               call nm_to_n_and_m(k, n, m, gdp)
               if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
               write(9891985,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
             enddo
          endif
11111     continue
          !in case idry==1 reset kfsuv_ghost recompute qyk (v0 it is not passed to sud therefore for not changing the arguments of sud I call it here)
          if (idry==1.and.cutcell.gt.0.and.GhostMethod.le.1) then           
            !Note: I use the explicit qyk. However, if idry=1 it means that in drychk all the 4 qyk,qxk in the 4 edges of a dry cell are set to zero so here   I  have to recompute qyk  
            !MODIFICARE PER EXACT FREE!!
          !  do k=1,kmax             
          !     CALL interpG_ATv1LOCATION(v0(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,0._fp) ! this is needed only   cause  in kfsuv_ghost i set v0 to zero
          !  enddo  
            !CALL interpG_ATv1LOCATION(hv0,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub)
            !call qxORqy_ghosts(v0,hv0,gvv,agvv,thick,porosv,qyk,mmax,nmax,nmaxus,kmax,nst,totGHOSTv1,mGPv1,nGPv1,nlb,nub,mlb,mub,nmlb,nmub,icx) !explicit,    from hv0 and v0 
            if (changeKFUVcut) call kfsuv_ghost   (Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,1,0,nlb,nub,mlb,mub,nmlb,nmub,    gdp) !set kfs,kfu,kfv active in ghost points              
          endif
          icx            = nmaxddb
          icy            = 1
          if (sbkol) then
             !
             ! Correction for open discharge boundaries in explicit direction
             !
             call bccor(j         ,nmmaxj    ,kmax      ,nocol     ,icy       , &
                      & icx       ,zmodel    ,irocol(1, norow + 1) , &
                      & kcs       ,kfv       ,qyk       ,  &
                      & thick     ,circ2d(1, norow + 1) ,gdp       )
          endif
          !
          ! Computation of U1 and S1, i.e. evaluation of coupled momentum and
          ! continuity equation for one half time step
          !
          gdp%dd%suditer = 0
          call timer_start(timer_sud, gdp)
          call timer_start(timer_1stsud, gdp)
          !if (.not.testGHOSTaccur) then  
          call sud(dischy    ,nst       ,icreep    ,betac     ,mmax      , &
                 & nmaxus    ,kFLcut, &
                 & nmax      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                 & lstsci    ,nsrc      ,lsecfl    ,norow     ,icx       , &
                 & icy       ,dismmt    ,irocol(1, 1)         ,mnksrc    , &
                 & kfu       ,kfv       ,kfs       ,kcs       ,kspu      , &
                 & kadu      ,kadv      ,kcu       ,kfumin    ,kfumax    , &
                 & porosu    ,s0        ,s1        ,u0        ,u1        , &
                 & v1INTu    ,xcor      ,ycor                            , &
                 & v1        ,w1        ,r0        ,qxk       ,qyk       , &
                 & qzk       ,guu       ,gvv       ,gvu       ,gsqs      , &
                 & gud       ,gvd       ,gvz       ,gsqiu     ,dteu      , &
                 & circ2d(1,1),circ3d(1, 1, 1),disch,porosv           , &
                 & umdis     ,umean     ,hu        ,hv        ,dpu       ,dzu1      , &
                 & dpdksi    ,thick     ,sig       ,dps       ,taubpu    , &
                 & taubsu    ,rho       ,sumrho    ,wsu       ,fxw       , &
                 & wsbodyu   ,idry      ,crbc(1,1) ,vicuv     ,wrka9     , &
                 & vnu2d     ,vicww     ,rxx       ,rxy       ,dfu       , &
                 & deltau    ,tp        ,rlabda    ,cfurou    ,cfvrou    , &
                 & rttfu     ,diapl     ,rnpl      , &
                 & windsu    ,patm      ,fcorio    ,evap      ,ubrlsu    , &
                 & uwtypu    ,hkru      ,pship     ,tgfsep    ,wrka1     , &
                 & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
                 & wrka7     ,wrka8     ,wrka15    ,wrkb1     ,wrkb2     , &
                 & wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     ,wrkb7     , &
                 & wrkb8     ,wrkb15    ,wrkb16    ,soumud    ,dis_nf    , &
                 & precip    ,ustokes   ,aguu      ,agvv      ,agsqs     , &
                 & qxk_tinyCUT, GHOSTu1 ,GHOSTv1   ,xG_L      ,yG_L      , &
                 & kWDu      ,kWDv      ,timnow    , &
                 & gdp )
          !endif
          call timer_stop(timer_1stsud, gdp)
          call timer_stop(timer_sud, gdp)
          if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
             do k =1,nmmax
               call nm_to_n_and_m(k, n, m, gdp)
               if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
               write(9891986,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
             enddo
          endif
          !
          ! Check for drying in waterlevel points in the X-direction
          !
          icx = nmaxddb
          icy = 1
          call timer_start(timer_drychk, gdp)
          !if (cutcell==2) call drychk_cc(kfs_cc,   poros    ,agsqs   ,s1     ,dps    ,dpL   ,dpH    ,nmmax   ,nmlb, nmub,zmodel)
          if (.not.testGHOSTaccur) then
          call drychk(idry      ,s1        ,qxk       ,qyk       ,icx       , &
                    & icy       ,dps       ,kfu       ,kfv       ,kfs       , &
                    & j         ,nmmaxj    ,nmmax     ,kmax      ,nfltyp    , &
                    & excbed    ,kcs       ,nst       ,gdp       )
          endif
          !if (changeKFUVcut) then
          !   call kfsuv_ghost(Umean, Vmean, qxk, qyk, hu, hv, dpu, dpv, gsqs, kfs, kfu, kfv, kcs, kcu, kcv, &
          !                    s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,1,0,nlb,nub,mlb,mub,nmlb,nmub)
          !endif
          if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
             do k =1,nmmax
               call nm_to_n_and_m(k, n, m, gdp)
               if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
               write(9891987,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
             enddo
          endif
          call timer_stop(timer_drychk, gdp)
          !
          ! DD code added:
          !
          !
          ! Synchronize on Dry Point
          !
          if (nfltyp==0) then
             nhystp = nxtdry(d3dflow_check_adi_dry, 0, gdp)
          else
             nhystp = nxtdry(d3dflow_check_adi_dry, idry, gdp)
          endif
          !this part has to be improved
          if (cutcell.gt.0.and.GhostMethod.le.1) then
          
             if ((nfltyp/=0).and.(nhystp==d3dflow_build_adi_zeta .or. (nhystp==noneighbors .and. idry==1))) then
                continue !sud is repeated, what should I do?
             else
             endif
          endif
          if (nfltyp/=0) then
             !
             ! If waterlevel is below bottom then isolate waterlevel point by setting
             ! the surrounding velocities to zero and repeat the computation of SUD
             !
             if (nhystp==d3dflow_build_adi_zeta .or.                               &
               & (nhystp==noneighbors .and. idry==1)) then
               contIDRY = contIDRY +1
               if (contIDRY > 50) then
                  write(*,*) 'Deadlock in sud because of drying'
                  call d3stop(1, gdp)
               endif
               goto 11111
          
             endif
          !
          ! DD code added end
          !
          endif
          !
       endif    !    END OF if (.not.onlyUZD) then  !skip sud and pre-sud stuff 
!
!      set ghost velocity points with active edge=0 to zero vel and kf=1. Reset dps and s1., dpu,dpv, qxk,Umean, qyk,Vmean
       if (cutcell.eq.2.and.ghostmethod.ne.3) then
          !da verificare se s0 cambia in sud (l unico modo è per il wettign e drying penso) e se non cambia eliminare questa chiamata a s0iss1_ghost
         ! IF (free_S1_sud.EQ.1) THEN
         !    CALL s0iss1_ghost(s0,s00,nlb,nub,mlb,mub,kmax,0) !basically s0 = s00 ! in order to have the mass check correct in updmassbal for fully emerged ghost cells (where s0 is a ghost point and comparereal(poros(nGP,mGP),0._fp))
         ! ENDIF
          !S00 SHOULD BE passed to kfsuv_ghost_cutEDGES and so only s0 in the fully emerged ghost cells  ghost cell is reset
          !IF I REMOVED I HAVE WEIRD RESULTS! CHECK!
          KFvalue = 1
          IF (dontRESETghost) KFvalue = 0
          if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,KFvalue,nlb,nub,mlb,mub,nmlb,nmub, gdp) 
          if (resetV1toV0) call resetV1toV0subr(vel00,v1,agvv,mGPv1,nGPv1,totGHOSTv1,kmax,nlb,nub,mlb,mub) !to try well banancing
       endif
!
       !
       ! Compute Volume and Areas to be used in routines that computes 
       ! the transport of matter (consistency with WAQ)
       ! Use dummy arrays to represent the unused Z-model arrays
       ! Moved here for compatibility with cutcell approach (kfs has to be reset on fully emerged ghost cells since aqsgs=1 there)
       !
       call timer_start(timer_comvol, gdp)
       call comvol(nmmax     ,kmax      ,zmodel    ,kcs       ,kcu       , &
                 & thick     ,guu       ,gsqs      ,dps       ,s1        , &
                 & wrkb8     ,wrkb9     ,hu        ,porosu    ,volum1    , &
                 & areau     ,aguu      ,agsqs     ,kfs       ,gdp       )
       call timer_stop(timer_comvol, gdp)
    !
    ! END OF COMPUTATION OF STAGE 1 FOR ADI METHOD
    !
    endif
    !
    ! =====================================
    ! COMPUTATION OF STAGE 2 FOR ADI METHOD
    ! Computation of U1, i.e. evaluate momentum equation for one half timest
    ! =====================================
    !
    if (stage=='stage2') then
       !
       ! Calculate HU and set KFU = 0 for HU < HTRSH (.5*DRYFLC)
       ! hu is already calculated in SUD for the wet points (kfu=1)
       ! Calling checku is only necessary because the hu calculation
       ! is performed on the full computational domain (kcu=1) instead of
       ! only the wet points (kfu=1)
       !
       if (resetV1toV0)vel00 = u0
       contIDRY = 0
       idry  = 0
       flood = .false.
       icx   = nmaxddb
       icy   = 1
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891990,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
!
       if (periodSURFACE) then  ! prescribe periodic velocity components. I do it here so the ghost stencil already has it. And checku needs the periodic water level to compute the correct hu
          if (PERIODalongM/=1) then 
             call velocityPERIOD(v0,u0,icx,nlb,nub,mlb,mub,kmax, gdp) ! the second argument has to be the tangential velocity
          else
             call velocityPERIOD(u0,v0,icx,nlb,nub,mlb,mub,kmax, gdp) ! the second argument has to be the tangential velocity
          endif
          if (callSUBR_WATERlevelPERIOD)  call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax, gdp)  !it should not be needed, after sud its already periodic
       endif
!
       call timer_start(timer_checku, gdp)
       call checku(hu        ,s0        ,dpu       ,umean     , &
                 & kfu       ,kcs       ,kcu       , &
                 & kspu      ,hkru      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & aguu      ,gdp       )
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891991,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       call timer_stop(timer_checku, gdp)
!
!
       if (cutcell.gt.0) then
          if (GhostMethod.eq.1.or.GhostMethod.eq.2) then
             !compute u0 and v0 at ghost points
             call cutcell_pre_uzd_stage2(icx        ,icy        ,u0         ,v0         ,u1         ,&
                                       & u0INTv     ,v0INTu     ,guu        ,gvv        ,xcor       ,&
                                       & ycor                                               ,&             
                                       & v1         ,gsqs       ,kcs        ,dpu        ,dpv        ,&
                                       & Umean      ,Vmean      ,thick      ,qxk        ,qyk        ,&
                                       & hu         ,hv         ,s0         ,s1         ,dps        ,&
                                       & kfs        ,kfu        ,kfv        ,kcu        ,kcv        ,&
                                       & mmax       ,nmax       ,nmmax      ,nmaxus     ,kmax       ,&
                                       & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                                       & nmlb       ,nmub       ,gdp%d%ddbound,lunscr     ,Irov     ,gdp)
              if(testGHOSTaccur) call PRINTtestGHOSTaccur(u0,v0,nlb,nub,mlb,mub,kmax, gdp)
          endif
          if (deactGHOST_smallcut) then !deactivate small partially cut edges before uzd to turn off update there
             call kfuv_ghost_smallcut (kfu,kfv,nlb,nub,mlb,mub,0, gdp)
          endif
       endif
       IF (printINTERMghost) then
         !THERE IS NO POINT TO PRINT s1,u1,v1 they are printed in quarterdt
          CALL postpr_ghost(nst+1 , s0 , u0 , v0 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                         & kmax,nlb,nub,mlb,mub,gdp)
       ENDIF
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891992,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       call timer_start(timer_uzd, gdp)
       gdp%dd%uzditer = 0
       icx            = nmaxddb
       icy            = 1
       call timer_start(timer_2nduzd, gdp)
      ! if (.not.testGHOSTaccur) then
       if (.NOT.noUZD) then
       call uzd(icreep    ,dpdksi    ,s0        ,u0        , &
              & u1        ,v0        ,u0INTv    ,v0INTu    ,w1        ,umean     , &
              & hu        ,hv        ,guu       ,gvv       ,gvu       ,gsqs      , &
              & gvd       ,gud       ,gvz       ,gsqiu     ,qxk       ,qyk       , &
              & disch     ,umdis     ,dismmt    ,mnksrc    ,kcu       , &
              & kcs       ,kfu       ,kfv       ,kfs       , &
              & kspu      ,kadu      ,kadv      ,norow     ,icx       ,icy       , &
              & irocol    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & nmax      ,mmax      ,nmaxus    ,&
              & nsrc      ,lsecfl    ,lstsci    ,betac     ,nst       , &
              & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
              & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
              & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,wrkb15    , &
              & wrkb16    ,taubpu    ,taubsu    ,rho       ,sumrho    , &
              & thick     ,sig       ,dps       ,wsu       ,fxw       ,wsbodyu   , &
              & vicuv     ,vnu2d     ,vicww     ,rxx       ,rxy       , &
              & dfu       ,deltau    ,tp        ,rlabda    , &
              & diapl     ,rnpl      ,ghostU1   ,ghostV1   , &
              & cfurou    ,cfvrou    ,rttfu     ,r0        ,windsu    , &
              & patm      ,fcorio    ,ubrlsu    ,hkru      , &
              & pship     ,tgfsep    ,dteu      ,ustokes   ,.false.   , &
              & xG_L      ,yG_L      ,kWDu      ,kWDv      ,aguu      , &
              & xcor      ,ycor      ,gdp       )
              if(testGHOSTaccur) call PRINTtestGHOSTaccur(u1,v0,nlb,nub,mlb,mub,kmax, gdp)           
      ! endif
       else
          U1 = U0
       endif
       call timer_stop(timer_2nduzd, gdp)
       call timer_stop(timer_uzd, gdp)
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891993,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif

       if (QUARTERdt) then      
          !in this way i can print v0 (ghost point), otherwhise i print v1 that is not defined yet    
         ! call postpr_hdt(s0,nst, gdp)      
          CALL postpr_ghost(nst+1 , s0 , u1 , v0 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                         & kmax,nlb,nub,mlb,mub,gdp)              
       endif
       if (cutcell.gt.0) then
          if(GhostMethod.eq.1.or.GhostMethod.eq.2)  then
             if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,0,nlb,nub,mlb,mub,nmlb,nmub, gdp) !set kfs,kfu,kfv NOT active in ghost points. Only s0,v1,v0 and u0 should be reset
          endif
          if (deactGHOST_smallcut) then !activate small partially cut edges for sud
             call kfuv_ghost_smallcut (kfu,kfv,nlb,nub,mlb,mub,1, gdp)
          endif
       endif       
       !
       !     computation proceeds in Y direction
       !
       !
       ! CHECK FOR FLOODING AND DRYING IN "V" POINTS
       !
       flood = .true.
       icx   = 1
       icy   = nmaxddb
       call timer_start(timer_checku, gdp)
       call checku(hv        ,s0        ,dpv       ,vmean     , &
                 & kfv       ,kcs       ,kcv       , &
                 & kspv      ,hkrv      ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,icx       ,flood     ,dps       , &
                 & agvv      ,gdp       )
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891994,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       call timer_stop(timer_checku, gdp)
!      
       if (periodSURFACE) then  ! prescribe periodic velocity components.
          ! It is actually not necessary to recompute u1 if implicit periodic, since it should have been converged from uzd up to given tolerance.
          ! v1 could just be copied from v0 since nothing changed but its not much cheaper so I just call the subroutine
          if (.not.TRANSVperIMPL.or.testGHOSTaccur) then
             if (PERIODalongM==1) then 
                call velocityPERIOD(u1,v1,icx,nlb,nub,mlb,mub,kmax, gdp)    
             else
                call velocityPERIOD(v1,u1,icx,nlb,nub,mlb,mub,kmax, gdp)   
             endif
          endif
          ! call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax) not needed nothing changed
       endif

       if (.not. onlyUZD) then
          !
          ! Skip sud and pre-sud stuff 
          ! Cutcell modification
          !
          call cutcell_pre_sud_stage2(icx        ,icy        ,u0         ,v0         ,u1         ,&
                                    & u0INTv     ,u1INTv     ,v0INTu                             ,&
                                    & v1         ,gsqs       ,kcs        ,dpu        ,dpv        ,&
                                    & Umean      ,Vmean      ,thick      ,qxk        ,qyk        ,&
                                    & hu         ,hv         ,s0         ,s1         ,dps        ,&
                                    & guu        ,gvv        ,xcor       ,ycor                   ,&       
                                    & kfs        ,kfu        ,kfv        ,kcu        ,kcv        ,&
                                    & mmax       ,nmax       ,nmmax      ,nmaxus     ,kmax       ,&
                                    & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                                    & nmlb       ,nmub       ,gdp%d%ddbound,lunscr     ,Irov     ,gdp)
          if(testGHOSTaccur) call PRINTtestGHOSTaccur(u1,v1,nlb,nub,mlb,mub,kmax, gdp)
          IF (printINTERMghost) then
             IF (iprintINTERMghost01 ==0) THEN
                CALL postpr_ghost(nst+1 , s0 , u0 , v0 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                               & kmax,nlb,nub,mlb,mub,gdp)
             ELSEIF (iprintINTERMghost01 ==1) THEN
                CALL postpr_ghost(nst+1 , s1 , u1 , v1 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                               & kmax,nlb,nub,mlb,mub,gdp)
             ELSE
                CALL postpr_ghost(nst+1 , s0 , u0 , v0 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                               & kmax,nlb,nub,mlb,mub,gdp)
                CALL postpr_ghost(nst+1 , s1 , u1 , v1 , qxk , qyk , Umean , Vmean , hu , hv , dpu , dpv , dps , &
                               & kmax,nlb,nub,mlb,mub,gdp)
             ENDIF
          ENDIF
          if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
             do k =1,nmmax
               call nm_to_n_and_m(k, n, m, gdp)
               if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
               write(9891995,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
             enddo
          endif
22222     continue
          !in case idry==1 reset kfsuv_ghost recompute qxk (u0 it is not passed to sud therefore for not changing the arguments of sud I call it here)
          if (idry==1.and.cutcell.gt.0.and.GhostMethod.le.1) then           
            !Note: I use the explicit qxk. However, if idry=1 it means that in drychk all the 4 qyk,qxk in the 4 edges of a dry cell are set to zero so here   I  have to recompute qxk  
            do k=1,kmax             
               CALL interpG_ATu1LOCATION(u0(nmlb,k),kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,-1,nlb,nub,mlb,mub,nmlb,nmub,0._fp, gdp) ! this is needed only    cause in kfsuv_ghost i set u0 to zero
            enddo  
            ! see comment stage 1: hu should not be needed at ghost point
            !CALL interpG_ATu1LOCATION(hu0,kcs,lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,1,nlb,nub,mlb,mub,nmlb,nmub,0._fp)
            !call qxORqy_ghosts(u0,hu0,guu,aguu,thick,porosu,qxk,mmax,nmax,nmaxus,kmax,nst,totGHOSTu1,mGPu1,nGPu1,nlb,nub,mlb,mub,nmlb,nmub,icx)
            if (changeKFUVcut) call kfsuv_ghost   (Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,1,0,nlb,nub,mlb,mub,nmlb,nmub,    gdp) !set kfs,kfu,kfv active in ghost points              
          endif
          
          icx            = 1
          icy            = nmaxddb
          if (sbkol) then
             !
             ! Correction for open discharge boundaries in explicit direction
             !
             call bccor(j         ,nmmaxj    ,kmax      ,norow     ,icy       , &
                      & icx       ,zmodel    ,irocol(1,1),kcs      ,kfu       , &
                      & qxk       ,thick     ,circ2d(1,1)          ,gdp       ) 
          endif
          !
          ! Computation of V1 and S1, i.e. evaluation of coupled momentum and
          ! continuity equation for one half time step
          !
          gdp%dd%suditer = 0
          call timer_start(timer_sud, gdp)
          call timer_start(timer_2ndsud, gdp)
          !if (.not.testGHOSTaccur) then
          call sud(dischy    ,nst       ,icreep    ,betac     ,nmax      , &
                 & nmaxus    ,kFLcut, &
                 & mmax      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                 & lstsci    ,nsrc      ,lsecfl    ,nocol     ,icx       , &
                 & icy       ,dismmt    ,irocol(1, norow + 1) ,mnksrc    , &
                 & kfv       ,kfu       ,kfs       ,kcs       ,kspv      , &
                 & kadv      ,kadu      ,kcv       ,kfvmin    ,kfvmax    , &
                 & porosv    ,s0        ,s1        ,v0        ,v1        , &
                 & u1INTv    ,xcor      ,ycor                            , &
                 & u1        ,w1        ,r0        ,qyk       ,qxk       , &
                 & qzk       ,gvv       ,guu       ,guv       ,gsqs      , &
                 & gvd       ,gud       ,guz       ,gsqiv     ,dtev      , &
                 & circ2d(1,norow+1),circ3d(1,1,norow+1),disch,porosu , &
                 & vmdis     ,vmean     ,hv        ,hu        ,dpv       ,dzv1      , &
                 & dpdeta    ,thick     ,sig       ,dps       ,taubpv    , &
                 & taubsv    ,rho       ,sumrho    ,wsv       ,fyw       , &
                 & wsbodyv   ,idry      ,crbc(1, norow + 1)   ,vicuv     ,wrka9     , &
                 & vnu2d     ,vicww     ,ryy       ,rxy       ,dfv       , &
                 & deltav    ,tp        ,rlabda    ,cfvrou    ,cfurou    , &
                 & rttfv     ,diapl     ,rnpl      , &
                 & windsv    ,patm      ,fcorio    ,evap      ,ubrlsv    , &
                 & uwtypv    ,hkrv      ,pship     ,tgfsep    ,wrka1     , &
                 & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
                 & wrka7     ,wrka8     ,wrka16    ,wrkb1     ,wrkb2     , &
                 & wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     ,wrkb7     , &
                 & wrkb8     ,wrkb15    ,wrkb16    ,soumud    ,dis_nf    , &
                 & precip    ,vstokes   ,agvv      ,aguu      ,agsqs     , &
                 & qyk_tinyCUT, GHOSTv1 ,GHOSTu1   ,yG_L      ,xG_L      , &
                 & kWDv      ,kWDu      ,timnow   , &
                 & gdp )
          !endif
          call timer_stop(timer_2ndsud, gdp)
          call timer_stop(timer_sud, gdp)
          if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
             do k =1,nmmax
               call nm_to_n_and_m(k, n, m, gdp)
               if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
               write(9891996,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
             enddo
          endif
          !
          ! Check for drying in waterlevel points in the X-direction
          !
          icx = nmaxddb
          icy = 1
          call timer_start(timer_drychk, gdp)
          !if (cutcell==2) call drychk_cc(kfs_cc,   poros    ,agsqs   ,s1     ,dps    ,dpL   ,dpH    ,nmmax   ,nmlb, nmub ,zmodel)     
          if (.not.testGHOSTaccur) then
          call drychk(idry      ,s1        ,qxk       ,qyk       ,icx       , &
                    & icy       ,dps       ,kfu       ,kfv       ,kfs       , &
                    & j         ,nmmaxj    ,nmmax     ,kmax      ,nfltyp    , &
                    & excbed    ,kcs       ,nst       ,gdp        )
          endif
          call timer_stop(timer_drychk, gdp)
          !if (changeKFUVcut) call kfsuv_ghost   (Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,1,0,nlb,nub,mlb,mub,nmlb,nmub)
          if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
             do k =1,nmmax
               call nm_to_n_and_m(k, n, m, gdp)
               if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
               write(9891997,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
             enddo
          endif
          !
          !
          ! DD code added:
          !
          !
          ! Synchronize on Dry Point
          !
          if (nfltyp==0) then
             nhystp = nxtdry(d3dflow_check_adi_dry, 0, gdp)
          else
             nhystp = nxtdry(d3dflow_check_adi_dry, idry, gdp)
          endif
          !this part has to be improved
          if (cutcell.gt.0.and.GhostMethod.le.1) then
          
             if ((nfltyp/=0).and.(nhystp==d3dflow_build_adi_zeta .or. (nhystp==noneighbors .and. idry==1))) then
                continue !sud is repeated, what should I do?
             else
             endif
          endif
          if (nfltyp/=0) then
             !
             ! If waterlevel is below bottom then isolate waterlevel point by setting
             ! the surrounding velocities to zero and repeat the computation of SUD
             !
             if (nhystp==d3dflow_build_adi_zeta .or.                               &
               & (nhystp==noneighbors .and. idry==1)) then
               contIDRY = contIDRY +1
               if (contIDRY > 50) then
                  write(*,*) 'Deadlock in sud because of drying'
                  call d3stop(1, gdp)
               endif
               goto 22222
             endif
          !
          ! DD code added end
          !
          endif
       
       ENDIF  !END OF if (.not.onlyUZD) then  !skip sud and pre-sud stuff 
!
!      set ghost velocity points with active edge=0 to zero vel and kf=1. Reset dps and s1., dpu,dpv, qxk,Umean, qyk,Vmean
       if (cutcell.eq.2.and.ghostmethod.ne.3) then
         !da verificare se s0 cambia in sud (l unico modo è per il wettign e drying penso) e se non cambia eliminare questa chiamata a s0iss1_ghost
           !IF (free_S1_sud.EQ.1) THEN
          !    CALL s0iss1_ghost(s0,s00,nlb,nub,mlb,mub,kmax,0) !basically s0 = s00 ! in order to have the mass check correct in updmassbal for fully emerged ghost cells (where s0 is a ghost point and comparereal(poros(nGP,mGP),0._fp))
          ! ENDIF
          !copy velocities at ghost point for taubot
        !  CALL u0isu1_ghost(uGHOST,u1,nlb,nub,mlb,mub,kmax) 
        !  CALL v0isv1_ghost(vGHOST,v1,nlb,nub,mlb,mub,kmax)           
          !S00 SHOULD BE passed to kfsuv_ghost_cutEDGES and so only s0 in the fully emerged ghost cells ghost cell is reset     
          !IF I REMOVED I HAVE WEIRD RESULTS! CHECK!
          KFvalue = 1
          IF (dontRESETghost) KFvalue = 0
          if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,KFvalue,nlb,nub,mlb,mub,nmlb,nmub, gdp) 
          if (resetV1toV0) call resetV1toV0subr(vel00,u1,aguu,mGPu1,nGPu1,totGHOSTu1,kmax,nlb,nub,mlb,mub) !to try well banancing
       endif
!
       !
       ! Compute Volume and Areas to be used in routines that computes
       ! the transport of matter (consistency with WAQ)
       ! Moved here for compatibility with cutcell approach (kfs has to be reset on fully emerged ghost cells since aqsgs=1 there)
       !
       call timer_start(timer_comvol, gdp)
       call comvol(nmmax     ,kmax      ,zmodel    ,kcs       ,kcv       , &
                 & thick     ,gvv       ,gsqs      ,dps       ,s1        , &
                 & wrkb8     ,wrkb9     ,hv        ,porosv    ,volum1    , &
                 & areav     ,agvv      ,agsqs     ,kfs       ,gdp       )
       call timer_stop(timer_comvol, gdp)
!

    !
    ! END OF COMPUTATION OF STAGE 2 FOR ADI METHOD
    !
    endif
!
    if (cutcell.eq.2)   then
       call PLIC_VOF_STEP(gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,dps,dpU,dpV,xcor,ycor,alfas,&
                      lunscr,lundia,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,drycrt,&
                      thick,guu,gvv,hu,hv,porosu,porosv,qxk,qyk,Umean,Vmean,stage,dummy0,dummy0,dummy0,dummy0,gdp%d%ddbound,nmmax,Zmodel, gdp)
    endif
    !DO NM=1,NMMAX
    !   IF (STAGE == 'stage1') THEN
    !      WRITE(9991234+10*NST,'(2I8,15F25.15)') NST,NM,U1(NM,1)
    !  ! ELSE
    !      WRITE(9991235+10*NST,'(2I8,15F25.15)') NST,NM,U1(NM,1)
    !   ENDIF
    !ENDDO
!
end subroutine adi
