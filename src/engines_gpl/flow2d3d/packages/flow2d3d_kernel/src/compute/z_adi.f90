subroutine z_adi(stage     ,j         ,nmmaxj    ,nmmax     ,kmax      , &
               & mmax      ,nmax      ,zmodel    ,nst       ,nfltyp    , &
               & nocol     ,norow     ,nsrc      ,dismmt    ,irocol    , &
               & mnksrc    ,kfu       ,kfv       ,kfs       ,kspu      , &
               & kspv      ,kadu      ,kadv      ,kcs       ,kcu       , &
               & kcv       ,kfsmin    ,kfsmax    ,kfsmn0    ,kfsmx0    , &
               & kfumin    ,kfumax    ,kfumn0    ,kfumx0    ,kfvmin    , &
               & kfvmax    ,kfvmn0    ,kfvmx0    ,kfuz0     ,kfvz0     , &
               & kfsz0     ,kfuz1     ,kfvz1     ,kfsz1     ,kcu45     , &
               & kcv45     ,kcscut    ,porosu    ,porosv    ,areau     , &
               & areav     ,volum1    ,s0        ,s1        ,w1        , &
               & u0        ,u1        ,v0        ,v1        ,hu        , &
               & hv        ,thick     ,umean     ,ubrlsu    ,ubrlsv    , &
               & vmean     ,dpu       ,dpv       ,dps       ,dzu0      , &
               & dzv0      ,dzs0      ,dzu1      ,dzv1      ,dzs1      , &
               & qxk       ,qyk       ,qzk       ,evap      ,circ2d    , &
               & circ3d    ,drhodx    ,drhody    ,disch     ,umdis     , &
               & vmdis     ,wsu       ,wsv       ,dfu       ,dfv       , &
               & deltau    ,deltav    ,tp        ,rlabda    ,wsbodyu   , &
               & wsbodyv   ,fxw       ,fyw       ,gud       ,gvd       , &
               & guu       ,guv       ,gvv       ,gvu       ,guz       , &
               & gvz       ,gsqs      ,gsqiu     ,gsqiv     ,taubpu    , &
               & taubpv    ,taubsu    ,taubsv    ,vicuv     ,vnu2d     , &
               & vicww     ,rxx       ,rxy       ,ryy       ,windu     , &
               & windv     ,patm      ,fcorio    ,tgfsep    ,wrka1     , &
               & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
               & wrka7     ,wrka8     ,wrka15    ,wrka16    ,wrkb1     , &
               & wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     , &
               & wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    ,zk        , &
               & p0        ,crbc      ,hu0       ,hv0       ,wrkb11    , &
               & wrkb12    ,wrkb13    ,wrkb14    ,pship     ,diapl     , &
               & rnpl      ,sbkol     ,cfurou    ,cfvrou    ,r0        , &
               & lstsci    ,precip    ,nmaxus    ,xcor      ,ycor      , &
               & alfas     ,gdp       )
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
!  $Id: z_adi.f90 6195 2016-06-09 17:28:43Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/z_adi.f90 $
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
    real(fp)                 , pointer :: drycrt
    real(fp)                 , pointer :: hdt
    real(fp), dimension(:,:) , pointer :: ustokes
    real(fp), dimension(:,:) , pointer :: vstokes
    integer , dimension(:)   , pointer :: modify_dzsuv
    logical                  , pointer :: ztbml
    logical                  , pointer :: ztbml_upd_r1
    integer                  , pointer :: lunscr
    integer                  , pointer :: irov
    integer                  , pointer :: nmlb
    integer                  , pointer :: nmub
    integer                  , pointer :: mlb
    integer                  , pointer :: mub
    integer                  , pointer :: nlb
    integer                  , pointer :: nub
    integer                  , pointer :: lundia
    integer                  , pointer :: itstrt
    include 'flow_steps_f.inc'
    integer                 , pointer :: cutcell
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: agvv
    real(fp), dimension(:,:), pointer :: agsqs
    integer, dimension(:,:) , pointer :: GHOSTu1
    integer, dimension(:,:) , pointer :: GHOSTv1
    integer                 , pointer :: GhostMethod
    integer                 , pointer :: idebugCUThardINI
    integer                 , pointer :: idebugCUThardFIN
    integer                 , pointer :: doNOTdebugGHOSTS
    integer, dimension(:,:) , pointer :: GHOSTs1
    logical                 , pointer :: onlyUZD
    logical                 , pointer :: periodSURFACE
    logical                 , pointer :: TRANSVperIMPL
    integer                 , pointer :: PERIODalongM
    logical                 , pointer :: printINTERMghost
    integer                 , pointer :: iprintINTERMghost01
    integer, dimension(:,:) , pointer :: kfs_cc
    real(fp), dimension(:,:), pointer :: poros
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: dpH
    logical                 , pointer :: QUARTERdt
    logical                 , pointer :: callSUBR_WATERlevelPERIOD
    real(fp), dimension(:,:), pointer :: xG_L
    real(fp), dimension(:,:), pointer :: yG_L
    logical                 , pointer :: changeKFUVcut
    real(fp), dimension(:,:), pointer :: u0INTv
    real(fp), dimension(:,:), pointer :: u1INTv
    real(fp), dimension(:,:), pointer :: v0INTu
    real(fp), dimension(:,:), pointer :: v1INTu
!
! Global variables
!
    integer                                               :: j       !!  Begin pointer for arrays which have
                                                                     !!  been transformed into 1D arrays.
                                                                     !!  Due to the shift in the 2nd (M-)
                                                                     !!  index, J = -2*NMAX + 1
    integer                                               :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                               :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nfltyp  !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nmmax   !  Description and declaration in dimens.igs
    integer                                               :: nmaxus
    integer                                               :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                               :: nocol   !  Description and declaration in esm_alloc_int.f90
    integer                                               :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nst     !!  Time step number
    integer, dimension(7, norow + nocol)                  :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                           :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfsmax  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfsmin  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfsmx0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfsmn0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfumax  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfumin  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfumx0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfumn0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfvmax  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfvmin  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfvmx0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfvmn0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)     :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)     :: kspv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kadv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kcscut  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kcu45   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kcv45   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfsz0   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfuz0   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfvz0   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfsz1   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfuz1   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfvz1   !  Description and declaration in esm_alloc_int.f90
    logical                                               :: sbkol   !  Description and declaration in procs.igs
    logical                                               :: zmodel  !  Description and declaration in procs.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)         :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)         :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(12, norow + nocol)                :: crbc    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow + nocol)                 :: circ2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: xcor    
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: ycor
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: deltav  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dfv     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)          :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dpv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: evap    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: fyw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gsqiv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: guz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: hu0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: hv0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: precip  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubpv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: tgfsep  !!  Water elev. induced by tide gen.force
                                                                     !!  Internal work array WRKB17 used
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: vmean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: windu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: windv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka15  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka16  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka2   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka3   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka4   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka5   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka6   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka7   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wrka8   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsbodyv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: alfas
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: qzk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)    :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: areau   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: areav   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzs0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzs1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzu0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzu1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzv0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzv1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: p0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: porosu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: porosv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: ryy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: drhodx  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: drhody  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: ubrlsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: v0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: volum1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb2   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb3   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb4   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb5   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb6   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb7   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb8   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb9   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb10  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb11  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb12  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb13  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: wrkb14  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                             :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                           :: zk
    real(fp), dimension(kmax, 2, norow + nocol)           :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                             :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                             :: umdis   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                             :: vmdis   !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrc)                         :: dismmt  !  Description and declaration in esm_alloc_char.f90
    character(8)                            , intent(in)  :: stage   !!  First or Second half time step
!
! Local variables
!
    integer :: k
    integer :: n
    integer :: m
    integer :: icx
    integer :: icy
    integer :: idry
    integer :: nhystp
    integer :: nmaxddb
    logical :: flood   ! Flag for activating flooding part of checku subroutine
    !real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0INTv
    !real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u1INTv
    !real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v0INTu
    !real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1INTu
!
!! executable statements -------------------------------------------------------
!
    cutcell                   => gdp%gdimbound%cutcell
    aguu                      => gdp%gdimbound%aguu
    agvv                      => gdp%gdimbound%agvv
    agsqs                     => gdp%gdimbound%agsqs
    GHOSTu1                   => gdp%gdimbound%GHOSTu1
    GHOSTv1                   => gdp%gdimbound%GHOSTv1
    GhostMethod               => gdp%gdimbound%GhostMethod
    idebugCUThardINI          => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN          => gdp%gdimbound%idebugCUThardFIN
    doNOTdebugGHOSTS          => gdp%gdimbound%doNOTdebugGHOSTS
    GHOSTs1                   => gdp%gdimbound%GHOSTs1
    onlyUZD                   => gdp%gdimbound%onlyUZD
    periodSURFACE             => gdp%gdimbound%periodSURFACE
    TRANSVperIMPL             => gdp%gdimbound%TRANSVperIMPL
    PERIODalongM              => gdp%gdimbound%PERIODalongM
    printINTERMghost          => gdp%gdimbound%printINTERMghost
    iprintINTERMghost01       => gdp%gdimbound%iprintINTERMghost01
    kfs_cc                    => gdp%gdimbound%kfs_cc
    poros                     => gdp%gdimbound%poros
    dpL                       => gdp%gdimbound%dpL
    dpH                       => gdp%gdimbound%dpH
    QUARTERdt                 => gdp%gdimbound%QUARTERdt
    callSUBR_WATERlevelPERIOD => gdp%gdimbound%callSUBR_WATERlevelPERIOD
    xG_L                      => gdp%gdimbound%xG_L
    yG_L                      => gdp%gdimbound%yG_L
    changeKFUVcut             => gdp%gdimbound%changeKFUVcut
    u0INTv                    => gdp%gdimbound%Dwrkak1
    u1INTv                    => gdp%gdimbound%Dwrkak2
    v0INTu                    => gdp%gdimbound%Dwrkak3
    v1INTu                    => gdp%gdimbound%Dwrkak4
    hdt                => gdp%gdnumeco%hdt
    ustokes            => gdp%gdtrisol%ustokes
    vstokes            => gdp%gdtrisol%vstokes
    modify_dzsuv       => gdp%gdzmodel%modify_dzsuv
    ztbml              => gdp%gdzmodel%ztbml
    ztbml_upd_r1       => gdp%gdzmodel%ztbml_upd_r1
    lunscr             => gdp%gdinout%lunscr
    irov               => gdp%gdphysco%irov
    nmlb               => gdp%d%nmlb  
    nmub               => gdp%d%nmub
    mlb                => gdp%d%mlb  
    nlb                => gdp%d%nlb  
    mub                => gdp%d%mub  
    nub                => gdp%d%nub  
    itstrt             => gdp%gdinttim%itstrt
    lundia             => gdp%gdinout%lundia
    drycrt             => gdp%gdnumeco%drycrt
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    !
    !
    ! =====================================
    ! COMPUTATION OF STAGE 1 FOR ADI METHOD
    ! =====================================
    !
    if (stage=='stage1') then
       !
       ! Computation of V1, i.e. evaluate momentum equation for one half timest
       !     calculate hv and set kfv = 0 for hv < htrsh (.5*dryflc)
       !
       ! - For explicit Z-model schematisation Z_UZD is a dummy subroutine 
       !   (for synchronisation with domain decomposition implementation of the sigma-model)
       !
       ! - For implicit Z-model schematisation, the V-velocities are updated in Z_UZD
       !
       !
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
          if (callSUBR_WATERlevelPERIOD)  call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax, gdp)  !it should not be needed, after sud its already periodic
       endif
       !      cutcell modification
       if (cutcell.gt.0.and.(GhostMethod.eq.1.or.GhostMethod.eq.2)) THEN
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
       endif
       call timer_start(timer_uzd, gdp)
       !
       gdp%dd%uzditer = 0
       icx = 1
       icy = nmaxddb
       call timer_start(timer_1stuzd, gdp)
       call z_uzd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx         , &
                & icy       ,nsrc      ,kcs       ,kcv45     ,kcscut      , &
                & kcv       ,kfv       ,kfvz0     ,kfvmn0    ,kfvmx0      , &
                & kfu       ,kfuz0     ,kfumn0    ,kfumx0    ,dzu0        , &
                & kfs       ,kfsz0     ,kfsmn0    ,kfsmx0    , &
                & v0        ,u0        ,w1        ,hv        ,hu          ,dzv0        ,dzs0      , &
                & gvv       ,guu       ,guv       ,gvu       ,gsqs        , &
                & gvd       ,gud       ,gvz       ,guz       ,gsqiv       , &
                & disch     ,umdis     ,kspv      ,mnksrc    ,dismmt      , &
                & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5       , &
                & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10      , &
                & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,circ2d(1,norow + 1) ,circ3d(1,1,norow + 1) , &
                & vicuv     ,vnu2d     ,vicww     ,tgfsep    ,dps         , &
                & dfv       ,deltav    ,tp        ,rlabda    ,fyw         ,wsbodyv   , &
                & drhody    ,wsv       ,taubpv    ,taubsv    ,ryy         , &
                & rxy       ,windv     ,patm      ,fcorio    ,p0          , &
                & ubrlsv    ,pship     ,diapl     ,rnpl      ,cfvrou      , &
                & v1        ,s0        ,dpv       ,qyk       ,qxk         , &
                & nocol     ,norow     ,irocol(1, norow + 1) ,nst         ,vmean       , &
                & nmax      ,mmax      ,nmaxus    ,v0INTu    ,u0INTv      ,&
                & crbc(1,norow + 1)    ,vstokes   ,xcor      ,ycor        ,gdp       )
       call timer_stop(timer_1stuzd, gdp)
       call timer_stop(timer_uzd, gdp)
       !this now is only for checku. double check if its still needed
       if (cutcell.gt.0.and.(GhostMethod.eq.1.or.GhostMethod.eq.2))  then
           if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,0,nlb,nub,mlb,mub,nmlb,nmub, gdp) !set kfs,kfu,kfv NOT active in ghost points. Only s0,v1,v0 and u0 should be reset
       endif
       !
       !     computation proceeds in X direction
       !
       ! CHECK FOR FLOODING AND DRYING IN "U" POINTS
       !
       ! Update HU, DZU1. DZS1 is updated in Z_DRYCHK after Z_SUD based
       ! on the computed water level at new time step
       !
       flood = .true.
       idry  = 0
       icx   = nmaxddb
       icy   = 1
       call timer_start(timer_checku, gdp)
       call z_checku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                   & flood     ,kfu       ,kcs       ,kcu       ,kspu      , &
                   & kfumn0    ,kfumx0    ,hu        ,s0        ,dpu       , &
                   & dps       ,umean     ,kfuz0     ,kfsmn0    ,kfsmx0    , &
                   & u0        ,dzu0      ,zk        ,aguu      ,gdp       )
       call timer_stop(timer_checku, gdp)
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
          if (.not.TRANSVperIMPL) then
             if (PERIODalongM/=1) then 
                call velocityPERIOD(v1,u1,icx,nlb,nub,mlb,mub,kmax, gdp)    
             else
                call velocityPERIOD(u1,v1,icx,nlb,nub,mlb,mub,kmax, gdp)   
             endif
          endif
          !if (callSUBR_WATERlevelPERIOD)  call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax) not needed nothing changed
       endif
       !
       if (cutcell>0 .and. .not.onlyUZD) then  !skip sud and pre-sud stuff 
          !
          ! cutcell modification
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
       endif
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
       !
       ! Computation of U1 and S1, i.e. evaluation of coupled momentum and
       ! continuity equation for one half time step. 
       !
       ! HU is updated by routine UPWHU in Z_CHECKU
       !
11111  continue
       call timer_start(timer_sud, gdp)
       !
       gdp%dd%suditer = 0
       icx = nmaxddb
       icy = 1
       call timer_start(timer_1stsud, gdp)
       call z_sud(j         ,nmmaxj    ,nmmax     ,kmax      ,mmax      , &
                & nmax      ,nsrc      ,nst       ,icx       ,icy       , &
                & flood     ,norow     ,irocol(1, 1)         ,mnksrc    ,kfsmx0    , &
                & kfu       ,kfv       ,kfs       ,kcs       ,kcu       , &
                & kfuz0     ,kfvz0     ,kfsz0     ,kspu      ,kcu45     , &
                & kcscut    ,kfumn0    ,kfsmn0    ,kfumx0    ,kfvmn0    , &
                & kfvmx0    ,thick     ,circ2d(1, 1) ,circ3d(1, 1, 1)   ,s0        , &
                & s1        ,u0        ,u1        ,v1        ,w1        , &
                & qxk       ,qyk       ,qzk       ,guu       ,gvv       , &
                & guv       ,gvu       ,gud       ,gvd       ,guz       , &
                & gvz       ,gsqiu     ,gsqs      ,disch     ,umdis     , &
                & dismmt    ,umean     ,evap      ,hu        ,hv        ,dps       , &
                & dpu       ,dzs0      ,dzu0      ,dzv0      ,wrka1     , &
                & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &
                & wrka7     ,wrka8     ,wrka15    ,wrkb1     ,wrkb2     , &
                & wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     ,wrkb7     , &
                & wrkb8     ,wsu       ,taubpu    ,taubsu    ,vicuv     , &
                & vnu2d     ,vicww     ,rxx       ,rxy       ,windu     , &
                & tp        ,rlabda    ,dfu       ,deltau    ,fxw       ,wsbodyu   , &
                & patm      ,fcorio    ,tgfsep    ,drhodx    ,zk        , &
                & p0        ,crbc(1, 1),idry      ,porosu    ,ubrlsu    , &
                & pship     ,diapl     ,rnpl      ,cfurou    ,precip    , &
                & ustokes   ,GHOSTu1   ,GHOSTv1   ,xG_L      ,yG_L      , &
                & aguu      ,agvv      ,agsqs     ,nmaxus    , &                                     
                & v1INTu    ,xcor      ,ycor      , &
                & gdp       )
       call timer_stop(timer_1stsud, gdp)
       call timer_stop(timer_sud, gdp)
       !turn off ghost points. Might have already be done in sud
       if (cutcell.gt.0.and.GhostMethod.le.1) then        
           if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,0,nlb,nub,mlb,mub,nmlb,nmub, gdp) !DA OTTIMIZZARE: THIS IS NEEDED ONLY TO DEACTIVATE GHOST VELOCITY POINTS (S1 CAN STAY ACTIVE) NOT TO GET STUCK ON A DEADLOCK WITH IDRY=1, SINCE drycheck keep finding s1(n,m)=dps(n,m). an option to remove this at all and also the reactivation below is to replace s1(nm)<= with < in checkdry.
       endif
       !
       ! Check for drying in waterlevel points in the X-direction
       ! Update DZS1
       ! NOTE: Z_DRYCHK is called with arrays KFUZ0, KFVZ0 and KFSZ1. 
       !       KFUZ1 and KFVZ1 are the arrays correspondng to the original geometry (S0)
       !       KFSZ1 is to be determined, corresponding to the new geometry (S1)
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_drychk, gdp)
       !if (cutcell==2) call drychk_cc(kfs_cc,   poros    ,agsqs   ,s1     ,dps    ,dpL   ,dpH    ,nmmax   ,nmlb, nmub,zmodel)
       call z_drychk(idry      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                   & nfltyp    ,icx       ,icy       ,kfu       ,kfv       , &
                   & kfs       ,kcs       ,kfuz0     ,kfvz0     ,kfsz1     , &
                   & kfsmin    ,kfsmn0    ,kfsmax    ,kfsmx0    ,s1        , &
                   & r0        ,dps       ,qxk       ,qyk       ,w1        , &
                   & lstsci    ,dzs1      ,zk        ,nst       ,gdp       )
       call timer_stop(timer_drychk, gdp)
       !
       ! Update the layer geometry in U-velocity points based on the new water levels S1
       ! NOTE: Z_DRYCHKU is called with arrays KFUZ1, KFVZ1 and KFSZ1. 
       !       KFSZ1/DZS1 have just been determined, corresponding to the new geometry (S1)
       !       KFUZ1/DZU1 are to be determined, corresponding to the new geometry (S1)
       !       KFVZ1/DZV1 are updated to the new geomety after the second half time step
       !
       icx = nmaxddb
       icy = 1
       call z_drychku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                    & kcs       ,kfu       ,kcu       ,kspu      ,kfsmax    , &
                    & kfsmin    ,kfsz1     ,kfuz1     ,kfumin    ,kfumn0    ,kfumax    , &
                    & kfumx0    ,hu        ,s1        ,dpu       ,dps       , &
                    & umean     ,u0        ,u1        ,dzu0      ,dzu1      , &
                    & dzs1      ,zk        ,kfsmx0    ,guu       ,qxk       , &
                    & aguu      ,gdp       )
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891987,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       !
       ! If requested by keyword ZTBML 
       ! (Z-model TauBottom Modified Layering)
       ! --> modify the near-bed layering to obtain smoother bottom shear stress representation in z-layer models
       !
       if (ztbml) then
          !
          ! Call with modify_dzsuv(1:2) = 1, to modify dzs1 and dzu1
          ! (and possibly R0 and qzk)
          !
          modify_dzsuv(1:2) = 1
          modify_dzsuv(3)   = 0
          ztbml_upd_r1      = .false.
          call z_taubotmodifylayers(nmmax  ,kmax     ,lstsci       ,icx     ,icy          , & 
                                  & kfs    ,kfsmin   ,kfsmax       ,dps     ,dzs1         , &
                                  & kfu    ,kfumin   ,kfumax       ,dpu     ,dzu1         , &
                                  & kfv    ,kfvmin   ,kfvmax       ,dpv     ,dzv1         , &
                                  & r0     ,s0       ,s1           ,zk      ,modify_dzsuv , &
                                  & hdt    ,gsqs     ,kfsmx0       ,qzk     ,umean        , &
                                  & vmean  ,dzs0     ,ztbml_upd_r1 ,gdp      )
       endif
!
!      set ghost velocity points with active edge=0 to zero vel and kf=1. Reset dps and s1., dpu,dpv, qxk,Umean, qyk,Vmean
       if (cutcell.eq.2)   then
          if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,1,nlb,nub,mlb,mub,nmlb,nmub, gdp) 
       endif
       !
       ! Compute Volume and Areas to be used in routines that computes 
       ! the transport of matter (consistency with WAQ)
       !
       ! For areas use the DZU array from previous time step (DZU0)
       !
       call timer_start(timer_comvol, gdp)
       call comvol(nmmax     ,kmax      ,zmodel    ,kcs       ,kcu       , &
                 & thick     ,guu       ,gsqs      ,dps       ,s1        , &
                 & dzs1      ,dzu0      ,hu        ,porosu    ,volum1    , &
                 & areau     ,aguu      ,agsqs     ,kfs       ,gdp       )
       call timer_stop(timer_comvol, gdp)
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
       !
       if (nfltyp/=0) then
          !
          ! If waterlevel is below bottom then isolate waterlevel point by setting
          ! the surrounding velocities to zero and repeat the computation of Z_SUD
          !
          if (nhystp==d3dflow_build_adi_zeta .or.                               &
            & (nhystp==noneighbors .and. idry==1)) goto 11111
       !
       ! DD code added end
       !
       endif
    !
    ! END OF COMPUTATION OF STAGE 1 FOR ADI METHOD
    !
    endif
    !
    ! =====================================
    ! COMPUTATION OF STAGE 2 FOR ADI METHOD
    ! =====================================
    !
    if (stage=='stage2') then
       !
       ! Computation of U1, i.e. evaluate momentum equation for one half timest
       !     calculate hu and set kfu = 0 for hu < htrsh (.5*dryflc)
       !
       ! - For explicit Z-model schematisation Z_UZD is a dummy subroutine 
       !   (for synchronisation with domain decomposition implementation of the sigma-model)
       !
       ! - For implicit Z-model schematisation, the U-velocities are updated in Z_UZD
       !
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
       if (cutcell.gt.0.and.(GhostMethod.eq.1.or.GhostMethod.eq.2)) THEN
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
       icx = nmaxddb
       icy = 1
       call timer_start(timer_2nduzd, gdp)
       call z_uzd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx         , &
                & icy       ,nsrc      ,kcs       ,kcu45     ,kcscut      , &
                & kcu       ,kfu       ,kfuz0     ,kfumn0    ,kfumx0      , &
                & kfv       ,kfvz0     ,kfvmn0    ,kfvmx0    ,dzv0        , &
                & kfs       ,kfsz0     ,kfsmn0    ,kfsmx0    , &
                & u0        ,v0        ,w1        ,hu        ,hv          ,dzu0        ,dzs0          , &
                & guu       ,gvv       ,gvu       ,guv       ,gsqs        , &
                & gud       ,gvd       ,guz       ,gvz       ,gsqiu       , &
                & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt      , &
                & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5       , &
                & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10      , &
                & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,circ2d(1,1) ,circ3d(1,1,1) , &
                & vicuv     ,vnu2d     ,vicww     ,tgfsep    ,dps         , &
                & dfu       ,deltau    ,tp        ,rlabda    ,fxw         ,wsbodyu       , &
                & drhodx    ,wsu       ,taubpu    ,taubsu    ,rxx         , &
                & rxy       ,windu     ,patm      ,fcorio    ,p0          , &
                & ubrlsu    ,pship     ,diapl     ,rnpl      ,cfurou      , &
                & u1        ,s0        ,dpu       ,qxk       ,qyk         , &
                & norow     ,nocol     ,irocol(1, 1)         ,nst         ,umean       , &
                & nmax      ,mmax      ,nmaxus    ,u0INTv    ,v0INTu     ,&
                & crbc(1,1) ,ustokes   ,xcor      ,ycor      ,gdp         )
       call timer_stop(timer_2nduzd, gdp)
       call timer_stop(timer_uzd, gdp)
!
       if (QUARTERdt) then      
          call postpr_hdt(nst, gdp)          
       endif
       if (cutcell.gt.0.and.(GhostMethod.eq.1.or.GhostMethod.eq.2))  then
          if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,0,nlb,nub,mlb,mub,nmlb,nmub, gdp) !set kfs,kfu,kfv NOT active in ghost points. Only s0,v1,v0 and u0 should be reset
       endif
       !
       ! CHECK FOR FLOODING AND DRYING IN "V" POINTS
       !
       flood = .true.
       idry  = 0
       icx   = 1
       icy   = nmaxddb
       call timer_start(timer_checku, gdp)
       call z_checku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                   & flood     ,kfv       ,kcs       ,kcv       ,kspv      , &
                   & kfvmn0    ,kfvmx0    ,hv        ,s0        ,dpv       , &
                   & dps       ,vmean     ,kfvz0     ,kfsmn0    ,kfsmx0    , &
                   & v0        ,dzv0      ,zk        ,agvv      ,gdp       )
       call timer_stop(timer_checku, gdp)
       if (periodSURFACE) then  ! prescribe periodic velocity components.
          ! It is actually not necessary to recompute u1 if implicit periodic, since it should have been converged from uzd up to given tolerance.
          ! v1 could just be copied from v0 since nothing changed but its not much cheaper so I just call the subroutine
          if (.not.TRANSVperIMPL) then
             if (PERIODalongM==1) then 
                call velocityPERIOD(u1,v1,icx,nlb,nub,mlb,mub,kmax, gdp)    
             else
                call velocityPERIOD(v1,u1,icx,nlb,nub,mlb,mub,kmax, gdp)   
             endif
          endif
          ! call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax) not needed nothing changed
       endif
       !
       if (cutcell>0 .and. .not.onlyUZD) then  !skip sud and pre-sud stuff 
          !
          ! cutcell modification
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
       endif
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
       !
       ! Computation of V1 and S1, i.e. evaluation of coupled momentum and
       ! continuity equation for one half time step
       !
22222  continue
       call timer_start(timer_sud, gdp)
       !
       gdp%dd%suditer = 0
       icx = 1
       icy = nmaxddb
       call timer_start(timer_2ndsud, gdp)
       call z_sud(j         ,nmmaxj    ,nmmax     ,kmax      ,nmax      , &                          
                & mmax      ,nsrc      ,nst       ,icx       ,icy       , &                          
                & flood     ,nocol     ,irocol(1, norow + 1) ,mnksrc    ,kfsmx0    , &               
                & kfv       ,kfu       ,kfs       ,kcs       ,kcv       , &                          
                & kfvz0     ,kfuz0     ,kfsz0     ,kspv      ,kcv45     , &                          
                & kcscut    ,kfvmn0    ,kfsmn0    ,kfvmx0    ,kfumn0    , &                          
                & kfumx0    ,thick     ,circ2d(1, norow + 1) ,circ3d(1, 1, norow + 1) ,s0        , & 
                & s1        ,v0        ,v1        ,u1        ,w1        , &                          
                & qyk       ,qxk       ,qzk       ,gvv       ,guu       , &                          
                & gvu       ,guv       ,gvd       ,gud       ,gvz       , &                          
                & guz       ,gsqiv     ,gsqs      ,disch     ,vmdis     , &                          
                & dismmt    ,vmean     ,evap      ,hv        ,hu        ,dps       , &               
                & dpv       ,dzs0      ,dzv0      ,dzu0      ,wrka1     , &                          
                & wrka2     ,wrka3     ,wrka4     ,wrka5     ,wrka6     , &                          
                & wrka7     ,wrka8     ,wrka16    ,wrkb1     ,wrkb2     , &                          
                & wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     ,wrkb7     , &                          
                & wrkb8     ,wsv       ,taubpv    ,taubsv    ,vicuv     , &                          
                & vnu2d     ,vicww     ,ryy       ,rxy       ,windv     , &                          
                & tp        ,rlabda    ,dfv       ,deltav    ,fyw       ,wsbodyv      , &            
                & patm      ,fcorio    ,tgfsep    ,drhody    ,zk        , &                          
                & p0        ,crbc(1, norow + 1)   ,idry      ,porosv    ,ubrlsv       , &            
                & pship     ,diapl     ,rnpl      ,cfvrou    ,precip    , &                          
                & vstokes   ,GHOSTv1   ,GHOSTu1   ,yG_L      ,xG_L      , &                          
                & agvv      ,aguu      ,agsqs     ,nmaxus    , &                                     
                & u1INTv    ,xcor      ,ycor      , &
                & gdp       )                                                                        
!                                                                                                    
       if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN                                 
          do k =1,nmmax
            call nm_to_n_and_m(k, n, m, gdp)
            if ((doNOTdebugGHOSTS==1).AND.(GHOSTs1(N,M).EQ.1.OR.GHOSTU1(N,M).EQ.1.OR.GHOSTV1(N,M).EQ.1)) CYCLE
            write(9891996,'(2i6,15f21.15)') nst,k,s0(k),u0(k,1),v0(k,1),s1(k),u1(k,1),v1(k,1),hu(k),hv(k),dpu(k),dpv(k),dps(k)
          enddo
       endif
       !
       if (cutcell.gt.0.and.GhostMethod.le.1) then 
          !SBAGLIATO: DEVO METTERE kfu e kfv =0 solo se il edge è tutto asciutto! altrimenti non mi funzia il checking del wetting and drying
          if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,0,nlb,nub,mlb,mub,nmlb,nmub, gdp) !DA OTTIMIZZARE: THIS IS NEEDED ONLY TO DEACTIVATE GHOST VELOCITY POINTS (S1 CAN STAY ACTIVE) NOT TO GET STUCK ON A DEADLOCK WITH IDRY=1, SINCE drycheck keep finding s1(n,m)=dps(n,m). an option to remove this at all and also the reactivation below is to replace s1(nm)<= with < in checkdry.
       endif
!
       call timer_stop(timer_2ndsud, gdp)
       call timer_stop(timer_sud, gdp)
       !
       ! Check for drying in waterlevel points in the X-direction
       ! NOTE: Z_DRYCHK is called with arrays KFUZ0, KFVZ0 and KFSZ1. 
       !       KFUZ0 and KFVZ0 are the arrays corresponding to the original geometry (S0)
       !       KFSZ1 is to be determined, corresponding to the new geometry (S1)
       !
       icx = nmaxddb
       icy = 1
       call timer_start(timer_drychk, gdp)
       !if (cutcell==2) call drychk_cc(kfs_cc,   poros    ,agsqs   ,s1     ,dps    ,dpL   ,dpH    ,nmmax   ,nmlb, nmub ,zmodel)     
       call z_drychk(idry      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                   & nfltyp    ,icx       ,icy       ,kfu       ,kfv       , &
                   & kfs       ,kcs       ,kfuz0     ,kfvz0     ,kfsz1     , &
                   & kfsmin    ,kfsmn0    ,kfsmax    ,kfsmx0    ,s1        , &
                   & r0        ,dps       ,qxk       ,qyk       ,w1        , &
                   & lstsci    ,dzs1      ,zk        ,nst       ,gdp       )
       call timer_stop(timer_drychk, gdp)
       !
       ! Update the layer geometry in V-velocity points based on the new water levels S1
       ! NOTE: Z_DRYCHKU is called with arrays KFUZ1, KFVZ1 and KFSZ1. 
       !       KFSZ1/DZS1 have just been determined, corresponding to the new geometry (S1)
       !       KFVZ1/DZV1 are to be determined, corresponding to the new geometry (S1)
       !       KFUZ1/DZU1 were updated to the new geomety (half a time step ago)
       !                  after the first half time step
       !
       icx = 1
       icy = nmaxddb
       call z_drychku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                    & kcs       ,kfv       ,kcv       ,kspv      ,kfsmax    , &
                    & kfsmin    ,kfsz1     ,kfvz1     ,kfvmin    ,kfvmn0    ,kfvmax    , &
                    & kfvmx0    ,hv        ,s1        ,dpv       ,dps       , &
                    & vmean     ,v0        ,v1        ,dzv0      ,dzv1      , &
                    & dzs1      ,zk        ,kfsmx0    ,gvv       ,qyk       , &
                    & agvv      ,gdp       )
       !
       ! If requested by keyword ZTBML 
       ! (Z-model TauBottom Modified Layering)
       ! --> modify the near-bed layering to obtain smoother bottom shear stress representation in z-layer models
       !
       if (ztbml) then
          !
          ! Call with modify_dzsuv(1) = 1 and modify_dzsuv(3) = 1, to modify dzs1 and dzv1
          ! (and possibly R0 and qzk)
          !
          modify_dzsuv(1) = 1
          modify_dzsuv(2) = 0
          modify_dzsuv(3) = 1
          ztbml_upd_r1    = .false.
          call z_taubotmodifylayers(nmmax  ,kmax     ,lstsci       ,icx     ,icy          , & 
                                  & kfs    ,kfsmin   ,kfsmax       ,dps     ,dzs1         , &
                                  & kfu    ,kfumin   ,kfumax       ,dpu     ,dzu1         , &
                                  & kfv    ,kfvmin   ,kfvmax       ,dpv     ,dzv1         , &
                                  & r0     ,s0       ,s1           ,zk      ,modify_dzsuv , &
                                  & hdt    ,gsqs     ,kfsmx0       ,qzk     ,umean        , &
                                  & vmean  ,dzs0     ,ztbml_upd_r1 ,gdp      )
       endif
!      set ghost velocity points with active edge=0 to zero vel and kf=1. Reset dps and s1., dpu,dpv, qxk,Umean, qyk,Vmean
       if (cutcell.eq.2)   then
          if (changeKFUVcut) call kfsuv_ghost(Umean,Vmean,qxk,qyk,hu,hv,dpu,dpv,gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,s0,u0,v0,dps,mmax,nmax,kmax,nmaxus,0,1,nlb,nub,mlb,mub,nmlb,nmub, gdp) 
       endif
       !
       ! Compute Volume and Areas to be used in routines that computes 
       ! the transport of matter (consistency with WAQ)
       !
       call timer_start(timer_comvol, gdp)
       call comvol(nmmax     ,kmax      ,zmodel    ,kcs       ,kcv       , &
                 & thick     ,gvv       ,gsqs      ,dps       ,s1        , &
                 & dzs1      ,dzv0      ,hv        ,porosv    ,volum1    , &
                 & areau     ,agvv      ,agsqs     ,kfs       ,gdp       )
       call timer_stop(timer_comvol, gdp)
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
       !
       if (nfltyp/=0) then
          !
          ! If waterlevel is below bottom then isolate waterlevel point by setting
          ! the surrounding velocities to zero and repeat the computation of Z_SUD
          !
          if (nhystp==d3dflow_build_adi_zeta .or.                               &
            & (nhystp==noneighbors .and. idry==1)) goto 22222
       !
       ! DD code added end
       !
       endif
    !
    ! END OF COMPUTATION OF STAGE 2 FOR ADI METHOD
    !
    endif
    if (cutcell.eq.2)   then
       call PLIC_VOF_STEP(gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,dps,dpU,dpV,xcor,ycor,alfas,&
                      lunscr,lundia,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,drycrt,&
                      thick,guu,gvv,hu,hv,porosu,porosv,qxk,qyk,Umean,Vmean,stage,kfumn0,kfvmn0,kfumx0,kfvmx0,gdp%d%ddbound,nmmax,Zmodel, gdp)
    endif
end subroutine z_adi
