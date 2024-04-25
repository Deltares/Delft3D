subroutine z_predict_nhfull(j         ,nmmaxj    ,nmmax     ,kmax      , &
                          & mmax      ,nmax      ,nfltyp    ,nst       , &
                          & nocol     ,norow     ,nsrc      ,dismmt    ,irocol    , &
                          & mnksrc    ,kfu       ,kfv       ,kfs       ,            &
                          & kspu      ,kspv      ,kadu      ,kadv      ,kcs       , &
                          & kcu       ,kcv       ,kfsmn0    ,kfsmx0    , &
                          & kfumn0    ,kfumx0    ,kfvmn0    , &
                          & kfvmx0    ,kfuz0     ,kfvz0     ,kfsz0     ,kcu45     , &
                          & kcv45     ,kcscut    ,s0        ,w0        , &
                          & u0        ,u1        ,v0        ,v1        ,hu        , &
                          & hv        ,thick     ,umean     ,ubrlsu    ,ubrlsv    , &
                          & vmean     ,dpu       ,dpv       ,dps       ,dzu0      , &
                          & dzv0      ,dzs0      ,qxk       ,qyk       ,circ2d    , &
                          & circ3d    ,drhodx    ,drhody    ,disch     ,umdis     , &
                          & vmdis     ,wsu       ,wsv       ,dfu       ,dfv       , &
                          & deltau    ,deltav    ,tp        ,rlabda    ,wsbodyu   , &
                          & wsbodyv   ,fxw       ,fyw       ,gud       ,gvd       , &
                          & guu       ,guv       ,gvv       ,gvu       ,guz       , &
                          & gvz       ,gsqs      ,gsqiu     ,gsqiv     ,taubpu    , &
                          & taubpv    ,taubsu    ,taubsv    ,vicuv     ,vnu2d     , &
                          & vicww     ,rxx       ,rxy       ,ryy       ,windu     , &
                          & windv     ,patm      ,fcorio    ,tgfsep    ,wrkb1     , &
                          & wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     ,wrkb6     , &
                          & wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    ,wrkb11    , &
                          & wrkb12    ,wrkb13    ,wrkb14    , &
                          & zk        ,p0        ,crbc      , &
                          & pship     ,diapl     ,rnpl      ,cfurou    ,cfvrou    , &
                          & precip    ,gdp       )

!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: ADI performs one time step of the Alternating
!              Direction Implicit (ADI) method
! Method used: A.D.I. method is used.
!              Upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
!
!!--pseudo code and references--------------------------------------------------
!
! Written by Sebastian Ullmann
!
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
    real(fp)     , dimension(:,:)        , pointer :: ustokes
    real(fp)     , dimension(:,:)        , pointer :: vstokes
    include 'flow_steps_f.inc'
!
! Global variables
!
    integer                                               :: j       !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                               :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nfltyp  !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nmmax   !  Description and declaration in dimens.igs
    integer                                               :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                               :: nocol   !  Description and declaration in esm_alloc_int.f90
    integer                                               :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                               :: nst     !!  Time step number
    integer, dimension(5, norow + nocol)                  :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                           :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfsmn0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfsmx0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfumn0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfumx0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfvmn0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)             :: kfvmx0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)     :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)     :: kspv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,   kmax)     :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,   kmax)     :: kadv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kcscut  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kcu45   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kcv45   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfsz0   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfuz0   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)       :: kfvz0   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)         :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)         :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(12, norow+nocol)                  :: crbc    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow + nocol)                 :: circ2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: deltav  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dfv     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)          :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: dpv     !  Description and declaration in esm_alloc_real.f90
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
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: precip  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubpv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: taubsv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: tgfsep  !!  Water elev. induced by tide gen.force. Internal work array WRKB17 used
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: vmean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: windu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: windv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)            :: wsbodyv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)    :: w0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)  :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzs0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzu0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: dzv0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      :: p0      !  Description and declaration in esm_alloc_real.f90
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
    real(fp), dimension(kmax)                             :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                           :: zk
    real(fp), dimension(kmax, 2, norow + nocol)           :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                             :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                             :: umdis   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                             :: vmdis   !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrc)                         :: dismmt  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer :: idummy
    integer :: icx
    integer :: icy
    integer :: idry
    integer :: nhystp
    integer :: nmaxddb
    logical :: flood   ! Flag for activating flooding part of checku subroutine
!
!! executable statements -------------------------------------------------------
!
    ustokes             => gdp%gdtrisol%ustokes
    vstokes             => gdp%gdtrisol%vstokes
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    !
    ! Calculate HU and set KFU = 0 for HU < HTRSH (.5*DRYFLC)
    ! Calling checku is necessary because the hu calculation
    ! is performed on the full computational domain (kcu=1)
    ! only the wet points (kfu=1) and flooding is allowed.
    !
    idry  = 0
    flood = .true.
    icx   = nmaxddb
    icy   = 1
    call z_checku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                & flood     ,kfu       ,kcs       ,kcu       ,kspu      , &
                & kfumn0    ,kfumx0    ,hu        ,s0        ,dpu       , &
                & dps       ,umean     ,kfuz0     ,kfsmn0    ,kfsmx0    , &
                & u0        ,dzu0      ,zk        ,gdp       )
    !
    ! Calculate HV and set KFV = 0 for HV < HTRSH (.5*DRYFLC)
    ! Calling checku is necessary because the hv calculation
    ! is performed on the full computational domain (kcv=1) instead of
    ! only the wet points (kfv=1) and flooding is allowed.
    !
    idry  = 0
    flood = .true.
    icx   = 1
    icy   = nmaxddb
    call z_checku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                & flood     ,kfv       ,kcs       ,kcv       ,kspv      , &
                & kfvmn0    ,kfvmx0    ,hv        ,s0        ,dpv       , &
                & dps       ,vmean     ,kfvz0     ,kfsmn0    ,kfsmx0    , &
                & v0        ,dzv0      ,zk        ,gdp       )
    !
    ! Computation of U1, i.e. evaluate momentum equation for one timest
    !     calculate hu and set kfu = 0 for hu < htrsh (.5*dryflc)
    !                          kfu = 1 for hu > trsh
    !           
    call timer_start(timer_uzd, gdp)
    gdp%dd%uzditer = 0
    icx            = nmaxddb
    icy            = 1
    call z_uzd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & icy       ,nsrc      ,kcs       ,kcu45     ,kcscut    , &
             & kcu       ,kfu       ,kfuz0     ,kfumn0    ,kfumx0    , &
             & kfv       ,kfvz0     ,kfvmn0    ,kfvmx0    ,dzv0      , &
             & kfs       ,kfsz0     ,kfsmn0    ,kfsmx0    , &
             & u0        ,v0        ,w0        ,hu        ,hv        ,dzu0      ,dzs0      , &
             & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
             & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
             & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt    , &
             & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
             & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
             & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,circ2d(1, 1) , circ3d(1, 1, 1) , &
             & vicuv     ,vnu2d     ,vicww     ,tgfsep    ,dps       , &
             & dfu       ,deltau    ,tp        ,rlabda    ,fxw       ,wsbodyu   , &
             & drhodx    ,wsu       ,taubpu    ,taubsu    ,rxx       , &
             & rxy       ,windu     ,patm      ,fcorio    ,p0        , &
             & ubrlsu    ,pship     ,diapl     ,rnpl      ,cfurou    , &
             & u1        ,s0        ,dpu       ,qxk       ,qyk       , &
             & norow     ,nocol     ,irocol(1, 1)         ,nst       ,umean     , &
             & crbc(1,1) ,ustokes   ,gdp       )
    !
    ! Computation of V1, i.e. evaluate momentum equation for one half timest
    !     calculate hv and set kfv = 0 for hv < htrsh (.5*dryflc)
    !                              = 1 for hv > trsh
    !
    gdp%dd%uzditer = 0
    icx            = 1
    icy            = nmaxddb
    !
    call z_uzd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & icy       ,nsrc      ,kcs       ,kcv45     ,kcscut    , &
             & kcv       ,kfv       ,kfvz0     ,kfvmn0    ,kfvmx0    , &
             & kfu       ,kfuz0     ,kfumn0    ,kfumx0    ,dzu0      , &
             & kfs       ,kfsz0     ,kfsmn0    ,kfsmx0    , &
             & v0        ,u0        ,w0        ,hv        ,hu        ,dzv0      ,dzs0      , &
             & gvv       ,guu       ,guv       ,gvu       ,gsqs      , &
             & gvd       ,gud       ,gvz       ,guz       ,gsqiv     , &
             & disch     ,vmdis     ,kspv      ,mnksrc    ,dismmt    , &
             & wrkb1     ,wrkb2     ,wrkb3     ,wrkb4     ,wrkb5     , &
             & wrkb6     ,wrkb7     ,wrkb8     ,wrkb9     ,wrkb10    , &
             & wrkb11    ,wrkb12    ,wrkb13    ,wrkb14    ,circ2d(1, norow+1) , circ3d(1, 1, norow+1) , &
             & vicuv     ,vnu2d     ,vicww     ,tgfsep    ,dps       , &
             & dfv       ,deltav    ,tp        ,rlabda    ,fyw       ,wsbodyv   , &
             & drhody    ,wsv       ,taubpv    ,taubsv    ,ryy       , &
             & rxy       ,windv     ,patm      ,fcorio    ,p0        , &
             & ubrlsv    ,pship     ,diapl     ,rnpl      ,cfvrou    , &
             & v1        ,s0        ,dpv       ,qyk       ,qxk       , &
             & nocol     ,norow     ,irocol(1, norow + 1) ,nst       ,vmean     , &
             & crbc(1,norow + 1)    ,vstokes   ,gdp       )
    call timer_stop(timer_uzd, gdp)
    !
    ! DD code added:
    !
    !
    ! Synchronize on Dry Point
    if (nfltyp == 0) then
       nhystp = nxtdry(d3dflow_check_adi_dry, 0, gdp)
    else
       nhystp = nxtdry(d3dflow_check_adi_dry, idry, gdp)
    endif
end subroutine z_predict_nhfull
