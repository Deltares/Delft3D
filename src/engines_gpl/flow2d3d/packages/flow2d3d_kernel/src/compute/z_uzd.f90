subroutine z_uzd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
               & icy       ,nsrc      ,kcs       ,kcs45     ,kcscut    , &
               & kcu       ,kfu       ,kfuz0     ,kfumn0    ,kfumx0    , &
               & kfv       ,kfvz0     ,kfvmn0    ,kfvmx0    ,dzv0      , &
               & kfs       ,kfsz0     ,kfsmn0    ,kfsmx0    , &
               & u0        ,v0        ,w0        ,hu        ,hv        ,dzu0      ,dzs0      , &
               & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
               & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
               & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt    , &
               & aak       ,bbk       ,cck       ,ddk       ,bdx       , &
               & bux       ,bdy       ,buy       ,buxuy     ,buxdy     , & 
               & bdxuy     ,bdxdy     ,uvdwk     ,vvdwk     ,circ2d    ,circ3d    , &
               & vicuv     ,vnu2d     ,vicww     ,tgfsep    ,dps       , &
               & dfu       ,deltau    ,tp        ,rlabda    ,fxw       ,wsbodyu   , &
               & drhodx    ,wsu       ,taubpu    ,taubsu    ,rxx       , &
               & rxy       ,windsu    ,patm      ,fcorio    ,p0        , &
               & ubrlsu    ,pship     ,diapl     ,rnpl      ,cfurou    , &
               & u1        ,s0        ,dpu       ,qxk       ,qyk       , &
               & norow     ,nocol     ,irocol    ,nst       ,umean     , &
               & crbc      ,ustokes   ,gdp       )
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
! This subroutine evaluates/solves the horizontal momentum equation for a Z-layer
! vertical grid. It is called at each half time step for the case of a
! hydrostatic flow model or in the prediction step of the full non-hydrostatic method
! (full time step).
! For the spatial and temporal dsicretisation there are different options.
! Reference : A.O.I. - scheme (G.S. Stelling and
! J.J. Leendertse, Approximation of Convective
! Processes by Cyclic AOI Methods, Proceedings,
! ASCE Conference, Tampa, 1991).
!
! Z_UZD has the following options in the evaluation: 
!
! For the horizontal advection:
! - Horizontal Advection: depending on the flag MOMSOL. Options: 
!    implicit, upwind scheme            (IUPW)
!    explicit, multi-directional upwind (MDUE)
!    implicit, multi-directional upwind (MDUI)
!    explicit, finite volume approach, conservation of momentum (FINVOL)
!    explicit, combination of conservation of momentum and energy following
!              Stelling and Duinmeyer   (FLOOD)
! The vertical viscosity term is integrated implicitly
! - Horizontal Viscosity is integrated implicitly, along Z-planes
! - Vertical Advection is integrated with a flux formulation:
!              implicitly, central scheme 
!              explicitly, flux formulation
! - Vertical viscosity is always integrated implicitly.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'flow_steps_f.inc'
    !    
    real(fp)                     , pointer :: hdt
    real(fp)                     , pointer :: ag
    integer                      , pointer :: iro
    integer                      , pointer :: irov
    real(fp)                     , pointer :: rhow
    real(fp)                     , pointer :: vicmol
    character(8)                 , pointer :: dpsopt
    real(fp)                     , pointer :: eps
    integer                      , pointer :: lundia
    real(fp)                     , pointer :: drycrt
    real(fp)                     , pointer :: dryflc
    real(fp)                     , pointer :: gammax
    integer                      , pointer :: ibaroc
    logical                      , pointer :: cstbnd
    logical                      , pointer :: old_corio
    character(6)                 , pointer :: momsol
    logical                      , pointer :: slplim
    real(fp)                     , pointer :: rhofrac
    logical                      , pointer :: wind
    logical                      , pointer :: wave
    logical                      , pointer :: roller
    logical                      , pointer :: xbeach
    integer                      , pointer :: nh_level
    logical                      , pointer :: nonhyd
    real(fp)                     , pointer :: dzmin
    integer                      , pointer :: mfg
    integer                      , pointer :: nfg
    integer                      , pointer :: no_dis
    logical                      , pointer :: nf_src_mom
    real(fp), dimension(:,:,:)   , pointer :: nf_src_momu
    real(fp), dimension(:,:,:)   , pointer :: nf_src_momv
    real(fp)                     , pointer :: momrelax
!
! Global variables
!
    integer                                                              :: icx     !  Increment in the X-dir., 
                                                                                    !  if icx=NMAX then computation proceeds in the X-dir., 
                                                                                    !  if icx=1    then computation proceeds in the Y-dir.
    integer                                                              :: icy     !  Increment in the Y-dir. (see icx)
    integer                                                              :: j       !  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                                    !  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                              :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                              :: nmmax   !  Description and declaration in dimens.igs
    integer                                                              :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                                 , intent(in) :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in) :: nst     !  Time step number
    integer     , dimension(7, nsrc)                        , intent(in) :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer                                                              :: nocol   !  Description and declaration in esm_alloc_int.f90
    integer                                                              :: norow   !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(5, norow)                                    :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfsmn0  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfsmx0  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfumn0  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfumx0  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfvmn0  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfvmx0  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)  , intent(in) :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kcs45
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: kcscut  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kfsz0   !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kfuz0   !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: kfvz0   !  Description and declaration in esm_alloc_int.f90
    real(fp)    , dimension(4, norow)                                    :: circ2d  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(kmax, 2, norow)                              :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 3)                    :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(12, norow)                                   :: crbc    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(prec)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: guz     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: tgfsep  !  Water elevation induced by tide generating forces
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                       :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)  , intent(in) :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)  , intent(in) :: w0      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)             :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: aak     !  Internal work array, lower diagonal tridiagonal matrix, implicit coupling
                                                                                    !  of layer velocity in (N,M,K) with layer velocity in (N,M,K-1)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bbk     !  Internal work array, coefficient layer velocity in (N,M,K) implicit part
                                                                                    !  with layer velocity in (N+1,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: cck     !  Internal work array, upper diagonal tridiagonal matrix, implicit coupling
                                                                                    !  of layer velocity in (N,M,K) with layer velocity in (N,M,K+1)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: ddk     !  Internal work array, diagonal space at (N,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bdx     !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N,M-1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bdy     !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N-1,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bux     !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N,M+1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: buy     !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N+1,M,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: buxuy   !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N+1,M+1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: buxdy   !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N-1,M+1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bdxuy   !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N+1,M-1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: bdxdy   !  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                                    !  with layer velocity in (N-1,M-1,K)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: drhodx  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: dzs0    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: dzu0    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: dzv0    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: p0      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: ustokes !  Description and declaration in trisol.igs
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: v0      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: uvdwk   !  Internal work array for Jac.iteration
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                 :: vvdwk   !  Internal work array for Jac.iteration
    real(fp)    , dimension(nsrc)                           , intent(in) :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrc)                           , intent(in) :: umdis   !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrc)                           , intent(in) :: dismmt  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer            :: ddb
    integer            :: iada
    integer            :: iadc
    integer            :: ibf
    integer            :: ibl
    integer            :: ic
    integer            :: icxy    ! MAX value of ICX and ICY 
    integer            :: idifc
    integer            :: idifd
    integer            :: idifu
    integer            :: idis
    integer            :: isrc
    integer            :: iter
    integer            :: itr
    integer            :: k
    integer            :: kdo
    integer            :: kenm
    integer            :: kfad
    integer            :: kk
    integer            :: kkmax
    integer            :: kmaxtl
    integer            :: kmin
    integer            :: kup
    integer            :: maskval
    integer            :: maxk
    integer            :: mf
    integer            :: ml
    integer            :: n
    integer            :: nmf
    integer            :: nml
    integer            :: nmsta
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmdis
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    integer            :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    integer            :: nhystp
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: adza
    real(fp)           :: adzb
    real(fp)           :: adzc
    real(fp)           :: area
    real(fp)           :: bdmwrp
    real(fp)           :: bdmwrs
    real(fp)           :: bi
    real(fp)           :: cbot
    real(fp)           :: cuu
    real(fp)           :: cvv
    real(fp)           :: dbk
    real(fp)           :: ddx
    real(fp)           :: ddy
    real(fp)           :: ddza
    real(fp)           :: ddzb
    real(fp)           :: ddzc
    real(fp)           :: dgeta
    real(fp)           :: dgvnm
    real(fp)           :: dpsmax
    real(fp)           :: drythreshold
    real(fp)           :: dux
    real(fp)           :: duy
    real(fp)           :: dz
    real(fp)           :: dzdo
    real(fp)           :: dzup
    real(fp)           :: eps1
    real(fp)           :: facmax
    real(fp)           :: ff
    real(fp)           :: fxwl     ! local, modified fxw
    real(fp)           :: geta
    real(fp)           :: getad
    real(fp)           :: getau
    real(fp)           :: gksi
    real(fp)           :: gksid
    real(fp)           :: gksiu
    real(fp)           :: gsqi
    real(fp)           :: hl
    real(fp)           :: hr
    real(fp)           :: hnm
    real(fp)           :: h0fac
    real(fp)           :: drytrsh
    real(fp)           :: hugsqs   ! HU(NM/NMD) * GSQS(NM) Depending on UMDIS the HU of point NM or NMD will be used 
    real(fp)           :: qwind
    real(fp), external :: redvic
    real(fp)           :: smax
    real(fp)           :: svvv
    real(fp)           :: thvert   ! theta coefficient for vertical advection terms
    real(fp)           :: timest
    real(fp)           :: trelaxi
    real(fp)           :: uuu
    real(fp)           :: uweir
    real(fp)           :: vih
    real(fp)           :: viznm
    real(fp)           :: viznmd
    real(fp)           :: vvvc     ! Tangential velocity component used in Coriolis term
    real(fp)           :: www
    real(fp)           :: wavg0
    real(fp)           :: wsbodyul ! local, modified wsbodyu
    real(fp)           :: wsul     ! local, modified wsu
    real(fp)           :: wsumax
    real(fp)           :: zz
    character(20)      :: errtxt
!
!! executable statements -------------------------------------------------------
!
    !
    eps            => gdp%gdconst%eps
    lundia         => gdp%gdinout%lundia
    drycrt         => gdp%gdnumeco%drycrt
    dryflc         => gdp%gdnumeco%dryflc
    gammax         => gdp%gdnumeco%gammax
    hdt            => gdp%gdnumeco%hdt
    ibaroc         => gdp%gdnumeco%ibaroc
    cstbnd         => gdp%gdnumeco%cstbnd
    old_corio      => gdp%gdnumeco%old_corio
    momsol         => gdp%gdnumeco%momsol
    slplim         => gdp%gdnumeco%slplim
    ag             => gdp%gdphysco%ag
    iro            => gdp%gdphysco%iro
    irov           => gdp%gdphysco%irov
    rhofrac        => gdp%gdphysco%rhofrac
    rhow           => gdp%gdphysco%rhow
    vicmol         => gdp%gdphysco%vicmol
    wind           => gdp%gdprocs%wind
    wave           => gdp%gdprocs%wave
    roller         => gdp%gdprocs%roller
    xbeach         => gdp%gdprocs%xbeach
    dpsopt         => gdp%gdnumeco%dpsopt
    nh_level       => gdp%gdnonhyd%nh_level
    nonhyd         => gdp%gdprocs%nonhyd
    dzmin          => gdp%gdzmodel%dzmin
    mfg            => gdp%gdparall%mfg
    nfg            => gdp%gdparall%nfg
    no_dis         => gdp%gdnfl%no_dis
    nf_src_mom     => gdp%gdnfl%nf_src_mom
    nf_src_momu    => gdp%gdnfl%nf_src_momu
    nf_src_momv    => gdp%gdnfl%nf_src_momv
    momrelax       => gdp%gdnfl%momrelax
    !
    drytrsh      = drycrt
    drythreshold = 0.1_fp * dryflc
    nm_pos       = 1
    !
    ! Flag for vertical advection set to 1.0 by default = Central implicit discretisation of 
    ! advection in vertical (0.0 means 1st order upwind explicit)
    ! 
    thvert = 1.0_fp
    !
    ! Determine umean again based on the velocities in the whole water column
    ! instead of only the velocities in the top layer(s) as in z_checku
    !
    ! needed in z_hormom_fls and in z_cucbp_nhfull for alpha-coefficient on open boundary
    !
    umean = 0.0_fp
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          hnm = 0.0_fp
          do k = kfumn0(nm), kfumx0(nm)
             umean(nm) = umean(nm) + u0(nm,k)*dzu0(nm,k)
             hnm       = hnm + dzu0(nm,k)
          enddo
          umean(nm) = umean(nm) / max(hnm, 0.01_fp)
       else
       endif
    enddo
    !
    if (momsol /= 'mdue  ' .or. (nonhyd .and. nh_level == nh_full) ) then
       !
       ! Horizontal momentum using either ADI-scheme or in a fully non-hydrostatic computation
       !
       call timer_start(timer_uzd_ini, gdp)
       ddb      = gdp%d%ddbound
       icxy     = max(icx, icy)
       if (nonhyd .and. nh_level == nh_full) then
          !
          ! Fully non-hydrostatic mode: one whole timestep
          !
          timest = 2.0_fp * hdt
       else
          !
          ! Hydrostatic mode (ADI): half a time step
          !
          timest = hdt
       endif
       !
       !
       ! factor in maximum wave force 1/4 alpha rho g gammax**2 h**2 / tp /(sqrt(g h)
       ! = facmax * h**1.5/tp
       !
       facmax = 0.25_fp*sqrt(ag)*rhow*gammax**2
       !
       if (icx==1) then
          ff = -1.0_fp
       else
          ff = 1.0_fp
       endif
       kmaxtl = 0
       !
       ! Initialise arrays aak, bbk, cck and ddk for all (nm, k)
       !
       aak  = 0.0_fp
       bbk  = 1.0_fp/timest
       cck  = 0.0_fp
       ddk  = 0.0_fp
       bdx  = 0.0_fp
       bdy  = 0.0_fp
       bux  = 0.0_fp
       buy  = 0.0_fp
       bdxuy= 0.0_fp
       bdxdy= 0.0_fp
       buxuy= 0.0_fp
       buxdy= 0.0_fp
       !
       call timer_stop(timer_uzd_ini, gdp)
       !
       ! Fill the array elements and the right hand side
       !
       call timer_start(timer_uzd_rhs, gdp)
       do nm = 1, nmmax
          ndm        = nm - icy
          nmu        = nm + icx
          ndmu       = nm + icx - icy
          gksi       = gvu(nm)
          if (kfu(nm) == 1) then
             do k = kfumn0(nm), kfumx0(nm)
                if (kfuz0(nm, k)==1 .and. kcs(nm)*kcs(nmu)>0) then
                   !
                   ! BAROCLINIC PRESSURE, CORIOLIS, ATMOSPHERIC PRES. and TIDE GEN. FORCE
                   !
                   if (old_corio) then
                      vvvc = 0.25_fp * (v0(ndm,k)+v0(ndmu,k)+v0(nm,k)+v0(nmu,k))
                   else
                      !
                      ! Improved implementation Coriolis term for deep areas following
                      ! Kleptsova, Pietrzak and Stelling, 2009.
                      !
                      svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                      hl   = real(dps(nm),fp) + s0(nm)
                      hr   = real(dps(nmu),fp) + s0(nmu)
                      vvvc = (  (  v0(nm,  k)*hv(nm  )*kfv(nm  )      &
                      &          + v0(ndm, k)*hv(ndm )*kfv(ndm ))/max(drythreshold,hl)  & 
                      &       + (  v0(nmu, k)*hv(nmu )*kfv(nmu )      &
                      &          + v0(ndmu,k)*hv(ndmu)*kfv(ndmu))/max(drythreshold,hr)) &
                      &      / svvv
                   endif
                   ddk(nm, k) = u0(nm, k)/timest                                   &
                              & - ag*(s0(nmu) - s0(nm))/gksi                       &
                              & + ff*fcorio(nm)*vvvc                               &
                              & - ag/rhow*drhodx(nm, k)*kfsz0(nm, k)*kfsz0(nmu, k) &
                              & - (patm(nmu) - patm(nm))/(gksi*rhow)               &
                              & - (pship(nmu) - pship(nm))/(gksi*rhow)             &
                              & + ag*(tgfsep(nmu) - tgfsep(nm))/gksi               &
                              & - (p0(nmu, k) - p0(nm, k))*kfsz0(nmu,k)*kfsz0(nm,k)/(gksi*rhow)
                else
                   ddk(nm, k) = u0(nm,k)/timest
                endif
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_rhs, gdp)
       !
       ! Horizontal advection
       !
       call timer_start(timer_uzd_momsol, gdp)
       if (momsol == 'iupw  ') then
          !
          ! Horizontal advection (first order upwind implicit)
          !
          call z_hormom_iupw(nmmax     ,kmax      ,icx       ,icy       ,kcs     , &
                           & kcscut    ,kfu       ,kfuz0     ,kfumn0    ,kfumx0  , &
                           & kfvz0     ,u0        ,v0        ,guu       ,gvu     , &
                           & gvd       ,guz       ,gsqiu     ,bdx       ,bux     , &
                           & bbk       ,bdy       ,ddk       ,buy       ,gdp     )
       elseif (momsol == 'mdui  ') then
          !
          ! Multi-direction upwind implicit advection
          !
          call z_hormom_mdui(nmmax     ,kmax      ,icx       , &
                           & icy       ,kcs       ,kcs45     ,kcscut    , &
                           & kfu       ,kfuz0     ,kfumn0    ,kfumx0    , &
                           & kfvz0     ,kfsmn0    ,kfsmx0    , &
                           & u0        ,v0        ,hu        ,bdx       ,&
                           & bux       ,bdy       ,buy       ,buxuy     ,buxdy     , & 
                           & bdxuy     ,bdxdy     ,bbk       , &
                           & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                           & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                           & ddk       ,gdp       )
          !
       elseif (momsol == 'mdue  ') then
          !
          ! Advection determined explicitly using multi-directional upwind method
          ! Using the whole time step (fully non-hydrostatic mode)
          !
          call z_hormom_mdue(nmmax     ,kmax      ,icx       , &
                           & icy       ,kcs       ,kcs45     ,kcscut    , &
                           & kfu       ,kfuz0     ,kfumn0    ,kfumx0    , &
                           & kfvz0     ,kfsmn0    ,kfsmx0    , &
                           & u0        ,v0        ,hu        , &
                           & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                           & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                           & ddk       ,gdp       )
          !
       elseif (momsol == 'finvol') then
          !
          ! Finite volume approach for horizontal advection
          !
          ! Temporary switch for vertical advection of horizontal momentum
          ! THVERT = 0 means explicit 1st order upwind vertical advection
          !
          thvert = 0.0_fp
          ! 
          call z_hormom_finvol(nmmax     ,kmax      ,icx       ,icy       ,kcs       , &
                             & kfu       ,kcu       ,kfv       ,kfuz0     ,kfumn0    , &
                             & kfumx0    ,kfvz0     ,kfs       ,kfsz0     ,kfsmn0    , &
                             & kfsmx0    ,u0        ,v0        ,w0        ,dzs0      , &
                             & dzu0      ,dzv0      ,s0        ,guu       ,gvv       , &
                             & gvu       ,guv       ,gsqs      ,gud       ,gvd       , &
                             & guz       ,gvz       ,ddk       ,p0        ,gdp       )
          !
       elseif (momsol == 'flood ') then
          !
          ! Flooding solver for horizontal advection
          !
          ! Temporary switch for vertical advection of horizontal momentum (PvdP 2009)
          ! THVERT = 0 means explicit 1st order upwind vertical advection
          !
          !thvert = 0.0_fp
          !
          call z_hormom_fls(nmmax     ,kmax      ,icx       , &
                          & icy       ,kcs       ,kcs45     ,kcscut    , &
                          & kfu       ,kfuz0     ,kfumn0    ,kfumx0    ,kfv       , &
                          & kfvz0     ,kfsz0     ,kfsmn0    ,kfsmx0    ,kspu      , &
                          & u0        ,v0        ,hu        ,kfvmn0    ,kfvmx0    , &
                          & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                          & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                          & qxk       ,qyk       ,dzu0      ,dzv0      ,dzs0      , &
                          & ddk       ,umean     ,dps       ,s0        ,uvdwk     , &
                          & vvdwk     ,gdp       )
          !
          ! Reset arrays uvdwk and vvdwk which have been used as workarrays in z_hormom_fls
          !
          uvdwk = 0.0_fp
          vvdwk = 0.0_fp
       else
          !
          ! No correct advection option specified: error?
          !
       endif
       call timer_stop(timer_uzd_momsol, gdp)
       !
       ! 2D Weir not included
       ! energy loss for CDW (remainder structure untested yet)
       !
       call timer_start(timer_uzd_eloss, gdp)
       call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
                 & kspu      ,gvu       ,u0        ,v0        ,bbk       , &
                  & ubrlsu    ,diapl     ,rnpl      ,gdp       )
       call timer_stop(timer_uzd_eloss, gdp)
       !
       call timer_start(timer_uzd_stress, gdp)
       do nm = 1, nmmax
          nmu  = nm + icx
          kmin = kfumn0(nm)
          if (kfu(nm) == 1 .and. kcs(nm)*kcs(nmu) > 0) then
             kkmax = max(kmin, kfumx0(nm))
             !
             ! WIND AND BOTTOM FRICTION
             !
             qwind  = 0.0_fp
             bdmwrp = 0.0_fp
             bdmwrs = 0.0_fp
             !
             ! BOTTOM STRESS DUE TO FLOW AND WAVES
             !
             ! Special measures for smooth inundation
             !
             ! Estimate velocity on the basis of local equilibrium by solving u(nm)
             ! with an explicit procedure. So that high velocities can be avoided
             ! during flooding (kfu=1 and u=0.) in regions with steep topography.
             ! Velocity is estimated assuming critical flow over a short-crested
             ! weir.
             ! Gates are excluded
             !
             if (dpsopt == 'DP' .or. slplim) then
                if (kfu(nm) == 1 .and. abs(u0(nm,kmin)) <= 1.0e-15_fp .and. kspu(nm, 0) /= 4 .and. kspu(nm, 0) /= 10) then
                   !
                   ! cfurou(nm,1) contains u/u*
                   !
                   uweir      = sqrt( 2.0_fp/3.0_fp*ag*max(hu(nm),drytrsh) )
                   taubpu(nm) = uweir / (cfurou(nm,1)**2.0_fp)
                endif
             endif
             !
             ! Slope correction for steep slopes
             !
             if (slplim) then
                nmu    = nm + icx
                dpsmax = max(-dps(nm),-dps(nmu))
                if (s0(nm) < dpsmax) then
                   do k = kfumn0(nm), kfumx0(nm)
                      ddk(nm,k) = ddk(nm,k) - ag*(s0(nm)-dpsmax)/gvu(nm)
                   enddo
                elseif (s0(nmu) < dpsmax) then
                   do k = kfumn0(nm), kfumx0(nm)
                      ddk(nm,k) = ddk(nm,k) + ag*(s0(nmu)-dpsmax)/gvu(nm)
                   enddo
                endif
             endif
             !
             ! End of special measures for smooth inundation
             !
             ! WIND FRICTION AND BOTTOM STRESS DUE TO FLOW AND WAVES
             !
             ! Apply factor h0fac to reduce wind force from cells with small depth
             ! Note that the direction of windsu is opposite to what is expected
             ! This is due to the change in sign in windtogridc.f90
             !
             h0fac = 1.0_fp
             if (windsu(nm) < 0.0_fp .and.  s0(nm)+real(dps(nm),fp) < 2.0_fp*dryflc) then
                h0fac = (s0(nm)+real(dps(nm),fp)) / (2.0_fp*dryflc)
             elseif (windsu(nm) > 0.0_fp .and.  s0(nmu)+real(dps(nmu),fp) < 2.0_fp*dryflc) then
                h0fac = (s0(nmu)+real(dps(nmu),fp)) / (2.0_fp*dryflc)
             endif          
             qwind          = h0fac*windsu(nm) / max(dzu0(nm, kkmax),drytrsh)
             cbot           = taubpu(nm)
             bdmwrp         = cbot / max(dzu0(nm, kmin),drytrsh)
             bdmwrs         = taubsu(nm) / max(dzu0(nm, kmin),drytrsh)
             bbk(nm, kmin)  = bbk(nm, kmin) + bdmwrp
             ddk(nm, kkmax) = ddk(nm, kkmax) - qwind/rhow
             ddk(nm, kmin)  = ddk(nm, kmin) + bdmwrs
             !
             ! WAVE FORCE AT SURFACE
             !
             !
             ! physical limit to wsu
             !
             if (wave) then
                wsumax = facmax*hu(nm)**(1.5)/max(0.1_fp, 0.5_fp*(tp(nm)+tp(nmu)))
                wsul   = sign(min(abs(wsu(nm)), wsumax), wsu(nm))
                !
                ddk(nm, kkmax) = ddk(nm, kkmax) + wsul/(rhow*max(dzu0(nm, kkmax),drytrsh))
                !
                ! WAVE INDUCED BODY FORCE
                !
                if (roller .or. xbeach) then
                   fxwl = sign(min(abs(fxw(nm)), wsumax), fxw(nm))
                   do k = kfumn0(nm), kfumx0(nm)
                      ddk(nm, k) = ddk(nm, k) + fxwl/(rhow*hu(nm))
                   enddo
                else
                   wsbodyul = sign(min(abs(wsbodyu(nm)), wsumax), wsbodyu(nm))
                   do k = kfumn0(nm), kfumx0(nm)
                      ddk(nm, k) = ddk(nm, k) + wsbodyul/(rhow*hu(nm))
                   enddo
                endif
             endif
          endif
       enddo
       call timer_stop(timer_uzd_stress, gdp)
       !
       ! In case of 3D waves:
       ! Added shear stress in wave boundary layer due to streaming
       !
       call timer_start(timer_uzd_shrwav, gdp)
       if (wave .and. kmax>1) then
          call z_shrwav(nmmax     ,kmax      ,icx       ,dfu       ,deltau    , &
                      & tp        ,rlabda    ,hu        ,kfu       ,ddk       , &
                      & kfumn0    ,kfumx0    ,dzu0      ,gdp       )
       endif
       call timer_stop(timer_uzd_shrwav, gdp)
       !
       ! DISCHARGE ADDITION OF MOMENTUM
       !
       call timer_start(timer_uzd_dismmt, gdp)
       do isrc = 1, nsrc
          nm = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
          nmd = nm - icx
          if (dismmt(isrc) == 'Y' .and. disch(isrc) > 0.0_fp) then
             if (umdis(isrc) >= 0.0_fp) then
                nmdis  = nm
                hugsqs = hu(nm)*gsqs(nm)
             else
                nmdis  = nmd
                hugsqs = hu(nmd)*gsqs(nm)
             endif
             kk = mnksrc(3, isrc)
             if (kfu(nmdis) == 1) then
                if (kk == 0) then
                   do k = 1, kmax
                      bbk(nmdis, k) = bbk(nmdis, k) + disch(isrc)/hugsqs
                      ddk(nmdis, k) = ddk(nmdis, k) + umdis(isrc)*disch(isrc)      &
                                    & /hugsqs
                   enddo
                else
                   bbk(nmdis, kk) = bbk(nmdis, kk) + disch(isrc)                   &
                                  & /(dzu0(nmdis, kk)*gsqs(nm))
                   ddk(nmdis, kk) = ddk(nmdis, kk) + umdis(isrc)*disch(isrc)       &
                                  & /(dzu0(nmdis, kk)*gsqs(nm))
                endif
             endif
          endif
       enddo
       if (nf_src_mom) then
          !
          ! DISCHARGE ADDITION OF MOMENTUM FROM NEARFIELD
          !
          ! Instead of weighting the nearfield momentum with the volume added via the nearfield source,
          ! it is choosen to use a trelax, based on the time step.
          ! This should force the cell velocity to get the a value "growing" fast to the nearfield momentum value.
          !
          ! Only change ddk/bbk when the momentum source in the present direction is non-negative
          ! Inverse of the relaxation time:
          !
          trelaxi = 1.0 / (momrelax*2.0*hdt)
          do nm = 1, nmmax
             if (kfu(nm) == 1) then
                do k = kfumn0(nm), kfumx0(nm)
                   if (kfuz0(nm, k) == 1) then
                     do idis = 1, no_dis
                        if (icx == 1 ) then
                           if (comparereal(nf_src_momv(nm,k,idis), 0.0_fp) /= 0) then
                              ddk(nm,k) = ddk(nm,k) + nf_src_momv(nm,k,idis)*trelaxi
                              bbk(nm,k) = bbk(nm,k) + trelaxi
                           endif
                        else 
                           if (comparereal(nf_src_momu(nm,k,idis), 0.0_fp) /= 0) then
                              ddk(nm,k) = ddk(nm,k) + nf_src_momu(nm,k,idis)*trelaxi
                              bbk(nm,k) = bbk(nm,k) + trelaxi
                           endif
                        endif
                     enddo
                   endif
                enddo
             endif
          enddo
       endif
       call timer_stop(timer_uzd_dismmt, gdp)
       !
       ! VERTICAL ADVECTION AND VISCOSITY, IMPLICIT
       !
       call timer_start(timer_uzd_advdiffv, gdp)
       do nm = 1, nmmax
          if (kfu(nm)==1 .and. kfumx0(nm)>kfumn0(nm)) then
             kmaxtl = 1
             nmu    = nm + icx
             do k = kfumn0(nm), kfumx0(nm)
                if (kfuz0(nm, k) == 1) then
                   kfad = 0
                   kdo  = k - 1
                   kup  = k + 1
                   if (k == kfumn0(nm)) then
                      kfad = 1
                      kdo  = k
                   endif
                   if (k == kfumx0(nm)) then
                      kfad = -1
                      kup  = k
                   endif
                   !
                   ! Free slip between open and closed layers of a gate
                   !
                   iada = 1
                   iadc = 1
                   if (kspu(nm, 0) == 4 .or. kspu(nm, 0) == 10) then
                      iada = max(1 - (kspu(nm, kdo) + kspu(nm, k)), 0)
                      iadc = max(1 - (kspu(nm, k) + kspu(nm, kup)), 0)
                   endif
                   !
                   dzup = dzu0(nm, k) + dzu0(nm, kup)
                   dzdo = dzu0(nm, k) + dzu0(nm, kdo)
                   !
                   ! ADVECTION IN VERTICAL DIRECTION; W*DU/DZ
                   !
                   ! Is this correct at the surface when there are no neighbours?
                   !
                   if (thvert == 1.0_fp) then
                      !
                      ! Central implicit
                      !
                      maskval = min(kcs(nm), 2)*min(kcs(nmu), 2)
                      www     = 0.25_fp*abs(maskval)*(w0(nm, k - 1) + w0(nm, k) + w0(nmu, k - 1) + w0(nmu, k))
                      if (www < 0.0_fp) then
                         adza = -2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad))
                         adzc =  2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad)) + kfad*(1 + kfad)*www/dzup
                      else
                         adza = -2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad)) + abs(kfad)*( - 1 + kfad)*www/dzdo
                         adzc =  2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad))
                      endif
                      adza = iada*adza
                      adzc = iadc*adzc
                      adzb = -adza - adzc
                   else
                      !
                      ! First order upwind explicit
                      !
                      adza = 0.0_fp
                      adzc = 0.0_fp
                      adzb = 0.0_fp
                      !
                      ! downwards face
                      !
                      if (k > kfumn0(nm)) then
                          area  = guu(nm) * gvu(nm)
                          wavg0 = 0.5_fp * (w0(nm,k-1)+w0(nmu,k-1))
                          if (wavg0 > 0.0_fp) then
                              ddk(nm,k) = ddk(nm,k) + area*wavg0*u0(nm,k-1)
                          else
                              ddk(nm,k) = ddk(nm,k) + area*wavg0*u0(nm,k)
                          endif
                      endif
                      !
                      ! upwards face
                      !
                      if (k < kfumx0(nm)) then
                          area  = guu(nm) * gvu(nm)
                          wavg0 = 0.5_fp * (w0(nm,k)+w0(nmu,k))
                          if (wavg0 > 0.0_fp) then
                              ddk(nm,k) = ddk(nm,k) - area*wavg0*u0(nm,k)
                          else
                              ddk(nm,k) = ddk(nm,k) - area*wavg0*u0(nm,k+1)
                          endif
                      endif
                   endif
                   !
                   ! Subtitution in coefficients
                   !
                   aak (nm, k) = aak(nm, k) + adza
                   bbk (nm, k) = bbk(nm, k) + adzb
                   cck (nm, k) = cck(nm, k) + adzc
                   !
                   ! VERTICAL VISCOSITY
                   !
                   !
                   ! viznmd calculation 
                   ! restriction is moved from Z_TURCLO to here
                   !
                   viznmd = 0.25_fp * (2 - kfad*(1 + kfad))                 &
                          & * (2.0_fp*vicmol + redvic(vicww(nm , kdo), gdp) &
                          &                  + redvic(vicww(nmu, kdo), gdp))
                   !
                   ! viznm calculation 
                   ! restriction is moved from Z_TURCLO to here
                   !
                   viznm  = 0.25_fp * (2 + kfad*(1 - kfad))               &
                          & * (2.0_fp*vicmol + redvic(vicww(nm , k), gdp) &
                          &                  + redvic(vicww(nmu, k), gdp))
                   dz    = dzu0(nm, k)
                   !
                   ddza = iada * 2.0_fp * viznmd / (dzdo*dz)
                   ddzc = iadc * 2.0_fp * viznm  / (dzup*dz)
                   ddzb = -ddza - ddzc
                   !
                   ! subtitution in coefficients
                   !
                   aak(nm, k) = aak(nm, k) - ddza
                   bbk(nm, k) = bbk(nm, k) - ddzb
                   cck(nm, k) = cck(nm, k) - ddzc
                   !
                   ! Effect of waves, due to Stokes drift 
                   !
                  ddk(nm, k) = ddk(nm,k) - ( ddzc*(ustokes(nm,kup)-ustokes(nm,k  )) -   &
                                          &  ddza*(ustokes(nm,k  )-ustokes(nm,kdo))   )               
                endif
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_advdiffv, gdp)
       !
       ! HORIZONTAL VISCOSITY
       !
       call timer_start(timer_uzd_vih, gdp)
       if (irov>0) then
          !
          ! Stresses due to rigid walls
          !     implemented fully explicit
          !
          call z_vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                      & icy       ,kcs45     ,kcs       ,kfu       ,kfv       , &
                      & kfs       ,u0        ,v0        ,vicuv     ,vnu2d     , &
                      & gud       ,guu       ,gvd       ,gvu       ,gvz       , &
                      & ddk       ,rxx       ,rxy       ,kfuz0     ,kfvz0     , &
                      & kfsz0     ,kfumn0    ,kfumx0    ,gdp       )
       !
       ! for Crank Nicolson method: is computed here (implicitly for the whole time step)
       ! for fractional step method: is computed in z_cucnp (explicitly for the whole time step)
       !
       else
          do nm = 1, nmmax
             nmd  = nm - icx
             ndm  = nm - icy
             ndmd = nm - icx - icy
             nmu  = nm + icx
             num  = nm + icy
             numu = nm + icx + icy
             ndmu = nm + icx - icy
             numd = nm - icx + icy
             if (kfu(nm) == 1 .and. kcs(nm) > 0 .and. kcs(nmu) > 0) then
                do k = kfumn0(nm), kfumx0(nm)
                   if (kfuz0(nm, k) == 1) then
                      gksid = gvz(nm)
                      gksiu = gvz(nmu)
                      gksi  = gvu(nm)
                      getad = gud(ndm)
                      getau = gud(nm)
                      geta  = guu(nm)
                      idifd = kfvz0(ndm, k)*kfvz0(ndmu, k)*kfuz0(ndm, k)
                      idifu = kfvz0(nm , k)*kfvz0(nmu , k)*kfuz0(num, k)
                      idifc = abs(2 - abs(kcs(nm)))*abs(2 - abs(kcs(nmu)))
                      !
                      ! EDDY VISCOSITY FOR KMAX = 1, USING LAPLACE OPERATOR
                      ! (2*VIH*(D2U/DX2 + D2U/DY2)
                      ! Vih multiplied by 1.0 (instead of 0.5) because it is included here for the whole time step
                      !
                      vih        = vicuv(nm, k) + vicuv(nmu, k) + vnu2d(nm) + vnu2d(ndm)
                      bbk(nm, k) = bbk(nm, k)                    &
                                 & + 2.0_fp*vih/(gksid*gksiu)*idifc  &
                                 & +        vih/(getau*geta) *idifu  &
                                 & +        vih/(getad*geta) *idifd
                      bux(nm, k) = bux(nm, k) - vih/(gksiu*gksi)*idifc
                      bdx(nm, k) = bdx(nm, k) - vih/(gksid*gksi)*idifc
                      buy(nm, k) = buy(nm, k) - vih/(getau*geta)*idifu
                      bdy(nm, k) = bdy(nm, k) - vih/(getad*geta)*idifd
                   endif
                enddo
             endif
          enddo
       !
       endif
       call timer_stop(timer_uzd_vih, gdp)
       !
       ! BOUNDARY CONDITIONS
       !
       call timer_start(timer_uzd_bouncond, gdp)
       !
       ! For fully non-hydrostatic simulations the boundary conditions are handled in z_cucbp_nhfull
       !
       if (nonhyd .and. nh_level == nh_full) then
          call z_cucbp_nhfull(kmax      ,norow     ,icx       , &
                            & icy       ,irocol    ,kcs       ,kfu       , &
                            & kfumn0    ,kfumx0    ,s0        ,u0        , &
                            & hu        ,umean     ,guu       ,gvu       , &
                            & dzu0      ,circ2d    ,circ3d    ,dpu       , &
                            & aak       ,bbk       ,cck       ,ddk       , &
                            & crbc      ,gdp       )
       else
          !
          do ic = 1, norow
             !
             n   = irocol(1, ic)
             mf  = irocol(2, ic) - 1
             ml  = irocol(3, ic)
             ibf = irocol(4, ic)
             ibl = irocol(5, ic)
             nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
             nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
             !
             ! IMPLEMENTATION OF BOUNDARY CONDITIONS
             !
             if (kcu(nmf)==1 .and. kfu(nmf)==1) then
                if (ibf==3 .or. ibf==5 .or. ibf==6 .or. ibf==7) then
                   do k = kfumn0(nmf), kfumx0(nmf)
                      aak  (nmf, k) = 0.0_fp
                      bbk  (nmf, k) = 1.0_fp
                      bux  (nmf, k) = 0.0_fp
                      bdx  (nmf, k) = 0.0_fp
                      buy  (nmf, k) = 0.0_fp
                      bdy  (nmf, k) = 0.0_fp
                      buxuy(nmf, k) = 0.0_fp
                      bdxuy(nmf, k) = 0.0_fp
                      buxdy(nmf, k) = 0.0_fp
                      bdxdy(nmf, k) = 0.0_fp
                      cck  (nmf, k) = 0.0_fp
                      ddk  (nmf, k) = u0(nmf, k)
                   enddo
                endif
             endif
             if (kcu(nml)==1 .and. kfu(nml)==1) then
                if (ibl==3 .or. ibl==5 .or. ibl==6 .or. ibl==7) then
                   do k = kfumn0(nml), kfumx0(nml)
                      aak  (nml, k) = 0.0_fp
                      bbk  (nml, k) = 1.0_fp
                      bux  (nml, k) = 0.0_fp
                      bdx  (nml, k) = 0.0_fp
                      buy  (nml, k) = 0.0_fp
                      bdy  (nml, k) = 0.0_fp
                      buxuy(nml, k) = 0.0_fp
                      bdxuy(nml, k) = 0.0_fp
                      buxdy(nml, k) = 0.0_fp
                      bdxdy(nml, k) = 0.0_fp
                      cck  (nml, k) = 0.0_fp
                      ddk  (nml, k) = u0(nml, k)
                   enddo
                endif
             endif
          enddo
       endif
       call timer_stop(timer_uzd_bouncond, gdp)
       !
       ! left hand-side is now set by Delft3D-FLOW instead of the mapper
       !
       call timer_start(timer_uzd_lhs, gdp)
       do nm = 1, nmmax
          if (kcu(nm)==3 .or. kcu(nm)==-1) then
             do k = 1, kmax
                aak(nm,k) = 0.0_fp
                bbk(nm,k) = 1.0_fp
                cck(nm,k) = 0.0_fp
                ddk(nm,k) = u0(nm,k)
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_lhs, gdp)
       !
       ! Domain decomposition:
       !        end of "build_system_for_velocity",
       !        mapper can build the coupling equations
       !
       call timer_start(timer_uzd_rest, gdp) 
       if (icx == 1) then
          !
          ! D3dFlowMap_Build_V: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_v, gdp)
       else
          !
          ! D3dFlowMap_Build_U: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_u, gdp)
       endif
       call timer_stop(timer_uzd_rest, gdp) 
       !
       uvdwk = 0.0_fp
       vvdwk = 0.0_fp
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       call timer_start(timer_uzd_rowsc, gdp)
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             do k = kfumn0(nm), kfumx0(nm)
                bi           = 1.0_fp / bbk(nm, k)
                aak  (nm, k) = aak  (nm, k) * bi
                bbk  (nm, k) = 1.0_fp
                bux  (nm, k) = bux  (nm, k) * bi
                bdx  (nm, k) = bdx  (nm, k) * bi
                buy  (nm, k) = buy  (nm, k) * bi
                bdy  (nm, k) = bdy  (nm, k) * bi
                buxuy(nm, k) = buxuy(nm, k) * bi
                bdxuy(nm, k) = bdxuy(nm, k) * bi
                buxdy(nm, k) = buxdy(nm, k) * bi
                bdxdy(nm, k) = bdxdy(nm, k) * bi
                cck  (nm, k) = cck  (nm, k) * bi
                ddk  (nm, k) = ddk  (nm, k) * bi
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_rowsc, gdp)
       !
       !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       call timer_start(timer_uzd_solve1, gdp)
       !
       ! System of equations is reduced for all points
       ! right hand side is reduced within iterative loops
       !
       ! Division by the pivot for k=1 is not needed anymore
       ! because of row scaling
       !
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             do k = kfumn0(nm)+1, kfumx0(nm)
                bi         = 1.0_fp/(bbk(nm, k) - aak(nm, k)*cck(nm, k - 1))
                bbk(nm, k) = bi
                cck(nm, k) = cck(nm, k)*bi
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_solve1, gdp)
       !
       ! ITERATION LOOP
       !
       call timer_start(timer_uzd_solve2, gdp)
       iter = 0
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             do k = 1, kmax
                u1   (nm, k) = u0(nm, k)
                uvdwk(nm, k) = u0(nm, k)
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_solve2, gdp)
       !
       ! ensure that loop starts at point of correct color in own subdomain
       !
       if (mod(mfg+nfg,2) == 1) then
          !
          ! red points
          !
          nmsta = 1
       else
          !
          ! black points
          !
          nmsta = 2
       endif
       !
       ! Domain decomposition:
       !     resume point for next solve
       !
  222 continue
       gdp%dd%uzditer = gdp%dd%uzditer + 1
       !
       ! End Domain decomposition addition
       !
       itr = 1
       loop_iteration: do while (itr == 1 .and. iter < 50)
          iter = iter + 1
          !
          ! ITERATIVE SOLUTION METHOD USING CHECKERBOARD JACOBI
          ! IN HORIZONTAL DIRECTION
          ! ATTENTION : AN ODD NUMBER OF GRIDPOINTS IN V-DIRECTION
          !             ( NMAX ) IS ASSUMED!!!!!
          !
          itr = 0
          if (icx == 1) then
             call timer_start(timer_uzd_solve3u, gdp)
          else
             call timer_start(timer_uzd_solve5v, gdp)
          endif
          !
          ! loop starts at red or black point depending on own subdomain
          !
          nmsta = 3 - nmsta
          !
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm), kfumx0(nm)
                   !
                   ! COMPUTE RIGHT HAND SIDE
                   !
                   uvdwk(nm, k) = ddk(nm,k)                      &
                                & - bdx  (nm,k)*u1(nm-icx    ,k) &
                                & - bdy  (nm,k)*u1(nm-icy    ,k) &
                                & - buy  (nm,k)*u1(nm+icy    ,k) &
                                & - bux  (nm,k)*u1(nm+icx    ,k) &
                                & - bdxdy(nm,k)*u1(nm-icx-icy,k) &
                                & - bdxuy(nm,k)*u1(nm-icx+icy,k) &
                                & - buxuy(nm,k)*u1(nm+icx+icy,k) &
                                & - buxdy(nm,k)*u1(nm+icx-icy,k)
                enddo
             endif
          enddo
          if (icx == 1) then
             call timer_stop(timer_uzd_solve3u, gdp)
             call timer_start(timer_uzd_solve4u, gdp)
          else
             call timer_stop(timer_uzd_solve5v, gdp)
             call timer_start(timer_uzd_solve6v, gdp)
          endif
          !
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                kmin = kfumn0(nm)
                vvdwk(nm, kmin) = uvdwk(nm, kmin)*bbk(nm, kmin)
             endif
          enddo
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm)+1, kfumx0(nm)
                   vvdwk(nm, k) = (uvdwk(nm, k) - aak(nm, k)*vvdwk(nm, k - 1))*bbk(nm, k)
                enddo
             endif
          enddo
          !       
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumx0(nm)-1, kfumn0(nm), -1
                   vvdwk(nm, k) = vvdwk(nm, k) - cck(nm, k)*vvdwk(nm, k + 1)
                enddo
             endif
          enddo
          !
          ! CHECK FOR CONVERGENCE
          !
          loop_nm_1: do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm), kfumx0(nm)
                   if (abs(vvdwk(nm,k)-u1(nm,k)) > eps) then
                      itr = 1
                      exit loop_nm_1
                   endif
                enddo
             endif
          enddo loop_nm_1
          if (icx == 1) then
             call timer_stop(timer_uzd_solve4u, gdp)
             call timer_start(timer_uzd_solve3u, gdp)
          else
             call timer_stop(timer_uzd_solve6v, gdp)
             call timer_start(timer_uzd_solve5v, gdp)
          endif
          !
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm), kfumx0(nm)
                   u1(nm, k) = vvdwk(nm, k)
                enddo
             endif
          enddo
          !
          ! exchange u1 with neighbours for parallel runs
          !
          call dfexchg ( u1, 1, kmax, dfloat, nm_pos, gdp )
          !
          !
          ! loop starts at point of other color now (black respectively red)
          !
          nmsta = 3 - nmsta
          !
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm), kfumx0(nm)
                   !
                   ! COMPUTE RIGHT HAND SIDE
                   !
                   uvdwk(nm, k) = ddk(nm,k)                      &
                                & - bdx  (nm,k)*u1(nm-icx    ,k) &
                                & - bdy  (nm,k)*u1(nm-icy    ,k) &
                                & - buy  (nm,k)*u1(nm+icy    ,k) &
                                & - bux  (nm,k)*u1(nm+icx    ,k) &
                                & - bdxdy(nm,k)*u1(nm-icx-icy,k) &
                                & - bdxuy(nm,k)*u1(nm-icx+icy,k) &
                                & - buxuy(nm,k)*u1(nm+icx+icy,k) &
                                & - buxdy(nm,k)*u1(nm+icx-icy,k)
                enddo
             endif
          enddo
          !
          if (icx == 1) then
             call timer_stop(timer_uzd_solve3u, gdp)
             call timer_start(timer_uzd_solve4u, gdp)
          else
             call timer_stop(timer_uzd_solve5v, gdp)
             call timer_start(timer_uzd_solve6v, gdp)
          endif
          !
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                kmin           = kfumn0(nm)
                vvdwk(nm,kmin) = uvdwk(nm,kmin) * bbk(nm, kmin)
             endif
          enddo
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm)+1, kfumx0(nm)
                   vvdwk(nm,k) = (uvdwk(nm,k) - aak(nm,k)*vvdwk(nm,k-1)) * bbk(nm,k)
                enddo
             endif
          enddo
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumx0(nm)-1, kfumn0(nm), -1
                   vvdwk(nm,k) = vvdwk(nm,k) - cck(nm,k)*vvdwk(nm,k+1)
                enddo
             endif
          enddo
          !
          ! CHECK FOR CONVERGENCE
          !
          loop_nm_2: do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm), kfumx0(nm)
                   if (abs(vvdwk(nm,k)-u1(nm,k)) > eps) then
                      itr = 1
                      exit loop_nm_2
                   endif
                enddo
             endif
          enddo loop_nm_2
          !
          if (icx == 1) then
             call timer_stop(timer_uzd_solve4u, gdp)
          else
             call timer_stop(timer_uzd_solve6v, gdp)
          endif
          !
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                do k = kfumn0(nm), kfumx0(nm)
                   u1(nm, k) = vvdwk(nm, k)
                enddo
             endif
          enddo
          !
          ! exchange u1 with neighbours for parallel runs
          !
          call dfexchg ( u1, 1, kmax, dfloat, nm_pos, gdp )
          !
          ! set right-hand side to u1 in the halo area so that convergence check can be done safely
          !
          do nm = 1, nmmax
             if ( kcu(nm) == -1 ) then
                do k = kfumn0(nm), kfumx0(nm)
                   ddk(nm,k) = u1(nm,k)
                enddo
             endif
          enddo
          !
          !
          ! determine global maximum of 'itr' over all nodes
          ! Note: this enables to synchronize the iteration process
          !
          call dfreduce_gdp( itr, 1, dfint, dfmax, gdp )
       enddo loop_iteration
       !
       if (gdp%gdflwpar%flwoutput%iteroutputsteps >= gdp%gdinttim%ntstep) then
          write (lundia, '(2(a,i0))') 'z_uzd (ntstep  ,iter):',gdp%gdinttim%ntstep, '   ',iter
       endif
       if (iter >= 50) then
          write (errtxt, '(i0)') nst
          call prterr(lundia    ,'S205'    ,trim(errtxt)    )
       endif
       !
       ! Domain decomposition:
       !        end "solve_system_for_velocity"
       !
       if (icx == 1) then
          !
          ! D3dFlowMap_Check_V: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_v, gdp)
          if (nhystp == d3dflow_solve_v) goto 222
       else
          !
          ! D3dFlowMap_Check_U: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_u, gdp)
          if (nhystp == d3dflow_solve_u) goto 222
       endif
       !
       ! End Domain decomposition addition
       !
       !
       ! DEFINE NEW DEPTH AVERAGED VELOCITY
       !
       call timer_start(timer_uzd_umean, gdp)
       !
       ! Initialise umean for all (nm) 
       !
       umean = 0.0
       !
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             hnm = 0.0_fp
             do k = kfumn0(nm), kfumx0(nm)
                umean(nm) = umean(nm) + dzu0(nm,k)*u1(nm, k)
                hnm       = hnm + dzu0(nm,k)  
             enddo
             umean(nm) = umean(nm) / hnm
           endif
       enddo
       call timer_stop(timer_uzd_umean, gdp)
       !
    else       ! if (momsol == 'mdue  ') then
       !
       !  Time integration following the original approach of Bijvelds and Stelling
       !  with a dummy UZD (velocities are computed in Z_SUD/Z_CUCNP and frozen
       !  for the second half timestep. This is done in this part.
       !
       call timer_start(timer_uzd_ini, gdp)
       !
       ! Initialise aak, bbk and cck for all (nm, k)
       !
       aak = 0.0
       bbk = 1.0
       cck = 0.0
       !
       call timer_stop(timer_uzd_ini, gdp)
       call timer_start(timer_uzd_rhs, gdp)
       !
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             do k = 1, kmax
                ddk(nm, k) = u0(nm, k)
             enddo
          endif
       enddo
       call timer_stop(timer_uzd_rhs, gdp)
       !
       ! End of "build_system_for_velocity",
       ! mapper can build the coupling equations
       !
       call timer_start(timer_uzd_rest, gdp) 
       if (icx==1) then
          !
          ! D3dFlowMap_Build_V: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_v, gdp)
          !
       else
          !
          ! D3dFlowMap_Build_U: poke the coupling equations into system
          !
          nhystp = nxtstp(d3dflow_build_u, gdp)
          !
       endif
       call timer_stop(timer_uzd_rest, gdp) 
       !
       ! Resume point for next solve
       !
     333 continue
       gdp%dd%uzditer = gdp%dd%uzditer + 1
       !
       if (icx==1) then
          !
          ! D3dFlowMap_Check_V: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_v, gdp)
          if (nhystp==d3dflow_solve_v) goto 333
       else
          !
          ! D3dFlowMap_Check_U: Check for convergence
          !
          nhystp = nxtstp(d3dflow_solve_u, gdp)
          !
          if (nhystp==d3dflow_solve_u) goto 333
       endif
    endif
end subroutine z_uzd
