subroutine difu(icreep    ,timest    ,lundia    ,nst       ,icx       , &
              & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
              & lsec      ,lsed      ,lsts      ,norow     ,nocol     ,irocol    , &
              & nob       ,nto,      &
              & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
              & kadu      ,kadv      ,s0        ,s1        ,hu        , &
              & hv        ,dps       ,qxk       ,qyk       ,qzk       , &
              & guu       ,gvv       ,guv       ,gvu       ,gsqs      , &
              & rbnd      ,sigdif    ,sigmol    ,r0        ,r1        , &
              & sour      ,sink      ,ws        ,sedtyp    ,thick     , &
              & sig       ,dicuv     ,vicww     ,dsdksi    ,dsdeta    , &
              & dtdksi    ,dtdeta    ,aak       ,bbk       ,cck       , &
              & bdddx     ,bddx      ,bdx       ,bux       ,buux      , &
              & buuux     ,uvdwk     ,vvdwk     ,areau     ,areav     , &
              & aakl      ,bbkl      ,cckl      ,ddkl      , &
              & eqmbcsand ,eqmbcmud  ,seddif    ,volum0    ,volum1    , &
              & rscale    ,bruvai    ,nrob      ,gdp       )
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
!  $Id: difu.f90 6340 2016-07-20 15:43:16Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/difu.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes transport in the u, v and w-direction.
!              Implicit in the u- and w-direction, explicit in
!              v-direction.
!              Sinks are treated implicitly and sources explicit-
!              y. A special approach is used for the hori-
!              ontal diffusion to avoid artificial creeping.
! Method used: Reference : On the approximation of horizontal
!              gradients in sigma co-ordinates for bathymetry
!              with steep bottom slopes (G.S. Stelling and J.
!              van Kester - International Journal for Methods
!              in Fluids, Vol. 18 1994)
!              - Horizontal Advection in U-direction :
!                implicit, higher order upwind
!              - Horizontal Advection in V-direction :
!                explicit, central scheme
!              - Horizontal Diffusion :
!                3D: explicit, along Z-planes
!                2D: implicit in U-direction
!                    explicit in V-direction
!              - Option: horizontal diffusion strictly horizontal
!                using special filter
!              - Vertical Advection :
!                implicit, central scheme
!              - Vertical Diffusion : implicit
!              - Sources are integrated explicitly.
!              - Sinks are integrated implicitly.
!     Comment: For the Thatcher Harleman boundaries the boundary
!              points for outflow are reflected from the inner
!              points; for inflow the boundary conditions are
!              used (see also thahbc.for).
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use flow2d3d_timers
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
    integer                , pointer :: iro
    integer                , pointer :: mfg
    integer                , pointer :: nfg
    integer                , pointer :: nudge
    integer                , pointer :: itmor
    integer                , pointer :: itstrt
    real(fp)               , pointer :: ck
    real(fp)               , pointer :: dicoww
    real(fp)               , pointer :: eps
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: vicmol
    real(fp)               , pointer :: xlo
    real(fp), dimension(:,:,:)         , pointer :: fluxu
    real(fp), dimension(:,:,:)         , pointer :: fluxv
    logical                   , pointer :: bnd_distr_perC
    integer                   , pointer :: distQHn
    integer                   , pointer :: distQHm
    real(fp)                  , pointer :: reltim_qtq_C
    real(fp), dimension(:,:,:), pointer :: qfiltC
    logical                   , pointer :: suspLOADper
    logical                   , pointer :: suspCONCper
    logical                   , pointer :: printFLUXuv
    logical                   , pointer :: USEfixedBEDequilQS
    logical                   , pointer :: periodSURFACE
    real(fp)                  , pointer :: DELAYfixedBEDequilQS
    integer                   , pointer :: idebugCUThardINI
    integer                   , pointer :: idebugCUThardFIN
!
! Global variables
!
    integer                                             , intent(in)  :: nto   !  Description and declaration in esm_alloc_int.f90
integer                                                 , intent(in)  :: icreep    !  Description and declaration in tricom.igs
integer                                                               :: icx       !!  Increment in the X-dir., if ICX= NMAX
                                                                                   !!  then computation proceeds in the X-
                                                                                   !!  dir. If icx=1 then computation pro-
                                                                                   !!  ceeds in the Y-dir.
integer                                                               :: icy       !!  Increment in the Y-dir. (see ICX)
integer                                                               :: j         !!  Begin pointer for arrays which have
                                                                                   !!  been transformed into 1D arrays.
                                                                                   !!  Due to the shift in the 2nd (M-)
                                                                                   !!  index, J = -2*NMAX + 1
integer                                                               :: kmax      !  Description and declaration in esm_alloc_int.f90
integer                                                               :: lsal      !  Description and declaration in dimens.igs
integer                                                 , intent(in)  :: lsec      !  Description and declaration in dimens.igs
integer                                                               :: lsecfl    !  Description and declaration in dimens.igs
integer                                                               :: lsed      !  Description and declaration in esm_alloc_int.f90
integer                                                 , intent(in)  :: lsts      !  Description and declaration in dimens.igs
integer                                                 , intent(in)  :: lstsc     !  Description and declaration in dimens.igs
integer                                                               :: lstsci    !  Description and declaration in esm_alloc_int.f90
integer                                                               :: ltem      !  Description and declaration in dimens.igs
integer                                                               :: lundia    !  Description and declaration in inout.igs
integer                                                               :: nmmax     !  Description and declaration in dimens.igs
integer                                                               :: nmmaxj    !  Description and declaration in dimens.igs
integer                                                               :: norow     !  Description and declaration in esm_alloc_int.f90
integer                                                               :: nocol     !  Description and declaration in esm_alloc_int.f90
integer                                                               :: nrob
integer                                                 , intent(in)  :: nst
integer , dimension(8, nrob)                            , intent(in)  :: nob 
integer, dimension(7, norow+nocol)                                    :: irocol    !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcu       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfu       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfv       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: kadu      !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: kadv      !  Description and declaration in esm_alloc_int.f90
integer, dimension(lsed)                                , intent(in)  :: sedtyp    !! sediment type: 0=total/1=noncoh/2=coh
logical                                                 , intent(in)  :: eqmbcsand !  Description and declaration in morpar.igs
logical                                                 , intent(in)  :: eqmbcmud  !  Description and declaration in morpar.igs
real(fp)                                                , intent(in)  :: timest    !!  Half Integration time step [sec.]
real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                          :: dps       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gsqs      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guv       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvu       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvv       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hu        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hv        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s0        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bruvai    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: vicww     !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: qzk       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed), intent(in)  :: seddif    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)              :: ws        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: aak       !!  Internal work array (in CUCNP & UZD)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bbk       !!  Internal work array (in CUCNP & UZD)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdddx     !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M-3,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bddx      !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M-2,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdx       !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M-1,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: buuux     !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M+3,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: buux      !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M+2,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bux       !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M+1,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: cck       !!  Internal work array (in CUCNP & UZD)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)                    :: dicuv     !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dsdeta    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dsdksi    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dtdeta    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dtdksi    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areau
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areav
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qxk       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qyk       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: rscale    !  Internal work array, row scaling parameter in difu
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: volum0
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: volum1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: uvdwk     !!  Internal work array for Jac.iteration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: vvdwk     !!  Internal work array for Jac.iteration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: aakl      !!  Internal work array, lower diagonal
                                                                                   !!  tridiagonal matrix, implicit coupling
                                                                                   !!  of concentration in (N,M,K) with con-
                                                                                   !!  centration in (N,M,K-1)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: bbkl      !!  Internal work array, main diagonal
                                                                                   !!  tridiagonal matrix, implicit coupling
                                                                                   !!  of concentration in (N,M,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: cckl      !!  Internal work array, upper diagonal
                                                                                   !!  tridiagonal matrix, implicit coupling
                                                                                   !!  of concentration in (N,M,K) with con-
                                                                                   !!  centration in (N,M,K+1)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: ddkl      !!  Internal work array, diagonal space
                                                                                   !!  at (N,M,K,L)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r0        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sink      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sour      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(kmax)                                             :: sig       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(kmax)                                             :: thick     !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(kmax, max(lstsc, 1), 2, norow+nocol), intent(in)  :: rbnd      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(lstsci)                                           :: sigdif    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(lstsci)                             , intent(in)  :: sigmol    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
integer                               :: icxOK
integer                               :: icyOK
integer                               :: nmsta
integer                               :: ddb
integer                               :: iad1
integer                               :: iad2
integer                               :: iad3
integer                               :: ib
integer                               :: ibf
integer                               :: ibl
integer                               :: ic
integer                               :: icstart
integer                               :: icend
integer                               :: icxy
integer                               :: iter
integer                               :: itr
integer                               :: j1
integer                               :: j2
integer                               :: j3
integer                               :: jj
integer                               :: k
integer                               :: kfw
integer                               :: l
integer                               :: ll
integer                               :: ls
integer                               :: lst
integer                               :: maskval
integer                               :: mf
integer                               :: ml
integer                               :: n
integer                               :: ndm
integer                               :: nhystp
integer                               :: nm
integer                               :: nmd
integer                               :: nmdd
integer                               :: nmf
integer                               :: nmfu
integer                               :: nml
integer                               :: nmlu
integer, dimension(10)                :: nms
integer                               :: nmu
integer                               :: nmuu
integer                               :: nnudge
integer                               :: num
integer                               :: n1
integer                               :: npbi
integer                               :: mpbi
integer                               :: npbt
integer                               :: mpbt
integer                               :: npbAL
integer                               :: mpbAL
integer                               :: npbORT
integer                               :: mpbORT
integer                               :: npbORTm1
integer                               :: mpbORTm1
integer                               :: kcsi
integer                               :: nmpbt   ! NM index of boundary velocity point
integer                               :: nmpbi   ! NM index of 1st water level point inside domain
integer                               :: nmpbtAL ! NM index of 1st velocity point inside domain ALigned with the direction of the boundary discharge
integer                               :: nmpbORT  
integer                               :: nmpbORTm1 
integer                               :: shiftPERnm
real(fp)                              :: a
real(fp)                              :: qsk(1:kmax,lstsc,nrob)
real(fp)                              :: qk(1:kmax,nrob)
real(fp)                              :: qBOU(kmax)
real(fp)                              :: qAL (kmax)
real(fp)                              :: F_AL(kmax,lstsc)
real(fp),allocatable,save             :: Qs_EQ(:,:)
real(fp)                              :: QStot(lstsc,nto)
real(fp)                              :: Qtot(nto)
real(fp)                              :: q
real(fp)                              :: qz
real(fp)                              :: qs(lstsc)
real(fp)                              :: qsz
real(fp)                              :: adza
real(fp)                              :: adzc
real(fp)                              :: bi
real(fp), dimension(:,:), allocatable :: cavg
real(fp)                              :: cl
real(fp)                              :: cr
real(fp)                              :: d0k    ! Internal work array
real(fp)                              :: ddzc
real(fp)                              :: difiwe
real(fp)                              :: difl
real(fp)                              :: difr
real(fp)                              :: diz1
real(fp)                              :: epsitr ! Maximum value of relative error and absolute error of iteration process
real(fp)                              :: flux
real(fp), dimension(:,:), allocatable :: cflx
real(fp)                              :: h0
real(fp)                              :: h0i
real(fp)                              :: h0new
real(fp)                              :: h0old
real(fp), dimension(10)               :: mu
real(fp)                              :: nudgefac
real(fp)                              :: qxu
real(fp)                              :: qyv
real(fp)                              :: qzw
real(fp)                              :: rb
real(fp), external                    :: reddic
real(fp)                              :: rp
real(fp)                              :: sqrtbv
real(fp)                              :: timesti ! inverse of time step
real(fp)                              :: tnudge
real(fp)                              :: tsg
real(fp), dimension(kmax,lstsc,nrob)  :: distFAC      !  correction factor to distribute concentration BC based on internal values
character(20)                         :: errtxt
integer                               :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
logical                               :: bnd_distr_avgc
logical                               :: posdir
logical                               :: udir
logical                               :: vdir
logical,SAVE                          :: firstCALL = .TRUE.
logical                               :: ACTIVEequilQS
!
!! executable statements -------------------------------------------------------
!
    bnd_distr_perC       => gdp%gdimbound%bnd_distr_perC
    distQHn              => gdp%gdimbound%distQHn
    distQHm              => gdp%gdimbound%distQHm
    reltim_qtq_C         => gdp%gdimbound%reltim_qtq_C
    qfiltC               => gdp%gdimbound%qfiltC
    suspLOADper          => gdp%gdimbound%suspLOADper
    suspCONCper          => gdp%gdimbound%suspCONCper
    printFLUXuv          => gdp%gdimbound%printFLUXuv
    USEfixedBEDequilQS   => gdp%gdimbound%USEfixedBEDequilQS
    periodSURFACE        => gdp%gdimbound%periodSURFACE
    DELAYfixedBEDequilQS => gdp%gdimbound%DELAYfixedBEDequilQS
    idebugCUThardINI     => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN     => gdp%gdimbound%idebugCUThardFIN
    eps         => gdp%gdconst%eps
    vicmol      => gdp%gdphysco%vicmol
    dicoww      => gdp%gdphysco%dicoww
    iro         => gdp%gdphysco%iro
    xlo         => gdp%gdturcoe%xlo
    ck          => gdp%gdturcoe%ck
    mfg         => gdp%gdparall%mfg
    nfg         => gdp%gdparall%nfg
    nudge       => gdp%gdnumeco%nudge
    hdt         => gdp%gdnumeco%hdt
    fluxu       => gdp%gdflwpar%fluxu
    fluxv       => gdp%gdflwpar%fluxv
    itmor       => gdp%gdmorpar%itmor
    itstrt      => gdp%gdinttim%itstrt
    !
    ! INITIALISATION
    !
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    nm_pos = 1
    !
    !  INITIALIZE
    !
    call timer_start(timer_difu_ini, gdp)
    !
    ! Preprocess boundaries if we want to distribute the influx based on the
    ! concentrations computed inside the model domain.
    !
    ! Alberto version: This part can be put in a subroutine called before "call difu"  that provides variable distFAC, that is 
    ! then passed to difu. In this way the if(icy) can all be removed.
    !
    ! bnd_distr_avgc: distribute C at Q boundary using neighbour cells and conserving total Qsusp 
    ! bnd_distr_perC: distribute C at Q boundary using distribution at correspondent periodic H boundary. 
    ! suspLOADper: periodical NOT prescribed: copy suspended load from downstream to upstream (and impose correspondend concentration)
    ! suspCONCper: periodical NOT prescribed: copy suspended conc from downstream to upstream make load 
    !                 
    !
    ACTIVEequilQS =  USEfixedBEDequilQS.and.nst >= itmor*DELAYfixedBEDequilQS
    !by prescribing DELAYfixedBEDequilQS<0 I allow to compute the sed disch exiting downstream on the first time step and then prescr that value upstream for all the duration of the simulation
    bnd_distr_avgc = gdp%gdbcdat%distr_avc
    if (COUNT ((/bnd_distr_avgc, bnd_distr_perC,suspLOADper,suspCONCper/))>1) then !move it at the beginning of the code
       write(lundia,*) 'ERROR in difu: Only one between bnd_distr_avgc, bnd_distr_perC,suspLOADper and suspCONCper can be prescribed' 
       call d3stop(1, gdp)
    endif
    if (USEfixedBEDequilQS.and.itmor-itstrt==0.and.DELAYfixedBEDequilQS>0._fp) then !move it at the beginning of the code
       write(lundia,*) 'ERROR in difu: If USEfixedBEDequilQS=Y then itmor must be >0'
       call d3stop(1, gdp)
    endif
    distFAC(1:kmax,1:lstsc,:) = 1._fp ! needed for activating Neumann when both bnd_distr_avgc and bnd_distr_perC are false
    if (bnd_distr_avgc .or. bnd_distr_perC .or. suspLOADper .or. suspCONCper) then  
       if (USEfixedBEDequilQS.and..not. allocated(Qs_EQ)) allocate(Qs_EQ(lstsc,nto))
       do n1 = 1, nto
          Qtot(n1) = 0._fp
          QStot(1:lstsc,n1) = 0.0_fp
       enddo
       !
       ! calculate total suspended discharge for each cell
       ! calculate qtot  
       !
       do n = 1, nrob
! 
          if ((bnd_distr_perC.or. suspLOADper .or. suspCONCper).and.nob(3, n)/=7 ) cycle !if periodic only do it on the Q section (otherwise shiftPERnm is meaning less)
          q = 0._fp
          ! only do something for total discharge boundaries (7)  
         ! if (nob(3, n)/=7 ) then !.and. nob(3, n)/=2) then for now exclude water level boundaries (2) (that can be of type QH, otherwise for qtq_per it is going out of array when doing qsk(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k)
         !    cycle
         ! endif
         !If not periodic I do it also for water level boundary (and QH boundary), so it works for tidal waves too
          n1        = nob(8, n)
          mpbt      = nob(1, n)
          npbt      = nob(2, n)
          if (nob(4, n)==2) then
             mpbt = mpbt - 1
             mpbi = mpbt
             mpbAL = mpbt - 1
             mpbORT = mpbAL
             mpbORTm1 = mpbAL 
             posdir = .false.
          elseif (nob(4, n)==1) then
             mpbi = mpbt + 1
             mpbAL = mpbt + 1
             mpbORT = mpbAL
             mpbORTm1 = mpbAL 
             posdir = .true.
          else
             mpbi = mpbt
             mpbAL = mpbt
             mpbORT = mpbAL
             mpbORTm1 = mpbORT-1
          endif
          if (nob(6, n)==2) then
             npbt = npbt - 1
             npbi = npbt
             npbAL = npbt - 1
             npbORT = npbAL
             npbORTm1 = npbAL
             posdir = .false.
          elseif (nob(6, n)==1) then
             npbi = npbt + 1
             npbAL = npbt + 1
             npbORT = npbAL
             npbORTm1 = npbAL
             posdir = .true.
          else
             npbi = npbt
             npbAL = npbt
             npbORT = npbAL
             npbORTm1 = npbORT-1
          endif
          if (icy==1) then ! if I do separate directory only the first if is used
             icxOK = icx
             icyOK = icy
          else
             icxOK = icy
             icyOK = icx
          endif
          if (bnd_distr_perC) then
             shiftPERnm = icyOK*distQHn+icxOK*distQHm
          else
             shiftPERnm = 0
          endif
          nmpbt   = (npbt   + ddb)  *icyOK + (mpbt   + ddb)  *icxOK - icxy              !if periodic, its the Q boundary
          nmpbi   = (npbi   + ddb)  *icyOK + (mpbi   + ddb)  *icxOK - icxy - shiftPERnm !if periodic, its the zeta point just inside the domain at the H boundary
          nmpbtAL = (npbAL  + ddb)  *icyOK + (mpbAL  + ddb)  *icxOK - icxy - shiftPERnm !if periodic, its at the H boundary
     !     nmpbORT   = (npbORT + ddb)  *icyOK + (mpbORT + ddb)  *icxOK - icxy
     !     nmpbORTm1 = (npbORTm1 + ddb)*icyOK + (mpbORTm1 + ddb)*icxOK - icxy
          kcsi  = kcs(nmpbi)
          !
          ! Determine direction dependent parameters
          !
          q  = 0._fp
          qs(:) = 0._fp

          if (nob(4,n) > 0) then
             udir  = .true.
             vdir  = .false.
             if (icy==1) then ! if I do separate directory only the first if is used
                qBOU(1:kmax) = qxk(nmpbt, 1:kmax) !qxk is qxk
                qAL (1:kmax) = qxk(nmpbtAL, 1:kmax)
             else
                qBOU(1:kmax) = qyk(nmpbt, 1:kmax) !qyk is qxk
                qAL (1:kmax) = qyk(nmpbtAL, 1:kmax)
             endif
             F_AL(1:kmax,1:lstsc) = fluxu(nmpbtAL,1:kmax,1:lstsc)
          elseif (nob(6,n) > 0) then
             udir  = .false.
             vdir  = .true.
             if (icy==1) then   ! if I do separate directory only the first if is used
                qBOU(1:kmax) = qyk(nmpbt, 1:kmax) !qxk is qxk
                qAL (1:kmax) = qyk(nmpbtAL, 1:kmax)
             else
                qBOU(1:kmax) = qxk(nmpbt, 1:kmax) !qyk is qxk
                qAL (1:kmax) = qxk(nmpbtAL, 1:kmax)
             endif
             F_AL(1:kmax,1:lstsc) = fluxv(nmpbtAL,1:kmax,1:lstsc)
          else
          endif 
          if (printFLUXuv.and.bnd_distr_perC) then !print FLUXu and FLUXv in order to have boundary value at equilibrium.
             write(9999988,'(i9,100f25.15)')    nst, (sum(F_AL(1:kmax,j)),j=1,lstsc)
          endif
             !
             ! qxk/qyk should be zero for z-layers out of range?
             !
          if (bnd_distr_avgc) then
             if (kcsi==1) then
                q= 0._fp
                do k = 1, kmax
                   qz = 0._fp !used in case one day I wanna implement non-PRISMATIC CASE
                   if (posdir .and. qBOU(k)>0.0) then
                      qz = qz + qBOU(k) !water discharge  ! only sum up discharge weights for boundary cells strictly inside this partition
                      qk(k,n) = qz
                   elseif (.not.posdir .and. qBOU(k)<0.0) then
                      qz = qz - qBOU(k) !water discharge  ! only sum up discharge weights for boundary cells strictly inside this partition
                      qk(k,n) = qz
                   else                   
                      qk(k,n) = 0._fp
                   endif
                   q = q + qz
                enddo     
             endif
             do l = 1, lstsc   
                do k = 1, kmax
                   qsz = 0._fp
                   if (posdir .and. qAL(k)>0.0) then                   
                      qsz = qsz   + F_AL(k,l) !r0(nmpbi, k, l)*qAL(k)  !suspended discharge
                      qsk(k,l,n) = qsz
                   elseif (.not.posdir .and. qAL(k)<0.0) then                   
                      qsz = qsz   - F_AL(k,l) ! r0(nmpbi, k, l)*qAL(k) ! suspended discharge
                      qsk(k,l,n) = qsz
                   else                   
                      qsk(k,l,n) = 0._fp
                   endif
                   qs(l) = qs(l) + qsz
                enddo     
                if (reltim_qtq_C>0) then
                   if (.NOT.firstCALL) then
                      do k=1,kmax
                         a = exp( - hdt/reltim_qtq_C/60.0_fp) !hdt in sec, reltim in minutes
                         qfiltC(k,l,n) = a*qfiltC(k,l,n) + (1._fp - a)*qsk(k,l,n)
                         qsk(k,l,n) = qfiltC(k,l,n)
                      enddo
                      qs(l) = sum(qsk(:,l,n))  
                   else
                      qfiltC(1:kmax,l,n) = qsk(1:kmax,l,n)
                   endif
                endif      
             enddo           
!
          elseif (bnd_distr_perC.or.suspLOADper) then

             if (kcsi==1) then
                qk(1:kmax,n) = qBOU(1:kmax) ! doesnt matter if exiting (the fraction will be negative), see comment below next to distFAC.
                q = q + sum(qBOU(1:kmax)) !water discharge  ! only sum up discharge weights for boundary cells strictly inside this partition
             endif
             do l = 1, lstsc   
                do k = 1, kmax
                   qsk(k,l,n) = F_AL(k,l) !r0(nmpbi, k, l)*qAL(k) !!here nmpbi contains shift in m,n between the Q (internal)  boundary and the H (internal) periodic boundary
                   if (reltim_qtq_C>0) then
                      if (.NOT.firstCALL) then
                         a = exp( - hdt/reltim_qtq_C/60.0_fp) !hdt in sec, reltim in minutes. note "a" can be precomputed
                         qfiltC(k,l,n) = a*qfiltC(k,l,n) + (1._fp - a)*qsk(k,l,n)
                         qsk(k,l,n) = qfiltC(k,l,n)
                      else
                         qfiltC(k,l,n) = qsk(k,l,n)
                      endif
                   endif
                enddo
                qs(l) = sum(qsk(:,l,n))  
             enddo
          elseif (suspCONCper) then
             q = 0._fp !not used, just not to have NaN in Qtot
             qs(:) = 0._fp !not used, just not to have NaN in Qtot
             do l = 1, lstsc   
                do k = 1, kmax
                   qsk(k,l,n) = r0(nmpbi, k, l)
                enddo
             enddo
          endif
! 
          ! Parrallel code: check if above I need to use kcsi also for sediment
          if (kcsi==1) then ! only sum up discharge weights for boundary cells strictly inside this partition
             Qtot(n1)  = Qtot(n1)  + q
             do l = 1, lstsc
                QStot(l,n1) = QStot(l,n1) + qs(l)
             enddo
          endif 
       enddo  
       if (USEfixedBEDequilQS.and.((nst < itmor .and. DELAYfixedBEDequilQS>0._fp)   .or. nst == itstrt )) then !if (nst < itmor) then 

          do n1 = 1, nto
             Qs_EQ(1:lstsc,n1) = QStot(1:lstsc,n1) !if periodic it is undefined at H boundary
          enddo

       endif
 !
       do n = 1, nrob
          if (bnd_distr_perC.and.nob(3, n)/=7 ) then
             !if periodic only do it on the Q section (otherwise shiftPERnm is meaningless)
             distFAC(1:kmax,1:lstsc,n) = 1._fp
             cycle
          endif
          n1 = nob(8, n)
          if (bnd_distr_avgc .or. bnd_distr_perC) then
             do l = 1, lstsc
                if (comparereal(QStot(l,n1),0._fp)/=0) then        
                   do k=1,kmax
                      if (comparereal(qk(k,n),0._fp)/=0) then 
                         if (.not.ACTIVEequilQS) then !it might get unstable if I do it during warm-up, i.e. far from equilibrium
                            distFAC(k,l,n) = qsk(k,l,n)/QStot(l,n1)*Qtot(n1)/qk(k,n) !it cannot be negative even if qk/Qtot is negative, since ifi it is also qsk(k,l,n)/QStot(l,n1) is negative
                         else
                            distFAC(k,l,n) = qsk(k,l,n)/QStot(l,n1)*Qs_EQ(l,n1)/qk(k,n) !Qtot(n1)*C0 has been replaced by the constant Qs_EQ
                         endif
                      else
                         distFAC(k,l,n) = 1._fp !does it matter which value i give to the corrective coefficient or Neumann condition overwrites everything?
                      endif
                   enddo
                else
                   distFAC(1:kmax,l,n) = 1._fp 
                endif
             enddo
          elseif (suspLOADper) then
             do l = 1, lstsc
                if (comparereal(QStot(l,n1),0._fp)/=0) then        
                   do k=1,kmax
                      if (comparereal(qk(k,n),0._fp)/=0) then 
                          distFAC(k,l,n) = qsk(k,l,n)/qk(k,n) !it gives me a concentration then I force distFAC
                      else
                         distFAC(k,l,n) = 0._fp !first step zero concentration is prescribed
                      endif
                   enddo
                else
                   distFAC(1:kmax,l,n) = 0._fp !first step zero concentration is prescribed
                endif
             enddo
          elseif(suspCONCper) then
             do l = 1, lstsc    
                do k=1,kmax
                   distFAC(k,l,n) = qsk(k,l,n) !it has concentration!
                enddo
             enddo            
          endif
       enddo

!    !Bert and Alberto version
!    if (bnd_distr_avgc .or. bnd_distr_perC) then !.or.distr_perC) then
!
!       allocate( cavg(lstsc,nrob), cflx(0:lstsc,nrob) )
!       !
!       ! Compute total flux through open boundaries if the concentration imposed
!       ! would be equal to the computed concentration just inside the model.
!       !
!       cavg = 0.0_fp
!       cflx = 0.0_fp
!       do ic = 1, norow+nocol
!          n    = irocol(1, ic)
!          mf   = irocol(2, ic) - 1
!          ml   = irocol(3, ic)
!          ibf  = irocol(6, ic)
!          ibl  = irocol(7, ic)
!          nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
!          nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
!          nmfu = nmf + icx
!          nmlu = nml + icx
!          !
!          if (ibf>0) then
!             do k = 1, kmax
!                if (qxk(nmf, k) > 0.0) then    !old version by Bert and Alberto
!                   cflx(0,ibf) = cflx(0,ibf) + qxk(nmf, k)
!                   do l = 1, lstsc
!                      cavg(l, ibf) = rbnd(k, l, 1, ic)
!                      cflx(l, ibf) = cflx(l, ibf) + r0(nmfu, k, l)*qxk(nmf, k)  
!                   enddo !new version by Alberto
!                endif
!             enddo
!          endif
!          if (ibl>0) then
!             do k = 1, kmax
!                if (qxk(nml, k) < 0.0) then
!                   cflx(0, ibl) = cflx(0, ibl) + qxk(nml, k)
!                   do l = 1, lstsc
!                      cavg(l, ibl) = rbnd(k, l, 2, ic)
!                      cflx(l, ibl) = cflx(l, ibl) + r0(nml, k, l)*qxk(nml, k)
!                   enddo
!                endif
!             enddo
!          endif
!       enddo
!       !
!       ! Compute the correction factor: we would like the influx to be equal to
!       ! cavg * total influx, while the concentration profile matches the internally
!       ! computed concentration profile. After this step cflux(l,ib) will contain the
!       ! correction factor if it's possible to distribute the flux proportionally to
!       ! the concentrations, while cavg(l,ib) will contain the average concentration
!       ! if it's not possible to distribute the flux proportionally. The latter only
!       ! happens if the flux computed above is zero (usually only during startup).
!       !
!       do ib = 1, nrob
!          do l = 1, lstsc
!             if (abs(cflx(l, ib)) > 0.0_fp) then
!                !
!                ! We want to make concentration at the boundary equal to:
!                !
!                ! c_{nm,k;bnd} = (c_{nm,k;intern} * c_{avg} * SUM q_{nm,k}) / SUM ( c_{nm,k;intern} q_{nm,k})
!                !
!                ! We precompute the part that is independent of local index nm,k.
!                ! Set the average concentration to zero.
!                !
!                ! f = (c_{avg} * SUM q_{nm,k}) / SUM ( c_{nm,k;intern} q_{nm,k})
!                !
!                cflx(l, ib) = (cavg(l, ib)*cflx(0, ib)) / cflx(l, ib)
!                cavg(l, ib) = 0.0_fp
!             else
!                !
!                ! Concentrations are zero, so we can't distribute the flux proportially to computed concentrations
!                ! so, we use the average concentration uniformly
!                !
!                cflx(l, ib) = 0.0_fp
!             endif
!          enddo
!       enddo
       !
       ! Adjust the concentrations for the explicit Y direction.
       ! Messing with r0 is not so nice, but it avoids having to make exceptions for Y-boundaries below and in difuflux.
       !
       icxOK = icy
       icyOK = icx
       if (icy==1) then
          icstart = norow+1
          icend   = norow+nocol
       else
          icstart = 1
          icend   = norow
       endif
       do ic = icstart, icend
          n    = irocol(1, ic)
          mf   = irocol(2, ic) - 1
          ml   = irocol(3, ic)
          ibf  = irocol(6, ic)
          ibl  = irocol(7, ic)
          nmf  = (n + ddb)*icyOK + (mf + ddb)*icxOK - icxy
          nml  = (n + ddb)*icyOK + (ml + ddb)*icxOK - icxy
          nmfu = nmf + icxOK
          nmlu = nml + icxOK
          !
          if (ibf>0) then
             do k = 1, kmax
                if (qyk(nmf, k) > 0.0) then
                   do l = 1, lstsc
                      if ((bnd_distr_avgc .or. bnd_distr_perC).and..not.ACTIVEequilQS) then
                         r0(nmf, k, l) = rbnd(k, l, 1, ic)*distFAC(k,l,ibf)   
                      else
                         r0(nmf, k, l) =                   distFAC(k,l,ibf)   
                      endif
                   enddo
                endif
             enddo
          endif
          if (ibl>0) then
             do k = 1, kmax
                if (qyk(nml, k) < 0.0) then
                   do l = 1, lstsc
                      if ((bnd_distr_avgc .or. bnd_distr_perC).and..not.ACTIVEequilQS) then
                         r0(nmlu, k, l) = rbnd(k, l, 2, ic)*distFAC(k,l,ibl)
                      else
                         r0(nmlu, k, l) =                   distFAC(k,l,ibl)
                      endif
                   enddo
                endif
             enddo
          endif
       enddo
     endif
    !
    ! Initialise arrays aak - cck for all (nm,k)
    !
    aak   = 0.0_fp
    buuux = 0.0_fp
    buux  = 0.0_fp
    bux   = 0.0_fp
    bdx   = 0.0_fp
    bddx  = 0.0_fp
    bdddx = 0.0_fp
    cck   = 0.0_fp
    !
    timesti = 1.0_fp / timest
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             !if (volum1(nm, k).gt.THRESsmallCELL) then
             bbk(nm, k) = volum1(nm, k) * timesti
             !else
             !   bbk(nm, k) = 1._fp
             !endif
          else
             bbk(nm, k) = 1.0_fp
             if (lsec > 0) r0(nm, k, lsecfl) = 0.0_fp
          endif
       enddo
    enddo
    do l = 1, lstsci
       if (lsec==2 .and. l==lsecfl) then
          cycle
       endif
       do k = 1, kmax
          do nm = 1, nmmax
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                !if (volum1(nm, k).gt.THRESsmallCELL) then
                ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) * timesti
                !else
                !   ddkl(nm, k, l) = r0(nm, k, l)
                !endif
             else
                ddkl(nm, k, l) = r0(nm, k, l)
             endif
          enddo
       enddo
    enddo
    call timer_stop(timer_difu_ini, gdp)
    !
    ! CONTRIBUTION OF ADVECTION IN X-DIRECTION
    !
    call timer_start(timer_difu_horadv, gdp)
    do k = 1, kmax
       !
       ! CONTRIBUTION TO VOLUME NM AND NMU
       !
       nmd  = -icx
       nmdd = -icx - icx
       nmu  =  icx
       nmuu =  icx + icx
       do nm = 1, nmmax
          nmd  = nmd  + 1
          nmdd = nmdd + 1
          nmu  = nmu  + 1
          nmuu = nmuu + 1
          qxu  = qxk(nm, k)/6.0_fp
          if (qxu > 0.0) then
             iad1 =      kfu(nm)  *kadu(nm, k)
             iad2 = iad1*kfu(nmd) *kadu(nmd, k)
             iad3 = iad2*kfu(nmdd)*kadu(nmdd, k)
             !
             j1 = 6*iad1 + 3*iad2 +   iad3
             j2 =        - 3*iad2 - 2*iad3
             j3 =                     iad3
             !
             bbk  (nm , k) = bbk  (nm , k) + qxu*j1
             bdx  (nm , k) = bdx  (nm , k) + qxu*j2
             bddx (nm , k) = bddx (nm , k) + qxu*j3
             bdx  (nmu, k) = bdx  (nmu, k) - qxu*j1
             bddx (nmu, k) = bddx (nmu, k) - qxu*j2
             bdddx(nmu, k) = bdddx(nmu, k) - qxu*j3
          else
             iad1 =        kfu(nm)   * kadu(nm  , k)
             iad2 = iad1 * kfu(nmu)  * kadu(nmu , k)
             iad3 = iad2 * kfu(nmuu) * kadu(nmuu, k)
             !
             j1 = 6*iad1 + 3*iad2 +   iad3
             j2 =        - 3*iad2 - 2*iad3
             j3 =                     iad3
             !
             bux  (nm , k) = bux  (nm , k) + qxu*j1
             buux (nm , k) = buux (nm , k) + qxu*j2
             buuux(nm , k) = buuux(nm , k) + qxu*j3
             bbk  (nmu, k) = bbk  (nmu, k) - qxu*j1
             bux  (nmu, k) = bux  (nmu, k) - qxu*j2
             buux (nmu, k) = buux (nmu, k) - qxu*j3
          endif
       enddo
    enddo
    !
    ! CONTRIBUTION OF ADVECTION IN Y-DIRECTION
    !
    do l = 1, lstsci
       if (lsec==2 .and. l==lsecfl) then
          cycle
       endif
       do k = 1, kmax
          !
          ! CONTRIBUTION TO VOLUME NM AND NUM
          !
          ndm = -icy
          num =  icy
          do nm = 1, nmmax
             ndm = ndm + 1
             num = num + 1
             qyv  = qyk(nm, k)
             iad1 = kfv(nm)*kadv(nm, k)
             iad2 = iad1*kfv(num)*kadv(num, k)*kfv(ndm)*kadv(ndm, k)
             if (qyv > 0.0_fp) then
                d0k = 0.5_fp*qyv*( (2*iad1 - iad2)*r0(nm , k, l) &
                    &          +          iad2 *r0(num, k, l))
             else
                d0k = 0.5_fp*qyv*( (2*iad1 - iad2)*r0(num, k, l) &
                    &          +          iad2 *r0(nm , k, l))
             endif
             if (kcs(nm)  == 1) ddkl(nm , k, l) = ddkl(nm , k, l) - d0k
             if (kcs(num) == 1) ddkl(num, k, l) = ddkl(num, k, l) + d0k
          enddo
       enddo
    enddo
    call timer_stop(timer_difu_horadv, gdp)
    !
    !
    ! Explicit algoritm (call DIFACR) leads to extra stablity criterium
    ! DT <= (DX**2)/(2*DICUV)
    !
    ! This diffusion part (loop 410) is constituent independent.
    ! The value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
    call timer_start(timer_difu_hordiff, gdp)
    if (icreep==0 .or. kmax==1) then
       !
       ! HORIZONTAL DIFFUSION IN X-DIRECTION ALONG SIGMA PLANES
       !
       do k = 1, kmax
          !
          ! CONTRIBUTION TO VOLUME NM AND NMU
          !
          nmu = icx
          do nm = 1, nmmax
             nmu = nmu + 1
             if (kfu(nm)*kadu(nm, k) /= 0) then
                difl    = dicuv(nm, k)
                difr    = dicuv(nmu, k)
                flux    = 0.5*(difl + difr)/(0.7*gvu(nm))
                maskval = max(0, 2 - abs(kcs(nm)))
                bbk(nm, k) = bbk(nm, k) + areau(nm, k)*flux*maskval
                bux(nm, k) = bux(nm, k) - areau(nm, k)*flux*maskval
                maskval    = max(0, 2 - abs(kcs(nmu)))
                bbk(nmu, k) = bbk(nmu, k) + areau(nm, k)*flux*maskval
                bdx(nmu, k) = bdx(nmu, k) - areau(nm, k)*flux*maskval
             endif
          enddo
       enddo
       !
       ! HORIZONTAL DIFFUSION IN Y-DIRECTION ALONG SIGMA PLANES
       !
       do l = 1, lstsci
          if (lsec==2 .and. l==lsecfl) then
             cycle
          endif
          do k = 1, kmax
             !
             ! CONTRIBUTION TO VOLUME NM AND NUM
             !
             num = icy
             do nm = 1, nmmax
                num = num + 1
                if (kfv(nm)*kadv(nm, k) /= 0) then
                   cl      = r0(nm, k, l)
                   difl    = dicuv(nm, k)
                   cr      = r0(num, k, l)
                   difr    = dicuv(num, k)
                   flux    = 0.5_fp*(cr - cl)*(difl + difr)/(0.7*guv(nm))
                   maskval = max(0, 2 - abs(kcs(nm)))
                   ddkl(nm, k, l)  = ddkl(nm, k, l) + areav(nm, k)*flux*maskval
                   maskval         = max(0, 2 - abs(kcs(num)))
                   ddkl(num, k, l) = ddkl(num, k, l) - areav(nm, k)*flux*maskval
                endif
             enddo
          enddo
       enddo
    else
       !
       ! Explicit algoritm (call DIFACR) leads to extra stablity criterium
       ! dt <= (dx**2)/(2*dicuv)
       !
       ! HORIZONTAL DIFFUSION ALONG Z-PLANES (only if KMAX > 1 and Anti Creep)
       !
       call difacr(icx       ,icy       ,j         ,nmmaxj    ,nmmax     , &
                 & kmax      ,lstsci    ,lsal      ,ltem      ,kcs       , &
                 & kfu       ,kfv       ,kadu      ,kadv      ,s0        , &
                 & dps       ,r0        ,ddkl      ,guu       ,gvv       , &
                 & guv       ,gvu       ,thick     ,sig       ,dicuv     , &
                 & sigdif    ,dsdksi    ,dtdksi    ,dsdeta    ,dtdeta    , &
                 & gdp       )
    endif
    call timer_stop(timer_difu_hordiff, gdp)
    call timer_start(timer_difu_vertadv, gdp)
    if (kmax > 1) then
       do k = 1, kmax - 1
          if (k==1 .or. k==kmax - 1) then
             kfw = 1
          else
             kfw = 0
          endif
          do nm = 1, nmmax
             !
             ! ADVECTION IN VERTICAL DIRECTION; W*DC/DZ
             !
             if (kfs(nm) == 1) then
                qzw = qzk(nm, k)
               ! if (k==1.and.mod(nst,10)==0) write(*,*) ' qzw is zeroooo' 
                if (qzw > 0.0) then
                   adza = 0.5_fp*qzw*(1 - kfw)
                   adzc = 0.5_fp*qzw*(1 + kfw)
                else
                   adza = 0.5_fp*qzw*(1 + kfw)
                   adzc = 0.5_fp*qzw*(1 - kfw)
                endif
                aak(nm, k + 1) = aak(nm, k + 1) + adza
                bbk(nm, k + 1) = bbk(nm, k + 1) + adzc
                bbk(nm, k    ) = bbk(nm, k    ) - adza
                cck(nm, k    ) = cck(nm, k    ) - adzc
             endif
          enddo
       enddo
    endif
    do l = 1, lstsci
       if (lsec==2 .and. l==lsecfl) then
          cycle
       endif
       do k = 1, kmax
          do nm = 1, nmmax
                aakl(nm, k, l) = aak(nm, k)
                bbkl(nm, k, l) = bbk(nm, k)
                cckl(nm, k, l) = cck(nm, k)
          enddo
       enddo
    enddo
    call timer_stop(timer_difu_vertadv, gdp)
    !
    ! DIFFUSION IN VERTICAL DIRECTION
    !
    call timer_start(timer_difu_vertdiff, gdp)
    if (kmax > 1) then
       do l = 1, lstsci
          !
          ! l = sediment: ls > 0
          ! else        : ls = 0
          !
          !if (mod(nst,100)==0) write(*,*)'tolta diffusione ddzc'
          ls = 0
          if ((l>max(lsal, ltem)) .and. (l<=lsts)) ls = l - max(lsal, ltem)
          do k = 1, kmax - 1
             tsg = 0.5_fp * (thick(k) + thick(k+1))
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   h0  = max(0.1_fp, s0(nm) + real(dps(nm),fp))
                   h0i = 1.0_fp / h0
                   !
                   ! Internal wave contribution
                   !
                   sqrtbv = max(0.0_fp, bruvai(nm, k))
                   sqrtbv = sqrt(sqrtbv)
                   difiwe = 0.2_fp * sqrtbv * xlo**2
                   if (ls > 0) then
                      !
                      ! sediment constituent:
                      ! No dicoww-restriction in reddic
                      !
                      diz1 = vicmol/sigmol(l) + difiwe + seddif(nm, k, ls)/sigdif(l)
                   else
                      !
                      ! all other constituents:
                      ! dicoww-restriction is moved from TURCLO to here (in reddic)
                      ! vicww is used instead of dicww
                      !
                      diz1 = vicmol/sigmol(l) + reddic(difiwe + vicww(nm,k)/sigdif(l), gdp)
                   endif
                   ddzc             = gsqs(nm) * diz1 * h0i / tsg
                   aakl(nm, k+1, l) = aakl(nm, k+1, l) - ddzc
                   bbkl(nm, k+1, l) = bbkl(nm, k+1, l) + ddzc
                   bbkl(nm, k  , l) = bbkl(nm, k  , l) + ddzc
                   cckl(nm, k  , l) = cckl(nm, k  , l) - ddzc
                endif
             enddo
          enddo
       enddo
    endif
    call timer_stop(timer_difu_vertdiff, gdp)
    !
    ! Include settling velocities and Dirichlet BC for sediments in
    ! matrices AAKL/BBKL/CCKL/DDKL
    !
    if (lsed > 0) then
       call timer_start(timer_difu_difws, gdp)
       call dif_ws(j         ,nmmaxj    ,nmmax     ,kmax      ,lsal      , &
                 & ltem      ,lstsci    ,lsed      ,kcs       ,kfs       , &
                 & gsqs      ,ws        ,aakl      ,bbkl      ,cckl      , &
                 & gdp       )
       call timer_stop(timer_difu_difws, gdp)
    endif
    !
    ! SET VALUES IN OPEN BOUNDARY POINTS (IN PART. FOR Y-DIRECTION)
    !     On open boundary no seconday flow (=> loop over LSTSC)
    !
    call timer_start(timer_difu_bounopen, gdp)
    do nm = 1, nmmax
       if (kcs(nm) == 2) then
          do l = 1, lstsc
             do k = 1, kmax
                ddkl(nm, k, l) = r0(nm, k, l)
                aakl(nm, k, l) = 0.0_fp
                bbkl(nm, k, l) = 1.0_fp
                cckl(nm, k, l) = 0.0_fp
             enddo
          enddo
       endif
    enddo
    !
    ! BOUNDARY CONDITIONS
    !     On open boundary no seconday flow (=> loop over LSTSC)
    !
    if (icy==1) then
       icstart = 1
       icend   = norow
    else
       icstart = norow+1
       icend   = norow+nocol
    endif
    do ic = icstart, icend
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmfu = nmf + icx
       nmlu = nml + icx
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS
       !
       if (bnd_distr_avgc.or.bnd_distr_perC) then
          !
          ! average concentration at boundary specified, concentration to be imposed at the
          ! boundary depends on the locally computed concentration inside the model
          !
          if (kcu(nmf) == 1) then
             ibf  = irocol(6, ic)
             do k = 1, kmax
                do l = 1, lstsc
                   if ((bnd_distr_avgc .or. bnd_distr_perC).and..not.ACTIVEequilQS) then
                      ddkl(nmf, k, l) = rbnd(k, l, 1, ic)*distFAC(k,l,ibf)
                   else
                      ddkl(nmf, k, l) =                   distFAC(k,l,ibf)
                   endif
                enddo     
             enddo
          endif
          if (kcu(nml) == 1) then
             ibl  = irocol(7, ic)
             do k = 1, kmax
                do l = 1, lstsc
                   if ((bnd_distr_avgc .or. bnd_distr_perC).and..not.ACTIVEequilQS) then
                      ddkl(nmlu, k, l) = rbnd(k, l, 2, ic)*distFAC(k,l,ibl)
                   else
                      ddkl(nmlu, k, l) =                   distFAC(k,l,ibl)
                   endif
                enddo     
             enddo
          endif
       else
          !
          ! default: local concentration at boundary specified
          !
       if (kcu(nmf) == 1) then
          do k = 1, kmax
             do l = 1, lstsc
                ddkl(nmf, k, l) = rbnd(k, l, 1, ic)
             enddo
          enddo
       endif
       if (kcu(nml) == 1) then
          do k = 1, kmax
             do l = 1, lstsc
                ddkl(nmlu, k, l) = rbnd(k, l, 2, ic)
             enddo
          enddo
       endif
       endif
       !
       ! optional Neumann boundary condition for suspended sediment fractions
       !
       lst = max(lsal, ltem)
       do l = 1, lsed
          ll = lst + l
          if ((eqmbcsand .and. sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) .or. &
            & (eqmbcmud  .and. sedtyp(l) == SEDTYP_COHESIVE)             ) then
             if (kcu(nmf) == 1) then
                ibf  = irocol(6, ic)
                do k = 1, kmax
                   if (comparereal(distFAC(k,ll,ibf),1._fp)==0) then !If C is distributed, do not prescribe Neumann!
                   ddkl(nmf, k, ll) = max(0.0_fp, r0(nmfu, k, ll))
                   endif
                enddo
             endif
             if (kcu(nml) == 1) then
                ibl  = irocol(7, ic)
                do k = 1, kmax
                   if (comparereal(distFAC(k,ll,ibl),1._fp)==0) then !If C is distributed, do not prescribe Neumann!
                   ddkl(nmlu, k, ll) = max(0.0_fp, r0(nml, k, ll))
                   endif
                enddo
             endif
          endif
       enddo
    enddo
    call timer_stop(timer_difu_bounopen, gdp)
    do l = 1, lstsci
       !
       ! SOURCES AND SINK TERMS
       !
       ! SINKS ARE TREATED IMPLICITLY
       !
       call timer_start(timer_difu_sourcesink, gdp)
       if (lsec==2 .and. l==lsecfl) then
          !
          ! secondary flow (equilibrium equals to new intensity)
          !       start-up problems SINK might be 0.0 when
          !       UMOD = 0.0 in SECRHS
          !
          do k = 1, kmax
             do nm = 1, nmmax
                if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                   h0new = s1(nm) + real(dps(nm),fp)
                   if (abs(sink(nm,k,l)*h0new) > eps) then
                      h0old = s0(nm) + real(dps(nm),fp)
                      r1(nm, k, l) = sour(nm, k, l)*h0old/(sink(nm, k, l)*h0new)
                   else
                      r1(nm, k, l) = 0.0_fp
                   endif
                endif
             enddo
          enddo
       else
          do k = 1, kmax
             do nm = 1, nmmax
                if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                   !if (volum1(nm, k).gt.THRESsmallCELL) then ! I already set bbkl(nm, k, l)=1 if volum1 small
                   bbkl(nm, k, l) = bbkl(nm, k, l) + sink(nm, k, l)*volum1(nm, k)
                   ddkl(nm, k, l) = ddkl(nm, k, l) + sour(nm, k, l)*volum0(nm, k)
                   !endif
                endif
             enddo
          enddo
          !
          ! set concentrations in dry points and in open boundary points
          !
          do k = 1, kmax
             do nm = 1, nmmax
                if ((kfs(nm)==0 .and. kcs(nm)==1) .or. kcs(nm)==2) then
                   r1(nm, k, l) = ddkl(nm, k, l)
                endif
             enddo
          enddo
       endif
       call timer_stop(timer_difu_sourcesink, gdp)
       !
       if (l == lsecfl) then
          !
          ! boundary conditions secondary flow (spiral motion intensity)
          !
          call timer_start(timer_difu_secbou, gdp)
         ! if (periodSURFACE) then !CAN BE UNCOMMENTED AFTER THE PERIODIC WATER DEPTH PROBLEM IS FIXED
         !    call periodSPIRAL(r0,r1,gdp%d%nlb,gdp%d%nub,gdp%d%mlb,gdp%d%mub,kmax,lsecfl,lstsci,gdp)
         ! else
             if (icy==1) then
          call secbou(j         ,nmmaxj    ,kmax      ,icx       ,icy       , &
                    & lstsci    ,lsecfl    ,kfu       ,irocol    ,norow     , &
                    & s0        ,s1        ,dps       ,r1        ,sour      , &
                    & sink      ,gdp       )
             else
                call secbou(j         ,nmmaxj    ,kmax      ,icx       ,icy       , &
                          & lstsci    ,lsecfl    ,kfu       ,irocol(1,norow+1),nocol     , &
                          & s0        ,s1        ,dps       ,r1        ,sour      , &
                          & sink      ,gdp       )
             endif
         ! endif
          call timer_stop(timer_difu_secbou, gdp)
          if (lsec == 2) then
             !
             ! exchange r1 with neighbours for parallel runs
             !
             call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
             !
             cycle
          endif
       endif
       !
       ! DD code added:
       !
       ! left hand-side is now set by Delft3D-FLOW instead of the mapper
       !
       call timer_start(timer_difu_lhs, gdp)
       do nm = 1, nmmax
          if (kcs(nm) == 3 ) then
             do k = 1, kmax
                aakl(nm,k,l) = 0.0_fp
                bbkl(nm,k,l) = 1.0_fp
                cckl(nm,k,l) = 0.0_fp
                ddkl(nm,k,l) = r0(nm,k,l)
             enddo
          endif
       enddo
       call timer_stop(timer_difu_lhs, gdp)
       !
       !
       !        D3dFlow_Build_ADI_Conc: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_adi_conc, gdp)
       !
       ! DD code added end
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       !   Store scale factor in array rscale
       !   They are used for the constituent independent flux arrays b[d/u][d/u][d/u]x
       !
       call timer_start(timer_difu_rowsc, gdp)
       do k = 1, kmax
          do nm = 1, nmmax
             if (kfs(nm)==1) then
                rscale(nm, k)    = 1.0_fp / bbkl(nm, k, l)
                aakl  (nm, k, l) = aakl(nm, k, l) * rscale(nm, k)
                bbkl  (nm, k, l) = 1.0_fp
                cckl  (nm, k, l) = cckl(nm, k, l) * rscale(nm, k)
                ddkl  (nm, k, l) = ddkl(nm, k, l) * rscale(nm, k)
             endif
          enddo
       enddo
       call timer_stop(timer_difu_rowsc, gdp)
       !
       !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       ! Division by the pivot for k=1 is not needed anymore
       ! because of row scaling
       !
       call timer_start(timer_difu_solve1, gdp)
       do nm = 1, nmmax
          if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
             do k = 2, kmax
                bi             = 1.0_fp/(bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
                bbkl(nm, k, l) = bi
                cckl(nm, k, l) = cckl(nm, k, l)*bi
             enddo
          endif
       enddo
       call timer_stop(timer_difu_solve1, gdp)
       !
       ! ITERATION LOOP
       !
       call timer_start(timer_difu_solve2, gdp)
       iter = 0
       do k = 1, kmax
          do nm = 1, nmmax
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                r1(nm, k, l) = r0(nm, k, l)
                uvdwk(nm, k) = r0(nm, k, l)
             endif
          enddo
       enddo
       call timer_stop(timer_difu_solve2, gdp)
       !
       ! exchange r1 with neighbours for parallel runs
       !
       call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
       !
       ! assure that loop starts at point of correct color in own subdomain
       !
       if (mod(mfg+nfg,2) == 1) then
          ! red points
          nmsta = 1
       else
          ! black points
          nmsta = 2
       endif
       !
       ! DD code added:
       !
       !
       !       (re)solve system of equations
       !
  111  continue
       gdp%dd%difuiter = gdp%dd%difuiter + 1
       !
       ! DD code added end
       !
 1100  continue
       iter = iter + 1
       !
       ! ITERATIVE SOLUTION METHOD USING CHECKERBOARD JACOBI
       ! IN HORIZONTAL DIRECTION
       !
       itr = 0
       !
       ! set concentrations in coupling points
       !
       call timer_start(timer_difu_solve3, gdp)
       do k = 1, kmax
          do nm = 1, nmmax
             if (kcs(nm) == 3) then
                r1(nm, k, l) = ddkl(nm, k, l)
             endif
          enddo
       enddo
       call timer_stop(timer_difu_solve3, gdp)
       if(icx == 1) then
         call timer_start(timer_difu_solve4u, gdp)
       else
         call timer_start(timer_difu_solve6v, gdp)
       end if
       !
       ! loop starts at red or black point depending on own subdomain
       !
       nmsta = 3 - nmsta
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             ! ( CHECK FOR KCS TO AVOID AN ARRAY INDEX OUT OF BOUNDS )
             !
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                uvdwk(nm, k) =   bdddx(nm, k) * r1(nm - icx - icx - icx, k, l) &
                             & + bddx (nm, k) * r1(nm - icx - icx, k, l)       &
                             & + bdx  (nm, k) * r1(nm - icx, k, l)             &
                             & + bux  (nm, k) * r1(nm + icx, k, l)             &
                             & + buux (nm, k) * r1(nm + icx + icx, k, l)       &
                             & + buuux(nm, k) * r1(nm + icx + icx + icx, k, l)
                uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm, k)
             endif
          enddo
       enddo
   !  if (nst.ge.764.and.nst.le.764.and. iter<=3) THEN
   !     do nm = nmsta, nmmax, 2        
   !         if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then             
   !            do k=1,kmax
   !              write(9892003,'(5i6,35f21.15)') nst,nm,iter,k,L,uvdwk(nm, k), ddkl(nm,k,l),bdddx(nm,k),bddx(nm,k),bdx (nm,k), &
   !                           bux(nm,k),buux(nm,k),buuux(nm,k),bbkl(nm, k,l),aakl(nm, k,l),cckl(nm, k,l),&
   !                           r1(nm - icx - icx - icx, k, l),r1(nm - icx - icx, k, l),r1(nm - icx, k, l),r1(nm + icx, k, l),&
   !                           r1(nm + icx + icx, k, l), r1(nm + icx + icx + icx, k, l)
   !            enddo
   !         endif
   !     enddo
   !  endif
       if(icx == 1) then
         call timer_stop(timer_difu_solve4u, gdp)
         call timer_start(timer_difu_solve5u, gdp)
       else
         call timer_stop(timer_difu_solve6v, gdp)
         call timer_start(timer_difu_solve7v, gdp)
       end if
       do nm = nmsta, nmmax, 2
          if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
             vvdwk(nm, 1) = uvdwk(nm, 1)*bbkl(nm, 1, l)
          endif
       enddo
       do k = 2, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                vvdwk(nm,k) = (uvdwk(nm,k) - aakl(nm,k,l)*vvdwk(nm,k-1)) * bbkl(nm,k,l)
             endif
          enddo
       enddo
       do k = kmax - 1, 1, -1
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                 vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l) *vvdwk(nm, k + 1)
             endif
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                epsitr = max(1.e-9_fp, 1.e-9_fp*abs(r1(nm, k, l)))
                if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) itr = 1
                r1(nm, k, l) = vvdwk(nm, k)
             endif
          enddo
       enddo
       if(icx == 1) then
         call timer_stop(timer_difu_solve5u, gdp)
         call timer_start(timer_difu_solve4u, gdp)
       else
         call timer_stop(timer_difu_solve7v, gdp)
         call timer_start(timer_difu_solve6v, gdp)
       end if
       !
       call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
       !
       ! loop starts at point of other color now (black respectively red)
       !
       nmsta = 3 - nmsta
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                uvdwk(nm, k) =   bdddx(nm, k) * r1(nm - icx - icx - icx, k, l) &
                             & + bddx (nm, k) * r1(nm - icx - icx, k, l)       &
                             & + bdx  (nm, k) * r1(nm - icx, k, l)             &
                             & + bux  (nm, k) * r1(nm + icx, k, l)             &
                             & + buux (nm, k) * r1(nm + icx + icx, k, l)       &
                             & + buuux(nm, k) * r1(nm + icx + icx + icx, k, l)
                uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm, k)
             endif
          enddo
       enddo
    !  if (nst.ge.764.and.nst.le.764.and. iter<=3) THEN
    !     do nm = nmsta, nmmax, 2         
    !         if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then   
    !            do k=1,kmax
    !              write(9892003,'(5i6,35f21.15)') nst,nm,iter,k,L,uvdwk(nm, k), ddkl(nm,k,l),bdddx(nm,k),bddx(nm,k),bdx (nm,k), &
    !                           bux(nm,k),buux(nm,k),buuux(nm,k),bbkl(nm, k,l),aakl(nm, k,l),cckl(nm, k,l),&
    !                           r1(nm - icx - icx - icx, k, l),r1(nm - icx - icx, k, l),r1(nm - icx, k, l),r1(nm + icx, k, l),&
    !                           r1(nm + icx + icx, k, l), r1(nm + icx + icx + icx, k, l)
    !            enddo
    !         endif
    !     enddo
    !  endif
       if(icx == 1) then
         call timer_stop(timer_difu_solve4u, gdp)
         call timer_start(timer_difu_solve5u, gdp)
       else
         call timer_stop(timer_difu_solve6v, gdp)
         call timer_start(timer_difu_solve7v, gdp)
       end if
       do nm = nmsta, nmmax, 2
          if (kfs(nm)==1 .and. kcs(nm) == 1) then
             vvdwk(nm, 1) = uvdwk(nm, 1)*bbkl(nm, 1, l)
          endif
       enddo
       do k = 2, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                vvdwk(nm, k) = (uvdwk(nm,k) -  aakl(nm,k,l)*vvdwk(nm,k-1)) * bbkl(nm,k,l)
             endif 
          enddo
       enddo
       do k = kmax - 1, 1, -1
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l) *vvdwk(nm, k + 1)
             endif 
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                epsitr = max(1.e-9_fp, 1.e-9_fp*abs(r1(nm, k, l)))
                if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) itr = 1
                r1(nm, k, l) = vvdwk(nm, k)
             endif
          enddo
       enddo
       if(icx == 1) then
         call timer_stop(timer_difu_solve5u, gdp)
       else
         call timer_stop(timer_difu_solve7v, gdp)
       end if
       call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
       !
       ! determine global maximum of 'itr' over all nodes
       ! Note: this enables to synchronize the iteration process
       !
       call dfreduce_gdp( itr, 1, dfint, dfmax, gdp )
       !
       if (itr>0 .and. iter<50) goto 1100
       !
       if (gdp%gdflwpar%flwoutput%iteroutputsteps >= gdp%gdinttim%ntstep) then
          write (lundia, '(3(a,i0))') 'difu(ntstep,l,iter):',gdp%gdinttim%ntstep, ' ', l, ' ',iter
       endif
       if (iter >= 50) then
          write (errtxt, '(i0,a,i0)') l, ' ', nst
          call prterr(lundia    ,'S206'    ,trim(errtxt)    )
       endif
       !
       ! DD code added:
       !
       !
       !       D3dFlow_Solve_ADI_Conc: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_adi_conc, gdp)
       if (nhystp == d3dflow_solve_adi_conc) goto 111
       !
       ! DD code added end
       !
       ! Nudging of constituents at open boundaries
       !
       if (nudge==1) then
          ! Nudging layer
          nnudge    = 4
          nudgefac  = 10.0_fp
          tnudge    = hdt
          mu(1)     = max(hdt / tnudge, 1.0_fp)
          do jj = 2, nnudge
             mu(jj) = mu(jj-1) / nudgefac
          enddo
          !
          do nm = 1, nmmax
             nmu = nm + icx
             nmd = nm - icx
             if (kcs(nmd) == 2 .and. kcs(nm) == 1 ) then
                nms(1) = nm
                do jj = 2, nnudge
                   nms(jj) = nms(jj-1) + icx
                enddo
                do jj = 1, nnudge
                   do k = 1, kmax
                      if (r1(nmd, k, l )>1.0e-6) then
                         rb = r1(nmd, k, l )
                         rp = r1(nms(jj), k, l)
                         r1(nms(jj), k, l) = rp + mu(jj)*(rb-rp)
                         r0(nms(jj), k, l) = r1(nms(jj), k, l)
                      endif
                   enddo
                enddo
             endif
             if (kcs(nmu) == 2 .and. kcs(nm) == 1) then
                nms(1) = nm
                do jj = 2, nnudge
                   nms(jj) = nms(jj-1) - icx
                enddo
                do jj = 1, nnudge
                   do k = 1, kmax
                      if (r1(nmu, k, l )>1.0e-6) then
                         rb = r1(nmu, k, l )
                         rp = r1(nms(jj), k, l)
                         r1(nms(jj), k, l) = rp + mu(jj)*(rb-rp)
                         r0(nms(jj), k, l) = r1(nms(jj), k, l)
                      endif
                   enddo
                enddo
             endif
          enddo
       endif
       !
    enddo
    if (nst.ge.idebugCUThardINI.and.nst.le.idebugCUThardFIN) THEN
       do k =1,nmmax
         write(9891998,'(2i6,15f21.15)') nst,k,r0(k,1,1),r1(k,1,1) 
       enddo
    endif
    !
    if (bnd_distr_avgc.or.bnd_distr_perC) then
   !    deallocate( cavg, cflx )
    endif
    firstCALL = .false.
end subroutine difu
