subroutine tritra(stage     ,lundia    ,nst       ,icreep    , &
                & trasol    ,j         ,nmmaxj    ,eulerisoglm, &
                & nmmax     ,nmax      ,mmax      ,kmax      ,lstsci    , &
                & lstsc     ,lsal      ,ltem      ,lsecfl    ,lsec      , &
                & lsed      ,lsts      ,norow     ,nocol     ,irocol    , &
                & kcs       ,kcu       ,kcv       ,kfs       ,kfu       , &
                & kfv       ,kadu      ,kadv      ,alfas     ,s0        , &
                & s1        ,hu        ,hv        ,dps       ,qxk       , &
                & qyk       ,qzk       ,guu       ,gvv       ,guv       , &
                & gvu       ,gsqs      ,rbnd      ,sigdif    ,sigmol    , &
                & r0        ,r1        ,sour      ,sink      ,ws        , &
                & sedtyp    ,thick     ,sig       ,dicuv     , &
                & vicww     ,dsdksi    ,dsdeta    ,dtdksi    ,dtdeta    , &
                & aak       ,bbk       ,cck       ,bdddx     ,bddx      , &
                & bdx       ,bux       ,buux      ,buuux     , &
                & uvdwk     ,vvdwk     ,aakl      ,bbkl      ,cckl      , &
                & ddkl      ,eqmbcsand ,seddif    ,cgc       , &
                & theta     ,wsu       ,wsv       ,xcor      ,ycor      , &
                & vicuv     ,c         ,tp        ,qxkw      ,qykw      , &
                & qxkr      ,qykr      ,grmasu    ,grmasv    ,eqmbcmud  , &
                & ewabr0    ,ewabr1    ,grfacu    ,grfacv    ,df        , &
                & ewave0    ,ewave1    ,eroll0    ,eroll1    ,sinkw     , &
                & sourw     ,sinkr     ,sourr     ,fxw       ,fyw       , &
                & wenf      ,wenl      ,dis       ,grmsur    ,grmsvr    , &
                & areau     ,areav     ,volum0    ,volum1    ,xz        , &
                & yz        ,rlabda    ,hbd       ,rscale    ,bruvai    , &
                & hrms      ,dzs1      ,kfsmin    ,kfsmax    ,gdp       )
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
!    Function: Main routine to compute transports for conserva-
!              tive constituents (Salinity, Temperature, user
!              defined constituents and Spiral intensity)
! Method used:
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
    real(fp)      , pointer :: f_lam
    character(5)  , pointer :: disform
    real(fp)      , pointer :: hdt
    logical       , pointer :: wave
    logical       , pointer :: roller
    logical       , pointer :: wavcmp
    logical       , pointer :: snelli
!
! Global variables
!
    integer                                                         :: icreep       !  Description and declaration in tricom.igs
    integer                                                         :: j            !!  Begin pointer for arrays which have
                                                                                    !!  been transformed into 1D arrays.
                                                                                    !!  Due to the shift in the 2nd (M-)
                                                                                    !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax         !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: lsal         !  Description and declaration in dimens.igs
    integer                                                         :: lsec         !  Description and declaration in dimens.igs
    integer                                                         :: lsecfl       !  Description and declaration in dimens.igs
    integer                                                         :: lsed         !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: lsts         !  Description and declaration in dimens.igs
    integer                                                         :: lstsc        !  Description and declaration in dimens.igs
    integer                                                         :: lstsci       !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: ltem         !  Description and declaration in dimens.igs
    integer                                                         :: lundia       !  Description and declaration in inout.igs
    integer                                                         :: mmax         !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: nmax         !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax        !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj       !  Description and declaration in dimens.igs
    integer                                                         :: nocol        !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: norow        !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nst
    integer   , dimension(5, norow + nocol)                         :: irocol       !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcs          !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcu          !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcv          !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfs          !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfu          !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfv          !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsmax       !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsmin       !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: kadu         !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: kadv         !  Description and declaration in esm_alloc_int.f90
    logical                                                         :: eqmbcsand    !  Description and declaration in morpar.igs
    logical                                                         :: eqmbcmud     !  Description and declaration in morpar.igs
    logical                                                         :: eulerisoglm  !  Description and declaration in morpar.igs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: alfas        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: c            !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: cgc          !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,4)                  :: dis          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: df           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs1         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: eroll0       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: eroll1       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: ewabr0       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: ewabr1       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: ewave0       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: ewave1       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: fxw          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: fyw          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gsqs         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: grmasu       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: grmasv       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: grmsur       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: grmsvr       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: grfacu       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: grfacv       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guu          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guv          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvu          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvv          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hbd          !  breaker delay depth
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hrms         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hu           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hv           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: qxkr         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: qxkw         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: qykr         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: qykw         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: rlabda       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s0           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s1           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: sinkr        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: sinkw        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: sourr        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: sourw        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: theta
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: tp           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: wsu          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: wsv          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: xcor         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: ycor         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: xz           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: yz           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: bruvai       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: vicww        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: qzk          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)      :: seddif       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)      :: ws           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)          :: vicuv        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak          !!  Internal work array (in CUCNP & UZD)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areau
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areav
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbk          !!  Internal work array (in CUCNP & UZD)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bdddx        !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M-3,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bddx         !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M-2,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bdx          !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M-1,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: buuux        !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M+3,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: buux         !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M+2,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bux          !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M+1,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck          !!  Internal work array (in CUCNP & UZD)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)            :: dicuv        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dsdeta       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dsdksi       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dtdeta       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dtdksi       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: qxk          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: qyk          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rscale       !  Internal work array, row scaling parameter in difu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: uvdwk        !!  Internal work array for Jac.iteration
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: volum0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: volum1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: vvdwk        !!  Internal work array for Jac.iteration
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: aakl         !!  Internal work array, lower diagonal
                                                                                    !!  tridiagonal matrix, implicit coupling
                                                                                    !!  of concentration in (N,M,K) with con-
                                                                                    !!  centration in (N,M,K-1)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: bbkl         !!  Internal work array, main diagonal
                                                                                    !!  tridiagonal matrix, implicit coupling
                                                                                    !!  of concentration in (N,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: cckl         !!  Internal work array, upper diagonal
                                                                                    !!  tridiagonal matrix, implicit coupling
                                                                                    !!  of concentration in (N,M,K) with con-
                                                                                    !!  centration in (N,M,K+1)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: ddkl         !!  Internal work array, diagonal space
                                                                                    !!  at (N,M,K,L)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: r0           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: r1           !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: sink         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)      :: sour         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                     :: sig          !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                     :: thick        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax, max(lstsc, 1), 2, norow+nocol)      :: rbnd         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(lstsci)                                   :: sigdif       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(lstsci)                                   :: sigmol       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(norow + nocol)                            :: wenf         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(norow + nocol)                            :: wenl         !  Description and declaration in esm_alloc_real.f90
    character(13)                                     , intent(in)  :: trasol       !  Description and declaration in tricom.igs
    integer, dimension(lsed)                                        :: sedtyp       !!  sediment type: 0=total/1=noncoh/2=coh
    character(8)                                      , intent(in)  :: stage        !!  First or second half time step
!
! Local variables
!
    integer                                   :: icx
    integer                                   :: icy
    integer                                   :: nmaxddb
    integer                                   :: i
    integer                                   :: istat
    integer                                   :: k
    real(fp)                                  :: timest  ! Time step to use for transport solver depending on options
    real(fp) , dimension(:,:), allocatable    :: qykfac

!! executable statements -------------------------------------------------------
!
    f_lam      => gdp%gdbetaro%f_lam
    disform    => gdp%gdbetaro%disform
    hdt        => gdp%gdnumeco%hdt
    wave       => gdp%gdprocs%wave
    roller     => gdp%gdprocs%roller
    wavcmp     => gdp%gdprocs%wavcmp
    snelli     => gdp%gdprocs%snelli
    !
    icx     = 0
    icy     = 0
    nmaxddb = nmax + 2*gdp%d%ddbound
    !
    ! Longshore discharges set to zero for beach profile mode
    !
    if (snelli) then
       allocate(qykfac(gdp%d%nmlb:gdp%d%nmub, kmax), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'Tritra: memory alloc error')
          call d3stop(1, gdp)
       else
          qykfac = 0.0_fp
       endif
    endif
    !=======================================================================
    ! First half time step
    !=======================================================================
    !
    if (stage == 'stage1') then
       !
       ! ADI method
       ! with TIMEST = HDT
       ! and  ICX    = NMAX, ICY    = 1
       !
       timest = hdt
       !
       ! Transport solver 'cyclic-method'
       !
       if (trasol == 'cyclic-method') then
          gdp%dd%difuiter = 0
          if (lstsci > 0) then
             icx = nmaxddb
             icy = 1
             if (roller) then
                 call rolcor(hrms      ,tp        ,theta     ,hu        ,hv         , &
                           & guu       ,gvv       ,qxk       ,qyk       ,eulerisoglm, &
                           & grmasu    ,grmasv    ,grfacu    ,grfacv    ,grmsur     , &
                           & grmsvr    ,nmax      ,mmax      ,kmax      ,thick      , &
                           & dzs1      ,kfsmin    ,kfsmax    ,sig       ,1.0_fp     , &
                           & kfu       ,kcu       ,kfv       ,kcv       ,gdp        )
             endif
             call timer_start(timer_1stdifu, gdp)
             if (snelli) then
                call difu(icreep    ,timest    ,lundia    ,nst       ,icx       , &
                        & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                        & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
                        & lsec      ,lsed      ,lsts      ,norow     ,irocol    , &
                        & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                        & kadu      ,kadv      ,s0        ,s1        ,hu        , &
                        & hv        ,dps       ,qxk       ,qykfac    ,qzk       , &  ! qykfac
                        & guu       ,gvv       ,guv       ,gvu       ,gsqs      , &
                        & rbnd      ,sigdif    ,sigmol    ,r0        ,r1        , &
                        & sour      ,sink      ,ws        ,sedtyp    ,thick     , &
                        & sig       ,dicuv     ,vicww     ,dsdksi    ,dsdeta    , &
                        & dtdksi    ,dtdeta    ,aak       ,bbk       ,cck       , &
                        & bdddx     ,bddx      ,bdx       ,bux       ,buux      , &
                        & buuux     ,uvdwk     ,vvdwk     ,areau     ,areav     , &
                        & aakl      ,bbkl      ,cckl      ,ddkl      , &
                        & eqmbcsand ,eqmbcmud  ,seddif    ,volum0    ,volum1    , &
                        & rscale    ,bruvai    ,gdp       )
             else
                call difu(icreep    ,timest    ,lundia    ,nst       ,icx       , &
                        & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                        & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
                        & lsec      ,lsed      ,lsts      ,norow     ,irocol    , &
                        & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                        & kadu      ,kadv      ,s0        ,s1        ,hu        , &
                        & hv        ,dps       ,qxk       ,qyk       ,qzk       , &  ! qyk
                        & guu       ,gvv       ,guv       ,gvu       ,gsqs      , &
                        & rbnd      ,sigdif    ,sigmol    ,r0        ,r1        , &
                        & sour      ,sink      ,ws        ,sedtyp    ,thick     , &
                        & sig       ,dicuv     ,vicww     ,dsdksi    ,dsdeta    , &
                        & dtdksi    ,dtdeta    ,aak       ,bbk       ,cck       , &
                        & bdddx     ,bddx      ,bdx       ,bux       ,buux      , &
                        & buuux     ,uvdwk     ,vvdwk     ,areau     ,areav     , &
                        & aakl      ,bbkl      ,cckl      ,ddkl      , &
                        & eqmbcsand ,eqmbcmud  ,seddif    ,volum0    ,volum1    , &
                        & rscale    ,bruvai    ,gdp       )
             endif
             call timer_stop(timer_1stdifu, gdp)
             !
             call timer_start(timer_tritra_rest, gdp)
             call difuflux(stage     ,lundia    ,kmax      ,nmmax     ,nmmaxj    , &
                         & lstsci    ,r0        ,r1        ,qxk       ,qyk       , &
                         & dicuv     ,guv       ,gvu       ,areau     ,areav     , &
                         & kfu       ,kfv       ,kfs       ,kcs       ,timest    , &
                         & icx       ,icy       ,lsed      ,s1        ,dps       , &
                         & gdp       )
             if (roller) then
                 call rolcor(hrms      ,tp        ,theta     ,hu        ,hv         , &
                           & guu       ,gvv       ,qxk       ,qyk       ,eulerisoglm, &
                           & grmasu    ,grmasv    ,grfacu    ,grfacv    ,grmsur     , &
                           & grmsvr    ,nmax      ,mmax      ,kmax      ,thick      , &
                           & dzs1      ,kfsmin    ,kfsmax    ,sig       ,-1.0_fp    , &
                           & kfu       ,kcu       ,kfv       ,kcv       ,gdp       )
             endif
             call timer_stop(timer_tritra_rest, gdp)
          endif
          if (roller) then
             !
             ! f_lam < 0 implies breaker delay applied on water depth in wave model
             !
             call timer_start(timer_tritra_rest, gdp)
             if (f_lam < 0.0) then
                call hds_wf(kfs      ,dps      ,s0       ,xz       ,yz       , &
                          & nmax     ,mmax     ,theta    ,rlabda   ,lundia   , &
                          & hbd      ,f_lam    ,gdp      )
             endif
             if (disform == 'R2004') then
                !
                ! propagate property 'wave is broken' ewabr
                !
                icx   = nmaxddb
                icy   = 1
                sourw = 0.0
                sinkw = 0.0
                call difuwe(timest    ,lundia    ,nst       ,icx       ,icy       , &
                          & nmmax     ,norow     ,irocol    ,kadu      ,kadv      , &
                          & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                          & kmax      , &
                          & qxkw      ,qykw      ,gsqs      ,ewabr0    ,ewabr1    , &
                          & sourw     ,sinkw     ,bbk       ,bdddx     ,bddx      , &
                          & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                          & vvdwk     ,bbkl      ,ddkl      ,1         ,wavcmp    , &
                          & wenf      ,wenl      ,dps       ,s0        ,gdp       )
             endif
             call waveu(nmmax     ,kfs       ,sourw     , &
                      & sinkw     ,ewave0    ,dps       ,s0        ,tp        , &
                      & c         ,hbd       ,ewabr1    ,df        ,gdp       )
             icx = nmaxddb
             icy = 1
             call difuwe(timest    ,lundia    ,nst       ,icx       ,icy       , &
                       & nmmax     ,norow     ,irocol    ,kadu      ,kadv      , &
                       & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                       & kmax      , &
                       & qxkw      ,qykw      ,gsqs      ,ewave0    ,ewave1    , &
                       & sourw     ,sinkw     ,bbk       ,bdddx     ,bddx      , &
                       & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                       & vvdwk     ,bbkl      ,ddkl      ,0         ,wavcmp    , &
                       & wenf      ,wenl      ,dps       ,s0        ,gdp       )
             call rollu(nmmax     ,kfs       ,sourr     ,df        , &
                      & sinkr     ,sinkw     ,ewave0    ,c         ,eroll0    , &
                      & dis       ,tp        ,dps       ,s0        ,gdp       )
             icx = nmaxddb
             icy = 1
             call difuwe(timest    ,lundia    ,nst       ,icx       ,icy       , &
                       & nmmax     ,norow     ,irocol    ,kadu      ,kadv      , &
                       & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                       & kmax      , &
                       & qxkr      ,qykr      ,gsqs      ,eroll0    ,eroll1    , &
                       & sourr     ,sinkr     ,bbk       ,bdddx     ,bddx      , &
                       & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                       & vvdwk     ,bbkl      ,ddkl      ,2         ,wavcmp    , &
                       & wenf      ,wenl      ,dps       ,s0        ,gdp       )
             call turwav(nmmax     ,kmax      ,kfs       , &
                       & vicuv     ,dis       ,dps       ,s0        ,gdp       )
             call radstr(ewave1    ,eroll1    ,sinkr     ,c         ,cgc       , &
                       & theta     ,wsu       ,wsv       ,fxw       ,fyw       , &
                       & alfas     ,xcor      ,ycor      ,guv       ,gvu       , &
                       & guu       ,gvv       ,gsqs      ,kfs       ,kcs       , &
                       & nmax      ,mmax      ,norow     ,nocol     ,irocol    , &
                       & wavcmp    ,s0        ,dps       ,hu        ,hv        , &
                       & gdp       )
             call timer_stop(timer_tritra_rest, gdp)
          endif
       !
       !
       ! Transport solver 'van leer-2   '
       !
       elseif (trasol == 'van leer-2   ') then
          !
          call timer_start(timer_tritra_rest, gdp)
          gdp%dd%difuiter = 0
          icx = nmaxddb
          icy = 1
          call difuvl(icreep    ,timest    ,lundia    ,nst       ,icx       , &
                    & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                    & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
                    & lsec      ,lsed      ,lsts      ,norow     ,irocol    , &
                    & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                    & kadu      ,kadv      ,s0        ,s1        ,hu        , &
                    & hv        ,dps       ,qxk       ,qyk       ,qzk       , &
                    & guu       ,gvv       ,guv       ,gvu       ,gsqs      , &
                    & rbnd      ,sigdif    ,sigmol    ,r0        ,r1        , &
                    & sour      ,sink      ,ws        ,thick     ,sig       , &
                    & dicuv     ,vicww     ,dsdksi    ,dsdeta    ,dtdksi    , &
                    & dtdeta    ,aak       ,bbk       ,cck       ,bdddx     , &
                    & bddx      ,bdx       ,bux       ,buux      ,buuux     , &
                    & uvdwk     ,vvdwk     ,areau     ,areav     ,volum0    , &
                    & volum1    ,aakl      ,bbkl      ,cckl      ,ddkl      , &
                    & bruvai    ,stage     ,eqmbcsand ,eqmbcmud  ,sedtyp    , &
                    & seddif    ,gdp       )
          call timer_stop(timer_tritra_rest, gdp)
       else
       endif
    endif
    !
    !
    !=======================================================================
    ! Second half time step
    !=======================================================================
    !
    if (stage == 'stage2') then
       !
       ! ADI method
       ! with TIMEST = HDT
       ! and  ICX    = 1   , ICY    = NMAX
       !
       timest = hdt
       !
       ! Transport solver 'cyclic-method'
       !
       if (trasol == 'cyclic-method') then
          gdp%dd%difuiter = 0
          if (lstsci > 0) then
             icx = 1
             icy = nmaxddb
             if (roller) then
                 call rolcor(hrms      ,tp        ,theta     ,hu        ,hv         , &
                           & guu       ,gvv       ,qxk       ,qyk       ,eulerisoglm, &
                           & grmasu    ,grmasv    ,grfacu    ,grfacv    ,grmsur     , &
                           & grmsvr    ,nmax      ,mmax      ,kmax      ,thick      , &
                           & dzs1      ,kfsmin    ,kfsmax    ,sig       ,1.0_fp     , &
                           & kfu       ,kcu       ,kfv       ,kcv       ,gdp       )
             endif
             call timer_start(timer_2nddifu, gdp)
             if (snelli) then
                call difu(icreep    ,timest    ,lundia    ,nst       ,icx       , &
                        & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                        & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
                        & lsec      ,lsed      ,lsts      ,nocol     ,irocol(1,norow+1) , &
                        & kcs       ,kcv       ,kfs       ,kfv       ,kfu       , &
                        & kadv      ,kadu      ,s0        ,s1        ,hv        , &
                        & hu        ,dps       ,qykfac    ,qxk       ,qzk       , &          ! qykfac
                        & gvv       ,guu       ,gvu       ,guv       ,gsqs      , &
                        & rbnd(1,1,1,norow+1)  ,sigdif    ,sigmol    ,r0        ,r1     , &
                        & sour      ,sink      ,ws        ,sedtyp    ,thick     , &
                        & sig       ,dicuv     ,vicww     ,dsdeta    ,dsdksi    , &
                        & dtdeta    ,dtdksi    ,aak       ,bbk       ,cck       , &
                        & bdddx     ,bddx      ,bdx       ,bux       ,buux      , &
                        & buuux     ,uvdwk     ,vvdwk     ,areav     ,areau     , &
                        & aakl      ,bbkl      ,cckl      ,ddkl      , &
                        & eqmbcsand ,eqmbcmud  ,seddif    ,volum0    ,volum1    , &
                        & rscale    ,bruvai    ,gdp       )
             else
                call difu(icreep    ,timest    ,lundia    ,nst       ,icx       , &
                        & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                        & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
                        & lsec      ,lsed      ,lsts      ,nocol     ,irocol(1,norow+1) , &
                        & kcs       ,kcv       ,kfs       ,kfv       ,kfu       , &
                        & kadv      ,kadu      ,s0        ,s1        ,hv        , &
                        & hu        ,dps       ,qyk       ,qxk       ,qzk       , &          ! qyk
                        & gvv       ,guu       ,gvu       ,guv       ,gsqs      , &
                        & rbnd(1,1,1,norow+1)  ,sigdif    ,sigmol    ,r0        ,r1     , &
                        & sour      ,sink      ,ws        ,sedtyp    ,thick     , &
                        & sig       ,dicuv     ,vicww     ,dsdeta    ,dsdksi    , &
                        & dtdeta    ,dtdksi    ,aak       ,bbk       ,cck       , &
                        & bdddx     ,bddx      ,bdx       ,bux       ,buux      , &
                        & buuux     ,uvdwk     ,vvdwk     ,areav     ,areau     , &
                        & aakl      ,bbkl      ,cckl      ,ddkl      , &
                        & eqmbcsand ,eqmbcmud  ,seddif    ,volum0    ,volum1    , &
                        & rscale    ,bruvai    ,gdp       )
             endif
             call timer_stop(timer_2nddifu, gdp)
             !
             call timer_start(timer_tritra_rest, gdp)
             call difuflux(stage     ,lundia    ,kmax      ,nmmax     ,nmmaxj    , &
                         & lstsci    ,r0        ,r1        ,qxk       ,qyk       , &
                         & dicuv     ,guv       ,gvu       ,areau     ,areav     , &
                         & kfu       ,kfv       ,kfs       ,kcs       ,timest    , &
                         & icy       ,icx       ,lsed      ,s1        ,dps       , &
                         & gdp       )
             if (roller) then
                 call rolcor(hrms      ,tp        ,theta     ,hu        ,hv         , &
                           & guu       ,gvv       ,qxk       ,qyk       ,eulerisoglm, &
                           & grmasu    ,grmasv    ,grfacu    ,grfacv    ,grmsur     , &
                           & grmsvr    ,nmax      ,mmax      ,kmax      ,thick      , &
                           & dzs1      ,kfsmin    ,kfsmax    ,sig       ,-1.0_fp    , &
                           & kfu       ,kcu       ,kfv       ,kcv       ,gdp       )
             endif
             call timer_stop(timer_tritra_rest, gdp)
          endif
          if (roller) then
             !
             ! f_lam < 0 implies breaker delay applied on water depth in wave model
             !
             call timer_start(timer_tritra_rest, gdp)
             if (f_lam < 0.0) then
                call hds_wf(kfs      ,dps      ,s0       ,xz       ,yz       , &
                          & nmax     ,mmax     ,theta    ,rlabda   ,lundia   , &
                          & hbd      ,f_lam    ,gdp      )
             endif
             if (disform == 'R2004') then
                 !
                 ! propagate property 'wave is broken' ewabr
                 !
                 sourw = 0.0
                 sinkw = 0.0
                 call difuwe(timest    ,lundia    ,nst       ,icx       ,icy       , &
                           & nmmax     ,nocol     ,irocol(1, norow + 1) ,kadv      ,kadu      , &
                           & kcs       ,kcv       ,kfs       ,kfv       ,kfu       , &
                           & kmax      , &
                           & qykw      ,qxkw      ,gsqs      ,ewabr0    ,ewabr1    , &
                           & sourw     ,sinkw     ,bbk       ,bdddx     ,bddx      , &
                           & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                           & vvdwk     ,bbkl      ,ddkl      ,1         ,wavcmp    , &
                           & wenf(norow+1),wenl(norow+1),dps ,s0        ,gdp       )
             endif
             call waveu(nmmax     ,kfs       ,sourw     , &
                      & sinkw     ,ewave0    ,dps       ,s0        ,tp        , &
                      & c         ,hbd       ,ewabr1    ,df        ,gdp       )
             icx = 1
             icy = nmaxddb
             call difuwe(timest    ,lundia    ,nst       ,icx       ,icy       , &
                       & nmmax     ,nocol     ,irocol(1, norow + 1) ,kadv      ,kadu      , &
                       & kcs       ,kcv       ,kfs       ,kfv       ,kfu       , &
                       & kmax      , &
                       & qykw      ,qxkw      ,gsqs      ,ewave0    ,ewave1    , &
                       & sourw     ,sinkw     ,bbk       ,bdddx     ,bddx      , &
                       & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                       & vvdwk     ,bbkl      ,ddkl      ,0         ,wavcmp    , &
                       & wenf(norow+1),wenl(norow+1),dps ,s0        ,gdp       )
             call rollu(nmmax     ,kfs       ,sourr     ,df        , &
                      & sinkr     ,sinkw     ,ewave0    ,c         ,eroll0    , &
                      & dis       ,tp        ,dps       ,s0        ,gdp       )
             icx = 1
             icy = nmaxddb
             call difuwe(timest    ,lundia    ,nst       ,icx       ,icy       , &
                       & nmmax     ,nocol     ,irocol(1, norow + 1) ,kadv      ,kadu      , &
                       & kcs       ,kcv       ,kfs       ,kfv       ,kfu       , &
                       & kmax      , &
                       & qykr      ,qxkr      ,gsqs      ,eroll0    ,eroll1    , &
                       & sourr     ,sinkr     ,bbk       ,bdddx     ,bddx      , &
                       & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                       & vvdwk     ,bbkl      ,ddkl      ,2         ,wavcmp    , &
                       & wenf(norow+1),wenl(norow+1),dps ,s0        ,gdp       )
             call turwav(nmmax     ,kmax      ,kfs       , &
                       & vicuv     ,dis       ,dps       ,s0        ,gdp       )
             call radstr(ewave1    ,eroll1    ,sinkr     ,c         ,cgc       , &
                       & theta     ,wsu       ,wsv       ,fxw       ,fyw       , &
                       & alfas     ,xcor      ,ycor      ,guv       ,gvu       , &
                       & guu       ,gvv       ,gsqs      ,kfs       ,kcs       , &
                       & nmax      ,mmax      ,norow     ,nocol     ,irocol    , &
                       & wavcmp    ,s0        ,dps       ,hu        ,hv        , &
                       & gdp       )
             call timer_stop(timer_tritra_rest, gdp)
          endif
       !
       ! Transport solver 'van leer-2   '
       !
       elseif (trasol == 'van leer-2   ') then
          call timer_start(timer_tritra_rest, gdp)
          gdp%dd%difuiter = 0
          icx = 1
          icy = nmaxddb
          call difuvl(icreep    ,timest    ,lundia    ,nst       ,icx       , &
                    & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                    & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
                    & lsec      ,lsed      ,lsts      ,nocol     ,irocol(1, norow + 1) , &
                    & kcs       ,kcv       ,kfs       ,kfv       ,kfu       , &
                    & kadv      ,kadu      ,s0        ,s1        ,hv        , &
                    & hu        ,dps       ,qyk       ,qxk       ,qzk       , &
                    & gvv       ,guu       ,gvu       ,guv       ,gsqs      , &
                    & rbnd(1, 1, 1, norow + 1)        ,sigdif    ,sigmol    ,r0        ,r1        , &
                    & sour      ,sink      ,ws        ,thick     ,sig       , &
                    & dicuv     ,vicww     ,dsdeta    ,dsdksi    ,dtdeta    , &
                    & dtdksi    ,aak       ,bbk       ,cck       ,bdddx     , &
                    & bddx      ,bdx       ,bux       ,buux      ,buuux     , &
                    & uvdwk     ,vvdwk     ,areav     ,areau     ,volum0    , &
                    & volum1    ,aakl      ,bbkl      ,cckl      ,ddkl      , &
                    & bruvai    ,stage     ,eqmbcsand ,eqmbcmud  ,sedtyp    , &
                    & seddif    ,gdp       )
          call timer_stop(timer_tritra_rest, gdp)
       else
       endif
    endif
    !
    ! Deallocate possible local array
    !
    if (snelli) then
       deallocate(qykfac, stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'Tritra: memory dealloc error')
          call d3stop(1, gdp)
       endif
    endif
end subroutine tritra
