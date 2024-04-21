subroutine near_field(u0     ,v0     ,rho      ,thick  , &
                    & kmax   ,alfas  ,dps      ,s0     , &
                    & lstsci ,lsal   ,ltem     ,xz     , &
                    & yz     ,nmmax  ,nflrwmode,namcon , &
                    & kcs    ,kfu    ,kfv      , &
                    & r0     ,time   ,saleqs   ,temeqs , &
                    & s1     ,kfsmn0 ,kfsmx0   ,dzs0   , &
                    & sig    ,zk     ,gdp   )
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
!    Function: Converts flow results to cormix input
!              To do:
!              1) interpolete to equidistant layer distribution jet3d
! Parallel:
!    All parameters are in n,m instead of nm, because:
!    - dfgather produces n,m arrays
!    - conversion subroutines n,m <=> nm do not work on global arrays
!    - subroutines can be called with specified dimensions, such that all parallel
!      stuff is concentrated in this near_field subroutine
!
!    The master partition gathers all input arrays on the full global domain,
!    handles the communication with Cosumo/Cormix,
!    and calculates the arrays glb_disnf and glb_sournf (both on the full global
!    domain in n,m).
!    glb_disnf and glb_sournf are distributed to all partitions (call dfbroadc).
!    Each partition copies his part of these arrays to the local disnf/sournf in
!    nm.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use system_utils, only: FILESEP, directory_exists, makedir
    use dfparall
    use dffunctionals, only: dfgather
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer                            , pointer :: idensform
    integer                            , pointer :: ifirst
    integer                            , pointer :: no_dis
    integer       , dimension(:)       , pointer :: m_diff
    integer       , dimension(:)       , pointer :: n_diff
    integer       , dimension(:)       , pointer :: no_amb
    integer       , dimension(:,:)     , pointer :: m_amb
    integer       , dimension(:,:)     , pointer :: n_amb
    integer       , dimension(:)       , pointer :: m_intake
    integer       , dimension(:)       , pointer :: n_intake
    integer       , dimension(:)       , pointer :: k_intake
    real(fp)      , dimension(:)       , pointer :: x_diff
    real(fp)      , dimension(:)       , pointer :: y_diff
    real(fp)      , dimension(:,:)     , pointer :: x_amb
    real(fp)      , dimension(:,:)     , pointer :: y_amb
    real(fp)      , dimension(:)       , pointer :: x_intake
    real(fp)      , dimension(:)       , pointer :: y_intake
    real(fp)      , dimension(:)       , pointer :: z_intake
    real(fp)      , dimension(:)       , pointer :: q_diff
    real(fp)      , dimension(:,:)     , pointer :: const_diff
    real(fp)      , dimension(:)       , pointer :: rho0_diff
    real(fp)      , dimension(:)       , pointer :: d0
    real(fp)      , dimension(:)       , pointer :: h0
    real(fp)      , dimension(:)       , pointer :: sigma0
    real(fp)      , dimension(:)       , pointer :: theta0
    real(fp)      , dimension(:,:,:)   , pointer :: disnf
    real(fp)      , dimension(:,:,:)   , pointer :: disnf_intake
    real(fp)      , dimension(:,:,:)   , pointer :: disnf_entr
    real(fp)      , dimension(:,:,:)   , pointer :: nf_src_momu
    real(fp)      , dimension(:,:,:)   , pointer :: nf_src_momv
    real(fp)      , dimension(:,:,:,:) , pointer :: sournf
    character(256), dimension(:)       , pointer :: waitfilesold
    character(256), dimension(:,:)     , pointer :: basecase
    character(256)                     , pointer :: nflmod
    integer                            , pointer :: lundia
    integer                            , pointer :: mfg
    integer                            , pointer :: mlg
    integer                            , pointer :: nfg
    integer                            , pointer :: nlg
    integer                            , pointer :: nmaxgl
    integer                            , pointer :: mmaxgl
    integer       , dimension(:,:)     , pointer :: iarrc
    integer       , dimension(:)       , pointer :: mf
    integer       , dimension(:)       , pointer :: ml
    integer       , dimension(:)       , pointer :: nf
    integer       , dimension(:)       , pointer :: nl
    logical                            , pointer :: zmodel
	logical                            , pointer :: nf_src_mom
	logical                            , pointer :: skipuniqueid

!
! Global variables
!
    integer                                                                       , intent(in)         :: kmax     !  Description and declaration in
    integer                                                                       , intent(in)         :: lstsci
    integer                                                                       , intent(in)         :: lsal
    integer                                                                       , intent(in)         :: ltem
    integer                                                                       , intent(in)         :: nmmax
    integer                                                                       , intent(in)         :: nflrwmode
    real(fp)                                                                      , intent(in)         :: time
    real(fp)                                                                      , intent(in)         :: saleqs
    real(fp)                                                                      , intent(in)         :: temeqs
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: kcs
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: kfu
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: kfv
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: kfsmn0   !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: kfsmx0   !  Description and declaration in esm_alloc_int.f90
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: s0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: s1       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: xz       !  Description and declaration in
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: yz       !  Description and declaration in
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: alfas    !  Description and declaration in
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in), target :: dzs0     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in), target :: rho      !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in), target :: u0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in), target :: v0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax,lstsci) , intent(in), target :: r0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                                                  , intent(in)         :: thick    !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                                                  , intent(in)         :: sig      !  Vertical coordinates of cell interfaces (SIGMA-MODEL)
    real(fp)   , dimension(0:kmax)                                                , intent(in)         :: zk       !  Vertical coordinates of cell interfaces (Z-MODEL)
    real(prec) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in), target :: dps      !  Description and declaration in esm_alloc_real.f90
    character(20), dimension(lstsci)                                              , intent(in)         :: namcon   !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                             :: ifatal
    integer                                             :: i
    integer                                             :: iamb
    integer                                             :: idis
    integer                                             :: ierror
    integer                                             :: ii
    integer                                             :: jj
    integer                                             :: imom
    integer                                             :: istat
    integer                                             :: m
    integer                                             :: mlb
    integer                                             :: mub
    integer                                             :: n
    integer                                             :: nlb
    integer                                             :: nub
    integer                                             :: nm
    integer                                             :: k_dummy
    integer                                             :: seedSize
    integer                                             :: seedIrand
    integer, dimension(:), allocatable                  :: seed
    real(fp)                                            :: dummy
    real(fp)                                            :: flwang
    real(fp)                                            :: signx
    real(fp), dimension(10)                             :: linkinf ! 1: dis_sal, 2: dis_temp, 3: dis_dens, 4: amb_vel, 5: amb_dir, 6: rel_dir, 7: n_start, 8: m_start, 9: n_end, 10: m_end
    real(fp)                                            :: xstart
    real(fp)                                            :: xend
    real(fp)                                            :: ystart
    real(fp)                                            :: yend
    integer , dimension(:,:)      , allocatable, target :: glb_kcs
    integer , dimension(:,:)      , allocatable, target :: glb_kfu
    integer , dimension(:,:)      , allocatable, target :: glb_kfv
    integer , dimension(:,:)      , allocatable, target :: glb_kfsmx0
    integer , dimension(:,:)      , allocatable, target :: glb_kfsmn0
    real(fp), dimension(:,:)      , allocatable, target :: glb_alfas
    real(fp), dimension(:,:,:)    , allocatable, target :: glb_dzs0
    real(fp), dimension(:,:)      , allocatable, target :: glb_s0
    real(fp), dimension(:,:,:,:)  , allocatable, target :: glb_r0
    real(fp), dimension(:,:,:)    , allocatable, target :: glb_rho
    real(fp), dimension(:,:,:)    , allocatable, target :: glb_u0
    real(fp), dimension(:,:,:)    , allocatable, target :: glb_v0
    real(hp), dimension(:,:)      , allocatable, target :: glb_dps
    real(fp), dimension(:,:)      , allocatable, target :: glb_s1
    real(fp), dimension(:,:)      , allocatable, target :: glb_xz
    real(fp), dimension(:,:)      , allocatable, target :: glb_yz
    real(fp), dimension(:,:,:,:)  , allocatable, target :: glb_disnf
    real(fp), dimension(:,:,:,:)  , allocatable, target :: glb_disnf_intake
    real(fp), dimension(:,:,:,:)  , allocatable, target :: glb_disnf_entr
    real(fp), dimension(:,:,:,:,:), allocatable, target :: glb_sournf
    real(fp), dimension(:,:,:,:)  , allocatable, target :: glb_nf_src_momu
    real(fp), dimension(:,:,:,:)  , allocatable, target :: glb_nf_src_momv
    integer , dimension(:,:)      , pointer             :: kcs_ptr
    integer , dimension(:,:)      , pointer             :: kfu_ptr
    integer , dimension(:,:)      , pointer             :: kfv_ptr
    integer , dimension(:,:)      , pointer             :: kfsmx0_ptr
    integer , dimension(:,:)      , pointer             :: kfsmn0_ptr
    real(fp), dimension(:,:)      , pointer             :: alfas_ptr
    real(fp), dimension(:,:,:)    , pointer             :: dzs0_ptr
    real(fp), dimension(:,:)      , pointer             :: s0_ptr
    real(fp), dimension(:,:,:,:)  , pointer             :: r0_ptr
    real(fp), dimension(:,:,:)    , pointer             :: rho_ptr
    real(fp), dimension(:,:,:)    , pointer             :: u0_ptr
    real(fp), dimension(:,:,:)    , pointer             :: v0_ptr
    real(hp), dimension(:,:)      , pointer             :: dps_ptr
    real(fp), dimension(:,:)      , pointer             :: s1_ptr
    real(fp), dimension(:,:)      , pointer             :: xz_ptr
    real(fp), dimension(:,:)      , pointer             :: yz_ptr
    logical                                             :: corend
    logical                                             :: first_time
    logical                                             :: error
    logical                                             :: error_reading
    logical                                             :: inside
    logical                                             :: waitlog       ! Write the names of the files to wait for to screen, only the first time that subroutine wait_until_finished is visited
    character(3)                                        :: c_inode
    character(3)                                        :: c_idis
    character(14)                                       :: cctime
    character(256), dimension(3)                        :: filename
    character(256), dimension(:), allocatable           :: waitfiles
    character(300)                                      :: errmsg
!
!! executable statements -------------------------------------------------------
!
    zmodel         => gdp%gdprocs%zmodel
    idensform      => gdp%gdphysco%idensform
    nflmod         => gdp%gdnfl%nflmod
    no_dis         => gdp%gdnfl%no_dis
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    no_amb         => gdp%gdnfl%no_amb
    m_amb          => gdp%gdnfl%m_amb
    n_amb          => gdp%gdnfl%n_amb
    m_intake       => gdp%gdnfl%m_intake
    n_intake       => gdp%gdnfl%n_intake
    k_intake       => gdp%gdnfl%k_intake
    x_diff         => gdp%gdnfl%x_diff
    y_diff         => gdp%gdnfl%y_diff
    x_amb          => gdp%gdnfl%x_amb
    y_amb          => gdp%gdnfl%y_amb
    x_intake       => gdp%gdnfl%x_intake
    y_intake       => gdp%gdnfl%y_intake
    z_intake       => gdp%gdnfl%z_intake
    q_diff         => gdp%gdnfl%q_diff
    const_diff     => gdp%gdnfl%const_diff
    rho0_diff      => gdp%gdnfl%rho0_diff
    d0             => gdp%gdnfl%d0
    h0             => gdp%gdnfl%h0
    sigma0         => gdp%gdnfl%sigma0
    theta0         => gdp%gdnfl%theta0
    waitfilesold   => gdp%gdnfl%waitfilesold
    basecase       => gdp%gdnfl%basecase
    disnf          => gdp%gdnfl%disnf
    disnf_intake   => gdp%gdnfl%disnf_intake
    disnf_entr     => gdp%gdnfl%disnf_entr
    sournf         => gdp%gdnfl%sournf
    nf_src_momu    => gdp%gdnfl%nf_src_momu
    nf_src_momv    => gdp%gdnfl%nf_src_momv
    lundia         => gdp%gdinout%lundia
    mfg            => gdp%gdparall%mfg
    mlg            => gdp%gdparall%mlg
    nfg            => gdp%gdparall%nfg
    nlg            => gdp%gdparall%nlg
    mf             => gdp%gdparall%mf
    ml             => gdp%gdparall%ml
    nf             => gdp%gdparall%nf
    nl             => gdp%gdparall%nl
    iarrc          => gdp%gdparall%iarrc
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
    nf_src_mom     => gdp%gdnfl%nf_src_mom
    skipuniqueid   => gdp%gdnfl%skipuniqueid

    filename = ' '
    !    
    write(c_inode,'(i3.3)') inode
    !
    ! By defining the dimensions nlb, nub, mlb, mub and using the _ptr variants,
    ! the calls to wri_FF2NF and desa are the same for both parallel and sequential
    !
    if (parll) then
       nlb = 1
       nub = nmaxgl
       mlb = 1
       mub = mmaxgl
       !
       ! A call to dfgather will cause that the second parameter (e.g. glb_kcs) will be (re-)allocated for the master partition only
       ! The parameter is undefined for the other partitions
       !
       call dfgather(kcs   , glb_kcs   , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(kfu   , glb_kfu   , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(kfv   , glb_kfv   , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(kfsmx0, glb_kfsmx0, nf, nl, mf, ml, iarrc, gdp)
       call dfgather(kfsmn0, glb_kfsmn0, nf, nl, mf, ml, iarrc, gdp)
       call dfgather(alfas , glb_alfas , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(dzs0  , glb_dzs0  , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(s0    , glb_s0    , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(r0    , glb_r0    , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(rho   , glb_rho   , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(u0    , glb_u0    , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(v0    , glb_v0    , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(dps   , glb_dps   , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(s1    , glb_s1    , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(xz    , glb_xz    , nf, nl, mf, ml, iarrc, gdp)
       call dfgather(yz    , glb_yz    , nf, nl, mf, ml, iarrc, gdp)
       if (inode == master) then
          kcs_ptr    => glb_kcs
          kfu_ptr    => glb_kfu
          kfv_ptr    => glb_kfv
          kfsmx0_ptr => glb_kfsmx0
          kfsmn0_ptr => glb_kfsmn0
          alfas_ptr  => glb_alfas
          dzs0_ptr   => glb_dzs0
          s0_ptr     => glb_s0
          s1_ptr     => glb_s1
          r0_ptr     => glb_r0
          rho_ptr    => glb_rho
          u0_ptr     => glb_u0
          v0_ptr     => glb_v0
          dps_ptr    => glb_dps
          xz_ptr     => glb_xz
          yz_ptr     => glb_yz
       endif
    else
       nlb = gdp%d%nlb
       nub = gdp%d%nub
       mlb = gdp%d%mlb
       mub = gdp%d%mub
       kcs_ptr    => kcs
       kfu_ptr    => kfu
       kfv_ptr    => kfv
       kfsmx0_ptr => kfsmx0
       kfsmn0_ptr => kfsmn0
       alfas_ptr  => alfas
       dzs0_ptr   => dzs0
       s0_ptr     => s0
       s1_ptr     => s1
       r0_ptr     => r0
       rho_ptr    => rho
       u0_ptr     => u0
       v0_ptr     => v0
       dps_ptr    => dps
       xz_ptr     => xz
       yz_ptr     => yz
       xz_ptr     => xz
    endif
    !
    if (nflrwmode /= NFLWRITE) then
       ! Only allocate glb_disnf/glb_sournf when reading near field files and performing
       ! the related computations
       !
       ! glb_disnf and glb_sournf must be allocated by all partitions:
       ! After the master partition has calculated them, they will be
       ! copied to all partitions
       !
       allocate(glb_disnf        (nlb:nub,mlb:mub,1:kmax,1:no_dis)         , stat=ierror)
       allocate(glb_disnf_intake (nlb:nub,mlb:mub,1:kmax,1:no_dis)         , stat=ierror)
       allocate(glb_disnf_entr   (nlb:nub,mlb:mub,1:kmax,1:no_dis)         , stat=ierror)
       allocate(glb_sournf       (nlb:nub,mlb:mub,1:kmax,1:lstsci,1:no_dis), stat=ierror)
       allocate(glb_nf_src_momu  (nlb:nub,mlb:mub,1:kmax,1:no_dis)         , stat=ierror)
       allocate(glb_nf_src_momv  (nlb:nub,mlb:mub,1:kmax,1:no_dis)         , stat=ierror)
       if (ierror /= 0) then
          call prterr(lundia, 'U021', 'near_field: memory allocation error')
          call d3stop(1, gdp)
       endif
    endif
    !
    ! Both for parallel and sequential:
    ! Only the master partition communicates with Cosumo/Cormix and calculates glb_disnf and glb_sournf
    !
    if (inode == master) then
       ifatal = 0
       !
       ! Convert flow results (velocities and densities) at (mdiff,ndiff) to nearfield input
       ! and write cormix/nrfield input file
       !
       select case (nflmod)
          case('corjet')
             !!
             !! Convert flow results to input for cormix and write to input file
             !!
             !call wri_cormix(u0    ,v0    ,rho   ,thick ,kmax  ,dps   , &
             !              & s0    ,alfas ,gdp                        )
             !!
             !! Do the Cormix simulation
             !!
             !corend = .false.
             !call util_system('corjet.bat')
             !do while (.not. corend)
             !   inquire (file='corjetrun.end',exist=corend)
             !enddo
             !!
             !! Finally convert cormix results to flow input
             !!
             !call corjet2flow(thick  ,kmax   ,dps   ,s0   ,disnf    ,sournf , &
             !               & lstsci ,lsal   ,ltem  ,xz   ,yz       ,nmmax  , &
             !               & kcs    ,time   ,gdp   )
          case('cortime')
             !!
             !! Read the general information from the corinp.dat file every time a cortime simulation is requested.
             !! This allows for restarting of cormix on a different pc (request Robin Morelissen)
             !!
             !call corinp_gen(idensform,gdp)
             !!
             !! Convert flow results to input for cormix and write to input file
             !!
             !write(cctime,'(f14.3)') time/60.0_fp
             !!
             !do idis = 1, no_dis
             !   filename(1) = trim(basecase(idis,1))//'.cmx'
             !   filename(2) = trim(basecase(idis,2))//'_time-step-'//trim(adjustl(cctime))//'.prd'
             !   filename(3) = trim(basecase(idis,2))//'_trajectory_time-step-'//trim(adjustl(cctime))//'_'//c_inode//'.tek'
             !   !
             !   call wri_cortim(u0    ,v0    ,rho   ,thick ,kmax  ,dps    , &
             !                 & s0    ,alfas ,time  ,taua         ,r0     , &
             !                 & lstsci,lsal  ,ltem  ,idensform    ,saleqs , &
             !                 & temeqs,idis  ,filename(1)         ,linkinf, &
             !                 & kfsmn0,kfsmx0,dzs0  ,gdp    )
             !   !
             !   ! Wait for the Cortime simulation to end (use existance of output file as indicator)
             !   !
             !   !            corend   = .false.
             !   !            do while (.not. corend)
             !   !               inquire (file=filename(2),exist=corend)
             !   !            enddo
             !   !
             !   ! Finally convert cortime results to flow input
             !   !
             !   call wait_until_finished(filename(2),gdp)
             !   !
             !   call cortim2flow(thick  ,kmax   ,dps   ,s0   ,r0       ,         &
             !                  & lstsci ,lsal   ,ltem  ,xz   ,yz       ,nmmax  , &
             !                  & kcs    ,filename      ,taua           ,idis   , &
             !                  & linkinf,gdp           )
             !enddo
             !!
          case('generic')
             !
             ! Write near field input files
             !
             if (nflrwmode /= NFLREADOLD) then
                allocate(waitfiles(no_dis), stat=ierror)
                waitfiles = ' '
                write(cctime,'(f14.3)') time/60.0_fp
                !
                ! Read the general information from the settings.xml file every time a cortime simulation is requested.
                ! This allows for restarting of cormix on a different pc (request Robin Morelissen)
                !    
                call corinp_gen2(error,gdp)
                !
                ! Convert x,y,z coordinates to n,m,k
                !
                do idis = 1, no_dis
                   call findnmk(nlb    ,nub       ,mlb         ,mub         , &
                              & xz_ptr ,yz_ptr    ,dps_ptr     ,s0_ptr      ,kcs_ptr, &
                              & thick  ,kmax      ,x_diff(idis),y_diff(idis),0.0_fp ,n_diff(idis), m_diff(idis), &
                              & k_dummy,kfsmn0_ptr,kfsmx0_ptr  ,dzs0_ptr    ,zmodel ,inside      , gdp         )
                   do iamb = 1, no_amb(idis)
                      call findnmk(nlb    ,nub       ,mlb              ,mub              , &
                                 & xz_ptr ,yz_ptr    ,dps_ptr          ,s0_ptr           ,kcs_ptr, &
                                 & thick  ,kmax      ,x_amb(idis,iamb) ,y_amb(idis,iamb) ,0.0_fp ,n_amb(idis,iamb), m_amb(idis,iamb), &
                                 & k_dummy,kfsmn0_ptr,kfsmx0_ptr       ,dzs0_ptr         ,zmodel ,inside          , gdp             )
                   enddo
                   call findnmk(nlb           ,nub       ,mlb           ,mub           , &
                              & xz_ptr        ,yz_ptr    ,dps_ptr       ,s0_ptr        ,kcs_ptr, &
                              & thick         ,kmax      ,x_intake(idis),y_intake(idis),z_intake(idis),n_intake(idis), m_intake(idis), &
                              & k_intake(idis),kfsmn0_ptr,kfsmx0_ptr    ,dzs0_ptr      ,zmodel        ,inside        , gdp           )
                enddo
                !
                ! Improved UniqueId generation:
                ! Part 1: Generate a random integer, seedIrand, based on date/time (call RANDOM_SEED())
                call random_seed()
                call random_number(dummy)
                seedIrand = floor(dummy * 123456789.8e0)
                !
                ! Convert flow results to input for cormix and write to input file
                ! Write all input files (one for each discharge) in the following do loop
                !
                do idis = 1, no_dis
                   !
                   ! Add SubGridModel number and uniqueId to filename to prevent overwriting
                   !
                   write(c_idis,'(i3.3)') idis
                   !
                   if (skipuniqueid) then
                      gdp%uniqueid = ' '
                   else
                      !
                      ! Improved UniqueId generation:
                      ! Part 2: Use seed array with elements "idis" and "seedIrand". "seedSize" is typically 2.
                      call random_seed(size=seedSize)
                      allocate(seed(seedSize))
                      do i=1, seedSize
                         if (mod(i,2) == 0) then
                            seed(i) = idis
                         else
                            seed(i) = seedIrand
                         endif
                      enddo
                      call random_seed(put=seed)
                      deallocate(seed)
                      !
                      ! Create uniqueId
                      do i=1,6
                         call random_number(dummy)
                         gdp%uniqueid(i:i) = char(floor(65.0_fp+dummy*26.0_fp))
                      enddo
                   endif
                   !
                   call makedir(trim(gdp%gdnfl%base_path(idis)))
                   if (.not.directory_exists(trim(gdp%gdnfl%base_path(idis)))) then
                      ifatal = 1
                      exit
                   endif
                   !
                   filename(1) = trim(gdp%gdnfl%base_path(idis))//'FF2NF_'//trim(gdp%uniqueid)//'_'//trim(gdp%runid)//'_'//c_inode//'_SubMod'//c_idis//'_'//trim(adjustl(cctime))//'.xml'
                   filename(2) = trim(basecase(idis,1))//'COSUMO'//FILESEP//'NF2FF'//FILESEP//'NF2FF_'//trim(gdp%uniqueid)//'_'//trim(gdp%runid)//'_'//c_inode//'_SubMod'//c_idis//'_'//trim(adjustl(cctime))//'.xml'
                   filename(3) = trim(basecase(idis,1))
                   waitfiles(idis) = filename(2)
                   !
                   ! You should get the filenames (the dirs) from the COSUMOsettings.xml
                   !
                   call wri_FF2NF(nlb       ,nub       ,mlb      ,mub      ,kmax   , &
                                & lstsci    ,lsal      ,ltem     ,idensform,idis   , &
                                & time      ,saleqs   ,temeqs   ,thick  , &
                                & sig       ,zk        ,kfu_ptr  ,kfv_ptr  , &
                                & alfas_ptr ,s0_ptr    ,s1_ptr   ,u0_ptr   ,v0_ptr , &
                                & r0_ptr    ,rho_ptr   ,dps_ptr  ,xz_ptr   ,yz_ptr , &
                                & kfsmn0_ptr,kfsmx0_ptr,dzs0_ptr ,filename ,namcon , &
                                & ifatal, gdp    )
                   if (ifatal /= 0) then
                      exit
                   endif
                enddo
             endif
             !
             ! Read near field input files and process them
             !
             if (nflrwmode /= NFLWRITE .and. ifatal == 0) then
                waitlog = .true.
                do
                   if (nflrwmode == NFLWRITEREADNEW) then
                      !
                      ! Wait until the new near field files are written
                      ! This is the default case
                      !
                      call wait_until_finished(no_dis, waitfiles, idis, filename(2), waitlog, ifatal, gdp)
                   else
                      !
                      ! Use the old near field files, written some time ago
                      !
                      call wait_until_finished(no_dis, waitfilesold, idis, filename(2), waitlog, ifatal, gdp)
                   endif
                   if (ifatal /= 0) then
                      exit
                   endif
                   !
                   waitlog = .false.
                   !
                   ! idis=0 when all files are processed
                   !
                   if (idis == 0) exit
                   !
                   call nf_2_flow(filename(2), error, gdp)
                   !
                   ! Error: just try again
                   !
                   if (error) cycle
                   !
                   ! Fill sources and sinks following the Desa Method of Prof. Lee
                   !
                   call desa(nlb     ,nub      ,mlb       ,mub            ,kmax           , &
                           & lstsci  ,no_dis   ,lsal      ,ltem           , &
                           & idis    ,thick    , &
                           & kcs_ptr ,xz_ptr   ,yz_ptr    ,alfas_ptr      , &
                           & dps_ptr ,s0_ptr   ,r0_ptr    ,kfsmn0_ptr     ,kfsmx0_ptr     , &
                           & dzs0_ptr,glb_disnf,glb_disnf_intake, glb_disnf_entr, glb_sournf,glb_nf_src_momu,glb_nf_src_momv, &
                           & linkinf ,error    ,gdp      )
                   ! Error: just try again
                   !
                   if (error) cycle
                   if (associated(gdp%gdnfl%nf_intake)) deallocate(gdp%gdnfl%nf_intake, stat=ierror)
                   if (associated(gdp%gdnfl%nf_sink  )) deallocate(gdp%gdnfl%nf_sink  , stat=ierror)
                   if (associated(gdp%gdnfl%nf_sour  )) deallocate(gdp%gdnfl%nf_sour  , stat=ierror)
                   nullify(gdp%gdnfl%nf_intake)
                   nullify(gdp%gdnfl%nf_sink)
                   nullify(gdp%gdnfl%nf_sour)
                   !
                   ! No error appeared: Remove the processed file from the waitfiles
                   !
                   if (nflrwmode == NFLWRITEREADNEW) then
                      waitfiles(idis) = ' '
                   else
                      waitfilesold(idis) = ' '
                   endif
                enddo
             endif
             if (ifatal == 0) then
                if (nflrwmode==NFLWRITE .or. nflrwmode==NFLWRITEREADOLD) then
                   !
                   ! Store the new waitfiles, to be used later on
                   waitfilesold(:) = waitfiles(:)
                endif
                if (nflrwmode /= NFLREADOLD) then
                   deallocate(waitfiles, stat=ierror)
                endif
             endif
          case ('jet3d')
             !!
             !! Convert flow results to input for jet3d and write to input file
             !!
             !call wri_jet3d(u0    ,v0    ,rho    ,thick ,kmax      ,dps   , &
             !             & s0    ,alfas ,flwang ,signx ,idensform ,time  ,gdp   )
             !!
             !! Do the Jet3d simulation
             !!
             !corend = .false.
             !call util_system('jet3d.bat')
             !do while (.not. corend)
             !   inquire (file='jet3drun.end',exist=corend)
             !enddo
             !!
             !! Finally convert Jet3D results to flow input
             !!    The appoach followed is the DESA approach as suggested by Prof. Lee
             !!    Clean up the mess afterwards
             !!
             !call jet3d2flow(thick  ,kmax   ,dps  ,s0   ,r0       ,        &
             !              & lstsci ,lsal   ,ltem ,xz   ,yz       ,nmmax  ,&
             !              & kcs    ,flwang ,signx,time ,linkinf  ,gdp  )
             !call util_system('del str3dinp.xxx')
             !call util_system('del str3dprt.xxx')
             !call util_system('del str3dtek.xxx')
             !call util_system('del jet3drun.end')
          case ('nrfield')
             !
             ! Nothing (yet)
             !
       end select
    endif
    if (parll) then
       call dfbroadc(ifatal, 1, dfint, error, errmsg)
    endif
    if (ifatal /= 0) then
       call d3stop(1,gdp)
    endif
    !
    ! Copy the global disnf/sournf/nf_src_momu/nf_src_momv to the local ones, even if not parallel,
    ! only when having processed the near field data
    !
    if (nflrwmode /= NFLWRITE) then
       if (parll) then
          ! 
          ! First scatter glb_disnf/glb_sournf/glb_nf_src_momu/glb_nf_src_momv to all nodes 
          ! 
                          call dfbroadc(glb_disnf       ,nmaxgl*mmaxgl*kmax*no_dis       ,dfdble,error,errmsg)
          if (.not.error) call dfbroadc(glb_disnf_intake,nmaxgl*mmaxgl*kmax*no_dis       ,dfdble,error,errmsg)
          if (.not.error) call dfbroadc(glb_disnf_entr  ,nmaxgl*mmaxgl*kmax*no_dis       ,dfdble,error,errmsg)
          if (.not.error) call dfbroadc(glb_sournf      ,nmaxgl*mmaxgl*kmax*lstsci*no_dis,dfdble,error,errmsg)
          if (.not.error) call dfbroadc(glb_nf_src_momu ,nmaxgl*mmaxgl*kmax*no_dis       ,dfdble,error,errmsg)
          if (.not.error) call dfbroadc(glb_nf_src_momv ,nmaxgl*mmaxgl*kmax*no_dis       ,dfdble,error,errmsg)
          if (.not.error) call dfbroadc(gdp%gdnfl%momrelax, 1                            ,dfdble,error,errmsg)
          if (.not.error) then
             !
             ! The master partition sets nf_src_mom based on the NF2FF input read and has to broadcast it
             ! using the integer variant imom
             !
             imom = 0
             if (inode==master) then
                if (nf_src_mom) then
                   imom = 1
                endif
             endif
             call dfbroadc(imom, 1, dfint,error,errmsg)
             if (imom == 1) then
                nf_src_mom = .true.
             else
                nf_src_mom = .false.
             endif
          endif
          if (error) then
             call write_error(errmsg, unit=lundia)
          else
             do m = mfg, mlg 
                do n = nfg, nlg 
                   call n_and_m_to_nm(n-nfg+1, m-mfg+1, nm, gdp)
                   disnf       (nm,:,:)   = glb_disnf       (n,m,:,:) 
                   disnf_intake(nm,:,:)   = glb_disnf_intake(n,m,:,:) 
                   disnf_entr  (nm,:,:)   = glb_disnf_entr  (n,m,:,:) 
                   sournf      (nm,:,:,:) = glb_sournf      (n,m,:,:,:)
                   nf_src_momu (nm,:,:)   = glb_nf_src_momu (n,m,:,:) 
                   nf_src_momv (nm,:,:)   = glb_nf_src_momv (n,m,:,:)
                enddo 
             enddo 
          endif
       else
          do m = mlb, mub
             do n = nlb, nub
                call n_and_m_to_nm(n, m, nm, gdp)
                disnf       (nm,:,:)   = glb_disnf       (n,m,:,:)
                disnf_intake(nm,:,:)   = glb_disnf_intake(n,m,:,:)
                disnf_entr  (nm,:,:)   = glb_disnf_entr  (n,m,:,:)
                sournf      (nm,:,:,:) = glb_sournf      (n,m,:,:,:)
                nf_src_momu (nm,:,:)   = glb_nf_src_momu (n,m,:,:)
                nf_src_momv (nm,:,:)   = glb_nf_src_momv (n,m,:,:)
             enddo
          enddo
       endif
    endif
    !
    if (parll .and. inode==master) then
       deallocate(glb_kcs   , stat=ierror)
       deallocate(glb_kfsmx0, stat=ierror)
       deallocate(glb_kfsmn0, stat=ierror)
       deallocate(glb_alfas , stat=ierror)
       deallocate(glb_dzs0  , stat=ierror)
       deallocate(glb_s0    , stat=ierror)
       deallocate(glb_r0    , stat=ierror)
       deallocate(glb_rho   , stat=ierror)
       deallocate(glb_u0    , stat=ierror)
       deallocate(glb_v0    , stat=ierror)
       deallocate(glb_dps   , stat=ierror)
       deallocate(glb_s1    , stat=ierror)
       deallocate(glb_xz    , stat=ierror)
       deallocate(glb_yz    , stat=ierror)
    endif
    if (nflrwmode /= NFLWRITE) then
       deallocate(glb_disnf       , stat=ierror)
       deallocate(glb_disnf_intake, stat=ierror)
       deallocate(glb_disnf_entr  , stat=ierror)
       deallocate(glb_sournf      , stat=ierror)
       deallocate(glb_nf_src_momu , stat=ierror)
       deallocate(glb_nf_src_momv , stat=ierror)
    endif
end subroutine near_field
