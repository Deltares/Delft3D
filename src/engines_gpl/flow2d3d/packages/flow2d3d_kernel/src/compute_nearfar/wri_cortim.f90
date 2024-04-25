subroutine wri_cortim(u0     ,v0       ,rho       ,thick  ,kmax   ,dps    , &
                    & s0     ,alfas    ,time      ,taua   ,r0     ,lstsci , &
                    & lsal   ,ltem     ,idensform ,saleqs ,temeqs , &
                    & idis   ,filename ,linkinf   , &
                    & kfsmn0 ,kfsmx0   ,dzs0      , gdp   )
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
!  $Id: wri_cortim.f90 6185 2016-06-08 15:20:28Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/wri_cortim.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes input for cortim
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer ,dimension(:)          , pointer :: m_diff
    integer ,dimension(:)          , pointer :: n_diff
    integer ,dimension(:,:)        , pointer :: m_amb
    integer ,dimension(:,:)        , pointer :: n_amb
    integer ,dimension(:)          , pointer :: m_intake
    integer ,dimension(:)          , pointer :: n_intake
    integer ,dimension(:)          , pointer :: k_intake

    real(fp),dimension(:)          , pointer :: q_diff
    real(fp),dimension(:,:)        , pointer :: const_diff
    real(fp),dimension(:)          , pointer :: d0
    real(fp),dimension(:)          , pointer :: h0
    real(fp),dimension(:)          , pointer :: sigma0
    integer                        , pointer :: lunsrc
    logical                        , pointer :: zmodel
!
! Global variables
!
    integer                                                     , intent(in) :: idis
    integer                                                     , intent(in) :: kmax
    integer                                                     , intent(in) :: lstsci
    integer                                                     , intent(in) :: lsal
    integer                                                     , intent(in) :: ltem
    integer                                                     , intent(in) :: idensform
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmx0     ! Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmn0     ! Description and declaration in esm_alloc_int.f90
    real(fp)                                                    , intent(out):: taua
    real(fp)   , dimension(8)                                   , intent(out):: linkinf
    real(fp)                                                    , intent(in) :: saleqs
    real(fp)                                                    , intent(in) :: temeqs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: alfas
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: dzs0       ! Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: s0
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: rho
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: u0
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: v0
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax,lstsci)  , intent(in) :: r0
    real(fp)   , dimension(kmax)                                , intent(in) :: thick
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: dps
    character*256                                                            :: filename
!
! Local variables
!
    integer                                :: ierror
    integer                                :: ilen
    integer                                :: k
    integer                                :: nm_diff
    integer                                :: nmd_diff
    integer                                :: ndm_diff
    integer                                :: nm_amb
    integer                                :: nmd_amb
    integer                                :: ndm_amb
    integer                 , external     :: newlun
    integer                                :: luntmp
    real(fp)                               :: deg2rad
    real(fp)                               :: drohj
    real(fp)                               :: ha
    real(fp)                               :: hd
    real(fp)                               :: hint
    real(fp)                               :: pi
    real(fp)                               :: rad2deg
    real(fp)                               :: rhoam
    real(fp)                               :: rhoas
    real(fp)                               :: rhoab
    real(fp)                               :: taurel
    real(fp)                               :: time
    real(fp)                               :: uuu
    real(fp)                               :: ua
    real(fp)                               :: umag
    real(fp)                               :: vvv
    real(fp)                               :: sal
    real(fp)                               :: temp
    real(fp)                               :: dummy
    real(fp)                               :: add
    real(fp)                               :: rho0
    real(fp), dimension(:), allocatable    :: dzs0_nm_amb
    character*1                            :: tab
    character*1                            :: stype1
    character*1                            :: stype2
    character*3                            :: c_inode
    character*12                           :: ctime
    character*12                           :: ctime1
    character*12                           :: cha
    character*12                           :: chd
    character*12                           :: ch0
    character*12                           :: cua
    character*12                           :: crhoam
    character*12                           :: crhoas
    character*12                           :: crhoab
    character*12                           :: chint
    character*12                           :: cdrohj
    character*12                           :: cq0
    character*12                           :: crho0
    character*12                           :: cd0
    character*12                           :: ctaua
    logical                                :: linkinp
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    m_amb          => gdp%gdnfl%m_amb
    n_amb          => gdp%gdnfl%n_amb
    m_intake       => gdp%gdnfl%m_intake
    n_intake       => gdp%gdnfl%n_intake
    k_intake       => gdp%gdnfl%k_intake
    q_diff         => gdp%gdnfl%q_diff
    const_diff     => gdp%gdnfl%const_diff
    d0             => gdp%gdnfl%d0
    h0             => gdp%gdnfl%h0
    sigma0         => gdp%gdnfl%sigma0
    zmodel         => gdp%gdprocs%zmodel
    !
    write(c_inode(1:3),'(i3.3)') inode
    !
    pi      = acos(-1.0_fp)
    rad2deg = 180.0_fp / pi
    deg2rad = pi / 180.0_fp
    tab     = char(9)
    allocate(dzs0_nm_amb(kmax), stat=ierror)
    if (zmodel) then
       dzs0_nm_amb = dzs0(nm_amb,:)
    else
       dzs0_nm_amb = -999.0_fp
    endif
    !
    ! Read the general diffusor characteritics from cormix input file
    !
    call n_and_m_to_nm(n_diff(idis)    , m_diff(idis)     , nm_diff  , gdp)
    call n_and_m_to_nm(n_diff(idis) - 1, m_diff(idis)     , ndm_diff , gdp)
    call n_and_m_to_nm(n_diff(idis)    , m_diff(idis) - 1 , nmd_diff , gdp)
    call n_and_m_to_nm(n_amb(idis,1)   , m_amb(idis,1)    , nm_amb   , gdp)
    call n_and_m_to_nm(n_amb(idis,1)- 1, m_amb(idis,1)    , ndm_amb  , gdp)
    call n_and_m_to_nm(n_amb(idis,1)   , m_amb(idis,1)- 1 , nmd_amb  , gdp)
    !
    ! Compute the depths
    !
    ha = s0(nm_amb)+real(dps(nm_amb),fp)
    hd = s0(nm_diff)+real(dps(nm_diff),fp)
    !
    ! Compute depth averaged velocity magnitude and direction
    !
    uuu = 0.0_fp  
    vvv = 0.0_fp
    do k = 1, kmax
       uuu      = uuu + 0.5_fp * (u0(nm_amb ,k) + u0(nmd_amb ,k))*thick(k)
       vvv      = vvv + 0.5_fp * (v0(nm_amb ,k) + v0(ndm_amb ,k))*thick(k)
    enddo
    umag = sqrt (uuu*uuu + vvv*vvv)
    taua = atan2(vvv,uuu)*rad2deg + alfas(nm_amb)
    taua = mod(taua + 360.0_fp,360.0_fp)
    ua   = umag
    !
    ! Density profile classification (Cormixtype)
    !
    call determine_densprof(kmax           ,thick          ,s0(nm_amb)     ,real(dps(nm_amb),fp) ,rho(nm_amb,:) , &
                          & ha             ,hd             ,stype1         ,stype2               ,rhoam         , &
                          & rhoas          ,rhoab          ,hint           ,drohj                , &
                          & kfsmn0(nm_amb) ,kfsmx0(nm_amb) ,dzs0_nm_amb    ,zmodel         )
    !
    ! Compute the density of the discharged water
    !
    sal  = const_diff(idis,1)
    temp = const_diff(idis,1)
    if (lsal /= 0) then
       call coupled (add,r0,kmax,lstsci,lsal,thick,m_intake(idis),n_intake(idis),k_intake(idis),gdp)
       sal = sal + add
    else
       sal = saleqs
    endif
    if (ltem /= 0) then
       call coupled (add,r0,kmax,lstsci,ltem,thick,m_intake(idis),n_intake(idis),k_intake(idis),gdp)
       temp = temp + add
    else
       temp = temeqs
    endif
    select case (idensform)
       case( dens_Eckart )
          call dens_eck    (temp, sal ,rho0, dummy, dummy)
       case( dens_Unesco)
          call dens_unes   (temp, sal ,rho0, dummy, dummy)
       case( dens_NaClSol)
          call dens_nacl   (temp, sal ,rho0, dummy, dummy)
    end select
    !
    ! Write Cortime input file
    !
    luntmp = newlun(gdp)
    linkinp   = .true.
    do while (linkinp)
       inquire (file=trim(gdp%gdnfl%base_path(idis))//'cortime_'//trim(gdp%runid)//'_'//c_inode//'.linkinp',exist=linkinp)
    enddo
    open (luntmp,file=trim(gdp%gdnfl%base_path(idis))//'cortime_'//trim(gdp%runid)//'_'//c_inode//'.linkinp',status='new')
    write (luntmp,'(''CorTime v7.0'')')
    write (luntmp,'()')
    write (luntmp,'(''File name='',a1,a )') tab,'cortime_'//trim(gdp%runid)//'_'//c_inode//'.linkinp'
    write (luntmp,'(''Base case='',a1,a )') tab,trim(filename)
    write (luntmp,'(''Node='',a1,a )') tab,trim(gdp%runid)
    write (luntmp,'(''TOTSTEP='' ,a1,i1)') tab,1
    write (luntmp,'()')
    write (luntmp,'(27(a,a1),a)') 'TIME' , tab, 'HA'   , tab, 'HD'   , tab, 'UA'     , tab, &
   &                              'UorS' , tab, 'RHOAM', tab, 'STYPE', tab, 'RHOAS'  , tab, &
   &                              'RHOAB', tab, 'HINT' , tab, 'DROHJ', tab, 'Q0'     , tab, &
   &                              'C0'   , tab, 'RHO0' , tab, 'Gamma', tab, 'Sigma'  , tab, &
   &                              'D0'   , tab, 'B0'   , tab, 'H0'   , tab, 'PollTyp', tab, &
   &                              'L1Sub', tab, 'L1Den', tab, 'L2Sub', tab, 'L2Den'  , tab, &
   &                              'L3Sub', tab, 'L3Den', tab, 'Distb', tab, 'PHI'
    !
    ! Make character strings from all requested input
    !
    write (ctime (1:12),'(f12.3)') time/60.0_fp
    write (cha   (1:12),'(f12.3)') ha
    write (chd   (1:12),'(f12.3)') hd
    write (cua   (1:12),'(f12.3)') max(ua,0.001_fp)
    if (stype1 == 'U') then
       write(crhoam(1:12),'(f12.3)') rhoam
       crhoas = '-'
       crhoab = '-'
       stype2 = '-'
       chint  = '-'
       cdrohj = '-'
    else
       crhoam ='-'
       write (crhoas(1:12),'(f12.3)') rhoas
       write (crhoab(1:12),'(f12.3)') rhoab
       if (stype2 == 'C') then
          write (chint (1:12),'(f12.3)') hint
          write (cdrohj(1:12),'(f12.3)') drohj
       else
          chint  = '-'
          cdrohj = '-'
       endif
    endif
    write (cq0   (1:12),'(f12.8)') q_diff(idis)
    write (crho0 (1:12),'(f12.3)') rho0
    write (ch0   (1:12),'(f12.3)') h0(idis)
    write (cd0   (1:12),'(f12.3)') d0(idis)
    !
    ! sigma0 given as direction relative to north in stead of main flow direction; 0, pointing to east, 90 pointing to north etc.
    ! ctaua is port direction relative to main flow direction
    !
    taua = mod(taua,360.0_fp)
    taurel = mod(sigma0(idis) - taua + 360.0_fp,360.0_fp)
    if (taurel > 179.0_fp .and. taurel < 181.0_fp) then
       taurel = 179.0_fp
    endif
    write (ctaua (1:12),'(f12.3)') taurel
    ctime = adjustl(ctime )
    cha   = adjustl(cha   )
    chd   = adjustl(chd   )
    cua   = adjustl(cua   )
    crhoam= adjustl(crhoam)
    crhoas= adjustl(crhoas)
    crhoab= adjustl(crhoab)
    chint = adjustl(chint )
    cdrohj= adjustl(cdrohj)
    cq0   = adjustl(cq0   )
    crho0 = adjustl(crho0 )
    ch0   = adjustl(ch0   )
    cd0   = adjustl(cd0   )
    ctaua = adjustl(ctaua )
    write (luntmp,'(27(a,a1),a)') trim(ctime)  , tab, trim(cha)    , tab, trim(chd)    , tab, trim(cua)      , tab, &
   &                              trim(stype1) , tab, trim(crhoam) , tab, trim(stype2) , tab, trim(crhoas)   , tab, &
   &                              trim(crhoab) , tab, trim(chint)  , tab, trim(cdrohj) , tab, trim(cq0)      , tab, &
   &                              '1.0'        , tab, trim(crho0)  , tab, '-'          , tab, '-'            , tab, &
   &                              trim(cd0)    , tab, '-'          , tab, '-'          , tab, '-'            , tab, &
   &                              '-'          , tab, '-'          , tab, '-'          , tab, '-'            , tab, &
   &                              '-'          , tab, '-'          , tab, '-'          , tab, trim(ctaua)
    close (luntmp)
    !
    ! Store some general information to write to the plume trajectory file
    !
    linkinf(1) = sal
    linkinf(2) = temp
    linkinf(3) = rho0
    linkinf(4) = ua
    linkinf(5) = taua
    linkinf(6) = taurel
    !
    deallocate(dzs0_nm_amb, stat=ierror)         
end subroutine wri_cortim
