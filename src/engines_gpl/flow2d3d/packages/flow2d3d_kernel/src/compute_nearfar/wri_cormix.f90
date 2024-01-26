subroutine wri_cormix(u0    ,v0    ,rho    ,thick ,kmax  ,dps   ,&
                    & s0    ,alfas ,gdp    )
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
!    Function: Writes input for cormix (corjet)
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer ,dimension(:)          , pointer :: m_diff
    integer ,dimension(:)          , pointer :: n_diff
    integer ,dimension(:,:)        , pointer :: m_amb
    integer ,dimension(:,:)        , pointer :: n_amb
    real(fp),dimension(:)          , pointer :: q_diff
    real(fp),dimension(:,:)        , pointer :: const_diff
    real(fp),dimension(:)          , pointer :: rho0_diff
    real(fp),dimension(:)          , pointer :: d0
    real(fp),dimension(:)          , pointer :: h0
    real(fp),dimension(:)          , pointer :: sigma0
    real(fp),dimension(:)          , pointer :: theta0
    character(256)    , pointer :: nflmod
!
! Global variables
!
    integer                                             , intent(in) :: kmax  !  Description and declaration in tricom.igs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in) :: alfas !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in) :: s0    !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in) :: rho   !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in) :: u0    !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in) :: v0    !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(kmax)                        , intent(in) :: thick !  Description and declaration in esm_alloc_real.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in) :: dps   !  Description and declaration in esm_alloc_real.f90 gs
!
! Local variables
!
    integer                                :: k
    integer                                :: nm_diff
    integer                                :: nmd_diff
    integer                                :: ndm_diff
    integer                                :: nm_amb
    integer                                :: nmd_amb
    integer                                :: ndm_amb
    integer                 , external     :: newlun
    integer                                :: luntmp
    real(fp)                               :: thck
    real(fp)                               :: uuu
    real(fp)                               :: vvv
    real(fp)                               :: rhohulp
    real(fp) , dimension(:) , allocatable  :: h1
    real(fp) , dimension(:) , allocatable  :: ua
    real(fp) , dimension(:) , allocatable  :: taua
    real(fp) , dimension(:) , allocatable  :: rhoa
    logical                                :: check
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    m_amb          => gdp%gdnfl%m_amb
    n_amb          => gdp%gdnfl%n_amb
    q_diff         => gdp%gdnfl%q_diff
    const_diff     => gdp%gdnfl%const_diff
    rho0_diff      => gdp%gdnfl%rho0_diff
    d0             => gdp%gdnfl%d0
    h0             => gdp%gdnfl%h0
    sigma0         => gdp%gdnfl%sigma0
    theta0         => gdp%gdnfl%theta0
    nflmod         => gdp%gdnfl%nflmod
    !
    allocate (h1   (kmax) )
    allocate (ua   (kmax) )
    allocate (taua (kmax) )
    allocate (rhoa (kmax) )
    !
    ! Read the general diffusor characteritics from jet3d input file
    !
    call n_and_m_to_nm(n_diff(1)    , m_diff(1)     , nm_diff  , gdp)
    call n_and_m_to_nm(n_diff(1) - 1, m_diff(1)     , ndm_diff , gdp)
    call n_and_m_to_nm(n_diff(1)    , m_diff(1) - 1 , nmd_diff , gdp)
    call n_and_m_to_nm(n_amb(1,1)   , m_amb(1,1)    , nm_amb   , gdp)
    call n_and_m_to_nm(n_amb(1,1)- 1, m_amb(1,1)    , ndm_amb  , gdp)
    call n_and_m_to_nm(n_amb(1,1)   , m_amb(1,1)- 1 , nmd_amb  , gdp)
    !
    ! Compute heights above bed
    !
    h1(1) = 0.5_fp * thick(kmax) * (s0(nm_amb)+real(dps(nm_amb),fp))
    !
    do k = 2, kmax
       thck   = 0.5_fp * (thick(kmax-k+2) + thick(kmax-k+1))
       h1 (k) = h1(k-1) + thck*(s0(nm_amb) + real(dps(nm_amb),fp))
    enddo
    !
    ! Compute velocity magnitude and direction
    ! Fill rhojet with densities
    !
    do k = 1, kmax
       uuu      = 0.5_fp * (u0(nm_amb ,kmax-k+1) + u0(nmd_amb ,kmax-k+1))
       vvv      = 0.5_fp * (v0(nm_amb ,kmax-k+1) + v0(ndm_amb ,kmax-k+1))
       ua   (k) = sqrt (uuu*uuu + vvv*vvv)
       taua (k) = atan2(vvv,uuu)*raddeg + alfas(nm_amb)
       taua (k) = mod(taua(k) + 360.0_fp,360.0_fp)
       rhoa (k) = rho(nm_amb, kmax-k+1)
    enddo
    !
    ! Make sure the density profile is stable
    !
    do k = 1, kmax - 1
       if (rhoa(k) < rhoa(k+1)) then
          rhoa(k+1) = rhoa(k)
        endif
    enddo
    !
    ! Write corjet
    !
    open (newunit=luntmp,file='corjet.inp',status='unknown')
    write (luntmp,'(''# Corjet input file'')')
    write (luntmp,'(''# Title line (50 characters max.):'')')
    write (luntmp,'(''Corjet run initiated from flow'')')
    write (luntmp,'(''# empty'')')
    write (luntmp,'(''# empty'')')
    write (luntmp,'(3i5)') 1, 2, kmax
    write (luntmp,'(''# empty'')')
    write (luntmp,'(''# empty'')')
    do k = 1, kmax
       write (luntmp,'(i5,4f12.3)') k, h1(k), rhoa(k), ua(k), taua(k)
    enddo
    !
    write (luntmp,'(''# empty'')')
    write (luntmp,'(''# empty'')')
    uuu = q_diff(1) / (0.25_fp*pi*d0(1)*d0(1))
    !
    write (luntmp,'(i5,8f12.3,2i5)') 1, d0, h0, uuu, theta0(1), sigma0(1), 100.0_fp, 0.0_fp, rho0_diff, 1, 1
    write (luntmp,'(''# empty'')')
    write (luntmp,'(''# empty'')')
    write (luntmp,'(3f12.3,i5)') s0(nm_diff)+real(dps(nm_diff),fp), 0.0_fp, 1000.0_fp, 10
    close (luntmp)
    !
    deallocate (h1)
    deallocate (ua)
    deallocate (taua)
    deallocate (rhoa)
    !
end subroutine wri_cormix
