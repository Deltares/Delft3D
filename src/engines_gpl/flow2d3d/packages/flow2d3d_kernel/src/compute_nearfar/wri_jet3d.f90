subroutine wri_jet3d(u0    ,v0    ,rho    ,thick ,kmax      ,dps   ,&
                   & s0    ,alfas ,flwang ,sign  ,idensform ,time  ,gdp   )
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
!    Function: Writes input for jet3d
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
    integer                                             , intent(in)  :: kmax     !  Description and declaration in tricom.igs
    integer                                             , intent(in)  :: idensform!  Description and declaration in tricom.igs
    real(fp)                                            , intent(in)  :: time
    real(fp)                                            , intent(out) :: flwang   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: alfas    !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: s0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: u0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: v0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in)  :: rho      !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                        , intent(in)  :: thick    !  Description and declaration in esm_alloc_real.f90
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)  :: dps      !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                     :: irec
    integer                                     :: k
    integer                                     :: kjet
    integer                                     :: nm_diff
    integer                                     :: nmd_diff
    integer                                     :: ndm_diff
    integer                                     :: nm_amb
    integer                                     :: nmd_amb
    integer                                     :: ndm_amb
    integer                       , external    :: newlun
    integer                                     :: luntmp1
    integer                                     :: luntmp2
    real(fp)                                    :: sign
    real(fp)                                    :: thck
    real(fp)                                    :: uuu
    real(fp)                                    :: vvv
    real(fp)                                    :: theta
    real(fp)                                    :: dummy
    real(fp)                                    :: q_tmp
    real(fp)       , dimension(:) , allocatable :: h1
    real(fp)       , dimension(:) , allocatable :: umag
    real(fp)       , dimension(:) , allocatable :: rhojet
    real(fp)       , dimension(:) , allocatable :: h2
    real(fp)       , dimension(:) , allocatable :: uu1
    real(fp)       , dimension(:) , allocatable :: vv1
    real(fp)       , dimension(:) , allocatable :: rr
    character(256)                              :: record
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
    sign    = 1.0_fp
    !
    allocate (h1    (kmax+1) )
    allocate (umag  (kmax+1) )
    allocate (rhojet(kmax+1) )
    allocate (h2    (kmax)   )
    allocate (uu1   (kmax)   )
    allocate (vv1   (kmax)   )
    allocate (rr    (kmax)   )
    !
    ! Temporarily: read time varying q_diff from file
    !

!   call reatim(time,q_tmp,gdp)

    !
    ! Read the general diffuser characteritics from jet3d input file
    !
    luntmp1 = newlun(gdp)
    open (luntmp1, file='str3dinp.def', status='old')
    !
    ! Position diffuser
    !
    call skipstarlines (luntmp1)
    read (luntmp1,*) m_diff(1)
    call skipstarlines (luntmp1)
    read (luntmp1,*) n_diff(1)
    !
    call n_and_m_to_nm (n_diff(1)    , m_diff(1)    , nm_diff , gdp)
    call n_and_m_to_nm (n_diff(1) - 1, m_diff(1)    , ndm_diff, gdp)
    call n_and_m_to_nm (n_diff(1)    , m_diff(1) - 1, nmd_diff, gdp)
    !
    ! Position where to get ambient conditions
    !
    call skipstarlines (luntmp1)
    read (luntmp1,*) m_amb(1,1)
    call skipstarlines (luntmp1)
    read (luntmp1,*) n_amb(1,1)
    !
    call n_and_m_to_nm (n_amb(1,1)     , m_amb(1,1)     , nm_amb , gdp)
    call n_and_m_to_nm (n_amb(1,1)  - 1, m_amb(1,1)     , ndm_amb, gdp)
    call n_and_m_to_nm (n_amb(1,1)     , m_amb(1,1)  - 1, nmd_amb, gdp)
    !
    ! Read discharge characteristics
    !
    call skipstarlines (luntmp1)
    read (luntmp1,*) q_diff(1)
    call skipstarlines (luntmp1)
    read (luntmp1,*) const_diff(1,1)
    call skipstarlines (luntmp1)
    read (luntmp1,*) const_diff(1,2)
    select case (idensform)
       case( dens_Eckart )
          call dens_eck    (const_diff(1,1), const_diff(1,1),rho0_diff, dummy, dummy)
       case( dens_Unesco)
          call dens_unes   (const_diff(1,1), const_diff(1,1),rho0_diff, dummy, dummy)
       case( dens_NaClSol)
          call dens_nacl   (const_diff(1,1), const_diff(1,1),rho0_diff, dummy, dummy)
    end select
    call skipstarlines (luntmp1)
    !
    ! Compute depth averaged velocity direction
    !
    uuu = 0.0_fp
    vvv = 0.0_fp
    !
    do k = 1, kmax
       uuu = uuu + thick(k)*(u0(nm_amb ,k) + u0(nmd_amb ,k))
       vvv = vvv + thick(k)*(v0(nm_amb ,k) + v0(ndm_amb ,k))
    enddo
    !
    flwang = atan2(vvv,uuu) * raddeg
    !
    ! Compute heights relative to reference plane (let op, jet3d veronderstelt equidistante laagverdeling).
    !
    h1(1) = real(dps(nm_diff),fp)
    h2(1) = -1.0_fp*s0(nm_amb) + 0.5_fp*thick(1)*(s0(nm_amb) + real(dps(nm_amb),fp))
    do k = 2, kmax
       h1(k) = h1(k-1) -          (s0(nm_diff) + real(dps(nm_diff),fp))/kmax
       h2(k) = h2(k-1) + thick(k)*(s0(nm_amb)  + real(dps(nm_amb),fp))
    enddo
    h1 (kmax + 1) = -1.0_fp * s0(nm_diff)
    !
    ! Determine velocity component in the main flow direction and densities at jet3d heights
    !
    do k = 1, kmax
       uu1(k) = 0.5_fp * (u0(nm_amb,k) + u0(nmd_amb,k))
       vv1(k) = 0.5_fp * (v0(nm_amb,k) + v0(ndm_amb,k))
       rr (k) = rho(nm_amb,k)
    enddo

    do kjet = 1, kmax + 1
       call interp_tk (h2, uu1, kmax, h1(kjet), uuu         )
       call interp_tk (h2, vv1, kmax, h1(kjet), vvv         )
       call interp_tk (h2, rr , kmax, h1(kjet), rhojet(kjet))
       !
       umag(kjet) = max(uuu*cos(flwang*degrad) + vvv*sin(flwang*degrad),0.001_fp)
    enddo
    flwang = flwang + alfas(nm_amb)
    !
    ! Write jet3d input file
    !
    luntmp2 = newlun(gdp)
    open (luntmp2,file='str3dinp.xxx',status='unknown')
    read  (luntmp1,'(a256)') record
    write (record(11:20),'(f10.3)') s0(nm_diff) + real(dps(nm_diff),fp)
    write (luntmp2,'(a256)') record
    !
    read  (luntmp1,'( )')
    write (luntmp2,'(3i5)') 1, kmax+1, 0
    !
    read  (luntmp1,'( )')
    write (luntmp2,'(8f10.3 )') (umag(k), k = 1,kmax+1)
    !
    read  (luntmp1,'( )')
    write (luntmp2,'(2i5)') 1, kmax
    !
    read  (luntmp1,'( )')
    write (luntmp2,'(8f10.3 )') (rhojet(k), k = 1,kmax+1)
    !
    do irec = 1, 2
       read  (luntmp1, '(a256)') record
       write (luntmp2, '(a256)') record
    enddo
    !
    read  (luntmp1,'(a256)') record
    write (record(1:10) ,'(f10.3)') const_diff(1,1)
    read  (record(11:20),'(f10.0)') theta
    theta = theta - flwang
    !
    ! Correct (relative) diffusor angle since straal3D tends to be unstable in case of
    ! counterflowing jets
    !
    if (theta >  180.0_fp) then
       theta = theta - 360.0_fp
    endif
    if (theta >= 0.0_fp .and. theta <  5.0_fp) then
       theta =  5.0_fp
    endif
    if (theta <= 0.0_fp .and. theta > -5.0_fp) then
       theta = -5.0_fp
    endif
    if (theta >  175.0_fp) then
       theta = 175.0_fp
    endif
    if (theta < -175.0_fp) then
       theta = -175.0_fp
    endif
    if (theta < 0.0_fp) then
       theta = -1.0_fp*theta
       sign = -1.0_fp
    endif
    !
    write (record(11:20),'(f10.3)') theta
    write (luntmp2,'(a256)') record
    !
    read  (luntmp1,'(a256)') record
    read  (record(1:10),'(f10.0)') d0(1)
    uuu = max(4.0_fp * q_diff(1) / (pi*d0(1)*d0(1)),.001_fp)
    write (record(11:20),'(f10.3)') uuu
    write (record(21:30),'(f10.3)') rho0_diff
    write (luntmp2,'(a256)') record
    !
    read  (luntmp1, '(a256)') record
    write (luntmp2, '(a256)') record
    close (luntmp1)
    close (luntmp2)
    !
    deallocate (h1    )
    deallocate (umag  )
    deallocate (rhojet)
    deallocate (h2    )
    deallocate (uu1   )
    deallocate (vv1   )
    deallocate (rr    )
    !
end subroutine wri_jet3d
