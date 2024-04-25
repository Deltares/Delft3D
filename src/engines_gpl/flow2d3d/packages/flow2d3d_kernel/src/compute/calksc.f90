subroutine calksc(nmmax     ,dps       ,s1        ,lsedtot   , &
                & u         ,v         ,kfs       ,z0urou    , &
                & z0vrou    ,kfu       ,kfv       ,sig       , &
                & kmax      ,hrms      ,rlabda    ,tp        , &
                & deltau    ,deltav    ,icx       ,icy       ,gdp       )
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
! Calculate ripple height, mega ripple height and dune height Van Rijn (2004)
! Routine is called for U/V-direction respectively.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use sediment_basics_module
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: dt
    real(fp)                             , pointer :: tunit
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: z0
    real(fp)                             , pointer :: z0v
    logical                              , pointer :: wave
    logical                              , pointer :: lftrto
    logical                              , pointer :: spatial_bedform
    logical                              , pointer :: v2dwbl
    real(fp)      , dimension(:)         , pointer :: sedd50
    real(fp)      , dimension(:)         , pointer :: sedd50fld
    real(fp)      , dimension(:)         , pointer :: sedd90
    integer                              , pointer :: i50
    integer                              , pointer :: i90
    real(fp)              , dimension(:) , pointer :: xx
    integer                              , pointer :: ntrt
    integer , dimension(:,:)             , pointer :: ittdef
    integer                              , pointer :: bdfrpt
    real(fp), dimension(:)               , pointer :: bedformD50
    real(fp), dimension(:)               , pointer :: bedformD90
    real(fp), dimension(:)               , pointer :: kdpar
    real(fp), dimension(:)               , pointer :: duneheight
    real(fp), dimension(:)               , pointer :: dunelength
    real(fp), dimension(:)               , pointer :: rksr
    real(fp), dimension(:)               , pointer :: rksmr
    real(fp), dimension(:)               , pointer :: rksd
    real(fp), dimension(:,:)             , pointer :: dxx
    real(fp), dimension(:,:)             , pointer :: rttdef
    character(256)                       , pointer :: flsdia
!
! Local parameters
!
    real(fp), parameter :: rwe = 1.65
!
! Global variables
!
    integer                                           , intent(in)  :: icx
    integer                                           , intent(in)  :: icy
    integer                                           , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: kmax    !  Description and declaration in dimens.igs
    integer                                           , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv     !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: deltav  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hrms    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                       , intent(in)  :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u       !!  EULARIAN U-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v       !!  EULARIAN V-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: z0urou  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: z0vrou  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: itrt
    integer  :: k
    integer  :: kn
    integer  :: kmaxx
    integer  :: nm
    integer  :: nmd
    integer  :: ndm
    real(fp) :: a11
    real(fp) :: arg
    real(fp) :: d50
    real(fp) :: d90
    real(fp) :: deltas
    real(fp) :: depth
    real(fp) :: fact
    real(fp) :: fch2
    real(fp) :: fcoarse
    real(fp) :: hs
    real(fp) :: llabda  ! local limited rlabda value
    real(fp) :: par1    ! scale factor ripples
    real(fp) :: par2    ! scale factor mega-ripples
    real(fp) :: par3    ! scale factor dunes
    real(fp) :: par4    ! relaxation time scale ripples (minutes)
    real(fp) :: par5    ! relaxation time scale mega-ripples (minutes)
    real(fp) :: par6    ! relaxation time scale dunes (minutes)
    real(fp) :: psi
    real(fp) :: raih
    real(fp) :: relaxr
    real(fp) :: relaxmr
    real(fp) :: relaxd
    real(fp) :: rksr0
    real(fp) :: rksmr0
    real(fp) :: rksd0
    real(fp) :: rmax
    real(fp) :: rr
    real(fp) :: t1
    real(fp) :: u1
    real(fp) :: u2dh
    real(fp) :: uuu
    real(fp) :: vvv
    real(fp) :: umax
    real(fp) :: umod
    real(fp) :: uon
    real(fp) :: uoff
    real(fp) :: uw
    real(fp) :: uwbih
    real(fp) :: uwc
    real(fp) :: z0rou
    real(fp) :: zcc
!
!! executable statements -------------------------------------------------------
!
    bdfrpt                  => gdp%gdbedformpar%bdfrpt
    kdpar                   => gdp%gdbedformpar%kdpar
    bedformD50              => gdp%gdbedformpar%bedformD50
    bedformD90              => gdp%gdbedformpar%bedformD90
    spatial_bedform         => gdp%gdbedformpar%spatial_bedform
    duneheight              => gdp%gdbedformpar%duneheight
    dunelength              => gdp%gdbedformpar%dunelength
    rksr                    => gdp%gdbedformpar%rksr
    rksmr                   => gdp%gdbedformpar%rksmr
    rksd                    => gdp%gdbedformpar%rksd
    dt                      => gdp%gdexttim%dt
    tunit                   => gdp%gdexttim%tunit
    dxx                     => gdp%gderosed%dxx
    v2dwbl                  => gdp%gdnumeco%v2dwbl
    ag                      => gdp%gdphysco%ag
    z0                      => gdp%gdphysco%z0
    z0v                     => gdp%gdphysco%z0v
    wave                    => gdp%gdprocs%wave
    lftrto                  => gdp%gdprocs%lftrto
    flsdia                  => gdp%gdsedpar%flsdia   
    sedd50                  => gdp%gdsedpar%sedd50
    sedd50fld               => gdp%gdsedpar%sedd50fld
    sedd90                  => gdp%gdsedpar%sedd90
    i50                     => gdp%gdmorpar%i50
    i90                     => gdp%gdmorpar%i90
    xx                      => gdp%gdmorpar%xx
    ntrt                    => gdp%gdtrachy%ntrt
    ittdef                  => gdp%gdtrachy%ittdef
    rttdef                  => gdp%gdtrachy%rttdef
    !
    ! Main loop over area data
    !
    ! Calculate roughness based on bedforms.
    ! Either through input duneheight, dunelength or 
    ! otherwise. 
    !
    select case (bdfrpt)
    case (0)
       !
       ! Van Rijn 2004
       !
       par1 = kdpar(1)
       par2 = kdpar(2)
       par3 = kdpar(3)
       par4 = kdpar(4)
       par5 = kdpar(5)
       par6 = kdpar(6)
       !
       relaxr  = exp(- dt / max(1.0e-20_fp, par4))
       relaxmr = exp(- dt / max(1.0e-20_fp, par5))
       relaxd  = exp(- dt / max(1.0e-20_fp, par6))
       !
       do nm = 1, nmmax
          if (kfs(nm)>0) then
             !
             nmd   = nm - icx
             ndm   = nm - icy
             !
             depth = s1(nm) + real(dps(nm),fp)
             !
             ! Velocity in zeta point
             !
             kmaxx = kmax
             !
             if (v2dwbl .and. wave .and. kmax>1) then
                !
                ! Determine representative 2Dh velocity based on velocities in first layer above wave boundary layer 
                ! kmaxx is the first layer with its centre above the wave boundary layer
                ! Only implemented for sigma layers
                !
                fact   = max(kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm), 1)
                deltas = (deltau(nm) + deltau(nmd) + deltav(nm) + deltav(ndm)) / fact
                !
                do k = kmax, 1, -1
                   zcc = (1.0 + sig(k))*depth
                   if (zcc>deltas .or. zcc>0.5*depth) then
                      kmaxx = k
                      exit
                   endif
                enddo
             endif
             !
             uuu = 0.5_fp * (u(nm,kmaxx)*kfu(nm) + u(nmd,kmaxx)*kfu(nmd))
             vvv = 0.5_fp * (v(nm,kmaxx)*kfv(nm) + v(ndm,kmaxx)*kfv(ndm))
             !
             ! Depth-average velocity (similar as in TAUBOT)
             !
             umod   = sqrt(uuu**2 + vvv**2)
             kn = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
             !
             ! Calculate total (possibly wave enhanced) roughness
             !
             z0rou = (  kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) &
                   &  + kfv(ndm)*z0vrou(ndm) + kfv(nm)*z0vrou(nm)  ) / kn
             if (kmax == 1) then
                u2dh = umod
             else
                u2dh = (umod/depth*((depth + z0rou)*log(1.0_fp + depth/z0rou) - depth)) &
                     & / log(1.0_fp + (1.0_fp + sig(kmaxx))*depth/z0rou)
             endif
             if (wave) then
                llabda = max(rlabda(nm), 0.1_fp)
                hs     = hrms(nm) * sqrt(2.0_fp)
                arg = 2.0_fp * pi * depth / llabda
                if (arg > 50.0_fp) then
                   uw = 0.0_fp
                else
                   uw = 2.0_fp * pi * hs / (2.0_fp * sinh(arg) * tp(nm))
                endif
                rr    = -0.4_fp*hs/depth + 1.0_fp
                umax  = rr * 2.0_fp * uw
                t1    = tp(nm) * (ag/depth)**0.5_fp
                u1    = umax / (ag*depth)**0.5_fp
                a11   = -0.0049_fp*t1**2 - 0.069_fp*t1 + 0.2911_fp
                raih  = max(0.5_fp  , -5.25_fp - 6.1_fp*tanh(a11*u1-1.76_fp))
                rmax  = max(0.62_fp , min(-2.5_fp*depth/llabda+0.85_fp, 0.75_fp))
                uon   = umax * (0.5_fp+(rmax-0.5_fp)*tanh((raih-0.5_fp)/(rmax-0.5_fp)))
                uoff  = umax - uon
                uon   = max(1.0e-5_fp , uon)
                uoff  = max(1.0e-5_fp , uoff)
                uwbih = (0.5_fp*uon**3.0_fp + 0.5_fp*uoff**3.0_fp)**(1.0_fp/3.0_fp)
             else
                uw    = 0.0_fp
                uwbih = 0.0_fp
             endif
             if (lsedtot > 0) then
                d50 = dxx(nm, i50)
                d90 = dxx(nm, i90)
             else if (spatial_bedform) then
                d50 = bedformD50(nm)
                d90 = bedformD90(nm)
             else
                d50 = bedformD50(1)
                d90 = bedformD90(1)
             endif
             !
             ! Van Rijn 2004 roughness predictor
             !
             fch2    = max(min(d50/(1.5_fp*dsand) , 1.0_fp) , 0.3_fp)
             fcoarse = min((0.25_fp*dgravel/d50)**1.5_fp , 1.0_fp)
             !
             uwc     = sqrt(uwbih**2 + u2dh**2)
             d50     = min(d50,0.0005_fp)
             d50     = max(d50,0.0001_fp)
             psi     = uwc**2/(rwe*ag*d50)
             !
             ! Small-scale ripples
             !
             if (psi <= 50.0_fp) then
                rksr0 = 150.0_fp * fcoarse * d50
             elseif (psi >= 250.0_fp) then
                rksr0 = 20.0_fp * fcoarse * d50
             else
                rksr0 = (182.5_fp-0.65_fp*psi) * fcoarse * d50
             endif
             if (d50 < dsilt) then
                rksr0 = 20_fp * dsilt
             endif
             rksr0    = min(max(d90 , rksr0) , 0.02_fp*depth) * par1
             rksr(nm) = relaxr*rksr(nm) + (1.0_fp-relaxr)*rksr0
             !
             ! Mega-ripples
             !
             if (par2 > 0.0_fp) then
                if (psi <= 50.0_fp) then
                   rksmr0 = 0.0002_fp * fch2 * psi * depth
                elseif (psi < 550.0_fp) then
                   rksmr0 = (0.011_fp - 0.00002_fp*psi) * fch2 * depth
                else
                   rksmr0 = 0.0_fp
                endif
             else
                rksmr0 = 0.0_fp
             endif
             if (rksmr0 < 0.02_fp) then
                if (d50 >= 1.5_fp*dsand) then
                   rksmr0 = 0.02_fp
                elseif (d50 >= dsilt) then
                   rksmr0 = 200.0_fp * (d50 / (1.5_fp*dsand)) * d50
                else
                   rksmr0 = 0.0_fp
                endif
             endif
             !
             ! In revision 7868, the following code was commented out because it doesn't
             ! match the paper of Van Rijn(2007). The following code may, however, be needed
             ! to solve some issues in shallow areas, so for the time being we leave it in
             ! such that it can be reactivated easily when needed.
             !
             if (depth <= 1.0_fp) then
                rksmr0 = rksmr0 * depth
             endif
             if (d50 < dsilt) then
                rksmr0 = 0.0_fp
             endif
             rksmr0    = min(0.2_fp, rksmr0*par2)
             rksmr(nm) = relaxmr*rksmr(nm) + (1.0_fp-relaxmr)*rksmr0
             !
             ! Dunes
             !
             if (depth>1.0_fp .and. par3>0.0_fp) then
                if (psi <= 100.0_fp) then
                   rksd0 = 0.0004_fp * psi * depth
                elseif (psi < 600.0_fp) then
                   rksd0 = (0.048_fp - 0.00008_fp*psi) * depth
                else
                   rksd0 = 0.0_fp
                endif
             else
                rksd0 = 0.0_fp
             endif
             rksd0 = rksd0*par3
             rksd(nm)  = relaxd*rksd(nm) + (1.0_fp-relaxd)*rksd0
          else
             rksr(nm)  = 0.01_fp
             rksmr(nm) = 0.0_fp
             rksd(nm)  = 0.0_fp
          endif
       enddo
    case (1)
       !
       ! Van Rijn 1984 roughness predictor
       !
       do nm = 1, nmmax
          if (associated(gdp%gderosed%dxx))  then 
             d90 = dxx(nm, i90)
          else if (spatial_bedform) then
             d90 = bedformD90(nm)
          else
             d90 = bedformD90(1)
          endif 
          if (kfs(nm)>0) then
             dunelength(nm) = max(dunelength(nm),1e-6_fp)
             t1 = 1.0_fp - exp(-25.0_fp*duneheight(nm)/dunelength(nm))
             rksd(nm) = 1.1_fp*duneheight(nm)*t1
             rksmr(nm) = 0.0_fp
             rksr(nm)  = 3.0_fp*d90
          else
             rksr(nm)  = 0.0_fp
             rksmr(nm) = 0.0_fp
             rksd(nm)  = 0.0_fp
          endif
       enddo
    case (2)
       !
       ! Power relation on basis of dune height for roughness.
       !
       do nm = 1, nmmax
          if (associated(gdp%gderosed%dxx))  then 
             d90 = dxx(nm, i90)
          else if (spatial_bedform) then
             d90 = bedformD90(nm)
          else
             d90 = bedformD90(1)
          endif 
          if (kfs(nm)>0) then
             rksd(nm)  = kdpar(1)*abs(duneheight(nm))**kdpar(2)
             rksmr(nm) = 0.0_fp
             rksr(nm)  = 3.0_fp*d90
          else
             rksr(nm)  = 0.0_fp
             rksmr(nm) = 0.0_fp
             rksd(nm)  = 0.0_fp
          endif
       enddo
    end select
end subroutine calksc
