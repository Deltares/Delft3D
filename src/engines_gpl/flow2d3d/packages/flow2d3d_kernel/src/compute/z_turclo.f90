subroutine z_turclo(j         ,nmmaxj    ,nmmax     ,kmax      ,ltur      , &
                  & icx       ,icy       ,tkemod    , &
                  & kcs       ,kfu       ,kfv       ,kfs       ,kfuz1     , &
                  & kfvz1     ,kfsz1     ,kfumin    ,kfumax    ,kfvmin    , &
                  & kfvmax    ,kfsmin    ,kfsmax    ,s1        ,dps       , &
                  & hu        ,hv        ,u1        ,v1        ,rtur1     , &
                  & thick     ,sig       ,rho       ,vicuv     ,vicww     , &
                  & dicuv     ,dicww     ,windsu    ,windsv    ,z0urou    , &
                  & z0vrou    ,bruvai    ,rich      ,dudz      ,dvdz      , &
                  & dzu1      ,dzv1      ,dzs1      ,zk        ,ueul      , &
                  & veul      ,gdp       )
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
!    Function: Computes eddy viscosity and eddy diffusivity.
!              dependent of closure model (ltur).
!              ltur=0    algebraic model
!              ltur=1    k-L model
!              ltur=2    k-epsilon model  (SANCTUM-model)
!              For ltur=1,2 transport equations are solved.
!              - For tkemod = 'Constant   ' user input is used
!
! Method used: Reference: R.E. Uittenbogaard, J.A.Th.M. van
!              Kester, G.S. Stelling, Implementation of three
!              turbulence models in 3D-FLOW for rectangular
!              grids, Deltares report Z81, april 1992)
!              Fixed Layer Approach
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
    !
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: z0v
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: vicmol
    real(fp)               , pointer :: vicoww
    real(fp)               , pointer :: dicoww
    real(fp)               , pointer :: cmukl
    real(fp)               , pointer :: cmukep
    real(fp)               , pointer :: zwi
    integer                , pointer :: inpzw
!
! Global variables
!
    integer                                                 , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                    !!  then computation proceeds in the X-
                                                                                    !!  dir. If icx=1 then computation pro-
                                                                                    !!  ceeds in the Y-dir.
    integer                                                 , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                               :: j      !!  Begin pointer for arrays which have
                                                                                    !!  been transformed into 1D arrays.
                                                                                    !!  Due to the shift in the 2nd (M-)
                                                                                    !!  index, J = -2*NMAX + 1
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                               :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in)  :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)                  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)                  :: vicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: ueul   !!  Eulerian velocity in X-direction (including Stokes drift)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: veul   !!  Eulerian velocity in Y-direction (including Stokes drift)
    real(fp), dimension(kmax)                                             :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                             :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                           :: zk
    character(12)                                           , intent(in)  :: tkemod !  Description and declaration in tricom.igs
!
! Local variables
!
    integer  :: kbg       ! Denotes the k-index of vicuv/dicuv containing the background values
    integer  :: khtur     ! Denotes the k-index of vicuv/dicuv containing the HLES values
    integer  :: k
    integer  :: kmin
    integer  :: kup
    integer  :: maskval
    integer  :: ndm
    integer  :: nm
    integer  :: nmd
    integer  :: nm_pos    ! Indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp) :: aa
    real(fp) :: bb
    real(fp) :: difiwe
    real(fp) :: drhodz
    real(fp) :: dz
    real(fp) :: ee
    real(fp) :: epsd      ! Underbound denominator
    real(fp) :: fl
    real(fp) :: fs
    real(fp) :: h0
    real(fp) :: reldik
    real(fp) :: rl
    real(fp) :: rz
    real(fp) :: shear
    real(fp) :: sqrtbv    ! Square root of Brunt Vaisly frequency BRUVAI(NM,K)
    real(fp) :: tke
    real(fp) :: tkewin
    real(fp) :: ustbe
    real(fp) :: ustbot
    real(fp) :: ustwi
    real(fp) :: ustwin
    real(fp) :: ustwkw
    real(fp) :: utot
    real(fp) :: uuu
    real(fp) :: vvv
    real(fp) :: zcord
    real(fp) :: zw
    real(fp) :: zwc
!
    data epsd/1.e-20/
!
!! executable statements -------------------------------------------------------
!
    cmukl       => gdp%gdturcoe%cmukl
    cmukep      => gdp%gdturcoe%cmukep
    zwi         => gdp%gdturcoe%zwi
    inpzw       => gdp%gdturcoe%inpzw
    rhow        => gdp%gdphysco%rhow
    ag          => gdp%gdphysco%ag
    z0          => gdp%gdphysco%z0
    z0v         => gdp%gdphysco%z0v
    vonkar      => gdp%gdphysco%vonkar
    vicmol      => gdp%gdphysco%vicmol
    vicoww      => gdp%gdphysco%vicoww
    dicoww      => gdp%gdphysco%dicoww
    !
    ! Initialization
    !
    kbg    = kmax + 1
    khtur  = kmax + 2
    ee     = exp(1.0)
    nm_pos = 1
    !
    ! Copy the computed HLES eddy viscosity to the index that will be
    ! used by the program 
    ! This part of routine is only of interest in case KMAX = 1
    ! Copying of HLES eddy viscosity for 3D see below)
    !
    if (kmax==1) then
       do nm = 1, nmmax
          vicuv(nm, 1) = vicuv(nm, kbg)
          dicuv(nm, 1) = dicuv(nm, kbg)
       enddo
       goto 2000
    endif
    !
    ! Free surface moving through grid, set VICWW, DICWW, BRUVAI and RICH zero
    !
    do nm = 1, nmmax
       do k = 1, kmax
          vicww (nm, k) = 0.0
          dicww (nm, k) = 0.0
          bruvai(nm, k) = 0.0
          rich  (nm, k) = 0.0
       enddo
    enddo
    !
    if (tkemod=='Constant   ') then
       !=======================================================================
       !
       ! Vertical eddy viscosities/diffusivities at layer interfaces
       !
       ! Constant values for eddy viscosity & diffusivity
       !
       do nm = 1, nmmax
          if (kfsmin(nm)/=0) then
             do k = kfsmin(nm) - 1, kfsmax(nm)
                vicww(nm, k) = vicoww
                dicww(nm, k) = dicoww
             enddo
          endif
       enddo
    else
       !=======================================================================
       !
       ! Other turbulence models
       !
       !=======================================================================
       !
       ! Use DICWW as scratch array to compute Z0 in water-level points
       !
       nmd = -icx
       ndm = -icy
       do nm = 1, nmmax
          vicww(nm, kmax) = 0.0
          nmd = nmd + 1
          ndm = ndm + 1
          if (kfs(nm)==1) then
             dicww(nm, kmax) = (kfu(nm)*z0urou(nm) + kfu(nmd)*z0urou(nmd)       &
                             & + kfv(nm)*z0vrou(nm) + kfv(ndm)*z0vrou(ndm))     &
                             & /(kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
          endif
       enddo
       !
       ! Production and buoyancy term (only vertical gradients)
       !
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             do k = 1, kmax
                dudz(nm, k) = 0.0
                if (k>=kfumin(nm) .and. k<=kfumax(nm) - 1) then
                   kup         = k + 1
                   dz          = 0.5_fp*(dzu1(nm, k) + dzu1(nm, kup))
                   dudz(nm, k) = (ueul(nm, kup) - ueul(nm, k))/dz
                endif
             enddo
          endif
       enddo
       do nm = 1, nmmax
          if (kfv(nm) == 1) then
             do k = 1, kmax
                dvdz(nm, k) = 0.0
                if (k>=kfvmin(nm) .and. k<=kfvmax(nm) - 1) then
                   kup         = k + 1
                   dz          = 0.5_fp*(dzv1(nm, k) + dzv1(nm, kup))
                   dvdz(nm, k) = (veul(nm, kup) - veul(nm, k))/dz
                endif
             enddo
          endif
       enddo
       !
       ! Delft3D-16494: NOT NECESSARY?
       !
       ! parallel case: exchange arrays in the overlapping cells
       !
       call dfexchg(dudz, 0, kmax, dfloat, nm_pos, gdp)
       call dfexchg(dvdz, 0, kmax, dfloat, nm_pos, gdp)
       !
       do nm = 1, nmmax
          if (kfs(nm)==1) then
             nmd = nm - icx
             ndm = nm - icy
             h0     = max(s1(nm) + real(dps(nm),fp), 0.01_fp)
             if (kcs(nm)==3) then
                maskval = kcs(nm) - 2
             else
                maskval = kcs(nm)
             endif
             do k = kfsmin(nm), kfsmax(nm) - 1
                kup = k + 1
                dz     = 0.5_fp*(dzs1(nm, k) + dzs1(nm, kup))
                drhodz = (rho(nm, kup) - rho(nm, k))/dz
                shear = maskval*0.5_fp*(  dudz(nm, k)**2 + dudz(nmd, k)**2         &
                      &                 + dvdz(nm, k)**2 + dvdz(ndm, k)**2)
                shear = max(1E-8, shear)
                bruvai(nm, k) = -ag*drhodz/rho(nm, k)
                rich  (nm, k) = bruvai(nm, k)/shear
             enddo
          endif
       enddo
       !=======================================================================
       !
       ! Algebraic turbulence model (LTUR = 0)
       !
       !=======================================================================
       if (ltur==0) then
          do nm = 1, nmmax
             nmd = nm - icx
             ndm = nm - icy
             zcord = 0.
             if (kfs(nm)==1) then
                do k = kfsmin(nm), kfsmax(nm)
                   uuu = (ueul(nmd, k) + ueul(nm, k))                               &
                       & /max(1, kfuz1(nmd, k) + kfuz1(nm, k))
                   vvv = (veul(ndm, k) + veul(nm, k))                               &
                       & /max(1, kfvz1(ndm, k) + kfvz1(nm, k))
                   utot = sqrt(uuu*uuu + vvv*vvv)
                   !
                   ! Bottom is assumed at Z0
                   !
                   if (k==kfsmin(nm)) then
                      zcord = zcord + .5*dzs1(nm, k)
                   else
                      zcord = zcord + .5*(dzs1(nm, k - 1) + dzs1(nm, k))
                   endif
                   rz = 1.0 + zcord/dicww(nm, kmax)
                   vicww(nm, kmax) = vicww(nm, kmax)                            &
                                   & + utot*vonkar/(log(rz)*(kfsmax(nm)         &
                                   & - kfsmin(nm) + 1))
                enddo
             endif
          enddo
          !
          do nm = 1, nmmax
             nmd = nm - icx
             ndm = nm - icy
             if (kfs(nm)==1) then
                kmin = kfsmin(nm)
                if (kfsmin(nm)/=kfsmax(nm)) then
                   uuu = (ueul(nmd, kmin + 1) + ueul(nm, kmin + 1))                 &
                       & /max(1, kfuz1(nmd, kmin + 1) + kfuz1(nm, kmin + 1))
                   vvv = (veul(ndm, kmin + 1) + veul(nm, kmin + 1))                 &
                       & /max(1, kfvz1(ndm, kmin + 1) + kfvz1(nm, kmin + 1))
                   dz = dzs1(nm, kmin) + 0.5_fp*dzs1(nm, kmin + 1)
                else
                   uuu = (ueul(nmd, kmin) + ueul(nm, kmin))                         &
                       & /max(1, kfuz1(nmd, kmin) + kfuz1(nm, kmin))
                   vvv = (veul(ndm, kmin) + veul(nm, kmin))                         &
                       & /max(1, kfvz1(ndm, kmin) + kfvz1(nm, kmin))
                   dz = dzs1(nm, kmin)/ee
                endif
                utot = sqrt(uuu*uuu + vvv*vvv)
                !
                ! Bottom is assumed at Z0
                !
                rz              = 1.0_fp + dz/dicww(nm, kmax)
                ustbot          = utot*vonkar/log(rz)
                vicww(nm, kmax) = max(vicww(nm, kmax), ustbot)
             endif
          enddo
          !
          ! Kolmogorov-Prandtl mixing length model
          !
          do nm = 1, nmmax
             reldik = 0.
             do k = kfsmin(nm), kfsmax(nm) - 1
                nmd = nm - icx
                ndm = nm - icy
                if (kfs(nm)==1) then
                   h0 = max(0.01_fp, real(dps(nm),fp) + s1(nm))
                   !
                   ! Damping function mixing length  (FL)
                   ! Damping function diffusivity    (FS)
                   !
                   if (rich(nm, k)>=0.0) then
                      fl = exp( - 2.3*min(rich(nm, k), 30.0_fp))
                      aa = (1. + 3.33*rich(nm, k))
                      aa = aa*sqrt(aa)
                      bb = (1. + 10.0*rich(nm, k))
                      bb = sqrt(bb)
                      fs = aa/bb
                   else
                      fl = (1. - 14.*rich(nm, k))**0.25
                      fs = 1.
                   endif
                   !
                   ! Mixing length
                   !
                   reldik = reldik + dzs1(nm, k)/h0
                   rl = vonkar*h0*reldik*sqrt(1.0 - reldik)*fl
                   !
                   ! Algebraic eddy viscosity (Uittenbogaard '91 )
                   !
                   tkewin = sqrt(windsu(nm)**2 + windsv(nm)**2)
                   ustwi = sqrt(tkewin/rhow)
                   ustbe = vicww(nm, kmax)
                   tke = ustbe*ustbe*(1. - reldik)/sqrt(cmukep)                 &
                       & + ustwi*ustwi*(reldik)/sqrt(cmukep)
                   vicww(nm, k) = cmukl*rl*sqrt(tke)
                   !
                   ! Eddy viscosity is maximum Uittenbogaard/Kolmogorov-Prandtl
                   !
                   if (kcs(nm)==3) then
                      maskval = kcs(nm) - 2
                   else
                      maskval = kcs(nm)
                   endif
                   shear = 0.5*maskval*(dudz(nm, k)**2 + dudz(nmd, k)           &
                         & **2 + dvdz(nm, k)**2 + dvdz(ndm, k)**2)
                   shear = max(epsd, shear)
                   vicww(nm, k) = max(vicww(nm, k), rl*rl*sqrt(shear))
                   dicww(nm, k) = vicww(nm, k)/fs
                endif
             enddo
          enddo
       !=======================================================================
       !
       !    K-L model (LTUR = 1)
       !
       !=======================================================================
       elseif (ltur==1) then
          do nm = 1, nmmax
             reldik = 0.
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (kfs(nm)==1) then
                   h0 = max(0.01_fp, real(dps(nm),fp) + s1(nm))
                   !
                   ! Damping function mixing length  (FL)
                   ! Damping function diffusivity    (FS)
                   !
                   if (rich(nm, k)>=0.0) then
                      fl = exp( - 2.3*min(rich(nm, k), 30.0_fp))
                   else
                      fl = (1. - 14.*rich(nm, k))**0.25
                   endif
                   fs = 1.
                   reldik = reldik + dzs1(nm, k)/h0
                   rl = vonkar*h0*reldik*sqrt(1.0 - reldik)*fl
                   vicww(nm, k) = cmukl*rl*sqrt(rtur1(nm, k, 1))
                   dicww(nm, k) = cmukl*rl*sqrt(rtur1(nm, k, 1))/fs
                endif
             enddo
          enddo
       !=======================================================================
       !
       !   K-EPS model (LTUR = 2)
       !
       !=======================================================================
       else
          do nm = 1, nmmax
             h0 = real(dps(nm),fp) + s1(nm)
             if (kfs(nm)==1 .and. h0>0.01_fp) then
                do k = kfsmin(nm), kfsmax(nm) - 1
                   vicww(nm, k) = cmukep*rtur1(nm, k, 1)                        &
                                & **2/max(rtur1(nm, k, 2), epsd)
                   dicww(nm, k) = vicww(nm, k)
                enddo
             else
                !
                ! Make vicww and dicww small for points
                ! that are dry or almost dry
                ! Actually, it would be more consistent to use
                ! dicww(nm, k) = vicmol/sigmol
                ! but sigmol depends on l and is not available here
                ! So, just use
                ! dicww(nm, k) = vicmol
                !
                do k = kfsmin(nm), kfsmax(nm) - 1
                   vicww(nm, k) = vicmol
                   dicww(nm, k) = vicmol
                enddo
             endif
          enddo
       endif
       !
       ! Bottom and free surface (for all turbulence models except constant)
       !
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          h0  = real(dps(nm),fp) + s1(nm)
          if (kfs(nm)==1 .and. h0>0.01_fp) then
             !
             !
             ! Bottom is assumed at Z0 (for all turbulence models except constant)
             !
             kmin = kfsmin(nm)
             if (kfsmin(nm)/=kfsmax(nm)) then
                uuu = (ueul(nmd, kmin + 1) + ueul(nm, kmin + 1))                    &
                    & /max(1, kfuz1(nmd, kmin + 1) + kfuz1(nm, kmin + 1))
                vvv = (veul(ndm, kmin + 1) + veul(nm, kmin + 1))                    &
                    & /max(1, kfvz1(ndm, kmin + 1) + kfvz1(nm, kmin + 1))
                dz = dzs1(nm, kmin) + 0.5*dzs1(nm, kmin + 1)
             else
                uuu = (ueul(nmd, kmin) + ueul(nm, kmin))                            &
                    & /max(1, kfuz1(nmd, kmin) + kfuz1(nm, kmin))
                vvv = (veul(ndm, kmin) + veul(nm, kmin))                            &
                    & /max(1, kfvz1(ndm, kmin) + kfvz1(nm, kmin))
                dz = dzs1(nm, kmin)/ee
             endif
             utot   = sqrt(uuu*uuu + vvv*vvv)
             rz     = 1.0 + dz/dicww(nm, kmax)
             ustbot = abs(utot)*vonkar/log(rz)
             !
             vicww(nm, kmin - 1) = vonkar*ustbot*dicww(nm, kmax)
             dicww(nm, kmin - 1) = vicww(nm, kmin - 1)
             if (kfsmax(nm)/=kfsmin(nm)) then
                !
                ! Viscosity/difusivity reduction depending on stratification.
                ! The so-called Munk-Anderson (1948) damping function is applied.
                ! The stratified flows formulation of Busch (1972) is applied.
                ! See Delft3D-FLOW manual section 9.5 "Turbulence".
                ! See also [Journal of Marine Res., Vol 1] for related formulas.
                !
                if (rich(nm, kmin) >= 0.0_fp) then
                   fl = exp( - 2.3_fp * min(rich(nm, kmin), 30.0_fp))
                   aa = (1.0_fp + 3.33_fp*rich(nm, kmin))
                   aa = aa*sqrt(aa)
                   bb = (1.0_fp + 10.0_fp*rich(nm, kmin))
                   bb = sqrt(bb)
                   fs = aa/bb
                else
                   fl = (1.0_fp - 14.0_fp*rich(nm, kmin))**0.25_fp
                   fs = 1.0_fp
                endif
                vicww(nm, kmin) = vonkar*ustbot*dzs1(nm, kmin)*fl
                dicww(nm, kmin) = vicww(nm,kmin)/fs
             endif
             vicww(nm, kmax) = 0.0
             dicww(nm, kmax) = 0.0
             zwc = .5*dzs1(nm, kfsmax(nm))
             zw = inpzw*zwi + (1 - inpzw)*zwc
             ustwkw = sqrt(windsu(nm)**2 + windsv(nm)**2)/rhow
             ustwin = sqrt(ustwkw)
             vicww(nm, kfsmax(nm)) = vonkar*zw*ustwin
             dicww(nm, kfsmax(nm)) = vonkar*zw*ustwin
             !
             ! RTUR1 can be used to determine viscosity at surface
             ! Only for k-eps model
             !
             if (ltur == 2) then
                vicww(nm, kfsmax(nm)) = cmukep*rtur1(nm, kfsmax(nm), 1)**2   &
                                      & /max(rtur1(nm, kfsmax(nm), 2), epsd)
                dicww(nm, kfsmax(nm)) = vicww(nm, kfsmax(nm))
             endif
          else
             !
             ! Make vicww and dicww small for points
             ! that are dry or almost dry (for k=kfsmin and k=kfsmax)
             ! Actually, it would be more consistent to use
             ! dicww(nm, k) = vicmol/sigmol
             ! but sigmol depends on l and is not available here
             ! So, just use
             ! dicww(nm, k) = vicmol
             !
             ! KFSMAX may be uninitialized (-1)
             !
             vicww(nm, kfsmin(nm)) = vicmol
             dicww(nm, kfsmin(nm)) = vicmol
             vicww(nm, max(kfsmax(nm),kfsmin(nm))) = vicmol
             dicww(nm, max(kfsmax(nm),kfsmin(nm))) = vicmol
          endif
       enddo
       !
       ! The following code is moved to UZD, CUCNP:
       ! vicww(nm,k) = max(vicww(nm,k), vicoww)
       ! vicww(nm,k) = min(vicww(nm,k), 10)
       ! The following code is moved to DIFU, DIFUVL:
       ! dicww(nm,k) = max(dicww(nm,k), dicoww)
       ! dicww(nm,k) = min(dicww(nm,k), 10)
       !
    endif
    !
    ! Delft3D-16494: NOT NECESSARY?
    ! parallel case: exchange arrays in the overlapping cells
    !
    call dfexchg(vicww, 0, kmax, dfloat, nm_pos, gdp)
    call dfexchg(dicww, 0, kmax, dfloat, nm_pos, gdp)
    !
    ! Horizontal eddy viscosities and diffusivities in density points
    !
    do nm = 1, nmmax
       if (kfsmax(nm)>kfsmin(nm)) then
         do k = kfsmin(nm), kfsmax(nm)
            if (kfs(nm)==1) then
               vicuv(nm, k) = 0.5 * (vicww(nm, k) + vicww(nm, k-1)) &
                            & + vicuv(nm, kbg) + vicuv(nm, khtur)
               dicuv(nm, k) = 0.5 * (dicww(nm, k) + dicww(nm, k-1)) &
                            & + dicuv(nm, kbg) + dicuv(nm, khtur)
            endif
         enddo
       endif
    enddo
 2000 continue
end subroutine z_turclo
