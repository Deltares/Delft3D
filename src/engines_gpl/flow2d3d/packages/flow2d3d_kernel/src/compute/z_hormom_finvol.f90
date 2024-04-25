subroutine z_hormom_finvol(nmmax     ,kmax      ,icx       ,icy       ,kcs       , &
                         & kfu       ,kcu       ,kfv       ,kfuz0     ,kfumn0    , &
                         & kfumx0    ,kfvz0     ,kfs       ,kfsz0     ,kfsmn0    , &
                         & kfsmx0    ,u0        ,v0        ,w0        ,dzs0      , &
                         & dzu0      ,dzv0      ,s0        ,guu       ,gvv       , &
                         & gvu       ,guv       ,gsqs      ,gud       ,gvd       , &
                         & guz       ,gvz       ,ddk       ,p0        ,gdp       )
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
!    Function: The coefficient for the momentum equations are
!              computed and the stored in the arrays AAK, BBK,
!              CCK, and DDK (velocity points).
!              This is identical to the Z_CUCNP. However, now
!              an explicit time integration is applied.
! Method used: Reference : On the approximation of horizontal
!              gradients in sigma co-ordinates for bathymetry
!              with steep bottom slopes (G.S. Stelling and J.
!              van Kester - International Journal for Methods
!              in Fluids, Vol. 18 1994)
!              - Horizontal Advection in U-direction :
!                explicit, central scheme.
!              - Horizontal Advection in V-direction :
!                explicit, central scheme
!              - Horizontal Diffusion : explicit, along
!                Z-planes (3D), implicit (2DH)
!              - Vertical Advection : implicit, central scheme
!              - Vertical Diffusion : implicit
!              - roughness (partial slip) of rigid walls
!              - blockage flow by rigid sheets
!                (implemented for TAISEI)
!              Special approximation pressure term, based
!              on limiter to avoid artificial flow.
! Dummy subroutine for Z-model / domaindecomposition
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
    ! They replace the  include igd / include igp lines
    !
    include 'flow_steps_f.inc'
    !
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: vicmol
!
! Global variables
!
    integer                                                         :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX
    integer                                                         :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmn0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumn0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: p0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer            :: k
    integer            :: kadx
    integer            :: kady
    integer            :: kadz
    integer            :: kk
    integer            :: kd
    integer            :: kdd
    integer            :: ku
    integer            :: kup
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmu
    integer            :: num
    integer            :: ndmuu
    integer            :: numd
    integer            :: nuum
    integer            :: nmuu
    integer            :: numu
    integer            :: sfad     ! temporary work variable
    real(fp)           :: wsurfr
    real(fp)           :: wsurfl
    real(fp)           :: vol0     ! size of HU volume  
    real(fp)           :: area     ! area of flux interface (east)
    real(fp)           :: uavg0    ! transport velocity at interface, east
    real(fp)           :: vavg0    ! transport velocity at interface, north
    real(fp)           :: wavg0    ! transport velocity at interface, top
    real(fp)           :: pcoef    ! temporary value for coefficient pressure derivative
    real(fp)           :: ddkadx
    real(fp)           :: ddkady
    real(fp)           :: ddkadz
!
!! executable statements -------------------------------------------------------
!
    ag         => gdp%gdphysco%ag
    rhow       => gdp%gdphysco%rhow
    !
    ! Finite Volume, explicit first order upwind for horizontal terms
    !
    do nm = 1, nmmax
       nmd   = nm - icx
       ndm   = nm - icy
       ndmd  = nm - icx - icy
       nmu   = nm + icx
       nmuu  = nm + icx + icx
       num   = nm + icy
       numu  = nm + icx + icy
       ndmu  = nm + icx - icy
       ndmuu = nm + icx + icx - icy
       numd  = nm - icx + icy
       do k = 1, kmax
          ku = k + 1
          kd = k - 1
          !
          ! FOR EACH ACTIVE U-VOLUME
          !
          if (kfuz0(nm,k)*kcu(nm) == 1) then
             !
             ! CONVECTIVE FLUXES
             !
             kadx   = 1
             kady   = 1
             kadz   = 1
             ddkadx = 0.0_fp
             ddkady = 0.0_fp
             ddkadz = 0.0_fp
             ! 
             ! Fluxes west
             !
             if (kfsz0(nm,k) == 0) then ! .or. (kfu(nmd) /= 0 .and. k > kfumx0(nmd))
                !
                ! Velocity point active, left cell non-active, therefore velocity to the left
                ! Outflow into a non-active cell
                !
                area  = guu(nm)*dzs0(nmu,k) ! or guz(nm)?
                ddkadx = ddkadx + area*(u0(nm,k)**2)
             else
                !
                ! Internal domain
                !
                if (kcs(nm) == 2) then
                   if (u0(nm,k) < 0.0_fp) then !.or. kfsz0(nmu,k) == 0) then
                      !
                      ! Constant extrapolation outwards
                      !
                      area  = guu(nm)*dzs0(nm,k)
                      ddkadx = ddkadx + area*(u0(nm,k)**2)
                   else !if (kfsz0(nmu,k) == 1) then
                      !
                      ! No convection inwards
                      ! Set west flux equal to the east flux that is computed below
                      !
                      kadx = 0
                   endif
                else
                   !
                   ! Rectangular volumes at free surface
                   !
!                   if ( k == kfsmx0(nm) .or. k == kfsmx0(nmu) ) then
!                       area = guz(nm) * ( dzs0(nm,k) + dzs0(nmu,k) ) / real(kfsz0(nm,k)+kfsz0(nmu,k),fp)
!                   else
                       area  = guz(nm)*dzs0(nm,k)
!                   endif
                   uavg0  = 0.5_fp * (u0(nmd,k)+u0(nm,k))
                   ddkadx = ddkadx + area*( 0.5_fp*(uavg0+abs(uavg0))*u0(nmd,k) + 0.5_fp*(uavg0-abs(uavg0))*u0(nm,k) )
                endif
             endif
             !
             ! Fluxes east
             !
             if (kfsz0(nmu,k) == 0) then ! .or. (kfu(nmu) /= 0 .and. k > kfumx0(nmu) )
                !
                ! Velocity point active, right cell non-active, therefore velocity to the right
                ! Outflow into a non-active cell
                !
                area  = guu(nm)*dzs0(nm,k) ! or guz(nmu) ?
                ddkadx = ddkadx - area*( u0(nm,k)**2 )
             else
                !
                ! Internal domain
                !
                if (kcs(nmu) == 2) then
                   if ( u0(nm,k) > 0.0_fp ) then !.or. kfsz0(nm,k) == 0) then
                      !
                      ! Constant extrapolation outwards
                      !
                      area  = guu(nm)*dzs0(nmu,k)
                      ddkadx = ddkadx - area*(u0(nm,k)**2)
                   else !if (kfsz0(nm,k) == 1) then
                      !
                      ! No convection inwards
                      ! Set east flux equal to the west flux that is computed above
                      !
                      kadx = 0
                   endif
                else
                   !
                   ! Rectangular volumes at free surface
                   !
!                   if ( k == kfsmx0(nm) .or. k == kfsmx0(nmu) ) then
!                       area = guz(nm) * ( dzs0(nm,k) + dzs0(nmu,k) ) / real(kfsz0(nm,k)+kfsz0(nmu,k),fp)
!                   else
                       area  = guz(nm)*dzs0(nmu,k)
!                   endif
                   uavg0  = 0.5_fp * (u0(nm,k)+u0(nmu,k))
                   ddkadx = ddkadx - area*( 0.5_fp*(uavg0+abs(uavg0))*u0(nm,k) + 0.5_fp*(uavg0-abs(uavg0))*u0(nmu,k) )
                endif
             endif
             !
             ! Vertical fluxes
             ! 1) add flux through upper face of volume and scale
             ! if necessary (constant flux extrapolation),
             ! 2) add flux through lower face
             !
             
             !
             ! west side of upper face of volume
             !
!             sfad = 2
!             if (k == kfsmx0(nm) .and. kcs(nm) == 1 ) then
!                !
!                ! conservation correction at free surface
!                ! note: at a water level boundary the continuity equation cannot be used
!                !
!                wsurfl = w0(nm,kd)*gsqs(nm) + ( dzu0(nmd,k)*u0(nmd,k)*guu(nmd) - dzu0(nm,k)*u0(nm,k)*guu(nm) + &
!                                      &   dzv0(ndm,k)*v0(ndm,k)*gvv(ndm) - dzv0(nm,k)*v0(nm,k)*gvv(nm) )
!                ddkadz = ddkadz - 0.5_fp*wsurfl*u0(nm,k)
!             elseif (k < kmax) then
!                if( kfsz0(nm,k)*kfsz0(nm,ku) == 1) then
!                !
!                ! internal flux, perpendicular inflow at water level boundaries
!                !
!                wavg0 = w0(nm,k) !+w0(nmu,k)) / real(kfsz0(nm,k)+kfsz0(nmu,k),fp) ! ( max(1,(kfsz0(nm,k)+kfsz0(nmu,k)) )
!                area  = 0.5_fp*gsqs(nm) !+ gsqs(nmu) )
!                ddkadz = ddkadz - area*( 0.5_fp*(wavg0+abs(wavg0))*u0(nm,k) +  0.5_fp*(wavg0-abs(wavg0))*u0(nm,ku) )
!                else
!                    sfad = sfad - 1
!                endif 
!             else
!                !
!                ! no information for w available (use constant extrapolation of flux at other side)
!                ! note: incorrect for non-uniform horizontal grid spacing
!                !
!                sfad = sfad - 1
!             endif
!             !
!             ! east side of upper face of volume
!             !
!             if (k == kfsmx0(nmu) .and. kcs(nmu) == 1 ) then
!                !
!                ! conservation correction at free surface
!                ! note: at a water level boundary the continuity equation cannot be used
!                !
!                wsurfr = w0(nmu,kd)*gsqs(nmu) + ( dzu0(nm,k)*u0(nm,k)*guu(nm) - dzu0(nmu,k)*u0(nmu,k)*guu(nmu) + &
!                                        &   dzv0(nm,k)*v0(nm,k)*gvv(nm) - dzv0(num,k)*v0(num,k)*gvv(num) )
!                ddkadz = ddkadz - 0.5_fp*wsurfr*u0(nm,k)
!             elseif ( k < kmax ) then
!                if (kfsz0(nmu,k)*kfsz0(nmu,ku) == 1) then
!                wavg0 = w0(nmu,k) !/ real(kfsz0(nm,k)+kfsz0(nmu,k),fp) ! ( max(1,(kfsz0(nm,k)+kfsz0(nmu,k)) )
!                area  = 0.5_fp*gsqs(nmu)
!                ddkadz = ddkadz - area*( 0.5_fp*(wavg0+abs(wavg0))*u0(nm,k) +  0.5_fp*(wavg0-abs(wavg0))*u0(nm,ku) )
!                else
!                    sfad = sfad - 1
!                endif
!             else
!                !
!                ! no information for w available (use constant extrapolation of flux at other side)
!                ! note: incorrect for non-uniform horizontal grid spacing
!                !
!                sfad = sfad - 1
!             endif
!             !
!             ! 
!             !
!             ddkadz = ddkadz * 2.0_fp / real(max(1,sfad),fp)
!             !
!             ! now also add the flux through the lower face of the volume
!             !
!             if (k > kfumin(nm)) then
!                !
!                ! lower face of volume
!                ! perpendicular inflow at water level boundaries
!                !
!                wavg0 = (w0(nm,kd)+w0(nmu,kd)) / real(kfsz0(nm,kd)*kfsz0(nm,k)+kfsz0(nmu,k)*kfsz0(nmu,kd),fp) ! ( max(1,(kfsz0(nm,kd)+kfsz0(nmu,kd)) )
!                area  = 0.5_fp*( gsqs(nm) + gsqs(nmu) )
!                ddkadz = ddkadz + area*( 0.5_fp*(wavg0+abs(wavg0))*u0(nm,kd) + 0.5_fp*(wavg0-abs(wavg0))*u0(nm,k) )
!             endif
             !
             ! Update system coefficients
             !
             vol0  = ( guz(nm )*gvz(nm )*dzs0(nm ,k) + &
                   &   guz(nmu)*gvz(nmu)*dzs0(nmu,k) ) / real(kfsz0(nm,k)+kfsz0(nmu,k),fp) ! ( max(1,kfsz0(nm,k)+kfsz0(nmu,k)) )
                   
             ddk(nm,k) = ddk(nm,k) + ( ddkadx*real(kadx,fp) + ddkady*real(kady,fp) + ddkadz*real(kadz,fp) ) / vol0
          endif
       enddo
    enddo
end subroutine z_hormom_finvol
